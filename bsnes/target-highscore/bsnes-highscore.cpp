#include "bsnes-highscore.h"

#include "program.cpp"

struct _bsnesCore
{
  HsCore parent_instance;

  Emulator::Interface *emulator;
  Program *program;

  HsSoftwareContext *context;

  HsGameBoyModel sgb_model;
  HsGameBoyModel pending_sgb_model;

  gboolean loaded;
  HsSuperNesAccessory accessory;
  HsGameBoyAccessory gb_accessory;
};

static void bsnes_game_boy_core_init (HsGameBoyCoreInterface *iface);
static void bsnes_super_nes_core_init (HsSuperNesCoreInterface *iface);
static void bsnes_super_game_boy_core_init (HsSuperGameBoyCoreInterface *iface);

G_DEFINE_FINAL_TYPE_WITH_CODE (bsnesCore, bsnes_core, HS_TYPE_CORE,
                               G_IMPLEMENT_INTERFACE (HS_TYPE_GAME_BOY_CORE, bsnes_game_boy_core_init)
                               G_IMPLEMENT_INTERFACE (HS_TYPE_SUPER_NES_CORE, bsnes_super_nes_core_init)
                               G_IMPLEMENT_INTERFACE (HS_TYPE_SUPER_GAME_BOY_CORE, bsnes_super_game_boy_core_init));

static void
setup_input (bsnesCore *self)
{
  switch (self->accessory) {
    case HS_SUPER_NES_ACCESSORY_NONE:
      self->emulator->connect (SuperFamicom::ID::Port::Controller1, SuperFamicom::ID::Device::Gamepad);
      self->emulator->connect (SuperFamicom::ID::Port::Controller2, SuperFamicom::ID::Device::Gamepad);
      break;
    case HS_SUPER_NES_ACCESSORY_MULTITAP:
      self->emulator->connect (SuperFamicom::ID::Port::Controller1, SuperFamicom::ID::Device::Gamepad);
      self->emulator->connect (SuperFamicom::ID::Port::Controller2, SuperFamicom::ID::Device::SuperMultitap);
      break;
    default:
      g_assert_not_reached ();
  }
}

static HsSuperGameBoyFirmware
get_sgb_firmware_id (bsnesCore *self)
{
  if (self->pending_sgb_model == HS_GAME_BOY_MODEL_SGB2)
    return HS_SUPER_GAME_BOY_FIRMWARE_SGB2_ROM;

  return HS_SUPER_GAME_BOY_FIRMWARE_SGB_ROM;
}

static gboolean
check_sgb_model (bsnesCore *self, GError **error)
{
  if (self->pending_sgb_model == HS_GAME_BOY_MODEL_SGB ||
      self->pending_sgb_model == HS_GAME_BOY_MODEL_SGB2) {
    return TRUE;
  }

  g_set_error (error, HS_CORE_ERROR, HS_CORE_ERROR_INTERNAL, "bsnes only supports Super Game Boy");
  return FALSE;
}

static gboolean
try_migrate_libretro_save (bsnesCore   *self,
                           const char  *save_path,
                           GError     **error)
{
  g_autoptr (GFile) save_file = g_file_new_for_path (save_path);

  if (!g_file_query_exists (save_file, NULL))
    return TRUE;

  if (g_file_query_file_type (save_file, G_FILE_QUERY_INFO_NONE, NULL) == G_FILE_TYPE_DIRECTORY)
    return TRUE;

  HsPlatform platform = hs_core_get_platform (HS_CORE (self));
  const char *dest_name;

  if (platform == HS_PLATFORM_SUPER_NES) {
    dest_name = "save.srm";
  } else if (platform == HS_PLATFORM_SUPER_GAME_BOY) {
    dest_name = "save.sav";
  } else {
    return TRUE;
  }

  // Make a temporary file
  g_autofree char *cache_path = hs_core_get_cache_path (HS_CORE (self));
  g_autoptr (GFile) cache_dir = g_file_new_for_path (cache_path);
  if (!g_file_query_exists (cache_dir, NULL) &&
      !g_file_make_directory_with_parents (cache_dir, NULL, error)) {
    return FALSE;
  }

  g_autofree char *tmp_path = g_build_filename (cache_path, "bsnes-save-XXXXXX", NULL);
  tmp_path = g_mkdtemp (tmp_path);
  g_autoptr (GFile) tmp_file = g_file_new_for_path (tmp_path);

  // Move the old save, replace it with a directory
  g_autoptr (GFile) tmp_save_file = g_file_get_child (tmp_file, "save");
  if (!g_file_move (save_file, tmp_save_file, G_FILE_COPY_BACKUP, NULL, NULL, NULL, error))
    return FALSE;

  if (!g_file_make_directory_with_parents (save_file, NULL, error))
    return FALSE;

  g_autoptr (GFile) dest_file = g_file_get_child (save_file, dest_name);
  if (!g_file_move (tmp_save_file, dest_file, G_FILE_COPY_BACKUP, NULL, NULL, NULL, error))
    return FALSE;

  if (!g_file_delete (tmp_file, NULL, error))
    return FALSE;

  hs_core_log (HS_CORE (self), HS_LOG_MESSAGE, "Libretro save files migrated successfully");

  return TRUE;
}

static gboolean
try_rename_save_files (bsnesCore   *self,
                       const char  *save_path,
                       GError     **error)
{
  g_autoptr (GFile) save_dir = g_file_new_for_path (save_path);

  HsPlatform platform = hs_core_get_platform (HS_CORE (self));
  if (platform != HS_PLATFORM_SUPER_NES)
    return TRUE;

  if (!g_file_query_exists (save_dir, NULL))
    return TRUE;

  if (g_file_query_file_type (save_dir, G_FILE_QUERY_INFO_NONE, NULL) != G_FILE_TYPE_DIRECTORY)
    return FALSE;

  // Initially I went with internal names: save.ram, download.ram, time.rtc
  // For better interoperability, let's rename them to: save.srm, save.psr, save.rtc

  g_autoptr (GFileEnumerator) enumerator = NULL;
  GFileInfo *info;
  g_autoptr (GFile) save_file = NULL;
  g_autoptr (GFile) save_dest = NULL;
  g_autoptr (GFile) download_file = NULL;
  g_autoptr (GFile) download_dest = NULL;
  g_autoptr (GFile) time_file = NULL;
  g_autoptr (GFile) time_dest = NULL;

  enumerator =
    g_file_enumerate_children (save_dir, G_FILE_ATTRIBUTE_STANDARD_NAME,
                               G_FILE_QUERY_INFO_NONE, NULL, error);
  if (!enumerator)
    return FALSE;

  while ((info = g_file_enumerator_next_file (enumerator, NULL, error))) {
    const char *filename = g_file_info_get_name (info);

    if (g_strcmp0 (filename, "save.srm") && (g_str_has_suffix (filename, ".srm") || !g_strcmp0 (filename, "save.ram")) && !save_file) {
      save_file = g_file_get_child (save_dir, filename);
      save_dest = g_file_get_child (save_dir, "save.srm");
    } else if (g_strcmp0 (filename, "save.psr") && (g_str_has_suffix (filename, ".psr") || !g_strcmp0 (filename, "download.ram")) && !download_file) {
      download_file = g_file_get_child (save_dir, filename);
      download_dest = g_file_get_child (save_dir, "save.psr");
    } else if (g_strcmp0 (filename, "save.rtc") && (g_str_has_suffix (filename, ".rtc") || !g_strcmp0 (filename, "time.rtc")) && !time_file) {
      time_file = g_file_get_child (save_dir, filename);
      time_dest = g_file_get_child (save_dir, "save.rtc");
    }

    g_object_unref (info);
  }

  if (save_file && !g_file_move (save_file, save_dest, G_FILE_COPY_OVERWRITE, NULL, NULL, NULL, error))
    return FALSE;
  if (download_file && !g_file_move (download_file, download_dest, G_FILE_COPY_OVERWRITE, NULL, NULL, NULL, error))
    return FALSE;
  if (time_file && !g_file_move (time_file, time_dest, G_FILE_COPY_OVERWRITE, NULL, NULL, NULL, error))
    return FALSE;

  if (save_file || download_file || time_file)
    hs_core_log (HS_CORE (self), HS_LOG_MESSAGE, "Save files renamed successfully");

  return TRUE;
}

static void
print_image_cb (GB_gameboy_t *gb,
                uint32_t     *image,
                uint8_t       height,
                uint8_t       top_margin,
                uint8_t       bottom_margin,
                uint8_t       exposure)
{
  bsnesCore *self = BSNES_CORE (GB_get_user_data (gb));

  guint8 width = 160;
  guint8 *data = g_new (guint8, width * height);

  for (int i = 0; i < width * height; i++)
    data[i] = image[i] & 0xFF;

  GBytes *bytes = g_bytes_new_take (data, width * height);

  hs_game_boy_core_emit_print_started (HS_GAME_BOY_CORE (self), bytes,
                                       top_margin, bottom_margin, exposure);

  g_bytes_unref (bytes);
}

static void
printer_done_cb (GB_gameboy_t *gb)
{
  bsnesCore *self = BSNES_CORE (GB_get_user_data (gb));

  hs_game_boy_core_emit_print_done (HS_GAME_BOY_CORE (self));
}

static void
update_gb_accessory (bsnesCore *self)
{
  // Only SGB2 has a link cable port, not SGB
  if (self->sgb_model == HS_GAME_BOY_MODEL_SGB)
    return;

  switch (self->gb_accessory) {
    case HS_GAME_BOY_ACCESSORY_NONE:
      // FIXME: SameBoy doesn't have a way to unplug it?..
      break;
    case HS_GAME_BOY_ACCESSORY_PRINTER:
      GB_connect_printer (&gameboy, print_image_cb, printer_done_cb);
      break;
    default:
      g_assert_not_reached ();
  }
}

static gboolean
bsnes_core_load_rom (HsCore      *core,
                     const char **rom_paths,
                     int          n_rom_paths,
                     const char  *save_path,
                     GError     **error)

{
  bsnesCore *self = BSNES_CORE (core);
  HsPlatform platform = hs_core_get_platform (core);

  g_assert (n_rom_paths == 1);

  if (!try_migrate_libretro_save (self, save_path, error))
    return FALSE;

  if (!try_rename_save_files (self, save_path, error))
    return FALSE;

  self->emulator = new SuperFamicom::Interface;
  self->program = new Program (self->emulator);

  self->program->filterRender = &Filter::None::render;
  self->program->filterSize = &Filter::None::size;
  self->program->updateVideoPalette ();

  self->context = hs_core_create_software_context (core, 576, 540, HS_PIXEL_FORMAT_B8G8R8X8);
  self->program->context = self->context;

  g_set_str (&self->program->saveDir, save_path);

  if (platform == HS_PLATFORM_SUPER_GAME_BOY) {
    const char *firmware_path;

    hs_core_reset_used_firmware (HS_CORE (self));

    if (!check_sgb_model (self, error))
      return FALSE;

    self->program->gameBoy.location = string (rom_paths[0]);

    firmware_path = hs_core_query_firmware_path (core, get_sgb_firmware_id (self));

    if (!firmware_path) {
      if (self->pending_sgb_model == HS_GAME_BOY_MODEL_SGB2)
        g_set_error (error, HS_CORE_ERROR, HS_CORE_ERROR_MISSING_FIRMWARE, "Missing Super Game Boy 2 ROM");
      else
        g_set_error (error, HS_CORE_ERROR, HS_CORE_ERROR_MISSING_FIRMWARE, "Missing Super Game Boy ROM");

      return FALSE;
    }

    self->program->superFamicom.location = string (firmware_path);
  } else {
    self->program->superFamicom.location = string (rom_paths[0]);
  }

  self->program->base_name = string (rom_paths[0]);

  self->program->load ();
  self->sgb_model = self->pending_sgb_model;
  self->loaded = TRUE;

  if (platform == HS_PLATFORM_SUPER_GAME_BOY) {
    GB_set_user_data (&gameboy, self);
    update_gb_accessory (self);
  }

  setup_input (self);

  return TRUE;
}

static void
bsnes_core_poll_input (HsCore *core, HsInputState *input_state)
{
  bsnesCore *self = BSNES_CORE (core);

  self->program->inputState = input_state->super_nes;
}

static void
bsnes_core_run_frame (HsCore *core)
{
  bsnesCore *self = BSNES_CORE (core);

  self->emulator->run ();

  hs_core_play_samples (core, self->program->audioOut, self->program->audioLength);
  self->program->audioLength = 0;
}

static gboolean
maybe_reload_for_sgb (bsnesCore *self, GError **error)
{
  const char *firmware_path;

  if (hs_core_get_platform (HS_CORE (self)) != HS_PLATFORM_SUPER_GAME_BOY)
    return TRUE;

  if (self->sgb_model == self->pending_sgb_model)
    return TRUE;

  hs_core_reset_used_firmware (HS_CORE (self));

  if (!check_sgb_model (self, error))
    return FALSE;

  firmware_path = hs_core_query_firmware_path (HS_CORE (self), get_sgb_firmware_id (self));

  if (!firmware_path) {
    if (self->pending_sgb_model == HS_GAME_BOY_MODEL_SGB2)
      g_set_error (error, HS_CORE_ERROR, HS_CORE_ERROR_MISSING_FIRMWARE, "Missing Super Game Boy 2 ROM");
    else
      g_set_error (error, HS_CORE_ERROR, HS_CORE_ERROR_MISSING_FIRMWARE, "Missing Super Game Boy ROM");

    return FALSE;
  }

  self->program->superFamicom.location = string (firmware_path);

  self->program->load ();
  self->sgb_model = self->pending_sgb_model;

  if (self->sgb_model == HS_GAME_BOY_MODEL_SGB2) {
    GB_set_user_data (&gameboy, self);

    update_gb_accessory (self);
  }

  setup_input (self);

  return TRUE;
}

static gboolean
bsnes_core_reset (HsCore *core, gboolean hard, GError **error)
{
  bsnesCore *self = BSNES_CORE (core);

  if (hard) {
    if (hs_core_get_platform (core) == HS_PLATFORM_SUPER_GAME_BOY && self->pending_sgb_model != self->sgb_model)
      return maybe_reload_for_sgb (self, error);

    self->emulator->power ();
    self->program->colorburstPhase = 0;
  } else {
    self->emulator->reset ();
  }

  return TRUE;
}

static void
bsnes_core_stop (HsCore *core)
{
  bsnesCore *self = BSNES_CORE (core);

  delete self->program;
  self->program = NULL;
  self->emulator = NULL;
  self->loaded = FALSE;

  g_clear_object (&self->context);
}

static gboolean
bsnes_core_reload_save (HsCore      *core,
                        const char  *save_path,
                        GError     **error)
{
  bsnesCore *self = BSNES_CORE (core);

  g_set_str (&self->program->saveDir, save_path);

  if (!try_migrate_libretro_save (self, save_path, error))
    return FALSE;

  if (!try_rename_save_files (self, save_path, error))
    return FALSE;

  self->program->load ();

  setup_input (self);

  return TRUE;
}

static gboolean
bsnes_core_sync_save (HsCore  *core,
                      GError **error)
{
  bsnesCore *self = BSNES_CORE (core);

  self->program->save ();

  return TRUE;
}

static void
bsnes_core_load_state (HsCore          *core,
                       const char      *path,
                       HsStateCallback  callback)
{
  bsnesCore *self = BSNES_CORE (core);
  g_autoptr (GFile) file = g_file_new_for_path (path);
  GError *error = NULL;
  char *data;
  size_t size;

  if (!maybe_reload_for_sgb (self, &error)) {
    callback (core, &error);
    return;
  }

  if (!g_file_load_contents (file, NULL, &data, &size, NULL, &error)) {
    callback (core, &error);
    return;
  }

  serializer s ((guchar *) data, size);
  self->emulator->unserialize (s);

  self->program->colorburstPhase = (hs_core_get_colorburst_offset (core) > 0.1) ? 1 : 0;

  callback (core, NULL);
}

static void
bsnes_core_save_state (HsCore          *core,
                       const char      *path,
                       HsStateCallback  callback)
{
  bsnesCore *self = BSNES_CORE (core);
  g_autoptr (GFile) file = g_file_new_for_path (path);
  GError *error = NULL;

  auto serializer = self->emulator->serialize ();

  if (!g_file_replace_contents (file, (char *) serializer.data (), serializer.size (),
                                NULL, FALSE, G_FILE_CREATE_NONE, NULL, NULL, &error)) {
    callback (core, &error);
    return;
  }

  callback (core, NULL);
}

static double
bsnes_core_get_frame_rate (HsCore *core)
{
  bsnesCore *self = BSNES_CORE (core);

  if (self->program->superFamicom.region == "NTSC")
    return 21477272.0 / 357366.0;
  else
    return 21281370.0 / 425568.0;
}

static double
bsnes_core_get_aspect_ratio (HsCore *core)
{
  bsnesCore *self = BSNES_CORE (core);

  double width = 256;
  double height = 240;
  double par;

  if (self->program->superFamicom.region == "NTSC")
    par = 8.0 / 7.0;
  else
    par = 2950000.0 / 2128137.0;

  return width / height * par;
}

static double
bsnes_core_get_sample_rate (HsCore *core)
{
  return 48000;
}

static int
bsnes_core_get_channels (HsCore *core)
{
  return 2;
}

static HsRegion
bsnes_core_get_region (HsCore *core)
{
  bsnesCore *self = BSNES_CORE (core);

  if (self->program->superFamicom.region == "NTSC")
    return HS_REGION_NTSC;
  else
    return HS_REGION_PAL;
}

static void
bsnes_core_class_init (bsnesCoreClass *klass)
{
  HsCoreClass *core_class = HS_CORE_CLASS (klass);

  core_class->load_rom = bsnes_core_load_rom;
  core_class->poll_input = bsnes_core_poll_input;
  core_class->run_frame = bsnes_core_run_frame;
  core_class->reset = bsnes_core_reset;
  core_class->stop = bsnes_core_stop;

  core_class->reload_save = bsnes_core_reload_save;
  core_class->sync_save = bsnes_core_sync_save;

  core_class->load_state = bsnes_core_load_state;
  core_class->save_state = bsnes_core_save_state;

  core_class->get_frame_rate = bsnes_core_get_frame_rate;
  core_class->get_aspect_ratio = bsnes_core_get_aspect_ratio;

  core_class->get_sample_rate = bsnes_core_get_sample_rate;
  core_class->get_channels = bsnes_core_get_channels;

  core_class->get_region = bsnes_core_get_region;
}

static void
bsnes_core_init (bsnesCore *self)
{
  self->pending_sgb_model = HS_GAME_BOY_MODEL_SGB;
}

static void
bsnes_game_boy_core_set_model (HsGameBoyCore *core, HsGameBoyModel model)
{
  bsnesCore *self = BSNES_CORE (core);

  switch (model) {
  case HS_GAME_BOY_MODEL_DMG:
  case HS_GAME_BOY_MODEL_MGB:
  case HS_GAME_BOY_MODEL_CGB:
  case HS_GAME_BOY_MODEL_AGB:
    hs_core_log_literal (HS_CORE (self), HS_LOG_CRITICAL, "bsnes only supports Super Game Boy");
    break;
  case HS_GAME_BOY_MODEL_SGB:
  case HS_GAME_BOY_MODEL_SGB2:
    self->pending_sgb_model = model;
    break;
  default:
    g_assert_not_reached ();
  }
}

static void
bsnes_game_boy_core_set_accessory (HsGameBoyCore *core, HsGameBoyAccessory accessory)
{
  bsnesCore *self = BSNES_CORE (core);

  if (self->gb_accessory == accessory)
    return;

  self->gb_accessory = accessory;

  if (self->sgb_model == HS_GAME_BOY_MODEL_SGB2 && self->loaded)
    update_gb_accessory (self);
}

static void
bsnes_game_boy_core_init (HsGameBoyCoreInterface *iface)
{
  iface->set_model = bsnes_game_boy_core_set_model;
  iface->set_accessory = bsnes_game_boy_core_set_accessory;
}

static void
bsnes_super_nes_core_set_accessory (HsSuperNesCore      *core,
                                    HsSuperNesAccessory  accessory)
{
  bsnesCore *self = BSNES_CORE (core);

  self->accessory = accessory;

  if (self->loaded)
    setup_input (self);
}

static void
bsnes_super_nes_core_init (HsSuperNesCoreInterface *iface)
{
  iface->set_accessory = bsnes_super_nes_core_set_accessory;
}

static void
bsnes_super_game_boy_core_init (HsSuperGameBoyCoreInterface *iface)
{
}

GType
hs_get_core_type (void)
{
  return BSNES_TYPE_CORE;
}