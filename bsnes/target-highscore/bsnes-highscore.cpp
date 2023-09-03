#include "bsnes-highscore.h"

#include "program.cpp"

struct _bsnesCore
{
  HsCore parent_instance;

  Emulator::Interface *emulator;
  Program *program;

  HsSoftwareContext *context;
};

static void bsnes_super_nes_core_init (HsSuperNesCoreInterface *iface);

G_DEFINE_FINAL_TYPE_WITH_CODE (bsnesCore, bsnes_core, HS_TYPE_CORE,
                               G_IMPLEMENT_INTERFACE (HS_TYPE_SUPER_NES_CORE, bsnes_super_nes_core_init));

static gboolean
bsnes_core_load_rom (HsCore      *core,
                     const char  *rom_path,
                     const char  *save_path,
                     GError     **error)

{
  bsnesCore *self = BSNES_CORE (core);

  self->emulator = new SuperFamicom::Interface;
  self->program = new Program (self->emulator);

  self->program->filterRender = &Filter::None::render;
  self->program->filterSize = &Filter::None::size;
  self->program->updateVideoPalette ();

  self->context = hs_core_create_software_context (core, 2304, 2160, HS_PIXEL_FORMAT_XRGB8888_REV);
  self->program->context = self->context;

  self->program->saveDir = g_strdup (save_path);

  self->program->superFamicom.location = string (rom_path);
  self->program->base_name = string (rom_path);

  self->program->load ();

  self->emulator->connect (SuperFamicom::ID::Port::Controller1, SuperFamicom::ID::Device::Gamepad);
  self->emulator->connect (SuperFamicom::ID::Port::Controller2, SuperFamicom::ID::Device::Gamepad);

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

static void
bsnes_core_reset (HsCore *core)
{
  bsnesCore *self = BSNES_CORE (core);

  self->emulator->reset ();
}

static void
bsnes_core_stop (HsCore *core)
{
  bsnesCore *self = BSNES_CORE (core);

  delete self->program;
  self->program = NULL;
  self->emulator = NULL;

  g_clear_object (&self->context);
}

static gboolean
bsnes_core_save_data (HsCore  *core,
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

  if (!g_file_load_contents (file, NULL, &data, &size, NULL, &error)) {
    callback (core, &error);
    return;
  }

  serializer s ((guchar *) data, size);
  self->emulator->unserialize (s);

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

  if (self->program->superFamicom.region == "NTSC")
    return 1.306122;
  else
    return 1.584216;
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

static void
bsnes_core_class_init (bsnesCoreClass *klass)
{
  HsCoreClass *core_class = HS_CORE_CLASS (klass);

  core_class->load_rom = bsnes_core_load_rom;
  core_class->poll_input = bsnes_core_poll_input;
  core_class->run_frame = bsnes_core_run_frame;
  core_class->reset = bsnes_core_reset;
  core_class->stop = bsnes_core_stop;

  core_class->save_data = bsnes_core_save_data;

  core_class->load_state = bsnes_core_load_state;
  core_class->save_state = bsnes_core_save_state;

  core_class->get_frame_rate = bsnes_core_get_frame_rate;
  core_class->get_aspect_ratio = bsnes_core_get_aspect_ratio;

  core_class->get_sample_rate = bsnes_core_get_sample_rate;
  core_class->get_channels = bsnes_core_get_channels;
}

static void
bsnes_core_init (bsnesCore *self)
{
}

static void
bsnes_super_nes_core_init (HsSuperNesCoreInterface *iface)
{
}

GType
hs_get_core_type (void)
{
  return BSNES_TYPE_CORE;
}