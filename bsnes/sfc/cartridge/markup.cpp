#ifdef CARTRIDGE_CPP

void Cartridge::parse_markup(const char *markup) {
  mapping.reset();

  XML::Document document(markup);
  auto &cartridge = document["cartridge"];
  region = cartridge["region"].data != "PAL" ? Region::NTSC : Region::PAL;

  parse_markup_rom(cartridge["rom"]);
  parse_markup_ram(cartridge["ram"]);
  parse_markup_icd2(cartridge["icd2"]);
  parse_markup_bsx(cartridge["bsx"]);
  parse_markup_sufamiturbo(cartridge["sufamiturbo"]);
  parse_markup_nss(cartridge["nss"]);
  parse_markup_sa1(cartridge["sa1"]);
  parse_markup_superfx(cartridge["superfx"]);
  parse_markup_armdsp(cartridge["armdsp"]);
  parse_markup_hitachidsp(cartridge["hitachidsp"]);
  parse_markup_necdsp(cartridge["necdsp"]);
  parse_markup_epsonrtc(cartridge["epsonrtc"]);
  parse_markup_sharprtc(cartridge["sharprtc"]);
  parse_markup_spc7110(cartridge["spc7110"]);
  parse_markup_sdd1(cartridge["sdd1"]);
  parse_markup_obc1(cartridge["obc1"]);
  parse_markup_msu1(cartridge["msu1"]);
}

//

void Cartridge::parse_markup_map(Mapping &m, XML::Node &map) {
  m.offset = numeral(map["offset"].data);
  m.size = numeral(map["size"].data);

  string data = map["mode"].data;
  if(data == "direct") m.mode = Bus::MapMode::Direct;
  if(data == "linear") m.mode = Bus::MapMode::Linear;
  if(data == "shadow") m.mode = Bus::MapMode::Shadow;

  lstring part;
  part.split(":", map["address"].data);
  if(part.size() != 2) return;

  lstring subpart;
  subpart.split("-", part[0]);
  if(subpart.size() == 1) {
    m.banklo = hex(subpart[0]);
    m.bankhi = m.banklo;
  } else if(subpart.size() == 2) {
    m.banklo = hex(subpart[0]);
    m.bankhi = hex(subpart[1]);
  }

  subpart.split("-", part[1]);
  if(subpart.size() == 1) {
    m.addrlo = hex(subpart[0]);
    m.addrhi = m.addrlo;
  } else if(subpart.size() == 2) {
    m.addrlo = hex(subpart[0]);
    m.addrhi = hex(subpart[1]);
  }
}

void Cartridge::parse_markup_memory(MappedRAM &ram, XML::Node &node, unsigned id, bool writable) {
  string name = node["name"].data;
  unsigned size = numeral(node["size"].data);
  ram.map(allocate<uint8>(size, 0xff), size);
  if(name.empty() == false) {
    interface->loadRequest(id, name);
    if(writable) memory.append({id, name});
  }
}

//

void Cartridge::parse_markup_rom(XML::Node &root) {
  if(root.exists() == false) return;
  parse_markup_memory(rom, root, ID::ROM, false);

  for(auto &node : root) {
    if(node.name != "map") continue;
    Mapping m(rom);
    parse_markup_map(m, node);
    if(m.size == 0) m.size = rom.size();
    mapping.append(m);
  }
}

void Cartridge::parse_markup_ram(XML::Node &root) {
  if(root.exists() == false) return;
  parse_markup_memory(ram, root, ID::RAM, true);

  for(auto &node : root) {
    Mapping m(ram);
    parse_markup_map(m, node);
    if(m.size == 0) m.size = ram.size();
    mapping.append(m);
  }
}

void Cartridge::parse_markup_icd2(XML::Node &root) {
  if(root.exists() == false) return;
  has_gb_slot = true;

  interface->loadRequest(ID::SuperGameBoy, "Game Boy", "gb");

  icd2.revision = max(1, numeral(root["revision"].data));

  for(auto &node : root) {
    if(node.name != "map") continue;
    Mapping m({&ICD2::read, &icd2}, {&ICD2::write, &icd2});
    parse_markup_map(m, node);
    mapping.append(m);
  }
}

void Cartridge::parse_markup_bsx(XML::Node &root) {
  if(root.exists() == false) return;
  has_bs_cart = root["mmio"].exists();
  has_bs_slot = true;

  interface->loadRequest(ID::Satellaview, "BS-X Satellaview", "bs");

  if(has_bs_cart) {
    parse_markup_memory(bsxcartridge.psram, root["psram"], ID::BsxPSRAM, true);
  }

  for(auto &node : root["slot"]) {
    if(node.name != "map") continue;
    if(bsxflash.memory.size() == 0) continue;

    Mapping m(bsxflash.memory);
    parse_markup_map(m, node);
    mapping.append(m);
  }

  for(auto &node : root["mmio"]) {
    if(node.name != "map") continue;
    Mapping m({&BSXCartridge::mmio_read, &bsxcartridge}, {&BSXCartridge::mmio_write, &bsxcartridge});
    parse_markup_map(m, node);
    mapping.append(m);
  }

  for(auto &node : root["mcu"]) {
    if(node.name != "map") continue;
    Mapping m({&BSXCartridge::mcu_read, &bsxcartridge}, {&BSXCartridge::mcu_write, &bsxcartridge});
    parse_markup_map(m, node);
    mapping.append(m);
  }
}

void Cartridge::parse_markup_sufamiturbo(XML::Node &root) {
  if(root.exists() == false) return;
  has_st_slots = true;

  //load required slot A (will request slot B if slot A cartridge is linkable)
  interface->loadRequest(ID::SufamiTurboSlotA, "Sufami Turbo - Slot A", "st");

  for(auto &slot : root) {
    if(slot.name != "slot") continue;
    bool slotid = slot["id"].data == "A" ? 0 : slot["id"].data == "B" ? 1 : 0;
    for(auto &node : slot) {
      if(node.name == "rom") {
        for(auto &leaf : node) {
          if(leaf.name != "map") continue;
          SuperFamicom::Memory &memory = slotid == 0 ? sufamiturbo.slotA.rom : sufamiturbo.slotB.rom;
          if(memory.size() == 0) continue;

          Mapping m(memory);
          parse_markup_map(m, leaf);
          if(m.size == 0) m.size = memory.size();
          if(m.size) mapping.append(m);
        }
      }
      if(node.name == "ram") {
        unsigned ram_size = numeral(node["size"].data);
        for(auto &leaf : node) {
          if(leaf.name != "map") continue;
          SuperFamicom::Memory &memory = slotid == 0 ? sufamiturbo.slotA.ram : sufamiturbo.slotB.ram;
          if(memory.size() == 0) continue;

          Mapping m(memory);
          parse_markup_map(m, leaf);
          if(m.size == 0) m.size = memory.size();
          if(m.size) mapping.append(m);
        }
      }
    }
  }
}

void Cartridge::parse_markup_nss(XML::Node &root) {
  if(root.exists() == false) return;
  has_nss_dip = true;

  nss.dip = interface->dipSettings(root);
}

void Cartridge::parse_markup_sa1(XML::Node &root) {
  if(root.exists() == false) return;
  has_sa1 = true;

  auto &mcurom = root["mcu"]["rom"];
  auto &mcuram = root["mcu"]["ram"];
  auto &iram = root["iram"];
  auto &bwram = root["bwram"];
  auto &mmio = root["mmio"];

  for(auto &node : mcurom) {
    if(node.name != "map") continue;
    Mapping m({&SA1::mmc_read, &sa1}, {&SA1::mmc_write, &sa1});
    parse_markup_map(m, node);
    mapping.append(m);
  }

  for(auto &node : mcuram) {
    if(node.name != "map") continue;
    Mapping m({&SA1::mmc_cpu_read, &sa1}, {&SA1::mmc_cpu_write, &sa1});
    parse_markup_map(m, node);
    mapping.append(m);
  }

  for(auto &node : iram) {
    if(node.name != "map") continue;
    Mapping m(sa1.cpuiram);
    parse_markup_map(m, node);
    if(m.size == 0) m.size = 2048;
    mapping.append(m);
  }

  parse_markup_memory(ram, bwram, ID::RAM, true);
  for(auto &node : bwram) {
    if(node.name != "map") continue;
    Mapping m(sa1.cpubwram);
    parse_markup_map(m, node);
    if(m.size == 0) m.size = ram.size();
    mapping.append(m);
  }

  for(auto &node : mmio) {
    if(node.name != "map") continue;
    Mapping m({&SA1::mmio_read, &sa1}, {&SA1::mmio_write, &sa1});
    parse_markup_map(m, node);
    mapping.append(m);
  }
}

void Cartridge::parse_markup_superfx(XML::Node &root) {
  if(root.exists() == false) return;
  has_superfx = true;

  for(auto &node : root) {
    if(node.name == "rom") {
      parse_markup_memory(rom, node, ID::ROM, false);

      for(auto &leaf : node) {
        if(leaf.name != "map") continue;
        Mapping m(superfx.rom);
        parse_markup_map(m, leaf);
        mapping.append(m);
      }
    }
    if(node.name == "ram") {
      parse_markup_memory(ram, node, ID::RAM, false);

      for(auto &leaf : node) {
        if(leaf.name != "map") continue;
        Mapping m(superfx.ram);
        parse_markup_map(m, leaf);
        if(m.size == 0) m.size = ram.size();
        mapping.append(m);
      }
    }
    if(node.name == "mmio") {
      for(auto &leaf : node) {
        if(leaf.name != "map") continue;
        Mapping m({&SuperFX::mmio_read, &superfx}, {&SuperFX::mmio_write, &superfx});
        parse_markup_map(m, leaf);
        mapping.append(m);
      }
    }
  }
}

void Cartridge::parse_markup_armdsp(XML::Node &root) {
  if(root.exists() == false) return;
  has_armdsp = true;

  string firmware = root["firmware"].data;
  string sha256 = root["sha256"].data;

  interface->loadRequest(ID::ArmDSP, firmware);

  for(auto &node : root) {
    if(node.name != "map") continue;
    Mapping m({&ArmDSP::mmio_read, &armdsp}, {&ArmDSP::mmio_write, &armdsp});
    parse_markup_map(m, node);
    mapping.append(m);
  }
}

void Cartridge::parse_markup_hitachidsp(XML::Node &root) {
  if(root.exists() == false) return;
  has_hitachidsp = true;

  for(unsigned n = 0; n < 1024; n++) hitachidsp.dataROM[n] = 0x000000;

  hitachidsp.frequency = numeral(root["frequency"].data);
  if(hitachidsp.frequency == 0) hitachidsp.frequency = 20000000;
  string firmware = root["firmware"].data;
  string sha256 = root["sha256"].data;

  interface->loadRequest(ID::HitachiDSP, firmware);

  for(auto &node : root) {
    if(node.name == "rom") {
      for(auto &leaf : node) {
        if(leaf.name != "map") continue;
        Mapping m({&HitachiDSP::rom_read, &hitachidsp}, {&HitachiDSP::rom_write, &hitachidsp});
        parse_markup_map(m, leaf);
        mapping.append(m);
      }
    }
    if(node.name == "mmio") {
      for(auto &leaf : node) {
        Mapping m({&HitachiDSP::dsp_read, &hitachidsp}, {&HitachiDSP::dsp_write, &hitachidsp});
        parse_markup_map(m, leaf);
        mapping.append(m);
      }
    }
  }
}

void Cartridge::parse_markup_necdsp(XML::Node &root) {
  if(root.exists() == false) return;
  has_necdsp = true;

  for(unsigned n = 0; n < 16384; n++) necdsp.programROM[n] = 0x000000;
  for(unsigned n = 0; n <  2048; n++) necdsp.dataROM[n] = 0x0000;

  necdsp.frequency = numeral(root["frequency"].data);
  if(necdsp.frequency == 0) necdsp.frequency = 8000000;
  necdsp.revision
  = root["model"].data == "uPD7725"  ? NECDSP::Revision::uPD7725
  : root["model"].data == "uPD96050" ? NECDSP::Revision::uPD96050
  : NECDSP::Revision::uPD7725;
  string firmware = root["firmware"].data;
  string sha256 = root["sha256"].data;

  if(necdsp.revision == NECDSP::Revision::uPD7725) {
    interface->loadRequest(ID::Nec7725DSP, firmware);
  }

  if(necdsp.revision == NECDSP::Revision::uPD96050) {
    interface->loadRequest(ID::Nec96050DSP, firmware);
    string name = root["ram"]["name"].data;
    interface->loadRequest(ID::NecDSPRAM, name);
    memory.append({ID::NecDSPRAM, name});
  }

  for(auto &node : root) {
    if(node.name == "dr") {
      for(auto &leaf : node) {
        Mapping m({&NECDSP::dr_read, &necdsp}, {&NECDSP::dr_write, &necdsp});
        parse_markup_map(m, leaf);
        mapping.append(m);
      }
    }
    if(node.name == "sr") {
      for(auto &leaf : node) {
        Mapping m({&NECDSP::sr_read, &necdsp}, {&NECDSP::sr_write, &necdsp});
        parse_markup_map(m, leaf);
        mapping.append(m);
      }
    }
    if(node.name == "dp") {
      for(auto &leaf : node) {
        Mapping m({&NECDSP::dp_read, &necdsp}, {&NECDSP::dp_write, &necdsp});
        parse_markup_map(m, leaf);
        mapping.append(m);
      }
    }
  }
}

void Cartridge::parse_markup_epsonrtc(XML::Node &root) {
  if(root.exists() == false) return;
  has_epsonrtc = true;

  string name = root["name"].data;
  interface->loadRequest(ID::EpsonRTC, name);
  memory.append({ID::EpsonRTC, name});

  for(auto &node : root) {
    if(node.name != "map") continue;
    Mapping m({&EpsonRTC::read, &epsonrtc}, {&EpsonRTC::write, &epsonrtc});
    parse_markup_map(m, node);
    mapping.append(m);
  }
}

void Cartridge::parse_markup_sharprtc(XML::Node &root) {
  if(root.exists() == false) return;
  has_sharprtc = true;

  string name = root["name"].data;
  interface->loadRequest(ID::SharpRTC, name);
  memory.append({ID::SharpRTC, name});

  for(auto &node : root) {
    if(node.name != "map") continue;
    Mapping m({&SharpRTC::read, &sharprtc}, {&SharpRTC::write, &sharprtc});
    parse_markup_map(m, node);
    mapping.append(m);
  }
}

void Cartridge::parse_markup_spc7110(XML::Node &root) {
  if(root.exists() == false) return;
  has_spc7110 = true;

  auto &mmio = root["mmio"];
  auto &dcu = root["dcu"];
  auto &mcurom = root["mcu"]["rom"];
  auto &mcuram = root["mcu"]["ram"];

  spc7110.prom_base = numeral(mcurom["program"]["offset"].data);
  spc7110.prom_size = numeral(mcurom["program"]["size"].data);

  spc7110.drom_base = numeral(mcurom["data"]["offset"].data);
  spc7110.drom_size = numeral(mcurom["data"]["size"].data);

  for(auto &node : mmio) {
    if(node.name != "map") continue;
    Mapping m({&SPC7110::mmio_read, &spc7110}, {&SPC7110::mmio_write, &spc7110});
    parse_markup_map(m, node);
    mapping.append(m);
  }

  for(auto &node : dcu) {
    if(node.name != "map") continue;
    Mapping m({&SPC7110::dcu_read, &spc7110}, {&SPC7110::dcu_write, &spc7110});
    parse_markup_map(m, node);
    mapping.append(m);
  }

  for(auto &node : mcurom) {
    if(node.name != "map") continue;
    Mapping m({&SPC7110::mcurom_read, &spc7110}, {&SPC7110::mcurom_write, &spc7110});
    parse_markup_map(m, node);
    mapping.append(m);
  }

  for(auto &node : mcuram) {
    if(node.name != "map") continue;
    Mapping m({&SPC7110::mcuram_read, &spc7110}, {&SPC7110::mcuram_write, &spc7110});
    parse_markup_map(m, node);
    mapping.append(m);
  }
}

void Cartridge::parse_markup_sdd1(XML::Node &root) {
  if(root.exists() == false) return;
  has_sdd1 = true;

  for(auto &node : root["mmio"]) {
    if(node.name != "map") continue;
    Mapping m({&SDD1::mmio_read, &sdd1}, {&SDD1::mmio_write, &sdd1});
    parse_markup_map(m, node);
    mapping.append(m);
  }

  for(auto &node : root["mcu"]) {
    if(node.name != "map") continue;
    Mapping m({&SDD1::mcu_read, &sdd1}, {&SDD1::mcu_write, &sdd1});
    parse_markup_map(m, node);
    mapping.append(m);
  }
}

void Cartridge::parse_markup_obc1(XML::Node &root) {
  if(root.exists() == false) return;
  has_obc1 = true;

  for(auto &node : root) {
    if(node.name != "map") continue;
    Mapping m({&OBC1::read, &obc1}, {&OBC1::write, &obc1});
    parse_markup_map(m, node);
    mapping.append(m);
  }
}

void Cartridge::parse_markup_msu1(XML::Node &root) {
  if(root.exists() == false) return;
  has_msu1 = true;

  for(auto &node : root) {
    if(node.name != "map") continue;
    Mapping m({&MSU1::mmio_read, &msu1}, {&MSU1::mmio_write, &msu1});
    parse_markup_map(m, node);
    mapping.append(m);
  }
}

Cartridge::Mapping::Mapping() {
  mode = Bus::MapMode::Direct;
  banklo = bankhi = addrlo = addrhi = offset = size = 0;
}

Cartridge::Mapping::Mapping(SuperFamicom::Memory &memory) {
  read = {&SuperFamicom::Memory::read, &memory};
  write = {&SuperFamicom::Memory::write, &memory};
  mode = Bus::MapMode::Direct;
  banklo = bankhi = addrlo = addrhi = offset = size = 0;
}

Cartridge::Mapping::Mapping(const function<uint8 (unsigned)> &read_, const function<void (unsigned, uint8)> &write_) {
  read = read_;
  write = write_;
  mode = Bus::MapMode::Direct;
  banklo = bankhi = addrlo = addrhi = offset = size = 0;
}

#endif
