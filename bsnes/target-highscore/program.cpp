#include <emulator/emulator.hpp>
#include <sfc/interface/interface.hpp>
#include <filter/filter.hpp>
#include <nall/directory.hpp>
#include <nall/instance.hpp>
#include <nall/decode/rle.hpp>
#include <nall/encode/rle.hpp>
#include <nall/hash/crc16.hpp>
using namespace nall;

#include <heuristics/heuristics.hpp>
#include <heuristics/heuristics.cpp>
#include <heuristics/super-famicom.cpp>
#include <heuristics/game-boy.cpp>
#include <heuristics/bs-memory.cpp>

#include "resources.hpp"

struct Program : Emulator::Platform
{
	Program(Emulator::Interface *emulator);
	~Program();

	auto open(uint id, string name, vfs::file::mode mode, bool required) -> shared_pointer<vfs::file> override;
	auto load(uint id, string name, string type, vector<string> options = {}) -> Emulator::Platform::Load override;
	auto videoFrame(const uint16* data, uint pitch, uint width, uint height, uint scale) -> void override;
	auto audioFrame(const double* samples, uint channels) -> void override;
	auto inputPoll(uint port, uint device, uint input) -> int16 override;
	auto inputRumble(uint port, uint device, uint input, bool enable) -> void override;

	auto load() -> void;
	auto loadFile(string location) -> vector<uint8_t>;
	auto loadSuperFamicom(string location) -> bool;
	auto loadGameBoy(string location) -> bool;
	auto loadBSMemory(string location) -> bool;

	auto save() -> void;

	auto openRomSuperFamicom(string name, vfs::file::mode mode) -> shared_pointer<vfs::file>;
	auto openRomGameBoy(string name, vfs::file::mode mode) -> shared_pointer<vfs::file>;
	auto openRomBSMemory(string name, vfs::file::mode mode) -> shared_pointer<vfs::file>;

	auto hackPatchMemory(vector<uint8_t>& data) -> void;
	auto updateVideoPalette() -> void;

	string base_name;

	bool overscan = false;

	Emulator::Interface *emulator;

public:	
	struct Game {
		explicit operator bool() const { return (bool)location; }
		
		string option;
		string location;
		string manifest;
		Markup::Node document;
		boolean patched;
		boolean verified;
	};

	struct SuperFamicom : Game {
		string title;
		string region;
		vector<uint8_t> program;
		vector<uint8_t> data;
		vector<uint8_t> expansion;
		vector<uint8_t> firmware;
	} superFamicom;

	struct GameBoy : Game {
		vector<uint8_t> program;
	} gameBoy;

	struct BSMemory : Game {
		vector<uint8_t> program;
	} bsMemory;

	int16_t audioOut[4000];
	uint audioLength;

	HsSoftwareContext *context;

	uint32_t palette[32768];
	Filter::Render filterRender;
	Filter::Size filterSize;

	HsSuperNesInputState inputState;

	char* saveDir;
};

Program::Program(Emulator::Interface *emulator)
{
	Emulator::platform = this;

	this->emulator = emulator;
}

Program::~Program()
{
	delete emulator;

	free (saveDir);
}

auto Program::save() -> void
{
	if(!emulator->loaded()) return;
	emulator->save();
}

auto Program::open(uint id, string name, vfs::file::mode mode, bool required) -> shared_pointer<vfs::file>
{
	shared_pointer<vfs::file> result;

	if (name == "ipl.rom" && mode == vfs::file::mode::read) {
		result = vfs::memory::file::open(iplrom, sizeof(iplrom));
	}

	if (name == "boards.bml" && mode == vfs::file::mode::read) {
		result = vfs::memory::file::open(Boards, sizeof(Boards));
	}

	if (id == 1) { //Super Famicom
		if (name == "manifest.bml" && mode == vfs::file::mode::read) {
			result = vfs::memory::file::open(superFamicom.manifest.data<uint8_t>(), superFamicom.manifest.size());
		}
		else if (name == "program.rom" && mode == vfs::file::mode::read) {
			result = vfs::memory::file::open(superFamicom.program.data(), superFamicom.program.size());
		}
		else if (name == "data.rom" && mode == vfs::file::mode::read) {
			result = vfs::memory::file::open(superFamicom.data.data(), superFamicom.data.size());
		}
		else if (name == "expansion.rom" && mode == vfs::file::mode::read) {
			result = vfs::memory::file::open(superFamicom.expansion.data(), superFamicom.expansion.size());
		}
		else {
			result = openRomSuperFamicom(name, mode);
		}
	}
	else if (id == 2) { //Game Boy
		if (name == "manifest.bml" && mode == vfs::file::mode::read) {
			result = vfs::memory::file::open(gameBoy.manifest.data<uint8_t>(), gameBoy.manifest.size());
		}
		else if (name == "program.rom" && mode == vfs::file::mode::read) {
			result = vfs::memory::file::open(gameBoy.program.data(), gameBoy.program.size());
		}
		else {
			result = openRomGameBoy(name, mode);
		}
	}
	else if (id == 3) {  //BS Memory
		if (name == "manifest.bml" && mode == vfs::file::mode::read) {
			result = vfs::memory::file::open(bsMemory.manifest.data<uint8_t>(), bsMemory.manifest.size());
		}
		else if (name == "program.rom" && mode == vfs::file::mode::read) {
			result = vfs::memory::file::open(bsMemory.program.data(), bsMemory.program.size());
		}
		else if(name == "program.flash") {
			//writes are not flushed to disk in bsnes
			result = vfs::memory::file::open(bsMemory.program.data(), bsMemory.program.size());
		}
		else {
			result = openRomBSMemory(name, mode);
		}
	}
	return result;
}

auto Program::updateVideoPalette() -> void {
  double luminance = 100.0 / 100.0;
  double saturation = 100.0 / 100.0;
  double gamma = 150.0 / 100.0;

  uint depth = 24;

  for(uint color : range(32768)) {
    uint16 r = (color >> 10) & 31;
    uint16 g = (color >>  5) & 31;
    uint16 b = (color >>  0) & 31;

    r = r << 3 | r >> 2; r = r << 8 | r << 0;
    g = g << 3 | g >> 2; g = g << 8 | g << 0;
    b = b << 3 | b >> 2; b = b << 8 | b << 0;

    if(saturation != 1.0) {
      uint16 grayscale = uclamp<16>((r + g + b) / 3);
      double inverse = max(0.0, 1.0 - saturation);
      r = uclamp<16>(r * saturation + grayscale * inverse);
      g = uclamp<16>(g * saturation + grayscale * inverse);
      b = uclamp<16>(b * saturation + grayscale * inverse);
    }

    if(gamma != 1.0) {
      double reciprocal = 1.0 / 32767.0;
      r = r > 32767 ? r : uint16(32767 * pow(r * reciprocal, gamma));
      g = g > 32767 ? g : uint16(32767 * pow(g * reciprocal, gamma));
      b = b > 32767 ? b : uint16(32767 * pow(b * reciprocal, gamma));
    }

    if(luminance != 1.0) {
      r = uclamp<16>(r * luminance);
      g = uclamp<16>(g * luminance);
      b = uclamp<16>(b * luminance);
    }

    switch(depth) {
    case 24: palette[color] = r >> 8 << 16 | g >> 8 <<  8 | b >> 8 << 0; break;
    case 30: palette[color] = r >> 6 << 20 | g >> 6 << 10 | b >> 6 << 0; break;
    }
  }
}

auto Program::load() -> void {
	emulator->unload();
	emulator->load();

	// per-game hack overrides
	auto title = superFamicom.title;
	auto region = superFamicom.region;

	//relies on mid-scanline rendering techniques
	if(title == "AIR STRIKE PATROL" || title == "DESERT FIGHTER") emulator->configure("Hacks/PPU/Fast", false);

	//the dialogue text is blurry due to an issue in the scanline-based renderer's color math support
	if(title == "マーヴェラス") emulator->configure("Hacks/PPU/Fast", false);

	//stage 2 uses pseudo-hires in a way that's not compatible with the scanline-based renderer
	if(title == "SFC クレヨンシンチャン") emulator->configure("Hacks/PPU/Fast", false);

	//title screen game select (after choosing a game) changes OAM tiledata address mid-frame
	//this is only supported by the cycle-based PPU renderer
	if(title == "Winter olympics") emulator->configure("Hacks/PPU/Fast", false);

	//title screen shows remnants of the flag after choosing a language with the scanline-based renderer
	if(title == "WORLD CUP STRIKER") emulator->configure("Hacks/PPU/Fast", false);

	//relies on cycle-accurate writes to the echo buffer
	if(title == "KOUSHIEN_2") emulator->configure("Hacks/DSP/Fast", false);

	//will hang immediately
	if(title == "RENDERING RANGER R2") emulator->configure("Hacks/DSP/Fast", false);

	//will hang sometimes in the "Bach in Time" stage
	if(title == "BUBSY II" && region == "PAL") emulator->configure("Hacks/DSP/Fast", false);

	//fixes an errant scanline on the title screen due to writing to PPU registers too late
	if(title == "ADVENTURES OF FRANKEN" && region == "PAL") emulator->configure("Hacks/PPU/RenderCycle", 32);

	//fixes an errant scanline on the title screen due to writing to PPU registers too late
	if(title == "FIREPOWER 2000" || title == "SUPER SWIV") emulator->configure("Hacks/PPU/RenderCycle", 32);

	//fixes an errant scanline on the title screen due to writing to PPU registers too late
	if(title == "NHL '94" || title == "NHL PROHOCKEY'94") emulator->configure("Hacks/PPU/RenderCycle", 32);

	//fixes an errant scanline on the title screen due to writing to PPU registers too late
	if(title == "Sugoro Quest++") emulator->configure("Hacks/PPU/RenderCycle", 128);

	if (emulator->configuration("Hacks/Hotfixes")) {
		//this game transfers uninitialized memory into video RAM: this can cause a row of invalid tiles
		//to appear in the background of stage 12. this one is a bug in the original game, so only enable
		//it if the hotfixes option has been enabled.
		if(title == "The Hurricanes") emulator->configure("Hacks/Entropy", "None");

		//Frisky Tom attract sequence sometimes hangs when WRAM is initialized to pseudo-random patterns
		if (title == "ニチブツ・アーケード・クラシックス") emulator->configure("Hacks/Entropy", "None");
	}

	emulator->power();
}

auto Program::load(uint id, string name, string type, vector<string> options) -> Emulator::Platform::Load {
	if (id == 1)
	{
		if (loadSuperFamicom(superFamicom.location))
		{
			return {id, superFamicom.region};
		}
	}
	else if (id == 2)
	{
		if (loadGameBoy(gameBoy.location))
		{
			return { id, "" };
		}
	}
	else if (id == 3) {
		if (loadBSMemory(bsMemory.location)) {
			return { id, "" };
		}
	}
	return { id, options(0) };
}

auto Program::videoFrame(const uint16* data, uint pitch, uint width, uint height, uint scale) -> void {
	if (!overscan)
	{
		uint multiplier = height / 240;
		data += 8 * (pitch >> 1) * multiplier;
		height -= 16 * multiplier;
	}

	uint filterWidth = width, filterHeight = height;

	filterSize(filterWidth, filterHeight);

	// Scale the NTSC filter properly for HD Mode 7
	if ((scale > 1) && (filterWidth == 602))
	{
		filterWidth = 301 * scale;
	}

	auto *fb = hs_software_context_get_framebuffer (context);
	filterRender(palette, (uint32*) fb, filterWidth << 2, (const uint16_t*)data, pitch, width, height);

	HsRectangle rect;
	hs_rectangle_init (&rect, 0, 0, filterWidth, filterHeight);
	hs_software_context_set_area (context, &rect);
	hs_software_context_set_row_stride (context, filterWidth << 2);
}

// Double the fun!
static int16_t d2i16(double v)
{
	v *= 0x8000;
	if (v > 0x7fff)
		v = 0x7fff;
	else if (v < -0x8000)
		v = -0x8000;
	return int16_t(floor(v + 0.5));
}

auto Program::audioFrame(const double* samples, uint channels) -> void
{
	assert (audioLength < 4000 - 2);

	audioOut[audioLength++] = d2i16(samples[0]);
	audioOut[audioLength++] = d2i16(samples[1]);
}

auto pollInputDevices(Program* self, uint port, uint device, uint input) -> int16
{
	unsigned hs_player;
	HsSuperNesButton hs_button;

	static const HsSuperNesButton button_mapping[] = {
		HS_SUPER_NES_BUTTON_UP,
		HS_SUPER_NES_BUTTON_DOWN,
		HS_SUPER_NES_BUTTON_LEFT,
		HS_SUPER_NES_BUTTON_RIGHT,
		HS_SUPER_NES_BUTTON_B,
		HS_SUPER_NES_BUTTON_A,
		HS_SUPER_NES_BUTTON_Y,
		HS_SUPER_NES_BUTTON_X,
		HS_SUPER_NES_BUTTON_L,
		HS_SUPER_NES_BUTTON_R,
		HS_SUPER_NES_BUTTON_SELECT,
		HS_SUPER_NES_BUTTON_START,
	};

	switch (port)
	{
		case SuperFamicom::ID::Port::Controller1:
			hs_player = 0;
			break;
		case SuperFamicom::ID::Port::Controller2:
			hs_player = 1;
			break;
		default:
			return 0;
	}

	switch (device)
	{
		case SuperFamicom::ID::Device::Gamepad:
			hs_button = button_mapping[input];
			break;

		case SuperFamicom::ID::Device::Mouse:
			return 0; // TODO

		case SuperFamicom::ID::Device::SuperMultitap:
			return 0; // TODO

		// TODO: SuperScope/Justifiers.
		// Do we care? The v94 port hasn't hooked them up. :)

		default:
			return 0;
	}

	return (self->inputState.pad_buttons[hs_player] & 1 << hs_button) > 0;
}

auto Program::inputPoll(uint port, uint device, uint input) -> int16
{
	return pollInputDevices(this, port, device, input);
}

auto Program::inputRumble(uint port, uint device, uint input, bool enable) -> void
{
}

auto Program::openRomSuperFamicom(string name, vfs::file::mode mode) -> shared_pointer<vfs::file>
{
	if(name == "program.rom" && mode == vfs::file::mode::read)
	{
		return vfs::memory::file::open(superFamicom.program.data(), superFamicom.program.size());
	}

	if(name == "data.rom" && mode == vfs::file::mode::read)
	{
		return vfs::memory::file::open(superFamicom.data.data(), superFamicom.data.size());
	}

	if(name == "expansion.rom" && mode == vfs::file::mode::read)
	{
		return vfs::memory::file::open(superFamicom.expansion.data(), superFamicom.expansion.size());
	}

	if(name == "msu1/data.rom")
	{
		return vfs::fs::file::open({Location::notsuffix(superFamicom.location), ".msu"}, mode);
	}

	if(name.match("msu1/track*.pcm"))
	{
		name.trimLeft("msu1/track", 1L);
		return vfs::fs::file::open({Location::notsuffix(superFamicom.location), name}, mode);
	}

	if(name == "save.ram")
	{
		string save_path = string(saveDir).transform("\\", "/");

		return vfs::fs::file::open(save_path, mode);
	}

	return {};
}

auto Program::openRomGameBoy(string name, vfs::file::mode mode) -> shared_pointer<vfs::file> {
	g_assert_not_reached(); // We don't support SGB atm
}

auto Program::openRomBSMemory(string name, vfs::file::mode mode) -> shared_pointer<vfs::file> {
	if (name == "program.rom" && mode == vfs::file::mode::read)
	{
		return vfs::memory::file::open(bsMemory.program.data(), bsMemory.program.size());
	}

	if (name == "program.flash")
	{
		//writes are not flushed to disk
		return vfs::memory::file::open(bsMemory.program.data(), bsMemory.program.size());
	}

	return {};
}

auto Program::loadFile(string location) -> vector<uint8_t>
{
	return file::read(location);
}

auto Program::loadSuperFamicom(string location) -> bool
{
	vector<uint8_t> rom;
	rom = loadFile(location);

	if(rom.size() < 0x8000) return false;

	//assume ROM and IPS agree on whether a copier header is present
	//superFamicom.patched = applyPatchIPS(rom, location);
	if((rom.size() & 0x7fff) == 512) {
		//remove copier header
		memory::move(&rom[0], &rom[512], rom.size() - 512);
		rom.resize(rom.size() - 512);
	}

	auto heuristics = Heuristics::SuperFamicom(rom, location);
	auto sha256 = Hash::SHA256(rom).digest();

	superFamicom.title = heuristics.title();
	superFamicom.region = heuristics.videoRegion();
	superFamicom.manifest = heuristics.manifest();

	hackPatchMemory(rom);
	superFamicom.document = BML::unserialize(superFamicom.manifest);
	superFamicom.location = location;

	uint offset = 0;
	if(auto size = heuristics.programRomSize()) {
		superFamicom.program.resize(size);
		memory::copy(&superFamicom.program[0], &rom[offset], size);
		offset += size;
	}
	if(auto size = heuristics.dataRomSize()) {
		superFamicom.data.resize(size);
		memory::copy(&superFamicom.data[0], &rom[offset], size);
		offset += size;
	}
	if(auto size = heuristics.expansionRomSize()) {
		superFamicom.expansion.resize(size);
		memory::copy(&superFamicom.expansion[0], &rom[offset], size);
		offset += size;
	}
	if(auto size = heuristics.firmwareRomSize()) {
		superFamicom.firmware.resize(size);
		memory::copy(&superFamicom.firmware[0], &rom[offset], size);
		offset += size;
	}
	return true;
}

auto Program::loadGameBoy(string location) -> bool {
	vector<uint8_t> rom;
	rom = loadFile(location);

	if (rom.size() < 0x4000) return false;

	auto heuristics = Heuristics::GameBoy(rom, location);
	auto sha256 = Hash::SHA256(rom).digest();

	gameBoy.manifest = heuristics.manifest();
	gameBoy.document = BML::unserialize(gameBoy.manifest);
	gameBoy.location = location;
	gameBoy.program = rom;

	return true;
}

auto Program::loadBSMemory(string location) -> bool {
	vector<uint8_t> rom;
	rom = loadFile(location);

	if (rom.size() < 0x8000) return false;

	auto heuristics = Heuristics::BSMemory(rom, location);
	auto sha256 = Hash::SHA256(rom).digest();

	bsMemory.manifest = heuristics.manifest();
	bsMemory.document = BML::unserialize(bsMemory.manifest);
	bsMemory.location = location;

	bsMemory.program = rom;
	return true;
}

auto Program::hackPatchMemory(vector<uint8_t>& data) -> void
{
	auto title = superFamicom.title;

	if(title == "Satellaview BS-X" && data.size() >= 0x100000) {
		//BS-X: Sore wa Namae o Nusumareta Machi no Monogatari (JPN) (1.1)
		//disable limited play check for BS Memory flash cartridges
		//benefit: allow locked out BS Memory flash games to play without manual header patching
		//detriment: BS Memory ROM cartridges will cause the game to hang in the load menu
		if(data[0x4a9b] == 0x10) data[0x4a9b] = 0x80;
		if(data[0x4d6d] == 0x10) data[0x4d6d] = 0x80;
		if(data[0x4ded] == 0x10) data[0x4ded] = 0x80;
		if(data[0x4e9a] == 0x10) data[0x4e9a] = 0x80;
	}
}
