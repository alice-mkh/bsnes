name := bsnes-highscore
local := false
openmp := true

flags += -Wno-attributes -Wno-narrowing -Wno-multichar -g -fPIC -fvisibility=hidden

version := 115
commit := $(shell git describe --always --dirty --match '')

options += $(shell pkg-config --libs glib-2.0 highscore-1)
flags += $(shell pkg-config --cflags glib-2.0 highscore-1)

objects := bsnes-highscore $(objects)
objects := $(patsubst %,obj/%.o,$(objects))

obj/bsnes-highscore.o: target-highscore/bsnes-highscore.cpp

out/bsnes.highscore: target-highscore/bsnes.highscore.in
	$(shell cat $< | sed -E "s/@version@/$(version)/" | sed -E "s/@commit@/$(commit)/" > $@)

all: $(objects)
	$(strip $(compiler) -o out/$(name).so -shared $(objects) -Wl,--no-undefined -lgomp -Wl,-Bdynamic $(options))

core_install_dir := $(libdir)/highscore/cores/bsnes

install: all out/bsnes.highscore
	mkdir -p $(core_install_dir)/
	cp out/$(name).so $(core_install_dir)/$(name).so
	cp out/bsnes.highscore $(core_install_dir)/bsnes.highscore
