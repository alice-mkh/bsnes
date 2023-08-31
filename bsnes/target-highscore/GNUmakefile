name := bsnes-highscore
local := false
openmp := true
flags += -Wno-narrowing -Wno-multichar -g -fPIC -fvisibility=hidden

options += $(shell pkg-config --libs glib-2.0 highscore-1)
flags += $(shell pkg-config --cflags glib-2.0 highscore-1)

objects := bsnes-highscore $(objects)
objects := $(patsubst %,obj/%.o,$(objects))

obj/bsnes-highscore.o: target-highscore/bsnes-highscore.cpp

all: $(objects)
	$(strip $(compiler) -o out/$(name).so -shared $(objects) -Wl,--no-undefined -lgomp -Wl,-Bdynamic $(options))

install: all
	mkdir -p $(libdir)/highscore/cores/
	cp out/$(name).so $(libdir)/highscore/cores/$(name).so
	cp target-highscore/bsnes.highscore $(libdir)/highscore/cores/bsnes.highscore
