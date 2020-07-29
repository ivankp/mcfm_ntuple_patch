ROOT_CPPFLAGS := $(shell root-config --cflags)
ROOT_LDFLAGS  := $(shell root-config --ldflags) \
  -Wl,-rpath,$(shell root-config --libdir)
ROOT_LDLIBS   := $(shell root-config --libs)

all: root_ntuples.so

clean:
	@rm -fv ntuple_interface_dl.o root_ntuples.so

%.o: %.c
	gcc -Wall -O3 -c $< -o $@

%.o: %.f
	gfortran -Wall -O3 -c $< -o $@

root_ntuples.so: %.so: %.cc
	g++ -Wall -O3 -fPIC -shared $(ROOT_CPPFLAGS) $(ROOT_LDFLAGS) $< -o $@ $(ROOT_LDLIBS)
