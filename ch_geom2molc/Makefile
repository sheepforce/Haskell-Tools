# the installation prefix where it will be installed
PREFIX		= ~/.local/bin

# the Haskell compiler
HC		= ghc

# the Haskell compiler flags
HFLAGS		= -threaded

HINCLUDE	= ../modules

all: Ch_Geom2Molc

Ch_Geom2Molc:
	$(HC) -i$(HINCLUDE) --make $(HFLAGS) Ch_Geom2Molc

clean: 
	rm -f Ch_Geom2Molc.hi Ch_Geom2Molc.o
	rm -f Ch_Geom2Molc_Opts.o Ch_Geom2Molc_Opts.hi
	rm -f Ch_Geom2Molc

install: all
	cp Ch_Geom2Molc $(PREFIX)/ch_geom2molc
