# the installation prefix where it will be installed
PREFIX		= ~/.local/bin

# the Haskell compiler
HC		= ghc

# the Haskell compiler flags
HFLAGS		= -threaded

HINCLUDE	= ../modules

all: Monitor_MD

Monitor_MD:
	$(HC) -i$(HINCLUDE) --make $(HFLAGS) Ch_GeomAlign

clean: 
	rm -f Ch_GeomAlign.hi Ch_GeomAlign.o
	rm -f Ch_GeomAlign_Opts.o Ch_GeomAlign_Opts.hi
	rm -f Ch_GeomAlign

install: all
	cp Ch_GeomAlign $(PREFIX)/ch_geomalign
