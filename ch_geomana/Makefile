# the installation prefix where it will be installed
PREFIX		= ~/.local/bin

# the Haskell compiler
HC		= ghc

# the Haskell compiler flags
HFLAGS		= -threaded

HINCLUDE	= ../modules

all: Monitor_MD

Monitor_MD:
	$(HC) -i$(HINCLUDE) --make $(HFLAGS) Ch_GeoAna

clean: 
	rm -f Ch_GeoAna.hi Ch_GeoAna.o
	rm -f Ch_GeoAna_Opts.o Ch_GeoAna_Opts.hi
	rm -f Ch_GeoAna

install: all
	cp Ch_GeoAna $(PREFIX)/ch_geoana
