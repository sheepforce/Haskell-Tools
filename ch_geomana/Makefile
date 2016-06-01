# the installation prefix where it will be installed
PREFIX		= ~/.local/bin

# the Haskell compiler
HC		= ghc

# the Haskell compiler flags
HFLAGS		= -threaded

HINCLUDE	= ../modules

all: Ch_GeomAna

Ch_GeomAna:
	$(HC) -i$(HINCLUDE) --make $(HFLAGS) Ch_GeomAna

clean: 
	rm -f Ch_GeomAna.hi Ch_GeomAna.o
	rm -f Ch_GeomAna_Opts.o Ch_GeomAna_Opts.hi
	rm -f Ch_GeomAna

install: all
	cp Ch_GeomAna $(PREFIX)/ch_geomana
