# the installation prefix where it will be installed
PREFIX		= ~/.local/bin

# the Haskell compiler
HC		= ghc

# the Haskell compiler flags
HFLAGS		= -threaded

all: Monitor_MD

Monitor_MD:
	$(HC) --make $(HFLAGS) AnaGeom_xyz

clean: 
	rm -f AnaGeom_xyz.hi AnaGeom_xyz.o
	rm -f Algebra.hi Algebra.o
	rm -f Chemistry/XYZ.hi Chemistry/XYZ.o
	rm -f AnaGeom_xyzOpts.o AnaGeom_xyzOpts.hi
	rm -f AnaGeom_xyz

install: all
	cp AnaGeom_xyz $(PREFIX)/.
