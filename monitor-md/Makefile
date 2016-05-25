# the installation prefix where it will be installed

# the Haskell compiler
HC		= ghc

# the Haskell compiler flags
HFLAGS		= -threaded

all: Monitor_MD

Monitor_MD:
	$(HC) --make $(HFLAGS) Monitor_MD

clean: 
	rm -f Monitor_MD.hi Monitor_MD.o
	rm -f Algebra.hi Algebra.o
	rm -f Chemistry/XYZ.hi Chemistry/XYZ.o
	rm -f MonitorOpt.o MonitorOpt.hi
	rm -f Monitor_MD

install: all
	cp Monitod_MD
