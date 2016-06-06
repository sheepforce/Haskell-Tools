# the installation prefix where it will be installed
PREFIX		= ~/.local/bin

# the Haskell compiler
HC		= ghc

# the Haskell compiler flags
HFLAGS		= -threaded
HINCLUDE	= ../modules


# building routines
.PHONY: all
all: ch_geomana ch_geomalign ch_geom2molc

ch_geomana:
	cd ch_geomana ; $(HC) -i$(HINCLUDE) --make $(HFLAGS) Ch_GeomAna

ch_geomalign:
	cd ch_geomalign ; $(HC) -i$(HINCLUDE) --make $(HFLAGS) Ch_GeomAlign

ch_geom2molc:
	cd ch_geom2molc ; $(HC) -i$(HINCLUDE) --make $(HFLAGS) Ch_Geom2Molc

# cleaning up the directories
clean: clean_ch_geomana clean_ch_geomalign clean_ch_geom2molc clean_modules

clean_ch_geomana:
	cd ch_geomana && rm -f Ch_GeomAna.hi Ch_GeomAna.o Ch_GeomAna_Opts.o Ch_GeomAna_Opts.hi Ch_GeomAna

clean_ch_geomalign:
	cd ch_geomalign && rm -f Ch_GeomAlign.hi Ch_GeomAlign.o Ch_GeomAlign_Opts.o Ch_GeomAlign_Opts.hi Ch_GeomAlign

clean_ch_geom2molc:
	cd ch_geom2molc && rm -f Ch_Geom2Molc.hi Ch_Geom2Molc.o Ch_Geom2Molc_Opts.o Ch_Geom2Molc_Opts.hi Ch_Geom2Molc

clean_modules:
	cd modules && rm -f Algebra.o Algebra.hi
	cd modules/Chemistry && rm -f XYZ.o XYZ.hi


# install the executables
install: install_clean_ch_geomana install_ch_geomalign install_ch_geom2molc

install_clean_ch_geomana:
	cp ch_geomana/Ch_GeomAna $(PREFIX)/ch_geomana

install_ch_geomalign:
	cp ch_geomalign/Ch_GeomAlign $(PREFIX)/ch_geomalign

install_ch_geom2molc:
	cp ch_geom2molc/Ch_Geom2Molc $(PREFIX)/ch_geom2molc