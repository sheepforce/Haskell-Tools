# the installation prefix where it will be installed
PREFIX		= ~/.local/bin

# the Haskell compiler
HC		= ghc

# the Haskell compiler flags
HFLAGS		= -threaded
HINCLUDE	= ../modules


# building routines
.PHONY: all
all: ch_geomana ch_geomalign ch_geom2molc ch_geomcut ch_incidems ch_hessconv

ch_geomana:
	cd ch_geomana && $(HC) -i$(HINCLUDE) --make $(HFLAGS) Ch_GeomAna

ch_geomalign:
	cd ch_geomalign && $(HC) -i$(HINCLUDE) --make $(HFLAGS) Ch_GeomAlign

ch_geom2molc:
	cd ch_geom2molc && $(HC) -i$(HINCLUDE) --make $(HFLAGS) Ch_Geom2Molc

ch_geomcut:
	cd ch_geomcut && $(HC) -i$(HINCLUDE) --make $(HFLAGS) Ch_GeomCut

ch_incidems:
	cd ch_incide-ms && $(HC) -i$(HINCLUDE) --make $(HFLAGS) Ch_Incide_ms

ch_hessconv:
	cd ch_hessconv && $(HC) -i$(HINCLUDE) --make $(HFLAGS) Ch_HessConv

# cleaning up the directories
clean: clean_ch_geomana clean_ch_geomalign clean_ch_geom2molc clean_ch_geomcut clean_ch_incidems clean_ch_hessconv clean_modules

clean_ch_geomana:
	cd ch_geomana && rm -f Ch_GeomAna.hi Ch_GeomAna.o Ch_GeomAna_Opts.o Ch_GeomAna_Opts.hi Ch_GeomAna

clean_ch_geomalign:
	cd ch_geomalign && rm -f Ch_GeomAlign.hi Ch_GeomAlign.o Ch_GeomAlign_Opts.o Ch_GeomAlign_Opts.hi Ch_GeomAlign

clean_ch_geom2molc:
	cd ch_geom2molc && rm -f Ch_Geom2Molc.hi Ch_Geom2Molc.o Ch_Geom2Molc_Opts.o Ch_Geom2Molc_Opts.hi Ch_Geom2Molc

clean_ch_geomcut:
	cd ch_geomcut && rm -f Ch_GeomCut.hi Ch_GeomCut.o Ch_GeomCut_Opts.o Ch_GeomCut_Opts.hi Ch_GeomCut

clean_ch_incidems:
	cd ch_incide-ms && rm -f Ch_Incide_ms.hi Ch_Incide_ms.o Ch_Incide_ms_Opts.hi Ch_Incide_ms_Opts.o Ch_Incide_ms

clean_ch_hessconv:
	cd ch_hessconv && rm -f Ch_HessConv.hi Ch_HessConv.o Ch_HessConv.hi Ch_HessConv.o Ch_HessConv

clean_modules:
	cd modules && rm -f Algebra.o Algebra.hi
	cd modules/Chemistry && rm -f XYZ.o XYZ.hi


# install the executables
install: install_clean_ch_geomana install_ch_geomalign install_ch_geom2molc install_ch_geomcut install_ch_incidems

install_clean_ch_geomana:
	cp ch_geomana/Ch_GeomAna $(PREFIX)/ch_geomana

install_ch_geomalign:
	cp ch_geomalign/Ch_GeomAlign $(PREFIX)/ch_geomalign

install_ch_geom2molc:
	cp ch_geom2molc/Ch_Geom2Molc $(PREFIX)/ch_geom2molc

install_ch_geomcut:
	cp ch_geomcut/Ch_GeomCut $(PREFIX)/ch_geomcut

install_ch_incidems:
	cp ch_incide-ms/Ch_Incide_ms $(PREFIX)/ch_incide-ms
