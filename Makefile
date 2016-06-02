all: ch_geomana ch_geomalign

install: ch_geomana ch_geomalign
	cd ch_geomana && $(MAKE) install
	cd ch_geomalign && $(MAKE) install

ch_geomana:
	cd ch_geomana && $(MAKE) all

ch_geomalign:
	cd ch_geomalign && $(MAKE) all
 
clean:
	cd ch_geomana && $(MAKE) clean
	cd ch_geomalign &&  $(MAKE) clean
	cd modules && rm -rf *.hi *.o
	cd modules/Chemistry && rm -rf *.hi *.o