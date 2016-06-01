all: ch_geomana

ch_geomana:
	$(MAKE) -B all -C ch_geomana
 
clean:
	$(MAKE) clean -C ch_geomana
	cd modules; rm -rf *.hi *.o
	cd modules/Chemistry; rm -rf *.hi *.o