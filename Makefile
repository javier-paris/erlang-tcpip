all: 
	$(MAKE) -C c_src
	$(MAKE) -C src

clean:
	$(MAKE) -C c_src clean
	$(MAKE) -C src clean
