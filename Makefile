all: 
	$(MAKE) -C lib
	$(MAKE) -C ebin

clean:
	$(MAKE) -C lib clean
	$(MAKE) -C ebin clean
