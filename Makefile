all: 
	$(MAKE) -C lib
	$(MAKE) -C ebin

eqc:
	$(MAKE) -C lib 
	$(MAKE) -C ebin eqc

clean:
	$(MAKE) -C lib clean
	$(MAKE) -C ebin clean
