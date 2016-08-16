all: 
	erl -make

eqc:
	erl -noshell -eval "make:all([{parse_transform, eqc_cover}])" -s init stop

clean:
	rm -f *.beam *~
