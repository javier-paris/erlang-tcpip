all: eth_driver.so checksum.so

checksum.so: checksum.c
	gcc -o checksum.so -Wall -I/usr/lib/erlang/usr/include -fpic -shared checksum.c

eth_driver.so: eth_driver.c
	gcc -o eth_driver.so -Wall -I/usr/lib/erlang/usr/include -fpic -shared eth_driver.c

clean:
	rm -f *~ eth_driver.so checksum.so *.o
