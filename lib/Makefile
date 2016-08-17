all: eth_driver.so checksum.so

checksum.so: ../c_src/checksum.c
	gcc -o checksum.so -Wall -I/usr/lib/erlang/usr/include -fpic -shared ../c_src/checksum.c

eth_driver.so: ../c_src/eth_driver.c
	gcc -o eth_driver.so -Wall -I/usr/lib/erlang/usr/include -fpic -shared ../c_src/eth_driver.c

clean:
	rm -f *~ eth_driver.so checksum.so *.o
