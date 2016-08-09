all: eth_port.beam eth.beam arp.beam ip.beam icmp.beam checksum.beam udp.beam fragment.beam packet_check.beam iss.beam tcp_con.beam tcp_pool.beam tcp.beam tcb.beam tcp_packet.beam closed.beam seq.beam syn_sent.beam syn_rcvd.beam established.beam listen.beam tcp_input.beam close_wait.beam last_ack.beam fin_wait_1.beam fin_wait_2.beam closing.beam time_wait.beam socket.beam out_order.beam congestion.beam rtt.beam

%.beam: %.erl
	erlc +debug_info -W $<

clean:
	rm -f *.beam *~
