/*****************************************************************************/
/*   Ethernet raw access driver                                              */
/*                                                                           */
/* erlang-tcpip, Copyright (C) 2004 Javier Paris                             */
/*                                                                           */
/* Licensed under the Apache License, Version 2.0 (the "License");           */
/* you may not use this file except in compliance with the License.          */
/* You may obtain a copy of the License at                                   */
/*                                                                           */
/* http://www.apache.org/licenses/LICENSE-2.0                                */
/*                                                                           */
/* Unless required by applicable law or agreed to in writing, software       */
/* distributed under the License is distributed on an "AS IS" BASIS,         */
/* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  */
/* See the License for the specific language governing permissions and       */
/* limitations under the License.                                            */
/*                                                                           */
/*****************************************************************************/

#include <errno.h>
#include <sys/ioctl.h>
#include <net/if.h>
#include <netinet/in.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <net/ethernet.h>
#include <linux/if_packet.h>
#include <sys/select.h>
#include <pthread.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <erl_driver.h>

#define BUFSIZE 65536

struct eth_data {
  ErlDrvPort port;
  int socket;
  char *iface;
};

static ErlDrvData eth_start(ErlDrvPort port, char *buff);
static void eth_stop(ErlDrvData drv_data);
static void eth_input(ErlDrvData drv_data, ErlDrvEvent event);
static int eth_control(ErlDrvData drv_data, unsigned int command,
		       char *buf, int len, char **rbuf, int rlen);
static void eth_outputv(ErlDrvData drv_data, ErlIOVec *ev);


static ErlDrvEntry eth_driver_entry = {
  NULL,                  /* init, N/A */
  eth_start,             /* start, called when port is opened */
  eth_stop,              /* stop, called when port is closed */
  NULL,                  /* output, called when erlang has sent */
  eth_input,             /* ready_input, called when input descriptor 
			    ready */
  NULL,                  /* ready_output, called when output 
			    descriptor ready */
  "eth_driver",          /* char *driver_name, the argument 
			    to open_port */
  NULL,                  /* finish, called when unloaded */
  NULL,                  /* void * that is not used (BC) */
  eth_control,           /* control, port_control callback */
  NULL,                  /* timeout, called on timeouts */
  eth_outputv,           /* outputv, vector output interface */
  NULL,					 /* Ready Async */
  NULL, 				 /* flush */
  NULL,					 /* call */
  NULL, 				 /* event */
  ERL_DRV_EXTENDED_MARKER,
  ERL_DRV_EXTENDED_MAJOR_VERSION,
  ERL_DRV_EXTENDED_MINOR_VERSION,
  0,
  NULL,
  NULL,
  NULL,
  NULL
};


/*
  Reverses integer endianess
*/
int reverse_endian(int size)
{
  union {
    int reversed;
    char byte[4];
  } rev;
  
  rev.byte[0] = (size >> 24) & 0xff;
  rev.byte[1] = (size >> 16) & 0xff;
  rev.byte[2] = (size >> 8)  & 0xff;
  rev.byte[3] = size & 0xff;
  
  return rev.reversed;
}


/*
 Opens a raw ethernet socket
*/

int open_raw_socket()
{
  int sock;
  
  if((sock = socket(PF_PACKET, SOCK_RAW, htons(ETH_P_ALL))) < 0) {
    fprintf(stderr, "Error opening socket:%s\n",strerror(errno));
    exit(0);
  }
  
  if(fcntl(sock, F_SETFL, O_NONBLOCK) < 0) {
    fprintf(stderr, "Error setting non blocking operation\n");
    exit(0);
  }

  return sock;
}

/*
 Get the kernel index for a network interface
*/

int get_ifindex(int socket, char *ifname)
{
  struct ifreq ifr;
  
  strncpy(ifr.ifr_name, ifname, IFNAMSIZ);
  if(ioctl(socket, SIOCGIFINDEX, &ifr) < 0) {
    fprintf(stderr, "Error getting interface index for %s:%s\n",ifname, strerror(errno));
    exit(0);
  }
  
  return ifr.ifr_ifindex;
}

/*
 Bind the socket to a network interface so that it doesn't receive packets
 from any other.
*/

int bind_to_interface(int socket, char *ifname)
{
  int ifindex;
  struct sockaddr_ll options;
  
  ifindex = get_ifindex(socket, ifname);
  
  options.sll_family   = AF_PACKET;
  options.sll_protocol = htons(ETH_P_ALL);
  options.sll_ifindex  = ifindex;
  
  if(bind(socket, (struct sockaddr *)&options, sizeof(struct sockaddr_ll))<0) {
    fprintf(stderr, "Error binding socket:%s\n",strerror(errno));
    exit(0);
  }
  
  return 0;
}

/*
 Loop reading packets from socket
*/
static void eth_input(ErlDrvData drv_data, ErlDrvEvent event)
{
  int size; 
  ErlDrvBinary *buffer;
  struct eth_data *drv= (struct eth_data *) drv_data;
  
  while((buffer=driver_alloc_binary(BUFSIZE),
         size = recv(drv->socket, buffer->orig_bytes, BUFSIZE, 0)) > 0) {
    driver_output_binary(drv->port, NULL, 0, buffer, 0, size);
    driver_free_binary(buffer);
  }

  // In the last iteration of the while an alloc is made, but
  //no driver_free is done, so it must be done here
  driver_free_binary(buffer);

  if(errno != EAGAIN) {
    fprintf(stderr, "Error reading packet:%s\n", strerror(errno));
    exit(0);
  }
}

static void eth_input2(ErlDrvData drv_data, ErlDrvEvent event)
{
  int size; 
  char buffer[BUFSIZE];
  struct eth_data *drv= (struct eth_data *) drv_data;
  
  while((size = recv(drv->socket, buffer, BUFSIZE, 0)) > 0) {
    driver_output(drv->port, buffer, size);
  }
  if(errno != EAGAIN) {
    fprintf(stderr, "Error reading packet:%s\n", strerror(errno));
    exit(0);
  }
}


static void eth_outputv(ErlDrvData drv_data, ErlIOVec *ev)
{
  struct eth_data *drv = (struct eth_data *) drv_data;

  writev(drv->socket, (struct iovec *) ev->iov, ev->vsize);
}

int enter_promiscuous_mode(int socket, char *ifname)
{
  struct ifreq ifr;
  
  strncpy(ifr.ifr_name, ifname, IFNAMSIZ);
  if(ioctl(socket, SIOCGIFFLAGS, &ifr) < 0) {
    fprintf(stderr, "Error getting interface flags:%s\n",strerror(errno));
    exit(0);
  }

  ifr.ifr_flags |= IFF_PROMISC;
  
  if(ioctl(socket, SIOCSIFFLAGS, &ifr) < 0) {
    fprintf(stderr, "Error setting interface flags:%s\n",strerror(errno));
    exit(0);
  }

  return 0;
}

/*
  Exits promiscuous mode in the interface
*/
int exit_promiscuous_mode(int socket, char *ifname)
{
  struct ifreq ifr;
  
  strncpy(ifr.ifr_name, ifname, IFNAMSIZ);
  if(ioctl(socket, SIOCGIFFLAGS, &ifr) < 0) {
    fprintf(stderr, "Error exitting promiscuos mode:%s\n",strerror(errno));
    exit(0);
  }
  
  ifr.ifr_flags ^= (ifr.ifr_flags & IFF_PROMISC);
  
  if(ioctl(socket, SIOCSIFFLAGS, &ifr) < 0) {
    fprintf(stderr, "Error exitting promiscuous mode:%s\n",strerror(errno));
    exit(0);
  }

  return 0;
}

DRIVER_INIT(eth_driver)
{
  return &eth_driver_entry;
}

static ErlDrvData eth_start(ErlDrvPort port, char *buff)
{
  struct eth_data *drv = malloc(sizeof(struct eth_data));
  drv->port = port;
  drv->socket = -1;
  drv->iface = NULL;
  return (ErlDrvData) drv;
}

static void eth_stop(ErlDrvData drv_data)
{
  struct eth_data *drv = (struct eth_data *) drv_data;

  driver_select(drv->port, (ErlDrvEvent) drv->socket, DO_READ, 0);
  exit_promiscuous_mode(drv->socket, drv->iface);
  free(drv->iface);
  close(drv->socket);
  free((ErlDrvPort *) drv_data);
}

static struct tpacket_stats get_packet_stats(int sock)
{
    struct tpacket_stats st;
    unsigned size=sizeof(struct tpacket_stats);

    if(getsockopt(sock, SOL_PACKET, PACKET_STATISTICS, &st, &size)) {
	st.tp_packets=-1;
        st.tp_drops=-1;
    }

    return st;
}

static int get_iface_mtu(int sock, char *iface) 
{
    struct ifreq ifr;
  
    strncpy(ifr.ifr_name, iface, IFNAMSIZ);
    if(ioctl(sock, SIOCGIFMTU, &ifr) < 0) {
      fprintf(stderr, "Error getting interface mtu:%s\n",strerror(errno));
      exit(0);
    }

    return ifr.ifr_mtu;
}

static int eth_control(ErlDrvData drv_data, unsigned int command,
		       char *buf, int len, char **rbuf, int rlen)
{
    struct eth_data *drv = (struct eth_data *) drv_data;
    int sock;
    char *ifname;
    struct tpacket_stats st;

    switch(command) {
    case 0: // specify the interface to bind
	ifname = malloc(len+1);

	strncpy(ifname, buf, len);
	ifname[len] = '\0';

	sock = open_raw_socket();
	bind_to_interface(sock, ifname);
	enter_promiscuous_mode(sock, ifname);

	driver_select(drv->port, (ErlDrvEvent) sock, DO_READ, 1);
	drv->socket = sock;
	drv->iface = ifname;

	return 0;
	break;
    case 1: // get packets stats
	st = get_packet_stats(drv->socket);
	if(rlen < 2*(sizeof(int))) {
            *rbuf=driver_realloc(*rbuf, sizeof(int));
	}
	((int *) (*rbuf))[0] = st.tp_packets;
        ((int *) (*rbuf))[1] = st.tp_drops;

        return 2*sizeof(int);
        break;
    case 2: { // get iface MTU
        int mtu;
        mtu = get_iface_mtu(drv->socket, drv->iface);
	
	if(rlen < (sizeof(int))) {
	  *rbuf = driver_realloc(*rbuf, sizeof(int));
	}
	((int *) (*rbuf))[0] = mtu;
	
	return sizeof(int);
	break;
	}
    }
    return -1;
}
