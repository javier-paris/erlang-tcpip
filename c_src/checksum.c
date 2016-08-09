/*****************************************************************************/
/*                                                                           */
/* Checksum Computing                                                        */
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

#include <stdlib.h>
#include <erl_driver.h>

#define INT16 65535

static ErlDrvData chk_start(ErlDrvPort port, char *buff);
static void chk_stop(ErlDrvData drv_data);
static void chk_outputv(ErlDrvData drv_data, ErlIOVec *ev);
static int chk_control(ErlDrvData drv_data, unsigned int command, 
		       unsigned char *buf, int len, char **rbuf, int rlen);


static ErlDrvEntry checksum_driver_entry = {
  NULL,                  /* init, N/A */
  chk_start,             /* start, called when port is opened */
  chk_stop,              /* stop, called when port is closed */
  NULL,                  /* output, called when erlang has sent */
  NULL,                  /* ready_input, called when input descriptor 
			    ready */
  NULL,                  /* ready_output, called when output 
			    descriptor ready */
  "checksum",            /* char *driver_name, the argument 
			    to open_port */
  NULL,                  /* finish, called when unloaded */
  NULL,                  /* void * that is not used (BC) */
  chk_control,           /* control, port_control callback */
  NULL,                  /* timeout, called on timeouts */
  chk_outputv,           /* outputv, vector output interface */
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

DRIVER_INIT(checksum)
{
  return &checksum_driver_entry;
}

static ErlDrvData chk_start(ErlDrvPort port, char *buff) 
{
  ErlDrvPort *drv = malloc(sizeof(ErlDrvPort));
  *drv = port;
  return (ErlDrvData) drv;
}

static void chk_stop(ErlDrvData drv_data)
{
  free((ErlDrvPort *) drv_data);
}

static int chk_control(ErlDrvData drv_data, unsigned int command, 
		       unsigned char *buf, int len, char **rbuf, int rlen)
{
  int i;
  unsigned int acc=0;

  for(i=0; (i+1)<len; i+=2) {
    acc += (buf[i] << 8) + buf[i+1];
  }

  if((i+1)==len) {
    acc += buf[i] << 8;
  }

  while(acc > INT16) {
    acc = (acc & INT16) + (acc >> 16);
  }
  
  acc = (~acc) & INT16;
  
  if(rlen <4) *rbuf = realloc(*rbuf, 4);
  **((int **)rbuf) = acc;

  return 2;
}

static void chk_outputv(ErlDrvData drv_data, ErlIOVec *ev)
{
  SysIOVec *iov = ev->iov;
  int i, j, size, acc=0, odd=0;
  char *buffer;

  for(i=0; i<(ev->vsize); i++) {
    buffer = iov[i].iov_base;
    size = iov[i].iov_len;

    if(odd) {
      acc += buffer[0]; ///// Was =
    }
    
    for(j=odd; (j+1) <size; j+=2) {
      acc += (buffer[j] << 8) + buffer[j+1];
    }
    
    if(j < size) {
      acc += buffer[j] << 8;
      odd=1;
    } else odd=0;
  }

  while(acc > INT16) {
    acc = (acc & INT16) + (acc >> 16);
  }
  
  acc = (~acc) & INT16;

  driver_output(*((ErlDrvPort *) drv_data), (char *) &acc, 4);
}
    
