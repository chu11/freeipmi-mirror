/*
ipmi-lan-test-udm.c: Test Utility and Example implementation for LAN driver.
Copyright (C) 2005 FreeIPMI Core Team

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA
*/

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <error.h>
#include <string.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <freeipmi/freeipmi.h>

#define IPMI_HOST   "debian-ia64"
#define AUTH_TYPE   IPMI_SESSION_AUTH_TYPE_NONE
#define USERNAME    ""
#define PASSWORD    ""
#define PRIVILEGE_LEVEL IPMI_PRIV_LEVEL_ADMIN

int 
main (void)
{
  ipmi_device_t dev;
  
  struct hostent *hostinfo;
  struct sockaddr_in host;
  
  host.sin_family = AF_INET;
  host.sin_port = htons (RMCP_AUX_BUS_SHUNT);
  hostinfo = gethostbyname (IPMI_HOST);
  if (hostinfo == NULL)
    {
      fprintf (stderr, "Unknown host %s.\n", IPMI_HOST);
      exit (EXIT_FAILURE);
    }
  host.sin_addr = *(struct in_addr *) hostinfo->h_addr;
  
  if (ipmi_open_outofband (&dev, 
			   IPMI_DEVICE_LAN, 
			   IPMI_MODE_DEFAULT, 
			   (struct sockaddr *) &host, 
			   sizeof (struct sockaddr), 
			   AUTH_TYPE, 
			   USERNAME, 
			   PASSWORD, 
			   PRIVILEGE_LEVEL, 
			   IPMI_NET_FN_APP_RQ, 
			   IPMI_BMC_IPMB_LUN_BMC) != 0)
    {
      fprintf (stderr, "ipmi_open_outofband() failed\n");
      exit (EXIT_FAILURE);
    }
  
  /* IPMI Get Device ID command */
  {
    fiid_obj_t obj_cmd_rq;
    fiid_obj_t obj_cmd_rs;
    
    obj_cmd_rq = alloca (fiid_obj_len_bytes (tmpl_cmd_get_dev_id_rq));
    memset (obj_cmd_rq, 0, fiid_obj_len_bytes (tmpl_cmd_get_dev_id_rq));
    
    obj_cmd_rs = alloca (fiid_obj_len_bytes (tmpl_cmd_get_dev_id_rs));
    memset (obj_cmd_rs, 0, fiid_obj_len_bytes (tmpl_cmd_get_dev_id_rs));
    
    fill_cmd_get_dev_id (obj_cmd_rq);
    
    if (ipmi_cmd (&dev, 
		  tmpl_cmd_get_dev_id_rq, 
		  obj_cmd_rq, 
		  tmpl_cmd_get_dev_id_rs, 
		  obj_cmd_rs) != 0)
      {
	fprintf (stderr, "ipmi_cmd() failed\n");
      }
    
    fiid_obj_dump (fileno (stdout), obj_cmd_rs, tmpl_cmd_get_dev_id_rs);
  }
  
  if (ipmi_close (&dev) != 0)
    {
      fprintf (stderr, "ipmi_close() failed\n");
      exit (EXIT_FAILURE);
    }
  
  return (0);
}
