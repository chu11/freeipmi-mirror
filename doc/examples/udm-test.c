/*
   udm-test.c: an example program about usage of libfreeipmi.
   Copyright (C) 2005 FreeIPMI Core Team

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
*/

#if HAVE_CONFIG_H
# include "config.h"
# include "freeipmi.h"
#else 
# include <freeipmi/freeipmi.h>
#endif

int 
main (int argc, char **argv)
{
  char *hostname = NULL; /* ipmi hostname for out-of-band */
  int auth_type = IPMI_SESSION_AUTH_TYPE_NONE; 
  /* for out-of-band, it can also be IPMI_SESSION_AUTH_TYPE_MD2, 
     IPMI_SESSION_AUTH_TYPE_MD5, IPMI_SESSION_AUTH_TYPE_STRAIGHT_PASSWD_KEY, 
     IPMI_SESSION_AUTH_TYPE_OEM_PROP */
  char username[] = ""; /* ipmi username for out-of-band */
  char password[] = ""; /* ipmi user's password for out-of-band */
  int priv_level = IPMI_PRIV_LEVEL_USER; 
  /* for out-of-band, can also be IPMI_PRIV_LEVEL_CALLBACK, 
     IPMI_PRIV_LEVEL_OPERATOR, IPMI_PRIV_LEVEL_ADMIN, IPMI_PRIV_LEVEL_OEM */
  
  int disable_auto_probe = 0; /* to disable automatic probing, set non-zero here */
  int driver_address = 0; /* driver address, if needed */
  char *driver_device = NULL; /* driver device, if needed */
  
  ipmi_device_t dev;
  
  struct hostent *hostinfo;
  struct sockaddr_in host;
  
  fiid_obj_t obj_cmd_rs;
  
  if (hostname != NULL)
    {
      host.sin_family = AF_INET;
      host.sin_port = htons (RMCP_AUX_BUS_SHUNT);
      hostinfo = gethostbyname (hostname);
      if (hostinfo == NULL)
	{
	  perror ("gethostbyname()");
	  exit (EXIT_FAILURE);
	}
      host.sin_addr = *(struct in_addr *) hostinfo->h_addr;
      if (ipmi_open_outofband (&dev, 
			       IPMI_DEVICE_LAN, 
			       IPMI_MODE_DEFAULT, 
			       (struct sockaddr *) &host, 
			       sizeof (struct sockaddr), 
			       auth_type, 
			       username, 
			       password, 
			       priv_level) != 0)
	{
	  perror ("ipmi_open_outofband()");
	  exit (EXIT_FAILURE);
	}
    }
  else 
    {
      if (ipmi_open_inband (&dev, 
			    disable_auto_probe, 
			    IPMI_DEVICE_KCS, /* can also be any inband device */
			    driver_address, 
			    0, /* register spacing */
			    driver_device, 
			    IPMI_MODE_DEFAULT) != 0)
	{
	  perror ("ipmi_open_inband()");
	  exit (EXIT_FAILURE);
	}
    }
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_dev_id_rs)))
    {
      perror("fiid_obj_create");
      exit (EXIT_FAILURE);
    }
  if (ipmi_cmd_get_dev_id (&dev, obj_cmd_rs) != 0)
    {
      perror ("ipmi_cmd()");
      exit (EXIT_FAILURE);
    }
  fiid_obj_dump (fileno (stdout), obj_cmd_rs);
  
  if (ipmi_close (&dev) != 0)
    {
      perror ("ipmi_close()");
      exit (EXIT_FAILURE);
    }
  
  return (0);
}
