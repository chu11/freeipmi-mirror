/*
bmc-info.c: displays BMC information.
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
#include <argp.h>
#include "freeipmi.h"

#include "argp-common.h"
#include "bmc-info-argp.h"
#include "ipmi-common.h"

typedef struct channel_info 
{
  uint8_t channel_number;
  uint8_t medium_type;
  uint8_t actual_medium_type;
  uint8_t protocol_type;
  uint8_t actual_protocol_type;
} channel_info_t;

channel_info_t channel_info_list[8];

int 
display_get_dev_id (ipmi_device_t *dev)
{
  fiid_obj_t cmd_rs = NULL;
  uint64_t val = 0;
  
  fiid_obj_alloca (cmd_rs, tmpl_cmd_get_dev_id_rs);
  if (ipmi_cmd_get_dev_id (dev, cmd_rs) != 0)
    {
      ipmi_error (cmd_rs, "ipmi_cmd_get_dev_id()");
      return (-1);
    }
  
  FIID_OBJ_GET (cmd_rs, 
		tmpl_cmd_get_dev_id_rs, 
		(uint8_t *)"dev_id", 
		&val);
  fprintf (stdout, "Device ID:         %X\n", (unsigned int) val);
  
  FIID_OBJ_GET (cmd_rs, 
		tmpl_cmd_get_dev_id_rs, 
		(uint8_t *)"dev_rev.rev", 
		&val);
  fprintf (stdout, "Device Revision:   %d\n", (unsigned int) val);
  
  FIID_OBJ_GET (cmd_rs, 
		tmpl_cmd_get_dev_id_rs, 
		(uint8_t *)"dev_rev.sdr_support", 
		&val);
  if (val)
    fprintf (stdout, "                   [SDR Support]\n");
  
  {
    uint64_t maj, min;
    FIID_OBJ_GET (cmd_rs, 
		  tmpl_cmd_get_dev_id_rs, 
		  (uint8_t *)"firmware_rev1.major_rev", 
		  &maj);
    FIID_OBJ_GET (cmd_rs, 
		  tmpl_cmd_get_dev_id_rs, 
		  (uint8_t *)"firmware_rev2.minor_rev", 
		  &min);
    fprintf (stdout, "Firmware Revision: %d.%d\n", 
	     (unsigned int) maj, (unsigned int) min);
  }
  
  FIID_OBJ_GET (cmd_rs, 
		tmpl_cmd_get_dev_id_rs, 
		(uint8_t *)"firmware_rev1.dev_available", 
		&val);
  if (val == 0)
    fprintf (stdout, 
	     "                   [Device Available (normal operation)]\n");
  else
    {
      fprintf (stdout, 
	       "                   [Device Not Available]\n");
      fprintf (stdout, 
	       "                   [firmware, SDR update or self init in progress]\n");
    }
  
  {
    uint64_t ms, ls;
    FIID_OBJ_GET (cmd_rs, 
		  tmpl_cmd_get_dev_id_rs, 
		  (uint8_t *)"ipmi_ver.ms_bits", 
		  &ms);
    FIID_OBJ_GET (cmd_rs, 
		  tmpl_cmd_get_dev_id_rs, 
		  (uint8_t *)"ipmi_ver.ls_bits", 
		  &ls);
    fprintf (stdout, 
	     "IPMI Version:      %d.%d\n", (unsigned int) ms, (unsigned int) ls);
  }
  
  fprintf (stdout, "Additional Device Support:\n");
  
  FIID_OBJ_GET (cmd_rs, 
		tmpl_cmd_get_dev_id_rs, 
		(uint8_t *)"additional_dev_support.sensor_dev", 
		&val);
  if(val)
    fprintf (stdout, "                   [Sensor Device]\n");
  
  FIID_OBJ_GET (cmd_rs, 
		tmpl_cmd_get_dev_id_rs, 
		(uint8_t *)"additional_dev_support.sdr_repo_dev", 
		&val);
  if(val)
    fprintf (stdout, "                   [SDR Repository Device]\n");
  
  FIID_OBJ_GET (cmd_rs, 
		tmpl_cmd_get_dev_id_rs, 
		(uint8_t *)"additional_dev_support.sel_dev", 
		&val);
  if(val)
    fprintf (stdout, "                   [SEL Device]\n");
  
  FIID_OBJ_GET (cmd_rs, 
		tmpl_cmd_get_dev_id_rs, 
		(uint8_t *)"additional_dev_support.fru_inventory_dev", 
		&val);
  if(val)
    fprintf (stdout, "                   [FRU Inventory Device]\n");
  
  FIID_OBJ_GET (cmd_rs, 
		tmpl_cmd_get_dev_id_rs, 
		(uint8_t *)"additional_dev_support.ipmb_evnt_receiver", 
		&val);
  if(val)
    fprintf (stdout, "                   [IPMB Event Receiver]\n");
  
  FIID_OBJ_GET (cmd_rs, 
		tmpl_cmd_get_dev_id_rs, 
		(uint8_t *)"additional_dev_support.ipmb_evnt_generator", 
		&val);
  if(val)
    fprintf (stdout, "                   [IPMB Event Generator]\n");
  
  FIID_OBJ_GET (cmd_rs, 
		tmpl_cmd_get_dev_id_rs, 
		(uint8_t *)"additional_dev_support.bridge", 
		&val);
  if(val)
    fprintf (stdout, "                   [Bridge]\n");
  
  FIID_OBJ_GET (cmd_rs, 
		tmpl_cmd_get_dev_id_rs, 
		(uint8_t *)"additional_dev_support.chassis_dev", 
		&val);
  if(val)
    fprintf (stdout, "                   [Chassis Device]\n");
  
  {
    uint64_t manf_id, prod_id;
    
    FIID_OBJ_GET (cmd_rs, tmpl_cmd_get_dev_id_rs, (uint8_t *)"manf_id.id", &manf_id);
    fprintf (stdout, "Manufacturer ID:   %Xh\n", (unsigned int) manf_id);
    
    FIID_OBJ_GET (cmd_rs, tmpl_cmd_get_dev_id_rs, (uint8_t *)"prod_id", &prod_id);
    fprintf (stdout, "Product ID:        %Xh\n", (unsigned int) prod_id);
    
    FIID_OBJ_GET (cmd_rs, tmpl_cmd_get_dev_id_rs, (uint8_t *)"aux_firmware_rev_info", &val);
    switch (manf_id)
      {
      case IPMI_MANF_ID_INTEL: 
	switch (prod_id)
	  {
	    /* I am assuming all Intel products will decode alike.
                                 -- Anand Babu <ab@gnu.org.in>  */
	  case IPMI_PROD_ID_SR870BN4:
	  default:
	    {
	      uint64_t bc_maj, bc_min, pia_maj, pia_min;
	      FIID_OBJ_GET (cmd_rs,
			    tmpl_cmd_get_dev_id_sr870bn4_rs, 
			    (uint8_t *)"aux_firmware_rev_info.boot_code.major",
			    &bc_maj);
	      FIID_OBJ_GET (cmd_rs,
			    tmpl_cmd_get_dev_id_sr870bn4_rs, 
			    (uint8_t *)"aux_firmware_rev_info.boot_code.minor",
			    &bc_min);
	      FIID_OBJ_GET (cmd_rs,
			    tmpl_cmd_get_dev_id_sr870bn4_rs, 
			    (uint8_t *)"aux_firmware_rev_info.pia.major",
			    &pia_maj);
	      FIID_OBJ_GET (cmd_rs,
			    tmpl_cmd_get_dev_id_sr870bn4_rs, 
			    (uint8_t *)"aux_firmware_rev_info.pia.minor",
			    &pia_min);
	      fprintf (stdout, 
		       "Aux Firmware Revision Info: Boot Code v%02x.%2x, PIA v%02x.%2x\n",
		       (unsigned int) bc_maj, (unsigned int) bc_min, 
		       (unsigned int) pia_maj, (unsigned int) pia_min);
	      break;
	    }
	  }
	break;
      default:
	fprintf (stdout, "Aux Firmware Revision Info: %Xh\n", (unsigned int) val);
      }
  }
  
  return 0;
}

static channel_info_t *
get_channel_info_list (ipmi_device_t *dev)
{
  uint8_t *data_rs = NULL; 
  uint8_t i;
  uint8_t ci;
  uint64_t val;
  
  fiid_obj_alloca (data_rs, tmpl_get_channel_info_rs);
  for (i = 0, ci = 0; i < 8; i++)
    {
      if (ipmi_cmd_get_channel_info2 (dev, 
				      i, 
				      data_rs) != 0)
	continue;
      
      fiid_obj_get (data_rs, 
		    tmpl_get_channel_info_rs, 
		    (uint8_t *)"actual_channel_number", 
		    &val);
      channel_info_list[ci].channel_number = (uint8_t) val;
      
      fiid_obj_get (data_rs, 
		    tmpl_get_channel_info_rs, 
		    (uint8_t *)"channel_medium_type", 
		    &val);
      channel_info_list[ci].medium_type = 
	channel_info_list[ci].actual_medium_type = (uint8_t) val;
      
      fiid_obj_get (data_rs, 
		    tmpl_get_channel_info_rs, 
		    (uint8_t *)"channel_protocol_type", 
		    &val);
      channel_info_list[ci].protocol_type = 
	channel_info_list[ci].actual_protocol_type = (uint8_t) val;
      
      if (channel_info_list[ci].actual_medium_type >= 0x0D && 
	  channel_info_list[ci].actual_medium_type <= 0x5F)
	channel_info_list[ci].medium_type = IPMI_CHANNEL_MEDIUM_TYPE_RESERVED;
      
      if (channel_info_list[ci].actual_medium_type >= 0x60 && 
	  channel_info_list[ci].actual_medium_type <= 0x7F)
	channel_info_list[ci].medium_type = IPMI_CHANNEL_MEDIUM_TYPE_OEM;
      
      if (channel_info_list[ci].actual_protocol_type == 0x03 || 
	  (channel_info_list[ci].actual_protocol_type >= 0x0A && 
	   channel_info_list[ci].actual_protocol_type <= 0x1B))
	channel_info_list[ci].protocol_type = IPMI_CHANNEL_PROTOCOL_TYPE_RESERVED;
      
      if (channel_info_list[ci].actual_protocol_type >= 0x1C && 
	  channel_info_list[ci].actual_protocol_type <= 0x1F)
	channel_info_list[ci].protocol_type = IPMI_CHANNEL_PROTOCOL_TYPE_OEM;
      
      ci++;
    }
  
  return (channel_info_list);
}

int 
display_channel_info (ipmi_device_t *dev)
{
  channel_info_t *channel_list;
  uint8_t i;
  
  channel_list = get_channel_info_list (dev);
  
  printf ("Channel Information:\n");
  for (i = 0; i < 8; i++)
    {
      if (channel_list[i].medium_type == IPMI_CHANNEL_MEDIUM_TYPE_RESERVED)
	continue;
      
      printf ("       Channel No: %d\n", channel_list[i].channel_number);
      
      switch (channel_list[i].medium_type)
	{
	case IPMI_CHANNEL_MEDIUM_TYPE_RESERVED:
	  printf ("      Medium Type: %s\n", "Reserved");
	  break;
	case IPMI_CHANNEL_MEDIUM_TYPE_IPMB:
	  printf ("      Medium Type: %s\n", "IPMB (I2C)");
	  break;
	case IPMI_CHANNEL_MEDIUM_TYPE_ICMB_10:
	  printf ("      Medium Type: %s\n", "ICMB v1.0");
	  break;
	case IPMI_CHANNEL_MEDIUM_TYPE_ICMB_09:
	  printf ("      Medium Type: %s\n", "ICMB v0.9");
	  break;
	case IPMI_CHANNEL_MEDIUM_TYPE_LAN_802_3:
	  printf ("      Medium Type: %s\n", "802.3 LAN");
	  break;
	case IPMI_CHANNEL_MEDIUM_TYPE_RS232:
	  printf ("      Medium Type: %s\n", "Asynch. Serial/Modem (RS-232)");
	  break;
	case IPMI_CHANNEL_MEDIUM_TYPE_OTHER_LAN:
	  printf ("      Medium Type: %s\n", "Other LAN");
	  break;
	case IPMI_CHANNEL_MEDIUM_TYPE_PCI_SMBUS:
	  printf ("      Medium Type: %s\n", "PCI SMBus");
	  break;
	case IPMI_CHANNEL_MEDIUM_TYPE_SMBUS_10_11:
	  printf ("      Medium Type: %s\n", "SMBus v1.0/1.1");
	  break;
	case IPMI_CHANNEL_MEDIUM_TYPE_SMBUS_20:
	  printf ("      Medium Type: %s\n", "SMBus v2.0");
	  break;
	case IPMI_CHANNEL_MEDIUM_TYPE_USB_1X:
	  printf ("      Medium Type: %s\n", "USB 1.x");
	  break;
	case IPMI_CHANNEL_MEDIUM_TYPE_USB_2X:
	  printf ("      Medium Type: %s\n", "USB 2.x");
	  break;
	case IPMI_CHANNEL_MEDIUM_TYPE_SYS_IFACE:
	  printf ("      Medium Type: %s\n", "System Interface (KCS, SMIC, or BT)");
	  break;
	case IPMI_CHANNEL_MEDIUM_TYPE_OEM:
	  printf ("      Medium Type: %s\n", "OEM");
	  break;
	}
      
      switch (channel_list[i].protocol_type)
	{
	case IPMI_CHANNEL_PROTOCOL_TYPE_RESERVED:
	  printf ("    Protocol Type: %s\n", "Reserved");
	  break;
	case IPMI_CHANNEL_PROTOCOL_TYPE_IPMB:
	  printf ("    Protocol Type: %s\n", "IPMB-1.0");
	  break;
	case IPMI_CHANNEL_PROTOCOL_TYPE_ICMB_10:
	  printf ("    Protocol Type: %s\n", "ICMB-1.0");
	  break;
	case IPMI_CHANNEL_PROTOCOL_TYPE_SMBUS_1X_2X:
	  printf ("    Protocol Type: %s\n", "IPMI-SMBus");
	  break;
	case IPMI_CHANNEL_PROTOCOL_TYPE_KCS:
	  printf ("    Protocol Type: %s\n", "KCS");
	  break;
	case IPMI_CHANNEL_PROTOCOL_TYPE_SMIC:
	  printf ("    Protocol Type: %s\n", "SMIC");
	  break;
	case IPMI_CHANNEL_PROTOCOL_TYPE_BT_10:
	  printf ("    Protocol Type: %s\n", "BT-10");
	  break;
	case IPMI_CHANNEL_PROTOCOL_TYPE_BT_15:
	  printf ("    Protocol Type: %s\n", "BT-15");
	  break;
	case IPMI_CHANNEL_PROTOCOL_TYPE_TMODE:
	  printf ("    Protocol Type: %s\n", "TMODE");
	  break;
	case IPMI_CHANNEL_PROTOCOL_TYPE_OEM:
	  printf ("    Protocol Type: %s\n", "OEM");
	  break;
	}
    }
  
  return 0;
}

int 
main (int argc, char **argv)
{
  struct arguments *args = NULL;
  ipmi_device_t dev;
  
  struct hostent *hostinfo;
  struct sockaddr_in host;
  
  struct rlimit resource_limit;
  
  /* generate core dump on seg-fault */
  if (ipmi_is_root ())
    {
      resource_limit.rlim_cur =
	resource_limit.rlim_max = RLIM_INFINITY;
      if (setrlimit (RLIMIT_CORE, &resource_limit) != 0)
	perror ("warning: setrlimit()");
    }
  
  bmc_info_argp_parse (argc, argv);
  args = bmc_info_get_arguments ();
  
  if (args->common.host != NULL)
    {
      host.sin_family = AF_INET;
      host.sin_port = htons (RMCP_AUX_BUS_SHUNT);
      hostinfo = gethostbyname (args->common.host);
      if (hostinfo == NULL)
	{
	  perror ("gethostbyname()");
	  exit (EXIT_FAILURE);
	}
      host.sin_addr = *(struct in_addr *) hostinfo->h_addr;
      
      memset (&dev, 0, sizeof (ipmi_device_t));
      if (ipmi_open_outofband (&dev, 
			       IPMI_DEVICE_LAN, 
			       IPMI_MODE_DEFAULT, 
			       args->common.packet_retry_timeout, 
			       args->common.packet_retry_max, 
			       (struct sockaddr *) &host, 
			       sizeof (struct sockaddr), 
			       args->common.auth_type, 
			       args->common.username, 
			       args->common.password, 
			       args->common.priv_level) != 0)
	{
	  perror ("ipmi_open_outofband()");
	  exit (EXIT_FAILURE);
	}
    }
  else 
    {
      memset (&dev, 0, sizeof (ipmi_device_t));
      if (args->common.driver_type == IPMI_DEVICE_UNKNOWN)
	{
	  if (ipmi_open_inband (&dev, 
				args->common.disable_auto_probe, 
				IPMI_DEVICE_KCS, 
				args->common.driver_address, 
				0,
				args->common.driver_device, 
				IPMI_MODE_DEFAULT) != 0)
	    {
	      if (ipmi_open_inband (&dev, 
				    args->common.disable_auto_probe, 
				    IPMI_DEVICE_SSIF, 
				    args->common.driver_address, 
				    0,
				    args->common.driver_device, 
				    IPMI_MODE_DEFAULT) != 0)
		{
		  perror ("ipmi_open_inband()");
		  return (-1);
		}
	    }
	}
      else 
	{
	  if (ipmi_open_inband (&dev, 
				args->common.disable_auto_probe, 
				args->common.driver_type, 
				args->common.driver_address, 
				0,
				args->common.driver_device, 
				IPMI_MODE_DEFAULT) != 0)
	    {
	      perror ("ipmi_open_inband()");
	      return (-1);
	    }
	}
    }
  
  display_get_dev_id (&dev);
  display_channel_info (&dev);
  
  if (ipmi_close (&dev) != 0)
    {
      perror ("ipmi_close()");
      exit (EXIT_FAILURE);
    }
  
  return (0);
}
