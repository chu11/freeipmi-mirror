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

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <error.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#include <sys/resource.h>
#if TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else /* !TIME_WITH_SYS_TIME */
#if HAVE_SYS_TIME_H
#include <sys/time.h>
#else /* !HAVE_SYS_TIME_H */
#include <time.h>
#endif /* !HAVE_SYS_TIME_H */
#endif /* !TIME_WITH_SYS_TIME */
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <err.h>
#include <argp.h>

#include <freeipmi/freeipmi.h>
#include <freeipmi/udm/udm.h>

#include "argp-common.h"
#include "bmc-info-argp.h"
#include "ipmi-common.h"

typedef struct channel_info 
{
  uint8_t channel_number;
  uint8_t medium_type;
  uint8_t protocol_type;
} channel_info_t;

channel_info_t channel_info_list[8];

#define _FIID_OBJ_GET(bytes, field, val)               \
do {                                                   \
    uint64_t _val = 0, *_val_ptr;                      \
    _val_ptr = val;                                    \
    if (fiid_obj_get (bytes, field, &_val) < 0)        \
      {                                                \
        err (1, "fiid_obj_get (%p, \"%s\", %p) error", \
             bytes, field, val);                       \
      }                                                \
    *_val_ptr = _val;                                  \
} while (0)

int 
display_get_device_id (ipmi_device_t *dev)
{
  fiid_obj_t cmd_rs = NULL;
  uint64_t val = 0;
  
  if (!(cmd_rs = fiid_obj_create (tmpl_cmd_get_device_id_rs)))
    {
      perror ("fiid_obj_create");
      exit (EXIT_FAILURE);
    }

  if (ipmi_cmd_get_device_id (dev, cmd_rs) != 0)
    {
      ipmi_error (cmd_rs, 
                  dev->net_fn,
                  "ipmi_cmd_get_device_id()");
      return (-1);
    }
  
  _FIID_OBJ_GET (cmd_rs, 
                 "device_id", 
                 &val);
  fprintf (stdout, "Device ID:         %X\n", (unsigned int) val);
  
  _FIID_OBJ_GET (cmd_rs, 
                 "device_revision.revision", 
                 &val);
  fprintf (stdout, "Device Revision:   %d\n", (unsigned int) val);
  
  _FIID_OBJ_GET (cmd_rs, 
                 "device_revision.sdr_support", 
                 &val);
  if (val)
    fprintf (stdout, "                   [SDR Support]\n");
  
  {
    uint64_t maj, min;
    _FIID_OBJ_GET (cmd_rs, 
                   "firmware_revision1.major_revision", 
                   &maj);
    _FIID_OBJ_GET (cmd_rs, 
                   "firmware_revision2.minor_revision", 
                   &min);
    fprintf (stdout, "Firmware Revision: %d.%d\n", 
	     (unsigned int) maj, (unsigned int) min);
  }
  
  _FIID_OBJ_GET (cmd_rs, 
                 "firmware_revision1.device_available", 
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
    _FIID_OBJ_GET (cmd_rs, 
                   "ipmi_version.ms_bits", 
                   &ms);
    _FIID_OBJ_GET (cmd_rs, 
                   "ipmi_version.ls_bits", 
                   &ls);
    fprintf (stdout, 
	     "IPMI Version:      %d.%d\n", (unsigned int) ms, (unsigned int) ls);
  }
  
  fprintf (stdout, "Additional Device Support:\n");
  
  _FIID_OBJ_GET (cmd_rs, 
                 "additional_device_support.sensor_device", 
                 &val);
  if(val)
    fprintf (stdout, "                   [Sensor Device]\n");
  
  _FIID_OBJ_GET (cmd_rs, 
                 "additional_device_support.sdr_repository_device", 
                 &val);
  if(val)
    fprintf (stdout, "                   [SDR Repository Device]\n");
  
  _FIID_OBJ_GET (cmd_rs, 
                 "additional_device_support.sel_device", 
                 &val);
  if(val)
    fprintf (stdout, "                   [SEL Device]\n");
  
  _FIID_OBJ_GET (cmd_rs, 
                 "additional_device_support.fru_inventory_device", 
                 &val);
  if(val)
    fprintf (stdout, "                   [FRU Inventory Device]\n");
  
  _FIID_OBJ_GET (cmd_rs, 
                 "additional_device_support.ipmb_event_receiver", 
                 &val);
  if(val)
    fprintf (stdout, "                   [IPMB Event Receiver]\n");
  
  _FIID_OBJ_GET (cmd_rs, 
                 "additional_device_support.ipmb_event_generator", 
                 &val);
  if(val)
    fprintf (stdout, "                   [IPMB Event Generator]\n");
  
  _FIID_OBJ_GET (cmd_rs, 
                 "additional_device_support.bridge", 
                 &val);
  if(val)
    fprintf (stdout, "                   [Bridge]\n");
  
  _FIID_OBJ_GET (cmd_rs, 
                 "additional_device_support.chassis_device", 
                 &val);
  if(val)
    fprintf (stdout, "                   [Chassis Device]\n");
  
  {
    uint64_t manufacturer_id, product_id;
    
    _FIID_OBJ_GET (cmd_rs, "manufacturer_id.id", &manufacturer_id);
    fprintf (stdout, "Manufacturer ID:   %Xh\n", (unsigned int) manufacturer_id);
    
    _FIID_OBJ_GET (cmd_rs, "product_id", &product_id);
    fprintf (stdout, "Product ID:        %Xh\n", (unsigned int) product_id);
    
    _FIID_OBJ_GET (cmd_rs, "auxiliary_firmware_revision_info", &val);
    switch (manufacturer_id)
      {
      case IPMI_MANUFACTURER_ID_INTEL: 
	switch (product_id)
	  {
	    /* I am assuming all Intel products will decode alike.
               -- Anand Babu <ab@gnu.org.in>  */
	  case IPMI_PRODUCT_ID_SR870BN4:
	  default:
	    {
	      uint64_t bc_maj, bc_min, pia_maj, pia_min;
	      fiid_obj_t intel_rs;
	      uint8_t buf[1024];
	      int32_t len;

	      if (!(intel_rs = fiid_obj_create(tmpl_cmd_get_device_id_sr870bn4_rs)))
		{
		  perror ("fiid_obj_create");
		  exit (EXIT_FAILURE);
		}

	      if ((len = fiid_obj_get_all(cmd_rs, buf, 1024)) < 0)
		{
		  perror("fiid_obj_get_all");
		  exit (EXIT_FAILURE);
		}

	      if (fiid_obj_set_all(intel_rs, buf, len) < 0)
		{
		  perror("fiid_obj_set_all");
		  exit (EXIT_FAILURE);
		}
	      
	      _FIID_OBJ_GET (intel_rs,
                             "auxiliary_firmware_revision_info.boot_code.major",
                             &bc_maj);
	      _FIID_OBJ_GET (intel_rs,
                             "auxiliary_firmware_revision_info.boot_code.minor",
                             &bc_min);
	      _FIID_OBJ_GET (intel_rs,
                             "auxiliary_firmware_revision_info.pia.major",
                             &pia_maj);
	      _FIID_OBJ_GET (intel_rs,
                             "auxiliary_firmware_revision_info.pia.minor",
                             &pia_min);
	      fprintf (stdout, 
		       "Aux Firmware Revision Info: Boot Code v%02x.%2x, PIA v%02x.%2x\n",
		       (unsigned int) bc_maj, (unsigned int) bc_min, 
		       (unsigned int) pia_maj, (unsigned int) pia_min);

	      fiid_obj_destroy(intel_rs);
	      break;
	    }
	  }
	break;
      default:
	fprintf (stdout, "Aux Firmware Revision Info: %Xh\n", (unsigned int) val);
      }
  }

  fiid_obj_destroy(cmd_rs);
  return 0;
}

static channel_info_t *
get_channel_info_list (ipmi_device_t *dev)
{
  fiid_obj_t data_rs = NULL; 
  uint8_t i;
  uint8_t ci;
  uint64_t val;
  
  if (!(data_rs = fiid_obj_create (tmpl_cmd_get_channel_info_rs)))
    {
      perror ("fiid_obj_create");
      exit (EXIT_FAILURE);
    }

  for (i = 0, ci = 0; i < 8; i++)
    {
      if (ipmi_cmd_get_channel_info (dev, 
				     i, 
				     data_rs) != 0)
	continue;
      
      fiid_obj_get (data_rs, 
		    "actual_channel_number", 
		    &val);
      channel_info_list[ci].channel_number = (uint8_t) val;
      
      fiid_obj_get (data_rs, 
		    "channel_medium_type", 
		    &val);
      channel_info_list[ci].medium_type = (uint8_t) val;
      
      fiid_obj_get (data_rs, 
		    "channel_protocol_type", 
		    &val);
      channel_info_list[ci].protocol_type = (uint8_t) val;
      
      ci++;
    }

  fiid_obj_destroy(data_rs);
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
      if (IPMI_CHANNEL_MEDIUM_TYPE_IS_RESERVED(channel_info_list[i].medium_type))
        continue;
      
      printf ("       Channel No: %d\n", channel_list[i].channel_number);
      
      if (IPMI_CHANNEL_MEDIUM_TYPE_IS_RESERVED(channel_info_list[i].medium_type))
        printf ("      Medium Type: %s\n", "Reserved");
      else if (channel_info_list[i].medium_type == IPMI_CHANNEL_MEDIUM_TYPE_IPMB)
        printf ("      Medium Type: %s\n", "IPMB (I2C)");
      else if (channel_info_list[i].medium_type == IPMI_CHANNEL_MEDIUM_TYPE_ICMB_10)
        printf ("      Medium Type: %s\n", "ICMB v1.0");
      else if (channel_info_list[i].medium_type == IPMI_CHANNEL_MEDIUM_TYPE_ICMB_09)
        printf ("      Medium Type: %s\n", "ICMB v0.9");
      else if (channel_info_list[i].medium_type == IPMI_CHANNEL_MEDIUM_TYPE_LAN_802_3)
        printf ("      Medium Type: %s\n", "802.3 LAN");
      else if (channel_info_list[i].medium_type == IPMI_CHANNEL_MEDIUM_TYPE_RS232)
        printf ("      Medium Type: %s\n", "Asynch. Serial/Modem (RS-232)");
      else if (channel_info_list[i].medium_type == IPMI_CHANNEL_MEDIUM_TYPE_OTHER_LAN)
        printf ("      Medium Type: %s\n", "Other LAN");
      else if (channel_info_list[i].medium_type == IPMI_CHANNEL_MEDIUM_TYPE_PCI_SMBUS)
        printf ("      Medium Type: %s\n", "PCI SMBus");
      else if (channel_info_list[i].medium_type == IPMI_CHANNEL_MEDIUM_TYPE_SMBUS_10_11)
        printf ("      Medium Type: %s\n", "SMBus v1.0/1.1");
      else if (channel_info_list[i].medium_type == IPMI_CHANNEL_MEDIUM_TYPE_SMBUS_20)
        printf ("      Medium Type: %s\n", "SMBus v2.0");
      else if (channel_info_list[i].medium_type == IPMI_CHANNEL_MEDIUM_TYPE_USB_1X)
        printf ("      Medium Type: %s\n", "USB 1.x");
      else if (channel_info_list[i].medium_type == IPMI_CHANNEL_MEDIUM_TYPE_USB_2X)
        printf ("      Medium Type: %s\n", "USB 2.x");
      else if (channel_info_list[i].medium_type == IPMI_CHANNEL_MEDIUM_TYPE_SYS_IFACE)
        printf ("      Medium Type: %s\n", "System Interface (KCS, SMIC, or BT)");
      else if (IPMI_CHANNEL_MEDIUM_TYPE_IS_OEM(channel_info_list[i].medium_type))
        printf ("      Medium Type: %s\n", "OEM");

      if (IPMI_CHANNEL_PROTOCOL_TYPE_IS_RESERVED(channel_info_list[i].protocol_type))
        printf ("    Protocol Type: %s\n", "Reserved");
      else if (IPMI_CHANNEL_PROTOCOL_TYPE_IPMB)
        printf ("    Protocol Type: %s\n", "IPMB-1.0");
      else if (IPMI_CHANNEL_PROTOCOL_TYPE_ICMB_10)
        printf ("    Protocol Type: %s\n", "ICMB-1.0");
      else if (IPMI_CHANNEL_PROTOCOL_TYPE_SMBUS_1X_2X)
        printf ("    Protocol Type: %s\n", "IPMI-SMBus");
      else if (IPMI_CHANNEL_PROTOCOL_TYPE_KCS)
        printf ("    Protocol Type: %s\n", "KCS");
      else if (IPMI_CHANNEL_PROTOCOL_TYPE_SMIC)
        printf ("    Protocol Type: %s\n", "SMIC");
      else if (IPMI_CHANNEL_PROTOCOL_TYPE_BT_10)
        printf ("    Protocol Type: %s\n", "BT-10");
      else if (IPMI_CHANNEL_PROTOCOL_TYPE_BT_15)
        printf ("    Protocol Type: %s\n", "BT-15");
      else if (IPMI_CHANNEL_PROTOCOL_TYPE_TMODE)
        printf ("    Protocol Type: %s\n", "TMODE");
      else if (IPMI_CHANNEL_PROTOCOL_TYPE_IS_OEM(channel_info_list[i].protocol_type))
        printf ("    Protocol Type: %s\n", "OEM");
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

#ifdef NDEBUG
  /* Clear out argv data for security purposes on ps(1). */
  for (i = 1; i < argc; i++)
    memset(argv[i], '\0', strlen(argv[i]));
#endif /* NDEBUG */
  
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
			       args->common.authentication_type, 
			       args->common.username, 
			       args->common.password, 
			       args->common.privilege_level) != 0)
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
				args->common.register_spacing,
				args->common.driver_device, 
				IPMI_MODE_DEFAULT) != 0)
	    {
	      if (ipmi_open_inband (&dev, 
				    args->common.disable_auto_probe, 
				    IPMI_DEVICE_SSIF, 
				    args->common.driver_address, 
                                    args->common.register_spacing,
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
				args->common.register_spacing,
				args->common.driver_device, 
				IPMI_MODE_DEFAULT) != 0)
	    {
	      perror ("ipmi_open_inband()");
	      return (-1);
	    }
	}
    }
  
  display_get_device_id (&dev);
  display_channel_info (&dev);
  
  if (ipmi_close (&dev) != 0)
    {
      perror ("ipmi_close()");
      exit (EXIT_FAILURE);
    }
  
  return (0);
}
