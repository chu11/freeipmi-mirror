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
#include <stdlib.h>
#ifdef HAVE_ERROR_H
#include <error.h>
#endif
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
#include <err.h>
#include <argp.h>
#include <assert.h>

#include <freeipmi/freeipmi.h>
#include <freeipmi/udm/udm.h>

#include "argp-common.h"
#include "bmc-info.h"
#include "bmc-info-argp.h"
#include "ipmi-common.h"

typedef struct channel_info 
{
  uint8_t channel_number;
  uint8_t medium_type;
  uint8_t protocol_type;
} channel_info_t;

#define NUM_CHANNELS 8

#define _FIID_OBJ_GET(bytes, field, val)               \
do {                                                   \
    uint64_t _val = 0, *_val_ptr;                      \
    _val_ptr = val;                                    \
    if (fiid_obj_get (bytes, field, &_val) < 0)        \
      {                                                \
        fprintf (stderr,                               \
                 "fiid_obj_get: %s: %s\n",             \
                 field,                                \
                 strerror(errno));                     \
        return (-1);                                   \
      }                                                \
    *_val_ptr = _val;                                  \
} while (0)

int 
display_get_device_id (bmc_info_state_data_t *state_data)
{
  fiid_obj_t cmd_rs = NULL;
  uint64_t val = 0;
  
  assert(state_data);

  if (!(cmd_rs = fiid_obj_create (tmpl_cmd_get_device_id_rs)))
    {
      perror ("fiid_obj_create");
      return (-1);
    }

  if (ipmi_cmd_get_device_id (state_data->dev, cmd_rs) != 0)
    {
      perror ("ipmi_cmd_get_device_id()");
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
                  return (-1);
		}

	      if ((len = fiid_obj_get_all(cmd_rs, buf, 1024)) < 0)
		{
		  perror("fiid_obj_get_all");
                  return (-1);
		}

	      if (fiid_obj_set_all(intel_rs, buf, len) < 0)
		{
		  perror("fiid_obj_set_all");
                  return (-1);
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

static int
get_channel_info_list (bmc_info_state_data_t *state_data, channel_info_t *channel_info_list)
{
  fiid_obj_t data_rs = NULL; 
  uint8_t i;
  uint8_t ci;
  uint64_t val;
  
  assert(state_data);
  assert(channel_info_list);

  if (!(data_rs = fiid_obj_create (tmpl_cmd_get_channel_info_rs)))
    {
      perror ("fiid_obj_create");
      return (-1);
    }

  for (i = 0, ci = 0; i < NUM_CHANNELS; i++)
    {
      if (ipmi_cmd_get_channel_info (state_data->dev, 
				     i, 
				     data_rs) != 0)
	continue;
      
      _FIID_OBJ_GET (data_rs, 
                     "actual_channel_number", 
                     &val);
      channel_info_list[ci].channel_number = (uint8_t) val;
      
      _FIID_OBJ_GET (data_rs, 
                     "channel_medium_type", 
                     &val);
      channel_info_list[ci].medium_type = (uint8_t) val;
      
      _FIID_OBJ_GET (data_rs, 
                     "channel_protocol_type", 
                     &val);
      channel_info_list[ci].protocol_type = (uint8_t) val;
      
      ci++;
    }

  fiid_obj_destroy(data_rs);
  return (0);
}

int 
display_channel_info (bmc_info_state_data_t *state_data)
{
  channel_info_t channel_info_list[NUM_CHANNELS];
  uint8_t i;
  
  assert(state_data);

  memset(channel_info_list, '\0', sizeof(channel_info_t) * NUM_CHANNELS);
  if (get_channel_info_list (state_data, channel_info_list) < 0)
    return (-1);

  printf ("Channel Information:\n");
  for (i = 0; i < NUM_CHANNELS; i++)
    {
      char *medium_type = NULL;
      char *protocol_type = NULL;

      if (IPMI_CHANNEL_MEDIUM_TYPE_IS_RESERVED(channel_info_list[i].medium_type))
        continue;
      
      printf ("       Channel No: %d\n", channel_info_list[i].channel_number);
      
      if (IPMI_CHANNEL_MEDIUM_TYPE_IS_RESERVED(channel_info_list[i].medium_type))
        medium_type = "Reserved";
      else if (channel_info_list[i].medium_type == IPMI_CHANNEL_MEDIUM_TYPE_IPMB)
        medium_type = "IPMB (I2C)";
      else if (channel_info_list[i].medium_type == IPMI_CHANNEL_MEDIUM_TYPE_ICMB_10)
        medium_type = "ICMB v1.0";
      else if (channel_info_list[i].medium_type == IPMI_CHANNEL_MEDIUM_TYPE_ICMB_09)
        medium_type = "ICMB v0.9";
      else if (channel_info_list[i].medium_type == IPMI_CHANNEL_MEDIUM_TYPE_LAN_802_3)
        medium_type = "802.3 LAN";
      else if (channel_info_list[i].medium_type == IPMI_CHANNEL_MEDIUM_TYPE_RS232)
        medium_type = "Asynch. Serial/Modem (RS-232)";
      else if (channel_info_list[i].medium_type == IPMI_CHANNEL_MEDIUM_TYPE_OTHER_LAN)
        medium_type = "Other LAN";
      else if (channel_info_list[i].medium_type == IPMI_CHANNEL_MEDIUM_TYPE_PCI_SMBUS)
        medium_type = "PCI SMBus";
      else if (channel_info_list[i].medium_type == IPMI_CHANNEL_MEDIUM_TYPE_SMBUS_10_11)
        medium_type = "SMBus v1.0/1.1";
      else if (channel_info_list[i].medium_type == IPMI_CHANNEL_MEDIUM_TYPE_SMBUS_20)
        medium_type = "SMBus v2.0";
      else if (channel_info_list[i].medium_type == IPMI_CHANNEL_MEDIUM_TYPE_USB_1X)
        medium_type = "USB 1.x";
      else if (channel_info_list[i].medium_type == IPMI_CHANNEL_MEDIUM_TYPE_USB_2X)
        medium_type = "USB 2.x";
      else if (channel_info_list[i].medium_type == IPMI_CHANNEL_MEDIUM_TYPE_SYS_IFACE)
        medium_type = "System Interface (KCS, SMIC, or BT)";
      else if (IPMI_CHANNEL_MEDIUM_TYPE_IS_OEM(channel_info_list[i].medium_type))
        medium_type = "OEM";

      if (IPMI_CHANNEL_PROTOCOL_TYPE_IS_RESERVED(channel_info_list[i].protocol_type))
        protocol_type = "Reserved";
      else if (channel_info_list[i].protocol_type == IPMI_CHANNEL_PROTOCOL_TYPE_IPMB)
        protocol_type = "IPMB-1.0";
      else if (channel_info_list[i].protocol_type == IPMI_CHANNEL_PROTOCOL_TYPE_ICMB_10)
        protocol_type = "ICMB-1.0";
      else if (channel_info_list[i].protocol_type == IPMI_CHANNEL_PROTOCOL_TYPE_SMBUS_1X_2X)
        protocol_type = "IPMI-SMBus";
      else if (channel_info_list[i].protocol_type == IPMI_CHANNEL_PROTOCOL_TYPE_KCS)
        protocol_type = "KCS";
      else if (channel_info_list[i].protocol_type == IPMI_CHANNEL_PROTOCOL_TYPE_SMIC)
        protocol_type = "SMIC";
      else if (channel_info_list[i].protocol_type == IPMI_CHANNEL_PROTOCOL_TYPE_BT_10)
        protocol_type = "BT-10";
      else if (channel_info_list[i].protocol_type == IPMI_CHANNEL_PROTOCOL_TYPE_BT_15)
        protocol_type = "BT-15";
      else if (channel_info_list[i].protocol_type == IPMI_CHANNEL_PROTOCOL_TYPE_TMODE)
        protocol_type = "TMODE";
      else if (IPMI_CHANNEL_PROTOCOL_TYPE_IS_OEM(channel_info_list[i].protocol_type))
        protocol_type = "OEM";

      if (medium_type)
        fprintf (stdout, "      Medium Type: %s\n", medium_type);
      
      if (protocol_type)
        fprintf (stdout, "    Protocol Type: %s\n", protocol_type);
                  
    }
  
  return 0;
}

static void
_disable_coredump(void)
{
  /* Disable core dumping when not-debugging.  Do not want username,
   * password or other important stuff to core dump.
   */
#ifdef NDEBUG
  struct rlimit resource_limit;

  if (!getrlimit(RLIMIT_CORE, &resource_limit))
    {
      resource_limit.rlim_cur = 0;
      if (setrlimit (RLIMIT_CORE, &resource_limit) != 0)
        perror ("warning: setrlimit()");
    }
#endif /* NDEBUG */
}

static int
_bmc_info (void *arg)
{
  bmc_info_state_data_t state_data;
  bmc_info_prog_data_t *prog_data;
  ipmi_device_t dev = NULL;
  int exit_code = -1;
  
  prog_data = (bmc_info_prog_data_t *)arg;

  if (prog_data->args->common.host != NULL)
    {
      if (!(dev = ipmi_open_outofband (IPMI_DEVICE_LAN,
                                       prog_data->args->common.host,
                                       prog_data->args->common.username,
                                       prog_data->args->common.password,
                                       prog_data->args->common.authentication_type,
                                       prog_data->args->common.privilege_level,
                                       prog_data->args->common.session_timeout,
                                       prog_data->args->common.retry_timeout,
                                       prog_data->debug_flags)))
        {
          perror ("ipmi_open_outofband()");
          exit_code = EXIT_FAILURE;
          goto cleanup;
        }
    }
  else
    {
      if (!ipmi_is_root())
        {
          fprintf(stderr, "%s: Permission Denied\n", prog_data->progname);
          exit_code = EXIT_FAILURE;
          goto cleanup;
        }

      if (prog_data->args->common.driver_type == IPMI_DEVICE_UNKNOWN)
        {
          if (!(dev = ipmi_open_inband (IPMI_DEVICE_OPENIPMI,
                                        prog_data->args->common.disable_auto_probe,
                                        prog_data->args->common.driver_address,
                                        prog_data->args->common.register_spacing,
                                        prog_data->args->common.driver_device,
                                        prog_data->debug_flags)))
            {
              if (!(dev = ipmi_open_inband (IPMI_DEVICE_KCS,
                                            prog_data->args->common.disable_auto_probe,
                                            prog_data->args->common.driver_address,
                                            prog_data->args->common.register_spacing,
                                            prog_data->args->common.driver_device,
                                            prog_data->debug_flags)))
                {
                  if (!(dev = ipmi_open_inband (IPMI_DEVICE_SSIF,
                                                prog_data->args->common.disable_auto_probe,
                                                prog_data->args->common.driver_address,
                                                prog_data->args->common.register_spacing,
                                                prog_data->args->common.driver_device,
                                                prog_data->debug_flags)))
                    {
                      perror ("ipmi_open_inband()");
                      exit_code = EXIT_FAILURE;
                      goto cleanup;
                    }
                }
            }
        }
      else
        {
          if (!(dev = ipmi_open_inband (prog_data->args->common.driver_type,
                                        prog_data->args->common.disable_auto_probe,
                                        prog_data->args->common.driver_address,
                                        prog_data->args->common.register_spacing,
                                        prog_data->args->common.driver_device,
                                        prog_data->debug_flags)))
            {
              perror ("ipmi_open_inband()");
              exit_code = EXIT_FAILURE;
              goto cleanup;
            }
        }
    }

  memset(&state_data, '\0', sizeof(bmc_info_state_data_t));
  state_data.dev = dev;
  state_data.prog_data = prog_data;

  if (display_get_device_id (&state_data) < 0)
    {
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }
  
  if (display_channel_info (&state_data) < 0)
    {
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }
  
  exit_code = 0;
 cleanup:
  if (dev)
    ipmi_close_device (dev);
  return exit_code;
}

int 
main (int argc, char **argv)
{
  bmc_info_prog_data_t prog_data;
  struct bmc_info_arguments cmd_args;
  int exit_code;
#ifdef NDEBUG
  int i;
#endif /* NDEBUG */
  
  _disable_coredump();
  
  prog_data.progname = argv[0];
  bmc_info_argp_parse (argc, argv, &cmd_args);
  prog_data.args = &cmd_args;

#ifdef NDEBUG
  /* Clear out argv data for security purposes on ps(1). */
  for (i = 1; i < argc; i++)
    memset(argv[i], '\0', strlen(argv[i]));
#endif /* NDEBUG */
  
#ifndef NDEBUG
  if (prog_data.args->common.debug)
    prog_data.debug_flags = IPMI_FLAGS_DEBUG_DUMP;
  else
    prog_data.debug_flags = IPMI_FLAGS_DEFAULT;
#else  /* NDEBUG */
  prog_data.debug_flags = IPMI_FLAGS_DEFAULT;
#endif /* NDEBUG */
  
  exit_code = _bmc_info(&prog_data);
  
  return (exit_code);
}
