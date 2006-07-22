/* 
   ipmi_wrapper.c: higher level wrapper to libfreeipmi functions
   
   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2, or (at
   your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA. */

#include "bmc-ipmi-wrapper.h"

/* achu: caching to make bmc-config work more quickly */
static uint8_t lan_channel_number_initialized = false;
static int8_t lan_channel_number;
static uint8_t serial_channel_number_initialized = false;
static int8_t serial_channel_number;
static uint8_t sol_channel_number_initialized = false;
static int8_t sol_channel_number;

int8_t 
get_lan_channel_number (ipmi_device_t dev)
{
  if (lan_channel_number_initialized)
    return lan_channel_number;
  
  lan_channel_number = ipmi_get_channel_number (dev, 
						IPMI_CHANNEL_MEDIUM_TYPE_LAN_802_3);

  if (!(lan_channel_number < 0))
    lan_channel_number_initialized = true;
  return lan_channel_number;
}

int8_t 
get_serial_channel_number (ipmi_device_t dev)
{
  if (serial_channel_number_initialized)
    return serial_channel_number;
  
  serial_channel_number = ipmi_get_channel_number (dev, 
						   IPMI_CHANNEL_MEDIUM_TYPE_RS232);
  if (!(serial_channel_number < 0))
    serial_channel_number_initialized = true;
  return serial_channel_number;
}

int8_t 
get_sol_channel_number (ipmi_device_t dev)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;

  if (sol_channel_number_initialized)
    return sol_channel_number;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_sol_configuration_parameters_sol_payload_channel_rs)))
    goto cleanup;

  if (ipmi_cmd_get_sol_configuration_parameters_sol_payload_channel (dev,
								     get_lan_channel_number (dev),
								     IPMI_GET_SOL_PARAMETER,
								     SET_SELECTOR,
								     BLOCK_SELECTOR,
								     obj_cmd_rs) != 0)
    {
      sol_channel_number = get_lan_channel_number (dev);
      sol_channel_number_initialized = true;
      goto cleanup;
    }
  
  if (fiid_obj_get(obj_cmd_rs,
		   "payload_channel",
		   &val) < 0)
    {
      sol_channel_number = get_lan_channel_number (dev);
      sol_channel_number_initialized = true;
      goto cleanup;
    }
  sol_channel_number = val;
  sol_channel_number_initialized = true;

 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return sol_channel_number;
}
