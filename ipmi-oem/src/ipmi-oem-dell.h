/*
  Copyright (C) 2008-2009 FreeIPMI Core Team

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

#ifndef _IPMI_OEM_DELL_H
#define _IPMI_OEM_DELL_H

#include "ipmi-oem.h"

int ipmi_oem_dell_get_system_info (ipmi_oem_state_data_t *state_data);

int ipmi_oem_dell_get_nic_selection (ipmi_oem_state_data_t *state_data);

int ipmi_oem_dell_set_nic_selection (ipmi_oem_state_data_t *state_data);

int ipmi_oem_dell_get_ssh_config (ipmi_oem_state_data_t *state_data);

int ipmi_oem_dell_set_ssh_config (ipmi_oem_state_data_t *state_data);

int ipmi_oem_dell_get_telnet_config (ipmi_oem_state_data_t *state_data);

int ipmi_oem_dell_set_telnet_config (ipmi_oem_state_data_t *state_data);

int ipmi_oem_dell_get_web_server_config (ipmi_oem_state_data_t *state_data);

int ipmi_oem_dell_set_web_server_config (ipmi_oem_state_data_t *state_data);

int ipmi_oem_dell_get_active_directory_config (ipmi_oem_state_data_t *state_data);

int ipmi_oem_dell_set_active_directory_config (ipmi_oem_state_data_t *state_data);

int ipmi_oem_dell_reset_to_defaults (ipmi_oem_state_data_t *state_data);

int ipmi_oem_dell_get_power_info (ipmi_oem_state_data_t *state_data);

int ipmi_oem_dell_reset_power_info (ipmi_oem_state_data_t *state_data);

int ipmi_oem_dell_get_instantaneous_power_consumption_info (ipmi_oem_state_data_t *state_data);

int ipmi_oem_dell_get_power_headroom_info (ipmi_oem_state_data_t *state_data);

int ipmi_oem_dell_get_average_power_history (ipmi_oem_state_data_t *state_data);

int ipmi_oem_dell_get_peak_power_history (ipmi_oem_state_data_t *state_data);

int ipmi_oem_dell_get_fcb_version (ipmi_oem_state_data_t *state_data);

#endif
