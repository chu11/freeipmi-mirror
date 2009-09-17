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

#ifndef _IPMI_OEM_INVENTEC_H
#define _IPMI_OEM_INVENTEC_H

#include "ipmi-oem.h"

int ipmi_oem_inventec_get_nic_status (ipmi_oem_state_data_t *state_data);
int ipmi_oem_inventec_set_nic_status (ipmi_oem_state_data_t *state_data);

int ipmi_oem_inventec_get_mac_address (ipmi_oem_state_data_t *state_data);
int ipmi_oem_inventec_set_mac_address (ipmi_oem_state_data_t *state_data);

int ipmi_oem_inventec_get_bmc_services (ipmi_oem_state_data_t *state_data);
int ipmi_oem_inventec_set_bmc_services (ipmi_oem_state_data_t *state_data);

int ipmi_oem_inventec_read_eeprom (ipmi_oem_state_data_t *state_data);

int ipmi_oem_inventec_clear_eeprom (ipmi_oem_state_data_t *state_data);

#endif
