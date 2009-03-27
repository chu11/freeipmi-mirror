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

#ifndef _IPMI_OEM_COMMON_H
#define _IPMI_OEM_COMMON_H

#include "ipmi-oem.h"

#define IPMI_OEM_MAX_BYTES      256
#define IPMI_OEM_ERR_BUFLEN     1024

int ipmi_oem_check_response_and_completion_code (ipmi_oem_state_data_t *state_data,
                                                 uint8_t *bytes_rs,
                                                 unsigned int bytes_rs_len,
                                                 unsigned int expected_bytes_rs_len,
                                                 uint8_t cmd,
                                                 uint8_t netfn);

#endif
