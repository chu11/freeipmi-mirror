/*
  Copyright (C) 2003-2010 FreeIPMI Core Team

  Based on ipmitool.c provided by Amitoj Singh <amitoj@fnal.gov> and
  Don Holmgren <djholm@fnal.gov>

  Under GNU/Linux, requires i2c-dev, i2c-i801, i2c-core drivers version >= 2.8.7

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
  Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.
*/

#ifndef IPMI_SSIF_DRIVER_API_H
#define IPMI_SSIF_DRIVER_API_H

#include <stdint.h>
#include <freeipmi/api/ipmi-api.h>
#include <freeipmi/fiid/fiid.h>

int ipmi_ssif_cmd_api (ipmi_ctx_t ctx,
                       fiid_obj_t obj_cmd_rq,
                       fiid_obj_t obj_cmd_rs);

int ipmi_ssif_cmd_raw_api (ipmi_ctx_t ctx,
                           const void *buf_rq,
                           unsigned int buf_rq_len,
                           void *buf_rs,
                           unsigned int buf_rs_len);

#endif /* IPMI_SSIF_DRIVER_API_H */
