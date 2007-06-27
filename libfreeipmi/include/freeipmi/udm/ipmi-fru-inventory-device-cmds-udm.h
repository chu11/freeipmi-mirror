/*****************************************************************************\
 *  $Id: ipmi-fru-inventory-device-cmds-udm.h,v 1.1 2007-06-27 21:35:34 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-232183
 *
 *  This file is part of Ipmi-fru, a tool used for retrieving
 *  motherboard field replaceable unit (FRU) information. For details,
 *  see http://www.llnl.gov/linux/.
 *
 *  Ipmi-fru is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 2 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmi-fru is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmi-fru; if not, write to the Free Software Foundation, Inc.,
 *  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA.
\*****************************************************************************/

#ifndef _IPMI_FRU_INVENTORY_DEVICE_CMDS_UDM_H
#define	_IPMI_FRU_INVENTORY_DEVICE_CMDS_UDM_H

#ifdef __cplusplus
extern "C" {
#endif

#include <freeipmi/fiid.h>
#include <freeipmi/udm/ipmi-udm.h>

int8_t ipmi_cmd_get_fru_inventory_area_info (ipmi_device_t dev,
                                             uint8_t fru_device_id,
                                             fiid_obj_t obj_cmd_rs);
  
int8_t ipmi_cmd_read_fru_data (ipmi_device_t dev,
                               uint8_t fru_device_id,
                               uint16_t fru_inventory_offset_to_read,
                               uint8_t count_to_read,
                               fiid_obj_t obj_cmd_rs);

#ifdef __cplusplus
}
#endif

#endif /* _IPMI_FRU_INVENTORY_DEVICE_CMDS_UDM_H */
