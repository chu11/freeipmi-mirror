/*
   Copyright (C) 2003-2009 FreeIPMI Core Team

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

#ifndef _IPMI_SYSTEM_INFO_PARAMETER_OEM_SPEC_H
#define _IPMI_SYSTEM_INFO_PARAMETER_OEM_SPEC_H

#ifdef __cplusplus
extern "C" {
#endif

/* 
 * Dell
 */
  
/*
 * Dell Poweredge R610
 * Dell Poweredge R710
 */

/* achu: names taken from code, are correct names? */
#define IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_ASSET_TAG              0xC4
#define IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_SERVICE_TAG            0xC5
#define IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_PRODUCT_NAME           0xD1
#define IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_10G_MAC_ADDRESSES      0xCB
#define IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_11G_MAC_ADDRESSES      0xDA
#define IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_IDRAC_VALIDATOR        0xDD
#define IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_POWER_CAPACITY         0xEA
#define IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_AVERAGE_POWER_HISTORY  0xEB
#define IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_PEAK_POWER_HISTORY     0xEC


#ifdef __cplusplus
}
#endif
#endif /* _IPMI_SYSTEM_INFO_PARAMETER_SPEC_H */
