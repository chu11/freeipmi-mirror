/* 
   ipmi-probe.c - Locate IPMI interfaces by any means necessary

   Copyright (C) 2003 FreeIPMI Core Team

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
   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  
*/

#ifndef _IPMI_PROBE_H
#define	_IPMI_PROBE_H	1

#ifdef __cplusplus
extern "C" {
#endif

enum ipmi_interface
{
  ipmi_interface_kcs = 1,
  ipmi_interface_smic = 2,
  ipmi_interface_bt = 3,
  ipmi_interface_last
};

typedef enum ipmi_interface ipmi_interface_t;

struct ipmi_probe_info
{
  ipmi_interface_t type;
  int bmc_io_mapped;
  union {
    u_int64_t bmc_iobase_addr;
    u_int64_t bmc_membase_addr;
  } base;
  u_int16_t intr_num;
};
typedef struct ipmi_probe_info ipmi_probe_info_t;

ipmi_probe_info_t*
ipmi_probe (ipmi_interface_t type, ipmi_probe_info_t* pinfo, int* statusp);

#ifdef __cplusplus
}
#endif

#endif /* ipmi-probe.h */

