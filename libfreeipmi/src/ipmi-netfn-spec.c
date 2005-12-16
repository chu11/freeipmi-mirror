/* 
   ipmi-netfn-spec.c: IPMI Network Function Specification

   Copyright (C) 2003, 2004, 2005 FreeIPMI Core Team

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

#include "freeipmi.h"

/* Assemble NetFn/LUN byte for IMPI command */
uint8_t
ipmi_netfn2byte (net_fn_t net_fn)
{
  uint8_t netfn_byte=0;
  /* 2 LS bits are LUN; rest are NetFn */
  netfn_byte = (net_fn.fn<<2);
  netfn_byte += net_fn.lun;
  return netfn_byte;
}
