/* 
   ipmi_wrapper.h: higher level wrapper to libfreeipmi functions
   
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


#ifndef _BMC_IPMI_WRAPPER_H
#define _BMC_IPMI_WRAPPER_H

#include "bmc-common.h"
#include "bmc-types.h"

int8_t get_lan_channel_number (ipmi_device_t dev);
int8_t get_serial_channel_number (ipmi_device_t dev);
int8_t get_sol_channel_number (ipmi_device_t dev);

#endif
