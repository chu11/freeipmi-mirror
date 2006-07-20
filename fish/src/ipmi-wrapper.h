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


#ifndef _IPMI_WRAPPER_H
#define _IPMI_WRAPPER_H


typedef struct channel_info 
{
  uint8_t channel_number;
  uint8_t medium_type;
  uint8_t protocol_type;
} channel_info;

ipmi_device_t *fi_get_ipmi_device ();

int fi_ipmi_open (struct arguments *args);
int fi_ipmi_close ();

char *get_sdr_cache_filename ();

channel_info *get_channel_info_list ();
int8_t get_lan_channel_number ();
int8_t get_serial_channel_number ();
int8_t get_sol_channel_number ();
uint8_t get_lan_channel_number_known ();
uint8_t get_serial_channel_number_known ();

int display_get_device_id ();

int ipmi_ping (char *host, unsigned int sock_timeout);

#endif
