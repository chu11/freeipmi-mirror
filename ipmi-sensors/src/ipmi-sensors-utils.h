/* 
   ipmi-sensors-utils.h: IPMI Sensors tool utility functions
   
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


#ifndef _IPMI_SENSORS_UTILS_H
#define _IPMI_SENSORS_UTILS_H

int str2long (char *str, int base, long *l);
int str2ulong (char *str, int base, long *ul);
int str2int (char *str, int base, int *i);
int str2uint (char *str, int base, unsigned int *ui);
void str_replace_chr (char *str, char chr, char with);
int sensors_group_cmp (sdr_record_t *sdr_record, char *group_name);
int sensors_list_cmp (sdr_record_t *sdr_record, 
		      unsigned int *sensors_list, 
		      unsigned int sensors_list_length);
double round_double2 (double d);

#endif
