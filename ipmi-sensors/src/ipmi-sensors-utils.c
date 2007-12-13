/* 
   Copyright (C) 2006 FreeIPMI Core Team

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

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <errno.h>
#include <limits.h>

#include "ipmi-sensor-api.h"
#include "freeipmi/ipmi-sdr-record-format.h"

#include "freeipmi-portability.h"

void 
str_replace_chr (char *str, char chr, char with)
{
  char *p = NULL;
  char *s = NULL;
  
  if (str == NULL)
    return;
  
  for (s = str;
       (p = strchr (s, chr));
       s = p + 1)
    {
      *p = with;
    }
}

int 
sensors_group_cmp (sdr_record_t *sdr_record, char *group_name)
{
  char *sdr_group_name = NULL;
  
  switch (sdr_record->record_type)
    {
    case IPMI_SDR_FORMAT_FULL_RECORD:
      sdr_group_name = (char *) ipmi_get_sensor_group (sdr_record->record.sdr_full_record.sensor_type);
      break;
    case IPMI_SDR_FORMAT_COMPACT_RECORD:
      sdr_group_name = (char *) ipmi_get_sensor_group (sdr_record->record.sdr_compact_record.sensor_type);
      break;
    case IPMI_SDR_FORMAT_EVENT_ONLY_RECORD:
      sdr_group_name = (char *) ipmi_get_sensor_group (sdr_record->record.sdr_event_only_record.sensor_type);
      break;
    }
  
  if (sdr_group_name)
    {
      if (strcasecmp (sdr_group_name, group_name) == 0)
	return 0;
      sdr_group_name = strdupa (sdr_group_name);
      str_replace_chr (sdr_group_name, ' ', '_');
      return (strcasecmp (sdr_group_name, group_name));
    }
  
  return (-1);
}

int 
sensors_list_cmp (sdr_record_t *sdr_record, 
		  unsigned int *sensors_list, 
		  unsigned int sensors_list_length)
{
  int i;
  
  for (i = 0; i < sensors_list_length; i++)
    {
      if (sdr_record->record_id == sensors_list[i])
	return 0;
    }
  
  return (-1);
}

double 
round_double2 (double d)
{
  double r = 0.0;
  
  r = (d - (long) d) * 100.0;
  
  if ((r - (long) r) > 0.5)
    return ((long) d + (((long) r + 1) / 100.0));
  
  return ((long) d + ((long) r / 100.0));
}
