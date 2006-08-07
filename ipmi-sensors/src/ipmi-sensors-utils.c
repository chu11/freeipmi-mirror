/* 
   ipmi-sensors-utils.c: IPMI Sensors tool utility functions
   
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

#include <stdlib.h>
#include <errno.h>
#include <limits.h>
#include <string.h>

#include "ipmi-sensor-api.h"
#include "freeipmi/ipmi-sdr-record-types.h"

int 
str2long (char *str, int base, long *l)
{
  long value;
  char *tail = NULL;
  int errnum;
  
  errno = 0;
  value = strtol (str, &tail, base);
  errnum = errno;
  
  if (errnum)
    return (-1); // overflow
  
  if (tail[0] != '\0')
    return (-1); // invalid integer format
  
  *l = value;
  
  return 0;
}

int 
str2ulong (char *str, int base, unsigned long *ul)
{
  long l;
  unsigned long value;
  char *tail = NULL;
  int errnum;
  
  errno = 0;
  l = strtol (str, &tail, base);
  errnum = errno;
  
  if (tail[0] != '\0')
    return (-1); // invalid integer format
  
  if (!errnum)
    {
      if (l < 0)
	return (-1); // minus sign is present
    }
  
  errno = 0;
  value = strtoul (str, &tail, base);
  errnum = errno;
  
  if (errnum)
    return (-1); // overflow
  
  if (tail[0] != '\0')
    return (-1); // invalid integer format
  
  *ul = value;
  
  return 0;
}

int 
str2int (char *str, int base, int *i)
{
  long l;
  
  if (!str2long (str, base, &l))
    {
      if (l >= INT_MIN && l <= INT_MAX)
	{
	  *i = l;
	  return 0;
	}
    }
  
  return (-1);
}

int 
str2uint (char *str, int base, int *ui)
{
  unsigned long ul;
  
  if (!str2ulong (str, base, &ul))
    {
      if (ul >= INT_MIN && ul <= UINT_MAX)
	{
	  *ui = ul;
	  return 0;
	}
    }
  
  return (-1);
}

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
		  int *sensors_list, 
		  int sensors_list_length)
{
  int i;
  
  for (i = 0; i < sensors_list_length; i++)
    {
      if (sdr_record->record_id == sensors_list[i])
	return 0;
    }
  
  return (-1);
}
