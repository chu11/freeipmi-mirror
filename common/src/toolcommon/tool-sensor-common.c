/*
  Copyright (C) 2003-2009 FreeIPMI Core Team

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2, or (at your option)
  any later version.

  This program is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA
*/

#if HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <assert.h>
#include <errno.h>

#include <freeipmi/freeipmi.h>

#include "tool-sensor-common.h"

#include "freeipmi-portability.h"
#include "pstdout.h"

static void
_str_replace_char (char *str, char chr, char with)
{
  char *p = NULL;
  char *s = NULL;
  
  assert (str);
  
  for (s = str;
       (p = strchr (s, chr));
       s = p + 1)
    *p = with;
}

const char *
get_sensor_group_output_string (unsigned int sensor_type)
{
  const char *sensor_group;

  if ((sensor_group = ipmi_get_sensor_type_string (sensor_type)))
    return (sensor_group);

  return (UNRECOGNIZED_SENSOR_GROUP);
}

void
get_sensor_group_cmdline_string (char *sensor_group)
{
  assert (sensor_group);

  _str_replace_char (sensor_group, ' ', '_');
  _str_replace_char (sensor_group, '/', '_');
}

int
display_sensor_group_cmdline (pstdout_state_t pstate, 
                              unsigned int sensor_type)
{
  const char *sensor_group;
  char *tmpstr = NULL;

  sensor_group = get_sensor_group_output_string (sensor_type);

  if (!(tmpstr = strdupa (sensor_group)))
    {
      PSTDOUT_FPRINTF (pstate,
                       stderr,
                       "strdupa: %s\n",
                       strerror (errno));
      return (-1);
    }

  get_sensor_group_cmdline_string (tmpstr);

  PSTDOUT_PRINTF (pstate, "%s\n", tmpstr);
  return (0);
}

int
display_string_cmdline (pstdout_state_t pstate, 
                        const char *str)
{
  char *tmpstr;

  assert (str);

  if (!(tmpstr = strdupa (str)))
    {
      PSTDOUT_FPRINTF (pstate,
                       stderr,
                       "strdupa: %s\n",
                       strerror (errno));
      return (-1);
    } 

  get_sensor_group_cmdline_string (tmpstr);

  PSTDOUT_PRINTF (pstate, "%s\n", tmpstr);
  return (0);
}
