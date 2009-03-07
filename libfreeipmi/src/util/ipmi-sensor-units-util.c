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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <errno.h>

#include "freeipmi/util/ipmi-sensor-units-util.h"
#include "freeipmi/record-format/ipmi-sdr-record-format.h"
#include "freeipmi/spec/ipmi-sensor-units-spec.h"

#include "libcommon/ipmi-trace.h"

#include "freeipmi-portability.h"

int
ipmi_sensor_units_string (uint8_t sensor_units_percentage,
                          uint8_t sensor_units_modifier,
                          uint8_t sensor_units_rate,
                          uint8_t sensor_base_unit_type,
                          uint8_t sensor_modifier_unit_type,
                          char *buf,
                          unsigned int buflen,
                          unsigned int abbreviated_units_flag)
{
  char *sensor_units = NULL;
  int rv;

  if (!IPMI_SDR_PERCENTAGE_VALID(sensor_units_percentage)
      || !IPMI_SDR_MODIFIER_UNIT_VALID(sensor_units_modifier)
      || !IPMI_SENSOR_RATE_UNIT_VALID(sensor_units_rate)
      || !IPMI_SENSOR_UNIT_VALID(sensor_base_unit_type)
      || !IPMI_SENSOR_UNIT_VALID(sensor_modifier_unit_type)
      || !buf
      || !buflen)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  /* achu: I don't know what to do with this yet */
  if (sensor_units_percentage == IPMI_SDR_PERCENTAGE_YES)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  /* achu: I assume this must be the case */
  if (sensor_units_modifier != IPMI_SDR_MODIFIER_UNIT_NONE
      && sensor_units_rate != IPMI_SENSOR_RATE_UNIT_NONE)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (abbreviated_units_flag)
    sensor_units = ipmi_sensor_units_abbreviated;
  else
    sensor_units = ipmi_sensor_units;

  if (sensor_units_modifier == IPMI_SDR_MODIFIER_UNIT_NONE
      && sensor_units_rate == IPMI_SENSOR_RATE_UNIT_NONE)
    {
      rv = snprintf (buf,
                     buflen,
                     "%s",
                     sensor_units[sensor_base_unit_type]);
      return (rv);
    }
  
  if (sensor_units_rate != IPMI_SENSOR_RATE_UNIT_NONE)
    {
      rv = snprintf (buf,
                     buflen,
                     "%s %s",
                     sensor_units[sensor_base_unit_type],
                     ipmi_sensor_rate_units[sensor_units_rate]);
      return (rv);
    }
  
  /* else sensor_units_modifier != IPMI_SDR_MODIFIER_UNIT_NONE */

  if (sensor_units_modifier == IPMI_SDR_MODIFIER_UNIT_DIVIDE)
    rv = snprintf (buf,
                   buflen,
                   "%s / %s",
                   sensor_units[sensor_base_unit_type],
                   sensor_units[sensor_modifier_unit_type]);
  else
    rv = snprintf (buf,
                   buflen,
                   "%s * %s",
                   sensor_units[sensor_base_unit_type],
                   sensor_units[sensor_modifier_unit_type]);

  return (rv);
}
