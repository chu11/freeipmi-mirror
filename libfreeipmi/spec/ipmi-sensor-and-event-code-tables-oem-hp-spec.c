/*
 * Copyright (C) 2003-2015 FreeIPMI Core Team
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#ifdef STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <sys/types.h>
#include <assert.h>
#include <errno.h>

#include "freeipmi/spec/oem/ipmi-sensor-and-event-code-tables-oem-hp-spec.h"
#include "freeipmi/fiid/fiid.h"

#include "libcommon/ipmi-fiid-util.h"
#include "libcommon/ipmi-trace.h"

#include "freeipmi-portability.h"

/*
 * HP Proliant DL160 G8
 */

const char * const ipmi_oem_hp_uid_light[] =
  {
    "on",
    "off",
    "blinking",
  };
unsigned int ipmi_oem_hp_uid_light_max_index = 0x02;

const char * const ipmi_oem_hp_health_led[] =
  {
    "green",
    "amber",
    "red",
  };
unsigned int ipmi_oem_hp_health_led_max_index = 0x02;
