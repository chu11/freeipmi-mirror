/*
 * Copyright (C) 2003-2014 FreeIPMI Core Team
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
#include <math.h>
#include <errno.h>

#include "freeipmi/util/ipmi-entity-ids-util.h"
#include "freeipmi/spec/ipmi-entity-ids-spec.h"

#include "libcommon/ipmi-trace.h"

#include "freeipmi-portability.h"

const char *
ipmi_get_entity_id_string (uint8_t entity_id)
{
  if (IPMI_ENTITY_ID_VALID (entity_id))
    return (ipmi_entity_ids_pretty[entity_id]);
  else if (IPMI_ENTITY_ID_IS_CHASSIS_SPECIFIC (entity_id))
    return (ipmi_entity_id_chassis_specific);
  else if (IPMI_ENTITY_ID_IS_BOARD_SET_SPECIFIC (entity_id))
    return (ipmi_entity_id_board_set_specific);
  else if (IPMI_ENTITY_ID_IS_OEM_SYSTEM_INTEGRATOR_DEFINED (entity_id))
    return (ipmi_entity_id_oem_system_integrator);
  
  return (ipmi_entity_id_oem_system_integrator);
}
