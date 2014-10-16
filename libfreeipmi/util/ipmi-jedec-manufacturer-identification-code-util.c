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
#include <assert.h>
#include <errno.h>

#include "freeipmi/util/ipmi-jedec-manufacturer-identification-code-util.h"
#include "freeipmi/spec/ipmi-jedec-manufacturer-identification-code-spec.h"

#include "libcommon/ipmi-trace.h"

#include "freeipmi-portability.h"

char *
_find_str (uint8_t id, const struct ipmi_jedec_manufacturer_id_pair *pairs)
{
  struct ipmi_jedec_manufacturer_id_pair *tmppair;

  assert (pairs);

  tmppair = (struct ipmi_jedec_manufacturer_id_pair *)pairs;
  while (tmppair->str)
    {
      if (tmppair->id == id)
	return (tmppair->str);
      tmppair++;
    }

  return (NULL);
}

char *
ipmi_jedec_manufacturer_id_search (uint8_t continuation_codes_count, uint8_t id)
{
  if (!IPMI_JEDEC_MANUFACTURER_CONTINUATION_CODES_VALID (continuation_codes_count))
    {
      SET_ERRNO (EINVAL);
      return (NULL);
    }

  switch (continuation_codes_count)
    {
    case 0:
      return (_find_str (id, ipmi_jedec_manufacturer_id_bank1));
    case 1:
      return (_find_str (id, ipmi_jedec_manufacturer_id_bank2));
    case 2:
      return (_find_str (id, ipmi_jedec_manufacturer_id_bank3));
    case 3:
      return (_find_str (id, ipmi_jedec_manufacturer_id_bank4));
    case 4:
      return (_find_str (id, ipmi_jedec_manufacturer_id_bank5));
    case 5:
      return (_find_str (id, ipmi_jedec_manufacturer_id_bank6));
    case 6:
      return (_find_str (id, ipmi_jedec_manufacturer_id_bank7));
    case 7:
      return (_find_str (id, ipmi_jedec_manufacturer_id_bank8));
    case 8:
      return (_find_str (id, ipmi_jedec_manufacturer_id_bank9));
    default:
      return (NULL);
    }
}
