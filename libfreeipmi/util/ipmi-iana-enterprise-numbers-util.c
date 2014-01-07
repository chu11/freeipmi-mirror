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

#include "freeipmi/util/ipmi-iana-enterprise-numbers-util.h"
#include "freeipmi/spec/ipmi-iana-enterprise-numbers-spec.h"

#include "libcommon/ipmi-trace.h"

#include "freeipmi-portability.h"

static int
_is_special_case_manufacturer_id (uint32_t manufacturer_id)
{
  if (manufacturer_id == IPMI_IANA_ENTERPRISE_ID_SUPERMICRO_WORKAROUND)
    return (1);

  return (0);
}

int
ipmi_iana_enterprise_numbers_string (uint32_t manufacturer_id,
                                     char *buf,
                                     unsigned int buflen)
{
  char *str = NULL;
  int rv = -1;

  if ((!IPMI_IANA_ENTERPRISE_ID_VALID (manufacturer_id)
       && !_is_special_case_manufacturer_id (manufacturer_id))
      || !buf
      || !buflen)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (IPMI_IANA_ENTERPRISE_ID_VALID (manufacturer_id))
    {
      str = (char *)ipmi_iana_enterprise_numbers[manufacturer_id];

      /* some entries are NULL, b/c manufacturers got deleted */
      if (str)
        rv = snprintf (buf,
                       buflen,
                       "%s",
                       str);
      else
        rv = 0;
    }
  else
    {
      /* it's a special case manufacturer id */
      
      if (manufacturer_id == IPMI_IANA_ENTERPRISE_ID_SUPERMICRO_WORKAROUND)
        str = (char *)ipmi_iana_enterprise_numbers[IPMI_IANA_ENTERPRISE_ID_SUPERMICRO];

      /* some entries are NULL, b/c manufacturers got deleted */
      if (str)
        rv = snprintf (buf,
                       buflen,
                       "%s",
                       str);
    }

  return (rv);
}
