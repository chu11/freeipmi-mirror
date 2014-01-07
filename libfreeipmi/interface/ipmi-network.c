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
#ifdef STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <errno.h>

#include "ipmi-network.h"
#include "libcommon/ipmi-trace.h"

#include "freeipmi-portability.h"

ssize_t
ipmi_network_sendto (int s,
		     const void *buf,
		     size_t len,
		     int flags,
		     const struct sockaddr *to,
		     socklen_t tolen)
{
  ssize_t rv;

  if (!buf
      || !len)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if ((rv = sendto (s, buf, len, flags, to, tolen)) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  return (rv);
}

ssize_t
ipmi_network_recvfrom (int s,
		       void *buf,
		       size_t len,
		       int flags,
		       struct sockaddr *from,
		       socklen_t *fromlen)
{
  ssize_t rv;

  if (!buf
      || !len)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if ((rv = recvfrom (s, buf, len, flags, from, fromlen)) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }
  
  return (rv);
}

