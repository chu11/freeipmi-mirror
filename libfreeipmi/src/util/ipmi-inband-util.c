/*
 * Copyright (C) 2003-2011 FreeIPMI Core Team
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

/* 2's complement checksum of preceding bytes in the connection header
   or between the previous checksum. 8-bit checksum algorithm:
   Initialize checksum to 0.
   For each byte, checksum = (checksum + byte) modulo 256. Then find
   1's compliment of checksum and add one to it.
   To verify add all the bytes and the checksum and then % 256 should
   yield 0.
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
#include <sys/types.h>
#include <sys/stat.h>
#if HAVE_FCNTL_H
#include <fcntl.h>
#endif /* HAVE_FCNTL_H */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */

#include "freeipmi/util/ipmi-outofband-util.h"
#include "freeipmi/api/ipmi-api.h"

#include "api/ipmi-api-defs.h"
#include "api/ipmi-api-trace.h"
#include "api/ipmi-api-util.h"
#include "libcommon/ipmi-fiid-util.h"
#include "libcommon/ipmi-trace.h"

#include "freeipmi-portability.h"

int
ipmi_ctx_find_inband (ipmi_ctx_t ctx,
                      ipmi_driver_type_t driver_type,
                      int disable_auto_probe,
                      uint16_t driver_address,
                      uint8_t register_spacing,
                      const char *driver_device,
                      unsigned int workaround_flags,
                      unsigned int flags)
{
  ipmi_locate_ctx_t locate_ctx = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  if (!(locate_ctx = ipmi_locate_ctx_create ()))
    {
      
      goto cleanup;
    }

  rv = 0;
 cleanup:
  if (locate_ctx)
    ipmi_locate_ctx_destroy (locate_ctx);
  return (rv);
}
