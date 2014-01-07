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
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <assert.h>
#include <errno.h>

#include "freeipmi/locate/ipmi-locate.h"

#include "ipmi-locate-defs.h"
#include "ipmi-locate-util.h"
#include "ipmi-locate-trace.h"

#include "freeipmi-portability.h"

typedef int ((*ipmi_locate_func)(ipmi_locate_ctx_t ctx,
                                 ipmi_interface_type_t,
                                 struct ipmi_locate_info *));

static char * ipmi_locate_ctx_errmsg[] =
  {
    "success",
    "locate context null",
    "locate context invalid",
    "invalid parameter",
    "permission denied",
    "out of memory",
    "internal system error",
    "internal error",
    "errnum out of range",
    NULL,
  };

ipmi_locate_ctx_t
ipmi_locate_ctx_create (void)
{
  ipmi_locate_ctx_t ctx = NULL;

  if (!(ctx = (ipmi_locate_ctx_t)malloc (sizeof (struct ipmi_locate_ctx))))
    {
      ERRNO_TRACE (errno);
      return (NULL);
    }
  memset (ctx, '\0', sizeof (struct ipmi_locate_ctx));

  ctx->magic = IPMI_LOCATE_CTX_MAGIC;

  ctx->errnum = IPMI_LOCATE_ERR_SUCCESS;
  return (ctx);
}

void
ipmi_locate_ctx_destroy (ipmi_locate_ctx_t ctx)
{
  if (!ctx || ctx->magic != IPMI_LOCATE_CTX_MAGIC)
    return;

  ctx->magic = ~IPMI_LOCATE_CTX_MAGIC;
  ctx->errnum = IPMI_LOCATE_ERR_SUCCESS;
  free (ctx);
}

int
ipmi_locate_ctx_errnum (ipmi_locate_ctx_t ctx)
{
  if (!ctx)
    return (IPMI_LOCATE_ERR_NULL);
  else if (ctx->magic != IPMI_LOCATE_CTX_MAGIC)
    return (IPMI_LOCATE_ERR_INVALID);
  else
    return (ctx->errnum);
}

char *
ipmi_locate_ctx_strerror (int errnum)
{
  if (errnum >= IPMI_LOCATE_ERR_SUCCESS && errnum <= IPMI_LOCATE_ERR_ERRNUMRANGE)
    return (ipmi_locate_ctx_errmsg[errnum]);
  else
    return (ipmi_locate_ctx_errmsg[IPMI_LOCATE_ERR_ERRNUMRANGE]);
}

char *
ipmi_locate_ctx_errormsg (ipmi_locate_ctx_t ctx)
{
  return (ipmi_locate_ctx_strerror (ipmi_locate_ctx_errnum (ctx)));
}

static int
_ipmi_locate_get_device_info (ipmi_locate_ctx_t ctx,
                              ipmi_interface_type_t type,
                              struct ipmi_locate_info *info,
                              int try_defaults)
{
  static ipmi_locate_func things_to_try[] =
    {
      ipmi_locate_dmidecode_get_device_info,
      ipmi_locate_smbios_get_device_info,
      ipmi_locate_acpi_spmi_get_device_info,
      ipmi_locate_pci_get_device_info,
      ipmi_locate_defaults_get_device_info,
      NULL
    };
  unsigned int i, things_to_try_len;
  struct ipmi_locate_info linfo;
  int rv;

  if (!ctx || ctx->magic != IPMI_LOCATE_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_locate_ctx_errormsg (ctx), ipmi_locate_ctx_errnum (ctx));
      return (-1);
    }

  if (!IPMI_INTERFACE_TYPE_VALID (type) || !info)
    {
      LOCATE_SET_ERRNUM (ctx, IPMI_LOCATE_ERR_PARAMETERS);
      return (-1);
    }

  if (try_defaults)
    things_to_try_len = 5;
  else
    things_to_try_len = 4;

  for (i = 0; i < things_to_try_len; i++)
    {
      memset (&linfo, 0, sizeof (struct ipmi_locate_info));
      rv = (*things_to_try[i])(ctx, type, &linfo);
      if (!rv)
        {
          memcpy (info, &linfo, sizeof (struct ipmi_locate_info));
          /* reset errnum if set previously */
          ctx->errnum = IPMI_LOCATE_ERR_SUCCESS;
          return (0);
        }
    }

  LOCATE_SET_ERRNUM (ctx, IPMI_LOCATE_ERR_SYSTEM_ERROR);
  return (-1);
}

int
ipmi_locate_get_device_info (ipmi_locate_ctx_t ctx,
                             ipmi_interface_type_t type,
                             struct ipmi_locate_info *info)
{
  return (_ipmi_locate_get_device_info (ctx, type, info, 1));
}

int
ipmi_locate_discover_device_info (ipmi_locate_ctx_t ctx,
                                  ipmi_interface_type_t type,
                                  struct ipmi_locate_info *info)
{
  return (_ipmi_locate_get_device_info (ctx, type, info, 0));
}
