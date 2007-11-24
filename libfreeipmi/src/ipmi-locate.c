/* 
   ipmi-locate.c - Locate IPMI interfaces by scanning various system
   information

   Copyright (C) 2003, 2004, 2005 FreeIPMI Core Team

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
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif 
#include <errno.h>

#include "freeipmi/ipmi-locate.h"

#include "ipmi-locate-definitions.h"

#include "err-wrappers.h"
#include "freeipmi-portability.h"
#include "xmalloc.h"

typedef int ((*ipmi_locate_func)(ipmi_locate_ctx_t, ipmi_interface_type_t, struct ipmi_locate_info *));

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
ipmi_locate_ctx_create(void)
{
  ipmi_locate_ctx_t ctx = NULL;

  ERR_CLEANUP ((ctx = (ipmi_locate_ctx_t)xmalloc(sizeof(struct ipmi_locate_ctx))));

  ctx->magic = IPMI_LOCATE_CTX_MAGIC;
  ctx->errnum = IPMI_LOCATE_CTX_ERR_SUCCESS;
  return ctx;

 cleanup:
  if (ctx)
    xfree(ctx);
  return (NULL);
}

int8_t
ipmi_locate_ctx_destroy(ipmi_locate_ctx_t ctx)
{
  ERR(ctx && ctx->magic == IPMI_LOCATE_CTX_MAGIC);

  ctx->magic = ~IPMI_LOCATE_CTX_MAGIC;
  ctx->errnum = IPMI_LOCATE_CTX_ERR_SUCCESS;
  xfree(ctx);
  return (0);
}

char *
ipmi_locate_ctx_strerror(int32_t errnum)
{
  if (errnum >= IPMI_LOCATE_CTX_ERR_SUCCESS && errnum <= IPMI_LOCATE_CTX_ERR_ERRNUMRANGE)
    return ipmi_locate_ctx_errmsg[errnum];
  else
    return ipmi_locate_ctx_errmsg[IPMI_LOCATE_CTX_ERR_ERRNUMRANGE];
}

int32_t
ipmi_locate_ctx_errnum(ipmi_locate_ctx_t ctx)
{
  if (!ctx)
    return (IPMI_LOCATE_CTX_ERR_NULL);
  else if (ctx->magic != IPMI_LOCATE_CTX_MAGIC)
    return (IPMI_LOCATE_CTX_ERR_INVALID);
  else
    return (ctx->errnum);
}

int 
ipmi_locate_get_device_info (ipmi_locate_ctx_t ctx,
                             ipmi_interface_type_t type,
                             struct ipmi_locate_info *info)
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
  struct ipmi_locate_info linfo;
  int i, rv;

  ERR(ctx && ctx->magic == IPMI_LOCATE_CTX_MAGIC);

#if 0
  /* LOCATE XXX */
  (IPMI_INTERFACE_TYPE_VALID(type) && info);
#endif
  
  for (i = 0; things_to_try[i] != NULL; i++)
    {
      memset (&linfo, 0, sizeof (struct ipmi_locate_info));
      rv = (*things_to_try[i])(ctx, type, &linfo);

      if (!rv)
	{
	  memcpy(info, &linfo, sizeof(struct ipmi_locate_info));
	  return 0;
	}
    }

  return (-1);
}
