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
#ifdef STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <errno.h>

#include "freeipmi/api/ipmi-api.h"
#include "freeipmi/locate/ipmi-locate.h"
#include "freeipmi/spec/ipmi-comp-code-spec.h"
#include "freeipmi/driver/ipmi-kcs-driver.h"
#include "freeipmi/driver/ipmi-openipmi-driver.h"
#include "freeipmi/driver/ipmi-ssif-driver.h"
#include "freeipmi/driver/ipmi-sunbmc-driver.h"
#include "freeipmi/util/ipmi-util.h"

#include "ipmi-ctx.h"
#include "ipmi-err-wrappers-api.h"

#include "freeipmi-portability.h"

int
api_fiid_obj_template_compare (ipmi_ctx_t ctx, fiid_obj_t obj, fiid_template_t tmpl)
{
  int ret;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    return (-1);

  if (!fiid_obj_valid(obj_cmd_rs))
    {
      API_SET_ERRNUM(IPMI_ERR_INTERNAL_ERROR);
      return (-1);
    }

  if (!fiid_obj_valid(obj_cmd_rs))
    {
      API_SET_ERRNUM(IPMI_ERR_INTERNAL_ERROR);
      return (-1);
    }
 
  if ((ret = fiid_obj_template_compare (obj, tmpl)) < 0)
    {
      API_ERRNO_TO_API_ERRNUM(ctx, errno);
      return (-1);
    }

  if (!__ret)
    {
      ctx->errnum = IPMI_ERR_PARAMETERS;
      return (-1);
    }

  return (0);
}
