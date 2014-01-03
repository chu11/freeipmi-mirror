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
#include <limits.h>
#include <errno.h>

#include "freeipmi/interface/ipmi-kcs-interface.h"
#include "freeipmi/interface/ipmi-interface.h"
#include "freeipmi/fiid/fiid.h"
#include "freeipmi/spec/ipmi-ipmb-lun-spec.h"
#include "freeipmi/spec/ipmi-netfn-spec.h"

#include "libcommon/ipmi-fiid-util.h"
#include "libcommon/ipmi-fill-util.h"
#include "libcommon/ipmi-trace.h"

#include "freeipmi-portability.h"

fiid_template_t tmpl_hdr_kcs =
  {
    { 2, "lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "net_fn", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

int
fill_hdr_ipmi_kcs (uint8_t lun,
                   uint8_t fn,
                   fiid_obj_t obj_kcs_hdr)
{
  if (!IPMI_BMC_LUN_VALID (lun)
      || !IPMI_NET_FN_VALID (fn)
      || !fiid_obj_valid (obj_kcs_hdr))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_kcs_hdr, tmpl_hdr_kcs) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_kcs_hdr);
  FILL_FIID_OBJ_SET (obj_kcs_hdr, "lun", lun);
  FILL_FIID_OBJ_SET (obj_kcs_hdr, "net_fn", fn);

  return (0);
}

int
assemble_ipmi_kcs_pkt (fiid_obj_t obj_kcs_hdr,
                       fiid_obj_t obj_cmd,
                       void *pkt,
                       unsigned int pkt_len,
		       unsigned int flags)
{
  int obj_cmd_len, obj_kcs_hdr_len;
  unsigned int utmp;
  unsigned int flags_mask = 0;

  if (!fiid_obj_valid (obj_kcs_hdr)
      || !fiid_obj_valid (obj_cmd)
      || !pkt
      || (flags & ~flags_mask))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_kcs_hdr, tmpl_hdr_kcs) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  if (FIID_OBJ_PACKET_VALID (obj_kcs_hdr) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_kcs_hdr);
      return (-1);
    }
  if (FIID_OBJ_PACKET_VALID (obj_cmd) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_cmd);
      return (-1);
    }

  if ((obj_kcs_hdr_len = fiid_obj_len_bytes (obj_kcs_hdr)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_kcs_hdr);
      return (-1);
    }
  if ((obj_cmd_len = fiid_obj_len_bytes (obj_cmd)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_cmd);
      return (-1);
    }

  /* int overflow not possible here */
  if (pkt_len < (obj_kcs_hdr_len + obj_cmd_len))
    {
      SET_ERRNO (EMSGSIZE);
      return (-1);
    }

  memset (pkt, 0, pkt_len);
  if ((obj_kcs_hdr_len = fiid_obj_get_all (obj_kcs_hdr,
                                           pkt,
                                           pkt_len)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_kcs_hdr);
      return (-1);
    }
  if ((obj_cmd_len = fiid_obj_get_all (obj_cmd,
                                       pkt + obj_kcs_hdr_len,
                                       pkt_len - obj_kcs_hdr_len)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_cmd);
      return (-1);
    }

  utmp = obj_kcs_hdr_len + obj_cmd_len;
  if (utmp > INT_MAX)
    {
      SET_ERRNO (EMSGSIZE);
      return (-1);
    }

  return (obj_kcs_hdr_len + obj_cmd_len);
}

int
unassemble_ipmi_kcs_pkt (const void *pkt,
                         unsigned int pkt_len,
                         fiid_obj_t obj_kcs_hdr,
                         fiid_obj_t obj_cmd,
			 unsigned int flags)
{
  unsigned int indx = 0;
  int len;
  unsigned int flags_mask = (IPMI_INTERFACE_FLAGS_NO_LEGAL_CHECK);

  if (!pkt
      || !fiid_obj_valid (obj_kcs_hdr)
      || !fiid_obj_valid (obj_cmd)
      || (flags & ~flags_mask))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_kcs_hdr, tmpl_hdr_kcs) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  if (fiid_obj_clear (obj_kcs_hdr) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_kcs_hdr);
      return (-1);
    }

  if (fiid_obj_clear (obj_cmd) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_cmd);
      return (-1);
    }

  if ((len = fiid_obj_set_all (obj_kcs_hdr, pkt + indx, pkt_len - indx)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_kcs_hdr);
      return (-1);
    }
  indx += len;

  if (pkt_len <= indx)
    {
      /* cannot parse packet */
      ERR_TRACE ("malformed packet", EINVAL);
      return (0);
    }

  if ((len = fiid_obj_set_all (obj_cmd, pkt + indx, pkt_len - indx)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_cmd);
      return (-1);
    }
  indx += len;

  if (FIID_OBJ_PACKET_VALID (obj_kcs_hdr) == 1
      && ((flags & IPMI_INTERFACE_FLAGS_NO_LEGAL_CHECK) || FIID_OBJ_PACKET_SUFFICIENT (obj_cmd) == 1))
    return (1);

  return (0);
}
