/* 
   ipmi-kcs.c: IPMI KCS

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
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.  

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#ifdef STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <errno.h>

#include "freeipmi/ipmi-kcs.h"
#include "freeipmi/ipmi-ipmb-interface.h"
#include "freeipmi/ipmi-netfn-spec.h"

#include "fiid-wrappers.h"
#include "freeipmi-portability.h"

fiid_template_t tmpl_hdr_kcs =
  {
    {2, "lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {6, "net_fn", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

int8_t
fill_hdr_ipmi_kcs (uint8_t lun, 
		   uint8_t fn, 
		   fiid_obj_t obj_hdr)
{
  if (!IPMI_BMC_LUN_VALID(lun)
      || !IPMI_NET_FN_VALID(fn)
      || !fiid_obj_valid(obj_hdr))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_TEMPLATE_COMPARE(obj_hdr, tmpl_hdr_kcs);

  FIID_OBJ_SET (obj_hdr, (uint8_t *)"lun", lun);
  FIID_OBJ_SET (obj_hdr, (uint8_t *)"net_fn", fn);
  return 0;
}

int32_t 
assemble_ipmi_kcs_pkt (fiid_obj_t obj_hdr, 
		       fiid_obj_t obj_cmd, 
		       uint8_t *pkt, 
		       uint32_t pkt_len)
{
  int32_t obj_cmd_len, obj_hdr_len;

  if (!(fiid_obj_valid(obj_hdr)
        && fiid_obj_valid(obj_cmd)
        && pkt))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_TEMPLATE_COMPARE(obj_hdr, tmpl_hdr_kcs);
  FIID_OBJ_PACKET_VALID(obj_hdr);
  FIID_OBJ_PACKET_VALID(obj_cmd);

  FIID_OBJ_LEN_BYTES (obj_hdr_len, obj_hdr);
  FIID_OBJ_LEN_BYTES (obj_cmd_len, obj_cmd);

  if (pkt_len < (obj_hdr_len + obj_cmd_len))
    {
      errno = EMSGSIZE;
      return (-1);
    }

  memset (pkt, 0, pkt_len);
  FIID_OBJ_GET_ALL_LEN (obj_hdr_len, obj_hdr, pkt, pkt_len);
  FIID_OBJ_GET_ALL_LEN (obj_cmd_len, obj_cmd, pkt + obj_hdr_len, pkt_len - obj_hdr_len);
  return (obj_hdr_len + obj_cmd_len);
}

int32_t 
unassemble_ipmi_kcs_pkt (uint8_t *pkt, 
			 uint32_t pkt_len, 
			 fiid_obj_t obj_hdr, 
			 fiid_obj_t obj_cmd)
{
  uint32_t indx = 0;
  int32_t len;

  if (!(pkt
        && fiid_obj_valid(obj_hdr)
        && fiid_obj_valid(obj_cmd)))
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_TEMPLATE_COMPARE(obj_hdr, tmpl_hdr_kcs);

  FIID_OBJ_SET_ALL_LEN (len, obj_hdr, pkt + indx, pkt_len - indx);
  indx += len;

  if (pkt_len <= indx)
    return 0;

  FIID_OBJ_SET_ALL_LEN (len, obj_cmd, pkt + indx, pkt_len - indx);
  indx += len;

  return 0;
}
