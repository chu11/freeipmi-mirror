/* 
   Copyright (C) 2003-2008 FreeIPMI Core Team

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

#include "freeipmi/interface/ipmi-kcs-interface.h"
#include "freeipmi/spec/ipmi-ipmb-lun-spec.h"
#include "freeipmi/spec/ipmi-netfn-spec.h"

#include "libcommon/ipmi-err-wrappers.h"
#include "libcommon/ipmi-fiid-wrappers.h"

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
		   fiid_obj_t obj_kcs_hdr)
{
  ERR_EINVAL (IPMI_BMC_LUN_VALID(lun)
	      && IPMI_NET_FN_VALID(fn)
	      && fiid_obj_valid(obj_kcs_hdr));

  FIID_OBJ_TEMPLATE_COMPARE(obj_kcs_hdr, tmpl_hdr_kcs);

  FIID_OBJ_CLEAR (obj_kcs_hdr);
  FIID_OBJ_SET (obj_kcs_hdr, "lun", lun);
  FIID_OBJ_SET (obj_kcs_hdr, "net_fn", fn);
  return 0;
}

int32_t 
assemble_ipmi_kcs_pkt (fiid_obj_t obj_kcs_hdr, 
		       fiid_obj_t obj_cmd, 
		       uint8_t *pkt, 
		       uint32_t pkt_len)
{
  int32_t obj_cmd_len, obj_kcs_hdr_len;

  ERR_EINVAL (fiid_obj_valid(obj_kcs_hdr)
	      && fiid_obj_valid(obj_cmd)
	      && pkt);

  FIID_OBJ_TEMPLATE_COMPARE(obj_kcs_hdr, tmpl_hdr_kcs);
  FIID_OBJ_PACKET_VALID(obj_kcs_hdr);
  FIID_OBJ_PACKET_VALID(obj_cmd);

  FIID_OBJ_LEN_BYTES (obj_kcs_hdr_len, obj_kcs_hdr);
  FIID_OBJ_LEN_BYTES (obj_cmd_len, obj_cmd);

  ERR_EMSGSIZE (!(pkt_len < (obj_kcs_hdr_len + obj_cmd_len)));

  memset (pkt, 0, pkt_len);
  FIID_OBJ_GET_ALL_LEN (obj_kcs_hdr_len, obj_kcs_hdr, pkt, pkt_len);
  FIID_OBJ_GET_ALL_LEN (obj_cmd_len, obj_cmd, pkt + obj_kcs_hdr_len, pkt_len - obj_kcs_hdr_len);
  return (obj_kcs_hdr_len + obj_cmd_len);
}

int32_t 
unassemble_ipmi_kcs_pkt (uint8_t *pkt, 
			 uint32_t pkt_len, 
			 fiid_obj_t obj_kcs_hdr, 
			 fiid_obj_t obj_cmd)
{
  uint32_t indx = 0;
  int32_t len;

  ERR_EINVAL (pkt
	      && fiid_obj_valid(obj_kcs_hdr)
	      && fiid_obj_valid(obj_cmd));

  FIID_OBJ_TEMPLATE_COMPARE(obj_kcs_hdr, tmpl_hdr_kcs);

  FIID_OBJ_SET_ALL_LEN (len, obj_kcs_hdr, pkt + indx, pkt_len - indx);
  indx += len;

  if (pkt_len <= indx)
    return 0;

  FIID_OBJ_SET_ALL_LEN (len, obj_cmd, pkt + indx, pkt_len - indx);
  indx += len;

  return 0;
}
