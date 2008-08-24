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
#if HAVE_ALLOCA_H
#include <alloca.h>
#endif /* HAVE_ALLOCA_H */
#include <errno.h>

#include "freeipmi/util/ipmi-ipmb-util.h"
#include "freeipmi/interface/ipmi-ipmb-interface.h"
#include "freeipmi/util/ipmi-util.h"

#include "libcommon/ipmi-err-wrappers.h"
#include "libcommon/ipmi-fiid-wrappers.h"

#include "freeipmi-portability.h"
#include "secure.h"

int8_t 
ipmi_ipmb_check_rq_seq (fiid_obj_t obj_ipmb_msg_hdr, uint8_t rq_seq)
{
  uint64_t rq_seq_recv;
  int32_t len;

  ERR_EINVAL (fiid_obj_valid(obj_ipmb_msg_hdr));
  
  FIID_OBJ_FIELD_LOOKUP (obj_ipmb_msg_hdr, "rq_seq");
  
  FIID_OBJ_FIELD_LEN (len, obj_ipmb_msg_hdr, "rq_seq");
  ERR_EINVAL (len);
  
  FIID_OBJ_GET(obj_ipmb_msg_hdr, "rq_seq", &rq_seq_recv);
  
  return ((((uint8_t)rq_seq_recv) == rq_seq) ? 1 : 0);
}

int8_t
ipmi_ipmb_check_checksum (uint8_t rq_addr,
			  fiid_obj_t obj_ipmb_msg_hdr,
			  fiid_obj_t obj_cmd,
			  fiid_obj_t obj_ipmb_msg_trlr)
{
  int32_t obj_ipmb_msg_hdr_len, obj_cmd_len, obj_len, len, req_len;
  uint8_t checksum1_recv, checksum1_calc, checksum2_recv, checksum2_calc;
  uint8_t *buf = NULL;
  uint32_t buflen;
  uint64_t val;
  
  ERR_EINVAL (fiid_obj_valid(obj_ipmb_msg_hdr)
              && fiid_obj_valid(obj_cmd)
              && fiid_obj_valid(obj_ipmb_msg_trlr));
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_ipmb_msg_hdr, tmpl_ipmb_msg_hdr_rs);
  FIID_OBJ_TEMPLATE_COMPARE(obj_ipmb_msg_trlr, tmpl_ipmb_msg_trlr);

  FIID_OBJ_FIELD_LEN (len, obj_ipmb_msg_hdr, "checksum1");
  FIID_TEMPLATE_FIELD_LEN(req_len, tmpl_ipmb_msg_hdr_rs, "checksum1");
  ERR_EINVAL (len == req_len);

  FIID_OBJ_FIELD_LEN (len, obj_ipmb_msg_trlr, "checksum2");
  FIID_TEMPLATE_FIELD_LEN(req_len, tmpl_ipmb_msg_trlr, "checksum2");
  ERR_EINVAL (len == req_len);

  FIID_OBJ_LEN_BYTES (obj_ipmb_msg_hdr_len, obj_ipmb_msg_hdr);
  FIID_OBJ_LEN_BYTES (obj_cmd_len, obj_cmd);

  FIID_OBJ_GET (obj_ipmb_msg_hdr, "checksum1", &val);
  checksum1_recv = val;

  ERR ((buf = (uint8_t *)alloca(obj_ipmb_msg_hdr_len + 1)));

  /* achu: The rq_addr isn't in the ipmb_msg_hdr response, but it's
   * part of the calculated checksum stored in the header.  If you're
   * thinking that's dumb.  I think so too.
   */
  buf[0] = rq_addr;
  
  FIID_OBJ_GET_BLOCK_LEN(len, obj_ipmb_msg_hdr, "rq_lun", "net_fn", buf + 1, obj_ipmb_msg_hdr_len);
  checksum1_calc = ipmi_checksum(buf, len + 1);

  if (checksum1_recv != checksum1_calc)
    return (0);

  FIID_OBJ_GET (obj_ipmb_msg_trlr, "checksum2", &val);
  checksum2_recv = val;

  buflen = obj_ipmb_msg_hdr_len + obj_cmd_len;
  ERR ((buf = (uint8_t *)alloca(buflen)));

  len = 0;
  FIID_OBJ_GET_BLOCK_LEN(obj_len, obj_ipmb_msg_hdr, "rs_addr", "rq_seq", buf, buflen - len);
  len += obj_len;
  FIID_OBJ_GET_ALL_LEN(obj_len, obj_cmd, buf + len, buflen - len);
  len += obj_len;

  checksum2_calc = ipmi_checksum(buf, len);

  if (checksum2_recv != checksum2_calc)
    return (0);

  return (1);
}
