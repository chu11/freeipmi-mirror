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
#include <assert.h>
#include <errno.h>

#include "freeipmi/cmds/ipmi-messaging-support-cmds.h"
#include "freeipmi/interface/ipmi-ipmb-interface.h"
#include "freeipmi/spec/ipmi-ipmb-lun-spec.h"
#include "freeipmi/spec/ipmi-netfn-spec.h"
#include "freeipmi/spec/ipmi-slave-address-spec.h"
#include "freeipmi/util/ipmi-util.h"

#include "libcommon/ipmi-err-wrappers.h"
#include "libcommon/ipmi-fiid-wrappers.h"

#include "freeipmi-portability.h"
#include "secure.h"

fiid_template_t tmpl_ipmb_msg_hdr_rq =
  {
    {8, "rs_addr", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "rs_lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {6, "net_fn", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "checksum1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "rq_addr", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "rq_lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {6, "rq_seq", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_ipmb_msg_hdr_rs =
  {
    {2, "rq_lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {6, "net_fn", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "checksum1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "rs_addr", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "rs_lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {6, "rq_seq", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_ipmb_msg_trlr = 
  {
    {8, "checksum2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_ipmb_msg = 
  {
    {2040, "data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_VARIABLE},
    {0, "", 0}
  };

#define IPMB_MAX_LEN 65536   /* udp max size */

int8_t 
fill_ipmb_msg_hdr (uint8_t rs_addr,
                   uint8_t net_fn, 
                   uint8_t rs_lun, 
                   uint8_t rq_addr,
                   uint8_t rq_lun,
                   uint8_t rq_seq, 
                   fiid_obj_t obj_ipmb_msg_hdr)
{
  uint8_t checksum_buf[1024];
  int32_t checksum_len;
  uint8_t checksum;

  ERR_EINVAL (IPMI_NET_FN_VALID(net_fn)
	      && IPMI_BMC_LUN_VALID(rs_lun)
	      && IPMI_BMC_LUN_VALID(rq_lun)
	      && !(rq_seq > IPMI_IPMB_REQUESTER_SEQUENCE_NUMBER_MAX)
	      && fiid_obj_valid(obj_ipmb_msg_hdr));

  FIID_OBJ_TEMPLATE_COMPARE(obj_ipmb_msg_hdr, tmpl_ipmb_msg_hdr_rq);

  FIID_OBJ_CLEAR (obj_ipmb_msg_hdr);
  FIID_OBJ_SET (obj_ipmb_msg_hdr, "rs_addr", rs_addr);
  FIID_OBJ_SET (obj_ipmb_msg_hdr, "net_fn", net_fn);
  FIID_OBJ_SET (obj_ipmb_msg_hdr, "rs_lun", rs_lun);
  
  FIID_OBJ_GET_BLOCK_LEN (checksum_len,
			  obj_ipmb_msg_hdr, 
			  "rs_addr", 
			  "net_fn", 
			  checksum_buf, 
			  1024);

  checksum = ipmi_checksum(checksum_buf, checksum_len);
  FIID_OBJ_SET (obj_ipmb_msg_hdr, "checksum1", checksum);
  FIID_OBJ_SET (obj_ipmb_msg_hdr, "rq_addr", rq_addr);
  FIID_OBJ_SET (obj_ipmb_msg_hdr, "rq_lun", rq_lun);
  FIID_OBJ_SET (obj_ipmb_msg_hdr, "rq_seq", rq_seq);

  return (0);
}

int32_t 
assemble_ipmi_ipmb_msg (fiid_obj_t obj_ipmb_msg_hdr,
                        fiid_obj_t obj_cmd,
                        fiid_obj_t obj_ipmb_msg)
{
  uint8_t buf[IPMB_MAX_LEN+1];
  uint32_t indx;
  uint8_t *checksum_data_ptr = NULL;
  uint32_t checksum_data_count = 0;
  int32_t len;
  int32_t required_len = 0;
  fiid_obj_t obj_ipmb_msg_trlr = NULL;
  uint8_t checksum;
  int32_t rv = -1;

  ERR_EINVAL (fiid_obj_valid(obj_ipmb_msg_hdr) 
	      && fiid_obj_valid(obj_cmd) 
              && fiid_obj_valid(obj_ipmb_msg));
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_ipmb_msg_hdr, tmpl_ipmb_msg_hdr_rq);
  FIID_OBJ_TEMPLATE_COMPARE(obj_ipmb_msg, tmpl_ipmb_msg);

  FIID_OBJ_PACKET_VALID(obj_ipmb_msg_hdr);
  FIID_OBJ_PACKET_VALID(obj_cmd);

  FIID_TEMPLATE_LEN_BYTES (len, tmpl_ipmb_msg_hdr_rq);
  required_len += len;
  
  FIID_OBJ_LEN_BYTES (len, obj_cmd);
  required_len += len;

  FIID_TEMPLATE_LEN_BYTES (len, tmpl_ipmb_msg_trlr);
  required_len += len;

  ERR_EMSGSIZE (!(IPMB_MAX_LEN < required_len));

  memset(buf, '\0', IPMB_MAX_LEN+1);

  indx = 0;

  FIID_OBJ_GET_BLOCK_LEN_CLEANUP(len,
				 obj_ipmb_msg_hdr,
				 "rs_addr",
				 "checksum1",
				 buf + indx,
				 IPMB_MAX_LEN - indx);
  indx += len;

  checksum_data_ptr = (buf + indx);

  FIID_OBJ_GET_BLOCK_LEN_CLEANUP(len,
				 obj_ipmb_msg_hdr,
				 "rq_addr",
				 "rq_seq",
				 buf + indx,
				 IPMB_MAX_LEN - indx);
  indx += len;
  checksum_data_count += len;

  FIID_OBJ_GET_ALL_LEN_CLEANUP (len, obj_cmd, buf + indx, IPMB_MAX_LEN - indx);
  indx += len;
  checksum_data_count += len;

  FIID_OBJ_CREATE_CLEANUP(obj_ipmb_msg_trlr, tmpl_ipmb_msg_trlr);

  checksum = ipmi_checksum (checksum_data_ptr, checksum_data_count);
  
  FIID_OBJ_SET_ALL_CLEANUP (obj_ipmb_msg_trlr, &checksum, sizeof(checksum));
  
  FIID_OBJ_GET_ALL_LEN_CLEANUP (len, obj_ipmb_msg_trlr, buf + indx, IPMB_MAX_LEN - indx);
  indx += len;

  FIID_OBJ_SET_ALL_LEN_CLEANUP(len, obj_ipmb_msg, buf, indx);

  rv = len;
 cleanup:
  FIID_OBJ_DESTROY(obj_ipmb_msg_trlr);
  return rv;
}

int8_t
unassemble_ipmi_ipmb_msg (fiid_obj_t obj_ipmb_msg,
                          fiid_obj_t obj_ipmb_msg_hdr,
                          fiid_obj_t obj_cmd,
                          fiid_obj_t obj_ipmb_msg_trlr)
{
  uint8_t buf[IPMB_MAX_LEN+1];
  int32_t buf_len;
  uint32_t indx;
  uint32_t ipmb_msg_len;
  int32_t obj_ipmb_msg_trlr_len;
  int32_t len;

  ERR_EINVAL (fiid_obj_valid(obj_ipmb_msg)
	      && fiid_obj_valid(obj_ipmb_msg_hdr) 
	      && fiid_obj_valid(obj_cmd)
	      && fiid_obj_valid(obj_ipmb_msg_trlr));

  FIID_OBJ_TEMPLATE_COMPARE(obj_ipmb_msg_hdr, tmpl_ipmb_msg_hdr_rs);
  FIID_OBJ_TEMPLATE_COMPARE(obj_ipmb_msg_trlr, tmpl_ipmb_msg_trlr);

  memset (buf, '\0', IPMB_MAX_LEN+1);

  FIID_OBJ_GET_ALL_LEN (buf_len, obj_ipmb_msg, buf, IPMB_MAX_LEN);

  indx = 0;

  FIID_OBJ_CLEAR(obj_ipmb_msg_hdr);
  FIID_OBJ_SET_ALL_LEN(len, obj_ipmb_msg_hdr, buf + indx, buf_len - indx);
  indx += len;
  
  if (buf_len <= indx)
    return 0;
  
  FIID_TEMPLATE_LEN_BYTES (obj_ipmb_msg_trlr_len, tmpl_ipmb_msg_trlr);
  
  if ((buf_len - indx) >= obj_ipmb_msg_trlr_len)
    ipmb_msg_len = (buf_len - indx) - obj_ipmb_msg_trlr_len;
  else if ((buf_len - indx) < obj_ipmb_msg_trlr_len)
    ipmb_msg_len = 0;
  
  if (ipmb_msg_len)
    {
      FIID_OBJ_CLEAR(obj_cmd);
      FIID_OBJ_SET_ALL_LEN(len, obj_cmd, buf + indx, ipmb_msg_len);
      indx += len;
      
      if (buf_len <= indx)
	return 0;
    }
  
  FIID_OBJ_CLEAR(obj_ipmb_msg_trlr);
  FIID_OBJ_SET_ALL_LEN(len, obj_ipmb_msg_trlr, buf + indx, buf_len - indx);
  indx += len;
  
  return 0;
}
