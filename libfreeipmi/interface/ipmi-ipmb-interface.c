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
#if HAVE_ALLOCA_H
#include <alloca.h>
#endif /* HAVE_ALLOCA_H */
#include <assert.h>
#include <errno.h>

#include "freeipmi/cmds/ipmi-messaging-support-cmds.h"
#include "freeipmi/fiid/fiid.h"
#include "freeipmi/interface/ipmi-interface.h"
#include "freeipmi/interface/ipmi-ipmb-interface.h"
#include "freeipmi/spec/ipmi-ipmb-lun-spec.h"
#include "freeipmi/spec/ipmi-netfn-spec.h"
#include "freeipmi/spec/ipmi-slave-address-spec.h"
#include "freeipmi/util/ipmi-util.h"

#include "libcommon/ipmi-fiid-util.h"
#include "libcommon/ipmi-fill-util.h"
#include "libcommon/ipmi-trace.h"

#include "freeipmi-portability.h"
#include "secure.h"

fiid_template_t tmpl_ipmb_msg_hdr_rq =
  {
    { 8, "rs_addr", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "rs_lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "net_fn", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "checksum1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "rq_addr", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "rq_lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "rq_seq", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_ipmb_msg_hdr_rs =
  {
    { 2, "rq_lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "net_fn", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "checksum1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "rs_addr", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "rs_lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "rq_seq", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_ipmb_msg_trlr =
  {
    { 8, "checksum2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_ipmb_msg =
  {
    { 2040, "data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

#define IPMB_MAX_LEN 65536   /* udp max size */

int
fill_ipmb_msg_hdr (uint8_t rs_addr,
                   uint8_t net_fn,
                   uint8_t rs_lun,
                   uint8_t rq_addr,
                   uint8_t rq_lun,
                   uint8_t rq_seq,
                   fiid_obj_t obj_ipmb_msg_hdr)
{
  uint8_t checksum_buf[1024];
  int checksum_len;
  uint8_t checksum;

  if (!IPMI_NET_FN_VALID (net_fn)
      || !IPMI_BMC_LUN_VALID (rs_lun)
      || !IPMI_BMC_LUN_VALID (rq_lun)
      || (rq_seq > IPMI_IPMB_REQUESTER_SEQUENCE_NUMBER_MAX)
      || !fiid_obj_valid (obj_ipmb_msg_hdr))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_ipmb_msg_hdr, tmpl_ipmb_msg_hdr_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_ipmb_msg_hdr);
  FILL_FIID_OBJ_SET (obj_ipmb_msg_hdr, "rs_addr", rs_addr);
  FILL_FIID_OBJ_SET (obj_ipmb_msg_hdr, "net_fn", net_fn);
  FILL_FIID_OBJ_SET (obj_ipmb_msg_hdr, "rs_lun", rs_lun);

  if ((checksum_len = fiid_obj_get_block (obj_ipmb_msg_hdr,
                                          "rs_addr",
                                          "net_fn",
                                          checksum_buf,
                                          1024)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_ipmb_msg_hdr);
      return (-1);
    }

  checksum = ipmi_checksum (checksum_buf, checksum_len);
  FILL_FIID_OBJ_SET (obj_ipmb_msg_hdr, "checksum1", checksum);
  FILL_FIID_OBJ_SET (obj_ipmb_msg_hdr, "rq_addr", rq_addr);
  FILL_FIID_OBJ_SET (obj_ipmb_msg_hdr, "rq_lun", rq_lun);
  FILL_FIID_OBJ_SET (obj_ipmb_msg_hdr, "rq_seq", rq_seq);

  return (0);
}

int
assemble_ipmi_ipmb_msg (fiid_obj_t obj_ipmb_msg_hdr,
                        fiid_obj_t obj_cmd,
                        fiid_obj_t obj_ipmb_msg,
			unsigned int flags)
{
  uint8_t buf[IPMB_MAX_LEN+1];
  unsigned int indx = 0;
  uint8_t *checksum_data_ptr = NULL;
  unsigned int checksum_data_count = 0;
  unsigned int required_len = 0;
  fiid_obj_t obj_ipmb_msg_trlr = NULL;
  uint8_t checksum;
  int len, rv = -1;
  unsigned int flags_mask = 0;

  if (!fiid_obj_valid (obj_ipmb_msg_hdr)
      || !fiid_obj_valid (obj_cmd)
      || !fiid_obj_valid (obj_ipmb_msg)
      || (flags & ~flags_mask))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_ipmb_msg_hdr, tmpl_ipmb_msg_hdr_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }
  if (FIID_OBJ_TEMPLATE_COMPARE (obj_ipmb_msg, tmpl_ipmb_msg) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  if (FIID_OBJ_PACKET_VALID (obj_ipmb_msg_hdr) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_ipmb_msg_hdr);
      return (-1);
    }
  if (FIID_OBJ_PACKET_VALID (obj_cmd) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_cmd);
      return (-1);
    }

  if ((len = fiid_template_len_bytes (tmpl_ipmb_msg_hdr_rq)) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }
  required_len += len;

  if ((len = fiid_obj_len_bytes (obj_cmd)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_cmd);
      return (-1);
    }
  required_len += len;

  if ((len = fiid_template_len_bytes (tmpl_ipmb_msg_trlr)) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }
  required_len += len;

  if (IPMB_MAX_LEN < required_len)
    {
      SET_ERRNO (EMSGSIZE);
      return (-1);
    }

  memset (buf, '\0', IPMB_MAX_LEN+1);

  if ((len = fiid_obj_get_block (obj_ipmb_msg_hdr,
                                 "rs_addr",
                                 "checksum1",
                                 buf + indx,
                                 IPMB_MAX_LEN - indx)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_ipmb_msg_hdr);
      goto cleanup;
    }
  indx += len;

  checksum_data_ptr = (buf + indx);

  if ((len = fiid_obj_get_block (obj_ipmb_msg_hdr,
                                 "rq_addr",
                                 "rq_seq",
                                 buf + indx,
                                 IPMB_MAX_LEN - indx)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_ipmb_msg_hdr);
      goto cleanup;
    }
  indx += len;
  checksum_data_count += len;

  if ((len = fiid_obj_get_all (obj_cmd, buf + indx, IPMB_MAX_LEN - indx)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_cmd);
      goto cleanup;
    }
  indx += len;
  checksum_data_count += len;

  if (!(obj_ipmb_msg_trlr = fiid_obj_create (tmpl_ipmb_msg_trlr)))
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  checksum = ipmi_checksum (checksum_data_ptr, checksum_data_count);

  if (fiid_obj_set_all (obj_ipmb_msg_trlr, &checksum, sizeof (checksum)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_ipmb_msg_trlr);
      goto cleanup;
    }

  if ((len = fiid_obj_get_all (obj_ipmb_msg_trlr, buf + indx, IPMB_MAX_LEN - indx)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_ipmb_msg_trlr);
      goto cleanup;
    }
  indx += len;

  if ((len = fiid_obj_set_all (obj_ipmb_msg, buf, indx)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_ipmb_msg);
      goto cleanup;
    }

  rv = len;
 cleanup:
  fiid_obj_destroy (obj_ipmb_msg_trlr);
  return (rv);
}

int
unassemble_ipmi_ipmb_msg (fiid_obj_t obj_ipmb_msg,
                          fiid_obj_t obj_ipmb_msg_hdr,
                          fiid_obj_t obj_cmd,
                          fiid_obj_t obj_ipmb_msg_trlr,
			  unsigned int flags)
{
  uint8_t buf[IPMB_MAX_LEN+1];
  int buf_len, obj_ipmb_msg_trlr_len, len;
  unsigned int indx = 0;
  unsigned int ipmb_msg_len;
  unsigned int flags_mask = (IPMI_INTERFACE_FLAGS_NO_LEGAL_CHECK);

  if (!fiid_obj_valid (obj_ipmb_msg)
      || !fiid_obj_valid (obj_ipmb_msg_hdr)
      || !fiid_obj_valid (obj_cmd)
      || !fiid_obj_valid (obj_ipmb_msg_trlr)
      || (flags & ~flags_mask))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_ipmb_msg_hdr, tmpl_ipmb_msg_hdr_rs) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }
  if (FIID_OBJ_TEMPLATE_COMPARE (obj_ipmb_msg_trlr, tmpl_ipmb_msg_trlr) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  memset (buf, '\0', IPMB_MAX_LEN+1);

  if ((buf_len = fiid_obj_get_all (obj_ipmb_msg, buf, IPMB_MAX_LEN)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_ipmb_msg);
      return (-1);
    }

  if (fiid_obj_clear (obj_ipmb_msg_hdr) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_ipmb_msg_hdr);
      return (-1);
    }

  if (fiid_obj_clear (obj_cmd) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_cmd);
      return (-1);
    }
  
  if (fiid_obj_clear (obj_ipmb_msg_trlr) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_ipmb_msg_trlr);
      return (-1);
    }

  if ((len = fiid_obj_set_all (obj_ipmb_msg_hdr, buf + indx, buf_len - indx)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_ipmb_msg_hdr);
      return (-1);
    }
  indx += len;
  
  if (buf_len <= indx)
    {
      /* cannot parse packet */
      ERR_TRACE ("malformed packet", EINVAL);
      return (0);
    }
  
  if ((obj_ipmb_msg_trlr_len = fiid_template_len_bytes (tmpl_ipmb_msg_trlr)) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }
  
  if ((buf_len - indx) <= obj_ipmb_msg_trlr_len)
    {
      /* cannot parse packet */
      ERR_TRACE ("malformed packet", EINVAL);
      return (0);
    }

  ipmb_msg_len = (buf_len - indx) - obj_ipmb_msg_trlr_len;

  if ((len = fiid_obj_set_all (obj_cmd, buf + indx, ipmb_msg_len)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_cmd);
      return (-1);
    }
  indx += len;
      
  if (buf_len <= indx)
    {
      /* cannot parse packet */
      ERR_TRACE ("malformed packet", EINVAL);
      return (0);
    }

  if ((len = fiid_obj_set_all (obj_ipmb_msg_trlr, buf + indx, buf_len - indx)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_ipmb_msg_trlr);
      return (-1);
    }
  indx += len;
  
  if (FIID_OBJ_PACKET_VALID (obj_ipmb_msg_hdr) == 1
      && ((flags & IPMI_INTERFACE_FLAGS_NO_LEGAL_CHECK) || FIID_OBJ_PACKET_SUFFICIENT (obj_cmd) == 1)
      && FIID_OBJ_PACKET_VALID (obj_ipmb_msg_trlr) == 1)
    return (1);

  return (0);
}
