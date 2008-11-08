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
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#if HAVE_INTTYPES_H
#include <inttypes.h>
#endif /* HAVE_INTTYPES_H */
#include <errno.h>

#include "freeipmi/debug/ipmi-debug.h"

#include "ipmi-debug-common.h"

#include "libcommon/ipmi-bit-ops.h"
#include "libcommon/ipmi-err-wrappers.h"
#include "libcommon/ipmi-fiid-wrappers.h"
#include "freeipmi/interface/ipmi-ipmb-interface.h"
#include "freeipmi/cmds/ipmi-messaging-support-cmds.h"

#include "freeipmi-portability.h"

#define IPMI_DEBUG_MAX_BUF_LEN        65536
#define IPMI_DEBUG_MAX_PKT_LEN        65536
#define IPMI_DEBUG_CHAR_PER_LINE          8
#define IPMI_DEBUG_DEFAULT_FD STDERR_FILENO

#define IPMI_DEBUG_MAX_UNEXPECTED_BYTES 65536
#define IPMI_DEBUG_MAX_UNEXPECTED_BITS  (IPMI_DEBUG_MAX_UNEXPECTED_BYTES*8)

fiid_template_t tmpl_unexpected_data =
  {
    {IPMI_DEBUG_MAX_UNEXPECTED_BITS, "unexpected_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

int8_t
ipmi_obj_dump (int fd,
               const char *prefix, 
               const char *hdr,
               const char *trlr, 
               fiid_obj_t obj)
{
#if 0
  char *default_hdr = 
    "================================================================\n"
    "[ VALUE               TAG NAME:LENGTH                          ]\n"
    "================================================================";
  char *default_trlr = 
    "================================================================";
#endif
  char prefix_buf[IPMI_DEBUG_MAX_PREFIX_LEN];
  fiid_iterator_t iter = NULL;
  int rv = -1;

  ERR_EINVAL (fiid_obj_valid(obj));
  
  ERR(!(ipmi_debug_set_prefix (prefix_buf, IPMI_DEBUG_MAX_PREFIX_LEN, prefix) < 0));

  ERR(!(ipmi_debug_output_str (fd, prefix_buf, hdr) < 0));

  FIID_ITERATOR_CREATE (iter, obj);

  while (!fiid_iterator_end(iter))
    {
      int32_t field_len;
      char *key = NULL;

      FIID_ITERATOR_KEY_CLEANUP(key, iter);

      FIID_ITERATOR_FIELD_LEN_CLEANUP(field_len, iter);
    
      if (!field_len)
        {
          fiid_iterator_next(iter);
          continue;
        }

      if (prefix)
        IPMI_DEBUG_DPRINTF_CLEANUP ((fd, "%s", prefix_buf));

      if (field_len <= 64)
        {
          uint64_t val = 0;

	  FIID_ITERATOR_GET_CLEANUP (iter, &val);

          IPMI_DEBUG_DPRINTF_CLEANUP ((fd, "[%16"PRIX64"h] = %s[%2db]\n", (uint64_t) val, key, field_len));
        }
      else
        {
          uint8_t buf[IPMI_DEBUG_MAX_BUF_LEN];
          int len;

          IPMI_DEBUG_DPRINTF_CLEANUP ((fd, "[  BYTE ARRAY ... ] = %s[%2dB]\n", key, BITS_ROUND_BYTES(field_len)));

	  FIID_ITERATOR_GET_DATA_LEN_CLEANUP(len, iter, buf, IPMI_DEBUG_MAX_BUF_LEN);
       
          ERR_CLEANUP (!(ipmi_debug_output_byte_array(fd, prefix_buf, buf, len) < 0));
        }

      fiid_iterator_next(iter);
    }

  ERR_CLEANUP (!(ipmi_debug_output_str(fd, prefix_buf, trlr) < 0));

  rv = 0;
 cleanup:
  if (iter)
    fiid_iterator_destroy(iter);
  return (rv);
}

int8_t
ipmi_obj_dump_ipmb (int fd,
                    const char *prefix, 
                    const char *hdr,
                    const char *trlr, 
                    fiid_obj_t obj,
                    fiid_template_t tmpl_ipmb_msg_hdr,
                    fiid_template_t tmpl_ipmb_cmd)
{
  char *cmd_hdr =
    "IPMI Command Data:\n"
    "------------------";
  char *ipmb_msg_hdr =
    "IPMB Message Header:\n"
    "--------------------";
  char *ipmb_cmd_hdr =
    "IPMB Message Data:\n"
    "------------------";
  char *ipmb_msg_trlr_hdr =
    "IPMB Message Trailer:\n"
    "---------------------";
  char prefix_buf[IPMI_DEBUG_MAX_PREFIX_LEN];
  uint8_t ipmb_buf[IPMI_DEBUG_MAX_PKT_LEN];
  int32_t ipmb_buf_len;
  fiid_obj_t obj_cmd = NULL;
  fiid_obj_t obj_ipmb_msg_hdr = NULL;
  fiid_obj_t obj_ipmb_cmd = NULL;
  fiid_obj_t obj_ipmb_msg_trlr = NULL;
  int32_t obj_ipmb_msg_trlr_len = 0;
  int32_t obj_ipmb_cmd_len = 0;
  int32_t ipmb_hdr_len = 0;
  int32_t ipmb_cmd_len = 0;
  int ret1, ret2;
  int rv = -1;

  ERR_EINVAL (fiid_obj_valid(obj) 
              && tmpl_ipmb_msg_hdr
              && tmpl_ipmb_cmd);

  if ((ret1 = fiid_obj_template_compare(obj, tmpl_cmd_send_message_rq)) < 0)
    return -1;

  if ((ret2 = fiid_obj_template_compare(obj, tmpl_cmd_get_message_rs)) < 0)
    return -1;

  ERR_EINVAL ((ret1 || ret2));
  
  ERR(!(ipmi_debug_set_prefix (prefix_buf, IPMI_DEBUG_MAX_PREFIX_LEN, prefix) < 0));

  ERR(!(ipmi_debug_output_str (fd, prefix_buf, hdr) < 0));

  FIID_OBJ_DUP_CLEANUP (obj_cmd, obj);
  FIID_OBJ_CREATE_CLEANUP (obj_ipmb_msg_hdr, tmpl_ipmb_msg_hdr);
  FIID_OBJ_CREATE_CLEANUP (obj_ipmb_cmd, tmpl_ipmb_cmd);
  FIID_OBJ_CREATE_CLEANUP (obj_ipmb_msg_trlr, tmpl_ipmb_msg_trlr);
  
  memset(ipmb_buf, '\0', IPMI_DEBUG_MAX_PKT_LEN);
  FIID_OBJ_GET_DATA_LEN_CLEANUP (ipmb_buf_len,
                                 obj_cmd,
                                 "message_data",
                                 ipmb_buf,
                                 IPMI_DEBUG_MAX_PKT_LEN);
  
  FIID_OBJ_CLEAR_FIELD_CLEANUP (obj_cmd, "message_data");
  
  ERR_CLEANUP (!(ipmi_obj_dump(fd,
                               prefix,
                               cmd_hdr,
                               NULL,
                               obj_cmd) < 0));

  if (ipmb_buf_len)
    {
      FIID_TEMPLATE_LEN_BYTES (obj_ipmb_msg_trlr_len, tmpl_ipmb_msg_trlr);

      FIID_OBJ_SET_ALL_LEN_CLEANUP (ipmb_hdr_len,
				    obj_ipmb_msg_hdr,
				    ipmb_buf,
				    ipmb_buf_len);

      ERR_CLEANUP (!(ipmi_obj_dump(fd,
				   prefix,
				   ipmb_msg_hdr,
				   NULL,
				   obj_ipmb_msg_hdr) < 0));
      
      if ((ipmb_buf_len - ipmb_hdr_len) >= obj_ipmb_msg_trlr_len)
        obj_ipmb_cmd_len = (ipmb_buf_len - ipmb_hdr_len) - obj_ipmb_msg_trlr_len;
      else
        obj_ipmb_cmd_len = 0;
      
      if (obj_ipmb_cmd_len)
        {
          FIID_OBJ_SET_ALL_LEN_CLEANUP (ipmb_cmd_len,
                                        obj_ipmb_cmd,
                                        ipmb_buf + ipmb_hdr_len,
                                        obj_ipmb_cmd_len);
          
          ERR_CLEANUP (!(ipmi_obj_dump(fd,
                                       prefix,
                                       ipmb_cmd_hdr,
                                       NULL,
                                       obj_ipmb_cmd) < 0));
        }
      
      FIID_OBJ_SET_ALL_CLEANUP (obj_ipmb_msg_trlr,
                                ipmb_buf + ipmb_hdr_len + ipmb_cmd_len,
                                (ipmb_buf_len - ipmb_hdr_len - ipmb_cmd_len));
      
      ERR_CLEANUP (!(ipmi_obj_dump(fd,
                                   prefix,
                                   ipmb_msg_trlr_hdr,
                                   NULL,
                                   obj_ipmb_msg_trlr) < 0));
    }


  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY(obj_cmd);
  FIID_OBJ_DESTROY(obj_ipmb_msg_hdr);
  FIID_OBJ_DESTROY(obj_ipmb_cmd);
  FIID_OBJ_DESTROY(obj_ipmb_msg_trlr);
  return (rv);
}
