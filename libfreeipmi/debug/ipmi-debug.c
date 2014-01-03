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
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#if HAVE_INTTYPES_H
#include <inttypes.h>
#endif /* HAVE_INTTYPES_H */
#include <errno.h>

#include "freeipmi/debug/ipmi-debug.h"
#include "freeipmi/cmds/ipmi-messaging-support-cmds.h"
#include "freeipmi/fiid/fiid.h"
#include "freeipmi/interface/ipmi-ipmb-interface.h"

#include "libcommon/ipmi-bit-ops.h"
#include "libcommon/ipmi-fiid-util.h"
#include "libcommon/ipmi-fill-util.h"
#include "libcommon/ipmi-trace.h"

#include "ipmi-debug-common.h"

#include "freeipmi-portability.h"

#define IPMI_DEBUG_MAX_BUF_LEN        65536
#define IPMI_DEBUG_MAX_PKT_LEN        65536
#define IPMI_DEBUG_CHAR_PER_LINE          8
#define IPMI_DEBUG_DEFAULT_FD STDERR_FILENO

#define IPMI_DEBUG_MAX_UNEXPECTED_BYTES 65536
#define IPMI_DEBUG_MAX_UNEXPECTED_BITS  (IPMI_DEBUG_MAX_UNEXPECTED_BYTES*8)

fiid_template_t tmpl_unexpected_data =
  {
    { IPMI_DEBUG_MAX_UNEXPECTED_BITS, "unexpected_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

int
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

  if (!fiid_obj_valid (obj))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (debug_set_prefix (prefix_buf, IPMI_DEBUG_MAX_PREFIX_LEN, prefix) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  if (debug_output_str (fd, prefix_buf, hdr) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  if (!(iter = fiid_iterator_create (obj)))
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj);
      goto cleanup;
    }

  while (!fiid_iterator_end (iter))
    {
      int field_len;
      char *key = NULL;

      if (!(key = fiid_iterator_key (iter)))
        {
          FIID_ITERATOR_ERROR_TO_ERRNO (iter);
          goto cleanup;
        }

      if ((field_len = fiid_iterator_field_len (iter)) < 0)
        {
          FIID_ITERATOR_ERROR_TO_ERRNO (iter);
          goto cleanup;
        }

      if (!field_len)
        {
          fiid_iterator_next (iter);
          continue;
        }

      if (prefix)
        {
          if (debug_dprintf (fd, "%s", prefix_buf) < 0)
            {
              ERRNO_TRACE (errno);
              goto cleanup;
            }
        }

      /* special case, always dump some fields in byte array format */
      if (field_len <= 64
          && strcasecmp (key, "raw_data")
          && strcasecmp (key, "user_name")
          && strcasecmp (key, "payload_data")
	  && strcasecmp (key, "record_data"))
        {
          uint64_t val = 0;

          if (fiid_iterator_get (iter, &val) < 0)
            {
              FIID_ITERATOR_ERROR_TO_ERRNO (iter);
              goto cleanup;
            }

          if (debug_dprintf (fd, "[%16" PRIX64 "h] = %s[%2ub]\n", (uint64_t) val, key, field_len) < 0)
            {
              ERRNO_TRACE (errno);
              goto cleanup;
            }
        }
      else
        {
          uint8_t buf[IPMI_DEBUG_MAX_BUF_LEN];
          int len;

          if (debug_dprintf (fd, "[  BYTE ARRAY ... ] = %s[%2uB]\n", key, BITS_ROUND_BYTES (field_len)) < 0)
            {
              ERRNO_TRACE (errno);
              goto cleanup;
            }

          if ((len = fiid_iterator_get_data (iter, buf, IPMI_DEBUG_MAX_BUF_LEN)) < 0)
            {
              FIID_ITERATOR_ERROR_TO_ERRNO (iter);
              goto cleanup;
            }

          if (debug_output_byte_array (fd, prefix_buf, buf, len) < 0)
            {
              ERRNO_TRACE (errno);
              goto cleanup;
            }
        }

      fiid_iterator_next (iter);
    }

  if (debug_output_str (fd, prefix_buf, trlr) < 0)
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_iterator_destroy (iter);
  return (rv);
}

int
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
  char prefix_buf[IPMI_DEBUG_MAX_PREFIX_LEN];
  uint8_t ipmb_buf[IPMI_DEBUG_MAX_PKT_LEN];
  int ipmb_buf_len;
  fiid_obj_t obj_cmd = NULL;
  int rv = -1;

  if (!fiid_obj_valid (obj)
      || !tmpl_ipmb_msg_hdr
      || !tmpl_ipmb_cmd)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj, tmpl_cmd_send_message_rq) < 0
      && FIID_OBJ_TEMPLATE_COMPARE (obj, tmpl_cmd_get_message_rs) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj);
      return (-1);
    }

  if (debug_set_prefix (prefix_buf, IPMI_DEBUG_MAX_PREFIX_LEN, prefix) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  if (debug_output_str (fd, prefix_buf, hdr) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  if (!(obj_cmd = fiid_obj_dup (obj)))
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj);
      goto cleanup;
    }

  memset (ipmb_buf, '\0', IPMI_DEBUG_MAX_PKT_LEN);

  if ((ipmb_buf_len = fiid_obj_get_data (obj_cmd,
                                         "message_data",
                                         ipmb_buf,
                                         IPMI_DEBUG_MAX_PKT_LEN)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_cmd);
      goto cleanup;
    }

  if (fiid_obj_clear_field (obj_cmd, "message_data") < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_cmd);
      goto cleanup;
    }

  if (ipmi_obj_dump (fd,
                     prefix,
                     cmd_hdr,
                     NULL,
                     obj_cmd) < 0)
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  if (ipmb_buf_len)
    {
      if (debug_dump_ipmb (fd,
			   prefix,
			   ipmb_buf,
			   ipmb_buf_len,
			   tmpl_ipmb_msg_hdr,
			   tmpl_ipmb_cmd) < 0)
        goto cleanup;
    }

  if (debug_output_str (fd, prefix_buf, trlr) < 0)
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd);
  return (rv);
}

int
ipmi_dump_hex (int fd,
               const char *prefix,
               const char *hdr,
               const char *trlr,
               const void *buf,
               unsigned int buf_len)
{
  char prefix_buf[IPMI_DEBUG_MAX_PREFIX_LEN];
  int rv = -1;

  if (!buf || !buf_len)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (debug_set_prefix (prefix_buf, IPMI_DEBUG_MAX_PREFIX_LEN, prefix) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  if (debug_output_str (fd, prefix_buf, hdr) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  if (prefix)
    {
      if (debug_dprintf (fd, "%s", prefix_buf) < 0)
        {
          ERRNO_TRACE (errno);
          goto cleanup;
        }
    }

  if (debug_dprintf (fd, "[  HEX DUMP ..... ] = %s[%2uB]\n", "HEX", buf_len) < 0)
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  if (debug_output_byte_array (fd, prefix_buf, buf, buf_len) < 0)
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  if (debug_output_str (fd, prefix_buf, trlr) < 0)
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }
  
  rv = 0;
 cleanup:
  return (rv);
}
