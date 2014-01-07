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
#include <errno.h>

#include "freeipmi/util/ipmi-ipmb-util.h"
#include "freeipmi/fiid/fiid.h"
#include "freeipmi/interface/ipmi-ipmb-interface.h"
#include "freeipmi/util/ipmi-util.h"

#include "libcommon/ipmi-fiid-util.h"
#include "libcommon/ipmi-trace.h"

#include "freeipmi-portability.h"
#include "secure.h"

int
ipmi_ipmb_check_rq_seq (fiid_obj_t obj_ipmb_msg_hdr, uint8_t rq_seq)
{
  uint8_t rq_seq_recv;
  uint64_t val;

  if (!fiid_obj_valid (obj_ipmb_msg_hdr))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_FIELD_LOOKUP (obj_ipmb_msg_hdr, "rq_seq") < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_ipmb_msg_hdr);
      return (-1);
    }

  if (FIID_OBJ_GET (obj_ipmb_msg_hdr, "rq_seq", &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_ipmb_msg_hdr);
      return (-1);
    }
  rq_seq_recv = val;

  return ((rq_seq_recv == rq_seq) ? 1 : 0);
}

int
ipmi_ipmb_check_checksum (uint8_t rq_addr,
                          fiid_obj_t obj_ipmb_msg_hdr,
                          fiid_obj_t obj_cmd,
                          fiid_obj_t obj_ipmb_msg_trlr)
{
  int obj_ipmb_msg_hdr_len, obj_cmd_len, obj_len;
  uint8_t checksum1_recv, checksum1_calc, checksum2_recv, checksum2_calc;
  uint8_t *buf1 = NULL;
  uint8_t *buf2 = NULL;
  unsigned int buflen;
  unsigned int len = 0;
  uint64_t val;
  int rv = -1;

  if (!fiid_obj_valid (obj_ipmb_msg_hdr)
      || !fiid_obj_valid (obj_cmd)
      || !fiid_obj_valid (obj_ipmb_msg_trlr))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_ipmb_msg_hdr, tmpl_ipmb_msg_hdr_rs) < 0)
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }
  if (FIID_OBJ_TEMPLATE_COMPARE (obj_ipmb_msg_trlr, tmpl_ipmb_msg_trlr) < 0)
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  if ((obj_ipmb_msg_hdr_len = fiid_obj_len_bytes (obj_ipmb_msg_hdr)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_ipmb_msg_hdr);
      goto cleanup;
    }
  if ((obj_cmd_len = fiid_obj_len_bytes (obj_cmd)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_cmd);
      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_ipmb_msg_hdr, "checksum1", &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_ipmb_msg_hdr);
      goto cleanup;
    }

  checksum1_recv = val;

  if (!(buf1 = malloc (obj_ipmb_msg_hdr_len + 1)))
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  /* achu: The rq_addr isn't in the ipmb_msg_hdr response, but it's
   * part of the calculated checksum stored in the header.  If you're
   * thinking that's dumb.  I think so too.
   */
  buf1[0] = rq_addr;

  if ((obj_len = fiid_obj_get_block (obj_ipmb_msg_hdr,
                                    "rq_lun",
                                    "net_fn",
                                    buf1 + 1,
                                    obj_ipmb_msg_hdr_len)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_ipmb_msg_hdr);
      goto cleanup;
    }
  checksum1_calc = ipmi_checksum (buf1, obj_len + 1);

  if (checksum1_recv != checksum1_calc)
    {
      rv = 0;
      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_ipmb_msg_trlr, "checksum2", &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_ipmb_msg_trlr);
      goto cleanup;
    }

  checksum2_recv = val;

  buflen = obj_ipmb_msg_hdr_len + obj_cmd_len;
  if (!(buf2 = malloc (buflen)))
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  if ((obj_len = fiid_obj_get_block (obj_ipmb_msg_hdr,
                                     "rs_addr",
                                     "rq_seq",
                                     buf2,
                                     buflen - len)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_ipmb_msg_hdr);
      goto cleanup;
    }
  len += obj_len;

  if ((obj_len = fiid_obj_get_all (obj_cmd, buf2 + len, buflen - len)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_cmd);
      goto cleanup;
    }
  len += obj_len;

  checksum2_calc = ipmi_checksum (buf2, len);

  if (checksum2_recv != checksum2_calc)
    {
      rv = 0;
      goto cleanup;
    }

  rv = 1;
 cleanup:
  free (buf1);
  free (buf2);
  return (rv);
}
