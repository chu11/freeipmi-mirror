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
#include <stdarg.h>
#endif /* STDC_HEADERS */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <errno.h>

#include "freeipmi/debug/ipmi-debug.h"
#include "freeipmi/fiid/fiid.h"
#include "freeipmi/interface/rmcp-interface.h"

#include "ipmi-debug-common.h"

#include "libcommon/ipmi-fiid-util.h"
#include "libcommon/ipmi-trace.h"

#include "freeipmi-portability.h"

int
ipmi_dump_rmcp_packet (int fd,
                       const char *prefix,
                       const char *hdr,
                       const char *trlr,
                       uint8_t *pkt,
                       unsigned int pkt_len,
                       fiid_template_t tmpl_cmd)
{
  uint32_t indx = 0;
  char prefix_buf[IPMI_DEBUG_MAX_PREFIX_LEN];
  char *rmcp_hdr =
    "RMCP Header:\n"
    "------------";
  char *rmcp_cmd =
    "RMCP Command Data:\n"
    "------------------";
  char *unexpected_hdr =
    "Unexpected Data:\n"
    "----------------";
  fiid_obj_t obj_rmcp_hdr = NULL;
  fiid_obj_t obj_cmd = NULL;
  fiid_obj_t obj_unexpected_data = NULL;
  int32_t len;
  int rv = -1;

  if (!pkt || !tmpl_cmd)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (ipmi_debug_set_prefix (prefix_buf, IPMI_DEBUG_MAX_PREFIX_LEN, prefix) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  if (ipmi_debug_output_str (fd, prefix_buf, hdr) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  /* Dump rmcp header */

  if (!(obj_rmcp_hdr = fiid_obj_create (tmpl_rmcp_hdr)))
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  if ((len = fiid_obj_set_all (obj_rmcp_hdr, pkt + indx, pkt_len - indx)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcp_hdr);
      goto cleanup;
    }
  indx += len;

  if (ipmi_obj_dump (fd, prefix, rmcp_hdr, NULL, obj_rmcp_hdr) < 0)
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  if (pkt_len <= indx)
    {
      rv = 0;
      goto cleanup;
    }

  /* Dump command data */

  if (!(obj_cmd = fiid_obj_create (tmpl_cmd)))
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  if ((len = fiid_obj_set_all (obj_cmd, pkt + indx, pkt_len - indx)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_cmd);
      goto cleanup;
    }
  indx += len;

  if (ipmi_obj_dump (fd, prefix, rmcp_cmd, NULL, obj_cmd) < 0)
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  if (pkt_len <= indx)
    {
      rv = 0;
      goto cleanup;
    }

  /* Dump unexpected stuff */

  if (!(obj_unexpected_data = fiid_obj_create (tmpl_unexpected_data)))
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  if ((len = fiid_obj_set_all (obj_unexpected_data, pkt + indx, pkt_len - indx)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_unexpected_data);
      goto cleanup;
    }
  indx += len;

  if (ipmi_obj_dump (fd, prefix, unexpected_hdr, NULL, obj_unexpected_data) < 0)
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  if (ipmi_debug_output_str (fd, prefix_buf, trlr) < 0)
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_rmcp_hdr);
  fiid_obj_destroy (obj_cmd);
  fiid_obj_destroy (obj_unexpected_data);
  return (rv);
}

