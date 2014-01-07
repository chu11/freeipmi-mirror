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

#include "freeipmi/interface/rmcp-interface.h"
#include "freeipmi/interface/ipmi-interface.h"
#include "freeipmi/fiid/fiid.h"

#include "libcommon/ipmi-fill-util.h"
#include "libcommon/ipmi-trace.h"

#include "freeipmi-portability.h"

fiid_template_t tmpl_rmcp_hdr =
  {
    { 8, "version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "sequence_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 5, "message_class.class", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "message_class.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "message_class.ack", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

int
fill_rmcp_hdr (uint8_t message_class, fiid_obj_t obj_rmcp_hdr)
{
  if (!RMCP_HDR_MESSAGE_CLASS_VALID (message_class)
      || !fiid_obj_valid (obj_rmcp_hdr))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_rmcp_hdr, tmpl_rmcp_hdr) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_rmcp_hdr);
  FILL_FIID_OBJ_SET (obj_rmcp_hdr, "version", RMCP_VERSION_1_0);
  FILL_FIID_OBJ_SET (obj_rmcp_hdr, "reserved", 0);
  FILL_FIID_OBJ_SET (obj_rmcp_hdr, "sequence_number", RMCP_HDR_SEQ_NUM_NO_RMCP_ACK);
  FILL_FIID_OBJ_SET (obj_rmcp_hdr, "message_class.class", message_class);
  FILL_FIID_OBJ_SET (obj_rmcp_hdr, "message_class.reserved", 0);
  FILL_FIID_OBJ_SET (obj_rmcp_hdr, "message_class.ack", RMCP_HDR_MESSAGE_CLASS_BIT_RMCP_NORMAL);
  return (0);
}

int
fill_rmcp_hdr_ipmi (fiid_obj_t obj_rmcp_hdr)
{
  return (fill_rmcp_hdr (RMCP_HDR_MESSAGE_CLASS_IPMI, obj_rmcp_hdr));
}

int
fill_rmcp_hdr_asf (fiid_obj_t obj_rmcp_hdr)
{
  return (fill_rmcp_hdr (RMCP_HDR_MESSAGE_CLASS_ASF, obj_rmcp_hdr));
}

int
assemble_rmcp_pkt (fiid_obj_t obj_rmcp_hdr,
                   fiid_obj_t obj_cmd,
                   void *pkt,
                   unsigned int pkt_len,
		   unsigned int flags)
{
  int obj_cmd_len, obj_rmcp_hdr_len;
  unsigned int flags_mask = 0;

  if (!fiid_obj_valid (obj_rmcp_hdr)
      || !fiid_obj_valid (obj_cmd)
      || !pkt
      || !pkt_len
      || (flags & ~flags_mask))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_rmcp_hdr, tmpl_rmcp_hdr) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  if (FIID_OBJ_PACKET_VALID (obj_rmcp_hdr) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcp_hdr);
      return (-1);
    }
  if (FIID_OBJ_PACKET_VALID (obj_cmd) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_cmd);
      return (-1);
    }

  if ((obj_rmcp_hdr_len = fiid_obj_len_bytes (obj_rmcp_hdr)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcp_hdr);
      return (-1);
    }
  if ((obj_cmd_len = fiid_obj_len_bytes (obj_cmd)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_cmd);
      return (-1);
    }

  if (pkt_len < (obj_rmcp_hdr_len + obj_cmd_len))
    {
      SET_ERRNO (EMSGSIZE);
      return (-1);
    }

  memset (pkt, '\0', pkt_len);
  if ((obj_rmcp_hdr_len = fiid_obj_get_all (obj_rmcp_hdr,
                                            pkt,
                                            pkt_len)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcp_hdr);
      return (-1);
    }
  if ((obj_cmd_len = fiid_obj_get_all (obj_cmd,
                                       pkt + obj_rmcp_hdr_len,
                                       pkt_len - obj_rmcp_hdr_len)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_cmd);
      return (-1);
    }

  return (obj_rmcp_hdr_len + obj_cmd_len);
}

int
unassemble_rmcp_pkt (const void *pkt,
                     unsigned int pkt_len,
                     fiid_obj_t obj_rmcp_hdr,
                     fiid_obj_t obj_cmd,
		     unsigned int flags)
{
  unsigned int indx = 0;
  int len;
  unsigned int flags_mask = (IPMI_INTERFACE_FLAGS_NO_LEGAL_CHECK);

  if (!pkt
      || !fiid_obj_valid (obj_rmcp_hdr)
      || !fiid_obj_valid (obj_cmd)
      || (flags & ~flags_mask))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_rmcp_hdr, tmpl_rmcp_hdr) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  if (fiid_obj_clear (obj_rmcp_hdr) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcp_hdr);
      return (-1);
    }

  if (fiid_obj_clear (obj_cmd) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_cmd);
      return (-1);
    }

  if ((len = fiid_obj_set_all (obj_rmcp_hdr, pkt + indx, pkt_len - indx)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcp_hdr);
      return (-1);
    }
  indx += len;

  if (pkt_len <= indx)
    {
      /* trace, but don't error out, cannot parse packet */
      ERR_TRACE ("malformed packet", EINVAL);
      return (0);
    }

  if ((len = fiid_obj_set_all (obj_cmd, pkt + indx, pkt_len - indx)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_cmd);
      return (-1);
    }
  indx += len;

  if (FIID_OBJ_PACKET_VALID (obj_rmcp_hdr) == 1
      && ((flags & IPMI_INTERFACE_FLAGS_NO_LEGAL_CHECK) || FIID_OBJ_PACKET_SUFFICIENT (obj_cmd) == 1))
    return (1);

  return (0);
}
