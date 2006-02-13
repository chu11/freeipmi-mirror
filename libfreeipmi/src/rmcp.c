/* 
   rmcp.c - remote management control protocol procedures

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

#include "freeipmi.h"

fiid_template_t tmpl_hdr_rmcp =
  {
    {8, "ver"},
    {8, "reserved1"},
    {8, "seq_num"},
    {5, "msg_class.class"},
    {2, "msg_class.reserved1"},
    {1, "msg_class.ack"},
    {0, ""}
  };

fiid_template_t tmpl_cmd_asf_presence_ping = 
  {
    {32, "iana_enterprise_num"},
    {8,  "msg_type"},
    {8,  "msg_tag"},
    {8,  "reserved1"},
    {8,  "data_len"},
    {0,  ""}
  };

fiid_template_t tmpl_cmd_asf_presence_pong =
  {
    {32, "iana_enterprise_num"},
    {8,  "msg_type"},
    {8,  "msg_tag"},
    {8,  "reserved1"},
    {8,  "data_len"},
    {32, "oem_iana_enterprise_num"},
    {32, "oem_defined"},
    {4,  "supported_entities.ver"},
    {3,  "supported_entities.reserved1"},
    {1,  "supported_entities.ipmi_supported"},
    {8,  "supported_interactions"},
    {48, "reserved2"},
    {0,  ""}
  };

int8_t
fill_hdr_rmcp (uint8_t message_class, fiid_obj_t obj_hdr) 
{
  uint32_t hdr_len;
  if (obj_hdr == NULL)
    {
      errno = EINVAL;
      return -1;
    }
  
  hdr_len = fiid_obj_len_bytes (tmpl_hdr_rmcp);

  FIID_OBJ_SET (obj_hdr, tmpl_hdr_rmcp, (uint8_t *)"ver", 
		RMCP_VER_1_0);
  FIID_OBJ_SET (obj_hdr, tmpl_hdr_rmcp, (uint8_t *)"seq_num", 
		RMCP_HDR_SEQ_NUM_NO_RMCP_ACK);
  FIID_OBJ_SET (obj_hdr, tmpl_hdr_rmcp, (uint8_t *)"msg_class.class", 
		message_class);
  FIID_OBJ_SET (obj_hdr, tmpl_hdr_rmcp, (uint8_t *)"msg_class.ack",
		RMCP_HDR_MSG_CLASS_BIT_RMCP_NORMAL);
  return 0;
}

int8_t
fill_hdr_rmcp_ipmi (fiid_obj_t obj_hdr) 
{
  return fill_hdr_rmcp(RMCP_HDR_MSG_CLASS_IPMI, obj_hdr);
}

int8_t
fill_hdr_rmcp_asf (fiid_obj_t obj_hdr)
{
  return fill_hdr_rmcp(RMCP_HDR_MSG_CLASS_ASF, obj_hdr);
}

int8_t
fill_cmd_asf_presence_ping(uint8_t msg_tag, fiid_obj_t obj_cmd)
{
  if (obj_cmd == NULL)
    {
      errno = EINVAL;
      return -1;
    }
  FIID_OBJ_SET (obj_cmd, tmpl_cmd_asf_presence_ping, (uint8_t *)"iana_enterprise_num",
		htonl(RMCP_ASF_IANA_ENTERPRISE_NUM));
  FIID_OBJ_SET (obj_cmd, tmpl_cmd_asf_presence_ping, (uint8_t *)"msg_type",
		RMCP_ASF_MSG_TYPE_PRESENCE_PING);
  FIID_OBJ_SET (obj_cmd, tmpl_cmd_asf_presence_ping, (uint8_t *)"msg_tag", msg_tag);
  FIID_OBJ_SET (obj_cmd, tmpl_cmd_asf_presence_ping, (uint8_t *)"data_len", 0x00);
  return 0;
}

int32_t
assemble_rmcp_pkt (fiid_obj_t obj_hdr, fiid_obj_t obj_cmd, fiid_template_t tmpl_cmd, uint8_t *pkt, uint32_t pkt_len)
{
  uint32_t obj_cmd_len, obj_hdr_len;
  
  if (!(obj_hdr && obj_cmd && tmpl_cmd && pkt))
    {
      errno = EINVAL;
      return -1;
    }

  obj_hdr_len = fiid_obj_len_bytes (tmpl_hdr_rmcp);
  obj_cmd_len = fiid_obj_len_bytes (tmpl_cmd);

  if (pkt_len < (obj_hdr_len + obj_cmd_len))
    {
      errno = EMSGSIZE;
      return -1;
    }

  memset (pkt, '\0', obj_hdr_len + obj_cmd_len);
  memcpy (pkt, obj_hdr, obj_hdr_len);
  memcpy (pkt + obj_hdr_len, obj_cmd, obj_cmd_len);
  return (obj_hdr_len + obj_cmd_len);
}  

int32_t
unassemble_rmcp_pkt (void *pkt, uint32_t pkt_len, fiid_template_t tmpl_cmd, fiid_obj_t obj_hdr, fiid_obj_t obj_cmd)
{
  uint32_t obj_len, indx = 0;

  if (!(pkt && tmpl_cmd))
    {
      errno = EINVAL;
      return -1;
    }

  indx = 0;
  obj_len = fiid_obj_len_bytes (tmpl_hdr_rmcp);
  if (obj_hdr)
    memcpy (obj_hdr, pkt + indx, FREEIPMI_MIN (pkt_len - indx, obj_len));
  indx += obj_len;

  if (pkt_len <= indx)
    return 0;

  obj_len = fiid_obj_len_bytes (tmpl_cmd);
  if (obj_cmd)
    memcpy (obj_cmd, pkt + indx, FREEIPMI_MIN (pkt_len - indx, obj_len));
  indx += obj_len;
 
  return 0;
}

int8_t
ipmi_rmcp_msg_tag_chk (uint8_t msg_tag, fiid_obj_t pong)
{
  uint64_t val;
  if (!pong)
    return -1;
  
  fiid_obj_get (pong, tmpl_cmd_asf_presence_pong, (uint8_t *)"msg_tag", &val);
  if (msg_tag == val)
    return 1;
  else
    return 0;
}
