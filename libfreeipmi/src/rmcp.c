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
   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  

*/

#include "freeipmi.h"

fiid_template_t tmpl_hdr_rmcp =
  {
    {8, "ver", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "seq_num", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {5, "msg_class.class", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "msg_class.reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "msg_class.ack", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_asf_presence_ping = 
  {
    {32, "iana_enterprise_num", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "msg_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "msg_tag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "data_len", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_asf_presence_pong =
  {
    {32, "iana_enterprise_num", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "msg_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "msg_tag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "data_len", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {32, "oem_iana_enterprise_num", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {32, "oem_defined", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,  "supported_entities.ver", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {3,  "supported_entities.reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1,  "supported_entities.ipmi_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "supported_interactions", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {48, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0,  "", 0}
  };

int8_t
fill_hdr_rmcp (uint8_t message_class, fiid_obj_t obj_hdr) 
{
  int8_t rv;

  if (!fiid_obj_valid(obj_hdr))
    {
      errno = EINVAL;
      return -1;
    }

  if ((rv = fiid_obj_template_compare(obj_hdr, tmpl_hdr_rmcp)) < 0)
    return (-1);
  
  if (!rv)
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_SET (obj_hdr, (uint8_t *)"ver", RMCP_VER_1_0);
  FIID_OBJ_SET (obj_hdr, (uint8_t *)"reserved1", 0);
  FIID_OBJ_SET (obj_hdr, (uint8_t *)"seq_num", RMCP_HDR_SEQ_NUM_NO_RMCP_ACK);
  FIID_OBJ_SET (obj_hdr, (uint8_t *)"msg_class.class", message_class);
  FIID_OBJ_SET (obj_hdr, (uint8_t *)"msg_class.reserved1", 0);
  FIID_OBJ_SET (obj_hdr, (uint8_t *)"msg_class.ack",
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
  int8_t rv;

  if (!fiid_obj_valid(obj_cmd))
    {
      errno = EINVAL;
      return -1;
    }

  if ((rv = fiid_obj_template_compare(obj_cmd, tmpl_cmd_asf_presence_ping)) < 0)
    return (-1);
  
  if (!rv)
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_SET (obj_cmd, (uint8_t *)"iana_enterprise_num",
                htonl(RMCP_ASF_IANA_ENTERPRISE_NUM));
  FIID_OBJ_SET (obj_cmd, (uint8_t *)"msg_type",
                RMCP_ASF_MSG_TYPE_PRESENCE_PING);
  FIID_OBJ_SET (obj_cmd, (uint8_t *)"msg_tag", msg_tag);
  FIID_OBJ_SET (obj_cmd, (uint8_t *)"reserved1", 0);
  FIID_OBJ_SET (obj_cmd, (uint8_t *)"data_len", 0x00);
  return 0;
}

int32_t
assemble_rmcp_pkt (fiid_obj_t obj_hdr, fiid_obj_t obj_cmd, uint8_t *pkt, uint32_t pkt_len)
{
  uint32_t obj_cmd_len, obj_hdr_len;
  int8_t rv;

  if (!(fiid_obj_valid(obj_hdr) 
        && fiid_obj_valid(obj_cmd)
        && pkt))
    {
      errno = EINVAL;
      return (-1);
    }

  if ((rv = fiid_obj_template_compare(obj_hdr, tmpl_hdr_rmcp)) < 0)
    return (-1);
  
  if (!rv)
    {
      errno = EINVAL;
      return (-1);
    }

  if ((rv = fiid_obj_template_compare(obj_cmd, tmpl_cmd_asf_presence_ping)) < 0)
    return (-1);
  
  if (!rv)
    {
      errno = EINVAL;
      return (-1);
    }

  if ((rv = fiid_obj_packet_valid(obj_hdr)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return (-1);
    }

  if ((rv = fiid_obj_packet_valid(obj_cmd)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return (-1);
    }

  obj_hdr_len = fiid_obj_len_bytes (obj_hdr);
  ERR(obj_hdr_len != -1);
  obj_cmd_len = fiid_obj_len_bytes (obj_cmd);
  ERR(obj_cmd_len != -1);

  if (pkt_len < (obj_hdr_len + obj_cmd_len))
    {
      errno = EMSGSIZE;
      return -1;
    }

  memset (pkt, '\0', pkt_len);
  ERR((obj_hdr_len = fiid_obj_get_all(obj_hdr, pkt, pkt_len)) != -1);
  ERR((obj_cmd_len = fiid_obj_get_all(obj_cmd, pkt + obj_hdr_len, pkt_len - obj_hdr_len)) != -1);
  return (obj_hdr_len + obj_cmd_len);
}  

int32_t
unassemble_rmcp_pkt (void *pkt, uint32_t pkt_len, fiid_obj_t obj_hdr, fiid_obj_t obj_cmd)
{
  uint32_t indx = 0;
  int32_t len;
  int8_t rv;

  if (!(pkt
        && fiid_obj_valid(obj_hdr)
        && fiid_obj_valid(obj_cmd)))
    {
      errno = EINVAL;
      return -1;
    }

  if ((rv = fiid_obj_template_compare(obj_hdr, tmpl_hdr_rmcp)) < 0)
    return (-1);
  
  if (!rv)
    {
      errno = EINVAL;
      return (-1);
    }

  if ((rv = fiid_obj_template_compare(obj_cmd, tmpl_cmd_asf_presence_pong)) < 0)
    return (-1);
  
  if (!rv)
    {
      errno = EINVAL;
      return (-1);
    }

  ERR(!((len = fiid_obj_set_all(obj_hdr, pkt + indx, pkt_len - indx)) < 0));
  indx += len;

  if (pkt_len <= indx)
    return 0;

  ERR(!((len = fiid_obj_set_all(obj_cmd, pkt + indx, pkt_len - indx)) < 0));
  indx += len;

  return 0;
}

int8_t
ipmi_rmcp_ping (int sockfd, struct sockaddr *hostaddr, unsigned long hostaddr_len, uint32_t msg_tag, fiid_obj_t pong)
{
  int8_t rv;

  if (!(sockfd && hostaddr && fiid_obj_valid(pong)))
    {
      errno = EINVAL;
      return -1;
    }

  if ((rv = fiid_obj_template_compare(pong, tmpl_cmd_asf_presence_pong)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return -1;
    }
  
  {/* asf_presence_ping request */
    fiid_obj_t obj_hdr = NULL;
    fiid_obj_t obj_cmd = NULL;
    int32_t hdr_len, cmd_len;
    uint8_t *pkt = NULL;
    uint32_t pkt_len = 0;
    int status = 0;
  
    rv = -1;

    if (!(obj_hdr = fiid_obj_create(tmpl_hdr_rmcp)))
      goto cleanup1;

    if (!(obj_cmd = fiid_obj_create(tmpl_cmd_asf_presence_ping)))
      goto cleanup1;

    if ((hdr_len = fiid_template_len_bytes(tmpl_hdr_rmcp)) < 0)
      goto cleanup1;

    if ((cmd_len = fiid_template_len_bytes(tmpl_cmd_asf_presence_ping)) < 0)
      goto cleanup1;
	
    pkt_len = hdr_len + cmd_len;
    pkt = alloca (pkt_len);
    memset (pkt, 0, pkt_len);
    ERR (pkt);

    if (fill_hdr_rmcp_asf (obj_hdr) < 0)
      goto cleanup1;

    if (fill_cmd_asf_presence_ping (msg_tag, obj_cmd) < 0)
      goto cleanup1;

    if (assemble_rmcp_pkt (obj_hdr, 
			   obj_cmd, 
			   pkt, 
			   pkt_len) < 0)
      goto cleanup1;

    if ((status = ipmi_lan_sendto (sockfd, 
				   pkt,
				   pkt_len,
				   0, 
				   hostaddr, 
				   hostaddr_len)) < 0)
      goto cleanup1;

    rv = 0;
  cleanup1:
    if (obj_hdr)
      fiid_obj_destroy(obj_hdr);
    if (obj_cmd)
      fiid_obj_destroy(obj_cmd);
    if (rv < 0)
      return (rv);
  }

  {/* asf_presence_ping response */ 
    struct sockaddr_in from, *hostaddr_in;
    socklen_t fromlen;
    fiid_obj_t obj_hdr = NULL;
    int32_t hdr_len, cmd_len;
    uint8_t *pkt = NULL;
    uint32_t pkt_len = 0;
    int32_t recv_len;
    uint64_t val;

    rv = -1;

    if (!(obj_hdr = fiid_obj_create(tmpl_hdr_rmcp)))
      goto cleanup2;

    if ((hdr_len = fiid_template_len_bytes(tmpl_hdr_rmcp)) < 0)
      goto cleanup2;

    if ((cmd_len = fiid_template_len_bytes(tmpl_cmd_asf_presence_pong)) < 0)
      goto cleanup2;

    pkt_len = hdr_len + cmd_len;
    pkt = alloca (pkt_len);
    memset (pkt, 0, pkt_len);
    ERR (pkt);

    if ((recv_len = ipmi_lan_recvfrom (sockfd,
				       pkt,
				       pkt_len,
				       0, 
				       (struct sockaddr *)&from, 
				       &fromlen)) < 0)
      goto cleanup2;

    hostaddr_in = (struct sockaddr_in *) hostaddr;
    if ((from.sin_family == AF_INET) && 
	(from.sin_addr.s_addr != hostaddr_in->sin_addr.s_addr))
      {
#if 0
	printf ("ipmi_ping warning: Reply came from [%s] instead of [%s]." 
		"Please tune your time-out value to something higher\n", 
		inet_ntoa (from.sin_addr), inet_ntoa (hostaddr->sin_addr));
#endif
	errno = EBADMSG;
	goto cleanup2;
      }
    
    if (unassemble_rmcp_pkt (pkt, 
			     recv_len,
			     obj_hdr, 
			     pong) < 0)
      goto cleanup2;

    if (fiid_obj_get(pong,
		     (uint8_t *)"msg_type",
		     &val) < 0)
      goto cleanup2;

    if (val != RMCP_ASF_MSG_TYPE_PRESENCE_PONG)
      {
	errno = EBADMSG;
	goto cleanup2;
      }

    if (fiid_obj_get(pong,
		     (uint8_t *)"msg_tag",
		     &val) < 0)
      goto cleanup2;

    if (val != msg_tag)
      {
	errno = EBADMSG;
	goto cleanup2;
      }
    
    rv = 0;
  cleanup2:
    if (obj_hdr)
      fiid_obj_destroy(obj_hdr);
    if (rv < 0)
      return (rv);
    }

  return (0);
}

int8_t
ipmi_rmcp_msg_tag_chk (uint8_t msg_tag, fiid_obj_t pong)
{
  uint64_t val;
  int8_t rv;

  if (!fiid_obj_valid(pong))
    {
      errno = EINVAL;
      return (-1);
    }

  if ((rv = fiid_obj_template_compare(pong, tmpl_cmd_asf_presence_pong)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return -1;
    }
  

  if (fiid_obj_get (pong, (uint8_t *)"msg_tag", &val) < 0)
    return (-1);

  if (msg_tag == val)
    return 1;
  else
    return 0;
}

