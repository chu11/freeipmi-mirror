/* 
   rmcp.c - remote management control protocol procedures

   Copyright (C) 2003 FreeIPMI Core Team

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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>

#ifdef STDC_HEADERS
#include <string.h>
#else
# include <sys/types.h>
# ifndef HAVE_MEMCPY
static void*
memcpy (void *dest, const void *src, size_t n)
{
  while (0 <= --n) ((unsigned char*)dest) [n] = ((unsigned char*)src) [n];
  return dest;
}
# endif
# ifndef HAVE_MEMSET
static void*
memset (void *s, int c, size_t n)
{
  while (0 <= --n) ((unsigned char*)s) [n] = (unsigned char) c;
  return s;
}
# endif
#endif

#include <errno.h>
#include <netinet/in.h>

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
fill_hdr_rmcp (u_int8_t message_class, fiid_obj_t obj_hdr) 
{
  u_int32_t hdr_len;
  if (obj_hdr == NULL)
    {
      errno = EINVAL;
      return -1;
    }
  
  hdr_len = fiid_obj_len_bytes (tmpl_hdr_rmcp);

  FIID_OBJ_SET (obj_hdr, tmpl_hdr_rmcp, "ver", 
		RMCP_VER_1_0);
  FIID_OBJ_SET (obj_hdr, tmpl_hdr_rmcp, "seq_num", 
		RMCP_HDR_SEQ_NUM_NO_RMCP_ACK);
  FIID_OBJ_SET (obj_hdr, tmpl_hdr_rmcp, "msg_class.class", 
		message_class);
  FIID_OBJ_SET (obj_hdr, tmpl_hdr_rmcp, "msg_class.ack",
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
fill_cmd_asf_presence_ping(u_int8_t msg_tag, fiid_obj_t obj_cmd)
{
  if (obj_cmd == NULL)
    {
      errno = EINVAL;
      return -1;
    }
  FIID_OBJ_SET (obj_cmd, tmpl_cmd_asf_presence_ping, "iana_enterprise_num",
		htonl(RMCP_ASF_IANA_ENTERPRISE_NUM));
  FIID_OBJ_SET (obj_cmd, tmpl_cmd_asf_presence_ping, "msg_type",
		RMCP_ASF_MSG_TYPE_PRESENCE_PING);
  FIID_OBJ_SET (obj_cmd, tmpl_cmd_asf_presence_ping, "msg_tag", msg_tag);
  FIID_OBJ_SET (obj_cmd, tmpl_cmd_asf_presence_ping, "data_len", 0x00);
  return 0;
}

int8_t
assemble_rmcp_pkt (fiid_obj_t obj_hdr, fiid_obj_t obj_cmd, fiid_template_t tmpl_cmd, u_int8_t *pkt, u_int32_t pkt_len)
{
  u_int32_t obj_cmd_len, obj_hdr_len;
  
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

int8_t
unassemble_rmcp_pkt (void *pkt, u_int32_t pkt_len, fiid_template_t tmpl_cmd, fiid_obj_t obj_hdr, fiid_obj_t obj_cmd)
{
  u_int32_t indx = 0;
  if (!(pkt && tmpl_cmd))
    {
      errno = EINVAL;
      return -1;
    }

  indx = 0;
  if (obj_hdr)
    memcpy (obj_hdr, pkt + indx, FREEIPMI_MIN (pkt_len - indx, fiid_obj_len_bytes (tmpl_hdr_rmcp)));
  indx += fiid_obj_len_bytes (tmpl_hdr_rmcp);

  if (pkt_len <= indx)
    return 0;

  if (obj_cmd)
    memcpy (obj_cmd, pkt + indx, FREEIPMI_MIN (pkt_len - indx, fiid_obj_len_bytes (tmpl_cmd)));
  indx += fiid_obj_len_bytes (tmpl_cmd);
 
  return 0;
}

int8_t
ipmi_rmcp_ping (int sockfd, struct sockaddr *hostaddr, unsigned long hostaddr_len, u_int32_t msg_tag, fiid_obj_t pong)
{
  int status = 0;
  fiid_obj_t obj_hdr = NULL;
  fiid_obj_t obj_cmd = NULL;
  u_int8_t *pkt = NULL;;

  if (!(sockfd && hostaddr && pong))
    {
      errno = EINVAL;
      return -1;
    }
  
  {/* asf_presence_ping request */
    u_int32_t pkt_len = 0;

    obj_hdr = alloca (fiid_obj_len_bytes (tmpl_hdr_rmcp));
    memset (obj_hdr, 0, fiid_obj_len_bytes (tmpl_hdr_rmcp));
    ERR (obj_hdr);

    obj_cmd = alloca (fiid_obj_len_bytes (tmpl_cmd_asf_presence_ping));
    memset (obj_cmd, 0, fiid_obj_len_bytes (tmpl_cmd_asf_presence_ping));
    ERR (obj_cmd);

    pkt_len = fiid_obj_len_bytes (tmpl_hdr_rmcp) + 
      fiid_obj_len_bytes (tmpl_cmd_asf_presence_ping);
    pkt = alloca (pkt_len);
    memset (pkt, 0, pkt_len);
    ERR (pkt);

    ERR (fill_hdr_rmcp_asf (obj_hdr) != -1);
    ERR (fill_cmd_asf_presence_ping (msg_tag, obj_cmd) != -1);
    ERR (assemble_rmcp_pkt (obj_hdr, obj_cmd, 
			      tmpl_cmd_asf_presence_ping, pkt, pkt_len) > 0);
    status = ipmi_lan_sendto (sockfd, pkt, pkt_len, 0, hostaddr, hostaddr_len);
    ERR (status != -1);
  }

  {/* asf_presence_ping response */ 
    struct sockaddr_in from, *hostaddr_in;
    socklen_t fromlen;
    u_int32_t  pkt_len;
    pkt_len = fiid_obj_len_bytes (tmpl_hdr_rmcp) + 
      fiid_obj_len_bytes (tmpl_cmd_asf_presence_pong);
    pkt = alloca (pkt_len);
    memset (pkt, 0, pkt_len);
    ERR (pkt);

    /*     ipmi_input_timeout (sockfd, 1); */
    status = ipmi_lan_recvfrom (sockfd, pkt, pkt_len, 0, (struct sockaddr *)&from, &fromlen);
    ERR (status != -1);

    /* FIXME: <ab@gnu.org.in>
       We need to verify 
       - IANA Enterprise Number (4542 = ASF IANA)
       - Msg Type (40h = Presence Pong)
       - Msg TAG  (from Ping Request)
    */

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
	return (-1);
      }
    ERR (unassemble_rmcp_pkt (pkt, pkt_len, 
			      tmpl_cmd_asf_presence_pong, NULL, pong) != -1);
/*     if (ipmi_rmcp_msg_tag_chk (msg_tag, pong) != 1) */
/*       { */
/* 	errno = EAGAIN; */
/* 	return (-1); */
/*       } */
    }
  return (0);
}

int8_t
ipmi_rmcp_msg_tag_chk (u_int8_t msg_tag, fiid_obj_t pong)
{
  u_int64_t val;
  if (!pong)
    return -1;

  fiid_obj_get (pong, tmpl_cmd_asf_presence_pong, "msg_tag", &val);
  if (msg_tag == val)
    return 1;
  else
    return 0;
}
