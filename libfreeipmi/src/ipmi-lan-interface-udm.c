/* 
   ipmi-lan-interface-udm.c - IPMI UDM LAN Interface

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

#include "freeipmi-build.h"
#include "err-wrappers.h"
#include "fiid-wrappers.h"

fiid_template_t tmpl_lan_raw_hdr = 
  {
    {2, "lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {6, "net_fn", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_lan_raw =
  {
    {8192, "raw_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

static int8_t
_ipmi_lan_cmd_send(ipmi_device_t *dev, fiid_obj_t obj_cmd_rq)
{
  uint8_t *pkt;
  int32_t pkt_len = 1024;
  int32_t send_len = 0;

  assert(dev
	 && dev->io.outofband.local_sockfd 
	 && fiid_obj_valid(obj_cmd_rq)
	 && fiid_obj_packet_valid(obj_cmd_rq));

  FIID_OBJ_CLEAR(dev->io.outofband.rq.obj_rmcp_hdr);
  FIID_OBJ_CLEAR(dev->io.outofband.rq.obj_lan_session_hdr);
  FIID_OBJ_CLEAR(dev->io.outofband.rq.obj_lan_msg_hdr);
  FIID_OBJ_CLEAR(dev->io.outofband.rq.obj_lan_msg_trlr);   
    
  pkt = alloca (pkt_len);
  ERR (pkt);
  memset (pkt, 0, pkt_len);
    
  ERR (fill_rmcp_hdr_ipmi (dev->io.outofband.rq.obj_rmcp_hdr) != -1);
  ERR (fill_lan_msg_hdr (dev->net_fn,
			 dev->lun,
			 dev->io.outofband.rq_seq,
			 dev->io.outofband.rq.obj_lan_msg_hdr) != -1);
  ERR (fill_lan_session_hdr (dev->io.outofband.authentication_type,
			     dev->io.outofband.session_sequence_number,
			     dev->io.outofband.session_id,
			     NULL,
			     0,
			     dev->io.outofband.rq.obj_lan_session_hdr) != -1);
  ERR ((send_len = assemble_ipmi_lan_pkt (dev->io.outofband.rq.obj_rmcp_hdr,
					  dev->io.outofband.rq.obj_lan_session_hdr,
					  dev->io.outofband.rq.obj_lan_msg_hdr,
					  obj_cmd_rq,
					  dev->io.outofband.password,
					  IPMI_MAX_AUTHENTICATION_CODE_LENGTH,
					  pkt,
					  pkt_len)) != -1);

  dev->io.outofband.session_sequence_number++;
  IPMI_LAN_RQ_SEQ_INC (dev->io.outofband.rq_seq);
  
  ERR (!(ipmi_lan_sendto (dev->io.outofband.local_sockfd, 
			  pkt, 
			  send_len, 
			  0, 
			  &(dev->io.outofband.remote_host), 
			  dev->io.outofband.remote_host_len) < 0));
  
  return (0);
}

static int8_t
_ipmi_lan_cmd_recv(ipmi_device_t *dev, fiid_obj_t obj_cmd_rs)
{
  struct sockaddr_in from;
  socklen_t fromlen = 0;
    
  uint8_t *pkt = NULL;
  uint32_t pkt_len = 1024;
  int32_t recv_len = 0;
    
  assert(dev
	 && dev->io.outofband.local_sockfd 
	 && fiid_obj_valid(obj_cmd_rs));
  
  FIID_OBJ_CLEAR(dev->io.outofband.rs.obj_rmcp_hdr); 
  FIID_OBJ_CLEAR(dev->io.outofband.rs.obj_lan_session_hdr); 
  FIID_OBJ_CLEAR(dev->io.outofband.rs.obj_lan_msg_hdr); 
  FIID_OBJ_CLEAR(dev->io.outofband.rs.obj_lan_msg_trlr); 
   
  pkt = alloca (pkt_len);
  ERR (pkt);
  memset (pkt, 0, pkt_len);

  ERR (!((recv_len = ipmi_lan_recvfrom (dev->io.outofband.local_sockfd, 
					pkt, 
					pkt_len, 
					0, 
					(struct sockaddr *) &from, 
					&fromlen)) < 0));
    
  ERR (unassemble_ipmi_lan_pkt(pkt,
			       recv_len,
			       dev->io.outofband.rs.obj_rmcp_hdr,
			       dev->io.outofband.rs.obj_lan_session_hdr,
			       dev->io.outofband.rs.obj_lan_msg_hdr,
			       obj_cmd_rs,
			       dev->io.outofband.rs.obj_lan_msg_trlr) != -1);
  
  ERR (ipmi_lan_check_session_authcode (pkt, 
					recv_len, 
					dev->io.outofband.authentication_type,
					dev->io.outofband.password,
					IPMI_MAX_AUTHENTICATION_CODE_LENGTH) == 1);

  return (0);
}

int8_t 
ipmi_lan_cmd2 (ipmi_device_t *dev, 
	       fiid_obj_t obj_cmd_rq,
	       fiid_obj_t obj_cmd_rs)
{
  if (!(dev 
        && dev->io.outofband.local_sockfd 
        && fiid_obj_valid(obj_cmd_rq)
        && fiid_obj_valid(obj_cmd_rs)))
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_PACKET_VALID(obj_cmd_rq);
  ERR (!(_ipmi_lan_cmd_send(dev, obj_cmd_rq) < 0));
  ERR (!(_ipmi_lan_cmd_recv(dev, obj_cmd_rs) < 0));
  
  return (0);
}

static int8_t
_ipmi_lan_raw_send_parse(ipmi_device_t *dev, 
			 uint8_t *buf_rq, 
			 size_t buf_rq_len)
{
  uint64_t val = 0;
  fiid_obj_t obj_hdr = NULL;
  int8_t rv = -1;

  assert (dev && buf_rq && buf_rq_len >= 2);
    
  FIID_OBJ_CREATE_CLEANUP (obj_hdr, tmpl_lan_raw_hdr);
  
  FIID_OBJ_SET_ALL_CLEANUP (obj_hdr, buf_rq, buf_rq_len);
  
  FIID_OBJ_GET_CLEANUP (obj_hdr, (uint8_t *)"lun", &val);
  dev->lun = val;
  
  FIID_OBJ_GET_CLEANUP (obj_hdr, (uint8_t *)"net_fn", &val);
  dev->net_fn = val;
  
  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_hdr);
  return (rv);
} 

static fiid_obj_t
_ipmi_lan_raw_create_cmd(ipmi_device_t *dev, 
			 uint8_t *buf_rq, 
			 size_t buf_rq_len)
{
  fiid_obj_t obj_cmd_rq = NULL;
  fiid_obj_t rv = NULL;
    
  assert (dev && buf_rq && buf_rq_len >= 2);

  FIID_OBJ_CREATE_CLEANUP (obj_cmd_rq, tmpl_lan_raw);
  FIID_OBJ_SET_ALL_CLEANUP (obj_cmd_rq, buf_rq + 1, buf_rq_len - 1);
  
  rv = obj_cmd_rq;
 cleanup:
  return (rv);
}

static int8_t 
_ipmi_lan_cmd_raw_send (ipmi_device_t *dev, 
			fiid_obj_t obj_cmd_rq)
{
  uint8_t *pkt;
  uint32_t pkt_len = 1024;

  assert (dev 
	  && fiid_obj_valid(obj_cmd_rq)
	  && fiid_obj_packet_valid(obj_cmd_rq));

  FIID_OBJ_CLEAR(dev->io.outofband.rq.obj_rmcp_hdr);
  FIID_OBJ_CLEAR(dev->io.outofband.rq.obj_lan_session_hdr);
  FIID_OBJ_CLEAR(dev->io.outofband.rq.obj_lan_msg_hdr);
  FIID_OBJ_CLEAR(dev->io.outofband.rq.obj_lan_msg_trlr); 
  
  pkt = alloca (pkt_len);
  ERR (pkt);
  memset (pkt, 0, pkt_len);
    
  ERR (fill_rmcp_hdr_ipmi (dev->io.outofband.rq.obj_rmcp_hdr) != -1);
  ERR (fill_lan_msg_hdr (dev->net_fn,
			 dev->lun,
			 dev->io.outofband.rq_seq,
			 dev->io.outofband.rq.obj_lan_msg_hdr) != -1);
  ERR (fill_lan_session_hdr (dev->io.outofband.authentication_type,
			     dev->io.outofband.session_sequence_number,
			     dev->io.outofband.session_id,
			     NULL,
			     0,
			     dev->io.outofband.rq.obj_lan_session_hdr) != -1);
  ERR (assemble_ipmi_lan_pkt (dev->io.outofband.rq.obj_rmcp_hdr,
			      dev->io.outofband.rq.obj_lan_session_hdr,
			      dev->io.outofband.rq.obj_lan_msg_hdr,
			      obj_cmd_rq,
			      dev->io.outofband.password,
			      IPMI_MAX_AUTHENTICATION_CODE_LENGTH,
			      pkt,
			      pkt_len) != -1);

  dev->io.outofband.session_sequence_number++;
  IPMI_LAN_RQ_SEQ_INC (dev->io.outofband.rq_seq);
  
  ERR (!(ipmi_lan_sendto (dev->io.outofband.local_sockfd, pkt, pkt_len, 0, 
			  &(dev->io.outofband.remote_host), 
			  dev->io.outofband.remote_host_len) < 0));
  
  return (0);
}

static fiid_obj_t
_ipmi_lan_cmd_raw_recv (ipmi_device_t *dev)
{
  fiid_obj_t obj_cmd_rs = NULL;

  struct sockaddr_in from;
  socklen_t fromlen = 0;
  
  uint8_t *pkt = NULL;
  int32_t pkt_len = 1024;
  int32_t recv_len;
  fiid_obj_t rv = NULL;

  assert (dev && dev->io.outofband.local_sockfd);

  FIID_OBJ_CLEAR_CLEANUP(dev->io.outofband.rs.obj_rmcp_hdr); 
  FIID_OBJ_CLEAR_CLEANUP(dev->io.outofband.rs.obj_lan_session_hdr); 
  FIID_OBJ_CLEAR_CLEANUP(dev->io.outofband.rs.obj_lan_msg_hdr); 
  FIID_OBJ_CLEAR_CLEANUP(dev->io.outofband.rs.obj_lan_msg_trlr); 
  
  pkt = alloca (pkt_len);
  ERR_CLEANUP (pkt);
  memset (pkt, 0, pkt_len);

  ERR_CLEANUP (!((recv_len = ipmi_lan_recvfrom (dev->io.outofband.local_sockfd, 
						pkt, 
						pkt_len, 
						0, 
						(struct sockaddr *) &from, 
						&fromlen)) < 0));
       
  FIID_OBJ_CREATE_CLEANUP(obj_cmd_rs, tmpl_lan_raw);

  ERR_CLEANUP (!(unassemble_ipmi_lan_pkt(pkt,
					 recv_len,
					 dev->io.outofband.rs.obj_rmcp_hdr,
					 dev->io.outofband.rs.obj_lan_session_hdr,
					 dev->io.outofband.rs.obj_lan_msg_hdr,
					 obj_cmd_rs,
					 dev->io.outofband.rs.obj_lan_msg_trlr) < 0));
    
  ERR_CLEANUP (!(ipmi_lan_check_session_authcode (pkt, 
						  recv_len, 
						  dev->io.outofband.authentication_type,
						  dev->io.outofband.password,
						  IPMI_MAX_AUTHENTICATION_CODE_LENGTH) != 1));
       
  rv = obj_cmd_rs;
 cleanup:    
  return (rv);
}

static int8_t
_ipmi_lan_raw_recv_parse(ipmi_device_t *dev, 
			 fiid_obj_t obj_cmd_rs, 
			 uint8_t *buf_rs, 
			 size_t *buf_rs_len)
{
  uint64_t val = 0;
  fiid_obj_t obj_hdr = NULL;
  int8_t rv = -1;
  int32_t len;

  assert (dev
	  && fiid_obj_valid(obj_cmd_rs) 
	  && buf_rs 
	  && buf_rs_len 
	  && *buf_rs_len >= 2);

  FIID_OBJ_CREATE_CLEANUP(obj_hdr, tmpl_lan_raw_hdr);
    
  FIID_OBJ_GET_CLEANUP (dev->io.outofband.rs.obj_lan_msg_hdr, 
			(uint8_t *)"rq_lun", 
			&val);
  
  FIID_OBJ_SET_CLEANUP (obj_hdr, (uint8_t *)"lun", val);
  
  FIID_OBJ_GET_CLEANUP (dev->io.outofband.rs.obj_lan_msg_hdr, 
			(uint8_t *)"net_fn", 
			&val);
    
  FIID_OBJ_SET_CLEANUP (obj_hdr, (uint8_t *)"net_fn", val);
  
  FIID_OBJ_GET_ALL_CLEANUP (obj_hdr, buf_rs, 1);
    
  FIID_OBJ_GET_ALL_LEN_CLEANUP (len, obj_cmd_rs, buf_rs + 1, (*buf_rs_len) - 1);

  *buf_rs_len = len + 1;
  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_hdr);
  return (rv);
}

int8_t 
ipmi_lan_cmd_raw2 (ipmi_device_t *dev, 
		   uint8_t *buf_rq, 
		   size_t buf_rq_len, 
		   uint8_t *buf_rs, 
		   size_t *buf_rs_len)
{
  fiid_obj_t obj_cmd_rq = NULL;
  fiid_obj_t obj_cmd_rs = NULL;
  int8_t rv = -1;

  if (!(dev  
	&& dev->io.outofband.local_sockfd
	&& buf_rq
	&& buf_rq_len >= 2
	&& buf_rs
	&& buf_rs_len
	&& *buf_rs_len >= 2))
    {
      errno = EINVAL;
      return (-1);
    }
  
  ERR_CLEANUP (!(_ipmi_lan_raw_send_parse(dev, buf_rq, buf_rq_len) < 0));
  ERR_CLEANUP (!((obj_cmd_rq = _ipmi_lan_raw_create_cmd(dev, buf_rq, buf_rq_len)) < 0));
  ERR_CLEANUP (!(_ipmi_lan_cmd_raw_send (dev, obj_cmd_rq) < 0));
  ERR_CLEANUP (!((obj_cmd_rs = _ipmi_lan_cmd_raw_recv (dev)) < 0));
  ERR_CLEANUP (!(_ipmi_lan_raw_recv_parse(dev, obj_cmd_rs, buf_rs, buf_rs_len) < 0));

  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN (obj_cmd_rq);
  FIID_OBJ_DESTROY_NO_RETURN (obj_cmd_rs);
  return (rv);
}
