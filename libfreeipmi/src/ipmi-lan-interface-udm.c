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

#include "freeipmi.h"
#include "err-wrappers.h"
#include "fiid-wrappers.h"

/* XXX - need to clean this up */
static int32_t
_ipmi_lan_pkt_min_size(uint8_t authentication_type,
                       fiid_template_t tmpl_lan_msg, 
                       fiid_obj_t obj_cmd)
{
  uint32_t msg_len = 0;
  int32_t len;

  assert(IPMI_AUTHENTICATION_TYPE_VALID(authentication_type)
         && tmpl_lan_msg
         && fiid_obj_valid(obj_cmd));

  FIID_TEMPLATE_LEN_BYTES (len, tmpl_rmcp_hdr);
  msg_len += len;
  FIID_TEMPLATE_LEN_BYTES (len, tmpl_lan_msg);
  msg_len += len;
  FIID_TEMPLATE_LEN_BYTES (len, tmpl_lan_msg_trlr);
  msg_len += len;
  FIID_TEMPLATE_BLOCK_LEN_BYTES (len,
				 tmpl_lan_session_hdr,
				 (uint8_t *)"authentication_type",
				 (uint8_t *)"session_id");
  msg_len += len;
  FIID_TEMPLATE_FIELD_LEN_BYTES (len,
				 tmpl_lan_session_hdr,
				 (uint8_t *)"ipmi_msg_len");
  msg_len += len;
  
  if (authentication_type == IPMI_AUTHENTICATION_TYPE_MD2
      || authentication_type == IPMI_AUTHENTICATION_TYPE_MD5
      || authentication_type == IPMI_AUTHENTICATION_TYPE_STRAIGHT_PASSWORD_KEY
      || authentication_type == IPMI_AUTHENTICATION_TYPE_OEM_PROP) 
    msg_len += IPMI_MAX_AUTHENTICATION_CODE_LENGTH;
  
  return msg_len;
}
                       
static int32_t 
_ipmi_lan_pkt_size (uint8_t authentication_type, 
		    fiid_template_t tmpl_lan_msg, 
		    fiid_obj_t obj_cmd)
{
  uint32_t msg_len = 0;
  int32_t len;
  
  assert(IPMI_AUTHENTICATION_TYPE_VALID(authentication_type)
         && tmpl_lan_msg
         && fiid_obj_valid(obj_cmd));

  ERR(!((len = _ipmi_lan_pkt_min_size(authentication_type, tmpl_lan_msg, obj_cmd)) < 0));
  msg_len += len;
  FIID_OBJ_LEN_BYTES (len, obj_cmd);
  msg_len += len;

  return msg_len;
}

static int32_t 
_ipmi_lan_pkt_rq_size (uint8_t authentication_type, fiid_obj_t obj_cmd)
{
  return _ipmi_lan_pkt_size(authentication_type, tmpl_lan_msg_hdr_rq, obj_cmd);
}

static int32_t 
_ipmi_max_lan_pkt_size (uint8_t authentication_type, 
                        fiid_template_t tmpl_lan_msg, 
                        fiid_obj_t obj_cmd)
{
  uint32_t msg_len = 0;
  int32_t len;
  fiid_field_t *tmpl = NULL;
  int32_t rv = -1;

  assert(IPMI_AUTHENTICATION_TYPE_VALID(authentication_type)
         && tmpl_lan_msg
         && fiid_obj_valid(obj_cmd));

  ERR_CLEANUP (!((len = _ipmi_lan_pkt_min_size(authentication_type, 
					       tmpl_lan_msg, 
					       obj_cmd)) < 0));
  msg_len += len;
  
  FIID_OBJ_TEMPLATE_CLEANUP(tmpl, obj_cmd);
  FIID_TEMPLATE_LEN_BYTES_CLEANUP(len, tmpl);
  msg_len += len;
  
  rv = msg_len;
 cleanup:
  FIID_TEMPLATE_FREE_NO_RETURN(tmpl);
  return (rv);
}

static int32_t 
_ipmi_max_lan_pkt_rs_size (uint8_t authentication_type, fiid_obj_t obj_cmd)
{
  return _ipmi_max_lan_pkt_size(authentication_type, tmpl_lan_msg_hdr_rs, obj_cmd);
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

  FIID_OBJ_CLEAR(dev->io.outofband.rq.obj_rmcp_hdr);
  FIID_OBJ_CLEAR(dev->io.outofband.rq.obj_lan_session_hdr);
  FIID_OBJ_CLEAR(dev->io.outofband.rq.obj_lan_msg_hdr);
  FIID_OBJ_CLEAR(dev->io.outofband.rq.obj_lan_msg_trlr); 
  FIID_OBJ_CLEAR(dev->io.outofband.rs.obj_rmcp_hdr); 
  FIID_OBJ_CLEAR(dev->io.outofband.rs.obj_lan_session_hdr); 
  FIID_OBJ_CLEAR(dev->io.outofband.rs.obj_lan_msg_hdr); 
  FIID_OBJ_CLEAR(dev->io.outofband.rs.obj_lan_msg_trlr); 
  
  {
    uint8_t *pkt;
    int32_t pkt_len;
    int status = 0;
    
    if ((pkt_len = _ipmi_lan_pkt_rq_size (dev->io.outofband.authentication_type, obj_cmd_rq)) < 0)
      return (-1);
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

#if 0
    printf("DEBUGGING:\n");
    
    ipmi_dump_lan_packet (STDERR_FILENO,
			  NULL,
			  NULL,
			  pkt,
			  pkt_len,
			  *(dev->io.outofband.rs.tmpl_msg_hdr_ptr));
#endif

    dev->io.outofband.session_sequence_number++;
    IPMI_LAN_RQ_SEQ_INC (dev->io.outofband.rq_seq);
    
    status = ipmi_lan_sendto (dev->io.outofband.local_sockfd, pkt, pkt_len, 0, 
			      &(dev->io.outofband.remote_host), 
			      dev->io.outofband.remote_host_len);
    ERR (status != -1);
  }
  
  {
    struct sockaddr_in from;
    socklen_t fromlen = 0;
    
    uint8_t *pkt = NULL;
    uint32_t pkt_max_size = 1024;
    int32_t bytes_received = 0;
    int32_t max_pkt_len;
    
    if ((max_pkt_len = _ipmi_max_lan_pkt_rs_size (dev->io.outofband.authentication_type, obj_cmd_rs)) < 0)
      return (-1);
    ERR (max_pkt_len <= pkt_max_size);
    
    pkt = alloca (pkt_max_size);
    ERR (pkt);
    memset (pkt, 0, pkt_max_size);
    ERR (!((bytes_received = ipmi_lan_recvfrom (dev->io.outofband.local_sockfd, 
                                                pkt, 
                                                pkt_max_size, 
                                                0, 
                                                (struct sockaddr *) &from, 
                                                &fromlen)) < 0));
    
    ERR (unassemble_ipmi_lan_pkt(pkt,
                                 bytes_received,
                                 dev->io.outofband.rs.obj_rmcp_hdr,
                                 dev->io.outofband.rs.obj_lan_session_hdr,
                                 dev->io.outofband.rs.obj_lan_msg_hdr,
                                 obj_cmd_rs,
                                 dev->io.outofband.rs.obj_lan_msg_trlr) != -1);
    
    ERR (ipmi_lan_check_session_authcode (pkt, 
                                          bytes_received, 
                                          dev->io.outofband.authentication_type,
                                          dev->io.outofband.password,
                                          IPMI_MAX_AUTHENTICATION_CODE_LENGTH) == 1);

  }
  
  return (0);
}

static int8_t 
ipmi_lan_cmd_raw_send (ipmi_device_t *dev, 
		       fiid_obj_t obj_cmd_rq)
{
  if (!dev || !fiid_obj_valid(obj_cmd_rq))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_CLEAR(dev->io.outofband.rq.obj_rmcp_hdr);
  FIID_OBJ_CLEAR(dev->io.outofband.rq.obj_lan_session_hdr);
  FIID_OBJ_CLEAR(dev->io.outofband.rq.obj_lan_msg_hdr);
  FIID_OBJ_CLEAR(dev->io.outofband.rq.obj_lan_msg_trlr); 
  FIID_OBJ_CLEAR(dev->io.outofband.rs.obj_rmcp_hdr); 
  FIID_OBJ_CLEAR(dev->io.outofband.rs.obj_lan_session_hdr); 
  FIID_OBJ_CLEAR(dev->io.outofband.rs.obj_lan_msg_hdr); 
  FIID_OBJ_CLEAR(dev->io.outofband.rs.obj_lan_msg_trlr); 
  
  {
    uint8_t *pkt;
    uint32_t pkt_len;
    int status = 0;
    
    if ((pkt_len = _ipmi_lan_pkt_rq_size (dev->io.outofband.authentication_type, obj_cmd_rq)) < 0)
      return (-1);
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
    
    status = ipmi_lan_sendto (dev->io.outofband.local_sockfd, pkt, pkt_len, 0, 
			      &(dev->io.outofband.remote_host), 
			      dev->io.outofband.remote_host_len);
    ERR (status != -1);
  }
  
  return (0);
}

int8_t 
ipmi_lan_cmd_raw2 (ipmi_device_t *dev, 
		   uint8_t *buf_rq, 
		   size_t buf_rq_len, 
		   uint8_t *buf_rs, 
		   size_t *buf_rs_len)
{
  size_t buf_rs_len_in;

  if (!(dev && 
	dev->io.outofband.local_sockfd && 
	buf_rq && 
	buf_rq_len >= 2 && 
	buf_rs && 
	buf_rs_len && 
	*buf_rs_len))
    {
      errno = EINVAL;
      return (-1);
    }
  
  buf_rs_len_in = *buf_rs_len;
  *buf_rs_len = 0;
  
  {
    uint64_t val = 0;
    fiid_obj_t obj_hdr = NULL;
    fiid_template_t tmpl_hdr_cmd = 
      {
	{2, "lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
	{6, "net_fn", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
	{0, "", 0}
      };
    int8_t err_flag = 0;
    
    FIID_OBJ_CREATE_CLEANUP1 (obj_hdr, tmpl_hdr_cmd);

    FIID_OBJ_SET_ALL_CLEANUP1 (obj_hdr, buf_rq, buf_rq_len);

    FIID_OBJ_GET_CLEANUP1 (obj_hdr, (uint8_t *)"lun", &val);
    dev->lun = val;

    FIID_OBJ_GET_CLEANUP1 (obj_hdr, (uint8_t *)"net_fn", &val);
    dev->net_fn = val;

    err_flag++;
  cleanup1:
    FIID_OBJ_DESTROY_NO_RETURN(obj_hdr);
    if (!err_flag)
      return (-1);
  }
  
  {
    fiid_obj_t obj_cmd_rq = NULL;
    size_t obj_cmd_rq_len = 0;
    fiid_field_t *tmpl_var_cmd_rq = NULL;
    int8_t retval = 0;
    int8_t err_flag = 0;

    obj_cmd_rq_len = buf_rq_len - 1;

    if (!(tmpl_var_cmd_rq = fiid_template_make ((obj_cmd_rq_len * 8), 
                                                (uint8_t *)"COMMAND_RQ_DATA",
                                                FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED)))
      return (-1);

    FIID_OBJ_CREATE_CLEANUP2 (obj_cmd_rq, tmpl_var_cmd_rq);
    
    FIID_OBJ_SET_ALL_CLEANUP2 (obj_cmd_rq, buf_rq + 1, buf_rq_len - 1);

    ERR_CLEANUP2 (!((retval = ipmi_lan_cmd_raw_send (dev, obj_cmd_rq)) < 0));

    err_flag++;
  cleanup2:
    FIID_OBJ_DESTROY_NO_RETURN (obj_cmd_rq);
    FIID_TEMPLATE_FREE_NO_RETURN (tmpl_var_cmd_rq);
    if (!err_flag)
      return (-1);
  }
  
  {
    fiid_obj_t obj_cmd_rs = NULL;
    size_t obj_cmd_rs_len = 0; 
    fiid_field_t *tmpl_var_cmd_rs = NULL;
    
    struct sockaddr_in from;
    socklen_t fromlen = 0;
    
    uint8_t *pkt = NULL;
    uint32_t pkt_max_size = 1024;
    int32_t pkt_len;
    int32_t bytes_received;
    int32_t pkt_hdrs_size;
    int32_t rmcp_len, hdr_len, session_len, authcode_len, ipmi_msg_len, trlr_len;
    int8_t err_flag = 0;

    FIID_TEMPLATE_LEN_BYTES_CLEANUP3 (rmcp_len, *(dev->io.outofband.rs.tmpl_rmcp_hdr_ptr));

    FIID_TEMPLATE_LEN_BYTES_CLEANUP3 (hdr_len, *(dev->io.outofband.rs.tmpl_lan_msg_hdr_ptr));

    FIID_TEMPLATE_BLOCK_LEN_BYTES_CLEANUP3 (session_len, 
					    *(dev->io.outofband.rs.tmpl_lan_session_hdr_ptr),
					    (uint8_t *)"authentication_type",
					    (uint8_t *)"session_id");

    if (dev->io.outofband.authentication_type != IPMI_AUTHENTICATION_TYPE_NONE)
      authcode_len = IPMI_MAX_AUTHENTICATION_CODE_LENGTH;
    else
      authcode_len = 0;

    FIID_TEMPLATE_FIELD_LEN_BYTES_CLEANUP3 (ipmi_msg_len, 
					    *(dev->io.outofband.rs.tmpl_lan_session_hdr_ptr),
					    (uint8_t *)"ipmi_msg_len");
                                                          
    FIID_TEMPLATE_LEN_BYTES_CLEANUP3 (trlr_len, *(dev->io.outofband.rs.tmpl_lan_msg_trlr_ptr));
    
    pkt_hdrs_size = rmcp_len + hdr_len + session_len + authcode_len + ipmi_msg_len + trlr_len;
    
    pkt_len = 1024 - pkt_hdrs_size - 1;
    ERR (pkt_len <= pkt_max_size);
    
    pkt = alloca (pkt_max_size);
    ERR (pkt);
    memset (pkt, 0, pkt_max_size);
    ERR_CLEANUP3 (!((bytes_received = ipmi_lan_recvfrom (dev->io.outofband.local_sockfd, 
							 pkt, 
							 pkt_max_size, 
							 0, 
							 (struct sockaddr *) &from, 
							 &fromlen)) < 0));
    ERR (bytes_received >= pkt_hdrs_size);
    
    obj_cmd_rs_len = (bytes_received - pkt_hdrs_size);

    if ((buf_rs_len_in - 1) < obj_cmd_rs_len)
      {
        errno = EINVAL;
	goto cleanup3;
      }

    ERR_CLEANUP3 ((tmpl_var_cmd_rs = fiid_template_make ((obj_cmd_rs_len * 8), 
							 (uint8_t *)"COMMAND_RS_DATA",
							 FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED)));

    FIID_OBJ_CREATE_CLEANUP3(obj_cmd_rs, tmpl_var_cmd_rs);

    ERR_CLEANUP3 (!(unassemble_ipmi_lan_pkt(pkt,
					    bytes_received,
					    dev->io.outofband.rs.obj_rmcp_hdr,
					    dev->io.outofband.rs.obj_lan_session_hdr,
					    dev->io.outofband.rs.obj_lan_msg_hdr,
					    obj_cmd_rs,
					    dev->io.outofband.rs.obj_lan_msg_trlr) < 0));

    ERR_CLEANUP3 (!(ipmi_lan_check_session_authcode (pkt, 
						     bytes_received, 
						     dev->io.outofband.authentication_type,
						     dev->io.outofband.password,
						     IPMI_MAX_AUTHENTICATION_CODE_LENGTH) != 1));

    FIID_OBJ_GET_ALL_CLEANUP3 (obj_cmd_rs, buf_rs + 1, buf_rs_len_in - 1);

    err_flag++;
  cleanup3:    
    FIID_OBJ_DESTROY_NO_RETURN (obj_cmd_rs);
    FIID_TEMPLATE_FREE_NO_RETURN (tmpl_var_cmd_rs);
    if (!err_flag)
      return (-1);

    {
      uint64_t val = 0;
      fiid_obj_t obj_hdr = NULL;
      fiid_template_t tmpl_hdr_cmd = 
	{
	  {2, "lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
	  {6, "net_fn", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
	  {0, "", 0}
	};
      
      err_flag = 0;
      
      FIID_OBJ_CREATE_CLEANUP4(obj_hdr, tmpl_hdr_cmd);
      
      FIID_OBJ_GET_CLEANUP4 (dev->io.outofband.rs.obj_lan_msg_hdr, 
			     (uint8_t *)"rq_lun", 
			     &val);
      
      FIID_OBJ_SET_CLEANUP4 (obj_hdr, (uint8_t *)"lun", val);
      
      FIID_OBJ_GET_CLEANUP4 (dev->io.outofband.rs.obj_lan_msg_hdr, 
			     (uint8_t *)"net_fn", 
			     &val);
      
      FIID_OBJ_SET_CLEANUP4 (obj_hdr, (uint8_t *)"net_fn", val);
      
      FIID_OBJ_GET_ALL_CLEANUP4 (obj_hdr, buf_rs, 1);
      
      err_flag++;
    cleanup4:
      FIID_OBJ_DESTROY_NO_RETURN(obj_hdr);
      if (!err_flag)
	return (-1);
      *buf_rs_len = obj_cmd_rs_len + 1;
    }
  }
  
  return (0);
}
