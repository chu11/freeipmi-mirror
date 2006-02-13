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

/* XXX - need to clean this up */
static int32_t
_ipmi_lan_pkt_min_size(uint8_t auth_type,
                       fiid_template_t tmpl_lan_msg, 
                       fiid_obj_t obj_cmd)
{
  uint32_t msg_len = 0;
  int32_t len;

  assert(IPMI_SESSION_AUTH_TYPE_VALID(auth_type)
         && tmpl_lan_msg
         && fiid_obj_valid(obj_cmd));

  ERR(!((len = fiid_template_len_bytes (tmpl_hdr_rmcp)) < 0));
  msg_len += len;
  ERR(!((len = fiid_template_len_bytes (tmpl_lan_msg)) < 0));
  msg_len += len;
  ERR(!((len = fiid_template_len_bytes (tmpl_lan_msg_trlr)) < 0));
  msg_len += len;
  ERR(!((len = fiid_template_block_len_bytes (tmpl_lan_session_hdr,
					      (uint8_t *)"auth_type",
					      (uint8_t *)"session_id")) < 0));
  msg_len += len;
  ERR(!((len = fiid_template_field_len_bytes (tmpl_lan_session_hdr,
					      (uint8_t *)"ipmi_msg_len")) < 0));
  msg_len += len;
  
  if (auth_type == IPMI_SESSION_AUTH_TYPE_MD2
      || auth_type == IPMI_SESSION_AUTH_TYPE_MD5
      || auth_type == IPMI_SESSION_AUTH_TYPE_STRAIGHT_PASSWD_KEY
      || auth_type == IPMI_SESSION_AUTH_TYPE_OEM_PROP) 
    msg_len += IPMI_SESSION_MAX_AUTH_CODE_LEN;
  
  return msg_len;
}
                       
static int32_t 
_ipmi_lan_pkt_size (uint8_t auth_type, 
		    fiid_template_t tmpl_lan_msg, 
		    fiid_obj_t obj_cmd)
{
  uint32_t msg_len = 0;
  int32_t len;
  
  assert(IPMI_SESSION_AUTH_TYPE_VALID(auth_type)
         && tmpl_lan_msg
         && fiid_obj_valid(obj_cmd));

  ERR(!((len = _ipmi_lan_pkt_min_size(auth_type, tmpl_lan_msg, obj_cmd)) < 0));
  msg_len += len;
  ERR(!((len = fiid_obj_len_bytes (obj_cmd)) < 0));
  msg_len += len;

  return msg_len;
}

static int32_t 
_ipmi_lan_pkt_rq_size (uint8_t auth_type, fiid_obj_t obj_cmd)
{
  return _ipmi_lan_pkt_size(auth_type, tmpl_lan_msg_hdr_rq, obj_cmd);
}

static int32_t 
_ipmi_max_lan_pkt_size (uint8_t auth_type, 
                        fiid_template_t tmpl_lan_msg, 
                        fiid_obj_t obj_cmd)
{
  uint32_t msg_len = 0;
  int32_t len;
  fiid_field_t *tmpl = NULL;
  int32_t rv = -1;

  assert(IPMI_SESSION_AUTH_TYPE_VALID(auth_type)
         && tmpl_lan_msg
         && fiid_obj_valid(obj_cmd));

  if ((len = _ipmi_lan_pkt_min_size(auth_type, tmpl_lan_msg, obj_cmd)) < 0)
    goto cleanup;
  msg_len += len;
  
  if (!(tmpl = fiid_obj_template(obj_cmd)))
    goto cleanup;
  if ((len = fiid_template_len_bytes(tmpl)) < 0)
    goto cleanup;
  msg_len += len;
  
  rv = msg_len;
 cleanup:
  if (tmpl)
    fiid_template_free(tmpl);
  return (rv);
}

static int32_t 
_ipmi_max_lan_pkt_rs_size (uint8_t auth_type, fiid_obj_t obj_cmd)
{
  return _ipmi_max_lan_pkt_size(auth_type, tmpl_lan_msg_hdr_rs, obj_cmd);
}

int8_t 
ipmi_lan_cmd2 (ipmi_device_t *dev, 
	       fiid_obj_t obj_cmd_rq, 
	       fiid_obj_t obj_cmd_rs)
{
  int8_t rv;

  if (!(dev 
        && dev->io.outofband.local_sockfd 
        && fiid_obj_valid(obj_cmd_rq)
        && fiid_obj_valid(obj_cmd_rs)))
    {
      errno = EINVAL;
      return (-1);
    }
  
  if ((rv = fiid_obj_packet_valid(obj_cmd_rq)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return (-1);
    }

  if (fiid_obj_clear(dev->io.outofband.rq.obj_hdr_rmcp) < 0)
    return (-1);
  if (fiid_obj_clear(dev->io.outofband.rq.obj_hdr_session) < 0)
     return (-1);
  if (fiid_obj_clear(dev->io.outofband.rq.obj_msg_hdr) < 0)
    return (-1);
  if (fiid_obj_clear(dev->io.outofband.rq.obj_msg_trlr) < 0) 
    return (-1);
  if (fiid_obj_clear(dev->io.outofband.rs.obj_hdr_rmcp) < 0) 
    return (-1);
  if (fiid_obj_clear(dev->io.outofband.rs.obj_hdr_session) < 0) 
    return (-1);
  if (fiid_obj_clear(dev->io.outofband.rs.obj_msg_hdr) < 0) 
    return (-1);
  if (fiid_obj_clear(dev->io.outofband.rs.obj_msg_trlr) < 0) 
    return (-1);
  
  {
    uint8_t *pkt;
    int32_t pkt_len;
    int status = 0;
    
    if ((pkt_len = _ipmi_lan_pkt_rq_size (dev->io.outofband.auth_type, obj_cmd_rq)) < 0)
      return (-1);
    pkt = alloca (pkt_len);
    ERR (pkt);
    memset (pkt, 0, pkt_len);
    
    ERR (fill_hdr_rmcp_ipmi (dev->io.outofband.rq.obj_hdr_rmcp) != -1);
    ERR (fill_lan_msg_hdr (dev->net_fn,
                           dev->lun,
                           dev->io.outofband.rq_seq,
                           dev->io.outofband.rq.obj_msg_hdr) != -1);
    ERR (fill_hdr_session (dev->io.outofband.auth_type,
                           dev->io.outofband.session_seq_num,
                           dev->io.outofband.session_id,
                           NULL,
                           0,
                           dev->io.outofband.rq.obj_hdr_session) != -1);
    ERR (assemble_ipmi_lan_pkt (dev->io.outofband.rq.obj_hdr_rmcp,
                                dev->io.outofband.rq.obj_hdr_session,
                                dev->io.outofband.rq.obj_msg_hdr,
                                obj_cmd_rq,
                                dev->io.outofband.password,
                                IPMI_SESSION_MAX_AUTH_CODE_LEN,
                                pkt,
                                pkt_len) != -1);

#if 0
printf("DEBUGGING:\n");

	fiid_obj_dump_lan(STDERR_FILENO,
			  NULL,
			  NULL,
			  pkt,
			  pkt_len,
			  *(dev->io.outofband.rs.tmpl_msg_hdr_ptr));
#endif

    dev->io.outofband.session_seq_num++;
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
    
    if ((max_pkt_len = _ipmi_max_lan_pkt_rs_size (dev->io.outofband.auth_type, obj_cmd_rs)) < 0)
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
                                 dev->io.outofband.rs.obj_hdr_rmcp,
                                 dev->io.outofband.rs.obj_hdr_session,
                                 dev->io.outofband.rs.obj_msg_hdr,
                                 obj_cmd_rs,
                                 dev->io.outofband.rs.obj_msg_trlr) != -1);
    
    ERR (check_hdr_session_authcode (pkt, 
                                     bytes_received, 
                                     dev->io.outofband.auth_type,
                                     dev->io.outofband.password,
                                     IPMI_SESSION_MAX_AUTH_CODE_LEN) == 1);

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

  if (fiid_obj_clear(dev->io.outofband.rq.obj_hdr_rmcp) < 0)
    return (-1);
  if (fiid_obj_clear(dev->io.outofband.rq.obj_hdr_session) < 0)
     return (-1);
  if (fiid_obj_clear(dev->io.outofband.rq.obj_msg_hdr) < 0)
    return (-1);
  if (fiid_obj_clear(dev->io.outofband.rq.obj_msg_trlr) < 0) 
    return (-1);
  if (fiid_obj_clear(dev->io.outofband.rs.obj_hdr_rmcp) < 0) 
    return (-1);
  if (fiid_obj_clear(dev->io.outofband.rs.obj_hdr_session) < 0) 
    return (-1);
  if (fiid_obj_clear(dev->io.outofband.rs.obj_msg_hdr) < 0) 
    return (-1);
  if (fiid_obj_clear(dev->io.outofband.rs.obj_msg_trlr) < 0) 
    return (-1);
  
  {
    uint8_t *pkt;
    uint32_t pkt_len;
    int status = 0;
    
    if ((pkt_len = _ipmi_lan_pkt_rq_size (dev->io.outofband.auth_type, obj_cmd_rq)) < 0)
      return (-1);
    pkt = alloca (pkt_len);
    ERR (pkt);
    memset (pkt, 0, pkt_len);
    
    ERR (fill_hdr_rmcp_ipmi (dev->io.outofband.rq.obj_hdr_rmcp) != -1);
    ERR (fill_lan_msg_hdr (dev->net_fn,
                           dev->lun,
                           dev->io.outofband.rq_seq,
                           dev->io.outofband.rq.obj_msg_hdr) != -1);
    ERR (fill_hdr_session (dev->io.outofband.auth_type,
                           dev->io.outofband.session_seq_num,
                           dev->io.outofband.session_id,
                           NULL,
                           0,
                           dev->io.outofband.rq.obj_hdr_session) != -1);
    ERR (assemble_ipmi_lan_pkt (dev->io.outofband.rq.obj_hdr_rmcp,
                                dev->io.outofband.rq.obj_hdr_session,
                                dev->io.outofband.rq.obj_msg_hdr,
                                obj_cmd_rq,
                                dev->io.outofband.password,
                                IPMI_SESSION_MAX_AUTH_CODE_LEN,
                                pkt,
                                pkt_len) != -1);

    dev->io.outofband.session_seq_num++;
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
    fiid_obj_t obj_hdr;
    fiid_template_t tmpl_hdr_cmd = 
      {
	{2, "lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
	{6, "net_fn", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
	{0, "", 0}
      };
    
    if (!(obj_hdr = fiid_obj_create(tmpl_hdr_cmd)))
      return (-1);

    if (fiid_obj_set_all(obj_hdr,
                         buf_rq,
                         buf_rq_len) < 0)
      {
        fiid_obj_destroy(obj_hdr);
        return (-1);
      }

    if (fiid_obj_get (obj_hdr, (uint8_t *)"lun", &val) < 0)
      {
        fiid_obj_destroy(obj_hdr);
        return (-1);
      }
    dev->lun = val;

    if (fiid_obj_get (obj_hdr, (uint8_t *)"net_fn", &val) < 0)
      {
        fiid_obj_destroy(obj_hdr);
        return (-1);
      }
    dev->net_fn = val;

    fiid_obj_destroy(obj_hdr);
  }
  
  {
    fiid_obj_t obj_cmd_rq = NULL;
    size_t obj_cmd_rq_len = 0;
    fiid_field_t *tmpl_var_cmd_rq = NULL;
    int8_t retval = 0;

    obj_cmd_rq_len = buf_rq_len - 1;

    if (!(tmpl_var_cmd_rq = fiid_template_make ((obj_cmd_rq_len * 8), 
                                                (uint8_t *)"COMMAND_RQ_DATA",
                                                FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED)))
      return (-1);

    if (!(obj_cmd_rq = fiid_obj_create(tmpl_var_cmd_rq)))
      {
        fiid_template_free (tmpl_var_cmd_rq);
        return (-1);
      }
    
    if (fiid_obj_set_all(obj_cmd_rq,
                         buf_rq + 1,
                         buf_rq_len - 1) < 0)
      {
        fiid_obj_destroy(obj_cmd_rq);
        fiid_template_free (tmpl_var_cmd_rq);
        return (-1);
      }

    if ((retval = ipmi_lan_cmd_raw_send (dev, obj_cmd_rq)) < 0)
      {
        fiid_obj_destroy(obj_cmd_rq);
        fiid_template_free (tmpl_var_cmd_rq);
        return (-1);
      }

    fiid_obj_destroy(obj_cmd_rq);
    fiid_template_free (tmpl_var_cmd_rq);
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

    if ((rmcp_len = fiid_template_len_bytes (*(dev->io.outofband.rs.tmpl_hdr_rmcp_ptr))) < 0)
      return (-1);

    if ((hdr_len = fiid_template_len_bytes (*(dev->io.outofband.rs.tmpl_msg_hdr_ptr))) < 0)
      return (-1);

    if ((session_len = fiid_template_block_len_bytes (*(dev->io.outofband.rs.tmpl_lan_session_hdr_ptr),
                                                      (uint8_t *)"auth_type",
                                                      (uint8_t *)"session_id")) < 0)
      return (-1);

    if (dev->io.outofband.auth_type != IPMI_SESSION_AUTH_TYPE_NONE)
      authcode_len = IPMI_SESSION_MAX_AUTH_CODE_LEN;
    else
      authcode_len = 0;

    if ((ipmi_msg_len = fiid_template_field_len_bytes (*(dev->io.outofband.rs.tmpl_lan_session_hdr_ptr),
                                                       (uint8_t *)"ipmi_msg_len")) < 0)
      return (-1);
                                                          
    if ((trlr_len = fiid_template_len_bytes (*(dev->io.outofband.rs.tmpl_msg_trlr_ptr))) < 0)
      return (-1);
    
    pkt_hdrs_size = rmcp_len + hdr_len + session_len + authcode_len + ipmi_msg_len + trlr_len;
    
    pkt_len = 1024 - pkt_hdrs_size - 1;
    ERR (pkt_len <= pkt_max_size);
    
    pkt = alloca (pkt_max_size);
    ERR (pkt);
    memset (pkt, 0, pkt_max_size);
    if ((bytes_received = ipmi_lan_recvfrom (dev->io.outofband.local_sockfd, 
                                             pkt, 
                                             pkt_max_size, 
                                             0, 
                                             (struct sockaddr *) &from, 
                                             &fromlen)) < 0)
      return (-1);

    ERR (bytes_received >= pkt_hdrs_size);
    
    obj_cmd_rs_len = (bytes_received - pkt_hdrs_size);

    if ((buf_rs_len_in - 1) < obj_cmd_rs_len)
      {
        errno = EINVAL;
        return (-1);
      }

    if (!(tmpl_var_cmd_rs = fiid_template_make ((obj_cmd_rs_len * 8), 
                                                (uint8_t *)"COMMAND_RS_DATA",
                                                FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED)))
      return (-1);

    if (!(obj_cmd_rs = fiid_obj_create(tmpl_var_cmd_rs)))
      {
	fiid_template_free (tmpl_var_cmd_rs);
        fiid_obj_destroy(obj_cmd_rs);
        return (-1);
      }

    if (unassemble_ipmi_lan_pkt(pkt,
                                bytes_received,
                                dev->io.outofband.rs.obj_hdr_rmcp,
                                dev->io.outofband.rs.obj_hdr_session,
                                dev->io.outofband.rs.obj_msg_hdr,
                                obj_cmd_rs,
                                dev->io.outofband.rs.obj_msg_trlr) < 0)
      {
	fiid_template_free (tmpl_var_cmd_rs);
        fiid_obj_destroy(obj_cmd_rs);
	return (-1);
      }

    if (check_hdr_session_authcode (pkt, 
                                    bytes_received, 
                                    dev->io.outofband.auth_type,
                                    dev->io.outofband.password,
                                    IPMI_SESSION_MAX_AUTH_CODE_LEN) != 1)
      {
	fiid_template_free (tmpl_var_cmd_rs);
        fiid_obj_destroy(obj_cmd_rs);
	return (-1);
      }

    if (fiid_obj_get_all(obj_cmd_rs,
                         buf_rs + 1,
                         buf_rs_len_in - 1) < 0)
      {
	fiid_template_free (tmpl_var_cmd_rs);
        fiid_obj_destroy(obj_cmd_rs);
	return (-1);
      }
    
    fiid_template_free (tmpl_var_cmd_rs);
    fiid_obj_destroy(obj_cmd_rs);
    
    {
      uint64_t val = 0;
      fiid_obj_t obj_hdr;
      fiid_template_t tmpl_hdr_cmd = 
	{
	  {2, "lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
	  {6, "net_fn", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
	  {0, "", 0}
	};
      
      if (!(obj_hdr = fiid_obj_create(tmpl_hdr_cmd)))
        return (-1);

      if (fiid_obj_get (dev->io.outofband.rs.obj_msg_hdr, 
                        (uint8_t *)"rq_lun", &val) < 0)
        {
          fiid_obj_destroy(obj_hdr);
          return (-1);
        }

      if (fiid_obj_set (obj_hdr,
                        (uint8_t *)"lun",
                        val) < 0)
        {
          fiid_obj_destroy(obj_hdr);
          return (-1);
        }

      if (fiid_obj_get (dev->io.outofband.rs.obj_msg_hdr, 
                        (uint8_t *)"net_fn", &val) < 0)
        {
          fiid_obj_destroy(obj_hdr);
          return (-1);
        }

      if (fiid_obj_set(obj_hdr,
                       (uint8_t *)"net_fn", 
                       val) < 0)
        {
          fiid_obj_destroy(obj_hdr);
          return (-1);
        }
        
      if (fiid_obj_get_all(obj_hdr,
                           buf_rs,
                           1) < 0)
        {
          fiid_obj_destroy(obj_hdr);
          return (-1);
        }

      fiid_obj_destroy(obj_hdr);
      *buf_rs_len = obj_cmd_rs_len + 1;
    }
  }
  
  return (0);
}
