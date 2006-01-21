/* 
   ipmi-lan-interface.c - IPMI LAN Interface

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

/* IPMI LAN Message Request Header */
fiid_template_t tmpl_lan_msg_hdr_rq =
  {
    {8, "rs_addr"},
    {2, "rs_lun"},
    {6, "net_fn"},
    {8, "chksum1"},
    {8, "rq_addr"},
    {2, "rq_lun"},
    {6, "rq_seq"},
    {0, ""}
  };

/* IPMI LAN Message Response Header */
fiid_template_t tmpl_lan_msg_hdr_rs =
  {
    {8, "rq_addr"},
    {2, "rq_lun"},
    {6, "net_fn"},
    {8, "chksum1"},
    {8, "rs_addr"},
    {2, "rs_lun"},
    {6, "rq_seq"},
    {0, ""}
  };

/* IPMI LAN Message Trailer */
fiid_template_t tmpl_lan_msg_trlr = 
  {
    {8, "chksum2"},
    {0, ""}
  };

int 
get_rq_checksum1 (ipmi_device_t *dev, uint8_t *checksum)
{
  *checksum = ipmi_chksum (dev->io.outofband.rq.obj_msg_hdr, 2);
  return (0);
}

int 
get_rs_checksum1 (ipmi_device_t *dev, uint8_t *checksum)
{
  *checksum = ipmi_chksum (dev->io.outofband.rs.obj_msg_hdr, 2);
  return (0);
}

int 
get_rq_checksum2 (ipmi_device_t *dev, 
		  fiid_obj_t obj_cmd, 
		  fiid_template_t tmpl_cmd, 
		  uint8_t *checksum)
{
  fiid_field_t *tmpl_var_checksum2_data = NULL;
  fiid_obj_t var_checksum2_data = NULL;
  int var_checksum2_data_length = 0;
  int cmd_length = 0;
  uint64_t val = 0;
  
  ERR ((cmd_length = fiid_obj_len_bytes (tmpl_cmd)) != -1);
  
  tmpl_var_checksum2_data = fiid_template_make (8, "rq_addr", 
						2, "rq_lun", 
						6, "rq_seq", 
						(cmd_length * 8), "COMMAND_DATA");
  var_checksum2_data_length = fiid_obj_len_bytes (tmpl_var_checksum2_data);
  var_checksum2_data = alloca (var_checksum2_data_length);
  memset (var_checksum2_data, 0, var_checksum2_data_length);
  
  if (fiid_obj_get (dev->io.outofband.rq.obj_msg_hdr,
		    *(dev->io.outofband.rq.tmpl_msg_hdr_ptr), 
		    (uint8_t *)"rq_addr", 
		    &val) == -1)
    {
      free (tmpl_var_checksum2_data);
      return (-1);
    }
  if (fiid_obj_set (var_checksum2_data, 
		    tmpl_var_checksum2_data, 
		    (uint8_t *)"rq_addr", 
		    val) == -1)
    {
      free (tmpl_var_checksum2_data);
      return (-1);
    }
  
  if (fiid_obj_get (dev->io.outofband.rq.obj_msg_hdr,
		    *(dev->io.outofband.rq.tmpl_msg_hdr_ptr), 
		    (uint8_t *)"rq_lun", 
		    &val) == -1)
    {
      free (tmpl_var_checksum2_data);
      return (-1);
    }
  if (fiid_obj_set (var_checksum2_data, 
		    tmpl_var_checksum2_data, 
		    (uint8_t *)"rq_lun", 
		    val) == -1)
    {
      free (tmpl_var_checksum2_data);
      return (-1);
    }
  
  if (fiid_obj_get (dev->io.outofband.rq.obj_msg_hdr,
		    *(dev->io.outofband.rq.tmpl_msg_hdr_ptr), 
		    (uint8_t *)"rq_seq", 
		    &val) == -1)
    {
      free (tmpl_var_checksum2_data);
      return (-1);
    }
  if (fiid_obj_set (var_checksum2_data, 
		    tmpl_var_checksum2_data, 
		    (uint8_t *)"rq_seq", 
		    val) == -1)
    {
      free (tmpl_var_checksum2_data);
      return (-1);
    }
  
  if (fiid_obj_set_data (var_checksum2_data, 
			 tmpl_var_checksum2_data, 
			 (uint8_t *)"COMMAND_DATA", 
			 obj_cmd, 
			 cmd_length) == -1)
    {
      free (tmpl_var_checksum2_data);
      return (-1);
    }
  
  *checksum = ipmi_chksum (var_checksum2_data, 
			   var_checksum2_data_length);
  
  free (tmpl_var_checksum2_data);
  return (0);
}

int 
get_rs_checksum2 (ipmi_device_t *dev, 
		  fiid_obj_t obj_cmd, 
		  fiid_template_t tmpl_cmd, 
		  uint8_t *checksum)
{
  fiid_field_t *tmpl_var_checksum2_data = NULL;
  fiid_obj_t var_checksum2_data = NULL;
  int var_checksum2_data_length = 0;
  int cmd_length = 0;
  uint64_t val = 0;
  
  ERR ((cmd_length = fiid_obj_len_bytes (tmpl_cmd)) != -1);
  
  tmpl_var_checksum2_data = fiid_template_make (8, "rs_addr", 
						2, "rs_lun", 
						6, "rq_seq", 
						(cmd_length * 8), "COMMAND_DATA");
  var_checksum2_data_length = fiid_obj_len_bytes (tmpl_var_checksum2_data);
  var_checksum2_data = alloca (var_checksum2_data_length);
  memset (var_checksum2_data, 0, var_checksum2_data_length);
  
  if (fiid_obj_get (dev->io.outofband.rs.obj_msg_hdr,
		    *(dev->io.outofband.rs.tmpl_msg_hdr_ptr), 
		    (uint8_t *)"rs_addr", 
		    &val) == -1)
    {
      free (tmpl_var_checksum2_data);
      return (-1);
    }
  if (fiid_obj_set (var_checksum2_data, 
		    tmpl_var_checksum2_data, 
		    (uint8_t *)"rs_addr", 
		    val) == -1)
    {
      free (tmpl_var_checksum2_data);
      return (-1);
    }
  
  if (fiid_obj_get (dev->io.outofband.rs.obj_msg_hdr,
		    *(dev->io.outofband.rs.tmpl_msg_hdr_ptr), 
		    (uint8_t *)"rs_lun", 
		    &val) == -1)
    {
      free (tmpl_var_checksum2_data);
      return (-1);
    }
  if (fiid_obj_set (var_checksum2_data, 
		    tmpl_var_checksum2_data, 
		    (uint8_t *)"rs_lun", 
		    val) == -1)
    {
      free (tmpl_var_checksum2_data);
      return (-1);
    }
  
  if (fiid_obj_get (dev->io.outofband.rs.obj_msg_hdr,
		    *(dev->io.outofband.rs.tmpl_msg_hdr_ptr), 
		    (uint8_t *)"rq_seq", 
		    &val) == -1)
    {
      free (tmpl_var_checksum2_data);
      return (-1);
    }
  if (fiid_obj_set (var_checksum2_data, 
		    tmpl_var_checksum2_data, 
		    (uint8_t *)"rq_seq", 
		    val) == -1)
    {
      free (tmpl_var_checksum2_data);
      return (-1);
    }
  
  if (fiid_obj_set_data (var_checksum2_data, 
			 tmpl_var_checksum2_data, 
			 (uint8_t *)"COMMAND_DATA", 
			 obj_cmd, 
			 cmd_length) == -1)
    {
      free (tmpl_var_checksum2_data);
      return (-1);
    }
  
  *checksum = ipmi_chksum (var_checksum2_data, 
			   var_checksum2_data_length);
  
  free (tmpl_var_checksum2_data);
  return (0);
}

int8_t 
fill_lan_msg_hdr (uint8_t net_fn, 
		  uint8_t rs_lun, 
		  uint8_t rq_seq, 
		  fiid_obj_t obj_msg)
{
  if ((net_fn > IPMI_NET_FN_TRANSPORT_RS)
      || (rs_lun > IPMI_BMC_IPMB_LUN_OEM_LUN2)
      || (rq_seq > IPMI_LAN_SEQ_NUM_MAX)
      || (obj_msg == NULL))
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_SET (obj_msg, tmpl_lan_msg_hdr_rq, (uint8_t *)"rs_addr", IPMI_SLAVE_ADDR_BMC);
  FIID_OBJ_SET (obj_msg, tmpl_lan_msg_hdr_rq, (uint8_t *)"net_fn", net_fn);
  FIID_OBJ_SET (obj_msg, tmpl_lan_msg_hdr_rq, (uint8_t *)"rs_lun", rs_lun);
  FIID_OBJ_SET (obj_msg, tmpl_lan_msg_hdr_rq, (uint8_t *)"chksum1", 
		ipmi_chksum (obj_msg, IPMI_LAN_PKT_RQ_CHKSUM1_BLOCK_LEN));
  FIID_OBJ_SET (obj_msg, tmpl_lan_msg_hdr_rq, (uint8_t *)"rq_addr", IPMI_SLAVE_ADDR_SWID);
  FIID_OBJ_SET (obj_msg, tmpl_lan_msg_hdr_rq, (uint8_t *)"rq_lun", IPMI_BMC_IPMB_LUN_BMC);
  FIID_OBJ_SET (obj_msg, tmpl_lan_msg_hdr_rq, (uint8_t *)"rq_seq", rq_seq);
  return (0);
}

int8_t 
fill_lan_msg_hdr2 (ipmi_device_t *dev)
{
  uint8_t checksum = 0;
  
  if (dev->io.outofband.rq.obj_msg_hdr == NULL)
    {
      errno = EINVAL;
      return -1;
    }
  
  FIID_OBJ_SET (dev->io.outofband.rq.obj_msg_hdr, 
		*(dev->io.outofband.rq.tmpl_msg_hdr_ptr), 
		(uint8_t *)"rs_addr", 
		IPMI_SLAVE_ADDR_BMC);
  FIID_OBJ_SET (dev->io.outofband.rq.obj_msg_hdr, 
		*(dev->io.outofband.rq.tmpl_msg_hdr_ptr), 
		(uint8_t *)"rs_lun", 
		dev->lun);
  FIID_OBJ_SET (dev->io.outofband.rq.obj_msg_hdr, 
		*(dev->io.outofband.rq.tmpl_msg_hdr_ptr), 
		(uint8_t *)"net_fn", 
		dev->net_fn);
  get_rq_checksum1 (dev, &checksum);
  FIID_OBJ_SET (dev->io.outofband.rq.obj_msg_hdr, 
		*(dev->io.outofband.rq.tmpl_msg_hdr_ptr), 
		(uint8_t *)"chksum1", 
		checksum);
  FIID_OBJ_SET (dev->io.outofband.rq.obj_msg_hdr, 
		*(dev->io.outofband.rq.tmpl_msg_hdr_ptr), 
		(uint8_t *)"rq_addr", 
		IPMI_SLAVE_ADDR_SWID);
  FIID_OBJ_SET (dev->io.outofband.rq.obj_msg_hdr, 
		*(dev->io.outofband.rq.tmpl_msg_hdr_ptr), 
		(uint8_t *)"rq_lun", 
		IPMI_BMC_IPMB_LUN_BMC);
  FIID_OBJ_SET (dev->io.outofband.rq.obj_msg_hdr, 
		*(dev->io.outofband.rq.tmpl_msg_hdr_ptr), 
		(uint8_t *)"rq_seq", 
		dev->io.outofband.rq_seq);
  
  return (0);
}

int8_t 
fill_lan_msg_trlr2 (ipmi_device_t *dev, 
		    fiid_obj_t obj_cmd, 
		    fiid_template_t tmpl_cmd)
{
  uint8_t checksum = 0;
  
  ERR (get_rq_checksum2 (dev, 
			 obj_cmd, 
			 tmpl_cmd, 
			 &checksum) == 0);
  
  FIID_OBJ_SET (dev->io.outofband.rq.obj_msg_trlr, 
		*(dev->io.outofband.rq.tmpl_msg_trlr_ptr), 
		(uint8_t *)"chksum2", 
		checksum);
  
  return (0);
}

int8_t 
fill_hdr_session2 (ipmi_device_t *dev, 
		   fiid_obj_t obj_cmd, 
		   fiid_template_t tmpl_cmd)
{
  uint8_t *auth_code = NULL;
  uint8_t auth_code_length = 0;
  
  if (dev == NULL || tmpl_cmd == NULL)
    {
      errno = EINVAL;
      return (-1);
    }
  
 
  FIID_OBJ_SET (dev->io.outofband.rq.obj_hdr_session, 
		*(dev->io.outofband.rq.tmpl_hdr_session_ptr), 
		(uint8_t *)"auth_type", 
		dev->io.outofband.auth_type);
  FIID_OBJ_SET (dev->io.outofband.rq.obj_hdr_session, 
		*(dev->io.outofband.rq.tmpl_hdr_session_ptr), 
		(uint8_t *)"session_seq_num", 
		dev->io.outofband.session_seq_num);
  FIID_OBJ_SET (dev->io.outofband.rq.obj_hdr_session, 
		*(dev->io.outofband.rq.tmpl_hdr_session_ptr), 
		(uint8_t *)"session_id", 
		dev->io.outofband.session_id);
  
  switch (dev->io.outofband.auth_type)
    {
    case IPMI_SESSION_AUTH_TYPE_NONE:
      break;
    case IPMI_SESSION_AUTH_TYPE_STRAIGHT_PASSWD_KEY:
      /* XXX: Bala, you assume that the template always passed in
       * is contains a field called "auth_code" which is incorrect.
       *
       * Please see fill_hdr_session().
       */
      ERR (fiid_obj_set_data (dev->io.outofband.rq.obj_hdr_session, 
			      *(dev->io.outofband.rq.tmpl_hdr_session_ptr), 
			      (uint8_t *)"auth_code", 
			      dev->io.outofband.password, 
			      IPMI_SESSION_MAX_AUTH_CODE_LEN) != -1);
      break;
    case IPMI_SESSION_AUTH_TYPE_MD2:
      {
	ipmi_md2_t ctx;
	uint8_t digest[IPMI_MD2_DIGEST_LEN];
	
	ipmi_md2_init (&ctx);
	ipmi_md2_update_data (&ctx, 
			      dev->io.outofband.password,
			      IPMI_SESSION_MAX_AUTH_CODE_LEN);
	ipmi_md2_update_data (&ctx, 
			      (uint8_t *)&(dev->io.outofband.session_id), 
			      sizeof (dev->io.outofband.session_id));
	ipmi_md2_update_data (&ctx, 
			      dev->io.outofband.rq.obj_msg_hdr, 
			      fiid_obj_len_bytes (*(dev->io.outofband.rq.tmpl_msg_hdr_ptr)));
	ipmi_md2_update_data (&ctx, 
			      obj_cmd, 
			      fiid_obj_len_bytes (tmpl_cmd));
	ipmi_md2_update_data (&ctx, 
			      dev->io.outofband.rq.obj_msg_trlr, 
			      fiid_obj_len_bytes (*(dev->io.outofband.rq.tmpl_msg_trlr_ptr)));
	ipmi_md2_update_data (&ctx, 
			      (uint8_t *)&(dev->io.outofband.session_seq_num), 
			      sizeof (dev->io.outofband.session_seq_num));
	ipmi_md2_update_data (&ctx, 
			      dev->io.outofband.password,
			      IPMI_SESSION_MAX_AUTH_CODE_LEN);
	ipmi_md2_finish (&ctx, digest, IPMI_MD2_DIGEST_LEN);
	
	auth_code_length = IPMI_MD2_DIGEST_LEN;
	auth_code = alloca (auth_code_length);
	memset (auth_code, 0, auth_code_length);
	memcpy (auth_code, digest, auth_code_length);
      }
      ERR (fiid_obj_set_data (dev->io.outofband.rq.obj_hdr_session, 
			      *(dev->io.outofband.rq.tmpl_hdr_session_ptr), 
			      (uint8_t *)"auth_code", 
			      auth_code, 
			      auth_code_length) != -1);
      break;
    case IPMI_SESSION_AUTH_TYPE_MD5:
      {
	ipmi_md5_t ctx;
	uint8_t digest[IPMI_MD5_DIGEST_LEN];
	
	ipmi_md5_init (&ctx);
	ipmi_md5_update_data (&ctx, 
			      dev->io.outofband.password,	      
			      IPMI_SESSION_MAX_AUTH_CODE_LEN);
	ipmi_md5_update_data (&ctx, 
			      (uint8_t *)&(dev->io.outofband.session_id), 
			      sizeof (dev->io.outofband.session_id));
	ipmi_md5_update_data (&ctx, 
			      dev->io.outofband.rq.obj_msg_hdr, 
			      fiid_obj_len_bytes (*(dev->io.outofband.rq.tmpl_msg_hdr_ptr)));
	ipmi_md5_update_data (&ctx, 
			      obj_cmd, 
			      fiid_obj_len_bytes (tmpl_cmd));
	ipmi_md5_update_data (&ctx, 
			      dev->io.outofband.rq.obj_msg_trlr, 
			      fiid_obj_len_bytes (*(dev->io.outofband.rq.tmpl_msg_trlr_ptr)));
	ipmi_md5_update_data (&ctx, 
			      (uint8_t *)&(dev->io.outofband.session_seq_num), 
			      sizeof (dev->io.outofband.session_seq_num));
	ipmi_md5_update_data (&ctx, 
			      dev->io.outofband.password,
			      IPMI_SESSION_MAX_AUTH_CODE_LEN);
	ipmi_md5_finish (&ctx, digest, IPMI_MD5_DIGEST_LEN);
	
	auth_code_length = IPMI_MD5_DIGEST_LEN;
	auth_code = alloca (auth_code_length);
	memset (auth_code, 0, auth_code_length);
	memcpy (auth_code, digest, auth_code_length);
      }
      ERR (fiid_obj_set_data (dev->io.outofband.rq.obj_hdr_session, 
			      *(dev->io.outofband.rq.tmpl_hdr_session_ptr), 
			      (uint8_t *)"auth_code", 
			      auth_code, 
			      auth_code_length) != -1);
      break;
    case IPMI_SESSION_AUTH_TYPE_OEM_PROP:
      fprintf (stderr, "%s:%d:%s(): auth_type OEM is not supported\n", 
	       __FILE__, __LINE__, __PRETTY_FUNCTION__);
    default:
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_SET (dev->io.outofband.rq.obj_hdr_session, 
		*(dev->io.outofband.rq.tmpl_hdr_session_ptr), 
		(uint8_t *)"ipmi_msg_len", 
		(fiid_obj_len_bytes (*(dev->io.outofband.rq.tmpl_msg_hdr_ptr)) + 
		 fiid_obj_len_bytes (tmpl_cmd) + 
		 fiid_obj_len_bytes (*(dev->io.outofband.rq.tmpl_msg_trlr_ptr))));
  
  return (0);
}

int32_t 
_ipmi_lan_pkt_size (uint8_t auth_type, 
		    fiid_template_t tmpl_lan_msg, 
		    fiid_template_t tmpl_cmd)
{
  uint32_t msg_len;
  
  msg_len = fiid_obj_len_bytes (tmpl_hdr_rmcp) +
    fiid_obj_len_bytes (tmpl_lan_msg) +
    fiid_obj_len_bytes (tmpl_cmd) +
    fiid_obj_len_bytes (tmpl_lan_msg_trlr);

  if (auth_type == IPMI_SESSION_AUTH_TYPE_NONE) 
    msg_len += fiid_obj_len_bytes(tmpl_hdr_session);
  else if (auth_type == IPMI_SESSION_AUTH_TYPE_MD2
           || auth_type == IPMI_SESSION_AUTH_TYPE_MD5
           || auth_type == IPMI_SESSION_AUTH_TYPE_STRAIGHT_PASSWD_KEY
           || auth_type == IPMI_SESSION_AUTH_TYPE_OEM_PROP) 
    msg_len += fiid_obj_len_bytes(tmpl_hdr_session_auth);
  else 
    /* fatal error, library should not call this function with a bad auth_type */
    ERR_EXIT(0);
  
  return msg_len;
}

int32_t 
_ipmi_lan_pkt_rq_size (uint8_t auth_type, 
		       fiid_template_t tmpl_cmd)
{
  return _ipmi_lan_pkt_size(auth_type, tmpl_lan_msg_hdr_rq, tmpl_cmd);
}

int32_t 
_ipmi_lan_pkt_rq_size2 (ipmi_device_t *dev, 
			fiid_template_t tmpl_cmd)
{
  return (fiid_obj_len_bytes (*(dev->io.outofband.rq.tmpl_hdr_rmcp_ptr)) + 
	  fiid_obj_len_bytes (*(dev->io.outofband.rq.tmpl_hdr_session_ptr)) + 
	  fiid_obj_len_bytes (*(dev->io.outofband.rq.tmpl_msg_hdr_ptr)) + 
	  fiid_obj_len_bytes (tmpl_cmd) + 
	  fiid_obj_len_bytes (*(dev->io.outofband.rq.tmpl_msg_trlr_ptr)));
}

int32_t 
_ipmi_lan_pkt_rs_size (uint8_t auth_type, 
		       fiid_template_t tmpl_cmd)
{
  return _ipmi_lan_pkt_size(auth_type, tmpl_lan_msg_hdr_rs, tmpl_cmd);
}

int32_t 
_ipmi_lan_pkt_rs_size2 (ipmi_device_t *dev, 
			fiid_template_t tmpl_cmd)
{
  return (fiid_obj_len_bytes (*(dev->io.outofband.rs.tmpl_hdr_rmcp_ptr)) + 
	  fiid_obj_len_bytes (*(dev->io.outofband.rs.tmpl_hdr_session_ptr)) + 
	  fiid_obj_len_bytes (*(dev->io.outofband.rs.tmpl_msg_hdr_ptr)) + 
	  fiid_obj_len_bytes (tmpl_cmd) + 
	  fiid_obj_len_bytes (*(dev->io.outofband.rs.tmpl_msg_trlr_ptr)));
}

/*
  Complete IPMI LAN Request Packet
  +----------------------+
  |  RMCP                |
  |  Session             |
  |  Message             |
  |  Command             |
  |    Data              |
  |  Checksum            |
  +----------------------+
*/

int32_t 
assemble_ipmi_lan_pkt (fiid_obj_t obj_hdr_rmcp, 
		       fiid_obj_t obj_hdr_session, 
		       fiid_template_t tmpl_hdr_session, 
		       fiid_obj_t obj_msg_hdr, 
		       fiid_obj_t obj_cmd, 
		       fiid_template_t tmpl_cmd, 
		       uint8_t *pkt, 
		       uint32_t pkt_len)
{
  uint64_t auth_type;
  uint32_t indx, required_len;
  uint8_t *auth_code_field_ptr = NULL;
  uint8_t *msg_data_ptr = NULL;
  uint32_t msg_data_count = 0;
  uint32_t obj_len;
  ipmi_chksum_t chksum;

  if (!(obj_hdr_rmcp && obj_hdr_session && tmpl_hdr_session && 
        obj_msg_hdr && obj_cmd && tmpl_cmd && pkt))
    {
      errno = EINVAL;
      return -1;
    }
  
  if (!fiid_obj_field_lookup(tmpl_hdr_session, (uint8_t *)"auth_type")
      || !fiid_obj_field_lookup(tmpl_hdr_session, (uint8_t *)"session_seq_num")
      || !fiid_obj_field_lookup(tmpl_hdr_session, (uint8_t *)"session_id")
      || !fiid_obj_field_lookup(tmpl_hdr_session, (uint8_t *)"ipmi_msg_len"))
    {
      errno = EINVAL;
      return -1;
    }

  fiid_obj_get(obj_hdr_session, tmpl_hdr_session, (uint8_t *)"auth_type", &auth_type);
  if (!IPMI_1_5_SESSION_AUTH_TYPE_VALID(auth_type))
    {
      errno = EINVAL;
      return -1;
    }
  
  required_len = _ipmi_lan_pkt_rq_size((uint8_t)auth_type, tmpl_cmd);
  if (pkt_len < required_len) 
    {
      errno = EMSGSIZE;
      return -1;
    }

  memset (pkt, 0, pkt_len);

  indx = 0;
  obj_len = fiid_obj_len_bytes (tmpl_hdr_rmcp);
  memcpy (pkt, obj_hdr_rmcp, obj_len);
  indx += obj_len;

  obj_len = fiid_obj_field_len_bytes (tmpl_hdr_session, (uint8_t *)"auth_type");
  memcpy (pkt + indx, 
          obj_hdr_session + fiid_obj_field_start_bytes (tmpl_hdr_session, (uint8_t *)"auth_type"), 
          obj_len);
  indx += obj_len;
  
  obj_len = fiid_obj_field_len_bytes (tmpl_hdr_session, (uint8_t *)"session_seq_num");
  memcpy (pkt + indx, 
          obj_hdr_session + fiid_obj_field_start_bytes (tmpl_hdr_session, (uint8_t *)"session_seq_num"), 
          obj_len);
  indx += obj_len;

  obj_len = fiid_obj_field_len_bytes (tmpl_hdr_session, (uint8_t *)"session_id");
  memcpy (pkt + indx, 
          obj_hdr_session + fiid_obj_field_start_bytes (tmpl_hdr_session, (uint8_t *)"session_id"), 
          obj_len);
  indx += obj_len;

  /* auth_code generated last.  Save pointers for later calculate */
  if (auth_type == IPMI_SESSION_AUTH_TYPE_MD2
      || auth_type == IPMI_SESSION_AUTH_TYPE_MD5
      || auth_type == IPMI_SESSION_AUTH_TYPE_STRAIGHT_PASSWD_KEY
      || auth_type == IPMI_SESSION_AUTH_TYPE_OEM_PROP) 
    {
      auth_code_field_ptr = (pkt + indx); 
      indx += IPMI_SESSION_MAX_AUTH_CODE_LEN;
    }
    
  obj_len = fiid_obj_field_len_bytes (tmpl_hdr_session, (uint8_t *)"ipmi_msg_len");
  memcpy (pkt + indx, 
          obj_hdr_session + fiid_obj_field_start_bytes (tmpl_hdr_session, (uint8_t *)"ipmi_msg_len"), 
          obj_len);
  indx += obj_len;

  msg_data_ptr = (pkt + indx);
  obj_len = fiid_obj_len_bytes (tmpl_lan_msg_hdr_rq);
  memcpy (pkt + indx, obj_msg_hdr, obj_len);
  indx += obj_len;
  msg_data_count += obj_len;

  obj_len = fiid_obj_len_bytes (tmpl_cmd);
  memcpy (pkt + indx, obj_cmd, obj_len);
  indx += obj_len;
  msg_data_count += obj_len;

  chksum = ipmi_chksum (pkt + IPMI_LAN_PKT_RQ_CHKSUM2_BLOCK_INDX (auth_type), 
                        IPMI_LAN_PKT_RQ_CHKSUM2_BLOCK_LEN (tmpl_cmd));
  obj_len = fiid_obj_len_bytes (tmpl_lan_msg_trlr);
  memcpy (pkt + indx, &chksum, obj_len);
  indx += obj_len;
  msg_data_count += obj_len;

  /* Auth type must be done last, some authentication like md2 and md5
   * require all fields, including checksums, to be calculated
   * beforehand
   */
  if (auth_type == IPMI_SESSION_AUTH_TYPE_MD2
      || auth_type == IPMI_SESSION_AUTH_TYPE_MD5
      || auth_type == IPMI_SESSION_AUTH_TYPE_STRAIGHT_PASSWD_KEY
      || auth_type == IPMI_SESSION_AUTH_TYPE_OEM_PROP) 
    {
      if (fiid_obj_field_lookup (tmpl_hdr_session, (uint8_t *)"auth_code"))
        {
          ERR_EXIT(fiid_obj_field_len_bytes (tmpl_hdr_session, (uint8_t *)"auth_code") == IPMI_SESSION_MAX_AUTH_CODE_LEN);
          memcpy (auth_code_field_ptr,
                  obj_hdr_session + fiid_obj_field_start_bytes (tmpl_hdr_session, (uint8_t *)"auth_code"),
                  fiid_obj_field_len_bytes (tmpl_hdr_session, (uint8_t *)"auth_code"));
        }
      else if (fiid_obj_field_lookup (tmpl_hdr_session, (uint8_t *)"auth_calc_data"))
        {
          if (auth_type == IPMI_SESSION_AUTH_TYPE_STRAIGHT_PASSWD_KEY)
            {
              ERR_EXIT(fiid_obj_field_len_bytes (tmpl_hdr_session, (uint8_t *)"auth_calc_data") >= IPMI_SESSION_MAX_AUTH_CODE_LEN);

              /* achu: Do not copy based on field length of "auth_calc_data" */
              memcpy (auth_code_field_ptr,
                      obj_hdr_session + fiid_obj_field_start_bytes (tmpl_hdr_session, (uint8_t *)"auth_calc_data"),
                      IPMI_SESSION_MAX_AUTH_CODE_LEN);
            }
          else if (auth_type == IPMI_SESSION_AUTH_TYPE_MD2
                   || auth_type == IPMI_SESSION_AUTH_TYPE_MD5) 
            {
              uint8_t pwbuf[IPMI_SESSION_MAX_AUTH_CODE_LEN];

              ERR_EXIT(fiid_obj_field_len_bytes (tmpl_hdr_session, (uint8_t *)"auth_calc_data") >= IPMI_SESSION_MAX_AUTH_CODE_LEN);
              ERR_EXIT(IPMI_SESSION_MAX_AUTH_CODE_LEN == IPMI_MD2_DIGEST_LEN);

              /* Must zero extend password.  No null termination is required. 
	       * Also, must memcpy instead of strcpy, password need not be
	       * 1 word
	       */   
              memset(pwbuf, '\0', IPMI_SESSION_MAX_AUTH_CODE_LEN);
              memcpy(pwbuf, 
		     (obj_hdr_session + 
		      fiid_obj_field_start_bytes (tmpl_hdr_session, 
						  (uint8_t *)"auth_calc_data")), 
		     IPMI_SESSION_MAX_AUTH_CODE_LEN);

              if (auth_type == IPMI_SESSION_AUTH_TYPE_MD2)
                {
                  ipmi_md2_t ctx;
                  uint8_t digest[IPMI_MD2_DIGEST_LEN];
             
                  ipmi_md2_init(&ctx);
                  ipmi_md2_update_data(&ctx, pwbuf, IPMI_SESSION_MAX_AUTH_CODE_LEN);
                  ipmi_md2_update_data(&ctx, 
                                       (obj_hdr_session +
                                        fiid_obj_field_start_bytes (tmpl_hdr_session,
                                                                    (uint8_t *)"session_id")), 
                                       fiid_obj_field_len_bytes (tmpl_hdr_session,
                                                                 (uint8_t *)"session_id"));
                  ipmi_md2_update_data(&ctx, msg_data_ptr, msg_data_count);
                  ipmi_md2_update_data(&ctx, 
                                       (obj_hdr_session +
                                        fiid_obj_field_start_bytes (tmpl_hdr_session,
                                                                    (uint8_t *)"session_seq_num")),
                                       fiid_obj_field_len_bytes (tmpl_hdr_session,
                                                                 (uint8_t *)"session_seq_num"));
                  ipmi_md2_update_data(&ctx, pwbuf, IPMI_SESSION_MAX_AUTH_CODE_LEN);
                  ipmi_md2_finish(&ctx, digest, IPMI_MD2_DIGEST_LEN);
                  
                  memcpy (auth_code_field_ptr, digest, IPMI_SESSION_MAX_AUTH_CODE_LEN);
                }
              else if (auth_type == IPMI_SESSION_AUTH_TYPE_MD5)
                {
                  ipmi_md5_t ctx;
                  uint8_t digest[IPMI_MD5_DIGEST_LEN];
                              
                  ipmi_md5_init(&ctx);
                  ipmi_md5_update_data(&ctx, pwbuf, IPMI_SESSION_MAX_AUTH_CODE_LEN);
                  ipmi_md5_update_data(&ctx, 
                                       (obj_hdr_session +
                                        fiid_obj_field_start_bytes (tmpl_hdr_session,
                                                                    (uint8_t *)"session_id")), 
                                       fiid_obj_field_len_bytes (tmpl_hdr_session,
                                                                 (uint8_t *)"session_id"));
                  ipmi_md5_update_data(&ctx, msg_data_ptr, msg_data_count);
                  ipmi_md5_update_data(&ctx, 
                                       (obj_hdr_session +
                                        fiid_obj_field_start_bytes (tmpl_hdr_session,
                                                                    (uint8_t *)"session_seq_num")),
                                       fiid_obj_field_len_bytes (tmpl_hdr_session,
                                                                 (uint8_t *)"session_seq_num"));
                  ipmi_md5_update_data(&ctx, pwbuf, IPMI_SESSION_MAX_AUTH_CODE_LEN);
                  ipmi_md5_finish(&ctx, digest, IPMI_MD5_DIGEST_LEN);
                  
                  memcpy (auth_code_field_ptr, digest, IPMI_SESSION_MAX_AUTH_CODE_LEN);
                }
            }
          else
            {
              /* tmpl_hdr_session_auth_calc does not support this
               * authentication type 
               */
              errno = EINVAL;
              return -1;
            }
        }
    }

  return indx;
}

int32_t 
assemble_ipmi_lan_pkt2 (ipmi_device_t *dev, 
			fiid_obj_t obj_cmd, 
			fiid_template_t tmpl_cmd, 
			uint8_t *pkt, 
			uint32_t pkt_len)
{
  uint32_t index, required_len;
  uint32_t obj_len;

  if (!(dev && 
	obj_cmd && 
	tmpl_cmd && 
	pkt))
    {
      errno = EINVAL;
      return -1;
    }
  
  required_len = _ipmi_lan_pkt_rq_size2 (dev, tmpl_cmd);
  if (pkt_len < required_len)
    {
      errno = EMSGSIZE;
      return -1;
    }
  
  memset (pkt, 0, pkt_len);
  
  index = 0;

  obj_len = fiid_obj_len_bytes (*(dev->io.outofband.rq.tmpl_hdr_rmcp_ptr));
  memcpy (pkt, 
	  dev->io.outofband.rq.obj_hdr_rmcp, 
	  obj_len);
  index += obj_len;
  
  obj_len = fiid_obj_len_bytes (*(dev->io.outofband.rq.tmpl_hdr_session_ptr));
  memcpy ((pkt + index), 
          dev->io.outofband.rq.obj_hdr_session,
	  obj_len);
  index += obj_len;
  
  obj_len = fiid_obj_len_bytes (*(dev->io.outofband.rq.tmpl_msg_hdr_ptr));
  memcpy ((pkt + index), 
	  dev->io.outofband.rq.obj_msg_hdr,
	  obj_len);
  index += obj_len;
  
  obj_len = fiid_obj_len_bytes (tmpl_cmd);
  memcpy ((pkt + index), 
	  obj_cmd,
	  obj_len);
  index += obj_len;
  
  obj_len = fiid_obj_len_bytes (*(dev->io.outofband.rq.tmpl_msg_trlr_ptr));
  memcpy ((pkt + index), 
	  dev->io.outofband.rq.obj_msg_trlr,
	  obj_len);
  index += obj_len;
  
  return index;
}

/*
  Complete IPMI LAN Response Packet
  +----------------------+
  |  Session             |
  |  RMCP                |
  |  Message             |
  |  Command             |
  |    Completion Code   |
  |    Data              |
  |  Checksum            |
  +----------------------+
Optional Arguments : (pass NULL to ignore)
  hdr_rmcp, session, msg, cmd and chksum
*/

int8_t 
unassemble_ipmi_lan_pkt (uint8_t *pkt, 
			 uint32_t pkt_len, 
			 fiid_template_t tmpl_hdr_session, 
			 fiid_template_t tmpl_cmd, 
			 fiid_obj_t obj_hdr_rmcp, 
			 fiid_obj_t obj_hdr_session, 
			 fiid_obj_t obj_msg_hdr, 
			 fiid_obj_t obj_cmd, 
			 fiid_obj_t obj_msg_trlr)
{
  uint8_t auth_type;
  uint32_t auth_type_offset, indx;
  uint32_t obj_cmd_len, obj_len, obj_rmcp_hdr_len, obj_msg_trlr_len;

  if (!(pkt && tmpl_hdr_session && tmpl_cmd))
    {
      errno = EINVAL;
      return -1;
    }

  if (!fiid_obj_field_lookup(tmpl_hdr_session, (uint8_t *)"auth_type")
      || !fiid_obj_field_lookup(tmpl_hdr_session, (uint8_t *)"session_seq_num")
      || !fiid_obj_field_lookup(tmpl_hdr_session, (uint8_t *)"session_id")
      || !fiid_obj_field_lookup(tmpl_hdr_session, (uint8_t *)"ipmi_msg_len"))
    {
      errno = EINVAL;
      return -1;
    }

  indx = 0;
  obj_rmcp_hdr_len = fiid_obj_len_bytes (tmpl_hdr_rmcp);
  if (obj_hdr_rmcp)
    {
      memcpy (obj_hdr_rmcp, pkt + indx,
	      FREEIPMI_MIN(pkt_len - indx, obj_rmcp_hdr_len));
    }
  indx += obj_rmcp_hdr_len;

  if (pkt_len <= indx)
    return 0;

  if ((pkt_len - indx) < fiid_obj_field_end_bytes (tmpl_hdr_session, (uint8_t *)"auth_type"))
    {
      /* Special case, return after copying this */
      memcpy(obj_hdr_session, pkt + indx, (pkt_len - indx));
      return 0;
    }

  auth_type_offset = obj_rmcp_hdr_len + fiid_obj_field_start_bytes (tmpl_hdr_session, (uint8_t *)"auth_type");
  auth_type = pkt[auth_type_offset];

  if (obj_hdr_session)
    {
      obj_len = fiid_obj_field_len_bytes (tmpl_hdr_session, (uint8_t *)"auth_type");
      memcpy (obj_hdr_session + 
	      fiid_obj_field_start_bytes (tmpl_hdr_session, (uint8_t *)"auth_type"), 
	      pkt + indx, 
	      FREEIPMI_MIN ((pkt_len - indx), obj_len));
      indx += obj_len;
      
      if (pkt_len <= indx)
        return 0;

      obj_len = fiid_obj_field_len_bytes (tmpl_hdr_session, (uint8_t *)"session_seq_num");
      memcpy (obj_hdr_session + 
	      fiid_obj_field_start_bytes (tmpl_hdr_session, (uint8_t *)"session_seq_num"), 
	      pkt + indx, 
	      FREEIPMI_MIN ((pkt_len - indx), obj_len));
      indx += obj_len;

      if (pkt_len <= indx)
        return 0;

      obj_len = fiid_obj_field_len_bytes (tmpl_hdr_session, (uint8_t *)"session_id");
      memcpy (obj_hdr_session + 
	      fiid_obj_field_start_bytes (tmpl_hdr_session, (uint8_t *)"session_id"), 
	      pkt + indx,
	      FREEIPMI_MIN ((pkt_len - indx), obj_len));
      indx += obj_len;

      if (pkt_len <= indx)
        return 0;

      if (auth_type != IPMI_SESSION_AUTH_TYPE_NONE)
        {
          if (fiid_obj_field_lookup (tmpl_hdr_session, (uint8_t *)"auth_code")) 
            {
              obj_len = fiid_obj_field_len_bytes (tmpl_hdr_session, (uint8_t *)"auth_code");
	      ERR_EXIT(obj_len == IPMI_SESSION_MAX_AUTH_CODE_LEN);
              memcpy (obj_hdr_session + 
		      fiid_obj_field_start_bytes (tmpl_hdr_session, (uint8_t *)"auth_code"), 
		      pkt + indx, 
		      FREEIPMI_MIN ((pkt_len - indx), obj_len));
              indx += obj_len;
            }
	  else if (fiid_obj_field_lookup (tmpl_hdr_session, (uint8_t *)"auth_calc_data"))
	    {
              obj_len = fiid_obj_field_len_bytes (tmpl_hdr_session, (uint8_t *)"auth_calc_data");
	      ERR_EXIT(obj_len >= IPMI_SESSION_MAX_AUTH_CODE_LEN);

	      /* Must copy IPMI_SESSION_MAX_AUTH_CODE_LEN,
	      auth_calc_data may be > IPMI_SESSION_MAX_AUTH_CODE_LEN
	      */
              memcpy (obj_hdr_session +
		      fiid_obj_field_start_bytes (tmpl_hdr_session, (uint8_t *)"auth_calc_data"),
                      pkt + indx, 
		      FREEIPMI_MIN ((pkt_len - indx), IPMI_SESSION_MAX_AUTH_CODE_LEN));
              indx += IPMI_SESSION_MAX_AUTH_CODE_LEN;
	    }
          else
            {
              /* achu: user passed invalid template and we cannot
               * store the authentication code anywhere.
               */ 
              errno = EINVAL;
              return -1;
            }
        }
      
      obj_len = fiid_obj_field_len_bytes (tmpl_hdr_session, (uint8_t *)"ipmi_msg_len");
      memcpy (obj_hdr_session +
	      fiid_obj_field_start_bytes (tmpl_hdr_session, (uint8_t *)"ipmi_msg_len"),
	      pkt + indx,
	      obj_len);
      indx += obj_len;
    }
  else
    {
      indx += fiid_obj_field_len_bytes (tmpl_hdr_session, (uint8_t *)"auth_type");
      indx += fiid_obj_field_len_bytes (tmpl_hdr_session, (uint8_t *)"session_seq_num");
      indx += fiid_obj_field_len_bytes (tmpl_hdr_session, (uint8_t *)"session_id");
      if (auth_type != IPMI_SESSION_AUTH_TYPE_NONE)
        indx += IPMI_SESSION_MAX_AUTH_CODE_LEN;
      indx += fiid_obj_field_len_bytes (tmpl_hdr_session, (uint8_t *)"ipmi_msg_len");
    }

  if (pkt_len <= indx)
    return 0;

  obj_len = fiid_obj_len_bytes (tmpl_lan_msg_hdr_rs);
  if (obj_msg_hdr)
    memcpy (obj_msg_hdr,
	    pkt + indx,
	    FREEIPMI_MIN((pkt_len - indx), obj_len));
  indx += obj_len;

  if (pkt_len <= indx)
    return 0;

  obj_cmd_len = fiid_obj_len_bytes (tmpl_cmd);
  obj_msg_trlr_len = fiid_obj_len_bytes (tmpl_lan_msg_trlr);

  if ((pkt_len - indx) <= obj_cmd_len)
    {
      if ((pkt_len - indx) > obj_msg_trlr_len)
        obj_cmd_len = (pkt_len - indx) - obj_msg_trlr_len;
      else
        obj_cmd_len = (pkt_len - indx);
    }

  if (obj_cmd)
    memcpy (obj_cmd,
            pkt + indx,
            FREEIPMI_MIN((pkt_len - indx), obj_cmd_len));
  indx += obj_cmd_len;

  if (pkt_len <= indx)
    return 0;

  obj_len = fiid_obj_len_bytes (tmpl_lan_msg_trlr);
  if (obj_msg_trlr)
    memcpy (obj_msg_trlr,
	    pkt + indx,
	    FREEIPMI_MIN((pkt_len - indx), obj_msg_trlr_len));
  indx += obj_msg_trlr_len;
  
  return 0;
}

int8_t 
unassemble_ipmi_lan_pkt2 (ipmi_device_t *dev, 
			  uint8_t *pkt, 
			  uint32_t pkt_len, 
			  fiid_obj_t obj_cmd, 
			  fiid_template_t tmpl_cmd)
{
  fiid_field_t *tmpl_lan_packet = NULL;
  int lan_packet_length = 0;
  
  int rmcp_length = 0;
  int session_length = 0;
  int hdr_length = 0;
  int cmd_length = 0;
  int trlr_length = 0;
  
  ERR ((rmcp_length = 
	fiid_obj_len_bytes (*(dev->io.outofband.rs.tmpl_hdr_rmcp_ptr))) != -1);
  ERR ((session_length = 
	fiid_obj_len_bytes (*(dev->io.outofband.rs.tmpl_hdr_session_ptr))) != -1);
  ERR ((hdr_length = 
	fiid_obj_len_bytes (*(dev->io.outofband.rs.tmpl_msg_hdr_ptr))) != -1);
  ERR ((cmd_length = 
	fiid_obj_len_bytes (tmpl_cmd)) != -1);
  ERR ((trlr_length = 
	fiid_obj_len_bytes (*(dev->io.outofband.rs.tmpl_msg_trlr_ptr))) != -1);
  
  tmpl_lan_packet = fiid_template_make ((rmcp_length * 8),    "RMCP_HDR", 
					(session_length * 8), "SESSION_HDR", 
					(hdr_length * 8),     "MSG_HDR", 
					(cmd_length * 8),     "COMMAND_DATA", 
					(trlr_length * 8),    "TRLR_HDR");
  lan_packet_length = fiid_obj_len_bytes (tmpl_lan_packet);
  
  if (pkt_len != lan_packet_length)
    {
      free (tmpl_lan_packet);
      return (-1);
    }
  
  fiid_obj_get_data (pkt, 
		     tmpl_lan_packet, 
		     (uint8_t *)"RMCP_HDR", 
		     dev->io.outofband.rs.obj_hdr_rmcp,
                     rmcp_length);
  fiid_obj_get_data (pkt, 
		     tmpl_lan_packet, 
		     (uint8_t *)"SESSION_HDR", 
		     dev->io.outofband.rs.obj_hdr_session,
                     session_length);
  fiid_obj_get_data (pkt, 
		     tmpl_lan_packet, 
		     (uint8_t *)"MSG_HDR", 
		     dev->io.outofband.rs.obj_msg_hdr,
                     hdr_length);
  fiid_obj_get_data (pkt, 
		     tmpl_lan_packet, 
		     (uint8_t *)"COMMAND_DATA", 
		     obj_cmd,
                     cmd_length);
  fiid_obj_get_data (pkt, 
		     tmpl_lan_packet, 
		     (uint8_t *)"TRLR_HDR", 
		     dev->io.outofband.rs.obj_msg_trlr,
                     trlr_length);
  
  free (tmpl_lan_packet);
  
  return 0;
}

int 
ipmi_lan_validate_checksum (ipmi_device_t *dev, 
			    fiid_obj_t obj_cmd, 
			    fiid_template_t tmpl_cmd)
{
  uint8_t checksum = 0;
  uint64_t val = 0;
  
  checksum = 0;
  ERR (get_rs_checksum1 (dev, &checksum) == 0);
  FIID_OBJ_GET (dev->io.outofband.rs.obj_msg_hdr, 
		*(dev->io.outofband.rs.tmpl_msg_hdr_ptr), 
		(uint8_t *)"chksum1", 
		&val);
  ERR (val == checksum);
  
  checksum = 0;
  ERR (get_rs_checksum2 (dev, obj_cmd, tmpl_cmd, &checksum) == 0);
  FIID_OBJ_GET (dev->io.outofband.rs.obj_msg_trlr, 
		*(dev->io.outofband.rs.tmpl_msg_trlr_ptr), 
		(uint8_t *)"chksum2", 
		&val);
  ERR (val == checksum);
  
  return (0);
}

ssize_t 
ipmi_lan_sendto (int sockfd, 
		 const void *pkt, 
		 size_t pkt_len, 
		 int flags, 
		 const struct sockaddr *to, 
		 unsigned int tolen)
{
  void *_pkt;
  ssize_t bytes_sent;
  size_t _pkt_len;
  size_t pad_len = 0;

  if (pkt == NULL || pkt_len < 0)
    {
      errno = EINVAL;
      return -1;
    }

  /*
    Note from Table 12-8, RMCP Packet for IPMI via Ethernet footnote
    Some LAN adapter chips may have a problem where packets of overall
    lengths 56, 84, 112, 128, or 156 are not handled correctly. The
    PAD byte is added as necessary to avoid these overall
    lengths. Remote console software must use the PAD byte when
    formatting packets to any 10/100 Ethernet device that accepts RMCP
    packets. -- Anand Babu
  */
  _pkt_len = pkt_len;
  if (_pkt_len == 56  ||
      _pkt_len == 84  ||
      _pkt_len == 112 ||
      _pkt_len == 128 ||
      _pkt_len == 156)
    {
      pad_len += IPMI_LAN_PKT_PAD_SIZE;
    }

  _pkt_len += pad_len;
  _pkt = alloca (_pkt_len);         
  memset (_pkt, 0, _pkt_len);
  memcpy (_pkt, pkt, pkt_len);
  
  bytes_sent = sendto (sockfd, _pkt, _pkt_len, flags, to, tolen);

  if (bytes_sent == -1)
    return -1;
  else
    return (bytes_sent - pad_len);
}

ssize_t 
ipmi_lan_recvfrom (int sockfd, 
		   void *pkt, 
		   size_t pkt_len, 
		   int flags, 
		   struct sockaddr *from, 
		   unsigned int *fromlen)
{
  ssize_t bytes_recvd = 0;
  void *recv_buf;
  size_t recv_buf_len;
  size_t pad_len = 0;

  if (pkt == NULL || pkt_len < 0)
    {
      errno = EINVAL;
      return -1;
    }

  if (pkt_len < 1024)
    recv_buf_len = 1024;
  else
    recv_buf_len = pkt_len;
  
  /* See comment in ipmi_lan_sendto */
  /* WILL LET THIS CHECK GO SOON --ab@gnu.org.in */
  if (recv_buf_len == 56  ||
      recv_buf_len == 84  ||
      recv_buf_len == 112 ||
      recv_buf_len == 128 ||
      recv_buf_len == 156)
    {
      pad_len = IPMI_LAN_PKT_PAD_SIZE;
    }

  recv_buf_len += pad_len;
  recv_buf = alloca (recv_buf_len);
  
  bytes_recvd = recvfrom (sockfd, recv_buf, recv_buf_len, flags, from, fromlen);
  if (bytes_recvd == -1)
    {
     /*  if (recv_buf) free (recv_buf); */
      return -1;
    }
  
  recv_buf_len = pad_len ? (bytes_recvd - pad_len) : bytes_recvd;
  memcpy (pkt, recv_buf, recv_buf_len);
  /* if (recv_buf) free (recv_buf); */
  return (recv_buf_len);
}

int8_t 
ipmi_lan_cmd (uint32_t sockfd, 
	      struct sockaddr *hostaddr, 
	      size_t hostaddr_len, 
	      uint8_t auth_type, 
	      uint32_t session_seq_num, 
	      uint32_t session_id, 
	      uint8_t *auth_code_data, 
	      uint32_t auth_code_data_len, 
	      uint8_t net_fn, 
	      uint8_t lun, 
	      uint8_t rq_seq, 
	      fiid_obj_t obj_cmd_rq, 
	      fiid_template_t tmpl_cmd_rq, 
	      fiid_obj_t obj_cmd_rs, 
	      fiid_template_t tmpl_cmd_rs)
{
  fiid_template_t *tmpl_hdr_session_ptr;

  if (!(hostaddr && sockfd && hostaddr_len && tmpl_cmd_rq && obj_cmd_rq 
	&& tmpl_cmd_rs && obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }

  if (auth_type == IPMI_SESSION_AUTH_TYPE_NONE)
    tmpl_hdr_session_ptr = &tmpl_hdr_session;
  else if (auth_type == IPMI_SESSION_AUTH_TYPE_OEM_PROP)
    tmpl_hdr_session_ptr = &tmpl_hdr_session_auth;
  else if (auth_type == IPMI_SESSION_AUTH_TYPE_MD2
	   || auth_type == IPMI_SESSION_AUTH_TYPE_MD5
	   || auth_type == IPMI_SESSION_AUTH_TYPE_STRAIGHT_PASSWD_KEY)
    tmpl_hdr_session_ptr = &tmpl_hdr_session_auth_calc;
  else
    {
      errno = EINVAL;
      return (-1);
    }

  {
    fiid_obj_t obj_hdr_rmcp;
    fiid_obj_t obj_hdr_session;
    fiid_obj_t obj_msg_hdr;
    uint8_t *pkt;
    uint32_t pkt_len;
    int status = 0;

    FIID_OBJ_ALLOCA (obj_hdr_rmcp, tmpl_hdr_rmcp);
    FIID_OBJ_ALLOCA (obj_hdr_session, *tmpl_hdr_session_ptr);
    FIID_OBJ_ALLOCA (obj_msg_hdr, tmpl_lan_msg_hdr_rq);

    pkt_len = _ipmi_lan_pkt_rq_size(auth_type, tmpl_cmd_rq); 
    pkt = alloca (pkt_len);
    ERR (pkt);
    memset (pkt, 0, pkt_len);
   
    ERR (fill_hdr_rmcp_ipmi (obj_hdr_rmcp) != -1);
    ERR (fill_hdr_session (*tmpl_hdr_session_ptr, auth_type, session_seq_num, 
			   session_id, auth_code_data, auth_code_data_len,
                           tmpl_cmd_rq, obj_hdr_session) != -1);
    ERR (fill_lan_msg_hdr (net_fn, lun, rq_seq, obj_msg_hdr) != -1);
    ERR (assemble_ipmi_lan_pkt (obj_hdr_rmcp, obj_hdr_session, *tmpl_hdr_session_ptr,
				obj_msg_hdr, obj_cmd_rq, tmpl_cmd_rq,
				pkt, pkt_len) != -1);
    /* __DEBUG >> */
/*     fiid_obj_dump (2, obj_hdr_rmcp, tmpl_hdr_rmcp); */
/*     fiid_obj_dump (2, obj_hdr_session, *tmpl_hdr_session_ptr); */
/*     fiid_obj_dump (2, obj_msg_hdr, tmpl_lan_msg_hdr_rq); */
/*     fiid_obj_dump (2, obj_cmd_rq, tmpl_cmd_rq); */
    /* __DEBUG >> */

    status = ipmi_lan_sendto (sockfd, pkt, pkt_len, 0, hostaddr, hostaddr_len);
    ERR (status != -1);
  }

  {
    struct sockaddr_in from;
    socklen_t fromlen;
    fiid_obj_t obj_hdr_session;
    uint8_t *pkt;
    uint32_t _pkt_len = 1024;
    int32_t pkt_len;

    FIID_OBJ_ALLOCA (obj_hdr_session, *tmpl_hdr_session_ptr);

    pkt_len = _ipmi_lan_pkt_rs_size (auth_type, tmpl_cmd_rs);
    pkt     = alloca (_pkt_len);
    memset (pkt, 0, _pkt_len);
    ERR (pkt);
    
    fromlen = sizeof(struct sockaddr_in);
    pkt_len = ipmi_lan_recvfrom (sockfd, pkt, _pkt_len, 0, (struct sockaddr *)&from, 
				&fromlen);
    ERR (pkt_len != -1);
    ERR (ipmi_lan_check_chksum (pkt, pkt_len) == 1);
    ERR (unassemble_ipmi_lan_pkt (pkt, pkt_len, *tmpl_hdr_session_ptr, tmpl_cmd_rs,
				  0, obj_hdr_session, 0, obj_cmd_rs, 0) != -1);

    /* __DEBUG__ >> */
/*     fiid_obj_dump (2, obj_cmd_rs, tmpl_cmd_rs); */
    /* __DEBUG__ << */

    /* Caller is reponsible for checking return code */
    /* ERR (ipmi_comp_test (obj_cmd_rs)); */

  }
  return (0);
}

int8_t 
ipmi_lan_cmd2 (ipmi_device_t *dev, 
	       fiid_obj_t obj_cmd_rq, 
	       fiid_template_t tmpl_cmd_rq, 
	       fiid_obj_t obj_cmd_rs, 
	       fiid_template_t tmpl_cmd_rs)
{
  if (!(dev && 
	dev->io.outofband.local_sockfd && 
	tmpl_cmd_rq && 
	obj_cmd_rq && 
	tmpl_cmd_rs && 
	obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }
  
  memset (dev->io.outofband.rq.obj_hdr_rmcp, 
	  0, 
	  fiid_obj_len_bytes (*(dev->io.outofband.rq.tmpl_hdr_rmcp_ptr)));
  memset (dev->io.outofband.rq.obj_hdr_session, 
	  0, 
	  fiid_obj_len_bytes (*(dev->io.outofband.rq.tmpl_hdr_session_ptr)));
  memset (dev->io.outofband.rq.obj_msg_hdr, 
	  0, 
	  fiid_obj_len_bytes (*(dev->io.outofband.rq.tmpl_msg_hdr_ptr)));
  memset (dev->io.outofband.rq.obj_msg_trlr, 
	  0, 
	  fiid_obj_len_bytes (*(dev->io.outofband.rq.tmpl_msg_trlr_ptr)));
  
  memset (dev->io.outofband.rs.obj_hdr_rmcp, 
	  0, 
	  fiid_obj_len_bytes (*(dev->io.outofband.rs.tmpl_hdr_rmcp_ptr)));
  memset (dev->io.outofband.rs.obj_hdr_session, 
	  0, 
	  fiid_obj_len_bytes (*(dev->io.outofband.rs.tmpl_hdr_session_ptr)));
  memset (dev->io.outofband.rs.obj_msg_hdr, 
	  0, 
	  fiid_obj_len_bytes (*(dev->io.outofband.rs.tmpl_msg_hdr_ptr)));
  memset (dev->io.outofband.rs.obj_msg_trlr, 
	  0, 
	  fiid_obj_len_bytes (*(dev->io.outofband.rs.tmpl_msg_trlr_ptr)));
  
  {
    uint8_t *pkt;
    uint32_t pkt_len;
    int status = 0;
    
    pkt_len = _ipmi_lan_pkt_rq_size2 (dev, tmpl_cmd_rq);
    pkt = alloca (pkt_len);
    ERR (pkt);
    memset (pkt, 0, pkt_len);
    
    ERR (fill_hdr_rmcp_ipmi (dev->io.outofband.rq.obj_hdr_rmcp) != -1);
    ERR (fill_lan_msg_hdr2 (dev) != -1);
    ERR (fill_lan_msg_trlr2 (dev, 
			     obj_cmd_rq, 
			     tmpl_cmd_rq) != -1);
    ERR (fill_hdr_session2 (dev, 
			    obj_cmd_rq, 
			    tmpl_cmd_rq) != -1);
    ERR (assemble_ipmi_lan_pkt2 (dev, 
				 obj_cmd_rq, 
				 tmpl_cmd_rq, 
				 pkt, 
				 pkt_len) != -1);

#if 0
printf("DEBUGGING:\n");

	fiid_obj_dump_lan(STDERR_FILENO,
			NULL,
			NULL,
			pkt,
			pkt_len,
			*(dev->io.outofband.rs.tmpl_hdr_session_ptr),
			*(dev->io.outofband.rs.tmpl_msg_hdr_ptr),
			tmpl_cmd_rq);
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
    int32_t pkt_len;
    
    pkt_len = _ipmi_lan_pkt_rs_size2 (dev, tmpl_cmd_rs);
    ERR (pkt_len <= pkt_max_size);
    
    pkt = alloca (pkt_max_size);
    ERR (pkt);
    memset (pkt, 0, pkt_max_size);
    bytes_received = ipmi_lan_recvfrom (dev->io.outofband.local_sockfd, 
					pkt, 
					pkt_max_size, 
					0, 
					(struct sockaddr *) &from, 
					&fromlen);
    
    if (bytes_received > pkt_len)
      {
	int i;
	
	fprintf (stderr, "%s(): received invalid packet.\n", __PRETTY_FUNCTION__);
	fprintf (stderr, 
		 "received packet size: %d\n" 
		 "expected packet size: %d\n", 
		 bytes_received, 
		 pkt_len);
	fprintf (stderr, "packet data:\n");
	for (i = 0; i < bytes_received; i++)
	  fprintf (stderr, "%02X ", pkt[i]);
	fprintf (stderr, "\n");
	
	return (-1);
      }
    
    if (bytes_received < pkt_len)
      {
	int min_len = 0;
	
	min_len = (fiid_obj_len_bytes (*(dev->io.outofband.rs.tmpl_hdr_rmcp_ptr)) + 
		   fiid_obj_len_bytes (*(dev->io.outofband.rs.tmpl_hdr_session_ptr)) + 
		   fiid_obj_len_bytes (*(dev->io.outofband.rs.tmpl_msg_hdr_ptr)) + 
		   2 + /* This means, CMD + COMP_CODE */
		   fiid_obj_len_bytes (*(dev->io.outofband.rs.tmpl_msg_trlr_ptr)));
	
	if (bytes_received < min_len)
	  {
	    int i;
	    
	    fprintf (stderr, "%s(): received invalid packet.\n", __PRETTY_FUNCTION__);
	    fprintf (stderr, 
		     "received packet size: %d\n" 
		     "expected packet size: %d\n" 
		     "minimum packet size: %d\n", 
		     bytes_received, 
		     pkt_len, 
		     min_len);
	    fprintf (stderr, "packet data:\n");
	    for (i = 0; i < bytes_received; i++)
	      fprintf (stderr, "%02X ", pkt[i]);
	    fprintf (stderr, "\n");
	    
	    return (-1);
	  }
	
	{
	  int i;
	  int trlr_len = 0;
	  int fill_len = 0;
	  int trlr_start = 0;
	  uint8_t *tmp_buf = NULL;
	  
	  trlr_len = fiid_obj_len_bytes (*(dev->io.outofband.rs.tmpl_msg_trlr_ptr));
	  trlr_start = bytes_received - trlr_len;
	  tmp_buf = alloca (trlr_len);
	  memcpy (tmp_buf, &pkt[trlr_start], trlr_len);
	  
	  fill_len = (pkt_len - bytes_received);
	  for (i = 0; i < fill_len; i++)
	    pkt[trlr_start + i] = 0;
	  
	  memcpy (&pkt[trlr_start + fill_len], 
		  tmp_buf, 
		  trlr_len);
	  
	  bytes_received += fill_len;
	}
      }
    
    ERR (unassemble_ipmi_lan_pkt2 (dev, 
				   pkt, 
				   pkt_len, 
				   obj_cmd_rs, 
				   tmpl_cmd_rs) != -1);
    
    ERR (ipmi_lan_validate_checksum (dev, 
				     obj_cmd_rs, 
				     tmpl_cmd_rs) == 0);
  }
  
  return (0);
}

static int8_t 
ipmi_lan_cmd_raw_send (ipmi_device_t *dev, 
		       fiid_obj_t obj_cmd_rq, 
		       fiid_template_t tmpl_cmd_rq)
{
  memset (dev->io.outofband.rq.obj_hdr_rmcp, 
	  0, 
	  fiid_obj_len_bytes (*(dev->io.outofband.rq.tmpl_hdr_rmcp_ptr)));
  memset (dev->io.outofband.rq.obj_hdr_session, 
	  0, 
	  fiid_obj_len_bytes (*(dev->io.outofband.rq.tmpl_hdr_session_ptr)));
  memset (dev->io.outofband.rq.obj_msg_hdr, 
	  0, 
	  fiid_obj_len_bytes (*(dev->io.outofband.rq.tmpl_msg_hdr_ptr)));
  memset (dev->io.outofband.rq.obj_msg_trlr, 
	  0, 
	  fiid_obj_len_bytes (*(dev->io.outofband.rq.tmpl_msg_trlr_ptr)));
  
  memset (dev->io.outofband.rs.obj_hdr_rmcp, 
	  0, 
	  fiid_obj_len_bytes (*(dev->io.outofband.rs.tmpl_hdr_rmcp_ptr)));
  memset (dev->io.outofband.rs.obj_hdr_session, 
	  0, 
	  fiid_obj_len_bytes (*(dev->io.outofband.rs.tmpl_hdr_session_ptr)));
  memset (dev->io.outofband.rs.obj_msg_hdr, 
	  0, 
	  fiid_obj_len_bytes (*(dev->io.outofband.rs.tmpl_msg_hdr_ptr)));
  memset (dev->io.outofband.rs.obj_msg_trlr, 
	  0, 
	  fiid_obj_len_bytes (*(dev->io.outofband.rs.tmpl_msg_trlr_ptr)));
  
  {
    uint8_t *pkt;
    uint32_t pkt_len;
    int status = 0;
    
    pkt_len = _ipmi_lan_pkt_rq_size2 (dev, tmpl_cmd_rq);
    pkt = alloca (pkt_len);
    ERR (pkt);
    memset (pkt, 0, pkt_len);
    
    ERR (fill_hdr_rmcp_ipmi (dev->io.outofband.rq.obj_hdr_rmcp) != -1);
    ERR (fill_lan_msg_hdr2 (dev) != -1);
    ERR (fill_lan_msg_trlr2 (dev, 
			     obj_cmd_rq, 
			     tmpl_cmd_rq) != -1);
    ERR (fill_hdr_session2 (dev, 
			    obj_cmd_rq, 
			    tmpl_cmd_rq) != -1);
    ERR (assemble_ipmi_lan_pkt2 (dev, 
				 obj_cmd_rq, 
				 tmpl_cmd_rq, 
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
  if (!(dev && 
	dev->io.outofband.local_sockfd && 
	buf_rq && 
	buf_rq_len && 
	buf_rs && 
	buf_rs_len && 
	*buf_rs_len))
    {
      errno = EINVAL;
      return (-1);
    }
  
  *buf_rs_len = 0;
  
  {
    uint64_t val = 0;
    
    fiid_template_t tmpl_hdr_cmd = 
      {
	{2, "lun"},
	{6, "net_fn"},
	{0, ""}
      };
    
    FIID_OBJ_GET ((fiid_obj_t) buf_rq, tmpl_hdr_cmd, (uint8_t *)"lun", &val);
    dev->lun = val;
    FIID_OBJ_GET ((fiid_obj_t) buf_rq, tmpl_hdr_cmd, (uint8_t *)"net_fn", &val);
    dev->net_fn = val;
  }
  
  {
    fiid_obj_t obj_cmd_rq = NULL;
    size_t obj_cmd_rq_len = 0;
    fiid_field_t *tmpl_var_cmd_rq = NULL;
    int8_t retval = 0;
    
    obj_cmd_rq = (fiid_obj_t) (buf_rq + 1);
    obj_cmd_rq_len = buf_rq_len - 1;
    
    tmpl_var_cmd_rq = fiid_template_make ((obj_cmd_rq_len * 8), (uint8_t *)"COMMAND_RQ_DATA");
    retval = ipmi_lan_cmd_raw_send (dev, 
				    obj_cmd_rq, 
				    tmpl_var_cmd_rq);
    free (tmpl_var_cmd_rq);
    
    ERR (retval == 0);
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
    
    pkt_hdrs_size = 
      (fiid_obj_len_bytes (*(dev->io.outofband.rs.tmpl_hdr_rmcp_ptr)) + 
       fiid_obj_len_bytes (*(dev->io.outofband.rs.tmpl_hdr_session_ptr)) + 
       fiid_obj_len_bytes (*(dev->io.outofband.rs.tmpl_msg_hdr_ptr)) + 
       fiid_obj_len_bytes (*(dev->io.outofband.rs.tmpl_msg_trlr_ptr)));
    
    tmpl_var_cmd_rs = fiid_template_make (((1024 - pkt_hdrs_size - 1) * 8), 
					  "COMMAND_RS_DATA");
    pkt_len = _ipmi_lan_pkt_rs_size2 (dev, tmpl_var_cmd_rs);
    free (tmpl_var_cmd_rs);
    ERR (pkt_len <= pkt_max_size);
    
    pkt = alloca (pkt_max_size);
    ERR (pkt);
    memset (pkt, 0, pkt_max_size);
    bytes_received = ipmi_lan_recvfrom (dev->io.outofband.local_sockfd, 
					pkt, 
					pkt_max_size, 
					0, 
					(struct sockaddr *) &from, 
					&fromlen);
    ERR (bytes_received >= pkt_hdrs_size);
    
    obj_cmd_rs_len = (bytes_received - pkt_hdrs_size);
    tmpl_var_cmd_rs = fiid_template_make ((obj_cmd_rs_len * 8), "COMMAND_RS_DATA");
    obj_cmd_rs = (fiid_obj_t) (buf_rs + 1);
    
    if (unassemble_ipmi_lan_pkt2 (dev, 
				  pkt, 
				  bytes_received, 
				  obj_cmd_rs, 
				  tmpl_var_cmd_rs) == -1)
      {
	free (tmpl_var_cmd_rs);
	return (-1);
      }
    
    if (ipmi_lan_validate_checksum (dev, 
				    obj_cmd_rs, 
				    tmpl_var_cmd_rs) != 0)
      {
	free (tmpl_var_cmd_rs);
	return (-1);
      }
    
    free (tmpl_var_cmd_rs);
    
    {
      uint64_t val = 0;
      
      fiid_template_t tmpl_hdr_cmd = 
	{
	  {2, "lun"},
	  {6, "net_fn"},
	  {0, ""}
	};
      
      FIID_OBJ_GET (dev->io.outofband.rs.obj_msg_hdr, 
		    *(dev->io.outofband.rs.tmpl_msg_hdr_ptr), 
		    (uint8_t *)"rq_lun", &val);
      FIID_OBJ_SET ((fiid_obj_t) buf_rs, tmpl_hdr_cmd, (uint8_t *)"lun", val);
      
      FIID_OBJ_GET (dev->io.outofband.rs.obj_msg_hdr, 
		    *(dev->io.outofband.rs.tmpl_msg_hdr_ptr), 
		    (uint8_t *)"net_fn", &val);
      FIID_OBJ_SET ((fiid_obj_t) buf_rs, tmpl_hdr_cmd, (uint8_t *)"net_fn", val);
      
      *buf_rs_len = obj_cmd_rs_len + 1;
    }
  }
  
  return (0);
}

int8_t 
ipmi_lan_check_net_fn (fiid_template_t tmpl_msg_hdr, 
		       fiid_obj_t obj_msg_hdr, 
		       uint8_t net_fn)
{
  uint64_t net_fn_recv;

  if (!(obj_msg_hdr && tmpl_msg_hdr))
    {
      errno = EINVAL;
      return (-1);
    }

  if (!fiid_obj_field_lookup (tmpl_msg_hdr, (uint8_t *)"net_fn"))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_GET(obj_msg_hdr, tmpl_msg_hdr, (uint8_t *)"net_fn", &net_fn_recv);

  return ((((int8_t)net_fn_recv) == net_fn) ? 1 : 0);
}

int8_t 
ipmi_lan_check_rq_seq (fiid_template_t tmpl_msg_hdr, 
		       fiid_obj_t obj_msg_hdr, 
		       uint8_t rq_seq)
{
  uint64_t rq_seq_recv;

  if (!(obj_msg_hdr && tmpl_msg_hdr))
    {
      errno = EINVAL;
      return (-1);
    }

  if (!fiid_obj_field_lookup (tmpl_msg_hdr, (uint8_t *)"rq_seq"))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_GET(obj_msg_hdr, tmpl_msg_hdr, (uint8_t *)"rq_seq", &rq_seq_recv);

  return ((((int8_t)rq_seq_recv) == rq_seq) ? 1 : 0);
}

int8_t 
ipmi_lan_check_chksum (uint8_t *pkt, uint64_t pkt_len)
{
  uint8_t auth_type;
  uint32_t auth_type_offset, required_len;

  if (pkt == NULL)
    {
      errno = EINVAL;
      return (-1);
    }

  if (pkt_len < (fiid_obj_len_bytes (tmpl_hdr_rmcp) + fiid_obj_field_end_bytes (tmpl_hdr_session, (uint8_t *)"auth_type")))
    return (0);

  auth_type_offset = fiid_obj_len_bytes (tmpl_hdr_rmcp) + fiid_obj_field_start_bytes (tmpl_hdr_session, (uint8_t *)"auth_type");
  auth_type = pkt[auth_type_offset];

  if (ipmi_chksum_test (pkt + IPMI_LAN_PKT_CHKSUM1_BLOCK_INDX (auth_type), 
			IPMI_LAN_PKT_RS_CHKSUM1_BLOCK_LEN + 1))
    {
      required_len = IPMI_LAN_PKT_CHKSUM1_BLOCK_INDX (auth_type) + IPMI_LAN_PKT_RS_CHKSUM1_BLOCK_LEN + 1;
      if (pkt_len <= required_len)
	return (0);

      if (ipmi_chksum_test (pkt + IPMI_LAN_PKT_RS_CHKSUM2_BLOCK_INDX (auth_type),
			    pkt_len - required_len))
	{
	  return (1);
	}
    }

  return (0);
}
