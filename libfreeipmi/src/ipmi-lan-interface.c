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
  if (!dev || !checksum)
    {
      errno = EINVAL;
      return (-1);
    }

  *checksum = ipmi_chksum (dev->io.outofband.rq.obj_msg_hdr, 2);
  return (0);
}

int 
get_rs_checksum1 (ipmi_device_t *dev, uint8_t *checksum)
{
  if (!dev || !checksum)
    {
      errno = EINVAL;
      return (-1);
    }

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
  
  if (!dev 
      || !obj_cmd
      || !tmpl_cmd
      || !checksum)
    {
      errno = EINVAL;
      return (-1);
    }

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
      fiid_template_free (tmpl_var_checksum2_data);
      return (-1);
    }
  if (fiid_obj_set (var_checksum2_data, 
		    tmpl_var_checksum2_data, 
		    (uint8_t *)"rq_addr", 
		    val) == -1)
    {
      fiid_template_free (tmpl_var_checksum2_data);
      return (-1);
    }
  
  if (fiid_obj_get (dev->io.outofband.rq.obj_msg_hdr,
		    *(dev->io.outofband.rq.tmpl_msg_hdr_ptr), 
		    (uint8_t *)"rq_lun", 
		    &val) == -1)
    {
      fiid_template_free (tmpl_var_checksum2_data);
      return (-1);
    }
  if (fiid_obj_set (var_checksum2_data, 
		    tmpl_var_checksum2_data, 
		    (uint8_t *)"rq_lun", 
		    val) == -1)
    {
      fiid_template_free (tmpl_var_checksum2_data);
      return (-1);
    }
  
  if (fiid_obj_get (dev->io.outofband.rq.obj_msg_hdr,
		    *(dev->io.outofband.rq.tmpl_msg_hdr_ptr), 
		    (uint8_t *)"rq_seq", 
		    &val) == -1)
    {
      fiid_template_free (tmpl_var_checksum2_data);
      return (-1);
    }
  if (fiid_obj_set (var_checksum2_data, 
		    tmpl_var_checksum2_data, 
		    (uint8_t *)"rq_seq", 
		    val) == -1)
    {
      fiid_template_free (tmpl_var_checksum2_data);
      return (-1);
    }
  
  if (fiid_obj_set_data (var_checksum2_data, 
			 tmpl_var_checksum2_data, 
			 (uint8_t *)"COMMAND_DATA", 
			 obj_cmd, 
			 cmd_length) == -1)
    {
      fiid_template_free (tmpl_var_checksum2_data);
      return (-1);
    }
  
  *checksum = ipmi_chksum (var_checksum2_data, 
			   var_checksum2_data_length);
  
  fiid_template_free (tmpl_var_checksum2_data);
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
  
  if (!dev 
      || !obj_cmd
      || !tmpl_cmd
      || !checksum)
    {
      errno = EINVAL;
      return (-1);
    }

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
      fiid_template_free (tmpl_var_checksum2_data);
      return (-1);
    }
  if (fiid_obj_set (var_checksum2_data, 
		    tmpl_var_checksum2_data, 
		    (uint8_t *)"rs_addr", 
		    val) == -1)
    {
      fiid_template_free (tmpl_var_checksum2_data);
      return (-1);
    }
  
  if (fiid_obj_get (dev->io.outofband.rs.obj_msg_hdr,
		    *(dev->io.outofband.rs.tmpl_msg_hdr_ptr), 
		    (uint8_t *)"rs_lun", 
		    &val) == -1)
    {
      fiid_template_free (tmpl_var_checksum2_data);
      return (-1);
    }
  if (fiid_obj_set (var_checksum2_data, 
		    tmpl_var_checksum2_data, 
		    (uint8_t *)"rs_lun", 
		    val) == -1)
    {
      fiid_template_free (tmpl_var_checksum2_data);
      return (-1);
    }
  
  if (fiid_obj_get (dev->io.outofband.rs.obj_msg_hdr,
		    *(dev->io.outofband.rs.tmpl_msg_hdr_ptr), 
		    (uint8_t *)"rq_seq", 
		    &val) == -1)
    {
      fiid_template_free (tmpl_var_checksum2_data);
      return (-1);
    }
  if (fiid_obj_set (var_checksum2_data, 
		    tmpl_var_checksum2_data, 
		    (uint8_t *)"rq_seq", 
		    val) == -1)
    {
      fiid_template_free (tmpl_var_checksum2_data);
      return (-1);
    }
  
  if (fiid_obj_set_data (var_checksum2_data, 
			 tmpl_var_checksum2_data, 
			 (uint8_t *)"COMMAND_DATA", 
			 obj_cmd, 
			 cmd_length) == -1)
    {
      fiid_template_free (tmpl_var_checksum2_data);
      return (-1);
    }
  
  *checksum = ipmi_chksum (var_checksum2_data, 
			   var_checksum2_data_length);
  
  fiid_template_free (tmpl_var_checksum2_data);
  return (0);
}

int8_t 
fill_lan_msg_hdr (uint8_t net_fn, 
		  uint8_t rs_lun, 
		  uint8_t rq_seq, 
		  fiid_obj_t obj_msg)
{
  if (!IPMI_NET_FN_VALID(net_fn)
      || !IPMI_BMC_LUN_VALID(rs_lun)
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
  
  if (!dev 
      || dev->io.outofband.rq.obj_msg_hdr == NULL)
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
  
  if (dev == NULL
      || dev->io.outofband.rq.obj_msg_trlr == NULL
      || obj_cmd == NULL
      || tmpl_cmd == NULL)
    {
      errno = EINVAL;
      return -1;
    }

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
  
  if (dev == NULL 
      || dev->io.outofband.rq.obj_hdr_session == NULL
      || obj_cmd == NULL
      || tmpl_cmd == NULL)
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
	uint8_t session_id_buf[4];
	uint8_t session_seq_num_buf[4];

	ipmi_md2_init (&ctx);
	ipmi_md2_update_data (&ctx, 
			      dev->io.outofband.password,
			      IPMI_SESSION_MAX_AUTH_CODE_LEN);
        session_id_buf[0] = (dev->io.outofband.session_id & 0x000000ff);
        session_id_buf[1] = (dev->io.outofband.session_id & 0x0000ff00) >> 8;
        session_id_buf[2] = (dev->io.outofband.session_id & 0x00ff0000) >> 16;
        session_id_buf[3] = (dev->io.outofband.session_id & 0xff000000) >> 24;
	ipmi_md2_update_data (&ctx, 
                              session_id_buf,
                              4);
	ipmi_md2_update_data (&ctx, 
			      dev->io.outofband.rq.obj_msg_hdr, 
			      fiid_obj_len_bytes (*(dev->io.outofband.rq.tmpl_msg_hdr_ptr)));
	ipmi_md2_update_data (&ctx, 
			      obj_cmd, 
			      fiid_obj_len_bytes (tmpl_cmd));
	ipmi_md2_update_data (&ctx, 
			      dev->io.outofband.rq.obj_msg_trlr, 
			      fiid_obj_len_bytes (*(dev->io.outofband.rq.tmpl_msg_trlr_ptr)));
        session_seq_num_buf[0] = (dev->io.outofband.session_seq_num & 0x000000ff);
        session_seq_num_buf[1] = (dev->io.outofband.session_seq_num & 0x0000ff00) >> 8;
        session_seq_num_buf[2] = (dev->io.outofband.session_seq_num & 0x00ff0000) >> 16;
        session_seq_num_buf[3] = (dev->io.outofband.session_seq_num & 0xff000000) >> 24;
	ipmi_md2_update_data (&ctx, 
                              session_seq_num_buf,
                              4);
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
	uint8_t session_id_buf[4];
	uint8_t session_seq_num_buf[4];
	
	ipmi_md5_init (&ctx);
	ipmi_md5_update_data (&ctx, 
			      dev->io.outofband.password,	      
			      IPMI_SESSION_MAX_AUTH_CODE_LEN);
        session_id_buf[0] = (dev->io.outofband.session_id & 0x000000ff);
        session_id_buf[1] = (dev->io.outofband.session_id & 0x0000ff00) >> 8;
        session_id_buf[2] = (dev->io.outofband.session_id & 0x00ff0000) >> 16;
        session_id_buf[3] = (dev->io.outofband.session_id & 0xff000000) >> 24;
	ipmi_md5_update_data (&ctx, 
                              session_id_buf,
                              4);
	ipmi_md5_update_data (&ctx, 
			      dev->io.outofband.rq.obj_msg_hdr, 
			      fiid_obj_len_bytes (*(dev->io.outofband.rq.tmpl_msg_hdr_ptr)));
	ipmi_md5_update_data (&ctx, 
			      obj_cmd, 
			      fiid_obj_len_bytes (tmpl_cmd));
	ipmi_md5_update_data (&ctx, 
			      dev->io.outofband.rq.obj_msg_trlr, 
			      fiid_obj_len_bytes (*(dev->io.outofband.rq.tmpl_msg_trlr_ptr)));
        session_seq_num_buf[0] = (dev->io.outofband.session_seq_num & 0x000000ff);
        session_seq_num_buf[1] = (dev->io.outofband.session_seq_num & 0x0000ff00) >> 8;
        session_seq_num_buf[2] = (dev->io.outofband.session_seq_num & 0x00ff0000) >> 16;
        session_seq_num_buf[3] = (dev->io.outofband.session_seq_num & 0xff000000) >> 24;
	ipmi_md5_update_data (&ctx, 
                              session_seq_num_buf,
                              4);
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

static int32_t 
_ipmi_lan_pkt_size (uint8_t auth_type, 
		    fiid_template_t tmpl_lan_msg, 
		    fiid_template_t tmpl_cmd)
{
  uint32_t msg_len;
  
  assert(IPMI_SESSION_AUTH_TYPE_VALID(auth_type)
         && tmpl_lan_msg
         && tmpl_cmd);

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
  
  return msg_len;
}

static int32_t 
_ipmi_lan_pkt_rq_size (uint8_t auth_type, 
		       fiid_template_t tmpl_cmd)
{
  return _ipmi_lan_pkt_size(auth_type, tmpl_lan_msg_hdr_rq, tmpl_cmd);
}

int32_t 
_ipmi_lan_pkt_rq_size2 (ipmi_device_t *dev, 
			fiid_template_t tmpl_cmd)
{
  if (!dev || !tmpl_cmd)
    {
      errno = EINVAL;
      return (-1);
    }

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
  if (!dev || !tmpl_cmd)
    {
      errno = EINVAL;
      return (-1);
    }

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
  if (!IPMI_SESSION_AUTH_TYPE_VALID(auth_type))
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

  /* auth_code generated last.  Save pointers for later calculation */
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

  /* Auth code must be done last, some authentication like md2 and md5
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
  
  if (!dev
      || !pkt
      || !obj_cmd
      || !tmpl_cmd)
    {
      errno = EINVAL;
      return (-1);
    }

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
  
  if (!dev
      || !obj_cmd
      || !tmpl_cmd)
    {
      errno = EINVAL;
      return (-1);
    }

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
		 const void *buffer, 
		 size_t buffer_size, 
		 int flags, 
		 const struct sockaddr *server_addr, 
		 socklen_t server_addr_len)
{
  void *packet = NULL;
  size_t packet_length = 0;
  ssize_t bytes_sent = 0;
  
  if (buffer == NULL || buffer_size == 0 || server_addr == NULL)
    {
      errno = EINVAL;
      return (-1);
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
  
  if (buffer_size == 56  ||
      buffer_size == 84  ||
      buffer_size == 112 ||
      buffer_size == 128 ||
      buffer_size == 156)
    {
      packet_length = buffer_size + IPMI_LAN_PKT_PAD_SIZE;
    }
  
  packet = alloca (packet_length);
  memset (packet, 0, packet_length);
  memcpy (packet, buffer, buffer_size);
  
  bytes_sent = sendto (sockfd, 
		       packet, 
		       packet_length, 
		       flags, 
		       server_addr, 
		       server_addr_len);
  
  if (bytes_sent == -1)
    return (-1);
  
  if (bytes_sent == packet_length)
    return (bytes_sent - IPMI_LAN_PKT_PAD_SIZE);
  
  return bytes_sent;
}

ssize_t 
ipmi_lan_recvfrom (int sockfd, 
		   void *buffer, 
		   size_t buffer_size, 
		   int flags, 
		   struct sockaddr *server_addr, 
		   socklen_t *server_addr_len)
{
  void *packet = NULL;
  size_t packet_length = 0;
  ssize_t bytes_received = 0;
  
  if (buffer == NULL || buffer_size == 0 || 
      server_addr == NULL || server_addr_len == NULL)
    {
      errno = EINVAL;
      return (-1);
    }
  
  if (buffer_size < 1024)
    packet_length = 1024;
  else 
    packet_length = buffer_size;
  
  /* See comment in ipmi_lan_sendto */
  /* WILL LET THIS CHECK GO SOON --ab@gnu.org.in */
  /*   if (buffer_size == 56  || */
  /*       buffer_size == 84  || */
  /*       buffer_size == 112 || */
  /*       buffer_size == 128 || */
  /*       buffer_size == 156) */
  /*     { */
  /*       packet_length = buffer_size + IPMI_LAN_PKT_PAD_SIZE; */
  /*     } */
  
  packet = alloca (packet_length);
  memset (packet, 0, packet_length);
  
  bytes_received = recvfrom (sockfd, 
			     packet, 
			     packet_length, 
			     flags, 
			     server_addr, 
			     server_addr_len);
  
  if (bytes_received == -1)
    return (-1);
  
  if (bytes_received > buffer_size)
    {
      fprintf (stderr, 
	       "%s(): oversized packet received.  received size:%d, expected size: %d\n", 
	       __PRETTY_FUNCTION__, bytes_received, buffer_size);
    }
  
  memcpy (buffer, packet, buffer_size);
  
  return bytes_received;
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

static int8_t 
ipmi_lan_cmd_send (ipmi_device_t *dev, 
		   fiid_obj_t obj_cmd_rq, 
		   fiid_template_t tmpl_cmd_rq)
{
  uint8_t *send_pkt;
  uint32_t send_pkt_len;
  int status = 0;
  
  if (!(dev && 
	dev->io.outofband.local_sockfd && 
	tmpl_cmd_rq && 
	obj_cmd_rq))
    {
      errno = EINVAL;
      return (-1);
    }
  
  send_pkt_len = _ipmi_lan_pkt_rq_size2 (dev, tmpl_cmd_rq);
  send_pkt = alloca (send_pkt_len);
  ERR (send_pkt);
  memset (send_pkt, 0, send_pkt_len);
  
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
  
  memset (send_pkt, 0, send_pkt_len);
  
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
			       send_pkt,
			       send_pkt_len) != -1);
  
#if 0
  printf("DEBUGGING:\n");
  
  fiid_obj_dump_lan (STDERR_FILENO,
		     NULL,
		     NULL,
		     send_pkt,
		     send_pkt_len,
		     *(dev->io.outofband.rs.tmpl_hdr_session_ptr),
		     *(dev->io.outofband.rs.tmpl_msg_hdr_ptr),
		     tmpl_cmd_rq);
#endif
  
  status = ipmi_lan_sendto (dev->io.outofband.local_sockfd, 
			    send_pkt, send_pkt_len, 0,
			    &(dev->io.outofband.remote_host),
			    dev->io.outofband.remote_host_len);
  
  if ((status == -1) || (status != send_pkt_len))
    {
      return (-1);
    }
  
  return (0);
}

static int8_t 
ipmi_lan_cmd_receive (ipmi_device_t *dev, 
		      fiid_obj_t obj_cmd_rs, 
		      fiid_template_t tmpl_cmd_rs)
{
  uint8_t *receive_pkt;
  uint32_t receive_pkt_len;
  const uint32_t pkt_max_size = 1024;
  int32_t bytes_received = 0;
  
  struct sockaddr_in server_addr;
  const socklen_t server_addr_len = sizeof (struct sockaddr_in);
  uint64_t val;
  
  fd_set fd_set;
  struct timeval timeout;
  int status = 0;
  
  int retry = 0;
  
  if (!(dev && 
	dev->io.outofband.local_sockfd && 
	tmpl_cmd_rs && 
	obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }
  
  receive_pkt_len = _ipmi_lan_pkt_rs_size2 (dev, tmpl_cmd_rs);
  ERR (receive_pkt_len <= pkt_max_size);
  receive_pkt = alloca (pkt_max_size);
  ERR (receive_pkt);
  memset (receive_pkt, 0, pkt_max_size);
  
  for (retry = 0; retry < dev->io.outofband.packet_retry_max; retry++)
    {
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
      
      memset (obj_cmd_rs,
	      0,
	      fiid_obj_len_bytes (tmpl_cmd_rs));
      
      if (dev->io.outofband.retry_timeout != 0) 
	{
	  timeout.tv_sec = 0;
	  timeout.tv_usec = dev->io.outofband.retry_timeout;
	  FD_ZERO (&fd_set);
	  FD_SET (dev->io.outofband.local_sockfd, &fd_set);
	  status = select (FD_SETSIZE, &fd_set, NULL, NULL, &timeout);
	  
	  if (status == -1)
	    {
	      return (-1);
	    }
	  
	  if (status == 0)
	    {
	      return (0); // resend the request
	    }
	}
      
      memset (receive_pkt, 0, pkt_max_size);
      bytes_received = recvfrom (dev->io.outofband.local_sockfd, 
				 receive_pkt, 
				 pkt_max_size, 
				 0, 
				 (struct sockaddr *) &server_addr,
				 &server_addr_len);
      
      if (bytes_received > receive_pkt_len)
	{
	  int i;
	  
	  fprintf (stderr, 
		   "%s(): received packet is too big.  "
		   "expected size = %d, received size = %d\n", 
		   __PRETTY_FUNCTION__, 
		   bytes_received,
		   receive_pkt_len);
	  fprintf (stderr, "packet data:\n");
	  for (i = 0; i < bytes_received; i++)
	    fprintf (stderr, "%02X ", receive_pkt[i]);
	  fprintf (stderr, "\n");
	  
	  fprintf (stderr, "please report to <freeipmi-devel@gnu.org>\n");
	  
	  return (-1);
	}
  
      if (bytes_received < receive_pkt_len)
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
	      
	      fprintf (stderr, 
		       "%s(): received packet is too small.  "
		       "expected size = %d, received size = %d, minimum size = %d\n", 
		       __PRETTY_FUNCTION__,
		       bytes_received,
		       receive_pkt_len,
		       min_len);
	      fprintf (stderr, "packet data:\n");
	      for (i = 0; i < bytes_received; i++)
		fprintf (stderr, "%02X ", receive_pkt[i]);
	      fprintf (stderr, "\n");
	      
	      fprintf (stderr, "please report to <freeipmi-devel@gnu.org>\n");
	      
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
	    memcpy (tmp_buf, &receive_pkt[trlr_start], trlr_len);
	    
	    fill_len = (receive_pkt_len - bytes_received);
	    for (i = 0; i < fill_len; i++)
	      receive_pkt[trlr_start + i] = 0;
	    
	    memcpy (&receive_pkt[trlr_start + fill_len],
		    tmp_buf,
		    trlr_len);
	    
	    bytes_received += fill_len;
	  }
	}
      
      ERR (unassemble_ipmi_lan_pkt2 (dev,
				     receive_pkt,
				     receive_pkt_len,
				     obj_cmd_rs,
				     tmpl_cmd_rs) != -1);
      
      val = 0;
      fiid_obj_get (dev->io.outofband.rs.obj_msg_hdr,
		    *(dev->io.outofband.rs.tmpl_msg_hdr_ptr), 
		    (uint8_t *)"rq_seq", 
		    &val);
      
      if (val == dev->io.outofband.rq_seq)
	{
	  ERR (ipmi_lan_validate_checksum (dev,
					   obj_cmd_rs,
					   tmpl_cmd_rs) == 0);
	  return (bytes_received);
	}
      else 
	{
	  syslog (LOG_MAKEPRI (LOG_LOCAL1, LOG_NOTICE), 
		  "%s(): received packet has mismatched rq_seq.  "
		  "packet rq_seq = %d, expected rq_seq = %d\n", 
		  __PRETTY_FUNCTION__, 
		  (int) val, 
		  (int) dev->io.outofband.rq_seq);
	  
	  if (dev->io.outofband.retry_timeout != 0) 
	    {
	      return (-1);
	    }
	}
    }
  
  return (0); // not yet received response.  Resend the request
}

int8_t 
ipmi_lan_cmd2 (ipmi_device_t *dev, 
	       fiid_obj_t obj_cmd_rq, 
	       fiid_template_t tmpl_cmd_rq, 
	       fiid_obj_t obj_cmd_rs, 
	       fiid_template_t tmpl_cmd_rs)
{
  int status = 0;
  int retry = 0;
  int retval = 0;
  
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
  
  for (retry = 0; retry < dev->io.outofband.packet_retry_max; retry++)
    {
      if (ipmi_lan_cmd_send (dev, 
			     obj_cmd_rq, 
			     tmpl_cmd_rq) == -1)
	{
	  retval = -1;
	  break;
	}
      
      status = ipmi_lan_cmd_receive (dev, 
				     obj_cmd_rs, 
				     tmpl_cmd_rs);
      
      if (status == -1)
	{
	  retval = -1;
	  break;
	}
      
      if (status != 0)
	{
	  retval = 0;
	  break;
	}
      
      IPMI_LAN_RQ_SEQ_INC (dev->io.outofband.rq_seq);
      retval = -1;
    }
  
  dev->io.outofband.session_seq_num++;
  IPMI_LAN_RQ_SEQ_INC (dev->io.outofband.rq_seq);
  return (retval);
}

static int8_t 
ipmi_lan_cmd_raw_send (ipmi_device_t *dev, 
		       fiid_obj_t obj_cmd_rq, 
		       fiid_template_t tmpl_cmd_rq)
{
  if (!dev || !obj_cmd_rq || !tmpl_cmd_rq)
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
	buf_rq_len && 
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
    fiid_template_free (tmpl_var_cmd_rq);
    
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
    
    pkt_len = 1024 - pkt_hdrs_size - 1;
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

    if ((buf_rs_len_in - 1) < obj_cmd_rs_len)
      {
        errno = EINVAL;
        return (-1);
      }

    tmpl_var_cmd_rs = fiid_template_make ((obj_cmd_rs_len * 8), "COMMAND_RS_DATA");
    obj_cmd_rs = (fiid_obj_t) (buf_rs + 1);
    
    if (unassemble_ipmi_lan_pkt2 (dev, 
				  pkt, 
				  bytes_received, 
				  obj_cmd_rs, 
				  tmpl_var_cmd_rs) == -1)
      {
	fiid_template_free (tmpl_var_cmd_rs);
	return (-1);
      }
    
    if (ipmi_lan_validate_checksum (dev, 
				    obj_cmd_rs, 
				    tmpl_var_cmd_rs) != 0)
      {
	fiid_template_free (tmpl_var_cmd_rs);
	return (-1);
      }
    
    fiid_template_free (tmpl_var_cmd_rs);
    
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

  if (!(obj_msg_hdr 
        && tmpl_msg_hdr 
        && IPMI_NET_FN_VALID(net_fn)))
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

  return ((((uint8_t)net_fn_recv) == net_fn) ? 1 : 0);
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

  return ((((uint8_t)rq_seq_recv) == rq_seq) ? 1 : 0);
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
