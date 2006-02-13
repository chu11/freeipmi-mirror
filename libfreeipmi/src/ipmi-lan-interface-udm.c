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
#if 0
	/* DEBUGGING */
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
#endif	
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
