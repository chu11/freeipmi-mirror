/* 
   ipmi-sdr-repo-cmds.c - IPMI SDR Repository commands

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

fiid_template_t tmpl_get_sdr_repo_info_rq =
  {
    {8, "cmd"}, 
    {0, ""}
  };

fiid_template_t tmpl_get_sdr_repo_info_rs =
  {
    {8,  "cmd"}, 
    {8,  "comp_code"}, 
    {4,  "sdr_version_major"}, 
    {4,  "sdr_version_minor"}, 
    {16, "record_count"}, //LS byte first
    {16, "free_space"}, //LS byte first
    {32, "recent_addition_timestamp"}, //LS byte first
    {32, "recent_erase_timestamp"}, 
    {1,  "get_sdr_repo_alloc_info_cmd_support"}, 
    {1,  "reserve_sdr_repo_cmd_support"}, 
    {1,  "partial_add_sdr_cmd_support"}, 
    {1,  "delete_sdr_cmd_support"}, 
    {1,  "reserved"}, 
    {2,  "modal_non_modal_sdr_repo_update_op_support"}, 
    {1,  "overflow_flag"}, 
    {0,  ""}
  };


fiid_template_t tmpl_get_sdr_repo_alloc_info_rq =
  {
    {8, "cmd"}, 
    {0, ""}
  };

fiid_template_t tmpl_get_sdr_repo_alloc_info_rs =
  {
    {8,  "cmd"}, 
    {8,  "comp_code"}, 
    {16, "no_of_possible_alloc_units"}, 
    {16, "calloction_unit_size"}, 
    {16, "no_of_free_alloc_units"}, 
    {16, "larget_free_block"}, 
    {8, "max_record_size"}, 
    {0, ""}
  };

fiid_template_t tmpl_reserve_sdr_repo_rq =
  {
    {8, "cmd"}, 
    {0, ""}
  };

fiid_template_t tmpl_reserve_sdr_repo_rs =
  {
    {8, "cmd"}, 
    {8, "comp_code"}, 
    {16, "reservation_id"}, //LS byte first
    {0, ""}
  };

fiid_template_t tmpl_get_sdr_rq =
  {
    {8,  "cmd"}, 
    {16, "reservation_id"}, //LS byte first
    {16, "record_id"}, //LS byte first
    {8,  "record_offset"}, 
    {8,  "bytes_read"}, 
    {0,  ""}
  };

fiid_template_t tmpl_get_sdr_rs =
  {
    {8,  "cmd"}, 
    {8,  "comp_code"}, 
    {16, "next_record_id"}, //LS byte first
    // record data field will be added on the fly
    {0,  ""}
  };

int8_t 
fill_kcs_get_repo_info (fiid_obj_t obj_data_rq)
{
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_get_sdr_repo_info_rq, 
		"cmd", 
		IPMI_CMD_GET_SDR_REPOSITORY_INFO);
  return 0;
}

int8_t 
ipmi_kcs_get_repo_info (fiid_obj_t obj_data_rs)
{
  fiid_obj_t obj_data_rq; 
  int8_t status;
  
  obj_data_rq = fiid_obj_calloc (tmpl_get_sdr_repo_info_rq);
  fill_kcs_get_repo_info (obj_data_rq);
  status = ipmi_kcs_cmd (IPMI_BMC_IPMB_LUN_BMC, IPMI_NET_FN_STORAGE_RQ, 
			 obj_data_rq, tmpl_get_sdr_repo_info_rq, 
			 obj_data_rs, tmpl_get_sdr_repo_info_rs);
  free (obj_data_rq);
  return status;
}

int8_t 
fill_kcs_get_repo_alloc_info (fiid_obj_t obj_data_rq)
{
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_get_sdr_repo_alloc_info_rq, 
		"cmd", 
		IPMI_CMD_GET_SDR_REPOSITORY_ALLOC_INFO);
  return 0;
}

int8_t 
ipmi_kcs_get_repo_alloc_info (fiid_obj_t obj_data_rs)
{
  fiid_obj_t obj_data_rq; 
  int8_t status;
  
  obj_data_rq = fiid_obj_calloc (tmpl_get_sdr_repo_alloc_info_rq);
  fill_kcs_get_repo_alloc_info (obj_data_rq);
  status = ipmi_kcs_cmd (IPMI_BMC_IPMB_LUN_BMC, IPMI_NET_FN_STORAGE_RQ, 
			 obj_data_rq, tmpl_get_sdr_repo_alloc_info_rq, 
			 obj_data_rs, tmpl_get_sdr_repo_alloc_info_rs);
  free (obj_data_rq);
  return status;
}

int8_t 
fill_kcs_reserve_repo (fiid_obj_t obj_data_rq)
{
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_reserve_sdr_repo_rq, 
		"cmd", 
		IPMI_CMD_RESERVE_SDR_REPOSITORY);
  return 0;
}

int8_t 
ipmi_kcs_reserve_repo (fiid_obj_t obj_data_rs)
{
  fiid_obj_t obj_data_rq; 
  int8_t status;
  
  obj_data_rq = fiid_obj_calloc (tmpl_reserve_sdr_repo_rq);
  fill_kcs_reserve_repo (obj_data_rq);
  status = ipmi_kcs_cmd (IPMI_BMC_IPMB_LUN_BMC, IPMI_NET_FN_STORAGE_RQ, 
			 obj_data_rq, tmpl_reserve_sdr_repo_rq, 
			 obj_data_rs, tmpl_reserve_sdr_repo_rs);
  free (obj_data_rq);
  return status;
}

int8_t 
fill_kcs_get_sensor_record_header (fiid_obj_t obj_data_rq, u_int16_t record_id)
{
  FIID_OBJ_SET (obj_data_rq,
		tmpl_get_sdr_rq,
		"cmd",
		IPMI_CMD_GET_SDR);
  
  FIID_OBJ_SET (obj_data_rq,
		tmpl_get_sdr_rq,
		"reservation_id",
		0x0);
  
  FIID_OBJ_SET (obj_data_rq,
		tmpl_get_sdr_rq,
		"record_id",
		record_id);
  
  FIID_OBJ_SET (obj_data_rq,
		tmpl_get_sdr_rq,
		"record_offset",
		0x0);
  
  FIID_OBJ_SET (obj_data_rq,
		tmpl_get_sdr_rq,
		"bytes_read",
		fiid_obj_len_bytes (tmpl_sdr_sensor_record_header));
  return 0;
}

int8_t 
ipmi_kcs_get_sensor_record_header (u_int16_t record_id, 
				   fiid_obj_t obj_data_rs, 
				   u_int8_t *sensor_record_header,
                                   u_int32_t sensor_record_header_len)
{
  fiid_obj_t obj_data_rq; 
  int8_t status;
  
  fiid_field_t *tmpl_var_len_get_sdr_rs = NULL;
  fiid_obj_t obj_var_len_data_rs;
  int header_length = 0; 
  
  header_length = fiid_obj_len_bytes (tmpl_sdr_sensor_record_header);
  tmpl_var_len_get_sdr_rs = fiid_template_make (8, "cmd", 
						8, "comp_code", 
						16, "next_record_id", 
						(header_length * 8), "record_data");
  obj_var_len_data_rs = fiid_obj_calloc (tmpl_var_len_get_sdr_rs);
  
  obj_data_rq = fiid_obj_calloc (tmpl_get_sdr_rq);
  fill_kcs_get_sensor_record_header (obj_data_rq, record_id);
  status = ipmi_kcs_cmd (IPMI_BMC_IPMB_LUN_BMC, IPMI_NET_FN_STORAGE_RQ, 
			 obj_data_rq, tmpl_get_sdr_rq, 
			 obj_var_len_data_rs, tmpl_var_len_get_sdr_rs);
  free (obj_data_rq);
  
  memcpy (obj_data_rs, 
	  obj_var_len_data_rs, 
	  fiid_obj_len_bytes (tmpl_var_len_get_sdr_rs) - header_length);
  fiid_obj_get_data (obj_var_len_data_rs, 
		     tmpl_var_len_get_sdr_rs, 
		     "record_data", 
		     sensor_record_header,
                     sensor_record_header_len);
  
  free (tmpl_var_len_get_sdr_rs);
  free (obj_var_len_data_rs);
  
  return status;
}

int8_t 
fill_kcs_get_sdr_chunk (fiid_obj_t obj_data_rq, 
			u_int16_t reservation_id, 
			u_int16_t record_id, 
			u_int8_t record_offset, 
			u_int8_t bytes_read)
{
  FIID_OBJ_SET (obj_data_rq,
		tmpl_get_sdr_rq,
		"cmd",
		IPMI_CMD_GET_SDR);
  
  FIID_OBJ_SET (obj_data_rq,
		tmpl_get_sdr_rq,
		"reservation_id",
		reservation_id);
  
  FIID_OBJ_SET (obj_data_rq,
		tmpl_get_sdr_rq,
		"record_id",
		record_id);
  
  FIID_OBJ_SET (obj_data_rq,
		tmpl_get_sdr_rq,
		"record_offset",
		record_offset);
  
  FIID_OBJ_SET (obj_data_rq,
		tmpl_get_sdr_rq,
		"bytes_read",
		bytes_read);
  return 0;
}

int8_t 
ipmi_kcs_get_sdr_chunk (u_int16_t reservation_id, 
			u_int16_t record_id, 
			u_int8_t record_offset, 
			u_int8_t bytes_read, 
			fiid_obj_t obj_data_rs, 
			u_int8_t *sensor_record_chunk,
                        u_int32_t sensor_record_chunk_len) 
{
  fiid_obj_t obj_data_rq; 
  int8_t status;
  
  fiid_field_t *tmpl_var_len_get_sdr_rs = NULL;
  fiid_obj_t obj_var_len_data_rs;
  
  tmpl_var_len_get_sdr_rs = fiid_template_make (8, "cmd", 
						8, "comp_code", 
						16, "next_record_id", 
						(bytes_read * 8), "record_data");
  obj_var_len_data_rs = fiid_obj_calloc (tmpl_var_len_get_sdr_rs);
  
  obj_data_rq = fiid_obj_calloc (tmpl_get_sdr_rq);
  fill_kcs_get_sdr_chunk (obj_data_rq, reservation_id, 
			  record_id, record_offset, bytes_read);
  status = ipmi_kcs_cmd (IPMI_BMC_IPMB_LUN_BMC, IPMI_NET_FN_STORAGE_RQ, 
			 obj_data_rq, tmpl_get_sdr_rq, 
			 obj_var_len_data_rs, tmpl_var_len_get_sdr_rs);
  free (obj_data_rq);
  
  memcpy (obj_data_rs, 
	  obj_var_len_data_rs, 
	  fiid_obj_len_bytes (tmpl_var_len_get_sdr_rs) - bytes_read);
  fiid_obj_get_data (obj_var_len_data_rs, 
		     tmpl_var_len_get_sdr_rs, 
		     "record_data", 
		     sensor_record_chunk,
                     sensor_record_chunk_len);
  
  free (tmpl_var_len_get_sdr_rs);
  free (obj_var_len_data_rs);
  
  return status;
}

int8_t 
ipmi_kcs_get_sdr (u_int16_t record_id, 
		  u_int8_t record_length, 
		  u_int8_t *sensor_record, 
		  u_int8_t *comp_code)
{
  u_int8_t record_offset; 
  u_int8_t bytes_read;
  
  u_int64_t val;
  u_int16_t reservation_id;
  u_int8_t data_rs[21];
  u_int8_t record_data[16];
  fiid_obj_t obj_data_rs = (fiid_obj_t) data_rs;
  
  reservation_id = 0x0;
  
  if (record_length > 16)
    {
      *comp_code = 0x0;
      if (ipmi_kcs_reserve_repo (data_rs) != 0)
	return -1;
      
      fiid_obj_get (data_rs,  
		    tmpl_reserve_sdr_repo_rs, 
		    "comp_code", 
		    &val);
      *comp_code = (u_int8_t) val;
      
      if (*comp_code != IPMI_KCS_STATUS_SUCCESS)
	return -1;
      
      fiid_obj_get (data_rs,  
		    tmpl_reserve_sdr_repo_rs, 
		    "reservation_id", 
		    &val);
      reservation_id = (u_int16_t) val;
      /*       printf ("Reservation ID: %X\n", reservation_id); */
    }
  
  for (record_offset = 0; record_offset < record_length; record_offset += 16)
    {
      bytes_read = 16; 
      if ((record_offset + bytes_read) > record_length)
	bytes_read = record_length - record_offset;
      
      /*       printf ("record_offset: %d, received bytes: %d\n", record_offset, bytes_read); */
      
      *comp_code = 0x0;
      memset (record_data, 0, 16);
      if (ipmi_kcs_get_sdr_chunk (reservation_id, 
				  record_id, 
				  record_offset, 
				  bytes_read, 
				  obj_data_rs, 
				  record_data,
                                  16) != 0)
	return -1;
      
      /* 	  printf ("received record_offset: %d, received bytes: %d\n", record_offset, bytes_read); */
      fiid_obj_get (data_rs, 
		    tmpl_get_sdr_rs, 
		    "comp_code", 
		    &val);
      *comp_code = (u_int8_t) val;
      if (*comp_code != IPMI_KCS_STATUS_SUCCESS)
	return -1;
      
      memcpy (sensor_record + record_offset, record_data, bytes_read);
    }
  
  return 0;
}

int8_t 
ipmi_cmd_get_sdr_repo_info2 (ipmi_device_t *dev, 
			     fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  ERR (dev != NULL);
  ERR (obj_cmd_rs != NULL);
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_get_sdr_repo_info_rq);
  ERR (fill_kcs_get_repo_info (obj_cmd_rq) == 0);
  dev->lun = IPMI_BMC_IPMB_LUN_BMC;
  dev->net_fn = IPMI_NET_FN_STORAGE_RQ;
  ERR (ipmi_cmd (dev, 
		 obj_cmd_rq, 
		 tmpl_get_sdr_repo_info_rq, 
		 obj_cmd_rs, 
		 tmpl_get_sdr_repo_info_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_get_sdr_repo_alloc_info2 (ipmi_device_t *dev, 
				   fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  ERR (dev != NULL);
  ERR (obj_cmd_rs != NULL);
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_get_sdr_repo_alloc_info_rq);
  ERR (fill_kcs_get_repo_alloc_info (obj_cmd_rq) == 0);
  dev->lun = IPMI_BMC_IPMB_LUN_BMC;
  dev->net_fn = IPMI_NET_FN_STORAGE_RQ;
  ERR (ipmi_cmd (dev, 
		 obj_cmd_rq, 
		 tmpl_get_sdr_repo_alloc_info_rq, 
		 obj_cmd_rs, 
		 tmpl_get_sdr_repo_alloc_info_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_reserve_sdr_repo2 (ipmi_device_t *dev, 
			    fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  ERR (dev != NULL);
  ERR (obj_cmd_rs != NULL);
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_reserve_sdr_repo_rq);
  ERR (fill_kcs_reserve_repo (obj_cmd_rq) == 0);
  dev->lun = IPMI_BMC_IPMB_LUN_BMC;
  dev->net_fn = IPMI_NET_FN_STORAGE_RQ;
  ERR (ipmi_cmd (dev, 
		 obj_cmd_rq, 
		 tmpl_reserve_sdr_repo_rq, 
		 obj_cmd_rs, 
		 tmpl_reserve_sdr_repo_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

static int8_t 
ipmi_cmd_get_sensor_record_header2 (ipmi_device_t *dev, 
				    u_int16_t record_id, 
				    fiid_obj_t obj_cmd_rs, 
				    fiid_obj_t sensor_record_header)
{
  fiid_field_t *tmpl_var_len_get_sdr_rs = NULL;
  
  int sdr_rs_length = 0;
  int header_length = 0; 
  
  fiid_obj_t obj_cmd_rq = NULL;
  fiid_obj_t local_obj_cmd_rs = NULL;
  
  ERR (dev != NULL);
  ERR (obj_cmd_rs != NULL);
  ERR (sensor_record_header != NULL);
  
  sdr_rs_length = fiid_obj_len_bytes (tmpl_get_sdr_rs);
  ERR (sdr_rs_length != -1);
  
  header_length = fiid_obj_len_bytes (tmpl_sdr_sensor_record_header);
  ERR (header_length != -1);
  
  tmpl_var_len_get_sdr_rs = fiid_template_make ((sdr_rs_length * 8), "sdr_rs", 
						(header_length * 8), "header_data");
  
  fiid_obj_alloca (obj_cmd_rq, tmpl_get_sdr_rq);
  if (obj_cmd_rq == NULL)
    {
      ipmi_xfree (tmpl_var_len_get_sdr_rs);
      return (-1);
    }
  fiid_obj_alloca (local_obj_cmd_rs, tmpl_var_len_get_sdr_rs);
  if (local_obj_cmd_rs == NULL)
    {
      ipmi_xfree (tmpl_var_len_get_sdr_rs);
      return (-1);
    }
  
  if (fill_kcs_get_sensor_record_header (obj_cmd_rq, 
					 record_id) != 0)
    {
      ipmi_xfree (tmpl_var_len_get_sdr_rs);
      return (-1);
    }
  dev->lun = IPMI_BMC_IPMB_LUN_BMC;
  dev->net_fn = IPMI_NET_FN_STORAGE_RQ;
  if (ipmi_cmd (dev, 
		obj_cmd_rq, 
		tmpl_get_sdr_rq, 
		local_obj_cmd_rs, 
		tmpl_var_len_get_sdr_rs) != 0)
    {
      ipmi_xfree (tmpl_var_len_get_sdr_rs);
      return (-1);
    }
  fiid_obj_get_data (local_obj_cmd_rs, 
		     tmpl_var_len_get_sdr_rs, 
		     "sdr_rs", 
		     obj_cmd_rs,
                     fiid_obj_len_bytes(tmpl_get_sdr_rs));
  fiid_obj_get_data (local_obj_cmd_rs, 
		     tmpl_var_len_get_sdr_rs, 
		     "header_data", 
		     sensor_record_header,
                     fiid_obj_len_bytes(tmpl_sdr_sensor_record_header));
  
  ipmi_xfree (tmpl_var_len_get_sdr_rs);
  
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

static int8_t 
ipmi_cmd_get_sdr_chunk2 (ipmi_device_t *dev, 
			 u_int16_t reservation_id, 
			 u_int16_t record_id, 
			 u_int8_t record_offset, 
			 u_int8_t bytes_read, 
			 fiid_obj_t obj_cmd_rs, 
			 u_int8_t *sensor_record_chunk,
                         u_int32_t sensor_record_chunk_len)
{
  fiid_field_t *tmpl_var_len_get_sdr_rs = NULL;
  
  int sdr_rs_length = 0;
  
  fiid_obj_t obj_cmd_rq = NULL;
  fiid_obj_t local_obj_cmd_rs = NULL;
  
  ERR (dev != NULL);
  ERR (obj_cmd_rs != NULL);
  ERR (sensor_record_chunk != NULL);
  
  sdr_rs_length = fiid_obj_len_bytes (tmpl_get_sdr_rs);
  ERR (sdr_rs_length != -1);
  
  tmpl_var_len_get_sdr_rs = fiid_template_make ((sdr_rs_length * 8), "sdr_rs", 
						(bytes_read * 8), "chunk_data");
  
  fiid_obj_alloca (obj_cmd_rq, tmpl_get_sdr_rq);
  if (obj_cmd_rq == NULL)
    {
      ipmi_xfree (tmpl_var_len_get_sdr_rs);
      return (-1);
    }
  fiid_obj_alloca (local_obj_cmd_rs, tmpl_var_len_get_sdr_rs);
  if (local_obj_cmd_rs == NULL)
    {
      ipmi_xfree (tmpl_var_len_get_sdr_rs);
      return (-1);
    }
  
  if (fill_kcs_get_sdr_chunk (obj_cmd_rq, reservation_id, 
			      record_id, record_offset, 
			      bytes_read) != 0)
    {
      ipmi_xfree (tmpl_var_len_get_sdr_rs);
      return (-1);
    }
  dev->lun = IPMI_BMC_IPMB_LUN_BMC;
  dev->net_fn = IPMI_NET_FN_STORAGE_RQ;
  if (ipmi_cmd (dev, 
		obj_cmd_rq, 
		tmpl_get_sdr_rq, 
		local_obj_cmd_rs, 
		tmpl_var_len_get_sdr_rs) != 0)
    {
      ipmi_xfree (tmpl_var_len_get_sdr_rs);
      return (-1);
    }
  
  fiid_obj_get_data (local_obj_cmd_rs, 
		     tmpl_var_len_get_sdr_rs, 
		     "sdr_rs", 
		     obj_cmd_rs,
                     fiid_obj_len_bytes(tmpl_get_sdr_rs));
  fiid_obj_get_data (local_obj_cmd_rs, 
		     tmpl_var_len_get_sdr_rs, 
		     "chunk_data", 
		     sensor_record_chunk,
                     sensor_record_chunk_len);
  
  ipmi_xfree (tmpl_var_len_get_sdr_rs);
  
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_get_sdr2 (ipmi_device_t *dev, 
		   u_int16_t record_id, 
		   fiid_obj_t obj_cmd_rs, 
		   fiid_obj_t *sensor_record)
{
  u_int64_t val = 0;
  
  u_int8_t record_length = 0;
  u_int16_t reservation_id = 0;
  u_int8_t record_offset = 0;
  u_int8_t bytes_read = 0;
  
  u_int8_t chunk_data[16];
  
  fiid_obj_t record_data = NULL;
  
  ERR (dev != NULL);
  ERR (obj_cmd_rs != NULL);
  ERR (sensor_record != NULL);
  
  {
    fiid_obj_t sensor_record_header = NULL;
    
    FIID_OBJ_ALLOCA (sensor_record_header, tmpl_sdr_sensor_record_header);
    ERR (ipmi_cmd_get_sensor_record_header2 (dev, 
					     record_id, 
					     obj_cmd_rs, 
					     sensor_record_header) == 0);
    FIID_OBJ_GET (sensor_record_header, 
		  tmpl_sdr_sensor_record_header, 
		  "record_length", 
		  &val);
    record_length = val;
    record_length += fiid_obj_len_bytes (tmpl_sdr_sensor_record_header);
  }
  
  if (record_length > 16)
    {
      fiid_obj_t local_obj_cmd_rs = NULL;
      
      FIID_OBJ_ALLOCA (local_obj_cmd_rs, tmpl_reserve_sdr_repo_rs);
      ERR (ipmi_cmd_reserve_sdr_repo2 (dev, local_obj_cmd_rs) == 0);
      FIID_OBJ_GET (local_obj_cmd_rs,  
		    tmpl_reserve_sdr_repo_rs, 
		    "reservation_id", 
		    &val);
      reservation_id = (u_int16_t) val;
    }
  
  record_data = alloca (record_length);
  memset (record_data, 0, record_length);
  
  for (record_offset = 0; record_offset < record_length; record_offset += 16)
    {
      bytes_read = 16;
      if ((record_offset + bytes_read) > record_length)
	bytes_read = record_length - record_offset;
      
      ERR (ipmi_cmd_get_sdr_chunk2 (dev, 
				    reservation_id, 
				    record_id, 
				    record_offset, 
				    bytes_read, 
				    obj_cmd_rs, 
				    chunk_data,
                                    16) == 0);
      
      memcpy (record_data + record_offset, chunk_data, bytes_read);
    }
  
  *sensor_record = ipmi_xmalloc (record_length);
  ERR (*sensor_record != NULL);
  memcpy (*sensor_record, record_data, record_length);
  
  return 0;
}

