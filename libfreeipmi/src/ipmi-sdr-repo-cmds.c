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
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0, "", 0}
  };

fiid_template_t tmpl_get_sdr_repo_info_rs =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4,  "sdr_version_major", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4,  "sdr_version_minor", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {16, "record_count", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, //LS byte first
    {16, "free_space", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, //LS byte first
    {32, "recent_addition_timestamp", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, //LS byte first
    {32, "recent_erase_timestamp", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1,  "get_sdr_repo_alloc_info_cmd_support", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1,  "reserve_sdr_repo_cmd_support", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1,  "partial_add_sdr_cmd_support", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1,  "delete_sdr_cmd_support", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1,  "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {2,  "modal_non_modal_sdr_repo_update_op_support", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1,  "overflow_flag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0,  "", 0}
  };


fiid_template_t tmpl_get_sdr_repo_alloc_info_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0, "", 0}
  };

fiid_template_t tmpl_get_sdr_repo_alloc_info_rs =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {16, "no_of_possible_alloc_units", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {16, "calloction_unit_size", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {16, "no_of_free_alloc_units", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {16, "larget_free_block", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "max_record_size", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0, "", 0}
  };

fiid_template_t tmpl_reserve_sdr_repo_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0, "", 0}
  };

fiid_template_t tmpl_reserve_sdr_repo_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {16, "reservation_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, //LS byte first
    {0, "", 0}
  };

fiid_template_t tmpl_get_sdr_rq =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {16, "reservation_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, //LS byte first
    {16, "record_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, //LS byte first
    {8,  "record_offset", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8,  "bytes_read", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0,  "", 0}
  };

fiid_template_t tmpl_get_sdr_rs =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {16, "next_record_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, //LS byte first
    {4096, "record_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_VARIABLE},
    {0,  "", 0}
  };

int8_t 
fill_kcs_get_repo_info (fiid_obj_t obj_data_rq)
{
  int8_t rv;

  if (!fiid_obj_valid(obj_data_rq))
    {
      errno = EINVAL;
      return -1;
    }

  if ((rv = fiid_obj_template_compare(obj_data_rq, tmpl_get_sdr_repo_info_rq)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"cmd", 
		IPMI_CMD_GET_SDR_REPOSITORY_INFO);
  return 0;
}

int8_t 
fill_kcs_get_repo_alloc_info (fiid_obj_t obj_data_rq)
{
  int8_t rv;

  if (!fiid_obj_valid(obj_data_rq))
    {
      errno = EINVAL;
      return -1;
    }

  if ((rv = fiid_obj_template_compare(obj_data_rq, tmpl_get_sdr_repo_alloc_info_rq)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"cmd", 
		IPMI_CMD_GET_SDR_REPOSITORY_ALLOC_INFO);
  return 0;
}

int8_t 
fill_kcs_reserve_repo (fiid_obj_t obj_data_rq)
{
  int8_t rv;

  if (!fiid_obj_valid(obj_data_rq))
    {
      errno = EINVAL;
      return -1;
    }

  if ((rv = fiid_obj_template_compare(obj_data_rq, tmpl_reserve_sdr_repo_rq)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"cmd", 
		IPMI_CMD_RESERVE_SDR_REPOSITORY);
  return 0;
}

int8_t 
fill_kcs_get_sensor_record_header (fiid_obj_t obj_data_rq, uint16_t record_id)
{
  int8_t rv;
  int32_t len;

  if (!fiid_obj_valid(obj_data_rq))
    {
      errno = EINVAL;
      return -1;
    }

  if ((rv = fiid_obj_template_compare(obj_data_rq, tmpl_get_sdr_rq)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return -1;
    }

  if ((len = fiid_template_len_bytes (tmpl_sdr_sensor_record_header)) < 0)
    return (-1);

  FIID_OBJ_SET (obj_data_rq,
		(uint8_t *)"cmd",
		IPMI_CMD_GET_SDR);
  
  FIID_OBJ_SET (obj_data_rq,
		(uint8_t *)"reservation_id",
		0x0);
  
  FIID_OBJ_SET (obj_data_rq,
		(uint8_t *)"record_id",
		record_id);
  
  FIID_OBJ_SET (obj_data_rq,
		(uint8_t *)"record_offset",
		0x0);
  
  FIID_OBJ_SET (obj_data_rq,
		(uint8_t *)"bytes_read",
                len);
  return 0;
}

int8_t 
fill_kcs_get_sdr_chunk (fiid_obj_t obj_data_rq, 
			uint16_t reservation_id, 
			uint16_t record_id, 
			uint8_t record_offset, 
			uint8_t bytes_read)
{
  int8_t rv;

  if (!fiid_obj_valid(obj_data_rq))
    {
      errno = EINVAL;
      return -1;
    }

  if ((rv = fiid_obj_template_compare(obj_data_rq, tmpl_get_sdr_rq)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_SET (obj_data_rq,
		(uint8_t *)"cmd",
		IPMI_CMD_GET_SDR);
  
  FIID_OBJ_SET (obj_data_rq,
		(uint8_t *)"reservation_id",
		reservation_id);
  
  FIID_OBJ_SET (obj_data_rq,
		(uint8_t *)"record_id",
		record_id);
  
  FIID_OBJ_SET (obj_data_rq,
		(uint8_t *)"record_offset",
		record_offset);
  
  FIID_OBJ_SET (obj_data_rq,
		(uint8_t *)"bytes_read",
		bytes_read);
  return 0;
}

int8_t 
ipmi_cmd_get_sdr_repo_info2 (ipmi_device_t *dev, 
			     fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t ret, rv = -1;

  if (!dev || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return -1;
    }

  if ((ret = fiid_obj_template_compare(obj_cmd_rs, tmpl_get_sdr_repo_info_rs)) < 0)
    goto cleanup;

  if (!ret)
    {
      errno = EINVAL;
      goto cleanup;
    }

  if (!(obj_cmd_rq = fiid_obj_create(tmpl_get_sdr_repo_info_rq)))
    goto cleanup;
  if (fill_kcs_get_repo_info (obj_cmd_rq) < 0)
    goto cleanup;
  if (ipmi_cmd (dev, 
		IPMI_BMC_IPMB_LUN_BMC, 
		IPMI_NET_FN_STORAGE_RQ, 
		obj_cmd_rq, 
		obj_cmd_rs) < 0)
    goto cleanup;
  if (ipmi_comp_test (obj_cmd_rs) != 1)
    goto cleanup;

  rv = 0;
 cleanup:
  if (obj_cmd_rq)
    fiid_obj_destroy(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_get_sdr_repo_alloc_info2 (ipmi_device_t *dev, 
				   fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t ret, rv = -1;

  if (!dev || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return -1;
    }
  
  if ((ret = fiid_obj_template_compare(obj_cmd_rs, tmpl_get_sdr_repo_alloc_info_rs)) < 0)
    goto cleanup;

  if (!ret)
    {
      errno = EINVAL;
      goto cleanup;
    }

  if (!(obj_cmd_rq = fiid_obj_create(tmpl_get_sdr_repo_alloc_info_rq)))
    goto cleanup;
  if (fill_kcs_get_repo_alloc_info (obj_cmd_rq) < 0)
    goto cleanup;
  if (ipmi_cmd (dev, 
		IPMI_BMC_IPMB_LUN_BMC, 
		IPMI_NET_FN_STORAGE_RQ, 
		obj_cmd_rq, 
		obj_cmd_rs) < 0)
    goto cleanup;
  if (ipmi_comp_test (obj_cmd_rs) != 1)
    goto cleanup;
  
  rv = 0;
 cleanup:
  if (obj_cmd_rq)
    fiid_obj_destroy(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_reserve_sdr_repo2 (ipmi_device_t *dev, 
			    fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t ret, rv = -1;
  
  if (!dev || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return -1;
    }
  
  if ((ret = fiid_obj_template_compare(obj_cmd_rs, tmpl_reserve_sdr_repo_rs)) < 0)
    goto cleanup;

  if (!ret)
    {
      errno = EINVAL;
      goto cleanup;
    }

  if (!(obj_cmd_rq = fiid_obj_create(tmpl_reserve_sdr_repo_rq)))
    goto cleanup;
  if (fill_kcs_reserve_repo (obj_cmd_rq) < 0)
    goto cleanup;
  if (ipmi_cmd (dev, 
		IPMI_BMC_IPMB_LUN_BMC, 
		IPMI_NET_FN_STORAGE_RQ, 
		obj_cmd_rq, 
		obj_cmd_rs) < 0)
    goto cleanup;
  if (ipmi_comp_test (obj_cmd_rs) != 1)
    goto cleanup;
  
  rv = 0;
 cleanup:
  if (obj_cmd_rq)
    fiid_obj_destroy(obj_cmd_rq);
  return (rv);
}

static int8_t 
ipmi_cmd_get_sensor_record_header2 (ipmi_device_t *dev, 
				    uint16_t record_id, 
				    fiid_obj_t obj_cmd_rs, 
				    fiid_obj_t sensor_record_header)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t ret, rv = -1;
  int32_t len;
  uint8_t *buf = NULL;

  if (!dev 
      || !fiid_obj_valid(obj_cmd_rs)
      || !sensor_record_header)
    {
      errno = EINVAL;
      return -1;
    }

  if ((ret = fiid_obj_template_compare(obj_cmd_rs, tmpl_get_sdr_rs)) < 0)
    goto cleanup;

  if (!ret)
    {
      errno = EINVAL;
      goto cleanup;
    }

  if ((ret = fiid_obj_template_compare(sensor_record_header, tmpl_sdr_sensor_record_header)) < 0)
    goto cleanup;

  if (!ret)
    {
      errno = EINVAL;
      goto cleanup;
    }

  if (!(obj_cmd_rq = fiid_obj_create(tmpl_get_sdr_rq)))
    goto cleanup;

  if (fill_kcs_get_sensor_record_header (obj_cmd_rq, record_id) < 0)
    goto cleanup;

  if (ipmi_cmd (dev, 
		IPMI_BMC_IPMB_LUN_BMC, 
		IPMI_NET_FN_STORAGE_RQ, 
		obj_cmd_rq, 
		obj_cmd_rs) < 0)
    goto cleanup;

  if (ipmi_comp_test (obj_cmd_rs) != 1)
    goto cleanup;

  if ((len = fiid_obj_field_len_bytes (obj_cmd_rs, (uint8_t *)"record_data")) < 0)
    goto cleanup;
  
  if (!(buf = (uint8_t *)malloc(len)))
    goto cleanup;

  if (fiid_obj_get_data(obj_cmd_rs,
			(uint8_t *)"record_data",
			buf,
			len) < 0)
    goto cleanup;

  if (fiid_obj_set_all(sensor_record_header,
		       buf,
		       len) < 0)
    goto cleanup;
   
  rv = 0;
 cleanup:
  if (obj_cmd_rq)
    fiid_obj_destroy(obj_cmd_rq);
  if (buf)
    free(buf);
  return (rv);
}

/* XXX duplicate functionality, should consolidate with above */
static int8_t 
ipmi_cmd_get_sdr_chunk2 (ipmi_device_t *dev, 
			 uint16_t reservation_id, 
			 uint16_t record_id, 
			 uint8_t record_offset, 
			 uint8_t bytes_read, 
			 fiid_obj_t obj_cmd_rs, 
			 uint8_t *sensor_record_chunk,
                         uint32_t sensor_record_chunk_len)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t ret, rv = -1;
  int32_t len;

  if (!dev 
      || !fiid_obj_valid(obj_cmd_rs)
      || !sensor_record_chunk)
    {
      errno = EINVAL;
      return -1;
    }
  
  if ((ret = fiid_obj_template_compare(obj_cmd_rs, tmpl_get_sdr_rs)) < 0)
    goto cleanup;

  if (!ret)
    {
      errno = EINVAL;
      goto cleanup;
    }

  if (!(obj_cmd_rq = fiid_obj_create(tmpl_get_sdr_rq)))
    goto cleanup;

  if (fill_kcs_get_sdr_chunk (obj_cmd_rq, 
			      reservation_id, 
			      record_id, 
			      record_offset, 
			      bytes_read) < 0)
    goto cleanup;

  if (ipmi_cmd (dev, 
		IPMI_BMC_IPMB_LUN_BMC, 
		IPMI_NET_FN_STORAGE_RQ, 
		obj_cmd_rq, 
		obj_cmd_rs) < 0)
    goto cleanup;
  
  if (ipmi_comp_test (obj_cmd_rs) != 1)
    goto cleanup;

  if (fiid_obj_get_data(obj_cmd_rs,
			(uint8_t *)"record_data",
			sensor_record_chunk,
			sensor_record_chunk_len) < 0)
      goto cleanup;
  
  rv = 0;
 cleanup:
  if (obj_cmd_rq)
    fiid_obj_destroy(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_get_sdr2 (ipmi_device_t *dev, 
		   uint16_t record_id, 
		   fiid_obj_t obj_cmd_rs, 
		   uint8_t *sensor_record,
		   uint32_t *sensor_record_len)
{
  uint64_t val = 0;
  
  uint8_t record_length = 0;
  uint16_t reservation_id = 0;
  uint8_t record_offset = 0;
  uint8_t bytes_read = 0;
  
  uint8_t chunk_data[16];
  
  uint8_t *record_data = NULL;

  int8_t ret, rv = -1;

  if (!dev 
      || !fiid_obj_valid(obj_cmd_rs)
      || !sensor_record
      || !sensor_record_len)
    {
      errno = EINVAL;
      return -1;
    }

  if ((ret = fiid_obj_template_compare(obj_cmd_rs, tmpl_get_sdr_rs)) < 0)
    goto cleanup;

  if (!ret)
    {
      errno = EINVAL;
      goto cleanup;
    }

  {
    fiid_obj_t sensor_record_header = NULL;
    int32_t hdr_len;

    if (!(sensor_record_header = fiid_obj_create(tmpl_sdr_sensor_record_header)))
      goto cleanup1;
    
    if ((hdr_len = fiid_template_len_bytes (tmpl_sdr_sensor_record_header)) < 0)
      goto cleanup1;

    if (ipmi_cmd_get_sensor_record_header2 (dev, 
					    record_id, 
					    obj_cmd_rs, 
					    sensor_record_header) < 0)
      goto cleanup1;

    if (fiid_obj_get (sensor_record_header,
		      (uint8_t *)"record_length", 
		      &val) < 0)
      goto cleanup1;
		      
    record_length = val;
    record_length += hdr_len;

    rv = 0;
  cleanup1:
    if (sensor_record_header)
      fiid_obj_destroy(sensor_record_header);
    if (rv < 0)
      return (rv);
  }
  
  /* achu: where does the 16 come from? */
  if (record_length > 16)
    {
      fiid_obj_t local_obj_cmd_rs = NULL;
      
      rv = -1;

      if (!(local_obj_cmd_rs = fiid_obj_create(tmpl_reserve_sdr_repo_rs)))
	goto cleanup2;
      
      if (ipmi_cmd_reserve_sdr_repo2 (dev, local_obj_cmd_rs) < 0)
	goto cleanup2;
      
      if (fiid_obj_get (local_obj_cmd_rs,  
			(uint8_t *)"reservation_id", 
			&val) < 0)
	goto cleanup2;

      reservation_id = (uint16_t) val;
      rv = 0;
    cleanup2:
      if (local_obj_cmd_rs)
	fiid_obj_destroy(local_obj_cmd_rs);
      if (rv < 0)
	return (rv);
    }
  
  rv = -1;
  record_data = alloca (record_length);
  memset (record_data, 0, record_length);
  
  for (record_offset = 0; record_offset < record_length; record_offset += 16)
    {
      bytes_read = 16;
      if ((record_offset + bytes_read) > record_length)
	bytes_read = record_length - record_offset;
      
      if (fiid_obj_clear(obj_cmd_rs) < 0)
	goto cleanup;

      if (ipmi_cmd_get_sdr_chunk2 (dev, 
				   reservation_id, 
				   record_id, 
				   record_offset, 
				   bytes_read, 
				   obj_cmd_rs, 
				   chunk_data,
				   16) < 0)
	goto cleanup;
      
      memcpy (record_data + record_offset, chunk_data, bytes_read);
    }
  
  if (*sensor_record_len < record_length)
    goto cleanup;

  memcpy(sensor_record, record_data, record_length);
  *sensor_record_len = record_length;
  
  rv = 0;
 cleanup:
  return (rv);
}

