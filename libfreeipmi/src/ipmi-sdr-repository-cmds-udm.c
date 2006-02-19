/* 
   ipmi-sdr-repository-cmds-udm.c - IPMI UDM SDR Repository commands

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

int8_t 
ipmi_cmd_get_sdr_repository_info2 (ipmi_device_t *dev, 
                                   fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;

  if (!dev || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_get_sdr_repository_info_rs);

  FIID_OBJ_CREATE(obj_cmd_rq, tmpl_get_sdr_repository_info_rq);

  ERR_CLEANUP (!(fill_cmd_get_repository_info (obj_cmd_rq) < 0));

  ERR_IPMI_CMD_CLEANUP (dev, 
			IPMI_BMC_IPMB_LUN_BMC, 
			IPMI_NET_FN_STORAGE_RQ, 
			obj_cmd_rq, 
			obj_cmd_rs);

  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_get_sdr_repository_allocation_info2 (ipmi_device_t *dev, 
                                              fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;

  if (!dev || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return -1;
    }
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_get_sdr_repository_allocation_info_rs);

  FIID_OBJ_CREATE(obj_cmd_rq, tmpl_get_sdr_repository_allocation_info_rq);

  ERR_CLEANUP (!(fill_cmd_get_repository_allocation_info (obj_cmd_rq) < 0));

  ERR_IPMI_CMD_CLEANUP (dev, 
			IPMI_BMC_IPMB_LUN_BMC, 
			IPMI_NET_FN_STORAGE_RQ, 
			obj_cmd_rq, 
			obj_cmd_rs);

  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_reserve_sdr_repository2 (ipmi_device_t *dev, 
                                  fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  if (!dev || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return -1;
    }
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_reserve_sdr_repository_rs);

  FIID_OBJ_CREATE(obj_cmd_rq, tmpl_reserve_sdr_repository_rq);

  ERR_CLEANUP (!(fill_cmd_reserve_sdr_repository (obj_cmd_rq) < 0));

  ERR_IPMI_CMD_CLEANUP (dev, 
			IPMI_BMC_IPMB_LUN_BMC, 
			IPMI_NET_FN_STORAGE_RQ, 
			obj_cmd_rq, 
			obj_cmd_rs);

  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_cmd_rq);
  return (rv);
}

static int8_t 
ipmi_cmd_get_sensor_record_header2 (ipmi_device_t *dev, 
				    uint16_t record_id, 
				    fiid_obj_t obj_cmd_rs, 
				    fiid_obj_t sensor_record_header)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  int32_t sensor_record_header_len;
  int32_t len;
  
  uint8_t *buf = NULL;

  if (!dev 
      || !fiid_obj_valid(obj_cmd_rs)
      || !sensor_record_header)
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_get_sdr_rs);

  FIID_OBJ_TEMPLATE_COMPARE(sensor_record_header, tmpl_sdr_sensor_record_header);

  FIID_OBJ_CREATE(obj_cmd_rq, tmpl_get_sdr_rq);

  FIID_TEMPLATE_LEN_BYTES_CLEANUP(sensor_record_header_len,
				  tmpl_sdr_sensor_record_header);

  ERR_CLEANUP (!(fill_cmd_get_sdr (0,
				   record_id, 
				   0,
				   sensor_record_header_len,
				   obj_cmd_rq) < 0));

  ERR_IPMI_CMD_CLEANUP (dev, 
			IPMI_BMC_IPMB_LUN_BMC, 
			IPMI_NET_FN_STORAGE_RQ, 
			obj_cmd_rq, 
			obj_cmd_rs);

  FIID_OBJ_FIELD_LEN_BYTES_CLEANUP (len, obj_cmd_rs, (uint8_t *)"record_data");
  
  if (!(buf = (uint8_t *)malloc(len)))
    goto cleanup;

  FIID_OBJ_GET_DATA_CLEANUP (obj_cmd_rs, (uint8_t *)"record_data", buf,	len);

  FIID_OBJ_SET_ALL_CLEANUP (sensor_record_header, buf, len);
   
  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_cmd_rq);
  if (buf)
    free(buf);
  return (rv);
}

/* XXX duplicate functionality, should consolidate with above */
static int8_t 
ipmi_cmd_get_sdr_chunk2 (ipmi_device_t *dev, 
			 uint16_t reservation_id, 
			 uint16_t record_id, 
			 uint8_t offset_into_record, 
			 uint8_t bytes_read, 
			 fiid_obj_t obj_cmd_rs, 
			 uint8_t *sensor_record_chunk,
                         uint32_t sensor_record_chunk_len)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;

  if (!dev 
      || !fiid_obj_valid(obj_cmd_rs)
      || !sensor_record_chunk)
    {
      errno = EINVAL;
      return -1;
    }
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_get_sdr_rs);

  FIID_OBJ_CREATE(obj_cmd_rq, tmpl_get_sdr_rq);

  ERR_CLEANUP (!(fill_cmd_get_sdr (reservation_id, 
				   record_id, 
				   offset_into_record, 
				   bytes_read,
				   obj_cmd_rq) < 0));

  ERR_IPMI_CMD_CLEANUP (dev, 
			IPMI_BMC_IPMB_LUN_BMC, 
			IPMI_NET_FN_STORAGE_RQ, 
			obj_cmd_rq, 
			obj_cmd_rs);
  
  FIID_OBJ_GET_DATA_CLEANUP (obj_cmd_rs,
			     (uint8_t *)"record_data",
			     sensor_record_chunk,
			     sensor_record_chunk_len);
  
  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_cmd_rq);
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
  uint8_t offset_into_record = 0;
  uint8_t bytes_read = 0; 
  uint8_t chunk_data[16];
  uint8_t *record_data = NULL;
  int8_t rv = -1;

  if (!dev 
      || !fiid_obj_valid(obj_cmd_rs)
      || !sensor_record
      || !sensor_record_len)
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_get_sdr_rs);

  {
    fiid_obj_t sensor_record_header = NULL;
    int32_t hdr_len;

    FIID_OBJ_CREATE_CLEANUP1(sensor_record_header, tmpl_sdr_sensor_record_header);
    
    FIID_TEMPLATE_LEN_BYTES_CLEANUP1 (hdr_len, tmpl_sdr_sensor_record_header);

    ERR_CLEANUP1 (!(ipmi_cmd_get_sensor_record_header2 (dev, 
							record_id, 
							obj_cmd_rs, 
							sensor_record_header) < 0));

    FIID_OBJ_GET_CLEANUP1 (sensor_record_header, (uint8_t *)"record_length", &val);
    record_length = val;
    record_length += hdr_len;

    rv = 0;
  cleanup1:
    FIID_OBJ_DESTROY_NO_RETURN(sensor_record_header);
    if (rv < 0)
      return (rv);
  }
  
  /* achu: where does the 16 come from? */
  if (record_length > 16)
    {
      fiid_obj_t local_obj_cmd_rs = NULL;
      
      rv = -1;

      FIID_OBJ_CREATE_CLEANUP2(local_obj_cmd_rs, tmpl_reserve_sdr_repository_rs);
      
      ERR_CLEANUP2 (!(ipmi_cmd_reserve_sdr_repository2 (dev, local_obj_cmd_rs) < 0));
      
      FIID_OBJ_GET_CLEANUP2 (local_obj_cmd_rs, (uint8_t *)"reservation_id", &val);
      reservation_id = (uint16_t) val;
      rv = 0;
    cleanup2:
      FIID_OBJ_DESTROY_NO_RETURN(local_obj_cmd_rs);
      if (rv < 0)
	return (rv);
    }
  
  rv = -1;
  record_data = alloca (record_length);
  memset (record_data, 0, record_length);
  
  for (offset_into_record = 0; offset_into_record < record_length; offset_into_record += 16)
    {
      bytes_read = 16;
      if ((offset_into_record + bytes_read) > record_length)
	bytes_read = record_length - offset_into_record;
      
      FIID_OBJ_CLEAR_CLEANUP (obj_cmd_rs);

      ERR_CLEANUP (!(ipmi_cmd_get_sdr_chunk2 (dev, 
					      reservation_id, 
					      record_id, 
					      offset_into_record, 
					      bytes_read, 
					      obj_cmd_rs, 
					      chunk_data,
					      16) < 0));
      
      memcpy (record_data + offset_into_record, chunk_data, bytes_read);
    }
  
  if (*sensor_record_len < record_length)
    goto cleanup;

  memcpy(sensor_record, record_data, record_length);
  *sensor_record_len = record_length;
  
  rv = 0;
 cleanup:
  return (rv);
}

