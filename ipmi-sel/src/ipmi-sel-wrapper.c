#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif	/* HAVE_UNISTD_H */
#include <error.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <sys/resource.h>
#if TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else /* !TIME_WITH_SYS_TIME */
#if HAVE_SYS_TIME_H
#include <sys/time.h>
#else /* !HAVE_SYS_TIME_H */
#include <time.h>
#endif /* !HAVE_SYS_TIME_H */
#endif /* !TIME_WITH_SYS_TIME */
#include <argp.h>

#include "freeipmi/fiid.h"
#include "freeipmi/ipmi-sel-cmds.h"
#include "freeipmi/ipmi-sel-record-types.h"
#include "freeipmi/ipmi-sensor-event-messages.h"
#include "freeipmi/udm/ipmi-sel-cmds-udm.h"

#include "err-wrappers.h"
#include "fiid-wrappers.h"
#include "freeipmi-portability.h"
#include "ipmi-common.h"
#include "ipmi-sensor-api.h"

#include <freeipmi/freeipmi.h>
#include <freeipmi/udm/udm.h>

#include "ipmi-sel-api.h"
#include "ipmi-sel-wrapper.h"

int 
get_local_sel_info (ipmi_device_t *dev, local_sel_info_t *sel_info)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  int rv = -1;
  
  ERR_EINVAL (dev && sel_info);

  FIID_OBJ_CREATE (obj_cmd_rs, tmpl_cmd_get_sel_info_rs);

  if (ipmi_cmd_get_sel_info (dev, obj_cmd_rs) != 0)
    {
      FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "cmd", &val);
      dev->cmd = val;

      FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "comp_code", &val);
      dev->comp_code = val;

      ipmi_strerror_cmd_r (obj_cmd_rs, 
                           dev->net_fn,
			   dev->errmsg, 
			   IPMI_ERR_STR_MAX_LEN);
      goto cleanup;
    }
  
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "sel_version_major", &val);
  sel_info->sel_version_major = val;
  
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "sel_version_minor", &val);
  sel_info->sel_version_minor = val;
  
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "entries", &val);
  sel_info->log_entry_count = val;
  
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "free_space", &val);
  sel_info->free_space = val;
  
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "most_recent_addition_timestamp", &val);
  sel_info->recent_addition_timestamp = val;
  
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "most_recent_erase_timestamp", &val);
  sel_info->recent_erase_timestamp = val;
  
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "get_sel_allocation_info_command_supported", &val);
  sel_info->get_sel_alloc_info_cmd_support = val;
  
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "reserve_sel_command_supported", &val);
  sel_info->reserve_sel_cmd_support = val;
  
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "partial_add_sel_entry_command_supported", &val);
  sel_info->partial_add_sel_entry_cmd_support = val;
  
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "delete_sel_command_supported", &val);
  sel_info->delete_sel_cmd_support = val;
  
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "overflow_flag", &val);
  sel_info->overflow_flag = val;
  
  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_cmd_rs);
  return (rv);
}

int 
get_local_sel_record (ipmi_device_t *dev, 
		      uint16_t record_id, 
		      sel_record_t *sel_rec, 
		      uint16_t *next_record_id)
{
  fiid_obj_t obj_cmd_rs;
  uint64_t val;
  int rv = -1;
  int32_t len;
  
  uint8_t record_data[SEL_RECORD_SIZE];
  uint32_t record_data_len = SEL_RECORD_SIZE;
  
  ERR_EINVAL (dev && sel_rec && next_record_id);
  
  FIID_OBJ_CREATE (obj_cmd_rs, tmpl_cmd_get_sel_entry_rs);
  
  if (ipmi_cmd_get_sel_entry (dev, 
			      0,
			      record_id, 
			      0,
			      IPMI_SEL_READ_ENTIRE_RECORD_BYTES_TO_READ,
			      obj_cmd_rs) != 0)
    {
      FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "cmd", &val);
      dev->cmd = val;
      
      FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "comp_code", &val);
      dev->comp_code = val;
      ipmi_strerror_cmd_r (obj_cmd_rs, 
                           dev->net_fn,
			   dev->errmsg, 
			   IPMI_ERR_STR_MAX_LEN);
      goto cleanup;
    }
  
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "next_record_id", &val);
  *next_record_id = val;
  
  FIID_OBJ_GET_DATA_LEN_CLEANUP (len,
				 obj_cmd_rs, 
				 "record_data", 
				 record_data,
				 record_data_len);
  record_data_len = len;
  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_cmd_rs);
  if (rv == 0)
    {
      return (get_sel_record (record_data, record_data_len, sel_rec));
    }
  else
    {
      return (rv);
    }
}

int 
get_local_sel_record_raw (ipmi_device_t *dev, 
			  uint16_t record_id, 
			  uint8_t *record_data, 
			  uint32_t record_data_len, 
			  uint16_t *next_record_id)
{
  fiid_obj_t obj_cmd_rs;
  uint64_t val;
  int rv = -1;
  int32_t len;
  
  ERR_EINVAL (dev && record_data && next_record_id);
  
  FIID_OBJ_CREATE (obj_cmd_rs, tmpl_cmd_get_sel_entry_rs);
  if (ipmi_cmd_get_sel_entry (dev, 
			      0,
			      record_id, 
			      0,
			      IPMI_SEL_READ_ENTIRE_RECORD_BYTES_TO_READ,
			      obj_cmd_rs) != 0)
    {
      FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "cmd", &val);
      dev->cmd = val;
      
      FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "comp_code", &val);
      dev->comp_code = val;
      ipmi_strerror_cmd_r (obj_cmd_rs, 
                           dev->net_fn,
			   dev->errmsg, 
			   IPMI_ERR_STR_MAX_LEN);
      goto cleanup;
    }
  
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "next_record_id", &val);
  *next_record_id = val;
  
  FIID_OBJ_GET_DATA_LEN_CLEANUP (len,
				 obj_cmd_rs, 
				 "record_data", 
				 record_data,
				 record_data_len);
  record_data_len = len;
  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_cmd_rs);
  return (rv);
}

int 
delete_local_sel_entry (ipmi_device_t *dev, uint16_t record_id)
{
  fiid_obj_t obj_cmd_rs;
  uint16_t reservation_id;
  uint64_t val;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_reserve_sel_rs)))
    goto cleanup;
  
  if (ipmi_cmd_reserve_sel (dev, obj_cmd_rs) != 0)
    {
      FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "cmd", &val);
      dev->cmd = val;
      
      FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "comp_code", &val);
      dev->comp_code = val;
      ipmi_strerror_cmd_r (obj_cmd_rs, 
                           dev->net_fn,
			   dev->errmsg, 
			   IPMI_ERR_STR_MAX_LEN);
      goto cleanup;
    }
  
  if (fiid_obj_get (obj_cmd_rs, "reservation_id", &val) < 0)
    goto cleanup;
  reservation_id = val;
  
  fiid_obj_destroy(obj_cmd_rs);
  obj_cmd_rs = NULL;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_delete_sel_entry_rs)))
    goto cleanup;
  
  if (ipmi_cmd_delete_sel_entry (dev, 
				 reservation_id, 
				 record_id, 
				 obj_cmd_rs) != 0)
    {
      FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "cmd", &val);
      dev->cmd = val;
      
      FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "comp_code", &val);
      dev->comp_code = val;
      ipmi_strerror_cmd_r (obj_cmd_rs, 
                           dev->net_fn,
			   dev->errmsg, 
			   IPMI_ERR_STR_MAX_LEN);
      goto cleanup;
    }
  
  fiid_obj_destroy(obj_cmd_rs);
  return 0;
  
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (-1);
}

int 
clear_sel_entries (ipmi_device_t *dev)
{
  fiid_obj_t obj_cmd_rs;
  uint16_t reservation_id;
  uint64_t val;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_reserve_sel_rs)))
    goto cleanup;

  if (ipmi_cmd_reserve_sel (dev, obj_cmd_rs) != 0)
    {
      FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "cmd", &val);
      dev->cmd = val;
      
      FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "comp_code", &val);
      dev->comp_code = val;
      ipmi_strerror_cmd_r (obj_cmd_rs, 
                           dev->net_fn,
			   dev->errmsg, 
			   IPMI_ERR_STR_MAX_LEN);
      goto cleanup;
    }
  
  if (fiid_obj_get (obj_cmd_rs, "reservation_id", &val) < 0)
    goto cleanup;
  reservation_id = val;
  
  fiid_obj_destroy(obj_cmd_rs);
  obj_cmd_rs = NULL;

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_clear_sel_rs)))
    goto cleanup;

  if (ipmi_cmd_clear_sel (dev, 
			  reservation_id, 
			  IPMI_SEL_CLEAR_OPERATION_INITIATE_ERASE, 
			  obj_cmd_rs) != 0)
    {
      FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "cmd", &val);
      dev->cmd = val;
      
      FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "comp_code", &val);
      dev->comp_code = val;
      ipmi_strerror_cmd_r (obj_cmd_rs, 
                           dev->net_fn,
			   dev->errmsg, 
			   IPMI_ERR_STR_MAX_LEN);
      goto cleanup;
    }
  
  return 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (-1);
}

int 
get_sel_clear_status (ipmi_device_t *dev, int *status)
{
  fiid_obj_t obj_cmd_rs;
  uint16_t reservation_id;
  uint64_t val;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_reserve_sel_rs)))
    goto cleanup;

  if (ipmi_cmd_reserve_sel (dev, obj_cmd_rs) != 0)
    {
      FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "cmd", &val);
      dev->cmd = val;
      
      FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "comp_code", &val);
      dev->comp_code = val;
      ipmi_strerror_cmd_r (obj_cmd_rs, 
                           dev->net_fn,
			   dev->errmsg, 
			   IPMI_ERR_STR_MAX_LEN);
      goto cleanup;
    }
  
  if (fiid_obj_get (obj_cmd_rs, "reservation_id", &val) < 0)
    goto cleanup;
  reservation_id = val;
  
  fiid_obj_destroy(obj_cmd_rs);
  obj_cmd_rs = NULL;

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_clear_sel_rs)))
    goto cleanup;

  if (ipmi_cmd_clear_sel (dev, 
			  reservation_id, 
			  IPMI_SEL_CLEAR_OPERATION_GET_ERASURE_STATUS, 
			  obj_cmd_rs) != 0)
    {
      FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "cmd", &val);
      dev->cmd = val;
      
      FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "comp_code", &val);
      dev->comp_code = val;
      ipmi_strerror_cmd_r (obj_cmd_rs, 
                           dev->net_fn,
			   dev->errmsg, 
			   IPMI_ERR_STR_MAX_LEN);
      goto cleanup;
    }
  
  if (fiid_obj_get (obj_cmd_rs, "erasure_progress", &val) < 0)
    goto cleanup;
  
  fiid_obj_destroy(obj_cmd_rs);
  *status = val;
  return 0;

 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (-1);
}
