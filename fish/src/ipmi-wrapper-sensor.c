#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "freeipmi.h"
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <sys/mman.h>

#ifdef STDC_HEADERS
#include <string.h>
#endif

#include <errno.h>

#include "ipmi-wrapper.h"
#include "fish.h"

sdr_repo_cache_t sdr_repo_cache;

int sensors_errno = 0;

void 
clear_sensors_errno ()
{
  sensors_errno = 0;
}

int 
get_sensors_errno ()
{
  return sensors_errno;
}

void 
set_sensors_errno (int eno)
{
  sensors_errno = eno;
}

sdr_repo_cache_t *
get_sdr_repo_cache ()
{
  return &sdr_repo_cache;
}

void 
display_current_threshold_sensor_full_record (sdr_repo_cache_t *sdr_repo_cache)
{
  u_int64_t val;
  int i;
  
  /* requires to decode values */
  short b;
  short m;
  char r_exponent;
  char b_exponent;
  u_int8_t linear;
  u_int8_t is_signed;
  
  u_int8_t base_unit;
  
  u_int8_t sensor_number;
  u_int8_t sensor_type;
  u_int8_t event_reading_type_code;
  double normal_min;
  double normal_max;
  
  u_int8_t data_rs[] = {0, 0, 0, 0, 0, 0};
  u_int8_t status;
  
  u_int8_t record_length;
  
  double sensor_reading = 0.0;
  
  ipmi_sensor_get_decode_params (sdr_repo_cache->cache_curr, 
				 &is_signed, &r_exponent, &b_exponent, 
				 &linear, &b, &m);
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record, 
		"record_length", 
		&val);
  record_length = val;
  record_length += fiid_obj_len_bytes (tmpl_sdr_sensor_record_header);
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record, 
		"sensor_base_unit", 
		&val);
  base_unit = val;
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record, 
		"event_reading_type", 
		&val);
  event_reading_type_code = val;
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record, 
		"sensor_number", 
		&val);
  sensor_number = val;
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record, 
		"sensor_type", 
		&val);
  sensor_type = val;
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record,
		"normal_min",
		&val);
  normal_min = ipmi_sensor_decode_value (r_exponent,
					 b_exponent,
					 m,
					 b,
					 linear,
					 is_signed,
					 val);
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record,
		"normal_max",
		&val);
  normal_max = ipmi_sensor_decode_value (r_exponent,
					 b_exponent,
					 m,
					 b,
					 linear,
					 is_signed,
					 val);
  
  printf ("%d: ", sdr_repo_cache->cache_curr_rec_id);
  for (i = 48; i < record_length; i++)
    printf ("%c", sdr_repo_cache->cache_curr[i]);
  printf (" (%s): ", ipmi_get_sensor_group (sensor_type));
  
  status = ipmi_kcs_get_threshold_reading (fi_get_sms_io_base (), 
					   sensor_number, 
					   data_rs);
  
  if (IPMI_COMP_CODE(data_rs) != IPMI_COMMAND_SUCCESS)
    {
      /*       char err_msg[IPMI_ERR_STR_MAX_LEN]; */
      /*       ipmi_strerror_cmd_r (data_rs, err_msg, IPMI_ERR_STR_MAX_LEN); */
      status = -1;
    }
  
  if (status == 0)
    {
      fiid_obj_get (data_rs, 
		    tmpl_get_sensor_threshold_reading_rs, 
		    "sensor_reading", 
		    &val);
      sensor_reading = ipmi_sensor_decode_value (r_exponent, 
						 b_exponent, 
						 m, 
						 b, 
						 linear, 
						 is_signed, 
						 (u_int8_t) val);
      printf ("%.2f %s ", sensor_reading, 
	      ipmi_sensor_units_short[base_unit]);
    }
  else 
    printf ("N/A ");
  
  printf ("(low=%.2f/", normal_min);
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record, 
		"nominal_reading", 
		&val);
  printf ("nom=%.2f/", 
	  ipmi_sensor_decode_value (r_exponent, 
				    b_exponent, 
				    m, 
				    b, 
				    linear, 
				    is_signed, 
				    val));
  
  printf ("high=%.2f) ", normal_max);
  
  if (status == 0)
    {
      if (ipmi_sensor_threshold_health_check (sensor_reading, 
					      normal_min, 
					      normal_max, 
					      data_rs))
	{
	  set_sensors_errno (0);
	  printf ("[OK]\n");
	}
      else 
	{
	  set_sensors_errno (1);
	  printf ("[ALERT]\n");
	}
    }
  else 
    {
      set_sensors_errno (-1);
      printf ("[FAILED]\n");
    }
  
}

void 
display_current_generic_discrete_sensor_full_record (sdr_repo_cache_t *sdr_repo_cache)
{
  u_int64_t val;
  int i;
  
  /* requires to decode values */
  short b;
  short m;
  char r_exponent;
  char b_exponent;
  u_int8_t linear;
  u_int8_t is_signed;
  
  u_int8_t base_unit;
  
  u_int8_t sensor_number;
  u_int8_t sensor_type;
  u_int8_t event_reading_type_code;
  double normal_min;
  double normal_max;
  
  u_int8_t data_rs[] = {0, 0, 0, 0, 0, 0};
  u_int8_t status;
  
  u_int8_t record_length;
  
  char *status_message = NULL;
  
  ipmi_sensor_get_decode_params (sdr_repo_cache->cache_curr, 
				 &is_signed, &r_exponent, &b_exponent, 
				 &linear, &b, &m);
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record, 
		"record_length", 
		&val);
  record_length = val;
  record_length += fiid_obj_len_bytes (tmpl_sdr_sensor_record_header);
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record, 
		"sensor_base_unit", 
		&val);
  base_unit = val;
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record, 
		"event_reading_type", 
		&val);
  event_reading_type_code = val;
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record, 
		"sensor_number", 
		&val);
  sensor_number = val;
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record, 
		"sensor_type", 
		&val);
  sensor_type = val;
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record,
		"normal_min",
		&val);
  normal_min = ipmi_sensor_decode_value (r_exponent,
					 b_exponent,
					 m,
					 b,
					 linear,
					 is_signed,
					 val);
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record,
		"normal_max",
		&val);
  normal_max = ipmi_sensor_decode_value (r_exponent,
					 b_exponent,
					 m,
					 b,
					 linear,
					 is_signed,
					 val);
  
  printf ("%d: ", sdr_repo_cache->cache_curr_rec_id);
  for (i = 48; i < record_length; i++)
    printf ("%c", sdr_repo_cache->cache_curr[i]);
  printf (" (%s): ", ipmi_get_sensor_group (sensor_type));
  
  status = ipmi_kcs_get_discrete_reading (fi_get_sms_io_base (), 
					  sensor_number, 
					  data_rs);
  
  if (IPMI_COMP_CODE(data_rs) != IPMI_COMMAND_SUCCESS)
    {
      /*       char err_msg[IPMI_ERR_STR_MAX_LEN]; */
      /*       ipmi_strerror_cmd_r (data_rs, err_msg, IPMI_ERR_STR_MAX_LEN); */
      status = -1;
    }
  
  printf ("N/A ");
  
  printf ("(low=%.2f/", normal_min);
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record, 
		"nominal_reading", 
		&val);
  printf ("nom=%.2f/", 
	  ipmi_sensor_decode_value (r_exponent, 
				    b_exponent, 
				    m, 
				    b, 
				    linear, 
				    is_signed, 
				    val));
  
  printf ("high=%.2f) ", normal_max);
  
  if (status == 0)
    {
      char key[65];
      
      for (i = 0; 
	   ipmi_event_reading_type_code_desc_ptr[event_reading_type_code][i] != NULL; 
	   i++)
	{
	  if (strcasecmp (ipmi_event_reading_type_code_desc_ptr[event_reading_type_code][i], 
			  "reserved") == 0)
	    continue;
	  
	  snprintf (key, 64, "state_%d_asserted", i);
	  fiid_obj_get (data_rs, 
			tmpl_get_sensor_discrete_reading_rs, 
			key, 
			&val);
	  
	  if (val == 1)
	    {
	      status_message = (char *)ipmi_event_reading_type_code_desc_ptr[event_reading_type_code][i];
	      break;
	    }
	}
      if (status_message == NULL)
	status_message = strdupa ("OK");
    }
  else 
    status_message = strdupa ("FAILED");
  
  printf ("[%s]\n", status_message);
}

void 
display_current_generic_discrete_sensor_compact_record (sdr_repo_cache_t *sdr_repo_cache)
{
  u_int64_t val;
  int i;
  
  u_int8_t base_unit;
  
  u_int8_t sensor_number;
  u_int8_t sensor_type;
  u_int8_t event_reading_type_code;
  
  u_int8_t data_rs[] = {0, 0, 0, 0, 0, 0};
  u_int8_t status;
  
  u_int8_t record_length;
  
  char *status_message = NULL;
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record, 
		"record_length", 
		&val);
  record_length = val;
  record_length += fiid_obj_len_bytes (tmpl_sdr_sensor_record_header);
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_compact_sensor_record, 
		"sensor_base_unit", 
		&val);
  base_unit = val;
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record, 
		"event_reading_type", 
		&val);
  event_reading_type_code = val;
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_compact_sensor_record, 
		"sensor_number", 
		&val);
  sensor_number = val;
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_compact_sensor_record, 
		"sensor_type", 
		&val);
  sensor_type = val;
  
  printf ("%d: ", sdr_repo_cache->cache_curr_rec_id);
  for (i = 32; i < record_length; i++)
    printf ("%c", sdr_repo_cache->cache_curr[i]);
  printf (" (%s): ", ipmi_get_sensor_group (sensor_type));
  
  status = ipmi_kcs_get_discrete_reading (fi_get_sms_io_base (), 
					  sensor_number, 
					  data_rs);
  
  if (IPMI_COMP_CODE(data_rs) != IPMI_COMMAND_SUCCESS)
    {
      /*       char err_msg[IPMI_ERR_STR_MAX_LEN]; */
      /*       ipmi_strerror_cmd_r (data_rs, err_msg, IPMI_ERR_STR_MAX_LEN); */
      status = -1;
    }
  
  if (status == 0)
    {
      char key[65];
      
      for (i = 0; 
	   ipmi_event_reading_type_code_desc_ptr[event_reading_type_code][i] != NULL; 
	   i++)
	{
	  if (strcasecmp (ipmi_event_reading_type_code_desc_ptr[event_reading_type_code][i], 
			  "reserved") == 0)
	    continue;
	  
	  snprintf (key, 64, "state_%d_asserted", i);
	  fiid_obj_get (data_rs, 
			tmpl_get_sensor_discrete_reading_rs, 
			key, 
			&val);
	  
	  if (val == 1)
	    {
	      status_message = (char *) ipmi_event_reading_type_code_desc_ptr[event_reading_type_code][i];
	      break;
	    }
	}
      if (status_message == NULL)
	status_message = strdupa ("OK");
    }
  else 
    {
      set_sensors_errno (-1);
      status_message = strdupa ("FAILED");
    }
  
  printf ("[%s]\n", status_message);
}

void 
display_current_discrete_sensor_full_record (sdr_repo_cache_t *sdr_repo_cache)
{
  u_int64_t val;
  int i;
  
  /* requires to decode values */
  short b;
  short m;
  char r_exponent;
  char b_exponent;
  u_int8_t linear;
  u_int8_t is_signed;
  
  u_int8_t base_unit;
  
  u_int8_t sensor_number;
  u_int8_t sensor_type;
  u_int8_t event_reading_type_code;
  
  u_int8_t data_rs[] = {0, 0, 0, 0, 0, 0};
  u_int8_t status;
  
  u_int8_t record_length;
  
  ipmi_sensor_get_decode_params (sdr_repo_cache->cache_curr, 
				 &is_signed, &r_exponent, &b_exponent, 
				 &linear, &b, &m);
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record, 
		"record_length", 
		&val);
  record_length = val;
  record_length += fiid_obj_len_bytes (tmpl_sdr_sensor_record_header);
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record, 
		"sensor_base_unit", 
		&val);
  base_unit = val;
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record, 
		"event_reading_type", 
		&val);
  event_reading_type_code = val;
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record, 
		"sensor_number", 
		&val);
  sensor_number = val;
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record, 
		"sensor_type", 
		&val);
  sensor_type = val;
  
  printf ("%d: ", sdr_repo_cache->cache_curr_rec_id);
  for (i = 48; i < record_length; i++)
    printf ("%c", sdr_repo_cache->cache_curr[i]);
  printf (" (%s): ", ipmi_get_sensor_group (sensor_type));
  
  status = ipmi_kcs_get_discrete_reading (fi_get_sms_io_base (), 
					  sensor_number, 
					  data_rs);
  
  if (IPMI_COMP_CODE(data_rs) != IPMI_COMMAND_SUCCESS)
    {
      /*       char err_msg[IPMI_ERR_STR_MAX_LEN]; */
      /*       ipmi_strerror_cmd_r (data_rs, err_msg, IPMI_ERR_STR_MAX_LEN); */
      status = -1;
    }
  
  printf ("N/A ");
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record,
		"normal_min",
		&val);
  printf ("(low=%.2f/",
	  ipmi_sensor_decode_value (r_exponent,
				    b_exponent,
				    m,
				    b,
				    linear,
				    is_signed,
				    val));
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record, 
		"nominal_reading", 
		&val);
  printf ("nom=%.2f/", 
	  ipmi_sensor_decode_value (r_exponent, 
				    b_exponent, 
				    m, 
				    b, 
				    linear, 
				    is_signed, 
				    val));
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record,
		"normal_max",
		&val);
  printf ("high=%.2f) ",
	  ipmi_sensor_decode_value (r_exponent,
				    b_exponent,
				    m,
				    b,
				    linear,
				    is_signed,
				    val));
  
  if (status == 0)
    {
      if (ipmi_sensor_discrete_health_check (sensor_type, data_rs))
	{
	  set_sensors_errno (0);
	  printf ("[OK]\n");
	}
      else 
	{
	  set_sensors_errno (1);
	  printf ("[ALERT]\n");
	}
    }
  else 
    {
      set_sensors_errno (-1);
      printf ("[FAILED]\n");
    }
}

void 
display_current_discrete_sensor_compact_record (sdr_repo_cache_t *sdr_repo_cache)
{
  u_int64_t val;
  int i;
  
  u_int8_t base_unit;
  
  u_int8_t sensor_number;
  u_int8_t sensor_type;
  u_int8_t event_reading_type_code;
  
  u_int8_t data_rs[] = {0, 0, 0, 0, 0, 0};
  u_int8_t status;
  
  u_int8_t record_length;
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record, 
		"record_length", 
		&val);
  record_length = val;
  record_length += fiid_obj_len_bytes (tmpl_sdr_sensor_record_header);
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_compact_sensor_record, 
		"sensor_base_unit", 
		&val);
  base_unit = val;
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record, 
		"event_reading_type", 
		&val);
  event_reading_type_code = val;
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_compact_sensor_record, 
		"sensor_number", 
		&val);
  sensor_number = val;
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_compact_sensor_record, 
		"sensor_type", 
		&val);
  sensor_type = val;
  
  printf ("%d: ", sdr_repo_cache->cache_curr_rec_id);
  for (i = 32; i < record_length; i++)
    printf ("%c", sdr_repo_cache->cache_curr[i]);
  printf (" (%s): ", ipmi_get_sensor_group (sensor_type));
  
  status = ipmi_kcs_get_discrete_reading (fi_get_sms_io_base (), 
					  sensor_number, 
					  data_rs);
  
  if (IPMI_COMP_CODE(data_rs) != IPMI_COMMAND_SUCCESS)
    {
      /*       char err_msg[IPMI_ERR_STR_MAX_LEN]; */
      /*       ipmi_strerror_cmd_r (data_rs, err_msg, IPMI_ERR_STR_MAX_LEN); */
      status = -1;
    }
  
  if (status == 0)
    {
      if (ipmi_sensor_discrete_health_check (sensor_type, data_rs))
	{
	  set_sensors_errno (0);
	  printf ("[OK]\n");
	}
      else 
	{
	  set_sensors_errno (1);
	  printf ("[ALERT]\n");
	}
    }
  else 
    {
      set_sensors_errno (-1);
      printf ("[FAILED]\n");
    }
}

void 
display_current_oem_sensor_full_record (sdr_repo_cache_t *sdr_repo_cache)
{
  printf ("OEM sensor class full record type: not implemented\n");
}

void 
display_current_oem_sensor_compact_record (sdr_repo_cache_t *sdr_repo_cache)
{
  printf ("OEM sensor class compact record type: not implemented\n");
}

u_int8_t  
display_current_sensor (sdr_repo_cache_t *sdr_repo_cache)
{
  u_int64_t val;
  
  if (!ipmi_is_sensor_reading_available (sdr_repo_cache))
    return false;
  
  switch (ipmi_sdr_repo_cache_sensor_classify (sdr_repo_cache))
    {
    case IPMI_SENSOR_CLASS_THRESHOLD:
      fiid_obj_get (sdr_repo_cache->cache_curr, 
		    tmpl_sdr_sensor_record_header, 
		    "record_type", 
		    &val);
      
      switch (val)
	{
	case IPMI_SDR_FORMAT_FULL_RECORD:
	  display_current_threshold_sensor_full_record (sdr_repo_cache);
	  fflush (stdout);
	  break;
	      
	case IPMI_SDR_FORMAT_COMPACT_RECORD:
	case IPMI_SDR_FORMAT_EVENT_ONLY_RECORD:
	case IPMI_SDR_FORMAT_ENTITY_ASSO_RECORD:
	case IPMI_SDR_FORMAT_DEV_ENTITY_ASSO_RECORD:
	case IPMI_SDR_FORMAT_GEN_DEV_LOCATOR_RECORD:
	case IPMI_SDR_FORMAT_FRU_DEV_LOCATOR_RECORD:
	case IPMI_SDR_FORMAT_MGMT_CNTRLR_DEV_LOCATOR_RECORD:
	case IPMI_SDR_FORMAT_MGMT_CNTRLR_CONFIRMATION_RECORD:
	case IPMI_SDR_FORMAT_BMC_MSG_CHANNEL_INFO_RECORD:
	case IPMI_SDR_FORMAT_OEM_RECORD:
	  {
	    char errstr[IPMI_ERR_STR_MAX_LEN];
	    snprintf (errstr, IPMI_ERR_STR_MAX_LEN, 
		      "BUG: %s: %d: %s: record-id %d: "
		      "sensor class threshold record type [%02X] "
		      "is not a full record\n", 
		      __FILE__, __LINE__, __PRETTY_FUNCTION__, 
		      sdr_repo_cache->cache_curr_rec_id, (u_int8_t) val);
		
	    syslog (LOG_MAKEPRI(LOG_LOCAL1, LOG_ERR), errstr);
	    printf ("__DEBUG__: %s", errstr);
	  }
	  break;
	      
	default:
	  break;
	}
      break;
	  
    case IPMI_SENSOR_CLASS_GENERIC_DISCRETE:
      fiid_obj_get (sdr_repo_cache->cache_curr, 
		    tmpl_sdr_sensor_record_header, 
		    "record_type", 
		    &val);
	  
      switch (val)
	{
	case IPMI_SDR_FORMAT_FULL_RECORD:
	  display_current_generic_discrete_sensor_full_record (sdr_repo_cache);
	  fflush (stdout);
	  break;
	      
	case IPMI_SDR_FORMAT_COMPACT_RECORD:
	  display_current_generic_discrete_sensor_compact_record (sdr_repo_cache);
	  fflush (stdout);
	  break;
	      
	case IPMI_SDR_FORMAT_EVENT_ONLY_RECORD:
	case IPMI_SDR_FORMAT_ENTITY_ASSO_RECORD:
	case IPMI_SDR_FORMAT_DEV_ENTITY_ASSO_RECORD:
	case IPMI_SDR_FORMAT_GEN_DEV_LOCATOR_RECORD:
	case IPMI_SDR_FORMAT_FRU_DEV_LOCATOR_RECORD:
	case IPMI_SDR_FORMAT_MGMT_CNTRLR_DEV_LOCATOR_RECORD:
	case IPMI_SDR_FORMAT_MGMT_CNTRLR_CONFIRMATION_RECORD:
	case IPMI_SDR_FORMAT_BMC_MSG_CHANNEL_INFO_RECORD:
	case IPMI_SDR_FORMAT_OEM_RECORD:
	  break;
	      
	default:
	  break;
	}
      break;
	  
    case IPMI_SENSOR_CLASS_SENSOR_SPECIFIC_DISCRETE:
      fiid_obj_get (sdr_repo_cache->cache_curr, 
		    tmpl_sdr_sensor_record_header, 
		    "record_type", 
		    &val);
	  
      switch (val)
	{
	case IPMI_SDR_FORMAT_FULL_RECORD:
	  display_current_discrete_sensor_full_record (sdr_repo_cache);
	  fflush (stdout);
	  break;
		
	case IPMI_SDR_FORMAT_COMPACT_RECORD:
	  display_current_discrete_sensor_compact_record (sdr_repo_cache);
	  fflush (stdout);
	  break;
	      
	case IPMI_SDR_FORMAT_EVENT_ONLY_RECORD:
	case IPMI_SDR_FORMAT_ENTITY_ASSO_RECORD:
	case IPMI_SDR_FORMAT_DEV_ENTITY_ASSO_RECORD:
	case IPMI_SDR_FORMAT_GEN_DEV_LOCATOR_RECORD:
	case IPMI_SDR_FORMAT_FRU_DEV_LOCATOR_RECORD:
	case IPMI_SDR_FORMAT_MGMT_CNTRLR_DEV_LOCATOR_RECORD:
	case IPMI_SDR_FORMAT_MGMT_CNTRLR_CONFIRMATION_RECORD:
	case IPMI_SDR_FORMAT_BMC_MSG_CHANNEL_INFO_RECORD:
	case IPMI_SDR_FORMAT_OEM_RECORD:
	  break;
	      
	default:
	  break;
	}
      break;
	  
    case IPMI_SENSOR_CLASS_OEM:
      fiid_obj_get (sdr_repo_cache->cache_curr, 
		    tmpl_sdr_sensor_record_header, 
		    "record_type", 
		    &val);
	  
      switch (val)
	{
	case IPMI_SDR_FORMAT_FULL_RECORD:
	  display_current_oem_sensor_full_record (sdr_repo_cache);
	  fflush (stdout);
	  break;
		
	case IPMI_SDR_FORMAT_COMPACT_RECORD:
	  display_current_oem_sensor_compact_record (sdr_repo_cache);
	  fflush (stdout);
	  break;
	      
	case IPMI_SDR_FORMAT_EVENT_ONLY_RECORD:
	case IPMI_SDR_FORMAT_ENTITY_ASSO_RECORD:
	case IPMI_SDR_FORMAT_DEV_ENTITY_ASSO_RECORD:
	case IPMI_SDR_FORMAT_GEN_DEV_LOCATOR_RECORD:
	case IPMI_SDR_FORMAT_FRU_DEV_LOCATOR_RECORD:
	case IPMI_SDR_FORMAT_MGMT_CNTRLR_DEV_LOCATOR_RECORD:
	case IPMI_SDR_FORMAT_MGMT_CNTRLR_CONFIRMATION_RECORD:
	case IPMI_SDR_FORMAT_BMC_MSG_CHANNEL_INFO_RECORD:
	case IPMI_SDR_FORMAT_OEM_RECORD:
	  break;
	      
	default:
	  break;
	}
      break;
	  
    default:
      /* unknown sensor type */
      break;
    }
  return true;
}

int 
get_sdr_total_records (sdr_repo_cache_t *sdr_repo_cache)
{
  return (sdr_repo_cache->total_records);
}

void 
display_verbose_current_threshold_sensor_full_record (sdr_repo_cache_t *sdr_repo_cache)
{
  u_int64_t val;
  int i;
  
  /* requires to decode values */
  short b;
  short m;
  char r_exponent;
  char b_exponent;
  u_int8_t linear;
  u_int8_t is_signed;
  
  u_int8_t base_unit;
  
  u_int8_t sensor_number;
  u_int8_t sensor_type;
  u_int8_t event_reading_type_code;
  double normal_min;
  double normal_max;
  
  u_int8_t data_rs[] = {0, 0, 0, 0, 0, 0};
  u_int8_t status;
  
  u_int8_t record_length;
  
  double sensor_reading;
  
  u_int8_t alert_flag = 0;
  
  ipmi_sensor_get_decode_params (sdr_repo_cache->cache_curr, 
				 &is_signed, &r_exponent, &b_exponent, 
				 &linear, &b, &m);
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record, 
		"record_length", 
		&val);
  record_length = val;
  record_length += fiid_obj_len_bytes (tmpl_sdr_sensor_record_header);
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record, 
		"sensor_base_unit", 
		&val);
  base_unit = val;
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record, 
		"event_reading_type", 
		&val);
  event_reading_type_code = val;
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record, 
		"sensor_number", 
		&val);
  sensor_number = val;
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record, 
		"sensor_type", 
		&val);
  sensor_type = val;
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record,
		"normal_min",
		&val);
  normal_min = ipmi_sensor_decode_value (r_exponent,
					 b_exponent,
					 m,
					 b,
					 linear,
					 is_signed,
					 val);
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record,
		"normal_max",
		&val);
  normal_max = ipmi_sensor_decode_value (r_exponent,
					 b_exponent,
					 m,
					 b,
					 linear,
					 is_signed,
					 val);
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record,
		"record_id",
		&val);
  printf ("Record ID: %d\n", (u_int16_t) val);
  
  printf ("Sensor type: ");
  for (i = 48; i < record_length; i++)
    printf ("%c", sdr_repo_cache->cache_curr[i]);
  printf (" (%s)\n", ipmi_get_sensor_group (sensor_type));
  
  printf ("Sensor number: #%d\n", sensor_number);
  
  printf ("Event/Reading type code: %02Xh\n", event_reading_type_code);
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record,
		"lower_non_recoverable_threshold",
		&val);
  printf ("Lower non-recoverable threshold: %.2f %s\n",
	  ipmi_sensor_decode_value (r_exponent,
				    b_exponent,
				    m,
				    b,
				    linear,
				    is_signed,
				    val), 
	  ipmi_sensor_units[base_unit]);
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record,
		"upper_non_recoverable_threshold",
		&val);
  printf ("Upper non-recoverable threshold: %.2f %s\n",
	  ipmi_sensor_decode_value (r_exponent,
				    b_exponent,
				    m,
				    b,
				    linear,
				    is_signed,
				    val), 
	  ipmi_sensor_units[base_unit]);
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record,
		"lower_critical_threshold",
		&val);
  printf ("Lower Critical threshold: %.2f %s\n",
	  ipmi_sensor_decode_value (r_exponent,
				    b_exponent,
				    m,
				    b,
				    linear,
				    is_signed,
				    val),
	  ipmi_sensor_units[base_unit]);
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record,
		"upper_critical_threshold",
		&val);
  printf ("Upper Critical threshold: %.2f %s\n",
	  ipmi_sensor_decode_value (r_exponent,
				    b_exponent,
				    m,
				    b,
				    linear,
				    is_signed,
				    val),
	  ipmi_sensor_units[base_unit]);
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record,
		"lower_non_critical_threshold",
		&val);
  printf ("Lower non-critical threshold: %.2f %s\n",
	  ipmi_sensor_decode_value (r_exponent,
				    b_exponent,
				    m,
				    b,
				    linear,
				    is_signed,
				    val),
	  ipmi_sensor_units[base_unit]);
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record,
		"upper_non_critical_threshold",
		&val);
  printf ("Upper non-critical threshold: %.2f %s\n",
	  ipmi_sensor_decode_value (r_exponent,
				    b_exponent,
				    m,
				    b,
				    linear,
				    is_signed,
				    val),
	  ipmi_sensor_units[base_unit]);
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record,
		"sensor_min_reading",
		&val);
  printf ("Sensor min. reading: %.2f %s\n",
	  ipmi_sensor_decode_value (r_exponent,
				    b_exponent,
				    m,
				    b,
				    linear,
				    is_signed,
				    val),
	  ipmi_sensor_units[base_unit]);
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record,
		"sensor_max_reading",
		&val);
  printf ("Sensor max. reading: %.2f %s\n",
	  ipmi_sensor_decode_value (r_exponent,
				    b_exponent,
				    m,
				    b,
				    linear,
				    is_signed,
				    val),
	  ipmi_sensor_units[base_unit]);
  
  printf ("Normal min.: %.2f %s\n", 
	  normal_min, 
	  ipmi_sensor_units[base_unit]);
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record, 
		"nominal_reading", 
		&val);
  printf ("Nominal reading: %.2f %s\n",
	  ipmi_sensor_decode_value (r_exponent,
				    b_exponent,
				    m,
				    b,
				    linear,
				    is_signed,
				    val), 
	  ipmi_sensor_units[base_unit]);
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record,
		"normal_max",
		&val);
  printf ("Normal max: %.2f %s\n",
	  normal_max, 
	  ipmi_sensor_units[base_unit]);
  
  status = ipmi_kcs_get_threshold_reading (fi_get_sms_io_base (), 
					   sensor_number, 
					   data_rs);
  
  if (IPMI_COMP_CODE(data_rs) != IPMI_COMMAND_SUCCESS)
    {
      /*       char err_msg[IPMI_ERR_STR_MAX_LEN]; */
      /*       ipmi_strerror_cmd_r (data_rs, err_msg, IPMI_ERR_STR_MAX_LEN); */
      status = -1;
    }
  
  if (status == 0)
    {
      fiid_obj_get (data_rs, 
		    tmpl_get_sensor_threshold_reading_rs, 
		    "sensor_reading", 
		    &val);
      sensor_reading = ipmi_sensor_decode_value (r_exponent, 
						 b_exponent, 
						 m, 
						 b, 
						 linear, 
						 is_signed, 
						 (u_int8_t) val);
      printf ("Sensor reading: %.2f %s\n", 
	      sensor_reading, 
	      ipmi_sensor_units[base_unit]);
      
      fiid_obj_get (data_rs, 
		    tmpl_get_sensor_threshold_reading_rs, 
		    "status_comparison_lower_non_critical_threshold", 
		    &val);
      if (val == 1)
	{
	  printf ("Sensor status: at or below (<=) lower non-critical threshold\n");
	  alert_flag = 1;
	}
      
      fiid_obj_get (data_rs, 
		    tmpl_get_sensor_threshold_reading_rs, 
		    "status_comparison_lower_critical_threshold", 
		    &val);
      if (val == 1)
	{
	  printf ("Sensor status: at or below (<=) lower critical threshold\n");
	  alert_flag = 1;
	}
      
      fiid_obj_get (data_rs, 
		    tmpl_get_sensor_threshold_reading_rs, 
		    "status_comparison_lower_non_recoverable_threshold", 
		    &val);
      if (val == 1)
	{
	  printf ("Sensor status: at or below (<=) lower non-recoverable threshold\n");
	  alert_flag = 1;
	}
      
      fiid_obj_get (data_rs, 
		    tmpl_get_sensor_threshold_reading_rs, 
		    "status_comparison_upper_non_critical_threshold", 
		    &val);
      if (val == 1)
	{
	  printf ("Sensor status: at or above (>=) upper non-critical threshold\n");
	  alert_flag = 1;
	}
      
      fiid_obj_get (data_rs, 
		    tmpl_get_sensor_threshold_reading_rs, 
		    "status_comparison_upper_critical_threshold", 
		    &val);
      if (val == 1)
	{
	  printf ("Sensor status: at or above (>=) upper critical threshold\n");
	  alert_flag = 1;
	}
      
      fiid_obj_get (data_rs, 
		    tmpl_get_sensor_threshold_reading_rs, 
		    "status_comparison_upper_non_recoverable_threshold", 
		    &val);
      if (val == 1)
	{
	  printf ("Sensor status: at or above (>=) upper non-recoverable threshold\n");
	  alert_flag = 1;
	}
      
      if (sensor_reading < normal_min || sensor_reading > normal_max)
	alert_flag = 1;
      
      if (alert_flag)
	printf ("Sensor status: ALERT\n");
      else 
	printf ("Sensor status: OK\n");
    }
  else 
    {
      printf ("Sensor reading: N/A\n");
      printf ("Sensor status: FAILED\n");
    }
}

void 
display_verbose_current_generic_discrete_sensor_full_record (sdr_repo_cache_t *sdr_repo_cache)
{
  u_int64_t val;
  int i;
  
  /* requires to decode values */
  short b;
  short m;
  char r_exponent;
  char b_exponent;
  u_int8_t linear;
  u_int8_t is_signed;
  
  u_int8_t base_unit;
  
  u_int8_t sensor_number;
  u_int8_t sensor_type;
  u_int8_t event_reading_type_code;
  
  u_int8_t data_rs[] = {0, 0, 0, 0, 0, 0};
  u_int8_t status;
  
  u_int8_t record_length;
  
  char *status_message = NULL;
  
  ipmi_sensor_get_decode_params (sdr_repo_cache->cache_curr, 
				 &is_signed, &r_exponent, &b_exponent, 
				 &linear, &b, &m);
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record, 
		"record_length", 
		&val);
  record_length = val;
  record_length += fiid_obj_len_bytes (tmpl_sdr_sensor_record_header);
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record, 
		"sensor_base_unit", 
		&val);
  base_unit = val;
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record, 
		"event_reading_type", 
		&val);
  event_reading_type_code = val;
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record, 
		"sensor_number", 
		&val);
  sensor_number = val;
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record, 
		"sensor_type", 
		&val);
  sensor_type = val;
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record,
		"record_id",
		&val);
  printf ("Record ID: %d\n", (u_int16_t) val);
  
  printf ("Sensor type: ");
  for (i = 48; i < record_length; i++)
    printf ("%c", sdr_repo_cache->cache_curr[i]);
  printf (" (%s)\n", ipmi_get_sensor_group (sensor_type));
  
  printf ("Sensor number: #%d\n", sensor_number);
  
  printf ("Event/Reading type code: %02Xh\n", event_reading_type_code);
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record,
		"lower_non_recoverable_threshold",
		&val);
  printf ("Lower non-recoverable threshold: %.2f %s\n",
	  ipmi_sensor_decode_value (r_exponent,
				    b_exponent,
				    m,
				    b,
				    linear,
				    is_signed,
				    val), 
	  ipmi_sensor_units[base_unit]);
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record,
		"upper_non_recoverable_threshold",
		&val);
  printf ("Upper non-recoverable threshold: %.2f %s\n",
	  ipmi_sensor_decode_value (r_exponent,
				    b_exponent,
				    m,
				    b,
				    linear,
				    is_signed,
				    val), 
	  ipmi_sensor_units[base_unit]);
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record,
		"lower_critical_threshold",
		&val);
  printf ("Lower Critical threshold: %.2f %s\n",
	  ipmi_sensor_decode_value (r_exponent,
				    b_exponent,
				    m,
				    b,
				    linear,
				    is_signed,
				    val),
	  ipmi_sensor_units[base_unit]);
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record,
		"upper_critical_threshold",
		&val);
  printf ("Upper Critical threshold: %.2f %s\n",
	  ipmi_sensor_decode_value (r_exponent,
				    b_exponent,
				    m,
				    b,
				    linear,
				    is_signed,
				    val),
	  ipmi_sensor_units[base_unit]);
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record,
		"lower_non_critical_threshold",
		&val);
  printf ("Lower non-critical threshold: %.2f %s\n",
	  ipmi_sensor_decode_value (r_exponent,
				    b_exponent,
				    m,
				    b,
				    linear,
				    is_signed,
				    val),
	  ipmi_sensor_units[base_unit]);
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record,
		"upper_non_critical_threshold",
		&val);
  printf ("Upper non-critical threshold: %.2f %s\n",
	  ipmi_sensor_decode_value (r_exponent,
				    b_exponent,
				    m,
				    b,
				    linear,
				    is_signed,
				    val),
	  ipmi_sensor_units[base_unit]);
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record,
		"sensor_min_reading",
		&val);
  printf ("Sensor min. reading: %.2f %s\n",
	  ipmi_sensor_decode_value (r_exponent,
				    b_exponent,
				    m,
				    b,
				    linear,
				    is_signed,
				    val),
	  ipmi_sensor_units[base_unit]);
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record,
		"sensor_max_reading",
		&val);
  printf ("Sensor max. reading: %.2f %s\n",
	  ipmi_sensor_decode_value (r_exponent,
				    b_exponent,
				    m,
				    b,
				    linear,
				    is_signed,
				    val),
	  ipmi_sensor_units[base_unit]);
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record,
		"normal_min",
		&val);
  printf ("Normal min.: %.2f %s\n",
	  ipmi_sensor_decode_value (r_exponent,
				    b_exponent,
				    m,
				    b,
				    linear,
				    is_signed,
				    val), 
	  ipmi_sensor_units[base_unit]);
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record, 
		"nominal_reading", 
		&val);
  printf ("Nominal reading: %.2f %s\n",
	  ipmi_sensor_decode_value (r_exponent,
				    b_exponent,
				    m,
				    b,
				    linear,
				    is_signed,
				    val), 
	  ipmi_sensor_units[base_unit]);
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record,
		"normal_max",
		&val);
  printf ("Normal max: %.2f %s\n",
	  ipmi_sensor_decode_value (r_exponent,
				    b_exponent,
				    m,
				    b,
				    linear,
				    is_signed,
				    val), 
	  ipmi_sensor_units[base_unit]);
  
  printf ("Sensor reading: N/A\n");
  
  status = ipmi_kcs_get_discrete_reading (fi_get_sms_io_base (), 
					  sensor_number, 
					  data_rs);
  
  if (IPMI_COMP_CODE(data_rs) != IPMI_COMMAND_SUCCESS)
    {
      /*       char err_msg[IPMI_ERR_STR_MAX_LEN]; */
      /*       ipmi_strerror_cmd_r (data_rs, err_msg, IPMI_ERR_STR_MAX_LEN); */
      status = -1;
    }
  
  if (status == 0)
    {
      char key[65];
      
      for (i = 0; 
	   ipmi_event_reading_type_code_desc_ptr[event_reading_type_code][i] != NULL; 
	   i++)
	{
	  if (strcasecmp (ipmi_event_reading_type_code_desc_ptr[event_reading_type_code][i], 
			  "reserved") == 0)
	    continue;
	  
	  snprintf (key, 64, "state_%d_asserted", i);
	  fiid_obj_get (data_rs, 
			tmpl_get_sensor_discrete_reading_rs, 
			key, 
			&val);
	  
	  if (val == 1)
	    {
	      status_message = (char *)ipmi_event_reading_type_code_desc_ptr[event_reading_type_code][i];
	      break;
	    }
	}
      if (status_message == NULL)
	status_message = strdupa ("OK");
    }
  else 
    status_message = strdupa ("FAILED");
  
  printf ("Sensor status: [%s]\n", status_message);
}

void 
display_verbose_current_generic_discrete_sensor_compact_record (sdr_repo_cache_t *sdr_repo_cache)
{
  u_int64_t val;
  int i;
  
  u_int8_t base_unit;
  
  u_int8_t sensor_number;
  u_int8_t sensor_type;
  u_int8_t event_reading_type_code;
  
  u_int8_t data_rs[] = {0, 0, 0, 0, 0, 0};
  u_int8_t status;
  
  u_int8_t record_length;
  
  char *status_message = NULL;
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record, 
		"record_length", 
		&val);
  record_length = val;
  record_length += fiid_obj_len_bytes (tmpl_sdr_sensor_record_header);
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_compact_sensor_record, 
		"sensor_base_unit", 
		&val);
  base_unit = val;
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record, 
		"event_reading_type", 
		&val);
  event_reading_type_code = val;
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_compact_sensor_record, 
		"sensor_number", 
		&val);
  sensor_number = val;
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_compact_sensor_record, 
		"sensor_type", 
		&val);
  sensor_type = val;
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_compact_sensor_record, 
		"record_id",
		&val);
  printf ("Record ID: %d\n", (u_int16_t) val);
  
  printf ("Sensor type: ");
  for (i = 32; i < record_length; i++)
    printf ("%c", sdr_repo_cache->cache_curr[i]);
  printf (" (%s)\n", ipmi_get_sensor_group (sensor_type));
  
  printf ("Sensor number: #%d\n", sensor_number);
  
  printf ("Event/Reading type code: %02Xh\n", event_reading_type_code);
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_compact_sensor_record, 
		"positive_hysteresis", 
		&val);
  printf ("Hysteresis +ve: %d\n", (u_int8_t) val);
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_compact_sensor_record, 
		"negative_hysteresis", 
		&val);
  printf ("Hysteresis -ve: %d\n", (u_int8_t) val);
  
  status = ipmi_kcs_get_discrete_reading (fi_get_sms_io_base (), 
					  sensor_number, 
					  data_rs);
  
  if (IPMI_COMP_CODE(data_rs) != IPMI_COMMAND_SUCCESS)
    {
      /*       char err_msg[IPMI_ERR_STR_MAX_LEN]; */
      /*       ipmi_strerror_cmd_r (data_rs, err_msg, IPMI_ERR_STR_MAX_LEN); */
      status = -1;
    }
  
  if (status == 0)
    {
      char key[65];
      
      for (i = 0; 
	   ipmi_event_reading_type_code_desc_ptr[event_reading_type_code][i] != NULL; 
	   i++)
	{
	  if (strcasecmp (ipmi_event_reading_type_code_desc_ptr[event_reading_type_code][i], 
			  "reserved") == 0)
	    continue;
	  
	  snprintf (key, 64, "state_%d_asserted", i);
	  fiid_obj_get (data_rs, 
			tmpl_get_sensor_discrete_reading_rs, 
			key, 
			&val);
	  
	  if (val == 1)
	    {
	      status_message = (char *) ipmi_event_reading_type_code_desc_ptr[event_reading_type_code][i];
	      break;
	    }
	}
      if (status_message == NULL)
	status_message = strdupa ("OK");
    }
  else 
    status_message = strdupa ("FAILED");
  
  printf ("Sensor status: [%s]\n", status_message);
}

void 
display_verbose_current_discrete_sensor_full_record (sdr_repo_cache_t *sdr_repo_cache)
{
  u_int64_t val;
  int i;
  
  /* requires to decode values */
  short b;
  short m;
  char r_exponent;
  char b_exponent;
  u_int8_t linear;
  u_int8_t is_signed;
  
  u_int8_t base_unit;
  
  u_int8_t sensor_number;
  u_int8_t sensor_type;
  u_int8_t event_reading_type_code;
  
  u_int8_t data_rs[] = {0, 0, 0, 0, 0, 0};
  u_int8_t status;
  
  u_int8_t record_length;
  
  char *status_message = NULL;
  
  ipmi_sensor_get_decode_params (sdr_repo_cache->cache_curr, 
				 &is_signed, &r_exponent, &b_exponent, 
				 &linear, &b, &m);
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record, 
		"record_length", 
		&val);
  record_length = val;
  record_length += fiid_obj_len_bytes (tmpl_sdr_sensor_record_header);
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record, 
		"sensor_base_unit", 
		&val);
  base_unit = val;
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record, 
		"event_reading_type", 
		&val);
  event_reading_type_code = val;
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record, 
		"sensor_number", 
		&val);
  sensor_number = val;
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record, 
		"sensor_type", 
		&val);
  sensor_type = val;
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record,
		"record_id",
		&val);
  printf ("Record ID: %d\n", (u_int16_t) val);
  
  printf ("Sensor type: ");
  for (i = 48; i < record_length; i++)
    printf ("%c", sdr_repo_cache->cache_curr[i]);
  printf (" (%s)\n", ipmi_get_sensor_group (sensor_type));
  
  printf ("Sensor number: #%d\n", sensor_number);
  
  printf ("Event/Reading type code: %02Xh\n", event_reading_type_code);
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record,
		"lower_non_recoverable_threshold",
		&val);
  printf ("Lower non-recoverable threshold: %.2f %s\n",
	  ipmi_sensor_decode_value (r_exponent,
				    b_exponent,
				    m,
				    b,
				    linear,
				    is_signed,
				    val), 
	  ipmi_sensor_units[base_unit]);
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record,
		"upper_non_recoverable_threshold",
		&val);
  printf ("Upper non-recoverable threshold: %.2f %s\n",
	  ipmi_sensor_decode_value (r_exponent,
				    b_exponent,
				    m,
				    b,
				    linear,
				    is_signed,
				    val), 
	  ipmi_sensor_units[base_unit]);
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record,
		"lower_critical_threshold",
		&val);
  printf ("Lower Critical threshold: %.2f %s\n",
	  ipmi_sensor_decode_value (r_exponent,
				    b_exponent,
				    m,
				    b,
				    linear,
				    is_signed,
				    val),
	  ipmi_sensor_units[base_unit]);
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record,
		"upper_critical_threshold",
		&val);
  printf ("Upper Critical threshold: %.2f %s\n",
	  ipmi_sensor_decode_value (r_exponent,
				    b_exponent,
				    m,
				    b,
				    linear,
				    is_signed,
				    val),
	  ipmi_sensor_units[base_unit]);
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record,
		"lower_non_critical_threshold",
		&val);
  printf ("Lower non-critical threshold: %.2f %s\n",
	  ipmi_sensor_decode_value (r_exponent,
				    b_exponent,
				    m,
				    b,
				    linear,
				    is_signed,
				    val),
	  ipmi_sensor_units[base_unit]);
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record,
		"upper_non_critical_threshold",
		&val);
  printf ("Upper non-critical threshold: %.2f %s\n",
	  ipmi_sensor_decode_value (r_exponent,
				    b_exponent,
				    m,
				    b,
				    linear,
				    is_signed,
				    val),
	  ipmi_sensor_units[base_unit]);
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record,
		"sensor_min_reading",
		&val);
  printf ("Sensor min. reading: %.2f %s\n",
	  ipmi_sensor_decode_value (r_exponent,
				    b_exponent,
				    m,
				    b,
				    linear,
				    is_signed,
				    val),
	  ipmi_sensor_units[base_unit]);
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record,
		"sensor_max_reading",
		&val);
  printf ("Sensor max. reading: %.2f %s\n",
	  ipmi_sensor_decode_value (r_exponent,
				    b_exponent,
				    m,
				    b,
				    linear,
				    is_signed,
				    val),
	  ipmi_sensor_units[base_unit]);
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record,
		"normal_min",
		&val);
  printf ("Normal min.: %.2f %s\n",
	  ipmi_sensor_decode_value (r_exponent,
				    b_exponent,
				    m,
				    b,
				    linear,
				    is_signed,
				    val), 
	  ipmi_sensor_units[base_unit]);
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record, 
		"nominal_reading", 
		&val);
  printf ("Nominal reading: %.2f %s\n",
	  ipmi_sensor_decode_value (r_exponent,
				    b_exponent,
				    m,
				    b,
				    linear,
				    is_signed,
				    val), 
	  ipmi_sensor_units[base_unit]);
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record,
		"normal_max",
		&val);
  printf ("Normal max: %.2f %s\n",
	  ipmi_sensor_decode_value (r_exponent,
				    b_exponent,
				    m,
				    b,
				    linear,
				    is_signed,
				    val), 
	  ipmi_sensor_units[base_unit]);
  
  printf ("Sensor reading: N/A\n");
  
  status = ipmi_kcs_get_discrete_reading (fi_get_sms_io_base (), 
					  sensor_number, 
					  data_rs);
  
  if (IPMI_COMP_CODE(data_rs) != IPMI_COMMAND_SUCCESS)
    {
      /*       char err_msg[IPMI_ERR_STR_MAX_LEN]; */
      /*       ipmi_strerror_cmd_r (data_rs, err_msg, IPMI_ERR_STR_MAX_LEN); */
      status = -1;
    }
  
  if (status == 0)
    {
      char key[65];
      struct ipmi_discrete_desc *discrete_sensor_desc;
      
      discrete_sensor_desc = (struct ipmi_discrete_desc *) 
	ipmi_sensor_type_desc_ptr[sensor_type];
      
      for (i = 0; discrete_sensor_desc[i].message != NULL; i++)
	{
	  if (strcasecmp (discrete_sensor_desc[i].message, "reserved") == 0)
	    continue;
	  
	  snprintf (key, 64, "state_%d_asserted", i);
	  fiid_obj_get (data_rs, 
			tmpl_get_sensor_discrete_reading_rs, 
			key, 
			&val);
	  
	  printf ("%s: [%s]\n", discrete_sensor_desc[i].message, 
		  ((discrete_sensor_desc[i].normal_code == val) ? "OK": "ALERT"));
	}
      return;
    }
  else 
    status_message = strdupa ("FAILED");
  
  printf ("Sensor status: [%s]\n", status_message);
}

void 
display_verbose_current_discrete_sensor_compact_record (sdr_repo_cache_t *sdr_repo_cache)
{
  u_int64_t val;
  int i;
  
  u_int8_t base_unit;
  
  u_int8_t sensor_number;
  u_int8_t sensor_type;
  u_int8_t event_reading_type_code;
  
  u_int8_t data_rs[] = {0, 0, 0, 0, 0, 0};
  u_int8_t status;
  
  u_int8_t record_length;
  
  char *status_message = NULL;
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record, 
		"record_length", 
		&val);
  record_length = val;
  record_length += fiid_obj_len_bytes (tmpl_sdr_sensor_record_header);
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_compact_sensor_record, 
		"sensor_base_unit", 
		&val);
  base_unit = val;
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_full_sensor_record, 
		"event_reading_type", 
		&val);
  event_reading_type_code = val;
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_compact_sensor_record, 
		"sensor_number", 
		&val);
  sensor_number = val;
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_compact_sensor_record, 
		"sensor_type", 
		&val);
  sensor_type = val;
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_compact_sensor_record, 
		"record_id",
		&val);
  printf ("Record ID: %d\n", (u_int16_t) val);
  
  printf ("Sensor type: ");
  for (i = 32; i < record_length; i++)
    printf ("%c", sdr_repo_cache->cache_curr[i]);
  printf (" (%s)\n", ipmi_get_sensor_group (sensor_type));
  
  printf ("Sensor number: #%d\n", sensor_number);
  
  printf ("Event/Reading type code: %02Xh\n", event_reading_type_code);
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_compact_sensor_record, 
		"positive_hysteresis", 
		&val);
  printf ("Hysteresis +ve: %d\n", (u_int8_t) val);
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_compact_sensor_record, 
		"negative_hysteresis", 
		&val);
  printf ("Hysteresis -ve: %d\n", (u_int8_t) val);
  
  status = ipmi_kcs_get_discrete_reading (fi_get_sms_io_base (), 
					  sensor_number, 
					  data_rs);
  
  if (IPMI_COMP_CODE(data_rs) != IPMI_COMMAND_SUCCESS)
    {
      /*       char err_msg[IPMI_ERR_STR_MAX_LEN]; */
      /*       ipmi_strerror_cmd_r (data_rs, err_msg, IPMI_ERR_STR_MAX_LEN); */
      status = -1;
    }
  
  if (status == 0)
    {
      char key[65];
      struct ipmi_discrete_desc *discrete_sensor_desc;
      
      discrete_sensor_desc = (struct ipmi_discrete_desc *) 
	ipmi_sensor_type_desc_ptr[sensor_type];
      
      for (i = 0; discrete_sensor_desc[i].message != NULL; i++)
	{
	  if (strcasecmp (discrete_sensor_desc[i].message, "reserved") == 0)
	    continue;
	  
	  snprintf (key, 64, "state_%d_asserted", i);
	  fiid_obj_get (data_rs, 
			tmpl_get_sensor_discrete_reading_rs, 
			key, 
			&val);
	  
	  printf ("%s: [%s]\n", discrete_sensor_desc[i].message, 
		  ((discrete_sensor_desc[i].normal_code == val) ? "OK": "ALERT"));
	}
      return;
    }
  else 
    status_message = strdupa ("FAILED");
  
  printf ("Sensor status: [%s]\n", status_message);
}

void 
display_verbose_current_oem_sensor_full_record (sdr_repo_cache_t *sdr_repo_cache)
{
  printf ("OEM sensor class full record type: not implemented\n");
}

void 
display_verbose_current_oem_sensor_compact_record (sdr_repo_cache_t *sdr_repo_cache)
{
  printf ("OEM sensor class compact record type: not implemented\n");
}

u_int8_t 
display_verbose_current_sensor (sdr_repo_cache_t *sdr_repo_cache)
{
  u_int64_t val;
  
  if (!ipmi_is_sensor_reading_available (sdr_repo_cache))
    return false;

  switch (ipmi_sdr_repo_cache_sensor_classify (sdr_repo_cache))
    {
    case IPMI_SENSOR_CLASS_THRESHOLD:
      fiid_obj_get (sdr_repo_cache->cache_curr, 
		    tmpl_sdr_sensor_record_header, 
		    "record_type", 
		    &val);
	  
      switch (val)
	{
	case IPMI_SDR_FORMAT_FULL_RECORD:
	  display_verbose_current_threshold_sensor_full_record (sdr_repo_cache);
	  fflush (stdout);
	  break;
	      
	case IPMI_SDR_FORMAT_COMPACT_RECORD:
	case IPMI_SDR_FORMAT_EVENT_ONLY_RECORD:
	case IPMI_SDR_FORMAT_ENTITY_ASSO_RECORD:
	case IPMI_SDR_FORMAT_DEV_ENTITY_ASSO_RECORD:
	case IPMI_SDR_FORMAT_GEN_DEV_LOCATOR_RECORD:
	case IPMI_SDR_FORMAT_FRU_DEV_LOCATOR_RECORD:
	case IPMI_SDR_FORMAT_MGMT_CNTRLR_DEV_LOCATOR_RECORD:
	case IPMI_SDR_FORMAT_MGMT_CNTRLR_CONFIRMATION_RECORD:
	case IPMI_SDR_FORMAT_BMC_MSG_CHANNEL_INFO_RECORD:
	case IPMI_SDR_FORMAT_OEM_RECORD:
	  {
	    char errstr[IPMI_ERR_STR_MAX_LEN];
	    snprintf (errstr, IPMI_ERR_STR_MAX_LEN, 
		      "BUG: %s: %d: %s: record-id %d: "
		      "sensor class threshold record type [%02X] "
		      "is not a full record\n", 
		      __FILE__, __LINE__, __PRETTY_FUNCTION__, 
		      sdr_repo_cache->cache_curr_rec_id, (u_int8_t) val);
		
	    syslog (LOG_MAKEPRI(LOG_LOCAL1, LOG_ERR), errstr);
	    printf ("__DEBUG__: %s", errstr);
	  }
	  break;
	      
	default:
	  break;
	}
      break;
	  
    case IPMI_SENSOR_CLASS_GENERIC_DISCRETE:
      fiid_obj_get (sdr_repo_cache->cache_curr, 
		    tmpl_sdr_sensor_record_header, 
		    "record_type", 
		    &val);
	  
      switch (val)
	{
	case IPMI_SDR_FORMAT_FULL_RECORD:
	  display_verbose_current_generic_discrete_sensor_full_record (sdr_repo_cache);
	  fflush (stdout);
	  break;
	      
	case IPMI_SDR_FORMAT_COMPACT_RECORD:
	  display_verbose_current_generic_discrete_sensor_compact_record (sdr_repo_cache);
	  fflush (stdout);
	  break;
	      
	case IPMI_SDR_FORMAT_EVENT_ONLY_RECORD:
	case IPMI_SDR_FORMAT_ENTITY_ASSO_RECORD:
	case IPMI_SDR_FORMAT_DEV_ENTITY_ASSO_RECORD:
	case IPMI_SDR_FORMAT_GEN_DEV_LOCATOR_RECORD:
	case IPMI_SDR_FORMAT_FRU_DEV_LOCATOR_RECORD:
	case IPMI_SDR_FORMAT_MGMT_CNTRLR_DEV_LOCATOR_RECORD:
	case IPMI_SDR_FORMAT_MGMT_CNTRLR_CONFIRMATION_RECORD:
	case IPMI_SDR_FORMAT_BMC_MSG_CHANNEL_INFO_RECORD:
	case IPMI_SDR_FORMAT_OEM_RECORD:
	  break;
	      
	default:
	  break;
	}
      break;
	  
    case IPMI_SENSOR_CLASS_SENSOR_SPECIFIC_DISCRETE:
      fiid_obj_get (sdr_repo_cache->cache_curr, 
		    tmpl_sdr_sensor_record_header, 
		    "record_type", 
		    &val);
	  
      switch (val)
	{
	case IPMI_SDR_FORMAT_FULL_RECORD:
	  display_verbose_current_discrete_sensor_full_record (sdr_repo_cache);
	  fflush (stdout);
	  break;
		
	case IPMI_SDR_FORMAT_COMPACT_RECORD:
	  display_verbose_current_discrete_sensor_compact_record (sdr_repo_cache);
	  fflush (stdout);
	  break;
	      
	case IPMI_SDR_FORMAT_EVENT_ONLY_RECORD:
	case IPMI_SDR_FORMAT_ENTITY_ASSO_RECORD:
	case IPMI_SDR_FORMAT_DEV_ENTITY_ASSO_RECORD:
	case IPMI_SDR_FORMAT_GEN_DEV_LOCATOR_RECORD:
	case IPMI_SDR_FORMAT_FRU_DEV_LOCATOR_RECORD:
	case IPMI_SDR_FORMAT_MGMT_CNTRLR_DEV_LOCATOR_RECORD:
	case IPMI_SDR_FORMAT_MGMT_CNTRLR_CONFIRMATION_RECORD:
	case IPMI_SDR_FORMAT_BMC_MSG_CHANNEL_INFO_RECORD:
	case IPMI_SDR_FORMAT_OEM_RECORD:
	  break;
	      
	default:
	  break;
	}
      break;
	  
    case IPMI_SENSOR_CLASS_OEM:
      fiid_obj_get (sdr_repo_cache->cache_curr, 
		    tmpl_sdr_sensor_record_header, 
		    "record_type", 
		    &val);
	  
      switch (val)
	{
	case IPMI_SDR_FORMAT_FULL_RECORD:
	  display_verbose_current_oem_sensor_full_record (sdr_repo_cache);
	  fflush (stdout);
	  break;
		
	case IPMI_SDR_FORMAT_COMPACT_RECORD:
	  display_verbose_current_oem_sensor_compact_record (sdr_repo_cache);
	  fflush (stdout);
	  break;
	      
	case IPMI_SDR_FORMAT_EVENT_ONLY_RECORD:
	case IPMI_SDR_FORMAT_ENTITY_ASSO_RECORD:
	case IPMI_SDR_FORMAT_DEV_ENTITY_ASSO_RECORD:
	case IPMI_SDR_FORMAT_GEN_DEV_LOCATOR_RECORD:
	case IPMI_SDR_FORMAT_FRU_DEV_LOCATOR_RECORD:
	case IPMI_SDR_FORMAT_MGMT_CNTRLR_DEV_LOCATOR_RECORD:
	case IPMI_SDR_FORMAT_MGMT_CNTRLR_CONFIRMATION_RECORD:
	case IPMI_SDR_FORMAT_BMC_MSG_CHANNEL_INFO_RECORD:
	case IPMI_SDR_FORMAT_OEM_RECORD:
	  break;
	      
	default:
	  break;
	}
      break;
	  
    default:
      /* unknown sensor type */
      break;
    }
  return true;
}

void 
display_very_verbose_entity_association_record (sdr_repo_cache_t *sdr_repo_cache)
{
  u_int64_t val;
  
  u_int8_t record_length;
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_entity_association_sensor_record, 
		"record_length", 
		&val);
  record_length = val;
  record_length += fiid_obj_len_bytes (tmpl_sdr_sensor_record_header);
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_entity_association_sensor_record, 
		"record_id", 
		&val);
  printf ("Record ID: %d\n", (u_int16_t) val);
  
  printf ("Sensor Number: NONE\n");
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_entity_association_sensor_record, 
		"container_entity_id", 
		&val);
  printf ("Container entity ID: %02Xh\n", (u_int8_t) val);
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_entity_association_sensor_record, 
		"container_entity_instance", 
		&val);
  printf ("Container entity instance: %02Xh\n", (u_int8_t) val);
}

void 
display_very_verbose_fru_dev_locator_record (sdr_repo_cache_t *sdr_repo_cache)
{
  u_int64_t val;
  int i;
  
  u_int8_t record_length;
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_logical_fru_device_locator_sensor_record, 
		"record_length", 
		&val);
  record_length = val;
  record_length += fiid_obj_len_bytes (tmpl_sdr_sensor_record_header);
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_logical_fru_device_locator_sensor_record, 
		"record_id", 
		&val);
  printf ("Record ID: %d\n", (u_int16_t) val);
  
  printf ("Sensor Number: NONE\n");
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_logical_fru_device_locator_sensor_record, 
		"device_type", 
		&val);
  printf ("Device type: %02Xh\n", (u_int8_t) val);
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_logical_fru_device_locator_sensor_record, 
		"device_type_modifier", 
		&val);
  printf ("Device type modifier: %02Xh\n", (u_int8_t) val);
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_logical_fru_device_locator_sensor_record, 
		"fru_entity_id", 
		&val);
  printf ("FRU entity ID: %02Xh\n", (u_int8_t) val);
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_logical_fru_device_locator_sensor_record, 
		"fru_entity_instance", 
		&val);
  printf ("FRU entity instance: %02Xh\n", (u_int8_t) val);
  
  printf ("Device name: ");
  for (i = 16; i < record_length; i++)
    printf ("%c", sdr_repo_cache->cache_curr[i]);
  printf ("\n");
}

void 
display_very_verbose_mgmt_control_dev_locator_record (sdr_repo_cache_t *sdr_repo_cache)
{
  u_int64_t val;
  int i;
  
  u_int8_t record_length;
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_management_controller_device_locator_sensor_record, 
		"record_length", 
		&val);
  record_length = val;
  record_length += fiid_obj_len_bytes (tmpl_sdr_sensor_record_header);
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_management_controller_device_locator_sensor_record, 
		"record_id", 
		&val);
  printf ("Record ID: %d\n", (u_int16_t) val);
  
  printf ("Sensor Number: NONE\n");
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_management_controller_device_locator_sensor_record, 
		"entity_id", 
		&val);
  printf ("Entity ID: %02Xh\n", (u_int8_t) val);
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_management_controller_device_locator_sensor_record, 
		"entity_instance", 
		&val);
  printf ("Entity instance: %02Xh\n", (u_int8_t) val);
  
  printf ("Device name: ");
  for (i = 16; i < record_length; i++)
    printf ("%c", sdr_repo_cache->cache_curr[i]);
  printf ("\n");
}

void 
display_very_verbose_oem_record (sdr_repo_cache_t *sdr_repo_cache)
{
  u_int64_t val;
  int i;
  
  u_int8_t record_length;
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_oem_sensor_record, 
		"record_length", 
		&val);
  record_length = val;
  record_length += fiid_obj_len_bytes (tmpl_sdr_sensor_record_header);
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_oem_sensor_record, 
		"record_id", 
		&val);
  printf ("Record ID: %d\n", (u_int16_t) val);
  
  printf ("Sensor Number: NONE\n");
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_oem_sensor_record, 
		"manufacturer_id", 
		&val);
  printf ("Manufacturer ID: %06Xh\n", (u_int32_t) val);
  
  printf ("OEM Data: ");
  for (i = 8; i < record_length; i++)
    printf ("%02X ", sdr_repo_cache->cache_curr[i]);
  printf ("\n");
}

u_int8_t 
display_very_verbose_current_sensor (sdr_repo_cache_t *sdr_repo_cache)
{
  u_int64_t val;
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_sensor_record_header, 
		"record_type", 
		&val);
  
  switch (val)
    {
    case IPMI_SDR_FORMAT_FULL_RECORD:
      switch (ipmi_sdr_repo_cache_sensor_classify (sdr_repo_cache))
	{
	case IPMI_SENSOR_CLASS_THRESHOLD:
	  display_verbose_current_threshold_sensor_full_record (sdr_repo_cache);
	  fflush (stdout);
	  break;
	case IPMI_SENSOR_CLASS_GENERIC_DISCRETE:
	  display_verbose_current_generic_discrete_sensor_full_record (sdr_repo_cache);
	  fflush (stdout);
	  break;
	case IPMI_SENSOR_CLASS_SENSOR_SPECIFIC_DISCRETE:
	  display_verbose_current_discrete_sensor_full_record (sdr_repo_cache);
	  fflush (stdout);
	  break;
	case IPMI_SENSOR_CLASS_OEM:
	  display_verbose_current_oem_sensor_full_record (sdr_repo_cache);
	  fflush (stdout);
	  break;
	}
      break;
      
    case IPMI_SDR_FORMAT_COMPACT_RECORD:
      switch (ipmi_sdr_repo_cache_sensor_classify (sdr_repo_cache))
	{
	case IPMI_SENSOR_CLASS_GENERIC_DISCRETE:
	  display_verbose_current_generic_discrete_sensor_compact_record (sdr_repo_cache);
	  fflush (stdout);
	  break;
	case IPMI_SENSOR_CLASS_SENSOR_SPECIFIC_DISCRETE:
	  display_verbose_current_discrete_sensor_compact_record (sdr_repo_cache);
	  fflush (stdout);
	  break;
	case IPMI_SENSOR_CLASS_OEM:
	  display_verbose_current_oem_sensor_compact_record (sdr_repo_cache);
	  fflush (stdout);
	  break;
	}
      break;
      
    case IPMI_SDR_FORMAT_ENTITY_ASSO_RECORD:
      display_very_verbose_entity_association_record (sdr_repo_cache);
      fflush (stdout);
      break;
      
    case IPMI_SDR_FORMAT_FRU_DEV_LOCATOR_RECORD:
      display_very_verbose_fru_dev_locator_record (sdr_repo_cache);
      fflush (stdout);
      break;
      
    case IPMI_SDR_FORMAT_MGMT_CNTRLR_DEV_LOCATOR_RECORD:
      display_very_verbose_mgmt_control_dev_locator_record (sdr_repo_cache);
      fflush (stdout);
      break;
      
    case IPMI_SDR_FORMAT_OEM_RECORD:
      display_very_verbose_oem_record (sdr_repo_cache);
      fflush (stdout);
      break;
      
    case IPMI_SDR_FORMAT_EVENT_ONLY_RECORD:
    case IPMI_SDR_FORMAT_DEV_ENTITY_ASSO_RECORD:
    case IPMI_SDR_FORMAT_GEN_DEV_LOCATOR_RECORD:
    case IPMI_SDR_FORMAT_MGMT_CNTRLR_CONFIRMATION_RECORD:
    case IPMI_SDR_FORMAT_BMC_MSG_CHANNEL_INFO_RECORD:
    default:
      {
	char errstr[IPMI_ERR_STR_MAX_LEN];
	snprintf (errstr, IPMI_ERR_STR_MAX_LEN, 
		  "BUG: %s: %d: %s: record-id %d: "
		  "record type [%02X] "
		  "is not handled\n", 
		  __FILE__, __LINE__, __PRETTY_FUNCTION__, 
		  sdr_repo_cache->cache_curr_rec_id, (u_int8_t) val);
	
	syslog (LOG_MAKEPRI(LOG_LOCAL1, LOG_ERR), errstr);
	printf ("__DEBUG__: %s", errstr);
      }
      fflush (stdout);
      break;
    }
  return true;
}

