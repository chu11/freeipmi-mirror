#include <freeipmi/freeipmi.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#ifdef __ia64__
#define SMS_IO_BASE IPMI_KCS_SMS_IO_BASE_SR870BN4
#else
#define SMS_IO_BASE IPMI_KCS_SMS_IO_BASE_DEFAULT
#endif

int 
get_sdr_total_records (sdr_repo_cache_t *sdr_repo_cache)
{
  return (sdr_repo_cache->total_records);
}

void 
display_verbose_current_threshold_sensor_full_record (sdr_repo_cache_t *sdr_repo_cache)
{
  u_int64_t val;
  
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
  
  fiid_obj_t obj_hdr_rs;
  fiid_obj_t obj_data_rs;
  u_int8_t status;
  
  u_int8_t record_length;
  
  double sensor_reading;
  
  u_int8_t alert_flag = 0;
  
  obj_hdr_rs = alloca (fiid_obj_len_bytes (tmpl_hdr_kcs));
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_get_sensor_threshold_reading_rs));
  
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
  
  {
    char *sensor_name = NULL;
    sensor_name = strndup ((char *) sdr_repo_cache->cache_curr + 48, 
			   (record_length - 48));
    
    printf ("Sensor type: %s (%s)\n", 
	    sensor_name, 
	    ipmi_get_sensor_group (sensor_type));
    free (sensor_name);
  }
  
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
  
  status = ipmi_kcs_get_threshold_reading (SMS_IO_BASE, 
					   sensor_number, 
					   obj_data_rs);
  
  if (IPMI_COMP_CODE(obj_data_rs) != IPMI_COMMAND_SUCCESS)
    {
      /*       char err_msg[IPMI_ERR_STR_MAX_LEN]; */
      /*       ipmi_strerror_cmd_r (obj_data_rs, err_msg, IPMI_ERR_STR_MAX_LEN); */
      status = -1;
    }
  
  if (status == 0)
    {
      fiid_obj_get (obj_data_rs, 
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
      
      fiid_obj_get (obj_data_rs, 
		    tmpl_get_sensor_threshold_reading_rs, 
		    "status_comparison_lower_non_critical_threshold", 
		    &val);
      if (val == 1)
	{
	  printf ("Sensor status: at or below (<=) lower non-critical threshold\n");
	  alert_flag = 1;
	}
      
      fiid_obj_get (obj_data_rs, 
		    tmpl_get_sensor_threshold_reading_rs, 
		    "status_comparison_lower_critical_threshold", 
		    &val);
      if (val == 1)
	{
	  printf ("Sensor status: at or below (<=) lower critical threshold\n");
	  alert_flag = 1;
	}
      
      fiid_obj_get (obj_data_rs, 
		    tmpl_get_sensor_threshold_reading_rs, 
		    "status_comparison_lower_non_recoverable_threshold", 
		    &val);
      if (val == 1)
	{
	  printf ("Sensor status: at or below (<=) lower non-recoverable threshold\n");
	  alert_flag = 1;
	}
      
      fiid_obj_get (obj_data_rs, 
		    tmpl_get_sensor_threshold_reading_rs, 
		    "status_comparison_upper_non_critical_threshold", 
		    &val);
      if (val == 1)
	{
	  printf ("Sensor status: at or above (>=) upper non-critical threshold\n");
	  alert_flag = 1;
	}
      
      fiid_obj_get (obj_data_rs, 
		    tmpl_get_sensor_threshold_reading_rs, 
		    "status_comparison_upper_critical_threshold", 
		    &val);
      if (val == 1)
	{
	  printf ("Sensor status: at or above (>=) upper critical threshold\n");
	  alert_flag = 1;
	}
      
      fiid_obj_get (obj_data_rs, 
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
display_verbose_current_digital_discrete_sensor_full_record (sdr_repo_cache_t *sdr_repo_cache)
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
  
  fiid_obj_t obj_hdr_rs;
  fiid_obj_t obj_data_rs;
  u_int8_t status;
  
  u_int8_t record_length;
  
  char *status_message = NULL;
  
  obj_hdr_rs = alloca (fiid_obj_len_bytes (tmpl_hdr_kcs));
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_get_sensor_discrete_reading_rs));
  
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
  
  {
    char *sensor_name = NULL;
    sensor_name = strndup ((char *) sdr_repo_cache->cache_curr + 48,
                           (record_length - 48));

    printf ("Sensor type: %s (%s)\n",
            sensor_name,
            ipmi_get_sensor_group (sensor_type));
    free (sensor_name);
  }
  
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
  
  status = ipmi_kcs_get_discrete_reading (SMS_IO_BASE, 
					  sensor_number, 
					  obj_data_rs);
  
  if (IPMI_COMP_CODE(obj_data_rs) != IPMI_COMMAND_SUCCESS)
    {
      /*       char err_msg[IPMI_ERR_STR_MAX_LEN]; */
      /*       ipmi_strerror_cmd_r (obj_data_rs, err_msg, IPMI_ERR_STR_MAX_LEN); */
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
	  fiid_obj_get (obj_data_rs, 
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
display_verbose_current_digital_discrete_sensor_compact_record (sdr_repo_cache_t *sdr_repo_cache)
{
  u_int64_t val;
  int i;
  
  u_int8_t base_unit;
  
  u_int8_t sensor_number;
  u_int8_t sensor_type;
  u_int8_t event_reading_type_code;
  
  fiid_obj_t obj_hdr_rs;
  fiid_obj_t obj_data_rs;
  u_int8_t status;
  
  u_int8_t record_length;
  
  char *status_message = NULL;
  
  obj_hdr_rs = alloca (fiid_obj_len_bytes (tmpl_hdr_kcs));
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_get_sensor_discrete_reading_rs));
  
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
  
  {
    char *sensor_name = NULL;
    sensor_name = strndup ((char *) sdr_repo_cache->cache_curr + 32,
                           (record_length - 32));

    printf ("Sensor type: %s (%s)\n",
            sensor_name,
            ipmi_get_sensor_group (sensor_type));
    free (sensor_name);
  }
  
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
  
  status = ipmi_kcs_get_discrete_reading (SMS_IO_BASE, 
					  sensor_number, 
					  obj_data_rs);
  
  if (IPMI_COMP_CODE(obj_data_rs) != IPMI_COMMAND_SUCCESS)
    {
      /*       char err_msg[IPMI_ERR_STR_MAX_LEN]; */
      /*       ipmi_strerror_cmd_r (obj_data_rs, err_msg, IPMI_ERR_STR_MAX_LEN); */
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
	  fiid_obj_get (obj_data_rs, 
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
  
  fiid_obj_t obj_hdr_rs;
  fiid_obj_t obj_data_rs;
  u_int8_t status;
  
  u_int8_t record_length;
  
  char *status_message = NULL;
  
  obj_hdr_rs = alloca (fiid_obj_len_bytes (tmpl_hdr_kcs));
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_get_sensor_discrete_reading_rs));
  
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
  
  {
    char *sensor_name = NULL;
    sensor_name = strndup ((char *) sdr_repo_cache->cache_curr + 48,
                           (record_length - 48));

    printf ("Sensor type: %s (%s)\n",
            sensor_name,
            ipmi_get_sensor_group (sensor_type));
    free (sensor_name);
  }
  
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
  
  status = ipmi_kcs_get_discrete_reading (SMS_IO_BASE, 
					  sensor_number, 
					  obj_data_rs);
  
  if (IPMI_COMP_CODE(obj_data_rs) != IPMI_COMMAND_SUCCESS)
    {
      /*       char err_msg[IPMI_ERR_STR_MAX_LEN]; */
      /*       ipmi_strerror_cmd_r (obj_data_rs, err_msg, IPMI_ERR_STR_MAX_LEN); */
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
	  fiid_obj_get (obj_data_rs, 
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
  
  fiid_obj_t obj_hdr_rs;
  fiid_obj_t obj_data_rs;
  u_int8_t status;
  
  u_int8_t record_length;
  
  char *status_message = NULL;
  
  obj_hdr_rs = alloca (fiid_obj_len_bytes (tmpl_hdr_kcs));
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_get_sensor_discrete_reading_rs));
  
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
  
  {
    char *sensor_name = NULL;
    sensor_name = strndup ((char *) sdr_repo_cache->cache_curr + 32,
                           (record_length - 32));

    printf ("Sensor type: %s (%s)\n",
            sensor_name,
            ipmi_get_sensor_group (sensor_type));
    free (sensor_name);
  }
  
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
  
  status = ipmi_kcs_get_discrete_reading (SMS_IO_BASE, 
					  sensor_number, 
					  obj_data_rs);
  
  if (IPMI_COMP_CODE(obj_data_rs) != IPMI_COMMAND_SUCCESS)
    {
      /*       char err_msg[IPMI_ERR_STR_MAX_LEN]; */
      /*       ipmi_strerror_cmd_r (obj_data_rs, err_msg, IPMI_ERR_STR_MAX_LEN); */
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
	  fiid_obj_get (obj_data_rs, 
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
  /* TODO:: OEM specification is required for implementation */
}

void 
display_verbose_current_oem_sensor_compact_record (sdr_repo_cache_t *sdr_repo_cache)
{
  /* TODO:: OEM specification is required for implementation */
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
/* 	case IPMI_SENSOR_CLASS_DIGITAL_DISCRETE: */
	case IPMI_SENSOR_CLASS_GENERIC_DISCRETE:
	  display_verbose_current_digital_discrete_sensor_full_record (sdr_repo_cache);
	  fflush (stdout);
	  break;
/* 	case IPMI_SENSOR_CLASS_DISCRETE: */
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
/* 	case IPMI_SENSOR_CLASS_DIGITAL_DISCRETE: */
	case IPMI_SENSOR_CLASS_GENERIC_DISCRETE:
	  display_verbose_current_digital_discrete_sensor_compact_record (sdr_repo_cache);
	  fflush (stdout);
	  break;
/* 	case IPMI_SENSOR_CLASS_DISCRETE: */
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
      /* TODO:: needs to implement in libfreeipmi*/
      break;
    }
  return true;
}

int 
main ()
{
  sdr_repo_cache_t sdr_repo_cache;
  char cache_filename[] = "/tmp/sdr-repo.cache";
  int retval;
  int total_records;
  int rec_id;
  
  /* init kcs */
  if (ipmi_kcs_io_init (SMS_IO_BASE, IPMI_KCS_SLEEP_USECS) != 0)
    {
      perror ("ipmi_kcs_io_init");
      exit (-1);
    }
  
  /* creating SDR cache */
  printf ("creating SDR cache file %s\n", cache_filename);
  retval = ipmi_sdr_cache_create (SMS_IO_BASE, cache_filename);
  if (retval)
    {
      fprintf (stderr, "error: ipmi_sdr_cache_create failed\n");
      exit (-1);
    }
  
/*   sdr_repo_cache.fd = open (cache_filename, O_RDONLY); */
  
  /* load SDR cache */
  if (ipmi_sdr_repo_cache_load (&sdr_repo_cache, cache_filename))
    {
      fprintf (stderr, "error: ipmi_sdr_repo_cache_load failed\n");
      exit (-1);
    }
  
  /* get total records */
  total_records = get_sdr_total_records (&sdr_repo_cache);
  printf ("Total SDR records: %d\n", total_records);
  
  /* display all records */
  for (rec_id = 1; rec_id <= total_records; rec_id++)
    {
      if (ipmi_sdr_repo_cache_seek (&sdr_repo_cache, rec_id))
	{
	  fprintf (stderr, 
		   "error: ipmi_sdr_repo_cache_seek failed on rec_id %d\n", 
		   rec_id);
	  break;
	}
      
      display_very_verbose_current_sensor (&sdr_repo_cache);
      printf ("\n\n");
    }
  
  /* alternative way using first/next approach */
  /* 
     if (ipmi_sdr_repo_cache_first (&sdr_repo_cache))
     {
     fprintf (stderr,
     "error: ipmi_sdr_repo_cache_seek failed on rec_id %d\n",
     rec_id);
     if (ipmi_sdr_repo_cache_unload (&sdr_repo_cache))
     {
     fprintf (stderr, "error: ipmi_sdr_repo_cache_unload failed\n");
     exit (-1);
     }
     }
     do
     {
     display_very_verbose_current_sensor (&sdr_repo_cache);
     }
     while (ipmi_sdr_repo_cache_next (&sdr_repo_cache) == 0);
  */
  
  if (ipmi_sdr_repo_cache_unload (&sdr_repo_cache))
    {
      fprintf (stderr, "error: ipmi_sdr_repo_cache_unload failed\n");
      exit (-1);
    }
  
  exit (0);
}
