/* 
   scm-procedures.c: gnu messenger procedures that are exported to
   guile environment
   
   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2, or (at
   your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You shouqd have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA. */


#include "common.h"

/*
  SCM
  ex_set_some_thing_x (SCM scm_some_thing)
  {
  int some_thing_length;
  
  // FIXME: currently I'm not validating the result of gh_scm2newstr
  // also note that Scheme strings may contain arbitrary data,
  // including null characters.  This means that null termination is
  // not a reliable way to determine the length of the returned value.
  // However, the function always copies the complete contents of
  // scm_some_thing, and sets *some_thing_length to the true length of the
  // string (when some_thing_length is non-null)
  
  set_some_thing (gh_scm2newstr (scm_some_thing, &some_thing_length));
  
  return (SCM_UNSPECIFIED);
  }
*/

/* returns current fish version */
SCM
ex_version (void)
{
  return (gh_str02scm (PACKAGE_VERSION));
}


SCM
ex_toggle_x (SCM scm_what2toggle)
{
  if (strcasecmp (gh_scm2newstr (scm_what2toggle, NULL), 
		  "bell") == 0)
    toggle_bell ();
  //  else if (strcasecmp (gh_scm2newstr (scm_what2toggle, NULL),
  //		       "who") == 0)
  //  toggle_who ();
  else
    // FIXME: I think this is now working!!  --ab@gnu.org.in
    scm_wrong_type_arg (gh_scm2newstr (scm_what2toggle, NULL),
			0, SCM_UNSPECIFIED);
  return (SCM_UNSPECIFIED);
}

SCM
ex_hook_return (void)
{
  set_hook_return (1);
  return (SCM_UNSPECIFIED);
}

SCM 
ex_load (SCM scm_filename)
{
  char *filename = NULL;
  int retval;
  
  filename = gh_scm2newstr (scm_filename, NULL);
  retval = fi_load (filename);
  free (filename);
  
  return (retval ? SCM_BOOL_F : SCM_BOOL_T);
}

SCM
ex_get_sysconfig_dir (void)
{
  return (gh_str02scm (PATH_CFG));
}

SCM
ex_register_command_x (SCM scm_command)
{
  register_command (scm_command);
  return (SCM_UNSPECIFIED);
}

SCM
ex_unregister_command_x (SCM scm_command)
{
  unregister_command (scm_command);
  return (SCM_UNSPECIFIED);
}

SCM
ex_set_prompt_x (SCM scm_prompt)
{
  set_fish_prompt (gh_scm2newstr (scm_prompt, NULL));
  return (SCM_UNSPECIFIED);
}

SCM
ex_exit (SCM scm_status)
{
  exit (gh_scm2int (scm_status)); 
  return (SCM_UNSPECIFIED);
}

SCM
ex_quit ()
{
  fi_quit ();
  return (SCM_UNSPECIFIED);
}

SCM 
ex_ipmi_ping (SCM scm_host, SCM scm_timeout)
{
  char *host = NULL;
  unsigned int sock_timeout = 0;
  int status = 0;
  
  host = gh_scm2newstr (scm_host, NULL);
  sock_timeout = gh_scm2long (scm_timeout);
  
  status = ipmi_ping (host, sock_timeout);
  xfree (host);
  
  return (status == 0 ? SCM_BOOL_T : SCM_BOOL_F);
}

SCM 
ex_get_script_command_line ()
{
  SCM scm_arg_list;
  
  int i;
  int argc;
  char **argv;
  
  scm_arg_list  = gh_list (SCM_UNDEFINED);
  argc = get_script_argc ();
  argv = get_script_argv ();
  
  for (i = 0; i < argc; i++)
    {
      if (gh_list_p (scm_arg_list) != 1)
        scm_arg_list = gh_list (gh_str02scm (argv[i]), SCM_UNDEFINED);
      else 
        {
          scm_arg_list = gh_append2 (scm_arg_list, 
				     gh_list (gh_str02scm (argv[i]), 
					      SCM_UNDEFINED));
        }
    }
  
  return (scm_arg_list);
}

SCM 
ex_sensors_get_group_list ()
{
  SCM scm_group_list;
  int i;
  
  scm_group_list = gh_list (SCM_UNDEFINED);
  
  for (i = 0; ipmi_sensor_types[i]; i++)
    {
      if (gh_list_p (scm_group_list) != 1)
	scm_group_list = gh_list (gh_str02scm (ipmi_sensor_types[i]), SCM_UNDEFINED);
      else 
        {
          scm_group_list = gh_append2 (scm_group_list, 
				       gh_list (gh_str02scm (ipmi_sensor_types[i]), 
						SCM_UNDEFINED));
        }
    }
  
  if (gh_list_p (scm_group_list) != 1)
    scm_group_list = gh_list (gh_str02scm (ipmi_oem_sensor_type), SCM_UNDEFINED);
  else 
    {
      scm_group_list = gh_append2 (scm_group_list, 
				   gh_list (gh_str02scm (ipmi_oem_sensor_type), 
					    SCM_UNDEFINED));
    }

  return (scm_group_list);
}

/*********** new sensor procedures ***************/
static SCM 
get_scm_sdr_full_record (sdr_full_record_t *record, 
			 SCM scm_sdr_record)
{
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("readable_lower_non_recoverable_threshold"), 
				    (record->readable_threshold_lower_non_recoverable_threshold ? 
				     SCM_BOOL_T : SCM_BOOL_F));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("readable_upper_non_recoverable_threshold"), 
				    (record->readable_threshold_upper_non_recoverable_threshold ? 
				     SCM_BOOL_T : SCM_BOOL_F));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("readable_lower_critical_threshold"), 
				    (record->readable_threshold_lower_critical_threshold ? 
				     SCM_BOOL_T : SCM_BOOL_F));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("readable_upper_critical_threshold"), 
				    (record->readable_threshold_upper_critical_threshold ? 
				     SCM_BOOL_T : SCM_BOOL_F));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("readable_lower_non_critical_threshold"), 
				    (record->readable_threshold_lower_non_critical_threshold ? 
				     SCM_BOOL_T : SCM_BOOL_F));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("readable_upper_non_critical_threshold"), 
				    (record->readable_threshold_upper_non_critical_threshold ? 
				     SCM_BOOL_T : SCM_BOOL_F));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("b"), 
				    gh_long2scm (record->b));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("m"), 
				    gh_long2scm (record->m));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("r_exponent"), 
				    gh_long2scm (record->r_exponent));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("b_exponent"), 
				    gh_long2scm (record->b_exponent));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("linear"), 
				    gh_long2scm (record->linear));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("analog_data_format"), 
				    gh_long2scm (record->analog_data_format));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("slave_system_software_id"), 
				    gh_long2scm (record->sensor_owner_id));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("sensor_number"), 
				    gh_long2scm (record->sensor_number));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("sensor_type"), 
				    gh_long2scm (record->sensor_type));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("group_name"), 
				    gh_str02scm (ipmi_get_sensor_group (record->sensor_type)));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("event_reading_type"), 
				    gh_long2scm (record->event_reading_type_code));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("sensor_unit"), 
				    gh_long2scm (record->sensor_unit));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("unit_short_string"), 
				    gh_str02scm (ipmi_sensor_units_abbreviated[record->sensor_unit]));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("unit_string"), 
				    gh_str02scm (ipmi_sensor_units[record->sensor_unit]));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("nominal_reading"), 
				    gh_double2scm (record->nominal_reading));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("normal_min"), 
				    gh_double2scm (record->normal_minimum));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("normal_max"), 
				    gh_double2scm (record->normal_maximum));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("sensor_min_reading"), 
				    gh_double2scm (record->sensor_minimum_reading));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("sensor_max_reading"), 
				    gh_double2scm (record->sensor_maximum_reading));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("negative_hysteresis"), 
				    gh_long2scm (record->negative_going_threshold_hysteresis));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("positive_hysteresis"), 
				    gh_long2scm (record->positive_going_threshold_hysteresis));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("lower_non_recoverable_threshold"), 
				    gh_double2scm (record->lower_non_recoverable_threshold));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("upper_non_recoverable_threshold"), 
				    gh_double2scm (record->upper_non_recoverable_threshold));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("lower_critical_threshold"), 
				    gh_double2scm (record->lower_critical_threshold));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("upper_critical_threshold"), 
				    gh_double2scm (record->upper_critical_threshold));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("lower_non_critical_threshold"), 
				    gh_double2scm (record->lower_non_critical_threshold));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("upper_non_critical_threshold"), 
				    gh_double2scm (record->upper_non_critical_threshold));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("sensor_name"), 
				    gh_str02scm (record->sensor_name));
  
  return scm_sdr_record;
}

static SCM 
get_scm_sdr_compact_record (sdr_compact_record_t *record, 
			    SCM scm_sdr_record)
{
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("slave_system_software_id"), 
				    gh_long2scm (record->sensor_owner_id));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("sensor_number"), 
				    gh_long2scm (record->sensor_number));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("sensor_type"), 
				    gh_long2scm (record->sensor_type));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("group_name"), 
				    gh_str02scm (ipmi_get_sensor_group (record->sensor_type)));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("event_reading_type"), 
				    gh_long2scm (record->event_reading_type_code));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("sensor_unit"), 
				    gh_long2scm (record->sensor_unit));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("unit_short_string"), 
				    gh_str02scm (ipmi_sensor_units_abbreviated[record->sensor_unit]));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("unit_string"), 
				    gh_str02scm (ipmi_sensor_units[record->sensor_unit]));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("negative_hysteresis"), 
				    gh_long2scm (record->negative_going_threshold_hysteresis));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("positive_hysteresis"), 
				    gh_long2scm (record->positive_going_threshold_hysteresis));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("sensor_name"), 
				    gh_str02scm (record->sensor_name));
  
  return scm_sdr_record;
}

static SCM 
get_scm_sdr_event_only_record (sdr_event_only_record_t *record, 
			       SCM scm_sdr_record)
{
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("slave_system_software_id"), 
				    gh_long2scm (record->sensor_owner_id));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("sensor_number"), 
				    gh_long2scm (record->sensor_number));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("sensor_type"), 
				    gh_long2scm (record->sensor_type));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("group_name"), 
				    gh_str02scm (ipmi_get_sensor_group (record->sensor_type)));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("event_reading_type"), 
				    gh_long2scm (record->event_reading_type_code));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("sensor_name"), 
				    gh_str02scm (record->sensor_name));
  
  return scm_sdr_record;
}

static SCM 
get_scm_sdr_entity_association_record (sdr_entity_association_record_t *record, 
				       SCM scm_sdr_record)
{
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("container_entity_id"), 
				    gh_long2scm (record->container_entity_id));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("container_entity_instance"), 
				    gh_long2scm (record->container_entity_instance));
  
  return scm_sdr_record;
}

static SCM 
get_scm_sdr_generic_device_locator_record (sdr_generic_device_locator_record_t *record, 
					   SCM scm_sdr_record)
{
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("direct_access_address"), 
				    gh_long2scm (record->direct_access_address));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("channel_number"), 
				    gh_long2scm (record->channel_number));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("device_slave_address"), 
				    gh_long2scm (record->device_slave_address));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("private_bus_id"), 
				    gh_long2scm (record->private_bus_id));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("lun_master_write_read_command"), 
				    gh_long2scm (record->lun_for_master_write_read_command));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("address_span"), 
				    gh_long2scm (record->address_span));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("device_type"), 
				    gh_long2scm (record->device_type));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("device_type_modifier"), 
				    gh_long2scm (record->device_type_modifier));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("entity_id"), 
				    gh_long2scm (record->entity_id));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("entity_instance"), 
				    gh_long2scm (record->entity_instance));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("device_name"), 
				    gh_str02scm (record->device_name));
  
  return scm_sdr_record;
}

static SCM 
get_scm_sdr_logical_fru_device_locator_record (sdr_logical_fru_device_locator_record_t *record, 
					       SCM scm_sdr_record)
{
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("device_type"), 
				    gh_long2scm (record->device_type));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("device_type_modifier"), 
				    gh_long2scm (record->device_type_modifier));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("fru_entity_id"), 
				    gh_long2scm (record->fru_entity_id));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("fru_entity_instance"), 
				    gh_long2scm (record->fru_entity_instance));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("device_name"), 
				    gh_str02scm (record->device_name));
  
  return scm_sdr_record;
}

static SCM 
get_scm_sdr_management_controller_device_locator_record (sdr_management_controller_device_locator_record_t *record, 
							 SCM scm_sdr_record)
{
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("entity_id"), 
				    gh_long2scm (record->entity_id));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("entity_instance"), 
				    gh_long2scm (record->entity_instance));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("device_name"), 
				    gh_str02scm (record->device_name));
  
  return scm_sdr_record;
}

static SCM 
get_scm_sdr_oem_record (sdr_oem_record_t *record, 
			SCM scm_sdr_record)
{
  char *oem_data = NULL;
  char *tmp_oem_data = NULL;
  int i;
  
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("manufacturer_id"), 
				    gh_long2scm (record->manufacturer_id));
  
  for (i = 0; i < record->oem_data_length; i++)
    {
      tmp_oem_data = oem_data;
      if (oem_data)
	{
	  oem_data = NULL;
	  asprintf (&oem_data, "%s %02X", tmp_oem_data, record->oem_data[i]);
	  free (tmp_oem_data);
	}
      else
	asprintf (&oem_data, "%02X", record->oem_data[i]);
    }
  
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("oem_data"), 
				    gh_str02scm (oem_data));
  
  free (oem_data);
  
  return scm_sdr_record;
}

SCM 
ex_get_sdr_record (SCM scm_record_id)
{
  sdr_record_t sdr_record;
  SCM scm_sdr_record = SCM_EOL;
  uint16_t record_id = 0;
  uint16_t next_record_id = 0;
  
  record_id = gh_scm2long (scm_record_id);
  
  memset (&sdr_record, 0, sizeof (sdr_record_t));
  
  if (get_sdr_record (fi_get_ipmi_device (), 
		      record_id, 
		      &next_record_id, 
		      &sdr_record))
    return SCM_BOOL_F;
  
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("record_id"), 
				    gh_long2scm (sdr_record.record_id));
  
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("record_type"), 
				    gh_long2scm (sdr_record.record_type));
  
  switch (sdr_record.record_type)
    {
    case IPMI_SDR_FORMAT_FULL_RECORD:
      scm_sdr_record = get_scm_sdr_full_record (&(sdr_record.record.sdr_full_record), 
						scm_sdr_record);
      break;
    case IPMI_SDR_FORMAT_COMPACT_RECORD:
      scm_sdr_record = get_scm_sdr_compact_record (&(sdr_record.record.sdr_compact_record), scm_sdr_record);
      break;
    case IPMI_SDR_FORMAT_EVENT_ONLY_RECORD:
      scm_sdr_record = get_scm_sdr_event_only_record (&(sdr_record.record.sdr_event_only_record), scm_sdr_record);
      break;
    case IPMI_SDR_FORMAT_ENTITY_ASSOCIATION_RECORD:
      scm_sdr_record = get_scm_sdr_entity_association_record (&(sdr_record.record.sdr_entity_association_record), scm_sdr_record);
      break;
    case IPMI_SDR_FORMAT_GENERIC_DEVICE_LOCATOR_RECORD:
      scm_sdr_record = get_scm_sdr_generic_device_locator_record (&(sdr_record.record.sdr_generic_device_locator_record), scm_sdr_record);
      break;
    case IPMI_SDR_FORMAT_FRU_DEVICE_LOCATOR_RECORD:
      scm_sdr_record = get_scm_sdr_logical_fru_device_locator_record (&(sdr_record.record.sdr_logical_fru_device_locator_record), scm_sdr_record);
      break;
    case IPMI_SDR_FORMAT_MANAGEMENT_CONTROLLER_DEVICE_LOCATOR_RECORD:
      scm_sdr_record = get_scm_sdr_management_controller_device_locator_record (&(sdr_record.record.sdr_management_controller_device_locator_record), scm_sdr_record);
      break;
    case IPMI_SDR_FORMAT_OEM_RECORD:
      scm_sdr_record = get_scm_sdr_oem_record (&(sdr_record.record.sdr_oem_record), scm_sdr_record);
      break;
    case IPMI_SDR_FORMAT_DEVICE_RELATIVE_ENTITY_ASSOCIATION_RECORD:
    case IPMI_SDR_FORMAT_MANAGEMENT_CONTROLLER_CONFIRMATION_RECORD:
    case IPMI_SDR_FORMAT_BMC_MESAAGE_CHANNEL_INFO_RECORD:
    default:
      {
	fprintf (stderr, 
		 "%s: record_type = %02Xh and record_id = %d not handled.  "
		 "Please report to freeipmi-devel@gnu.org\n", 
		 __PRETTY_FUNCTION__, sdr_record.record_type, sdr_record.record_id);
      }
    }
  
  scm_sdr_record = gh_list (gh_long2scm (next_record_id), 
			    scm_sdr_record, 
			    SCM_UNDEFINED);
  
  return scm_sdr_record;
}

static void 
scm2sdr_full_record (SCM scm_sdr_record, sdr_full_record_t *record)
{
  SCM scm_value;
  char *sensor_name_ptr = NULL;
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("b"));
  record->b = gh_scm2long (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("m"));
  record->m = gh_scm2long (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("r_exponent"));
  record->r_exponent = gh_scm2long (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("b_exponent"));
  record->b_exponent = gh_scm2long (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("linear"));
  record->linear = gh_scm2long (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("analog_data_format"));
  record->analog_data_format = gh_scm2long (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("slave_system_software_id"));
  record->sensor_owner_id = gh_scm2long (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("sensor_number"));
  record->sensor_number = gh_scm2long (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("sensor_type"));
  record->sensor_type = gh_scm2long (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("event_reading_type"));
  record->event_reading_type_code = gh_scm2long (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("sensor_unit"));
  record->sensor_unit = gh_scm2long (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("nominal_reading"));
  record->nominal_reading = gh_scm2double (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("normal_min"));
  record->normal_minimum = gh_scm2double (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("normal_max"));
  record->normal_maximum = gh_scm2double (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("sensor_min_reading"));
  record->sensor_minimum_reading = gh_scm2double (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("sensor_max_reading"));
  record->sensor_maximum_reading = gh_scm2double (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("negative_hysteresis"));
  record->negative_going_threshold_hysteresis = gh_scm2long (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("positive_hysteresis"));
  record->positive_going_threshold_hysteresis = gh_scm2long (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("lower_non_recoverable_threshold"));
  record->lower_non_recoverable_threshold = gh_scm2double (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("upper_non_recoverable_threshold"));
  record->upper_non_recoverable_threshold = gh_scm2double (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("lower_critical_threshold"));
  record->lower_critical_threshold = gh_scm2double (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("upper_critical_threshold"));
  record->upper_critical_threshold = gh_scm2double (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("lower_non_critical_threshold"));
  record->lower_non_critical_threshold = gh_scm2double (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("upper_non_critical_threshold"));
  record->upper_non_critical_threshold = gh_scm2double (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("sensor_name"));
  sensor_name_ptr = gh_scm2newstr (scm_value, NULL);
  strncpy (record->sensor_name, sensor_name_ptr, 16);
  free (sensor_name_ptr);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("readable_lower_critical_threshold"));
  record->readable_threshold_lower_critical_threshold = gh_scm2bool (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("readable_upper_critical_threshold"));
  record->readable_threshold_upper_critical_threshold = gh_scm2bool (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("readable_lower_non_critical_threshold"));
  record->readable_threshold_lower_non_critical_threshold = gh_scm2bool (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("readable_upper_non_critical_threshold"));
  record->readable_threshold_upper_non_critical_threshold = gh_scm2bool (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("readable_lower_non_recoverable_threshold"));
  record->readable_threshold_lower_non_recoverable_threshold = gh_scm2bool (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("readable_upper_non_recoverable_threshold"));
  record->readable_threshold_upper_non_recoverable_threshold = gh_scm2bool (scm_value);
  
  return;
}

static void 
scm2sdr_compact_record (SCM scm_sdr_record, sdr_compact_record_t *record)
{
  SCM scm_value;
  char *sensor_name_ptr = NULL;
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("slave_system_software_id"));
  record->sensor_owner_id = gh_scm2long (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("sensor_number"));
  record->sensor_number = gh_scm2long (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("sensor_type"));
  record->sensor_type = gh_scm2long (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("event_reading_type"));
  record->event_reading_type_code = gh_scm2long (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("sensor_unit"));
  record->sensor_unit = gh_scm2long (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("negative_hysteresis"));
  record->negative_going_threshold_hysteresis = gh_scm2long (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("positive_hysteresis"));
  record->positive_going_threshold_hysteresis = gh_scm2long (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("sensor_name"));
  sensor_name_ptr = gh_scm2newstr (scm_value, NULL);
  strncpy (record->sensor_name, sensor_name_ptr, 16);
  free (sensor_name_ptr);
  
  return;
}

static void 
scm2sdr_event_only_record (SCM scm_sdr_record, sdr_event_only_record_t *record)
{
  SCM scm_value;
  char *sensor_name_ptr = NULL;
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("slave_system_software_id"));
  record->sensor_owner_id = gh_scm2long (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("sensor_number"));
  record->sensor_number = gh_scm2long (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("sensor_type"));
  record->sensor_type = gh_scm2long (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("event_reading_type"));
  record->event_reading_type_code = gh_scm2long (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("sensor_name"));
  sensor_name_ptr = gh_scm2newstr (scm_value, NULL);
  strncpy (record->sensor_name, sensor_name_ptr, 16);
  free (sensor_name_ptr);
  
  return;
}

static void 
scm2sdr_entity_association_record (SCM scm_sdr_record, sdr_entity_association_record_t *record)
{
  SCM scm_value;
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("container_entity_id"));
  record->container_entity_id = gh_scm2long (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("container_entity_instance"));
  record->container_entity_instance = gh_scm2long (scm_value);
  
  return;
}

static void 
scm2sdr_generic_device_locator_record (SCM scm_sdr_record, sdr_generic_device_locator_record_t *record)
{
  SCM scm_value;
  char *device_name_ptr = NULL;
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("direct_access_address"));
  record->direct_access_address = gh_scm2long (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("channel_number"));
  record->channel_number = gh_scm2long (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("device_slave_address"));
  record->device_slave_address = gh_scm2long (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("private_bus_id"));
  record->private_bus_id = gh_scm2long (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("lun_master_write_read_command"));
  record->lun_for_master_write_read_command = gh_scm2long (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("address_span"));
  record->address_span = gh_scm2long (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("device_type"));
  record->device_type = gh_scm2long (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("device_type_modifier"));
  record->device_type_modifier = gh_scm2long (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("entity_id"));
  record->entity_id = gh_scm2long (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("entity_instance"));
  record->entity_instance = gh_scm2long (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("device_name"));
  device_name_ptr = gh_scm2newstr (scm_value, NULL);
  strncpy (record->device_name, device_name_ptr, 16);
  free (device_name_ptr);
  
  return;
}

static void 
scm2sdr_logical_fru_device_locator_record (SCM scm_sdr_record, sdr_logical_fru_device_locator_record_t *record)
{
  SCM scm_value;
  char *device_name_ptr = NULL;
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("device_type"));
  record->device_type = gh_scm2long (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("device_type_modifier"));
  record->device_type_modifier = gh_scm2long (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("fru_entity_id"));
  record->fru_entity_id = gh_scm2long (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("fru_entity_instance"));
  record->fru_entity_instance = gh_scm2long (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("device_name"));
  device_name_ptr = gh_scm2newstr (scm_value, NULL);
  strncpy (record->device_name, device_name_ptr, 16);
  free (device_name_ptr);
  
  return;
}

static void 
scm2sdr_management_controller_device_locator_record (SCM scm_sdr_record, sdr_management_controller_device_locator_record_t *record)
{
  SCM scm_value;
  char *device_name_ptr = NULL;
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("entity_id"));
  record->entity_id = gh_scm2long (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("entity_instance"));
  record->entity_instance = gh_scm2long (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("device_name"));
  device_name_ptr = gh_scm2newstr (scm_value, NULL);
  strncpy (record->device_name, device_name_ptr, 16);
  free (device_name_ptr);
  
  return;
}

static void 
scm2sdr_oem_record (SCM scm_sdr_record, sdr_oem_record_t *record)
{
  SCM scm_value;
  SCM scm_oem_data_list;
  int i;
  char *oem_data_ptr = NULL;
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("manufacturer_id"));
  record->manufacturer_id = gh_scm2long (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("oem_data"));
  scm_oem_data_list = scm_string_split (scm_value, gh_char2scm (' '));
  
  scm_value = scm_length (scm_oem_data_list);
  record->oem_data_length = gh_scm2long (scm_value);
  
  for (i = 0; i < record->oem_data_length; i++)
    {
      scm_value = scm_list_ref (scm_oem_data_list, gh_long2scm (i));
      oem_data_ptr = gh_scm2newstr (scm_value, NULL);
      record->oem_data[i] = strtol (oem_data_ptr, NULL, 16);
      free (oem_data_ptr);
    }
  
  return;
}

static int 
scm2sdr_record (SCM scm_sdr_record, sdr_record_t *sdr_record)
{
  SCM scm_value;
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("record_id"));
  sdr_record->record_id = gh_scm2long (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("record_type"));
  sdr_record->record_type = gh_scm2long (scm_value);
  
  switch (sdr_record->record_type)
    {
    case IPMI_SDR_FORMAT_FULL_RECORD:
      scm2sdr_full_record (scm_sdr_record, &(sdr_record->record.sdr_full_record));
      break;
    case IPMI_SDR_FORMAT_COMPACT_RECORD:
      scm2sdr_compact_record (scm_sdr_record, &(sdr_record->record.sdr_compact_record));
      break;
    case IPMI_SDR_FORMAT_EVENT_ONLY_RECORD:
      scm2sdr_event_only_record (scm_sdr_record, &(sdr_record->record.sdr_event_only_record));
      break;
    case IPMI_SDR_FORMAT_ENTITY_ASSOCIATION_RECORD:
      scm2sdr_entity_association_record (scm_sdr_record, &(sdr_record->record.sdr_entity_association_record));
      break;
    case IPMI_SDR_FORMAT_GENERIC_DEVICE_LOCATOR_RECORD:
      scm2sdr_generic_device_locator_record (scm_sdr_record, &(sdr_record->record.sdr_generic_device_locator_record));
      break;
    case IPMI_SDR_FORMAT_FRU_DEVICE_LOCATOR_RECORD:
      scm2sdr_logical_fru_device_locator_record (scm_sdr_record, &(sdr_record->record.sdr_logical_fru_device_locator_record));
      break;
    case IPMI_SDR_FORMAT_MANAGEMENT_CONTROLLER_DEVICE_LOCATOR_RECORD:
      scm2sdr_management_controller_device_locator_record (scm_sdr_record, &(sdr_record->record.sdr_management_controller_device_locator_record));
      break;
    case IPMI_SDR_FORMAT_OEM_RECORD:
      scm2sdr_oem_record (scm_sdr_record, &(sdr_record->record.sdr_oem_record));
      break;
    case IPMI_SDR_FORMAT_DEVICE_RELATIVE_ENTITY_ASSOCIATION_RECORD:
    case IPMI_SDR_FORMAT_MANAGEMENT_CONTROLLER_CONFIRMATION_RECORD:
    case IPMI_SDR_FORMAT_BMC_MESAAGE_CHANNEL_INFO_RECORD:
    default:
      {
	fprintf (stderr, 
		 "%s: record_type = %02Xh and record_id = %d not handled.  "
		 "Please report to freeipmi-devel@gnu.org\n", 
		 __PRETTY_FUNCTION__, sdr_record->record_type, sdr_record->record_id);
	return -1;
      }
    }
  
  return 0;
}

SCM 
ex_get_sensor_reading (SCM scm_sdr_record)
{
  sensor_reading_t sensor_reading;
  sdr_record_t sdr_record;
  SCM scm_sensor_reading = SCM_EOL;
  int i;
  SCM scm_event_message_list = SCM_EOL;
  
  scm2sdr_record (scm_sdr_record, &sdr_record);
  
  memset (&sensor_reading, 0, sizeof (sensor_reading_t));
  if (get_sensor_reading (fi_get_ipmi_device (), 
			  &sdr_record, 
			  &sensor_reading) != 0)
    return SCM_BOOL_F;
  
  scm_sensor_reading = scm_assoc_set_x (scm_sensor_reading, 
					gh_str02scm ("current_reading"), 
					gh_double2scm (sensor_reading.current_reading));
  scm_sensor_reading = scm_assoc_set_x (scm_sensor_reading, 
					gh_str02scm ("reading_availability_flag"), 
					(sensor_reading.reading_state ? 
					 SCM_BOOL_F : SCM_BOOL_T));
  scm_sensor_reading = scm_assoc_set_x (scm_sensor_reading, 
					gh_str02scm ("sensor_scanning_flag"), 
					(sensor_reading.sensor_scanning ? 
					 SCM_BOOL_T : SCM_BOOL_F));
  scm_sensor_reading = scm_assoc_set_x (scm_sensor_reading, 
					gh_str02scm ("event_messages_flag"), 
					(sensor_reading.event_messages_flag ? 
					 SCM_BOOL_T : SCM_BOOL_F));
  if (sensor_reading.event_message_list != NULL)
    {
      for (i = 0; 
	   sensor_reading.event_message_list[i]; 
	   i++)
	{
	  scm_event_message_list = 
	    gh_append2 (scm_event_message_list, 
			gh_list (gh_str02scm (sensor_reading.event_message_list[i]), 
				 SCM_UNDEFINED));
	}
    }
  scm_sensor_reading = scm_assoc_set_x (scm_sensor_reading, 
					gh_str02scm ("event_message_list"), 
					scm_event_message_list);
  
  if (sensor_reading.event_message_list != NULL)
    {
      for (i = 0; sensor_reading.event_message_list[i]; i++)
	free (sensor_reading.event_message_list[i]);
      free (sensor_reading.event_message_list);
    }
  
  return scm_sensor_reading;
}

SCM 
ex_get_sdr_cache_filename ()
{
  return gh_str02scm (get_sdr_cache_filename ());
}

SCM 
ex_get_sdr_repository_info ()
{
  SCM scm_repository_info_list = SCM_EOL;
  
  fiid_obj_t cmd_rs = NULL;
  
  char version_string[17];
  uint8_t sdr_major_version;
  uint8_t sdr_minor_version;
  uint16_t record_count;
  uint64_t val;
  
  /* get_repository_info */
  if (!(cmd_rs = fiid_obj_create(tmpl_cmd_get_sdr_repository_info_rs)))
    goto cleanup;

  if (ipmi_cmd_get_sdr_repository_info (fi_get_ipmi_device (), cmd_rs) != 0)
    {
      ipmi_error (cmd_rs, 
                  (fi_get_ipmi_device ())->net_fn,
                  "ipmi_cmd_get_sdr_repository_info2()");
      goto cleanup;
    }
  
  if (fiid_obj_get (cmd_rs, "comp_code", &val) < 0)
    goto cleanup;
  if (val != 0)
    return SCM_EOL;
  
  /* appending sdr version */
  if (fiid_obj_get (cmd_rs, "sdr_version_major", &val) < 0)
    goto cleanup;
  sdr_major_version = val;

  if (fiid_obj_get (cmd_rs, "sdr_version_minor", &val) < 0)
    goto cleanup;
  sdr_minor_version = val;
  snprintf (version_string, 17, 
	    "%d.%d", 
	    sdr_major_version, sdr_minor_version);
  scm_repository_info_list = scm_assoc_set_x (scm_repository_info_list, 
                                              gh_str02scm ("sdr_version"), 
                                              gh_str02scm (version_string));
  
  if (fiid_obj_get (cmd_rs, "record_count", &val) < 0)
    goto cleanup;
  record_count = val;

  scm_repository_info_list = scm_assoc_set_x (scm_repository_info_list, 
                                              gh_str02scm ("record_count"), 
                                              gh_long2scm (record_count));
  
  if (fiid_obj_get (cmd_rs, "free_space", &val) < 0)
    goto cleanup;
  scm_repository_info_list = scm_assoc_set_x (scm_repository_info_list, 
                                              gh_str02scm ("free_space"), 
                                              gh_long2scm (val));
  
  if (fiid_obj_get (cmd_rs, "most_recent_addition_timestamp", &val) < 0)
    goto cleanup;
  scm_repository_info_list = scm_assoc_set_x (scm_repository_info_list, 
                                              gh_str02scm ("recent_addition_timestamp"), 
                                              gh_ulong2scm (val));
  
  if (fiid_obj_get (cmd_rs, "most_recent_erase_timestamp", &val) < 0)
    goto cleanup;
  scm_repository_info_list = scm_assoc_set_x (scm_repository_info_list, 
                                              gh_str02scm ("recent_erase_timestamp"), 
                                              gh_ulong2scm (val));
  
  fiid_obj_destroy(cmd_rs);
  return (scm_repository_info_list);

 cleanup:
  if (cmd_rs)
    fiid_obj_destroy(cmd_rs);
  return (SCM_EOL);
}

SCM 
ex_get_bmc_info ()
{
  SCM scm_bmc_info_list = SCM_EOL;
  
  fiid_obj_t cmd_rs = NULL;
  uint64_t val = 0;
  
  if (!(cmd_rs = fiid_obj_create(tmpl_cmd_get_device_id_rs)))
    goto cleanup;

  if (ipmi_cmd_get_device_id (fi_get_ipmi_device (), cmd_rs) != 0)
    {
      ipmi_error (cmd_rs, 
                  (fi_get_ipmi_device ())->net_fn,
                  "ipmi_cmd_get_device_id()");
      goto cleanup;
    }
  
  if (fiid_obj_get (cmd_rs, "device_id", &val) < 0)
    goto cleanup;

  scm_bmc_info_list = scm_assoc_set_x (scm_bmc_info_list, 
				       gh_str02scm ("dev_id"), 
				       gh_long2scm ((unsigned int) val));
  
  if (fiid_obj_get (cmd_rs, "device_revision.revision", &val) < 0)
    goto cleanup;
  scm_bmc_info_list = scm_assoc_set_x (scm_bmc_info_list, 
				       gh_str02scm ("dev_revision"), 
				       gh_long2scm ((unsigned int) val));
  
  if (fiid_obj_get (cmd_rs, "device_revision.sdr_support", &val) < 0)
    goto cleanup;
  scm_bmc_info_list = scm_assoc_set_x (scm_bmc_info_list, 
				       gh_str02scm ("sdr_support"), 
				       gh_bool2scm ((unsigned int) val));
  
  {
    char version_string[17];
    uint64_t major, minor;
    
    if (fiid_obj_get (cmd_rs, "firmware_revision1.major_revision", &major) < 0)
      goto cleanup;

    if (fiid_obj_get (cmd_rs, "firmware_revision2.minor_revision", &minor) < 0)
      goto cleanup;

    snprintf (version_string, 17, 
	      "%d.%d", 
	      (int) major, (int) minor);
    scm_bmc_info_list = scm_assoc_set_x (scm_bmc_info_list, 
					 gh_str02scm ("firmware_revision"), 
					 gh_str02scm (version_string));
  }
  
  if (fiid_obj_get (cmd_rs, "firmware_revision1.dev_available", &val) < 0)
    goto cleanup;

  scm_bmc_info_list = scm_assoc_set_x (scm_bmc_info_list, 
				       gh_str02scm ("dev_availability"), 
				       gh_bool2scm ((unsigned int) val));
  {
    char version_string[17];
    uint64_t major, minor;
    
    if (fiid_obj_get (cmd_rs, "ipmi_version.ms_bits", &major) < 0)
      goto cleanup;

    if (fiid_obj_get (cmd_rs, "ipmi_version.ls_bits", &minor) < 0)
      goto cleanup;

    snprintf (version_string, 17, 
	      "%d.%d", 
	      (int) major, (int) minor);
    scm_bmc_info_list = scm_assoc_set_x (scm_bmc_info_list, 
					 gh_str02scm ("ipmi_version"), 
					 gh_str02scm (version_string));
  }
  
  if (fiid_obj_get (cmd_rs, "additional_device_support.sensor_device", &val) < 0)
    goto cleanup;
  scm_bmc_info_list = scm_assoc_set_x (scm_bmc_info_list, 
				       gh_str02scm ("sensor_dev_support"), 
				       gh_bool2scm ((unsigned int) val));
  
  if (fiid_obj_get (cmd_rs, "additional_device_support.sdr_repository_device", &val) < 0)
    goto cleanup;
  scm_bmc_info_list = scm_assoc_set_x (scm_bmc_info_list, 
				       gh_str02scm ("sdr_repo_dev_support"), 
				       gh_bool2scm ((unsigned int) val));

  if (fiid_obj_get (cmd_rs, "additional_device_support.sel_device", &val) < 0)
    goto cleanup;
  scm_bmc_info_list = scm_assoc_set_x (scm_bmc_info_list, 
				       gh_str02scm ("sel_dev_support"), 
				       gh_bool2scm ((unsigned int) val));
  
  if (fiid_obj_get (cmd_rs, "additional_device_support.fru_inventory_device", &val) < 0)
    goto cleanup;
  scm_bmc_info_list = scm_assoc_set_x (scm_bmc_info_list, 
				       gh_str02scm ("fru_inventory_dev_support"), 
				       gh_bool2scm ((unsigned int) val));
  
  if (fiid_obj_get (cmd_rs, "additional_device_support.ipmb_event_receiver", &val) < 0)
    goto cleanup;
  scm_bmc_info_list = scm_assoc_set_x (scm_bmc_info_list, 
				       gh_str02scm ("ipmb_event_receiver_support"), 
				       gh_bool2scm ((unsigned int) val));
  
  if (fiid_obj_get (cmd_rs, "additional_device_support.ipmb_event_generator", &val) < 0)
    goto cleanup;
  scm_bmc_info_list = scm_assoc_set_x (scm_bmc_info_list, 
				       gh_str02scm ("ipmb_event_generator_support"), 
				       gh_bool2scm ((unsigned int) val));
  
  if (fiid_obj_get (cmd_rs, "additional_device_support.bridge", &val) < 0)
    goto cleanup;
  scm_bmc_info_list = scm_assoc_set_x (scm_bmc_info_list, 
				       gh_str02scm ("bridge_support"), 
				       gh_bool2scm ((unsigned int) val));
  
  if (fiid_obj_get (cmd_rs, "additional_device_support.chassis_device", &val) < 0)
    goto cleanup;
  scm_bmc_info_list = scm_assoc_set_x (scm_bmc_info_list, 
				       gh_str02scm ("chassis_dev_support"), 
				       gh_bool2scm ((unsigned int) val));
  
  if (fiid_obj_get (cmd_rs, "manufacturer_id.id", &val) < 0)
    goto cleanup;
  scm_bmc_info_list = scm_assoc_set_x (scm_bmc_info_list, 
				       gh_str02scm ("manufacturer_id"), 
				       gh_long2scm ((unsigned int) val));
  
  if (fiid_obj_get (cmd_rs, "product_id", &val) < 0)
    goto cleanup;
  scm_bmc_info_list = scm_assoc_set_x (scm_bmc_info_list, 
				       gh_str02scm ("product_id"), 
				       gh_long2scm ((unsigned int) val));
  
  if (fiid_obj_get (cmd_rs, "auxiliary_firmware_revision_info", &val) < 0)
    goto cleanup;
  scm_bmc_info_list = scm_assoc_set_x (scm_bmc_info_list, 
				       gh_str02scm ("aux_firmware_rev_info"), 
				       gh_long2scm ((unsigned int) val));
  
  fiid_obj_destroy(cmd_rs);
  return scm_bmc_info_list;

 cleanup:
  if (cmd_rs)
    fiid_obj_destroy(cmd_rs);
  return (SCM_EOL);
}

SCM 
ex_ipmi_open (SCM scm_arg_list)
{
  SCM scm_value;
  struct arguments args;
  
  scm_value = scm_list_ref (scm_arg_list, gh_long2scm (0));
  if (scm_boolean_p (scm_value) == SCM_BOOL_T)
    args.common.disable_auto_probe = 0;
  else 
    args.common.disable_auto_probe = gh_scm2int (scm_value);
  
  scm_value = scm_list_ref (scm_arg_list, gh_long2scm (1));
  if (scm_boolean_p (scm_value) == SCM_BOOL_T)
    args.common.driver_type = IPMI_DEVICE_UNKNOWN;
  else 
    args.common.driver_type = gh_scm2int (scm_value);
  
  scm_value = scm_list_ref (scm_arg_list, gh_long2scm (2));
  if (scm_boolean_p (scm_value) == SCM_BOOL_T)
    args.common.driver_address = 0;
  else 
    args.common.driver_address = gh_scm2int (scm_value);
  
  scm_value = scm_list_ref (scm_arg_list, gh_long2scm (3));
  if (scm_boolean_p (scm_value) == SCM_BOOL_T)
    args.common.driver_device = NULL;
  else 
    args.common.driver_device = gh_scm2newstr (scm_value, NULL);
  
  scm_value = scm_list_ref (scm_arg_list, gh_long2scm (4));
  if (scm_boolean_p (scm_value) == SCM_BOOL_T)
    args.common.session_timeout = 0;
  else 
    args.common.session_timeout = gh_scm2int (scm_value);

  scm_value = scm_list_ref (scm_arg_list, gh_long2scm (4));
  if (scm_boolean_p (scm_value) == SCM_BOOL_T)
    args.common.retry_timeout = 0;
  else 
    args.common.retry_timeout = gh_scm2int (scm_value);
  
  scm_value = scm_list_ref (scm_arg_list, gh_long2scm (6));
  if (scm_boolean_p (scm_value) == SCM_BOOL_T)
    args.common.host = NULL;
  else 
    args.common.host = gh_scm2newstr (scm_value, NULL);
  
  scm_value = scm_list_ref (scm_arg_list, gh_long2scm (7));
  if (scm_boolean_p (scm_value) == SCM_BOOL_T)
    args.common.username = NULL;
  else 
    args.common.username = gh_scm2newstr (scm_value, NULL);
  
  scm_value = scm_list_ref (scm_arg_list, gh_long2scm (8));
  if (scm_boolean_p (scm_value) == SCM_BOOL_T)
    args.common.password = NULL;
  else 
    args.common.password = gh_scm2newstr (scm_value, NULL);
  
  scm_value = scm_list_ref (scm_arg_list, gh_long2scm (9));
  if (scm_boolean_p (scm_value) == SCM_BOOL_T)
    args.common.authentication_type = IPMI_AUTHENTICATION_TYPE_NONE;
  else 
    args.common.authentication_type = gh_scm2int (scm_value);
  
  scm_value = scm_list_ref (scm_arg_list, gh_long2scm (10));
  if (scm_boolean_p (scm_value) == SCM_BOOL_T)
    args.common.privilege_level = IPMI_PRIVILEGE_LEVEL_USER;
  else 
    args.common.privilege_level = gh_scm2int (scm_value);
  
  /*   scm_value = scm_list_ref (scm_arg_list, gh_long2scm (9)); */
  /*   if (scm_boolean_p (scm_value) == SCM_BOOL_T) */
  /*     args.script_file = 0; */
  /*   else  */
  /*     args.script_file = gh_scm2newstr (scm_value, NULL); */
  args.script_file = NULL;
  
  fi_set_arguments (&args);
  
  if (fi_ipmi_open (&args) == 0)
    {
      xfree (args.common.driver_device);
      xfree (args.common.host);
      xfree (args.common.username);
      xfree (args.common.password);
      return SCM_BOOL_T;
    }
  else 
    return SCM_BOOL_F;
}

SCM 
ex_ipmi_close ()
{
  if (fi_ipmi_close () == 0)
    return SCM_BOOL_T;
  else 
    return SCM_BOOL_F;
}

SCM 
ex_get_pef_info ()
{
  SCM scm_pef_info_list = SCM_EOL;
  
  fiid_obj_t cmd_rs = NULL;
  char version_string[17];
  uint8_t pef_major_version;
  uint8_t pef_minor_version;
  
  uint64_t val;
  uint8_t alert_support = 0;
  
  if (!(cmd_rs = fiid_obj_create(tmpl_cmd_get_pef_capabilities_rs)))
    goto cleanup;

  if (ipmi_cmd_get_pef_capabilities (fi_get_ipmi_device (), cmd_rs) != 0)
    {
      ipmi_error (cmd_rs, 
                  (fi_get_ipmi_device ())->net_fn,
                  "ipmi_cmd_get_pef_capabilities2()");
      goto cleanup;
    }
  
  if (fiid_obj_get (cmd_rs, "pef_version_major", &val) < 0)
    goto cleanup;
  pef_major_version = val;
  if (fiid_obj_get (cmd_rs, "pef_version_minor", &val) < 0)
    goto cleanup;
  pef_minor_version = val;
  snprintf (version_string, 17, 
	    "%d.%d", 
	    pef_major_version, pef_minor_version);
  scm_pef_info_list = scm_assoc_set_x (scm_pef_info_list, 
				       gh_str02scm ("pef_version"), 
				       gh_str02scm (version_string));
  
  if (fiid_obj_get (cmd_rs, "action_support.alert", &val) < 0)
    goto cleanup;
  alert_support = val;
  scm_pef_info_list = scm_assoc_set_x (scm_pef_info_list, 
				       gh_str02scm ("alert_support"), 
				       gh_bool2scm (val));
  
  if (fiid_obj_get (cmd_rs, "action_support.power_down", &val) < 0)
    goto cleanup;
  scm_pef_info_list = scm_assoc_set_x (scm_pef_info_list, 
				       gh_str02scm ("powerdown_support"), 
				       gh_bool2scm (val));
  
  if (fiid_obj_get (cmd_rs, "action_support.reset", &val) < 0)
    goto cleanup;
  scm_pef_info_list = scm_assoc_set_x (scm_pef_info_list, 
				       gh_str02scm ("reset_support"), 
				       gh_bool2scm (val));
  
  if (fiid_obj_get (cmd_rs, "action_support.power_cycle", &val) < 0)
    goto cleanup;
  scm_pef_info_list = scm_assoc_set_x (scm_pef_info_list, 
				       gh_str02scm ("powercycle_support"), 
				       gh_bool2scm (val));
  
  if (fiid_obj_get (cmd_rs, "action_support.oem_action", &val) < 0)
    goto cleanup;
  scm_pef_info_list = scm_assoc_set_x (scm_pef_info_list, 
				       gh_str02scm ("oem_support"), 
				       gh_bool2scm (val));
  
  if (fiid_obj_get (cmd_rs, "action_support.diagnostic_interrupt", &val) < 0)
    goto cleanup;
  scm_pef_info_list = scm_assoc_set_x (scm_pef_info_list, 
				       gh_str02scm ("diag_interrupt_support"), 
				       gh_bool2scm (val));
  
  if (fiid_obj_get (cmd_rs, "number_of_event_filter_table_entries", &val) < 0)
    goto cleanup;
  scm_pef_info_list = scm_assoc_set_x (scm_pef_info_list, 
				       gh_str02scm ("eft_entries_count"), 
				       gh_ulong2scm (val));
  
  fiid_obj_destroy(cmd_rs);
  cmd_rs = NULL;

  if (alert_support)
    {
      if (!(cmd_rs = fiid_obj_create(tmpl_cmd_get_pef_configuration_parameters_number_of_event_filters_rs)))
        goto cleanup;

      if (ipmi_cmd_get_pef_configuration_parameters_number_of_event_filters (fi_get_ipmi_device (), 
									     IPMI_GET_PEF_PARAMETER, 
									     SET_SELECTOR, 
									     BLOCK_SELECTOR, 
									     cmd_rs) != 0)
	{
	  ipmi_error (cmd_rs, 
                      (fi_get_ipmi_device ())->net_fn,
                      "ipmi_cmd_get_pef_configuration_parameters_number_of_event_filters2()");
          goto cleanup;
	}

      if (fiid_obj_get (cmd_rs, "number_of_event_filters", &val) < 0)
        goto cleanup;

      fiid_obj_destroy(cmd_rs);
      cmd_rs = NULL;
    }
  else 
    {
      val = 0;
    }
  scm_pef_info_list = scm_assoc_set_x (scm_pef_info_list, 
				       gh_str02scm ("num_event_filters"), 
				       (val ? gh_ulong2scm (val) : SCM_BOOL_F));
  
  if (alert_support)
    {
      if (!(cmd_rs = fiid_obj_create(tmpl_cmd_get_pef_configuration_parameters_number_of_alert_policy_entries_rs)))
        goto cleanup;

      if (ipmi_cmd_get_pef_configuration_parameters_number_of_alert_policy_entries (fi_get_ipmi_device (), 
										    IPMI_GET_PEF_PARAMETER, 
										    SET_SELECTOR, 
										    BLOCK_SELECTOR, 
										    cmd_rs) != 0)
	{
	  ipmi_error (cmd_rs, 
                      (fi_get_ipmi_device ())->net_fn,
                      "ipmi_cmd_get_pef_configuration_parameters_number_of_alert_policy_entries2()");
          goto cleanup;
	}
      
      if (fiid_obj_get (cmd_rs, "number_of_alert_policy_entries", &val) < 0)
        goto cleanup;

      fiid_obj_destroy(cmd_rs);
      cmd_rs = NULL;
    }
  else 
    {
      val = 0;
    }
  scm_pef_info_list = scm_assoc_set_x (scm_pef_info_list, 
				       gh_str02scm ("num_alert_policies"), 
				       (val ? gh_ulong2scm (val) : SCM_BOOL_F));
  
  if (alert_support)
    {
      if (!(cmd_rs = fiid_obj_create(tmpl_cmd_get_pef_configuration_parameters_number_of_alert_strings_rs)))
        goto cleanup;

      if (ipmi_cmd_get_pef_configuration_parameters_number_of_alert_strings (fi_get_ipmi_device (), 
									     IPMI_GET_PEF_PARAMETER, 
									     SET_SELECTOR, 
									     BLOCK_SELECTOR, 
									     cmd_rs) != 0)
	{
	  ipmi_error (cmd_rs, 
                      (fi_get_ipmi_device ())->net_fn,
                      "ipmi_cmd_get_pef_configuration_parameters_number_of_alert_strings2()");
          goto cleanup;
	}

      if (fiid_obj_get (cmd_rs, "number_of_alert_strings", &val) < 0)
        goto cleanup;

      fiid_obj_destroy(cmd_rs);
      cmd_rs = NULL;
    }
  else 
    {
      val = 0;
    }
  scm_pef_info_list = scm_assoc_set_x (scm_pef_info_list, 
				       gh_str02scm ("num_alert_strings"), 
				       (val ? gh_ulong2scm (val) : SCM_BOOL_F));
  
  fiid_obj_destroy(cmd_rs);
  return (scm_pef_info_list);

 cleanup:
  if (cmd_rs)
    fiid_obj_destroy(cmd_rs);
  return (SCM_BOOL_F);
}

SCM 
ex_string2number (SCM scm_string)
{
  char *str = NULL;
  int value = 0;
  char *tail = NULL;
  int errnum = 0;
  
  str = gh_scm2newstr (scm_string, NULL);
  errno = 0;
  value = strtol (str, &tail, 0);
  errnum = errno;
  
  if (errnum)
    {
      // overflow
      free (str);
      return SCM_BOOL_F;
    }
  
  if (tail[0] != '\0')
    {
      // invalid integer format
      free (str);
      return SCM_BOOL_F;
    }
  
  free (str);
  return (gh_long2scm (value));
}
