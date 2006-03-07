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

#define SEL_HEX_RECORD_SIZE 128

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
ex_cmd_get_device_id_display (void)
{
  if (display_get_device_id () == 0)
    return (SCM_BOOL_T);
  
  return (SCM_BOOL_F);
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
ex_get_default_sdr_repository_cache_filename (void)
{
  return (gh_str02scm (FI_DEFAULT_SDR_REPOSITORY_CACHE_FILENAME));
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

SCM
ex_sel_get_first_entry_raw ()
{
  uint8_t record_data[SEL_RECORD_SIZE];
  SCM scm_sel_record = SCM_EOL;
  uint32_t record_data_len;

  record_data_len = SEL_RECORD_SIZE;
  if (ipmi_sel_get_first_entry (fi_get_ipmi_device (), 
				fi_get_seld (), 
				record_data,
                                &record_data_len) == 0)
    {
      int i;
      for (i = SEL_RECORD_SIZE - 1; i >= 0; i--)
        scm_sel_record = gh_cons (gh_ulong2scm (record_data[i]), scm_sel_record);
    }
  else 
    {
      fprintf (stderr, "%s\n", 
	       (fi_get_ipmi_device ())->errmsg);
    }
  return scm_sel_record;
}

SCM
ex_sel_get_next_entry_raw ()
{
  uint8_t record_data[SEL_RECORD_SIZE];
  SCM scm_sel_record = SCM_EOL;
  uint32_t record_data_len;

  record_data_len = SEL_RECORD_SIZE;  
  if (ipmi_sel_get_next_entry (fi_get_ipmi_device (), 
			       fi_get_seld (), 
                               record_data,
                               &record_data_len) == 0)
    {
      int i;
      for (i = SEL_RECORD_SIZE - 1; i >= 0; i--)
        scm_sel_record = gh_cons (gh_ulong2scm (record_data[i]), scm_sel_record);
    }
  else 
    {
      fprintf (stderr, "%s\n", 
	       (fi_get_ipmi_device ())->errmsg);
    }
  return scm_sel_record;
}

SCM
ex_sel_get_first_entry_hex ()
{
  uint8_t record_data [SEL_RECORD_SIZE];
  uint8_t hex_data [SEL_HEX_RECORD_SIZE];
  uint32_t record_data_len;

  record_data_len = SEL_RECORD_SIZE;
  if (ipmi_sel_get_first_entry (fi_get_ipmi_device (), 
				fi_get_seld (), 
				record_data, 
                                &record_data_len) == 0)
    {
      snprintf ((char *)hex_data, SEL_HEX_RECORD_SIZE,
                "RID:[%02X][%02X] RT:[%02X] TS:[%02X][%02X][%02X][%02X] "
                "GID:[%02X][%02X] ER:[%02X] ST:[%02X] SN:[%02X] EDIR:[%02X] "
                "ED1: [%02X] ED2: [%02X] ED3: [%02X]\n",
                record_data[0], record_data[1], record_data[2], record_data[3], 
                record_data[4], record_data[5], record_data[6], record_data[7], 
                record_data[8], record_data[9], record_data[10], record_data[11], 
                record_data[12], record_data[13], record_data[14], record_data[15]);
      return gh_str02scm ((char *)hex_data);
    }
  else 
    {
      fprintf (stderr, "%s\n", 
	       (fi_get_ipmi_device ())->errmsg);
    }
  return SCM_BOOL_F;
}

SCM
ex_sel_get_next_entry_hex ()
{
  uint8_t record_data [SEL_RECORD_SIZE];
  uint8_t hex_data [SEL_HEX_RECORD_SIZE];
  uint32_t record_data_len;

  record_data_len = SEL_RECORD_SIZE;
  if (ipmi_sel_get_next_entry (fi_get_ipmi_device (), 
			       fi_get_seld (), 
                               record_data, 
                               &record_data_len) == 0)
    {
      snprintf ((char *)hex_data, SEL_HEX_RECORD_SIZE,
                "RID:[%02X][%02X] RT:[%02X] TS:[%02X][%02X][%02X][%02X] "
                "GID:[%02X][%02X] ER:[%02X] ST:[%02X] SN:[%02X] EDIR:[%02X] "
                "ED1: [%02X] ED2: [%02X] ED3: [%02X]\n",
                record_data[0], record_data[1], record_data[2], record_data[3], 
                record_data[4], record_data[5], record_data[6], record_data[7], 
                record_data[8], record_data[9], record_data[10], record_data[11], 
                record_data[12], record_data[13], record_data[14], record_data[15]);
      return gh_str02scm ((char *)hex_data);
    }
  else 
    {
      fprintf (stderr, "%s\n", 
	       (fi_get_ipmi_device ())->errmsg);
    }
  return SCM_BOOL_F;
}

SCM 
ex_sel_get_info_binary ()
{
  sel_info_t info;
  
  if (get_sel_info (fi_get_ipmi_device (), &info) == 0)
    {
      SCM tail = SCM_EOL;
      
      tail = gh_cons (info.flags & get_sel_alloc_info_cmd_support ? SCM_BOOL_T : SCM_BOOL_F, tail);
      tail = gh_cons (info.flags & reserve_sel_cmd_support ? SCM_BOOL_T : SCM_BOOL_F, tail);
      tail = gh_cons (info.flags & partial_add_sel_entry_cmd_support ? SCM_BOOL_T : SCM_BOOL_F, tail);
      tail = gh_cons (info.flags & delete_sel_cmd_support ? SCM_BOOL_T : SCM_BOOL_F, tail);
      tail = gh_cons (info.flags & overflow_flag ? SCM_BOOL_T : SCM_BOOL_F, tail);
      
      return gh_cons (gh_ulong2scm (info.version_major),
                      gh_cons (gh_ulong2scm (info.version_minor),
                               gh_cons (gh_ulong2scm (info.entry_count),
                                        gh_cons (gh_ulong2scm (info.last_add_time),
                                                 gh_cons (gh_ulong2scm (info.last_erase_time),
                                                          gh_cons (gh_ulong2scm (info.free_space),
                                                                   tail))))));
    }
  else 
    {
      fprintf (stderr, "%s\n", 
	       (fi_get_ipmi_device ())->errmsg);
    }
  return SCM_BOOL_F;
}

SCM 
ex_sel_get_first_entry ()
{
  uint8_t record_data[SEL_RECORD_SIZE];
  sel_record_t sel_rec;
  SCM scm_sel_record = SCM_EOL;
  int32_t record_data_len;

  record_data_len = SEL_RECORD_SIZE;
  if (ipmi_sel_get_first_entry (fi_get_ipmi_device (), 
				fi_get_seld (), 
				record_data, 
                                &record_data_len) != 0)
    {
      fprintf (stderr, "%s\n", 
	       (fi_get_ipmi_device ())->errmsg);
      return SCM_EOL;
    }
  
  if (get_sel_record (record_data, record_data_len, &sel_rec) != 0)
    return SCM_EOL;
  
  scm_sel_record = gh_list (gh_long2scm (sel_rec.record_id), SCM_UNDEFINED);
  scm_sel_record = gh_append2 (scm_sel_record, 
			       gh_list (gh_str02scm (sel_rec.timestamp), 
					SCM_UNDEFINED));
  scm_sel_record = gh_append2 (scm_sel_record, 
			       gh_list (gh_str02scm (sel_rec.sensor_info), 
					SCM_UNDEFINED));
  scm_sel_record = gh_append2 (scm_sel_record, 
			       gh_list (gh_str02scm (sel_rec.event_message), 
					SCM_UNDEFINED));
  scm_sel_record = gh_append2 (scm_sel_record, 
			       gh_list (gh_str02scm (sel_rec.event_data2_message), 
					SCM_UNDEFINED));
  scm_sel_record = gh_append2 (scm_sel_record, 
			       gh_list (gh_str02scm (sel_rec.event_data3_message), 
					SCM_UNDEFINED));
  
  if (sel_rec.timestamp) free (sel_rec.timestamp);
  if (sel_rec.sensor_info) free (sel_rec.sensor_info);
  if (sel_rec.event_message) free (sel_rec.event_message);
  if (sel_rec.event_data2_message) free (sel_rec.event_data2_message);
  if (sel_rec.event_data3_message) free (sel_rec.event_data3_message);
  
  return scm_sel_record;
}

SCM 
ex_sel_get_next_entry ()
{
  uint8_t record_data[SEL_RECORD_SIZE];
  sel_record_t sel_rec;
  SCM scm_sel_record = SCM_EOL;
  int32_t record_data_len;

  record_data_len = SEL_RECORD_SIZE;
  if (ipmi_sel_get_next_entry (fi_get_ipmi_device (), 
			       fi_get_seld (), 
			       record_data, 
                               &record_data_len) != 0)
    {
      fprintf (stderr, "%s\n", 
	       (fi_get_ipmi_device ())->errmsg);
      return SCM_EOL;
    }
  
  if (get_sel_record (record_data, record_data_len, &sel_rec) != 0)
    return SCM_EOL;
  
  scm_sel_record = gh_list (gh_long2scm (sel_rec.record_id), SCM_UNDEFINED);
  scm_sel_record = gh_append2 (scm_sel_record, 
			       gh_list (gh_str02scm (sel_rec.timestamp), 
					SCM_UNDEFINED));
  scm_sel_record = gh_append2 (scm_sel_record, 
			       gh_list (gh_str02scm (sel_rec.sensor_info), 
					SCM_UNDEFINED));
  scm_sel_record = gh_append2 (scm_sel_record, 
			       gh_list (gh_str02scm (sel_rec.event_message), 
					SCM_UNDEFINED));
  scm_sel_record = gh_append2 (scm_sel_record, 
			       gh_list (gh_str02scm (sel_rec.event_data2_message), 
					SCM_UNDEFINED));
  scm_sel_record = gh_append2 (scm_sel_record, 
			       gh_list (gh_str02scm (sel_rec.event_data3_message), 
					SCM_UNDEFINED));
  
  if (sel_rec.timestamp) free (sel_rec.timestamp);
  if (sel_rec.sensor_info) free (sel_rec.sensor_info);
  if (sel_rec.event_message) free (sel_rec.event_message);
  if (sel_rec.event_data2_message) free (sel_rec.event_data2_message);
  if (sel_rec.event_data3_message) free (sel_rec.event_data3_message);
  
  return scm_sel_record;
}

SCM 
ex_sel_delete_entry (SCM scm_record_id)
{
  uint16_t record_id;
  fiid_obj_t obj_cmd_rs;
  uint16_t reservation_id;
  uint64_t val;
  
  record_id = gh_scm2long (scm_record_id);
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_reserve_sel_rs)))
    goto cleanup;

  if (ipmi_cmd_reserve_sel (fi_get_ipmi_device (), 
			    obj_cmd_rs) != 0)
    {
      char errmsg[IPMI_ERR_STR_MAX_LEN] = { 0 };
      
      ipmi_strerror_cmd_r (obj_cmd_rs, 
                           (fi_get_ipmi_device ())->net_fn,
                           errmsg, 
                           IPMI_ERR_STR_MAX_LEN);
      fprintf (stderr, "%s\n", 
	       errmsg);
      goto cleanup;
    }
  
  if (fiid_obj_get (obj_cmd_rs, "reservation_id", &val) < 0)
    goto cleanup;
  reservation_id = val;
  
  fiid_obj_destroy(obj_cmd_rs);
  obj_cmd_rs = NULL;

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_delete_sel_entry_rs)))
    goto cleanup;

  if (ipmi_cmd_delete_sel_entry (fi_get_ipmi_device (), 
				 reservation_id, 
				 record_id, 
				 obj_cmd_rs) != 0)
    {
      char errmsg[IPMI_ERR_STR_MAX_LEN] = { 0 };
      
      ipmi_strerror_cmd_r (obj_cmd_rs, 
                           (fi_get_ipmi_device ())->net_fn,
                           errmsg, 
                           IPMI_ERR_STR_MAX_LEN);
      fprintf (stderr, "%s\n", 
	       errmsg);
      goto cleanup;
    }
  
  fiid_obj_destroy(obj_cmd_rs);
  return SCM_BOOL_T;

 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return SCM_BOOL_F;
}

SCM 
ex_sel_clear ()
{
  fiid_obj_t obj_cmd_rs;
  uint16_t reservation_id;
  uint64_t val;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_reserve_sel_rs)))
    goto cleanup;

  if (ipmi_cmd_reserve_sel (fi_get_ipmi_device (), 
			    obj_cmd_rs) != 0)
    {
      char errmsg[IPMI_ERR_STR_MAX_LEN] = { 0 };
      
      ipmi_strerror_cmd_r (obj_cmd_rs, 
                           (fi_get_ipmi_device ())->net_fn,
                           errmsg, 
                           IPMI_ERR_STR_MAX_LEN);
      fprintf (stderr, "%s\n", 
	       errmsg);
      goto cleanup;
    }
  
  if (fiid_obj_get (obj_cmd_rs, "reservation_id", &val) < 0)
    goto cleanup;
  reservation_id = val;
  
  fiid_obj_destroy(obj_cmd_rs);
  obj_cmd_rs = NULL;

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_clear_sel_rs)))
    goto cleanup;

  if (ipmi_cmd_clear_sel (fi_get_ipmi_device (), 
			  reservation_id, 
			  IPMI_SEL_CLEAR_OPERATION_INITIATE_ERASE, 
			  obj_cmd_rs) != 0)
    {
      char errmsg[IPMI_ERR_STR_MAX_LEN] = { 0 };
      
      ipmi_strerror_cmd_r (obj_cmd_rs,
                           (fi_get_ipmi_device ())->net_fn,
                           errmsg, 
                           IPMI_ERR_STR_MAX_LEN);
      fprintf (stderr, "%s\n", 
	       errmsg);
      goto cleanup;
    }
  
  return SCM_BOOL_T;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return SCM_BOOL_F;
}

SCM 
ex_sel_get_clear_status ()
{
  fiid_obj_t obj_cmd_rs;
  uint16_t reservation_id;
  uint64_t val;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_reserve_sel_rs)))
    goto cleanup;

  if (ipmi_cmd_reserve_sel (fi_get_ipmi_device (), 
			    obj_cmd_rs) != 0)
    {
      char errmsg[IPMI_ERR_STR_MAX_LEN] = { 0 };
      
      ipmi_strerror_cmd_r (obj_cmd_rs,
                           (fi_get_ipmi_device ())->net_fn,
                           errmsg, 
                           IPMI_ERR_STR_MAX_LEN);
      fprintf (stderr, "%s\n", 
	       errmsg);
      goto cleanup;
    }
  
  if (fiid_obj_get (obj_cmd_rs, "reservation_id", &val) < 0)
    goto cleanup;
  reservation_id = val;
  
  fiid_obj_destroy(obj_cmd_rs);
  obj_cmd_rs = NULL;

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_clear_sel_rs)))
    goto cleanup;

  if (ipmi_cmd_clear_sel (fi_get_ipmi_device (), 
			  reservation_id, 
			  IPMI_SEL_CLEAR_OPERATION_GET_ERASURE_STATUS, 
			  obj_cmd_rs) != 0)
    {
      char errmsg[IPMI_ERR_STR_MAX_LEN] = { 0 };
      
      ipmi_strerror_cmd_r (obj_cmd_rs, 
                           (fi_get_ipmi_device ())->net_fn,
                           errmsg, 
                           IPMI_ERR_STR_MAX_LEN);
      fprintf (stderr, "%s\n", 
	       errmsg);
      goto cleanup;
    }
  
  if (fiid_obj_get (obj_cmd_rs, "erasure_progress", &val) < 0)
    goto cleanup;
  
  fiid_obj_destroy(obj_cmd_rs);
  return (gh_long2scm (val));

 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return SCM_BOOL_F;
}

/***
 *** bmc-conf2 extension functions
 ***/
SCM 
ex_set_bmc_username (SCM scm_userid, SCM scm_username)
{
  uint8_t userid;
  uint8_t *username = NULL;
  int retval;
  
  userid = gh_scm2long (scm_userid);
  username = (uint8_t *)gh_scm2newstr (scm_username, NULL);

  retval = set_bmc_username (fi_get_ipmi_device (), userid, username);

  free (username);
  
  return (retval ? SCM_BOOL_F : SCM_BOOL_T);
}

SCM 
ex_set_bmc_enable_user (SCM scm_userid, SCM scm_user_status)
{
  uint8_t userid;
  int user_status;
  int retval;
  
  userid = gh_scm2long (scm_userid);
  user_status = gh_scm2bool (scm_user_status);
  
  retval = set_bmc_enable_user (fi_get_ipmi_device (), userid, user_status);
  
  return (retval ? SCM_BOOL_F : SCM_BOOL_T);
}

SCM 
ex_set_bmc_user_password (SCM scm_userid, SCM scm_password)
{
  uint8_t userid;
  uint8_t *password = NULL;
  int retval;
  
  userid = gh_scm2long (scm_userid);
  password = (uint8_t *)gh_scm2newstr (scm_password, NULL);
  
  retval = set_bmc_user_password (fi_get_ipmi_device (), userid, password);
  
  free (password);
  
  return (retval ? SCM_BOOL_F : SCM_BOOL_T);
}

SCM 
ex_set_bmc_user_lan_channel_access (SCM scm_userid, 
				    SCM scm_lan_user_ipmi_messaging, 
				    SCM scm_lan_user_link_authentication, 
				    SCM scm_lan_user_restricted_to_callback, 
				    SCM scm_lan_privilege_limit, 
				    SCM scm_lan_session_limit)
{
  uint8_t userid;
  uint8_t lan_user_ipmi_messaging;
  uint8_t lan_user_link_authentication;
  uint8_t lan_user_restricted_to_callback;
  uint8_t lan_privilege_limit;
  uint8_t lan_session_limit;
  int retval;
  
  /*   printf ("ex_ called\n"); */
  userid = gh_scm2long (scm_userid);
  
  retval = get_bmc_user_lan_channel_access (fi_get_ipmi_device (), 
					    userid, 
					    &lan_user_ipmi_messaging, 
					    &lan_user_link_authentication, 
					    &lan_user_restricted_to_callback, 
					    &lan_privilege_limit, 
					    &lan_session_limit);
  
  if (retval)
    return (retval ? SCM_BOOL_F : SCM_BOOL_T);
  
  /*   printf ("Before:\n"); */
  /*   printf ("ipmi_msgs %d\n", lan_user_ipmi_messaging); */
  /*   printf ("link_auth %d\n", lan_user_link_authentication); */
  /*   printf ("restrict_to_callback %d\n", lan_user_restricted_to_callback); */
  /*   printf ("priv_limit %d\n", lan_privilege_limit); */
  /*   printf ("session_limit %d\n", lan_session_limit); */
  
  if (scm_boolean_p (scm_lan_user_ipmi_messaging) == SCM_BOOL_T)
    lan_user_ipmi_messaging = gh_scm2bool (scm_lan_user_ipmi_messaging);
  
  if (scm_boolean_p (scm_lan_user_link_authentication) == SCM_BOOL_T)
    lan_user_link_authentication = gh_scm2bool (scm_lan_user_link_authentication);
  
  if (scm_boolean_p (scm_lan_user_restricted_to_callback) == SCM_BOOL_T)
    lan_user_restricted_to_callback = gh_scm2bool (scm_lan_user_restricted_to_callback);
  
  if (scm_integer_p (scm_lan_privilege_limit) == SCM_BOOL_T)
    lan_privilege_limit = gh_scm2long (scm_lan_privilege_limit);
  
  if (scm_integer_p (scm_lan_session_limit) == SCM_BOOL_T)
    lan_session_limit = gh_scm2long (scm_lan_session_limit);
  
  /*   printf ("After:\n"); */
  /*   printf ("ipmi_msgs %d\n", lan_user_ipmi_messaging); */
  /*   printf ("link_auth %d\n", lan_user_link_authentication); */
  /*   printf ("restrict_to_callback %d\n", lan_user_restricted_to_callback); */
  /*   printf ("priv_limit %d\n", lan_privilege_limit); */
  /*   printf ("session_limit %d\n", lan_session_limit); */
  
  retval = set_bmc_user_lan_channel_access (fi_get_ipmi_device (), 
					    userid, 
					    lan_user_ipmi_messaging, 
					    lan_user_link_authentication, 
					    lan_user_restricted_to_callback, 
					    lan_privilege_limit, 
					    lan_session_limit);
  
  return (retval ? SCM_BOOL_F : SCM_BOOL_T);
}

SCM 
ex_set_bmc_user_serial_channel_access (SCM scm_userid, 
				       SCM scm_serial_user_ipmi_messaging, 
				       SCM scm_serial_user_link_authentication, 
				       SCM scm_serial_user_restricted_to_callback, 
				       SCM scm_serial_privilege_limit, 
				       SCM scm_serial_session_limit)
{
  uint8_t userid;
  uint8_t serial_user_ipmi_messaging;
  uint8_t serial_user_link_authentication;
  uint8_t serial_user_restricted_to_callback;
  uint8_t serial_privilege_limit;
  uint8_t serial_session_limit;
  int retval;
  
  userid = gh_scm2long (scm_userid);
  
  retval = get_bmc_user_serial_channel_access (fi_get_ipmi_device (), 
					       userid, 
					       &serial_user_ipmi_messaging, 
					       &serial_user_link_authentication, 
					       &serial_user_restricted_to_callback, 
					       &serial_privilege_limit, 
					       &serial_session_limit);
  
  if (retval)
    return (retval ? SCM_BOOL_F : SCM_BOOL_T);
  
  if (scm_boolean_p (scm_serial_user_ipmi_messaging) == SCM_BOOL_T)
    serial_user_ipmi_messaging = gh_scm2bool (scm_serial_user_ipmi_messaging);
  
  if (scm_boolean_p (scm_serial_user_link_authentication) == SCM_BOOL_T)
    serial_user_link_authentication = gh_scm2bool (scm_serial_user_link_authentication);
  
  if (scm_boolean_p (scm_serial_user_restricted_to_callback) == SCM_BOOL_T)
    serial_user_restricted_to_callback = gh_scm2bool (scm_serial_user_restricted_to_callback);
  
  if (scm_integer_p (scm_serial_privilege_limit) == SCM_BOOL_T)
    serial_privilege_limit = gh_scm2long (scm_serial_privilege_limit);
  
  if (scm_integer_p (scm_serial_session_limit) == SCM_BOOL_T)
    serial_session_limit = gh_scm2long (scm_serial_session_limit);
  
  retval = set_bmc_user_serial_channel_access (fi_get_ipmi_device (), 
					       userid, 
					       serial_user_ipmi_messaging, 
					       serial_user_link_authentication, 
					       serial_user_restricted_to_callback, 
					       serial_privilege_limit, 
					       serial_session_limit);
  
  return (retval ? SCM_BOOL_F : SCM_BOOL_T);
}

SCM 
ex_set_bmc_lan_channel_volatile_access (SCM scm_access_mode, 
					SCM scm_user_level_authentication, 
					SCM scm_per_message_authentication, 
					SCM scm_pef_alerting, 
					SCM scm_channel_privilege_limit)
{
  uint8_t access_mode; 
  uint8_t user_level_authentication; 
  uint8_t per_message_authentication; 
  uint8_t pef_alerting; 
  uint8_t channel_privilege_limit;
  int retval;
  
  retval = get_bmc_lan_channel_volatile_access (fi_get_ipmi_device (), 
						&access_mode, 
						&user_level_authentication, 
						&per_message_authentication, 
						&pef_alerting, 
						&channel_privilege_limit);
  if (retval)
    return (retval ? SCM_BOOL_F : SCM_BOOL_T);
  
  if (scm_integer_p (scm_access_mode) == SCM_BOOL_T)
    access_mode = gh_scm2long (scm_access_mode);
  if (scm_boolean_p (scm_user_level_authentication) == SCM_BOOL_T)
    user_level_authentication = gh_scm2bool (scm_user_level_authentication);
  if (scm_boolean_p (scm_per_message_authentication) == SCM_BOOL_T)
    per_message_authentication = gh_scm2bool (scm_per_message_authentication);
  if (scm_boolean_p (scm_pef_alerting) == SCM_BOOL_T)
    pef_alerting = gh_scm2bool (scm_pef_alerting);
  if (scm_integer_p (scm_channel_privilege_limit) == SCM_BOOL_T)
    channel_privilege_limit = gh_scm2long (scm_channel_privilege_limit);
  
  retval = set_bmc_lan_channel_volatile_access (fi_get_ipmi_device (), 
						access_mode, 
						user_level_authentication, 
						per_message_authentication, 
						pef_alerting, 
						channel_privilege_limit);
  return (retval ? SCM_BOOL_F : SCM_BOOL_T);
}

SCM 
ex_set_bmc_lan_channel_non_volatile_access (SCM scm_access_mode, 
					    SCM scm_user_level_authentication, 
					    SCM scm_per_message_authentication, 
					    SCM scm_pef_alerting, 
					    SCM scm_channel_privilege_limit)
{
  uint8_t access_mode; 
  uint8_t user_level_authentication; 
  uint8_t per_message_authentication; 
  uint8_t pef_alerting; 
  uint8_t channel_privilege_limit;
  int retval;
  
  retval = get_bmc_lan_channel_non_volatile_access (fi_get_ipmi_device (), 
						    &access_mode, 
						    &user_level_authentication, 
						    &per_message_authentication, 
						    &pef_alerting, 
						    &channel_privilege_limit);
  if (retval)
    return (retval ? SCM_BOOL_F : SCM_BOOL_T);
  
  if (scm_integer_p (scm_access_mode) == SCM_BOOL_T)
    access_mode = gh_scm2long (scm_access_mode);
  if (scm_boolean_p (scm_user_level_authentication) == SCM_BOOL_T)
    user_level_authentication = gh_scm2bool (scm_user_level_authentication);
  if (scm_boolean_p (scm_per_message_authentication) == SCM_BOOL_T)
    per_message_authentication = gh_scm2bool (scm_per_message_authentication);
  if (scm_boolean_p (scm_pef_alerting) == SCM_BOOL_T)
    pef_alerting = gh_scm2bool (scm_pef_alerting);
  if (scm_integer_p (scm_channel_privilege_limit) == SCM_BOOL_T)
    channel_privilege_limit = gh_scm2long (scm_channel_privilege_limit);
  
  retval = set_bmc_lan_channel_non_volatile_access (fi_get_ipmi_device (), 
						    access_mode, 
						    user_level_authentication, 
						    per_message_authentication, 
						    pef_alerting, 
						    channel_privilege_limit);
  return (retval ? SCM_BOOL_F : SCM_BOOL_T);
}

SCM 
ex_set_bmc_lan_conf_ip_address_source (SCM scm_ip_address_source)
{
  uint8_t ip_address_source;
  uint8_t retval;
  
  ip_address_source = gh_scm2long (scm_ip_address_source);
  retval = set_bmc_lan_conf_ip_address_source (fi_get_ipmi_device (), ip_address_source);
  return (retval ? SCM_BOOL_F : SCM_BOOL_T);
}

SCM 
ex_set_bmc_lan_conf_ip_address (SCM scm_ip_address)
{
  char *ip_address;
  uint8_t retval;
  
  ip_address = gh_scm2newstr (scm_ip_address, NULL);
  retval = set_bmc_lan_conf_ip_address (fi_get_ipmi_device (), ip_address);
  free (ip_address);
  return (retval ? SCM_BOOL_F : SCM_BOOL_T);
}

SCM 
ex_set_bmc_lan_conf_mac_address (SCM scm_mac_address)
{
  char *mac_address;
  uint8_t retval;
  
  mac_address = gh_scm2newstr (scm_mac_address, NULL);
  retval = set_bmc_lan_conf_mac_address (fi_get_ipmi_device (), mac_address);
  free (mac_address);
  return (retval ? SCM_BOOL_F : SCM_BOOL_T);
}

SCM 
ex_set_bmc_lan_conf_subnet_mask (SCM scm_subnet_mask)
{
  char *subnet_mask;
  uint8_t retval;
  
  subnet_mask = gh_scm2newstr (scm_subnet_mask, NULL);
  retval = set_bmc_lan_conf_subnet_mask (fi_get_ipmi_device (), subnet_mask);
  free (subnet_mask);
  return (retval ? SCM_BOOL_F : SCM_BOOL_T);
}

SCM 
ex_set_bmc_lan_conf_default_gateway_address (SCM scm_gateway_address)
{
  char *gateway_address;
  uint8_t retval;
  
  gateway_address = gh_scm2newstr (scm_gateway_address, NULL);
  retval = set_bmc_lan_conf_default_gateway_address (fi_get_ipmi_device (), gateway_address);
  free (gateway_address);
  return (retval ? SCM_BOOL_F : SCM_BOOL_T);
}

SCM 
ex_set_bmc_lan_conf_default_gateway_mac_address (SCM scm_gateway_mac_address)
{
  char *gateway_mac_address;
  uint8_t retval;
  
  gateway_mac_address = gh_scm2newstr (scm_gateway_mac_address, NULL);
  retval = set_bmc_lan_conf_default_gateway_mac_address (fi_get_ipmi_device (), gateway_mac_address);
  free (gateway_mac_address);
  return (retval ? SCM_BOOL_F : SCM_BOOL_T);
}

SCM 
ex_set_bmc_lan_conf_backup_gateway_address (SCM scm_gateway_address)
{
  char *gateway_address;
  uint8_t retval;
  
  gateway_address = gh_scm2newstr (scm_gateway_address, NULL);
  retval = set_bmc_lan_conf_backup_gateway_address (fi_get_ipmi_device (), gateway_address);
  free (gateway_address);
  return (retval ? SCM_BOOL_F : SCM_BOOL_T);
}

SCM 
ex_set_bmc_lan_conf_backup_gateway_mac_address (SCM scm_gateway_mac_address)
{
  char *gateway_mac_address;
  uint8_t retval;
  
  gateway_mac_address = gh_scm2newstr (scm_gateway_mac_address, NULL);
  retval = set_bmc_lan_conf_backup_gateway_mac_address (fi_get_ipmi_device (), gateway_mac_address);
  free (gateway_mac_address);
  return (retval ? SCM_BOOL_F : SCM_BOOL_T);
}

SCM 
ex_set_bmc_lan_conf_vlan_id (SCM scm_vlan_id,
                             SCM scm_vlan_id_enable)
{
  uint32_t vlan_id;
  uint8_t vlan_id_enable;
  int retval;
  
  retval = get_bmc_lan_conf_vlan_id (fi_get_ipmi_device (), 
                                     &vlan_id,
				     &vlan_id_enable);
  if (retval)
    return (retval ? SCM_BOOL_F : SCM_BOOL_T);
  
  if (scm_integer_p (scm_vlan_id) == SCM_BOOL_T)
    vlan_id = gh_scm2long (scm_vlan_id);
  
  if (scm_boolean_p (scm_vlan_id_enable) == SCM_BOOL_T)
    vlan_id_enable = gh_scm2bool (scm_vlan_id_enable);

  retval = set_bmc_lan_conf_vlan_id (fi_get_ipmi_device (), 
                                     vlan_id,
				     vlan_id_enable);
  return (retval ? SCM_BOOL_F : SCM_BOOL_T);
}

SCM 
ex_set_bmc_lan_conf_vlan_priority (SCM scm_vlan_priority)
{
  uint8_t vlan_priority;
  uint8_t retval;
  
  retval = get_bmc_lan_conf_vlan_priority (fi_get_ipmi_device (), &vlan_priority);

  if (retval)
    return (retval ? SCM_BOOL_F : SCM_BOOL_T);

  if (scm_integer_p (scm_vlan_priority) == SCM_BOOL_T)
    vlan_priority = gh_scm2long (scm_vlan_priority);

  retval = set_bmc_lan_conf_vlan_priority (fi_get_ipmi_device (), vlan_priority);

  return (retval ? SCM_BOOL_F : SCM_BOOL_T);
}

SCM 
ex_set_bmc_lan_conf_authentication_type_callback_enables (SCM scm_authentication_type_none, 
                                                          SCM scm_authentication_type_md2, 
                                                          SCM scm_authentication_type_md5, 
                                                          SCM scm_authentication_type_straight_password, 
                                                          SCM scm_authentication_type_oem_proprietary)
{
  struct bmc_authentication_level authentication_type_enables;
  uint8_t retval;
  
  retval = get_bmc_lan_conf_authentication_type_enables (fi_get_ipmi_device (), &authentication_type_enables);
  
  if (retval)
    return (retval ? SCM_BOOL_F : SCM_BOOL_T);
  
  if (scm_boolean_p (scm_authentication_type_none) == SCM_BOOL_T)
    authentication_type_enables.callback.type_none = gh_scm2bool (scm_authentication_type_none);
  
  if (scm_boolean_p (scm_authentication_type_md2) == SCM_BOOL_T)
    authentication_type_enables.callback.type_md2 = gh_scm2bool (scm_authentication_type_md2);
  
  if (scm_boolean_p (scm_authentication_type_md5) == SCM_BOOL_T)
    authentication_type_enables.callback.type_md5 = gh_scm2bool (scm_authentication_type_md5);
  
  if (scm_boolean_p (scm_authentication_type_straight_password) == SCM_BOOL_T)
    authentication_type_enables.callback.type_straight_password = 
      gh_scm2bool (scm_authentication_type_straight_password);
  
  if (scm_boolean_p (scm_authentication_type_oem_proprietary) == SCM_BOOL_T)
    authentication_type_enables.callback.type_oem_proprietary = 
      gh_scm2bool (scm_authentication_type_oem_proprietary);
  
  retval = set_bmc_lan_conf_authentication_type_enables (fi_get_ipmi_device (), &authentication_type_enables);
  return (retval ? SCM_BOOL_F : SCM_BOOL_T);
}

SCM 
ex_set_bmc_lan_conf_authentication_type_user_enables (SCM scm_authentication_type_none, 
                                                      SCM scm_authentication_type_md2, 
                                                      SCM scm_authentication_type_md5, 
                                                      SCM scm_authentication_type_straight_password, 
                                                      SCM scm_authentication_type_oem_proprietary)
{
  struct bmc_authentication_level authentication_type_enables;
  uint8_t retval;
  
  retval = get_bmc_lan_conf_authentication_type_enables (fi_get_ipmi_device (), &authentication_type_enables);
  
  if (retval)
    return (retval ? SCM_BOOL_F : SCM_BOOL_T);
  
  if (scm_boolean_p (scm_authentication_type_none) == SCM_BOOL_T)
    authentication_type_enables.user.type_none = gh_scm2bool (scm_authentication_type_none);
  
  if (scm_boolean_p (scm_authentication_type_md2) == SCM_BOOL_T)
    authentication_type_enables.user.type_md2 = gh_scm2bool (scm_authentication_type_md2);
  
  if (scm_boolean_p (scm_authentication_type_md5) == SCM_BOOL_T)
    authentication_type_enables.user.type_md5 = gh_scm2bool (scm_authentication_type_md5);
  
  if (scm_boolean_p (scm_authentication_type_straight_password) == SCM_BOOL_T)
    authentication_type_enables.user.type_straight_password = 
      gh_scm2bool (scm_authentication_type_straight_password);
  
  if (scm_boolean_p (scm_authentication_type_oem_proprietary) == SCM_BOOL_T)
    authentication_type_enables.user.type_oem_proprietary = 
      gh_scm2bool (scm_authentication_type_oem_proprietary);
  
  retval = set_bmc_lan_conf_authentication_type_enables (fi_get_ipmi_device (), &authentication_type_enables);
  return (retval ? SCM_BOOL_F : SCM_BOOL_T);
}

SCM 
ex_set_bmc_lan_conf_authentication_type_operator_enables (SCM scm_authentication_type_none, 
                                                          SCM scm_authentication_type_md2, 
                                                          SCM scm_authentication_type_md5, 
                                                          SCM scm_authentication_type_straight_password, 
                                                          SCM scm_authentication_type_oem_proprietary)
{
  struct bmc_authentication_level authentication_type_enables;
  uint8_t retval;
  
  retval = get_bmc_lan_conf_authentication_type_enables (fi_get_ipmi_device (), &authentication_type_enables);
  
  if (retval)
    return (retval ? SCM_BOOL_F : SCM_BOOL_T);
  
  if (scm_boolean_p (scm_authentication_type_none) == SCM_BOOL_T)
    authentication_type_enables.operator.type_none = gh_scm2bool (scm_authentication_type_none);
  
  if (scm_boolean_p (scm_authentication_type_md2) == SCM_BOOL_T)
    authentication_type_enables.operator.type_md2 = gh_scm2bool (scm_authentication_type_md2);
  
  if (scm_boolean_p (scm_authentication_type_md5) == SCM_BOOL_T)
    authentication_type_enables.operator.type_md5 = gh_scm2bool (scm_authentication_type_md5);
  
  if (scm_boolean_p (scm_authentication_type_straight_password) == SCM_BOOL_T)
    authentication_type_enables.operator.type_straight_password = 
      gh_scm2bool (scm_authentication_type_straight_password);
  
  if (scm_boolean_p (scm_authentication_type_oem_proprietary) == SCM_BOOL_T)
    authentication_type_enables.operator.type_oem_proprietary = 
      gh_scm2bool (scm_authentication_type_oem_proprietary);
  
  retval = set_bmc_lan_conf_authentication_type_enables (fi_get_ipmi_device (), &authentication_type_enables);
  return (retval ? SCM_BOOL_F : SCM_BOOL_T);
}

SCM 
ex_set_bmc_lan_conf_authentication_type_admin_enables (SCM scm_authentication_type_none, 
                                                       SCM scm_authentication_type_md2, 
                                                       SCM scm_authentication_type_md5, 
                                                       SCM scm_authentication_type_straight_password, 
                                                       SCM scm_authentication_type_oem_proprietary)
{
  struct bmc_authentication_level authentication_type_enables;
  uint8_t retval;
  
  retval = get_bmc_lan_conf_authentication_type_enables (fi_get_ipmi_device (), &authentication_type_enables);
  
  if (retval)
    return (retval ? SCM_BOOL_F : SCM_BOOL_T);
  
  if (scm_boolean_p (scm_authentication_type_none) == SCM_BOOL_T)
    authentication_type_enables.admin.type_none = gh_scm2bool (scm_authentication_type_none);
  
  if (scm_boolean_p (scm_authentication_type_md2) == SCM_BOOL_T)
    authentication_type_enables.admin.type_md2 = gh_scm2bool (scm_authentication_type_md2);
  
  if (scm_boolean_p (scm_authentication_type_md5) == SCM_BOOL_T)
    authentication_type_enables.admin.type_md5 = gh_scm2bool (scm_authentication_type_md5);
  
  if (scm_boolean_p (scm_authentication_type_straight_password) == SCM_BOOL_T)
    authentication_type_enables.admin.type_straight_password = 
      gh_scm2bool (scm_authentication_type_straight_password);
  
  if (scm_boolean_p (scm_authentication_type_oem_proprietary) == SCM_BOOL_T)
    authentication_type_enables.admin.type_oem_proprietary = 
      gh_scm2bool (scm_authentication_type_oem_proprietary);
  
  retval = set_bmc_lan_conf_authentication_type_enables (fi_get_ipmi_device (), &authentication_type_enables);
  return (retval ? SCM_BOOL_F : SCM_BOOL_T);
}

SCM 
ex_set_bmc_lan_conf_authentication_type_oem_enables (SCM scm_authentication_type_none, 
                                                     SCM scm_authentication_type_md2, 
                                                     SCM scm_authentication_type_md5, 
                                                     SCM scm_authentication_type_straight_password, 
                                                     SCM scm_authentication_type_oem_proprietary)
{
  struct bmc_authentication_level authentication_type_enables;
  uint8_t retval;
  
  retval = get_bmc_lan_conf_authentication_type_enables (fi_get_ipmi_device (), &authentication_type_enables);
  
  if (retval)
    return (retval ? SCM_BOOL_F : SCM_BOOL_T);
  
  if (scm_boolean_p (scm_authentication_type_none) == SCM_BOOL_T)
    authentication_type_enables.oem.type_none = gh_scm2bool (scm_authentication_type_none);
  
  if (scm_boolean_p (scm_authentication_type_md2) == SCM_BOOL_T)
    authentication_type_enables.oem.type_md2 = gh_scm2bool (scm_authentication_type_md2);
  
  if (scm_boolean_p (scm_authentication_type_md5) == SCM_BOOL_T)
    authentication_type_enables.oem.type_md5 = gh_scm2bool (scm_authentication_type_md5);
  
  if (scm_boolean_p (scm_authentication_type_straight_password) == SCM_BOOL_T)
    authentication_type_enables.oem.type_straight_password = 
      gh_scm2bool (scm_authentication_type_straight_password);
  
  if (scm_boolean_p (scm_authentication_type_oem_proprietary) == SCM_BOOL_T)
    authentication_type_enables.oem.type_oem_proprietary = 
      gh_scm2bool (scm_authentication_type_oem_proprietary);
  
  retval = set_bmc_lan_conf_authentication_type_enables (fi_get_ipmi_device (), &authentication_type_enables);
  return (retval ? SCM_BOOL_F : SCM_BOOL_T);
}

SCM 
ex_set_bmc_lan_conf_bmc_generated_arp_control (SCM scm_bmc_generated_gratuitous_arps, 
                                               SCM scm_bmc_generated_arp_responses)
{
  uint8_t bmc_generated_gratuitous_arps;
  uint8_t bmc_generated_arp_responses;
  int retval;
  
  retval = get_bmc_lan_conf_bmc_generated_arp_control (fi_get_ipmi_device (), 
                                                       &bmc_generated_gratuitous_arps, 
                                                       &bmc_generated_arp_responses);
  if (retval)
    return (retval ? SCM_BOOL_F : SCM_BOOL_T);
  
  if (scm_boolean_p (scm_bmc_generated_gratuitous_arps) == SCM_BOOL_T)
    bmc_generated_gratuitous_arps = gh_scm2bool (scm_bmc_generated_gratuitous_arps);
  
  if (scm_boolean_p (scm_bmc_generated_arp_responses) == SCM_BOOL_T)
    bmc_generated_arp_responses = gh_scm2bool (scm_bmc_generated_arp_responses);
  
  retval = set_bmc_lan_conf_bmc_generated_arp_control (fi_get_ipmi_device (), 
                                                       bmc_generated_gratuitous_arps, 
                                                       bmc_generated_arp_responses);
  return (retval ? SCM_BOOL_F : SCM_BOOL_T);
}

SCM 
ex_set_bmc_lan_conf_gratuitous_arp_interval (SCM scm_gratuitous_arp_interval)
{
  uint8_t gratuitous_arp_interval;
  uint8_t retval;
  
  gratuitous_arp_interval = gh_scm2long (scm_gratuitous_arp_interval);
  retval = set_bmc_lan_conf_gratuitous_arp_interval (fi_get_ipmi_device (), 
                                                     gratuitous_arp_interval);
  return (retval ? SCM_BOOL_F : SCM_BOOL_T);
}

SCM 
ex_set_bmc_serial_channel_volatile_access (SCM scm_access_mode, 
					   SCM scm_user_level_authentication, 
					   SCM scm_per_message_authentication, 
					   SCM scm_pef_alerting, 
					   SCM scm_channel_privilege_limit)
{
  uint8_t access_mode; 
  uint8_t user_level_authentication; 
  uint8_t per_message_authentication; 
  uint8_t pef_alerting; 
  uint8_t channel_privilege_limit;
  int retval;
  
  retval = get_bmc_serial_channel_volatile_access (fi_get_ipmi_device (), 
						   &access_mode, 
						   &user_level_authentication, 
						   &per_message_authentication, 
						   &pef_alerting, 
						   &channel_privilege_limit);
  if (retval)
    return (retval ? SCM_BOOL_F : SCM_BOOL_T);
  
  if (scm_integer_p (scm_access_mode) == SCM_BOOL_T)
    access_mode = gh_scm2long (scm_access_mode);
  if (scm_boolean_p (scm_user_level_authentication) == SCM_BOOL_T)
    user_level_authentication = gh_scm2bool (scm_user_level_authentication);
  if (scm_boolean_p (scm_per_message_authentication) == SCM_BOOL_T)
    per_message_authentication = gh_scm2bool (scm_per_message_authentication);
  if (scm_boolean_p (scm_pef_alerting) == SCM_BOOL_T)
    pef_alerting = gh_scm2bool (scm_pef_alerting);
  if (scm_integer_p (scm_channel_privilege_limit) == SCM_BOOL_T)
    channel_privilege_limit = gh_scm2long (scm_channel_privilege_limit);
  
  retval = set_bmc_serial_channel_volatile_access (fi_get_ipmi_device (), 
						   access_mode, 
						   user_level_authentication, 
						   per_message_authentication, 
						   pef_alerting, 
						   channel_privilege_limit);
  return (retval ? SCM_BOOL_F : SCM_BOOL_T);
}

SCM 
ex_set_bmc_serial_channel_non_volatile_access (SCM scm_access_mode, 
					       SCM scm_user_level_authentication, 
					       SCM scm_per_message_authentication, 
					       SCM scm_pef_alerting, 
					       SCM scm_channel_privilege_limit)
{
  uint8_t access_mode; 
  uint8_t user_level_authentication; 
  uint8_t per_message_authentication; 
  uint8_t pef_alerting; 
  uint8_t channel_privilege_limit;
  int retval;
  
  retval = get_bmc_serial_channel_non_volatile_access (fi_get_ipmi_device (), 
						       &access_mode, 
						       &user_level_authentication, 
						       &per_message_authentication, 
						       &pef_alerting, 
						       &channel_privilege_limit);
  if (retval)
    return (retval ? SCM_BOOL_F : SCM_BOOL_T);
  
  if (scm_integer_p (scm_access_mode) == SCM_BOOL_T)
    access_mode = gh_scm2long (scm_access_mode);
  if (scm_boolean_p (scm_user_level_authentication) == SCM_BOOL_T)
    user_level_authentication = gh_scm2bool (scm_user_level_authentication);
  if (scm_boolean_p (scm_per_message_authentication) == SCM_BOOL_T)
    per_message_authentication = gh_scm2bool (scm_per_message_authentication);
  if (scm_boolean_p (scm_pef_alerting) == SCM_BOOL_T)
    pef_alerting = gh_scm2bool (scm_pef_alerting);
  if (scm_integer_p (scm_channel_privilege_limit) == SCM_BOOL_T)
    channel_privilege_limit = gh_scm2long (scm_channel_privilege_limit);
  
  retval = set_bmc_serial_channel_non_volatile_access (fi_get_ipmi_device (), 
						       access_mode, 
						       user_level_authentication, 
						       per_message_authentication, 
						       pef_alerting, 
						       channel_privilege_limit);
  return (retval ? SCM_BOOL_F : SCM_BOOL_T);
}

SCM 
ex_set_bmc_serial_conf_connection_mode (SCM scm_basic_mode, 
                                        SCM scm_ppp_mode, 
                                        SCM scm_terminal_mode, 
                                        SCM scm_connect_mode)
{
  uint8_t basic_mode; 
  uint8_t ppp_mode;
  uint8_t terminal_mode;
  uint8_t connect_mode;
  int retval;
  
  retval = get_bmc_serial_conf_connection_mode (fi_get_ipmi_device (), 
                                                &basic_mode, 
                                                &ppp_mode, 
                                                &terminal_mode, 
                                                &connect_mode);
  if (retval)
    return (retval ? SCM_BOOL_F : SCM_BOOL_T);
  
  if (scm_boolean_p (scm_basic_mode) == SCM_BOOL_T)
    basic_mode = gh_scm2bool (scm_basic_mode);
  if (scm_boolean_p (scm_ppp_mode) == SCM_BOOL_T)
    ppp_mode = gh_scm2bool (scm_ppp_mode);
  if (scm_boolean_p (scm_terminal_mode) == SCM_BOOL_T)
    terminal_mode = gh_scm2bool (scm_terminal_mode);
  if (scm_integer_p (scm_terminal_mode) == SCM_BOOL_T)
    connect_mode = gh_scm2bool (scm_connect_mode);
  
  retval = set_bmc_serial_conf_connection_mode (fi_get_ipmi_device (), 
                                                basic_mode, 
                                                ppp_mode, 
                                                terminal_mode, 
                                                connect_mode);
  return (retval ? SCM_BOOL_F : SCM_BOOL_T);
}

SCM 
ex_set_bmc_serial_conf_page_blackout_interval (SCM scm_page_blackout_interval)
{
  uint8_t page_blackout_interval;
  int retval;
  
  page_blackout_interval = gh_scm2long (scm_page_blackout_interval);
  retval = set_bmc_serial_conf_page_blackout_interval (fi_get_ipmi_device (), 
						       page_blackout_interval);
  return (retval ? SCM_BOOL_F : SCM_BOOL_T);
}

SCM 
ex_set_bmc_serial_conf_call_retry_interval (SCM scm_call_retry_interval)
{
  uint8_t call_retry_interval;
  int retval;
  
  call_retry_interval = gh_scm2long (scm_call_retry_interval);
  retval = set_bmc_serial_conf_call_retry_interval (fi_get_ipmi_device (), 
                                                    call_retry_interval);
  return (retval ? SCM_BOOL_F : SCM_BOOL_T);
}

SCM 
ex_set_bmc_serial_conf_ipmi_messaging_comm_settings (SCM scm_dtr_hangup, 
                                                     SCM scm_flow_control, 
                                                     SCM scm_bit_rate)
{
  uint8_t dtr_hangup;
  uint8_t flow_control;
  uint8_t bit_rate;
  int retval;
  
  retval = get_bmc_serial_conf_ipmi_messaging_comm_settings (fi_get_ipmi_device (), 
                                                             &dtr_hangup, 
                                                             &flow_control, 
                                                             &bit_rate);
  if (retval)
    return (retval ? SCM_BOOL_F : SCM_BOOL_T);
  
  if (scm_boolean_p (scm_dtr_hangup) == SCM_BOOL_T)
    dtr_hangup = gh_scm2bool (scm_dtr_hangup);
  if (scm_integer_p (scm_flow_control) == SCM_BOOL_T)
    flow_control = gh_scm2long (scm_flow_control);
  if (scm_integer_p (scm_bit_rate) == SCM_BOOL_T)
    bit_rate = gh_scm2long (scm_bit_rate);
  
  retval = set_bmc_serial_conf_ipmi_messaging_comm_settings (fi_get_ipmi_device (), 
                                                             dtr_hangup, 
                                                             flow_control, 
                                                             bit_rate);
  return (retval ? SCM_BOOL_F : SCM_BOOL_T);
}

SCM 
ex_set_bmc_power_restore_policy (SCM scm_power_restore_policy)
{
  uint8_t power_restore_policy;
  int retval;
  
  power_restore_policy = gh_scm2long (scm_power_restore_policy);
  retval = set_bmc_power_restore_policy (fi_get_ipmi_device (), 
					 power_restore_policy);
  return (retval ? SCM_BOOL_F : SCM_BOOL_T);
}

SCM 
ex_set_bmc_pef_conf_pef_control (SCM scm_pef, 
				 SCM scm_pef_event_messages, 
				 SCM scm_pef_startup_delay, 
				 SCM scm_pef_alert_startup_delay)
{
  uint8_t pef = 0;
  uint8_t pef_event_messages = 0;
  uint8_t pef_startup_delay = 0;
  uint8_t pef_alert_startup_delay = 0;
  
  if (get_pef_control (fi_get_ipmi_device (), 
		       &pef, 
		       &pef_event_messages, 
		       &pef_startup_delay, 
		       &pef_alert_startup_delay) != 0)
    {
      return SCM_BOOL_F;
    }
  
  if (scm_boolean_p (scm_pef) == SCM_BOOL_T)
    pef = gh_scm2bool (scm_pef);
  if (scm_boolean_p (scm_pef_event_messages) == SCM_BOOL_T)
    pef_event_messages = gh_scm2bool (scm_pef_event_messages);
  if (scm_boolean_p (scm_pef_startup_delay) == SCM_BOOL_T)
    pef_startup_delay = gh_scm2bool (scm_pef_startup_delay);
  if (scm_boolean_p (scm_pef_alert_startup_delay) == SCM_BOOL_T)
    pef_alert_startup_delay = gh_scm2bool (scm_pef_alert_startup_delay);
  
  if (set_pef_control (fi_get_ipmi_device (), 
		       pef, 
		       pef_event_messages, 
		       pef_startup_delay, 
		       pef_alert_startup_delay) != 0)
    {
      return SCM_BOOL_F;
    }
  
  return SCM_BOOL_T;
}

SCM 
ex_set_bmc_pef_conf_pef_action_global_control (SCM scm_alert_action, 
					       SCM scm_power_down_action, 
					       SCM scm_reset_action, 
					       SCM scm_power_cycle_action, 
					       SCM scm_oem_action, 
					       SCM scm_diagnostic_interrupt)
{
  uint8_t alert_action = 0;
  uint8_t power_down_action = 0;
  uint8_t reset_action = 0;
  uint8_t power_cycle_action = 0;
  uint8_t oem_action = 0;
  uint8_t diagnostic_interrupt = 0;

  if (get_pef_action_global_control (fi_get_ipmi_device (), 
				     &alert_action, 
				     &power_down_action, 
				     &reset_action, 
				     &power_cycle_action, 
				     &oem_action, 
				     &diagnostic_interrupt) != 0)
    {
      return SCM_BOOL_F;
    }
  
  if (scm_boolean_p (scm_alert_action) == SCM_BOOL_T)
    alert_action = gh_scm2bool (scm_alert_action);
  if (scm_boolean_p (scm_power_down_action) == SCM_BOOL_T)
    power_down_action = gh_scm2bool (scm_power_down_action);
  if (scm_boolean_p (scm_reset_action) == SCM_BOOL_T)
    reset_action = gh_scm2bool (scm_reset_action);
  if (scm_boolean_p (scm_power_cycle_action) == SCM_BOOL_T)
    power_cycle_action = gh_scm2bool (scm_power_cycle_action);
  if (scm_boolean_p (scm_oem_action) == SCM_BOOL_T)
    oem_action = gh_scm2bool (scm_oem_action);
  if (scm_boolean_p (scm_diagnostic_interrupt) == SCM_BOOL_T)
    diagnostic_interrupt = gh_scm2bool (scm_diagnostic_interrupt);
  
  if (set_pef_action_global_control (fi_get_ipmi_device (), 
				     alert_action, 
				     power_down_action, 
				     reset_action, 
				     power_cycle_action, 
				     oem_action, 
				     diagnostic_interrupt) != 0)
    {
      return SCM_BOOL_F;
    }
  
  return SCM_BOOL_T;
}
  
SCM 
ex_set_bmc_pef_conf_pef_startup_delay (SCM scm_pef_startup_delay)
{
  uint8_t pef_startup_delay = 0;
  
  if (scm_integer_p (scm_pef_startup_delay) == SCM_BOOL_F)
    return SCM_BOOL_F;
  
  pef_startup_delay = gh_scm2long (scm_pef_startup_delay);
  if (set_pef_startup_delay (fi_get_ipmi_device (), 
			     pef_startup_delay) != 0)
    {
      return SCM_BOOL_F;
    }
  
  return SCM_BOOL_T;
}

SCM 
ex_set_bmc_pef_conf_pef_alert_startup_delay (SCM scm_pef_alert_startup_delay)
{
  uint8_t pef_alert_startup_delay = 0;
  
  if (scm_integer_p (scm_pef_alert_startup_delay) == SCM_BOOL_F)
    return SCM_BOOL_F;
  
  pef_alert_startup_delay = gh_scm2long (scm_pef_alert_startup_delay);
  if (set_pef_alert_startup_delay (fi_get_ipmi_device (), 
				   pef_alert_startup_delay) != 0)
    {
      return SCM_BOOL_F;
    }
  
  return SCM_BOOL_T;
}

SCM 
ex_set_sol_sol_enable (SCM scm_sol_enable)
{
  int sol_enable;
  int retval;
  
  sol_enable = gh_scm2bool (scm_sol_enable);
  
  retval = set_sol_sol_enable (fi_get_ipmi_device (), sol_enable);
  
  return (retval ? SCM_BOOL_F : SCM_BOOL_T);
}

SCM 
ex_set_sol_sol_authentication (SCM scm_sol_privilege_level, 
                               SCM scm_force_sol_payload_authentication, 
                               SCM scm_force_sol_payload_encryption)
{
  uint8_t sol_privilege_level;
  uint8_t force_sol_payload_authentication;
  uint8_t force_sol_payload_encryption;
  int retval;
  
  retval = get_sol_sol_authentication (fi_get_ipmi_device (), 
                                       &sol_privilege_level,
                                       &force_sol_payload_authentication,
                                       &force_sol_payload_encryption);
  
  if (retval)
    return (retval ? SCM_BOOL_F : SCM_BOOL_T);
  
  if (scm_integer_p (scm_sol_privilege_level) == SCM_BOOL_T)
    sol_privilege_level = gh_scm2long (scm_sol_privilege_level);

  if (scm_boolean_p (scm_force_sol_payload_authentication) == SCM_BOOL_T)
    force_sol_payload_authentication = gh_scm2bool (scm_force_sol_payload_authentication);
  
  if (scm_boolean_p (scm_force_sol_payload_encryption) == SCM_BOOL_T)
    force_sol_payload_encryption = gh_scm2bool (scm_force_sol_payload_encryption);
  
  retval = set_sol_sol_authentication (fi_get_ipmi_device (), 
                                       sol_privilege_level,
                                       force_sol_payload_authentication,
                                       force_sol_payload_encryption);
  
  return (retval ? SCM_BOOL_F : SCM_BOOL_T);
}

SCM 
ex_set_sol_character_accumulate_interval_and_send_threshold (SCM scm_character_accumulate_interval, 
                                                             SCM scm_character_send_threshold)
{
  uint8_t character_accumulate_interval;
  uint8_t character_send_threshold;
  int retval;
  
  retval = get_sol_character_accumulate_interval_and_send_threshold (fi_get_ipmi_device (), 
                                                                     &character_accumulate_interval,
                                                                     &character_send_threshold);
  
  if (retval)
    return (retval ? SCM_BOOL_F : SCM_BOOL_T);
  
  if (scm_integer_p (scm_character_accumulate_interval) == SCM_BOOL_T)
    character_accumulate_interval = gh_scm2long (scm_character_accumulate_interval);
  
  if (scm_integer_p (scm_character_send_threshold) == SCM_BOOL_T)
    character_send_threshold = gh_scm2long (scm_character_send_threshold);
  
  retval = set_sol_character_accumulate_interval_and_send_threshold (fi_get_ipmi_device (), 
                                                                     character_accumulate_interval,
                                                                     character_send_threshold);
  
  return (retval ? SCM_BOOL_F : SCM_BOOL_T);
}

SCM 
ex_set_sol_sol_retry (SCM scm_retry_count, 
                      SCM scm_retry_interval)
{
  uint8_t retry_count;
  uint8_t retry_interval;
  int retval;
  
  retval = get_sol_sol_retry (fi_get_ipmi_device (), 
                              &retry_count,
                              &retry_interval);
  
  if (retval)
    return (retval ? SCM_BOOL_F : SCM_BOOL_T);
  
  if (scm_integer_p (scm_retry_count) == SCM_BOOL_T)
    retry_count = gh_scm2long (scm_retry_count);
  
  if (scm_integer_p (scm_retry_interval) == SCM_BOOL_T)
    retry_interval = gh_scm2long (scm_retry_interval);
  
  retval = set_sol_sol_retry (fi_get_ipmi_device (), 
                              retry_count,
                              retry_interval);
  
  return (retval ? SCM_BOOL_F : SCM_BOOL_T);
}

SCM 
ex_set_sol_sol_non_volatile_bit_rate (SCM scm_bit_rate)
{
  int bit_rate;
  int retval;
  
  bit_rate = gh_scm2long (scm_bit_rate);
  
  retval = set_sol_sol_non_volatile_bit_rate (fi_get_ipmi_device (), bit_rate);
  
  return (retval ? SCM_BOOL_F : SCM_BOOL_T);
}

SCM 
ex_set_sol_sol_volatile_bit_rate (SCM scm_bit_rate)
{
  int bit_rate;
  int retval;
  
  bit_rate = gh_scm2long (scm_bit_rate);
  
  retval = set_sol_sol_volatile_bit_rate (fi_get_ipmi_device (), bit_rate);
  
  return (retval ? SCM_BOOL_F : SCM_BOOL_T);
}

SCM 
ex_set_sol_sol_payload_port_number (SCM scm_port_number)
{
  int port_number;
  int retval;
  
  port_number = gh_scm2long (scm_port_number);
  
  retval = set_sol_sol_payload_port_number (fi_get_ipmi_device (), port_number);
  
  return (retval ? SCM_BOOL_F : SCM_BOOL_T);
}


/**** get_XXXX functions *****/
SCM 
ex_get_bmc_username (SCM scm_userid)
{
  uint8_t userid;
  uint8_t username[IPMI_MAX_USER_NAME_LENGTH+1];
  int retval;
  SCM return_list = SCM_EOL;
  
  userid = gh_scm2long (scm_userid);
  memset (username, 0, IPMI_MAX_USER_NAME_LENGTH+1);

  if ((retval = get_bmc_username (fi_get_ipmi_device (), userid, username, IPMI_MAX_USER_NAME_LENGTH+1)) == 0)
    return_list = scm_listify (scm_makfrom0str (username), SCM_UNDEFINED);

  return (retval ? SCM_BOOL_F : return_list);
}

SCM 
ex_get_bmc_user_lan_channel_access (SCM scm_userid)
{
  uint8_t userid;
  uint8_t lan_user_ipmi_messaging = 0;
  uint8_t lan_user_link_authentication = 0;
  uint8_t lan_user_restricted_to_callback = 0;
  uint8_t lan_privilege_limit = 0;
  uint8_t lan_session_limit = 0;
  int retval;
  SCM return_list = SCM_EOL;
  
  userid = gh_scm2long (scm_userid);
  if ((retval = get_bmc_user_lan_channel_access (fi_get_ipmi_device (), 
						 userid, 
						 &lan_user_ipmi_messaging, 
						 &lan_user_link_authentication, 
						 &lan_user_restricted_to_callback, 
						 &lan_privilege_limit, 
						 &lan_session_limit)) == 0)
    {
      return_list = gh_list (gh_bool2scm (lan_user_ipmi_messaging), 
			     gh_bool2scm (lan_user_link_authentication), 
			     gh_bool2scm (lan_user_restricted_to_callback), 
			     gh_long2scm (lan_privilege_limit), 
			     gh_long2scm (lan_session_limit), 
			     SCM_UNDEFINED);
    }
  
  return (retval ? SCM_BOOL_F : return_list);
}

SCM 
ex_get_bmc_user_serial_channel_access (SCM scm_userid)
{
  uint8_t userid;
  uint8_t serial_user_ipmi_messaging = 0;
  uint8_t serial_user_link_authentication = 0;
  uint8_t serial_user_restricted_to_callback = 0;
  uint8_t serial_privilege_limit = 0;
  uint8_t serial_session_limit = 0;
  int retval;
  SCM return_list = SCM_EOL;
  
  userid = gh_scm2long (scm_userid);
  if ((retval = get_bmc_user_serial_channel_access (fi_get_ipmi_device (), 
						    userid, 
						    &serial_user_ipmi_messaging, 
						    &serial_user_link_authentication, 
						    &serial_user_restricted_to_callback, 
						    &serial_privilege_limit, 
						    &serial_session_limit)) == 0)
    {
      return_list = gh_list (gh_bool2scm (serial_user_ipmi_messaging), 
			     gh_bool2scm (serial_user_link_authentication), 
			     gh_bool2scm (serial_user_restricted_to_callback), 
			     gh_long2scm (serial_privilege_limit), 
			     gh_long2scm (serial_session_limit), 
			     SCM_UNDEFINED);
    }
  
  return (retval ? SCM_BOOL_F : return_list);
}

SCM 
ex_get_bmc_lan_channel_volatile_access ()
{
  uint8_t access_mode = 0;
  uint8_t user_level_authentication = 0;
  uint8_t per_message_authentication = 0;
  uint8_t pef_alerting = 0;
  uint8_t channel_privilege_limit = 0;
  int retval;
  SCM return_list = SCM_EOL;
  
  if ((retval = get_bmc_lan_channel_volatile_access (fi_get_ipmi_device (), 
						     &access_mode, 
						     &user_level_authentication, 
						     &per_message_authentication, 
						     &pef_alerting, 
						     &channel_privilege_limit)) == 0)
    {
      return_list = gh_list (gh_long2scm (access_mode), 
			     gh_bool2scm (user_level_authentication), 
			     gh_bool2scm (per_message_authentication), 
			     gh_bool2scm (pef_alerting), 
			     gh_long2scm (channel_privilege_limit), 
			     SCM_UNDEFINED);
    }
  
  return (retval ? SCM_BOOL_F : return_list);
}

SCM 
ex_get_bmc_lan_channel_non_volatile_access ()
{
  uint8_t access_mode = 0;
  uint8_t user_level_authentication = 0;
  uint8_t per_message_authentication = 0;
  uint8_t pef_alerting = 0;
  uint8_t channel_privilege_limit = 0;
  int retval;
  SCM return_list = SCM_EOL;
  
  if ((retval = get_bmc_lan_channel_non_volatile_access (fi_get_ipmi_device (), 
							 &access_mode, 
							 &user_level_authentication, 
							 &per_message_authentication, 
							 &pef_alerting, 
							 &channel_privilege_limit)) == 0)
    {
      return_list = gh_list (gh_long2scm (access_mode), 
			     gh_bool2scm (user_level_authentication), 
			     gh_bool2scm (per_message_authentication), 
			     gh_bool2scm (pef_alerting), 
			     gh_long2scm (channel_privilege_limit), 
			     SCM_UNDEFINED);
    }
  
  return (retval ? SCM_BOOL_F : return_list);
}

SCM 
ex_get_bmc_lan_conf_ip_address_source ()
{
  uint8_t ip_address_source = 0;
  int retval;
  SCM return_list = SCM_EOL;
  
  if ((retval = get_bmc_lan_conf_ip_address_source (fi_get_ipmi_device (), 
                                                    &ip_address_source)) == 0)
    return_list = gh_list (gh_long2scm (ip_address_source), SCM_UNDEFINED);
  
  return (retval ? SCM_BOOL_F : return_list);
}

SCM 
ex_get_bmc_lan_conf_ip_address ()
{
  char ip_address[16];
  int retval;
  SCM return_list = SCM_EOL;
  
  if ((retval = get_bmc_lan_conf_ip_address (fi_get_ipmi_device (), 
                                             ip_address)) == 0)
    return_list = gh_list (gh_str02scm (ip_address), SCM_UNDEFINED);
  
  return (retval ? SCM_BOOL_F : return_list);
}

SCM 
ex_get_bmc_lan_conf_mac_address ()
{
  char mac_address[18];
  int retval;
  SCM return_list = SCM_EOL;
  
  if ((retval = get_bmc_lan_conf_mac_address (fi_get_ipmi_device (), 
                                              mac_address)) == 0)
    return_list = gh_list (gh_str02scm (mac_address), SCM_UNDEFINED);
  
  return (retval ? SCM_BOOL_F : return_list);
}

SCM 
ex_get_bmc_lan_conf_subnet_mask ()
{
  char subnet_mask[16];
  int retval;
  SCM return_list = SCM_EOL;
  
  if ((retval = get_bmc_lan_conf_subnet_mask (fi_get_ipmi_device (), 
					      subnet_mask)) == 0)
    return_list = gh_list (gh_str02scm (subnet_mask), SCM_UNDEFINED);
  
  return (retval ? SCM_BOOL_F : return_list);
}

SCM 
ex_get_bmc_lan_conf_default_gateway_address ()
{
  char gateway_address[16];
  int retval;
  SCM return_list = SCM_EOL;
  
  if ((retval = get_bmc_lan_conf_default_gateway_address (fi_get_ipmi_device (), 
                                                          gateway_address)) == 0)
    return_list = gh_list (gh_str02scm (gateway_address), SCM_UNDEFINED);
  
  return (retval ? SCM_BOOL_F : return_list);
}

SCM 
ex_get_bmc_lan_conf_default_gateway_mac_address ()
{
  char gateway_mac_address[18];
  int retval;
  SCM return_list = SCM_EOL;
  
  if ((retval = get_bmc_lan_conf_default_gateway_mac_address (fi_get_ipmi_device (), 
                                                              gateway_mac_address)) == 0)
    return_list = gh_list (gh_str02scm (gateway_mac_address), SCM_UNDEFINED);
  
  return (retval ? SCM_BOOL_F : return_list);
}

SCM 
ex_get_bmc_lan_conf_backup_gateway_address ()
{
  char gateway_address[16];
  int retval;
  SCM return_list = SCM_EOL;
  
  if ((retval = get_bmc_lan_conf_backup_gateway_address (fi_get_ipmi_device (), 
                                                         gateway_address)) == 0)
    return_list = gh_list (gh_str02scm (gateway_address), SCM_UNDEFINED);
  
  return (retval ? SCM_BOOL_F : return_list);
}

SCM 
ex_get_bmc_lan_conf_backup_gateway_mac_address (SCM scm_gateway_mac_address)
{
  char gateway_mac_address[18];
  int retval;
  SCM return_list = SCM_EOL;
  
  if ((retval = get_bmc_lan_conf_backup_gateway_mac_address (fi_get_ipmi_device (), 
                                                             gateway_mac_address)) == 0)
    return_list = gh_list (gh_str02scm (gateway_mac_address), SCM_UNDEFINED);
  
  return (retval ? SCM_BOOL_F : return_list);
}

SCM
ex_get_bmc_lan_conf_vlan_id ()
{
  uint32_t vlan_id = 0;
  uint8_t vlan_id_enable = 0;
  int retval;
  SCM return_list = SCM_EOL;

  if ((retval = get_bmc_lan_conf_vlan_id (fi_get_ipmi_device (), 
					  &vlan_id, 
                                          &vlan_id_enable)) == 0)
    return_list = gh_list (gh_long2scm (vlan_id), 
                           gh_bool2scm (vlan_id_enable),
			   SCM_UNDEFINED);

  return (retval ? SCM_BOOL_F : return_list);
}

SCM
ex_get_bmc_lan_conf_vlan_priority ()
{
  uint8_t vlan_priority = 0;
  int retval;
  SCM return_list = SCM_EOL;

  if ((retval = get_bmc_lan_conf_vlan_priority (fi_get_ipmi_device (), 
						&vlan_priority)) == 0)
    return_list = gh_list (gh_long2scm (vlan_priority), SCM_UNDEFINED);

  return (retval ? SCM_BOOL_F : return_list);
}

SCM 
ex_get_bmc_lan_conf_authentication_type_callback_enables ()
{
  struct bmc_authentication_level authentication_type_enables;
  uint8_t retval;
  SCM return_list = SCM_EOL;
  
  if ((retval = get_bmc_lan_conf_authentication_type_enables (fi_get_ipmi_device (), 
                                                              &authentication_type_enables)) == 0)
    {
      return_list = gh_list (gh_bool2scm (authentication_type_enables.callback.type_none), 
			     gh_bool2scm (authentication_type_enables.callback.type_md2), 
			     gh_bool2scm (authentication_type_enables.callback.type_md5), 
			     gh_bool2scm (authentication_type_enables.callback.type_straight_password), 
			     gh_bool2scm (authentication_type_enables.callback.type_oem_proprietary), 
			     SCM_UNDEFINED);
    }
  
  return (retval ? SCM_BOOL_F : return_list);
}

SCM 
ex_get_bmc_lan_conf_authentication_type_user_enables ()
{
  struct bmc_authentication_level authentication_type_enables;
  uint8_t retval;
  SCM return_list = SCM_EOL;
  
  if ((retval = get_bmc_lan_conf_authentication_type_enables (fi_get_ipmi_device (), 
                                                              &authentication_type_enables)) == 0)
    {
      return_list = gh_list (gh_bool2scm (authentication_type_enables.user.type_none), 
			     gh_bool2scm (authentication_type_enables.user.type_md2), 
			     gh_bool2scm (authentication_type_enables.user.type_md5), 
			     gh_bool2scm (authentication_type_enables.user.type_straight_password), 
			     gh_bool2scm (authentication_type_enables.user.type_oem_proprietary), 
			     SCM_UNDEFINED);
    }
  
  return (retval ? SCM_BOOL_F : return_list);
}

SCM 
ex_get_bmc_lan_conf_authentication_type_operator_enables ()
{
  struct bmc_authentication_level authentication_type_enables;
  uint8_t retval;
  SCM return_list = SCM_EOL;
  
  if ((retval = get_bmc_lan_conf_authentication_type_enables (fi_get_ipmi_device (), 
                                                              &authentication_type_enables)) == 0)
    {
      return_list = gh_list (gh_bool2scm (authentication_type_enables.operator.type_none), 
			     gh_bool2scm (authentication_type_enables.operator.type_md2), 
			     gh_bool2scm (authentication_type_enables.operator.type_md5), 
			     gh_bool2scm (authentication_type_enables.operator.type_straight_password), 
			     gh_bool2scm (authentication_type_enables.operator.type_oem_proprietary), 
			     SCM_UNDEFINED);
    }
  
  return (retval ? SCM_BOOL_F : return_list);
}

SCM 
ex_get_bmc_lan_conf_authentication_type_admin_enables ()
{
  struct bmc_authentication_level authentication_type_enables;
  uint8_t retval;
  SCM return_list = SCM_EOL;
  
  if ((retval = get_bmc_lan_conf_authentication_type_enables (fi_get_ipmi_device (), 
                                                              &authentication_type_enables)) == 0)
    {
      return_list = gh_list (gh_bool2scm (authentication_type_enables.admin.type_none), 
			     gh_bool2scm (authentication_type_enables.admin.type_md2), 
			     gh_bool2scm (authentication_type_enables.admin.type_md5), 
			     gh_bool2scm (authentication_type_enables.admin.type_straight_password), 
			     gh_bool2scm (authentication_type_enables.admin.type_oem_proprietary), 
			     SCM_UNDEFINED);
    }
  
  return (retval ? SCM_BOOL_F : return_list);
}

SCM 
ex_get_bmc_lan_conf_authentication_type_oem_enables ()
{
  struct bmc_authentication_level authentication_type_enables;
  uint8_t retval;
  SCM return_list = SCM_EOL;
  
  if ((retval = get_bmc_lan_conf_authentication_type_enables (fi_get_ipmi_device (), 
                                                              &authentication_type_enables)) == 0)
    {
      return_list = gh_list (gh_bool2scm (authentication_type_enables.oem.type_none), 
			     gh_bool2scm (authentication_type_enables.oem.type_md2), 
			     gh_bool2scm (authentication_type_enables.oem.type_md5), 
			     gh_bool2scm (authentication_type_enables.oem.type_straight_password), 
			     gh_bool2scm (authentication_type_enables.oem.type_oem_proprietary), 
			     SCM_UNDEFINED);
    }
  
  return (retval ? SCM_BOOL_F : return_list);
}

SCM 
ex_get_bmc_lan_conf_bmc_generated_arp_control ()
{
  uint8_t bmc_generated_gratuitous_arps;
  uint8_t bmc_generated_arp_responses;
  int retval;
  SCM return_list = SCM_EOL;
  
  if ((retval = get_bmc_lan_conf_bmc_generated_arp_control (fi_get_ipmi_device (), 
                                                            &bmc_generated_gratuitous_arps, 
                                                            &bmc_generated_arp_responses)) == 0)
    {
      return_list = gh_list (gh_bool2scm (bmc_generated_gratuitous_arps), 
			     gh_bool2scm (bmc_generated_arp_responses), 
			     SCM_UNDEFINED);
    }
  
  return (retval ? SCM_BOOL_F : return_list);
}

SCM 
ex_get_bmc_lan_conf_gratuitous_arp_interval ()
{
  uint8_t gratuitous_arp_interval;
  int retval;
  SCM return_list = SCM_EOL;
  
  if ((retval = get_bmc_lan_conf_gratuitous_arp_interval (fi_get_ipmi_device (), 
                                                          &gratuitous_arp_interval)) == 0)
    return_list = gh_list (gh_long2scm (gratuitous_arp_interval), SCM_UNDEFINED);
  
  return (retval ? SCM_BOOL_F : return_list);
}

SCM 
ex_get_bmc_serial_channel_volatile_access ()
{
  uint8_t access_mode = 0;
  uint8_t user_level_authentication = 0;
  uint8_t per_message_authentication = 0;
  uint8_t pef_alerting = 0;
  uint8_t channel_privilege_limit = 0;
  int retval;
  SCM return_list = SCM_EOL;
  
  if ((retval = get_bmc_serial_channel_volatile_access (fi_get_ipmi_device (), 
							&access_mode, 
							&user_level_authentication, 
							&per_message_authentication, 
							&pef_alerting, 
							&channel_privilege_limit)) == 0)
    {
      return_list = gh_list (gh_long2scm (access_mode), 
			     gh_bool2scm (user_level_authentication), 
			     gh_bool2scm (per_message_authentication), 
			     gh_bool2scm (pef_alerting), 
			     gh_long2scm (channel_privilege_limit), 
			     SCM_UNDEFINED);
    }
  
  return (retval ? SCM_BOOL_F : return_list);
}

SCM 
ex_get_bmc_serial_channel_non_volatile_access ()
{
  uint8_t access_mode = 0;
  uint8_t user_level_authentication = 0;
  uint8_t per_message_authentication = 0;
  uint8_t pef_alerting = 0;
  uint8_t channel_privilege_limit = 0;
  int retval;
  SCM return_list = SCM_EOL;
  
  if ((retval = get_bmc_serial_channel_non_volatile_access (fi_get_ipmi_device (), 
							    &access_mode, 
							    &user_level_authentication, 
							    &per_message_authentication, 
							    &pef_alerting, 
							    &channel_privilege_limit)) == 0)
    {
      return_list = gh_list (gh_long2scm (access_mode), 
			     gh_bool2scm (user_level_authentication), 
			     gh_bool2scm (per_message_authentication), 
			     gh_bool2scm (pef_alerting), 
			     gh_long2scm (channel_privilege_limit), 
			     SCM_UNDEFINED);
    }
  
  return (retval ? SCM_BOOL_F : return_list);
}

SCM 
ex_get_bmc_serial_conf_connection_mode ()
{
  uint8_t basic_mode = 0;
  uint8_t ppp_mode = 0;
  uint8_t terminal_mode = 0;
  uint8_t connect_mode = 0;
  int retval;
  SCM return_list = SCM_EOL;
  
  if ((retval = get_bmc_serial_conf_connection_mode (fi_get_ipmi_device (), 
                                                     &basic_mode, 
                                                     &ppp_mode, 
                                                     &terminal_mode, 
                                                     &connect_mode)) == 0)
    {
      return_list = gh_list (gh_bool2scm (basic_mode), 
			     gh_bool2scm (ppp_mode), 
			     gh_bool2scm (terminal_mode), 
			     gh_long2scm (connect_mode), 
			     SCM_UNDEFINED);
    }
  
  return (retval ? SCM_BOOL_F : return_list);
}

SCM 
ex_get_bmc_serial_conf_page_blackout_interval ()
{
  uint8_t page_blackout_interval = 0;
  int retval;
  SCM return_list = SCM_EOL;
  
  if ((retval = get_bmc_serial_conf_page_blackout_interval (fi_get_ipmi_device (), 
							    &page_blackout_interval)) == 0)
    return_list = gh_list (gh_long2scm (page_blackout_interval), SCM_UNDEFINED);
  
  return (retval ? SCM_BOOL_F : return_list);
}

SCM 
ex_get_bmc_serial_conf_call_retry_interval ()
{
  uint8_t call_retry_interval = 0;
  int retval;
  SCM return_list = SCM_EOL;
  
  if ((retval = get_bmc_serial_conf_call_retry_interval (fi_get_ipmi_device (), 
                                                         &call_retry_interval)) == 0)
    return_list = gh_list (gh_long2scm (call_retry_interval), SCM_UNDEFINED);
  
  return (retval ? SCM_BOOL_F : return_list);
}

SCM 
ex_get_bmc_serial_conf_ipmi_messaging_comm_settings ()
{
  uint8_t dtr_hangup = 0;
  uint8_t flow_control = 0;
  uint8_t bit_rate = 0;
  int retval;
  SCM return_list = SCM_EOL;
  
  if ((retval = get_bmc_serial_conf_ipmi_messaging_comm_settings (fi_get_ipmi_device (), 
                                                                  &dtr_hangup, 
                                                                  &flow_control, 
                                                                  &bit_rate)) == 0)
    {
      return_list = gh_list (gh_bool2scm (dtr_hangup), 
			     gh_long2scm (flow_control), 
			     gh_long2scm (bit_rate), 
			     SCM_UNDEFINED);
    }
  
  return (retval ? SCM_BOOL_F : return_list);
}

SCM 
ex_get_bmc_power_restore_policy ()
{
  uint8_t power_restore_policy = 0;
  int retval;
  SCM return_list = SCM_EOL;
  
  if ((retval = get_bmc_power_restore_policy (fi_get_ipmi_device (), 
					      &power_restore_policy)) == 0)
    return_list = gh_list (gh_long2scm (power_restore_policy), SCM_UNDEFINED);
  
  return (retval ? SCM_BOOL_F : return_list);
}

SCM 
ex_get_bmc_pef_conf_pef_control ()
{
  SCM scm_return_list = SCM_EOL;
  uint8_t pef_enable = 0;
  uint8_t pef_event_msgs_enable = 0;
  uint8_t pef_startup_delay_enable = 0;
  uint8_t pef_alert_startup_delay_enable = 0;
  
  if (get_pef_control (fi_get_ipmi_device (), 
		       &pef_enable, 
		       &pef_event_msgs_enable, 
		       &pef_startup_delay_enable, 
		       &pef_alert_startup_delay_enable) != 0)
    {
      return SCM_BOOL_F;
    }
  
  scm_return_list = gh_list (gh_bool2scm (pef_enable), 
			     gh_bool2scm (pef_event_msgs_enable), 
			     gh_bool2scm (pef_startup_delay_enable), 
			     gh_bool2scm (pef_alert_startup_delay_enable), 
			     SCM_UNDEFINED);
  
  return scm_return_list;
}

SCM 
ex_get_bmc_pef_conf_pef_action_global_control ()
{
  SCM scm_return_list = SCM_EOL;
  uint8_t alert_action = 0;
  uint8_t power_down_action = 0;
  uint8_t reset_action = 0;
  uint8_t power_cycle_action = 0;
  uint8_t oem_action = 0;
  uint8_t diagnostic_interrupt = 0;
  
  if (get_pef_action_global_control (fi_get_ipmi_device (), 
				     &alert_action, 
				     &power_down_action, 
				     &reset_action, 
				     &power_cycle_action, 
				     &oem_action, 
				     &diagnostic_interrupt) != 0)
    {
      return SCM_BOOL_F;
    }
  
  scm_return_list = gh_list (gh_bool2scm (alert_action), 
			     gh_bool2scm (power_down_action), 
			     gh_bool2scm (reset_action), 
			     gh_bool2scm (power_cycle_action), 
			     gh_bool2scm (oem_action), 
			     gh_bool2scm (diagnostic_interrupt), 
			     SCM_UNDEFINED);
  
  return scm_return_list;
}

SCM 
ex_get_bmc_pef_conf_pef_startup_delay ()
{
  uint8_t pef_startup_delay = 0;
  
  if (get_pef_startup_delay (fi_get_ipmi_device (), 
			     &pef_startup_delay) != 0)
    {
      return SCM_BOOL_F;
    }
  
  return gh_ulong2scm (pef_startup_delay);
}

SCM 
ex_get_bmc_pef_conf_pef_alert_startup_delay ()
{
  uint8_t pef_alert_startup_delay = 0;
  
  if (get_pef_alert_startup_delay (fi_get_ipmi_device (), 
				   &pef_alert_startup_delay) != 0)
    {
      return SCM_BOOL_F;
    }
  
  return gh_ulong2scm (pef_alert_startup_delay);
}

SCM 
ex_get_sol_sol_enable ()
{
  uint8_t sol_enable = 0;
  int retval;
  SCM return_list = SCM_EOL;
  
  if ((retval = get_sol_sol_enable (fi_get_ipmi_device (), 
                                    &sol_enable)) == 0)
    {
      return_list = gh_list (gh_bool2scm (sol_enable), 
			     SCM_UNDEFINED);
    }
  
  return (retval ? SCM_BOOL_F : return_list);
}

SCM 
ex_get_sol_sol_authentication ()
{
  uint8_t sol_privilege_level = 0;
  uint8_t force_sol_payload_authentication = 0;
  uint8_t force_sol_payload_encryption = 0;

  int retval;
  SCM return_list = SCM_EOL;
  
  if ((retval = get_sol_sol_authentication (fi_get_ipmi_device (), 
                                            &sol_privilege_level,
                                            &force_sol_payload_authentication,
                                            &force_sol_payload_encryption)) == 0)
    {
      return_list = gh_list (gh_long2scm(sol_privilege_level),
                             gh_bool2scm (force_sol_payload_authentication), 
                             gh_bool2scm (force_sol_payload_encryption), 
			     SCM_UNDEFINED);
    }
  
  return (retval ? SCM_BOOL_F : return_list);
}

SCM 
ex_get_sol_character_accumulate_interval_and_send_threshold ()
{
  uint8_t character_accumulate_interval = 0;
  uint8_t character_send_threshold = 0;

  int retval;
  SCM return_list = SCM_EOL;
  
  if ((retval = get_sol_character_accumulate_interval_and_send_threshold (fi_get_ipmi_device (), 
                                                                          &character_accumulate_interval,
                                                                          &character_send_threshold)) == 0)
    {
      return_list = gh_list (gh_long2scm (character_accumulate_interval), 
                             gh_long2scm (character_send_threshold), 
			     SCM_UNDEFINED);
    }
  
  return (retval ? SCM_BOOL_F : return_list);
}

SCM 
ex_get_sol_sol_retry ()
{
  uint8_t retry_count = 0;
  uint8_t retry_interval = 0;

  int retval;
  SCM return_list = SCM_EOL;
  
  if ((retval = get_sol_sol_retry (fi_get_ipmi_device (), 
                                   &retry_count,
                                   &retry_interval)) == 0)
    {
      return_list = gh_list (gh_long2scm (retry_count), 
                             gh_long2scm (retry_interval), 
			     SCM_UNDEFINED);
    }
  
  return (retval ? SCM_BOOL_F : return_list);
}

SCM 
ex_get_sol_sol_non_volatile_bit_rate ()
{
  uint8_t bit_rate = 0;
  int retval;
  SCM return_list = SCM_EOL;
  
  if ((retval = get_sol_sol_non_volatile_bit_rate (fi_get_ipmi_device (), 
                                                   &bit_rate)) == 0)
    {
      return_list = gh_list (gh_long2scm (bit_rate), 
			     SCM_UNDEFINED);
    }
  
  return (retval ? SCM_BOOL_F : return_list);
}

SCM 
ex_get_sol_sol_volatile_bit_rate ()
{
  uint8_t bit_rate = 0;
  int retval;
  SCM return_list = SCM_EOL;
  
  if ((retval = get_sol_sol_volatile_bit_rate (fi_get_ipmi_device (), 
                                               &bit_rate)) == 0)
    {
      return_list = gh_list (gh_long2scm (bit_rate), 
			     SCM_UNDEFINED);
    }
  
  return (retval ? SCM_BOOL_F : return_list);
}

SCM 
ex_get_sol_sol_payload_port_number ()
{
  uint16_t port_number = 0;
  int retval;
  SCM return_list = SCM_EOL;
  
  if ((retval = get_sol_sol_payload_port_number (fi_get_ipmi_device (), 
                                                 &port_number)) == 0)
    {
      return_list = gh_list (gh_long2scm (port_number), 
			     SCM_UNDEFINED);
    }
  
  return (retval ? SCM_BOOL_F : return_list);
}

/***********************************************************/
SCM 
ex_check_bmc_user_password (SCM scm_userid, SCM scm_password)
{
  uint8_t userid;
  uint8_t *password = NULL;
  int retval;
  
  userid = gh_scm2long (scm_userid);
  password = (uint8_t *)gh_scm2newstr (scm_password, NULL);
  
  retval = check_bmc_user_password (fi_get_ipmi_device (), userid, password);
  
  free (password);
  
  return (retval ? SCM_BOOL_T : SCM_BOOL_F);
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
    case IPMI_SDR_FORMAT_ENTITY_ASSO_RECORD:
      scm_sdr_record = get_scm_sdr_entity_association_record (&(sdr_record.record.sdr_entity_association_record), scm_sdr_record);
      break;
    case IPMI_SDR_FORMAT_GEN_DEV_LOCATOR_RECORD:
      scm_sdr_record = get_scm_sdr_generic_device_locator_record (&(sdr_record.record.sdr_generic_device_locator_record), scm_sdr_record);
      break;
    case IPMI_SDR_FORMAT_FRU_DEV_LOCATOR_RECORD:
      scm_sdr_record = get_scm_sdr_logical_fru_device_locator_record (&(sdr_record.record.sdr_logical_fru_device_locator_record), scm_sdr_record);
      break;
    case IPMI_SDR_FORMAT_MGMT_CNTRLR_DEV_LOCATOR_RECORD:
      scm_sdr_record = get_scm_sdr_management_controller_device_locator_record (&(sdr_record.record.sdr_management_controller_device_locator_record), scm_sdr_record);
      break;
    case IPMI_SDR_FORMAT_OEM_RECORD:
      scm_sdr_record = get_scm_sdr_oem_record (&(sdr_record.record.sdr_oem_record), scm_sdr_record);
      break;
    case IPMI_SDR_FORMAT_DEV_ENTITY_ASSO_RECORD:
    case IPMI_SDR_FORMAT_MGMT_CNTRLR_CONFIRMATION_RECORD:
    case IPMI_SDR_FORMAT_BMC_MSG_CHANNEL_INFO_RECORD:
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
    case IPMI_SDR_FORMAT_ENTITY_ASSO_RECORD:
      scm2sdr_entity_association_record (scm_sdr_record, &(sdr_record->record.sdr_entity_association_record));
      break;
    case IPMI_SDR_FORMAT_GEN_DEV_LOCATOR_RECORD:
      scm2sdr_generic_device_locator_record (scm_sdr_record, &(sdr_record->record.sdr_generic_device_locator_record));
      break;
    case IPMI_SDR_FORMAT_FRU_DEV_LOCATOR_RECORD:
      scm2sdr_logical_fru_device_locator_record (scm_sdr_record, &(sdr_record->record.sdr_logical_fru_device_locator_record));
      break;
    case IPMI_SDR_FORMAT_MGMT_CNTRLR_DEV_LOCATOR_RECORD:
      scm2sdr_management_controller_device_locator_record (scm_sdr_record, &(sdr_record->record.sdr_management_controller_device_locator_record));
      break;
    case IPMI_SDR_FORMAT_OEM_RECORD:
      scm2sdr_oem_record (scm_sdr_record, &(sdr_record->record.sdr_oem_record));
      break;
    case IPMI_SDR_FORMAT_DEV_ENTITY_ASSO_RECORD:
    case IPMI_SDR_FORMAT_MGMT_CNTRLR_CONFIRMATION_RECORD:
    case IPMI_SDR_FORMAT_BMC_MSG_CHANNEL_INFO_RECORD:
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
  
  uint64_t val;
  
  /* get_repository_info */
  if (!(cmd_rs = fiid_obj_create(tmpl_get_sdr_repository_info_rs)))
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
  scm_repository_info_list = scm_assoc_set_x (scm_repository_info_list, 
                                              gh_str02scm ("record_count"), 
                                              gh_long2scm (val));
  
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
    args.common.host = NULL;
  else 
    args.common.host = gh_scm2newstr (scm_value, NULL);
  
  scm_value = scm_list_ref (scm_arg_list, gh_long2scm (5));
  if (scm_boolean_p (scm_value) == SCM_BOOL_T)
    args.common.username = NULL;
  else 
    args.common.username = gh_scm2newstr (scm_value, NULL);
  
  scm_value = scm_list_ref (scm_arg_list, gh_long2scm (6));
  if (scm_boolean_p (scm_value) == SCM_BOOL_T)
    args.common.password = NULL;
  else 
    args.common.password = gh_scm2newstr (scm_value, NULL);
  
  scm_value = scm_list_ref (scm_arg_list, gh_long2scm (7));
  if (scm_boolean_p (scm_value) == SCM_BOOL_T)
    args.common.authentication_type = IPMI_AUTHENTICATION_TYPE_NONE;
  else 
    args.common.authentication_type = gh_scm2int (scm_value);
  
  scm_value = scm_list_ref (scm_arg_list, gh_long2scm (8));
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
  
  if (!(cmd_rs = fiid_obj_create(tmpl_get_pef_capabilities_rs)))
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
      if (!(cmd_rs = fiid_obj_create(tmpl_get_pef_configuration_parameters_number_of_event_filters_rs)))
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
      if (!(cmd_rs = fiid_obj_create(tmpl_get_pef_configuration_parameters_number_of_alert_policy_entries_rs)))
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
      if (!(cmd_rs = fiid_obj_create(tmpl_get_pef_configuration_parameters_number_of_alert_strings_rs)))
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

