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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

/* AIX requires this to be the first thing in the file.  */
#ifndef __GNUC__
# if HAVE_ALLOCA_H
#  include <alloca.h>
# else
#  ifdef _AIX
 #pragma alloca
#  else
#   ifndef alloca /* predefined by HP cc +Olibcalls */
char *alloca ();
#   endif
#  endif
# endif
#endif

#include <freeipmi.h>
#include <guile/gh.h>
#include <readline/readline.h>
#include <stdio.h>
#include <sys/stat.h>
#include <errno.h>
#include <math.h>

#if TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# if HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  include <time.h>
# endif
#endif

#include "fish.h"
#include "extension.h"
#include "fi-utils.h"
#include "interpreter.h"
#include "ipmi-wrapper.h"
#include "bmc-conf-checkout.h"
#include "bmc-conf-commit.h"
#include "bmc-conf-utils.h"
#include "bmc-conf-key-utils.h"
#include "ipmi-wrapper-sensor.h"
#include "ipmi-wrapper-sel.h"
#include "scm-procedures.h"
#include "xmalloc.h"

extern char *program_invocation_short_name;
extern int errno;


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

/* returns current freehoo version */
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
  fi_load (gh_scm2newstr (scm_filename, NULL));
  return (SCM_UNSPECIFIED);
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
ex_set_sms_io_base (SCM scm_sms_io_base)
{
  fi_set_sms_io_base (gh_scm2int (scm_sms_io_base));
  return (SCM_UNSPECIFIED);
}

SCM
ex_get_sock_timeout ()
{
  return (gh_ulong2scm (fi_get_sock_timeout ()));
}

SCM
ex_set_sock_timeout (SCM scm_sock_timeout)
{
  fi_set_sock_timeout (gh_scm2long (scm_sock_timeout));
  return (SCM_UNSPECIFIED);
}

SCM
ex_set_driver_poll_interval (SCM scm_driver_poll_interval)
{
  set_driver_poll_interval (gh_scm2long (scm_driver_poll_interval));
  return (SCM_UNSPECIFIED);
}

SCM
ex_ipmi_ping (SCM scm_host_addr)
{
  int status = 0;
  u_int8_t *pong;
  pong   = fiid_obj_alloc (tmpl_cmd_asf_presence_pong);
  status = ipmi_ping (fi_get_sockfd (),
		      gh_scm2newstr (scm_host_addr, NULL), pong);
  xfree (pong);
  if (errno == EBADMSG)
    warn ("Increase your socket timeout value");

  if (status == 0)
    return (SCM_BOOL_T);
  else
    return (SCM_BOOL_F);
}

SCM
ex_kcs_get_dev_id_display (void)
{
  u_int8_t *obj_hdr_rs;
  u_int8_t *obj_cmd_rs;
  u_int32_t cmd_rs_len;

  obj_hdr_rs = fiid_obj_alloc (tmpl_hdr_kcs);
  obj_cmd_rs = fiid_obj_alloc (tmpl_cmd_get_dev_id_rs);
  cmd_rs_len = fiid_obj_len_bytes (tmpl_cmd_get_dev_id_rs);

  if (ipmi_kcs_get_dev_id (fi_get_sms_io_base (), obj_hdr_rs, obj_cmd_rs) != 0)
    {
      fprintf (stderr, "ipmi_kcs_get_dev_id (%Xh, %p, %p)\n", 
	   IPMI_KCS_SMS_IO_BASE_SR870BN4, obj_hdr_rs, obj_cmd_rs);
      return (SCM_BOOL_F);
    }
/*   fiid_obj_dump (1, obj_hdr_rs, tmpl_hdr_kcs); */
/*   fiid_obj_dump (1, obj_cmd_rs, tmpl_cmd_get_dev_id_rs); */
  if (display_get_dev_id (obj_cmd_rs, cmd_rs_len) != 0)
      return (SCM_BOOL_F);
    
  return (SCM_BOOL_T);
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
ex_bmc_config_checkout (SCM scm_filename)
{
  char *filename;
  
  filename = gh_scm2newstr (scm_filename, NULL);
  
  bmc_config_checkout (filename);
  
  return (SCM_UNSPECIFIED);
}

SCM 
ex_bmc_config_commit (SCM scm_filename)
{
  char *filename;
  
  filename = gh_scm2newstr (scm_filename, NULL);
  
  bmc_config_commit (filename);
  
  free (filename);
  
  return (SCM_UNSPECIFIED);
}

SCM 
ex_bmc_config_edit_key_pair (SCM scm_filename, SCM scm_key, SCM scm_value)
{
  char *filename = NULL;
  char *key = NULL;
  char *value = NULL;
  int retval;
  
  filename = gh_scm2newstr (scm_filename, NULL);
  key = gh_scm2newstr (scm_key, NULL);
  value = gh_scm2newstr (scm_value, NULL);
  
  retval = edit_key_pair_bmc_config_file (filename, key, value);
  
  free (filename);
  free (key);
  free (value);
  
  return (gh_bool2scm (retval));
}

/* SCM  */
/* ex_bmc_config_check_key_orig (SCM scm_filename, SCM scm_key) */
/* { */
/*   char *filename = NULL; */
/*   char *key = NULL; */
/*   int retval; */
  
/*   filename = gh_scm2newstr (scm_filename, NULL); */
/*   key = gh_scm2newstr (scm_key, NULL); */
  
/*   retval = check_bmc_config_file_key (filename, key); */
  
/*   free (filename); */
/*   free (key); */
  
/*   if (retval == 0) */
/*     return SCM_BOOL_T; */
/*   return SCM_BOOL_F; */
/* } */

SCM 
ex_bmc_config_check_key (SCM scm_key)
{
  char *key = NULL;
  int retval;
  
  key = gh_scm2newstr (scm_key, NULL);
  retval = bmc_config_validate_key (key);  
  free (key);
  
  if (retval)
    return SCM_BOOL_T;
  return SCM_BOOL_F;
}

SCM 
ex_bmc_config_diff_key_pair (SCM scm_filename, SCM scm_key, SCM scm_value)
{
  char *filename = NULL;
  char *key = NULL;
  char *value = NULL;
  int retval;
  
  filename = gh_scm2newstr (scm_filename, NULL);
  key = gh_scm2newstr (scm_key, NULL);
  value = gh_scm2newstr (scm_value, NULL);
  
  retval = bmc_config_diff_key_pair (filename, key, value);
  
  free (filename);
  free (key);
  free (value);
  
  return (gh_int2scm (retval));
}

SCM 
ex_bmc_config_diff_file (SCM scm_bmc_filename, SCM scm_filename)
{
  char *bmc_filename = NULL;
  char *filename = NULL;
  int retval;
  
  bmc_filename = gh_scm2newstr (scm_bmc_filename, NULL);
  filename = gh_scm2newstr (scm_filename, NULL);
  
  retval = bmc_config_diff_file (bmc_filename, filename);
  
  free (bmc_filename);
  free (filename);
  
  return (gh_int2scm (retval));
}

SCM 
ex_sensors_cache_create (SCM scm_cache_filename)
{
  char *cache_filename = NULL;
  u_int8_t retval;
  
  cache_filename = gh_scm2newstr (scm_cache_filename, NULL);
  
  retval = ipmi_sdr_cache_create (fi_get_sms_io_base (), cache_filename);
  
  free (cache_filename);
  
  if (retval)
    return (SCM_BOOL_F);
  
  return (SCM_BOOL_T);
}

SCM 
ex_get_default_sdr_repo_cache_filename (void)
{
  return (gh_str02scm (FI_DEFAULT_SDR_REPO_CACHE_FILENAME));
}

SCM 
ex_sensors_cache_load (SCM scm_cache_filename)
{
  char *filename;
  struct stat stbuf;

  sdr_repo_cache_t *sdr_repo_cache = get_sdr_repo_cache ();
  filename = gh_scm2newstr (scm_cache_filename, NULL);
  
  if (stat (filename, &stbuf) == 0)
    {
      if (!S_ISREG (stbuf.st_mode))
	{
	  printf ("%d, %d\n", stbuf.st_mode, S_IFREG);
	  fprintf (stderr, "%s: Not a regular file [%s].\n",
		   program_invocation_short_name, filename);
	  return (SCM_BOOL_F);
	}
    }
  else
    {
      fprintf (stderr, "%s: stat failed on file [%s]; %s\n",
	       program_invocation_short_name, filename, strerror (errno));
      return (SCM_BOOL_F);
    }
  
  if (ipmi_sdr_repo_cache_load (sdr_repo_cache, filename) == -1)
    return (SCM_BOOL_F);
  
  return (SCM_BOOL_T);
}

SCM 
ex_sensors_cache_unload ()
{
  sdr_repo_cache_t *sdr_repo_cache = get_sdr_repo_cache ();
  
  if (ipmi_sdr_repo_cache_unload (sdr_repo_cache))
    return (SCM_BOOL_F);
  
  return (SCM_BOOL_T);
}

SCM 
ex_sensors_cache_seek (SCM scm_rec_id)
{
  u_int16_t rec_id;
  
  sdr_repo_cache_t *sdr_repo_cache = get_sdr_repo_cache ();
  
  rec_id = gh_scm2long (scm_rec_id);
  
  if (ipmi_sdr_repo_cache_seek (sdr_repo_cache, rec_id))
    return (SCM_BOOL_F);
  
  return (SCM_BOOL_T);
}

SCM 
ex_sensors_cache_first ()
{
  sdr_repo_cache_t *sdr_repo_cache = get_sdr_repo_cache ();
  
  if (ipmi_sdr_repo_cache_first (sdr_repo_cache))
    return (SCM_BOOL_F);
  
  return (SCM_BOOL_T);
}

SCM 
ex_sensors_cache_next ()
{
  sdr_repo_cache_t *sdr_repo_cache = get_sdr_repo_cache ();
  
  if (ipmi_sdr_repo_cache_next (sdr_repo_cache))
    return (SCM_BOOL_F);
  
  return (SCM_BOOL_T);
}

SCM 
ex_sensors_cache_display ()
{
  sdr_repo_cache_t *sdr_repo_cache = get_sdr_repo_cache ();
  
  if (display_current_sensor (sdr_repo_cache))
    return (SCM_BOOL_T);
  
  return (SCM_BOOL_F);
}

SCM 
ex_sensors_cache_get_total_records ()
{
  int total_records;
  
  sdr_repo_cache_t *sdr_repo_cache = get_sdr_repo_cache ();
  
  total_records = get_sdr_total_records (sdr_repo_cache);
  
  return (gh_long2scm (total_records));
}

SCM 
ex_sensors_cache_verbose_display ()
{
  sdr_repo_cache_t *sdr_repo_cache = get_sdr_repo_cache ();
  
  if (display_verbose_current_sensor (sdr_repo_cache))
    return (SCM_BOOL_T);
  
  return (SCM_BOOL_F);
}

SCM 
ex_sensors_cache_very_verbose_display ()
{
  sdr_repo_cache_t *sdr_repo_cache = get_sdr_repo_cache ();
  
  if (display_very_verbose_current_sensor (sdr_repo_cache))
    return (SCM_BOOL_T);
  
  return (SCM_BOOL_F);
}

SCM 
ex_sensors_cache_get_current_group ()
{
  const char *group_name;
  
  sdr_repo_cache_t *sdr_repo_cache = get_sdr_repo_cache ();
  
  group_name = ipmi_sdr_repo_cache_get_sensor_group (sdr_repo_cache);
  
  if (group_name)
    return (gh_str02scm (group_name));
  return (gh_str02scm (""));
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
ex_sdr_get_repo_info ()
{
  SCM scm_repo_info_list;
  
  u_int8_t *data_rs = NULL;
  
  char version_string[17];
  u_int8_t sdr_major_version;
  u_int8_t sdr_minor_version;
  
  u_int64_t val;
  time_t time;
  
  /* get_repo_info */
  data_rs = alloca (fiid_obj_len_bytes (tmpl_get_sdr_repo_info_rs));
  if (ipmi_kcs_get_repo_info (fi_get_sms_io_base(), data_rs) != 0)
    return SCM_EOL;
  
  fiid_obj_get (data_rs, 
		tmpl_get_sdr_repo_info_rs, 
		"comp_code", 
		&val);
  if (val != 0)
    return SCM_EOL;
  
  /* appending sdr version */
  fiid_obj_get (data_rs,
		tmpl_get_sdr_repo_info_rs,
		"sdr_version_major",
		&val);
  sdr_major_version = val;
  fiid_obj_get (data_rs,
		tmpl_get_sdr_repo_info_rs,
		"sdr_version_minor",
		&val);
  sdr_minor_version = val;
  snprintf (version_string, 17, 
	    "%d.%d", 
	    sdr_major_version, sdr_minor_version);
  scm_repo_info_list = gh_list (gh_str02scm (version_string), SCM_UNDEFINED);
  
  /* appending record-count */
  fiid_obj_get (data_rs,
		tmpl_get_sdr_repo_info_rs,
		"record_count",
		&val);
  scm_repo_info_list = gh_append2 (scm_repo_info_list,
				   gh_list (gh_long2scm (val),
					    SCM_UNDEFINED));
  
  /* appending free_space */
  fiid_obj_get (data_rs,
		tmpl_get_sdr_repo_info_rs,
		"free_space",
		&val);
  scm_repo_info_list = gh_append2 (scm_repo_info_list,
				   gh_list (gh_long2scm (0x10000 - val),
					    SCM_UNDEFINED));
  
  /* appending recent_addition_timestamp */
  fiid_obj_get (data_rs,
		tmpl_get_sdr_repo_info_rs,
		"recent_addition_timestamp",
		&val);
  time = val;
  scm_repo_info_list = gh_append2 (scm_repo_info_list,
				   gh_list (gh_str02scm (ctime (&time)),
					    SCM_UNDEFINED));
  
  /* appending recent_erase_timestamp */
  fiid_obj_get (data_rs,
		tmpl_get_sdr_repo_info_rs,
		"recent_erase_timestamp",
		&val);
  time = val;
  scm_repo_info_list = gh_append2 (scm_repo_info_list,
				   gh_list (gh_str02scm (ctime (&time)),
					    SCM_UNDEFINED));
  return (scm_repo_info_list);
}

SCM
ex_kcs_get_poll_count ()
{
  u_int64_t poll_count;
  SCM kcs_poll_count;

  poll_count = ipmi_kcs_get_poll_count ();
  kcs_poll_count = gh_long2scm (poll_count);
  
  return (kcs_poll_count);
}

SCM 
ex_sel_display_first_entry ()
{
  u_int8_t record_data[16];
  
  if (ipmi_sel_get_first_entry (fi_get_sms_io_base (), get_seld (), record_data) != 0)
    {
/*       fprintf (stderr, "ipmi_sel_get_first_entry failed\n"); */
      return SCM_BOOL_F;
    }
  
  if (display_sel_record (record_data) == 0)
    return SCM_BOOL_T;
  return SCM_BOOL_F;
}

SCM 
ex_sel_display_next_entry ()
{
  u_int8_t record_data[16];
  
  if (ipmi_sel_get_next_entry (fi_get_sms_io_base (), get_seld (), record_data) != 0)
    {
/*       fprintf (stderr, "ipmi_sel_get_next_entry failed\n"); */
      return SCM_BOOL_F;
    }
  
  if (display_sel_record (record_data) == 0)
    return SCM_BOOL_T;
  return SCM_BOOL_F;
}

SCM 
ex_sel_get_first_entry ()
{
  u_int8_t record_data[16];
  struct sel_record sel_rec;
  SCM scm_sel_record = SCM_EOL;
  
  if (ipmi_sel_get_first_entry (fi_get_sms_io_base (), get_seld (), record_data) != 0)
    {
/*       fprintf (stderr, "ipmi_sel_get_first_entry failed\n"); */
      return SCM_EOL;
    }
  
  if (get_sel_record (record_data, &sel_rec) != 0)
    return SCM_EOL;
  
  scm_sel_record = gh_list (gh_long2scm (sel_rec.record_id), SCM_UNDEFINED);
  scm_sel_record = gh_append2 (scm_sel_record, 
			       gh_list (gh_ulong2scm (sel_rec.timestamp), 
					SCM_UNDEFINED));
  scm_sel_record = gh_append2 (scm_sel_record, 
			       gh_list (gh_str02scm (sel_rec.sensor_desc), 
					SCM_UNDEFINED));
  scm_sel_record = gh_append2 (scm_sel_record, 
			       gh_list (gh_str02scm (sel_rec.event_desc), 
					SCM_UNDEFINED));
  scm_sel_record = gh_append2 (scm_sel_record, 
			       gh_list (gh_str02scm (sel_rec.generator_id), 
					SCM_UNDEFINED));
  
  free (sel_rec.sensor_desc);
  free (sel_rec.event_desc);
  free (sel_rec.generator_id);
  
  return scm_sel_record;
}

SCM 
ex_sel_get_next_entry ()
{
  u_int8_t record_data[16];
  struct sel_record sel_rec;
  SCM scm_sel_record = SCM_EOL;
  
  if (ipmi_sel_get_next_entry (fi_get_sms_io_base (), get_seld (), record_data) != 0)
    {
/*       fprintf (stderr, "ipmi_sel_get_next_entry failed\n"); */
      return SCM_EOL;
    }
  
  if (get_sel_record (record_data, &sel_rec) != 0)
    return SCM_EOL;
  
  scm_sel_record = gh_list (gh_long2scm (sel_rec.record_id), SCM_UNDEFINED);
  scm_sel_record = gh_append2 (scm_sel_record, 
			       gh_list (gh_ulong2scm (sel_rec.timestamp), 
					SCM_UNDEFINED));
  scm_sel_record = gh_append2 (scm_sel_record, 
			       gh_list (gh_str02scm (sel_rec.sensor_desc), 
					SCM_UNDEFINED));
  scm_sel_record = gh_append2 (scm_sel_record, 
			       gh_list (gh_str02scm (sel_rec.event_desc), 
					SCM_UNDEFINED));
  scm_sel_record = gh_append2 (scm_sel_record, 
			       gh_list (gh_str02scm (sel_rec.generator_id), 
					SCM_UNDEFINED));
  
  free (sel_rec.sensor_desc);
  free (sel_rec.event_desc);
  free (sel_rec.generator_id);
  
  return scm_sel_record;
}

SCM 
ex_sel_delete_entry (SCM scm_record_id)
{
  u_int16_t record_id;
  fiid_obj_t obj_data_rs;
  u_int16_t reservation_id;
  u_int8_t status;
  u_int64_t val;
  
  record_id = gh_scm2long (scm_record_id);
  
  {
    obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_reserve_sel_rs));
    status = ipmi_kcs_reserve_sel (fi_get_sms_io_base (), 
				   obj_data_rs);
    if (status != 0)
      {
	fprintf (stderr, 
		 "error: ipmi_kcs_reserve_sel() failed.\n");
	return SCM_BOOL_F;
      }
    
    if (IPMI_COMP_CODE(obj_data_rs) != IPMI_COMMAND_SUCCESS)
      {
	char err_msg[IPMI_ERR_STR_MAX_LEN];
	ipmi_strerror_cmd_r (obj_data_rs, err_msg, IPMI_ERR_STR_MAX_LEN);
	fprintf (stderr, 
		 "error: ipmi_kcs_reserve_sel() failed with %s\n", 
		 err_msg);
	return SCM_BOOL_F;
      }
    
    fiid_obj_get (obj_data_rs, 
		  tmpl_reserve_sel_rs, 
		  "reservation_id", 
		  &val);
    reservation_id = val;
  }
  
  {
    obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_delete_sel_entry_rs));
    status = ipmi_kcs_delete_sel_entry (fi_get_sms_io_base (), 
					reservation_id, 
					record_id, 
					obj_data_rs);
    if (status != 0)
      {
	fprintf (stderr, 
		 "error: ipmi_kcs_delete_sel_entry() failed.\n");
	return SCM_BOOL_F;
      }
    
    if (IPMI_COMP_CODE(obj_data_rs) == IPMI_COMMAND_SUCCESS)
      return SCM_BOOL_T;
    
    if (IPMI_COMP_CODE(obj_data_rs) == IPMI_SEL_OPERATION_NOT_SUPPORTED)
      {
	fprintf (stderr, 
		 "error: delete operation not supported.\n");
	return SCM_BOOL_F;
      }
    
    if (IPMI_COMP_CODE(obj_data_rs) == IPMI_SEL_ERASE_IN_PROGRESS)
      {
	fprintf (stderr, 
		 "error: sel erase in progress.\n");
	return SCM_BOOL_F;
      }
    
    {
      char err_msg[IPMI_ERR_STR_MAX_LEN];
      ipmi_strerror_cmd_r (obj_data_rs, err_msg, IPMI_ERR_STR_MAX_LEN);
      fprintf (stderr, 
	       "error: ipmi_kcs_delete_sel_entry() failed with %s\n", 
	       err_msg);
      return SCM_BOOL_F;
    }
  }
  
  return SCM_BOOL_T;
}

SCM 
ex_sel_clear ()
{
  fiid_obj_t obj_data_rs;
  u_int16_t reservation_id;
  u_int8_t status;
  u_int64_t val;
  
  {
    obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_reserve_sel_rs));
    status = ipmi_kcs_reserve_sel (fi_get_sms_io_base (), 
				   obj_data_rs);
    if (status != 0)
      {
	fprintf (stderr, 
		 "error: ipmi_kcs_reserve_sel() failed.\n");
	return SCM_BOOL_F;
      }
    
    if (IPMI_COMP_CODE(obj_data_rs) != IPMI_COMMAND_SUCCESS)
      {
	char err_msg[IPMI_ERR_STR_MAX_LEN];
	ipmi_strerror_cmd_r (obj_data_rs, err_msg, IPMI_ERR_STR_MAX_LEN);
	fprintf (stderr, 
		 "error: ipmi_kcs_reserve_sel() failed with %s\n", 
		 err_msg);
	return SCM_BOOL_F;
      }
    
    fiid_obj_get (obj_data_rs, 
		  tmpl_reserve_sel_rs, 
		  "reservation_id", 
		  &val);
    reservation_id = val;
  }
  
  {
    obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_clear_sel_rs));
    status = ipmi_kcs_clear_sel (fi_get_sms_io_base (), 
				 reservation_id, 
				 IPMI_SEL_INITIATE_ERASE, 
				 obj_data_rs);
    if (status != 0)
      {
	fprintf (stderr, 
		 "error: ipmi_kcs_clear_sel() failed.\n");
	return SCM_BOOL_F;
      }
    
    if (IPMI_COMP_CODE(obj_data_rs) != IPMI_COMMAND_SUCCESS)
      {
	char err_msg[IPMI_ERR_STR_MAX_LEN];
	ipmi_strerror_cmd_r (obj_data_rs, err_msg, IPMI_ERR_STR_MAX_LEN);
	fprintf (stderr, 
		 "error: ipmi_kcs_clear_sel() failed with %s\n", 
		 err_msg);
	return SCM_BOOL_F;
      }
  }
  
  return SCM_BOOL_T;
}

SCM 
ex_sel_get_clear_status ()
{
  fiid_obj_t obj_data_rs;
  u_int16_t reservation_id;
  u_int8_t status;
  u_int64_t val;
  
  {
    obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_reserve_sel_rs));
    status = ipmi_kcs_reserve_sel (fi_get_sms_io_base (), 
				   obj_data_rs);
    if (status != 0)
      {
	fprintf (stderr, 
		 "error: ipmi_kcs_reserve_sel() failed.\n");
	return SCM_BOOL_F;
      }
    
    if (IPMI_COMP_CODE(obj_data_rs) != IPMI_COMMAND_SUCCESS)
      {
	char err_msg[IPMI_ERR_STR_MAX_LEN];
	ipmi_strerror_cmd_r (obj_data_rs, err_msg, IPMI_ERR_STR_MAX_LEN);
	fprintf (stderr, 
		 "error: ipmi_kcs_reserve_sel() failed with %s\n", 
		 err_msg);
	return SCM_BOOL_F;
      }
    
    fiid_obj_get (obj_data_rs, 
		  tmpl_reserve_sel_rs, 
		  "reservation_id", 
		  &val);
    reservation_id = val;
  }
  
  {
    obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_clear_sel_rs));
    status = ipmi_kcs_clear_sel (fi_get_sms_io_base (), 
				 reservation_id, 
				 IPMI_SEL_GET_ERASURE_STATUS, 
				 obj_data_rs);
    if (status != 0)
      {
	fprintf (stderr, 
		 "error: ipmi_kcs_clear_sel() failed.\n");
	return SCM_BOOL_F;
      }
    
    if (IPMI_COMP_CODE(obj_data_rs) != IPMI_COMMAND_SUCCESS)
      {
	char err_msg[IPMI_ERR_STR_MAX_LEN];
	ipmi_strerror_cmd_r (obj_data_rs, err_msg, IPMI_ERR_STR_MAX_LEN);
	fprintf (stderr, 
		 "error: ipmi_kcs_clear_sel() failed with %s\n", 
		 err_msg);
	return SCM_BOOL_F;
      }
  }
  
  fiid_obj_get (obj_data_rs, 
		tmpl_clear_sel_rs, 
		"erasure_progress", 
		&val);
  return (gh_long2scm (val));
}

SCM 
ex_get_sensors_errno ()
{
  return (gh_long2scm (get_sensors_errno ()));
}
