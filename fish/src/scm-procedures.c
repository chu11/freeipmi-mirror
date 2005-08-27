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

#if defined(__FreeBSD__) && !defined(EBADMSG)
#define EBADMSG		ENOMSG
#endif

#include "fish.h"
#include "extension.h"
#include "fi-utils.h"
#include "interpreter.h"
#include "ipmi-wrapper.h"
#include "bmc-conf2.h"
#include "scm-procedures.h"
#include "xmalloc.h"

extern char *program_invocation_short_name;
extern int errno;

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
  u_int8_t *obj_cmd_rs;
  u_int32_t cmd_rs_len;
  u_int8_t retval;
  
  obj_cmd_rs = fiid_obj_alloc (tmpl_cmd_get_dev_id_rs);
  cmd_rs_len = fiid_obj_len_bytes (tmpl_cmd_get_dev_id_rs);
  
  if (ipmi_kcs_get_dev_id (obj_cmd_rs) != 0)
    {
      fprintf (stderr, "ipmi_kcs_get_dev_id (%p)\n", obj_cmd_rs);
      return (SCM_BOOL_F);
    }
/*   fiid_obj_dump (1, obj_cmd_rs, tmpl_cmd_get_dev_id_rs); */
  retval = display_get_dev_id (obj_cmd_rs, cmd_rs_len);
  free (obj_cmd_rs);
  
  if (retval != 0)
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
ex_get_default_sdr_repo_cache_filename (void)
{
  return (gh_str02scm (FI_DEFAULT_SDR_REPO_CACHE_FILENAME));
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
  if (ipmi_kcs_get_repo_info (data_rs) != 0)
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
ex_sel_get_first_entry_raw ()
{
  u_int8_t record_data[SEL_RECORD_SIZE];
  SCM scm_sel_record = SCM_EOL;
  
  if (ipmi_sel_get_first_entry (get_seld (), record_data) == 0)
    {
      int i;
      for (i = SEL_RECORD_SIZE - 1; i >= 0; i--)
        scm_sel_record = gh_cons (gh_ulong2scm (record_data[i]), scm_sel_record);
    }
  return scm_sel_record;
}

SCM
ex_sel_get_next_entry_raw ()
{
  u_int8_t record_data[SEL_RECORD_SIZE];
  SCM scm_sel_record = SCM_EOL;
  
  if (ipmi_sel_get_next_entry (get_seld (), record_data) == 0)
    {
      int i;
      for (i = SEL_RECORD_SIZE - 1; i >= 0; i--)
        scm_sel_record = gh_cons (gh_ulong2scm (record_data[i]), scm_sel_record);
    }
  return scm_sel_record;
}

SCM
ex_sel_get_first_entry_hex ()
{
  u_int8_t record_data [SEL_RECORD_SIZE];
  u_int8_t hex_data [SEL_HEX_RECORD_SIZE];
  
  if (ipmi_sel_get_first_entry (get_seld (), record_data) == 0)
    {
      snprintf (hex_data, SEL_HEX_RECORD_SIZE,
                "RID:[%02X][%02X] RT:[%02X] TS:[%02X][%02X][%02X][%02X] "
                "GID:[%02X][%02X] ER:[%02X] ST:[%02X] SN:[%02X] EDIR:[%02X] "
                "ED1: [%02X] ED2: [%02X] ED3: [%02X]\n",
                record_data[0], record_data[1], record_data[2], record_data[3], 
                record_data[4], record_data[5], record_data[6], record_data[7], 
                record_data[8], record_data[9], record_data[10], record_data[11], 
                record_data[12], record_data[13], record_data[14], record_data[15]);
      return gh_str02scm (hex_data);
    }
  else return SCM_BOOL_F;
}

SCM
ex_sel_get_next_entry_hex ()
{
  u_int8_t record_data [SEL_RECORD_SIZE];
  u_int8_t hex_data [SEL_HEX_RECORD_SIZE];
  
  if (ipmi_sel_get_next_entry (get_seld (), record_data) == 0)
    {
      snprintf (hex_data, SEL_HEX_RECORD_SIZE,
                "RID:[%02X][%02X] RT:[%02X] TS:[%02X][%02X][%02X][%02X] "
                "GID:[%02X][%02X] ER:[%02X] ST:[%02X] SN:[%02X] EDIR:[%02X] "
                "ED1: [%02X] ED2: [%02X] ED3: [%02X]\n",
                record_data[0], record_data[1], record_data[2], record_data[3], 
                record_data[4], record_data[5], record_data[6], record_data[7], 
                record_data[8], record_data[9], record_data[10], record_data[11], 
                record_data[12], record_data[13], record_data[14], record_data[15]);
      return gh_str02scm (hex_data);
    }
  else return SCM_BOOL_F;
}

SCM
ex_sel_get_info ()
{
  sel_info_t info;

  if (get_sel_info (&info) == 0)
    {
      char buf [1024];
      struct tm tmtime;
      char addtime [32];
      char erasetime [32];
      time_t bintime;

      bintime = info.last_add_time;
      gmtime_r (&bintime, &tmtime);
      strftime (addtime, 32, "%m/%d/%Y - %H:%M:%S", &tmtime);
      bintime = info.last_erase_time;
      gmtime_r (&bintime, &tmtime);
      strftime (erasetime, 32, "%m/%d/%Y - %H:%M:%S", &tmtime);
      snprintf (buf, 1024,
                "Version                     IPMI v%lu.%lu\n"
                "Number of Entries           %lu\n"
                "Last Add Time               %s\n"
                "Last Erase Time             %s\n"
                "Free Space Remaining        %lu\n\n",
                info.version_major, info.version_minor,
                info.entry_count,
                addtime, erasetime,
                info.free_space);
      return gh_str02scm (buf);
    }
  else return SCM_BOOL_F;
}

SCM
ex_sel_get_info_binary ()
{
  sel_info_t info;

  if (get_sel_info (&info) == 0)
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
  else return SCM_BOOL_F;
}

SCM 
ex_sel_get_first_entry ()
{
  u_int8_t record_data[SEL_RECORD_SIZE];
  struct sel_record sel_rec;
  SCM scm_sel_record = SCM_EOL;
  
  if (ipmi_sel_get_first_entry (get_seld (), record_data) != 0)
    {
      /* fprintf (stderr, "ipmi_sel_get_first_entry failed\n"); */
      return SCM_EOL;
    }
  
  if (get_sel_record (record_data, &sel_rec) != 0)
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
  u_int8_t record_data[SEL_RECORD_SIZE];
  struct sel_record sel_rec;
  SCM scm_sel_record = SCM_EOL;
  
  if (ipmi_sel_get_next_entry (get_seld (), record_data) != 0)
    {
      /* fprintf (stderr, "ipmi_sel_get_next_entry failed\n"); */
      return SCM_EOL;
    }
  
  if (get_sel_record (record_data, &sel_rec) != 0)
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
  u_int16_t record_id;
  fiid_obj_t obj_data_rs;
  u_int16_t reservation_id;
  u_int8_t status;
  u_int64_t val;
  
  record_id = gh_scm2long (scm_record_id);
  
  {
    obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_reserve_sel_rs));
    status = ipmi_kcs_reserve_sel (obj_data_rs);
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
    status = ipmi_kcs_delete_sel_entry (reservation_id, 
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
    status = ipmi_kcs_reserve_sel (obj_data_rs);
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
    status = ipmi_kcs_clear_sel (reservation_id, 
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
    status = ipmi_kcs_reserve_sel (obj_data_rs);
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
    status = ipmi_kcs_clear_sel (reservation_id, 
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

/***
 *** bmc-conf2 extension functions
 ***/
SCM 
ex_set_bmc_username (SCM scm_userid, SCM scm_username)
{
  u_int8_t userid;
  u_int8_t *username = NULL;
  int retval;
  
  userid = gh_scm2long (scm_userid);
  username = gh_scm2newstr (scm_username, NULL);
  
  retval = set_bmc_username (userid, username);
  
  free (username);
  
  return (retval ? SCM_BOOL_F : SCM_BOOL_T);
}

SCM 
ex_set_bmc_enable_user (SCM scm_userid, SCM scm_user_status)
{
  u_int8_t userid;
  int user_status;
  int retval;
  
  userid = gh_scm2long (scm_userid);
  user_status = gh_scm2bool (scm_user_status);
  
  retval = set_bmc_enable_user (userid, user_status);
  
  return (retval ? SCM_BOOL_F : SCM_BOOL_T);
}

SCM 
ex_set_bmc_user_password (SCM scm_userid, SCM scm_password)
{
  u_int8_t userid;
  u_int8_t *password = NULL;
  int retval;
  
  userid = gh_scm2long (scm_userid);
  password = gh_scm2newstr (scm_password, NULL);
  
  retval = set_bmc_user_password (userid, password);
  
  free (password);
  
  return (retval ? SCM_BOOL_F : SCM_BOOL_T);
}

SCM 
ex_set_bmc_user_lan_channel_access (SCM scm_userid, 
				    SCM scm_lan_enable_ipmi_msgs, 
				    SCM scm_lan_enable_link_auth, 
				    SCM scm_lan_enable_restrict_to_callback, 
				    SCM scm_lan_privilege_limit, 
				    SCM scm_lan_session_limit)
{
  u_int8_t userid;
  u_int8_t lan_enable_ipmi_msgs;
  u_int8_t lan_enable_link_auth;
  u_int8_t lan_enable_restrict_to_callback;
  u_int8_t lan_privilege_limit;
  u_int8_t lan_session_limit;
  int retval;
  
/*   printf ("ex_ called\n"); */
  userid = gh_scm2long (scm_userid);
  
  retval = get_bmc_user_lan_channel_access (userid, 
					    &lan_enable_ipmi_msgs, 
					    &lan_enable_link_auth, 
					    &lan_enable_restrict_to_callback, 
					    &lan_privilege_limit, 
					    &lan_session_limit);
  
  if (retval)
    return (retval ? SCM_BOOL_F : SCM_BOOL_T);
  
/*   printf ("Before:\n"); */
/*   printf ("ipmi_msgs %d\n", lan_enable_ipmi_msgs); */
/*   printf ("link_auth %d\n", lan_enable_link_auth); */
/*   printf ("restrict_to_callback %d\n", lan_enable_restrict_to_callback); */
/*   printf ("priv_limit %d\n", lan_privilege_limit); */
/*   printf ("session_limit %d\n", lan_session_limit); */
  
  if (scm_boolean_p (scm_lan_enable_ipmi_msgs) == SCM_BOOL_T)
    lan_enable_ipmi_msgs = gh_scm2bool (scm_lan_enable_ipmi_msgs);
  
  if (scm_boolean_p (scm_lan_enable_link_auth) == SCM_BOOL_T)
    lan_enable_link_auth = gh_scm2bool (scm_lan_enable_link_auth);
  
  if (scm_boolean_p (scm_lan_enable_restrict_to_callback) == SCM_BOOL_T)
    lan_enable_restrict_to_callback = gh_scm2bool (scm_lan_enable_restrict_to_callback);
  
  if (scm_integer_p (scm_lan_privilege_limit) == SCM_BOOL_T)
    lan_privilege_limit = gh_scm2long (scm_lan_privilege_limit);
  
  if (scm_integer_p (scm_lan_session_limit) == SCM_BOOL_T)
    lan_session_limit = gh_scm2long (scm_lan_session_limit);
  
/*   printf ("After:\n"); */
/*   printf ("ipmi_msgs %d\n", lan_enable_ipmi_msgs); */
/*   printf ("link_auth %d\n", lan_enable_link_auth); */
/*   printf ("restrict_to_callback %d\n", lan_enable_restrict_to_callback); */
/*   printf ("priv_limit %d\n", lan_privilege_limit); */
/*   printf ("session_limit %d\n", lan_session_limit); */
  
  retval = set_bmc_user_lan_channel_access (userid, 
					    lan_enable_ipmi_msgs, 
					    lan_enable_link_auth, 
					    lan_enable_restrict_to_callback, 
					    lan_privilege_limit, 
					    lan_session_limit);
  
  return (retval ? SCM_BOOL_F : SCM_BOOL_T);
}

SCM 
ex_set_bmc_user_serial_channel_access (SCM scm_userid, 
				       SCM scm_serial_enable_ipmi_msgs, 
				       SCM scm_serial_enable_link_auth, 
				       SCM scm_serial_enable_restrict_to_callback, 
				       SCM scm_serial_privilege_limit, 
				       SCM scm_serial_session_limit)
{
  u_int8_t userid;
  u_int8_t serial_enable_ipmi_msgs;
  u_int8_t serial_enable_link_auth;
  u_int8_t serial_enable_restrict_to_callback;
  u_int8_t serial_privilege_limit;
  u_int8_t serial_session_limit;
  int retval;
  
  userid = gh_scm2long (scm_userid);
  
  retval = get_bmc_user_serial_channel_access (userid, 
					       &serial_enable_ipmi_msgs, 
					       &serial_enable_link_auth, 
					       &serial_enable_restrict_to_callback, 
					       &serial_privilege_limit, 
					       &serial_session_limit);
  
  if (retval)
    return (retval ? SCM_BOOL_F : SCM_BOOL_T);
  
  if (scm_boolean_p (scm_serial_enable_ipmi_msgs) == SCM_BOOL_T)
    serial_enable_ipmi_msgs = gh_scm2bool (scm_serial_enable_ipmi_msgs);
  
  if (scm_boolean_p (scm_serial_enable_link_auth) == SCM_BOOL_T)
    serial_enable_link_auth = gh_scm2bool (scm_serial_enable_link_auth);
  
  if (scm_boolean_p (scm_serial_enable_restrict_to_callback) == SCM_BOOL_T)
    serial_enable_restrict_to_callback = gh_scm2bool (scm_serial_enable_restrict_to_callback);
  
  if (scm_integer_p (scm_serial_privilege_limit) == SCM_BOOL_T)
    serial_privilege_limit = gh_scm2long (scm_serial_privilege_limit);
  
  if (scm_integer_p (scm_serial_session_limit) == SCM_BOOL_T)
    serial_session_limit = gh_scm2long (scm_serial_session_limit);
  
  retval = set_bmc_user_serial_channel_access (userid, 
					       serial_enable_ipmi_msgs, 
					       serial_enable_link_auth, 
					       serial_enable_restrict_to_callback, 
					       serial_privilege_limit, 
					       serial_session_limit);
  
  return (retval ? SCM_BOOL_F : SCM_BOOL_T);
}

SCM 
ex_set_bmc_lan_channel_volatile_access (SCM scm_access_mode, 
					SCM scm_enable_user_level_auth, 
					SCM scm_enable_per_message_auth, 
					SCM scm_enable_pef_alerting, 
					SCM scm_channel_privilege_limit)
{
  u_int8_t access_mode; 
  u_int8_t enable_user_level_auth; 
  u_int8_t enable_per_message_auth; 
  u_int8_t enable_pef_alerting; 
  u_int8_t channel_privilege_limit;
  int retval;
  
  retval = get_bmc_lan_channel_volatile_access (&access_mode, 
						&enable_user_level_auth, 
						&enable_per_message_auth, 
						&enable_pef_alerting, 
						&channel_privilege_limit);
  if (retval)
    return (retval ? SCM_BOOL_F : SCM_BOOL_T);
  
  if (scm_integer_p (scm_access_mode) == SCM_BOOL_T)
    access_mode = gh_scm2long (scm_access_mode);
  if (scm_boolean_p (scm_enable_user_level_auth) == SCM_BOOL_T)
    enable_user_level_auth = gh_scm2bool (scm_enable_user_level_auth);
  if (scm_boolean_p (scm_enable_per_message_auth) == SCM_BOOL_T)
    enable_per_message_auth = gh_scm2bool (scm_enable_per_message_auth);
  if (scm_boolean_p (scm_enable_pef_alerting) == SCM_BOOL_T)
    enable_pef_alerting = gh_scm2bool (scm_enable_pef_alerting);
  if (scm_integer_p (scm_channel_privilege_limit) == SCM_BOOL_T)
    channel_privilege_limit = gh_scm2long (scm_channel_privilege_limit);
  
  retval = set_bmc_lan_channel_volatile_access (access_mode, 
						enable_user_level_auth, 
						enable_per_message_auth, 
						enable_pef_alerting, 
						channel_privilege_limit);
  return (retval ? SCM_BOOL_F : SCM_BOOL_T);
}

SCM 
ex_set_bmc_lan_channel_non_volatile_access (SCM scm_access_mode, 
					    SCM scm_enable_user_level_auth, 
					    SCM scm_enable_per_message_auth, 
					    SCM scm_enable_pef_alerting, 
					    SCM scm_channel_privilege_limit)
{
  u_int8_t access_mode; 
  u_int8_t enable_user_level_auth; 
  u_int8_t enable_per_message_auth; 
  u_int8_t enable_pef_alerting; 
  u_int8_t channel_privilege_limit;
  int retval;
  
  retval = get_bmc_lan_channel_non_volatile_access (&access_mode, 
						    &enable_user_level_auth, 
						    &enable_per_message_auth, 
						    &enable_pef_alerting, 
						    &channel_privilege_limit);
  if (retval)
    return (retval ? SCM_BOOL_F : SCM_BOOL_T);
  
  if (scm_integer_p (scm_access_mode) == SCM_BOOL_T)
    access_mode = gh_scm2long (scm_access_mode);
  if (scm_boolean_p (scm_enable_user_level_auth) == SCM_BOOL_T)
    enable_user_level_auth = gh_scm2bool (scm_enable_user_level_auth);
  if (scm_boolean_p (scm_enable_per_message_auth) == SCM_BOOL_T)
    enable_per_message_auth = gh_scm2bool (scm_enable_per_message_auth);
  if (scm_boolean_p (scm_enable_pef_alerting) == SCM_BOOL_T)
    enable_pef_alerting = gh_scm2bool (scm_enable_pef_alerting);
  if (scm_integer_p (scm_channel_privilege_limit) == SCM_BOOL_T)
    channel_privilege_limit = gh_scm2long (scm_channel_privilege_limit);
  
  retval = set_bmc_lan_channel_non_volatile_access (access_mode, 
						    enable_user_level_auth, 
						    enable_per_message_auth, 
						    enable_pef_alerting, 
						    channel_privilege_limit);
  return (retval ? SCM_BOOL_F : SCM_BOOL_T);
}

SCM 
ex_set_bmc_lan_conf_ip_addr_source (SCM scm_ip_address_source)
{
  u_int8_t ip_address_source;
  u_int8_t retval;
  
  ip_address_source = gh_scm2long (scm_ip_address_source);
  retval = set_bmc_lan_conf_ip_addr_source (ip_address_source);
  return (retval ? SCM_BOOL_F : SCM_BOOL_T);
}

SCM 
ex_set_bmc_lan_conf_ip_addr (SCM scm_ip_address)
{
  char *ip_address;
  u_int8_t retval;
  
  ip_address = gh_scm2newstr (scm_ip_address, NULL);
  retval = set_bmc_lan_conf_ip_addr (ip_address);
  free (ip_address);
  return (retval ? SCM_BOOL_F : SCM_BOOL_T);
}

SCM 
ex_set_bmc_lan_conf_mac_addr (SCM scm_mac_address)
{
  char *mac_address;
  u_int8_t retval;
  
  mac_address = gh_scm2newstr (scm_mac_address, NULL);
  retval = set_bmc_lan_conf_mac_addr (mac_address);
  free (mac_address);
  return (retval ? SCM_BOOL_F : SCM_BOOL_T);
}

SCM 
ex_set_bmc_lan_conf_subnet_mask (SCM scm_subnet_mask)
{
  char *subnet_mask;
  u_int8_t retval;
  
  subnet_mask = gh_scm2newstr (scm_subnet_mask, NULL);
  retval = set_bmc_lan_conf_subnet_mask (subnet_mask);
  free (subnet_mask);
  return (retval ? SCM_BOOL_F : SCM_BOOL_T);
}

SCM 
ex_set_bmc_lan_conf_default_gw_ip_addr (SCM scm_gw_ip_address)
{
  char *gw_ip_address;
  u_int8_t retval;
  
  gw_ip_address = gh_scm2newstr (scm_gw_ip_address, NULL);
  retval = set_bmc_lan_conf_default_gw_ip_addr (gw_ip_address);
  free (gw_ip_address);
  return (retval ? SCM_BOOL_F : SCM_BOOL_T);
}

SCM 
ex_set_bmc_lan_conf_default_gw_mac_addr (SCM scm_gw_mac_address)
{
  char *gw_mac_address;
  u_int8_t retval;
  
  gw_mac_address = gh_scm2newstr (scm_gw_mac_address, NULL);
  retval = set_bmc_lan_conf_default_gw_mac_addr (gw_mac_address);
  free (gw_mac_address);
  return (retval ? SCM_BOOL_F : SCM_BOOL_T);
}

SCM 
ex_set_bmc_lan_conf_backup_gw_ip_addr (SCM scm_gw_ip_address)
{
  char *gw_ip_address;
  u_int8_t retval;
  
  gw_ip_address = gh_scm2newstr (scm_gw_ip_address, NULL);
  retval = set_bmc_lan_conf_backup_gw_ip_addr (gw_ip_address);
  free (gw_ip_address);
  return (retval ? SCM_BOOL_F : SCM_BOOL_T);
}

SCM 
ex_set_bmc_lan_conf_backup_gw_mac_addr (SCM scm_gw_mac_address)
{
  char *gw_mac_address;
  u_int8_t retval;
  
  gw_mac_address = gh_scm2newstr (scm_gw_mac_address, NULL);
  retval = set_bmc_lan_conf_backup_gw_mac_addr (gw_mac_address);
  free (gw_mac_address);
  return (retval ? SCM_BOOL_F : SCM_BOOL_T);
}

SCM 
ex_set_bmc_lan_conf_vlan_id (SCM scm_vlan_id_flag, 
                             SCM scm_vlan_id)
{
  u_int8_t vlan_id_flag;
  u_int32_t vlan_id;
  int retval;
  
  retval = get_bmc_lan_conf_vlan_id (&vlan_id_flag, 
                                     &vlan_id);
  if (retval)
    return (retval ? SCM_BOOL_F : SCM_BOOL_T);
  
  if (scm_boolean_p (scm_vlan_id_flag) == SCM_BOOL_T)
    vlan_id_flag = gh_scm2bool (scm_vlan_id_flag);

  if (scm_integer_p (scm_vlan_id) == SCM_BOOL_T)
    vlan_id = gh_scm2long (scm_vlan_id);
  
  retval = set_bmc_lan_conf_vlan_id (vlan_id_flag, 
                                     vlan_id);
  return (retval ? SCM_BOOL_F : SCM_BOOL_T);
}

SCM 
ex_set_bmc_lan_conf_vlan_priority (SCM scm_vlan_priority)
{
  u_int8_t vlan_priority;
  u_int8_t retval;
  
  retval = get_bmc_lan_conf_vlan_priority (&vlan_priority);

  if (retval)
    return (retval ? SCM_BOOL_F : SCM_BOOL_T);

  if (scm_integer_p (scm_vlan_priority) == SCM_BOOL_T)
    vlan_priority = gh_scm2long (scm_vlan_priority);

  retval = set_bmc_lan_conf_vlan_priority (vlan_priority);

  return (retval ? SCM_BOOL_F : SCM_BOOL_T);
}

SCM 
ex_set_bmc_lan_conf_auth_type_callback_enables (SCM scm_auth_type_none, 
						SCM scm_auth_type_md2, 
						SCM scm_auth_type_md5, 
						SCM scm_auth_type_straight_password, 
						SCM scm_auth_type_oem_proprietary)
{
  struct bmc_auth_level auth_type_enables;
  u_int8_t retval;
  
  retval = get_bmc_lan_conf_auth_type_enables (&auth_type_enables);
  
  if (retval)
    return (retval ? SCM_BOOL_F : SCM_BOOL_T);
  
  if (scm_boolean_p (scm_auth_type_none) == SCM_BOOL_T)
    auth_type_enables.callback.type_none = gh_scm2bool (scm_auth_type_none);
  
  if (scm_boolean_p (scm_auth_type_md2) == SCM_BOOL_T)
    auth_type_enables.callback.type_md2 = gh_scm2bool (scm_auth_type_md2);
  
  if (scm_boolean_p (scm_auth_type_md5) == SCM_BOOL_T)
    auth_type_enables.callback.type_md5 = gh_scm2bool (scm_auth_type_md5);
  
  if (scm_boolean_p (scm_auth_type_straight_password) == SCM_BOOL_T)
    auth_type_enables.callback.type_straight_password = 
      gh_scm2bool (scm_auth_type_straight_password);
  
  if (scm_boolean_p (scm_auth_type_oem_proprietary) == SCM_BOOL_T)
    auth_type_enables.callback.type_oem_proprietary = 
      gh_scm2bool (scm_auth_type_oem_proprietary);
  
  retval = set_bmc_lan_conf_auth_type_enables (&auth_type_enables);
  return (retval ? SCM_BOOL_F : SCM_BOOL_T);
}

SCM 
ex_set_bmc_lan_conf_auth_type_user_enables (SCM scm_auth_type_none, 
					    SCM scm_auth_type_md2, 
					    SCM scm_auth_type_md5, 
					    SCM scm_auth_type_straight_password, 
					    SCM scm_auth_type_oem_proprietary)
{
  struct bmc_auth_level auth_type_enables;
  u_int8_t retval;
  
  retval = get_bmc_lan_conf_auth_type_enables (&auth_type_enables);
  
  if (retval)
    return (retval ? SCM_BOOL_F : SCM_BOOL_T);
  
  if (scm_boolean_p (scm_auth_type_none) == SCM_BOOL_T)
    auth_type_enables.user.type_none = gh_scm2bool (scm_auth_type_none);
  
  if (scm_boolean_p (scm_auth_type_md2) == SCM_BOOL_T)
    auth_type_enables.user.type_md2 = gh_scm2bool (scm_auth_type_md2);
  
  if (scm_boolean_p (scm_auth_type_md5) == SCM_BOOL_T)
    auth_type_enables.user.type_md5 = gh_scm2bool (scm_auth_type_md5);
  
  if (scm_boolean_p (scm_auth_type_straight_password) == SCM_BOOL_T)
    auth_type_enables.user.type_straight_password = 
      gh_scm2bool (scm_auth_type_straight_password);
  
  if (scm_boolean_p (scm_auth_type_oem_proprietary) == SCM_BOOL_T)
    auth_type_enables.user.type_oem_proprietary = 
      gh_scm2bool (scm_auth_type_oem_proprietary);
  
  retval = set_bmc_lan_conf_auth_type_enables (&auth_type_enables);
  return (retval ? SCM_BOOL_F : SCM_BOOL_T);
}

SCM 
ex_set_bmc_lan_conf_auth_type_operator_enables (SCM scm_auth_type_none, 
						SCM scm_auth_type_md2, 
						SCM scm_auth_type_md5, 
						SCM scm_auth_type_straight_password, 
						SCM scm_auth_type_oem_proprietary)
{
  struct bmc_auth_level auth_type_enables;
  u_int8_t retval;
  
  retval = get_bmc_lan_conf_auth_type_enables (&auth_type_enables);
  
  if (retval)
    return (retval ? SCM_BOOL_F : SCM_BOOL_T);
  
  if (scm_boolean_p (scm_auth_type_none) == SCM_BOOL_T)
    auth_type_enables.operator.type_none = gh_scm2bool (scm_auth_type_none);
  
  if (scm_boolean_p (scm_auth_type_md2) == SCM_BOOL_T)
    auth_type_enables.operator.type_md2 = gh_scm2bool (scm_auth_type_md2);
  
  if (scm_boolean_p (scm_auth_type_md5) == SCM_BOOL_T)
    auth_type_enables.operator.type_md5 = gh_scm2bool (scm_auth_type_md5);
  
  if (scm_boolean_p (scm_auth_type_straight_password) == SCM_BOOL_T)
    auth_type_enables.operator.type_straight_password = 
      gh_scm2bool (scm_auth_type_straight_password);
  
  if (scm_boolean_p (scm_auth_type_oem_proprietary) == SCM_BOOL_T)
    auth_type_enables.operator.type_oem_proprietary = 
      gh_scm2bool (scm_auth_type_oem_proprietary);
  
  retval = set_bmc_lan_conf_auth_type_enables (&auth_type_enables);
  return (retval ? SCM_BOOL_F : SCM_BOOL_T);
}

SCM 
ex_set_bmc_lan_conf_auth_type_admin_enables (SCM scm_auth_type_none, 
					     SCM scm_auth_type_md2, 
					     SCM scm_auth_type_md5, 
					     SCM scm_auth_type_straight_password, 
					     SCM scm_auth_type_oem_proprietary)
{
  struct bmc_auth_level auth_type_enables;
  u_int8_t retval;
  
  retval = get_bmc_lan_conf_auth_type_enables (&auth_type_enables);
  
  if (retval)
    return (retval ? SCM_BOOL_F : SCM_BOOL_T);
  
  if (scm_boolean_p (scm_auth_type_none) == SCM_BOOL_T)
    auth_type_enables.admin.type_none = gh_scm2bool (scm_auth_type_none);
  
  if (scm_boolean_p (scm_auth_type_md2) == SCM_BOOL_T)
    auth_type_enables.admin.type_md2 = gh_scm2bool (scm_auth_type_md2);
  
  if (scm_boolean_p (scm_auth_type_md5) == SCM_BOOL_T)
    auth_type_enables.admin.type_md5 = gh_scm2bool (scm_auth_type_md5);
  
  if (scm_boolean_p (scm_auth_type_straight_password) == SCM_BOOL_T)
    auth_type_enables.admin.type_straight_password = 
      gh_scm2bool (scm_auth_type_straight_password);
  
  if (scm_boolean_p (scm_auth_type_oem_proprietary) == SCM_BOOL_T)
    auth_type_enables.admin.type_oem_proprietary = 
      gh_scm2bool (scm_auth_type_oem_proprietary);
  
  retval = set_bmc_lan_conf_auth_type_enables (&auth_type_enables);
  return (retval ? SCM_BOOL_F : SCM_BOOL_T);
}

SCM 
ex_set_bmc_lan_conf_auth_type_oem_enables (SCM scm_auth_type_none, 
					   SCM scm_auth_type_md2, 
					   SCM scm_auth_type_md5, 
					   SCM scm_auth_type_straight_password, 
					   SCM scm_auth_type_oem_proprietary)
{
  struct bmc_auth_level auth_type_enables;
  u_int8_t retval;
  
  retval = get_bmc_lan_conf_auth_type_enables (&auth_type_enables);
  
  if (retval)
    return (retval ? SCM_BOOL_F : SCM_BOOL_T);
  
  if (scm_boolean_p (scm_auth_type_none) == SCM_BOOL_T)
    auth_type_enables.oem.type_none = gh_scm2bool (scm_auth_type_none);
  
  if (scm_boolean_p (scm_auth_type_md2) == SCM_BOOL_T)
    auth_type_enables.oem.type_md2 = gh_scm2bool (scm_auth_type_md2);
  
  if (scm_boolean_p (scm_auth_type_md5) == SCM_BOOL_T)
    auth_type_enables.oem.type_md5 = gh_scm2bool (scm_auth_type_md5);
  
  if (scm_boolean_p (scm_auth_type_straight_password) == SCM_BOOL_T)
    auth_type_enables.oem.type_straight_password = 
      gh_scm2bool (scm_auth_type_straight_password);
  
  if (scm_boolean_p (scm_auth_type_oem_proprietary) == SCM_BOOL_T)
    auth_type_enables.oem.type_oem_proprietary = 
      gh_scm2bool (scm_auth_type_oem_proprietary);
  
  retval = set_bmc_lan_conf_auth_type_enables (&auth_type_enables);
  return (retval ? SCM_BOOL_F : SCM_BOOL_T);
}

SCM 
ex_set_bmc_lan_conf_arp_control (SCM scm_enable_gratuitous_arps, 
				 SCM scm_enable_arp_response)
{
  u_int8_t enable_gratuitous_arps;
  u_int8_t enable_arp_response;
  int retval;
  
  retval = get_bmc_lan_conf_arp_control (&enable_gratuitous_arps, 
					 &enable_arp_response);
  if (retval)
    return (retval ? SCM_BOOL_F : SCM_BOOL_T);
  
  if (scm_boolean_p (scm_enable_gratuitous_arps) == SCM_BOOL_T)
    enable_gratuitous_arps = gh_scm2bool (scm_enable_gratuitous_arps);
  
  if (scm_boolean_p (scm_enable_arp_response) == SCM_BOOL_T)
    enable_arp_response = gh_scm2bool (scm_enable_arp_response);
  
  retval = set_bmc_lan_conf_arp_control (enable_gratuitous_arps, 
					 enable_arp_response);
  return (retval ? SCM_BOOL_F : SCM_BOOL_T);
}

SCM 
ex_set_bmc_lan_conf_gratuitous_arp (SCM scm_gratuitous_arp_interval)
{
  u_int8_t gratuitous_arp_interval;
  u_int8_t retval;
  
  gratuitous_arp_interval = gh_scm2long (scm_gratuitous_arp_interval);
  retval = set_bmc_lan_conf_gratuitous_arp (gratuitous_arp_interval);
  return (retval ? SCM_BOOL_F : SCM_BOOL_T);
}

SCM 
ex_set_bmc_serial_channel_volatile_access (SCM scm_access_mode, 
					   SCM scm_enable_user_level_auth, 
					   SCM scm_enable_per_message_auth, 
					   SCM scm_enable_pef_alerting, 
					   SCM scm_channel_privilege_limit)
{
  u_int8_t access_mode; 
  u_int8_t enable_user_level_auth; 
  u_int8_t enable_per_message_auth; 
  u_int8_t enable_pef_alerting; 
  u_int8_t channel_privilege_limit;
  int retval;
  
  retval = get_bmc_serial_channel_volatile_access (&access_mode, 
						   &enable_user_level_auth, 
						   &enable_per_message_auth, 
						   &enable_pef_alerting, 
						   &channel_privilege_limit);
  if (retval)
    return (retval ? SCM_BOOL_F : SCM_BOOL_T);
  
  if (scm_integer_p (scm_access_mode) == SCM_BOOL_T)
    access_mode = gh_scm2long (scm_access_mode);
  if (scm_boolean_p (scm_enable_user_level_auth) == SCM_BOOL_T)
    enable_user_level_auth = gh_scm2bool (scm_enable_user_level_auth);
  if (scm_boolean_p (scm_enable_per_message_auth) == SCM_BOOL_T)
    enable_per_message_auth = gh_scm2bool (scm_enable_per_message_auth);
  if (scm_boolean_p (scm_enable_pef_alerting) == SCM_BOOL_T)
    enable_pef_alerting = gh_scm2bool (scm_enable_pef_alerting);
  if (scm_integer_p (scm_channel_privilege_limit) == SCM_BOOL_T)
    channel_privilege_limit = gh_scm2long (scm_channel_privilege_limit);
  
  retval = set_bmc_serial_channel_volatile_access (access_mode, 
						   enable_user_level_auth, 
						   enable_per_message_auth, 
						   enable_pef_alerting, 
						   channel_privilege_limit);
  return (retval ? SCM_BOOL_F : SCM_BOOL_T);
}

SCM 
ex_set_bmc_serial_channel_non_volatile_access (SCM scm_access_mode, 
					       SCM scm_enable_user_level_auth, 
					       SCM scm_enable_per_message_auth, 
					       SCM scm_enable_pef_alerting, 
					       SCM scm_channel_privilege_limit)
{
  u_int8_t access_mode; 
  u_int8_t enable_user_level_auth; 
  u_int8_t enable_per_message_auth; 
  u_int8_t enable_pef_alerting; 
  u_int8_t channel_privilege_limit;
  int retval;
  
  retval = get_bmc_serial_channel_non_volatile_access (&access_mode, 
						       &enable_user_level_auth, 
						       &enable_per_message_auth, 
						       &enable_pef_alerting, 
						       &channel_privilege_limit);
  if (retval)
    return (retval ? SCM_BOOL_F : SCM_BOOL_T);
  
  if (scm_integer_p (scm_access_mode) == SCM_BOOL_T)
    access_mode = gh_scm2long (scm_access_mode);
  if (scm_boolean_p (scm_enable_user_level_auth) == SCM_BOOL_T)
    enable_user_level_auth = gh_scm2bool (scm_enable_user_level_auth);
  if (scm_boolean_p (scm_enable_per_message_auth) == SCM_BOOL_T)
    enable_per_message_auth = gh_scm2bool (scm_enable_per_message_auth);
  if (scm_boolean_p (scm_enable_pef_alerting) == SCM_BOOL_T)
    enable_pef_alerting = gh_scm2bool (scm_enable_pef_alerting);
  if (scm_integer_p (scm_channel_privilege_limit) == SCM_BOOL_T)
    channel_privilege_limit = gh_scm2long (scm_channel_privilege_limit);
  
  retval = set_bmc_serial_channel_non_volatile_access (access_mode, 
						       enable_user_level_auth, 
						       enable_per_message_auth, 
						       enable_pef_alerting, 
						       channel_privilege_limit);
  return (retval ? SCM_BOOL_F : SCM_BOOL_T);
}

SCM 
ex_set_bmc_serial_conf_conn_mode (SCM scm_enable_basic_mode, 
				  SCM scm_enable_ppp_mode, 
				  SCM scm_enable_terminal_mode, 
				  SCM scm_connect_mode)
{
  u_int8_t enable_basic_mode; 
  u_int8_t enable_ppp_mode;
  u_int8_t enable_terminal_mode;
  u_int8_t connect_mode;
  int retval;
  
  retval = get_bmc_serial_conf_conn_mode (&enable_basic_mode, 
					  &enable_ppp_mode, 
					  &enable_terminal_mode, 
					  &connect_mode);
  if (retval)
    return (retval ? SCM_BOOL_F : SCM_BOOL_T);
  
  if (scm_boolean_p (scm_enable_basic_mode) == SCM_BOOL_T)
    enable_basic_mode = gh_scm2bool (scm_enable_basic_mode);
  if (scm_boolean_p (scm_enable_ppp_mode) == SCM_BOOL_T)
    enable_ppp_mode = gh_scm2bool (scm_enable_ppp_mode);
  if (scm_boolean_p (scm_enable_terminal_mode) == SCM_BOOL_T)
    enable_terminal_mode = gh_scm2bool (scm_enable_terminal_mode);
  if (scm_integer_p (scm_enable_terminal_mode) == SCM_BOOL_T)
    connect_mode = gh_scm2bool (scm_connect_mode);
  
  retval = set_bmc_serial_conf_conn_mode (enable_basic_mode, 
					  enable_ppp_mode, 
					  enable_terminal_mode, 
					  connect_mode);
  return (retval ? SCM_BOOL_F : SCM_BOOL_T);
}

SCM 
ex_set_bmc_serial_conf_page_blackout_interval (SCM scm_page_blackout_interval)
{
  u_int8_t page_blackout_interval;
  int retval;
  
  page_blackout_interval = gh_scm2long (scm_page_blackout_interval);
  retval = set_bmc_serial_conf_page_blackout_interval (page_blackout_interval);
  return (retval ? SCM_BOOL_F : SCM_BOOL_T);
}

SCM 
ex_set_bmc_serial_conf_call_retry_time (SCM scm_call_retry_time)
{
  u_int8_t call_retry_time;
  int retval;
  
  call_retry_time = gh_scm2long (scm_call_retry_time);
  retval = set_bmc_serial_conf_call_retry_time (call_retry_time);
  return (retval ? SCM_BOOL_F : SCM_BOOL_T);
}

SCM 
ex_set_bmc_serial_conf_ipmi_msg_comm_settings (SCM scm_enable_dtr_hangup, 
					       SCM scm_flow_control, 
					       SCM scm_bit_rate)
{
  u_int8_t enable_dtr_hangup;
  u_int8_t flow_control;
  u_int8_t bit_rate;
  int retval;
  
  retval = get_bmc_serial_conf_ipmi_msg_comm_settings (&enable_dtr_hangup, 
						       &flow_control, 
						       &bit_rate);
  if (retval)
    return (retval ? SCM_BOOL_F : SCM_BOOL_T);
  
  if (scm_boolean_p (scm_enable_dtr_hangup) == SCM_BOOL_T)
    enable_dtr_hangup = gh_scm2bool (scm_enable_dtr_hangup);
  if (scm_integer_p (scm_flow_control) == SCM_BOOL_T)
    flow_control = gh_scm2long (scm_flow_control);
  if (scm_integer_p (scm_bit_rate) == SCM_BOOL_T)
    bit_rate = gh_scm2long (scm_bit_rate);
  
  retval = set_bmc_serial_conf_ipmi_msg_comm_settings (enable_dtr_hangup, 
						       flow_control, 
						       bit_rate);
  return (retval ? SCM_BOOL_F : SCM_BOOL_T);
}

SCM 
ex_set_bmc_power_restore_policy (SCM scm_power_restore_policy)
{
  u_int8_t power_restore_policy;
  int retval;
  
  power_restore_policy = gh_scm2long (scm_power_restore_policy);
  retval = set_bmc_power_restore_policy (power_restore_policy);
  return (retval ? SCM_BOOL_F : SCM_BOOL_T);
}

/**** get_XXXX functions *****/
SCM 
ex_get_bmc_username (SCM scm_userid)
{
  u_int8_t userid;
  char username[17];
  int retval;
  SCM return_list = SCM_EOL;
  
  userid = gh_scm2long (scm_userid);
  memset (username, 0, 17);
  if ((retval = get_bmc_username (userid, username)) == 0)
    return_list = gh_list (gh_str02scm (username), SCM_UNDEFINED);
  
  return (retval ? SCM_BOOL_F : return_list);
}

SCM 
ex_get_bmc_user_lan_channel_access (SCM scm_userid)
{
  u_int8_t userid;
  u_int8_t lan_enable_ipmi_msgs = 0;
  u_int8_t lan_enable_link_auth = 0;
  u_int8_t lan_enable_restrict_to_callback = 0;
  u_int8_t lan_privilege_limit = 0;
  u_int8_t lan_session_limit = 0;
  int retval;
  SCM return_list = SCM_EOL;
  
  userid = gh_scm2long (scm_userid);
  if ((retval = get_bmc_user_lan_channel_access (userid, 
						 &lan_enable_ipmi_msgs, 
						 &lan_enable_link_auth, 
						 &lan_enable_restrict_to_callback, 
						 &lan_privilege_limit, 
						 &lan_session_limit)) == 0)
    {
      return_list = gh_list (gh_bool2scm (lan_enable_ipmi_msgs), 
			     gh_bool2scm (lan_enable_link_auth), 
			     gh_bool2scm (lan_enable_restrict_to_callback), 
			     gh_long2scm (lan_privilege_limit), 
			     gh_long2scm (lan_session_limit), 
			     SCM_UNDEFINED);
    }
  
  return (retval ? SCM_BOOL_F : return_list);
}

SCM 
ex_get_bmc_user_serial_channel_access (SCM scm_userid)
{
  u_int8_t userid;
  u_int8_t serial_enable_ipmi_msgs = 0;
  u_int8_t serial_enable_link_auth = 0;
  u_int8_t serial_enable_restrict_to_callback = 0;
  u_int8_t serial_privilege_limit = 0;
  u_int8_t serial_session_limit = 0;
  int retval;
  SCM return_list = SCM_EOL;
  
  userid = gh_scm2long (scm_userid);
  if ((retval = get_bmc_user_serial_channel_access (userid, 
						    &serial_enable_ipmi_msgs, 
						    &serial_enable_link_auth, 
						    &serial_enable_restrict_to_callback, 
						    &serial_privilege_limit, 
						    &serial_session_limit)) == 0)
    {
      return_list = gh_list (gh_bool2scm (serial_enable_ipmi_msgs), 
			     gh_bool2scm (serial_enable_link_auth), 
			     gh_bool2scm (serial_enable_restrict_to_callback), 
			     gh_long2scm (serial_privilege_limit), 
			     gh_long2scm (serial_session_limit), 
			     SCM_UNDEFINED);
    }
  
  return (retval ? SCM_BOOL_F : return_list);
}

SCM 
ex_get_bmc_lan_channel_volatile_access ()
{
  u_int8_t access_mode = 0;
  u_int8_t enable_user_level_auth = 0;
  u_int8_t enable_per_message_auth = 0;
  u_int8_t enable_pef_alerting = 0;
  u_int8_t channel_privilege_limit = 0;
  int retval;
  SCM return_list = SCM_EOL;
  
  if ((retval = get_bmc_lan_channel_volatile_access (&access_mode, 
						     &enable_user_level_auth, 
						     &enable_per_message_auth, 
						     &enable_pef_alerting, 
						     &channel_privilege_limit)) == 0)
    {
      return_list = gh_list (gh_long2scm (access_mode), 
			     gh_bool2scm (enable_user_level_auth), 
			     gh_bool2scm (enable_per_message_auth), 
			     gh_bool2scm (enable_pef_alerting), 
			     gh_long2scm (channel_privilege_limit), 
			     SCM_UNDEFINED);
    }
  
  return (retval ? SCM_BOOL_F : return_list);
}

SCM 
ex_get_bmc_lan_channel_non_volatile_access ()
{
  u_int8_t access_mode = 0;
  u_int8_t enable_user_level_auth = 0;
  u_int8_t enable_per_message_auth = 0;
  u_int8_t enable_pef_alerting = 0;
  u_int8_t channel_privilege_limit = 0;
  int retval;
  SCM return_list = SCM_EOL;
  
  if ((retval = get_bmc_lan_channel_non_volatile_access (&access_mode, 
							 &enable_user_level_auth, 
							 &enable_per_message_auth, 
							 &enable_pef_alerting, 
							 &channel_privilege_limit)) == 0)
    {
      return_list = gh_list (gh_long2scm (access_mode), 
			     gh_bool2scm (enable_user_level_auth), 
			     gh_bool2scm (enable_per_message_auth), 
			     gh_bool2scm (enable_pef_alerting), 
			     gh_long2scm (channel_privilege_limit), 
			     SCM_UNDEFINED);
    }
  
  return (retval ? SCM_BOOL_F : return_list);
}

SCM 
ex_get_bmc_lan_conf_ip_addr_source ()
{
  u_int8_t ip_address_source = 0;
  int retval;
  SCM return_list = SCM_EOL;
  
  if ((retval = get_bmc_lan_conf_ip_addr_source (&ip_address_source)) == 0)
    return_list = gh_list (gh_long2scm (ip_address_source), SCM_UNDEFINED);
  
  return (retval ? SCM_BOOL_F : return_list);
}

SCM 
ex_get_bmc_lan_conf_ip_addr ()
{
  char ip_address[16];
  int retval;
  SCM return_list = SCM_EOL;
  
  if ((retval = get_bmc_lan_conf_ip_addr (ip_address)) == 0)
    return_list = gh_list (gh_str02scm (ip_address), SCM_UNDEFINED);
  
  return (retval ? SCM_BOOL_F : return_list);
}

SCM 
ex_get_bmc_lan_conf_mac_addr ()
{
  char mac_address[18];
  int retval;
  SCM return_list = SCM_EOL;
  
  if ((retval = get_bmc_lan_conf_mac_addr (mac_address)) == 0)
    return_list = gh_list (gh_str02scm (mac_address), SCM_UNDEFINED);
  
  return (retval ? SCM_BOOL_F : return_list);
}

SCM 
ex_get_bmc_lan_conf_subnet_mask ()
{
  char subnet_mask[16];
  int retval;
  SCM return_list = SCM_EOL;
  
  if ((retval = get_bmc_lan_conf_subnet_mask (subnet_mask)) == 0)
    return_list = gh_list (gh_str02scm (subnet_mask), SCM_UNDEFINED);
  
  return (retval ? SCM_BOOL_F : return_list);
}

SCM 
ex_get_bmc_lan_conf_default_gw_ip_addr ()
{
  char gw_ip_address[16];
  int retval;
  SCM return_list = SCM_EOL;
  
  if ((retval = get_bmc_lan_conf_default_gw_ip_addr (gw_ip_address)) == 0)
    return_list = gh_list (gh_str02scm (gw_ip_address), SCM_UNDEFINED);
  
  return (retval ? SCM_BOOL_F : return_list);
}

SCM 
ex_get_bmc_lan_conf_default_gw_mac_addr ()
{
  char gw_mac_address[18];
  int retval;
  SCM return_list = SCM_EOL;
  
  if ((retval = get_bmc_lan_conf_default_gw_mac_addr (gw_mac_address)) == 0)
    return_list = gh_list (gh_str02scm (gw_mac_address), SCM_UNDEFINED);
  
  return (retval ? SCM_BOOL_F : return_list);
}

SCM 
ex_get_bmc_lan_conf_backup_gw_ip_addr ()
{
  char gw_ip_address[16];
  int retval;
  SCM return_list = SCM_EOL;
  
  if ((retval = get_bmc_lan_conf_backup_gw_ip_addr (gw_ip_address)) == 0)
    return_list = gh_list (gh_str02scm (gw_ip_address), SCM_UNDEFINED);
  
  return (retval ? SCM_BOOL_F : return_list);
}

SCM 
ex_get_bmc_lan_conf_backup_gw_mac_addr (SCM scm_gw_mac_address)
{
  char gw_mac_address[18];
  int retval;
  SCM return_list = SCM_EOL;
  
  if ((retval = get_bmc_lan_conf_backup_gw_mac_addr (gw_mac_address)) == 0)
    return_list = gh_list (gh_str02scm (gw_mac_address), SCM_UNDEFINED);
  
  return (retval ? SCM_BOOL_F : return_list);
}

SCM
ex_get_bmc_lan_conf_vlan_id ()
{
  u_int8_t vlan_id_flag = 0;
  u_int32_t vlan_id = 0;
  int retval;
  SCM return_list = SCM_EOL;

  if ((retval = get_bmc_lan_conf_vlan_id (&vlan_id_flag, &vlan_id)) == 0)
    return_list = gh_list (gh_bool2scm (vlan_id_flag), 
			   gh_long2scm (vlan_id), 
			   SCM_UNDEFINED);

  return (retval ? SCM_BOOL_F : return_list);
}

SCM
ex_get_bmc_lan_conf_vlan_priority ()
{
  u_int8_t vlan_priority = 0;
  int retval;
  SCM return_list = SCM_EOL;

  if ((retval = get_bmc_lan_conf_vlan_priority (&vlan_priority)) == 0)
    return_list = gh_list (gh_long2scm (vlan_priority), SCM_UNDEFINED);

  return (retval ? SCM_BOOL_F : return_list);
}

SCM 
ex_get_bmc_lan_conf_auth_type_callback_enables ()
{
  struct bmc_auth_level auth_type_enables;
  u_int8_t retval;
  SCM return_list = SCM_EOL;
  
  if ((retval = get_bmc_lan_conf_auth_type_enables (&auth_type_enables)) == 0)
    {
      return_list = gh_list (gh_bool2scm (auth_type_enables.callback.type_none), 
			     gh_bool2scm (auth_type_enables.callback.type_md2), 
			     gh_bool2scm (auth_type_enables.callback.type_md5), 
			     gh_bool2scm (auth_type_enables.callback.type_straight_password), 
			     gh_bool2scm (auth_type_enables.callback.type_oem_proprietary), 
			     SCM_UNDEFINED);
    }
  
  return (retval ? SCM_BOOL_F : return_list);
}

SCM 
ex_get_bmc_lan_conf_auth_type_user_enables ()
{
  struct bmc_auth_level auth_type_enables;
  u_int8_t retval;
  SCM return_list = SCM_EOL;
  
  if ((retval = get_bmc_lan_conf_auth_type_enables (&auth_type_enables)) == 0)
    {
      return_list = gh_list (gh_bool2scm (auth_type_enables.user.type_none), 
			     gh_bool2scm (auth_type_enables.user.type_md2), 
			     gh_bool2scm (auth_type_enables.user.type_md5), 
			     gh_bool2scm (auth_type_enables.user.type_straight_password), 
			     gh_bool2scm (auth_type_enables.user.type_oem_proprietary), 
			     SCM_UNDEFINED);
    }
  
  return (retval ? SCM_BOOL_F : return_list);
}

SCM 
ex_get_bmc_lan_conf_auth_type_operator_enables ()
{
  struct bmc_auth_level auth_type_enables;
  u_int8_t retval;
  SCM return_list = SCM_EOL;
  
  if ((retval = get_bmc_lan_conf_auth_type_enables (&auth_type_enables)) == 0)
    {
      return_list = gh_list (gh_bool2scm (auth_type_enables.operator.type_none), 
			     gh_bool2scm (auth_type_enables.operator.type_md2), 
			     gh_bool2scm (auth_type_enables.operator.type_md5), 
			     gh_bool2scm (auth_type_enables.operator.type_straight_password), 
			     gh_bool2scm (auth_type_enables.operator.type_oem_proprietary), 
			     SCM_UNDEFINED);
    }
  
  return (retval ? SCM_BOOL_F : return_list);
}

SCM 
ex_get_bmc_lan_conf_auth_type_admin_enables ()
{
  struct bmc_auth_level auth_type_enables;
  u_int8_t retval;
  SCM return_list = SCM_EOL;
  
  if ((retval = get_bmc_lan_conf_auth_type_enables (&auth_type_enables)) == 0)
    {
      return_list = gh_list (gh_bool2scm (auth_type_enables.admin.type_none), 
			     gh_bool2scm (auth_type_enables.admin.type_md2), 
			     gh_bool2scm (auth_type_enables.admin.type_md5), 
			     gh_bool2scm (auth_type_enables.admin.type_straight_password), 
			     gh_bool2scm (auth_type_enables.admin.type_oem_proprietary), 
			     SCM_UNDEFINED);
    }
  
  return (retval ? SCM_BOOL_F : return_list);
}

SCM 
ex_get_bmc_lan_conf_auth_type_oem_enables ()
{
  struct bmc_auth_level auth_type_enables;
  u_int8_t retval;
  SCM return_list = SCM_EOL;
  
  if ((retval = get_bmc_lan_conf_auth_type_enables (&auth_type_enables)) == 0)
    {
      return_list = gh_list (gh_bool2scm (auth_type_enables.oem.type_none), 
			     gh_bool2scm (auth_type_enables.oem.type_md2), 
			     gh_bool2scm (auth_type_enables.oem.type_md5), 
			     gh_bool2scm (auth_type_enables.oem.type_straight_password), 
			     gh_bool2scm (auth_type_enables.oem.type_oem_proprietary), 
			     SCM_UNDEFINED);
    }
  
  return (retval ? SCM_BOOL_F : return_list);
}

SCM 
ex_get_bmc_lan_conf_arp_control ()
{
  u_int8_t enable_gratuitous_arps;
  u_int8_t enable_arp_response;
  int retval;
  SCM return_list = SCM_EOL;
  
  if ((retval = get_bmc_lan_conf_arp_control (&enable_gratuitous_arps, 
					      &enable_arp_response)) == 0)
    {
      return_list = gh_list (gh_bool2scm (enable_gratuitous_arps), 
			     gh_bool2scm (enable_arp_response), 
			     SCM_UNDEFINED);
    }
  
  return (retval ? SCM_BOOL_F : return_list);
}

SCM 
ex_get_bmc_lan_conf_gratuitous_arp ()
{
  u_int8_t gratuitous_arp_interval;
  int retval;
  SCM return_list = SCM_EOL;
  
  if ((retval = get_bmc_lan_conf_gratuitous_arp (&gratuitous_arp_interval)) == 0)
    return_list = gh_list (gh_long2scm (gratuitous_arp_interval), SCM_UNDEFINED);
  
  return (retval ? SCM_BOOL_F : return_list);
}

SCM 
ex_get_bmc_serial_channel_volatile_access ()
{
  u_int8_t access_mode = 0;
  u_int8_t enable_user_level_auth = 0;
  u_int8_t enable_per_message_auth = 0;
  u_int8_t enable_pef_alerting = 0;
  u_int8_t channel_privilege_limit = 0;
  int retval;
  SCM return_list = SCM_EOL;
  
  if ((retval = get_bmc_serial_channel_volatile_access (&access_mode, 
							&enable_user_level_auth, 
							&enable_per_message_auth, 
							&enable_pef_alerting, 
							&channel_privilege_limit)) == 0)
    {
      return_list = gh_list (gh_long2scm (access_mode), 
			     gh_bool2scm (enable_user_level_auth), 
			     gh_bool2scm (enable_per_message_auth), 
			     gh_bool2scm (enable_pef_alerting), 
			     gh_long2scm (channel_privilege_limit), 
			     SCM_UNDEFINED);
    }
  
  return (retval ? SCM_BOOL_F : return_list);
}

SCM 
ex_get_bmc_serial_channel_non_volatile_access ()
{
  u_int8_t access_mode = 0;
  u_int8_t enable_user_level_auth = 0;
  u_int8_t enable_per_message_auth = 0;
  u_int8_t enable_pef_alerting = 0;
  u_int8_t channel_privilege_limit = 0;
  int retval;
  SCM return_list = SCM_EOL;
  
  if ((retval = get_bmc_serial_channel_non_volatile_access (&access_mode, 
							    &enable_user_level_auth, 
							    &enable_per_message_auth, 
							    &enable_pef_alerting, 
							    &channel_privilege_limit)) == 0)
    {
      return_list = gh_list (gh_long2scm (access_mode), 
			     gh_bool2scm (enable_user_level_auth), 
			     gh_bool2scm (enable_per_message_auth), 
			     gh_bool2scm (enable_pef_alerting), 
			     gh_long2scm (channel_privilege_limit), 
			     SCM_UNDEFINED);
    }
  
  return (retval ? SCM_BOOL_F : return_list);
}

SCM 
ex_get_bmc_serial_conf_conn_mode ()
{
  u_int8_t enable_basic_mode = 0;
  u_int8_t enable_ppp_mode = 0;
  u_int8_t enable_terminal_mode = 0;
  u_int8_t connect_mode = 0;
  int retval;
  SCM return_list = SCM_EOL;
  
  if ((retval = get_bmc_serial_conf_conn_mode (&enable_basic_mode, 
					       &enable_ppp_mode, 
					       &enable_terminal_mode, 
					       &connect_mode)) == 0)
    {
      return_list = gh_list (gh_bool2scm (enable_basic_mode), 
			     gh_bool2scm (enable_ppp_mode), 
			     gh_bool2scm (enable_terminal_mode), 
			     gh_long2scm (connect_mode), 
			     SCM_UNDEFINED);
    }
  
  return (retval ? SCM_BOOL_F : return_list);
}

SCM 
ex_get_bmc_serial_conf_page_blackout_interval ()
{
  u_int8_t page_blackout_interval = 0;
  int retval;
  SCM return_list = SCM_EOL;
  
  if ((retval = get_bmc_serial_conf_page_blackout_interval (&page_blackout_interval)) == 0)
    return_list = gh_list (gh_long2scm (page_blackout_interval), SCM_UNDEFINED);
  
  return (retval ? SCM_BOOL_F : return_list);
}

SCM 
ex_get_bmc_serial_conf_call_retry_time ()
{
  u_int8_t call_retry_time = 0;
  int retval;
  SCM return_list = SCM_EOL;
  
  if ((retval = get_bmc_serial_conf_call_retry_time (&call_retry_time)) == 0)
    return_list = gh_list (gh_long2scm (call_retry_time), SCM_UNDEFINED);
  
  return (retval ? SCM_BOOL_F : return_list);
}

SCM 
ex_get_bmc_serial_conf_ipmi_msg_comm_settings ()
{
  u_int8_t enable_dtr_hangup = 0;
  u_int8_t flow_control = 0;
  u_int8_t bit_rate = 0;
  int retval;
  SCM return_list = SCM_EOL;
  
  if ((retval = get_bmc_serial_conf_ipmi_msg_comm_settings (&enable_dtr_hangup, 
							    &flow_control, 
							    &bit_rate)) == 0)
    {
      return_list = gh_list (gh_bool2scm (enable_dtr_hangup), 
			     gh_long2scm (flow_control), 
			     gh_long2scm (bit_rate), 
			     SCM_UNDEFINED);
    }
  
  return (retval ? SCM_BOOL_F : return_list);
}

SCM 
ex_get_bmc_power_restore_policy ()
{
  u_int8_t power_restore_policy = 0;
  int retval;
  SCM return_list = SCM_EOL;
  
  if ((retval = get_bmc_power_restore_policy (&power_restore_policy)) == 0)
    return_list = gh_list (gh_long2scm (power_restore_policy), SCM_UNDEFINED);
  
  return (retval ? SCM_BOOL_F : return_list);
}

/***********************************************************/
SCM 
ex_check_bmc_user_password (SCM scm_userid, SCM scm_password)
{
  u_int8_t userid;
  u_int8_t *password = NULL;
  int retval;
  
  userid = gh_scm2long (scm_userid);
  password = gh_scm2newstr (scm_password, NULL);
  
  retval = check_bmc_user_password (userid, password);
  
  free (password);
  
  return (retval ? SCM_BOOL_T : SCM_BOOL_F);
}

/*********** new sensor procedures ***************/
SCM 
get_scm_sdr_full_record (struct sdr_full_record *record, 
			 SCM scm_sdr_record)
{
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("readable_lower_non_recoverable_threshold"), 
				    (record->readable_lower_non_recoverable_threshold ? 
				     SCM_BOOL_T : SCM_BOOL_F));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("readable_upper_non_recoverable_threshold"), 
				    (record->readable_upper_non_recoverable_threshold ? 
				     SCM_BOOL_T : SCM_BOOL_F));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("readable_lower_critical_threshold"), 
				    (record->readable_lower_critical_threshold ? 
				     SCM_BOOL_T : SCM_BOOL_F));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("readable_upper_critical_threshold"), 
				    (record->readable_upper_critical_threshold ? 
				     SCM_BOOL_T : SCM_BOOL_F));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("readable_lower_non_critical_threshold"), 
				    (record->readable_lower_non_critical_threshold ? 
				     SCM_BOOL_T : SCM_BOOL_F));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("readable_upper_non_critical_threshold"), 
				    (record->readable_upper_non_critical_threshold ? 
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
				    gh_long2scm (record->slave_system_software_id));
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
				    gh_long2scm (record->event_reading_type));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("sensor_unit"), 
				    gh_long2scm (record->sensor_unit));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("unit_short_string"), 
				    gh_str02scm (ipmi_sensor_units_short[record->sensor_unit]));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("unit_string"), 
				    gh_str02scm (ipmi_sensor_units[record->sensor_unit]));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("nominal_reading"), 
				    gh_double2scm (record->nominal_reading));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("normal_min"), 
				    gh_double2scm (record->normal_min));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("normal_max"), 
				    gh_double2scm (record->normal_max));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("sensor_min_reading"), 
				    gh_double2scm (record->sensor_min_reading));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("sensor_max_reading"), 
				    gh_double2scm (record->sensor_max_reading));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("negative_hysteresis"), 
				    gh_long2scm (record->negative_hysteresis));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("positive_hysteresis"), 
				    gh_long2scm (record->positive_hysteresis));
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

SCM 
get_scm_sdr_compact_record (struct sdr_compact_record *record, 
			    SCM scm_sdr_record)
{
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("slave_system_software_id"), 
				    gh_long2scm (record->slave_system_software_id));
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
				    gh_long2scm (record->event_reading_type));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("sensor_unit"), 
				    gh_long2scm (record->sensor_unit));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("unit_short_string"), 
				    gh_str02scm (ipmi_sensor_units_short[record->sensor_unit]));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("unit_string"), 
				    gh_str02scm (ipmi_sensor_units[record->sensor_unit]));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("negative_hysteresis"), 
				    gh_long2scm (record->negative_hysteresis));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("positive_hysteresis"), 
				    gh_long2scm (record->positive_hysteresis));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("sensor_name"), 
				    gh_str02scm (record->sensor_name));
  
  return scm_sdr_record;
}

SCM 
get_scm_sdr_event_only_record (struct sdr_event_only_record *record, 
			       SCM scm_sdr_record)
{
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("slave_system_software_id"), 
				    gh_long2scm (record->slave_system_software_id));
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
				    gh_long2scm (record->event_reading_type));
  scm_sdr_record = scm_assoc_set_x (scm_sdr_record, 
				    gh_str02scm ("sensor_name"), 
				    gh_str02scm (record->sensor_name));
  
  return scm_sdr_record;
}

SCM 
get_scm_sdr_entity_association_record (struct sdr_entity_association_record *record, 
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

SCM 
get_scm_sdr_generic_device_locator_record (struct sdr_generic_device_locator_record *record, 
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
				    gh_long2scm (record->lun_master_write_read_command));
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

SCM 
get_scm_sdr_logical_fru_device_locator_record (struct sdr_logical_fru_device_locator_record *record, 
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

SCM 
get_scm_sdr_management_controller_device_locator_record (struct sdr_management_controller_device_locator_record *record, 
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

SCM 
get_scm_sdr_oem_record (struct sdr_oem_record *record, 
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
  struct sdr_record sdr_record;
  SCM scm_sdr_record = SCM_EOL;
  u_int16_t record_id = 0;
  u_int16_t next_record_id = 0;
  
  record_id = gh_scm2long (scm_record_id);
  
  memset (&sdr_record, 0, sizeof (struct sdr_record));
  
  if (get_sdr_record (record_id, &next_record_id, &sdr_record))
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

void 
scm2sdr_full_record (SCM scm_sdr_record, struct sdr_full_record *record)
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
  record->slave_system_software_id = gh_scm2long (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("sensor_number"));
  record->sensor_number = gh_scm2long (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("sensor_type"));
  record->sensor_type = gh_scm2long (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("event_reading_type"));
  record->event_reading_type = gh_scm2long (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("sensor_unit"));
  record->sensor_unit = gh_scm2long (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("nominal_reading"));
  record->nominal_reading = gh_scm2double (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("normal_min"));
  record->normal_min = gh_scm2double (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("normal_max"));
  record->normal_max = gh_scm2double (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("sensor_min_reading"));
  record->sensor_min_reading = gh_scm2double (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("sensor_max_reading"));
  record->sensor_max_reading = gh_scm2double (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("negative_hysteresis"));
  record->negative_hysteresis = gh_scm2long (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("positive_hysteresis"));
  record->positive_hysteresis = gh_scm2long (scm_value);
  
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
  record->readable_lower_critical_threshold = gh_scm2bool (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("readable_upper_critical_threshold"));
  record->readable_upper_critical_threshold = gh_scm2bool (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("readable_lower_non_critical_threshold"));
  record->readable_lower_non_critical_threshold = gh_scm2bool (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("readable_upper_non_critical_threshold"));
  record->readable_upper_non_critical_threshold = gh_scm2bool (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("readable_lower_non_recoverable_threshold"));
  record->readable_lower_non_recoverable_threshold = gh_scm2bool (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("readable_upper_non_recoverable_threshold"));
  record->readable_upper_non_recoverable_threshold = gh_scm2bool (scm_value);
  
  return;
}

void 
scm2sdr_compact_record (SCM scm_sdr_record, struct sdr_compact_record *record)
{
  SCM scm_value;
  char *sensor_name_ptr = NULL;
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("slave_system_software_id"));
  record->slave_system_software_id = gh_scm2long (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("sensor_number"));
  record->sensor_number = gh_scm2long (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("sensor_type"));
  record->sensor_type = gh_scm2long (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("event_reading_type"));
  record->event_reading_type = gh_scm2long (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("sensor_unit"));
  record->sensor_unit = gh_scm2long (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("negative_hysteresis"));
  record->negative_hysteresis = gh_scm2long (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("positive_hysteresis"));
  record->positive_hysteresis = gh_scm2long (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("sensor_name"));
  sensor_name_ptr = gh_scm2newstr (scm_value, NULL);
  strncpy (record->sensor_name, sensor_name_ptr, 16);
  free (sensor_name_ptr);
  
  return;
}

void 
scm2sdr_event_only_record (SCM scm_sdr_record, struct sdr_event_only_record *record)
{
  SCM scm_value;
  char *sensor_name_ptr = NULL;
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("slave_system_software_id"));
  record->slave_system_software_id = gh_scm2long (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("sensor_number"));
  record->sensor_number = gh_scm2long (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("sensor_type"));
  record->sensor_type = gh_scm2long (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("event_reading_type"));
  record->event_reading_type = gh_scm2long (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("sensor_name"));
  sensor_name_ptr = gh_scm2newstr (scm_value, NULL);
  strncpy (record->sensor_name, sensor_name_ptr, 16);
  free (sensor_name_ptr);
  
  return;
}

void 
scm2sdr_entity_association_record (SCM scm_sdr_record, struct sdr_entity_association_record *record)
{
  SCM scm_value;
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("container_entity_id"));
  record->container_entity_id = gh_scm2long (scm_value);
  
  scm_value = scm_assoc_ref (scm_sdr_record, gh_str02scm ("container_entity_instance"));
  record->container_entity_instance = gh_scm2long (scm_value);
  
  return;
}

void 
scm2sdr_generic_device_locator_record (SCM scm_sdr_record, struct sdr_generic_device_locator_record *record)
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
  record->lun_master_write_read_command = gh_scm2long (scm_value);
  
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

void 
scm2sdr_logical_fru_device_locator_record (SCM scm_sdr_record, struct sdr_logical_fru_device_locator_record *record)
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

void 
scm2sdr_management_controller_device_locator_record (SCM scm_sdr_record, struct sdr_management_controller_device_locator_record *record)
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

void 
scm2sdr_oem_record (SCM scm_sdr_record, struct sdr_oem_record *record)
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

int 
scm2sdr_record (SCM scm_sdr_record, struct sdr_record *sdr_record)
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
  struct sensor_reading sensor_reading;
  struct sdr_record sdr_record;
  SCM scm_sensor_reading = SCM_EOL;
  int i;
  SCM scm_event_message_list = SCM_EOL;
  
  scm2sdr_record (scm_sdr_record, &sdr_record);
  
  memset (&sensor_reading, 0, sizeof (struct sensor_reading));
  if (get_sensor_reading (&sdr_record, &sensor_reading) != 0)
    return SCM_BOOL_F;
  
  scm_sensor_reading = scm_assoc_set_x (scm_sensor_reading, 
					gh_str02scm ("current_reading"), 
					gh_double2scm (sensor_reading.current_reading));
  scm_sensor_reading = scm_assoc_set_x (scm_sensor_reading, 
					gh_str02scm ("reading_availability_flag"), 
					(sensor_reading.reading_availability_flag ? 
					 SCM_BOOL_F : SCM_BOOL_T));
  scm_sensor_reading = scm_assoc_set_x (scm_sensor_reading, 
					gh_str02scm ("sensor_scanning_flag"), 
					(sensor_reading.sensor_scanning_flag ? 
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
ex_get_sdr_repo_info ()
{
  SCM scm_repo_info_list = SCM_EOL;
  
  u_int8_t *data_rs = NULL;
  
  char version_string[17];
  u_int8_t sdr_major_version;
  u_int8_t sdr_minor_version;
  
  u_int64_t val;
  
  /* get_repo_info */
  data_rs = alloca (fiid_obj_len_bytes (tmpl_get_sdr_repo_info_rs));
  if (ipmi_kcs_get_repo_info (data_rs) != 0)
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
  scm_repo_info_list = scm_assoc_set_x (scm_repo_info_list, 
					gh_str02scm ("sdr_version"), 
					gh_str02scm (version_string));
  
  fiid_obj_get (data_rs,
		tmpl_get_sdr_repo_info_rs,
		"record_count",
		&val);
  scm_repo_info_list = scm_assoc_set_x (scm_repo_info_list, 
					gh_str02scm ("record_count"), 
					gh_long2scm (val));
  
  fiid_obj_get (data_rs,
		tmpl_get_sdr_repo_info_rs,
		"free_space",
		&val);
  scm_repo_info_list = scm_assoc_set_x (scm_repo_info_list, 
					gh_str02scm ("free_space"), 
					gh_long2scm (val));
  
  fiid_obj_get (data_rs,
		tmpl_get_sdr_repo_info_rs,
		"recent_addition_timestamp",
		&val);
  scm_repo_info_list = scm_assoc_set_x (scm_repo_info_list, 
					gh_str02scm ("recent_addition_timestamp"), 
					gh_ulong2scm (val));
  
  fiid_obj_get (data_rs,
		tmpl_get_sdr_repo_info_rs,
		"recent_erase_timestamp",
		&val);
  scm_repo_info_list = scm_assoc_set_x (scm_repo_info_list, 
					gh_str02scm ("recent_erase_timestamp"), 
					gh_ulong2scm (val));
  
  return (scm_repo_info_list);
}

