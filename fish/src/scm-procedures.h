/* 
   scm_procedures.h: scm procedures that are exported to
   guile environment
   
   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2, or (at
   your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA. */

#ifndef _SCM_PROCEDURES_H
#define _SCM_PROCEDURES_H


SCM ex_version (void);
SCM ex_display (SCM scm_message);
SCM ex_bell (void);
SCM ex_toggle_x (SCM scm_what2toggle);
SCM ex_hook_return (void);
SCM ex_register_command_x (SCM command);
SCM ex_unregister_command_x (SCM command);
SCM ex_set_prompt_x (SCM scm_prompt);
SCM ex_exit (SCM scm_status);
SCM ex_quit ();
SCM ex_set_sms_io_base (SCM scm_sms_io_base);

SCM ex_load (SCM scm_filename);
SCM ex_get_sysconfig_dir (void);

SCM ex_ipmi_ping (SCM scm_host_addr);
SCM ex_kcs_get_dev_id_display (void);

SCM ex_get_sock_timeout ();
SCM ex_set_sock_timeout (SCM scm_sock_timeout);
SCM ex_set_driver_poll_interval (SCM scm_driver_poll_interval);
SCM ex_get_script_command_line ();

SCM ex_bmc_config_checkout (SCM scm_filename);
SCM ex_bmc_config_commit (SCM scm_filename);
SCM ex_bmc_config_edit_key_pair (SCM scm_filename, SCM scm_key, SCM scm_value);
SCM ex_bmc_config_diff_key_pair (SCM scm_filename, SCM scm_key, SCM scm_value);
SCM ex_bmc_config_diff_file (SCM scm_bmc_filename, SCM scm_filename);
SCM ex_bmc_config_check_key (SCM scm_key);

SCM ex_sensors_cache_create (SCM scm_cache_filename);
SCM ex_get_default_sdr_repo_cache_filename (void);
SCM ex_sensors_cache_load (SCM scm_cache_filename);
SCM ex_sensors_cache_unload ();
SCM ex_sensors_cache_seek (SCM scm_rec_id);
SCM ex_sensors_cache_first ();
SCM ex_sensors_cache_next ();
SCM ex_sensors_cache_display ();
SCM ex_sensors_cache_get_total_records ();
SCM ex_sensors_cache_verbose_display ();
SCM ex_sensors_cache_very_verbose_display ();
SCM ex_sensors_cache_get_current_group ();
SCM ex_sensors_get_group_list ();
SCM ex_sdr_get_repo_info ();
SCM ex_kcs_get_poll_count ();
SCM ex_sel_display_first_entry ();
SCM ex_sel_display_next_entry ();
SCM ex_sel_get_first_entry ();
SCM ex_sel_get_next_entry ();
SCM ex_sel_delete_entry (SCM scm_record_id);
SCM ex_sel_clear ();
SCM ex_sel_get_clear_status ();
SCM ex_get_sensors_errno ();


#endif
