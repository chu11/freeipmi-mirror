/* 
   extension.c: freehoo extensions to guile
   
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


#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <guile/gh.h>
#include "fish.h"
#include "extension.h"
#include "scm-procedures.h"
#include "bmc-conf-utils.h"

SCM ex_fish_ready_hook;
int hook_return = 0;

int
get_hook_return (void)
{
  return hook_return;
}

void
set_hook_return (int hook_return_value)
{
  hook_return = hook_return_value;
}

SCM
get_fish_ready_hook ()
{
  return (ex_fish_ready_hook);
}

void
install_new_variables (void)
{
  // This is this is the place to install new variables
}

void
install_new_hooks (void)
{
  /* 
     ;; sample eqivalent code in scheme

     (define hook-test (make-hook 2))
     (define hook-proc (lambda (frm message)
     (display frm)
     (newline)
     (display message)
     (newline)))
     (add-hook! hook-test hook-proc)
     (run-hook hook-test "hi" "test")
   */

  /* scm_create_hook creates a hook with name, which is
     interned. This is only really useful from c, since in scheme, you
     can (define name (make-hook n_args))
   */

  ex_fish_ready_hook = scm_create_hook ("fi-fish-ready-hook", 0);
  /* hook: fi-fish-ready-hook
     is called from
     file: fish.c
     function: inner_main (...)
     with no arguments

     sets the hook like this
     (add-hook! fi-fish-ready-hook (lambda () (do-something)))
   */
}

void
install_new_procedures (void)
{
  /* installing general procedures */
  gh_new_procedure ("fi-load", ex_load, 1, 0, 0);
  /* example scheme expression
     (fi-load "discover.scm")
  */

  gh_new_procedure ("fi-register-command!",
		    ex_register_command_x, 1, 0, 0);
  /* example scheme expression
     (fi-register-command  "ping")
  */

  gh_new_procedure ("fi-unregister-command!",
		    ex_unregister_command_x, 1, 0, 0);
  /* example scheme expression
     (fi-unregister-command  "ping")
  */

  gh_new_procedure ("fi-version", ex_version, 0, 0, 0);
  /* example scheme expression
     (display (fi-version))
  */

  gh_new_procedure ("fi-toggle!", ex_toggle_x, 1, 0, 0);
  /* example scheme expression
     (fi-bell-toggle! 'bell)
  */

  gh_new_procedure ("fi-get-sysconfig-dir",
		    ex_get_sysconfig_dir, 0, 0, 0);
  /* example scheme expression
     (display (fi-get-sysconfig-dir))
  */

  gh_new_procedure ("fi-set-prompt!", ex_set_prompt_x, 1, 0, 0);
  /* example scheme expression
     ;; set prompt string
     (fi-set-prompt! "~qp~> ")
  */

  /* installing hook related procedures */
  gh_new_procedure ("fi-hook-return", ex_hook_return, 0, 0, 0);
  /* example scheme expression
     (fi-hook-return)
  */

  gh_new_procedure ("fi-exit", ex_exit, 1, 0, 0);
  /* example scheme expression
     (fi-exit 0)
  */

  gh_new_procedure ("fi-quit", ex_quit, 0, 0, 0);
  /* example scheme expression
     (fi-quit)
  */

  gh_new_procedure ("fi-set-sms-io-base!", ex_set_sms_io_base, 1, 0, 0);
  /* example scheme expression
     (fi-set-sms-io-base! #x0CA2)
  */

  gh_new_procedure ("fi-set-sock-timeout!", ex_set_sock_timeout, 1, 0, 0);
  /* example scheme expression
     (fi-set-sock-timeout! 3000) ;; 3 secs timeout
  */

  gh_new_procedure ("fi-set-default-driver-poll-interval", 
		    ex_set_driver_poll_interval, 1, 0, 0);
  /* example scheme expression
     (fi-set-default-driver-poll-interval 100)
  */

  gh_new_procedure ("fi-get-sock-timeout", ex_get_sock_timeout, 0, 0, 0);
  /* example scheme expression
     (define timeout (fi-set-sock-timeout))
  */

  gh_new_procedure ("fi-ping", ex_ipmi_ping, 1, 0, 0);
  /* example scheme expression
     (fi-ping "ipmi.gnu.org")
  */

  gh_new_procedure ("fi-kcs-get-dev-id-display", ex_kcs_get_dev_id_display, 0, 0, 0);
  /* example scheme expression
     (fi-kcs-get-dev-id-display)
  */
  
  gh_new_procedure ("fi-command-line", ex_get_script_command_line, 0, 0, 0);
  /* example scheme expression
     (display (fi-command-line))
  */
  
  gh_new_procedure ("fi-bmc-config-checkout", ex_bmc_config_checkout, 1, 0, 0);
  /* example scheme expression
     (fi-bmc-config-checkout "bmc-config.dat")
  */
  
  gh_new_procedure ("fi-bmc-config-commit", ex_bmc_config_commit, 1, 0, 0);
  /* example scheme expression
     (fi-bmc-config-commit "bmc-config.dat")
  */
  
  gh_new_procedure ("fi-bmc-config-edit-key-pair", ex_bmc_config_edit_key_pair, 3, 0, 0);
  /* example scheme expression
     (display (fi-bmc-config-edit-key-pair "bmc-config.dat", "gratuitous_arp_interval", "8"))
  */
  
  gh_new_procedure ("fi-bmc-config-check-key", ex_bmc_config_check_key, 1, 0, 0);
  /* example scheme expression
     (display (fi-bmc-config-check-key "gratuitous_arp_interval"))
  */
  
  gh_new_procedure ("fi-bmc-config-diff-key-pair", ex_bmc_config_diff_key_pair, 3, 0, 0);
  /* example scheme expression
     (display (fi-bmc-config-diff-key-pair "bmc-config.dat", "gratuitous_arp_interval", "8"))
  */
  
  gh_new_procedure ("fi-bmc-config-diff-file", ex_bmc_config_diff_file, 2, 0, 0);
  /* example scheme expression
     (display (fi-bmc-config-diff-file "bmc-config.dat", "new-bmc-config.dat"))
  */
  
  gh_new_procedure ("fi-sensors-cache-create", ex_sensors_cache_create, 1, 0, 0);
  /* example scheme expression
     (display (fi-sensors-cache-create "/tmp/sdr-repo.cache"))
  */
  
  gh_new_procedure ("fi-sensors-get-default-cache-filename", 
		    ex_get_default_sdr_repo_cache_filename, 0, 0, 0);
  /* example scheme expression
     (display (fi-sensors-get-default-cache-filename))
  */
  
  gh_new_procedure ("fi-sensors-cache-load", ex_sensors_cache_load, 1, 0, 0);
  /* example scheme expression
     (display (fi-sensors-cache-load "/tmp/sdr-repo.cache"))
  */
  
  gh_new_procedure ("fi-sensors-cache-unload", ex_sensors_cache_unload, 0, 0, 0);
  /* example scheme expression
     (display (fi-sensors-cache-unload))
  */
  
  gh_new_procedure ("fi-sensors-cache-seek", ex_sensors_cache_seek, 1, 0, 0);
  /* example scheme expression
     (display (fi-sensors-cache-seek))
  */
  
  gh_new_procedure ("fi-sensors-cache-first", ex_sensors_cache_first, 0, 0, 0);
  /* example scheme expression
     (display (fi-sensors-cache-first))
  */
  
  gh_new_procedure ("fi-sensors-cache-next", ex_sensors_cache_next, 0, 0, 0);
  /* example scheme expression
     (display (fi-sensors-cache-next))
  */
  
  gh_new_procedure ("fi-sensors-cache-display", ex_sensors_cache_display, 0, 0, 0);
  /* example scheme expression
     (display (fi-sensors-cache-display))
  */
  
  gh_new_procedure ("fi-sensors-cache-get-total-records", ex_sensors_cache_get_total_records, 0, 0, 0);
  /* example scheme expression
     (display (fi-sensors-cache-get-total-records))
  */
  
  gh_new_procedure ("fi-sensors-cache-verbose-display", ex_sensors_cache_verbose_display, 0, 0, 0);
  /* example scheme expression
     (display (fi-sensors-cache-verbose-display))
  */
  
  gh_new_procedure ("fi-sensors-cache-very-verbose-display", ex_sensors_cache_very_verbose_display, 0, 0, 0);
  /* example scheme expression
     (display (fi-sensors-cache-very-verbose-display))
  */
  
  gh_new_procedure ("fi-sensors-cache-get-current-group", ex_sensors_cache_get_current_group, 0, 0, 0);
  /* example scheme expression
     (display (fi-sensors-cache-get-current-group))
  */
  
  gh_new_procedure ("fi-sensors-get-group-list", ex_sensors_get_group_list, 0, 0, 0);
  /* example scheme expression
     (display (fi-sensors-get-group-list))
  */
  
  gh_new_procedure ("fi-sdr-get-repo-info", ex_sdr_get_repo_info, 0, 0, 0);
  /* example scheme expression
     (display (fi-sdr-get-repo-info))
  */
  
  gh_new_procedure ("fi-kcs-get-poll-count", ex_kcs_get_poll_count, 0, 0, 0);
  /* example scheme expression
     (display (fi-kcs-get-poll-count))
  */
  
  gh_new_procedure ("fi-sel-display-first-entry", 
		    ex_sel_display_first_entry, 0, 0, 0);
  /* example scheme expression
     (display (fi-sel-display-first-entry))
  */
  
  gh_new_procedure ("fi-sel-display-next-entry", 
		    ex_sel_display_next_entry, 0, 0, 0);
  /* example scheme expression
     (display (fi-sel-display-next-entry))
  */
  
  gh_new_procedure ("fi-sel-get-first-entry", 
		    ex_sel_get_first_entry, 0, 0, 0);
  /* example scheme expression
     (display (fi-sel-get-first-entry))
  */
  
  gh_new_procedure ("fi-sel-get-next-entry", 
		    ex_sel_get_next_entry, 0, 0, 0);
  /* example scheme expression
     (display (fi-sel-get-next-entry))
  */
  
  gh_new_procedure ("fi-sel-delete-entry", 
		    ex_sel_delete_entry, 1, 0, 0);
  /* example scheme expression
     (display (fi-sel-delete-entry 44))
  */
  
  gh_new_procedure ("fi-sel-clear", 
		    ex_sel_clear, 0, 0, 0);
  /* example scheme expression
     (display (fi-sel-clear))
  */
  
  gh_new_procedure ("fi-sel-get-clear-status", 
		    ex_sel_get_clear_status, 0, 0, 0);
  /* example scheme expression
     (display (fi-sel-get-clear-status))
  */
  
  gh_new_procedure ("fi-get-sensors-errno", 
		    ex_get_sensors_errno, 0, 0, 0);
  /* example scheme expression
     (display (fi-get-sensors-errno))
  */
}

// guile initialization area
void
guile_env_init (void)
{
  install_new_variables ();
  install_new_procedures ();
  install_new_hooks ();
}
