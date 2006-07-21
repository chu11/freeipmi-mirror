/* 
   extension.c: fish extensions to guile
   
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

#include "common.h"

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

  ex_fish_ready_hook = scm_permanent_object (scm_make_hook (SCM_INUM0));
  scm_c_define ("fi-fish-ready-hook", ex_fish_ready_hook); 

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

  gh_new_procedure ("fi-ping", ex_ipmi_ping, 2, 0, 0);
  /* example scheme expression
     (fi-ping "ipmi.gnu.org" timeout)
  */

  gh_new_procedure ("fi-command-line", ex_get_script_command_line, 0, 0, 0);
  /* example scheme expression
     (display (fi-command-line))
  */
  
  gh_new_procedure ("fi-sensors-get-group-list", ex_sensors_get_group_list, 0, 0, 0);
  /* example scheme expression
     (display (fi-sensors-get-group-list))
  */
 
  gh_new_procedure ("fi-get-sdr-record", ex_get_sdr_record, 1, 0, 0);
  /* 
     syntax in scheme: (fi-get-sdr-record)
  */
  
  gh_new_procedure ("fi-get-sensor-reading", ex_get_sensor_reading, 1, 0, 0);
  /* 
     syntax in scheme: (fi-get-sensor-reading)
  */
  
  gh_new_procedure ("fi-get-sdr-cache-filename", ex_get_sdr_cache_filename, 0, 0, 0);
  /* 
     syntax in scheme: (fi-get-sdr-cache-filename)
  */
  
  gh_new_procedure ("fi-get-sdr-repository-info", ex_get_sdr_repository_info, 0, 0, 0);
  /* 
     syntax in scheme: (fi-get-sdr-repository-info)
  */
  
  gh_new_procedure ("fi-ipmi-open", ex_ipmi_open, 1, 0, 0);
  gh_new_procedure ("fi-ipmi-close", ex_ipmi_close, 0, 0, 0);
  gh_new_procedure ("fi-get-pef-info", ex_get_pef_info, 0, 0, 0);
  
  gh_new_procedure ("fi-string->number", ex_string2number, 1, 0, 0);
}

// guile initialization area
void
guile_env_init (void)
{
  install_new_variables ();
  install_new_procedures ();
  install_new_hooks ();
}
