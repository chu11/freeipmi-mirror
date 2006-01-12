/* 
   $Id: fish.c,v 1.17.2.1 2006-01-12 22:02:43 chu11 Exp $ 

   fish - Free IPMI SHell - an extensible console based shell for
   managing large number of IPMI compatible systems.

   Copyright (C) 2003, 2004, 2005 FreeIPMI Core Team

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  
*/

#include "common.h"

static int setup_mode = 0;

void 
set_setup_mode (int setup_mode_value)
{
  setup_mode = setup_mode_value;
}

static void 
running_for_first_time (void)
{
  int retval;
  int error_no;
  
  struct stat config_file_stat;
  int config_file_fd;
  char *config_directory = NULL;
  char *config_filename = NULL;
  char *sdr_cache_dir = NULL;
  
  config_directory = get_config_directory ();
  retval = mkdir (config_directory, FI_CONFIG_DIRECTORY_MODE);
  error_no = errno;
  if (!(retval == 0 || error_no == EEXIST))
    {
      fprintf (stderr, "%s: mkdir(\"%s\"): %s\n", 
	       program_invocation_short_name, config_directory, strerror (error_no));
      exit (EXIT_FAILURE);
    }
  if (retval == 0)
    {
      printf ("\nRunning fish for first time!!\n");
      printf ("[%s] created\n", config_directory);
    }
  free (config_directory);
  
  asprintf (&sdr_cache_dir, 
	    "%s/%s/%s", 
	    get_home_directory (), 
	    FI_CONFIG_DIRECTORY, 
	    FI_SDR_CACHE_DIR);
  retval = mkdir (sdr_cache_dir, FI_CONFIG_DIRECTORY_MODE);
  error_no = errno;
  if (!(retval == 0 || error_no == EEXIST))
    {
      fprintf (stderr, "%s: mkdir(\"%s\"): %s\n", 
	       program_invocation_short_name, sdr_cache_dir, strerror (error_no));
      exit (EXIT_FAILURE);
    }
  if (retval == 0)
    {
      printf ("[%s] created\n", sdr_cache_dir);
    }
  free (sdr_cache_dir);
  
  config_filename = get_config_filename ();
  if (stat (config_filename, &config_file_stat) != 0)
    {
      config_file_fd = open (config_filename, O_WRONLY | O_CREAT, FI_CONFIG_FILE_MODE);
      error_no = errno;
      if (config_file_fd == -1)
	{
	  fprintf (stderr, "%s: open(\"%s\"): %s\n", 
		   program_invocation_short_name, config_filename, strerror (error_no));
	  exit (EXIT_FAILURE);
	}
      close (config_file_fd);
      printf ("[%s] created\n\n", config_filename);
    }
  free (config_filename);
  
  // this is required for delayed, higher level setup procedures
  // that needs to be executed after guile initialization
  // of after sign-in.
  set_setup_mode (1);
  
  return;
}

void 
inner_main (int argc, char **argv)
{
  struct arguments *arguments;
  int retval = 0;
  
  arguments = fi_get_arguments ();
  
  guile_env_init ();
  
  fi_load (FI_DEFAULT_CONFIG_FILE);
  fi_load (FI_GLOBAL_INIT_FILE);
  
/*   fi_ipmi_open (arguments); */
  
  if (arguments->script_file != NULL)
    {
      retval = fi_load (arguments->script_file);
    }
  else 
    {
      fi_show_version (stdout, NULL);
      read_eval_print_loop (FI_DEFAULT_PROMPT_STRING);
    }
  
  exit (fi_ipmi_close (arguments));
}

int 
main (int argc, char **argv)
{
  struct rlimit resource_limit;
  
  textdomain (PACKAGE);
  
  /* generate core dump on seg-fault */
  if (ipmi_is_root ())
    {
      resource_limit.rlim_cur =
	resource_limit.rlim_max = RLIM_INFINITY;
      if (setrlimit (RLIMIT_CORE, &resource_limit) != 0)
	perror ("warning: setrlimit()");
    }
  
  fi_argp_parse (argc, argv);
  
  running_for_first_time ();
  
  gh_enter (argc, argv, inner_main);
  
  /* control never reaches here. gh_enter calls exit internally */
  return (EXIT_FAILURE);
}

