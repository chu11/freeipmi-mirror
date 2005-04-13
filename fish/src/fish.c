/* 
   $Id: fish.c,v 1.16 2005-04-13 10:00:42 balamurugan Exp $ 

   fish - Free IPMI SHell - an extensible console based shell for managing large number of IPMI compatible systems.

   Copyright (C) 2003-2004 FreeIPMI Core Team

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

#include <termios.h>
#include <grp.h>
#include <pwd.h>
*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "freeipmi.h"
#include <stdio.h>
#include <argp.h>
#include <guile/gh.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/resource.h>
#include <sys/socket.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#ifdef STDC_HEADERS
#include <string.h>
#else
# ifndef HAVE_STRCHR
static char*
strchr (const char* s, int c)
{
  while (*s != '\0')
    if (*s == (char)c) return s;
    else s++;
  return NULL;
}
# endif
#endif

#include "fish.h"
#include "fi-utils.h"
#include "extension.h"
#include "interpreter.h"

#define EXIT_FAILURE 1

#if ENABLE_NLS
# include <libintl.h>
# define _(Text) gettext (Text)
#else
# define textdomain(Domain)
# define _(Text) Text
#endif
#define N_(Text) Text

char *xmalloc ();
char *xrealloc ();
char *xstrdup ();

static error_t parse_opt (int key, char *arg, struct argp_state *state);
static void show_version (FILE *stream, struct argp_state *state);

/* argp option keys */
enum {DUMMY_KEY=129, 
      BRIEF_KEY, POLL_INTERVAL_KEY, SMS_IO_BASE};

/* Option flags and variables.  These are initialized in parse_opt.  */
static int want_quiet;			/* --quiet, --silent */
static int want_brief;			/* --brief */
static int want_verbose = 0;		/* --verbose */
static int cmdline_want_verbose = -1;
static char *script_file = NULL;
static int script_arg_start_index = 0;
static int script_argc = 0;
static char **script_argv = NULL;

static int setup_mode = 0;
static int fi_sockfd = -1;
static unsigned int fi_sock_timeout = 0;
static unsigned int fi_retry_count = 0;
static unsigned long driver_poll_interval = IPMI_KCS_SLEEP_USECS ;
static unsigned long cmdline_driver_poll_interval = 0;

static unsigned int cmdline_sms_io_base = 0;
#ifdef __ia64__
static unsigned int sms_io_base = IPMI_KCS_SMS_IO_BASE_SR870BN4;
#else
static unsigned int sms_io_base = IPMI_KCS_SMS_IO_BASE_DEFAULT;
#endif
static u_int8_t reg_space = IPMI_REG_SPACE_DEFAULT;

static struct argp_option options[] =
{
  { "quiet",       'q',           NULL,            0,
    N_("Inhibit usual output"), 0 },
  { "silent",      0,             NULL,            OPTION_ALIAS,
    NULL, 0 },
  { "brief",       BRIEF_KEY,     NULL,            0,
    N_("Shorten output"), 0 },
  { "verbose",     'v',           NULL,            0,
    N_("Print more information"), 0 },
  { "script-file", 's',           "SCRIPT-FILE",   0,
    N_("Load and execute given SCRIPT-FILE"), 0 },
  { "driver-poll-interval", POLL_INTERVAL_KEY, "USEC", 0, 
    N_("User USEC driver poll interval"), 0 }, 
  { "sms-io-base", SMS_IO_BASE,  "SMS-IO-BASE",    0, 
    N_("SMS IO Base address"), 0}, 
  { NULL, 0, NULL, 0, NULL, 0 }
};

/* The argp functions examine these global variables.  */
const char *argp_program_bug_address = "<ab@gnu.org.in>";
void (*argp_program_version_hook) (FILE *, struct argp_state *) = show_version;

static struct argp argp =
{
  options, parse_opt, N_("[FILE...]"),
  N_("Free IPMI SHell - an extensible console based shell for managing large number of IPMI compatible systems."),
  NULL, NULL, NULL
};

unsigned int
fi_get_retry_count (void)
{
  return (fi_retry_count? fi_retry_count : FI_DEFAULT_RETRY_COUNT);
}

int
fi_get_sockfd (void)
{
  return (fi_sockfd);
}

unsigned int
fi_get_sock_timeout (void)
{
  return (fi_sock_timeout? fi_sock_timeout : FI_DEFAULT_SOCK_TIMEOUT);
}

void
fi_set_sock_timeout (unsigned int sock_timeout)
{
  struct timeval time;
  fi_sock_timeout = sock_timeout;
  
  time.tv_sec  =  fi_get_sock_timeout () / 1000;
  time.tv_usec = (fi_get_sock_timeout () % 1000) * 1000;

  if (setsockopt(fi_sockfd, SOL_SOCKET, SO_RCVTIMEO,
		 &time, sizeof(time)) < 0)
    {
      perror ("setsockopt (SO_RCVTIMEO)");
      exit (EXIT_FAILURE);
    }

  /*
  time.tv_sec  =  fi_get_sock_timeout () / 1000;
  time.tv_usec = (fi_get_sock_timeout () % 1000) * 1000;
  if (setsockopt(fi_sockfd, SOL_SOCKET, SO_SNDTIMEO,
		 &time, sizeof(time)) < 0)
    {
      perror ("setsockopt (SO_RCVTIMEO)");
      exit (EXIT_FAILURE);
    }
  */
}

void
fi_set_verbose (int verbose)
{
  want_verbose = verbose;
}

int
fi_get_verbose (void)
{
  return want_verbose;		/* --verbose */
}

u_int16_t 
fi_get_sms_io_base ()
{
  return sms_io_base;
}

void  
fi_set_sms_io_base (u_int16_t io_base)
{
  sms_io_base = io_base;
}

/* Parse a single option.  */
static error_t
parse_opt (int key, char *arg, struct argp_state *state)
{
  switch (key)
    {
    case ARGP_KEY_INIT:
      /* Set up default values.  */
      want_quiet = 0;
      want_brief = 0;
      want_verbose = 0;
      script_file = NULL;
      break;

    case 'q':			/* --quiet, --silent */
      want_quiet = 1;
      break;
    case BRIEF_KEY:		/* --brief */
      want_brief = 1;
      break;
    case 'v':			/* --verbose */
      cmdline_want_verbose = 1;
      break;
    case POLL_INTERVAL_KEY:     /* --driver-poll-interval */
      cmdline_driver_poll_interval = atol (arg);
      break;
    case SMS_IO_BASE:           /* --sms-io-base */
      cmdline_sms_io_base = atol (arg);
      break;
      
    case 's':                   /* --script-file */
      script_file = arg;
      script_arg_start_index = state->next;
      /* consume rest of args */
      state->next = state->argc;
      break;
      
    case ARGP_KEY_ARG:		/* [FILE]... */
      /* TODO: Do something with ARG, or remove this case and make
         main give argp_parse a non-NULL fifth argument.  */
      break;
      
    default:
      return ARGP_ERR_UNKNOWN;
    }
  return 0;
}

/* Show the version number and copyright information.  */
static void
show_version (FILE *stream, struct argp_state *state)
{
  (void) state;
  /* Print in small parts whose localizations can hopefully be copied
     from other programs.  */
  fputs("FreeIPMI Shell [fish-"PACKAGE_VERSION"]\n", stream);
  //  fprintf(stream, _("Written by %s.\n\n"), "Anand Babu");
  fprintf(stream, _("Copyright (C) %s %s\n"), "2003-2004", "FreeIPMI Core Team");
  fputs(_("\
This program is free software; you may redistribute it under the terms of\n\
the GNU General Public License.  This program has absolutely no warranty.\n"),
	stream);
}

/* char ** */
/* make_fish_argv (int argc, char **argv) */
/* { */
/*   int i; */
/*   char **fargv = NULL; */
  
/*   fargv = (char **) malloc (sizeof (char *) * argc); */
/*   for (i = 0; i < argc; i++) */
/*     fargv[i] = argv[i]; */
  
/*   return fargv; */
/* } */

char **
make_script_argv (int start_arg_index, int len, char **argv)
{
  int i;
  char **sargv = NULL;
  
  sargv = (char **) malloc (sizeof (char *) * len);
  
  for (i = 0; i < len; i++)
    {
      if (i == 0)
	{
	  char *script_name = NULL;
	  script_name = strchr (argv[start_arg_index + i], '=');
	  if (script_name != NULL)
	    {
	      sargv[i] = script_name + 1;
	      continue;
	    }
	}
      sargv[i] = argv[start_arg_index + i];
    }
  
  return sargv;
}

int 
get_script_argc ()
{
  return script_argc;
}

char **
get_script_argv ()
{
  return script_argv;
}

void 
inner_main (int argc, char **argv)
{
  textdomain(PACKAGE);
  argp_parse (&argp, argc, argv, ARGP_IN_ORDER, NULL, NULL);
  
  if (script_arg_start_index != 0)
    {
/*       int i; */
      
      if (script_arg_start_index == argc)
	script_argc = 1;
      else 
	script_argc = argc - script_arg_start_index + 1;
      script_argv = make_script_argv (script_arg_start_index - 1, 
				      script_argc, argv);
      
/*       for (i = 0; i < script_argc; i++) */
/* 	printf ("script_arg%d:[%s]\n", i, script_argv[i]); */
    }
  
/*   printf ("script_file: [%s]\n", script_file); */
  

  guile_env_init ();

  fi_sockfd = ipmi_open_free_udp_port ();
  if (fi_sockfd == -1)
    {
      perror ("ipmi_open_free_udp_port");
      exit (-1);
    }

  fi_set_sock_timeout (FI_DEFAULT_SOCK_TIMEOUT);
  
  fi_load (FI_DEFAULT_CONFIG_FILE);
  
  /* load and evaluate init.scm from well known path */
  fi_load (FI_GLOBAL_INIT_FILE);

  /* itz 2004-05-06 make sure command line overrides files if they conflict */
  if (cmdline_want_verbose != -1)
    want_verbose = cmdline_want_verbose;
  if (cmdline_driver_poll_interval != 0)
    driver_poll_interval = cmdline_driver_poll_interval;

  if (cmdline_sms_io_base != 0)
    sms_io_base = cmdline_sms_io_base;
  else
    {
      ipmi_locate_info_t locate_info;
      
      if (ipmi_locate (IPMI_INTERFACE_KCS, &locate_info) != NULL && (locate_info.addr_space_id == IPMI_ADDRESS_SPACE_ID_SYSTEM_IO))
	{
	  sms_io_base = locate_info.base_addr.bmc_iobase_addr;
	  reg_space   = locate_info.reg_space;
	}
    }
  
  if (ipmi_kcs_io_init (sms_io_base, reg_space, driver_poll_interval) != 0)
    {
      perror ("ipmi_kcs_io_init");
      exit (-1);
    }
  
  if (script_file)
    {
      fi_load (script_file);
      exit (0);
    }
  
  show_version (stdout, NULL);
  read_eval_print_loop (FI_DEFAULT_PROMPT_STRING);
  
  if (fi_sockfd != -1)
    close (fi_sockfd);
  exit (0);
}

int
main (int argc, char **argv)
{
  struct rlimit resource_limit;
  
  /* exits if non-root */
  if (!ipmi_is_root ())
    {
      fprintf (stderr, "error: You must be a root to run this tool\n");
      exit (-1);
    }
  
  /* generate core dump on seg-fault */
  resource_limit.rlim_cur =
    resource_limit.rlim_max = RLIM_INFINITY;
  if (setrlimit (RLIMIT_CORE, &resource_limit) != 0)
    {
      perror ("setrlimit");
      exit (EXIT_FAILURE);
    }
  
  /* command line arguments overrides all default settings, including
     the config file */
  running_for_first_time ();

  gh_enter (argc, argv, inner_main);

  /* control can never reach here. gh_enter calls exit internally */
  exit (EXIT_FAILURE);
 
  /* just to avoid compilation warning */
  return 0;
}

void
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

int
get_setup_mode (void)
{
  return setup_mode;
}

void
set_setup_mode (int setup_mode_value)
{
  setup_mode = setup_mode_value;
}

void
set_driver_poll_interval (int driver_poll_interval_value)
{
  driver_poll_interval = driver_poll_interval_value;
}

