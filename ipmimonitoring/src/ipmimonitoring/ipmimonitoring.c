/*****************************************************************************\
 *  $Id: ipmimonitoring.c,v 1.7 2007-04-27 16:20:57 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2006 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-222073
 *
 *  This file is part of Ipmimonitoring, an IPMI sensor monitoring
 *  library.  For details, see http://www.llnl.gov/linux/.
 *
 *  Ipmimonitoring is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 2 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmimonitoring is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmimonitoring; if not, write to the Free Software Foundation, Inc.,
 *  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA.
\*****************************************************************************/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#if HAVE_GETOPT_H
#include <getopt.h>
#endif /* HAVE_GETOPT_H */
#if TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else  /* !TIME_WITH_SYS_TIME */
# if HAVE_SYS_TIME_H
#  include <sys/time.h>
# else /* !HAVE_SYS_TIME_H */
#  include <time.h>
# endif /* !HAVE_SYS_TIME_H */
#endif /* !TIME_WITH_SYS_TIME */
#include <sys/resource.h>
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <sys/param.h>
#include <assert.h>
#include <errno.h>

#include <freeipmi/freeipmi.h>
#include "ipmi_monitoring.h"
#include "eliminate.h"
#include "error.h"
#include "pstdout.h"
#include "secure.h"

#ifndef MAXHOSTNAMELEN
#define MAXHOSTNAMELEN 64
#endif /* MAXHOSTNAMELEN */

static char *hostname = NULL;
static int hostname_set = 0;
static struct ipmi_monitoring_ipmi_config conf;
static char *username = NULL;
static char *password = NULL;
static int flags = 0;
static char *cache_dir;
static int regenerate_sdr_cache;
static int quiet_readings;
static int buffer_hostrange_output;
static int consolidate_hostrange_output;
static int fanout;
static int eliminate;

static void
_config_init(void)
{
  conf.username = NULL;
  conf.password = NULL;
  conf.privilege_level = -1;
  conf.authentication_type = -1;
  conf.session_timeout_len = -1;
  conf.retransmission_timeout_len = -1;
  conf.retransmission_backoff_count = -1;
  conf.workaround_flags = 0;

  cache_dir = NULL;
  regenerate_sdr_cache = 0;
  quiet_readings = 0;
  buffer_hostrange_output = 0;
  consolidate_hostrange_output = 0;
  fanout = 0;
  eliminate = 0;
}

static void
_usage(void)
{
  fprintf(stderr, "Usage: ipmimonitoring [OPTIONS]\n"
          "-H --help                     Output Help\n"
          "-V --version                  Output Version\n"
          "-h --hostname str             Hostname(s)\n"
          "-u --username name            Username\n"
          "-p --password pw              Password\n"
          "-P --password-prompt          Prompt for Password\n"
          "-l --privilege-level str      Privilege Level (user, operator, admin)\n"
          "-a --authentication-type str  Authentication Type (none, straight_password, md2, md5)\n"
          "-c --cache-dir str            Specify alternate SDR cache directory\n"
          "-r --regenerate-sdr-cache str Regenerate SDR cache\n"
          "-B --buffer-output            Buffer hostranged output\n"
          "-C --consolidate-output       Consolidate hostranged output\n"
          "-F --fanout num               Set multiple host fanout\n"
          "-E --eliminate                Eliminate undetected nodes.\n");
#ifndef NDEBUG
  fprintf(stderr,
          "-D --debug                    Turn on debugging\n"
          "-G --debugdump                Turn on packet dumps\n");
#endif /* NDEBUG */
  exit(0);
}

static void
_version(void)
{
  fprintf(stderr, "ipmimonitoring %s\n", VERSION);
  exit(0);
}

static void
_cmdline_parse(int argc, char **argv)
{
  char options[100];
  char *pw;
  char *ptr;
  int c;

#if HAVE_GETOPT_LONG
  struct option long_options[] =
    {
      {"help",                 0, NULL, 'H'},
      {"version",              0, NULL, 'V'},
      {"hostname",             1, NULL, 'h'},
      {"username",             1, NULL, 'u'},
      {"password",             1, NULL, 'p'},
      {"password-prompt",      1, NULL, 'P'},
      {"privilege-level",      1, NULL, 'l'},
      {"authentication-type",  1, NULL, 'a'},
      {"cache-dir",            1, NULL, 'c'},
      {"regenerate-sdr-cache", 0, NULL, 'r'},
      {"quiet-readings",       0, NULL, 'q'},
      {"buffer-output",        0, NULL, 'B'},
      {"consolidate-output",   0, NULL, 'C'},
      {"fanout",               1, NULL, 'F'},
      {"eliminate",            0, NULL, 'E'},
#ifndef NDEBUG
      {"debug",                0, NULL, 'D'},
      {"debugdump",            0, NULL, 'E'},
#endif /* NDEBUG */
      {0, 0, 0, 0}
    };
#endif /* HAVE_GETOPT_LONG */

  assert(argv);

  memset(options, '\0', sizeof(options));
  strcat(options, "HVh:u:p:Pl:a:c:rqBCF:E");
#ifndef NDEBUG
  strcat(options, "DG");
#endif /* NDEBUG */

  /* turn off output messages */
  opterr = 0;

#if HAVE_GETOPT_LONG
  while ((c = getopt_long(argc, argv, options, long_options, NULL)) != -1)
#else
  while ((c = getopt(argc, argv, options)) != -1)
#endif
    {
      switch (c)
        {
        case 'H':       /* --help */
          _usage();
          break;
        case 'V':
          _version();   /* --version */
          break;
        case 'h':       /* --hostname */
          if (strlen(optarg) > MAXHOSTNAMELEN)
            err_exit("Command Line Error: hostname too long");
          strcpy(hostname, optarg);
          hostname_set++;
          break;
        case 'u':       /* --username */
          if (strlen(optarg) > IPMI_MAX_USER_NAME_LENGTH)
            err_exit("Command Line Error: username too long");
          strcpy(username, optarg);
          conf.username = username;
         if (optarg)
           {
              int n;
              n = strlen(optarg);
              secure_memset(optarg, '\0', n);
            }
          break;
        case 'p':       /* --password */
          if (strlen(optarg) > IPMI_1_5_MAX_PASSWORD_LENGTH)
            err_exit("Command Line Error: password too long");
          strcpy(password, optarg);
          conf.password = password;
          if (optarg)
            {
              int n;
              n = strlen(optarg);
              secure_memset(optarg, '\0', n);
            }
          break;
        case 'P':       /* --password-prompt */
          if (!(pw = getpass("Password: ")))
            err_exit("getpass: %s", strerror(errno));
          if (strlen(pw) > IPMI_1_5_MAX_PASSWORD_LENGTH)
            err_exit("password too long");
          strcpy(password, pw);
          conf.password = password;
          break;
        case 'l':       /* --privilege-level */
          if (!strcasecmp(optarg, "user"))
            conf.privilege_level = IPMI_MONITORING_PRIVILEGE_USER;
          else if (!strcasecmp(optarg, "operator"))
            conf.privilege_level = IPMI_MONITORING_PRIVILEGE_OPERATOR;
          else if (!strcasecmp(optarg, "admin")
                   || !strcasecmp(optarg, "administrator"))
            conf.privilege_level = IPMI_MONITORING_PRIVILEGE_ADMIN;
          else
            err_exit("Command Line Error: Invalid privilege level");
          break;
        case 'a':       /* --authentication-type */
          if (!strcasecmp(optarg, "none"))
            conf.authentication_type = IPMI_MONITORING_AUTHENTICATION_TYPE_NONE;
          else if (!strcasecmp(optarg, "straight_password_key"))
            conf.authentication_type = IPMI_MONITORING_AUTHENTICATION_TYPE_STRAIGHT_PASSWORD_KEY;
          else if (!strcasecmp(optarg, "md2"))
            conf.authentication_type = IPMI_MONITORING_AUTHENTICATION_TYPE_MD2;
          else if (!strcasecmp(optarg, "md5"))
            conf.authentication_type = IPMI_MONITORING_AUTHENTICATION_TYPE_MD5;
          else
            err_exit("Command Line Error: Invalid authentication type");
          break;
        case 'c':
          cache_dir = optarg;
          break;
        case 'r':
          regenerate_sdr_cache++;
          break;
        case 'q':
          quiet_readings++;
          break;
        case 'B':
          buffer_hostrange_output++;
          break;
        case 'C':
          consolidate_hostrange_output++;
          break;
        case 'F':
          fanout = strtol(optarg, &ptr, 10);
          if ((ptr != (optarg + strlen(optarg)))
              || (fanout < PSTDOUT_FANOUT_MIN)
              || (fanout > PSTDOUT_FANOUT_MAX))
            err_exit("Command Line Error: Invalid fanout");
          break;
        case 'E':
          eliminate++;
          break;
#ifndef NDEBUG
        case 'D':       /* --debug */
          flags |= IPMI_MONITORING_FLAGS_DEBUG_STDERR;
          break;
        case 'G':       /* --debugdump */
          flags |= IPMI_MONITORING_FLAGS_DEBUG_IPMI_PACKETS;
          break;
#endif /* NDEBUG */
        case '?':
        default:
          err_exit("unknown command line option '%c'", c);
        }
    }
}
          
static void
_secure_initialization(void)
{
#ifdef NDEBUG
  struct rlimit rlim;

  if (getrlimit(RLIMIT_CORE, &rlim) < 0)
    {
      perror("getrlimit");
      exit(1);
    }

  rlim.rlim_cur = 0;
  if (setrlimit(RLIMIT_CORE, &rlim) < 0)
    {
      perror("setrlimit");
      exit(1);
    }

  if (!(username = (char *)secure_malloc(IPMI_MAX_USER_NAME_LENGTH+1)))
    {
      perror("malloc");
      exit(1);
    }
  
  if (!(password = (char *)secure_malloc(IPMI_1_5_MAX_PASSWORD_LENGTH+1)))
    {
      perror("malloc");
      exit(1);
    }
#else  /* !NDEBUG */
  if (!(username = (char *)malloc(IPMI_MAX_USER_NAME_LENGTH+1)))
    {
      perror("malloc");
      exit(1);
    }
  
  if (!(password = (char *)malloc(IPMI_1_5_MAX_PASSWORD_LENGTH+1)))
    {
      perror("malloc");
      exit(1);
    }
#endif /* !NDEBUG */

  if (!(hostname = (char *)malloc(MAXHOSTNAMELEN + 1)))
    {
      perror("malloc");
      exit(1);
    }
}

static int
_ipmimonitoring(pstdout_state_t pstate,
                const char *hostname,
                void *arg)
{
  ipmi_monitoring_ctx_t c = NULL;
  int i, num;
  int exit_code;
  unsigned int sensor_reading_flags;

  if (!(c = ipmi_monitoring_ctx_create()))
    {
      pstdout_perror(pstate, "ipmi_monitoring_ctx_create:");
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }
  
  sensor_reading_flags = IPMI_MONITORING_SENSOR_READING_FLAGS_IGNORE_UNREADABLE_SENSORS;
  if (regenerate_sdr_cache)
    sensor_reading_flags |= IPMI_MONITORING_SENSOR_READING_FLAGS_REREAD_SDR_CACHE;

  if ((num = ipmi_monitoring_sensor_readings_by_record_id(c,
							  (hostname_set) ? hostname : NULL,
                                                          (hostname_set) ? &conf : NULL,
							  sensor_reading_flags,
							  NULL,
							  0)) < 0)
    {
      pstdout_fprintf(pstate,
                      stderr,
                      "ipmi_monitoring_sensor_readings_by_record_id: %s\n",
                      ipmi_monitoring_ctx_strerror(ipmi_monitoring_ctx_errnum(c)));
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }
    
  for (i = 0; i < num; i++, ipmi_monitoring_iterator_next(c))
    {
      int record_id, sensor_state, sensor_units, sensor_reading_type;
      char *sensor_state_str, *sensor_units_str;
      char *sensor_name;
      void *sensor_reading;

      if ((record_id = ipmi_monitoring_iterator_record_id(c)) < 0)
	{
	  pstdout_fprintf(pstate, 
                          stderr, 
                          "ipmi_monitoring_iterator_record_id: %s\n", 
                          ipmi_monitoring_ctx_strerror(ipmi_monitoring_ctx_errnum(c)));
          exit_code = EXIT_FAILURE;
	  goto cleanup;
	}
      if (!(sensor_name = ipmi_monitoring_iterator_sensor_name(c)))
	{
	  pstdout_fprintf(pstate, 
                          stderr, 
                          "ipmi_monitoring_iterator_sensor_name: %s\n", 
                          ipmi_monitoring_ctx_strerror(ipmi_monitoring_ctx_errnum(c)));
          exit_code = EXIT_FAILURE;
	  goto cleanup;
	}
      if ((sensor_state = ipmi_monitoring_iterator_sensor_state(c)) < 0)
	{
	  pstdout_fprintf(pstate, 
                          stderr, 
                          "ipmi_monitoring_iterator_sensor_state: %s\n", 
                          ipmi_monitoring_ctx_strerror(ipmi_monitoring_ctx_errnum(c)));
          exit_code = EXIT_FAILURE;
	  goto cleanup;
	}
      if ((sensor_units = ipmi_monitoring_iterator_sensor_units(c)) < 0)
	{
	  pstdout_fprintf(pstate, 
                          stderr, 
                          "ipmi_monitoring_iterator_sensor_units: %s\n", 
                          ipmi_monitoring_ctx_strerror(ipmi_monitoring_ctx_errnum(c)));
          exit_code = EXIT_FAILURE;
	  goto cleanup;
	}
      if ((sensor_reading_type = ipmi_monitoring_iterator_sensor_reading_type(c)) < 0)
	{
	  pstdout_fprintf(pstate, 
                          stderr, 
                          "ipmi_monitoring_iterator_sensor_reading_type: %s\n", 
                          ipmi_monitoring_ctx_strerror(ipmi_monitoring_ctx_errnum(c)));
          exit_code = EXIT_FAILURE;
	  goto cleanup;
	}
      sensor_reading = ipmi_monitoring_iterator_sensor_reading(c);

      if (sensor_state == IPMI_MONITORING_SENSOR_STATE_NOMINAL)
	sensor_state_str = "Nominal";
      else if (sensor_state == IPMI_MONITORING_SENSOR_STATE_WARNING)
	sensor_state_str = "Warning";
      else if (sensor_state == IPMI_MONITORING_SENSOR_STATE_CRITICAL)
	sensor_state_str = "Critical";
      else
	sensor_state_str = "";

      pstdout_printf(pstate,
                     "%d: %s: [%s]", 
                     record_id, 
                     sensor_name, 
                     sensor_state_str);
      
      if (!quiet_readings)
        {
          if (sensor_reading_type == IPMI_MONITORING_SENSOR_READING_TYPE_UNSIGNED_INTEGER8_BOOL)
            pstdout_printf(pstate,
                           ": %s ", 
                           (*((uint8_t *)sensor_reading) ? "true" : "false"));
          else if (sensor_reading_type == IPMI_MONITORING_SENSOR_READING_TYPE_UNSIGNED_INTEGER32)
            pstdout_printf(pstate,
                           ": %d ", 
                           *((uint32_t *)sensor_reading));
          else if (sensor_reading_type == IPMI_MONITORING_SENSOR_READING_TYPE_DOUBLE)
            pstdout_printf(pstate,
                           ": %f ", 
                           *((double *)sensor_reading));
          else if (sensor_reading_type == IPMI_MONITORING_SENSOR_READING_TYPE_UNSIGNED_INTEGER16_BITMASK)
            pstdout_printf(pstate,
                           ": 0x%X ", 
                           *((uint16_t *)sensor_reading));
          
          if (sensor_units == IPMI_MONITORING_SENSOR_UNITS_CELSIUS)
            sensor_units_str = "C";
          else if (sensor_units == IPMI_MONITORING_SENSOR_UNITS_FAHRENHEIT)
            sensor_units_str = "F";
          else if (sensor_units == IPMI_MONITORING_SENSOR_UNITS_VOLTS)
            sensor_units_str = "V";
          else if (sensor_units == IPMI_MONITORING_SENSOR_UNITS_AMPS)
            sensor_units_str = "A";
          else if (sensor_units == IPMI_MONITORING_SENSOR_UNITS_RPM)
            sensor_units_str = "RPM";
          else
            sensor_units_str = "";
          pstdout_printf(pstate,
                         "%s", 
                         sensor_units_str);
        }
      pstdout_printf(pstate,
                     "\n");
    }

  exit_code = 0;
 cleanup:
  if (c)
    ipmi_monitoring_ctx_destroy(c);
  return exit_code;
}

int
main(int argc, char **argv)
{
  int exit_code;
  int errnum;
  int rv;

  err_init(argv[0]);
  err_set_flags(ERROR_STDOUT);

  _config_init();
  _secure_initialization();
  _cmdline_parse(argc, argv);

  if (pstdout_init() < 0)
    {
      fprintf(stderr,
              "pstdout_init: %s\n",
              pstdout_strerror(pstdout_errnum));
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }

  if (ipmi_monitoring_init(flags, &errnum) < 0)
    {
      fprintf(stderr, "ipmi_monitoring_init: %s\n", ipmi_monitoring_ctx_strerror(errnum));
      goto cleanup;
    }

  if (cache_dir)
    {
      if (ipmi_monitoring_sdr_cache_directory(cache_dir, &errnum) < 0)
        {
          fprintf(stderr, "ipmi_monitoring_init: %s\n", ipmi_monitoring_ctx_strerror(errnum));
          goto cleanup;
        }
    }
  
  if (hostname)
    {
      int count;

      if ((count = pstdout_hostnames_count(hostname)) < 0)
        {
          fprintf(stderr,
                  "pstdout_hostnames_count: %s\n",
                  pstdout_strerror(pstdout_errnum));
          exit_code = EXIT_FAILURE;
          goto cleanup;
        }

      if (count > 1)
        {
          unsigned int output_flags;

          if (buffer_hostrange_output)
            output_flags = PSTDOUT_OUTPUT_STDOUT_PREPEND_HOSTNAME | PSTDOUT_OUTPUT_BUFFER_STDOUT | PSTDOUT_OUTPUT_STDERR_PREPEND_HOSTNAME;
          else if (consolidate_hostrange_output)
            output_flags = PSTDOUT_OUTPUT_STDOUT_DEFAULT | PSTDOUT_OUTPUT_STDOUT_CONSOLIDATE | PSTDOUT_OUTPUT_STDERR_PREPEND_HOSTNAME;
          else
            output_flags = PSTDOUT_OUTPUT_STDOUT_PREPEND_HOSTNAME | PSTDOUT_OUTPUT_STDERR_PREPEND_HOSTNAME;

          if (pstdout_set_output_flags(output_flags) < 0)
            {
              fprintf(stderr,
                      "pstdout_set_output_flags: %s\n",
                      pstdout_strerror(pstdout_errnum));
              exit_code = EXIT_FAILURE;
              goto cleanup;
            }

          if (fanout)
            {
              if (pstdout_set_fanout(fanout) < 0)
                {
                  fprintf(stderr,
                          "pstdout_set_fanout: %s\n",
                          pstdout_strerror(pstdout_errnum));
                  exit_code = EXIT_FAILURE;
                  goto cleanup;
                }
            }
        }

      if (eliminate)
        {
          if (eliminate_nodes(&hostname) < 0)
            {
              exit_code = EXIT_FAILURE;
              goto cleanup;
            }
        }
    }
  
  if ((rv = pstdout_launch(hostname,
                           _ipmimonitoring,
                           NULL)) < 0)
    {
      fprintf(stderr,
              "pstdout_launch: %s\n",
              pstdout_strerror(pstdout_errnum));
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }
  
  exit_code = rv;
 cleanup:
  if (username)
    {
#ifdef NDEBUG
      secure_free(username, IPMI_MAX_USER_NAME_LENGTH+1);
#else  /* !NDEBUG */
      free(username);
#endif /* !NDEBUG */
    }
  if (password)
    {
#ifdef NDEBUG
      secure_free(password, IPMI_1_5_MAX_PASSWORD_LENGTH+1);
#else  /* !NDEBUG */
      free(password);
#endif /* !NDEBUG */
    }
  if (hostname)
    free(hostname);
  return (exit_code);
}

