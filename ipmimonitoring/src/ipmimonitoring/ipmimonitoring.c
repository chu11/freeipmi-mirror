/*****************************************************************************\
 *  $Id: ipmimonitoring.c,v 1.1 2007-01-30 21:52:57 chu11 Exp $
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
#include "error.h"
#include "secure.h"

#ifndef MAXHOSTNAMELEN
#define MAXHOSTNAMELEN 64
#endif /* MAXHOSTNAMELEN */

static char *hostname = NULL;
static int hostname_set = 0;
static struct ipmi_monitoring_ipmi_config conf;
static char *username = NULL;
static char *password = NULL;
static int debug_flags = 0;

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
}

static void
_usage(void)
{
  fprintf(stderr, "Usage: ipmimonitoring [OPTIONS]\n"
          "-H --help                    Output Help\n"
          "-V --version                 Output Version\n"
          "-h --hostname str            Hostname\n"
          "-u --username name           Username\n"
          "-p --password pw             Password\n"
          "-P                           Prompt for Password\n"
          "-l --privilege-level str     Privilege Level (user, operator, admin)\n"
          "-a --authentication-type str Authentication Type (none, straight_password, md2, md5)\n");
#ifndef NDEBUG
  fprintf(stderr,
          "-D --debug                   Turn on debugging\n"
          "-E --debugdump               Turn on packet dumps\n");
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
  int c;

#if HAVE_GETOPT_LONG
  struct option long_options[] =
    {
      {"help",                0, NULL, 'H'},
      {"version",             0, NULL, 'V'},
      {"hostname",            1, NULL, 'h'},
      {"username",            1, NULL, 'u'},
      {"password",            1, NULL, 'p'},
      {"password-prompt",     1, NULL, 'P'},
      {"privilege-level",     1, NULL, 'l'},
      {"authentication-type", 1, NULL, 'a'},
#ifndef NDEBUG
      {"debug",               0, NULL, 'D'},
      {"debugdump",           0, NULL, 'E'},
#endif /* NDEBUG */
      {0, 0, 0, 0}
    };
#endif /* HAVE_GETOPT_LONG */

  assert(argv);

  memset(options, '\0', sizeof(options));
  strcat(options, "HVh:u:p:Pl:a:");
#ifndef NDEBUG
  strcat(options, "DE");
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
          break;
        case 'p':       /* --password */
          if (strlen(optarg) > IPMI_1_5_MAX_PASSWORD_LENGTH)
            err_exit("Command Line Error: password too long");
          strcpy(password, optarg);
          conf.password = password;
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
#ifndef NDEBUG
        case 'D':       /* --debug */
          debug_flags |= IPMI_MONITORING_DEBUG_FLAGS_STDERR;
          break;
        case 'E':       /* --debugdump */
          debug_flags |= IPMI_MONITORING_DEBUG_FLAGS_IPMI_PACKETS;
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

int
main(int argc, char **argv)
{
  ipmi_monitoring_ctx_t c = NULL;
  int i, errnum, num, rv = 1;

  err_init(argv[0]);
  err_set_flags(ERROR_STDOUT);

  _config_init();
  _secure_initialization();
  _cmdline_parse(argc, argv);

#ifdef NDEBUG
  /* Clear out argv data for security purposes on ps(1). */
  for (i = 1; i < argc; i++)
    memset(argv[i], '\0', strlen(argv[i]));
#endif /* NDEBUG */

  if (ipmi_monitoring_init(debug_flags, &errnum) < 0)
    {
      fprintf(stderr, "ipmi_monitoring_init: %s\n", ipmi_monitoring_ctx_strerror(errnum));
      goto cleanup;
    }
  
  if (!(c = ipmi_monitoring_ctx_create()))
    {
      fprintf(stderr, "ipmi_monitoring_ctx_create: %s\n", strerror(errno));
      goto cleanup;
    }
  
  if ((num = ipmi_monitoring_sensor_readings_by_record_id(c,
							  (hostname_set) ? hostname : NULL,
                                                          (hostname_set) ? &conf : NULL,
							  IPMI_MONITORING_SENSOR_READING_FLAGS_IGNORE_UNREADABLE_SENSORS,
							  NULL,
							  0)) < 0)
    {
      fprintf(stderr, "ipmi_monitoring_sensor_readings_by_record_id: %s\n", ipmi_monitoring_ctx_strerror(ipmi_monitoring_ctx_errnum(c)));
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
	  fprintf(stderr, "ipmi_monitoring_iterator_record_id: %s\n", ipmi_monitoring_ctx_strerror(ipmi_monitoring_ctx_errnum(c)));
	  goto cleanup;
	}
      if (!(sensor_name = ipmi_monitoring_iterator_sensor_name(c)))
	{
	  fprintf(stderr, "ipmi_monitoring_iterator_sensor_name: %s\n", ipmi_monitoring_ctx_strerror(ipmi_monitoring_ctx_errnum(c)));
	  goto cleanup;
	}
      if ((sensor_state = ipmi_monitoring_iterator_sensor_state(c)) < 0)
	{
	  fprintf(stderr, "ipmi_monitoring_iterator_sensor_state: %s\n", ipmi_monitoring_ctx_strerror(ipmi_monitoring_ctx_errnum(c)));
	  goto cleanup;
	}
      if ((sensor_units = ipmi_monitoring_iterator_sensor_units(c)) < 0)
	{
	  fprintf(stderr, "ipmi_monitoring_iterator_sensor_units: %s\n", ipmi_monitoring_ctx_strerror(ipmi_monitoring_ctx_errnum(c)));
	  goto cleanup;
	}
      if ((sensor_reading_type = ipmi_monitoring_iterator_sensor_reading_type(c)) < 0)
	{
	  fprintf(stderr, "ipmi_monitoring_iterator_sensor_reading_type: %s\n", ipmi_monitoring_ctx_strerror(ipmi_monitoring_ctx_errnum(c)));
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

      printf("%d: %s: [%s]: ", record_id, sensor_name, sensor_state_str);
      
      if (sensor_reading_type == IPMI_MONITORING_SENSOR_READING_TYPE_UNSIGNED_INTEGER8_BOOL)
	printf("%s ", (*((uint8_t *)sensor_reading) ? "true" : "false"));
      else if (sensor_reading_type == IPMI_MONITORING_SENSOR_READING_TYPE_UNSIGNED_INTEGER32)
	printf("%d ", *((uint32_t *)sensor_reading));
      else if (sensor_reading_type == IPMI_MONITORING_SENSOR_READING_TYPE_DOUBLE)
	printf("%f ", *((double *)sensor_reading));
      else if (sensor_reading_type == IPMI_MONITORING_SENSOR_READING_TYPE_UNSIGNED_INTEGER16_BITMASK)
	printf("0x%X ", *((uint16_t *)sensor_reading));

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
      printf("%s", sensor_units_str);
      printf("\n");
    }

  rv = 0;
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
  if (c)
    ipmi_monitoring_ctx_destroy(c);
  exit(rv);
}
