/*****************************************************************************\
 *  $Id: ipmidetectd_config.c,v 1.18 2010-06-30 16:36:09 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2012 Lawrence Livermore National Security, LLC.
 *  Copyright (C) 2007 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-228523
 *
 *  This file is part of Ipmidetect, tools and libraries for detecting
 *  IPMI nodes in a cluster. For details, see http://www.llnl.gov/linux/.
 *
 *  Ipmidetect is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 3 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmidetect is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmidetect.  If not, see <http://www.gnu.org/licenses/>.
\*****************************************************************************/

#if HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#if HAVE_GETOPT_H
#include <getopt.h>
#endif /* HAVE_GETOPT_H */
#include <sys/types.h>
#include <sys/stat.h>
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <dirent.h>
#include <assert.h>
#include <errno.h>

#include "ipmidetectd.h"
#include "ipmidetectd_config.h"
#include "debug.h"

#include "freeipmi-portability.h"
#include "conffile.h"
#include "error.h"

struct ipmidetectd_config conf;

static void
_config_default (void)
{
  memset (&conf, '\0', sizeof (struct ipmidetectd_config));

#ifndef NDEBUG
  conf.debug = IPMIDETECTD_DEBUG_DEFAULT;
#endif /* NDEBUG */
  conf.config_file = NULL;
  conf.ipmiping_period = IPMIDETECTD_IPMIPING_PERIOD;
  conf.ipmidetectd_server_port = IPMIDETECTD_SERVER_PORT_DEFAULT;

  if (!(conf.hosts = hostlist_create (NULL)))
    IPMIDETECTD_EXIT (("hostlist_create: %s", strerror (errno)));
}

static void
_usage (void)
{
  fprintf (stderr, "Usage: cerebrod [OPTIONS]\n"
           "-h    --help          Output Help\n"
           "-v    --version       Output Version\n");
#ifndef NDEBUG
  fprintf (stderr,
           "-c    --config_file   Specify alternate config file\n"
           "-d    --debug         Turn on debugging and run daemon in foreground\n");
#endif /* NDEBUG */
  exit (0);
}

static void
_version (void)
{
  fprintf (stderr, "ipmidetectd %s\n", VERSION);
  exit (0);
}

static void
_cmdline_parse (int argc, char **argv)
{
  char options[100];
  int c;

#if HAVE_GETOPT_LONG
  struct option long_options[] =
    {
      { "help",                0, NULL, 'h'},
      { "version",             0, NULL, 'v'},
#ifndef NDEBUG
      { "config-file",         1, NULL, 'c'},
      { "debug",               0, NULL, 'd'},
#endif /* NDEBUG */
      { NULL,                  0, NULL, 0},
    };
#endif /* HAVE_GETOPT_LONG */

  assert (argv);

  memset (options, '\0', sizeof (options));
  strcat (options, "hv");
#ifndef NDEBUG
  strcat (options, "c:d");
#endif /* NDEBUG */

  /* turn off output messages */
  opterr = 0;

#if HAVE_GETOPT_LONG
  while ((c = getopt_long (argc, argv, options, long_options, NULL)) != -1)
#else
    while ((c = getopt (argc, argv, options)) != -1)
#endif
      {
	switch (c)
	  {
	  case 'h':       /* --help */
	    _usage ();
	    break;
	  case 'v':       /* --version */
	    _version ();
	    break;
#ifndef NDEBUG
	  case 'c':       /* --config-file */
	    if (!(conf.config_file = strdup (optarg)))
	      IPMIDETECTD_EXIT (("strdup: %s", strerror (errno)));
	    break;
	  case 'd':       /* --debug */
	    conf.debug++;
	    break;
#endif /* NDEBUG */
	  case '?':
	  default:
	    IPMIDETECTD_EXIT (("unknown command line option '%c'", c));
	  }
      }
}

static int
_cb_host (conffile_t cf,
          struct conffile_data *data,
          char *optionname,
          int option_type,
          void *option_ptr,
          int option_data,
          void *app_ptr,
          int app_data)
{
  if (!hostlist_push (conf.hosts, data->string))
    IPMIDETECTD_EXIT (("hostlist_push: %s", strerror (errno)));
  return (0);
}

static void
_config_file_parse (void)
{
  int ipmiping_period_flag,
    ipmidetectd_server_port_flag,
    host_flag;

  struct conffile_option options[] =
    {
      {
	"ipmiping_period",
	CONFFILE_OPTION_INT,
	-1,
	conffile_int,
	1,
	0,
	&(ipmiping_period_flag),
	&(conf.ipmiping_period),
	0
      },
      {
	"ipmidetectd_server_port",
	CONFFILE_OPTION_INT,
	-1,
	conffile_int,
	1,
	0,
	&(ipmidetectd_server_port_flag),
	&(conf.ipmidetectd_server_port),
	0,
      },
      {
	"host",
	CONFFILE_OPTION_STRING,
	-1,
	_cb_host,
	INT_MAX,
	0,
	&host_flag,
	NULL,
	0
      },
    };
  conffile_t cf = NULL;
  int legacy_file_loaded = 0;
  int num;
  
  if (!(cf = conffile_handle_create ()))
    {
      IPMIDETECTD_DEBUG (("conffile_handle_create"));
      goto cleanup;
    }

  num = sizeof (options)/sizeof (struct conffile_option);

  /* Try legacy file first */
  if (!conf.config_file)
    {
      if (!conffile_parse (cf,
                           IPMIDETECTD_CONFIG_FILE_LEGACY,
                           options,
                           num,
                           NULL,
                           0,
                           0))
        legacy_file_loaded++;
    }

  if (!legacy_file_loaded)
    {
      if (conffile_parse (cf,
                          conf.config_file ? conf.config_file : IPMIDETECTD_CONFIG_FILE_DEFAULT,
                          options,
                          num,
                          NULL,
                          0,
                          0) < 0)
        {
          char buf[CONFFILE_MAX_ERRMSGLEN];
          
          /* Its not an error if the default configuration file doesn't exist */
          if (!strcmp (conf.config_file, IPMIDETECTD_CONFIG_FILE_DEFAULT)
              && conffile_errnum (cf) == CONFFILE_ERR_EXIST)
            goto cleanup;
          
          if (conffile_errmsg (cf, buf, CONFFILE_MAX_ERRMSGLEN) < 0)
            IPMIDETECTD_EXIT (("conffile_parse: %d", conffile_errnum (cf)));
          else
            IPMIDETECTD_EXIT (("conffile_parse: %s", buf));
        }
    }

 cleanup:
  conffile_handle_destroy (cf);
}

static void
_arguments_error_check (void)
{
  /* Nothing to check right now */
}

void
ipmidetectd_config_setup (int argc, char **argv)
{
  assert (argv);

  _config_default ();
  _cmdline_parse (argc, argv);
  _config_file_parse ();
  _arguments_error_check ();

  if (!hostlist_count (conf.hosts))
    IPMIDETECTD_EXIT (("No nodes configured"));
}
