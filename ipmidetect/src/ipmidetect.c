/*****************************************************************************\
 *  $Id: ipmidetect.c,v 1.17 2010-02-08 22:02:30 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2011 Lawrence Livermore National Security, LLC.
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
#include <stdarg.h>
#endif /* STDC_HEADERS */

#if HAVE_GETOPT_H
#include <getopt.h>
#endif /* HAVE_GETOPT_H */
#include <dirent.h>
#if TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else  /* !TIME_WITH_SYS_TIME */
#if HAVE_SYS_TIME_H
#include <sys/time.h>
#else /* !HAVE_SYS_TIME_H */
#include <time.h>
#endif  /* !HAVE_SYS_TIME_H */
#endif /* !TIME_WITH_SYS_TIME */
#include <sys/types.h>
#include <sys/stat.h>
#if HAVE_FCNTL_H
#include <fcntl.h>
#endif /* HAVE_FCNTL_H */
#include <assert.h>
#include <errno.h>

#include "ipmidetect.h"

#include "freeipmi-portability.h"
#include "error.h"
#include "fd.h"
#include "hostlist.h"

/*
 * External variables for getopt
 */
extern char *optarg;
extern int optind, opterr, optopt;

/*
 * Definitions
 */
#define IPMIDETECT_DETECTED_NODES                0
#define IPMIDETECT_UNDETECTED_NODES              1
#define IPMIDETECT_DETECTED_AND_UNDETECTED_NODES 2

#define IPMIDETECT_BUFFERLEN         65536
#define IPMIDETECT_FORMATLEN         64
#define IPMIDETECT_OPTIONS_LEN       64
#define IPMIDETECT_LONG_OPTIONS_LEN  32
#define IPMIDETECT_MAXPATHLEN        256

/*
 * Ipmidetect Data
 *
 * hostname - hostname to connect to
 * port - port to connect o
 * output_type - detected, undetected, or both output
 * output_format - hostlist, comma, newline, or space output
 * handle - ipmidetect handle
 * inputted_nodes - nodes input via the cmdline or stdin
 */
static char *hostname = NULL;
static int port = 0;
static int output_type = IPMIDETECT_DETECTED_AND_UNDETECTED_NODES;
static char output_format = 0;
static ipmidetect_t handle;
static hostlist_t inputted_nodes = NULL;

/*
 * Ipmidetect output data
 */
static char detected_nodes[IPMIDETECT_BUFFERLEN];
static char undetected_nodes[IPMIDETECT_BUFFERLEN];
static char detectedfmt[IPMIDETECT_FORMATLEN];
static char undetectedfmt[IPMIDETECT_FORMATLEN];
static int detected_count = -1;
static int undetected_count = -1;

/*
 * _init_ipmidetect
 *
 * initialize globals
 */
static void
_init_ipmidetect (void)
{
  if (!(handle = ipmidetect_handle_create ()))
    err_exit ("%s: ipmidetect_handle_create", __FUNCTION__);

  if (!(inputted_nodes = hostlist_create (NULL)))
    err_exit ("%s: hostlist_create", __FUNCTION__);
}

/*
 * _cleanup_ipmidetect
 *
 * cleanup globals
 */
static void
_cleanup_ipmidetect (void)
{
  (void)ipmidetect_handle_destroy (handle);
  hostlist_destroy (inputted_nodes);
}

/*
 * _usage
 *
 * output usage and exit
 */
static void
_usage (void)
{
  fprintf (stderr,
           "Usage: ipmidetect [OPTIONS]... [NODES]...\n"
           "  -h         --help              Print help and exit\n"
           "  -v         --version           Print version and exit\n"
           "  -o STRING  --hostname=STRING   Server hostname\n"
           "  -p INT     --port=INT          Server port\n"
           "  -d         --detected          Output only detected nodes\n"
           "  -u         --undetected        Output only undetected nodes\n"
           "  -q         --hostrange         Output in hostrange format\n"
           "  -c         --comma             Output in comma separated list\n"
           "  -n         --newline           Output in newline separated list\n"
           "  -s         --space             Output in space separated list\n");

  fprintf (stderr, "\n");
  exit (1);
}

/*
 * _version
 *
 * output version and exit
 */
static void
_version (void)
{
  fprintf (stderr, "ipmidetect %s\n", VERSION);
  exit (1);
}

/*
 * _push_inputted_nodes
 *
 * push nodes onto a hostlist structure
 */
static void
_push_inputted_nodes (char *nodes)
{
  assert (nodes);

  /* Error if nodes aren't short hostnames */
  if (strchr (nodes, '.'))
    err_exit ("nodes must be listed in short hostname format");

  if (!hostlist_push (inputted_nodes, nodes))
    err_exit ("nodes improperly formatted");
}

/*
 * _read_nodes_from_stdin
 *
 * read nodes off of standard input rather than the command line
 */
static void
_read_nodes_from_stdin (void)
{
  char buf[IPMIDETECT_BUFFERLEN];
  int n;

  if ((n = fd_read_n (STDIN_FILENO, buf, IPMIDETECT_BUFFERLEN)) < 0)
    err_exit ("error reading from standard input: %s", strerror (errno));

  if (n == IPMIDETECT_BUFFERLEN)
    err_exit ("overflow in standard input buffer");

  if (n > 0)
    {
      char *ptr = strtok (buf, " \t\n\0");
      while (ptr)
        {
          _push_inputted_nodes (ptr);
          ptr = strtok (NULL, " \t\n\0");
        }
    }
}

/*
 * _cmdline_parse
 *
 * parse all cmdline input
 */
static void
_cmdline_parse (int argc, char **argv)
{
  int c, index;
  char soptions[IPMIDETECT_OPTIONS_LEN+1];
  char *endptr;

#if HAVE_GETOPT_LONG
  struct option loptions[IPMIDETECT_LONG_OPTIONS_LEN+1] =
    {
      { "help",         0, NULL, 'h'},
      { "version",      0, NULL, 'v'},
      { "hostname",     1, NULL, 'o'},
      { "port",         1, NULL, 'p'},
      { "detected",     0, NULL, 'd'},
      { "undetected",   0, NULL, 'u'},
      { "hostrange",    0, NULL, 'q'},
      { "comma",        0, NULL, 'c'},
      { "newline",      0, NULL, 'n'},
      { "space",        0, NULL, 's'},
      { 0, 0, 0, 0},
    };
#endif /* HAVE_GETOPT_LONG */

  assert (argv);

  memset (soptions, '\0', IPMIDETECT_OPTIONS_LEN+1);
  strncpy (soptions, "hvo:p:duqcns", IPMIDETECT_OPTIONS_LEN);

  /* turn off output messages printed by getopt_long */
  opterr = 0;

#if HAVE_GETOPT_LONG
  while ((c = getopt_long (argc, argv, soptions, loptions, NULL)) != -1)
#else
    while ((c = getopt (argc, argv, options)) != -1)
#endif
      {
    switch(c)
      {
      case 'h':
        _usage ();
      case 'v':
        _version ();
      case 'o':
        hostname = optarg;
        break;
      case 'p':
	errno = 0;
        port = strtol (optarg, &endptr, 10);
	if (errno
	    || endptr[0] != '\0')
          err_exit ("invalid port specified");
        break;
      case 'u':
        output_type = IPMIDETECT_UNDETECTED_NODES;
        break;
      case 'd':
        output_type = IPMIDETECT_DETECTED_NODES;
        break;
      case 'q':
        output_format = 0;
        break;
      case 'c':
        output_format = ',';
        break;
      case 'n':
        output_format = '\n';
        break;
      case 's':
        output_format = ' ';
        break;
      default:
      case '?':
        fprintf (stderr, "command line option error\n");
        _usage ();
      }
      }

  index = optind;

  /* Read nodes in from the command line or standard input */
  if (index < argc)
    {
      if (!strcmp (argv[index], "-"))
        _read_nodes_from_stdin ();
      else
        {
          while (index < argc)
            {
              _push_inputted_nodes (argv[index]);
              index++;
            }
        }

      /* remove any duplicate nodes listed */
      hostlist_uniq (inputted_nodes);
    }
}

/*
 * _get_input_nodes
 *
 * Get the detected or undetected status of nodes specified on the
 * cmdline or standard input.
 */
static void
_get_input_nodes (char *buf, int buflen, int which)
{
  hostlist_t hl = NULL;
  hostlist_iterator_t iter = NULL;
  char *node = NULL;

  assert (buf && buflen);
  assert (which == IPMIDETECT_DETECTED_NODES || which == IPMIDETECT_UNDETECTED_NODES);

  if (!(hl = hostlist_create (NULL)))
    err_exit ("%s: hostlist_create()", __FUNCTION__);

  if (!(iter = hostlist_iterator_create (inputted_nodes)))
    err_exit ("%s: hostlist_iterator_create()", __FUNCTION__);

  while ((node = hostlist_next (iter)))
    {
      int rv;

      if (which == IPMIDETECT_DETECTED_NODES)
        rv = ipmidetect_is_node_detected (handle, node);
      else
        rv = ipmidetect_is_node_undetected (handle, node);

      if (rv < 0)
        {
          if (ipmidetect_errnum (handle) == IPMIDETECT_ERR_NOTFOUND)
            err_exit ("Unknown node \"%s\"", node);
          else
            {
              char *msg = ipmidetect_errormsg (handle);
              if (which == IPMIDETECT_DETECTED_NODES)
                err_exit ("%s: ipmidetect_is_node_detected(): %s", __FUNCTION__, msg);
              else
                err_exit ("%s: ipmidetect_is_node_undetected(): %s", __FUNCTION__, msg);
            }
        }

      if (rv)
        {
          if (!hostlist_push_host (hl, node))
            err_exit ("%s: hostlist_push_host()", __FUNCTION__);
        }

      free (node);
    }

  hostlist_sort (hl);

  if (hostlist_ranged_string (hl, buflen, buf) < 0)
    err_exit ("%s: hostlist_ranged_string()", __FUNCTION__);

  hostlist_iterator_destroy (iter);
  hostlist_destroy (hl);
}

/*
 * _get_all_nodes
 *
 * Get the detected or undetected status of all nodes.
 */
static void
_get_all_nodes (char *buf, int buflen, int which)
{
  int rv;

  assert (buf && buflen > 0);
  assert (which == IPMIDETECT_DETECTED_NODES || which == IPMIDETECT_UNDETECTED_NODES);

  if (which == IPMIDETECT_DETECTED_NODES)
    rv = ipmidetect_get_detected_nodes_string (handle, buf, buflen);
  else
    rv = ipmidetect_get_undetected_nodes_string (handle, buf, buflen);

  if (rv < 0)
    {
      char *msg = ipmidetect_errormsg (handle);
      if (which == IPMIDETECT_DETECTED_NODES)
        err_exit ("%s: ipmidetect_get_detected_nodes_string(): %s", __FUNCTION__, msg);
      else
        err_exit ("%s: ipmidetect_get_undetected_nodes_string(): %s", __FUNCTION__, msg);
    }
}

/*
 * _get_nodes
 *
 * get the detected or undetected status of nodes.  The appropriate function call
 * to _get_input_nodes or _get_all_nodes will be done.
 */
static void
_get_nodes (char *buf, int buflen, int which, int *count)
{
  hostlist_t hl = NULL;

  assert (buf && buflen > 0 && count);
  assert (which == IPMIDETECT_DETECTED_NODES || which == IPMIDETECT_UNDETECTED_NODES);

  if (hostlist_count (inputted_nodes) > 0)
    _get_input_nodes (buf, buflen, which);
  else
    _get_all_nodes (buf, buflen, which);

  if (!(hl = hostlist_create (buf)))
    err_exit ("%s: hostlist_create()", __FUNCTION__);

  *count = hostlist_count (hl);

  hostlist_destroy (hl);
}

/*
 * _output_nodes
 *
 * output the nodes specified in the nodebuf to stdout
 */
static void
_output_nodes (char *nodebuf)
{
  assert (nodebuf);

  if (!output_format)
    fprintf (stdout, "%s\n", nodebuf);
  else
    {
      char tbuf[IPMIDETECT_BUFFERLEN];
      hostlist_t hl = NULL;
      char *ptr;

      /* output nodes separated by some break type */
      memset (tbuf, '\0', IPMIDETECT_BUFFERLEN);

      if (!(hl = hostlist_create (nodebuf)))
        err_exit ("%s: hostlist_create() error", __FUNCTION__);

      if (hostlist_deranged_string (hl, IPMIDETECT_BUFFERLEN, tbuf) < 0)
        err_exit ("%s: hostlist_deranged_string() error", __FUNCTION__);

      /* convert commas to appropriate break types */
      if (output_format != ',')
        {
          while ((ptr = strchr (tbuf, ',')))
            *ptr = (char)output_format;
        }

      /* start on the next line if its a newline separator */
      if (output_type == IPMIDETECT_DETECTED_AND_UNDETECTED_NODES && output_format == '\n')
        fprintf (stdout, "\n");

      fprintf (stdout,"%s\n", tbuf);
      hostlist_destroy (hl);
    }
}

/*
 * _log10
 *
 * portable log10() function that also allows us to avoid linking
 * against the math library.
 */
static int
_log10 (int num)
{
  int count = 0;

  if (num > 0)
    {
      while ((num /= 10) > 0)
        count++;
    }

  return (count);
}

/*
 * _create_formats
 *
 * create the output formats based on the detected and undetected count.
 */
static void
_create_formats (char *endstr)
{
  int max;

  assert (detected_count >= 0 && undetected_count >= 0);

  if (detected_count > undetected_count)
    max = _log10 (detected_count);
  else
    max = _log10 (undetected_count);

  max++;

  snprintf (detectedfmt,   IPMIDETECT_FORMATLEN, "detected:   %%%dd%s", max, endstr);
  snprintf (undetectedfmt, IPMIDETECT_FORMATLEN, "undetected: %%%dd%s", max, endstr);
}

/*
 * _output_data
 *
 * Output the current upundetected state based on inputs from the command line
 *
 * Returns exit_val;
 */
int
_output_data (void)
{
  int exit_val;

  if (ipmidetect_load_data (handle, hostname, port, 0) < 0)
    {
      int errnum = ipmidetect_errnum (handle);
      char *msg = ipmidetect_errormsg (handle);

      /* Check for "legit" errors and output appropriate message */
      if (errnum == IPMIDETECT_ERR_CONF_PARSE)
        err_exit ("Parse error in conf file");
      else if (errnum == IPMIDETECT_ERR_CONNECT)
        err_exit ("Cannot connect to server");
      else if (errnum == IPMIDETECT_ERR_CONNECT_TIMEOUT)
        err_exit ("Timeout connecting to server");
      else if (errnum == IPMIDETECT_ERR_HOSTNAME_INVALID)
        err_exit ("Invalid hostname");
      else
        err_exit ("%s: ipmidetect_load_data(): %s", __FUNCTION__, msg);
    }

  _get_nodes (detected_nodes,
              IPMIDETECT_BUFFERLEN,
              IPMIDETECT_DETECTED_NODES,
              &detected_count);

  _get_nodes (undetected_nodes,
              IPMIDETECT_BUFFERLEN,
              IPMIDETECT_UNDETECTED_NODES,
              &undetected_count);

  /* output up, undetected, or both up and undetected nodes */
  if (output_type == IPMIDETECT_DETECTED_AND_UNDETECTED_NODES)
    {
      if (output_format == '\n')
        {
          /* newline output is funny, thus special */
          snprintf (detectedfmt,
                    IPMIDETECT_FORMATLEN,
                    "detected %d:",
                    detected_count);
          snprintf (undetectedfmt,
                    IPMIDETECT_FORMATLEN,
                    "undetected %d:",
                    undetected_count);
        }
      else
        _create_formats (": ");

      fprintf (stdout, detectedfmt, detected_count);

      _output_nodes (detected_nodes);

      /* handle odd situation with newline output list */
      if (output_format == '\n')
        fprintf (stdout, "\n");

      fprintf (stdout, undetectedfmt, undetected_count);

      _output_nodes (undetected_nodes);
    }
  else if (output_type == IPMIDETECT_DETECTED_NODES)
    _output_nodes (detected_nodes);
  else
    _output_nodes (undetected_nodes);

  if (output_type == IPMIDETECT_DETECTED_AND_UNDETECTED_NODES)
    exit_val = 0;
  else if (output_type == IPMIDETECT_DETECTED_NODES)
    exit_val = (!undetected_count) ? 0 : 1;
  else
    exit_val = (!detected_count) ? 0 : 1;

  return (exit_val);
}

int
main (int argc, char *argv[])
{
  int exit_val;

  err_init (argv[0]);
  err_set_flags (ERROR_STDERR);

  _init_ipmidetect ();

  _cmdline_parse (argc, argv);

  exit_val = _output_data ();

  _cleanup_ipmidetect ();
  exit (exit_val);
}
