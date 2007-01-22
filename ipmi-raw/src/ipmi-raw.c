/*
ipmi-raw.c: executes IPMI commands by hex values.
Copyright (C) 2005 FreeIPMI Core Team

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA
*/

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif	/* HAVE_UNISTD_H */
#ifdef HAVE_ERROR_H
#include <error.h>
#endif
#include <sys/resource.h>
#if TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else /* !TIME_WITH_SYS_TIME */
#if HAVE_SYS_TIME_H
#include <sys/time.h>
#else /* !HAVE_SYS_TIME_H */
#include <time.h>
#endif /* !HAVE_SYS_TIME_H */
#endif /* !TIME_WITH_SYS_TIME */
#include <argp.h>
#include <limits.h>
#include <assert.h>

#include <freeipmi/freeipmi.h>
#include <freeipmi/udm/udm.h>

#include "argp-common.h"
#include "ipmi-raw-argp.h"
#include "ipmi-common.h"
#include "freeipmi-portability.h"

typedef struct ipmi_raw_prog_data
{
  char *progname;
  struct arguments *args;
  uint32_t debug_flags;
} ipmi_raw_prog_data_t;

typedef struct ipmi_raw_state_data
{
  ipmi_raw_prog_data_t *prog_data;
  ipmi_device_t dev;
} ipmi_raw_state_data_t;

int
ipmi_raw_cmdline (ipmi_raw_state_data_t *state_data)
{
  struct arguments *args;
  uint8_t *bytes_rq = NULL;
  int send_len;
  int i;
  uint8_t bytes_rs[ARG_MAX];
  int32_t rs_len;

  assert(state_data);
  assert(state_data->prog_data->args->cmd);
  assert(state_data->prog_data->args->cmd_length);

  args = state_data->prog_data->args;

  bytes_rq = args->cmd;
  send_len = args->cmd_length;

  if (send_len <= 2)
    {
      fprintf(stderr, "Invalid number of hex bytes\n");
      return (-1);
    }

  if ((rs_len = ipmi_cmd_raw (state_data->dev, 
                              bytes_rq[0],
                              bytes_rq[1],
                              &bytes_rq[2],
                              send_len - 2, 
                              bytes_rs, 
                              ARG_MAX)) >= 0)
    {
      printf ("rcvd: ");
      for (i = 0; i < rs_len; i++)
        printf ("%02X ", bytes_rs[i]);
      printf ("\n");
    }
  else 
    {
      perror ("ipmi_cmd_raw()");
      return -1;
    }

  return 0;
}

static int 
string2bytes (char *line, unsigned char **buf, int *len)
{
  const char delim[] = " \t\f\v\r\n";
  char *str = NULL;
  char *token = NULL;
  int count = 0;
  int i = 0;
  int l = 0;
  int value = 0;
  
  if (line == NULL || buf == NULL || len == NULL)
    return (-1);
  
  for (i = 0, count = 0; line[i]; i++)
    {
      if (strchr ((const char*)delim, (int) line[i]))
	count++;
    }
  count++;
  
  *buf = calloc ((strlen (line) - count), 1);
  str = (char *) strdupa (line);
  count = 0;
  while (1)
    {
      token = strsep (&str, delim);
      if (token == NULL)
	break;
      if (strcmp (token, "") == 0)
	continue;
      
      l = strlen (token);
      if (l > 2)
	{
	  fprintf (stderr, "invalid input\n");
	  free (*buf);
	  *buf = NULL;
	  *len = 0;
	  return (-1);
	}
      for (i = 0; i < l; i++)
	{
	  if (isxdigit (token[i]) == 0)
	    {
	      fprintf (stderr, "invalid input\n");
	      free (*buf);
	      *buf = NULL;
	      *len = 0;
	      return (-1);
	    }
	}
      
      value = strtol (token, (char **) NULL, 16);
      (*buf)[count++] = (unsigned char) value;
    }
  
  *len = count;
  
  return (0);
}

int
ipmi_raw_stream (ipmi_raw_state_data_t *state_data, FILE *stream)
{
  struct arguments *args;
  char *line = NULL;
  unsigned int line_count = 0;
  size_t n = 0;
  uint8_t *bytes_rq = NULL;
  int send_len;
  uint8_t bytes_rs[ARG_MAX];
  int32_t rs_len;
  int i, rv = -1;

  assert(state_data);
  assert(stream);

  args = state_data->prog_data->args;

  while (1)
    {
      if (getline (&line, &n, stream) < 0)
	{
	  /* perror ("getline()"); */
	  break;
	}
      line_count++;
      
      if (string2bytes (line, &bytes_rq, &send_len) < 0)
        goto cleanup;
      
      if (send_len <= 2)
        {
          fprintf(stderr, "Invalid number of hex bytes on line %d\n", line_count);
          goto end_loop;
        }

      if ((rs_len = ipmi_cmd_raw (state_data->dev, 
				  bytes_rq[0], 
				  bytes_rq[1], 
				  &bytes_rq[2], 
				  send_len - 2, 
				  bytes_rs, 
				  ARG_MAX)) < 0)
        {
          perror ("ipmi_cmd_raw()");
          goto end_loop;
        }
      
      printf ("rcvd: ");
      for (i = 0; i < rs_len; i++)
	printf ("%02X ", bytes_rs[i]);
      printf ("\n");

    end_loop:
      if (line)
        {
          free(line);
          line = NULL;
        }
      n = 0;
      if (bytes_rq)
        {
          free(bytes_rq);
          bytes_rq = NULL;
        }
      send_len = 0;
    }

  rv = 0;
 cleanup:
  if (line)
    free(line);
  if (bytes_rq)
    free(bytes_rq);
  return rv;
}

int
run_cmd_args (ipmi_raw_state_data_t *state_data)
{
  struct arguments *args;
  FILE *infile = NULL;
  int rv = -1;

  assert(state_data);

  args = state_data->prog_data->args;

  if (args->cmd && args->cmd_length)
    {
      if (ipmi_raw_cmdline(state_data) < 0)
        goto cleanup;
    }

  if (args->cmd_file)
    {
      if (!(infile = fopen (args->cmd_file, "r")))
        {
          perror ("fopen()");
          goto cleanup;
        }
    }
  else 
    {
      if (args->cmd_length == 0)
	infile = stdin;
    }

  if (infile)
    {
      if (ipmi_raw_stream (state_data, infile) < 0)
        goto cleanup;
    }

  rv = 0;
 cleanup:
  if (infile && infile != stdin)
    fclose(infile);
  return (rv);
}

static int
_ipmi_raw (void *arg)
{
  ipmi_raw_state_data_t state_data;
  ipmi_raw_prog_data_t *prog_data;
  ipmi_device_t dev = NULL;
  int exit_code = -1;

  prog_data = (ipmi_raw_prog_data_t *)arg;

  if (prog_data->args->common.host != NULL)
    {
      if (!(dev = ipmi_open_outofband (IPMI_DEVICE_LAN,
                                       prog_data->args->common.host,
                                       prog_data->args->common.username,
                                       prog_data->args->common.password,
                                       prog_data->args->common.authentication_type,
                                       prog_data->args->common.privilege_level,
                                       prog_data->args->common.session_timeout,
                                       prog_data->args->common.retry_timeout,
                                       prog_data->debug_flags)))
        {
          perror ("ipmi_open_outofband()");
          exit_code = EXIT_FAILURE;
          goto cleanup;
        }
    }
  else
    {
      if (!ipmi_is_root())
        {
          fprintf(stderr, "%s: Permission Denied\n", prog_data->progname);
          exit_code = EXIT_FAILURE;
          goto cleanup;
        }

      if (prog_data->args->common.driver_type == IPMI_DEVICE_UNKNOWN)
        {
          if (!(dev = ipmi_open_inband (IPMI_DEVICE_OPENIPMI,
                                        prog_data->args->common.disable_auto_probe,
                                        prog_data->args->common.driver_address,
                                        prog_data->args->common.register_spacing,
                                        prog_data->args->common.driver_device,
                                        prog_data->debug_flags)))
            {
              if (!(dev = ipmi_open_inband (IPMI_DEVICE_KCS,
                                            prog_data->args->common.disable_auto_probe,
                                            prog_data->args->common.driver_address,
                                            prog_data->args->common.register_spacing,
                                            prog_data->args->common.driver_device,
                                            prog_data->debug_flags)))
                {
                  if (!(dev = ipmi_open_inband (IPMI_DEVICE_SSIF,
                                                prog_data->args->common.disable_auto_probe,
                                                prog_data->args->common.driver_address,
                                                prog_data->args->common.register_spacing,
                                                prog_data->args->common.driver_device,
                                                prog_data->debug_flags)))
                    {
                      perror ("ipmi_open_inband()");
                      exit_code = EXIT_FAILURE;
                      goto cleanup;
                    }
                }
            }
        }
      else
        {
          if (!(dev = ipmi_open_inband (prog_data->args->common.driver_type,
                                        prog_data->args->common.disable_auto_probe,
                                        prog_data->args->common.driver_address,
                                        prog_data->args->common.register_spacing,
                                        prog_data->args->common.driver_device,
                                        prog_data->debug_flags)))
            {
              perror ("ipmi_open_inband()");
              exit_code = EXIT_FAILURE;
              goto cleanup;
            }
        }
    }

  memset(&state_data, '\0', sizeof(ipmi_raw_state_data_t));
  state_data.dev = dev;
  state_data.prog_data = prog_data;

  if (run_cmd_args (&state_data) < 0)
    {
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }

  exit_code = 0;
 cleanup:
  if (dev)
    ipmi_close_device (dev);
  return exit_code;
}

static void
_disable_coredump(void)
{
  /* Disable core dumping when not-debugging.  Do not want username,
   * password or other important stuff to core dump.
   */
#ifdef NDEBUG
  struct rlimit resource_limit;

  if (!getrlimit(RLIMIT_CORE, &resource_limit))
    {
      resource_limit.rlim_cur = 0;
      if (setrlimit (RLIMIT_CORE, &resource_limit) != 0)
        perror ("warning: setrlimit()");
    }
#endif /* NDEBUG */
}

int
main (int argc, char **argv)
{
  ipmi_raw_prog_data_t prog_data;
  int exit_code;
#ifdef NDEBUG
  int i;
#endif /* NDEBUG */

  _disable_coredump();

  prog_data.progname = argv[0];
  ipmi_raw_argp_parse (argc, argv);
  prog_data.args = ipmi_raw_get_arguments ();

#ifdef NDEBUG
  /* Clear out argv data for security purposes on ps(1). */
  for (i = 1; i < argc; i++)
    memset(argv[i], '\0', strlen(argv[i]));
#endif /* NDEBUG */

#ifndef NDEBUG
  if (prog_data.args->common.debug)
    prog_data.debug_flags = IPMI_FLAGS_DEBUG_DUMP;
  else
    prog_data.debug_flags = IPMI_FLAGS_DEFAULT;
#else  /* NDEBUG */
  prog_data.debug_flags = IPMI_FLAGS_DEFAULT;
#endif /* NDEBUG */

  exit_code = _ipmi_raw(&prog_data);

  return (exit_code);
}
