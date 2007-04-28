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
#include <argp.h>
#include <limits.h>
#include <assert.h>

#include <freeipmi/freeipmi.h>
#include <freeipmi/udm/udm.h>

#include "argp-common.h"
#include "ipmi-raw.h"
#include "ipmi-raw-argp.h"
#include "ipmi-common.h"
#include "freeipmi-portability.h"
#include "pstdout.h"
#include "eliminate.h"

int
ipmi_raw_cmdline (ipmi_raw_state_data_t *state_data)
{
  struct ipmi_raw_arguments *args;
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
      pstdout_fprintf(state_data->pstate, 
                      stderr, 
                      "Invalid number of hex bytes\n");
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
      pstdout_printf (state_data->pstate, "rcvd: ");
      for (i = 0; i < rs_len; i++)
        pstdout_printf (state_data->pstate, "%02X ", bytes_rs[i]);
      pstdout_printf (state_data->pstate, "\n");
    }
  else 
    {
      pstdout_perror (state_data->pstate, "ipmi_cmd_raw()");
      return -1;
    }

  return 0;
}

static int 
string2bytes (ipmi_raw_state_data_t *state_data,
              char *line,
              unsigned char **buf, 
              int *len)
{
  const char delim[] = " \t\f\v\r\n";
  char *str = NULL;
  char *ptr = NULL;
  char *token = NULL;
  int count = 0;
  int i = 0;
  int l = 0;
  int value = 0;
  int rv = -1;

  if (state_data == NULL
      || line == NULL 
      || buf == NULL 
      || len == NULL)
    return (-1);
  
  *buf = NULL;
  *len = 0;

  for (i = 0, count = 0; line[i]; i++)
    {
      if (strchr ((const char*)delim, (int) line[i]))
	count++;
    }
  count++;
  
  if (!(*buf = calloc ((strlen (line) - count), 1)))
    {
      pstdout_perror (state_data->pstate, "calloc");
      goto cleanup;
    }

  if (!(str = (char *) strdup (line)))
    {
      pstdout_perror (state_data->pstate, "strdup");
      goto cleanup;
    }
  ptr = str;
  count = 0;
  while (1)
    {
      token = strsep (&ptr, delim);
      if (token == NULL)
	break;
      if (strcmp (token, "") == 0)
	continue;
      
      l = strlen (token);

      if (l >= 2)
	{
          if (strncmp(token, "0x", 2) == 0)
            {
              token+=2;
	      if (*token == '\0')
		{
		  pstdout_fprintf (state_data->pstate, 
				   stderr, 
				   "invalid input\n");
		  goto cleanup;
		}
	      l = strlen (token);
            }
	}

      if (l > 2)
	{
	  pstdout_fprintf (state_data->pstate, 
			   stderr, 
			   "invalid input\n");
	  goto cleanup;
	}

      for (i = 0; i < l; i++)
	{
	  if (isxdigit (token[i]) == 0)
	    {
	      pstdout_fprintf (state_data->pstate, 
                               stderr, 
                               "invalid input\n");
              goto cleanup;
	    }
	}
      
      value = strtol (token, (char **) NULL, 16);
      (*buf)[count++] = (unsigned char) value;
    }
  
  *len = count;
  rv = 0;

 cleanup:
  if (rv < 0)
    {
      if (*buf)
        free(*buf);
      *buf = NULL;
      *len = 0;
    }
  if (str)
    free(str);
  return (rv);
}

int
ipmi_raw_stream (ipmi_raw_state_data_t *state_data, FILE *stream)
{
  struct ipmi_raw_arguments *args;
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
      
      if (string2bytes (state_data, line, &bytes_rq, &send_len) < 0)
        goto cleanup;
      
      if (send_len <= 2)
        {
          pstdout_fprintf(state_data->pstate,
                          stderr, 
                          "Invalid number of hex bytes on line %d\n", 
                          line_count);
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
          pstdout_perror (state_data->pstate, "ipmi_cmd_raw()");
          goto end_loop;
        }
      
      pstdout_printf (state_data->pstate, "rcvd: ");
      for (i = 0; i < rs_len; i++)
	pstdout_printf (state_data->pstate, "%02X ", bytes_rs[i]);
      pstdout_printf (state_data->pstate, "\n");

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
  struct ipmi_raw_arguments *args;
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
          pstdout_perror (state_data->pstate, "fopen()");
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
_ipmi_raw (pstdout_state_t pstate,
           const char *hostname,
           void *arg)
{
  ipmi_raw_state_data_t state_data;
  ipmi_raw_prog_data_t *prog_data;
  ipmi_device_t dev = NULL;
  int exit_code = -1;

  prog_data = (ipmi_raw_prog_data_t *)arg;

  if (hostname && strcmp(hostname, "localhost") != 0)
    {
      if (!(dev = ipmi_open_outofband (IPMI_DEVICE_LAN,
                                       hostname,
                                       prog_data->args->common.username,
                                       prog_data->args->common.password,
                                       prog_data->args->common.authentication_type,
                                       prog_data->args->common.privilege_level,
                                       prog_data->args->common.session_timeout,
                                       prog_data->args->common.retry_timeout,
                                       prog_data->debug_flags)))
        {
          pstdout_perror (pstate, "ipmi_open_outofband()");
          exit_code = EXIT_FAILURE;
          goto cleanup;
        }
    }
  else
    {
      if (!ipmi_is_root())
        {
          pstdout_fprintf(pstate,
                          stderr, 
                          "%s: Permission Denied\n", 
                          prog_data->progname);
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
                      pstdout_perror(pstate, "ipmi_open_inband");
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
              pstdout_perror(pstate, "ipmi_open_inband");
              exit_code = EXIT_FAILURE;
              goto cleanup;
            }
        }
    }

  memset(&state_data, '\0', sizeof(ipmi_raw_state_data_t));
  state_data.dev = dev;
  state_data.prog_data = prog_data;
  state_data.pstate = pstate;

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

int
main (int argc, char **argv)
{
  ipmi_raw_prog_data_t prog_data;
  struct ipmi_raw_arguments cmd_args;
  int exit_code;
  int rv;

  ipmi_disable_coredump();

  prog_data.progname = argv[0];
  ipmi_raw_argp_parse (argc, argv, &cmd_args);
  prog_data.args = &cmd_args;

  if (pstdout_init() < 0)
    {
      fprintf(stderr,
              "pstdout_init: %s\n",
              pstdout_strerror(pstdout_errnum));
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }

#ifndef NDEBUG
  if (prog_data.args->common.debug)
    prog_data.debug_flags = IPMI_FLAGS_DEBUG_DUMP;
  else
    prog_data.debug_flags = IPMI_FLAGS_DEFAULT;
#else  /* NDEBUG */
  prog_data.debug_flags = IPMI_FLAGS_DEFAULT;
#endif /* NDEBUG */

  if (prog_data.args->common.host)
    {
      int count;

      if ((count = pstdout_hostnames_count(prog_data.args->common.host)) < 0)
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

          if (prog_data.args->hostrange.buffer_hostrange_output)
            output_flags = PSTDOUT_OUTPUT_STDOUT_DEFAULT | PSTDOUT_OUTPUT_BUFFER_STDOUT | PSTDOUT_OUTPUT_STDERR_PREPEND_HOSTNAME;
          else if (prog_data.args->hostrange.consolidate_hostrange_output)
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

          if (prog_data.args->hostrange.fanout)
            {
              if (pstdout_set_fanout(prog_data.args->hostrange.fanout) < 0)
                {
                  fprintf(stderr,
                          "pstdout_set_fanout: %s\n",
                          pstdout_strerror(pstdout_errnum));
                  exit_code = EXIT_FAILURE;
                  goto cleanup;
                }
            }

          if (!(prog_data.args->cmd && prog_data.args->cmd_length)
              && !prog_data.args->cmd_file)
            {
              fprintf(stderr,
                      "Input via stdin not available for multiple hosts\n");
              exit_code = EXIT_FAILURE;
              goto cleanup;
            }
        }

      if (prog_data.args->hostrange.eliminate)
        {
          if (eliminate_nodes(&(prog_data.args->common.host)) < 0)
            {
              exit_code = EXIT_FAILURE;
              goto cleanup;
            }
        }
    }

  if ((rv = pstdout_launch(prog_data.args->common.host,
                           _ipmi_raw,
                           &prog_data)) < 0)
    {
      fprintf(stderr,
              "pstdout_launch: %s\n",
              pstdout_strerror(pstdout_errnum));
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }

  exit_code = rv;
 cleanup:
  return (exit_code);
}
