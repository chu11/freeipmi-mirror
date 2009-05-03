/*
  Copyright (C) 2005-2008 FreeIPMI Core Team

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
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <errno.h>
#include <assert.h>

#include <freeipmi/freeipmi.h>

#include "ipmi-raw.h"
#include "ipmi-raw-argp.h"

#include "freeipmi-portability.h"
#include "pstdout.h"
#include "tool-common.h"
#include "tool-cmdline-common.h"
#include "tool-hostrange-common.h"

int
ipmi_raw_cmdline (ipmi_raw_state_data_t *state_data)
{
  struct ipmi_raw_arguments *args;
  uint8_t *bytes_rq = NULL;
  int send_len;
  int i;
  uint8_t *bytes_rs = NULL;
  int32_t rs_len;
  int rv = -1;

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
      goto cleanup;
    }

  if (!(bytes_rs = calloc(state_data->prog_data->args->arg_max, sizeof(uint8_t))))
    {
      pstdout_perror(state_data->pstate, "calloc");
      goto cleanup;
    }

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx, 
                              bytes_rq[0],
                              bytes_rq[1],
                              &bytes_rq[2],
                              send_len - 2, 
                              bytes_rs, 
                              state_data->prog_data->args->arg_max)) >= 0)
    {
      pstdout_printf (state_data->pstate, "rcvd: ");
      for (i = 0; i < rs_len; i++)
        pstdout_printf (state_data->pstate, "%02X ", bytes_rs[i]);
      pstdout_printf (state_data->pstate, "\n");
    }
  else 
    {
      pstdout_fprintf(state_data->pstate,
                      stderr,
                      "ipmi_cmd_raw: %s\n",
                      ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  if (bytes_rs)
    free(bytes_rs);
  return rv;
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
  uint8_t *bytes_rs = NULL;
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

      if (!(bytes_rs = calloc (state_data->prog_data->args->arg_max, sizeof (uint8_t))))
        {
          pstdout_perror (state_data->pstate, "calloc");
          goto cleanup;
        }

      if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx, 
				  bytes_rq[0], 
				  bytes_rq[1], 
				  &bytes_rq[2], 
				  send_len - 2, 
				  bytes_rs, 
				  state_data->prog_data->args->arg_max)) < 0)
        {
          pstdout_fprintf(state_data->pstate,
                          stderr,
                          "ipmi_cmd_raw: %s\n",
                          ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
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
      if (bytes_rs)
        {
          free (bytes_rs);
          bytes_rs = NULL;
        }
      send_len = 0;
    }

  rv = 0;
 cleanup:
  if (line)
    free(line);
  if (bytes_rq)
    free(bytes_rq);
  if (bytes_rs)
    free(bytes_rs);
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

  if (args->cmd_length)
    {
      if (ipmi_raw_cmdline(state_data) < 0)
        goto cleanup;
      return 0;
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
    infile = stdin;

  if (ipmi_raw_stream (state_data, infile) < 0)
    goto cleanup;
  
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
  char errmsg[IPMI_OPEN_ERRMSGLEN];
  int exit_code = -1;

  prog_data = (ipmi_raw_prog_data_t *)arg;
  memset(&state_data, '\0', sizeof(ipmi_raw_state_data_t));

  state_data.prog_data = prog_data;
  state_data.pstate = pstate;

  if (!(state_data.ipmi_ctx = ipmi_open(prog_data->progname,
                                        hostname,
                                        &(prog_data->args->common),
                                        errmsg,
                                        IPMI_OPEN_ERRMSGLEN)))
    {
      pstdout_fprintf(pstate,
                      stderr,
                      "%s\n",
                      errmsg);
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }

  if (run_cmd_args (&state_data) < 0)
    {
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }

  exit_code = 0;
 cleanup:
  if (state_data.ipmi_ctx)
    {
      ipmi_ctx_close (state_data.ipmi_ctx);
      ipmi_ctx_destroy (state_data.ipmi_ctx);
    }
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

  memset(&prog_data, '\0', sizeof(ipmi_raw_prog_data_t));
  prog_data.progname = argv[0];
  ipmi_raw_argp_parse (argc, argv, &cmd_args);
  prog_data.args = &cmd_args;

  if (pstdout_setup(&(prog_data.args->common.hostname),
                    prog_data.args->hostrange.buffer_output,
                    prog_data.args->hostrange.consolidate_output,
                    prog_data.args->hostrange.fanout,
                    prog_data.args->hostrange.eliminate,
                    prog_data.args->hostrange.always_prefix) < 0)
    {
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }

  if ((rv = pstdout_launch(prog_data.args->common.hostname,
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
