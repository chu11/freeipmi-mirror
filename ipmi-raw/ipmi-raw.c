/*
 * Copyright (C) 2005-2014 FreeIPMI Core Team
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * 
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
#include "tool-util-common.h"

static int
ipmi_raw_cmdline (ipmi_raw_state_data_t *state_data)
{
  struct ipmi_raw_arguments *args;
  uint8_t *bytes_rq = NULL;
  unsigned int send_len;
  uint8_t *bytes_rs = NULL;
  int rs_len;
  int rv = -1;
  int i;

  assert (state_data);
  assert (state_data->prog_data->args->cmd);
  assert (state_data->prog_data->args->cmd_length);

  args = state_data->prog_data->args;

  bytes_rq = args->cmd;
  send_len = args->cmd_length;

  if (send_len <= 2)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "Invalid number of hex bytes\n");
      goto cleanup;
    }

  if (!IPMI_NET_FN_RQ_VALID (bytes_rq[1]))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "Invalid netfn value\n");
      goto cleanup;
    }
  
  if (!(bytes_rs = calloc (IPMI_RAW_MAX_ARGS, sizeof (uint8_t))))
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
			      IPMI_RAW_MAX_ARGS)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "ipmi_cmd_raw: %s\n",
		       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }
  
  pstdout_printf (state_data->pstate, "rcvd: ");
  for (i = 0; i < rs_len; i++)
    pstdout_printf (state_data->pstate, "%02X ", bytes_rs[i]);
  pstdout_printf (state_data->pstate, "\n");
  
  rv = 0;
 cleanup:
  free (bytes_rs);
  return (rv);
}

static int
string2bytes (ipmi_raw_state_data_t *state_data,
              const char *line,
              unsigned char **buf,
              unsigned int *len,
	      unsigned int line_count)
{
  const char delim[] = " \t\f\v\r\n";
  char *str = NULL;
  char *ptr = NULL;
  char *endptr = NULL;
  char *token = NULL;
  int delimbytes = 0;
  int count = 0;
  unsigned int i = 0;
  unsigned int l = 0;
  long value = 0;
  int rv = -1;

  assert (state_data);
  assert (line);
  assert (buf);
  assert (len);
  assert (line_count);

  *buf = NULL;
  *len = 0;

  for (i = 0, delimbytes = 0; line[i]; i++)
    {
      if (strchr ((const char*)delim, line[i]))
        delimbytes++;
    }

  /* Check for empty line */
  if (delimbytes >= strlen (line))
    {
      *len = count;
      return (0);
    }

  if (!(*buf = calloc ((strlen (line) - delimbytes), 1)))
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

  while (1)
    {
      token = strsep (&ptr, delim);
      if (!token)
        break;

      if (!strcmp (token, ""))
        continue;

      l = strlen (token);

      if (l >= 2)
        {
          if (!strncmp (token, "0x", 2))
            {
              token+=2;
              if (*token == '\0')
                {
                  pstdout_fprintf (state_data->pstate,
                                   stderr,
                                   "invalid input line: %u\n",
				   line_count);
                  goto cleanup;
                }
              l = strlen (token);
            }
        }

      if (l > 2)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "invalid input line: %u\n",
			   line_count);
          goto cleanup;
        }

      for (i = 0; i < l; i++)
        {
          if (!isxdigit (token[i]))
            {
              pstdout_fprintf (state_data->pstate,
                               stderr,
                               "invalid input line: %u\n",
			       line_count);
              goto cleanup;
            }
        }

      errno = 0;
      value = strtol (token, &endptr, 16);
      if (errno
	  || endptr[0] != '\0')
	{
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "invalid input line: %u\n",
			   line_count);
	  goto cleanup;
	}
      (*buf)[count++] = (unsigned char) value;
    }

  *len = count;
  rv = 0;

 cleanup:
  if (rv < 0)
    {
      free (*buf);
      *buf = NULL;
      *len = 0;
    }
  free (str);
  return (rv);
}

static int
ipmi_raw_stream (ipmi_raw_state_data_t *state_data, FILE *stream)
{
  struct ipmi_raw_arguments *args;
  char *line = NULL;
  unsigned int line_count = 0;
  size_t n = 0;
  uint8_t *bytes_rq = NULL;
  unsigned int send_len;
  uint8_t *bytes_rs = NULL;
  int rs_len;
  int i;
  int rv = -1;

  assert (state_data);
  assert (stream);

  args = state_data->prog_data->args;

  while (1)
    {
      if (getline (&line, &n, stream) < 0)
        {
          /* perror ("getline()"); */
          break;
        }
      line_count++;
      
      /* On invalid inputs, we exit instead of goto end loop.
       *
       * We could continue and read the next line, but the assumption
       * is that the user is writing a script of some sort to perform
       * a set of tasks.  We do not want to continue the set of tasks
       * if one in the middle is invalid.
       */
      if (string2bytes (state_data, line, &bytes_rq, &send_len, line_count) < 0)
        goto cleanup;

      /* Check for empty line */
      if (!send_len)
	continue;

      if (send_len <= 2)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "Invalid number of hex bytes on line %d\n",
                           line_count);
          goto end_loop;
        }

      if (!IPMI_NET_FN_RQ_VALID (bytes_rq[1]))
	{
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "Invalid netfn value on line %d\n",
			   line_count);
	  goto end_loop;
	}

      if (!(bytes_rs = calloc (IPMI_RAW_MAX_ARGS, sizeof (uint8_t))))
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
				  IPMI_RAW_MAX_ARGS)) < 0)
	{
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "ipmi_cmd_raw: %s\n",
			   ipmi_ctx_errormsg (state_data->ipmi_ctx));
	  goto end_loop;
	}

      pstdout_printf (state_data->pstate, "rcvd: ");
      for (i = 0; i < rs_len; i++)
        pstdout_printf (state_data->pstate, "%02X ", bytes_rs[i]);
      pstdout_printf (state_data->pstate, "\n");

    end_loop:
      free (line);
      line = NULL;
      free (bytes_rq);
      bytes_rq = NULL;
      free (bytes_rs);
      bytes_rs = NULL;
      send_len = 0;
      n = 0;
    }

  rv = 0;
 cleanup:
  free (line);
  free (bytes_rq);
  free (bytes_rs);
  return (rv);
}

int
run_cmd_args (ipmi_raw_state_data_t *state_data)
{
  struct ipmi_raw_arguments *args;
  FILE *infile = NULL;
  int rv = -1;

  assert (state_data);

  args = state_data->prog_data->args;

  if (args->cmd_length)
    {
      if (ipmi_raw_cmdline (state_data) < 0)
        goto cleanup;
      return (0);
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
    fclose (infile);
  return (rv);
}

static int
_ipmi_raw (pstdout_state_t pstate,
           const char *hostname,
           void *arg)
{
  ipmi_raw_state_data_t state_data;
  ipmi_raw_prog_data_t *prog_data;
  int exit_code = EXIT_FAILURE;

  assert (pstate);
  assert (arg);

  prog_data = (ipmi_raw_prog_data_t *)arg;
  memset (&state_data, '\0', sizeof (ipmi_raw_state_data_t));

  state_data.prog_data = prog_data;
  state_data.pstate = pstate;

  if (!(state_data.ipmi_ctx = ipmi_open (prog_data->progname,
                                         hostname,
                                         &(prog_data->args->common_args),
					 state_data.pstate)))
    goto cleanup;

  if (run_cmd_args (&state_data) < 0)
    goto cleanup;

  exit_code = EXIT_SUCCESS;
 cleanup:
  ipmi_ctx_close (state_data.ipmi_ctx);
  ipmi_ctx_destroy (state_data.ipmi_ctx);
  return (exit_code);
}

int
main (int argc, char **argv)
{
  ipmi_raw_prog_data_t prog_data;
  struct ipmi_raw_arguments cmd_args;
  int hosts_count;
  int rv;

  ipmi_disable_coredump ();

  memset (&prog_data, '\0', sizeof (ipmi_raw_prog_data_t));
  prog_data.progname = argv[0];
  ipmi_raw_argp_parse (argc, argv, &cmd_args);
  prog_data.args = &cmd_args;

  if ((hosts_count = pstdout_setup (&(prog_data.args->common_args.hostname),
				    &(prog_data.args->common_args))) < 0)
    return (EXIT_FAILURE);

  if (!hosts_count)
    return (EXIT_SUCCESS);

  if ((rv = pstdout_launch (prog_data.args->common_args.hostname,
                            _ipmi_raw,
                            &prog_data)) < 0)
    {
      fprintf (stderr,
               "pstdout_launch: %s\n",
               pstdout_strerror (pstdout_errnum));
      return (EXIT_FAILURE);
    }

  return (rv);
}
