/*
 * Copyright (C) 2011 FreeIPMI Core Team
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
#include <assert.h>
#include <errno.h>

#include <freeipmi/freeipmi.h>

#include "ipmi-pet.h"
#include "ipmi-pet-argp.h"

#include "freeipmi-portability.h"
#include "tool-common.h"
#include "tool-cmdline-common.h"
#include "tool-hostrange-common.h"
#include "tool-oem-common.h"
#include "tool-sdr-cache-common.h"
#include "tool-sensor-common.h"

#define IPMI_PET_FMT_BUFLEN    4096

#define IPMI_PLATFORM_EVENT_TRAP_MIN_VARIABLE_BINDINGS_LENGTH 47
#define IPMI_PLATFORM_EVENT_TRAP_MAX_VARIABLE_BINDINGS_LENGTH 110

static int
_ipmi_pet_init (ipmi_pet_state_data_t *state_data)
{
  struct ipmi_pet_arguments *args;
  int rv = -1;
 
  assert (state_data);

  if (!args->sdr.ignore_sdr_cache)
    {
      struct sensor_entity_id_counts *entity_ptr = NULL;

      if (args->entity_sensor_names)
	{
	  if (calculate_entity_id_counts (NULL,
					  state_data->sdr_cache_ctx,
					  state_data->sdr_parse_ctx,
					  &(state_data->entity_id_counts)) < 0)
	    goto cleanup;
	  
	  entity_ptr = &(state_data->entity_id_counts);
	}
      
      if (calculate_column_widths (NULL,
				   state_data->sdr_cache_ctx,
				   state_data->sdr_parse_ctx,
				   NULL,
				   0,
				   NULL,
				   0,
				   state_data->prog_data->args->non_abbreviated_units,
				   (entity_ptr) ? 1 : 0, /* shared_sensors */
				   1, /* count_event_only_records */
				   0, /* count_device_locator_records */
				   0, /* count_oem_records */
				   entity_ptr,
				   &(state_data->column_width)) < 0)
	goto cleanup;
    }
  else
    {
      if (calculate_column_widths_ignored_sdr_cache (state_data->prog_data->args->non_abbreviated_units,
						     &(state_data->column_width)) < 0)
	goto cleanup;
    }

  if (args->interpret_oem_data)
    {
      if (ipmi_get_oem_data (NULL,
                             state_data->ipmi_ctx,
                             &state_data->oem_data) < 0)
        goto cleanup;

      if (ipmi_sel_parse_ctx_set_manufacturer_id (state_data->sel_parse_ctx,
                                                  state_data->oem_data.manufacturer_id) < 0)
        {
          fprintf (stderr,
		   "ipmi_sel_parse_ctx_set_manufacturer_id: %s\n",
		   ipmi_sel_parse_ctx_errormsg (state_data->sel_parse_ctx));
          goto cleanup;
        }
      
      if (ipmi_sel_parse_ctx_set_product_id (state_data->sel_parse_ctx,
                                             state_data->oem_data.product_id) < 0)
        {
          fprintf (stderr,
		   "ipmi_sel_parse_ctx_set_product_id: %s\n",
		   ipmi_sel_parse_ctx_errormsg (state_data->sel_parse_ctx));
          goto cleanup;
        }
      
      if (args->output_event_state)
        {
          if (ipmi_interpret_ctx_set_manufacturer_id (state_data->interpret_ctx,
                                                      state_data->oem_data.manufacturer_id) < 0)
            {
              fprintf (stderr,
		       "ipmi_interpret_ctx_set_manufacturer_id: %s\n",
		       ipmi_interpret_ctx_errormsg (state_data->interpret_ctx));
              goto cleanup;
            }
	  
          if (ipmi_interpret_ctx_set_product_id (state_data->interpret_ctx,
                                                 state_data->oem_data.product_id) < 0)
            {
              fprintf (stderr,
		       "ipmi_interpret_ctx_set_product_id: %s\n",
		       ipmi_interpret_ctx_errormsg (state_data->interpret_ctx));
              goto cleanup;
            }
        }
    }

  rv = 0;
 cleanup:
  return (rv);
}


static int
_ipmi_pet_output_headers (ipmi_pet_state_data_t *state_data)
{
  int rv = -1;

  assert (state_data);

  if (!state_data->prog_data->args->no_header_output
      && !state_data->output_headers)
    {
      if (state_data->prog_data->args->comma_separated_output)
        {
          if (state_data->prog_data->args->no_sensor_type_output)
            printf ("Date,Time,%s",
		    SENSORS_HEADER_NAME_STR);
          else
            printf ("Date,Time,%s,%s",
		    SENSORS_HEADER_NAME_STR,
		    SENSORS_HEADER_TYPE_STR);
	  
          if (state_data->prog_data->args->output_event_state)
            printf (",%s",
		    SENSORS_HEADER_STATE_STR);
	  
          if (state_data->prog_data->args->verbose_count >= 1)
            printf (",Event Direction");
          
          printf (",Event");
          
          printf ("\n");
        }
      else
        {          
	  char fmt[IPMI_PET_FMT_BUFLEN+1];

          memset (fmt, '\0', IPMI_PET_FMT_BUFLEN + 1);

          if (state_data->prog_data->args->no_sensor_type_output)
            {
              snprintf (fmt,
                        IPMI_PET_FMT_BUFLEN,
                        "Date        | Time     | %%-%ds",
                        state_data->column_width.sensor_name);
              
              printf (fmt,
		      SENSORS_HEADER_NAME_STR);
            }
          else
            {
              snprintf (fmt,
                        IPMI_PET_FMT_BUFLEN,
                        "Date        | Time     | %%-%ds | %%-%ds",
                        state_data->column_width.sensor_name,
                        state_data->column_width.sensor_type);
              
              printf (fmt,
		      SENSORS_HEADER_NAME_STR,
		      SENSORS_HEADER_TYPE_STR);
            }
          
          if (state_data->prog_data->args->output_event_state)
            printf (" | %s   ",
		    SENSORS_HEADER_STATE_STR);
	  
          if (state_data->prog_data->args->verbose_count >= 1)
            printf (" | Event Direction  ");
          
          printf (" | Event");
          
          printf ("\n");
        }

      state_data->output_headers++;
    }

  rv = 0;
  return (rv);
}

static int
_ipmi_pet_cmdline (ipmi_pet_state_data_t *state_data)
{
  struct ipmi_pet_arguments *args;
  int rv = -1;

  assert (state_data);
  assert (state_data->prog_data->args->specific_trap_set);
  assert (state_data->prog_data->args->variable_bindings);
  assert (state_data->prog_data->args->variable_bindings_length);
  
  args = state_data->prog_data->args;
  
  if (state_data->prog_data->args->variable_bindings_length >= IPMI_PLATFORM_EVENT_TRAP_MIN_VARIABLE_BINDINGS_LENGTH
      && state_data->prog_data->args->variable_bindings_length <= IPMI_PLATFORM_EVENT_TRAP_MAX_VARIABLE_BINDINGS_LENGTH)
    {
      fprintf (stderr,
	       "Invalid number of variable binding bytes\n");
      goto cleanup;
    }

#if 0
  if (!(bytes_rs = calloc (IPMI_PET_MAX_ARGS, sizeof (uint8_t))))
    {
      perror ("calloc");
      goto cleanup;
    }

  if (state_data->prog_data->args->channel_number
      && state_data->prog_data->args->slave_address)
    {
      if ((rs_len = ipmi_cmd_raw_ipmb (state_data->ipmi_ctx,
                                       state_data->prog_data->args->channel_number_arg,
                                       state_data->prog_data->args->slave_address_arg,
                                       bytes_rq[0],
                                       bytes_rq[1],
                                       &bytes_rq[2],
                                       send_len - 2,
                                       bytes_rs,
                                       IPMI_RAW_MAX_ARGS)) < 0)
        {
          fprintf (stderr,
                           "ipmi_cmd_raw_ipmb: %s\n",
                           ipmi_ctx_errormsg (state_data->ipmi_ctx));
          goto cleanup;
        }
    }
  else
    {
      if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                                  bytes_rq[0],
                                  bytes_rq[1],
                                  &bytes_rq[2],
                                  send_len - 2,
                                  bytes_rs,
                                  IPMI_RAW_MAX_ARGS)) < 0)
        {
          fprintf (stderr,
                           "ipmi_cmd_raw: %s\n",
                           ipmi_ctx_errormsg (state_data->ipmi_ctx));
          goto cleanup;
        }
    }
  
  printf ("rcvd: ");
  for (i = 0; i < rs_len; i++)
    printf ("%02X ", bytes_rs[i]);
  printf ("\n");
#endif
  
  rv = 0;
 cleanup:
  return (rv);
}

static int
_flush_cache (ipmi_pet_state_data_t *state_data)
{
  assert (state_data);
  
  if (sdr_cache_flush_cache (state_data->sdr_cache_ctx,
                             NULL,
                             state_data->prog_data->args->sdr.quiet_cache,
                             state_data->hostname,
                             state_data->prog_data->args->sdr.sdr_cache_directory) < 0)
    return (-1);
  
  return (0);
}

static int
run_cmd_args (ipmi_pet_state_data_t *state_data)
{
  struct ipmi_pet_arguments *args;
  FILE *infile = NULL;
  int rv = -1;

  assert (state_data);
  
  args = state_data->prog_data->args;
  
  if (args->sdr.flush_cache)
    return (_flush_cache (state_data));
  
  if (args->variable_bindings_length)
    {
      if (_ipmi_pet_cmdline (state_data) < 0)
        goto cleanup;

      return (0);
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
    infile = stdin;

#if 0
  if (_ipmi_pet_stream (state_data, infile) < 0)
    goto cleanup;
#endif

  rv = 0;
 cleanup:
  if (infile && infile != stdin)
    fclose (infile);
  return (rv);
}

static int
_ipmi_pet (ipmi_pet_prog_data_t *prog_data)
{
  ipmi_pet_state_data_t state_data;
  char errmsg[IPMI_OPEN_ERRMSGLEN];
  int exit_code = -1;

  memset (&state_data, '\0', sizeof (ipmi_pet_state_data_t));
  state_data.prog_data = prog_data;
  state_data.hostname = prog_data->args->common.hostname;

  /* Special case, just flush, don't do an IPMI connection */
  if (!prog_data->args->sdr.flush_cache)
    {
      if (!(state_data.ipmi_ctx = ipmi_open (prog_data->progname,
                                             prog_data->args->common.hostname,
                                             &(prog_data->args->common),
                                             errmsg,
                                             IPMI_OPEN_ERRMSGLEN)))
        {
          fprintf (stderr,
		   "%s\n",
		   errmsg);
          exit_code = EXIT_FAILURE;
          goto cleanup;
        }
    }

  if (!(state_data.sdr_cache_ctx = ipmi_sdr_cache_ctx_create ()))
    {
      perror ("ipmi_sdr_cache_ctx_create()");
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }

  if (state_data.prog_data->args->common.debug)
    {
      /* Don't error out, if this fails we can still continue */
      if (ipmi_sdr_cache_ctx_set_flags (state_data.sdr_cache_ctx,
                                        IPMI_SDR_CACHE_FLAGS_DEBUG_DUMP) < 0)
        fprintf (stderr,
		 "ipmi_sdr_cache_ctx_set_flags: %s\n",
		 ipmi_sdr_cache_ctx_errormsg (state_data.sdr_cache_ctx));

      if (prog_data->args->common.hostname)
        {
          if (ipmi_sdr_cache_ctx_set_debug_prefix (state_data.sdr_cache_ctx,
                                                   prog_data->args->common.hostname) < 0)
            fprintf (stderr,
		     "ipmi_sdr_cache_ctx_set_debug_prefix: %s\n",
		     ipmi_sdr_cache_ctx_errormsg (state_data.sdr_cache_ctx));
        }
    }

  if (!(state_data.sdr_parse_ctx = ipmi_sdr_parse_ctx_create ()))
    {
      perror ("ipmi_sdr_parse_ctx_create()");
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }

  /* Special case, just flush, don't do SEL stuff */
  if (!prog_data->args->sdr.flush_cache)
    {
      if (!(state_data.sel_parse_ctx = ipmi_sel_parse_ctx_create (state_data.ipmi_ctx,
                                                                  prog_data->args->sdr.ignore_sdr_cache ? NULL : state_data.sdr_cache_ctx)))
        {
          perror ("ipmi_sel_parse_ctx_create()");
          goto cleanup;
        }
      
      if (state_data.prog_data->args->common.debug
	  && prog_data->args->common.hostname)
        {
          if (ipmi_sel_parse_ctx_set_debug_prefix (state_data.sel_parse_ctx,
                                                   prog_data->args->common.hostname) < 0)
            fprintf (stderr,
		     "ipmi_sel_parse_ctx_set_debug_prefix: %s\n",
		     ipmi_sel_parse_ctx_errormsg (state_data.sel_parse_ctx));
        }
    }

  if (prog_data->args->output_event_state)
    {
      unsigned int flags = 0;

      if (!(state_data.interpret_ctx = ipmi_interpret_ctx_create ()))
        {
          perror ("ipmi_interpret_ctx_create()");
          exit_code = EXIT_FAILURE;
          goto cleanup;
        }

      if (prog_data->args->event_state_config_file)
        {
          if (ipmi_interpret_load_sel_config (state_data.interpret_ctx,
                                              prog_data->args->event_state_config_file) < 0)
            {
              if (ipmi_interpret_ctx_errnum (state_data.interpret_ctx) == IPMI_INTERPRET_ERR_SEL_CONFIG_FILE_DOES_NOT_EXIST)
                fprintf (stderr,
			 "event state config file '%s' does not exist\n",
			 prog_data->args->event_state_config_file);
              else if (ipmi_interpret_ctx_errnum (state_data.interpret_ctx) == IPMI_INTERPRET_ERR_SEL_CONFIG_FILE_PARSE)
                fprintf (stderr,
			 "event state config file '%s' parse error\n",
			 prog_data->args->event_state_config_file);
              else
                fprintf (stderr,
			 "ipmi_interpret_load_sel_config: %s\n",
			 ipmi_interpret_ctx_errormsg (state_data.interpret_ctx));
              exit_code = EXIT_FAILURE;
              goto cleanup;
            }
        }
      else
        {
          if (ipmi_interpret_load_sel_config (state_data.interpret_ctx, NULL) < 0)
            {
              if (ipmi_interpret_ctx_errnum (state_data.interpret_ctx) == IPMI_INTERPRET_ERR_SEL_CONFIG_FILE_PARSE)
                fprintf (stderr,
			 "event state config file parse error\n");
              else
                fprintf (stderr,
			 "ipmi_interpret_load_sel_config: %s\n",
			 ipmi_interpret_ctx_errormsg (state_data.interpret_ctx));
              exit_code = EXIT_FAILURE;
              goto cleanup;
            }
        }

      if (prog_data->args->interpret_oem_data)
        flags |= IPMI_INTERPRET_FLAGS_INTERPRET_OEM_DATA;

      if (flags)
        {
          if (ipmi_interpret_ctx_set_flags (state_data.interpret_ctx, flags) < 0)
            {
              fprintf (stderr,
		       "ipmi_interpret_ctx_set_flags: %s\n",
		       ipmi_interpret_ctx_errormsg (state_data.interpret_ctx));
              exit_code = EXIT_FAILURE;
              goto cleanup;
            }
        }
    }
  
  if (run_cmd_args (&state_data) < 0)
    {
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }

  exit_code = 0;
 cleanup:
  if (state_data.sdr_cache_ctx)
    ipmi_sdr_cache_ctx_destroy (state_data.sdr_cache_ctx);
  if (state_data.sdr_parse_ctx)
    ipmi_sdr_parse_ctx_destroy (state_data.sdr_parse_ctx);
  if (state_data.ipmi_ctx)
    {
      ipmi_ctx_close (state_data.ipmi_ctx);
      ipmi_ctx_destroy (state_data.ipmi_ctx);
    }
  return (exit_code);
}

int
main (int argc, char **argv)
{
  ipmi_pet_prog_data_t prog_data;
  struct ipmi_pet_arguments cmd_args;

  ipmi_disable_coredump ();

  memset (&prog_data, '\0', sizeof (ipmi_pet_prog_data_t));
  prog_data.progname = argv[0];
  ipmi_pet_argp_parse (argc, argv, &cmd_args);
  prog_data.args = &cmd_args;

  return (_ipmi_pet (&prog_data));
}

