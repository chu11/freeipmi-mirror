/*
 * Copyright (C) 2003-2014 FreeIPMI Core Team
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
#ifdef HAVE_ERROR_H
#include <error.h>
#endif /* HAVE_ERROR_H */
#include <assert.h>
#include <errno.h>

#include <freeipmi/freeipmi.h>

#include "freeipmi-portability.h"
#include "parse-common.h"
#include "pstdout.h"
#include "tool-cmdline-common.h"
#include "tool-util-common.h"

error_t
cmdline_config_file_parse (int key, char *arg, struct argp_state *state)
{
  struct common_cmd_args *common_args;

  assert (state);

  common_args = state->input;

  switch (key)
    {
      /* ARGP_CONFIG_KEY for backwards compatability */
    case ARGP_CONFIG_KEY:
    case ARGP_CONFIG_FILE_KEY:
      free (common_args->config_file);
      if (!(common_args->config_file = strdup (arg)))
        {
          perror ("strdup");
          exit (EXIT_FAILURE);
        }
      break;
    case ARGP_KEY_ARG:
      break;
    case ARGP_KEY_END:
      break;
    default:
      /* don't parse anything else, fall to return (0) */
      break;
    }

  return (0);
}

/* From David Wheeler's Secure Programming Guide */
static void *
__secure_memset (void *s, int c, size_t n)
{
  volatile char *p;

  if (!s || !n)
    return (NULL);

  p = s;
  while (n--)
    *p++=c;

  return (s);
}

error_t
common_parse_opt (int key,
                  char *arg,
                  struct common_cmd_args *common_args)
{
  char *endptr;
  int tmp;
  unsigned int outofband_flags, outofband_2_0_flags, inband_flags, sdr_flags, section_flags;
  int n;

  assert (common_args);

  switch (key)
    {
      /*
       * inband options
       */
    case ARGP_DRIVER_TYPE_KEY:
      if (common_args->driver_type_outofband_only)
        {
          if ((tmp = parse_outofband_driver_type (arg)) < 0)
            {
              fprintf (stderr, "invalid driver type\n");
              exit (EXIT_FAILURE);
            }
        }
      else
        {
          if ((tmp = parse_driver_type (arg)) < 0)
            {
              fprintf (stderr, "invalid driver type\n");
              exit (EXIT_FAILURE);
            }
        }
      common_args->driver_type = tmp;
      break;
      /* ARGP_NO_PROBING_KEY for backwards compatability */
    case ARGP_NO_PROBING_KEY:
    case ARGP_DISABLE_AUTO_PROBE_KEY:
      common_args->disable_auto_probe = 1;
      break;
    case ARGP_DRIVER_ADDRESS_KEY:
      errno = 0;
      tmp = strtol (arg, &endptr, 0);
      if (errno
	  || endptr[0] != '\0'
          || tmp <= 0)
        {
          fprintf (stderr, "invalid driver address\n");
          exit (EXIT_FAILURE);
        }
      common_args->driver_address = tmp;
      break;
    case ARGP_DRIVER_DEVICE_KEY:
      free (common_args->driver_device);
      if (!(common_args->driver_device = strdup (arg)))
        {
          perror ("strdup");
          exit (EXIT_FAILURE);
        }
      break;
    case ARGP_REGISTER_SPACING_KEY:
      errno = 0;
      tmp = strtol (arg, &endptr, 0);
      if (errno
	  || endptr[0] != '\0'
          || tmp <= 0)
        {
          fprintf (stderr, "invalid register spacing\n");
          exit (EXIT_FAILURE);
        }
      common_args->register_spacing = tmp;
      break;
    case ARGP_TARGET_CHANNEL_NUMBER_KEY:
      errno = 0;
      tmp = strtol (arg, &endptr, 0);
      if (errno
	  || endptr[0] != '\0'
	  || tmp < 0
	  || !IPMI_CHANNEL_NUMBER_VALID (tmp))
        {
          fprintf (stderr, "invalid target channel numbern");
          exit (EXIT_FAILURE);
        }
      common_args->target_channel_number = tmp;
      common_args->target_channel_number_is_set = 1;
      break;
    case ARGP_TARGET_SLAVE_ADDRESS_KEY:
      errno = 0;
      tmp = strtol (arg, &endptr, 0);
      if (errno
	  || endptr[0] != '\0'
	  || tmp < 0)
        {
          fprintf (stderr, "invalid target slave addressn");
          exit (EXIT_FAILURE);
        }
      common_args->target_slave_address = tmp;
      common_args->target_slave_address_is_set = 1;
      break;

      /* 
       * outofband options
       */
    case ARGP_HOSTNAME_KEY:
      free (common_args->hostname);
      if (!(common_args->hostname = strdup (arg)))
        {
          perror ("strdup");
          exit (EXIT_FAILURE);
        }
      break;
    case ARGP_USERNAME_KEY:
      if (strlen (arg) > IPMI_MAX_USER_NAME_LENGTH)
        {
          fprintf (stderr, "username too long\n");
          exit (EXIT_FAILURE);
        }
      else
        {
	  free (common_args->username);
          if (!(common_args->username = strdup (arg)))
            {
              perror ("strdup");
              exit (EXIT_FAILURE);
            }
        }
      n = strlen (arg);
      __secure_memset (arg, '\0', n);
      break;
    case ARGP_PASSWORD_KEY:
      if (strlen (arg) > IPMI_2_0_MAX_PASSWORD_LENGTH)
        {
          fprintf (stderr, "password too long\n");
          exit (EXIT_FAILURE);
        }
      else
        {
	  free (common_args->password);
          if (!(common_args->password = strdup (arg)))
            {
              perror ("strdup");
              exit (EXIT_FAILURE);
            }
        }
      n = strlen (arg);
      __secure_memset (arg, '\0', n);
      break;
    case ARGP_PASSWORD_PROMPT_KEY:
      free (common_args->password);
      arg = getpass ("Password: ");
      if (arg && strlen (arg) > IPMI_2_0_MAX_PASSWORD_LENGTH)
        {
          fprintf (stderr, "password too long\n");
          exit (EXIT_FAILURE);
        }
      if (!(common_args->password = strdup (arg)))
        {
          perror ("strdup");
          exit (EXIT_FAILURE);
        }
      break;
    case ARGP_K_G_KEY:
      {
        int rv;
        
        if (common_args->k_g_len)
          {
            memset (common_args->k_g, '\0', IPMI_MAX_K_G_LENGTH + 1);
            common_args->k_g_len = 0;
          }
        
        if ((rv = check_kg_len (arg)) < 0)
          {
            fprintf (stderr, "k_g too long\n");
            exit (EXIT_FAILURE);
          }
        
        if ((rv = parse_kg (common_args->k_g, IPMI_MAX_K_G_LENGTH + 1, arg)) < 0)
          {
            fprintf (stderr, "k_g input formatted incorrectly\n");
            exit (EXIT_FAILURE);
          }
        if (rv > 0)
          common_args->k_g_len = rv;
        n = strlen (arg);
        __secure_memset (arg, '\0', n);
      }
      break;
    case ARGP_K_G_PROMPT_KEY:
      {
        int rv;
        
        if (common_args->k_g_len)
          {
            memset (common_args->k_g, '\0', IPMI_MAX_K_G_LENGTH + 1);
            common_args->k_g_len = 0;
          }
        
        arg = getpass ("K_g: ");
        
        if ((rv = check_kg_len (arg)) < 0)
          {
            fprintf (stderr, "k_g too long\n");
            exit (EXIT_FAILURE);
          }
        
        if ((rv = parse_kg (common_args->k_g, IPMI_MAX_K_G_LENGTH + 1, arg)) < 0)
          {
            fprintf (stderr, "k_g input formatted incorrectly\n");
            exit (EXIT_FAILURE);
          }
        if (rv > 0)
          common_args->k_g_len = rv;
      }
      break;
      /* ARGP_TIMEOUT_KEY for backwards compatability */
    case ARGP_TIMEOUT_KEY:
    case ARGP_SESSION_TIMEOUT_KEY:
      errno = 0;
      tmp = strtol (arg, &endptr, 0);
      if (errno
	  || endptr[0] != '\0'
          || tmp <= 0)
        {
          fprintf (stderr, "invalid session timeout\n");
          exit (EXIT_FAILURE);
        }
      common_args->session_timeout = tmp;
      break;
      /* ARGP_RETRY_TIMEOUT_KEY for backwards compatability */
    case ARGP_RETRY_TIMEOUT_KEY:
    case ARGP_RETRANSMISSION_TIMEOUT_KEY:
      errno = 0;
      tmp = strtol (arg, &endptr, 0);
      if (errno
	  || endptr[0] != '\0'
          || tmp <= 0)
        {
          fprintf (stderr, "invalid retransmission timeout\n");
          exit (EXIT_FAILURE);
        }
      common_args->retransmission_timeout = tmp;
      break;
      /* ARGP_AUTH_TYPE_KEY for backwards compatability */
    case ARGP_AUTH_TYPE_KEY:
    case ARGP_AUTHENTICATION_TYPE_KEY:
      if ((tmp = parse_authentication_type (arg)) < 0)
        {
          fprintf (stderr, "invalid authentication type\n");
          exit (EXIT_FAILURE);
        }
      common_args->authentication_type = tmp;
      break;
    case ARGP_CIPHER_SUITE_ID_KEY:
      errno = 0;
      tmp = strtol (arg, &endptr, 0);
      if (errno
	  || endptr[0] != '\0'
          || tmp < IPMI_CIPHER_SUITE_ID_MIN
          || tmp > IPMI_CIPHER_SUITE_ID_MAX)
        {
          fprintf (stderr, "invalid cipher suite id\n");
          exit (EXIT_FAILURE);
        }
      if (!IPMI_CIPHER_SUITE_ID_SUPPORTED (tmp))
	{
          fprintf (stderr, "unsupported cipher suite id\n");
          exit (EXIT_FAILURE);
	}
      common_args->cipher_suite_id = tmp;
      break;
      /* ARGP_PRIVILEGE_KEY for backwards compatability */
      /* ARGP_PRIV_LEVEL_KEY for backwards compatability */     \
    case ARGP_PRIVILEGE_KEY:
    case ARGP_PRIV_LEVEL_KEY:
    case ARGP_PRIVILEGE_LEVEL_KEY:
      if ((tmp = parse_privilege_level (arg)) < 0)
        {
          fprintf (stderr, "invalid privilege level\n");
          exit (EXIT_FAILURE);
        }
      common_args->privilege_level = tmp;
      break;

      /* 
       * misc options
       */

      /* ARGP_CONFIG_KEY for backwards compatability */
    case ARGP_CONFIG_KEY:
    case ARGP_CONFIG_FILE_KEY:
      /* ignore config option - should have been parsed earlier */
      break;
    case ARGP_WORKAROUND_FLAGS_KEY:
      if (parse_workaround_flags_tool (arg,
				       &outofband_flags,
				       &outofband_2_0_flags,
				       &inband_flags,
				       &sdr_flags,
				       &section_flags) < 0)
        {
          fprintf (stderr, "invalid workaround flags\n");
          exit (EXIT_FAILURE);
        }
      common_args->workaround_flags_outofband = outofband_flags;
      common_args->workaround_flags_outofband_2_0 = outofband_2_0_flags;
      common_args->workaround_flags_inband = inband_flags;
      common_args->workaround_flags_sdr = sdr_flags;
      common_args->section_specific_workaround_flags = section_flags;
      break;
    case ARGP_DEBUG_KEY:
      common_args->debug++;
      break;

      /* 
       * sdr options
       */
    case ARGP_FLUSH_CACHE_KEY:
      common_args->flush_cache = 1;
      break;
    case ARGP_QUIET_CACHE_KEY:
      common_args->quiet_cache = 1;
      break;
    case ARGP_SDR_CACHE_RECREATE_KEY:
      common_args->sdr_cache_recreate = 1;
      break;
    case ARGP_SDR_CACHE_FILE_KEY:
      free (common_args->sdr_cache_file);
      if (!(common_args->sdr_cache_file = strdup (arg)))
        {
          perror ("strdup");
          exit (EXIT_FAILURE);
        }
      break;
    case ARGP_SDR_CACHE_DIRECTORY_KEY:
      free (common_args->sdr_cache_directory);
      if (!(common_args->sdr_cache_directory = strdup (arg)))
        {
          perror ("strdup");
          exit (EXIT_FAILURE);
        }
      break;
    case ARGP_IGNORE_SDR_CACHE_KEY:
      common_args->ignore_sdr_cache = 1;
      break;

      /* 
       * time options
       */
    case ARGP_UTC_TO_LOCALTIME_KEY:
      common_args->utc_to_localtime = 1;
      break;
    case ARGP_LOCALTIME_TO_UTC_KEY:
      common_args->localtime_to_utc = 1;
      break;
    case ARGP_UTC_OFFSET_KEY:
      errno = 0;
      tmp = strtol (arg, &endptr, 0);
      if (errno
	  || endptr[0] != '\0'
	  || !IPMI_UTC_OFFSET_VALID (tmp))
        {
          fprintf (stderr, "invalid UTC offset\n");
          exit (EXIT_FAILURE);
        }
      common_args->utc_offset = tmp;
      break;

      /* 
       * hostrange options
       */
    case ARGP_BUFFER_OUTPUT_KEY:
      common_args->buffer_output = 1;
      break;
    case ARGP_CONSOLIDATE_OUTPUT_KEY:
      common_args->consolidate_output = 1;
      break;
    case ARGP_FANOUT_KEY:
      errno = 0;
      tmp = strtol (arg, &endptr, 10);
      if (errno
	  || endptr[0] != '\0'
          || (tmp < PSTDOUT_FANOUT_MIN)
          || (tmp > PSTDOUT_FANOUT_MAX))
        {
          fprintf (stderr, "invalid fanout\n");
          exit (EXIT_FAILURE);
          break;
        }
      common_args->fanout = tmp;
      break;
    case ARGP_ELIMINATE_KEY:
      common_args->eliminate = 1;
      break;
    case ARGP_ALWAYS_PREFIX_KEY:
      common_args->always_prefix = 1;
      break;
    default:
      return (ARGP_ERR_UNKNOWN);
    }

  return (0);
}

static void
_init_common_cmd_args (struct common_cmd_args *common_args)
{
  assert (common_args);

  common_args->disable_auto_probe = 0;
  common_args->driver_type = IPMI_DEVICE_UNKNOWN;
  common_args->driver_type_outofband_only = 0;
  common_args->driver_address = 0;
  common_args->driver_device = NULL;
  common_args->register_spacing = 0;
  common_args->target_channel_number = 0;
  common_args->target_channel_number_is_set = 0;
  common_args->target_slave_address = 0;
  common_args->target_slave_address_is_set = 0;

  common_args->hostname = NULL;
  common_args->username = NULL;
  common_args->password = NULL;
  memset (common_args->k_g, '\0', IPMI_MAX_K_G_LENGTH + 1);
  common_args->k_g_len = 0;
  common_args->session_timeout = 0;
  common_args->retransmission_timeout = 0;
  common_args->authentication_type = IPMI_AUTHENTICATION_TYPE_MD5;
  common_args->cipher_suite_id = 3;
  /* privilege_level set by parent function */

  common_args->config_file = NULL;
  common_args->workaround_flags_outofband = 0;
  common_args->workaround_flags_outofband_2_0 = 0;
  common_args->workaround_flags_inband = 0;
  common_args->section_specific_workaround_flags = 0;
  common_args->debug = 0;

  common_args->flush_cache = 0;
  common_args->quiet_cache = 0;
  common_args->sdr_cache_recreate = 0;
  common_args->sdr_cache_file = NULL;
  common_args->sdr_cache_directory = NULL;
  common_args->ignore_sdr_cache = 0;

  common_args->utc_to_localtime = 0;
  common_args->localtime_to_utc = 0;
  common_args->utc_offset = 0;

  common_args->buffer_output = 0;
  common_args->consolidate_output = 0;
  common_args->fanout = 0;
  common_args->eliminate = 0;
  common_args->always_prefix = 0;
}

void
init_common_cmd_args_user (struct common_cmd_args *common_args)
{
  assert (common_args);

  _init_common_cmd_args (common_args);
  common_args->privilege_level = IPMI_PRIVILEGE_LEVEL_USER;
}

void
init_common_cmd_args_operator (struct common_cmd_args *common_args)
{
  assert (common_args);

  _init_common_cmd_args (common_args);
  common_args->privilege_level = IPMI_PRIVILEGE_LEVEL_OPERATOR;
}

void
init_common_cmd_args_admin (struct common_cmd_args *common_args)
{
  _init_common_cmd_args (common_args);
  common_args->privilege_level = IPMI_PRIVILEGE_LEVEL_ADMIN;
}

void
verify_common_cmd_args_inband (struct common_cmd_args *common_args)
{
  assert (common_args);

  if (common_args->driver_device)
    {
      if (access (common_args->driver_device, R_OK|W_OK) < 0)
        {
          fprintf (stderr, "insufficient permission on driver device '%s'\n",
                   common_args->driver_device);
          exit (EXIT_FAILURE);
        }
    }
}

void
verify_common_cmd_args_outofband (struct common_cmd_args *common_args, int check_hostname)
{
  assert (common_args);

  if (check_hostname
      && (common_args->driver_type == IPMI_DEVICE_LAN
          || common_args->driver_type == IPMI_DEVICE_LAN_2_0)
      && !common_args->hostname)
    {
      fprintf (stderr, "hostname not specified\n");
      exit (EXIT_FAILURE);
    }

  /* We default to IPMI 1.5 if the user doesn't specify LAN vs. LAN_2_0 */

  if (((common_args->hostname
        && common_args->driver_type == IPMI_DEVICE_UNKNOWN)
       || common_args->driver_type == IPMI_DEVICE_LAN)
      && common_args->password
      && strlen (common_args->password) > IPMI_1_5_MAX_PASSWORD_LENGTH)
    {
      fprintf (stderr, "password too long\n");
      exit (EXIT_FAILURE);
    }
  /* else, 2_0 password length was checked in argp_parse() previously */

  if (common_args->retransmission_timeout > common_args->session_timeout)
    {
      fprintf (stderr, "retransmission timeout larger than session timeout\n");
      exit (EXIT_FAILURE);
    }

  if (common_args->k_g_len)
    {
      unsigned int i;
      int all_zeroes = 1;

      /* Special case, check to make sure user didn't input zero as a
       * k_g key.
       */
      for (i = 0; i < IPMI_MAX_K_G_LENGTH; i++)
        {
          if (common_args->k_g[i] != 0)
            {
              all_zeroes = 0;
              break;
            }
        }
      
      if (all_zeroes)
        common_args->k_g_len = 0;
    }
}

void
verify_common_cmd_args (struct common_cmd_args *common_args)
{
  assert (common_args);

  verify_common_cmd_args_inband (common_args);
  verify_common_cmd_args_outofband (common_args, 1);

  if (common_args->sdr_cache_directory)
    {
      if (access (common_args->sdr_cache_directory, R_OK|W_OK|X_OK) < 0)
        {
          fprintf (stderr, "insufficient permission on SDR cache directory '%s'\n",
                   common_args->sdr_cache_directory);
          exit (EXIT_FAILURE);
        }
    }

  if (common_args->buffer_output && common_args->consolidate_output)
    {
      fprintf (stderr, "cannot buffer and consolidate hostrange output, please select only one\n");
      exit (EXIT_FAILURE);
    }
}

