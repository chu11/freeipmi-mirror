/*
   Copyright (C) 2003-2008 FreeIPMI Core Team

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.
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

#include "freeipmi/interface/ipmi-rmcpplus-interface.h"
#include "freeipmi/spec/ipmi-authentication-type-spec.h"
#include "freeipmi/spec/ipmi-privilege-level-spec.h"
#include "freeipmi/util/ipmi-cipher-suite-util.h"

#include "freeipmi-portability.h"
#include "pstdout.h"
#include "tool-common.h"
#include "tool-cmdline-common.h"

#define WORKAROUND_FLAG_BUFLEN 1024

error_t 
cmdline_config_file_parse (int key, char *arg, struct argp_state *state)
{
  struct common_cmd_args *cmd_args = state->input;

  switch (key)
    {
      /* ARGP_CONFIG_KEY for backwards compatability */
    case ARGP_CONFIG_KEY:
    case ARGP_CONFIG_FILE_KEY:
      if (cmd_args->config_file)
	free (cmd_args->config_file);
      if (!(cmd_args->config_file = strdup(arg)))
        {
          perror("strdup");
          exit(1);
        }
      break;
    case ARGP_KEY_ARG:
      break;
    case ARGP_KEY_END:
      break;
    default:
      /* don't parse anything else, fall to return 0 */
      break;
    }
  
  return 0;
}

int 
parse_inband_driver_type(char *str)
{
  if (!str)
    return -1;

  if (strcasecmp (str, "kcs") == 0)
    return IPMI_DEVICE_KCS;
  else if (strcasecmp (str, "ssif") == 0)
    return IPMI_DEVICE_SSIF;
  /* support "open" for those that might be used to 
   * ipmitool.
   */
  else if (strcasecmp (str, "open") == 0
           || strcasecmp (str, "openipmi") == 0)
    return IPMI_DEVICE_OPENIPMI;
  /* support "bmc" for those that might be used to
   * ipmitool.
   */
  else if (strcasecmp (str, "bmc") == 0
           || strcasecmp (str, "sunbmc") == 0)
    return IPMI_DEVICE_SUNBMC;
  
  return -1;
}

int 
parse_outofband_driver_type(char *str)
{
  if (!str)
    return -1;

  if (strcasecmp (str, "lan") == 0)
    return IPMI_DEVICE_LAN;
  /* support "lanplus" for those that might be used to ipmitool.
   * support typo variants to ease. 
   */
  else if (strcasecmp (str, "lanplus") == 0
           || strcasecmp (str, "lan_2_0") == 0
           || strcasecmp (str, "lan20") == 0
           || strcasecmp (str, "lan_20") == 0
           || strcasecmp (str, "lan2_0") == 0
           || strcasecmp (str, "lan2_0") == 0)
    return IPMI_DEVICE_LAN_2_0;
  
  return -1;
}

int
parse_driver_type(char *str)
{
  int ret;

  if (!str)
    return -1;

  if ((ret = parse_inband_driver_type(str)) < 0)
    ret = parse_outofband_driver_type(str);

  return ret;
}

int
parse_authentication_type(char *str)
{
  if (!str)
    return -1;

  if (strcasecmp (str, IPMI_AUTHENTICATION_TYPE_NONE_STR) == 0)
    return IPMI_AUTHENTICATION_TYPE_NONE;
  /* keep "plain" for backwards compatability */
  else if (strcasecmp (str, IPMI_AUTHENTICATION_TYPE_STRAIGHT_PASSWORD_KEY_STR_OLD) == 0
           || strcasecmp (str, IPMI_AUTHENTICATION_TYPE_STRAIGHT_PASSWORD_KEY_STR) == 0)
    return IPMI_AUTHENTICATION_TYPE_STRAIGHT_PASSWORD_KEY;
  else if (strcasecmp (str, IPMI_AUTHENTICATION_TYPE_MD2_STR) == 0)
    return IPMI_AUTHENTICATION_TYPE_MD2;
  else if (strcasecmp (str, IPMI_AUTHENTICATION_TYPE_MD5_STR) == 0)
    return IPMI_AUTHENTICATION_TYPE_MD5;

  return -1;
}
int
parse_privilege_level(char *str)
{
  if (!str)
    return -1;

  if (strcasecmp (str, IPMI_PRIVILEGE_LEVEL_USER_STR) == 0)
    return IPMI_PRIVILEGE_LEVEL_USER;
  else if (strcasecmp (str, IPMI_PRIVILEGE_LEVEL_OPERATOR_STR) == 0)
    return IPMI_PRIVILEGE_LEVEL_OPERATOR;
  else if (strcasecmp (str, IPMI_PRIVILEGE_LEVEL_ADMIN_STR) == 0
           || strcasecmp (str, IPMI_PRIVILEGE_LEVEL_ADMIN_STR2) == 0)
    return IPMI_PRIVILEGE_LEVEL_ADMIN;
  
  return -1;
}

int
parse_workaround_flags(char *str)
{
  char buf[WORKAROUND_FLAG_BUFLEN+1];
  char *tok;
  int flags = 0;

  if (!str)
    return -1;

  memset(buf, '\0', WORKAROUND_FLAG_BUFLEN+1);
  strncpy(buf, str, WORKAROUND_FLAG_BUFLEN);

  tok = strtok(buf, ",");
  while (tok)
    {
      if (!strcasecmp(tok, IPMI_TOOL_WORKAROUND_FLAGS_ACCEPT_SESSION_ID_ZERO_STR))
        flags |= IPMI_TOOL_WORKAROUND_FLAGS_ACCEPT_SESSION_ID_ZERO;
      else if (!strcasecmp(tok, IPMI_TOOL_WORKAROUND_FLAGS_FORCE_PERMSG_AUTHENTICATION_STR))
        flags |= IPMI_TOOL_WORKAROUND_FLAGS_FORCE_PERMSG_AUTHENTICATION;
      else if (!strcasecmp(tok, IPMI_TOOL_WORKAROUND_FLAGS_CHECK_UNEXPECTED_AUTHCODE_STR))
        flags |= IPMI_TOOL_WORKAROUND_FLAGS_CHECK_UNEXPECTED_AUTHCODE;
      else if (!strcasecmp(tok, IPMI_TOOL_WORKAROUND_FLAGS_BIG_ENDIAN_SEQUENCE_NUMBER_STR))
        flags |= IPMI_TOOL_WORKAROUND_FLAGS_BIG_ENDIAN_SEQUENCE_NUMBER;
      else if (!strcasecmp(tok, IPMI_TOOL_WORKAROUND_FLAGS_AUTHENTICATION_CAPABILITIES_STR))
        flags |= IPMI_TOOL_WORKAROUND_FLAGS_AUTHENTICATION_CAPABILITIES;
      else if (!strcasecmp(tok, IPMI_TOOL_WORKAROUND_FLAGS_INTEL_2_0_SESSION_STR))
        flags |= IPMI_TOOL_WORKAROUND_FLAGS_INTEL_2_0_SESSION;
      else if (!strcasecmp(tok, IPMI_TOOL_WORKAROUND_FLAGS_SUPERMICRO_2_0_SESSION_STR))
        flags |= IPMI_TOOL_WORKAROUND_FLAGS_SUPERMICRO_2_0_SESSION;
      else if (!strcasecmp(tok, IPMI_TOOL_WORKAROUND_FLAGS_SUN_2_0_SESSION_STR))
        flags |= IPMI_TOOL_WORKAROUND_FLAGS_SUN_2_0_SESSION;
      else if (!strcasecmp (tok, IPMI_TOOL_WORKAROUND_FLAGS_OPEN_SESSION_PRIVILEGE_STR))
        flags |= IPMI_TOOL_WORKAROUND_FLAGS_OPEN_SESSION_PRIVILEGE;
      else if (!strcasecmp(tok, IPMI_TOOL_WORKAROUND_FLAGS_IGNORE_SOL_PAYLOAD_SIZE_STR))
        flags |= IPMI_TOOL_WORKAROUND_FLAGS_IGNORE_SOL_PAYLOAD_SIZE;
      else if (!strcasecmp(tok, IPMI_TOOL_WORKAROUND_FLAGS_IGNORE_SOL_PORT_STR))
        flags |= IPMI_TOOL_WORKAROUND_FLAGS_IGNORE_SOL_PORT;
      else if (!strcasecmp(tok, IPMI_TOOL_WORKAROUND_FLAGS_SKIP_SOL_ACTIVATION_STATUS_STR))
        flags |= IPMI_TOOL_WORKAROUND_FLAGS_SKIP_SOL_ACTIVATION_STATUS;
      tok = strtok(NULL, ",");
    }
  return flags;
}

/* From David Wheeler's Secure Programming Guide */
static void *
__secure_memset(void *s, int c, size_t n)
{
  volatile char *p;

  if (!s || !n)
    return NULL;

  p = s;
  while (n--)
    *p++=c;

  return s;
}

error_t 
common_parse_opt (int key, 
		  char *arg, 
		  struct argp_state *state, 
		  struct common_cmd_args *cmd_args)
{
  char *ptr;
  int tmp;
  int n;

  switch (key)
    {
    case ARGP_DRIVER_TYPE_KEY: 
      if (cmd_args->driver_type_outofband_only)
        {
          if ((tmp = parse_outofband_driver_type (arg)) < 0)
            {
              fprintf(stderr, "invalid driver type\n");
              exit(1);
            }
        }
      else
        {
          if ((tmp = parse_driver_type (arg)) < 0)
            {
              fprintf(stderr, "invalid driver type\n");
              exit(1);
            }
        }
      cmd_args->driver_type = tmp;
      break;
      /* ARGP_NO_PROBING_KEY for backwards compatability */
    case ARGP_NO_PROBING_KEY:
    case ARGP_DISABLE_AUTO_PROBE_KEY:
      cmd_args->disable_auto_probe = 1;
      break;
    case ARGP_DRIVER_ADDRESS_KEY:
      errno = 0;
      tmp = strtol (arg, &ptr, 0);
      if (ptr != (arg + strlen(arg))
          || errno
          || tmp <= 0)
        {
          fprintf (stderr, "invalid driver address\n");
          exit(1);
        }
      cmd_args->driver_address = tmp;
      break;
    case ARGP_DRIVER_DEVICE_KEY:
      if (cmd_args->driver_device)
	free (cmd_args->driver_device);
      if (!(cmd_args->driver_device = strdup (arg)))
        {
          perror("strdup");
          exit(1);
        }
      break;
    case ARGP_REGISTER_SPACING_KEY:
      errno = 0;
      tmp = strtol (arg, &ptr, 0);
      if (ptr != (arg + strlen(arg))
          || errno
          || tmp <= 0)
        {
          fprintf (stderr, "invalid register spacing\n");
          exit(1);
        }
      cmd_args->register_spacing = tmp;
      break;
    case ARGP_HOSTNAME_KEY:
      if (cmd_args->hostname)
	free (cmd_args->hostname);
      if (!(cmd_args->hostname = strdup (arg)))
        {
          perror("strdup");
          exit(1);
        }
      break;
    case ARGP_USERNAME_KEY:
      if (strlen (arg) > IPMI_MAX_USER_NAME_LENGTH)
        {
          fprintf (stderr, "username too long\n");
          exit(1);
        }
      else 
	{
	  if (cmd_args->username)
	    free (cmd_args->username);
	  if (!(cmd_args->username = strdup (arg)))
            {
              perror("strdup");
              exit(1);
            }
	}
      n = strlen(arg);
      __secure_memset(arg, '\0', n);
      break;
    case ARGP_PASSWORD_KEY:
      if (strlen (arg) > IPMI_2_0_MAX_PASSWORD_LENGTH)
        {
          fprintf (stderr, "password too long\n");
          exit(1);
        }
      else 
	{
	  if (cmd_args->password)
	    free (cmd_args->password);
	  if (!(cmd_args->password = strdup (arg)))
            {
              perror("strdup");
              exit(1);
            }
	}
      n = strlen(arg);
      __secure_memset(arg, '\0', n);
      break;
    case ARGP_PASSWORD_PROMPT_KEY:
      if (cmd_args->password)
        free (cmd_args->password);
      arg = getpass ("Password: ");
      if (arg && strlen (arg) > IPMI_2_0_MAX_PASSWORD_LENGTH)
        {
          fprintf (stderr, "password too long\n");
          exit(1);
        }
      if (!(cmd_args->password = strdup (arg)))
        {
          perror("strdup");
          exit(1);
        }
      break;
    case ARGP_K_G_KEY:
      {
        int rv;

        if (cmd_args->k_g_len)
          {
            memset(cmd_args->k_g, '\0', IPMI_MAX_K_G_LENGTH + 1);
            cmd_args->k_g_len = 0;
          }

        if ((rv = check_kg_len(arg)) < 0)
          {
            fprintf (stderr, "k_g too long\n");
            exit(1);
          }

        if ((rv = parse_kg(cmd_args->k_g, IPMI_MAX_K_G_LENGTH + 1, arg)) < 0)
          {
            fprintf (stderr, "k_g input formatted incorrectly\n");
            exit(1);
          }
        if (rv > 0)
          cmd_args->k_g_len = rv;
        n = strlen(arg);
        __secure_memset(arg, '\0', n);
      }
      break;
    case ARGP_K_G_PROMPT_KEY:
      {
        int rv;
        
        if (cmd_args->k_g_len)
          {
            memset(cmd_args->k_g, '\0', IPMI_MAX_K_G_LENGTH + 1);
            cmd_args->k_g_len = 0;
          }
        
        arg = getpass ("K_g: ");
        
        if ((rv = check_kg_len(arg)) < 0)
          {
            fprintf (stderr, "k_g too long\n");
            exit(1);
          }

        if ((rv = parse_kg(cmd_args->k_g, IPMI_MAX_K_G_LENGTH + 1, arg)) < 0)
          {
            fprintf (stderr, "k_g input formatted incorrectly\n");
            exit(1);
          }
        if (rv > 0)
          cmd_args->k_g_len = rv;
      }
      break;
      /* ARGP_TIMEOUT_KEY for backwards compatability */
    case ARGP_TIMEOUT_KEY:
    case ARGP_SESSION_TIMEOUT_KEY:
      errno = 0;
      tmp = strtol (arg, &ptr, 0);
      if (ptr != (arg + strlen(arg))
          || errno
          || tmp <= 0)
        {
          fprintf (stderr, "invalid session timeout\n");
          exit(1);
        }
      cmd_args->session_timeout = tmp;
      break;
    /* ARGP_RETRY_TIMEOUT_KEY for backwards compatability */
    case ARGP_RETRY_TIMEOUT_KEY:
    case ARGP_RETRANSMISSION_TIMEOUT_KEY:
      errno = 0;
      tmp = strtol (arg, &ptr, 0);
      if (ptr != (arg + strlen(arg))
          || errno
          || tmp <= 0)
        {
          fprintf (stderr, "invalid retransmission timeout\n");
          exit(1);
        }
      cmd_args->retransmission_timeout = tmp;
      break;
    /* ARGP_AUTH_TYPE_KEY for backwards compatability */
    case ARGP_AUTH_TYPE_KEY:
    case ARGP_AUTHENTICATION_TYPE_KEY: 
      if ((tmp = parse_authentication_type (arg)) < 0)
        {
          fprintf(stderr, "invalid authentication type\n");
          exit(1);
        }
      cmd_args->authentication_type = tmp;
      break;
    case ARGP_CIPHER_SUITE_ID_KEY: 
      errno = 0;
      tmp = strtol (arg, &ptr, 0);
      if (ptr != (arg + strlen(arg))
          || errno
          || tmp < IPMI_CIPHER_SUITE_ID_MIN
          || tmp > IPMI_CIPHER_SUITE_ID_MAX
          || !IPMI_CIPHER_SUITE_ID_SUPPORTED (tmp))
        {
          fprintf (stderr, "invalid cipher suite id\n");
          exit(1);
        }
      cmd_args->cipher_suite_id = tmp;
      break;
    /* ARGP_PRIVILEGE_KEY for backwards compatability */
    /* ARGP_PRIV_LEVEL_KEY for backwards compatability */\
    case ARGP_PRIVILEGE_KEY:
    case ARGP_PRIV_LEVEL_KEY:
    case ARGP_PRIVILEGE_LEVEL_KEY: 
      if ((tmp = parse_privilege_level (arg)) < 0)
        {
          fprintf(stderr, "invalid privilege level\n");
          exit(1);
        }
      cmd_args->privilege_level = tmp;
      break;
      /* ARGP_CONFIG_KEY for backwards compatability */
    case ARGP_CONFIG_KEY:
    case ARGP_CONFIG_FILE_KEY:
      /* ignore config option - should have been parsed earlier */
      break;
    case ARGP_WORKAROUND_FLAGS_KEY:
      if ((tmp = parse_workaround_flags(arg)) < 0)
        {
          fprintf(stderr, "invalid workaround flags\n");
          exit(1);
        }
      cmd_args->workaround_flags |= tmp;
      break;
    case ARGP_DEBUG_KEY:
      cmd_args->debug++;
      break;
    default:
      return ARGP_ERR_UNKNOWN;
    }
  
  return 0;
}

error_t 
sdr_parse_opt (int key, 
               char *arg, 
               struct argp_state *state, 
               struct sdr_cmd_args *cmd_args)
{
  switch (key)
    {
    case ARGP_FLUSH_CACHE_KEY:
      cmd_args->flush_cache = 1;
      break;
    case ARGP_QUIET_CACHE_KEY:
      cmd_args->quiet_cache = 1;
      break;
    case ARGP_SDR_CACHE_DIRECTORY_KEY:
      if (cmd_args->sdr_cache_directory)
        free(cmd_args->sdr_cache_directory);
      if (!(cmd_args->sdr_cache_directory = strdup (arg)))
        {
          perror("strdup");
          exit(1);
        }
      break;
    case ARGP_SDR_CACHE_RECREATE_KEY:
      cmd_args->sdr_cache_recreate = 1;
      break;
    case ARGP_IGNORE_SDR_CACHE_KEY:
      cmd_args->ignore_sdr_cache = 1;
      break;
    default:
      return ARGP_ERR_UNKNOWN;
    }
  
  return 0;
}

error_t 
hostrange_parse_opt (int key, 
                     char *arg, 
                     struct argp_state *state, 
                     struct hostrange_cmd_args *cmd_args)
{
  char *ptr;
  int tmp;

  switch (key)
    {
    case ARGP_BUFFER_OUTPUT_KEY:
      cmd_args->buffer_output = 1;
      break;
    case ARGP_CONSOLIDATE_OUTPUT_KEY:
      cmd_args->consolidate_output = 1;
      break;
    case ARGP_FANOUT_KEY:
      tmp = strtol(arg, &ptr, 10);
      if ((ptr != (arg + strlen(arg)))
          || (tmp < PSTDOUT_FANOUT_MIN)
          || (tmp > PSTDOUT_FANOUT_MAX))
        {
          fprintf (stderr, "invalid fanout\n");
          exit(1);
          break;
        }
      cmd_args->fanout = tmp;
      break;
    case ARGP_ELIMINATE_KEY:
      cmd_args->eliminate = 1;
      break;
    case ARGP_ALWAYS_PREFIX_KEY:
      cmd_args->always_prefix = 1;
      break;
    default:
      return ARGP_ERR_UNKNOWN;
    }
  
  return 0;
}

static void 
_init_common_cmd_args (struct common_cmd_args *cmd_args)
{
  cmd_args->disable_auto_probe = 0;
  cmd_args->driver_type = IPMI_DEVICE_UNKNOWN;
  cmd_args->driver_type_outofband_only = 0;
  cmd_args->driver_address = 0;
  cmd_args->driver_device = NULL;
  cmd_args->register_spacing = 0;
  cmd_args->session_timeout = 0;
  cmd_args->retransmission_timeout = 0;
  cmd_args->hostname = NULL;
  cmd_args->username = NULL;
  cmd_args->password = NULL;
  memset(cmd_args->k_g, '\0', IPMI_MAX_K_G_LENGTH + 1);
  cmd_args->k_g_len = 0;
  cmd_args->authentication_type = IPMI_AUTHENTICATION_TYPE_MD5;
  cmd_args->cipher_suite_id = 3;
  /* privilege_level set by parent function */
  cmd_args->config_file = NULL;
  cmd_args->workaround_flags = 0;
  cmd_args->debug = 0;
}

void 
init_common_cmd_args_user (struct common_cmd_args *cmd_args)
{
  _init_common_cmd_args(cmd_args);
  cmd_args->privilege_level = IPMI_PRIVILEGE_LEVEL_USER;
}

void 
init_common_cmd_args_operator (struct common_cmd_args *cmd_args)
{
  _init_common_cmd_args(cmd_args);
  cmd_args->privilege_level = IPMI_PRIVILEGE_LEVEL_OPERATOR;
}

void 
init_common_cmd_args_admin (struct common_cmd_args *cmd_args)
{
  _init_common_cmd_args(cmd_args);
  cmd_args->privilege_level = IPMI_PRIVILEGE_LEVEL_ADMIN;
}

void 
free_common_cmd_args (struct common_cmd_args *cmd_args)
{
  if (cmd_args->driver_device)
    {
      free (cmd_args->driver_device);
      cmd_args->driver_device = NULL;
    }
  if (cmd_args->hostname)
    {
      free (cmd_args->hostname);
      cmd_args->hostname = NULL;
    }
  if (cmd_args->username)
    {
      free (cmd_args->username);
      cmd_args->username = NULL;
    }
  if (cmd_args->password)
    {
      free (cmd_args->password);
      cmd_args->password = NULL;
    }
  if (cmd_args->config_file)
    {
      free (cmd_args->config_file);
      cmd_args->config_file = NULL;
    }
}

void
verify_common_cmd_args (struct common_cmd_args *cmd_args)
{
  if ((cmd_args->driver_type == IPMI_DEVICE_LAN
       || cmd_args->driver_type == IPMI_DEVICE_LAN_2_0)
      && !cmd_args->hostname)
    {
      fprintf (stderr, "hostname not specified\n");
      exit (1);
    }

  if (cmd_args->driver_device)
    {
      if (access (cmd_args->driver_device, R_OK|W_OK) != 0)
        {
          fprintf (stderr, "insufficient permission on driver device '%s'\n",
                   cmd_args->driver_device);
          exit(1);
        }
    }

  if (cmd_args->hostname)
    {
      /* We default to IPMI 1.5 if the user doesn't specify LAN vs. LAN_2_0 */

      if (cmd_args->driver_type != IPMI_DEVICE_LAN_2_0
          && cmd_args->password
          && strlen (cmd_args->password) > IPMI_1_5_MAX_PASSWORD_LENGTH)
        {
          fprintf (stderr, "password too long\n");
          exit (1);
        }
      /* else, 2_0 password length was checked in argp_parse() previously */
    }

  if (cmd_args->retransmission_timeout > cmd_args->session_timeout)
    {
      fprintf (stderr, "retransmission timeout larger than session timeout\n");
      exit(1);
    }
}

void 
init_sdr_cmd_args (struct sdr_cmd_args *cmd_args)
{
  cmd_args->flush_cache = 0;
  cmd_args->quiet_cache = 0;
  cmd_args->sdr_cache_directory = NULL;
  cmd_args->sdr_cache_recreate = 0;
  cmd_args->ignore_sdr_cache = 0;
}

void 
free_sdr_cmd_args (struct sdr_cmd_args *cmd_args)
{
  if (cmd_args->sdr_cache_directory)
    {
      free (cmd_args->sdr_cache_directory);
      cmd_args->sdr_cache_directory = NULL;
    }
}

void
verify_sdr_cmd_args (struct sdr_cmd_args *cmd_args)
{
  if (cmd_args->sdr_cache_directory)
    {
      if (access (cmd_args->sdr_cache_directory, R_OK|W_OK|X_OK) != 0)
        {
          fprintf (stderr, "insufficient permission on sensor cache directory '%s'\n",
                   cmd_args->sdr_cache_directory);
          exit(1);
        }
    }
}

void 
init_hostrange_cmd_args (struct hostrange_cmd_args *cmd_args)
{
  cmd_args->buffer_output = 0;
  cmd_args->consolidate_output = 0;
  cmd_args->fanout = 0;
  cmd_args->eliminate = 0;
  cmd_args->always_prefix = 0;
}

void 
free_hostrange_cmd_args (struct hostrange_cmd_args *cmd_args)
{
  /* nothing right now */
}

void
verify_hostrange_cmd_args (struct hostrange_cmd_args *cmd_args)
{
  if (cmd_args->buffer_output && cmd_args->consolidate_output)
    {
      fprintf (stderr, "cannot buffer and consolidate hostrange output, please select only one\n");
      exit(1);
    }
}
