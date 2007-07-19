/*
   argp-common.c: common work for argp for all freeipmi tools.
   Copyright (C) 2005 FreeIPMI Core Team

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
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
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
#endif /* HAVE_UNISTD_H */
#ifdef HAVE_ERROR_H
#include <error.h>
#endif
#include <assert.h>
#include <argp.h>

#include "freeipmi/ipmi-authentication-type-spec.h"
#include "freeipmi/ipmi-cipher-suite-spec.h"
#include "freeipmi/ipmi-privilege-level-spec.h"

#include "argp-common.h"
#include "freeipmi-portability.h"
#include "pstdout.h"
#include "tool-common.h"

#define WORKAROUND_FLAG_BUFLEN 1024

int
parse_driver_type(char *str)
{
  assert(str);

  if (strcasecmp (str, "lan") == 0)
    return IPMI_DEVICE_LAN;
  else if (strcasecmp (str, "lan_2_0") == 0
           || strcasecmp (str, "lan20") == 0
           || strcasecmp (str, "lan_20") == 0
           || strcasecmp (str, "lan2_0") == 0
           || strcasecmp (str, "lan2_0") == 0)
    return IPMI_DEVICE_LAN_2_0;
  else if (strcasecmp (str, "kcs") == 0)
    return IPMI_DEVICE_KCS;
  else if (strcasecmp (str, "ssif") == 0)
    return IPMI_DEVICE_SSIF;
  else if (strcasecmp (str, "openipmi") == 0)
    return IPMI_DEVICE_OPENIPMI;

  return -1;
}

int
parse_outofband_workaround_flags(char *str)
{
  char buf[WORKAROUND_FLAG_BUFLEN+1];
  char *tok;
  int flags = 0;

  assert(str);

  memset(buf, '\0', WORKAROUND_FLAG_BUFLEN+1);
  strncpy(buf, str, WORKAROUND_FLAG_BUFLEN);

  tok = strtok(buf, ",");
  while (tok)
    {
      if (!strcasecmp(tok, IPMI_OUTOFBAND_WORKAROUND_FLAGS_ACCEPT_SESSION_ID_ZERO_STR))
        flags |= IPMI_OUTOFBAND_WORKAROUND_FLAGS_FORCE_PERMSG_AUTHENTICATION;
      else if (!strcasecmp(tok, IPMI_OUTOFBAND_WORKAROUND_FLAGS_FORCE_PERMSG_AUTHENTICATION_STR))
        flags |= IPMI_OUTOFBAND_WORKAROUND_FLAGS_ACCEPT_SESSION_ID_ZERO;
      else if (!strcasecmp(tok, IPMI_OUTOFBAND_WORKAROUND_FLAGS_CHECK_UNEXPECTED_AUTHCODE_STR))
        flags |= IPMI_OUTOFBAND_WORKAROUND_FLAGS_CHECK_UNEXPECTED_AUTHCODE;
      else if (!strcasecmp(tok, IPMI_OUTOFBAND_WORKAROUND_FLAGS_BIG_ENDIAN_SEQUENCE_NUMBER_STR))
        flags |= IPMI_OUTOFBAND_WORKAROUND_FLAGS_BIG_ENDIAN_SEQUENCE_NUMBER;
      tok = strtok(NULL, ",");
    }
  return flags;
}

int
parse_outofband_2_0_workaround_flags(char *str)
{
  char buf[WORKAROUND_FLAG_BUFLEN+1];
  char *tok;
  int flags = 0;
  
  assert(str);

  memset(buf, '\0', WORKAROUND_FLAG_BUFLEN+1);
  strncpy(buf, str, WORKAROUND_FLAG_BUFLEN);

  tok = strtok(buf, ",");
  while (tok)
    {
      if (!strcasecmp(tok, IPMI_OUTOFBAND_2_0_WORKAROUND_FLAGS_INTEL_2_0_SESSION_STR))
        flags |= IPMI_OUTOFBAND_2_0_WORKAROUND_FLAGS_INTEL_2_0_SESSION;
      else if (!strcasecmp(tok, IPMI_OUTOFBAND_2_0_WORKAROUND_FLAGS_SUPERMICRO_2_0_SESSION_STR))
        flags |= IPMI_OUTOFBAND_2_0_WORKAROUND_FLAGS_SUPERMICRO_2_0_SESSION;
      else if (!strcasecmp(tok, IPMI_OUTOFBAND_2_0_WORKAROUND_FLAGS_SUN_2_0_SESSION_STR))
        flags |= IPMI_OUTOFBAND_2_0_WORKAROUND_FLAGS_SUN_2_0_SESSION;
      tok = strtok(NULL, ",");
    }
  return flags;
}

int
parse_inband_workaround_flags(char *str)
{
  assert(str);

  /* no inband workarounds to parse yet */
  return 0;
}

/* From David Wheeler's Secure Programming Guide */
static void *
__secure_memset(void *s, int c, size_t n)
{
  volatile char *p;

  assert(s);
  assert(n);

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
  int tmp;

  switch (key)
    {
    case DRIVER_TYPE_KEY: 
      if ((tmp = parse_driver_type (arg)) < 0)
        {
          fprintf(stderr, "invalid driver type specified\n");
          argp_usage (state);
        }
      cmd_args->driver_type = tmp;
      break;
    case NO_PROBING_KEY:
      cmd_args->disable_auto_probe = 1;
      break;
    case DRIVER_ADDRESS_KEY:
      {
	int value = 0;
	char *str = NULL;
	char *tail = NULL;
	int errnum = 0;
	
	str = strdupa (arg);
	errno = 0;
	value = strtol (str, &tail, 0);
	errnum = errno;
	
	if (errnum)
	  {
	    // overflow
	    fprintf (stderr, "invalid driver address\n");
	    argp_usage (state);
	    break;
	  }
	
	if (tail[0] != '\0')
	  {
	    // invalid integer format
	    fprintf (stderr, "invalid driver address\n");
	    argp_usage (state);
	    break;
	  }
	
	if (value < 0)
	  {
	    // negative number
	    fprintf (stderr, "invalid driver address\n");
	    argp_usage (state);
	    break;
	  }
	cmd_args->driver_address = value;
      }
      break;
    case DRIVER_DEVICE_KEY:
      if (cmd_args->driver_device != NULL)
	free (cmd_args->driver_device);
      if (!(cmd_args->driver_device = strdup (arg)))
        {
          perror("strdup");
          exit(1);
        }
      break;
    case REGISTER_SPACING_KEY:
      {
        int value = 0;
	char *str = NULL;
	char *tail = NULL;
	int errnum = 0;
	
	str = strdupa (arg);
	value = strtol (str, &tail, 0);
	errnum = errno;
	
	if (errnum)
	  {
	    // overflow
	    fprintf (stderr, "invalid register spacing\n");
	    argp_usage (state);
	    break;
	  }
	
	if (tail[0] != '\0')
	  {
	    // invalid integer format
	    fprintf (stderr, "invalid register spacing\n");
	    argp_usage (state);
	    break;
	  }
	
	if (value < 0)
	  {
	    // negative number
	    fprintf (stderr, "invalid register spacing\n");
	    argp_usage (state);
	    break;
	  }
	cmd_args->register_spacing = value;
      }
      break;
    case HOSTNAME_KEY:
      if (cmd_args->host != NULL)
	free (cmd_args->host);
      if (!(cmd_args->host = strdup (arg)))
        {
          perror("strdup");
          exit(1);
        }
      break;
    case USERNAME_KEY:
      if (strlen (arg) > IPMI_MAX_USER_NAME_LENGTH)
        {
          fprintf (stderr, "username too long\n");
          argp_usage (state);
        }
      else 
	{
	  if (cmd_args->username != NULL)
	    free (cmd_args->username);
	  if (!(cmd_args->username = strdup (arg)))
            {
              perror("strdup");
              exit(1);
            }
	}
      if (arg)
        {
          int n;
          n = strlen(arg);
          __secure_memset(arg, '\0', n);
        }
      break;
    case PASSWORD_KEY:
      if (arg && strlen (arg) > IPMI_2_0_MAX_PASSWORD_LENGTH)
        {
          fprintf (stderr, "password too long\n");
          argp_usage (state);
        }
      else 
	{
	  if (cmd_args->password != NULL)
	    free (cmd_args->password);
	  if (!(cmd_args->password = strdup (arg)))
            {
              perror("strdup");
              exit(1);
            }
	}
      if (arg)
        {
          int n;
          n = strlen(arg);
          __secure_memset(arg, '\0', n);
        }
      break;
    case PASSWORD_PROMPT_KEY:
      if (cmd_args->password != NULL)
        free (cmd_args->password);
      arg = getpass ("Password: ");
      if (arg && strlen (arg) > IPMI_2_0_MAX_PASSWORD_LENGTH)
        {
          fprintf (stderr, "password too long\n");
          argp_usage (state);
        }
      if (!(cmd_args->password = strdup (arg)))
        {
          perror("strdup");
          exit(1);
        }
      break;
    case K_G_KEY:
      {
        int rv;

        if (cmd_args->k_g_configured)
          {
            memset(cmd_args->k_g, '\0', IPMI_MAX_K_G_LENGTH);
            cmd_args->k_g_configured = 0;
          }

        if ((rv = parse_kg(cmd_args->k_g, IPMI_MAX_K_G_LENGTH, arg)) < 0)
          {
            fprintf (stderr, "k_g invalid\n");
            argp_usage (state);
          }
        if (rv > 0)
          cmd_args->k_g_configured++;
        if (arg)
          {
            int n;
            n = strlen(arg);
            __secure_memset(arg, '\0', n);
          }
      }
      break;
    case K_G_PROMPT_KEY:
      {
        int rv;
        
        if (cmd_args->k_g_configured)
          {
            memset(cmd_args->k_g, '\0', IPMI_MAX_K_G_LENGTH);
            cmd_args->k_g_configured = 0;
          }
        
        arg = getpass ("K_g: ");
        
        if ((rv = parse_kg(cmd_args->k_g, IPMI_MAX_K_G_LENGTH, arg)) < 0)
          {
            fprintf (stderr, "k_g invalid\n");
            argp_usage (state);
          }
        if (rv > 0)
          cmd_args->k_g_configured++;
      }
      break;
    case RETRANSMISSION_TIMEOUT_KEY:
      {
	int value = 0;
	char *str = NULL;
	char *tail = NULL;
	int errnum = 0;
	
	str = strdupa (arg);
	errno = 0;
	value = strtol (str, &tail, 0);
	errnum = errno;
	
	if (errnum)
	  {
	    // overflow
	    fprintf (stderr, "invalid packet retransmission timeout value\n");
	    argp_usage (state);
	    break;
	  }
	
	if (tail[0] != '\0')
	  {
	    // invalid integer format
	    fprintf (stderr, "invalid packet retransmission timeout value\n");
	    argp_usage (state);
	    break;
	  }
	
	if (value < 0)
	  {
	    // negative number
	    fprintf (stderr, "invalid packet retransmission timeout value\n");
	    argp_usage (state);
	    break;
	  }
	cmd_args->retransmission_timeout = value;
      }
      break;
    case SESSION_TIMEOUT_KEY:
      {
	int value = 0;
	char *str = NULL;
	char *tail = NULL;
	int errnum = 0;
	
	str = strdupa (arg);
	errno = 0;
	value = strtol (str, &tail, 0);
	errnum = errno;
	
	if (errnum)
	  {
	    // overflow
	    fprintf (stderr, "invalid packet session timeout value\n");
	    argp_usage (state);
	    break;
	  }
	
	if (tail[0] != '\0')
	  {
	    // invalid integer format
	    fprintf (stderr, "invalid packet session timeout value\n");
	    argp_usage (state);
	    break;
	  }
	
	if (value < 0)
	  {
	    // negative number
	    fprintf (stderr, "invalid packet session timeout value\n");
	    argp_usage (state);
	    break;
	  }
	cmd_args->session_timeout = value;
      }
      break;
    case AUTHENTICATION_TYPE_KEY: 
      if (strcasecmp (arg, "none") == 0)
        cmd_args->authentication_type = IPMI_AUTHENTICATION_TYPE_NONE;
      /* keep "plain" for backwards compatability */
      else if (strcasecmp (arg, "plain") == 0
               || strcasecmp (arg, "straight_password_key"))
        cmd_args->authentication_type = IPMI_AUTHENTICATION_TYPE_STRAIGHT_PASSWORD_KEY;
      else if (strcasecmp (arg, "md2") == 0)
        cmd_args->authentication_type = IPMI_AUTHENTICATION_TYPE_MD2;
      else if (strcasecmp (arg, "md5") == 0)
        cmd_args->authentication_type = IPMI_AUTHENTICATION_TYPE_MD5;
      else 
        {
          fprintf(stderr, "invalid authentication type specified\n");
          argp_usage (state);
        }
      break;
    case CIPHER_SUITE_ID_KEY: 
      {
	int value = 0;
	char *str = NULL;
	char *tail = NULL;
	int errnum = 0;
	
	str = strdupa (arg);
	errno = 0;
	value = strtol (str, &tail, 0);
	errnum = errno;
	
	if (errnum)
	  {
	    // overflow
	    fprintf (stderr, "invalid cipher suite id value\n");
	    argp_usage (state);
	    break;
	  }
	
	if (tail[0] != '\0')
	  {
	    // invalid integer format
	    fprintf (stderr, "invalid cipher suite id value\n");
	    argp_usage (state);
	    break;
	  }

        if (value < IPMI_CIPHER_SUITE_ID_MIN
            || value > IPMI_CIPHER_SUITE_ID_MAX)
          {
	    fprintf (stderr, "invalid cipher suite id value\n");
	    argp_usage (state);
	    break;
          }

	if (!IPMI_CIPHER_SUITE_ID_SUPPORTED (value))
	  {
	    fprintf (stderr, "unsupported cipher suite id value\n");
	    argp_usage (state);
	    break;
	  }
        cmd_args->cipher_suite_id = value;
      }
      break;
    case PRIVILEGE_LEVEL_KEY: 
      if (strcasecmp (arg, "user") == 0)
        cmd_args->privilege_level = IPMI_PRIVILEGE_LEVEL_USER;
      else if (strcasecmp (arg, "operator") == 0)
        cmd_args->privilege_level = IPMI_PRIVILEGE_LEVEL_OPERATOR;
      else if (strcasecmp (arg, "admin") == 0
               || strcasecmp (arg, "administrator") == 0)
        cmd_args->privilege_level = IPMI_PRIVILEGE_LEVEL_ADMIN;
      else if (strcasecmp (arg, "oem") == 0)
        cmd_args->privilege_level = IPMI_PRIVILEGE_LEVEL_OEM;
      else 
        {
          fprintf(stderr, "invalid privilege level specified\n");
          argp_usage (state);
        }
      break;
    case WORKAROUND_FLAGS_KEY:
      tmp = parse_outofband_workaround_flags(arg);
      cmd_args->workaround_flags |= tmp;
      tmp = parse_outofband_2_0_workaround_flags(arg);
      if (tmp && cmd_args->workaround_flags)
        {
          fprintf (stderr, "specified conflicting workaround options\n");
          argp_usage (state);
        }
      cmd_args->workaround_flags |= tmp;
      tmp = parse_inband_workaround_flags(arg);
      if (tmp && cmd_args->workaround_flags)
        {
          fprintf (stderr, "specified conflicting workaround options\n");
          argp_usage (state);
        }
      cmd_args->workaround_flags |= tmp;
      break;
#ifndef NDEBUG
    case DEBUG_KEY:
      cmd_args->flags |= IPMI_FLAGS_DEBUG_DUMP;
      break;
#endif /* NDEBUG */
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
    case FLUSH_CACHE_KEY:
      cmd_args->flush_cache_wanted = 1;
      break;
    case QUIET_CACHE_KEY:
      cmd_args->quiet_cache_wanted = 1;
      break;
    case SDR_CACHE_DIR_KEY:
      cmd_args->sdr_cache_dir_wanted = 1;
      if (!(cmd_args->sdr_cache_dir = strdup (arg)))
        {
          perror("strdup");
          exit(1);
        }
      if (access (cmd_args->sdr_cache_dir, R_OK|W_OK|X_OK) != 0)
        {
          fprintf (stderr, "insufficient permission on sensor cache directory [%s]\n",
                   cmd_args->sdr_cache_dir);
          argp_usage (state);
        }
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
  
  switch (key)
    {
    case BUFFER_KEY:
      cmd_args->buffer_hostrange_output = 1;
      break;
    case CONSOLIDATE_KEY:
      cmd_args->consolidate_hostrange_output = 1;
      break;
    case FANOUT_KEY:
      cmd_args->fanout = strtol(arg, &ptr, 10);
      if ((ptr != (arg + strlen(arg)))
          || (cmd_args->fanout < PSTDOUT_FANOUT_MIN)
          || (cmd_args->fanout > PSTDOUT_FANOUT_MAX))
        {
          fprintf (stderr, "invalid fanout\n");
          argp_usage (state);
          break;
        }
      break;
    case ELIMINATE_KEY:
      cmd_args->eliminate = 1;
      break;

    default:
      return ARGP_ERR_UNKNOWN;
    }
  
  return 0;
}

void 
init_common_cmd_args (struct common_cmd_args *cmd_args)
{
  cmd_args->disable_auto_probe = 0;
  cmd_args->driver_type = IPMI_DEVICE_UNKNOWN;
  cmd_args->driver_address = 0;
  cmd_args->driver_device = NULL;
  cmd_args->register_spacing = 0;
  cmd_args->session_timeout = 0;
  cmd_args->retransmission_timeout = 0;
  cmd_args->host = NULL;
  cmd_args->username = NULL;
  cmd_args->password = NULL;
  memset(cmd_args->k_g, '\0', IPMI_MAX_K_G_LENGTH);
  cmd_args->k_g_configured = 0;
  cmd_args->authentication_type = IPMI_AUTHENTICATION_TYPE_MD5;
  cmd_args->cipher_suite_id = 3;
  cmd_args->privilege_level = IPMI_PRIVILEGE_LEVEL_USER;
  cmd_args->workaround_flags = 0;
  cmd_args->flags = IPMI_FLAGS_DEFAULT;
}

void 
free_common_cmd_args (struct common_cmd_args *cmd_args)
{
  if (cmd_args->driver_device != NULL)
    {
      free (cmd_args->driver_device);
      cmd_args->driver_device = NULL;
    }
  if (cmd_args->host != NULL)
    {
      free (cmd_args->host);
      cmd_args->host = NULL;
    }
  if (cmd_args->username != NULL)
    {
      free (cmd_args->username);
      cmd_args->username = NULL;
    }
  if (cmd_args->password != NULL)
    {
      free (cmd_args->password);
      cmd_args->password = NULL;
    }
}

void
verify_common_cmd_args (struct common_cmd_args *cmd_args)
{
  if ((cmd_args->driver_type == IPMI_DEVICE_LAN
       || cmd_args->driver_type == IPMI_DEVICE_LAN_2_0)
      && !cmd_args->host)
    {
      fprintf (stderr, "host not specified\n");
      exit (1);
    }

  if (cmd_args->host)
    {
      /* We default to IPMI 1.5 if the user doesn't specify LAN vs. LAN_2_0 */

      if (cmd_args->driver_type != IPMI_DEVICE_LAN_2_0
          && cmd_args->password
          && strlen (cmd_args->password) > IPMI_1_5_MAX_PASSWORD_LENGTH)
        {
          fprintf (stderr, "password too long\n");
          exit (1);
        }
      /* else, 2_0 password length checked in argp_parse() */
    }
}

void 
init_sdr_cmd_args (struct sdr_cmd_args *cmd_args)
{
  cmd_args->flush_cache_wanted = 0;
  cmd_args->quiet_cache_wanted = 0;
  cmd_args->sdr_cache_dir_wanted = 0;
  cmd_args->sdr_cache_dir = NULL;
}

void 
free_sdr_cmd_args (struct sdr_cmd_args *cmd_args)
{
  if (cmd_args->sdr_cache_dir)
    {
      free (cmd_args->sdr_cache_dir);
      cmd_args->sdr_cache_dir = NULL;
    }
}

void
verify_sdr_cmd_args (struct sdr_cmd_args *cmd_args)
{
  /* nothing right now */
}

void 
init_hostrange_cmd_args (struct hostrange_cmd_args *cmd_args)
{
  cmd_args->buffer_hostrange_output = 0;
  cmd_args->consolidate_hostrange_output = 0;
  cmd_args->fanout = 0;
  cmd_args->eliminate = 0;
}

void 
free_hostrange_cmd_args (struct hostrange_cmd_args *cmd_args)
{
  /* nothing right now */
}

void
verify_hostrange_cmd_args (struct hostrange_cmd_args *cmd_args)
{
  /* nothing right now */
}
