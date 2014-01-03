/*****************************************************************************\
 *  $Id: ipmiconsole.c,v 1.102 2010-02-08 22:02:30 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2014 Lawrence Livermore National Security, LLC.
 *  Copyright (C) 2006-2007 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-221226
 *
 *  This file is part of Ipmiconsole, a set of IPMI 2.0 SOL libraries
 *  and utilities.  For details, see http://www.llnl.gov/linux/.
 *
 *  Ipmiconsole is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 3 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmiconsole is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmiconsole.  If not, see <http://www.gnu.org/licenses/>.
\*****************************************************************************/

#ifdef HAVE_CONFIG_H
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
#if HAVE_FCNTL_H
#include <fcntl.h>
#endif /* HAVE_FCNTL_H */
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
#include <sys/select.h>
#ifdef HAVE_PTHREAD_H
#include <pthread.h>
#endif /* HAVE_PTHREAD_H */
#include <limits.h>
#include <sys/resource.h>
#include <assert.h>
#include <errno.h>

#include "ipmiconsole.h"
#include "ipmiconsole_defs.h"

#include "ipmiconsole_ctx.h"
#include "ipmiconsole_debug.h"
#include "ipmiconsole_engine.h"
#include "ipmiconsole_util.h"

#include "freeipmi-portability.h"
#include "conffile.h"
#include "parse-common.h"
#include "secure.h"

/*
 * ipmi console errmsgs
 */
static char *ipmiconsole_errmsgs[] =
  {
    "success",                                            /* 0 */
    "ctx null",                                           /* 1 */
    "ctt invalid",                                        /* 2 */
    "engine already setup",                               /* 3 */
    "engine not setup",                                   /* 4 */
    "ctx not submitted",                                  /* 5 */
    "ctx is submitted",                                   /* 6 */
    "invalid parameters",                                 /* 7 */
    "hostname invalid",                                   /* 8 */
    "ipmi 2.0 unavailable",                               /* 9 */
    "cipher suite id unavailable",                        /* 10 */
    "username invalid",                                   /* 11 */
    "password invalid",                                   /* 12 */
    "k_g invalid",                                        /* 13 */
    "privilege level insufficient",                       /* 14 */
    "privilege level cannot be obtained for this user",   /* 15 */
    "SOL unavailable",                                    /* 16 */
    "SOL in use",                                         /* 17 */
    "SOL session stolen",                                 /* 18 */
    "SOL requires encryption",                            /* 19 */
    "SOL requires no encryption",                         /* 20 */
    "BMC Busy",                                           /* 21 */
    "BMC Error",                                          /* 22 */
    "BMC Implementation",                                 /* 23 */
    "connection timeout",                                 /* 24 */
    "session timeout",                                    /* 25 */
    "excess retransmissions sent",                        /* 26 */
    "excess errors received",                             /* 27 */
    "out of memory",                                      /* 28 */
    "too many open files",                                /* 29 */
    "internal system error",                              /* 30 */
    "internal error",                                     /* 31 */
    "errnum out of range",                                /* 32 */
    NULL
  };

#define IPMICONSOLE_ENGINE_CLOSE_FD_STR                   "closefd"
#define IPMICONSOLE_ENGINE_OUTPUT_ON_SOL_ESTABLISHED_STR  "outputonsolestablished"
#define IPMICONSOLE_ENGINE_LOCK_MEMORY_STR                "lockmemory"
#define IPMICONSOLE_ENGINE_SERIAL_KEEPALIVE_STR           "serialkeepalive"
#define IPMICONSOLE_ENGINE_SERIAL_KEEPALIVE_EMPTY_STR     "serialkeepaliveempty"

#define IPMICONSOLE_BEHAVIOR_ERROR_ON_SOL_INUSE_STR       "erroronsolinuse"
#define IPMICONSOLE_BEHAVIOR_DEACTIVATE_ONLY_STR          "deactivateonly"
#define IPMICONSOLE_BEHAVIOR_DEACTIVATE_ALL_INSTANCES_STR "deactivateallinstances"

#define IPMICONSOLE_DEBUG_STDOUT_STR                     "stdout"
#define IPMICONSOLE_DEBUG_STDERR_STR                     "stderr"
#define IPMICONSOLE_DEBUG_SYSLOG_STR                     "syslog"
#define IPMICONSOLE_DEBUG_FILE_STR                       "file"
#define IPMICONSOLE_DEBUG_IPMI_PACKETS_STR               "ipmipackets"

struct ipmiconsole_ctx_config default_config;

static int
_config_file_unsigned_int_positive (conffile_t cf,
                                    struct conffile_data *data,
                                    char *optionname,
                                    int option_type,
                                    void *option_ptr,
                                    int option_data,
                                    void *app_ptr,
                                    int app_data)
{
  unsigned int *value;

  assert (data);
  assert (option_ptr);

  value = (unsigned int *)option_ptr;
  
  if (data->intval <= 0)
    {
      IPMICONSOLE_DEBUG (("libipmiconsole config file %s invalid", optionname));
      return (0);
    }

  *value = (unsigned int)data->intval;
  return (0);
}

static int
_config_file_username (conffile_t cf,
                       struct conffile_data *data,
                       char *optionname,
                       int option_type,
                       void *option_ptr,
                       int option_data,
                       void *app_ptr,
                       int app_data)
{
  assert (data);

  if (strlen (data->string) > IPMI_MAX_USER_NAME_LENGTH)
    {
      IPMICONSOLE_DEBUG (("libipmiconsole config file username invalid length"));
      return (0);
    }

  strcpy (default_config.username, data->string);

  IPMICONSOLE_DEBUG (("libipmiconsole loaded alternate default username"));
  return (0);
}

static int
_config_file_password (conffile_t cf,
                       struct conffile_data *data,
                       char *optionname,
                       int option_type,
                       void *option_ptr,
                       int option_data,
                       void *app_ptr,
                       int app_data)
{
  assert (data);

  if (strlen (data->string) > IPMI_2_0_MAX_PASSWORD_LENGTH)
    {
      IPMICONSOLE_DEBUG (("libipmiconsole config file password invalid length"));
      return (0);
    }

  strcpy (default_config.password, data->string);

  IPMICONSOLE_DEBUG (("libipmiconsole loaded alternate default password"));
  return (0);
}

static int
_config_file_k_g (conffile_t cf,
                       struct conffile_data *data,
                       char *optionname,
                       int option_type,
                       void *option_ptr,
                       int option_data,
                       void *app_ptr,
                       int app_data)
{
  int rv;

  assert (data);

  if ((rv = parse_kg (default_config.k_g, IPMI_MAX_K_G_LENGTH + 1, data->string)) < 0)
    {
      IPMICONSOLE_DEBUG (("libipmiconsole config file k_g invalid format"));
      return (0);
    }

  if (rv > 0)
    default_config.k_g_len = rv;

  IPMICONSOLE_DEBUG (("libipmiconsole loaded alternate default k_g"));
  return (0);
}

static int
_config_file_privilege_level (conffile_t cf,
                              struct conffile_data *data,
                              char *optionname,
                              int option_type,
                              void *option_ptr,
                              int option_data,
                              void *app_ptr,
                              int app_data)
{
  int tmp;

  assert (data);

  if ((tmp = parse_privilege_level (data->string)) < 0)
    {
      IPMICONSOLE_DEBUG (("libipmiconsole config file privilege level invalid"));
      return (0);
    }

  default_config.privilege_level = tmp;

  IPMICONSOLE_DEBUG (("libipmiconsole loaded alternate default privilege level"));
  return (0);
}

static int
_config_file_cipher_suite_id (conffile_t cf,
                              struct conffile_data *data,
                              char *optionname,
                              int option_type,
                              void *option_ptr,
                              int option_data,
                              void *app_ptr,
                              int app_data)
{
  assert (data);

  if (data->intval < IPMI_CIPHER_SUITE_ID_MIN
      || data->intval > IPMI_CIPHER_SUITE_ID_MAX
      || !IPMI_CIPHER_SUITE_ID_SUPPORTED (data->intval))
    {
      IPMICONSOLE_DEBUG (("libipmiconsole config file cipher suite id invalid"));
      return (0);
    }

  default_config.cipher_suite_id = data->intval;

  IPMICONSOLE_DEBUG (("libipmiconsole loaded alternate default cipher suite id"));
  return (0);
}


static int
_config_file_workaround_flags (conffile_t cf,
                               struct conffile_data *data,
                               char *optionname,
                               int option_type,
                               void *option_ptr,
                               int option_data,
                               void *app_ptr,
                               int app_data)
{
  unsigned int workaround_flags = 0;
  unsigned int i;

  assert (data);

  for (i = 0; i < data->stringlist_len; i++)
    {
      unsigned int outofband_2_0_flags, section_flags;

      if (parse_workaround_flags (data->stringlist[i],
                                  NULL,
                                  &outofband_2_0_flags,
                                  NULL,
				  NULL,
                                  &section_flags) < 0)
        {
          IPMICONSOLE_DEBUG (("libipmiconsole config file workaround flag invalid"));
          return (0);
        }

      if (outofband_2_0_flags)
        {
          if (outofband_2_0_flags & IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_AUTHENTICATION_CAPABILITIES)
            workaround_flags |= IPMICONSOLE_WORKAROUND_AUTHENTICATION_CAPABILITIES;
          if (outofband_2_0_flags & IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_INTEL_2_0_SESSION)
            workaround_flags |= IPMICONSOLE_WORKAROUND_INTEL_2_0_SESSION;
          if (outofband_2_0_flags & IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_SUPERMICRO_2_0_SESSION)
            workaround_flags |= IPMICONSOLE_WORKAROUND_SUPERMICRO_2_0_SESSION;
          if (outofband_2_0_flags & IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_SUN_2_0_SESSION)
            workaround_flags |= IPMICONSOLE_WORKAROUND_SUN_2_0_SESSION;
          if (outofband_2_0_flags & IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_OPEN_SESSION_PRIVILEGE)
            workaround_flags |= IPMICONSOLE_WORKAROUND_OPEN_SESSION_PRIVILEGE;
          if (outofband_2_0_flags & IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_NON_EMPTY_INTEGRITY_CHECK_VALUE)
            workaround_flags |= IPMICONSOLE_WORKAROUND_NON_EMPTY_INTEGRITY_CHECK_VALUE;
          if (outofband_2_0_flags & IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_NO_CHECKSUM_CHECK)
            workaround_flags |= IPMICONSOLE_WORKAROUND_NO_CHECKSUM_CHECK;
        }

      if (section_flags)
        {
          if (section_flags & IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_IGNORE_SOL_PAYLOAD_SIZE)
            workaround_flags |= IPMICONSOLE_WORKAROUND_IGNORE_SOL_PAYLOAD_SIZE;
          if (section_flags & IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_IGNORE_SOL_PORT)
            workaround_flags |= IPMICONSOLE_WORKAROUND_IGNORE_SOL_PORT;
          if (section_flags & IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_SKIP_SOL_ACTIVATION_STATUS)
            workaround_flags |= IPMICONSOLE_WORKAROUND_SKIP_SOL_ACTIVATION_STATUS;
          if (section_flags & IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_SKIP_CHANNEL_PAYLOAD_SUPPORT)
            workaround_flags |= IPMICONSOLE_WORKAROUND_SKIP_CHANNEL_PAYLOAD_SUPPORT;
        }
    }

  default_config.workaround_flags = workaround_flags;

  IPMICONSOLE_DEBUG (("libipmiconsole loaded alternate default workaround flag"));
  return (0);
}

static int
_config_file_engine_flags (conffile_t cf,
                           struct conffile_data *data,
                           char *optionname,
                           int option_type,
                           void *option_ptr,
                           int option_data,
                           void *app_ptr,
                           int app_data)
{
  unsigned int engine_flags = 0;
  unsigned int i;

  assert (data);

  for (i = 0; i < data->stringlist_len; i++)
    {
      if (!strcasecmp (data->stringlist[i], IPMICONSOLE_ENGINE_CLOSE_FD_STR))
        engine_flags |= IPMICONSOLE_ENGINE_CLOSE_FD;
      else if (!strcasecmp (data->stringlist[i], IPMICONSOLE_ENGINE_OUTPUT_ON_SOL_ESTABLISHED_STR))
        engine_flags |= IPMICONSOLE_ENGINE_OUTPUT_ON_SOL_ESTABLISHED;
      else if (!strcasecmp (data->stringlist[i], IPMICONSOLE_ENGINE_LOCK_MEMORY_STR))
        engine_flags |= IPMICONSOLE_ENGINE_LOCK_MEMORY;
      else if (!strcasecmp (data->stringlist[i], IPMICONSOLE_ENGINE_SERIAL_KEEPALIVE_STR))
        engine_flags |= IPMICONSOLE_ENGINE_SERIAL_KEEPALIVE;
      else if (!strcasecmp (data->stringlist[i], IPMICONSOLE_ENGINE_SERIAL_KEEPALIVE_EMPTY_STR))
        engine_flags |= IPMICONSOLE_ENGINE_SERIAL_KEEPALIVE_EMPTY;
      else
        IPMICONSOLE_DEBUG (("libipmiconsole config file engine flag invalid"));
    }

  default_config.engine_flags = engine_flags;

  IPMICONSOLE_DEBUG (("libipmiconsole loaded alternate default engine flag"));
  return (0);
}

static int
_config_file_behavior_flags (conffile_t cf,
                           struct conffile_data *data,
                           char *optionname,
                           int option_type,
                           void *option_ptr,
                           int option_data,
                           void *app_ptr,
                           int app_data)
{
  unsigned int behavior_flags = 0;
  unsigned int i;

  assert (data);

  for (i = 0; i < data->stringlist_len; i++)
    {
      if (!strcasecmp (data->stringlist[i], IPMICONSOLE_BEHAVIOR_ERROR_ON_SOL_INUSE_STR))
        behavior_flags |= IPMICONSOLE_BEHAVIOR_ERROR_ON_SOL_INUSE;
      else if (!strcasecmp (data->stringlist[i], IPMICONSOLE_BEHAVIOR_DEACTIVATE_ONLY_STR))
        behavior_flags |= IPMICONSOLE_BEHAVIOR_DEACTIVATE_ONLY;
      else if (!strcasecmp (data->stringlist[i], IPMICONSOLE_BEHAVIOR_DEACTIVATE_ALL_INSTANCES_STR))
        behavior_flags |= IPMICONSOLE_BEHAVIOR_DEACTIVATE_ALL_INSTANCES;
      else
        IPMICONSOLE_DEBUG (("libipmiconsole config file behavior flag invalid"));
    }

  default_config.behavior_flags = behavior_flags;
  
  IPMICONSOLE_DEBUG (("libipmiconsole loaded alternate default behavior flag"));
  return (0);
}

static int
_config_file_debug_flags (conffile_t cf,
                           struct conffile_data *data,
                           char *optionname,
                           int option_type,
                           void *option_ptr,
                           int option_data,
                           void *app_ptr,
                           int app_data)
{
  unsigned int debug_flags = 0;
  unsigned int i;

  assert (data);

  for (i = 0; i < data->stringlist_len; i++)
    {
      if (!strcasecmp (data->stringlist[i], IPMICONSOLE_DEBUG_STDOUT_STR))
        debug_flags |= IPMICONSOLE_DEBUG_STDOUT;
      else if (!strcasecmp (data->stringlist[i], IPMICONSOLE_DEBUG_STDERR_STR))
        debug_flags |= IPMICONSOLE_DEBUG_STDERR;
      else if (!strcasecmp (data->stringlist[i], IPMICONSOLE_DEBUG_SYSLOG_STR))
        debug_flags |= IPMICONSOLE_DEBUG_SYSLOG;
      else if (!strcasecmp (data->stringlist[i], IPMICONSOLE_DEBUG_FILE_STR))
        debug_flags |= IPMICONSOLE_DEBUG_FILE;
      else if (!strcasecmp (data->stringlist[i], IPMICONSOLE_DEBUG_IPMI_PACKETS_STR))
        debug_flags |= IPMICONSOLE_DEBUG_IPMI_PACKETS;
      else
        IPMICONSOLE_DEBUG (("libipmiconsole config file debug flag invalid"));
    }

  default_config.debug_flags = debug_flags;

  IPMICONSOLE_DEBUG (("libipmiconsole loaded alternate default debug flag"));
  return (0);
}

static int
_config_file_sol_payload_instance (conffile_t cf,
				   struct conffile_data *data,
				   char *optionname,
				   int option_type,
				   void *option_ptr,
				   int option_data,
				   void *app_ptr,
				   int app_data)
{
  unsigned int *value;

  assert (data);
  assert (option_ptr);

  value = (unsigned int *)option_ptr;
  
  if (data->intval <= 0
      || !IPMI_PAYLOAD_INSTANCE_VALID (data->intval))
    {
      IPMICONSOLE_DEBUG (("libipmiconsole config file %s invalid", optionname));
      return (0);
    }

  *value = (unsigned int)data->intval;
  return (0);
}

static int
_ipmiconsole_defaults_setup (void)
{
  int libipmiconsole_context_username_count = 0, libipmiconsole_context_password_count = 0,
    libipmiconsole_context_k_g_count = 0, libipmiconsole_context_privilege_level_count = 0,
    libipmiconsole_context_cipher_suite_id_count = 0,
    libipmiconsole_context_workaround_flags_count = 0,
    libipmiconsole_context_session_timeout_len_count = 0,
    libipmiconsole_context_retransmission_timeout_len_count = 0,
    libipmiconsole_context_retransmission_backoff_count_count = 0,
    libipmiconsole_context_keepalive_timeout_len_count = 0,
    libipmiconsole_context_retransmission_keepalive_timeout_len_count = 0,
    libipmiconsole_context_acceptable_packet_errors_count_count = 0,
    libipmiconsole_context_maximum_retransmission_count_count = 0,
    libipmiconsole_context_engine_flags_count = 0,
    libipmiconsole_context_behavior_flags_count = 0,
    libipmiconsole_context_debug_flags_count = 0,
    libipmiconsole_context_sol_payload_instance_count = 0;

  struct conffile_option libipmiconsole_options[] =
    {
      {
        "libipmiconsole-context-username",
        CONFFILE_OPTION_STRING,
        -1,
        _config_file_username,
        1,
        0,
        &libipmiconsole_context_username_count,
        NULL,
        0,
      },
      {
        "libipmiconsole-context-password",
        CONFFILE_OPTION_STRING,
        -1,
        _config_file_password,
        1,
        0,
        &libipmiconsole_context_password_count,
        NULL,
        0,
      },
      {
        "libipmiconsole-context-k_g",
        CONFFILE_OPTION_STRING,
        -1,
        _config_file_k_g,
        1,
        0,
        &libipmiconsole_context_k_g_count,
        NULL,
        0,
      },
      {
        "libipmiconsole-context-privilege-level",
        CONFFILE_OPTION_STRING,
        -1,
        _config_file_privilege_level,
        1,
        0,
        &libipmiconsole_context_privilege_level_count,
        NULL,
        0,
      },
      {
        "libipmiconsole-context-cipher-suite-id",
        CONFFILE_OPTION_INT,
        -1,
        _config_file_cipher_suite_id,
        1,
        0,
        &libipmiconsole_context_cipher_suite_id_count,
        NULL,
        0,
      },
      {
        "libipmiconsole-context-workaround-flags",
        CONFFILE_OPTION_LIST_STRING,
        -1,
        _config_file_workaround_flags,
        1,
        0,
        &libipmiconsole_context_workaround_flags_count,
        NULL,
        0,
      },
      {
        "libipmiconsole-context-session-timeout-len",
        CONFFILE_OPTION_INT,
        -1,
        _config_file_unsigned_int_positive,
        1,
        0,
        &libipmiconsole_context_session_timeout_len_count,
        &(default_config.session_timeout_len),
        0
      },
      {
        "libipmiconsole-context-retransmission-timeout-len",
        CONFFILE_OPTION_INT,
        -1,
        _config_file_unsigned_int_positive,
        1,
        0,
        &libipmiconsole_context_retransmission_timeout_len_count,
        &(default_config.retransmission_timeout_len),
        0
      },
      {
        "libipmiconsole-context-retransmission-backoff-count",
        CONFFILE_OPTION_INT,
        -1,
        _config_file_unsigned_int_positive,
        1,
        0,
        &libipmiconsole_context_retransmission_backoff_count_count,
        &(default_config.retransmission_backoff_count),
        0
      },
      {
        "libipmiconsole-context-keepalive-timeout-len",
        CONFFILE_OPTION_INT,
        -1,
        _config_file_unsigned_int_positive,
        1,
        0,
        &libipmiconsole_context_keepalive_timeout_len_count,
        &(default_config.keepalive_timeout_len),
        0
      },
      {
        "libipmiconsole-context-retransmission-keepalive-timeout-len",
        CONFFILE_OPTION_INT,
        -1,
        _config_file_unsigned_int_positive,
        1,
        0,
        &libipmiconsole_context_retransmission_keepalive_timeout_len_count,
        &(default_config.retransmission_keepalive_timeout_len),
        0
      },
      {
        "libipmiconsole-context-acceptable-packet-errors-count",
        CONFFILE_OPTION_INT,
        -1,
        _config_file_unsigned_int_positive,
        1,
        0,
        &libipmiconsole_context_acceptable_packet_errors_count_count,
        &(default_config.acceptable_packet_errors_count),
        0
      },
      {
        "libipmiconsole-context-maximum-retransmission-count",
        CONFFILE_OPTION_INT,
        -1,
        _config_file_unsigned_int_positive,
        1,
        0,
        &libipmiconsole_context_maximum_retransmission_count_count,
        &(default_config.maximum_retransmission_count),
        0
      },
      {
        "libipmiconsole-context-engine-flags",
        CONFFILE_OPTION_LIST_STRING,
        -1,
        _config_file_engine_flags,
        1,
        0,
        &libipmiconsole_context_engine_flags_count,
        NULL,
        0,
      },
      {
        "libipmiconsole-context-behavior-flags",
        CONFFILE_OPTION_LIST_STRING,
        -1,
        _config_file_behavior_flags,
        1,
        0,
        &libipmiconsole_context_behavior_flags_count,
        NULL,
        0,
      },
      {
        "libipmiconsole-context-debug-flags",
        CONFFILE_OPTION_LIST_STRING,
        -1,
        _config_file_debug_flags,
        1,
        0,
        &libipmiconsole_context_debug_flags_count,
        NULL,
        0,
      },
      {
        "libipmiconsole-context-sol-payload-instance",
        CONFFILE_OPTION_INT,
        -1,
	_config_file_sol_payload_instance,
        1,
        0,
        &libipmiconsole_context_sol_payload_instance_count,
        &(default_config.sol_payload_instance),
        0
      },
    };

  conffile_t cf = NULL;
  int rv = -1;
  int libipmiconsole_options_len;

  memset (&default_config, '\0', sizeof (struct ipmiconsole_ctx_config));

  /* set base defaults */

  default_config.privilege_level = IPMI_PRIVILEGE_LEVEL_DEFAULT;
  default_config.cipher_suite_id = IPMI_CIPHER_SUITE_ID_DEFAULT;
  default_config.session_timeout_len = IPMICONSOLE_SESSION_TIMEOUT_LENGTH_DEFAULT;
  default_config.retransmission_timeout_len = IPMICONSOLE_RETRANSMISSION_TIMEOUT_LENGTH_DEFAULT;
  default_config.retransmission_backoff_count = IPMICONSOLE_RETRANSMISSION_BACKOFF_COUNT_DEFAULT;
  default_config.keepalive_timeout_len = IPMICONSOLE_KEEPALIVE_TIMEOUT_LENGTH_DEFAULT;
  default_config.retransmission_keepalive_timeout_len = IPMICONSOLE_RETRANSMISSION_KEEPALIVE_TIMEOUT_LENGTH_DEFAULT;
  default_config.acceptable_packet_errors_count = IPMICONSOLE_ACCEPTABLE_PACKET_ERRORS_COUNT_DEFAULT;
  default_config.maximum_retransmission_count = IPMICONSOLE_MAXIMUM_RETRANSMISSION_COUNT_DEFAULT;
  default_config.sol_payload_instance = IPMI_PAYLOAD_INSTANCE_DEFAULT;

  if (!(cf = conffile_handle_create ()))
    {
      IPMICONSOLE_DEBUG (("conffile_handle_create: %s", strerror (errno)));
      return (-1);
    }

  libipmiconsole_options_len = sizeof (libipmiconsole_options) / sizeof (struct conffile_option);
  if (conffile_parse (cf,
                      LIBIPMICONSOLE_CONFIG_FILE_DEFAULT,
                      libipmiconsole_options,
                      libipmiconsole_options_len,
                      NULL,
                      0,
                      0) < 0)
    {
      char buf[CONFFILE_MAX_ERRMSGLEN];
      
      /* Its not an error if the default configuration file doesn't exist */
      if (conffile_errnum (cf) == CONFFILE_ERR_EXIST)
        goto out;
      
      if (conffile_errmsg (cf, buf, CONFFILE_MAX_ERRMSGLEN) < 0)
        {
          IPMICONSOLE_DEBUG (("libipmiconsole loaded alternate default debug flag"));
          goto cleanup;
        }
    }
  
 out:
  rv = 0;
 cleanup:
  conffile_handle_destroy (cf);
  return (rv);
}

int
ipmiconsole_engine_init (unsigned int thread_count, unsigned int debug_flags)
{
  struct rlimit rlim;
  unsigned int i;

  if (thread_count > IPMICONSOLE_THREAD_COUNT_MAX
      || (debug_flags != IPMICONSOLE_DEBUG_DEFAULT
	  && debug_flags & ~IPMICONSOLE_DEBUG_MASK))
    {
      errno = EINVAL;
      return (-1);
    }

  if (!thread_count)
    thread_count = IPMICONSOLE_THREAD_COUNT_DEFAULT;

  /* Note: Must be called first before anything else for debugging purposes */
  if (ipmiconsole_debug_setup (debug_flags) < 0)
    goto cleanup;

  if (ipmiconsole_engine_is_setup ())
    return (0);

  if (ipmiconsole_engine_setup (thread_count) < 0)
    goto cleanup;

  for (i = 0; i < thread_count; i++)
    {
      if (ipmiconsole_engine_thread_create () < 0)
        goto cleanup;
    }

  /* If the file descriptor increase fails, ignore it */

  if (getrlimit (RLIMIT_NOFILE, &rlim) == 0)
    {
      rlim.rlim_cur = rlim.rlim_max;
      setrlimit (RLIMIT_NOFILE, &rlim);
    }

  if (_ipmiconsole_defaults_setup () < 0)
    goto cleanup;

  return (0);

 cleanup:
  ipmiconsole_debug_cleanup ();
  ipmiconsole_engine_cleanup (0);
  return (-1);
}

int
ipmiconsole_engine_submit (ipmiconsole_ctx_t c,
                           Ipmiconsole_callback callback,
                           void *callback_arg)
{
  int perr;

  if (!c
      || c->magic != IPMICONSOLE_CTX_MAGIC
      || c->api_magic != IPMICONSOLE_CTX_API_MAGIC)
    return (-1);

  if (!ipmiconsole_engine_is_setup ())
    {
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_NOT_SETUP);
      return (-1);
    }

  if (c->session_submitted)
    {
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_CTX_IS_SUBMITTED);
      return (-1);
    }

  /* Set to success, so we know if an IPMI error occurred later */
  ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_SUCCESS);

  if (ipmiconsole_ctx_non_blocking_setup (c,
                                          callback,
                                          callback_arg) < 0)
    goto cleanup;

  if (ipmiconsole_ctx_connection_setup (c) < 0)
    goto cleanup;

  if (ipmiconsole_ctx_session_setup (c) < 0)
    goto cleanup;

  if (ipmiconsole_engine_submit_ctx (c) < 0)
    goto cleanup;

  if ((perr = pthread_mutex_lock (&(c->signal.status_mutex))) != 0)
    {
      IPMICONSOLE_DEBUG (("pthread_mutex_lock: %s", strerror (perr)));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      /* don't go to cleanup, b/c the engine will call
       * ipmiconsole_ctx_connection_cleanup_session_not_submitted().
       */
      goto cleanup_ctx_fds_only;
    }

  /* Check for NOT_SUBMITTED, conceivably SOL_ERROR or SOL_ESTABLISHED
   * could already be set
   */
  if (c->signal.status == IPMICONSOLE_CTX_STATUS_NOT_SUBMITTED)
    c->signal.status = IPMICONSOLE_CTX_STATUS_SUBMITTED;

  if ((perr = pthread_mutex_unlock (&(c->signal.status_mutex))) != 0)
    {
      IPMICONSOLE_DEBUG (("pthread_mutex_unlock: %s", strerror (perr)));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      /* don't go to cleanup, b/c the engine will call
       * ipmiconsole_ctx_connection_cleanup_session_not_submitted().
       */
      goto cleanup_ctx_fds_only;
    }

  /* may have been set already */
  c->session_submitted++;
  ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_SUCCESS);
  return (0);

 cleanup:
  ipmiconsole_ctx_connection_cleanup_session_not_submitted (c);
 cleanup_ctx_fds_only:
  /* fds are the API responsibility, even though we didn't create them */
  ipmiconsole_ctx_fds_cleanup (c);
  return (-1);
}

static int
_ipmiconsole_blocking_notification_cleanup (ipmiconsole_ctx_t c)
{
  int perr;

  assert (c);
  assert (c->magic == IPMICONSOLE_CTX_MAGIC);
  assert (c->api_magic == IPMICONSOLE_CTX_API_MAGIC);

  if ((perr = pthread_mutex_lock (&(c->blocking.blocking_mutex))) != 0)
    {
      IPMICONSOLE_DEBUG (("pthread_mutex_lock: %s", strerror (perr)));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return (-1);
    }

  if (c->blocking.blocking_submit_requested)
    {
      /* ignore potential error, cleanup path */
      close (c->blocking.blocking_notification[0]);
      /* ignore potential error, cleanup path */
      close (c->blocking.blocking_notification[1]);
      c->blocking.blocking_notification[0] = -1;
      c->blocking.blocking_notification[1] = -1;
      c->blocking.blocking_submit_requested = 0;
      c->blocking.sol_session_established = 0;
    }

  if ((perr = pthread_mutex_unlock (&(c->blocking.blocking_mutex))) != 0)
    {
      IPMICONSOLE_DEBUG (("pthread_mutex_unlock: %s", strerror (perr)));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return (-1);
    }

  return (0);
}

static int
_ipmiconsole_blocking_notification_setup (ipmiconsole_ctx_t c)
{
  assert (c);
  assert (c->magic == IPMICONSOLE_CTX_MAGIC);
  assert (c->api_magic == IPMICONSOLE_CTX_API_MAGIC);

  /* We're setting up, so no mutex locking needed at this point */

  if (pipe (c->blocking.blocking_notification) < 0)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("pipe: %s", strerror (errno)));
      if (errno == EMFILE)
        ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_TOO_MANY_OPEN_FILES);
      else
        ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_SYSTEM_ERROR);
      goto cleanup;
    }
  c->blocking.blocking_submit_requested++;
  c->blocking.sol_session_established = 0;

  if (ipmiconsole_set_closeonexec (c, c->blocking.blocking_notification[0]) < 0)
    {
      IPMICONSOLE_DEBUG (("closeonexec error"));
      goto cleanup;
    }
  if (ipmiconsole_set_closeonexec (c, c->blocking.blocking_notification[1]) < 0)
    {
      IPMICONSOLE_DEBUG (("closeonexec error"));
      goto cleanup;
    }

  return (0);

 cleanup:
  _ipmiconsole_blocking_notification_cleanup (c);
  return (-1);
}

static int
_ipmiconsole_block (ipmiconsole_ctx_t c)
{
  fd_set rds;
  int n;

  assert (c);
  assert (c->magic == IPMICONSOLE_CTX_MAGIC);
  assert (c->api_magic == IPMICONSOLE_CTX_API_MAGIC);
  assert (c->blocking.blocking_submit_requested);

  FD_ZERO (&rds);
  FD_SET (c->blocking.blocking_notification[0], &rds);

  /* No mutex required here, just reading off the pipe, the pipe is
   * all controled in API land
   */

  if ((n = select (c->blocking.blocking_notification[0] + 1, &rds, NULL, NULL, NULL)) < 0)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("select: %s", strerror (errno)));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_SYSTEM_ERROR);
      goto cleanup;
    }

  if (!n)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("select returned 0"));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      goto cleanup;
    }

  if (FD_ISSET (c->blocking.blocking_notification[0], &rds))
    {
      uint8_t tmpbyte;
      ssize_t len;

      if ((len = read (c->blocking.blocking_notification[0], (void *)&tmpbyte, 1)) < 0)
        {
          IPMICONSOLE_CTX_DEBUG (c, ("read: %s", strerror (errno)));
          ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_SYSTEM_ERROR);
          goto cleanup;
        }

      if (!len)
        {
          IPMICONSOLE_CTX_DEBUG (c, ("blocking_notification closed"));
          ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
          goto cleanup;
        }

      if (tmpbyte == IPMICONSOLE_BLOCKING_NOTIFICATION_SOL_SESSION_ESTABLISHED)
        goto success;
      else if (tmpbyte == IPMICONSOLE_BLOCKING_NOTIFICATION_SOL_SESSION_ERROR)
        goto cleanup;
      else if (c->config.behavior_flags & IPMICONSOLE_BEHAVIOR_DEACTIVATE_ONLY
               && tmpbyte == IPMICONSOLE_BLOCKING_NOTIFICATION_SOL_SESSION_DEACTIVATED)
        goto success;
      else
        {
          IPMICONSOLE_CTX_DEBUG (c, ("blocking_notification returned invalid data: %u", tmpbyte));
          ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
          goto cleanup;
        }
    }

 success:
  return (0);

 cleanup:
  return (-1);
}

int
ipmiconsole_engine_submit_block (ipmiconsole_ctx_t c)
{
  int perr;

  if (!c
      || c->magic != IPMICONSOLE_CTX_MAGIC
      || c->api_magic != IPMICONSOLE_CTX_API_MAGIC)
    return (-1);

  if (!ipmiconsole_engine_is_setup ())
    {
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_NOT_SETUP);
      return (-1);
    }

  if (c->session_submitted)
    {
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_CTX_IS_SUBMITTED);
      return (-1);
    }

  /* Set to success, so we know if an IPMI error occurred later */
  ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_SUCCESS);

  if (ipmiconsole_ctx_connection_setup (c) < 0)
    goto cleanup;

  if (ipmiconsole_ctx_session_setup (c) < 0)
    goto cleanup;

  if (_ipmiconsole_blocking_notification_setup (c) < 0)
    goto cleanup;

  if (ipmiconsole_engine_submit_ctx (c) < 0)
    goto cleanup;

  if (_ipmiconsole_block (c) < 0)
    {
      /* don't go to cleanup, b/c the engine will call
       * ipmiconsole_ctx_connection_cleanup_session_not_submitted().
       */
      goto cleanup_ctx_fds_only;
    }

  if ((perr = pthread_mutex_lock (&(c->signal.status_mutex))) != 0)
    {
      IPMICONSOLE_DEBUG (("pthread_mutex_lock: %s", strerror (perr)));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      /* don't go to cleanup, b/c the engine will call
       * ipmiconsole_ctx_connection_cleanup_session_not_submitted().
       */
      goto cleanup_ctx_fds_only;
    }

  /* Check for NOT_SUBMITTED, conceivably SOL_ERROR or SOL_ESTABLISHED
   * could already be set
   */
  if (c->signal.status == IPMICONSOLE_CTX_STATUS_NOT_SUBMITTED)
    c->signal.status = IPMICONSOLE_CTX_STATUS_SUBMITTED;

  if ((perr = pthread_mutex_unlock (&(c->signal.status_mutex))) != 0)
    {
      IPMICONSOLE_DEBUG (("pthread_mutex_unlock: %s", strerror (perr)));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      /* don't go to cleanup, b/c the engine will call
       * ipmiconsole_ctx_connection_cleanup_session_not_submitted().
       */
      goto cleanup_ctx_fds_only;
    }

  _ipmiconsole_blocking_notification_cleanup (c);

  /* may have been set already */
  c->session_submitted++;
  ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_SUCCESS);
  return (0);

 cleanup:
  ipmiconsole_ctx_connection_cleanup_session_not_submitted (c);
 cleanup_ctx_fds_only:
  _ipmiconsole_blocking_notification_cleanup (c);
  /* fds are the API responsibility, even though we didn't create them */
  ipmiconsole_ctx_fds_cleanup (c);
  return (-1);
}

void
ipmiconsole_engine_teardown (int cleanup_sol_sessions)
{
  ipmiconsole_debug_cleanup ();
  ipmiconsole_engine_cleanup (cleanup_sol_sessions);
}

ipmiconsole_ctx_t
ipmiconsole_ctx_create (const char *hostname,
                        struct ipmiconsole_ipmi_config *ipmi_config,
                        struct ipmiconsole_protocol_config *protocol_config,
                        struct ipmiconsole_engine_config *engine_config)
{
  ipmiconsole_ctx_t c = NULL;
  char hostnamebuf[MAXHOSTNAMELEN_WITH_PORT + 1];
  char *hostname_ptr;
  uint16_t port = RMCP_PRIMARY_RMCP_PORT;

  if (!hostname
      || !ipmi_config
      || !protocol_config
      || !engine_config
      || (ipmi_config->username && strlen (ipmi_config->username) > IPMI_MAX_USER_NAME_LENGTH)
      || (ipmi_config->password && strlen (ipmi_config->password) > IPMI_2_0_MAX_PASSWORD_LENGTH)
      || (ipmi_config->k_g && ipmi_config->k_g_len > IPMI_MAX_K_G_LENGTH)
      || (ipmi_config->privilege_level >= 0
          && (ipmi_config->privilege_level != IPMICONSOLE_PRIVILEGE_USER
              && ipmi_config->privilege_level != IPMICONSOLE_PRIVILEGE_OPERATOR
              && ipmi_config->privilege_level != IPMICONSOLE_PRIVILEGE_ADMIN))
      || (ipmi_config->cipher_suite_id >= IPMI_CIPHER_SUITE_ID_MIN
          && !IPMI_CIPHER_SUITE_ID_SUPPORTED (ipmi_config->cipher_suite_id))
      || (ipmi_config->workaround_flags != IPMICONSOLE_WORKAROUND_DEFAULT
	  && ipmi_config->workaround_flags & ~IPMICONSOLE_WORKAROUND_MASK)
      || (engine_config->engine_flags != IPMICONSOLE_ENGINE_DEFAULT
	  && engine_config->engine_flags & ~IPMICONSOLE_ENGINE_MASK)
      || (engine_config->behavior_flags != IPMICONSOLE_BEHAVIOR_DEFAULT
	  && engine_config->behavior_flags & ~IPMICONSOLE_BEHAVIOR_MASK)
      || (engine_config->debug_flags != IPMICONSOLE_DEBUG_DEFAULT
	  && engine_config->debug_flags & ~IPMICONSOLE_DEBUG_MASK))
    {
      IPMICONSOLE_DEBUG (("invalid input parameters"));
      errno = EINVAL;
      return (NULL);
    }

  if (strchr (hostname, ':'))
    {
      char *ptr;

      if (strlen (hostname) > MAXHOSTNAMELEN_WITH_PORT)
	{
	  IPMICONSOLE_DEBUG (("invalid input parameters"));
	  errno = EINVAL;
	  return (NULL);
	} 
      
      memset (hostnamebuf, '\0', MAXHOSTNAMELEN_WITH_PORT + 1);
      strcpy (hostnamebuf, hostname);
      
      if ((ptr = strchr (hostnamebuf, ':')))
	{
	  char *endptr;
          int tmp;
	  
	  *ptr = '\0';
	  ptr++;
	  
	  if (strlen (hostnamebuf) > MAXHOSTNAMELEN)
	    {
	      IPMICONSOLE_DEBUG (("invalid input parameters"));
	      errno = EINVAL;
	      return (NULL);
	    } 
	  
	  errno = 0;
	  tmp = strtol (ptr, &endptr, 0);
	  if (errno
              || endptr[0] != '\0'
              || tmp <= 0
	      || tmp > USHRT_MAX)
	    {
	      IPMICONSOLE_DEBUG (("invalid input parameters"));
	      errno = EINVAL;
	      return (NULL);
	    }
	  
	  port = tmp;
	}

      hostname_ptr = hostnamebuf;
    }
  else
    {
      if (strlen (hostname) > MAXHOSTNAMELEN)
	{
	  IPMICONSOLE_DEBUG (("invalid input parameters"));
	  errno = EINVAL;
	  return (NULL);
	}

      hostname_ptr = (char *)hostname;
    }

  /* If engine is not setup, the default_config is not yet known */
  if (!ipmiconsole_engine_is_setup ())
    {
      IPMICONSOLE_DEBUG (("engine not initialized"));
      errno = EAGAIN;
      return (NULL);
    }

  if ((engine_config->engine_flags != IPMICONSOLE_ENGINE_DEFAULT
       && engine_config->engine_flags & IPMICONSOLE_ENGINE_LOCK_MEMORY)
      || default_config.engine_flags & IPMICONSOLE_ENGINE_LOCK_MEMORY)
    {
      if (!(c = (ipmiconsole_ctx_t)secure_malloc (sizeof (struct ipmiconsole_ctx))))
        {
          errno = ENOMEM;
          return (NULL);
        }
    }
  else
    {
      if (!(c = (ipmiconsole_ctx_t)malloc (sizeof (struct ipmiconsole_ctx))))
        {
          errno = ENOMEM;
          return (NULL);
        }
    }

  /* XXX: Should move much of this to engine_submit, to make context
   * creation faster for console concentrators
   */

  if (ipmiconsole_ctx_setup (c) < 0)
    goto cleanup;

  if (ipmiconsole_ctx_config_setup (c,
                                    hostname_ptr,
				    port,
                                    ipmi_config,
                                    protocol_config,
                                    engine_config) < 0)
    goto cleanup;

  /* must be called after ipmiconsole_ctx_config_setup() */
  if (ipmiconsole_ctx_debug_setup (c) < 0)
    goto cleanup;

  if (ipmiconsole_ctx_signal_setup (c) < 0)
    goto cleanup;

  if (ipmiconsole_ctx_blocking_setup (c) < 0)
    goto cleanup;

  /* only initializes value, no need to destroy/cleanup anything in here */
  ipmiconsole_ctx_fds_setup (c);

  c->session_submitted = 0;

  ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_SUCCESS);
  return (c);

 cleanup:

  ipmiconsole_ctx_config_cleanup (c);

  ipmiconsole_ctx_debug_cleanup (c);

  ipmiconsole_ctx_signal_cleanup (c);

  ipmiconsole_ctx_blocking_cleanup (c);

  /* Note: use engine_config->engine_flags not c->config.engine_flags,
   * b/c we don't know where we failed earlier.
   */
  if ((engine_config->engine_flags != IPMICONSOLE_ENGINE_DEFAULT
       && engine_config->engine_flags & IPMICONSOLE_ENGINE_LOCK_MEMORY)
      || default_config.engine_flags & IPMICONSOLE_ENGINE_LOCK_MEMORY)
    secure_free (c, sizeof (struct ipmiconsole_ctx));
  else
    free (c);
  return (NULL);
}

int
ipmiconsole_ctx_set_config (ipmiconsole_ctx_t c,
			    ipmiconsole_ctx_config_option_t config_option,
			    void *config_option_value)
{
  unsigned int *tmpptr;

  if (!c
      || c->magic != IPMICONSOLE_CTX_MAGIC
      || c->api_magic != IPMICONSOLE_CTX_API_MAGIC)
    return (-1);

  if ((config_option != IPMICONSOLE_CTX_CONFIG_OPTION_SOL_PAYLOAD_INSTANCE)
      || !config_option_value)
    {
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_PARAMETERS);
      return (-1);
    }

  if (c->session_submitted)
    {
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_CTX_IS_SUBMITTED);
      return (-1);
    }

  switch (config_option)
    {
    case IPMICONSOLE_CTX_CONFIG_OPTION_SOL_PAYLOAD_INSTANCE:
      tmpptr = (unsigned int *)config_option_value;
      if (!IPMI_PAYLOAD_INSTANCE_VALID((*tmpptr)))
	{
	  ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_PARAMETERS);
	  return (-1);
	}
      c->config.sol_payload_instance = *(tmpptr);
      break;
    default:
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return (-1);
    }

  ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_SUCCESS);
  return (0);
}

int
ipmiconsole_ctx_get_config (ipmiconsole_ctx_t c,
			    ipmiconsole_ctx_config_option_t config_option,
			    void *config_option_value)
{
  unsigned int *tmpptr;

  if (!c
      || c->magic != IPMICONSOLE_CTX_MAGIC
      || c->api_magic != IPMICONSOLE_CTX_API_MAGIC)
    return (-1);

  if ((config_option != IPMICONSOLE_CTX_CONFIG_OPTION_SOL_PAYLOAD_INSTANCE)
      || !config_option_value)
    {
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_PARAMETERS);
      return (-1);
    }

  switch (config_option)
    {
    case IPMICONSOLE_CTX_CONFIG_OPTION_SOL_PAYLOAD_INSTANCE:
      tmpptr = (unsigned int *)config_option_value;
      (*tmpptr) = c->config.sol_payload_instance;
      break;
    default:
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return (-1);
    }

  ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_SUCCESS);
  return (0);
}

int
ipmiconsole_ctx_errnum (ipmiconsole_ctx_t c)
{
  if (!c)
    return (IPMICONSOLE_ERR_CTX_NULL);
  else if (c->magic != IPMICONSOLE_CTX_MAGIC
           || c->api_magic != IPMICONSOLE_CTX_API_MAGIC)
    return (IPMICONSOLE_ERR_CTX_INVALID);
  else
    return (ipmiconsole_ctx_get_errnum (c));
}

char *
ipmiconsole_ctx_strerror (int errnum)
{
  if (errnum >= IPMICONSOLE_ERR_SUCCESS && errnum <= IPMICONSOLE_ERR_ERRNUMRANGE)
    return (ipmiconsole_errmsgs[errnum]);
  else
    return (ipmiconsole_errmsgs[IPMICONSOLE_ERR_ERRNUMRANGE]);
}

char *
ipmiconsole_ctx_errormsg (ipmiconsole_ctx_t c)
{
  return (ipmiconsole_ctx_strerror (ipmiconsole_ctx_errnum (c)));
}

ipmiconsole_ctx_status_t
ipmiconsole_ctx_status (ipmiconsole_ctx_t c)
{
  int status;
  int perr;

  if (!c
      || c->magic != IPMICONSOLE_CTX_MAGIC
      || c->api_magic != IPMICONSOLE_CTX_API_MAGIC)
    return (IPMICONSOLE_CTX_STATUS_ERROR);

  /* Do not check if the context is submitted, b/c it may not be.
   *
   * Also, do not set errnum == success for this function, it could be
   * returning IPMICONSOLE_CTX_STATUS_ERROR.
   */

  if ((perr = pthread_mutex_lock (&(c->signal.status_mutex))) != 0)
    {
      IPMICONSOLE_DEBUG (("pthread_mutex_lock: %s", strerror (perr)));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return (IPMICONSOLE_CTX_STATUS_ERROR);
    }

  status = c->signal.status;

  if ((perr = pthread_mutex_unlock (&(c->signal.status_mutex))) != 0)
    {
      IPMICONSOLE_DEBUG (("pthread_mutex_unlock: %s", strerror (perr)));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return (IPMICONSOLE_CTX_STATUS_ERROR);
    }

  return (status);
}

int
ipmiconsole_ctx_fd (ipmiconsole_ctx_t c)
{
  if (!c
      || c->magic != IPMICONSOLE_CTX_MAGIC
      || c->api_magic != IPMICONSOLE_CTX_API_MAGIC)
    return (-1);

  if (!c->session_submitted)
    {
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_CTX_NOT_SUBMITTED);
      return (-1);
    }

  ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_SUCCESS);
  return (c->fds.user_fd);
}

int
ipmiconsole_ctx_generate_break (ipmiconsole_ctx_t c)
{
  uint8_t tmpbyte;

  if (!c
      || c->magic != IPMICONSOLE_CTX_MAGIC
      || c->api_magic != IPMICONSOLE_CTX_API_MAGIC)
    return (-1);

  if (!c->session_submitted)
    {
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_CTX_NOT_SUBMITTED);
      return (-1);
    }

  tmpbyte = IPMICONSOLE_PIPE_GENERATE_BREAK_CODE;
  if (write (c->fds.asynccomm[1], &tmpbyte, 1) < 0)
    {
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_SYSTEM_ERROR);
      return (-1);
    }

  ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_SUCCESS);
  return (0);
}

void
ipmiconsole_ctx_destroy (ipmiconsole_ctx_t c)
{
  if (!c
      || c->magic != IPMICONSOLE_CTX_MAGIC
      || c->api_magic != IPMICONSOLE_CTX_API_MAGIC)
    return;

  if (c->session_submitted)
    {
      int perr;

      /* achu: fds cleanup will not race with engine polling, b/c the
       * garbage collection will not complete until the flags set
       * below are set.
       */
      ipmiconsole_ctx_fds_cleanup (c);

      if ((perr = pthread_mutex_lock (&(c->signal.destroyed_mutex))) != 0)
        IPMICONSOLE_DEBUG (("pthread_mutex_lock: %s", strerror (perr)));

      if (!c->signal.user_has_destroyed)
        c->signal.user_has_destroyed++;

      /* must change magic in this mutex, to avoid racing
       * to destroy the context.
       */
      c->api_magic = ~IPMICONSOLE_CTX_API_MAGIC;

      if ((perr = pthread_mutex_unlock (&(c->signal.destroyed_mutex))) != 0)
        IPMICONSOLE_DEBUG (("pthread_mutex_unlock: %s", strerror (perr)));

      return;
    }

  /* else session never submitted, so we have to cleanup */
  c->api_magic = ~IPMICONSOLE_CTX_API_MAGIC;
  ipmiconsole_ctx_config_cleanup (c);
  ipmiconsole_ctx_debug_cleanup (c);
  ipmiconsole_ctx_signal_cleanup (c);
  ipmiconsole_ctx_blocking_cleanup (c);
  ipmiconsole_ctx_cleanup (c);
}

int
ipmiconsole_username_is_valid (const char *username)
{
  if (!username)
    return (0);

  if (strlen (username) > IPMI_MAX_USER_NAME_LENGTH)
    return (0);

  return (1);
}

int
ipmiconsole_password_is_valid (const char *password)
{
  if (!password)
    return (0);

  if (strlen (password) > IPMI_2_0_MAX_PASSWORD_LENGTH)
    return (0);

  return (1);
}

int
ipmiconsole_k_g_is_valid (const unsigned char *k_g, unsigned int k_g_len)
{
  if (!k_g)
    return (0);

  if (k_g_len > IPMI_MAX_K_G_LENGTH)
    return (0);

  return (1);
}

int
ipmiconsole_privilege_level_is_valid (int privilege_level)
{
  if (privilege_level != IPMICONSOLE_PRIVILEGE_USER
      && privilege_level != IPMICONSOLE_PRIVILEGE_OPERATOR
      && privilege_level != IPMICONSOLE_PRIVILEGE_ADMIN)
    return (0);

  return (1);
}

int
ipmiconsole_cipher_suite_id_is_valid (int cipher_suite_id)
{
  if (!IPMI_CIPHER_SUITE_ID_SUPPORTED (cipher_suite_id))
    return (0);

  return (1);
}

int
ipmiconsole_workaround_flags_is_valid (unsigned int workaround_flags)
{
  if (workaround_flags & ~IPMICONSOLE_WORKAROUND_MASK)
    return (0);

  return (1);
}
