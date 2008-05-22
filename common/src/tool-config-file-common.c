/*
  Copyright (C) 2003-2008 FreeIPMI Core Team

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
#endif
#include <argp.h>
#include <errno.h>
#include <assert.h>

#include "tool-config-file-common.h"
#include "tool-common.h"
#include "pstdout.h"

#include "freeipmi/api/ipmi-api.h"
#include "freeipmi/cmds/ipmi-messaging-support-cmds.h"
#include "freeipmi/interface/ipmi-rmcpplus-interface.h"
#include "freeipmi/util/ipmi-cipher-suite-util.h"

#define CONFIG_FILE_OPTIONS_MAX 1024

int 
config_file_bool(conffile_t cf,
                 struct conffile_data *data,
                 char *optionname,
                 int option_type,
                 void *option_ptr,
                 int option_data,
                 void *app_ptr,
                 int app_data)
{
  int *bool;

  assert(option_ptr);

  bool = (int *)option_ptr;
  *bool = data->boolval;
  return 0;
}

int 
config_file_non_negative_int(conffile_t cf,
                             struct conffile_data *data,
                             char *optionname,
                             int option_type,
                             void *option_ptr,
                             int option_data,
                             void *app_ptr,
                             int app_data)
{
  int *value;

  assert(option_ptr);

  value = (int *)option_ptr;

  if (data->intval < 0)
    {
      fprintf(stderr, "Config File Error: invalid value for %s\n", optionname);
      exit(1);
    }
    
  *value = data->intval;
  return 0;
}

int 
config_file_positive_int(conffile_t cf,
                         struct conffile_data *data,
                         char *optionname,
                         int option_type,
                         void *option_ptr,
                         int option_data,
                         void *app_ptr,
                         int app_data)
{
  int *value;

  assert(option_ptr);

  value = (int *)option_ptr;

  if (data->intval <= 0)
    {
      fprintf(stderr, "Config File Error: invalid value for %s\n", optionname);
      exit(1);
    }
    
  *value = data->intval;
  return 0;
}

int 
config_file_string(conffile_t cf,
                   struct conffile_data *data,
                   char *optionname,
                   int option_type,
                   void *option_ptr,
                   int option_data,
                   void *app_ptr,
                   int app_data)
{
  char **value;

  assert(option_ptr);

  value = (char **)option_ptr;

  if (!(*value = strdup(data->string)))
    {
      perror("strdup");
      exit(1);
    }

  return 0;
}

int 
config_file_string_with_length_check(conffile_t cf,
                                     struct conffile_data *data,
                                     char *optionname,
                                     int option_type,
                                     void *option_ptr,
                                     int option_data,
                                     void *app_ptr,
                                     int app_data)
{
  char **value;

  assert(option_ptr);

  value = (char **)option_ptr;

  if (strlen(data->string) > option_data)
    {
      fprintf (stderr, "Config File Error: %s value too long\n", optionname);
      exit(1);
    }

  if (!(*value = strdup(data->string)))
    {
      perror("strdup");
      exit(1);
    }

  return 0;
}

int 
config_file_driver_type(conffile_t cf,
                        struct conffile_data *data,
                        char *optionname,
                        int option_type,
                        void *option_ptr,
                        int option_data,
                        void *app_ptr,
                        int app_data)
{
  struct common_cmd_args *cmd_args;
  int tmp;

  assert(option_ptr);

  cmd_args = (struct common_cmd_args *)option_ptr;

  if ((tmp = parse_driver_type(data->string)) < 0)
    {
      fprintf(stderr, "Config File Error: invalid value for %s\n", optionname);
      exit(1);
    }

  /* don't change default if we want outofband configuration only */
  if (cmd_args->driver_type_outofband_only
      && (tmp == IPMI_DEVICE_LAN
          || tmp == IPMI_DEVICE_LAN_2_0))
    cmd_args->driver_type = tmp;
  else
    cmd_args->driver_type = tmp;

  return 0;
}

int 
config_file_workaround_flags(conffile_t cf,
                             struct conffile_data *data,
                             char *optionname,
                             int option_type,
                             void *option_ptr,
                             int option_data,
                             void *app_ptr,
                             int app_data)
{
  struct common_cmd_args *cmd_args;
  int i, tmp;

  assert(option_ptr);

  cmd_args = (struct common_cmd_args *)option_ptr;

  cmd_args->workaround_flags = 0;
  
  for (i = 0; i < data->stringlist_len; i++)
    {
      if ((tmp = parse_workaround_flags(data->stringlist[i])) < 0)
        {
          fprintf(stderr, "Config File Error: invalid value for %s\n", optionname);
          exit(1);
        }
      cmd_args->workaround_flags |= tmp;
    }
  return 0;
}

int 
config_file_k_g(conffile_t cf,
                struct conffile_data *data,
                char *optionname,
                int option_type,
                void *option_ptr,
                int option_data,
                void *app_ptr,
                int app_data)
{
  struct common_cmd_args *cmd_args;
  int rv;

  assert(option_ptr);

  cmd_args = (struct common_cmd_args *)option_ptr;

  if ((rv = parse_kg(cmd_args->k_g, IPMI_MAX_K_G_LENGTH + 1, data->string)) < 0)
    {
      fprintf(stderr, "Config File Error: k_g input formatted incorrectly\n");
      exit(1);
    }

  if (rv > 0)
    cmd_args->k_g_len = rv;

  return 0;
}

int 
config_file_authentication_type(conffile_t cf,
                                struct conffile_data *data,
                                char *optionname,
                                int option_type,
                                void *option_ptr,
                                int option_data,
                                void *app_ptr,
                                int app_data)
{
  struct common_cmd_args *cmd_args;
  int tmp;

  assert(option_ptr);

  cmd_args = (struct common_cmd_args *)option_ptr;

  if ((tmp = parse_authentication_type(data->string)) < 0)
    {
      fprintf(stderr, "Config File Error: invalid value for %s\n", optionname);
      exit(1);
    }

  cmd_args->authentication_type = tmp;
  return 0;
}

int 
config_file_cipher_suite_id(conffile_t cf,
                            struct conffile_data *data,
                            char *optionname,
                            int option_type,
                            void *option_ptr,
                            int option_data,
                            void *app_ptr,
                            int app_data)
{
  struct common_cmd_args *cmd_args;

  assert(option_ptr);

  cmd_args = (struct common_cmd_args *)option_ptr;

  if (data->intval < IPMI_CIPHER_SUITE_ID_MIN
      || data->intval > IPMI_CIPHER_SUITE_ID_MAX
      || !IPMI_CIPHER_SUITE_ID_SUPPORTED(data->intval))
    {
      fprintf(stderr, "Config File Error: invalid value for %s\n", optionname);
      exit(1);
    }

  cmd_args->cipher_suite_id = data->intval;
  return 0;
}

int 
config_file_privilege_level(conffile_t cf,
                            struct conffile_data *data,
                            char *optionname,
                            int option_type,
                            void *option_ptr,
                            int option_data,
                            void *app_ptr,
                            int app_data)
{
  struct common_cmd_args *cmd_args;
  int tmp;

  assert(option_ptr);

  cmd_args = (struct common_cmd_args *)option_ptr;

  if ((tmp = parse_privilege_level(data->string)) < 0)
    {
      fprintf(stderr, "Config File Error: invalid value for %s\n", optionname);
      exit(1);
    }

  cmd_args->privilege_level = tmp;
  return 0;
}

int 
config_file_fanout(conffile_t cf,
                   struct conffile_data *data,
                   char *optionname,
                   int option_type,
                   void *option_ptr,
                   int option_data,
                   void *app_ptr,
                   int app_data)
{
  struct hostrange_cmd_args *hostrange_args;
  
  assert(option_ptr);
  
  hostrange_args = (struct hostrange_cmd_args *)option_ptr;
  
  if (data->intval < PSTDOUT_FANOUT_MIN
      || data->intval > PSTDOUT_FANOUT_MAX)
    {
      fprintf(stderr, "Config File Error: invalid value for %s\n", optionname);
      exit(1);
    }

  hostrange_args->fanout = data->intval;
  return 0;
}

static int
config_file_ipmiconsole_escape_char(conffile_t cf,
                                    struct conffile_data *data,
                                    char *optionname,
                                    int option_type,
                                    void *option_ptr,
                                    int option_data,
                                    void *app_ptr,
                                    int app_data)
{
  char *chr;

  assert(option_ptr);

  chr = (char *)option_ptr;

  *chr = data->string[0];
  return 0;
}

int 
config_file_ipmipower_ipmi_version(conffile_t cf,
                                   struct conffile_data *data,
                                   char *optionname,
                                   int option_type,
                                   void *option_ptr,
                                   int option_data,
                                   void *app_ptr,
                                   int app_data)
{
  struct common_cmd_args *cmd_args;
  int tmp;

  assert(option_ptr);

  cmd_args = (struct common_cmd_args *)option_ptr;
  
  if (!strcasecmp(data->string, "1.5"))
    tmp = IPMI_DEVICE_LAN;
  else if (!strcasecmp(data->string, "2.0"))
    tmp = IPMI_DEVICE_LAN_2_0;
  else
    {
      fprintf(stderr, "Config File Error: invalid value for %s\n", optionname);
      exit(1);
    }
  
  cmd_args->driver_type = tmp;
  return 0;
}

static void
_ignore_options(struct conffile_option *options, unsigned int options_len)
{
  int i;

  assert(options && options_len);

  for (i = 0; i < options_len; i++)
    options[i].option_type = CONFFILE_OPTION_IGNORE;
}

static void
_copy_options(struct conffile_option *to_options,
              unsigned int to_options_len,
              struct conffile_option *from_options,
              unsigned int from_options_len)
{
  int i;
  
  assert(to_options && from_options && from_options_len);

  /* note: can't memcpy .. sigh .. wish I did this in C++ w/ a copy constructor */
  for (i = 0; i < from_options_len; i++)
    {
      to_options[to_options_len + i].optionname = from_options[i].optionname;
      to_options[to_options_len + i].option_type = from_options[i].option_type;
      to_options[to_options_len + i].option_type_arg = from_options[i].option_type_arg;
      to_options[to_options_len + i].callback_func = from_options[i].callback_func;
      to_options[to_options_len + i].max_count = from_options[i].max_count;
      to_options[to_options_len + i].required_count = from_options[i].required_count;
      to_options[to_options_len + i].count_ptr = from_options[i].count_ptr;
      to_options[to_options_len + i].option_ptr = from_options[i].option_ptr;
      to_options[to_options_len + i].option_data = from_options[i].option_data;
    }
}

int
config_file_parse(const char *filename,
                  int no_error_if_not_found,
                  struct common_cmd_args *cmd_args, 
                  struct sdr_cmd_args *sdr_args, 
                  struct hostrange_cmd_args *hostrange_args, 
                  unsigned int support,
                  unsigned int tool_support,
                  void *data)
{
  struct conffile_option config_file_options[CONFIG_FILE_OPTIONS_MAX];
  unsigned int config_file_options_len = 0;

  int driver_type_count = 0, workaround_flags_count = 0;

  int disable_auto_probe_count = 0,
    driver_address_count = 0, driver_device_count = 0,
    register_spacing_count = 0;

  int username_count = 0, password_count = 0, k_g_count = 0, 
    session_timeout_count = 0, retransmission_timeout_count = 0, 
    authentication_type_count = 0, cipher_suite_id_count = 0, 
    privilege_level_count = 0;

  int quiet_cache_count = 0, sdr_cache_directory_count = 0;

  int buffer_output_count = 0, consolidate_output_count = 0, 
    fanout_count = 0, eliminate_count = 0, always_prefix_count = 0;

  struct config_file_data_ipmiconsole ipmiconsole_data;
  struct config_file_data_ipmiconsole *ipmiconsole_data_ptr;

  struct config_file_data_ipmipower ipmipower_data;
  struct config_file_data_ipmipower *ipmipower_data_ptr;

  struct conffile_option inband_and_outofband_options[] =
    {
      {
        "driver-type", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_driver_type, 
        1, 
        0,
        &driver_type_count, 
        cmd_args,
        0
      },
      {
        "workaround-flags", 
        CONFFILE_OPTION_LIST_STRING, 
        -1,
        config_file_workaround_flags, 
        1, 
        0,
        &workaround_flags_count, 
        cmd_args, 
        0
      }
    };

  struct conffile_option inband_options[] =
    {
      {
        "disable-auto-probe",
        CONFFILE_OPTION_BOOL, 
        -1,
        config_file_bool, 
        1, 
        0,
        &disable_auto_probe_count, 
        &(cmd_args->disable_auto_probe), 
        0
      },
      {
        "driver-address", 
        CONFFILE_OPTION_INT, 
        -1,
        config_file_non_negative_int, 
        1, 
        0,
        &driver_address_count, 
        &(cmd_args->driver_address), 
        0
      },
      {
        "driver-device", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_string, 
        1, 
        0,
        &driver_device_count, 
        &(cmd_args->driver_device), 
        0
      },
      {
        "register-spacing", 
        CONFFILE_OPTION_INT, 
        -1,
        config_file_positive_int, 
        1, 
        0,
        &register_spacing_count, 
        &(cmd_args->register_spacing), 
        0
      },
    };

  struct conffile_option outofband_options[] =
    {
      {
        "username", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_string_with_length_check, 
        1, 
        0, 
        &username_count, 
        &(cmd_args->username),
        IPMI_MAX_USER_NAME_LENGTH
 
      },
      {
        "password", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_string_with_length_check, 
        1, 
        0, 
        &password_count, 
        &(cmd_args->password), 
        IPMI_2_0_MAX_PASSWORD_LENGTH
      },
      {
        "k_g", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_k_g, 
        1, 
        0, 
        &k_g_count, 
        cmd_args, 
        0
      },
      /* timeout maintained for backwards compatability */
      {
        "timeout", 
        CONFFILE_OPTION_INT, 
        -1,
        config_file_positive_int, 
        1, 
        0, 
        &session_timeout_count, 
        &(cmd_args->session_timeout), 
        0
      },
      {
        "session-timeout", 
        CONFFILE_OPTION_INT, 
        -1,
        config_file_positive_int, 
        1, 
        0, 
        &session_timeout_count, 
        &(cmd_args->session_timeout), 
        0
      },
      /* retry-timeout maintained for backwards compatability */
      {
        "retry-timeout", 
        CONFFILE_OPTION_INT, 
        -1,
        config_file_positive_int, 
        1, 
        0,
        &retransmission_timeout_count, 
        &(cmd_args->retransmission_timeout), 
        0
      },
      {
        "retransmission-timeout", 
        CONFFILE_OPTION_INT, 
        -1,
        config_file_positive_int, 
        1, 
        0,
        &retransmission_timeout_count, 
        &(cmd_args->retransmission_timeout), 
        0
      },
      {
        "authentication-type", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_authentication_type, 
        1, 
        0, 
        &authentication_type_count, 
        cmd_args, 
        0
      },
      /* cipher_suite_id (underscored) maintained for backwards compatability */
      {
        "cipher_suite_id", 
        CONFFILE_OPTION_INT, 
        -1,
        config_file_cipher_suite_id, 
        1, 
        0, 
        &cipher_suite_id_count, 
        cmd_args, 
        0
      },
      {
        "cipher-suite-id", 
        CONFFILE_OPTION_INT, 
        -1,
        config_file_cipher_suite_id, 
        1, 
        0,
        &cipher_suite_id_count, 
        cmd_args, 
        0
      },
      /* privilege maintained for backwards compatability */
      {
        "privilege", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_privilege_level, 
        1, 
        0, 
        &privilege_level_count, 
        cmd_args, 
        0
      },
      {
        "privilege-level", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_privilege_level, 
        1, 
        0,
        &privilege_level_count, 
        cmd_args, 
        0
      },
    };

  struct conffile_option sdr_options[] =
    {
      {
        "quiet-cache",
        CONFFILE_OPTION_BOOL,
        -1,
        config_file_bool,
        1,
        0,
        &quiet_cache_count,
        &(sdr_args->quiet_cache),
        0
      },
      {
        "sdr-cache-directory", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_string, 
        1, 
        0,
        &sdr_cache_directory_count, 
        &(sdr_args->sdr_cache_directory), 
        0
      },
    };

  struct conffile_option hostrange_options[] =
    {
      {
        "buffer-output",
        CONFFILE_OPTION_BOOL,
        -1,
        config_file_bool,
        1,
        0,
        &buffer_output_count,
        &(hostrange_args->consolidate_output),
        0
      },
      {
        "consolidate-output",
        CONFFILE_OPTION_BOOL,
        -1,
        config_file_bool,
        1,
        0,
        &consolidate_output_count,
        &(hostrange_args->consolidate_output),
        0
      },
      {
        "fanout", 
        CONFFILE_OPTION_INT, 
        -1,
        config_file_fanout, 
        1, 
        0,
        &fanout_count, 
        hostrange_args, 
        0
      },
      {
        "eliminate",
        CONFFILE_OPTION_BOOL,
        -1,
        config_file_bool,
        1,
        0,
        &eliminate_count,
        &(hostrange_args->eliminate),
        0
      },
      {
        "always-prefix",
        CONFFILE_OPTION_BOOL,
        -1,
        config_file_bool,
        1,
        0,
        &always_prefix_count,
        &(hostrange_args->always_prefix),
        0
      },
    };

  /* 
   * Tool Config Options
   */

  /* 
   * Ipmiconsole
   */

  /* Notes:
   *
   * deactivate option is not useful for config files.
   */

  struct conffile_option ipmiconsole_options[] =
    {
      /* legacy - no ipmiconsole prefix */
      {
        "escape-char",
        CONFFILE_OPTION_STRING,
        -1,
        config_file_ipmiconsole_escape_char,
        1,
        0,
        &(ipmiconsole_data.escape_char_count),
        &(ipmiconsole_data.escape_char),
        0
      },
      {
        "ipmiconsole-escape-char",
        CONFFILE_OPTION_STRING,
        -1,
        config_file_ipmiconsole_escape_char,
        1,
        0,
        &(ipmiconsole_data.escape_char_count),
        &(ipmiconsole_data.escape_char),
        0
      },
      /* legacy - no ipmiconsole prefix */
      {
        "dont-steal",
        CONFFILE_OPTION_BOOL,
        -1,
        config_file_bool,
        1,
        0,
        &(ipmiconsole_data.dont_steal_count),
        &(ipmiconsole_data.dont_steal),
        0,
      },
      {
        "ipmiconsole-dont-steal",
        CONFFILE_OPTION_BOOL,
        -1,
        config_file_bool,
        1,
        0,
        &(ipmiconsole_data.dont_steal_count),
        &(ipmiconsole_data.dont_steal),
        0,
      },
      /* legacy - no ipmiconsole prefix */
      {
        "lock-memory",
        CONFFILE_OPTION_BOOL,
        -1,
        config_file_bool,
        1,
        0,
        &(ipmiconsole_data.lock_memory_count),
        &(ipmiconsole_data.lock_memory),
        0,
      },
      {
        "ipmiconsole-lock-memory",
        CONFFILE_OPTION_BOOL,
        -1,
        config_file_bool,
        1,
        0,
        &(ipmiconsole_data.lock_memory_count),
        &(ipmiconsole_data.lock_memory),
        0,
      },
    };

  /* 
   * Ipmipower
   */

  struct conffile_option ipmipower_options[] =
    {
      /* ipmi-version maintained for backwards compatability */
      {
        "ipmi-version",
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_ipmipower_ipmi_version,
        1, 
        0,
        &driver_type_count, 
        cmd_args,
        0
      },
      /* legacy - no ipmipower prefix */
      {
        "on-if-off", 
        CONFFILE_OPTION_BOOL, 
        -1,
        config_file_bool,
        1, 
        0, 
        &(ipmipower_data.on_if_off_count), 
        &(ipmipower_data.on_if_off),
        0
      },
      {
        "ipmipower-on-if-off", 
        CONFFILE_OPTION_BOOL, 
        -1,
        config_file_bool,
        1, 
        0, 
        &(ipmipower_data.on_if_off_count), 
        &(ipmipower_data.on_if_off),
        0
      },
      /* legacy - no ipmipower prefix */
      {
        "wait-until-on", 
        CONFFILE_OPTION_BOOL, 
        -1, 
        config_file_bool,
        1, 
        0, 
        &(ipmipower_data.wait_until_on_count),
        &(ipmipower_data.wait_until_on),
        0
      },
      {
        "ipmipower-wait-until-on", 
        CONFFILE_OPTION_BOOL, 
        -1, 
        config_file_bool,
        1, 
        0, 
        &(ipmipower_data.wait_until_on_count),
        &(ipmipower_data.wait_until_on),
        0
      },
      /* legacy - no ipmipower prefix */
      {
        "wait-until-off", 
        CONFFILE_OPTION_BOOL, 
        -1, 
        config_file_bool,
        1,
        0, 
        &(ipmipower_data.wait_until_off_count),
        &(ipmipower_data.wait_until_off),
        0
      },
      {
        "ipmipower-wait-until-off", 
        CONFFILE_OPTION_BOOL, 
        -1, 
        config_file_bool,
        1,
        0, 
        &(ipmipower_data.wait_until_off_count),
        &(ipmipower_data.wait_until_off),
        0
      },
      /* retry-wait-timeout for backwards comptability */
      {
        "retry-wait-timeout", 
        CONFFILE_OPTION_INT, 
        -1, 
        config_file_positive_int,
        1,
        0,
        &(ipmipower_data.retransmission_wait_timeout_count), 
        &(ipmipower_data.retransmission_wait_timeout),
        0
      },
      /* legacy - no ipmipower prefix */
      {
        "retransmission-wait-timeout", 
        CONFFILE_OPTION_INT, 
        -1,
        config_file_positive_int,
        1, 
        0, 
        &(ipmipower_data.retransmission_wait_timeout_count), 
        &(ipmipower_data.retransmission_wait_timeout),
        0
      },
      {
        "ipmipower-retransmission-wait-timeout", 
        CONFFILE_OPTION_INT, 
        -1,
        config_file_positive_int,
        1, 
        0, 
        &(ipmipower_data.retransmission_wait_timeout_count), 
        &(ipmipower_data.retransmission_wait_timeout),
        0
      },
      /* retry-backoff-count for backwards compatability */
      {
        "retry-backoff-count", 
        CONFFILE_OPTION_INT, 
        -1, 
        config_file_positive_int,
        1, 
        0,
        &(ipmipower_data.retransmission_backoff_count_count), 
        &(ipmipower_data.retransmission_backoff_count),
        0
      },
      /* legacy - no ipmipower prefix */
      {
        "retransmission-backoff-count",
        CONFFILE_OPTION_INT,
        -1,
        config_file_positive_int,
        1, 
        0,
        &(ipmipower_data.retransmission_backoff_count_count), 
        &(ipmipower_data.retransmission_backoff_count),
        0
      },
      {
        "ipmipower-retransmission-backoff-count",
        CONFFILE_OPTION_INT,
        -1,
        config_file_positive_int,
        1, 
        0,
        &(ipmipower_data.retransmission_backoff_count_count), 
        &(ipmipower_data.retransmission_backoff_count),
        0
      },
      /* legacy - no ipmipower prefix */
      {
        "ping-interval", 
        CONFFILE_OPTION_INT, 
        -1,
        config_file_non_negative_int,
        1,
        0, 
        &(ipmipower_data.ping_interval_count), 
        &(ipmipower_data.ping_interval),
        0
      },
      {
        "ipmipower-ping-interval", 
        CONFFILE_OPTION_INT, 
        -1,
        config_file_non_negative_int,
        1,
        0, 
        &(ipmipower_data.ping_interval_count), 
        &(ipmipower_data.ping_interval),
        0
      },
      /* legacy - no ipmipower prefix */
      {
        "ping-timeout", 
        CONFFILE_OPTION_INT, 
        -1, 
        config_file_non_negative_int,
        1,
        0,
        &(ipmipower_data.ping_timeout_count), 
        &(ipmipower_data.ping_timeout),
        0
      },
      {
        "ipmipower-ping-timeout", 
        CONFFILE_OPTION_INT, 
        -1, 
        config_file_non_negative_int,
        1,
        0,
        &(ipmipower_data.ping_timeout_count), 
        &(ipmipower_data.ping_timeout),
        0
      },
      /* legacy - no ipmipower prefix */
      {
        "ping-packet-count",
        CONFFILE_OPTION_INT,
        -1, 
        config_file_non_negative_int,
        1, 
        0, 
        &(ipmipower_data.ping_packet_count_count), 
        &(ipmipower_data.ping_packet_count),
        0
      },
      {
        "ipmipower-ping-packet-count",
        CONFFILE_OPTION_INT,
        -1, 
        config_file_non_negative_int,
        1, 
        0, 
        &(ipmipower_data.ping_packet_count_count), 
        &(ipmipower_data.ping_packet_count),
        0
      },
      /* legacy - no ipmipower prefix */
      {
        "ping-percent", 
        CONFFILE_OPTION_INT, 
        -1,
        config_file_non_negative_int,
        1, 
        0, 
        &(ipmipower_data.ping_percent_count), 
        &(ipmipower_data.ping_percent),
        0
      },
      {
        "ipmipower-ping-percent", 
        CONFFILE_OPTION_INT, 
        -1,
        config_file_non_negative_int,
        1, 
        0, 
        &(ipmipower_data.ping_percent_count), 
        &(ipmipower_data.ping_percent),
        0
      },
      /* legacy - no ipmipower prefix */
      {
        "ping-consec-count", 
        CONFFILE_OPTION_INT, 
        -1, 
        config_file_non_negative_int,
        1, 
        0, 
        &(ipmipower_data.ping_consec_count_count), 
        &(ipmipower_data.ping_consec_count),
       0
      },
      {
        "ipmipower-ping-consec-count", 
        CONFFILE_OPTION_INT, 
        -1, 
        config_file_non_negative_int,
        1, 
        0, 
        &(ipmipower_data.ping_consec_count_count), 
        &(ipmipower_data.ping_consec_count),
       0
      },
    };

  conffile_t cf = NULL;
  int rv = -1;
  int options_len;

  assert((!support
          || (((support & CONFIG_FILE_INBAND)
               || (support & CONFIG_FILE_OUTOFBAND))
              && cmd_args))
         && (!(support & CONFIG_FILE_SDR)
             || ((support & CONFIG_FILE_SDR) && sdr_args))
         && (!(support & CONFIG_FILE_HOSTRANGE)
             || ((support & CONFIG_FILE_HOSTRANGE) && hostrange_args))
         && (!tool_support
             || (tool_support && data)));

  memset(config_file_options, '\0', sizeof(struct conffile_option));

  /* set ignore options the tool doesn't care about */

  /* 
   * general support flags 
   */

  /* driver-type is for both inband and outofband */
  options_len = sizeof(inband_and_outofband_options)/sizeof(struct conffile_option);
  if (!(support & CONFIG_FILE_INBAND)
      && !(support & CONFIG_FILE_OUTOFBAND))
    _ignore_options(inband_and_outofband_options, options_len);
  
  _copy_options(config_file_options,
                config_file_options_len,
                inband_and_outofband_options,
                options_len);

  config_file_options_len += options_len;

  options_len = sizeof(inband_options)/sizeof(struct conffile_option);
  if (!(support & CONFIG_FILE_INBAND))
    _ignore_options(inband_options, options_len);
  
  _copy_options(config_file_options,
                config_file_options_len,
                inband_options,
                options_len);

  config_file_options_len += options_len;

  options_len = sizeof(outofband_options)/sizeof(struct conffile_option);
  if (!(support & CONFIG_FILE_OUTOFBAND))
    _ignore_options(outofband_options, options_len);
  
  _copy_options(config_file_options,
                config_file_options_len,
                outofband_options,
                options_len);

  config_file_options_len += options_len;

  options_len = sizeof(sdr_options)/sizeof(struct conffile_option);
  if (!(support & CONFIG_FILE_SDR))
    _ignore_options(sdr_options, options_len);
  
  _copy_options(config_file_options,
                config_file_options_len,
                sdr_options,
                options_len);

  config_file_options_len += options_len;

  options_len = sizeof(hostrange_options)/sizeof(struct conffile_option);
  if (!(support & CONFIG_FILE_HOSTRANGE))
    _ignore_options(hostrange_options, options_len);
  
  _copy_options(config_file_options,
                config_file_options_len,
                hostrange_options,
                options_len);

  config_file_options_len += options_len;

  /* 
   * tool flags 
   */

  options_len = sizeof(ipmiconsole_options)/sizeof(struct conffile_option);
  if (!(tool_support & CONFIG_FILE_TOOL_IPMICONSOLE))
    _ignore_options(ipmiconsole_options, options_len);
  
  _copy_options(config_file_options,
                config_file_options_len,
                ipmiconsole_options,
                options_len);

  config_file_options_len += options_len;

  options_len = sizeof(ipmipower_options)/sizeof(struct conffile_option);
  if (!(tool_support & CONFIG_FILE_TOOL_IPMIPOWER))
    _ignore_options(ipmipower_options, options_len);
  
  _copy_options(config_file_options,
                config_file_options_len,
                ipmipower_options,
                options_len);

  config_file_options_len += options_len;
  
  /* clear out config file data */

  memset(&ipmiconsole_data, '\0', sizeof(struct config_file_data_ipmiconsole));
  memset(&ipmipower_data, '\0', sizeof(struct config_file_data_ipmipower));

  if (!(cf = conffile_handle_create()))
    {
      fprintf(stderr, "conffile_handle_create: %s\n", strerror(errno));
      goto cleanup;
    }

  /* FREEIPMI_CONFIG_FILE_DEFAULT defined in config.h */
  if (!filename)
    filename = FREEIPMI_CONFIG_FILE_DEFAULT;

  if (conffile_parse(cf, 
                     filename,
                     config_file_options,
                     config_file_options_len, 
                     NULL,
                     0,
                     0) < 0)
    {
      char buf[CONFFILE_MAX_ERRMSGLEN];

      /* don't exit, but return -1 */
      if (conffile_errnum(cf) == CONFFILE_ERR_EXIST
          && no_error_if_not_found)
        goto cleanup;

      /* Its not an error if the default configuration file doesn't exist */
      if (!strcmp(filename, FREEIPMI_CONFIG_FILE_DEFAULT)
          && conffile_errnum(cf) == CONFFILE_ERR_EXIST)
        goto out;
      
      if (conffile_errmsg(cf, buf, CONFFILE_MAX_ERRMSGLEN) < 0)
        {
          fprintf(stderr, "conffile_parse: %d\n", conffile_errnum(cf));
          exit(1);
        }
      else
        {
          if (CONFFILE_IS_PARSE_ERR(conffile_errnum(cf))
              || conffile_errnum(cf) == CONFFILE_ERR_EXIST
              || conffile_errnum(cf) == CONFFILE_ERR_OPEN
              || conffile_errnum(cf) == CONFFILE_ERR_READ)
            fprintf(stderr, "Config File Error: %s\n", buf);
          else
            fprintf(stderr, "conffile_parse: %s\n", buf);
          exit(1);
        }
    }

  /* copy file data over to tool */
  
  if (tool_support & CONFIG_FILE_TOOL_IPMICONSOLE)
    {
      ipmiconsole_data_ptr = (struct config_file_data_ipmiconsole *)data;
      memcpy(ipmiconsole_data_ptr, 
             &ipmiconsole_data,
             sizeof(struct config_file_data_ipmiconsole));
    }
  else if (tool_support & CONFIG_FILE_TOOL_IPMIPOWER)
    {
      ipmipower_data_ptr = (struct config_file_data_ipmipower *)data;
      memcpy(ipmipower_data_ptr, 
             &ipmipower_data,
             sizeof(struct config_file_data_ipmipower));
    }

 out:
  rv = 0;
 cleanup:
  if (cf)
    conffile_handle_destroy(cf);
  return rv;
}
