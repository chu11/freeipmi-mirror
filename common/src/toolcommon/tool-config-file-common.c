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
#endif /* STDC_HEADERS */
#include <errno.h>
#include <assert.h>

#include "freeipmi/api/ipmi-api.h"
#include "freeipmi/cmds/ipmi-messaging-support-cmds.h"
#include "freeipmi/interface/ipmi-rmcpplus-interface.h"
#include "freeipmi/util/ipmi-cipher-suite-util.h"

#include "freeipmi-portability.h"
#include "pstdout.h"
#include "tool-config-file-common.h"
#include "tool-common.h"

#define CONFIG_FILE_OPTIONS_MAX 1024

struct cmd_args_config
{
  char *username;
  int username_set;
  int tool_specific_username_set;
  char *password;
  int password_set;
  int tool_specific_password_set;
  uint8_t k_g[IPMI_MAX_K_G_LENGTH+1];
  unsigned int k_g_len;
  int k_g_set;
  int tool_specific_k_g_set;
  int authentication_type;
  int authentication_type_set;
  int tool_specific_authentication_type_set;
  int cipher_suite_id;
  int cipher_suite_id_set;
  int tool_specific_cipher_suite_id_set;
  int privilege_level;
  int privilege_level_set;
  int tool_specific_privilege_level_set;
  int workaround_flags;
  int workaround_flags_set;
  int tool_specific_workaround_flags_set;
};

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
config_file_username(conffile_t cf,
                     struct conffile_data *data,
                     char *optionname,
                     int option_type,
                     void *option_ptr,
                     int option_data,
                     void *app_ptr,
                     int app_data)
{
  struct cmd_args_config *cmd_args_config;

  assert(option_ptr);

  cmd_args_config = (struct cmd_args_config *)option_ptr;

  if (cmd_args_config->tool_specific_username_set)
    return 0;
        
  if (strlen(data->string) > IPMI_MAX_USER_NAME_LENGTH)
    {
      fprintf (stderr, "Config File Error: %s value too long\n", optionname);
      exit(1);
    }

  if (!(cmd_args_config->username = strdup(data->string)))
    {
      perror("strdup");
      exit(1);
    }

  cmd_args_config->username_set++;

  return 0;
}

int 
config_file_password(conffile_t cf,
                     struct conffile_data *data,
                     char *optionname,
                     int option_type,
                     void *option_ptr,
                     int option_data,
                     void *app_ptr,
                     int app_data)
{
  struct cmd_args_config *cmd_args_config;

  assert(option_ptr);

  cmd_args_config = (struct cmd_args_config *)option_ptr;
        
  if (cmd_args_config->tool_specific_password_set)
    return 0;

  if (strlen(data->string) > IPMI_2_0_MAX_PASSWORD_LENGTH)
    {
      fprintf (stderr, "Config File Error: %s value too long\n", optionname);
      exit(1);
    }

  if (!(cmd_args_config->password = strdup(data->string)))
    {
      perror("strdup");
      exit(1);
    }

  cmd_args_config->password_set++;

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
  struct cmd_args_config *cmd_args_config;
  int rv;

  assert(option_ptr);

  cmd_args_config = (struct cmd_args_config *)option_ptr;

  if (cmd_args_config->tool_specific_k_g_set)
    return 0;

  if ((rv = parse_kg(cmd_args_config->k_g, IPMI_MAX_K_G_LENGTH + 1, data->string)) < 0)
    {
      fprintf(stderr, "Config File Error: k_g input formatted incorrectly\n");
      exit(1);
    }

  if (rv > 0)
    cmd_args_config->k_g_len = rv;

  cmd_args_config->k_g_set++;

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
  struct cmd_args_config *cmd_args_config;
  int tmp;

  assert(option_ptr);

  cmd_args_config = (struct cmd_args_config *)option_ptr;

  if (cmd_args_config->tool_specific_authentication_type_set)
    return 0;

  if ((tmp = parse_authentication_type(data->string)) < 0)
    {
      fprintf(stderr, "Config File Error: invalid value for %s\n", optionname);
      exit(1);
    }

  cmd_args_config->authentication_type = tmp;

  cmd_args_config->authentication_type_set++;

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
  struct cmd_args_config *cmd_args_config;

  assert(option_ptr);

  cmd_args_config = (struct cmd_args_config *)option_ptr;

  if (cmd_args_config->tool_specific_cipher_suite_id_set)
    return 0;

  if (data->intval < IPMI_CIPHER_SUITE_ID_MIN
      || data->intval > IPMI_CIPHER_SUITE_ID_MAX
      || !IPMI_CIPHER_SUITE_ID_SUPPORTED(data->intval))
    {
      fprintf(stderr, "Config File Error: invalid value for %s\n", optionname);
      exit(1);
    }

  cmd_args_config->cipher_suite_id = data->intval;

  cmd_args_config->cipher_suite_id_set++;

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
  struct cmd_args_config *cmd_args_config;
  int tmp;

  assert(option_ptr);

  cmd_args_config = (struct cmd_args_config *)option_ptr;

  if (cmd_args_config->tool_specific_privilege_level_set)
    return 0;

  if ((tmp = parse_privilege_level(data->string)) < 0)
    {
      fprintf(stderr, "Config File Error: invalid value for %s\n", optionname);
      exit(1);
    }

  cmd_args_config->privilege_level = tmp;

  cmd_args_config->privilege_level_set++;

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
  struct cmd_args_config *cmd_args_config;
  int i, tmp;

  assert(option_ptr);

  cmd_args_config = (struct cmd_args_config *)option_ptr;

  if (cmd_args_config->tool_specific_workaround_flags_set)
    return 0;

  cmd_args_config->workaround_flags = 0;
  
  for (i = 0; i < data->stringlist_len; i++)
    {
      if ((tmp = parse_workaround_flags(data->stringlist[i])) < 0)
        {
          fprintf(stderr, "Config File Error: invalid value for %s\n", optionname);
          exit(1);
        }
      cmd_args_config->workaround_flags |= tmp;
    }
  cmd_args_config->workaround_flags_set++;

  return 0;
}

int 
config_file_tool_specific_username(conffile_t cf,
                                   struct conffile_data *data,
                                   char *optionname,
                                   int option_type,
                                   void *option_ptr,
                                   int option_data,
                                   void *app_ptr,
                                   int app_data)
{
  struct cmd_args_config *cmd_args_config;

  assert(option_ptr);

  cmd_args_config = (struct cmd_args_config *)option_ptr;
        
  if (strlen(data->string) > IPMI_MAX_USER_NAME_LENGTH)
    {
      fprintf (stderr, "Config File Error: %s value too long\n", optionname);
      exit(1);
    }

  if (cmd_args_config->username_set)
    free(cmd_args_config->username);

  if (!(cmd_args_config->username = strdup(data->string)))
    {
      perror("strdup");
      exit(1);
    }

  cmd_args_config->username_set++;
  cmd_args_config->tool_specific_username_set++;

  return 0;
}

int 
config_file_tool_specific_password(conffile_t cf,
                                   struct conffile_data *data,
                                   char *optionname,
                                   int option_type,
                                   void *option_ptr,
                                   int option_data,
                                   void *app_ptr,
                                   int app_data)
{
  struct cmd_args_config *cmd_args_config;

  assert(option_ptr);

  cmd_args_config = (struct cmd_args_config *)option_ptr;
        
  if (strlen(data->string) > IPMI_2_0_MAX_PASSWORD_LENGTH)
    {
      fprintf (stderr, "Config File Error: %s value too long\n", optionname);
      exit(1);
    }

  if (cmd_args_config->password_set)
    free(cmd_args_config->password);

  if (!(cmd_args_config->password = strdup(data->string)))
    {
      perror("strdup");
      exit(1);
    }

  cmd_args_config->password_set++;
  cmd_args_config->tool_specific_password_set++;

  return 0;
}

int 
config_file_tool_specific_k_g(conffile_t cf,
                              struct conffile_data *data,
                              char *optionname,
                              int option_type,
                              void *option_ptr,
                              int option_data,
                              void *app_ptr,
                              int app_data)
{
  struct cmd_args_config *cmd_args_config;
  int rv;

  assert(option_ptr);

  cmd_args_config = (struct cmd_args_config *)option_ptr;

  if (cmd_args_config->k_g_set)
    memset(cmd_args_config->k_g, '\0', IPMI_MAX_K_G_LENGTH + 1);

  if ((rv = parse_kg(cmd_args_config->k_g, IPMI_MAX_K_G_LENGTH + 1, data->string)) < 0)
    {
      fprintf(stderr, "Config File Error: k_g input formatted incorrectly\n");
      exit(1);
    }

  if (rv > 0)
    cmd_args_config->k_g_len = rv;

  cmd_args_config->k_g_set++;
  cmd_args_config->tool_specific_k_g_set++;

  return 0;
}

int 
config_file_tool_specific_authentication_type(conffile_t cf,
                                              struct conffile_data *data,
                                              char *optionname,
                                              int option_type,
                                              void *option_ptr,
                                              int option_data,
                                              void *app_ptr,
                                              int app_data)
{
  struct cmd_args_config *cmd_args_config;
  int tmp;

  assert(option_ptr);

  cmd_args_config = (struct cmd_args_config *)option_ptr;

  if ((tmp = parse_authentication_type(data->string)) < 0)
    {
      fprintf(stderr, "Config File Error: invalid value for %s\n", optionname);
      exit(1);
    }

  cmd_args_config->authentication_type = tmp;

  cmd_args_config->authentication_type_set++;
  cmd_args_config->tool_specific_authentication_type_set++;

  return 0;
}

int 
config_file_tool_specific_cipher_suite_id(conffile_t cf,
                                          struct conffile_data *data,
                                          char *optionname,
                                          int option_type,
                                          void *option_ptr,
                                          int option_data,
                                          void *app_ptr,
                                          int app_data)
{
  struct cmd_args_config *cmd_args_config;

  assert(option_ptr);

  cmd_args_config = (struct cmd_args_config *)option_ptr;

  if (data->intval < IPMI_CIPHER_SUITE_ID_MIN
      || data->intval > IPMI_CIPHER_SUITE_ID_MAX
      || !IPMI_CIPHER_SUITE_ID_SUPPORTED(data->intval))
    {
      fprintf(stderr, "Config File Error: invalid value for %s\n", optionname);
      exit(1);
    }

  cmd_args_config->cipher_suite_id = data->intval;

  cmd_args_config->cipher_suite_id_set++;
  cmd_args_config->tool_specific_cipher_suite_id_set++;

  return 0;
}

int 
config_file_tool_specific_privilege_level(conffile_t cf,
                                          struct conffile_data *data,
                                          char *optionname,
                                          int option_type,
                                          void *option_ptr,
                                          int option_data,
                                          void *app_ptr,
                                          int app_data)
{
  struct cmd_args_config *cmd_args_config;
  int tmp;

  assert(option_ptr);

  cmd_args_config = (struct cmd_args_config *)option_ptr;

  if ((tmp = parse_privilege_level(data->string)) < 0)
    {
      fprintf(stderr, "Config File Error: invalid value for %s\n", optionname);
      exit(1);
    }

  cmd_args_config->privilege_level = tmp;

  cmd_args_config->privilege_level_set++;
  cmd_args_config->tool_specific_privilege_level_set++;

  return 0;
}

int 
config_file_tool_specific_workaround_flags(conffile_t cf,
                                           struct conffile_data *data,
                                           char *optionname,
                                           int option_type,
                                           void *option_ptr,
                                           int option_data,
                                           void *app_ptr,
                                           int app_data)
{
  struct cmd_args_config *cmd_args_config;
  int i, tmp;

  assert(option_ptr);

  cmd_args_config = (struct cmd_args_config *)option_ptr;

  cmd_args_config->workaround_flags = 0;
  
  for (i = 0; i < data->stringlist_len; i++)
    {
      if ((tmp = parse_workaround_flags(data->stringlist[i])) < 0)
        {
          fprintf(stderr, "Config File Error: invalid value for %s\n", optionname);
          exit(1);
        }
      cmd_args_config->workaround_flags |= tmp;
    }
  cmd_args_config->workaround_flags_set++;
  cmd_args_config->tool_specific_workaround_flags_set++;

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

int 
config_file_ipmi_sensors_groups(conffile_t cf,
                                struct conffile_data *data,
                                char *optionname,
                                int option_type,
                                void *option_ptr,
                                int option_data,
                                void *app_ptr,
                                int app_data)
{
  struct config_file_data_ipmi_sensors *config_file_data;
  int i;

  assert(option_ptr);

  config_file_data = (struct config_file_data_ipmi_sensors *)option_ptr;

  if (data->stringlist_len > CONFIG_FILE_IPMI_SENSORS_MAX_GROUPS)
    {
      fprintf(stderr, "Config File Error: invalid number of arguments for %s\n", optionname);
      exit(1);
    }

  for (i = 0; i < data->stringlist_len; i++)
    {
      if (strlen(data->stringlist[i]) > CONFIG_FILE_IPMI_SENSORS_MAX_GROUPS_STRING_LENGTH)
        {
          fprintf(stderr, "Config File Error: invalid value '%s' for %s\n", 
                  data->stringlist[i],
                  optionname);
          exit(1);
        }

      strncpy(config_file_data->groups[i], 
              data->stringlist[i], 
              CONFIG_FILE_IPMI_SENSORS_MAX_GROUPS_STRING_LENGTH);

      config_file_data->groups_length++;
    }

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
config_file_ipmimonitoring_groups(conffile_t cf,
                                  struct conffile_data *data,
                                  char *optionname,
                                  int option_type,
                                  void *option_ptr,
                                  int option_data,
                                  void *app_ptr,
                                  int app_data)
{
  struct config_file_data_ipmimonitoring *config_file_data;
  int i;

  assert(option_ptr);

  config_file_data = (struct config_file_data_ipmimonitoring *)option_ptr;

  if (data->stringlist_len > CONFIG_FILE_IPMIMONITORING_MAX_GROUPS)
    {
      fprintf(stderr, "Config File Error: invalid number of arguments for %s\n", optionname);
      exit(1);
    }

  for (i = 0; i < data->stringlist_len; i++)
    {
      if (strlen(data->stringlist[i]) > CONFIG_FILE_IPMIMONITORING_MAX_GROUPS_STRING_LENGTH)
        {
          fprintf(stderr, "Config File Error: invalid value '%s' for %s\n", 
                  data->stringlist[i],
                  optionname);
          exit(1);
        }

      strncpy(config_file_data->groups[i], 
              data->stringlist[i], 
              CONFIG_FILE_IPMIMONITORING_MAX_GROUPS_STRING_LENGTH);

      config_file_data->groups_length++;
    }

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
                  void *tool_data)
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

  int bmc_config_username_count = 0, bmc_config_password_count = 0, 
    bmc_config_k_g_count = 0, bmc_config_authentication_type_count = 0, 
    bmc_config_cipher_suite_id_count = 0, bmc_config_privilege_level_count = 0,
    bmc_config_workaround_flags_count = 0;

  int bmc_device_username_count = 0, bmc_device_password_count = 0, 
    bmc_device_k_g_count = 0, bmc_device_authentication_type_count = 0, 
    bmc_device_cipher_suite_id_count = 0, bmc_device_privilege_level_count = 0,
    bmc_device_workaround_flags_count = 0;

  int bmc_info_username_count = 0, bmc_info_password_count = 0, 
    bmc_info_k_g_count = 0, bmc_info_authentication_type_count = 0, 
    bmc_info_cipher_suite_id_count = 0, bmc_info_privilege_level_count = 0,
    bmc_info_workaround_flags_count = 0;

  int bmc_watchdog_workaround_flags_count = 0;

  int ipmi_chassis_username_count = 0, ipmi_chassis_password_count = 0, 
    ipmi_chassis_k_g_count = 0, ipmi_chassis_authentication_type_count = 0, 
    ipmi_chassis_cipher_suite_id_count = 0, ipmi_chassis_privilege_level_count = 0,
    ipmi_chassis_workaround_flags_count = 0;

  int ipmi_chassis_config_username_count = 0, ipmi_chassis_config_password_count = 0, 
    ipmi_chassis_config_k_g_count = 0, ipmi_chassis_config_authentication_type_count = 0, 
    ipmi_chassis_config_cipher_suite_id_count = 0, ipmi_chassis_config_privilege_level_count = 0,
    ipmi_chassis_config_workaround_flags_count = 0;

  int ipmi_fru_username_count = 0, ipmi_fru_password_count = 0, 
    ipmi_fru_k_g_count = 0, ipmi_fru_authentication_type_count = 0, 
    ipmi_fru_cipher_suite_id_count = 0, ipmi_fru_privilege_level_count = 0,
    ipmi_fru_workaround_flags_count = 0;

  int ipmi_oem_username_count = 0, ipmi_oem_password_count = 0, 
    ipmi_oem_k_g_count = 0, ipmi_oem_authentication_type_count = 0, 
    ipmi_oem_cipher_suite_id_count = 0, ipmi_oem_privilege_level_count = 0,
    ipmi_oem_workaround_flags_count = 0;

  int ipmi_raw_username_count = 0, ipmi_raw_password_count = 0, 
    ipmi_raw_k_g_count = 0, ipmi_raw_authentication_type_count = 0, 
    ipmi_raw_cipher_suite_id_count = 0, ipmi_raw_privilege_level_count = 0,
    ipmi_raw_workaround_flags_count = 0;

  int ipmi_sel_username_count = 0, ipmi_sel_password_count = 0, 
    ipmi_sel_k_g_count = 0, ipmi_sel_authentication_type_count = 0, 
    ipmi_sel_cipher_suite_id_count = 0, ipmi_sel_privilege_level_count = 0,
    ipmi_sel_workaround_flags_count = 0;
  
  int ipmi_sensors_username_count = 0, ipmi_sensors_password_count = 0, 
    ipmi_sensors_k_g_count = 0, ipmi_sensors_authentication_type_count = 0, 
    ipmi_sensors_cipher_suite_id_count = 0, ipmi_sensors_privilege_level_count = 0,
    ipmi_sensors_workaround_flags_count = 0;

  int ipmi_sensors_config_username_count = 0, ipmi_sensors_config_password_count = 0, 
    ipmi_sensors_config_k_g_count = 0, ipmi_sensors_config_authentication_type_count = 0, 
    ipmi_sensors_config_cipher_suite_id_count = 0, ipmi_sensors_config_privilege_level_count = 0,
    ipmi_sensors_config_workaround_flags_count = 0;
  
  int ipmiconsole_username_count = 0, ipmiconsole_password_count = 0, 
    ipmiconsole_k_g_count = 0, ipmiconsole_authentication_type_count = 0, 
    ipmiconsole_cipher_suite_id_count = 0, ipmiconsole_privilege_level_count = 0,
    ipmiconsole_workaround_flags_count = 0;

  int ipmimonitoring_username_count = 0, ipmimonitoring_password_count = 0, 
    ipmimonitoring_k_g_count = 0, ipmimonitoring_authentication_type_count = 0, 
    ipmimonitoring_cipher_suite_id_count = 0, ipmimonitoring_privilege_level_count = 0,
    ipmimonitoring_workaround_flags_count = 0;

  int ipmipower_username_count = 0, ipmipower_password_count = 0, 
    ipmipower_k_g_count = 0, ipmipower_authentication_type_count = 0, 
    ipmipower_cipher_suite_id_count = 0, ipmipower_privilege_level_count = 0,
    ipmipower_workaround_flags_count = 0;

  int pef_config_username_count = 0, pef_config_password_count = 0, 
    pef_config_k_g_count = 0, pef_config_authentication_type_count = 0, 
    pef_config_cipher_suite_id_count = 0, pef_config_privilege_level_count = 0,
    pef_config_workaround_flags_count = 0;

  struct config_file_data_bmc_watchdog bmc_watchdog_data;
  struct config_file_data_bmc_watchdog *bmc_watchdog_data_ptr;

  struct config_file_data_ipmi_fru ipmi_fru_data;
  struct config_file_data_ipmi_fru *ipmi_fru_data_ptr;

  struct config_file_data_ipmi_sensors ipmi_sensors_data;
  struct config_file_data_ipmi_sensors *ipmi_sensors_data_ptr;

  struct config_file_data_ipmiconsole ipmiconsole_data;
  struct config_file_data_ipmiconsole *ipmiconsole_data_ptr;

  struct config_file_data_ipmimonitoring ipmimonitoring_data;
  struct config_file_data_ipmimonitoring *ipmimonitoring_data_ptr;

  struct config_file_data_ipmipower ipmipower_data;
  struct config_file_data_ipmipower *ipmipower_data_ptr;

  struct cmd_args_config cmd_args_config;

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
        &cmd_args_config,
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
        config_file_positive_int, 
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
        config_file_username, 
        1, 
        0, 
        &username_count, 
        &cmd_args_config,
        0,
      },
      {
        "password", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_password, 
        1, 
        0, 
        &password_count, 
        &cmd_args_config,
        0,
      },
      {
        "k_g", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_k_g, 
        1, 
        0, 
        &k_g_count, 
        &cmd_args_config,
        0,
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
        &cmd_args_config,
        0,
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
        &cmd_args_config,
        0,
      },
      {
        "cipher-suite-id", 
        CONFFILE_OPTION_INT, 
        -1,
        config_file_cipher_suite_id, 
        1, 
        0,
        &cipher_suite_id_count, 
        &cmd_args_config,
        0,
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
        &cmd_args_config,
        0,
      },
      {
        "privilege-level", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_privilege_level, 
        1, 
        0,
        &privilege_level_count, 
        &cmd_args_config,
        0,
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
   * Bmc-config
   */
  struct conffile_option bmc_config_options[] =
    {
      {
        "bmc-config-username", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_username, 
        1, 
        0, 
        &bmc_config_username_count, 
        &cmd_args_config,
        0,
      },
      {
        "bmc-config-password", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_password, 
        1, 
        0, 
        &bmc_config_password_count, 
        &cmd_args_config,
        0,
      },
      {
        "bmc-config-k_g", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_k_g, 
        1, 
        0, 
        &bmc_config_k_g_count, 
        &cmd_args_config,
        0,
      },
      {
        "bmc-config-authentication-type", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_authentication_type, 
        1, 
        0, 
        &bmc_config_authentication_type_count, 
        &cmd_args_config,
        0,
      },
      {
        "bmc-config-cipher-suite-id", 
        CONFFILE_OPTION_INT, 
        -1,
        config_file_tool_specific_cipher_suite_id, 
        1, 
        0,
        &bmc_config_cipher_suite_id_count, 
        &cmd_args_config,
        0,
      },
      {
        "bmc-config-privilege-level", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_privilege_level, 
        1, 
        0,
        &bmc_config_privilege_level_count, 
        &cmd_args_config,
        0,
      },
      {
        "bmc-config-workaround-flags", 
        CONFFILE_OPTION_LIST_STRING, 
        -1,
        config_file_tool_specific_workaround_flags, 
        1, 
        0,
        &bmc_config_workaround_flags_count, 
        &cmd_args_config,
        0
      },
    };

  /* 
   * Bmc-device
   */
  struct conffile_option bmc_device_options[] =
    {
      {
        "bmc-device-username", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_username, 
        1, 
        0, 
        &bmc_device_username_count, 
        &cmd_args_config,
        0,
      },
      {
        "bmc-device-password", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_password, 
        1, 
        0, 
        &bmc_device_password_count, 
        &cmd_args_config,
        0,
      },
      {
        "bmc-device-k_g", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_k_g, 
        1, 
        0, 
        &bmc_device_k_g_count, 
        &cmd_args_config,
        0,
      },
      {
        "bmc-device-authentication-type", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_authentication_type, 
        1, 
        0, 
        &bmc_device_authentication_type_count, 
        &cmd_args_config,
        0,
      },
      {
        "bmc-device-cipher-suite-id", 
        CONFFILE_OPTION_INT, 
        -1,
        config_file_tool_specific_cipher_suite_id, 
        1, 
        0,
        &bmc_device_cipher_suite_id_count, 
        &cmd_args_config,
        0,
      },
      {
        "bmc-device-privilege-level", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_privilege_level, 
        1, 
        0,
        &bmc_device_privilege_level_count, 
        &cmd_args_config,
        0,
      },
      {
        "bmc-device-workaround-flags", 
        CONFFILE_OPTION_LIST_STRING, 
        -1,
        config_file_tool_specific_workaround_flags, 
        1, 
        0,
        &bmc_device_workaround_flags_count, 
        &cmd_args_config,
        0
      },
    };

  /* 
   * Bmc-info
   */
  struct conffile_option bmc_info_options[] =
    {
      {
        "bmc-info-username", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_username, 
        1, 
        0, 
        &bmc_info_username_count, 
        &cmd_args_config,
        0,
      },
      {
        "bmc-info-password", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_password, 
        1, 
        0, 
        &bmc_info_password_count, 
        &cmd_args_config,
        0,
      },
      {
        "bmc-info-k_g", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_k_g, 
        1, 
        0, 
        &bmc_info_k_g_count, 
        &cmd_args_config,
        0,
      },
      {
        "bmc-info-authentication-type", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_authentication_type, 
        1, 
        0, 
        &bmc_info_authentication_type_count, 
        &cmd_args_config,
        0,
      },
      {
        "bmc-info-cipher-suite-id", 
        CONFFILE_OPTION_INT, 
        -1,
        config_file_tool_specific_cipher_suite_id, 
        1, 
        0,
        &bmc_info_cipher_suite_id_count, 
        &cmd_args_config,
        0,
      },
      {
        "bmc-info-privilege-level", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_privilege_level, 
        1, 
        0,
        &bmc_info_privilege_level_count, 
        &cmd_args_config,
        0,
      },
      {
        "bmc-info-workaround-flags", 
        CONFFILE_OPTION_LIST_STRING, 
        -1,
        config_file_tool_specific_workaround_flags, 
        1, 
        0,
        &bmc_info_workaround_flags_count, 
        &cmd_args_config,
        0
      },
    };

  /* 
   * Bmc-watchdog
   */

  struct conffile_option bmc_watchdog_options[] =
    {
      {
        "bmc-watchdog-workaround-flags", 
        CONFFILE_OPTION_LIST_STRING, 
        -1,
        config_file_tool_specific_workaround_flags, 
        1, 
        0,
        &bmc_watchdog_workaround_flags_count, 
        &cmd_args_config,
        0
      },
      {
        "bmc-watchdog-logfile", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_string, 
        1, 
        0,
        &(bmc_watchdog_data.logfile_count),
        &(bmc_watchdog_data.logfile),
        0
      },
      {
        "bmc-watchdog-no-logging",
        CONFFILE_OPTION_BOOL,
        -1,
        config_file_bool,
        1,
        0,
        &(bmc_watchdog_data.no_logging_count),
        &(bmc_watchdog_data.no_logging),
        0,
      },
    };

  /* 
   * Ipmi-chassis 
   */
  struct conffile_option ipmi_chassis_options[] =
    {
      {
        "ipmi-chassis-username", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_username, 
        1, 
        0, 
        &ipmi_chassis_username_count, 
        &cmd_args_config,
        0,
      },
      {
        "ipmi-chassis-password", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_password, 
        1, 
        0, 
        &ipmi_chassis_password_count, 
        &cmd_args_config,
        0,
      },
      {
        "ipmi-chassis-k_g", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_k_g, 
        1, 
        0, 
        &ipmi_chassis_k_g_count, 
        &cmd_args_config,
        0,
      },
      {
        "ipmi-chassis-authentication-type", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_authentication_type, 
        1, 
        0, 
        &ipmi_chassis_authentication_type_count, 
        &cmd_args_config,
        0,
      },
      {
        "ipmi-chassis-cipher-suite-id", 
        CONFFILE_OPTION_INT, 
        -1,
        config_file_tool_specific_cipher_suite_id, 
        1, 
        0,
        &ipmi_chassis_cipher_suite_id_count, 
        &cmd_args_config,
        0,
      },
      {
        "ipmi-chassis-privilege-level", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_privilege_level, 
        1, 
        0,
        &ipmi_chassis_privilege_level_count, 
        &cmd_args_config,
        0,
      },
      {
        "ipmi-chassis-workaround-flags", 
        CONFFILE_OPTION_LIST_STRING, 
        -1,
        config_file_tool_specific_workaround_flags, 
        1, 
        0,
        &ipmi_chassis_workaround_flags_count, 
        &cmd_args_config,
        0
      },
    };

  /* 
   * Ipmi-chassis-config 
   */
  struct conffile_option ipmi_chassis_config_options[] =
    {
      {
        "ipmi-chassis-config-username", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_username, 
        1, 
        0, 
        &ipmi_chassis_config_username_count, 
        &cmd_args_config,
        0,
      },
      {
        "ipmi-chassis-config-password", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_password, 
        1, 
        0, 
        &ipmi_chassis_config_password_count, 
        &cmd_args_config,
        0,
      },
      {
        "ipmi-chassis-config-k_g", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_k_g, 
        1, 
        0, 
        &ipmi_chassis_config_k_g_count, 
        &cmd_args_config,
        0,
      },
      {
        "ipmi-chassis-config-authentication-type", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_authentication_type, 
        1, 
        0, 
        &ipmi_chassis_config_authentication_type_count, 
        &cmd_args_config,
        0,
      },
      {
        "ipmi-chassis-config-cipher-suite-id", 
        CONFFILE_OPTION_INT, 
        -1,
        config_file_tool_specific_cipher_suite_id, 
        1, 
        0,
        &ipmi_chassis_config_cipher_suite_id_count, 
        &cmd_args_config,
        0,
      },
      {
        "ipmi-chassis-config-privilege-level", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_privilege_level, 
        1, 
        0,
        &ipmi_chassis_config_privilege_level_count, 
        &cmd_args_config,
        0,
      },
      {
        "ipmi-chassis-config-workaround-flags", 
        CONFFILE_OPTION_LIST_STRING, 
        -1,
        config_file_tool_specific_workaround_flags, 
        1, 
        0,
        &ipmi_chassis_config_workaround_flags_count, 
        &cmd_args_config,
        0
      },
    };

  /* 
   * Ipmi-fru
   */

  struct conffile_option ipmi_fru_options[] =
    {
      {
        "ipmi-fru-username", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_username, 
        1, 
        0, 
        &ipmi_fru_username_count, 
        &cmd_args_config,
        0,
      },
      {
        "ipmi-fru-password", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_password, 
        1, 
        0, 
        &ipmi_fru_password_count, 
        &cmd_args_config,
        0,
      },
      {
        "ipmi-fru-k_g", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_k_g, 
        1, 
        0, 
        &ipmi_fru_k_g_count, 
        &cmd_args_config,
        0,
      },
      {
        "ipmi-fru-authentication-type", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_authentication_type, 
        1, 
        0, 
        &ipmi_fru_authentication_type_count, 
        &cmd_args_config,
        0,
      },
      {
        "ipmi-fru-cipher-suite-id", 
        CONFFILE_OPTION_INT, 
        -1,
        config_file_tool_specific_cipher_suite_id, 
        1, 
        0,
        &ipmi_fru_cipher_suite_id_count, 
        &cmd_args_config,
        0,
      },
      {
        "ipmi-fru-privilege-level", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_privilege_level, 
        1, 
        0,
        &ipmi_fru_privilege_level_count, 
        &cmd_args_config,
        0,
      },
      {
        "ipmi-fru-workaround-flags", 
        CONFFILE_OPTION_LIST_STRING, 
        -1,
        config_file_tool_specific_workaround_flags, 
        1, 
        0,
        &ipmi_fru_workaround_flags_count, 
        &cmd_args_config,
        0
      },
      {
        "ipmi-fru-skip-checks",
        CONFFILE_OPTION_BOOL,
        -1,
        config_file_bool,
        1,
        0,
        &(ipmi_fru_data.skip_checks_count),
        &(ipmi_fru_data.skip_checks),
        0,
      },
    };

  /* 
   * Ipmi-oem 
   */
  struct conffile_option ipmi_oem_options[] =
    {
      {
        "ipmi-oem-username", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_username, 
        1, 
        0, 
        &ipmi_oem_username_count, 
        &cmd_args_config,
        0,
      },
      {
        "ipmi-oem-password", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_password, 
        1, 
        0, 
        &ipmi_oem_password_count, 
        &cmd_args_config,
        0,
      },
      {
        "ipmi-oem-k_g", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_k_g, 
        1, 
        0, 
        &ipmi_oem_k_g_count, 
        &cmd_args_config,
        0,
      },
      {
        "ipmi-oem-authentication-type", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_authentication_type, 
        1, 
        0, 
        &ipmi_oem_authentication_type_count, 
        &cmd_args_config,
        0,
      },
      {
        "ipmi-oem-cipher-suite-id", 
        CONFFILE_OPTION_INT, 
        -1,
        config_file_tool_specific_cipher_suite_id, 
        1, 
        0,
        &ipmi_oem_cipher_suite_id_count, 
        &cmd_args_config,
        0,
      },
      {
        "ipmi-oem-privilege-level", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_privilege_level, 
        1, 
        0,
        &ipmi_oem_privilege_level_count, 
        &cmd_args_config,
        0,
      },
      {
        "ipmi-oem-workaround-flags", 
        CONFFILE_OPTION_LIST_STRING, 
        -1,
        config_file_tool_specific_workaround_flags, 
        1, 
        0,
        &ipmi_oem_workaround_flags_count, 
        &cmd_args_config,
        0
      },
    };

  /* 
   * Ipmi-raw 
   */
  struct conffile_option ipmi_raw_options[] =
    {
      {
        "ipmi-raw-username", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_username, 
        1, 
        0, 
        &ipmi_raw_username_count, 
        &cmd_args_config,
        0,
      },
      {
        "ipmi-raw-password", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_password, 
        1, 
        0, 
        &ipmi_raw_password_count, 
        &cmd_args_config,
        0,
      },
      {
        "ipmi-raw-k_g", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_k_g, 
        1, 
        0, 
        &ipmi_raw_k_g_count, 
        &cmd_args_config,
        0,
      },
      {
        "ipmi-raw-authentication-type", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_authentication_type, 
        1, 
        0, 
        &ipmi_raw_authentication_type_count, 
        &cmd_args_config,
        0,
      },
      {
        "ipmi-raw-cipher-suite-id", 
        CONFFILE_OPTION_INT, 
        -1,
        config_file_tool_specific_cipher_suite_id, 
        1, 
        0,
        &ipmi_raw_cipher_suite_id_count, 
        &cmd_args_config,
        0,
      },
      {
        "ipmi-raw-privilege-level", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_privilege_level, 
        1, 
        0,
        &ipmi_raw_privilege_level_count, 
        &cmd_args_config,
        0,
      },
      {
        "ipmi-raw-workaround-flags", 
        CONFFILE_OPTION_LIST_STRING, 
        -1,
        config_file_tool_specific_workaround_flags, 
        1, 
        0,
        &ipmi_raw_workaround_flags_count, 
        &cmd_args_config,
        0
      },
    };

  /* 
   * Ipmi-sel 
   */
  struct conffile_option ipmi_sel_options[] =
    {
      {
        "ipmi-sel-username", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_username, 
        1, 
        0, 
        &ipmi_sel_username_count, 
        &cmd_args_config,
        0,
      },
      {
        "ipmi-sel-password", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_password, 
        1, 
        0, 
        &ipmi_sel_password_count, 
        &cmd_args_config,
        0,
      },
      {
        "ipmi-sel-k_g", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_k_g, 
        1, 
        0, 
        &ipmi_sel_k_g_count, 
        &cmd_args_config,
        0,
      },
      {
        "ipmi-sel-authentication-type", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_authentication_type, 
        1, 
        0, 
        &ipmi_sel_authentication_type_count, 
        &cmd_args_config,
        0,
      },
      {
        "ipmi-sel-cipher-suite-id", 
        CONFFILE_OPTION_INT, 
        -1,
        config_file_tool_specific_cipher_suite_id, 
        1, 
        0,
        &ipmi_sel_cipher_suite_id_count, 
        &cmd_args_config,
        0,
      },
      {
        "ipmi-sel-privilege-level", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_privilege_level, 
        1, 
        0,
        &ipmi_sel_privilege_level_count, 
        &cmd_args_config,
        0,
      },
      {
        "ipmi-sel-workaround-flags", 
        CONFFILE_OPTION_LIST_STRING, 
        -1,
        config_file_tool_specific_workaround_flags, 
        1, 
        0,
        &ipmi_sel_workaround_flags_count, 
        &cmd_args_config,
        0
      },
    };

  /* 
   * Ipmi-sensors
   */

  struct conffile_option ipmi_sensors_options[] =
    {
      {
        "ipmi-sensors-username", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_username, 
        1, 
        0, 
        &ipmi_sensors_username_count, 
        &cmd_args_config,
        0,
      },
      {
        "ipmi-sensors-password", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_password, 
        1, 
        0, 
        &ipmi_sensors_password_count, 
        &cmd_args_config,
        0,
      },
      {
        "ipmi-sensors-k_g", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_k_g, 
        1, 
        0, 
        &ipmi_sensors_k_g_count, 
        &cmd_args_config,
        0,
      },
      {
        "ipmi-sensors-authentication-type", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_authentication_type, 
        1, 
        0, 
        &ipmi_sensors_authentication_type_count, 
        &cmd_args_config,
        0,
      },
      {
        "ipmi-sensors-cipher-suite-id", 
        CONFFILE_OPTION_INT, 
        -1,
        config_file_tool_specific_cipher_suite_id, 
        1, 
        0,
        &ipmi_sensors_cipher_suite_id_count, 
        &cmd_args_config,
        0,
      },
      {
        "ipmi-sensors-privilege-level", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_privilege_level, 
        1, 
        0,
        &ipmi_sensors_privilege_level_count, 
        &cmd_args_config,
        0,
      },
      {
        "ipmi-sensors-workaround-flags", 
        CONFFILE_OPTION_LIST_STRING, 
        -1,
        config_file_tool_specific_workaround_flags, 
        1, 
        0,
        &ipmi_sensors_workaround_flags_count, 
        &cmd_args_config,
        0
      },
      {
        "ipmi-sensors-quiet-readings",
        CONFFILE_OPTION_BOOL,
        -1,
        config_file_bool,
        1,
        0,
        &(ipmi_sensors_data.quiet_readings_count),
        &(ipmi_sensors_data.quiet_readings),
        0,
      },
      {
        "ipmi-sensors-groups",
        CONFFILE_OPTION_LIST_STRING,
        -1,
        config_file_ipmi_sensors_groups,
        1,
        0,
        &(ipmi_sensors_data.groups_count),
        &(ipmi_sensors_data),
        0,
      },
      {
        "ipmi-sensors-bridge-sensors",
        CONFFILE_OPTION_BOOL,
        -1,
        config_file_bool,
        1,
        0,
        &(ipmi_sensors_data.bridge_sensors_count),
        &(ipmi_sensors_data.bridge_sensors),
        0,
      },
    };

  /* 
   * Ipmi-sensors-config 
   */
  struct conffile_option ipmi_sensors_config_options[] =
    {
      {
        "ipmi-sensors-config-username", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_username, 
        1, 
        0, 
        &ipmi_sensors_config_username_count, 
        &cmd_args_config,
        0,
      },
      {
        "ipmi-sensors-config-password", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_password, 
        1, 
        0, 
        &ipmi_sensors_config_password_count, 
        &cmd_args_config,
        0,
      },
      {
        "ipmi-sensors-config-k_g", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_k_g, 
        1, 
        0, 
        &ipmi_sensors_config_k_g_count, 
        &cmd_args_config,
        0,
      },
      {
        "ipmi-sensors-config-authentication-type", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_authentication_type, 
        1, 
        0, 
        &ipmi_sensors_config_authentication_type_count, 
        &cmd_args_config,
        0,
      },
      {
        "ipmi-sensors-config-cipher-suite-id", 
        CONFFILE_OPTION_INT, 
        -1,
        config_file_tool_specific_cipher_suite_id, 
        1, 
        0,
        &ipmi_sensors_config_cipher_suite_id_count, 
        &cmd_args_config,
        0,
      },
      {
        "ipmi-sensors-config-privilege-level", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_privilege_level, 
        1, 
        0,
        &ipmi_sensors_config_privilege_level_count, 
        &cmd_args_config,
        0,
      },
      {
        "ipmi-sensors-config-workaround-flags", 
        CONFFILE_OPTION_LIST_STRING, 
        -1,
        config_file_tool_specific_workaround_flags, 
        1, 
        0,
        &ipmi_sensors_config_workaround_flags_count, 
        &cmd_args_config,
        0
      },
    };

  /* 
   * Ipmiconsole
   */

  /* Notes:
   *
   * deactivate option is not useful for config files.
   */

  struct conffile_option ipmiconsole_options[] =
    {
      {
        "ipmiconsole-username", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_username, 
        1, 
        0, 
        &ipmiconsole_username_count, 
        &cmd_args_config,
        0,
      },
      {
        "ipmiconsole-password", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_password, 
        1, 
        0, 
        &ipmiconsole_password_count, 
        &cmd_args_config,
        0,
      },
      {
        "ipmiconsole-k_g", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_k_g, 
        1, 
        0, 
        &ipmiconsole_k_g_count, 
        &cmd_args_config,
        0,
      },
      {
        "ipmiconsole-authentication-type", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_authentication_type, 
        1, 
        0, 
        &ipmiconsole_authentication_type_count, 
        &cmd_args_config,
        0,
      },
      {
        "ipmiconsole-cipher-suite-id", 
        CONFFILE_OPTION_INT, 
        -1,
        config_file_tool_specific_cipher_suite_id, 
        1, 
        0,
        &ipmiconsole_cipher_suite_id_count, 
        &cmd_args_config,
        0,
      },
      {
        "ipmiconsole-privilege-level", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_privilege_level, 
        1, 
        0,
        &ipmiconsole_privilege_level_count, 
        &cmd_args_config,
        0,
      },
      {
        "ipmiconsole-workaround-flags", 
        CONFFILE_OPTION_LIST_STRING, 
        -1,
        config_file_tool_specific_workaround_flags, 
        1, 
        0,
        &ipmiconsole_workaround_flags_count, 
        &cmd_args_config,
        0
      },
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
   * ipmimonitoring
   */

  struct conffile_option ipmimonitoring_options[] =
    {
      {
        "ipmimonitoring-username", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_username, 
        1, 
        0, 
        &ipmimonitoring_username_count, 
        &cmd_args_config,
        0,
      },
      {
        "ipmimonitoring-password", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_password, 
        1, 
        0, 
        &ipmimonitoring_password_count, 
        &cmd_args_config,
        0,
      },
      {
        "ipmimonitoring-k_g", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_k_g, 
        1, 
        0, 
        &ipmimonitoring_k_g_count, 
        &cmd_args_config,
        0,
      },
      {
        "ipmimonitoring-authentication-type", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_authentication_type, 
        1, 
        0, 
        &ipmimonitoring_authentication_type_count, 
        &cmd_args_config,
        0,
      },
      {
        "ipmimonitoring-cipher-suite-id", 
        CONFFILE_OPTION_INT, 
        -1,
        config_file_tool_specific_cipher_suite_id, 
        1, 
        0,
        &ipmimonitoring_cipher_suite_id_count, 
        &cmd_args_config,
        0,
      },
      {
        "ipmimonitoring-privilege-level", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_privilege_level, 
        1, 
        0,
        &ipmimonitoring_privilege_level_count, 
        &cmd_args_config,
        0,
      },
      {
        "ipmimonitoring-workaround-flags", 
        CONFFILE_OPTION_LIST_STRING, 
        -1,
        config_file_tool_specific_workaround_flags, 
        1, 
        0,
        &ipmimonitoring_workaround_flags_count, 
        &cmd_args_config,
        0
      },
      {
        "ipmimonitoring-quiet-readings",
        CONFFILE_OPTION_BOOL,
        -1,
        config_file_bool,
        1,
        0,
        &(ipmimonitoring_data.quiet_readings_count),
        &(ipmimonitoring_data.quiet_readings),
        0,
      },
      {
        "ipmimonitoring-groups",
        CONFFILE_OPTION_LIST_STRING,
        -1,
        config_file_ipmimonitoring_groups,
        1,
        0,
        &(ipmimonitoring_data.groups_count),
        &(ipmimonitoring_data),
        0,
      },
      {
        "ipmimonitoring-bridge-sensors",
        CONFFILE_OPTION_BOOL,
        -1,
        config_file_bool,
        1,
        0,
        &(ipmimonitoring_data.bridge_sensors_count),
        &(ipmimonitoring_data.bridge_sensors),
        0,
      },
      {
        "ipmimonitoring-sensor-config-file", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_string, 
        1, 
        0,
        &(ipmimonitoring_data.sensor_config_file_count),
        &(ipmimonitoring_data.sensor_config_file),
        0
      },
    };

  /* 
   * Ipmipower
   */

  struct conffile_option ipmipower_options[] =
    {
      {
        "ipmipower-username", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_username, 
        1, 
        0, 
        &ipmipower_username_count, 
        &cmd_args_config,
        0,
      },
      {
        "ipmipower-password", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_password, 
        1, 
        0, 
        &ipmipower_password_count, 
        &cmd_args_config,
        0,
      },
      {
        "ipmipower-k_g", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_k_g, 
        1, 
        0, 
        &ipmipower_k_g_count, 
        &cmd_args_config,
        0,
      },
      {
        "ipmipower-authentication-type", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_authentication_type, 
        1, 
        0, 
        &ipmipower_authentication_type_count, 
        &cmd_args_config,
        0,
      },
      {
        "ipmipower-cipher-suite-id", 
        CONFFILE_OPTION_INT, 
        -1,
        config_file_tool_specific_cipher_suite_id, 
        1, 
        0,
        &ipmipower_cipher_suite_id_count, 
        &cmd_args_config,
        0,
      },
      {
        "ipmipower-privilege-level", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_privilege_level, 
        1, 
        0,
        &ipmipower_privilege_level_count, 
        &cmd_args_config,
        0,
      },
      {
        "ipmipower-workaround-flags", 
        CONFFILE_OPTION_LIST_STRING, 
        -1,
        config_file_tool_specific_workaround_flags, 
        1, 
        0,
        &ipmipower_workaround_flags_count, 
        &cmd_args_config,
        0
      },
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

  /* 
   * Pef-config
   */
  struct conffile_option pef_config_options[] =
    {
      {
        "pef-config-username", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_username, 
        1, 
        0, 
        &pef_config_username_count, 
        &cmd_args_config,
        0,
      },
      {
        "pef-config-password", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_password, 
        1, 
        0, 
        &pef_config_password_count, 
        &cmd_args_config,
        0,
      },
      {
        "pef-config-k_g", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_k_g, 
        1, 
        0, 
        &pef_config_k_g_count, 
        &cmd_args_config,
        0,
      },
      {
        "pef-config-authentication-type", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_authentication_type, 
        1, 
        0, 
        &pef_config_authentication_type_count, 
        &cmd_args_config,
        0,
      },
      {
        "pef-config-cipher-suite-id", 
        CONFFILE_OPTION_INT, 
        -1,
        config_file_tool_specific_cipher_suite_id, 
        1, 
        0,
        &pef_config_cipher_suite_id_count, 
        &cmd_args_config,
        0,
      },
      {
        "pef-config-privilege-level", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_tool_specific_privilege_level, 
        1, 
        0,
        &pef_config_privilege_level_count, 
        &cmd_args_config,
        0,
      },
      {
        "pef-config-workaround-flags", 
        CONFFILE_OPTION_LIST_STRING, 
        -1,
        config_file_tool_specific_workaround_flags, 
        1, 
        0,
        &pef_config_workaround_flags_count, 
        &cmd_args_config,
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
         && (((tool_support & CONFIG_FILE_TOOL_BMC_CONFIG) && !tool_data)
             || ((tool_support & CONFIG_FILE_TOOL_BMC_DEVICE) && !tool_data)
             || ((tool_support & CONFIG_FILE_TOOL_BMC_INFO) && !tool_data)
             || ((tool_support & CONFIG_FILE_TOOL_BMC_WATCHDOG) && tool_data)
             || ((tool_support & CONFIG_FILE_TOOL_IPMI_CHASSIS) && !tool_data)
             || ((tool_support & CONFIG_FILE_TOOL_IPMI_CHASSIS_CONFIG) && !tool_data)
             || ((tool_support & CONFIG_FILE_TOOL_IPMI_FRU) && tool_data)
             || ((tool_support & CONFIG_FILE_TOOL_IPMI_OEM) && !tool_data)
             || ((tool_support & CONFIG_FILE_TOOL_IPMI_RAW) && !tool_data)
             || ((tool_support & CONFIG_FILE_TOOL_IPMI_SEL) && !tool_data)
             || ((tool_support & CONFIG_FILE_TOOL_IPMI_SENSORS) && tool_data)
             || ((tool_support & CONFIG_FILE_TOOL_IPMI_SENSORS_CONFIG) && !tool_data)
             || ((tool_support & CONFIG_FILE_TOOL_IPMICONSOLE) && tool_data)
             || ((tool_support & CONFIG_FILE_TOOL_IPMIMONITORING) && tool_data)
             || ((tool_support & CONFIG_FILE_TOOL_IPMIPOWER) && tool_data)
             || ((tool_support & CONFIG_FILE_TOOL_PEF_CONFIG) && !tool_data)));

  memset(config_file_options, '\0', sizeof(struct conffile_option));

  memset(&cmd_args_config, '\0', sizeof(struct cmd_args_config));

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

  options_len = sizeof(bmc_config_options)/sizeof(struct conffile_option);
  if (!(tool_support & CONFIG_FILE_TOOL_BMC_CONFIG))
    _ignore_options(bmc_config_options, options_len);
  
  _copy_options(config_file_options,
                config_file_options_len,
                bmc_config_options,
                options_len);

  config_file_options_len += options_len;

  options_len = sizeof(bmc_device_options)/sizeof(struct conffile_option);
  if (!(tool_support & CONFIG_FILE_TOOL_BMC_DEVICE))
    _ignore_options(bmc_device_options, options_len);
  
  _copy_options(config_file_options,
                config_file_options_len,
                bmc_device_options,
                options_len);

  config_file_options_len += options_len;

  options_len = sizeof(bmc_info_options)/sizeof(struct conffile_option);
  if (!(tool_support & CONFIG_FILE_TOOL_BMC_INFO))
    _ignore_options(bmc_info_options, options_len);
  
  _copy_options(config_file_options,
                config_file_options_len,
                bmc_info_options,
                options_len);

  config_file_options_len += options_len;

  options_len = sizeof(bmc_watchdog_options)/sizeof(struct conffile_option);
  if (!(tool_support & CONFIG_FILE_TOOL_BMC_WATCHDOG))
    _ignore_options(bmc_watchdog_options, options_len);
  
  _copy_options(config_file_options,
                config_file_options_len,
                bmc_watchdog_options,
                options_len);

  config_file_options_len += options_len;

  options_len = sizeof(ipmi_chassis_options)/sizeof(struct conffile_option);
  if (!(tool_support & CONFIG_FILE_TOOL_IPMI_CHASSIS))
    _ignore_options(ipmi_chassis_options, options_len);
  
  _copy_options(config_file_options,
                config_file_options_len,
                ipmi_chassis_options,
                options_len);

  config_file_options_len += options_len;

  options_len = sizeof(ipmi_chassis_config_options)/sizeof(struct conffile_option);
  if (!(tool_support & CONFIG_FILE_TOOL_IPMI_CHASSIS_CONFIG))
    _ignore_options(ipmi_chassis_config_options, options_len);
  
  _copy_options(config_file_options,
                config_file_options_len,
                ipmi_chassis_config_options,
                options_len);

  config_file_options_len += options_len;

  options_len = sizeof(ipmi_fru_options)/sizeof(struct conffile_option);
  if (!(tool_support & CONFIG_FILE_TOOL_IPMI_FRU))
    _ignore_options(ipmi_fru_options, options_len);
  
  _copy_options(config_file_options,
                config_file_options_len,
                ipmi_fru_options,
                options_len);

  config_file_options_len += options_len;

  options_len = sizeof(ipmi_oem_options)/sizeof(struct conffile_option);
  if (!(tool_support & CONFIG_FILE_TOOL_IPMI_OEM))
    _ignore_options(ipmi_oem_options, options_len);
  
  _copy_options(config_file_options,
                config_file_options_len,
                ipmi_oem_options,
                options_len);

  config_file_options_len += options_len;

  options_len = sizeof(ipmi_raw_options)/sizeof(struct conffile_option);
  if (!(tool_support & CONFIG_FILE_TOOL_IPMI_RAW))
    _ignore_options(ipmi_raw_options, options_len);
  
  _copy_options(config_file_options,
                config_file_options_len,
                ipmi_raw_options,
                options_len);

  config_file_options_len += options_len;

  options_len = sizeof(ipmi_sel_options)/sizeof(struct conffile_option);
  if (!(tool_support & CONFIG_FILE_TOOL_IPMI_SEL))
    _ignore_options(ipmi_sel_options, options_len);
  
  _copy_options(config_file_options,
                config_file_options_len,
                ipmi_sel_options,
                options_len);

  config_file_options_len += options_len;

  options_len = sizeof(ipmi_sensors_options)/sizeof(struct conffile_option);
  if (!(tool_support & CONFIG_FILE_TOOL_IPMI_SENSORS))
    _ignore_options(ipmi_sensors_options, options_len);
  
  _copy_options(config_file_options,
                config_file_options_len,
                ipmi_sensors_options,
                options_len);

  config_file_options_len += options_len;

  options_len = sizeof(ipmi_sensors_config_options)/sizeof(struct conffile_option);
  if (!(tool_support & CONFIG_FILE_TOOL_IPMI_SENSORS_CONFIG))
    _ignore_options(ipmi_sensors_config_options, options_len);
  
  _copy_options(config_file_options,
                config_file_options_len,
                ipmi_sensors_config_options,
                options_len);

  config_file_options_len += options_len;

  options_len = sizeof(ipmiconsole_options)/sizeof(struct conffile_option);
  if (!(tool_support & CONFIG_FILE_TOOL_IPMICONSOLE))
    _ignore_options(ipmiconsole_options, options_len);
  
  _copy_options(config_file_options,
                config_file_options_len,
                ipmiconsole_options,
                options_len);

  config_file_options_len += options_len;

  options_len = sizeof(ipmimonitoring_options)/sizeof(struct conffile_option);
  if (!(tool_support & CONFIG_FILE_TOOL_IPMIMONITORING))
    _ignore_options(ipmimonitoring_options, options_len);
  
  _copy_options(config_file_options,
                config_file_options_len,
                ipmimonitoring_options,
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
  
  options_len = sizeof(pef_config_options)/sizeof(struct conffile_option);
  if (!(tool_support & CONFIG_FILE_TOOL_PEF_CONFIG))
    _ignore_options(pef_config_options, options_len);
  
  _copy_options(config_file_options,
                config_file_options_len,
                pef_config_options,
                options_len);

  config_file_options_len += options_len;

  /* clear out config file data */

  memset(&bmc_watchdog_data, '\0', sizeof(struct config_file_data_bmc_watchdog));
  memset(&ipmi_fru_data, '\0', sizeof(struct config_file_data_ipmi_fru));
  memset(&ipmi_sensors_data, '\0', sizeof(struct config_file_data_ipmi_sensors));
  memset(&ipmiconsole_data, '\0', sizeof(struct config_file_data_ipmiconsole));
  memset(&ipmimonitoring_data, '\0', sizeof(struct config_file_data_ipmimonitoring));
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

  if (cmd_args_config.username_set)
    cmd_args->username = cmd_args_config.username;

  if (cmd_args_config.password_set)
    cmd_args->password = cmd_args_config.password;
  
  if (cmd_args_config.k_g_set)
    {
      memcpy(cmd_args->k_g, cmd_args_config.k_g, IPMI_MAX_K_G_LENGTH);
      cmd_args->k_g_len = cmd_args_config.k_g_len;
    }

  if (cmd_args_config.authentication_type_set)
    cmd_args->authentication_type = cmd_args_config.authentication_type;

  if (cmd_args_config.cipher_suite_id_set)
    cmd_args->cipher_suite_id = cmd_args_config.cipher_suite_id;

  if (cmd_args_config.privilege_level_set)
    cmd_args->privilege_level = cmd_args_config.privilege_level;

  if (cmd_args_config.workaround_flags_set)
    cmd_args->workaround_flags = cmd_args_config.workaround_flags;
         
  /* copy tool specific stuff */

  if (tool_support & CONFIG_FILE_TOOL_BMC_WATCHDOG)
    {
      bmc_watchdog_data_ptr = (struct config_file_data_bmc_watchdog *)tool_data;
      memcpy(bmc_watchdog_data_ptr, 
             &bmc_watchdog_data,
             sizeof(struct config_file_data_bmc_watchdog));
    }
  else if (tool_support & CONFIG_FILE_TOOL_IPMI_FRU)
    {
      ipmi_fru_data_ptr = (struct config_file_data_ipmi_fru *)tool_data;
      memcpy(ipmi_fru_data_ptr, 
             &ipmi_fru_data,
             sizeof(struct config_file_data_ipmi_fru));
    }
  else if (tool_support & CONFIG_FILE_TOOL_IPMI_SENSORS)
    {
      ipmi_sensors_data_ptr = (struct config_file_data_ipmi_sensors *)tool_data;
      memcpy(ipmi_sensors_data_ptr, 
             &ipmi_sensors_data,
             sizeof(struct config_file_data_ipmi_sensors));
    }
  else if (tool_support & CONFIG_FILE_TOOL_IPMICONSOLE)
    {
      ipmiconsole_data_ptr = (struct config_file_data_ipmiconsole *)tool_data;
      memcpy(ipmiconsole_data_ptr, 
             &ipmiconsole_data,
             sizeof(struct config_file_data_ipmiconsole));
    }
  else if (tool_support & CONFIG_FILE_TOOL_IPMIMONITORING)
    {
      ipmimonitoring_data_ptr = (struct config_file_data_ipmimonitoring *)tool_data;
      memcpy(ipmimonitoring_data_ptr, 
             &ipmimonitoring_data,
             sizeof(struct config_file_data_ipmimonitoring));
    }
  else if (tool_support & CONFIG_FILE_TOOL_IPMIPOWER)
    {
      ipmipower_data_ptr = (struct config_file_data_ipmipower *)tool_data;
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
