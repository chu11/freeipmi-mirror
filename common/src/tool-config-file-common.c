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
#include "conffile.h"

#include "freeipmi/api/ipmi-api.h"
#include "freeipmi/cmds/ipmi-messaging-support-cmds.h"
#include "freeipmi/interface/ipmi-rmcpplus-interface.h"
#include "freeipmi/util/ipmi-cipher-suite-util.h"

#define CONFIG_FILE_OPTIONS_MAX 1024

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
      fprintf(stderr, "Config File Error: invalid driver type\n");
      exit(1);
    }
  
  cmd_args->driver_type = tmp;
  return 0;
}

int 
config_file_no_probing(conffile_t cf,
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

  cmd_args->no_probing = data->boolval;
  return 0;
}

int 
config_file_driver_address(conffile_t cf,
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

  if (data->intval < 0)
    {
      fprintf(stderr, "Config File Error: invalid driver address\n");
      exit(1);
    }
    
  cmd_args->driver_address = data->intval;
  return 0;
}

int 
config_file_driver_device(conffile_t cf,
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

  if (!(cmd_args->driver_device = strdup(data->string)))
    {
      perror("strdup");
      exit(1);
    }

  return 0;
}

int 
config_file_register_spacing(conffile_t cf,
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

  if (data->intval <= 0)
    {
      fprintf(stderr, "Config File Error: invalid register spacing\n");
      exit(1);
    }
    
  cmd_args->register_spacing = data->intval;
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
  struct common_cmd_args *cmd_args;

  assert(option_ptr);

  cmd_args = (struct common_cmd_args *)option_ptr;

  if (strlen(data->string) > IPMI_MAX_USER_NAME_LENGTH)
    {
      fprintf (stderr, "Config File Error: username too long\n");
      exit(1);
    }

  if (!(cmd_args->username = strdup(data->string)))
    {
      perror("strdup");
      exit(1);
    }

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
  struct common_cmd_args *cmd_args;

  assert(option_ptr);

  cmd_args = (struct common_cmd_args *)option_ptr;

  if (strlen(data->string) > IPMI_2_0_MAX_PASSWORD_LENGTH)
    {
      fprintf (stderr, "Config File Error: password too long\n");
      exit(1);
    }

  if (!(cmd_args->password = strdup(data->string)))
    {
      perror("strdup");
      exit(1);
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
config_file_session_timeout(conffile_t cf,
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

  if (data->intval <= 0)
    {
      fprintf(stderr, "Config File Error: invalid session timeout\n");
      exit(1);
    }

  cmd_args->session_timeout = data->intval;
  return 0;
}

int 
config_file_retransmission_timeout(conffile_t cf,
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

  if (data->intval <= 0)
    {
      fprintf(stderr, "Config File Error: invalid retransmission timeout\n");
      exit(1);
    }

  cmd_args->retransmission_timeout = data->intval;
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
      fprintf(stderr, "Config File Error: invalid authentication type\n");
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
      fprintf(stderr, "Config File Error: invalid cipher suite id\n");
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
      fprintf(stderr, "Config File Error: invalid privilege level\n");
      exit(1);
    }

  cmd_args->privilege_level = tmp;
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
  int tmp;

  assert(option_ptr);

  cmd_args = (struct common_cmd_args *)option_ptr;

  if ((tmp = parse_workaround_flags(data->string)) < 0)
    {
      fprintf(stderr, "Config File Error: invalid workaround flags\n");
      exit(1);
    }

  cmd_args->workaround_flags = tmp;
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

int
config_file_parse(const char *filename,
                  struct common_cmd_args *cmd_args, 
                  unsigned int support)
{
  struct conffile_option config_file_options[CONFIG_FILE_OPTIONS_MAX];
  unsigned int config_file_options_len = 0;

  int driver_type_count = 0,
    no_probing_count = 0,
    driver_address_count = 0,
    driver_device_count = 0,
    register_spacing_count = 0;

  int username_count = 0,
    password_count = 0,
    k_g_count = 0,
    timeout_count = 0,
    session_timeout_count = 0,
    retry_timeout_count = 0,
    retransmission_timeout_count = 0,
    authentication_type_count = 0,
    u_cipher_suite_id_count = 0,
    cipher_suite_id_count = 0,
    privilege_count = 0,
    privilege_level_count = 0;

  int workaround_flags_count = 0;

  struct conffile_option inband_options[] =
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
        "no-probing",
        CONFFILE_OPTION_BOOL,
        -1,
        config_file_no_probing,
        1,
        0,
        &no_probing_count,
        cmd_args,
        0
      },
      {
        "driver-address",
        CONFFILE_OPTION_INT,
        -1,
        config_file_driver_address,
        1,
        0,
        &driver_address_count,
        cmd_args,
        0
      },
      {
        "driver-device",
        CONFFILE_OPTION_STRING,
        -1,
        config_file_driver_device,
        1,
        0,
        &driver_device_count,
        cmd_args,
        0
      },
      {
        "register-spacing",
        CONFFILE_OPTION_INT,
        -1,
        config_file_register_spacing,
        1,
        0,
        &register_spacing_count,
        cmd_args,
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
        cmd_args, 
        0
      },
      {
        "password", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_password, 
        1,
        0, 
        &password_count,
        cmd_args, 
        0
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
        config_file_session_timeout, 
        1,
        0, 
        &timeout_count,
        cmd_args, 
        0
      },
      {"session-timeout", 
       CONFFILE_OPTION_INT, 
       -1,
       config_file_session_timeout,
       1, 
       0, 
       &session_timeout_count,
       cmd_args, 
       0
      },
      /* retry-timeout maintained for backwards compatability */
      {
        "retry-timeout", 
        CONFFILE_OPTION_INT, 
        -1,
        config_file_retransmission_timeout,
        1,
        0,
        &retry_timeout_count,
        cmd_args, 
        0
      },
      {
        "retransmission-timeout", 
        CONFFILE_OPTION_INT, 
        -1,
        config_file_retransmission_timeout, 
        1,
        0,
        &retransmission_timeout_count,
        cmd_args, 
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
        &u_cipher_suite_id_count,
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
        &privilege_count,
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

  struct conffile_option misc_options[] =
    {
      {
        "workaround-flags", 
        CONFFILE_OPTION_STRING, 
        -1,
        config_file_workaround_flags, 
        1, 
        0,
        &workaround_flags_count,
        cmd_args, 
        0
      }
    };

  conffile_t cf = NULL;
  int rv = -1;

  memset(config_file_options, '\0', sizeof(struct conffile_option));

  /* set ignore options the tool doesn't care about */

  if (!(support & CONFIG_FILE_INBAND))
    _ignore_options(inband_options, sizeof(inband_options)/sizeof(struct conffile_option));
  
  memcpy(config_file_options + config_file_options_len,
         inband_options,
         sizeof(inband_options)/sizeof(struct conffile_option));
  config_file_options_len += sizeof(inband_options)/sizeof(struct conffile_option);

  if (!(support & CONFIG_FILE_OUTOFBAND))
    _ignore_options(outofband_options, sizeof(outofband_options)/sizeof(struct conffile_option));
  
  memcpy(config_file_options + config_file_options_len,
         outofband_options,
         sizeof(outofband_options)/sizeof(struct conffile_option));
  config_file_options_len += sizeof(outofband_options)/sizeof(struct conffile_option);

  if (!(support & CONFIG_FILE_MISC))
    _ignore_options(misc_options, sizeof(misc_options)/sizeof(struct conffile_option));

  memcpy(config_file_options + config_file_options_len,
         misc_options,
         sizeof(misc_options)/sizeof(struct conffile_option));
  config_file_options_len += sizeof(misc_options)/sizeof(struct conffile_option);

  if (!(cf = conffile_handle_create()))
    {
      fprintf(stderr, "conffile_handle_create: %s\n", strerror(errno));
      goto cleanup;
    }

  if (conffile_parse(cf, 
                     filename,
                     config_file_options, 
                     config_file_options_len, 
                     NULL, 
                     0, 
                     0) < 0)
    {
#if 0
      char buf[CONFFILE_MAX_ERRMSGLEN];

      /* Its not an error if the default configuration file doesn't exist */
      if (!strcmp(filename, IPMICONSOLE_CONFIG_FILE_DEFAULT)
          && conffile_errnum(cf) == CONFFILE_ERR_EXIST)
        goto cleanup;
      
      if (conffile_errmsg(cf, buf, CONFFILE_MAX_ERRMSGLEN) < 0)
        err_exit("conffile_parse: %d", conffile_errnum(cf));
      else
        err_exit("conffile_parse: %s", buf);
#endif
    }
  
  rv = 0;
 cleanup:
  if (cf)
    conffile_handle_destroy(cf);
  return rv;
}
