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

#ifndef _CONFIG_TOOL_COMMON_H_
#define _CONFIG_TOOL_COMMON_H_

#include <stdio.h>

#include "tool-cmdline-common.h"

#define IPMI_CTX_ERRNUM_IS_FATAL_ERROR(__ipmi_ctx) \
(((ipmi_ctx_errnum((__ipmi_ctx))) == IPMI_ERR_CTX_NULL \
  || (ipmi_ctx_errnum((__ipmi_ctx))) == IPMI_ERR_CTX_INVALID \
  || (ipmi_ctx_errnum((__ipmi_ctx))) == IPMI_ERR_PERMISSION \
  || (ipmi_ctx_errnum((__ipmi_ctx))) == IPMI_ERR_USERNAME_INVALID \
  || (ipmi_ctx_errnum((__ipmi_ctx))) == IPMI_ERR_PASSWORD_INVALID \
  || (ipmi_ctx_errnum((__ipmi_ctx))) == IPMI_ERR_K_G_INVALID \
  || (ipmi_ctx_errnum((__ipmi_ctx))) == IPMI_ERR_AUTHENTICATION_TYPE_UNAVAILABLE \
  || (ipmi_ctx_errnum((__ipmi_ctx))) == IPMI_ERR_CIPHER_SUITE_ID_UNAVAILABLE \
  || (ipmi_ctx_errnum((__ipmi_ctx))) == IPMI_ERR_PASSWORD_VERIFICATION_TIMEOUT \
  || (ipmi_ctx_errnum((__ipmi_ctx))) == IPMI_ERR_IPMI_2_0_UNAVAILABLE \
  || (ipmi_ctx_errnum((__ipmi_ctx))) == IPMI_ERR_CONNECTION_TIMEOUT \
  || (ipmi_ctx_errnum((__ipmi_ctx))) == IPMI_ERR_SESSION_TIMEOUT \
  || (ipmi_ctx_errnum((__ipmi_ctx))) == IPMI_ERR_DEVICE_ALREADY_OPEN \
  || (ipmi_ctx_errnum((__ipmi_ctx))) == IPMI_ERR_DEVICE_NOT_OPEN \
  || (ipmi_ctx_errnum((__ipmi_ctx))) == IPMI_ERR_DEVICE_NOT_SUPPORTED \
  || (ipmi_ctx_errnum((__ipmi_ctx))) == IPMI_ERR_DEVICE_NOT_FOUND \
  || (ipmi_ctx_errnum((__ipmi_ctx))) == IPMI_ERR_DRIVER_TIMEOUT \
  || (ipmi_ctx_errnum((__ipmi_ctx))) == IPMI_ERR_COMMAND_INVALID_FOR_SELECTED_INTERFACE \
  || (ipmi_ctx_errnum((__ipmi_ctx))) == IPMI_ERR_BAD_RMCPPLUS_STATUS_CODE \
  || (ipmi_ctx_errnum((__ipmi_ctx))) == IPMI_ERR_OUT_OF_MEMORY \
  || (ipmi_ctx_errnum((__ipmi_ctx))) == IPMI_ERR_HOSTNAME_INVALID \
  || (ipmi_ctx_errnum((__ipmi_ctx))) == IPMI_ERR_PARAMETERS \
  || (ipmi_ctx_errnum((__ipmi_ctx))) == IPMI_ERR_DRIVER_PATH_REQUIRED \
  || (ipmi_ctx_errnum((__ipmi_ctx))) == IPMI_ERR_IPMI_ERROR \
  || (ipmi_ctx_errnum((__ipmi_ctx))) == IPMI_ERR_SYSTEM_ERROR \
  || (ipmi_ctx_errnum((__ipmi_ctx))) == IPMI_ERR_LIBRARY_ERROR \
  || (ipmi_ctx_errnum((__ipmi_ctx))) == IPMI_ERR_INTERNAL_ERROR) ? 1 : 0)

#define CONFIG_CHECKOUT_KEY_COMMENTED_OUT                       0x01
#define CONFIG_CHECKOUT_KEY_COMMENTED_OUT_IF_VALUE_EMPTY        0x02
#define CONFIG_DO_NOT_CHECKOUT                                  0x04
#define CONFIG_READABLE_ONLY                                    0x08
#define CONFIG_UNDEFINED                                        0x10
#define CONFIG_USERNAME_NOT_SET_YET                             0x20

#define CONFIG_USERNAME_NOT_SET_YET_STR     "<username-not-set-yet>"

#define CONFIG_CHECKOUT_LINE_LEN                           45

#define CONFIG_PARSE_BUFLEN                                4096
#define CONFIG_OUTPUT_BUFLEN                               8192

#define CONFIG_MAX_SECTION_NAME_LEN                        128
#define CONFIG_MAX_DESCRIPTION_LEN                         1024

#define SET_SELECTOR      0x0
#define BLOCK_SELECTOR    0x0

#define same(a,b) (strcasecmp(a,b) == 0)

typedef enum
  {
    CONFIG_ACTION_CHECKOUT = 1,
    CONFIG_ACTION_COMMIT,
    CONFIG_ACTION_DIFF,
    CONFIG_ACTION_LIST_SECTIONS,
  } config_action_t;

typedef enum
  {
    CONFIG_ERR_FATAL_ERROR = -2,
    CONFIG_ERR_NON_FATAL_ERROR = -1,
    CONFIG_ERR_SUCCESS = 0,
  } config_err_t;

typedef enum
  {
    CONFIG_VALIDATE_FATAL_ERROR = -4,
    CONFIG_VALIDATE_NON_FATAL_ERROR = -3,
    CONFIG_VALIDATE_OUT_OF_RANGE_VALUE = -2,
    CONFIG_VALIDATE_INVALID_VALUE = -1,
    CONFIG_VALIDATE_VALID_VALUE = 0,
  } config_validate_t;

struct config_keypair
{
  char *section_name;
  char *key_name;
  char *value_input;
  struct config_keypair *next;
};

struct config_section_str
{
  char *section_name;
  struct config_section_str *next;
};

struct config_arguments
{
  struct common_cmd_args common;
  struct hostrange_cmd_args hostrange;

  config_action_t action;

  int verbose;
  char *filename;
  struct config_keypair *keypairs;
  struct config_section_str *section_strs;
};

struct config_keyvalue {
  struct config_key *key;
  char *value_input;
  char *value_output;
  struct config_keyvalue *next;
};

/* Fills in kv->value_output as a printable string  */
typedef config_err_t (*Key_Checkout) (const char *section_name,
                                      struct config_keyvalue *kv,
                                      void *arg);

/* Takes kv->value_input and commits it */
typedef config_err_t (*Key_Commit) (const char *section_name,
                                    const struct config_keyvalue *kv,
                                    void *arg);

/* Determines if an inputted value is valid */
typedef config_validate_t (*Key_Validate) (const char *section_name,
                                           const char *key_name,
                                           const char *value,
                                           void *arg);

/* Sectional pre commit call */
typedef config_err_t (*Section_Pre_Commit) (const char *section_name,
                                            void *arg);

/* Sectional post commit call */
typedef config_err_t (*Section_Post_Commit) (const char *section_name,
                                             void *arg);

struct config_key {
  char *key_name;
  char *description;
  unsigned int flags;
  Key_Checkout checkout;
  Key_Commit commit;
  Key_Validate validate;
  struct config_key *next;
};

struct config_section {
  char *section_name;
  char *section_comment_section_name;
  char *section_comment;
  unsigned int flags;
  Section_Pre_Commit section_pre_commit;
  Section_Post_Commit section_post_commit;
  /* keys in this section */
  struct config_key *keys;
  /* key and values for checkout/commit/diff */
  struct config_keyvalue *keyvalues;
  struct config_section *next;
};

#endif /* _CONFIG_TOOL_COMMON_H_ */
