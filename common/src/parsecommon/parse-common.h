/*
 * Copyright (C) 2003-2010 FreeIPMI Core Team
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

#ifndef _PARSE_COMMON_H
#define _PARSE_COMMON_H

#if HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <freeipmi/freeipmi.h>

#define IPMI_TOOL_WORKAROUND_FLAGS_OUTOFBAND_AUTHENTICATION_CAPABILITIES_STR         "authcap"
#define IPMI_TOOL_WORKAROUND_FLAGS_OUTOFBAND_ACCEPT_SESSION_ID_ZERO_STR              "idzero"
#define IPMI_TOOL_WORKAROUND_FLAGS_OUTOFBAND_FORCE_PERMSG_AUTHENTICATION_STR         "forcepermsg"
#define IPMI_TOOL_WORKAROUND_FLAGS_OUTOFBAND_CHECK_UNEXPECTED_AUTHCODE_STR           "unexpectedauth"
#define IPMI_TOOL_WORKAROUND_FLAGS_OUTOFBAND_BIG_ENDIAN_SEQUENCE_NUMBER_STR          "endianseq"

#define IPMI_TOOL_WORKAROUND_FLAGS_OUTOFBAND_2_0_AUTHENTICATION_CAPABILITIES_STR     "authcap"
#define IPMI_TOOL_WORKAROUND_FLAGS_OUTOFBAND_2_0_INTEL_2_0_SESSION_STR               "intel20"
#define IPMI_TOOL_WORKAROUND_FLAGS_OUTOFBAND_2_0_SUPERMICRO_2_0_SESSION_STR          "supermicro20"
#define IPMI_TOOL_WORKAROUND_FLAGS_OUTOFBAND_2_0_SUN_2_0_SESSION_STR                 "sun20"
#define IPMI_TOOL_WORKAROUND_FLAGS_OUTOFBAND_2_0_OPEN_SESSION_PRIVILEGE_STR          "opensesspriv"
#define IPMI_TOOL_WORKAROUND_FLAGS_OUTOFBAND_2_0_NON_EMPTY_INTEGRITY_CHECK_VALUE_STR "integritycheckvalue"

#define IPMI_TOOL_WORKAROUND_FLAGS_INBAND_ASSUME_IO_BASE_ADDRESS_STR                 "assumeio"

#define IPMI_TOOL_SPECIFIC_WORKAROUND_FLAGS_IGNORE_SOL_PAYLOAD_SIZE_STR        "solpayloadsize"
#define IPMI_TOOL_SPECIFIC_WORKAROUND_FLAGS_IGNORE_SOL_PORT_STR                "solport"
#define IPMI_TOOL_SPECIFIC_WORKAROUND_FLAGS_SKIP_SOL_ACTIVATION_STATUS_STR     "solstatus"
#define IPMI_TOOL_SPECIFIC_WORKAROUND_FLAGS_SKIP_CHECKS_STR                    "skipchecks"
#define IPMI_TOOL_SPECIFIC_WORKAROUND_FLAGS_ASSUME_SYSTEM_EVENT_STR            "assumesystemevent"
#define IPMI_TOOL_SPECIFIC_WORKAROUND_FLAGS_SLOW_COMMIT_STR                    "slowcommit"
#define IPMI_TOOL_SPECIFIC_WORKAROUND_FLAGS_VERY_SLOW_COMMIT_STR               "veryslowcommit"
#define IPMI_TOOL_SPECIFIC_WORKAROUND_FLAGS_SOL_CHANNEL_ASSUME_LAN_CHANNEL_STR "solchannelassumelanchannel"
#define IPMI_TOOL_SPECIFIC_WORKAROUND_FLAGS_IGNORE_STATE_FLAG_STR              "ignorestateflag"

#define IPMI_AUTHENTICATION_TYPE_NONE_STR                               "none"
#define IPMI_AUTHENTICATION_TYPE_STRAIGHT_PASSWORD_KEY_STR              "straight_password_key"
#define IPMI_AUTHENTICATION_TYPE_STRAIGHT_PASSWORD_KEY_STR_OLD          "plain"
#define IPMI_AUTHENTICATION_TYPE_MD2_STR                                "md2"
#define IPMI_AUTHENTICATION_TYPE_MD5_STR                                "md5"

#define IPMI_PRIVILEGE_LEVEL_USER_STR                                   "user"
#define IPMI_PRIVILEGE_LEVEL_OPERATOR_STR                               "operator"
#define IPMI_PRIVILEGE_LEVEL_ADMIN_STR                                  "admin"
#define IPMI_PRIVILEGE_LEVEL_ADMIN_STR2                                 "administrator"

int parse_inband_driver_type (const char *str);

int parse_outofband_driver_type (const char *str);

int parse_driver_type (const char *str);

int parse_authentication_type (const char *str);

int parse_privilege_level (const char *str);

int parse_workaround_flags (const char *str,
                            unsigned int *workaround_flags_outofband,
                            unsigned int *workaround_flags_outofband_2_0,
                            unsigned int *workaround_flags_inband,
                            unsigned int *tool_specific_workaround_flags);

#endif
