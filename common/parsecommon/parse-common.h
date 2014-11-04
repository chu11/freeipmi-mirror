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

#ifndef PARSE_COMMON_H
#define PARSE_COMMON_H

#if HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <freeipmi/freeipmi.h>

#define IPMI_PARSE_DEVICE_LAN_STR       "lan"
#define IPMI_PARSE_DEVICE_LAN_2_0_STR   "lan_2_0"
#define IPMI_PARSE_DEVICE_LAN_2_0_STR2  "lan20"
#define IPMI_PARSE_DEVICE_LAN_2_0_STR3  "lan_20"
#define IPMI_PARSE_DEVICE_LAN_2_0_STR4  "lan2_0"
#define IPMI_PARSE_DEVICE_LAN_2_0_STR5  "lanplus"
#define IPMI_PARSE_DEVICE_KCS_STR       "kcs"
#define IPMI_PARSE_DEVICE_SSIF_STR      "ssif"
#define IPMI_PARSE_DEVICE_OPENIPMI_STR  "openipmi"
#define IPMI_PARSE_DEVICE_OPENIPMI_STR2 "open"
#define IPMI_PARSE_DEVICE_SUNBMC_STR    "sunbmc"
#define IPMI_PARSE_DEVICE_SUNBMC_STR2   "bmc"
#define IPMI_PARSE_DEVICE_INTELDCMI_STR "inteldcmi"

#define IPMI_PARSE_WORKAROUND_FLAGS_DEFAULT                                       0x00000000

#define IPMI_PARSE_WORKAROUND_FLAGS_NONE                                          IPMI_PARSE_WORKAROUND_FLAGS_DEFAULT

#define IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_AUTHENTICATION_CAPABILITIES         0x00000001
#define IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_ACCEPT_SESSION_ID_ZERO              0x00000002
#define IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_FORCE_PERMSG_AUTHENTICATION         0x00000004
#define IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_CHECK_UNEXPECTED_AUTHCODE           0x00000008
#define IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_BIG_ENDIAN_SEQUENCE_NUMBER          0x00000010
#define IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_NO_AUTH_CODE_CHECK                  0x00000020
#define IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_NO_CHECKSUM_CHECK                   0x00000040

#define IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_AUTHENTICATION_CAPABILITIES     0x00000001
#define IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_INTEL_2_0_SESSION               0x00000002
#define IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_SUPERMICRO_2_0_SESSION          0x00000004
#define IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_SUN_2_0_SESSION                 0x00000008
#define IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_OPEN_SESSION_PRIVILEGE          0x00000010
#define IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_NON_EMPTY_INTEGRITY_CHECK_VALUE 0x00000020
#define IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_NO_CHECKSUM_CHECK               0x00000040

#define IPMI_PARSE_WORKAROUND_FLAGS_INBAND_ASSUME_IO_BASE_ADDRESS                 0x00000001
#define IPMI_PARSE_WORKAROUND_FLAGS_INBAND_SPIN_POLL                              0x00000002

#define IPMI_PARSE_WORKAROUND_FLAGS_SDR_ASSUME_MAX_SDR_RECORD_COUNT               0x00000001

#define IPMI_PARSE_WORKAROUND_FLAGS_NONE_STR                                          "none"

#define IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_AUTHENTICATION_CAPABILITIES_STR         "authcap"
#define IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_ACCEPT_SESSION_ID_ZERO_STR              "idzero"
#define IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_FORCE_PERMSG_AUTHENTICATION_STR         "forcepermsg"
#define IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_CHECK_UNEXPECTED_AUTHCODE_STR           "unexpectedauth"
#define IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_BIG_ENDIAN_SEQUENCE_NUMBER_STR          "endianseq"
#define IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_NO_AUTH_CODE_CHECK_STR                  "noauthcodecheck"
#define IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_NO_CHECKSUM_CHECK_STR                   "nochecksumcheck"

#define IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_AUTHENTICATION_CAPABILITIES_STR     "authcap"
#define IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_INTEL_2_0_SESSION_STR               "intel20"
#define IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_SUPERMICRO_2_0_SESSION_STR          "supermicro20"
#define IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_SUN_2_0_SESSION_STR                 "sun20"
#define IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_OPEN_SESSION_PRIVILEGE_STR          "opensesspriv"
#define IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_NON_EMPTY_INTEGRITY_CHECK_VALUE_STR "integritycheckvalue"
#define IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_NO_CHECKSUM_CHECK_STR               "nochecksumcheck"

#define IPMI_PARSE_WORKAROUND_FLAGS_INBAND_ASSUME_IO_BASE_ADDRESS_STR                 "assumeio"
#define IPMI_PARSE_WORKAROUND_FLAGS_INBAND_SPIN_POLL_STR                              "spinpoll"

#define IPMI_PARSE_WORKAROUND_FLAGS_SDR_ASSUME_MAX_SDR_RECORD_COUNT_STR               "assumemaxsdrrecordcount"

/* ipmiconsole */
#define IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_IGNORE_SOL_PAYLOAD_SIZE        0x00000001
#define IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_IGNORE_SOL_PORT                0x00000002
#define IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_SKIP_SOL_ACTIVATION_STATUS     0x00000004
#define IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_SKIP_CHANNEL_PAYLOAD_SUPPORT   0x00000008
#define IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_SERIAL_ALERTS_DEFERRED         0x00000010
#define IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_INCREMENT_SOL_PACKET_SEQUENCE  0x00000020

/* ipmi-fru */
#define IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_SKIP_CHECKS                    0x00000040

/* ipmi-sel */
#define IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_ASSUME_SYSTEM_EVENT            0x00000080

/* ipmi-sensors */
#define IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_DISCRETE_READING               0x00000100
#define IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_IGNORE_SCANNING_DISABLED       0x00000200
#define IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_ASSUME_BMC_OWNER               0x00000400
#define IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_IGNORE_AUTH_CODE               0x00000800

/* ipmi-config */
#define IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_SLOW_COMMIT                    0x00001000
#define IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_VERY_SLOW_COMMIT               0x00002000
#define IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_SOL_CHANNEL_ASSUME_LAN_CHANNEL 0x00004000

/* bmc-watchdog */
#define IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_IGNORE_STATE_FLAG              0x00008000

/* ipmi-pet */
#define IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_MALFORMED_ACK                  0x0001000

/* bmc-info */
#define IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_GUID_FORMAT                    0x0002000

/* ipmipower */
#define IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_IPMIPING                       0x0004000

#define IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_IGNORE_SOL_PAYLOAD_SIZE_STR        "solpayloadsize"
#define IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_IGNORE_SOL_PORT_STR                "solport"
#define IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_SKIP_SOL_ACTIVATION_STATUS_STR     "solstatus"
#define IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_SKIP_CHANNEL_PAYLOAD_SUPPORT_STR   "solchannelsupport"
#define IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_SERIAL_ALERTS_DEFERRED_STR         "serialalertsdeferred"
#define IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_INCREMENT_SOL_PACKET_SEQUENCE_STR  "solpacketseq"

#define IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_SKIP_CHECKS_STR                    "skipchecks"
#define IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_ASSUME_SYSTEM_EVENT_STR            "assumesystemevent"
#define IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_DISCRETE_READING_STR               "discretereading"
#define IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_IGNORE_SCANNING_DISABLED_STR       "ignorescanningdisabled"
#define IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_ASSUME_BMC_OWNER_STR               "assumebmcowner"
#define IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_IGNORE_AUTH_CODE_STR               "ignoreauthcode"
#define IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_SLOW_COMMIT_STR                    "slowcommit"
#define IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_VERY_SLOW_COMMIT_STR               "veryslowcommit"
#define IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_SOL_CHANNEL_ASSUME_LAN_CHANNEL_STR "solchannelassumelanchannel"
#define IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_IGNORE_STATE_FLAG_STR              "ignorestateflag"
#define IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_MALFORMED_ACK_STR                  "malformedack"
#define IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_GUID_FORMAT_STR                    "guidformat"
#define IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_IPMIPING_STR                       "ipmiping"

#define IPMI_PARSE_AUTHENTICATION_TYPE_NONE_STR                               "none"
#define IPMI_PARSE_AUTHENTICATION_TYPE_STRAIGHT_PASSWORD_KEY_STR              "straight_password_key"
#define IPMI_PARSE_AUTHENTICATION_TYPE_STRAIGHT_PASSWORD_KEY_STR2             "plain"
#define IPMI_PARSE_AUTHENTICATION_TYPE_MD2_STR                                "md2"
#define IPMI_PARSE_AUTHENTICATION_TYPE_MD5_STR                                "md5"

#define IPMI_PARSE_PRIVILEGE_LEVEL_USER_STR                                   "user"
#define IPMI_PARSE_PRIVILEGE_LEVEL_OPERATOR_STR                               "operator"
#define IPMI_PARSE_PRIVILEGE_LEVEL_ADMIN_STR                                  "admin"
#define IPMI_PARSE_PRIVILEGE_LEVEL_ADMIN_STR2                                 "administrator"

/* returns driver type on success, -1 on error */
int parse_inband_driver_type (const char *str);

/* returns driver type on success, -1 on error */
int parse_outofband_driver_type (const char *str);

/* returns driver type on success, -1 on error */
int parse_driver_type (const char *str);

/* returns authentication type on success, -1 on error */
int parse_authentication_type (const char *str);

/* returns privilege level on success, -1 on error */
int parse_privilege_level (const char *str);

/* returns 0 on success, -1 on error, flags set in in/out parameters */
int parse_workaround_flags (const char *str,
                            unsigned int *workaround_flags_outofband,
                            unsigned int *workaround_flags_outofband_2_0,
                            unsigned int *workaround_flags_inband,
			    unsigned int *workaround_flags_sdr,
                            unsigned int *tool_specific_workaround_flags);

/* returns 0 on success, -1 on error, flags set in in/out parameters */
/* specifically should be called by tools for user input situations and not libraries */
int parse_workaround_flags_tool (const char *str,
				 unsigned int *workaround_flags_outofband,
				 unsigned int *workaround_flags_outofband_2_0,
				 unsigned int *workaround_flags_inband,
				 unsigned int *workaround_flags_sdr,
				 unsigned int *tool_specific_workaround_flags);

/* Turn an input string into a 20-byte binary k_g key, length written
 *  into out on success, -1 on error
 */
int parse_kg (void *out, unsigned int outlen, const char *in);

void parse_get_freeipmi_outofband_flags (unsigned int parse_workaround_flags_outofband,
					 unsigned int *freeipmi_workaround_flags_outofband);

void parse_get_freeipmi_outofband_2_0_flags (unsigned int parse_workaround_flags_outofband_2_0,
					     unsigned int *freeipmi_workaround_flags_outofband_2_0);

void parse_get_freeipmi_inband_flags (unsigned int parse_workaround_flags_inband,
				      unsigned int *freeipmi_workaround_flags_inband);

#endif /* PARSE_COMMON_H */
