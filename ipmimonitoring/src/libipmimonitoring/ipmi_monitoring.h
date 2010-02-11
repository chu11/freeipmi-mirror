/*****************************************************************************\
 *  $Id: ipmi_monitoring.h,v 1.62 2010-02-11 21:49:24 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2010 Lawrence Livermore National Security, LLC.
 *  Copyright (C) 2006-2007 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-222073
 *
 *  This file is part of Ipmimonitoring, an IPMI sensor monitoring
 *  library.  For details, see http://www.llnl.gov/linux/.
 *
 *  Ipmimonitoring is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 3 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmimonitoring is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmimonitoring.  If not, see <http://www.gnu.org/licenses/>.
\*****************************************************************************/

#ifndef _IPMI_MONITORING_H
#define _IPMI_MONITORING_H

#ifdef __cplusplus
extern "C" {
#endif

enum ipmi_monitoring_error_codes
  {
    IPMI_MONITORING_ERR_SUCCESS                             = 0,
    IPMI_MONITORING_ERR_CTX_NULL                            = 1,
    IPMI_MONITORING_ERR_CTX_INVALID                         = 2,
    IPMI_MONITORING_ERR_PARAMETERS                          = 3,
    IPMI_MONITORING_ERR_PERMISSION                          = 4,
    IPMI_MONITORING_ERR_LIBRARY_UNINITIALIZED               = 5,
    IPMI_MONITORING_ERR_SENSOR_CONFIG_FILE_DOES_NOT_EXIST   = 6,
    IPMI_MONITORING_ERR_SENSOR_CONFIG_FILE_PARSE            = 7,
    IPMI_MONITORING_ERR_SDR_CACHE_PERMISSION                = 8,
    IPMI_MONITORING_ERR_SDR_CACHE_FILESYSTEM                = 9,
    IPMI_MONITORING_ERR_HOSTNAME_INVALID                    = 10,
    IPMI_MONITORING_ERR_SENSOR_NOT_FOUND                    = 11,
    IPMI_MONITORING_ERR_NO_SENSOR_READINGS                  = 12,
    IPMI_MONITORING_ERR_SENSOR_READINGS_LIST_END            = 13,
    IPMI_MONITORING_ERR_CONNECTION_TIMEOUT                  = 14,
    IPMI_MONITORING_ERR_SESSION_TIMEOUT                     = 15,
    IPMI_MONITORING_ERR_USERNAME_INVALID                    = 16,
    IPMI_MONITORING_ERR_PASSWORD_INVALID                    = 17,
    IPMI_MONITORING_ERR_PASSWORD_VERIFICATION_TIMEOUT       = 18,
    IPMI_MONITORING_ERR_K_G_INVALID                         = 19,
    IPMI_MONITORING_ERR_PRIVILEGE_LEVEL_INSUFFICIENT        = 20,
    IPMI_MONITORING_ERR_PRIVILEGEL_LEVEL_CANNOT_BE_OBTAINED = 21,
    IPMI_MONITORING_ERR_AUTHENTICATION_TYPE_UNAVAILABLE     = 22,
    IPMI_MONITORING_ERR_IPMI_2_0_UNAVAILABLE                = 23,
    IPMI_MONITORING_ERR_CIPHER_SUITE_ID_UNAVAILABLE         = 24,
    IPMI_MONITORING_ERR_CALLBACK_ERROR                      = 25,
    IPMI_MONITORING_ERR_BMC_BUSY                            = 26,
    IPMI_MONITORING_ERR_OUT_OF_MEMORY                       = 27,
    IPMI_MONITORING_ERR_IPMI_ERROR                          = 28,
    IPMI_MONITORING_ERR_SYSTEM_ERROR                        = 29,
    IPMI_MONITORING_ERR_INTERNAL_ERROR                      = 30,
    IPMI_MONITORING_ERR_ERRNUMRANGE                         = 31,
  };

enum ipmi_monitoring_sensor_type
  {
    IPMI_MONITORING_SENSOR_TYPE_RESERVED                            = 0x00,
    IPMI_MONITORING_SENSOR_TYPE_TEMPERATURE                         = 0x01,
    IPMI_MONITORING_SENSOR_TYPE_VOLTAGE                             = 0x02,
    IPMI_MONITORING_SENSOR_TYPE_CURRENT                             = 0x03,
    IPMI_MONITORING_SENSOR_TYPE_FAN                                 = 0x04,
    IPMI_MONITORING_SENSOR_TYPE_PHYSICAL_SECURITY                   = 0x05,
    IPMI_MONITORING_SENSOR_TYPE_PLATFORM_SECURITY_VIOLATION_ATTEMPT = 0x06,
    IPMI_MONITORING_SENSOR_TYPE_PROCESSOR                           = 0x07,
    IPMI_MONITORING_SENSOR_TYPE_POWER_SUPPLY                        = 0x08,
    IPMI_MONITORING_SENSOR_TYPE_POWER_UNIT                          = 0x09,
    IPMI_MONITORING_SENSOR_TYPE_COOLING_DEVICE                      = 0x0A,
    IPMI_MONITORING_SENSOR_TYPE_OTHER_UNITS_BASED_SENSOR            = 0x0B,
    IPMI_MONITORING_SENSOR_TYPE_MEMORY                              = 0x0C,
    IPMI_MONITORING_SENSOR_TYPE_DRIVE_SLOT                          = 0x0D,
    IPMI_MONITORING_SENSOR_TYPE_POST_MEMORY_RESIZE                  = 0x0E,
    IPMI_MONITORING_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS            = 0x0F,
    IPMI_MONITORING_SENSOR_TYPE_EVENT_LOGGING_DISABLED              = 0x10,
    IPMI_MONITORING_SENSOR_TYPE_WATCHDOG1                           = 0x11,
    IPMI_MONITORING_SENSOR_TYPE_SYSTEM_EVENT                        = 0x12,
    IPMI_MONITORING_SENSOR_TYPE_CRITICAL_INTERRUPT                  = 0x13,
    IPMI_MONITORING_SENSOR_TYPE_BUTTON_SWITCH                       = 0x14,
    IPMI_MONITORING_SENSOR_TYPE_MODULE_BOARD                        = 0x15,
    IPMI_MONITORING_SENSOR_TYPE_MICROCONTROLLER_COPROCESSOR         = 0x16,
    IPMI_MONITORING_SENSOR_TYPE_ADD_IN_CARD                         = 0x17,
    IPMI_MONITORING_SENSOR_TYPE_CHASSIS                             = 0x18,
    IPMI_MONITORING_SENSOR_TYPE_CHIP_SET                            = 0x19,
    IPMI_MONITORING_SENSOR_TYPE_OTHER_FRU                           = 0x1A,
    IPMI_MONITORING_SENSOR_TYPE_CABLE_INTERCONNECT                  = 0x1B,
    IPMI_MONITORING_SENSOR_TYPE_TERMINATOR                          = 0x1C,
    IPMI_MONITORING_SENSOR_TYPE_SYSTEM_BOOT_INITIATED               = 0x1D,
    IPMI_MONITORING_SENSOR_TYPE_BOOT_ERROR                          = 0x1E,
    IPMI_MONITORING_SENSOR_TYPE_OS_BOOT                             = 0x1F,
    IPMI_MONITORING_SENSOR_TYPE_OS_CRITICAL_STOP                    = 0x20,
    IPMI_MONITORING_SENSOR_TYPE_SLOT_CONNECTOR                      = 0x21,
    IPMI_MONITORING_SENSOR_TYPE_SYSTEM_ACPI_POWER_STATE             = 0x22,
    IPMI_MONITORING_SENSOR_TYPE_WATCHDOG2                           = 0x23,
    IPMI_MONITORING_SENSOR_TYPE_PLATFORM_ALERT                      = 0x24,
    IPMI_MONITORING_SENSOR_TYPE_ENTITY_PRESENCE                     = 0x25,
    IPMI_MONITORING_SENSOR_TYPE_MONITOR_ASIC_IC                     = 0x26,
    IPMI_MONITORING_SENSOR_TYPE_LAN                                 = 0x27,
    IPMI_MONITORING_SENSOR_TYPE_MANAGEMENT_SUBSYSTEM_HEALTH         = 0x28,
    IPMI_MONITORING_SENSOR_TYPE_BATTERY                             = 0x29,
    IPMI_MONITORING_SENSOR_TYPE_SESSION_AUDIT                       = 0x2A,
    IPMI_MONITORING_SENSOR_TYPE_VERSION_CHANGE                      = 0x2B,
    IPMI_MONITORING_SENSOR_TYPE_FRU_STATE                           = 0x2C,
    /* 0x2D - 0xBF - RESERVED */
    /* 0xC0 - 0xFF - OEM sensor types */
    IPMI_MONITORING_SENSOR_TYPE_OEM_MIN                             = 0xC0,
    IPMI_MONITORING_SENSOR_TYPE_OEM_MAX                             = 0xFF,
    IPMI_MONITORING_SENSOR_TYPE_UNKNOWN                             = 0xFFFF,
  } ipmi_monitoring_sensor_type_t;

enum ipmi_monitoring_sensor_state
  {
    IPMI_MONITORING_SENSOR_STATE_NOMINAL  = 0x00,
    IPMI_MONITORING_SENSOR_STATE_WARNING  = 0x01,
    IPMI_MONITORING_SENSOR_STATE_CRITICAL = 0x02,
    IPMI_MONITORING_SENSOR_STATE_UNKNOWN  = 0x03,
  };

enum ipmi_monitoring_sensor_units
  {
    IPMI_MONITORING_SENSOR_UNITS_NONE       = 0x00,
    IPMI_MONITORING_SENSOR_UNITS_CELSIUS    = 0x01,
    IPMI_MONITORING_SENSOR_UNITS_FAHRENHEIT = 0x02,
    IPMI_MONITORING_SENSOR_UNITS_VOLTS      = 0x03,
    IPMI_MONITORING_SENSOR_UNITS_AMPS       = 0x04,
    IPMI_MONITORING_SENSOR_UNITS_RPM        = 0x05,
    IPMI_MONITORING_SENSOR_UNITS_WATTS      = 0x06,
    IPMI_MONITORING_SENSOR_UNITS_UNKNOWN    = 0x07,
  };

enum ipmi_monitoring_sensor_reading_type
  {
    IPMI_MONITORING_SENSOR_READING_TYPE_UNSIGNED_INTEGER8_BOOL     = 0x00,
    IPMI_MONITORING_SENSOR_READING_TYPE_UNSIGNED_INTEGER32         = 0x01,
    IPMI_MONITORING_SENSOR_READING_TYPE_DOUBLE                     = 0x02,
    IPMI_MONITORING_SENSOR_READING_TYPE_UNKNOWN                    = 0x03,
  };

enum ipmi_monitoring_sensor_bitmask_type
  {
    IPMI_MONITORING_SENSOR_BITMASK_TYPE_THRESHOLD                           = 0x00,
    IPMI_MONITORING_SENSOR_BITMASK_TYPE_TRANSITION_STATE                    = 0x01,
    IPMI_MONITORING_SENSOR_BITMASK_TYPE_STATE                               = 0x02,
    IPMI_MONITORING_SENSOR_BITMASK_TYPE_PREDICTIVE_FAILURE                  = 0x03,
    IPMI_MONITORING_SENSOR_BITMASK_TYPE_LIMIT                               = 0x04,
    IPMI_MONITORING_SENSOR_BITMASK_TYPE_PERFORMANCE                         = 0x05,
    IPMI_MONITORING_SENSOR_BITMASK_TYPE_TRANSITION_SEVERITY                 = 0x06,
    IPMI_MONITORING_SENSOR_BITMASK_TYPE_DEVICE_PRESENT                      = 0x07,
    IPMI_MONITORING_SENSOR_BITMASK_TYPE_DEVICE_ENABLED                      = 0x08,
    IPMI_MONITORING_SENSOR_BITMASK_TYPE_TRANSITION_AVAILABILITY             = 0x09,
    IPMI_MONITORING_SENSOR_BITMASK_TYPE_REDUNDANCY                          = 0x0A,
    IPMI_MONITORING_SENSOR_BITMASK_TYPE_ACPI_POWER_STATE                    = 0x0B,
    IPMI_MONITORING_SENSOR_BITMASK_TYPE_PHYSICAL_SECURITY                   = 0x0C,
    IPMI_MONITORING_SENSOR_BITMASK_TYPE_PLATFORM_SECURITY_VIOLATION_ATTEMPT = 0x0D,
    IPMI_MONITORING_SENSOR_BITMASK_TYPE_PROCESSOR                           = 0x0E,
    IPMI_MONITORING_SENSOR_BITMASK_TYPE_POWER_SUPPLY                        = 0x0F,
    IPMI_MONITORING_SENSOR_BITMASK_TYPE_POWER_UNIT                          = 0x10,
    IPMI_MONITORING_SENSOR_BITMASK_TYPE_COOLING_DEVICE                      = 0x11,
    IPMI_MONITORING_SENSOR_BITMASK_TYPE_OTHER_UNITS_BASED_SENSOR            = 0x12,
    IPMI_MONITORING_SENSOR_BITMASK_TYPE_MEMORY                              = 0x13,
    IPMI_MONITORING_SENSOR_BITMASK_TYPE_DRIVE_SLOT                          = 0x14,
    IPMI_MONITORING_SENSOR_BITMASK_TYPE_POST_MEMORY_RESIZE                  = 0x15,
    IPMI_MONITORING_SENSOR_BITMASK_TYPE_SYSTEM_FIRMWARE_PROGRESS            = 0x16,
    IPMI_MONITORING_SENSOR_BITMASK_TYPE_EVENT_LOGGING_DISABLED              = 0x17,
    IPMI_MONITORING_SENSOR_BITMASK_TYPE_WATCHDOG1                           = 0x18,
    IPMI_MONITORING_SENSOR_BITMASK_TYPE_SYSTEM_EVENT                        = 0x19,
    IPMI_MONITORING_SENSOR_BITMASK_TYPE_CRITICAL_INTERRUPT                  = 0x1A,
    IPMI_MONITORING_SENSOR_BITMASK_TYPE_BUTTON_SWITCH                       = 0x1B,
    IPMI_MONITORING_SENSOR_BITMASK_TYPE_MODULE_BOARD                        = 0x1C,
    IPMI_MONITORING_SENSOR_BITMASK_TYPE_MICROCONTROLLER_COPROCESSOR         = 0x1D,
    IPMI_MONITORING_SENSOR_BITMASK_TYPE_ADD_IN_CARD                         = 0x1E,
    IPMI_MONITORING_SENSOR_BITMASK_TYPE_CHASSIS                             = 0x1F,
    IPMI_MONITORING_SENSOR_BITMASK_TYPE_CHIP_SET                            = 0x20,
    IPMI_MONITORING_SENSOR_BITMASK_TYPE_OTHER_FRU                           = 0x21,
    IPMI_MONITORING_SENSOR_BITMASK_TYPE_CABLE_INTERCONNECT                  = 0x22,
    IPMI_MONITORING_SENSOR_BITMASK_TYPE_TERMINATOR                          = 0x23,
    IPMI_MONITORING_SENSOR_BITMASK_TYPE_SYSTEM_BOOT_INITIATED               = 0x24,
    IPMI_MONITORING_SENSOR_BITMASK_TYPE_BOOT_ERROR                          = 0x25,
    IPMI_MONITORING_SENSOR_BITMASK_TYPE_OS_BOOT                             = 0x26,
    IPMI_MONITORING_SENSOR_BITMASK_TYPE_OS_CRITICAL_STOP                    = 0x27,
    IPMI_MONITORING_SENSOR_BITMASK_TYPE_SLOT_CONNECTOR                      = 0x28,
    IPMI_MONITORING_SENSOR_BITMASK_TYPE_SYSTEM_ACPI_POWER_STATE             = 0x29,
    IPMI_MONITORING_SENSOR_BITMASK_TYPE_WATCHDOG2                           = 0x2A,
    IPMI_MONITORING_SENSOR_BITMASK_TYPE_PLATFORM_ALERT                      = 0x2B,
    IPMI_MONITORING_SENSOR_BITMASK_TYPE_ENTITY_PRESENCE                     = 0x2C,
    IPMI_MONITORING_SENSOR_BITMASK_TYPE_MONITOR_ASIC_IC                     = 0x2D,
    IPMI_MONITORING_SENSOR_BITMASK_TYPE_LAN                                 = 0x2E,
    IPMI_MONITORING_SENSOR_BITMASK_TYPE_MANAGEMENT_SUBSYSTEM_HEALTH         = 0x2F,
    IPMI_MONITORING_SENSOR_BITMASK_TYPE_BATTERY                             = 0x30,
    IPMI_MONITORING_SENSOR_BITMASK_TYPE_SESSION_AUDIT                       = 0x31,
    IPMI_MONITORING_SENSOR_BITMASK_TYPE_VERSION_CHANGE                      = 0x32,
    IPMI_MONITORING_SENSOR_BITMASK_TYPE_FRU_STATE                           = 0x33,
    IPMI_MONITORING_SENSOR_BITMASK_TYPE_OEM                                 = 0xFE,
    IPMI_MONITORING_SENSOR_BITMASK_TYPE_UNKNOWN                             = 0xFF,
  };

enum ipmi_monitoring_driver_type
  {
    IPMI_MONITORING_DRIVER_TYPE_KCS      = 0x00,
    IPMI_MONITORING_DRIVER_TYPE_SSIF     = 0x01,
    IPMI_MONITORING_DRIVER_TYPE_OPENIPMI = 0x02,
    IPMI_MONITORING_DRIVER_TYPE_SUNBMC   = 0x03,
  };

enum ipmi_monitoring_protocol_version
  {
    IPMI_MONITORING_PROTOCOL_VERSION_1_5 = 0x00,
    IPMI_MONITORING_PROTOCOL_VERSION_2_0 = 0x01,
  };

enum ipmi_monitoring_privilege
  {
    IPMI_MONITORING_PRIVILEGE_LEVEL_USER     = 0x00,
    IPMI_MONITORING_PRIVILEGE_LEVEL_OPERATOR = 0x01,
    IPMI_MONITORING_PRIVILEGE_LEVEL_ADMIN    = 0x02,
  };

enum ipmi_monitoring_authentication_type
  {
    IPMI_MONITORING_AUTHENTICATION_TYPE_NONE                  = 0x00,
    IPMI_MONITORING_AUTHENTICATION_TYPE_STRAIGHT_PASSWORD_KEY = 0x01,
    IPMI_MONITORING_AUTHENTICATION_TYPE_MD2                   = 0x02,
    IPMI_MONITORING_AUTHENTICATION_TYPE_MD5                   = 0x03,
  };

enum ipmi_monitoring_flags
  {
    IPMI_MONITORING_FLAGS_NONE               = 0x00,
    IPMI_MONITORING_FLAGS_DEBUG              = 0x01,
    IPMI_MONITORING_FLAGS_DEBUG_IPMI_PACKETS = 0x02,
    IPMI_MONITORING_FLAGS_LOCK_MEMORY        = 0x04,
  };

/* Note: Non-logical bitmask order is set for consistency of masks
 * with libfreeipmi bitmasks.
 */
enum ipmi_monitoring_workaround_flags
  {
    IPMI_MONITORING_WORKAROUND_FLAGS_ACCEPT_SESSION_ID_ZERO      = 0x00000001,
    IPMI_MONITORING_WORKAROUND_FLAGS_FORCE_PERMSG_AUTHENTICATION = 0x00000002,
    IPMI_MONITORING_WORKAROUND_FLAGS_CHECK_UNEXPECTED_AUTHCODE   = 0x00000004,
    IPMI_MONITORING_WORKAROUND_FLAGS_BIG_ENDIAN_SEQUENCE_NUMBER  = 0x00000008,
    IPMI_MONITORING_WORKAROUND_FLAGS_AUTHENTICATION_CAPABILITIES = 0x00000010,
    IPMI_MONITORING_WORKAROUND_FLAGS_INTEL_2_0_SESSION           = 0x01000000,
    IPMI_MONITORING_WORKAROUND_FLAGS_SUPERMICRO_2_0_SESSION      = 0x02000000,
    IPMI_MONITORING_WORKAROUND_FLAGS_SUN_2_0_SESSION             = 0x04000000,
    IPMI_MONITORING_WORKAROUND_FLAGS_OPEN_SESSION_PRIVILEGE      = 0x08000000,
  };
  
enum ipmi_monitoring_sensor_reading_flags
  {
    IPMI_MONITORING_SENSOR_READING_FLAGS_REREAD_SDR_CACHE                 = 0x00000001,
    IPMI_MONITORING_SENSOR_READING_FLAGS_IGNORE_NON_INTERPRETABLE_SENSORS = 0x00000002,
    IPMI_MONITORING_SENSOR_READING_FLAGS_BRIDGE_SENSORS                   = 0x00000004,
    IPMI_MONITORING_SENSOR_READING_FLAGS_INTERPRET_OEM_DATA               = 0x00000008,
    IPMI_MONITORING_SENSOR_READING_FLAGS_SHARED_SENSORS                   = 0x00000010,
    IPMI_MONITORING_SENSOR_READING_FLAGS_IGNORE_UNREADABLE_SENSORS        = 0x00000002, /* legacy macro */
  };

/*
 * ipmi_monitoring_ipmi_config
 *
 * Configuration information for IPMI Inband monitoring
 *
 * driver_type
 *
 *   Use a specific in-band driver.
 *
 *   IPMI_MONITORING_DRIVER_TYPE_KCS
 *   IPMI_MONITORING_DRIVER_TYPE_SSIF
 *   IPMI_MONITORING_DRIVER_TYPE_OPENIPMI
 *   IPMI_MONITORING_DRIVER_TYPE_SUNBMC
 *
 *    Pass < 0 for default of IPMI_MONITORING_DRIVER_TYPE_KCS.
 *
 * disable_auto_probe
 *
 *   Flag informs the library if in-band driver information should be
 *   probed or not.
 *
 * driver_address
 *
 *   Use this specified driver address instead of a probed one.
 *
 * register_spacing
 *
 *   Use this register space instead of the probed one.
 *
 * driver_device
 *
 *   Use this driver device for the IPMI driver.
 *
 * Configuration information for IPMI Out-of-Band monitoring
 *
 * protocol_version
 *
 *   Indicate the IPMI protocol version to use
 *
 *   IPMI_MONITORING_PROTOCOL_VERSION_1_5
 *   IPMI_MONITORING_PROTOCOL_VERSION_2_0
 *
 *   Pass < 0 for default of IPMI_MONITORING_VERSION_1_5.
 *
 * username
 *
 *   BMC username. Pass NULL ptr for NULL username.  Maximum length of
 *   16 bytes.
 *
 * password
 *
 *   BMC password. Pass NULL ptr for NULL password.  Maximum length of
 *   16 bytes for IPMI 1.5, 20 bytes for IPMI 2.0
 *
 * k_g
 *
 *   BMC Key for 2-key authentication.  Pass NULL ptr to use password
 *   as BMC key.  The k_g key need not be an ascii string.
 *
 * k_g_len
 *
 *   Length of k_g.  Necessary b/c k_g may contain null values or in its
 *   hex key.  Maximum length of 20 bytes.
 *
 * privilege_level
 *
 *   privilege level to authenticate with.
 *
 *   Supported privilege levels:
 *
 *   IPMI_MONITORING_PRIVILEGE_LEVEL_USER
 *   IPMI_MONITORING_PRIVILEGE_LEVEL_OPERATOR
 *   IPMI_MONITORING_PRIVILEGE_LEVEL_ADMIN
 *
 *   Pass < 0 for default of IPMI_MONITORING_PRIVILEGE_LEVEL_USER.
 *
 * authentication_type
 *
 *   authentication type to use
 *
 *   IPMI_MONITORING_AUTHENTICATION_TYPE_NONE
 *   IPMI_MONITORING_AUTHENTICATION_TYPE_STRAIGHT_PASSWORD_KEY
 *   IPMI_MONITORING_AUTHENTICATION_TYPE_MD2
 *   IPMI_MONITORING_AUTHENTICATION_TYPE_MD5
 *
 *   Pass < 0 for default of IPMI_MONITORING_AUTHENTICATION_TYPE_MD5.
 *
 * cipher_suite_id
 *
 *   Cipher suite identifier to determine authentication, integrity,
 *   and confidentiality algorithms to use.
 *
 *   Supported Cipher Suite IDs
 *   (Key: A - Authentication Algorithm
 *         I - Integrity Algorithm
 *         C - Confidentiality Algorithm)
 *
 *   0 - A = None; I = None; C = None
 *   1 - A = HMAC-SHA1; I = None; C = None
 *   2 - A = HMAC-SHA1; I = HMAC-SHA1-96; C = None
 *   3 - A = HMAC-SHA1; I = HMAC-SHA1-96; C = AES-CBC-128
 *   6 - A = HMAC-MD5; I = None; C = None
 *   7 - A = HMAC-MD5; I = HMAC-MD5-128; C = None
 *   8 - A = HMAC-MD5; I = HMAC-MD5-128; C = AES-CBC-128
 *   11 - A = HMAC-MD5; I = MD5-128; C = None
 *   12 - A = HMAC-MD5; I = MD5-128; C = AES-CBC-128
 *
 *   Pass < 0 for default of 3.
 *
 * session_timeout_len
 *
 *   Specifies the session timeout length in milliseconds.  Pass <= 0
 *   to default to 60000 (60 seconds).
 *
 * retransmission_timeout_len
 *
 *   Specifies the packet retransmission timeout length in
 *   milliseconds.  Pass <= 0 to default to 500 (0.5 seconds).
 *
 * Configuration information for both Inband and Outofband
 *
 * workaround_flags
 *
 *   Bitwise OR of flags indicating any behavior which should be
 *   changed from the default to handle IPMI non-compliance problems.
 *   Some BMCs which are non-compliant may require a workaround flag
 *   for correct operation. Pass 0 for default of no modifications to
 *   behavior.
 */
struct ipmi_monitoring_ipmi_config
{
  int driver_type;
  int disable_auto_probe;
  unsigned int driver_address;
  unsigned int register_spacing;
  char *driver_device;

  int protocol_version;
  char *username;
  char *password;
  unsigned char *k_g;
  unsigned int k_g_len;
  int privilege_level;
  int authentication_type;
  int cipher_suite_id;
  int session_timeout_len;
  int retransmission_timeout_len;

  unsigned int workaround_flags;
};

typedef struct ipmi_monitoring_ctx *ipmi_monitoring_ctx_t;

/*
 * Ipmi_Monitoring_Sensor_Readings_Callback
 *
 * If callback returns < 0, libipmimonitoring will stop reading
 * remaining sensors.
 */
typedef int (*Ipmi_Monitoring_Sensor_Readings_Callback)(ipmi_monitoring_ctx_t c, void *callback_data);

/*
 * ipmi_monitoring_init
 *
 * Initialize the ipmi monitoring library.  Needs to be called only
 * once before any ipmi monitoring functions are called.  Threaded
 * applications are responsible for calling this function before any
 * thread may call another function in the library.
 *
 * Returns 0 on success, -1 on error
 */
int ipmi_monitoring_init (unsigned int flags, int *errnum);

/*
 * ipmi_monitoring_ctx_create
 *
 * Create an ipmi monitoring context
 *
 * Returns context on success, NULL on error
 */
ipmi_monitoring_ctx_t ipmi_monitoring_ctx_create (void);

/*
 * ipmi_monitoring_ctx_destroy
 *
 * Destroy an ipmi monitoring context
 */
void ipmi_monitoring_ctx_destroy (ipmi_monitoring_ctx_t c);

/*
 * ipmi_monitoring_ctx_errnum
 *
 * Returns the error code of the most recently caused error
 */
int ipmi_monitoring_ctx_errnum (ipmi_monitoring_ctx_t c);

/*
 * ipmi_monitoring_ctx_strerror
 *
 * Returns a pointer to statically allocated string describing the
 * error code in errnum.
 */
char *ipmi_monitoring_ctx_strerror (int errnum);

/*
 * ipmi_monitoring_ctx_errormsg
 *
 * Returns a pointer to statically allocated string describing the
 * most recently caused error.
 */
char *ipmi_monitoring_ctx_errormsg (ipmi_monitoring_ctx_t c);

/*
 * ipmi_monitoring_ctx_sensor_config_file
 *
 * Initialize the ipmi monitoring context with the interpretation
 * rules stored in a specific sensor configuration file.  If NULL
 * sensor_config_file is specified, the default sensor configuration
 * file will be read and parsed.
 *
 * Returns 0 on success, -1 on error
 */
int ipmi_monitoring_ctx_sensor_config_file (ipmi_monitoring_ctx_t c,
                                            const char *sensor_config_file);
  
/*
 * ipmi_monitoring_ctx_sdr_cache_directory
 *
 * Initialize the ipmi monitoring context with a different SDR cache
 * directory.
 *
 * Returns 0 on success, -1 on error
 */
int ipmi_monitoring_ctx_sdr_cache_directory (ipmi_monitoring_ctx_t c,
                                             const char *dir);

/*
 * ipmi_monitoring_ctx_sdr_cache_filenames
 *
 * Initialize how sdr cache filenames will be formatted when
 * written/read.
 *
 * The following may be included in the format.
 *
 * %L - the hostname of the local machine
 * %H - the remote host being monitored
 *
 * Atleast %H must be specified.
 *
 * Returns 0 on success, -1 on error
 */
int ipmi_monitoring_ctx_sdr_cache_filenames (ipmi_monitoring_ctx_t c,
                                             const char *format);

/*
 * ipmi_monitoring_sensor_readings_by_record_id
 *
 * Retrieve sensor readings by sensor numbers and store them in the monitoring context.
 *
 * If 'hostname' is NULL, sensors for the current node will be retrieved in-band.
 * If 'record_ids' is NULL, default sensors will be retrieved
 * (default == all sensors unless configured otherwise).
 * Callback function will be called after each sensor is read and parsed.
 *
 * Returns number of sensors values retrieved on success, -1 on error.
 * Sensor iterators below may be used after calling this function to
 * iterate through sensors.
 */
int ipmi_monitoring_sensor_readings_by_record_id (ipmi_monitoring_ctx_t c,
                                                  const char *hostname,
                                                  struct ipmi_monitoring_ipmi_config *config,
                                                  unsigned int sensor_reading_flags,
                                                  unsigned int *record_ids,
                                                  unsigned int record_ids_len,
                                                  Ipmi_Monitoring_Sensor_Readings_Callback callback,
                                                  void *callback_data);

/*
 * ipmi_monitoring_sensor_readings_by_sensor_type
 *
 * Retrieve sensor readings by sensor type and store them in the monitoring context.
 *
 * If 'hostname' is NULL, sensors for the current node will be retrieved in-band.
 * If 'sensor_types' is NULL, default sensors will be retrieved.
 * (default == all sensors unless configured otherwise).
 * Callback function will be called after each sensor is read and parsed.
 *
 * Returns number of sensors values retrieved on success, -1 on error
 * Sensor iterators below may be used after calling this function to
 * iterate through sensors.
 */
int ipmi_monitoring_sensor_readings_by_sensor_type (ipmi_monitoring_ctx_t c,
                                                    const char *hostname,
                                                    struct ipmi_monitoring_ipmi_config *config,
                                                    unsigned int sensor_reading_flags,
                                                    unsigned int *sensor_types,
                                                    unsigned int sensor_types_len,
                                                    Ipmi_Monitoring_Sensor_Readings_Callback callback,
                                                    void *callback_data);

/*
 * ipmi_monitoring_sensor_iterator_first
 *
 * Reset iterator to the first sensor reading
 *
 * Returns 0 on success, -1 on error
 */
int ipmi_monitoring_sensor_iterator_first (ipmi_monitoring_ctx_t c);

/*
 * ipmi_monitoring_sensor_iterator_next
 *
 * Advance iterator to the next set of sensors information
 *
 * Returns 0 on success, -1 on error
 */
int ipmi_monitoring_sensor_iterator_next (ipmi_monitoring_ctx_t c);

/*
 * ipmi_monitoring_sensor_iterator_destroy
 *
 * Destroy all internally stored sensors readings.
 */
void ipmi_monitoring_sensor_iterator_destroy (ipmi_monitoring_ctx_t c);

/*
 * ipmi_monitoring_sensor_read_record_id
 *
 * Returns the record id of the current sensor reading in a callback
 * or the iterator.
 */
int ipmi_monitoring_sensor_read_record_id (ipmi_monitoring_ctx_t c);

/*
 * ipmi_monitoring_sensor_read_sensor_number
 *
 * Returns the sensor number of the current sensor reading in a
 * callback or the iterator.  May be useful if
 * IPMI_MONITORING_SENSOR_READING_FLAGS_SHARED_SENSORS has been set.
 */
int ipmi_monitoring_sensor_read_sensor_number (ipmi_monitoring_ctx_t c);

/*
 * ipmi_monitoring_sensor_read_sensor_type
 *
 * Returns the sensor type of the current sensor reading in a
 * callback or the iterator.
 */
int ipmi_monitoring_sensor_read_sensor_type (ipmi_monitoring_ctx_t c);

/*
 * ipmi_monitoring_sensor_read_sensor_name
 *
 * Returns a pointer to the sensor name of the current sensor reading
 * in a callback or the iterator.
 */
char *ipmi_monitoring_sensor_read_sensor_name (ipmi_monitoring_ctx_t c);

/*
 * ipmi_monitoring_sensor_read_sensor_state
 *
 * Returns the current sensor state of the current sensor reading in a
 * callback or the iterator.
 */
int ipmi_monitoring_sensor_read_sensor_state (ipmi_monitoring_ctx_t c);

/*
 * ipmi_monitoring_sensor_read_sensor_units
 *
 * Returns the sensor units type of the current sensor reading in a
 * callback or the iterator.
 */
int ipmi_monitoring_sensor_read_sensor_units (ipmi_monitoring_ctx_t c);

/*
 * ipmi_monitoring_sensor_read_sensor_reading_type
 *
 * Returns the sensor reading type of the current sensor reading in a
 * callback or the iterator.
 */
int ipmi_monitoring_sensor_read_sensor_reading_type (ipmi_monitoring_ctx_t c);

/*
 * ipmi_monitoring_sensor_read_sensor_reading
 *
 * Returns a pointer to the sensor reading of the current sensor
 * reading in a callback or the iterator.  It is the responsibility of
 * the user to cast it to the correct type based on the reading type.
 * Returns NULL if no reading available.
 */
void *ipmi_monitoring_sensor_read_sensor_reading (ipmi_monitoring_ctx_t c);

/*
 * ipmi_monitoring_sensor_read_sensor_bitmask_type
 *
 * Returns the bitmask type of the current sensor reading in a
 * callback or the iterator.  See bitmasks in
 * ipmi_monitoring_bitmasks.h to determine bitmask conditions for a
 * given bitmask type.
 */
int ipmi_monitoring_sensor_read_sensor_bitmask_type (ipmi_monitoring_ctx_t c);

/*
 * ipmi_monitoring_sensor_read_sensor_bitmask
 *
 * Returns the bitmask of the current sensor reading in a callback or
 * the iterator.  See bitmasks in ipmi_monitoring_bitmasks.h to
 * determine bitmask conditions for a given bitmask type.
 */
int ipmi_monitoring_sensor_read_sensor_bitmask (ipmi_monitoring_ctx_t c);

/*
 * ipmi_monitoring_sensor_read_sensor_bitmask_strings
 *
 * Returns an array of strings representing the events in the bitmask.
 * Array is NULL terminated.  Returns NULL if no events are indicated.
 */
char **ipmi_monitoring_sensor_read_sensor_bitmask_strings (ipmi_monitoring_ctx_t c);

#ifdef __cplusplus
}
#endif

#endif /* _IPMI_MONITORING_H */
