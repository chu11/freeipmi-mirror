/*
 * Copyright (C) 2008-2011 FreeIPMI Core Team
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

#if HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#include <ctype.h>
#endif /* STDC_HEADERS */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <limits.h>
#include <assert.h>

#include <freeipmi/freeipmi.h>

#include "ipmi-oem.h"
#include "ipmi-oem-argp.h"
#include "ipmi-oem-common.h"
#include "ipmi-oem-inventec.h"

#include "freeipmi-portability.h"
#include "pstdout.h"

/* Inventec 5441/5442 Notes
 *
 * Copy Image Data Request
 *
 * 0x08 - network function
 * 0x03 - OEM cmd
 * 0x?? - In Progress
 *      - 00h - data transmission is in progress
 *      - 01h = data transmission completed
 * bytes 3:6 - image offset to be copied
 * bytes 7:N - image data to be copied
 *
 * Copy Image Data Response
 *
 * 0x03 - OEM cmd
 * 0x?? - Completion Code
 */

/* Inventec 5442 Notes
 *
 * Following appear to be too hardware specific, just putting it here
 * for documentation.
 *
 * Set Fan Control Request
 *
 * 0x34 - network function
 * 0x61 - OEM cmd
 * 0x?? - fan control setting
 *      0:6 - cuty cycle setting, 0-100
 *      7 - enabled/disabled fan control ; 0 - disable, 1 - enable
 *
 * Set Fan Control Response
 *
 * 0x61 - OEM cmd
 * 0x?? - Completion Code
 *
 * Get Fan Control Request
 *
 * 0x34 - network function
 * 0x62 - OEM cmd
 *
 * Get Fan Control Response
 *
 * 0x62 - OEM cmd
 * 0x?? - Completion Code
 * 0x?? - fan control setting
 *      0:6 - cuty cycle setting, 0-100
 *      7 - enabled/disabled fan control ; 0 - disable, 1 - enable
 *
 * Set FSC Table Request
 *
 * 0x34 - network function
 * 0x63 - OEM cmd
 * 0x?? - FSC Table Setting
 *      0:6 - fan table settings
 *          - 0x80 - 1st FSC fan table (default : 13800 RPM)
 *          - 0x81 - 2nd FSC fan table (FACEBOOK)
 *          - 0x82 - 3rd FSC fan table (Oscillation)
 *          - 0x83 - 4th FSC fan table (Western Geco)
 *          - 0x84 - 5th FSC fan table (Loki)
 *
 * Set FSC Table Response
 *
 * 0x63 - OEM cmd
 * 0x?? - Completion Code
 *
 * Get FSC Table Request
 *
 * 0x34 - network function
 * 0x64 - OEM cmd
 *
 * Get FSC Table Response
 *
 * 0x64 - OEM cmd
 * 0x?? - Completion Code
 * 0x?? - FSC Table Setting
 *      0:6 - fan table settings
 *          - 0x80 - 1st FSC fan table (default : 13800 RPM)
 *          - 0x81 - 2nd FSC fan table (FACEBOOK)
 *          - 0x82 - 3rd FSC fan table (Oscillation)
 *          - 0x83 - 4th FSC fan table (Western Geco)
 *          - 0x84 - 5th FSC fan table (Loki)
 *
 * Get FCB SKU Info Request
 *
 * 0x34 - network function
 * 0x6A - OEM cmd
 *
 * Get FCB SKU Info Response
 *
 * 0x6A - OEM cmd
 * 0x?? - Completion Code
 * 0x?? - FCB SKU Information
 *
 * Get FCB Power Throttling Status Request
 *
 * 0x34 - network function
 * 0x6B - OEM cmd
 *
 * Get FCB Power Throttling Status Response
 * 
 * 0x6B - OEM cmd
 * 0x?? - Completion Code
 * 0x?? - FCB Power Throttling status
 *
 * OEM Get PIC Model Request
 *
 * 0x34 - network function
 * 0x70 - OEM cmd
 *
 * OEM Get PIC Model Response
 * 
 * 0x70 - OEM cmd
 * 0x?? - Completion Code
 * 0x?? - PIC model
 *      0x10 - PIC16
 *      0x12 - PIC18
 *
 * OEM Set Flash Pin Request
 *
 * 0x34 - network function
 * 0x71 - OEM cmd
 * 0x?? - Pin Number
 * 0x?? - Value
 *
 * OEM Set Flash Pin Response
 * 
 * 0x71 - OEM cmd
 * 0x?? - Completion Code
 *
 * OEM Get Flash Pin Request
 *
 * 0x34 - network function
 * 0x72 - OEM cmd
 * 0x?? - Pin Number
 *
 * OEM Get Flash Pin Response
 * 
 * 0x72 - OEM cmd
 * 0x?? - Completion Code
 * 0x?? - Pin Value
 *
 * OEM New Master Write Read Request
 *
 * 0x34 - network function
 * 0x73 - OEM cmd
 * 0x?? - slave address
 * 0x?? - read count
 * 3:52 - data
 *
 * OEM New Master Write Read Response
 * 
 * 0x73 - OEM cmd
 * 0x?? - Completion Code
 * 2:51 - data
 *
 * Set Power Throttling Behavior Request
 *
 * 0x34 - network function
 * 0xB1 - OEM cmd
 * 0x?? - Power Throttling Enable (01h - enable)
 * 0x?? - Power Capping Enable (01h - enable)
 * 0x?? - Current Chassi Power Capping Value (low byte)
 * 0x?? - Current Chassis Power Capping Value (high byte)
 * 0x?? - Power Capping Max Value (low byte)
 * 0x?? - Power Capping Max Value (high byte)
 *
 * Set Power Throttling Behavior Response
 * 
 * 0xB1 - OEM cmd
 * 0x?? - Completion Code
 *
 * Get Power Throttling Behavior Request
 *
 * 0x34 - network function
 * 0xB2 - OEM cmd
 *
 * Get Power Throttling Behavior Response
 * 
 * 0xB2 - OEM cmd
 * 0x?? - Completion Code
 * 0x?? - Power Throttling Enable (01h - enable)
 * 0x?? - Power Capping Enable (01h - enable)
 * 0x?? - Current Chassi Power Capping Value (low byte)
 * 0x?? - Current Chassis Power Capping Value (high byte)
 * 0x?? - Power Capping Max Value (low byte)
 * 0x?? - Power Capping Max Value (high byte)
 *
 * Get PSU Mismatch and Type Request
 *
 * 0x34 - network function
 * 0xB3 - OEM cmd
 *
 * Get PSU Mismatch and Type Response
 *
 * 0xB3 - OEM cmd
 * 0x?? - Completion Code
 * 0x?? - PSU Mismatch
 *      - 00h - mismatch
 *      - 01h - match
 * 0x?? - PSU Type
 *      - 0:3 - PSU1 type
 *      - 4:7 - PSU2 type
 *      01h = 470 watt
 *      02h = 750 watt
 *      03h = 1100 watt
 *      04h = 1400 watt
 */

/* achu:
 *
 * Much of the Inventec extended configuration support is identical to
 * the Quanta extended configuration support.  However, there are
 * subtle differences.  Some extended configuration which is supported
 * in one may not be supported in the other.
 *
 * While duplicating code is usually never good, in this particular
 * circumstance I will flat out duplicate code under both
 * motherboards.  I believe it is important to clearly
 * document/delineate what is supported on what motherboards.
 *
 * In addition, I must take future motherboard support into
 * consideration.  Since these OEM extensions have apparently come
 * from a "common parent" code (I believe Avocent, but I may be wrong,
 * because Dell Poweredges are also Avocent based but have completely
 * different extensions), there may be other motherboards out there
 * that are tweaked differently as well.  To have a common code base
 * for all these different motherboards and their slight differences
 * seems like a bad idea.
 */

/* achu: all named from doc except 'lan' configuration id, which I assumed names */

#define IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_NIC                      0x02
#define IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_SOL                      0x03
#define IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_SECURITY                 0x04
#define IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_ACCOUNT_STATUS           0x05
#define IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_DNS                      0x06
#define IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_WEB_SERVER_CONFIGURATION 0x0C
#define IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_FIRMWARE_LOG             0x0E
#define IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_FIRMWARE_INFORMATION     0x0F
#define IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_FIRMWARE_UPDATE          0x10
#define IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_POWER_MANAGEMENT         0x11

/* nic status - 1 byte, 0 = shared, 1 = dedicated
 */
#define IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_NIC_MODE 0x01

/* sol idle timeout - 2 bytes, ls byte first, 0h = no timeout, default = 01h
 *
 * telnet/ssh redirect enable - 1 byte, 0 = disable, 1 = enabled 
 */
#define IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_SOL_SOL_IDLE_TIMEOUT           0x01
#define IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_SOL_TELNET_SSH_REDIRECT_ENABLE 0x02

/* service disabled - 1 byte, bitmask
 *                  - 0x01 = all service except IPMI are disabled
 *                           (takes precedence over other bits)
 *                  - 0x02 = KVM/Virtual Storage
 *                  - 0x04 = HTTP/HTTPS
 *                  - 0x08 = SSH/Telnet
 *
 * max authentication failures - 1 byte, 0 = disable
 *
 * lockout window - 2 bytes, in seconds, 0 = disable, default = 180
 *
 * lockout time - 2 bytes, in seconds, 0 = disable, default = 3600
 */
#define IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_SECURITY_SERVICE_DISABLED            0x01
#define IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_SECURITY_MAX_AUTHENTICATION_FAILURES 0x02
#define IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_SECURITY_LOCKOUT_WINDOW              0x03
#define IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_SECURITY_LOCKOUT_TIME                0x04

/* number of user - 1 byte, read only
 *
 * number of enabled user - 1 byte, read only
 *
 * user name - 1-17 bytes, read only
 *           - Inventec 5441/Xanadu II - reports stored as p-string, does not return p-string
 *           - Inventec 5442/Xanadu III - returns as p-string
 *
 * account status - 1 byte
 *                - 0x00 - status unspecified
 *                - 0x01 - enabled via set user password
 *                - 0x02 - disabled via set user password
 *                - 0x03 - user id is lockout
 */
#define IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_ACCOUNT_STATUS_NUMBER_OF_USER         0x01
#define IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_ACCOUNT_STATUS_NUMBER_OF_ENABLED_USER 0x02
#define IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_ACCOUNT_STATUS_USER_NAME              0x03
#define IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_ACCOUNT_STATUS_ACCOUNT_STATUS         0x04

/* DNS DHCP enable - 1 byte, 0 - false, 1 - true
 * - DNS server IP addresses should be assigned from DHCP server
 *
 * DNS Server1 - 4 bytes
 * - IP address for DNS server 1, read only if DNS DHCP Enable and DHCP are enabled
 *
 * DNS Server2 - 4 bytes
 * - IP address for DNS server 2, read only if DNS DHCP Enable and DHCP are enabled
 *
 * DNS Register BMC - 0 - false, 1 - true
 * - Enable registering the BMC host name on the DNS server
 *
 * DNS BMC Host Name - 1-64 bytes
 * - Specifies the DNS BMC host name, read only if DNS Register BMC is TRUE.
 * - Stored as P-string
 *
 * DNS Domain Name DHCP Enable - 1 byte
 * - DNS domain name should be assigned from DHCP
 *
 * DNS Domain Name - 1-256 bytes
 * - DNS domain name string, read only if DNS Domain Name DHCP Enable is TRUE
 * - Stored as P-string
 */
#define IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_DNS_DNS_DHCP_ENABLE             0x01
#define IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_DNS_DNS_SERVER1                 0x02
#define IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_DNS_DNS_SERVER2                 0x03
#define IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_DNS_DNS_REGISTER_BMC            0x04
#define IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_DNS_DNS_BMC_HOST_NAME           0x05
#define IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_DNS_DNS_DOMAIN_NAME_DHCP_ENABLE 0x06
#define IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_DNS_DNS_DOMAIN_NAME             0x07

/* web server enabled - 1 byte, 0 = false, 1 = true (default)
 *
 * max web sessions - 1 byte, read only
 *
 * active web sessions - 1 byte, read only
 *
 * web server timeout - 4 bytes, in seconds, 0 = disable, range 60-1920, default 300
 *
 * http port num - 2 bytes, default 80
 *
 * https port num - 2 bytes, default 443
 */
#define IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_WEB_SERVER_ENABLED  0x01
#define IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_MAX_WEB_SESSIONS    0x02
#define IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_ACTIVE_WEB_SESSIONS 0x03
#define IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_WEB_SERVER_TIMEOUT  0x04
#define IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_HTTP_PORT_NUM       0x05
#define IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_HTTPS_PORT_NUM      0x06

/* entity - 1 byte (read only)
 *
 * firmware version - 1-16 bytes (read only)
 *
 * branch - 1-16 bytes (read only)
 *
 * build information - 1-16 bytes (read only)
 *
 * update date / time - 3 bytes, from 0:00 1/1/08, lsbyte first
 */
#define IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_FIRMWARE_LOG_ENTITY            0x01
#define IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_FIRMWARE_LOG_FIRMWARE_VERSION  0x02
#define IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_FIRMWARE_LOG_BRANCH            0x03
#define IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_FIRMWARE_LOG_BUILD_INFORMATION 0x04
#define IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_FIRMWARE_LOG_UPDATE_DATE_TIME  0x05

/* name - 1-16 bytes (read only)
 *
 * description - 1-256 bytes (read only)
 *
 * entity - 1 byte (read only)
 *
 * product info - 1-64 bytes (read only)
 *
 * firmware version - 1-16 bytes (read only)
 *
 * branch - 1-16 bytes (read only)
 *
 * build information - 1-16 bytes (read only) 
 *
 */
#define IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_NAME              0x01
#define IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_DESCRIPTION       0x02
#define IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_ENTITY            0x03
#define IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_PRODUCT_INFO      0x04
#define IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_FIRMWARE_VERSION  0x05
#define IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_BRANCH            0x06
#define IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_BUILD_INFORMATION 0x07

#define IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_NAME_LEN              16
#define IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_DESCRIPTION_LEN       256
#define IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_PRODUCT_INFO_LEN      64
#define IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_FIRMWARE_VERSION_LEN  16
#define IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_BRANCH_LEN            16
#define IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_BUILD_INFORMATION_LEN 16

/* remote update enable - 1 byte, ??
 *
 * protocol - 1 byte, bitmask, 7:3 reserved, 2 : http, 1: ftp, 0: tftp (read only)
 *
 * uri - 1-256 bytes
 * note - first char should be a file or unit separator
 *
 * connection retry - 1 byte, 0 = no retries
 *
 * retry interval - 1 byte, in 5 second increments
 *
 * delay time - 1 byte, in seconds, 0h = immediate, ffh = random between 5 and 10
 */
#define IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_FIRMWARE_UPDATE_REMOTE_UPDATE_ENABLE 0x01
#define IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_FIRMWARE_UPDATE_PROTOCOL             0x02
#define IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_FIRMWARE_UPDATE_URI                  0x03
#define IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_FIRMWARE_UPDATE_CONNECTION_RETRY     0x04
#define IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_FIRMWARE_UPDATE_RETRY_INTERVAL       0x05
#define IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_FIRMWARE_UPDATE_DELAY_TIME           0x06

#define IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_FIRMWARE_UPDATE_RETRY_INTERVAL_INCREMENTS 5

#define IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_FIRMWARE_UPDATE_URI_LEN 256

/* power management enable - 1 byte, bit 7: 0 = disable, 1 = enable, reset reserved
 *
 * power staggery ac recovery - 1 byte, 0x00 = immediate power on
 * (default), 0x01 = auto, random between min and max below, 0x02 =
 * user defined, must be between min and max below
 *
 * power on delay - 2 bytes
 *
 * minimum power on delay - 2 bytes (read only)
 *
 * maximum power on delay - 2 bytes
 */
#define IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_POWER_MANAGEMENT_POWER_MANAGEMENT_ENABLE      0x01
#define IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_POWER_MANAGEMENT_POWER_STAGGERING_AC_RECOVERY 0x02
#define IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_POWER_MANAGEMENT_POWER_ON_DELAY               0x03
#define IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_POWER_MANAGEMENT_MINIMUM_POWER_ON_DELAY       0x04
#define IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_POWER_MANAGEMENT_MAXIMUM_POWER_ON_DELAY       0x05

#define IPMI_OEM_INVENTEC_EXTENDED_CONFIG_READ_ALL_BYTES           0xFF

#define IPMI_OEM_INVENTEC_EXTENDED_CONFIG_NIC_MODE_SHARED    0x00
#define IPMI_OEM_INVENTEC_EXTENDED_CONFIG_NIC_MODE_DEDICATED 0x01

#define IPMI_OEM_INVENTEC_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_ENABLE_ALL   0x00
#define IPMI_OEM_INVENTEC_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_ALL  0x01
#define IPMI_OEM_INVENTEC_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_KVM  0x02
#define IPMI_OEM_INVENTEC_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_HTTP 0x04
#define IPMI_OEM_INVENTEC_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_SSH  0x08

#define IPMI_OEM_INVENTEC_EXTENDED_CONFIG_ACCOUNT_STATUS_UNSPECIFIED 0x00
#define IPMI_OEM_INVENTEC_EXTENDED_CONFIG_ACCOUNT_STATUS_ENABLED     0x01
#define IPMI_OEM_INVENTEC_EXTENDED_CONFIG_ACCOUNT_STATUS_DISABLED    0x02
#define IPMI_OEM_INVENTEC_EXTENDED_CONFIG_ACCOUNT_STATUS_LOCKOUT     0x03

#define IPMI_OEM_INVENTEC_EXTENDED_CONFIG_DNS_DNS_BMC_HOST_NAME_MAX 64
#define IPMI_OEM_INVENTEC_EXTENDED_CONFIG_DNS_DNS_DOMAIN_NAME_MAX   256

/* DPNM = Dynamic Power Node Management */
#define IPMI_OEM_INVENTEC_EXTENDED_CONFIG_POWER_MANAGEMENT_ENABLE_DPNM_BITMASK 0x80
#define IPMI_OEM_INVENTEC_EXTENDED_CONFIG_POWER_MANAGEMENT_ENABLE_DPNM_SHIFT   7
#define IPMI_OEM_INVENTEC_EXTENDED_CONFIG_POWER_MANAGEMENT_ENABLE_DPNM_ENABLE  1
#define IPMI_OEM_INVENTEC_EXTENDED_CONFIG_POWER_MANAGEMENT_ENABLE_DPNM_DISABLE 0

#define IPMI_OEM_INVENTEC_EXTENDED_CONFIG_POWER_STAGGERING_AC_RECOVERY_IMMEDIATE    0x00
#define IPMI_OEM_INVENTEC_EXTENDED_CONFIG_POWER_STAGGERING_AC_RECOVERY_AUTO         0x01
#define IPMI_OEM_INVENTEC_EXTENDED_CONFIG_POWER_STAGGERING_AC_RECOVERY_USER_DEFINED 0x02

#define IPMI_OEM_INVENTEC_EXTENDED_CONFIG_FIRMWARE_INFORMATION_ENTITY_BMC         0
#define IPMI_OEM_INVENTEC_EXTENDED_CONFIG_FIRMWARE_INFORMATION_ENTITY_SYSTEM_BIOS 1
#define IPMI_OEM_INVENTEC_EXTENDED_CONFIG_FIRMWARE_INFORMATION_ENTITY_PDB         2
#define IPMI_OEM_INVENTEC_EXTENDED_CONFIG_FIRMWARE_INFORMATION_ENTITY_FCB         3

#define IPMI_OEM_INVENTEC_EXTENDED_CONFIG_FIRMWARE_UPDATE_PROTOCOL_BITMASK_TFTP  0x01
#define IPMI_OEM_INVENTEC_EXTENDED_CONFIG_FIRMWARE_UPDATE_PROTOCOL_BITMASK_FTP   0x02
#define IPMI_OEM_INVENTEC_EXTENDED_CONFIG_FIRMWARE_UPDATE_PROTOCOL_BITMASK_HTTP  0x04

#define IPMI_OEM_INVENTEC_EXTENDED_CONFIG_FIRMWARE_UPDATE_DELAY_TIME_RANDOM 0xFF

#define IPMI_OEM_INVENTEC_MAX_MACADDRLEN 24

#define IPMI_OEM_INVENTEC_UPDATE_FIRMWARE_INTERFACE_SYSTEM_INTERFACE 0x00
#define IPMI_OEM_INVENTEC_UPDATE_FIRMWARE_INTERFACE_NETWORKING       0x01
#define IPMI_OEM_INVENTEC_UPDATE_FIRMWARE_INTERFACE_USB              0x02

#define IPMI_OEM_INVENTEC_UPDATE_FIRMWARE_UPDATE_TYPE_BITMASK 0x80
#define IPMI_OEM_INVENTEC_UPDATE_FIRMWARE_UPDATE_TYPE_SHIFT   7

#define IPMI_OEM_INVENTEC_UPDATE_FIRMWARE_UPDATE_TYPE_FORCE_UPDATE  1
#define IPMI_OEM_INVENTEC_UPDATE_FIRMWARE_UPDATE_TYPE_NORMAL_UPDATE 0

#define IPMI_OEM_INVENTEC_UPDATE_FIRMWARE_OEM_DELL_CONFIG_PRESERVE   0x00
#define IPMI_OEM_INVENTEC_UPDATE_FIRMWARE_OEM_DELL_CONFIG_NOPRESERVE 0x01

#define IPMI_OEM_INVENTEC_GET_UPDATE_STATUS_TRANSMITTING_IMAGE          0x00
#define IPMI_OEM_INVENTEC_GET_UPDATE_STATUS_VALIDATING_IMAGE            0x01
#define IPMI_OEM_INVENTEC_GET_UPDATE_STATUS_PROGRAMMING                 0x02
#define IPMI_OEM_INVENTEC_GET_UPDATE_STATUS_READY_TO_ACCEPT_IMAGE       0x03
#define IPMI_OEM_INVENTEC_GET_UPDATE_STATUS_GENERAL_ERROR               0x80
#define IPMI_OEM_INVENTEC_GET_UPDATE_STATUS_CANNOT_ESTABLISH_CONNECTION 0x81
#define IPMI_OEM_INVENTEC_GET_UPDATE_STATUS_PATH_NOT_FOUND              0x82
#define IPMI_OEM_INVENTEC_GET_UPDATE_STATUS_TRANSMISSION_ABORT          0x83
#define IPMI_OEM_INVENTEC_GET_UPDATE_STATUS_CHECKSUM_ERROR              0x84
#define IPMI_OEM_INVENTEC_GET_UPDATE_STATUS_INCORRECT_PLATFORM          0x85
#define IPMI_OEM_INVENTEC_GET_UPDATE_STATUS_COMPLETED                   0xFF

#define IPMI_OEM_INVENTEC_ASSET_TAG_MAX 10

#define IPMI_OEM_INVENTEC_RESTORE_TO_DEFAULTS_RESTORE_FLAG_RESTORE_PARAMETERS_NOT_INCLUDED_BELOW 0x7
#define IPMI_OEM_INVENTEC_RESTORE_TO_DEFAULTS_RESTORE_FLAG_REMAINING_PARAMETERS_STAY_WHAT_IT_IS  0x0
#define IPMI_OEM_INVENTEC_RESTORE_TO_DEFAULTS_RESTORE_FLAG_SHIFT                                 5

#define IPMI_OEM_INVENTEC_RESTORE_TO_DEFAULTS_PEF_BITMASK                  0x10
#define IPMI_OEM_INVENTEC_RESTORE_TO_DEFAULTS_SERIAL_CONFIGURATION_BITMASK 0x08
#define IPMI_OEM_INVENTEC_RESTORE_TO_DEFAULTS_SOL_CONFIGURATION_BITMASK    0x04
#define IPMI_OEM_INVENTEC_RESTORE_TO_DEFAULTS_LAN_CONFIGURATION_BITMASK    0x02
#define IPMI_OEM_INVENTEC_RESTORE_TO_DEFAULTS_USER_ACCOUNTS_BITMASK        0x01

#define IPMI_OEM_INVENTEC_GET_RESTORE_STATUS_RESTORE_IN_PROGRESS 0x00
#define IPMI_OEM_INVENTEC_GET_RESTORE_STATUS_RESTORE_COMPLETE    0x01

#define IPMI_OEM_INVENTEC_EEPROM_AT24C256N_SLAVE_ADDRESS 0x53
#define IPMI_OEM_INVENTEC_EEPROM_AT24C256N_BUS_ID        2
#define IPMI_OEM_INVENTEC_EEPROM_AT24C256N_ADDRESS_MIN   0x0000
#define IPMI_OEM_INVENTEC_EEPROM_AT24C256N_ADDRESS_MAX   0x7FFF
#define IPMI_OEM_INVENTEC_EEPROM_AT24C256N_CLEAR_BYTE    0xFF

#define IPMI_OEM_BUFLEN 1024

static int
_inventec_get_reservation (ipmi_oem_state_data_t *state_data,
                           uint8_t *reservation_id)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  int rs_len;
  int rv = -1;

  /* Inventec 5441/5442 OEM
   *
   * Get Reservation Request
   *
   * 0x30 - OEM network function
   * 0x01 - OEM cmd
   *
   * Get Reservation Response 
   *
   * 0x01 - OEM cmd
   * 0x?? - Completion Code
   * 0x?? - Reservation ID
   */

  assert (state_data);
  assert (reservation_id);

  bytes_rq[0] = IPMI_CMD_OEM_INVENTEC_RESERVED_EXTENDED_CONFIGUATION;

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_INVENTEC_GENERIC_RQ, /* network function */
                              bytes_rq, /* data */
                              1, /* num bytes */
                              bytes_rs,
                              IPMI_OEM_MAX_BYTES)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_raw: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  if (ipmi_oem_check_response_and_completion_code (state_data,
                                                   bytes_rs,
                                                   rs_len,
                                                   3,
                                                   IPMI_CMD_OEM_INVENTEC_RESERVED_EXTENDED_CONFIGUATION,
                                                   IPMI_NET_FN_OEM_INVENTEC_GENERIC_RS,
                                                   NULL) < 0)
    goto cleanup;
  
  (*reservation_id) = bytes_rs[2];

  rv = 0;
 cleanup:
  return (rv);
}

static int
_ipmi_oem_inventec_get_extended_config_value (ipmi_oem_state_data_t *state_data,
                                              uint8_t configuration_id,
                                              uint8_t attribute_id,
                                              uint8_t index,
                                              unsigned int value_return_length,
                                              uint32_t *value)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  int rs_len;
  uint8_t reservation_id;
  int rv = -1;

  assert (state_data);
  assert (value_return_length == 1
          || value_return_length == 2
          || value_return_length == 4);
  assert (value);

  /* Inventec 5441/5442 OEM
   *
   * Get Extended Configuration Request
   *
   * 0x30 - OEM network function
   * 0x02 - OEM cmd
   * 0x?? - Reservation ID
   * 0x?? - Configuration ID
   * 0x?? - Attribute ID
   * 0x00 - Index 
   * 0x00 - Data Offset - LSB (unused here??)
   * 0x00 = Data Offset - MSB (unused here??)
   * 0xFF - Bytes to read (0xFF = all)
   * 
   * Get Extended Configuration Response
   *
   * 0x02 - OEM cmd
   * 0x?? - Completion Code
   * 0x?? - Configuration ID
   * 0x?? - Attribute ID
   * 0x00 - Index
   * 0x?? - number of bytes returned
   * bytes ...
   */

  if (_inventec_get_reservation (state_data,
                                 &reservation_id) < 0)
    goto cleanup;

  bytes_rq[0] = IPMI_CMD_OEM_INVENTEC_GET_EXTENDED_CONFIGURATION;
  bytes_rq[1] = reservation_id;
  bytes_rq[2] = configuration_id;
  bytes_rq[3] = attribute_id;
  bytes_rq[4] = index;
  bytes_rq[5] = 0x00;
  bytes_rq[6] = 0x00;
  bytes_rq[7] = IPMI_OEM_INVENTEC_EXTENDED_CONFIG_READ_ALL_BYTES;
  
  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_INVENTEC_GENERIC_RQ, /* network function */
                              bytes_rq, /* data */
                              8, /* num bytes */
                              bytes_rs,
                              IPMI_OEM_MAX_BYTES)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_raw: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  if (ipmi_oem_check_response_and_completion_code (state_data,
                                                   bytes_rs,
                                                   rs_len,
                                                   6 + value_return_length,
                                                   IPMI_CMD_OEM_INVENTEC_GET_EXTENDED_CONFIGURATION,
                                                   IPMI_NET_FN_OEM_INVENTEC_GENERIC_RS,
                                                   NULL) < 0)
    goto cleanup;

  (*value) = 0;
  if (value_return_length == 1)
    (*value) = bytes_rs[6];
  else if (value_return_length == 2)
    {
      (*value) = bytes_rs[6];
      (*value) |= (bytes_rs[7] << 8);
    }
  else
    {
      (*value) = bytes_rs[6];
      (*value) |= (bytes_rs[7] << 8);
      (*value) |= (bytes_rs[8] << 16);
      (*value) |= (bytes_rs[9] << 24);
    }

  rv = 0;
 cleanup:
  return (rv);
}

static int
_ipmi_oem_inventec_get_extended_config_string (ipmi_oem_state_data_t *state_data,
                                               uint8_t configuration_id,
                                               uint8_t attribute_id,
                                               uint8_t index,
                                               char *buf,
                                               unsigned int buflen)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  int rs_len;
  uint8_t reservation_id;
  int rv = -1;

  assert (state_data);
  assert (buf);
  assert (buflen);

  /* Inventec 5441/5442 OEM
   *
   * Get Extended Configuration Request
   *
   * 0x30 - OEM network function
   * 0x02 - OEM cmd
   * 0x?? - Reservation ID
   * 0x?? - Configuration ID
   * 0x?? - Attribute ID
   * 0x00 - Index
   * 0x00 - Data Offset - LSB (unused here??)
   * 0x00 = Data Offset - MSB (unused here??)
   * 0xFF - Bytes to read (0xFF = all)
   * 
   * Get Extended Configuration Response
   *
   * 0x02 - OEM cmd
   * 0x?? - Completion Code
   * 0x?? - Configuration ID
   * 0x?? - Attribute ID
   * 0x00 - Index
   * 0x?? - number of bytes returned
   * bytes ...
   */

  if (_inventec_get_reservation (state_data,
                                 &reservation_id) < 0)
    goto cleanup;

  bytes_rq[0] = IPMI_CMD_OEM_INVENTEC_GET_EXTENDED_CONFIGURATION;
  bytes_rq[1] = reservation_id;
  bytes_rq[2] = configuration_id;
  bytes_rq[3] = attribute_id;
  bytes_rq[4] = index;
  bytes_rq[5] = 0x00;
  bytes_rq[6] = 0x00;
  bytes_rq[7] = IPMI_OEM_INVENTEC_EXTENDED_CONFIG_READ_ALL_BYTES;
  
  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_INVENTEC_GENERIC_RQ, /* network function */
                              bytes_rq, /* data */
                              8, /* num bytes */
                              bytes_rs,
                              IPMI_OEM_MAX_BYTES)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_raw: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  if (ipmi_oem_check_response_and_completion_code (state_data,
                                                   bytes_rs,
                                                   rs_len,
                                                   6,
                                                   IPMI_CMD_OEM_INVENTEC_GET_EXTENDED_CONFIGURATION,
                                                   IPMI_NET_FN_OEM_INVENTEC_GENERIC_RS,
                                                   NULL) < 0)
    goto cleanup;

  memset (buf, '\0', buflen);
  if ((rs_len - 6) > 0)
    {
      uint8_t len;

      /* According to docs - all strings are stored as P-strings */
      
      len = bytes_rs[6];
      
      if (len != (rs_len - 7))
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "P-string length returned invalid: len = %u, rs_len = %u\n",
                           len, rs_len);
          goto cleanup;
        }

      if ((rs_len - 7) > buflen)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "internal buffer overflow: rs_len = %u, buflen = %u\n",
                           rs_len, buflen);
          goto cleanup;
        }
      memcpy (buf, &bytes_rs[7], rs_len - 7);
    }

  rv = 0;
 cleanup:
  return (rv);
}

static int
_ipmi_oem_inventec_set_extended_config_value (ipmi_oem_state_data_t *state_data,
                                              uint8_t configuration_id,
                                              uint8_t attribute_id,
                                              uint8_t index,
                                              unsigned int value_length,
                                              uint32_t value)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  int rs_len;
  uint8_t reservation_id;
  int rv = -1;

  assert (state_data);
  assert (value_length == 1
          || value_length == 2
          || value_length == 4);

  /* Inventec 5441/5442 OEM
   *
   * Set Extended Configuration Request
   *
   * 0x30 - OEM network function
   * 0x03 - OEM cmd
   * 0x?? - Reservation ID
   * 0x?? - Configuration ID
   * 0x?? - Attribute ID
   * 0x00 - Index
   * 0x00 - Data Offset - LSB (unused here??)
   * 0x00 = Data Offset - MSB (unused here??)
   * 0x01 - In progress bit (0x00 in progress, 0x01 - last config in this request)
   * bytes ... 
   * 
   * Set Extended Configuration Response
   *
   * 0x03 - OEM cmd
   * 0x?? - Completion Code
   * 0x?? - bytes written
   */

  if (_inventec_get_reservation (state_data,
                                 &reservation_id) < 0)
    goto cleanup;

  bytes_rq[0] = IPMI_CMD_OEM_INVENTEC_SET_EXTENDED_CONFIGURATION;
  bytes_rq[1] = reservation_id;
  bytes_rq[2] = configuration_id;
  bytes_rq[3] = attribute_id;
  bytes_rq[4] = index;
  bytes_rq[5] = 0x00;
  bytes_rq[6] = 0x00;
  bytes_rq[7] = 0x01;

  if (value_length == 1)
    bytes_rq[8] = (value & 0x000000FF);
  else if (value_length == 2)
    {
      bytes_rq[8] = (value & 0x000000FF);
      bytes_rq[9] = (value & 0x0000FF00) >> 8;
    }
  else
    {
      bytes_rq[8] = (value & 0x000000FF);
      bytes_rq[9] = (value & 0x0000FF00) >> 8;
      bytes_rq[10] = (value & 0x00FF0000) >> 16;
      bytes_rq[11] = (value & 0xFF000000) >> 24;
    }

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_INVENTEC_GENERIC_RQ, /* network function */
                              bytes_rq, /* data */
                              8 + value_length, /* num bytes */
                              bytes_rs,
                              IPMI_OEM_MAX_BYTES)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_raw: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  if (ipmi_oem_check_response_and_completion_code (state_data,
                                                   bytes_rs,
                                                   rs_len,
                                                   2,
                                                   IPMI_CMD_OEM_INVENTEC_SET_EXTENDED_CONFIGURATION,
                                                   IPMI_NET_FN_OEM_INVENTEC_GENERIC_RS,
                                                   NULL) < 0)
    goto cleanup;

  rv = 0;
 cleanup:
  return (rv);
}

static int
_ipmi_oem_inventec_set_extended_config_string (ipmi_oem_state_data_t *state_data,
                                               uint8_t configuration_id,
                                               uint8_t attribute_id,
                                               uint8_t index,
                                               char *buf,
                                               unsigned int buflen)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  int rs_len;
  uint8_t reservation_id;
  int rv = -1;

  assert (state_data);
  assert (buf);

  /* Inventec 5441/5442 OEM
   *
   * Set Extended Configuration Request
   *
   * 0x30 - OEM network function
   * 0x03 - OEM cmd
   * 0x?? - Reservation ID
   * 0x?? - Configuration ID
   * 0x?? - Attribute ID
   * 0x00 - Index
   * 0x00 - Data Offset - LSB (unused here??)
   * 0x00 = Data Offset - MSB (unused here??)
   * 0x01 - In progress bit (0x00 in progress, 0x01 - last config in this request)
   * bytes ... 
   * 
   * Set Extended Configuration Response
   *
   * 0x03 - OEM cmd
   * 0x?? - Completion Code
   * 0x?? - bytes written
   */

  if (_inventec_get_reservation (state_data,
                                 &reservation_id) < 0)
    goto cleanup;

  bytes_rq[0] = IPMI_CMD_OEM_INVENTEC_SET_EXTENDED_CONFIGURATION;
  bytes_rq[1] = reservation_id;
  bytes_rq[2] = configuration_id;
  bytes_rq[3] = attribute_id;
  bytes_rq[4] = index;
  bytes_rq[5] = 0x00;
  bytes_rq[6] = 0x00;
  bytes_rq[7] = 0x01;
  bytes_rq[8] = strlen (buf);

  if (buflen)
    memcpy (&bytes_rq[9], buf, buflen);
  
  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_INVENTEC_GENERIC_RQ, /* network function */
                              bytes_rq, /* data */
                              9 + buflen, /* num bytes */
                              bytes_rs,
                              IPMI_OEM_MAX_BYTES)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_raw: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }
  
  if (ipmi_oem_check_response_and_completion_code (state_data,
                                                   bytes_rs,
                                                   rs_len,
                                                   2,
                                                   IPMI_CMD_OEM_INVENTEC_SET_EXTENDED_CONFIGURATION,
                                                   IPMI_NET_FN_OEM_INVENTEC_GENERIC_RS,
                                                   NULL) < 0)
    goto cleanup;

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_inventec_get_nic_mode (ipmi_oem_state_data_t *state_data)
{
  uint32_t tmpvalue;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  /* Inventec 5441/5442 OEM
   *
   * achu: Dell appears to have also implemented an additional OEM
   * command that duplicates this behavior.  Currently, we do not
   * implement the Dell equivalent in ipmi-oem.
   *
   * Get LAN Source Request
   *
   * 0x34 - OEM network function
   * 0x14 - OEM cmd
   *
   * Get LAN Source Response
   *
   * 0x14 - OEM cmd
   * 0x?? - Completion Code
   * 0x?? - LAN Source Setting
   *      - 00h = shared, 01h = dedicated
   */

  if (_ipmi_oem_inventec_get_extended_config_value (state_data,
                                                    IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_NIC,
                                                    IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_NIC_MODE,
                                                    0,
                                                    1,
                                                    &tmpvalue) < 0)
    goto cleanup;

  switch (tmpvalue)
    {
    case IPMI_OEM_INVENTEC_EXTENDED_CONFIG_NIC_MODE_SHARED:
      pstdout_printf (state_data->pstate, "shared\n");
      break;
    case IPMI_OEM_INVENTEC_EXTENDED_CONFIG_NIC_MODE_DEDICATED:
      pstdout_printf (state_data->pstate, "dedicated\n");
      break;
    default:
      pstdout_printf (state_data->pstate, "unknown NIC mode: %Xh\n", tmpvalue);
      break;
    }

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_inventec_set_nic_mode (ipmi_oem_state_data_t *state_data)
{
  uint8_t mode;
  int rv = -1;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 1);

  if (strcasecmp (state_data->prog_data->args->oem_options[0], "shared")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "dedicated"))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[0]);
      goto cleanup;
    }

  /* Inventec 5441/5442 OEM
   *
   * achu: Dell appears to have also implemented an additional OEM
   * command that duplicates this behavior.  Currently, we do not
   * implement the Dell equivalent in ipmi-oem.
   *
   * Set LAN Source Request
   *
   * 0x34 - OEM network function
   * 0x13 - OEM cmd
   * 0x?? - LAN Source
   *      - 00h = shared, 01h = dedicated
   *
   * Set LAN Source Response
   *
   * 0x13 - OEM cmd
   * 0x?? - Completion Code
   * 0x?? - LAN Source Setting
   */

  if (!strcasecmp (state_data->prog_data->args->oem_options[0], "shared"))
    mode = IPMI_OEM_INVENTEC_EXTENDED_CONFIG_NIC_MODE_SHARED;
  else
    mode = IPMI_OEM_INVENTEC_EXTENDED_CONFIG_NIC_MODE_DEDICATED;

  if (_ipmi_oem_inventec_set_extended_config_value (state_data,
                                                    IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_NIC,
                                                    IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_NIC_MODE,
                                                    0,
                                                    1,
                                                    (uint32_t)mode) < 0)
    goto cleanup;

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_inventec_get_mac_address (ipmi_oem_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  char mac_address_str[IPMI_OEM_INVENTEC_MAX_MACADDRLEN+1];
  uint8_t mac_address_bytes[6];
  uint8_t lan_channel_number;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  /* Use normal IPMI to get the MAC address.  This is offered more as
   * a convenience to the user rather, so there is always a "get" and
   * a "set" command in ipmi-oem.
   */

  if (ipmi_get_channel_number (state_data->ipmi_ctx,
                               IPMI_CHANNEL_MEDIUM_TYPE_LAN_802_3,
                               &lan_channel_number) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_get_channel_number: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_lan_configuration_parameters_mac_address_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }
  
  if (ipmi_cmd_get_lan_configuration_parameters_mac_address (state_data->ipmi_ctx,
                                                             lan_channel_number,
                                                             IPMI_GET_LAN_PARAMETER,
                                                             IPMI_LAN_CONFIGURATION_PARAMETERS_NO_SET_SELECTOR,
                                                             IPMI_LAN_CONFIGURATION_PARAMETERS_NO_BLOCK_SELECTOR,
                                                             obj_cmd_rs) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_get_lan_configuration_parameters_mac_address: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  if (fiid_obj_get_data (obj_cmd_rs,
                         "mac_address",
                         mac_address_bytes,
                         6) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get_data: 'mac_address': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  
  memset (mac_address_str, '\0', IPMI_OEM_INVENTEC_MAX_MACADDRLEN+1);
  snprintf (mac_address_str,
            IPMI_OEM_INVENTEC_MAX_MACADDRLEN,
            "%02X:%02X:%02X:%02X:%02X:%02X",
            mac_address_bytes[0],
            mac_address_bytes[1],
            mac_address_bytes[2],
            mac_address_bytes[3],
            mac_address_bytes[4],
            mac_address_bytes[5]);

  pstdout_printf (state_data->pstate,
                  "%s\n",
                  mac_address_str);

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

int
ipmi_oem_inventec_set_mac_address (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  int rs_len;
  unsigned int tmp;
  uint8_t cmd;
  int rv = -1;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 2);

  if (strcasecmp (state_data->prog_data->args->oem_options[0], "shared")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "dedicated"))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[0]);
      goto cleanup;
    }

  if (sscanf (state_data->prog_data->args->oem_options[1],
              "%02x:%02x:%02x:%02x:%02x:%02x",
              &tmp,
              &tmp,
              &tmp,
              &tmp,
              &tmp,
              &tmp) != 6)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[1]);
      goto cleanup;
    }

  /* Inventec 5441/5442 OEM
   *
   * Set MAC Address Request
   *
   * 0x2E - OEM network function (is IPMI_NET_FN_OEM_GROUP_RQ)
   * 0x21 | 0x23 - OEM cmd - 0x21 = dedicated, 0x23 = shared
   * bytes 1-17: MAC address in ASCII (including semicolons)
   * 0x00 - sentinel value 0x00
   * 
   * Set MAC Address Response
   *
   * 0x21 | 0x23 - OEM cmd - 0x21 = dedicated, 0x23 = shared
   * 0x?? - Completion Code
   */

  if (!strcasecmp (state_data->prog_data->args->oem_options[0], "dedicated"))
    cmd = IPMI_CMD_OEM_INVENTEC_SET_DEDICATED_MAC_ADDRESS;
  else
    cmd = IPMI_CMD_OEM_INVENTEC_SET_SHARED_MAC_ADDRESS;
  
  bytes_rq[0] = cmd;
  memcpy (&bytes_rq[1],
          state_data->prog_data->args->oem_options[1],
          17);
  bytes_rq[18] = 0x00;
  
  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_GROUP_RQ, /* network function */
                              bytes_rq, /* data */
                              19, /* num bytes */
                              bytes_rs,
                              IPMI_OEM_MAX_BYTES)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_raw: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }
  
  if (ipmi_oem_check_response_and_completion_code (state_data,
                                                   bytes_rs,
                                                   rs_len,
                                                   2,
                                                   cmd,
                                                   IPMI_NET_FN_OEM_GROUP_RS,
                                                   NULL) < 0)
    goto cleanup;
  
  rv = 0;
 cleanup:
  return (rv);
}

static int
_inventec_get_bmc_services (ipmi_oem_state_data_t *state_data,
                            uint8_t *services)
{
  uint32_t tmpvalue;
  int rv = -1;

  assert (state_data);
  assert (services);

  if (_ipmi_oem_inventec_get_extended_config_value (state_data,
                                                    IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_SECURITY,
                                                    IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_SECURITY_SERVICE_DISABLED,
                                                    0,
                                                    1,
                                                    &tmpvalue) < 0)
    goto cleanup;

  (*services) = tmpvalue;

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_inventec_get_bmc_services (ipmi_oem_state_data_t *state_data)
{
  uint8_t services = 0;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  if (_inventec_get_bmc_services (state_data, &services) < 0)
    goto cleanup;

  if (services)
    {
      /* achu: it is not clear if only one bit or multiple bits can be
       * set.  I'm assuming if the "all" bit is set, there is no need
       * to output anything else.
       */
      if (services & IPMI_OEM_INVENTEC_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_ALL)
        {
          pstdout_printf (state_data->pstate, "All services except IPMI disabled\n");
          goto out;
        }
      if (services & IPMI_OEM_INVENTEC_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_KVM)
        pstdout_printf (state_data->pstate, "KVM/Virtual Storage disabled\n");
      if (services & IPMI_OEM_INVENTEC_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_HTTP)
        pstdout_printf (state_data->pstate, "HTTP/HTTPS disabled\n");
      if (services & IPMI_OEM_INVENTEC_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_SSH)
        pstdout_printf (state_data->pstate, "SSH/Telnet disabled\n");
    }
  else
    pstdout_printf (state_data->pstate, "All services enabled\n");

 out:
  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_inventec_set_bmc_services (ipmi_oem_state_data_t *state_data)
{
  int enable = 0;
  uint8_t services = 0;
  int rv = -1;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 2);

  if (strcasecmp (state_data->prog_data->args->oem_options[0], "enable")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "disable"))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[0]);
      goto cleanup;
    }

  if (strcasecmp (state_data->prog_data->args->oem_options[1], "all")
      && strcasecmp (state_data->prog_data->args->oem_options[1], "kvm")
      && strcasecmp (state_data->prog_data->args->oem_options[1], "http")
      && strcasecmp (state_data->prog_data->args->oem_options[1], "ssh"))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[1]);
      goto cleanup;
    }

  if (!strcasecmp (state_data->prog_data->args->oem_options[0], "enable"))
    enable = 1;
        
  /* if all, it's an easy special case */
  if (!strcasecmp (state_data->prog_data->args->oem_options[1], "all"))
    {
      if (enable)
        services = IPMI_OEM_INVENTEC_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_ENABLE_ALL;
      else
        services = IPMI_OEM_INVENTEC_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_ALL;
    }
  else
    {
      if (_inventec_get_bmc_services (state_data, &services) < 0)
        goto cleanup;

      if (enable && (services & IPMI_OEM_INVENTEC_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_ALL))
        {
          /* clear out "all" bit, and replace with remaining bits */
          services &= (~IPMI_OEM_INVENTEC_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_ALL);
          services |= IPMI_OEM_INVENTEC_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_KVM;
          services |= IPMI_OEM_INVENTEC_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_HTTP;
          services |= IPMI_OEM_INVENTEC_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_SSH;
        }

      if (!strcasecmp (state_data->prog_data->args->oem_options[1], "kvm"))
        {
          if (enable)
            services &= (~IPMI_OEM_INVENTEC_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_KVM);
          else
            services |= IPMI_OEM_INVENTEC_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_KVM;
        }
      else if (!strcasecmp (state_data->prog_data->args->oem_options[1], "http"))
        {
          if (enable)
            services &= (~IPMI_OEM_INVENTEC_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_HTTP);
          else
            services |= IPMI_OEM_INVENTEC_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_HTTP;
        }
      else /* !strcasecmp (state_data->prog_data->args->oem_options[1], "ssh") */
        {
          if (enable)
            services &= (~IPMI_OEM_INVENTEC_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_SSH);
          else
            services |= IPMI_OEM_INVENTEC_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_SSH;
        }
    }


  if (_ipmi_oem_inventec_set_extended_config_value (state_data,
                                                    IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_SECURITY,
                                                    IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_SECURITY_SERVICE_DISABLED,
                                                    0,
                                                    1,
                                                    (uint32_t)services) < 0)
    goto cleanup;

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_inventec_get_authentication_config (ipmi_oem_state_data_t *state_data)
{
  uint32_t tmpvalue;
  uint8_t maxauthenticationfailures;
  uint16_t lockoutwindow;
  uint16_t lockouttime;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  if (_ipmi_oem_inventec_get_extended_config_value (state_data,
                                                    IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_SECURITY,
                                                    IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_SECURITY_MAX_AUTHENTICATION_FAILURES,
                                                    0,
                                                    1,
                                                    &tmpvalue) < 0)
    goto cleanup;
  maxauthenticationfailures = tmpvalue;
  
  if (_ipmi_oem_inventec_get_extended_config_value (state_data,
                                                    IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_SECURITY,
                                                    IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_SECURITY_LOCKOUT_WINDOW,
                                                    0,
                                                    2,
                                                    &tmpvalue) < 0)
    goto cleanup;
  lockoutwindow = tmpvalue;
  
  if (_ipmi_oem_inventec_get_extended_config_value (state_data,
                                                    IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_SECURITY,
                                                    IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_SECURITY_LOCKOUT_TIME,
                                                    0,
                                                    2,
                                                    &tmpvalue) < 0)
    goto cleanup;
  lockouttime = tmpvalue;
  
  pstdout_printf (state_data->pstate,
		  "Max Authentication Failures : %u\n",
                  maxauthenticationfailures);
  
  pstdout_printf (state_data->pstate,
		  "Lockout Window              : %u seconds\n",
		  lockoutwindow);
  
  pstdout_printf (state_data->pstate,
		  "Lockout Time                : %u seconds\n",
		  lockouttime);

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_inventec_set_authentication_config (ipmi_oem_state_data_t *state_data)
{
  uint8_t maxauthenticationfailures = 0;
  uint16_t lockoutwindow = 0;
  uint16_t lockouttime = 0;
  int rv = -1;
  unsigned int i;

  assert (state_data);

  if (!state_data->prog_data->args->oem_options_count)
    {
      pstdout_printf (state_data->pstate,
		      "Option: maxauthenticationfailures=count\n"
		      "Option: lockoutwindow=seconds\n"
		      "Option: lockouttime=seconds\n");
      return (0); 
    }

  for (i = 0; i < state_data->prog_data->args->oem_options_count; i++)
    {
      char *key = NULL;
      char *value = NULL;
      
      if (ipmi_oem_parse_key_value (state_data,
                                    i,
                                    &key,
                                    &value) < 0)
        goto cleanup;

      if (!strcasecmp (key, "maxauthenticationfailures"))
        {
          if (ipmi_oem_parse_1_byte_field (state_data, i, value, &maxauthenticationfailures) < 0)
            goto cleanup;

          if (_ipmi_oem_inventec_set_extended_config_value (state_data,
                                                            IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_SECURITY,
                                                            IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_SECURITY_MAX_AUTHENTICATION_FAILURES,
                                                            0,
                                                            1,
                                                            (uint32_t)maxauthenticationfailures) < 0)
            goto cleanup;
        }
      else if (!strcasecmp (key, "lockoutwindow"))
        {
          if (ipmi_oem_parse_2_byte_field (state_data, i, value, &lockoutwindow) < 0)
            goto cleanup;
          
          if (_ipmi_oem_inventec_set_extended_config_value (state_data,
                                                            IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_SECURITY,
                                                            IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_SECURITY_LOCKOUT_WINDOW,
                                                            0,
                                                            2,
                                                            (uint32_t)lockoutwindow) < 0)
            goto cleanup;
        }
      else if (!strcasecmp (key, "lockouttime"))
        {
          if (ipmi_oem_parse_2_byte_field (state_data, i, value, &lockouttime) < 0)
            goto cleanup;
          
          if (_ipmi_oem_inventec_set_extended_config_value (state_data,
                                                            IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_SECURITY,
                                                            IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_SECURITY_LOCKOUT_TIME,
                                                            0,
                                                            2,
                                                            (uint32_t)lockouttime) < 0)
            goto cleanup;
        }
      else
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "%s:%s invalid OEM option argument '%s' : invalid key\n",
                           state_data->prog_data->args->oem_id,
                           state_data->prog_data->args->oem_command,
                           state_data->prog_data->args->oem_options[i]);
          goto cleanup;
        }

      free (key);
      free (value);
    }

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_inventec_get_account_status (ipmi_oem_state_data_t *state_data)
{
  fiid_obj_t get_user_access_obj_cmd_rs = NULL;
  fiid_obj_t get_user_name_obj_cmd_rs = NULL;
  uint32_t tmpvalue;
  uint8_t lan_channel_number;
  uint8_t number_of_users;
  uint64_t val;
  unsigned int i;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  /* achu:
   *
   * IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_ACCOUNT_STATUS_NUMBER_OF_USER
   * returns the number of enabled/disabled users, not the total,
   * which is not what we need in this function.
   */

  if (!(get_user_access_obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_user_access_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_get_channel_number (state_data->ipmi_ctx,
                               IPMI_CHANNEL_MEDIUM_TYPE_LAN_802_3,
                               &lan_channel_number) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_get_channel_number: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }
  
  if (ipmi_cmd_get_user_access (state_data->ipmi_ctx,
                                lan_channel_number,
                                1, /* user_id number - any will do for this call */
                                get_user_access_obj_cmd_rs) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_get_user_access: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  if (FIID_OBJ_GET (get_user_access_obj_cmd_rs,
                    "max_channel_user_ids",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "FIID_OBJ_GET: 'max_channel_user_ids': %s\n",
                       fiid_obj_errormsg (get_user_access_obj_cmd_rs));
      goto cleanup;
    }

  if (!(get_user_name_obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_user_name_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  number_of_users = val;
  
  pstdout_printf (state_data->pstate,
                  "Username         | Status\n");
  
  for (i = 0; i < number_of_users; i++)
    {
      char user_name[IPMI_MAX_USER_NAME_LENGTH + 1];
      uint8_t account_status;
      char *account_status_str = NULL;

      memset (user_name, '\0', IPMI_MAX_USER_NAME_LENGTH + 1);

      /* achu:
       *
       * Docs say
       * IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_ACCOUNT_STATUS_USER_NAME
       * string should be returned a P-string.  On Inventec
       * 5441/Xanadu II returned as ASCII, on Inventec 5442/Xanadu III
       * strings returned as P-string.  Use normal IPMI get username
       * function instead.
       * 
       */

      if (fiid_obj_clear (get_user_name_obj_cmd_rs) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_clear: %s\n",
                           fiid_obj_errormsg (get_user_name_obj_cmd_rs));
          goto cleanup;
        }
      
      if (ipmi_cmd_get_user_name (state_data->ipmi_ctx,
                                  i + 1,
                                  get_user_name_obj_cmd_rs) < 0)
        {
          /* Username is not set yet */
          if (ipmi_ctx_errnum (state_data->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE
              && (ipmi_check_completion_code (get_user_name_obj_cmd_rs,
                                              IPMI_COMP_CODE_INVALID_DATA_FIELD_IN_REQUEST) == 1))
            continue;

          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "ipmi_cmd_get_user_name: %s\n",
                           ipmi_ctx_errormsg (state_data->ipmi_ctx));
          goto cleanup;
        }

      if (fiid_obj_get_data (get_user_name_obj_cmd_rs,
                             "user_name",
                             user_name,
                             IPMI_MAX_USER_NAME_LENGTH) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_get_data: 'user_name': %s\n",
                           fiid_obj_errormsg (get_user_name_obj_cmd_rs));
          goto cleanup;
        }
      
      /* if not the first user id - but username is empty, skip output */
      if (i && !strlen (user_name))
        continue;

      if (_ipmi_oem_inventec_get_extended_config_value (state_data,
                                                        IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_ACCOUNT_STATUS,
                                                        IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_ACCOUNT_STATUS_ACCOUNT_STATUS,
                                                        i + 1,
                                                        1,
                                                        &tmpvalue) < 0)
        goto cleanup;
      account_status = tmpvalue;

      if (account_status == IPMI_OEM_INVENTEC_EXTENDED_CONFIG_ACCOUNT_STATUS_UNSPECIFIED)
        account_status_str = "Unspecified";
      else if (account_status == IPMI_OEM_INVENTEC_EXTENDED_CONFIG_ACCOUNT_STATUS_ENABLED)
        account_status_str = "Enabled";
      else if (account_status == IPMI_OEM_INVENTEC_EXTENDED_CONFIG_ACCOUNT_STATUS_DISABLED)
        account_status_str = "Disabled";
      else 
        account_status_str = "Lockout";

      pstdout_printf (state_data->pstate,
                      "%-16s | %s\n",
                      user_name,
                      account_status_str);
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (get_user_access_obj_cmd_rs);
  return (rv);
}

int
ipmi_oem_inventec_get_dns_config (ipmi_oem_state_data_t *state_data)
{
  uint32_t tmpvalue;
  uint8_t dnsdhcpenable;
  uint32_t dnsserver1;
  uint32_t dnsserver2;
  uint8_t dnsregisterbmc;
  char dnsbmchostname[IPMI_OEM_INVENTEC_EXTENDED_CONFIG_DNS_DNS_BMC_HOST_NAME_MAX + 1];
  uint8_t dnsdomainnamedhcpenable;
  char dnsdomainname[IPMI_OEM_INVENTEC_EXTENDED_CONFIG_DNS_DNS_DOMAIN_NAME_MAX + 1];
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  memset (dnsbmchostname, '\0', IPMI_OEM_INVENTEC_EXTENDED_CONFIG_DNS_DNS_BMC_HOST_NAME_MAX + 1);
  memset (dnsdomainname, '\0', IPMI_OEM_INVENTEC_EXTENDED_CONFIG_DNS_DNS_DOMAIN_NAME_MAX + 1);

  if (_ipmi_oem_inventec_get_extended_config_value (state_data,
                                                    IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_DNS,
                                                    IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_DNS_DNS_DHCP_ENABLE,
                                                    0,
                                                    1,
                                                    &tmpvalue) < 0)
    goto cleanup;
  dnsdhcpenable = tmpvalue;

  if (_ipmi_oem_inventec_get_extended_config_value (state_data,
                                                    IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_DNS,
                                                    IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_DNS_DNS_SERVER1,
                                                    0,
                                                    4,
                                                    &tmpvalue) < 0)
    goto cleanup;
   dnsserver1 = tmpvalue;

  if (_ipmi_oem_inventec_get_extended_config_value (state_data,
                                                    IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_DNS,
                                                    IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_DNS_DNS_SERVER2,
                                                    0,
                                                    4,
                                                    &tmpvalue) < 0)
    goto cleanup;
   dnsserver2 = tmpvalue;
  
  if (_ipmi_oem_inventec_get_extended_config_value (state_data,
                                                    IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_DNS,
                                                    IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_DNS_DNS_REGISTER_BMC,
                                                    0,
                                                    1,
                                                    &tmpvalue) < 0)
    goto cleanup;
  dnsregisterbmc = tmpvalue;

  if (_ipmi_oem_inventec_get_extended_config_string (state_data,
                                                     IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_DNS,
                                                     IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_DNS_DNS_BMC_HOST_NAME,
                                                     0,
                                                     dnsbmchostname,
                                                     IPMI_OEM_INVENTEC_EXTENDED_CONFIG_DNS_DNS_BMC_HOST_NAME_MAX) < 0)
    goto cleanup;

  if (_ipmi_oem_inventec_get_extended_config_value (state_data,
                                                    IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_DNS,
                                                    IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_DNS_DNS_DOMAIN_NAME_DHCP_ENABLE,
                                                    0,
                                                    1,
                                                    &tmpvalue) < 0)
    goto cleanup;
  dnsdomainnamedhcpenable = tmpvalue;

  if (_ipmi_oem_inventec_get_extended_config_string (state_data,
                                                     IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_DNS,
                                                     IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_DNS_DNS_DOMAIN_NAME,
                                                     0,
                                                     dnsdomainname,
                                                     IPMI_OEM_INVENTEC_EXTENDED_CONFIG_DNS_DNS_DOMAIN_NAME_MAX) < 0)
    goto cleanup;

  pstdout_printf (state_data->pstate,
		  "DNS DHCP             : %s\n",
		  (dnsdhcpenable) ? "Enabled" : "Disabled");

  pstdout_printf (state_data->pstate,
                  "DNS Server 1         : %u.%u.%u.%u\n",
                  (dnsserver1 & 0x000000FF),
                  (dnsserver1 & 0x0000FF00) >> 8,
                  (dnsserver1 & 0x00FF0000) >> 16,
                  (dnsserver1 & 0xFF000000) >> 24);

  pstdout_printf (state_data->pstate,
                  "DNS Server 2         : %u.%u.%u.%u\n",
                  (dnsserver2 & 0x000000FF),
                  (dnsserver2 & 0x0000FF00) >> 8,
                  (dnsserver2 & 0x00FF0000) >> 16,
                  (dnsserver2 & 0xFF000000) >> 24);
  
  pstdout_printf (state_data->pstate,
		  "DNS Register BMC     : %s\n",
		  (dnsdomainnamedhcpenable) ? "Enabled" : "Disabled");
  
  pstdout_printf (state_data->pstate,
                  "DNS BMC Host Name    : %s\n",
                  dnsbmchostname);
  
  pstdout_printf (state_data->pstate,
		  "DNS Domain Name DHCP : %s\n",
		  (dnsdomainnamedhcpenable) ? "Enabled" : "Disabled");
  
  pstdout_printf (state_data->pstate,
                  "DNS Domain Name      : %s\n",
                  dnsdomainname);
  
  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_inventec_set_dns_config (ipmi_oem_state_data_t *state_data)
{
  uint8_t dnsdhcpenable = 0;
  uint32_t dnsserver1 = 0;
  uint32_t dnsserver2 = 0;
  uint8_t dnsregisterbmc = 0;
  char dnsbmchostname[IPMI_OEM_INVENTEC_EXTENDED_CONFIG_DNS_DNS_BMC_HOST_NAME_MAX + 1];
  uint8_t dnsdomainnamedhcpenable = 0;
  char dnsdomainname[IPMI_OEM_INVENTEC_EXTENDED_CONFIG_DNS_DNS_DOMAIN_NAME_MAX + 1];
  int rv = -1;
  unsigned int i;

  assert (state_data);

  memset (dnsbmchostname, '\0', IPMI_OEM_INVENTEC_EXTENDED_CONFIG_DNS_DNS_BMC_HOST_NAME_MAX + 1);
  memset (dnsdomainname, '\0', IPMI_OEM_INVENTEC_EXTENDED_CONFIG_DNS_DNS_DOMAIN_NAME_MAX + 1);

  if (!state_data->prog_data->args->oem_options_count)
    {
      pstdout_printf (state_data->pstate,
		      "Option: dnsdhcp=enable|disable\n"
                      "Option: dnsserver1=ipaddress\n"
                      "Option: dnsserver2=ipaddress\n"
		      "Option: dnsregisterbmc=enable|disable\n"
                      "Option: dnsbmchostname=string\n"
		      "Option: dnsdomainnamedhcp=enable|disable\n"
                      "Option: dnsdomainname=string\n");
      return (0); 
    }

  for (i = 0; i < state_data->prog_data->args->oem_options_count; i++)
    {
      char *key = NULL;
      char *value = NULL;
      
      if (ipmi_oem_parse_key_value (state_data,
                                    i,
                                    &key,
                                    &value) < 0)
        goto cleanup;

      if (!strcasecmp (key, "dnsdhcp"))
        {
          if (ipmi_oem_parse_enable (state_data, i, value, &dnsdhcpenable) < 0)
            goto cleanup;
          
          
          if (_ipmi_oem_inventec_set_extended_config_value (state_data,
                                                            IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_DNS,
                                                            IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_DNS_DNS_DHCP_ENABLE,
                                                            0,
                                                            1,
                                                            (uint32_t)dnsdhcpenable) < 0)
            goto cleanup;
        }
      else if (!strcasecmp (key, "dnsserver1"))
        {
          if (ipmi_oem_parse_ip_address (state_data, i, value, &dnsserver1) < 0)
            goto cleanup;
          
          
          if (_ipmi_oem_inventec_set_extended_config_value (state_data,
                                                            IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_DNS,
                                                            IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_DNS_DNS_SERVER1,
                                                            0,
                                                            4,
                                                            (uint32_t)dnsserver1) < 0)
            goto cleanup;
        }
      else if (!strcasecmp (key, "dnsserver2"))
        {
          if (ipmi_oem_parse_ip_address (state_data, i, value, &dnsserver2) < 0)
            goto cleanup;
          
          
          if (_ipmi_oem_inventec_set_extended_config_value (state_data,
                                                            IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_DNS,
                                                            IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_DNS_DNS_SERVER2,
                                                            0,
                                                            4,
                                                            (uint32_t)dnsserver2) < 0)
            goto cleanup;
        }
      else if (!strcasecmp (key, "dnsregisterbmc"))
        {
          if (ipmi_oem_parse_enable (state_data, i, value, &dnsregisterbmc) < 0)
            goto cleanup;
          
          
          if (_ipmi_oem_inventec_set_extended_config_value (state_data,
                                                            IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_DNS,
                                                            IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_DNS_DNS_REGISTER_BMC,
                                                            0,
                                                            1,
                                                            (uint32_t)dnsregisterbmc) < 0)
            goto cleanup;
        }
      else if (!strcasecmp (key, "dnsbmchostname"))
        {
          uint8_t string_length = 0;

          if (ipmi_oem_parse_string (state_data,
                                     i,
                                     value,
                                     &string_length,
                                     dnsbmchostname,
                                     IPMI_OEM_INVENTEC_EXTENDED_CONFIG_DNS_DNS_BMC_HOST_NAME_MAX) < 0)
            goto cleanup;
          
          if (_ipmi_oem_inventec_set_extended_config_string (state_data,
                                                             IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_DNS,
                                                             IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_DNS_DNS_BMC_HOST_NAME,
                                                             0,
                                                             dnsbmchostname,
                                                             (unsigned int)string_length) < 0)
            goto cleanup;
        }
      else if (!strcasecmp (key, "dnsdomainnamedhcp"))
        {
          if (ipmi_oem_parse_enable (state_data, i, value, &dnsdomainnamedhcpenable) < 0)
            goto cleanup;
          
          
          if (_ipmi_oem_inventec_set_extended_config_value (state_data,
                                                            IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_DNS,
                                                            IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_DNS_DNS_DOMAIN_NAME_DHCP_ENABLE,
                                                            0,
                                                            1,
                                                            (uint32_t)dnsdomainnamedhcpenable) < 0)
            goto cleanup;
        }
      else if (!strcasecmp (key, "dnsdomainname"))
        {
          uint8_t string_length = 0;

          if (ipmi_oem_parse_string (state_data,
                                     i,
                                     value,
                                     &string_length,
                                     dnsdomainname,
                                     IPMI_OEM_INVENTEC_EXTENDED_CONFIG_DNS_DNS_DOMAIN_NAME_MAX) < 0)
            goto cleanup;
          
          if (_ipmi_oem_inventec_set_extended_config_string (state_data,
                                                             IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_DNS,
                                                             IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_DNS_DNS_DOMAIN_NAME,
                                                             0,
                                                             dnsdomainname,
                                                             (unsigned int)string_length) < 0)
            goto cleanup;
        }
      else
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "%s:%s invalid OEM option argument '%s' : invalid key\n",
                           state_data->prog_data->args->oem_id,
                           state_data->prog_data->args->oem_command,
                           state_data->prog_data->args->oem_options[i]);
          goto cleanup;
        }

      free (key);
      free (value);
    }

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_inventec_get_web_server_config (ipmi_oem_state_data_t *state_data)
{
  uint32_t tmpvalue;
  uint8_t webserverenabled;
  uint8_t maxwebsessions;
  uint8_t activewebsessions;
  uint32_t webservertimeout;
  uint16_t httpportnum;
  uint16_t httpsportnum;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  /* Inventec 5441/5442 OEM
   *
   * achu: Dell appears to have also implemented an additional OEM
   * command that duplicates this configuration.  Currently, we do not
   * implement the Dell equivalent in ipmi-oem.
   *
   * achu: The document states "web port" and "http port".  That
   * probably means "http" vs. "https" port.  The below documents this
   * typo.
   *
   * Get Web Port Num Request
   *
   * 0x34 - OEM network function
   * 0x03 - OEM cmd
   *
   * Get Web Port Num Response
   *
   * 0x03 - OEM cmd
   * 0x?? - Completion Code
   * 0x?? - web port num (LSB)
   * 0x?? - web port num (MSB)
   * 0x?? - http num (LSB)
   * 0x?? - http num (MSB)
   */

  if (_ipmi_oem_inventec_get_extended_config_value (state_data,
                                                    IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_WEB_SERVER_CONFIGURATION,
                                                    IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_WEB_SERVER_ENABLED,
                                                    0,
                                                    1,
                                                    &tmpvalue) < 0)
    goto cleanup;
  webserverenabled = tmpvalue;
  
  if (_ipmi_oem_inventec_get_extended_config_value (state_data,
                                                    IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_WEB_SERVER_CONFIGURATION,
                                                    IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_MAX_WEB_SESSIONS,
                                                    0,
                                                    1,
                                                    &tmpvalue) < 0)
    goto cleanup;
  maxwebsessions = tmpvalue;
  
  if (_ipmi_oem_inventec_get_extended_config_value (state_data,
                                                    IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_WEB_SERVER_CONFIGURATION,
                                                    IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_ACTIVE_WEB_SESSIONS,
                                                    0,
                                                    1,
                                                    &tmpvalue) < 0)
    goto cleanup;
  activewebsessions = tmpvalue;
  
  if (_ipmi_oem_inventec_get_extended_config_value (state_data,
                                                    IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_WEB_SERVER_CONFIGURATION,
                                                    IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_WEB_SERVER_TIMEOUT,
                                                    0,
                                                    4,
                                                    &tmpvalue) < 0)
    goto cleanup;
  webservertimeout = tmpvalue;
  
  if (_ipmi_oem_inventec_get_extended_config_value (state_data,
                                                    IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_WEB_SERVER_CONFIGURATION,
                                                    IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_HTTP_PORT_NUM,
                                                    0,
                                                    2,
                                                    &tmpvalue) < 0)
    goto cleanup;
  httpportnum = tmpvalue;
  
  if (_ipmi_oem_inventec_get_extended_config_value (state_data,
                                                    IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_WEB_SERVER_CONFIGURATION,
                                                    IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_HTTPS_PORT_NUM,
                                                    0,
                                                    2,
                                                    &tmpvalue) < 0)
    goto cleanup;
  httpsportnum = tmpvalue;
  
  pstdout_printf (state_data->pstate,
		  "Web Server          : %s\n",
		  (webserverenabled) ? "Enabled" : "Disabled");
  
  pstdout_printf (state_data->pstate,
		  "Max Web Sessions    : %u\n",
		  maxwebsessions);

  pstdout_printf (state_data->pstate,
		  "Active Web Sessions : %u\n",
		  activewebsessions);

  pstdout_printf (state_data->pstate,
		  "Web Server Timeout  : %u seconds\n",
		  webservertimeout);

  pstdout_printf (state_data->pstate,
		  "http Port Number    : %u\n",
		  httpportnum);

  pstdout_printf (state_data->pstate,
		  "https Port Number   : %u\n",
		  httpsportnum);

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_inventec_set_web_server_config (ipmi_oem_state_data_t *state_data)
{
  uint8_t webserverenabled = 0;
  uint32_t webservertimeout = 0;
  uint16_t httpportnumber = 0;
  uint16_t httpsportnumber = 0;
  int rv = -1;
  unsigned int i;

  assert (state_data);

  /* Inventec 5441/5442 OEM
   *
   * achu: Dell appears to have also implemented an additional OEM
   * command that duplicates this configuration.  Currently, we do not
   * implement the Dell equivalent in ipmi-oem.
   *
   * achu: The document states "web port" and "http port".  That
   * probably means "http" vs. "https" port.  The below documents this
   * typo.
   *
   * Set Web Port Num Request
   *
   * 0x34 - OEM network function
   * 0x02 - OEM cmd
   * 0x?? - web port num (LSB)
   * 0x?? - web port num (MSB)
   * 0x?? - http num (LSB)
   * 0x?? - http num (MSB)
   *
   * Set Web Port Num Response
   *
   * 0x02 - OEM cmd
   * 0x?? - Completion Code
   */

  if (!state_data->prog_data->args->oem_options_count)
    {
      pstdout_printf (state_data->pstate,
		      "Option: webserver=enable|disable\n"
		      "Option: webservertimeout=seconds\n"
		      "Option: httpportnumber=num\n"
		      "Option: httpsportnumber=num\n");
      return (0); 
    }

  for (i = 0; i < state_data->prog_data->args->oem_options_count; i++)
    {
      char *key = NULL;
      char *value = NULL;
      
      if (ipmi_oem_parse_key_value (state_data,
                                    i,
                                    &key,
                                    &value) < 0)
        goto cleanup;

      if (!strcasecmp (key, "webserver"))
        {
          if (ipmi_oem_parse_enable (state_data, i, value, &webserverenabled) < 0)
            goto cleanup;

          if (_ipmi_oem_inventec_set_extended_config_value (state_data,
                                                            IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_WEB_SERVER_CONFIGURATION,
                                                            IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_WEB_SERVER_ENABLED,
                                                            0,
                                                            1,
                                                            (uint32_t)webserverenabled) < 0)
            goto cleanup;
        }
      else if (!strcasecmp (key, "webservertimeout"))
        {
          if (ipmi_oem_parse_4_byte_field (state_data, i, value, &webservertimeout) < 0)
            goto cleanup;
          
          if (_ipmi_oem_inventec_set_extended_config_value (state_data,
                                                            IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_WEB_SERVER_CONFIGURATION,
                                                            IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_WEB_SERVER_TIMEOUT,
                                                            0,
                                                            4,
                                                            webservertimeout) < 0)
            goto cleanup;
        }
      else if (!strcasecmp (key, "httpportnumber"))
        {
          if (ipmi_oem_parse_2_byte_field (state_data, i, value, &httpportnumber) < 0)
            goto cleanup;
          
          if (_ipmi_oem_inventec_set_extended_config_value (state_data,
                                                            IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_WEB_SERVER_CONFIGURATION,
                                                            IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_HTTP_PORT_NUM,
                                                            0,
                                                            2,
                                                            (uint32_t)httpportnumber) < 0)
            goto cleanup;
        }
      else if (!strcasecmp (key, "httpsportnumber"))
        {
          if (ipmi_oem_parse_2_byte_field (state_data, i, value, &httpsportnumber) < 0)
            goto cleanup;
          
          if (_ipmi_oem_inventec_set_extended_config_value (state_data,
                                                            IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_WEB_SERVER_CONFIGURATION,
                                                            IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_HTTPS_PORT_NUM,
                                                            0,
                                                            2,
                                                            (uint32_t)httpsportnumber) < 0)
            goto cleanup;
        }
      else
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "%s:%s invalid OEM option argument '%s' : invalid key\n",
                           state_data->prog_data->args->oem_id,
                           state_data->prog_data->args->oem_command,
                           state_data->prog_data->args->oem_options[i]);
          goto cleanup;
        }

      free (key);
      free (value);
    }

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_inventec_get_power_management_config (ipmi_oem_state_data_t *state_data)
{
  uint32_t tmpvalue;
  uint8_t powermanagementenable;
  uint8_t dpnmpowermanagement;
  uint8_t powerstaggeringacrecovery;
  uint16_t powerondelay;
  uint16_t minpowerondelay;
  uint16_t maxpowerondelay;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  if (_ipmi_oem_inventec_get_extended_config_value (state_data,
                                                    IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_POWER_MANAGEMENT,
                                                    IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_POWER_MANAGEMENT_POWER_MANAGEMENT_ENABLE,
                                                    0,
                                                    1,
                                                    &tmpvalue) < 0)
    goto cleanup;
  powermanagementenable = tmpvalue;

  dpnmpowermanagement = (powermanagementenable & IPMI_OEM_INVENTEC_EXTENDED_CONFIG_POWER_MANAGEMENT_ENABLE_DPNM_BITMASK);
  dpnmpowermanagement >>= IPMI_OEM_INVENTEC_EXTENDED_CONFIG_POWER_MANAGEMENT_ENABLE_DPNM_SHIFT;
  
  if (_ipmi_oem_inventec_get_extended_config_value (state_data,
                                                    IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_POWER_MANAGEMENT,
                                                    IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_POWER_MANAGEMENT_POWER_STAGGERING_AC_RECOVERY,
                                                    0,
                                                    1,
                                                    &tmpvalue) < 0)
    goto cleanup;
  powerstaggeringacrecovery = tmpvalue;
  
  if (_ipmi_oem_inventec_get_extended_config_value (state_data,
                                                    IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_POWER_MANAGEMENT,
                                                    IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_POWER_MANAGEMENT_POWER_ON_DELAY,
                                                    0,
                                                    2,
                                                    &tmpvalue) < 0)
    goto cleanup;
  powerondelay = tmpvalue;

  if (_ipmi_oem_inventec_get_extended_config_value (state_data,
                                                    IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_POWER_MANAGEMENT,
                                                    IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_POWER_MANAGEMENT_MINIMUM_POWER_ON_DELAY,
                                                    0,
                                                    2,
                                                    &tmpvalue) < 0)
    goto cleanup;
  minpowerondelay = tmpvalue; 

  if (_ipmi_oem_inventec_get_extended_config_value (state_data,
                                                    IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_POWER_MANAGEMENT,
                                                    IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_POWER_MANAGEMENT_MAXIMUM_POWER_ON_DELAY,
                                                    0,
                                                    2,
                                                    &tmpvalue) < 0)
    goto cleanup;
  maxpowerondelay = tmpvalue; 
  
  pstdout_printf (state_data->pstate,
		  "DPNM Power Management        : %s\n",
		  (dpnmpowermanagement) ? "Enabled" : "Disabled");
  
  if (powerstaggeringacrecovery == IPMI_OEM_INVENTEC_EXTENDED_CONFIG_POWER_STAGGERING_AC_RECOVERY_IMMEDIATE)
    pstdout_printf (state_data->pstate,
                    "Power Staggering AC Recovery : Immediate\n");
  else if (powerstaggeringacrecovery == IPMI_OEM_INVENTEC_EXTENDED_CONFIG_POWER_STAGGERING_AC_RECOVERY_AUTO)
    pstdout_printf (state_data->pstate,
                    "Power Staggering AC Recovery : Auto\n");
  else if (powerstaggeringacrecovery == IPMI_OEM_INVENTEC_EXTENDED_CONFIG_POWER_STAGGERING_AC_RECOVERY_USER_DEFINED)
    pstdout_printf (state_data->pstate,
                    "Power Staggering AC Recovery : User Defined\n");
  else
    pstdout_printf (state_data->pstate,
                    "Power Staggering AC Recovery : %Xh\n",
                    powerstaggeringacrecovery);
  
  pstdout_printf (state_data->pstate,
		  "Power On Delay               : %u seconds\n",
                  powerondelay);
  
  pstdout_printf (state_data->pstate,
		  "Minimum Power On Delay       : %u seconds\n",
                  minpowerondelay);

  pstdout_printf (state_data->pstate,
		  "Maximum Power On Delay       : %u seconds\n",
                  maxpowerondelay);

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_inventec_set_power_management_config (ipmi_oem_state_data_t *state_data)
{
  uint8_t powermanagementenable = 0;
  uint8_t dpnmpowermanagement = 0;
  uint8_t powerstaggeringacrecovery = 0;
  uint16_t powerondelay = 0;
  uint16_t maxpowerondelay = 0;
  int rv = -1;
  unsigned int i;

  assert (state_data);

  if (!state_data->prog_data->args->oem_options_count)
    {
      pstdout_printf (state_data->pstate,
		      "Option: dpnmpowermanagement=enable|disable\n"
		      "Option: powerstaggeringacrecovery=immediate|auto|user\n"
		      "Option: powerondelay=seconds\n"
		      "Option: maxpowerondelay=seconds\n");
      return (0); 
    }

  for (i = 0; i < state_data->prog_data->args->oem_options_count; i++)
    {
      char *key = NULL;
      char *value = NULL;
      
      if (ipmi_oem_parse_key_value (state_data,
                                    i,
                                    &key,
                                    &value) < 0)
        goto cleanup;

      if (!strcasecmp (key, "dpnmpowermanagement"))
        {
          if (ipmi_oem_parse_enable (state_data, i, value, &dpnmpowermanagement) < 0)
            goto cleanup;

          powermanagementenable |= (dpnmpowermanagement << IPMI_OEM_INVENTEC_EXTENDED_CONFIG_POWER_MANAGEMENT_ENABLE_DPNM_SHIFT);
          
          if (_ipmi_oem_inventec_set_extended_config_value (state_data,
                                                            IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_POWER_MANAGEMENT,
                                                            IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_POWER_MANAGEMENT_POWER_MANAGEMENT_ENABLE,
                                                            0,
                                                            1,
                                                            (uint32_t)powermanagementenable) < 0)
            goto cleanup;
        }
      else if (!strcasecmp (key, "powerstaggeringacrecovery"))
        {
          if (strcasecmp (value, "immediate")
              && strcasecmp (value, "auto")
              && strcasecmp (value, "user"))
            {
              pstdout_fprintf (state_data->pstate,
                               stderr,
                               "%s:%s invalid OEM option argument '%s' : invalid value\n",
                               state_data->prog_data->args->oem_id,
                               state_data->prog_data->args->oem_command,
                               state_data->prog_data->args->oem_options[i]);
              goto cleanup;
            }

          if (!strcasecmp (value, "immediate"))
            powerstaggeringacrecovery = IPMI_OEM_INVENTEC_EXTENDED_CONFIG_POWER_STAGGERING_AC_RECOVERY_IMMEDIATE;
          else if (!strcasecmp (value, "auto"))
            powerstaggeringacrecovery = IPMI_OEM_INVENTEC_EXTENDED_CONFIG_POWER_STAGGERING_AC_RECOVERY_AUTO;
          else /* !strcasecmp (value, "user")) */
            powerstaggeringacrecovery = IPMI_OEM_INVENTEC_EXTENDED_CONFIG_POWER_STAGGERING_AC_RECOVERY_USER_DEFINED;

          if (_ipmi_oem_inventec_set_extended_config_value (state_data,
                                                            IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_POWER_MANAGEMENT,
                                                            IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_POWER_MANAGEMENT_POWER_STAGGERING_AC_RECOVERY,
                                                            0,
                                                            1,
                                                            (uint32_t)powerstaggeringacrecovery) < 0)
            goto cleanup;
        }
      else if (!strcasecmp (key, "powerondelay"))
        {
          if (ipmi_oem_parse_2_byte_field (state_data, i, value, &powerondelay) < 0)
            goto cleanup;
          
          if (_ipmi_oem_inventec_set_extended_config_value (state_data,
                                                            IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_POWER_MANAGEMENT,
                                                            IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_POWER_MANAGEMENT_POWER_ON_DELAY,
                                                            0,
                                                            2,
                                                            (uint32_t)powerondelay) < 0)
            goto cleanup;
        }
      else if (!strcasecmp (key, "maxpowerondelay"))
        {
          if (ipmi_oem_parse_2_byte_field (state_data, i, value, &maxpowerondelay) < 0)
            goto cleanup;
          
          if (_ipmi_oem_inventec_set_extended_config_value (state_data,
                                                            IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_POWER_MANAGEMENT,
                                                            IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_POWER_MANAGEMENT_MAXIMUM_POWER_ON_DELAY,
                                                            0,
                                                            2,
                                                            (uint32_t)maxpowerondelay) < 0)
            goto cleanup;
        }
      else
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "%s:%s invalid OEM option argument '%s' : invalid key\n",
                           state_data->prog_data->args->oem_id,
                           state_data->prog_data->args->oem_command,
                           state_data->prog_data->args->oem_options[i]);
          goto cleanup;
        }

      free (key);
      free (value);
    }

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_inventec_get_sol_idle_timeout (ipmi_oem_state_data_t *state_data)
{
  uint32_t tmpvalue;
  uint16_t timeout;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  if (_ipmi_oem_inventec_get_extended_config_value (state_data,
                                                    IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_SOL,
                                                    IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_SOL_SOL_IDLE_TIMEOUT,
                                                    0,
                                                    2,
                                                    &tmpvalue) < 0)
    goto cleanup;
  
  timeout = tmpvalue;

  pstdout_printf (state_data->pstate,
                  "%u minutes\n",
                  timeout);

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_inventec_set_sol_idle_timeout (ipmi_oem_state_data_t *state_data)
{
  uint16_t timeout;
  int rv = -1;
  
  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 1);
  
  if (strcasecmp (state_data->prog_data->args->oem_options[0], "none"))
    {
      unsigned int temp;
      char *endptr = NULL;
      
      errno = 0;
      
      temp = strtoul (state_data->prog_data->args->oem_options[0], &endptr, 10);
      if (errno
          || endptr[0] != '\0'
          || temp > USHRT_MAX)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "%s:%s invalid OEM option argument '%s'\n",
                           state_data->prog_data->args->oem_id,
                           state_data->prog_data->args->oem_command,
                           state_data->prog_data->args->oem_options[0]);
          goto cleanup;
        }

      timeout = temp;
    }
  else
    timeout = 0;

  if (_ipmi_oem_inventec_set_extended_config_value (state_data,
                                                    IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_SOL,
                                                    IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_SOL_SOL_IDLE_TIMEOUT,
                                                    0,
                                                    2,
                                                    (uint32_t)timeout) < 0)
    goto cleanup;

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_inventec_get_telnet_ssh_redirect_status (ipmi_oem_state_data_t *state_data)
{
  uint32_t tmpvalue;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  if (_ipmi_oem_inventec_get_extended_config_value (state_data,
                                                    IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_SOL,
                                                    IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_SOL_TELNET_SSH_REDIRECT_ENABLE,
                                                    0,
                                                    1,
                                                    &tmpvalue) < 0)
    goto cleanup;
  
  pstdout_printf (state_data->pstate,
                  "%s\n",
                  (tmpvalue) ? "enabled" : "disabled");
  
  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_inventec_set_telnet_ssh_redirect_status (ipmi_oem_state_data_t *state_data)
{
  uint8_t enable = 0;
  int rv = -1;
  
  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 1);
  
  if (strcasecmp (state_data->prog_data->args->oem_options[0], "enable")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "disable"))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[0]);
      goto cleanup;
    }
  
  if (!strcasecmp (state_data->prog_data->args->oem_options[0], "enable"))
    enable = 1;
  else
    enable = 0;
  
  if (_ipmi_oem_inventec_set_extended_config_value (state_data,
                                                    IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_SOL,
                                                    IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_SOL_TELNET_SSH_REDIRECT_ENABLE,
                                                    0,
                                                    1,
                                                    (uint32_t)enable) < 0)
    goto cleanup;

  rv = 0;
 cleanup:
  return (rv);
}

#if 0
/* waiting for verification from Dell */
int
ipmi_oem_inventec_get_firmware_update_config (ipmi_oem_state_data_t *state_data)
{
  uint32_t tmpvalue;
  uint8_t remote_update_enable;
  uint8_t protocol;
  char uri[IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_FIRMWARE_UPDATE_URI_LEN + 1];
  uint8_t connection_retry;
  uint8_t retry_interval;
  uint8_t delay_time;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  memset (uri, '\0', IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_FIRMWARE_UPDATE_URI + 1);

  if (_ipmi_oem_inventec_get_extended_config_value (state_data,
                                                    IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_FIRMWARE_UPDATE,
                                                    IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_FIRMWARE_UPDATE_REMOTE_UPDATE_ENABLE,
                                                    0,
                                                    1,
                                                    &tmpvalue) < 0)
    goto cleanup;
  remote_update_enable = tmpvalue;
  
  if (_ipmi_oem_inventec_get_extended_config_value (state_data,
                                                    IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_FIRMWARE_UPDATE,
                                                    IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_FIRMWARE_UPDATE_PROTOCOL,
                                                    0,
                                                    1,
                                                    &tmpvalue) < 0)
    goto cleanup;
  protocol = tmpvalue;

  if (_ipmi_oem_inventec_get_extended_config_string (state_data,
                                                     IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_FIRMWARE_UPDATE,
                                                     IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_FIRMWARE_UPDATE_URI,
                                                     0,
                                                     uri,
                                                     IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_FIRMWARE_UPDATE_URI_LEN) < 0)
    goto cleanup;
  
  if (_ipmi_oem_inventec_get_extended_config_value (state_data,
                                                    IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_FIRMWARE_UPDATE,
                                                    IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_FIRMWARE_UPDATE_CONNECTION_RETRY,
                                                    0,
                                                    1,
                                                    &tmpvalue) < 0)
    goto cleanup;
  connection_retry = tmpvalue;
  
  if (_ipmi_oem_inventec_get_extended_config_value (state_data,
                                                    IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_FIRMWARE_UPDATE,
                                                    IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_FIRMWARE_UPDATE_RETRY_INTERVAL,
                                                    0,
                                                    1,
                                                    &tmpvalue) < 0)
    goto cleanup;
  retry_interval = tmpvalue;
  
  if (_ipmi_oem_inventec_get_extended_config_value (state_data,
                                                    IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_FIRMWARE_UPDATE,
                                                    IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_FIRMWARE_UPDATE_DELAY_TIME,
                                                    0,
                                                    1,
                                                    &tmpvalue) < 0)
    goto cleanup;
  delay_time = tmpvalue;
  
  pstdout_printf (state_data->pstate,
                  "Remote Update    : %s\n",
                  remote_update_enable ? "Enabled" : "Disabled");
 
  pstdout_printf (state_data->pstate,
                  "Protocol TFTP    : %s\n",
                  (protocol & IPMI_OEM_INVENTEC_EXTENDED_CONFIG_FIRMWARE_UPDATE_PROTOCOL_BITMASK_TFTP) ? "Supported" : "Not Supported");
 
  pstdout_printf (state_data->pstate,
                  "Protocol FTP     : %s\n",
                  (protocol & IPMI_OEM_INVENTEC_EXTENDED_CONFIG_FIRMWARE_UPDATE_PROTOCOL_BITMASK_FTP) ? "Supported" : "Not Supported");
 
  pstdout_printf (state_data->pstate,
                  "Protocol HTTP    : %s\n",
                  (protocol & IPMI_OEM_INVENTEC_EXTENDED_CONFIG_FIRMWARE_UPDATE_PROTOCOL_BITMASK_HTTP) ? "Supported" : "Not Supported");
 
  /* First char will be a separator, don't bother outputting if necessary */
  if (uri[0] == 0x1C
      || uri[0] == 0x1D
      || uri[0] == 0x1E
      || uri[0] == 0x1F)
    pstdout_printf (state_data->pstate,
                    "URI              : %s\n",
                    &uri[1]);
  else
    pstdout_printf (state_data->pstate,
                    "URI              : %s\n",
                    uri);
 
  pstdout_printf (state_data->pstate,
                  "Connection Retry : %u\n",
                  connection_retry);
 
  /* is in 5 second increments */
  pstdout_printf (state_data->pstate,
                  "Retry Interval   : %u seconds\n",
                  retry_interval * IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_FIRMWARE_UPDATE_RETRY_INTERVAL_INCREMENTS);
 
  if (delay_time == IPMI_OEM_INVENTEC_EXTENDED_CONFIG_FIRMWARE_UPDATE_DELAY_TIME_RANDOM)
    pstdout_printf (state_data->pstate,
                    "Delay Time       : random\n",
                    delay_time);
  else
    pstdout_printf (state_data->pstate,
                    "Delay Time       : %u seconds\n",
                    delay_time);
 
  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_inventec_set_firmware_update_config (ipmi_oem_state_data_t *state_data)
{
  uint8_t remote_update_enable = 0;
  char uri[IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_FIRMWARE_UPDATE_URI_LEN + 1];
  uint8_t connection_retry = 0;
  uint8_t retry_interval = 0;
  uint8_t delay_time = 0;
  uint32_t tmpvalue = 0;
  int rv = -1;
  unsigned int i;

  assert (state_data);

  if (!state_data->prog_data->args->oem_options_count)
    {
      pstdout_printf (state_data->pstate,
		      "Option: remoteupdate=enable|disable\n"
		      "Option: URI=tftp://...|ftp://...|http://...\n"
		      "Option: connectionretry=num\n"
		      "Option: retryinterval=seconds\n"
		      "Option: delaytime=seconds|random\n");
      return (0); 
    }

  for (i = 0; i < state_data->prog_data->args->oem_options_count; i++)
    {
      char *key = NULL;
      char *value = NULL;
      
      if (ipmi_oem_parse_key_value (state_data,
                                    i,
                                    &key,
                                    &value) < 0)
        goto cleanup;

      if (!strcasecmp (key, "remoteupdate"))
        {
          if (ipmi_oem_parse_enable (state_data, i, value, &remote_update_enable) < 0)
            goto cleanup;
          
          if (_ipmi_oem_inventec_set_extended_config_value (state_data,
                                                            IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_FIRMWARE_UPDATE,
                                                            IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_FIRMWARE_UPDATE_REMOTE_UPDATE_ENABLE,
                                                            0,
                                                            1,
                                                            (uint32_t)remote_update_enable) < 0)
            goto cleanup;
        }
      else if (!strcasecmp (key, "uri"))
        {
          unsigned int min_required;
          unsigned int value_len;

          if (strncasecmp (value, "tftp://", 7)
              && strncasecmp (value, "ftp://", 6)
              && strncasecmp (value, "http://", 7))
            {
              pstdout_fprintf (state_data->pstate,
                               stderr,
                               "%s:%s invalid OEM option argument '%s' : invalid value\n",
                               state_data->prog_data->args->oem_id,
                               state_data->prog_data->args->oem_command,
                               state_data->prog_data->args->oem_options[i]);
              goto cleanup;
            }
          
          /* has to be atleast 1 char after what the prefix */
          if (!strncasecmp (value, "tftp://", 7)
              || !strncasecmp (value, "http://", 7))
            min_required = 8;
          else /* !strncasecmp (value, "ftp://", 6) */
            min_required = 7;
          
          /* - 1 for separator unit */
          if (strlen (value) < min_required
              || strlen (value) > (IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_FIRMWARE_UPDATE_URI_LEN - 1))
            {
              pstdout_fprintf (state_data->pstate,
                               stderr,
                               "%s:%s invalid OEM option argument '%s' : invalid URI length\n",
                               state_data->prog_data->args->oem_id,
                               state_data->prog_data->args->oem_command,
                               state_data->prog_data->args->oem_options[i]);
              goto cleanup;
            }

          value_len = strlen (value);
          
          /* First char should be a separator */
          uri[0] = 0x1F;
          memcpy (&uri[1], value, value_len);

          if (_ipmi_oem_inventec_set_extended_config_string (state_data,
                                                             IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_FIRMWARE_UPDATE,
                                                             IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_FIRMWARE_UPDATE_URI,
                                                             uri,
                                                             value_len + 1) < 0)
            goto cleanup;
        }
      else if (!strcasecmp (key, "connectionretry"))
        {
          if (ipmi_oem_parse_1_byte_field (state_data, i, value, &connection_retry) < 0)
            goto cleanup;

          if (_ipmi_oem_inventec_set_extended_config_value (state_data,
                                                            IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_FIRMWARE_UPDATE,
                                                            IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_FIRMWARE_UPDATE_CONNECTION_RETRY,
                                                            0,
                                                            1,
                                                            (uint32_t)connection_retry) < 0)
            goto cleanup;
        }
      else if (!strcasecmp (key, "retryinterval"))
        {
          if (ipmi_oem_parse_unsigned_int_range (state_data,
                                                 i,
                                                 value,
                                                 &tmpvalue,
                                                 0,
                                                 UCHAR_MAX * IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_FIRMWARE_UPDATE_RETRY_INTERVAL_INCREMENTS) < 0)
            goto cleanup;

          /* retry interval is in 5 second increments */
          retry_interval = tmpvalue / IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_FIRMWARE_UPDATE_RETRY_INTERVAL_INCREMENTS;

          if (_ipmi_oem_inventec_set_extended_config_value (state_data,
                                                            IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_FIRMWARE_UPDATE,
                                                            IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_FIRMWARE_UPDATE_RETRY_INTERVAL,
                                                            0,
                                                            1,
                                                            (uint32_t)retry_interval) < 0)
            goto cleanup;
        }
      else if (!strcasecmp (key, "delaytime"))
        {
          if (!strcasecmp (value, "random"))
            delay_time = IPMI_OEM_INVENTEC_EXTENDED_CONFIG_FIRMWARE_UPDATE_DELAY_TIME_RANDOM;
          else
            {
              if (ipmi_oem_parse_unsigned_int_range (state_data,
                                                     i,
                                                     value,
                                                     &tmpvalue,
                                                     0,
                                                     UCHAR_MAX - 1) < 0)
                goto cleanup;
              delay_time = tmpvalue;
            }

          if (_ipmi_oem_inventec_set_extended_config_value (state_data,
                                                            IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_FIRMWARE_UPDATE,
                                                            IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_FIRMWARE_UPDATE_DELAY_TIME,
                                                            0,
                                                            1,
                                                            (uint32_t)delay_time) < 0)
            goto cleanup;
        }
      else
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "%s:%s invalid OEM option argument '%s' : invalid key\n",
                           state_data->prog_data->args->oem_id,
                           state_data->prog_data->args->oem_command,
                           state_data->prog_data->args->oem_options[i]);
          goto cleanup;
        }

      free (key);
      free (value);
    }

  rv = 0;
 cleanup:
  return (rv);
}
#endif

#if 0
/* cannot verify */
int
ipmi_oem_inventec_get_firmware_information (ipmi_oem_state_data_t *state_data)
{
  uint32_t tmpvalue;
  char name[IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_NAME_LEN + 1];
  char description[IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_DESCRIPTION_LEN + 1];
  uint8_t entity;
  char *entity_str = NULL;
  char product_info[IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_PRODUCT_INFO_LEN + 1];
  char firmware_version[IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_FIRMWARE_VERSION_LEN + 1];
  char branch[IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_BRANCH_LEN + 1];
  char build_information[IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_BUILD_INFORMATION_LEN + 1];
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  memset (name, '\0', IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_NAME_LEN + 1);
  memset (description, '\0', IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_DESCRIPTION_LEN + 1);
  memset (product_info, '\0', IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_PRODUCT_INFO_LEN + 1);
  memset (firmware_version, '\0', IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_FIRMWARE_VERSION_LEN + 1);
  memset (branch, '\0', IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_BRANCH_LEN + 1);
  memset (build_information, '\0', IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_BUILD_INFORMATION_LEN + 1);

  if (_ipmi_oem_inventec_get_extended_config_string (state_data,
                                                     IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_FIRMWARE_INFORMATION,
                                                     IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_NAME,
                                                     0,
                                                     name,
                                                     IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_NAME_LEN) < 0)
    goto cleanup;

  if (_ipmi_oem_inventec_get_extended_config_string (state_data,
                                                     IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_FIRMWARE_INFORMATION,
                                                     IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_DESCRIPTION,
                                                     0,
                                                     description,
                                                     IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_DESCRIPTION_LEN) < 0)
    goto cleanup;

  if (_ipmi_oem_inventec_get_extended_config_value (state_data,
                                                    IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_FIRMWARE_INFORMATION,
                                                    IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_ENTITY,
                                                    0,
                                                    1,
                                                    &tmpvalue) < 0)
    goto cleanup;
  entity = tmpvalue;

  if (_ipmi_oem_inventec_get_extended_config_string (state_data,
                                                     IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_FIRMWARE_INFORMATION,
                                                     IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_PRODUCT_INFO,
                                                     0,
                                                     product_info,
                                                     IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_PRODUCT_INFO_LEN) < 0)
    goto cleanup;
 
  if (_ipmi_oem_inventec_get_extended_config_string (state_data,
                                                     IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_FIRMWARE_INFORMATION,
                                                     IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_FIRMWARE_VERSION,
                                                     0,
                                                     firmware_version,
                                                     IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_FIRMWARE_VERSION_LEN) < 0)
    goto cleanup;

  if (_ipmi_oem_inventec_get_extended_config_string (state_data,
                                                     IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_FIRMWARE_INFORMATION,
                                                     IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_BRANCH,
                                                     0,
                                                     branch,
                                                     IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_BRANCH_LEN) < 0)
    goto cleanup;

  if (_ipmi_oem_inventec_get_extended_config_string (state_data,
                                                     IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_FIRMWARE_INFORMATION,
                                                     IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_BUILD_INFORMATION,
                                                     0,
                                                     build_information,
                                                     IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_BUILD_INFORMATION_LEN) < 0)
    goto cleanup;

  pstdout_printf (state_data->pstate,
                  "BMC Name               : %s\n",
                  name);

  pstdout_printf (state_data->pstate,
                  "Controller Description : %s\n",
                  description);

  if (entity == IPMI_OEM_INVENTEC_EXTENDED_CONFIG_FIRMWARE_INFORMATION_ENTITY_BMC)
    entity_str = "BMC";
  else if (entity == IPMI_OEM_INVENTEC_EXTENDED_CONFIG_FIRMWARE_INFORMATION_ENTITY_SYSTEM_BIOS)
    entity_str = "System (BIOS)";
  else if (entity == IPMI_OEM_INVENTEC_EXTENDED_CONFIG_FIRMWARE_INFORMATION_ENTITY_PDB)
    entity_str = "PDB";
  else if (entity == IPMI_OEM_INVENTEC_EXTENDED_CONFIG_FIRMWARE_INFORMATION_ENTITY_FCB)
    entity_str = "FCB";
  else
    entity_str = "Unrecognized";

  pstdout_printf (state_data->pstate,
                  "Controller Entity      : %s\n",
                  entity_str);

  pstdout_printf (state_data->pstate,
                  "Product Info           : %s\n",
                  product_info);

  pstdout_printf (state_data->pstate,
                  "Firmware Version       : %s\n",
                  firmware_version);

  pstdout_printf (state_data->pstate,
                  "Branch                 : %s\n",
                  branch);

  pstdout_printf (state_data->pstate,
                  "Build Information      : %s\n",
                  build_information);

  rv = 0;
 cleanup:
  return (rv);
}
#endif

#if 0
/* waiting for verification from Dell */
int
ipmi_oem_inventec_update_firmware (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  uint8_t task_id;
  int config_parsed = 0;
  uint8_t config_value = 0;
  int transmitting_image_output = 0;
  int validating_image_output = 0;
  int programming_output = 0;
  int ready_to_accept_image = 0;
  unsigned int rq_len = 0;
  int rs_len;
  int rv = -1;
  int i;

  /* Inventec 5441/5442 OEM
   *
   * Update Firmware Request
   *
   * 0x08 - network function
   * 0x01 - OEM cmd
   * 0x?? - interface used
   *      - 00h - system interface (e.g. KCS)
   *      - 01h - networking (e.g. tftp, ftp, http)
   *      - 02h - USB MSC
   * 0x?? - update type
   *      [7]   - force update
   *            - 0h - normal update, an update operation will occur only when the
   *              BMC validate (sic) target board, target product and
   *              version number.
   *            - 1h - forced update, make the BMC update the image without
   *              validate (sic) target board, target product and
   *              version number.
   *      [6:0] - reserved
   * bytes 3 - 14: install options (OEM specific)
   *
   * Update Firmware Response
   *
   * 0x01 - OEM cmd
   * 0x?? - Completion Code
   * 0x?? - Task ID
   */

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count >= 1);

  if (strcasecmp (state_data->prog_data->args->oem_options[0], "tftp")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "ftp")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "http"))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[0]);
      goto cleanup;
    }
  
  /* start at index 1 - first option is required */
  for (i = 1; i < state_data->prog_data->args->oem_options_count; i++)
    {
      char *key = NULL;
      char *value = NULL;
      
      if (ipmi_oem_parse_key_value (state_data,
                                    i,
                                    &key,
                                    &value) < 0)
        goto cleanup;

      if (!strcasecmp (key, "config"))
        {
          if (strcasecmp (value, "preserve")
              && strcasecmp (value, "nopreserve"))
            {
              pstdout_fprintf (state_data->pstate,
                               stderr,
                               "%s:%s invalid OEM option argument '%s' : invalid value\n",
                               state_data->prog_data->args->oem_id,
                               state_data->prog_data->args->oem_command,
                               state_data->prog_data->args->oem_options[i]);
              goto cleanup;
            }
          
          config_parsed++;

          /* has to be atleast 1 char after what the prefix */
          if (!strcasecmp (value, "preserve"))
            config_value = IPMI_OEM_INVENTEC_UPDATE_FIRMWARE_OEM_DELL_CONFIG_PRESERVE;
          else /* !strcasecmp (value, "nopreserve") */
            config_value = IPMI_OEM_INVENTEC_UPDATE_FIRMWARE_OEM_DELL_CONFIG_NOPRESERVE;
        }
      else
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "%s:%s invalid OEM option argument '%s' : invalid option\n",
                           state_data->prog_data->args->oem_id,
                           state_data->prog_data->args->oem_command,
                           state_data->prog_data->args->oem_options[i]);
          goto cleanup;
        }

      free (key);
      free (value);
    }

  /* Check for "combos" between different optional arguments */

  /* achu - none yet */

  bytes_rq[0] = IPMI_CMD_OEM_INVENTEC_UPDATE_FIRMARE;

  /* what do you know, it all maps to one for now .. lucky us */
  if (!strcasecmp (state_data->prog_data->args->oem_options[0], "tftp")
      || !strcasecmp (state_data->prog_data->args->oem_options[0], "ftp")
      || !strcasecmp (state_data->prog_data->args->oem_options[0], "http"))
    bytes_rq[1] = IPMI_OEM_INVENTEC_UPDATE_FIRMWARE_INTERFACE_NETWORKING;
  
  bytes_rq[2] = IPMI_OEM_INVENTEC_UPDATE_FIRMWARE_UPDATE_TYPE_NORMAL_UPDATE;
  bytes_rq[2] <<= IPMI_OEM_INVENTEC_UPDATE_FIRMWARE_UPDATE_TYPE_SHIFT;

  rq_len = 3;

  if (config_parsed)
    {
      bytes_rq[3] = config_value;
      rq_len++;
    }

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_FIRMWARE_RQ, /* network function */
                              bytes_rq, /* data */
                              rq_len, /* num bytes */
                              bytes_rs,
                              IPMI_OEM_MAX_BYTES)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_raw: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  if (ipmi_oem_check_response_and_completion_code (state_data,
                                                   bytes_rs,
                                                   rs_len,
                                                   2,
                                                   IPMI_CMD_OEM_INVENTEC_UPDATE_FIRMARE,
                                                   IPMI_NET_FN_FIRMWARE_RS,
                                                   NULL) < 0)
    goto cleanup;

  task_id = bytes_rs[2];
  
  /* loop until it's done */
  while (1)
    {
      uint8_t update_status;

      /* Inventec 5441/5442 OEM
       *
       * Get Update Status Request
       *
       * 0x08 - network function
       * 0x02 - OEM cmd
       * 0x?? - Task ID
       *
       * Get Update Status Response
       *
       * 0x02 - OEM cmd
       * 0x?? - Completion Code
       * 0x?? - Status
       *      - 00h = transmitting image
       *      - 01h = validating image
       *      - 02h = programming
       *      - 03h = ready to accept image
       *      - 80h = general error
       *      - 81h = cannot establish connection
       *      - 82h = path not found
       *      - 83h = transmission abort
       *      - 84h = checkusm error
       *      - 85h = incorrect platform
       *      - FFh = completed
       * 0x?? - Progression Indicator
       *      - "This field is optional, if present its value indicates the
       *        current progress of the status specified in Status byte"
       */

      bytes_rq[0] = IPMI_CMD_OEM_INVENTEC_GET_UPDATE_STATUS;
      bytes_rq[1] = task_id;

      if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                                  0, /* lun */
                                  IPMI_NET_FN_FIRMWARE_RQ, /* network function */
                                  bytes_rq, /* data */
                                  2, /* num bytes */
                                  bytes_rs,
                                  IPMI_OEM_MAX_BYTES)) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "ipmi_cmd_raw: %s\n",
                           ipmi_ctx_errormsg (state_data->ipmi_ctx));
          goto cleanup;
        }
      
      if (ipmi_oem_check_response_and_completion_code (state_data,
                                                       bytes_rs,
                                                       rs_len,
                                                       2,
                                                       IPMI_CMD_OEM_INVENTEC_GET_UPDATE_STATUS,
                                                       IPMI_NET_FN_FIRMWARE_RS,
                                                       NULL) < 0)
        goto cleanup;

      update_status = bytes_rs[2];

      if (update_status == IPMI_OEM_INVENTEC_GET_UPDATE_STATUS_GENERAL_ERROR)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "General Error\n");
          goto cleanup;
        }
      else if (update_status == IPMI_OEM_INVENTEC_GET_UPDATE_STATUS_CANNOT_ESTABLISH_CONNECTION)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "Cannot Establish Connection\n");
          goto cleanup;
        }
      else if (update_status == IPMI_OEM_INVENTEC_GET_UPDATE_STATUS_PATH_NOT_FOUND)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "Path Not Found\n");
          goto cleanup;
        }
      else if (update_status == IPMI_OEM_INVENTEC_GET_UPDATE_STATUS_TRANSMISSION_ABORT)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "Transmission Abort\n");
          goto cleanup;
        }
      else if (update_status == IPMI_OEM_INVENTEC_GET_UPDATE_STATUS_CHECKSUM_ERROR)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "Checksum Error\n");
          goto cleanup;
        }
      else if (update_status == IPMI_OEM_INVENTEC_GET_UPDATE_STATUS_INCORRECT_PLATFORM)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "Incorrect Platform\n");
          goto cleanup;
        }
      else if (update_status == IPMI_OEM_INVENTEC_GET_UPDATE_STATUS_COMPLETED)
        break;

      if (state_data->prog_data->args->verbose_count)
        {
          if (update_status == IPMI_OEM_INVENTEC_GET_UPDATE_STATUS_TRANSMITTING_IMAGE
              && !transmitting_image_output)
            {
              pstdout_printf (state_data->pstate,
                              "Transmitting Image\n");
              transmitting_image_output++;
            }
          else if (update_status == IPMI_OEM_INVENTEC_GET_UPDATE_STATUS_VALIDATING_IMAGE
                   && !validating_image_output)
            {
              pstdout_printf (state_data->pstate,
                              "Validating Image\n");
              validating_image_output++;
            }
          else if (update_status == IPMI_OEM_INVENTEC_GET_UPDATE_STATUS_PROGRAMMING
                   && !programming_output)
            {
              pstdout_printf (state_data->pstate,
                              "Programming\n");
              programming_output++;
            }
          else if (update_status == IPMI_OEM_INVENTEC_GET_UPDATE_STATUS_READY_TO_ACCEPT_IMAGE
                   && !ready_to_accept_image)
            {
              pstdout_printf (state_data->pstate,
                              "Ready to Accept Image\n");
              ready_to_accept_image++;
            }
        }
      
      sleep (1);
    } 

  rv = 0;
 cleanup:
  return (rv);
}
#endif

int
ipmi_oem_inventec_get_board_id (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  int rs_len;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  /* Inventec 5441/Dell Xanadu II OEM
   * Inventec 5442/Dell Xanadu III OEM
   *
   * Get Board ID Request
   *
   * 0x34 - OEM network function
   * 0x10 - OEM cmd
   *
   * Get Board ID Response
   *
   * 0x10 - OEM cmd
   * 0x?? - Completion Code
   * 0x?? - board id
   */

  bytes_rq[0] = IPMI_CMD_OEM_INVENTEC_GET_BOARD_ID;

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_INVENTEC_SPECIFIC_RQ, /* network function */
                              bytes_rq, /* data */
                              1, /* num bytes */
                              bytes_rs,
                              IPMI_OEM_MAX_BYTES)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_raw: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  if (ipmi_oem_check_response_and_completion_code (state_data,
                                                   bytes_rs,
                                                   rs_len,
                                                   3,
                                                   IPMI_CMD_OEM_INVENTEC_GET_BOARD_ID,
                                                   IPMI_NET_FN_OEM_INVENTEC_SPECIFIC_RS,
                                                   NULL) < 0)
    goto cleanup;

  pstdout_printf (state_data->pstate,
                  "%Xh\n",
                  bytes_rs[2]);
  
  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_inventec_set_board_id (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  uint8_t boardid;
  unsigned int tmp;
  char *endptr;
  int rs_len;
  int rv = -1;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 1);

  errno = 0;
  
  tmp = strtoul (state_data->prog_data->args->oem_options[0],
                 &endptr,
                 IPMI_OEM_HEX_BASE);
  if (errno
      || endptr[0] != '\0'
      || tmp > UCHAR_MAX)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[0]);
      goto cleanup;
    }
  boardid = tmp;

  /* Inventec 5441/Dell Xanadu II OEM
   * Inventec 5442/Dell Xanadu III OEM
   *
   * Set Board ID Request
   *
   * 0x34 - OEM network function
   * 0x11 - OEM cmd
   * 0x?? - board id
   *
   * Set Board ID Response
   *
   * 0x11 - OEM cmd
   * 0x?? - Completion Code
   */

  bytes_rq[0] = IPMI_CMD_OEM_INVENTEC_SET_BOARD_ID;
  bytes_rq[1] = boardid;

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_INVENTEC_SPECIFIC_RQ, /* network function */
                              bytes_rq, /* data */
                              2, /* num bytes */
                              bytes_rs,
                              IPMI_OEM_MAX_BYTES)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_raw: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  if (ipmi_oem_check_response_and_completion_code (state_data,
                                                   bytes_rs,
                                                   rs_len,
                                                   2,
                                                   IPMI_CMD_OEM_INVENTEC_SET_BOARD_ID,
                                                   IPMI_NET_FN_OEM_INVENTEC_SPECIFIC_RS,
                                                   NULL) < 0)
    goto cleanup;

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_inventec_get_fcb_version (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  int rs_len;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  /* Inventec 5441/Dell Xanadu II OEM
   * Inventec 5442/Dell Xanadu III OEM
   *
   * Get FCB FW Version Request
   *
   * 0x34 - OEM network function
   * 0x16 - OEM cmd
   *
   * Get FCB FW Version Response
   *
   * 0x16 - OEM cmd
   * 0x?? - Completion Code
   * 0x?? - major version (in hex)
   * 0x?? - minor version (in hex)
   */

  bytes_rq[0] = IPMI_CMD_OEM_INVENTEC_GET_FCB_FW_VERSION;

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_INVENTEC_SPECIFIC_RQ, /* network function */
                              bytes_rq, /* data */
                              1, /* num bytes */
                              bytes_rs,
                              IPMI_OEM_MAX_BYTES)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_raw: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  if (ipmi_oem_check_response_and_completion_code (state_data,
                                                   bytes_rs,
                                                   rs_len,
                                                   4,
                                                   IPMI_CMD_OEM_INVENTEC_GET_FCB_FW_VERSION,
                                                   IPMI_NET_FN_OEM_INVENTEC_SPECIFIC_RS,
                                                   NULL) < 0)
    goto cleanup;

  pstdout_printf (state_data->pstate,
                  "%X.%02X\n",
                  bytes_rs[2],
                  bytes_rs[3]);

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_inventec_set_fcb_version (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  uint8_t majorversion;
  uint8_t minorversion;
  unsigned int tmp;
  char *endptr;
  int rs_len;
  int rv = -1;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 2);

  errno = 0;

  tmp = strtoul (state_data->prog_data->args->oem_options[0],
                 &endptr,
                 IPMI_OEM_HEX_BASE);
  if (errno
      || endptr[0] != '\0'
      || tmp > UCHAR_MAX)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[0]);
      goto cleanup;
    }
  majorversion = tmp;

  errno = 0;

  tmp = strtoul (state_data->prog_data->args->oem_options[1],
                 &endptr,
                 IPMI_OEM_HEX_BASE);
  if (errno
      || endptr[0] != '\0'
      || tmp > UCHAR_MAX)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[1]);
      goto cleanup;
    }
  minorversion = tmp;

  /* Inventec 5441/Dell Xanadu II OEM
   * Inventec 5442/Dell Xanadu III OEM
   *
   * Set FCB FW Version Request
   *
   * 0x34 - OEM network function
   * 0x15 - OEM cmd
   * 0x?? - major version (in hex)
   * 0x?? - minor version (in hex)
   *
   * Set FCB FW Version Response
   *
   * 0x15 - OEM cmd
   * 0x?? - Completion Code
   */

  bytes_rq[0] = IPMI_CMD_OEM_INVENTEC_SET_FCB_FW_VERSION;
  bytes_rq[1] = majorversion;
  bytes_rq[2] = minorversion;

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_INVENTEC_SPECIFIC_RQ, /* network function */
                              bytes_rq, /* data */
                              3, /* num bytes */
                              bytes_rs,
                              IPMI_OEM_MAX_BYTES)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_raw: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  if (ipmi_oem_check_response_and_completion_code (state_data,
                                                   bytes_rs,
                                                   rs_len,
                                                   2,
                                                   IPMI_CMD_OEM_INVENTEC_SET_FCB_FW_VERSION,
                                                   IPMI_NET_FN_OEM_INVENTEC_SPECIFIC_RS,
                                                   NULL) < 0)
    goto cleanup;

  rv = 0;
 cleanup:
  return (rv);
}

#if 0
/* cannot verify */
int
ipmi_oem_inventec_set_asset_tag (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  unsigned int asset_tag_len;
  unsigned int rq_len = 0;
  int rs_len;
  int rv = -1;
  int i;
  
  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 1);

  asset_tag_len = strlen (state_data->prog_data->args->oem_options[0]);
  if (asset_tag_len > IPMI_OEM_INVENTEC_ASSET_TAG_MAX)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s OEM option argument '%s' invalid length, max %u long\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[0],
                       IPMI_OEM_INVENTEC_ASSET_TAG_MAX);
      goto cleanup;
    }
  
  /* Inventec 5441/Dell Xanadu II
   * Inventec 5442/Dell Xanadu III
   *
   * Set Asset Tag Request
   *
   * 0x34 - OEM network function
   * 0x12 - OEM cmd
   * bytes 1-10: Asset Tag
   *
   * Set Asset Tag Response
   *
   * 0x12 - OEM cmd
   * 0x?? - Completion Code
   * 0x?? - count written
   */

  bytes_rq[0] = IPMI_CMD_OEM_INVENTEC_SET_ASSET_TAG;
  rq_len++;
  for (i = 0; i < asset_tag_len; i++)
    {
      bytes_rq[1 + i] = state_data->prog_data->args->oem_options[0][i];
      rq_len++;
    }

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_INVENTEC_SPECIFIC_RQ, /* network function */
                              bytes_rq, /* data */
                              rq_len, /* num bytes */
                              bytes_rs,
                              IPMI_OEM_MAX_BYTES)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_raw: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }
  
  if (ipmi_oem_check_response_and_completion_code (state_data,
                                                   bytes_rs,
                                                   rs_len,
                                                   2,
                                                   IPMI_CMD_OEM_INVENTEC_SET_ASSET_TAG,
                                                   IPMI_NET_FN_OEM_INVENTEC_SPECIFIC_RS,
                                                   NULL) < 0)
    goto cleanup;
  
  rv = 0;
 cleanup:
  return (rv);
}
#endif

#if 0
/* cannot verify */

int
ipmi_oem_inventec_get_dhcp_retry (ipmi_oem_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t configuration_parameter_data[IPMI_OEM_MAX_BYTES];
  uint8_t lan_channel_number;
  int len;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  /* Dell Xanadu II OEM
   *
   * Uses Get/Set Lan Configuration
   *
   * parameter = 192
   *
   * Data format
   *
   * 1st byte = retry count, 1 based, 0h = no retries, ffh = infinite
   * 2nd byte = retry interval, 1 based, 10 second increments
   * 3rd byte = retry timeout, 1 based, 1 minute increments
   *
   */

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_lan_configuration_parameters_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_get_channel_number (state_data->ipmi_ctx,
                               IPMI_CHANNEL_MEDIUM_TYPE_LAN_802_3,
                               &lan_channel_number) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_get_channel_number: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  if (ipmi_cmd_get_lan_configuration_parameters (state_data->ipmi_ctx,
                                                 lan_channel_number,
                                                 IPMI_GET_LAN_PARAMETER,
                                                 IPMI_LAN_PARAMETER_OEM_INVENTEC_DHCP_RETRY,
                                                 0,
                                                 0,
                                                 obj_cmd_rs) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_get_lan_configuration_parameters: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  if ((len = fiid_obj_get_data (obj_cmd_rs,
                                "configuration_parameter_data",
                                configuration_parameter_data,
                                IPMI_OEM_MAX_BYTES)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get_data: 'configuration_parameter_data': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  if (len < 3)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_get_lan_configuration_parameters: invalid buffer length returned: %d\n",
                       len);
      goto cleanup;
    }

  if (!configuration_parameter_data[0])
    pstdout_printf (state_data->pstate, "Retry Count    : no retries\n");
  else if (configuration_parameter_data[0] == 0xFF)
    pstdout_printf (state_data->pstate, "Retry Count    : infinite retries\n");
  else
    pstdout_printf (state_data->pstate, "Retry Count    : %u\n", configuration_parameter_data[0]);
  pstdout_printf (state_data->pstate, "Retry Interval : %u seconds\n", configuration_parameter_data[1] * 10);
  pstdout_printf (state_data->pstate, "Retry Timeout  : %u minutes\n", configuration_parameter_data[2]);
                  
  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

int
ipmi_oem_inventec_set_dhcp_retry (ipmi_oem_state_data_t *state_data)
{
  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 3);

  return (0);
}
#endif

int
ipmi_oem_inventec_get_sol_inactivity_timeout (ipmi_oem_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t configuration_parameter_data[IPMI_OEM_MAX_BYTES];
  uint8_t lan_channel_number;
  uint16_t sol_inactivity_timeout;
  int len;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  /* Dell Xanadu II OEM
   * Dell Xanadu III OEM
   *
   * Uses Get/Set SOL Configuration
   *
   * parameter = 192
   *
   * Data format
   *
   * 1st & 2nd byte = inactivity timeout, 1 based, 1 minute
   * increments, LSbyte first
   *
   */

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_sol_configuration_parameters_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_get_channel_number (state_data->ipmi_ctx,
                               IPMI_CHANNEL_MEDIUM_TYPE_LAN_802_3,
                               &lan_channel_number) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_get_channel_number: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  if (ipmi_cmd_get_sol_configuration_parameters (state_data->ipmi_ctx,
                                                 lan_channel_number,
                                                 IPMI_GET_SOL_PARAMETER,
                                                 IPMI_SOL_CONFIGURATION_PARAMETER_OEM_INVENTEC_SOL_TIMEOUT,
                                                 0,
                                                 0,
                                                 obj_cmd_rs) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_get_sol_configuration_parameters: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  if ((len = fiid_obj_get_data (obj_cmd_rs,
                                "configuration_parameter_data",
                                configuration_parameter_data,
                                IPMI_OEM_MAX_BYTES)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get_data: 'configuration_parameter_data': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  if (len < 2)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_get_sol_configuration_parameters: invalid buffer length returned: %d\n",
                       len);
      goto cleanup;
    }

  sol_inactivity_timeout = 0;
  sol_inactivity_timeout |= configuration_parameter_data[0];
  sol_inactivity_timeout |= (configuration_parameter_data[1] << 8);

  if (sol_inactivity_timeout)
    pstdout_printf (state_data->pstate, "SOL Inactivity Timeout : %u minutes\n", sol_inactivity_timeout);
  else
    pstdout_printf (state_data->pstate, "SOL Inactivity Timeout : no timeout\n", sol_inactivity_timeout);
                  
  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

int
ipmi_oem_inventec_set_sol_inactivity_timeout (ipmi_oem_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t configuration_parameter_data[IPMI_OEM_MAX_BYTES];
  uint8_t lan_channel_number;
  uint16_t sol_inactivity_timeout = 0;
  int rv = -1;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 1);

  if (strcasecmp (state_data->prog_data->args->oem_options[0], "none"))
    {
      char *endptr = NULL;
      unsigned int temp;

      errno = 0;
      
      temp = strtoul (state_data->prog_data->args->oem_options[0], &endptr, 10);
      if (errno
	  || endptr[0] != '\0'
          || temp > USHRT_MAX)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "%s:%s invalid OEM option argument '%s'\n",
                           state_data->prog_data->args->oem_id,
                           state_data->prog_data->args->oem_command,
                           state_data->prog_data->args->oem_options[0]);
          goto cleanup;
        }

      sol_inactivity_timeout = temp;
    }
  else
    sol_inactivity_timeout = 0;
  
  /* Dell Xanadu II OEM
   * Dell Xanadu III OEM
   *
   * From Dell Provided Docs
   *
   * Uses Get/Set SOL Configuration
   * parameter = 192
   *
   * Data format
   *
   * 1st & 2nd byte = inactivity timeout, 1 based, 1 minute
   * increments, LSbyte first
   *
   */

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_set_sol_configuration_parameters_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_get_channel_number (state_data->ipmi_ctx,
                               IPMI_CHANNEL_MEDIUM_TYPE_LAN_802_3,
                               &lan_channel_number) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_get_channel_number: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  configuration_parameter_data[0] = sol_inactivity_timeout & 0x00FF;
  configuration_parameter_data[1] = (sol_inactivity_timeout & 0xFF00) >> 8;

  if (ipmi_cmd_set_sol_configuration_parameters (state_data->ipmi_ctx,
                                                 lan_channel_number,
                                                 IPMI_SOL_CONFIGURATION_PARAMETER_OEM_INVENTEC_SOL_TIMEOUT,
                                                 configuration_parameter_data,
                                                 2,
                                                 obj_cmd_rs) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_get_sol_configuration_parameters: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

int
ipmi_oem_inventec_restore_to_defaults (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  uint8_t task_id;
  int rs_len;
  int rv = -1;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 1);

  if (strcasecmp (state_data->prog_data->args->oem_options[0], "all")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "user")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "lan")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "sol")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "serial")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "pef"))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[0]);
      goto cleanup;
    }
  
  /* Inventec 5441 OEM
   *
   * Request Restore To Defaults
   *
   * 0x30 - OEM network function
   * 0x04 - OEM cmd
   * 0x?? - bitmask
   *      [7:5] = 111b = restore parameters not included below
   *            = 000b = remaining parameters stay what it is
   *      [4] = 1b = restore PEFs to default
   *      [3] = 1b = restore serial configuration parameters to default
   *      [2] = 1b = restore SOL configuration parameters to default
   *      [1] = 1b = restore LAN configuration parameters to default
   *      [0] = 1b = restore user accounts to default
   * 
   * Response Restore To Defaults
   *
   * 0x04 - OEM cmd
   * 0x?? - Completion Code
   *      - 0xCC - one or more configs not supported
   * 0x?? - Task ID - used to get the restore status.  Invalid after
   *        120 seconds.  00h = reserved.
   *
   * Request Get Restore Status
   *
   * 0x30 - OEM network function
   * 0x05 - OEM cmd
   * 0x?? - Task ID
   *
   * Response Get Restore Status
   *
   * 0x05 - OEM cmd
   * 0x?? - Completion Code
   * 0x?? - restore status
   *      - 00h = restore in progress
   *      - 01h = restore complete
   */

  bytes_rq[0] = IPMI_CMD_OEM_INVENTEC_RESTORE_TO_DEFAULTS;
   
  bytes_rq[1] = IPMI_OEM_INVENTEC_RESTORE_TO_DEFAULTS_RESTORE_FLAG_REMAINING_PARAMETERS_STAY_WHAT_IT_IS;
  bytes_rq[1] <<= IPMI_OEM_INVENTEC_RESTORE_TO_DEFAULTS_RESTORE_FLAG_SHIFT;

  if (!strcasecmp (state_data->prog_data->args->oem_options[0], "all"))
    {
#if 0
      /* achu: Compared to Quanta, if you set this, it doesn't work.
       * I have no idea why.
       */
      bytes_rq[1] = IPMI_OEM_INVENTEC_RESTORE_TO_DEFAULTS_RESTORE_FLAG_RESTORE_PARAMETERS_NOT_INCLUDED_BELOW;
      bytes_rq[1] <<= IPMI_OEM_INVENTEC_RESTORE_TO_DEFAULTS_RESTORE_FLAG_SHIFT;
#endif
      bytes_rq[1] |= IPMI_OEM_INVENTEC_RESTORE_TO_DEFAULTS_USER_ACCOUNTS_BITMASK;
      bytes_rq[1] |= IPMI_OEM_INVENTEC_RESTORE_TO_DEFAULTS_LAN_CONFIGURATION_BITMASK;
      bytes_rq[1] |= IPMI_OEM_INVENTEC_RESTORE_TO_DEFAULTS_SOL_CONFIGURATION_BITMASK;
      bytes_rq[1] |= IPMI_OEM_INVENTEC_RESTORE_TO_DEFAULTS_SERIAL_CONFIGURATION_BITMASK;
      bytes_rq[1] |= IPMI_OEM_INVENTEC_RESTORE_TO_DEFAULTS_PEF_BITMASK;
    }
  else if (!strcasecmp (state_data->prog_data->args->oem_options[0], "user"))
    bytes_rq[1] |= IPMI_OEM_INVENTEC_RESTORE_TO_DEFAULTS_USER_ACCOUNTS_BITMASK;
  else if (!strcasecmp (state_data->prog_data->args->oem_options[0], "lan"))
    bytes_rq[1] |= IPMI_OEM_INVENTEC_RESTORE_TO_DEFAULTS_LAN_CONFIGURATION_BITMASK;
  else if (!strcasecmp (state_data->prog_data->args->oem_options[0], "sol"))
    bytes_rq[1] |= IPMI_OEM_INVENTEC_RESTORE_TO_DEFAULTS_SOL_CONFIGURATION_BITMASK;
  else if (!strcasecmp (state_data->prog_data->args->oem_options[0], "serial"))
    bytes_rq[1] |= IPMI_OEM_INVENTEC_RESTORE_TO_DEFAULTS_SERIAL_CONFIGURATION_BITMASK;
  else  /* !strcasecmp (state_data->prog_data->args->oem_options[0], "pef" */
    bytes_rq[1] |= IPMI_OEM_INVENTEC_RESTORE_TO_DEFAULTS_PEF_BITMASK;
           
  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_INVENTEC_GENERIC_RQ, /* network function */
                              bytes_rq, /* data */
                              2, /* num bytes */
                              bytes_rs,
                              IPMI_OEM_MAX_BYTES)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_raw: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }
  
  if (ipmi_oem_check_response_and_completion_code (state_data,
                                                   bytes_rs,
                                                   rs_len,
                                                   3,
                                                   IPMI_CMD_OEM_INVENTEC_RESTORE_TO_DEFAULTS,
                                                   IPMI_NET_FN_OEM_INVENTEC_GENERIC_RS,
                                                   NULL) < 0)
    goto cleanup;
  
  task_id = bytes_rs[2];
  
  /* don't quit until it is done */
  while (1)
    {
      bytes_rq[0] = IPMI_CMD_OEM_INVENTEC_GET_RESTORE_STATUS;
      bytes_rq[1] = task_id;
      
      if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
				  0, /* lun */
				  IPMI_NET_FN_OEM_INVENTEC_GENERIC_RQ, /* network function */
				  bytes_rq, /* data */
				  2, /* num bytes */
				  bytes_rs,
				  IPMI_OEM_MAX_BYTES)) < 0)
	{
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "ipmi_cmd_raw: %s\n",
			   ipmi_ctx_errormsg (state_data->ipmi_ctx));
	  goto cleanup;
	}
      
      if (ipmi_oem_check_response_and_completion_code (state_data,
						       bytes_rs,
						       rs_len,
						       3,
						       IPMI_CMD_OEM_INVENTEC_GET_RESTORE_STATUS,
						       IPMI_NET_FN_OEM_INVENTEC_GENERIC_RS,
                                                       NULL) < 0)
	goto cleanup;

      if (bytes_rs[2] == IPMI_OEM_INVENTEC_GET_RESTORE_STATUS_RESTORE_COMPLETE)
	break;

      sleep (1);
    }
  
  rv = 0;
 cleanup:
  return (rv);
}

#if 0
/* cannot verify */
int
ipmi_oem_inventec_set_system_guid (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  int rs_len;
  int rv = -1;
  int i;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 1);

  if (strlen (state_data->prog_data->args->oem_options[0]) != (IPMI_SYSTEM_GUID_LENGTH * 2))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s OEM option argument '%s' invalid length, must be %u long\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[0],
                       (IPMI_SYSTEM_GUID_LENGTH * 2));
      goto cleanup;
    }
  
  for (i = 0; i < (IPMI_SYSTEM_GUID_LENGTH * 2); i++)
    {
      if (!isxdigit (state_data->prog_data->args->oem_options[0][i]))
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "%s:%s OEM option argument '%s' contains invalid hex code\n",
                           state_data->prog_data->args->oem_id,
                           state_data->prog_data->args->oem_command,
                           state_data->prog_data->args->oem_options[0]);
          goto cleanup;
        }
    }

  /* Inventec 5441 OEM
   *
   * Set System GUID Request
   *
   * 0x30 - OEM network function
   * 0xB3 - OEM cmd
   * bytes 1-16: System GUID
   *
   * Set System GUID Response
   *
   * 0xB3 - OEM cmd
   * 0x?? - Completion Code
   */

  bytes_rq[0] = IPMI_CMD_OEM_INVENTEC_SET_SYSTEM_GUID;
  for (i = 0; i < IPMI_SYSTEM_GUID_LENGTH; i++)
    {
      char strbuf[IPMI_OEM_BUFLEN];
      char *endptr;
      long val;
      
      /* achu: there *must* be something faster than this, I just
       * can't find the magic lib call to do 1-char to 1-hex.  All the
       * strxxx() functions take NUL terminated strings.
       */

      memset (strbuf, '\0', IPMI_OEM_BUFLEN);
      strbuf[0] = state_data->prog_data->args->oem_options[0][i];
      errno = 0; 
      val = strtol (strbuf, &endptr, IPMI_OEM_HEX_BASE);
      if (errno
	  || endptr[0] != '\0')
	{
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "strtol: invalid string '%s'\n",
			   strbuf);
	  goto cleanup;
	}
      bytes_rq[1 + i] |= (val & 0x0F);

      memset (strbuf, '\0', IPMI_OEM_BUFLEN);
      strbuf[0] = state_data->prog_data->args->oem_options[0][i + 1];
      errno = 0; 
      val = strtol (strbuf, &endptr, IPMI_OEM_HEX_BASE);
      if (errno
	  || endptr[0] != '\0')
	{
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "strtol: invalid string '%s'\n",
			   strbuf);
	  goto cleanup;
	}
      bytes_rq[1 + i] |= ((val << 4) & 0xF0);
    }

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_INVENTEC_GENERIC_RQ, /* network function */
                              bytes_rq, /* data */
                              17, /* num bytes */
                              bytes_rs,
                              IPMI_OEM_MAX_BYTES)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_raw: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }
  
  if (ipmi_oem_check_response_and_completion_code (state_data,
                                                   bytes_rs,
                                                   rs_len,
                                                   2,
                                                   IPMI_CMD_OEM_INVENTEC_SET_SYSTEM_GUID,
                                                   IPMI_NET_FN_OEM_INVENTEC_GENERIC_RS,
                                                   NULL) < 0)
    goto cleanup;
  
  rv = 0;
 cleanup:
  return (rv);
}
#endif

static int
_ipmi_oem_inventec_read_eeprom_at24c256n (ipmi_oem_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t data_rq[IPMI_OEM_MAX_BYTES];
  uint8_t data_rs[IPMI_OEM_MAX_BYTES];
  unsigned int read_count = 0;
  int len;
  int rv = -1;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 1);

  /* Uses Master-Read Write Command
   *
   * Most addresses provided directly from Dell.
   *
   * byte 1 = 5 (channel = 0, bus id = 2, bus-type = 1 = private)
   * byte 2 = 0xA6 (slave address 7 bit = 0x53, lowest bit for r/w, 0b = read, 1b = write)
   * byte 3 = read count, we'll use 1
   * byte 4/5 - address to read, msb first
   * 
   * response
   *
   * byte 1 = comp-code
   * byte N = read data
   *
   * address ranges from 0x0000 - 0x7fff
   */

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_master_write_read_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }
  
  if (fiid_obj_clear (obj_cmd_rs) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_clear: %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  
  while (read_count <= IPMI_OEM_INVENTEC_EEPROM_AT24C256N_ADDRESS_MAX)
    {
      data_rq[0] = (read_count & 0xFF00) >> 8;
      data_rq[1] = (read_count & 0x00FF);
      
      if (ipmi_cmd_master_write_read (state_data->ipmi_ctx,
                                      IPMI_BUS_TYPE_PRIVATE,
                                      IPMI_OEM_INVENTEC_EEPROM_AT24C256N_BUS_ID,
                                      0,
                                      IPMI_OEM_INVENTEC_EEPROM_AT24C256N_SLAVE_ADDRESS,
                                      1,
                                      data_rq,
                                      2,
                                      obj_cmd_rs) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "ipmi_cmd_master_write_read: %s\n",
                           ipmi_ctx_errormsg (state_data->ipmi_ctx));
          goto cleanup;
        }

      if ((len = fiid_obj_get_data (obj_cmd_rs,
                                    "data",
                                    data_rs,
                                    IPMI_OEM_MAX_BYTES)) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_get_data: %s\n",
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }
      
      if (len)
        {
          int i;
          
          for (i = read_count; i < (read_count + len); i++)
            {
              if (i && (i % 8) == 0)
                pstdout_printf (state_data->pstate, "\n");
              
              pstdout_printf (state_data->pstate, "0x%02X ", data_rs[i - read_count]);
            }
          
          read_count += len;
        }
    }

  pstdout_printf (state_data->pstate, "\n");
  
  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

int
ipmi_oem_inventec_read_eeprom (ipmi_oem_state_data_t *state_data)
{
  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 1);

  if (!strcasecmp (state_data->prog_data->args->oem_options[0], "at24c256n"))
    return _ipmi_oem_inventec_read_eeprom_at24c256n (state_data);
  
  pstdout_fprintf (state_data->pstate,
                   stderr,
                   "%s:%s invalid OEM option argument '%s'\n",
                   state_data->prog_data->args->oem_id,
                   state_data->prog_data->args->oem_command,
                   state_data->prog_data->args->oem_options[0]);
  return (-1);
}

static int
_ipmi_oem_inventec_clear_eeprom_at24c256n (ipmi_oem_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t data_rq[IPMI_OEM_MAX_BYTES];
  unsigned int count = 0;
  unsigned int percent = 0;
  int rv = -1;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 1);

  /* Uses Master-Read Write Command
   *
   * Most addresses provided directly from Dell.
   *
   * byte 1 = 5 (channel = 0, bus id = 2, bus-type = 1 = private)
   * byte 2 = 0xA6 (slave address 7 bit = 0x53, lowest bit for r/w, 0b = read, 1b = write)
   * byte 3 = read count, 0 to write
   * byte 4/5 - address to read, msb first
   * byte 6 - data to write
   * 
   * response
   *
   * byte 1 = comp-code
   * byte N = read data
   *
   * address ranges from 0x0000 - 0x7fff
   */

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_master_write_read_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (fiid_obj_clear (obj_cmd_rs) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_clear: %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  if (state_data->prog_data->args->verbose_count)
    fprintf (stderr, "%u%%\r", percent);

  while (count <= IPMI_OEM_INVENTEC_EEPROM_AT24C256N_ADDRESS_MAX)
    {
      data_rq[0] = (count & 0xFF00) >> 8;
      data_rq[1] = (count & 0x00FF);
      data_rq[2] = IPMI_OEM_INVENTEC_EEPROM_AT24C256N_CLEAR_BYTE;

      if (ipmi_cmd_master_write_read (state_data->ipmi_ctx,
                                      IPMI_BUS_TYPE_PRIVATE,
                                      IPMI_OEM_INVENTEC_EEPROM_AT24C256N_BUS_ID,
                                      0,
                                      IPMI_OEM_INVENTEC_EEPROM_AT24C256N_SLAVE_ADDRESS,
                                      0,
                                      data_rq,
                                      3,
                                      obj_cmd_rs) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "ipmi_cmd_master_write_read: %s\n",
                           ipmi_ctx_errormsg (state_data->ipmi_ctx));
          goto cleanup;
        }

      if (state_data->prog_data->args->verbose_count)
        {
          if ((unsigned int)(((double)count/IPMI_OEM_INVENTEC_EEPROM_AT24C256N_ADDRESS_MAX) * 100) > percent)
            {
              fprintf (stderr, "%u%%\r", percent);
              percent++;
            }
        }

      count++;
    }

  if (state_data->prog_data->args->verbose_count)
    fprintf (stderr, "100%%\r\n");

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

int
ipmi_oem_inventec_clear_eeprom (ipmi_oem_state_data_t *state_data)
{
  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 1);

  if (!strcasecmp (state_data->prog_data->args->oem_options[0], "at24c256n"))
    return _ipmi_oem_inventec_clear_eeprom_at24c256n (state_data);
  
  pstdout_fprintf (state_data->pstate,
                   stderr,
                   "%s:%s invalid OEM option argument '%s'\n",
                   state_data->prog_data->args->oem_id,
                   state_data->prog_data->args->oem_command,
                   state_data->prog_data->args->oem_options[0]);
  return (-1);
}
