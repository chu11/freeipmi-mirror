/* 
   ipmi-sensor-types.h - IPMI Sensor Types

   Copyright (C) 2003, 2004, 2005 FreeIPMI Core Team

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
   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  
*/

#ifndef _IPMI_SENSOR_TYPES_H
#define _IPMI_SENSOR_TYPES_H

/* #define IPMI_SENSOR_TYPE_TEMPERATURE                    0x01 */
/* #define IPMI_SENSOR_TYPE_VOLTAGE                        0x02 */
/* #define IPMI_SENSOR_TYPE_CURRENT                        0x03 */
/* #define IPMI_SENSOR_TYPE_FAN                            0x04 */
/* #define IPMI_SENSOR_TYPE_PLATFORM_CHASSIS_INTRUSION     0x05 */
/* #define IPMI_SENSOR_TYPE_PLATFORM_SECURITY_VIOLATION    0x06 */
/* #define IPMI_SENSOR_TYPE_PROCESSOR                      0x07 */
/* #define IPMI_SENSOR_TYPE_POWER_SUPPLY                   0x08 */
/* #define IPMI_SENSOR_TYPE_POWER_UNIT                     0x09 */
/* #define IPMI_SENSOR_TYPE_COOLING_DEVICE                 0x0A */
/* #define IPMI_SENSOR_TYPE_FRU_SENSOR                     0x0B */
/* #define IPMI_SENSOR_TYPE_MEMORY                         0x0C */
/* #define IPMI_SENSOR_TYPE_DRIVE_SLOT                     0x0D */
/* #define IPMI_SENSOR_TYPE_POST_MEMORY_RESIZE             0x0E */
/* #define IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE                0x0F */
/* #define IPMI_SENSOR_TYPE_EVENT_LOGGING_DISABLED         0x10 */
/* #define IPMI_SENSOR_TYPE_WATCHDOG1                      0x11 */
/* #define IPMI_SENSOR_TYPE_SYSTEM_EVENT                   0x12 */
/* #define IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT             0x13 */
/* #define IPMI_SENSOR_TYPE_BUTTON                         0x14 */
/* #define IPMI_SENSOR_TYPE_BOARD                          0x15 */
/* #define IPMI_SENSOR_TYPE_MICROCONTROLLER                0x16 */
/* #define IPMI_SENSOR_TYPE_ADD_IN_CARD                    0x17 */
/* #define IPMI_SENSOR_TYPE_CHASSIS                        0x18 */
/* #define IPMI_SENSOR_TYPE_CHIP_SET                       0x19 */
/* #define IPMI_SENSOR_TYPE_OTHER_FRU                      0x1A */
/* #define IPMI_SENSOR_TYPE_CABLE_INTERCONNECT             0x1B */
/* #define IPMI_SENSOR_TYPE_TERMINATOR                     0x1C */
/* #define IPMI_SENSOR_TYPE_SYSTEM_BOOT_INITIATED          0x1D */
/* #define IPMI_SENSOR_TYPE_BOOT_ERROR                     0x1E */
/* #define IPMI_SENSOR_TYPE_OS_BOOT                        0x1F */
/* #define IPMI_SENSOR_TYPE_OS_CRITICAL_STOP               0x20 */
/* #define IPMI_SENSOR_TYPE_SLOT_CONNECTOR                 0x21 */
/* #define IPMI_SENSOR_TYPE_ACPI_POWER_STATE               0x22 */
/* #define IPMI_SENSOR_TYPE_WATCHDOG_2                     0x23 */
/* #define IPMI_SENSOR_TYPE_PLATFORM_ALERT                 0x24 */
/* #define IPMI_SENSOR_TYPE_ENTITY_PRESENCE                0x25 */
/* #define IPMI_SENSOR_TYPE_MONITOR_ASIC                   0x26 */
/* #define IPMI_SENSOR_TYPE_LAN                            0x27 */
/* #define IPMI_SENSOR_TYPE_MANAGEMENT_SUBSYSTEM_HEALTH    0x28 */
/* #define IPMI_SENSOR_TYPE_BATTERY                        0x29 */
/* #define IPMI_SENSOR_TYPE_SESSION_AUDIT                  0x2A */
/* #define IPMI_SENSOR_TYPE_VERSION_CHANGE                 0x2B */
/* #define IPMI_SENSOR_TYPE_FRU_STATE                      0x2C */

#ifdef __cplusplus
extern "C" {
#endif

enum ipmi_sensor_type
  {
    IPMI_SENSOR_TYPE_UNKNOWN, 
    IPMI_SENSOR_TYPE_TEMPERATURE, 
    IPMI_SENSOR_TYPE_VOLTAGE, 
    IPMI_SENSOR_TYPE_CURRENT, 
    IPMI_SENSOR_TYPE_FAN, 
    IPMI_SENSOR_TYPE_PLATFORM_CHASSIS_INTRUSION, 
    IPMI_SENSOR_TYPE_PLATFORM_SECURITY_VIOLATION, 
    IPMI_SENSOR_TYPE_PROCESSOR, 
    IPMI_SENSOR_TYPE_POWER_SUPPLY, 
    IPMI_SENSOR_TYPE_POWER_UNIT, 
    IPMI_SENSOR_TYPE_COOLING_DEVICE, 
    IPMI_SENSOR_TYPE_FRU_SENSOR, 
    IPMI_SENSOR_TYPE_MEMORY, 
    IPMI_SENSOR_TYPE_DRIVE_SLOT, 
    IPMI_SENSOR_TYPE_POST_MEMORY_RESIZE, 
    IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE, 
    IPMI_SENSOR_TYPE_EVENT_LOGGING_DISABLED, 
    IPMI_SENSOR_TYPE_WATCHDOG1, 
    IPMI_SENSOR_TYPE_SYSTEM_EVENT, 
    IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT, 
    IPMI_SENSOR_TYPE_BUTTON, 
    IPMI_SENSOR_TYPE_BOARD, 
    IPMI_SENSOR_TYPE_MICROCONTROLLER, 
    IPMI_SENSOR_TYPE_ADD_IN_CARD, 
    IPMI_SENSOR_TYPE_CHASSIS, 
    IPMI_SENSOR_TYPE_CHIP_SET, 
    IPMI_SENSOR_TYPE_OTHER_FRU, 
    IPMI_SENSOR_TYPE_CABLE_INTERCONNECT, 
    IPMI_SENSOR_TYPE_TERMINATOR, 
    IPMI_SENSOR_TYPE_SYSTEM_BOOT_INITIATED, 
    IPMI_SENSOR_TYPE_BOOT_ERROR, 
    IPMI_SENSOR_TYPE_OS_BOOT, 
    IPMI_SENSOR_TYPE_OS_CRITICAL_STOP, 
    IPMI_SENSOR_TYPE_SLOT_CONNECTOR, 
    IPMI_SENSOR_TYPE_ACPI_POWER_STATE, 
    IPMI_SENSOR_TYPE_WATCHDOG2, 
    IPMI_SENSOR_TYPE_PLATFORM_ALERT, 
    IPMI_SENSOR_TYPE_ENTITY_PRESENCE, 
    IPMI_SENSOR_TYPE_MONITOR_ASIC, 
    IPMI_SENSOR_TYPE_LAN, 
    IPMI_SENSOR_TYPE_MANAGEMENT_SUBSYSTEM_HEALTH, 
    IPMI_SENSOR_TYPE_BATTERY, 
    IPMI_SENSOR_TYPE_SESSION_AUDIT, 
    IPMI_SENSOR_TYPE_VERSION_CHANGE, 
    IPMI_SENSOR_TYPE_FRU_STATE
  };
  
enum ipmi_sensor_unit_type_code
  {
    IPMI_SENSOR_UNIT_UNSPECIFIED, 
    IPMI_SENSOR_UNIT_DEGREES_C, 
    IPMI_SENSOR_UNIT_DEGREES_F, 
    IPMI_SENSOR_UNIT_DEGREES_K, 
    IPMI_SENSOR_UNIT_VOLTS, 
    IPMI_SENSOR_UNIT_AMPS, 
    IPMI_SENSOR_UNIT_WATTS, 
    IPMI_SENSOR_UNIT_JOULES, 
    IPMI_SENSOR_UNIT_COULOMBS, 
    IPMI_SENSOR_UNIT_VA, 
    IPMI_SENSOR_UNIT_NITS, 
    IPMI_SENSOR_UNIT_LUMEN, 
    IPMI_SENSOR_UNIT_LUX, 
    IPMI_SENSOR_UNIT_CANDELA, 
    IPMI_SENSOR_UNIT_KPA, 
    IPMI_SENSOR_UNIT_PSI, 
    IPMI_SENSOR_UNIT_NEWTON, 
    IPMI_SENSOR_UNIT_CFM, 
    IPMI_SENSOR_UNIT_RPM, 
    IPMI_SENSOR_UNIT_HZ, 
    IPMI_SENSOR_UNIT_MICROSECOND, 
    IPMI_SENSOR_UNIT_MILLISECOND, 
    IPMI_SENSOR_UNIT_SECOND, 
    IPMI_SENSOR_UNIT_MINUTE, 
    IPMI_SENSOR_UNIT_HOUR, 
    IPMI_SENSOR_UNIT_DAY, 
    IPMI_SENSOR_UNIT_WEEK, 
    IPMI_SENSOR_UNIT_MIL, 
    IPMI_SENSOR_UNIT_INCHES, 
    IPMI_SENSOR_UNIT_FEET, 
    IPMI_SENSOR_UNIT_CU_IN, 
    IPMI_SENSOR_UNIT_CU_FEET, 
    IPMI_SENSOR_UNIT_MM, 
    IPMI_SENSOR_UNIT_CM, 
    IPMI_SENSOR_UNIT_M, 
    IPMI_SENSOR_UNIT_CU_CM, 
    IPMI_SENSOR_UNIT_CU_M, 
    IPMI_SENSOR_UNIT_LITERS, 
    IPMI_SENSOR_UNIT_FLUID_OUNCE, 
    IPMI_SENSOR_UNIT_RADIANS, 
    IPMI_SENSOR_UNIT_STERADIANS, 
    IPMI_SENSOR_UNIT_REVOLUTIONS, 
    IPMI_SENSOR_UNIT_CYCLES, 
    IPMI_SENSOR_UNIT_GRAVITIES, 
    IPMI_SENSOR_UNIT_OUNCE, 
    IPMI_SENSOR_UNIT_POUND, 
    IPMI_SENSOR_UNIT_FT_LB, 
    IPMI_SENSOR_UNIT_OZ_IN, 
    IPMI_SENSOR_UNIT_GAUSS, 
    IPMI_SENSOR_UNIT_GILBERTS, 
    IPMI_SENSOR_UNIT_HENRY, 
    IPMI_SENSOR_UNIT_MILLIHENRY, 
    IPMI_SENSOR_UNIT_FARAD, 
    IPMI_SENSOR_UNIT_MICROFARAD, 
    IPMI_SENSOR_UNIT_OHMS, 
    IPMI_SENSOR_UNIT_SIEMENS, 
    IPMI_SENSOR_UNIT_MOLE, 
    IPMI_SENSOR_UNIT_BECQUEREL, 
    IPMI_SENSOR_UNIT_PPM, 
    IPMI_SENSOR_UNIT_RESERVED, 
    IPMI_SENSOR_UNIT_DECIBELS, 
    IPMI_SENSOR_UNIT_DBA, 
    IPMI_SENSOR_UNIT_DBC, 
    IPMI_SENSOR_UNIT_GRAY, 
    IPMI_SENSOR_UNIT_SIEVERT, 
    IPMI_SENSOR_UNIT_COLOR_TEMP_DEG_K, 
    IPMI_SENSOR_UNIT_BIT, 
    IPMI_SENSOR_UNIT_KILOBIT, 
    IPMI_SENSOR_UNIT_MEGABIT, 
    IPMI_SENSOR_UNIT_GIGABIT, 
    IPMI_SENSOR_UNIT_BYTE, 
    IPMI_SENSOR_UNIT_KILOBYTE, 
    IPMI_SENSOR_UNIT_MEGABYTE, 
    IPMI_SENSOR_UNIT_GIGABYTE, 
    IPMI_SENSOR_UNIT_WORD, 
    IPMI_SENSOR_UNIT_DWORD, 
    IPMI_SENSOR_UNIT_LINE, 
    IPMI_SENSOR_UNIT_HIT, 
    IPMI_SENSOR_UNIT_MISS, 
    IPMI_SENSOR_UNIT_RETRY, 
    IPMI_SENSOR_UNIT_RESET, 
    IPMI_SENSOR_UNIT_OVERRUN_OVERFLOW, 
    IPMI_SENSOR_UNIT_UNDERRUN, 
    IPMI_SENSOR_UNIT_COLLISION, 
    IPMI_SENSOR_UNIT_PACKETS, 
    IPMI_SENSOR_UNIT_MESSAGES, 
    IPMI_SENSOR_UNIT_CHARACTERS, 
    IPMI_SENSOR_UNIT_ERROR, 
    IPMI_SENSOR_UNIT_CORRECTABLE_ERROR, 
    IPMI_SENSOR_UNIT_UNCORRECTABLE_ERROR
  };

enum ipmi_sensor_category
  {
    IPMI_SENSOR_CATEGORY_UNSPECIFIED, 
    IPMI_SENSOR_CATEGORY_THRESHOLD, 
    IPMI_SENSOR_CATEGORY_GENERIC, 
    IPMI_SENSOR_CATEGORY_SENSOR_SPECIFIC, 
    IPMI_SENSOR_CATEGORY_OEM
  };

enum ipmi_sensor_class
  {
    IPMI_SENSOR_CLASS_NOT_AVAILABLE, 
    IPMI_SENSOR_CLASS_THRESHOLD, 
    IPMI_SENSOR_CLASS_GENERIC_DISCRETE, 
    IPMI_SENSOR_CLASS_SENSOR_SPECIFIC_DISCRETE, 
    IPMI_SENSOR_CLASS_OEM
  };

extern const char *const ipmi_sensor_types[];
extern const char *const ipmi_oem_sensor_type;
extern const char *const ipmi_sensor_units[];
extern const char *const ipmi_sensor_units_short[];

typedef struct ipmi_discrete_desc
{
  char *message;
  bool normal_code;
} ipmi_discrete_desc_t ;


extern const char *const ipmi_sensor_type_threshold_desc[];

extern const ipmi_discrete_desc_t ipmi_sensor_type_dummy_desc[]; 
extern const ipmi_discrete_desc_t ipmi_sensor_type_physical_security_desc[]; 
extern const ipmi_discrete_desc_t ipmi_sersor_type_platform_security_violation_attempt[]; 
extern const ipmi_discrete_desc_t ipmi_sensor_type_processor[];
extern const ipmi_discrete_desc_t ipmi_sensor_type_power_supply[];
extern const ipmi_discrete_desc_t ipmi_sensor_type_power_unit[];
extern const ipmi_discrete_desc_t ipmi_sensor_type_memory[];
extern const ipmi_discrete_desc_t ipmi_sensor_type_system_firmware_progress[];
extern const ipmi_discrete_desc_t ipmi_sensor_type_event_logging_disabled[];
extern const ipmi_discrete_desc_t ipmi_sensor_type_watchdog_1[];
extern const ipmi_discrete_desc_t ipmi_sensor_type_system_event[];
extern const ipmi_discrete_desc_t ipmi_sensor_type_critical_interrupt[];
extern const ipmi_discrete_desc_t ipmi_sensor_type_button_switch[];
extern const ipmi_discrete_desc_t ipmi_sensor_type_chip_set[];
extern const ipmi_discrete_desc_t ipmi_sensor_type_system_boot_initiated[];
extern const ipmi_discrete_desc_t ipmi_sensor_type_boot_error[];
extern const ipmi_discrete_desc_t ipmi_sensor_type_os_boot[];
extern const ipmi_discrete_desc_t ipmi_sensor_type_os_critical_stop[];
extern const ipmi_discrete_desc_t ipmi_sensor_type_slot_connector[];
extern const ipmi_discrete_desc_t ipmi_sensor_type_system_acpi_power_state[];
extern const ipmi_discrete_desc_t ipmi_sensor_type_watchdog_2[];
extern const ipmi_discrete_desc_t ipmi_sensor_type_platform_alert[];
extern const ipmi_discrete_desc_t ipmi_sensor_type_entity_presence[];
extern const ipmi_discrete_desc_t ipmi_sensor_type_lan[]; 
extern const ipmi_discrete_desc_t ipmi_sensor_type_management_subsystem_health[];
extern const ipmi_discrete_desc_t ipmi_sensor_type_battery[];
extern const ipmi_discrete_desc_t ipmi_sensor_type_session_audit[];
extern const ipmi_discrete_desc_t ipmi_sensor_type_version_change[];
extern const ipmi_discrete_desc_t ipmi_sensor_type_fru_state[];

extern const struct ipmi_discrete_desc *const ipmi_sensor_type_desc_ptr[];

extern const char *const ipmi_event_reading_type_code_dummy_desc[];
extern const char *const ipmi_event_reading_type_code_2_desc[];
extern const char *const ipmi_event_reading_type_code_3_desc[];
extern const char *const ipmi_event_reading_type_code_6_desc[];
extern const char *const ipmi_event_reading_type_code_8_desc[];
extern const char *const ipmi_event_reading_type_code_11_desc[];

extern const char *const *const ipmi_event_reading_type_code_desc_ptr[];


int ipmi_sensor_classify (u_int8_t event_reading_type_code);

int ipmi_is_oem_reserved_sensor_type (int sensor_type);

const char *ipmi_get_sensor_group (int sensor_type);
int ipmi_sensor_threshold_health_check (double sensor_reading, 
					double normal_min, 
					double normal_max, 
					fiid_obj_t data_rs);
int ipmi_sensor_discrete_health_check (int sensor_type, fiid_obj_t data_rs);

#ifdef __cplusplus
}
#endif

#endif
