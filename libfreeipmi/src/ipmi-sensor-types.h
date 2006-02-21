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
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.  
*/

#ifndef _IPMI_SENSOR_TYPES_H
#define _IPMI_SENSOR_TYPES_H

#ifdef __cplusplus
extern "C" {
#endif

#define IPMI_SENSOR_TYPE_RESERVED                   0x00 
#define IPMI_SENSOR_TYPE_TEMPERATURE                   0x01 
#define IPMI_SENSOR_TYPE_VOLTAGE                       0x02 
#define IPMI_SENSOR_TYPE_CURRENT                       0x03
#define IPMI_SENSOR_TYPE_FAN                           0x04
#define IPMI_SENSOR_TYPE_PLATFORM_CHASSIS_INTRUSION    0x05
#define IPMI_SENSOR_TYPE_PLATFORM_SECURITY_VIOLATION   0x06
#define IPMI_SENSOR_TYPE_PROCESSOR                     0x07
#define IPMI_SENSOR_TYPE_POWER_SUPPLY                  0x08
#define IPMI_SENSOR_TYPE_POWER_UNIT                    0x09
#define IPMI_SENSOR_TYPE_COOLING_DEVICE                0x0A
#define IPMI_SENSOR_TYPE_FRU_SENSOR                    0x0B
#define IPMI_SENSOR_TYPE_MEMORY                        0x0C
#define IPMI_SENSOR_TYPE_DRIVE_SLOT                    0x0D
#define IPMI_SENSOR_TYPE_POST_MEMORY_RESIZE            0x0E
#define IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE               0x0F
#define IPMI_SENSOR_TYPE_EVENT_LOGGING_DISABLED        0x10
#define IPMI_SENSOR_TYPE_WATCHDOG1                     0x11
#define IPMI_SENSOR_TYPE_SYSTEM_EVENT                  0x12
#define IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT            0x13
#define IPMI_SENSOR_TYPE_BUTTON                        0x14
#define IPMI_SENSOR_TYPE_BOARD                         0x15
#define IPMI_SENSOR_TYPE_MICROCONTROLLER               0x16
#define IPMI_SENSOR_TYPE_ADD_IN_CARD                   0x17
#define IPMI_SENSOR_TYPE_CHASSIS                       0x18
#define IPMI_SENSOR_TYPE_CHIP_SET                      0x19
#define IPMI_SENSOR_TYPE_OTHER_FRU                     0x1A
#define IPMI_SENSOR_TYPE_CABLE_INTERCONNECT            0x1B
#define IPMI_SENSOR_TYPE_TERMINATOR                    0x1C
#define IPMI_SENSOR_TYPE_SYSTEM_BOOT_INITIATED         0x1D
#define IPMI_SENSOR_TYPE_BOOT_ERROR                    0x1E
#define IPMI_SENSOR_TYPE_OS_BOOT                       0x1F
#define IPMI_SENSOR_TYPE_OS_CRITICAL_STOP              0x20
#define IPMI_SENSOR_TYPE_SLOT_CONNECTOR                0x21
#define IPMI_SENSOR_TYPE_ACPI_POWER_STATE              0x22
#define IPMI_SENSOR_TYPE_WATCHDOG2                     0x23
#define IPMI_SENSOR_TYPE_PLATFORM_ALERT                0x24
#define IPMI_SENSOR_TYPE_ENTITY_PRESENCE               0x25
#define IPMI_SENSOR_TYPE_MONITOR_ASIC                  0x26
#define IPMI_SENSOR_TYPE_LAN                           0x27
#define IPMI_SENSOR_TYPE_MANAGEMENT_SUBSYSTEM_HEALTH   0x28
#define IPMI_SENSOR_TYPE_BATTERY                       0x29
#define IPMI_SENSOR_TYPE_SESSION_AUDIT                 0x2A
#define IPMI_SENSOR_TYPE_VERSION_CHANGE                0x2B
#define IPMI_SENSOR_TYPE_FRU_STATE                     0x2C

#define IPMI_SENSOR_TYPE_VALID(__sensor_type) \
        (((__sensor_type) >= IPMI_SENSOR_TYPE_RESERVED \
          && (__sensor_type) <= IPMI_SENSOR_TYPE_FRU_STATE) ? 1 : 0)

#define IPMI_SENSOR_TYPE_IS_OEM(__sensor_type) \
        (((__sensor_type) >= 0xC0 \
          && (__sensor_type) <= 0xFF) ? 1 : 0)

#define IPMI_SENSOR_UNIT_UNSPECIFIED             0
#define IPMI_SENSOR_UNIT_DEGREES_C               1
#define IPMI_SENSOR_UNIT_DEGREES_F               2
#define IPMI_SENSOR_UNIT_DEGREES_K               3
#define IPMI_SENSOR_UNIT_VOLTS                   4
#define IPMI_SENSOR_UNIT_AMPS                    5
#define IPMI_SENSOR_UNIT_WATTS                   6
#define IPMI_SENSOR_UNIT_JOULES                  7
#define IPMI_SENSOR_UNIT_COULOMBS                8
#define IPMI_SENSOR_UNIT_VA                      9
#define IPMI_SENSOR_UNIT_NITS                   10
#define IPMI_SENSOR_UNIT_LUMEN                  11
#define IPMI_SENSOR_UNIT_LUX                    12
#define IPMI_SENSOR_UNIT_CANDELA                13
#define IPMI_SENSOR_UNIT_KPA                    14
#define IPMI_SENSOR_UNIT_PSI                    15
#define IPMI_SENSOR_UNIT_NEWTON                 16
#define IPMI_SENSOR_UNIT_CFM                    17
#define IPMI_SENSOR_UNIT_RPM                    18
#define IPMI_SENSOR_UNIT_HZ                     19
#define IPMI_SENSOR_UNIT_MICROSECOND            20
#define IPMI_SENSOR_UNIT_MILLISECOND            21  
#define IPMI_SENSOR_UNIT_SECOND                 22
#define IPMI_SENSOR_UNIT_MINUTE                 23
#define IPMI_SENSOR_UNIT_HOUR                   24
#define IPMI_SENSOR_UNIT_DAY                    25
#define IPMI_SENSOR_UNIT_WEEK                   26
#define IPMI_SENSOR_UNIT_MIL                    27
#define IPMI_SENSOR_UNIT_INCHES                 28
#define IPMI_SENSOR_UNIT_FEET                   29
#define IPMI_SENSOR_UNIT_CU_IN                  30
#define IPMI_SENSOR_UNIT_CU_FEET                31
#define IPMI_SENSOR_UNIT_MM                     32
#define IPMI_SENSOR_UNIT_CM                     33
#define IPMI_SENSOR_UNIT_M                      34
#define IPMI_SENSOR_UNIT_CU_CM                  35
#define IPMI_SENSOR_UNIT_CU_M                   36
#define IPMI_SENSOR_UNIT_LITERS                 37
#define IPMI_SENSOR_UNIT_FLUID_OUNCE            38
#define IPMI_SENSOR_UNIT_RADIANS                39 
#define IPMI_SENSOR_UNIT_STERADIANS             40
#define IPMI_SENSOR_UNIT_REVOLUTIONS            41
#define IPMI_SENSOR_UNIT_CYCLES                 42
#define IPMI_SENSOR_UNIT_GRAVITIES              43
#define IPMI_SENSOR_UNIT_OUNCE                  44
#define IPMI_SENSOR_UNIT_POUND                  45
#define IPMI_SENSOR_UNIT_FT_LB                  46
#define IPMI_SENSOR_UNIT_OZ_IN                  47
#define IPMI_SENSOR_UNIT_GAUSS                  48
#define IPMI_SENSOR_UNIT_GILBERTS               49
#define IPMI_SENSOR_UNIT_HENRY                  50
#define IPMI_SENSOR_UNIT_MILLIHENRY             51
#define IPMI_SENSOR_UNIT_FARAD                  52
#define IPMI_SENSOR_UNIT_MICROFARAD             53
#define IPMI_SENSOR_UNIT_OHMS                   54
#define IPMI_SENSOR_UNIT_SIEMENS                55
#define IPMI_SENSOR_UNIT_MOLE                   56
#define IPMI_SENSOR_UNIT_BECQUEREL              57
#define IPMI_SENSOR_UNIT_PPM                    58
#define IPMI_SENSOR_UNIT_RESERVED               59
#define IPMI_SENSOR_UNIT_DECIBELS               60
#define IPMI_SENSOR_UNIT_DBA                    61
#define IPMI_SENSOR_UNIT_DBC                    62
#define IPMI_SENSOR_UNIT_GRAY                   63
#define IPMI_SENSOR_UNIT_SIEVERT                64
#define IPMI_SENSOR_UNIT_COLOR_TEMP_DEG_K       65
#define IPMI_SENSOR_UNIT_BIT                    66
#define IPMI_SENSOR_UNIT_KILOBIT                67
#define IPMI_SENSOR_UNIT_MEGABIT                68
#define IPMI_SENSOR_UNIT_GIGABIT                69
#define IPMI_SENSOR_UNIT_BYTE                   70
#define IPMI_SENSOR_UNIT_KILOBYTE               71
#define IPMI_SENSOR_UNIT_MEGABYTE               72
#define IPMI_SENSOR_UNIT_GIGABYTE               73
#define IPMI_SENSOR_UNIT_WORD                   74
#define IPMI_SENSOR_UNIT_DWORD                  75
#define IPMI_SENSOR_UNIT_QWORD                  76
#define IPMI_SENSOR_UNIT_LINE                   77
#define IPMI_SENSOR_UNIT_HIT                    78
#define IPMI_SENSOR_UNIT_MISS                   79
#define IPMI_SENSOR_UNIT_RETRY                  80
#define IPMI_SENSOR_UNIT_RESET                  81
#define IPMI_SENSOR_UNIT_OVERRUN_OVERFLOW       82
#define IPMI_SENSOR_UNIT_UNDERRUN               83
#define IPMI_SENSOR_UNIT_COLLISION              84
#define IPMI_SENSOR_UNIT_PACKETS                85
#define IPMI_SENSOR_UNIT_MESSAGES               86
#define IPMI_SENSOR_UNIT_CHARACTERS             87
#define IPMI_SENSOR_UNIT_ERROR                  88
#define IPMI_SENSOR_UNIT_CORRECTABLE_ERROR      89
#define IPMI_SENSOR_UNIT_UNCORRECTABLE_ERROR    90
#define IPMI_SENSOR_UNIT_FATAL_ERROR            91
#define IPMI_SENSOR_UNIT_GRAMS                  92

#define IPMI_SENSOR_UNIT_VALID(__sensor_unit) \
        (((__sensor_unit) >= IPMI_SENSOR_UNIT_UNSPECIFIED \
          && (__sensor_unit) <= IPMI_SENSOR_UNIT_GRAMS) ? 1 : 0)

extern const char *const ipmi_sensor_types[];
extern const char *const ipmi_oem_sensor_type;
extern const char *const ipmi_sensor_units[];
extern const char *const ipmi_sensor_units_abbreviated[];

#ifdef __cplusplus
}
#endif

#endif
