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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>

#include "freeipmi/spec/ipmi-device-types-spec.h"
#include "freeipmi/fiid/fiid.h"

#include "libcommon/ipmi-fiid-util.h"
#include "libcommon/ipmi-trace.h"

#include "freeipmi-portability.h"

/* achu:
 *
 * I acknowledge, this was a borderline call.  Do I want a
 * "ipmi_device_type_to_string" function, or do I fill in empty spots
 * with NULLs.  It's not a huge array (compared to to the IANA list),
 * but it's not tiny either.
 */

const char *const ipmi_device_types[] =
  {
    "reserved",                                                     /* 0x00 */
    "reserved",                                                     /* 0x01 */
    "DS1624 temperature sensor / EEPROM or equivalent",             /* 0x02 */
    "DS1621 temperature sensor or equivalent",                      /* 0x03 */
    "LM75 Temperature Sensor or equivalent",                        /* 0x04 */
    "'Heceta' ASIC or similar",                                     /* 0x05 */
    "reserved",                                                     /* 0x06 */
    "reserved",                                                     /* 0x07 */
    "EEPROM, 24C01 or equivalent",                                  /* 0x08 */
    "EEPROM, 24C02 or equivalent",                                  /* 0x09 */
    "EEPROM, 24C04 or equivalent",                                  /* 0x0A */
    "EEPROM, 24C08 or equivalent",                                  /* 0x0B */
    "EEPROM, 24C16 or equivalent",                                  /* 0x0C */
    "EEPROM, 24C17 or equivalent",                                  /* 0x0D */
    "EEPROM, 24C32 or equivalent",                                  /* 0x0E */
    "EEPROM, 24C64 or equivalent",                                  /* 0x0F */

    "FRU Inventory Device behind management controller",            /* 0x10 */
    "reserved",                                                     /* 0x11 */
    "reserved",                                                     /* 0x12 */
    "reserved",                                                     /* 0x13 */
    "PCF 8570 256 byte RAM or equivalent",                          /* 0x14 */
    "PCF 8573 clock/calendar or equivalent",                        /* 0x15 */
    "PCF 8574a 'i/o port' or equivalent",                           /* 0x16 */
    "PCF 8583 clock/calendar or equivalent",                        /* 0x17 */
    "PCF 8593 clock/calendar or equivalent",                        /* 0x18 */
    "Clock calendar, type not specified",                           /* 0x19 */
    "PCF 8591 AD, D/A Converter or equivalent",                     /* 0x1A */
    "i/o port, specific device not specified",                      /* 0x1B */
    "A/D Converter, specific device not specified",                 /* 0x1C */
    "D/A Converter, specific device not specified",                 /* 0x1D */
    "A/D, D/A Converter, specific device not specified",            /* 0x1E */
    "LCD controller / Driver, specific device not specified",       /* 0x1F */

    "Core Logic (chip set) Device, specific device not specified",  /* 0x20 */
    "LMC6874 Intelligent Battery controller, or equivalent",        /* 0x21 */
    "Intelligent Battery controller, specific device not specified",/* 0x22 */
    "Combo Management ASIC, specific device not specified",         /* 0x23 */
    "Maxim 1617 Temperature Sensor",                                /* 0x24 */
    "reserved",                                                     /* 0x25 */
    "reserved",                                                     /* 0x26 */
    "reserved",                                                     /* 0x27 */
    "reserved",                                                     /* 0x28 */
    "reserved",                                                     /* 0x29 */
    "reserved",                                                     /* 0x2A */
    "reserved",                                                     /* 0x2B */
    "reserved",                                                     /* 0x2C */
    "reserved",                                                     /* 0x2D */
    "reserved",                                                     /* 0x2E */
    "reserved",                                                     /* 0x2F */

    "reserved",                                                     /* 0x30 */
    "reserved",                                                     /* 0x31 */
    "reserved",                                                     /* 0x32 */
    "reserved",                                                     /* 0x33 */
    "reserved",                                                     /* 0x34 */
    "reserved",                                                     /* 0x35 */
    "reserved",                                                     /* 0x36 */
    "reserved",                                                     /* 0x37 */
    "reserved",                                                     /* 0x38 */
    "reserved",                                                     /* 0x39 */
    "reserved",                                                     /* 0x3A */
    "reserved",                                                     /* 0x3B */
    "reserved",                                                     /* 0x3C */
    "reserved",                                                     /* 0x3D */
    "reserved",                                                     /* 0x3E */
    "reserved",                                                     /* 0x3F */

    "reserved",                                                     /* 0x40 */
    "reserved",                                                     /* 0x41 */
    "reserved",                                                     /* 0x42 */
    "reserved",                                                     /* 0x43 */
    "reserved",                                                     /* 0x44 */
    "reserved",                                                     /* 0x45 */
    "reserved",                                                     /* 0x46 */
    "reserved",                                                     /* 0x47 */
    "reserved",                                                     /* 0x48 */
    "reserved",                                                     /* 0x49 */
    "reserved",                                                     /* 0x4A */
    "reserved",                                                     /* 0x4B */
    "reserved",                                                     /* 0x4C */
    "reserved",                                                     /* 0x4D */
    "reserved",                                                     /* 0x4E */
    "reserved",                                                     /* 0x4F */

    "reserved",                                                     /* 0x50 */
    "reserved",                                                     /* 0x51 */
    "reserved",                                                     /* 0x52 */
    "reserved",                                                     /* 0x53 */
    "reserved",                                                     /* 0x54 */
    "reserved",                                                     /* 0x55 */
    "reserved",                                                     /* 0x56 */
    "reserved",                                                     /* 0x57 */
    "reserved",                                                     /* 0x58 */
    "reserved",                                                     /* 0x59 */
    "reserved",                                                     /* 0x5A */
    "reserved",                                                     /* 0x5B */
    "reserved",                                                     /* 0x5C */
    "reserved",                                                     /* 0x5D */
    "reserved",                                                     /* 0x5E */
    "reserved",                                                     /* 0x5F */

    "reserved",                                                     /* 0x60 */
    "reserved",                                                     /* 0x61 */
    "reserved",                                                     /* 0x62 */
    "reserved",                                                     /* 0x63 */
    "reserved",                                                     /* 0x64 */
    "reserved",                                                     /* 0x65 */
    "reserved",                                                     /* 0x66 */
    "reserved",                                                     /* 0x67 */
    "reserved",                                                     /* 0x68 */
    "reserved",                                                     /* 0x69 */
    "reserved",                                                     /* 0x6A */
    "reserved",                                                     /* 0x6B */
    "reserved",                                                     /* 0x6C */
    "reserved",                                                     /* 0x6D */
    "reserved",                                                     /* 0x6E */
    "reserved",                                                     /* 0x6F */

    "reserved",                                                     /* 0x70 */
    "reserved",                                                     /* 0x71 */
    "reserved",                                                     /* 0x72 */
    "reserved",                                                     /* 0x73 */
    "reserved",                                                     /* 0x74 */
    "reserved",                                                     /* 0x75 */
    "reserved",                                                     /* 0x76 */
    "reserved",                                                     /* 0x77 */
    "reserved",                                                     /* 0x78 */
    "reserved",                                                     /* 0x79 */
    "reserved",                                                     /* 0x7A */
    "reserved",                                                     /* 0x7B */
    "reserved",                                                     /* 0x7C */
    "reserved",                                                     /* 0x7D */
    "reserved",                                                     /* 0x7E */
    "reserved",                                                     /* 0x7F */

    "reserved",                                                     /* 0x80 */
    "reserved",                                                     /* 0x81 */
    "reserved",                                                     /* 0x82 */
    "reserved",                                                     /* 0x83 */
    "reserved",                                                     /* 0x84 */
    "reserved",                                                     /* 0x85 */
    "reserved",                                                     /* 0x86 */
    "reserved",                                                     /* 0x87 */
    "reserved",                                                     /* 0x88 */
    "reserved",                                                     /* 0x89 */
    "reserved",                                                     /* 0x8A */
    "reserved",                                                     /* 0x8B */
    "reserved",                                                     /* 0x8C */
    "reserved",                                                     /* 0x8D */
    "reserved",                                                     /* 0x8E */
    "reserved",                                                     /* 0x8F */

    "reserved",                                                     /* 0x90 */
    "reserved",                                                     /* 0x91 */
    "reserved",                                                     /* 0x92 */
    "reserved",                                                     /* 0x93 */
    "reserved",                                                     /* 0x94 */
    "reserved",                                                     /* 0x95 */
    "reserved",                                                     /* 0x96 */
    "reserved",                                                     /* 0x97 */
    "reserved",                                                     /* 0x98 */
    "reserved",                                                     /* 0x99 */
    "reserved",                                                     /* 0x9A */
    "reserved",                                                     /* 0x9B */
    "reserved",                                                     /* 0x9C */
    "reserved",                                                     /* 0x9D */
    "reserved",                                                     /* 0x9E */
    "reserved",                                                     /* 0x9F */

    "reserved",                                                     /* 0xA0 */
    "reserved",                                                     /* 0xA1 */
    "reserved",                                                     /* 0xA2 */
    "reserved",                                                     /* 0xA3 */
    "reserved",                                                     /* 0xA4 */
    "reserved",                                                     /* 0xA5 */
    "reserved",                                                     /* 0xA6 */
    "reserved",                                                     /* 0xA7 */
    "reserved",                                                     /* 0xA8 */
    "reserved",                                                     /* 0xA9 */
    "reserved",                                                     /* 0xAA */
    "reserved",                                                     /* 0xAB */
    "reserved",                                                     /* 0xAC */
    "reserved",                                                     /* 0xAD */
    "reserved",                                                     /* 0xAE */
    "reserved",                                                     /* 0xAF */

    "reserved",                                                     /* 0xB0 */
    "reserved",                                                     /* 0xB1 */
    "reserved",                                                     /* 0xB2 */
    "reserved",                                                     /* 0xB3 */
    "reserved",                                                     /* 0xB4 */
    "reserved",                                                     /* 0xB5 */
    "reserved",                                                     /* 0xB6 */
    "reserved",                                                     /* 0xB7 */
    "reserved",                                                     /* 0xB8 */
    "reserved",                                                     /* 0xB9 */
    "reserved",                                                     /* 0xBA */
    "reserved",                                                     /* 0xBB */
    "reserved",                                                     /* 0xBC */
    "reserved",                                                     /* 0xBD */
    "reserved",                                                     /* 0xBE */
    "Other / unspecified device",                                   /* 0xBF */
    NULL
  };

const char *const ipmi_oem_device_type = "OEM specified device";

const char * const ipmi_device_type_modifier_ds1624_temperature_sensor_eeprom_or_equivalent[] =
  {
    "unspecified",
    NULL
  };
unsigned int ipmi_device_type_modifier_ds1624_temperature_sensor_eeprom_or_equivalent_max_index = 0x00;

const char * const ipmi_device_type_modifier_ds1621_temperature_sensor_eeprom_or_equivalent[] =
  {
    "unspecified",
    NULL
  };
unsigned int ipmi_device_type_modifier_ds1621_temperature_sensor_eeprom_or_equivalent_max_index = 0x00;

const char * const ipmi_device_type_modifier_lm75_temperature_sensor_eeprom_or_equivalent[] =
  {
    "unspecified",
    NULL
  };
unsigned int ipmi_device_type_modifier_lm75_temperature_sensor_eeprom_or_equivalent_max_index = 0x00;

const char * const ipmi_device_type_modifier_heceta_asic_or_similar[] =
  {
    "Heceta 1 e.g. LM78",
    "Heceta 2 e.g. LM79",
    "LM80",
    "Heceta 3 e.g. LM81/ DM9240 / DS1760",
    "Heceta 4",
    "Heceta 5",
    NULL
  };
unsigned int ipmi_device_type_modifier_heceta_asic_or_similar_max_index = 0x05;

const char * const ipmi_device_type_modifier_eeprom_24c01_or_equivalent[] =
  {
    "unspecified",
    "DIMM Memory ID",
    "IPMI FRU Inventory",
    "System Processor Cartridge FRU / PIROM",
    NULL
  };
unsigned int ipmi_device_type_modifier_eeprom_24c01_or_equivalent_max_index = 0x03;

const char * const ipmi_device_type_modifier_eeprom_24c02_or_equivalent[] =
  {
    "unspecified",
    "DIMM Memory ID",
    "IPMI FRU Inventory",
    "System Processor Cartridge FRU / PIROM",
    NULL
  };
unsigned int ipmi_device_type_modifier_eeprom_24c02_or_equivalent_max_index = 0x03;

const char * const ipmi_device_type_modifier_eeprom_24c04_or_equivalent[] =
  {
    "unspecified",
    "DIMM Memory ID",
    "IPMI FRU Inventory",
    "System Processor Cartridge FRU / PIROM",
    NULL
  };
unsigned int ipmi_device_type_modifier_eeprom_24c04_or_equivalent_max_index = 0x03;

const char * const ipmi_device_type_modifier_eeprom_24c08_or_equivalent[] =
  {
    "unspecified",
    "DIMM Memory ID",
    "IPMI FRU Inventory",
    "System Processor Cartridge FRU / PIROM",
    NULL
  };
unsigned int ipmi_device_type_modifier_eeprom_24c08_or_equivalent_max_index = 0x03;

const char * const ipmi_device_type_modifier_eeprom_24c16_or_equivalent[] =
  {
    "unspecified",
    "DIMM Memory ID",
    "IPMI FRU Inventory",
    "System Processor Cartridge FRU / PIROM",
    NULL
  };
unsigned int ipmi_device_type_modifier_eeprom_24c16_or_equivalent_max_index = 0x03;

const char * const ipmi_device_type_modifier_eeprom_24c17_or_equivalent[] =
  {
    "unspecified",
    "DIMM Memory ID",
    "IPMI FRU Inventory",
    "System Processor Cartridge FRU / PIROM",
    NULL
  };
unsigned int ipmi_device_type_modifier_eeprom_24c17_or_equivalent_max_index = 0x03;

const char * const ipmi_device_type_modifier_eeprom_24c32_or_equivalent[] =
  {
    "unspecified",
    "DIMM Memory ID",
    "IPMI FRU Inventory",
    "System Processor Cartridge FRU / PIROM",
    NULL
  };
unsigned int ipmi_device_type_modifier_eeprom_24c32_or_equivalent_max_index = 0x03;

const char * const ipmi_device_type_modifier_eeprom_24c64_or_equivalent[] =
  {
    "unspecified",
    "DIMM Memory ID",
    "IPMI FRU Inventory",
    "System Processor Cartridge FRU / PIROM",
    NULL
  };
unsigned int ipmi_device_type_modifier_eeprom_24c64_or_equivalent_max_index = 0x03;

/* achu: not a typo, 00h and 0x02 are same.  00h for backwards compatability, see spec */
const char * const ipmi_device_type_modifier_fru_inventory_device_behind_management_controller[] =
  {
    "IPMI FRU Inventory",
    "DIMM Memory ID",
    "IPMI FRU Inventory",
    "System Processor Cartridge FRU / PIROM",
    NULL
  };
unsigned int ipmi_device_type_modifier_fru_inventory_device_behind_management_controller_max_index = 0x03;

const char * const ipmi_device_type_modifier_pcf_8570_256_byte_ram_or_equivalent[] =
  {
    "unspecified",
    NULL
  };
unsigned int ipmi_device_type_modifier_pcf_8570_256_byte_ram_or_equivalent_max_index = 0x00;

const char * const ipmi_device_type_modifier_pcf_8573_clock_calendar_or_equivalent[] =
  {
    "unspecified",
    NULL
  };
unsigned int ipmi_device_type_modifier_pcf_8573_clock_calendar_or_equivalent_max_index = 0x00;

const char * const ipmi_device_type_modifier_pcf_8574a_io_port_or_equivalent[] =
  {
    "unspecified",
    NULL
  };
unsigned int ipmi_device_type_modifier_pcf_8574a_io_port_or_equivalent_max_index = 0x00;

const char * const ipmi_device_type_modifier_pcf_8583_clock_calendar_or_equivalent[] =
  {
    "unspecified",
    NULL
  };
unsigned int ipmi_device_type_modifier_pcf_8583_clock_calendar_or_equivalent_max_index = 0x00;

const char * const ipmi_device_type_modifier_pcf_8593_clock_calendar_or_equivalent[] =
  {
    "unspecified",
    NULL
  };
unsigned int ipmi_device_type_modifier_pcf_8593_clock_calendar_or_equivalent_max_index = 0x00;

const char * const ipmi_device_type_modifier_clock_calendar_type_not_specified[] =
  {
    "unspecified",
    NULL
  };
unsigned int ipmi_device_type_modifier_clock_calendar_type_not_specified_max_index = 0x00;

const char * const ipmi_device_type_modifier_pcf_8591_ad_da_converter_or_equivalent[] =
  {
    "unspecified",
    NULL
  };
unsigned int ipmi_device_type_modifier_pcf_8591_ad_da_converter_or_equivalent_max_index = 0x00;

const char * const ipmi_device_type_modifier_io_port_specific_device_not_specified[] =
  {
    "unspecified",
    NULL
  };
unsigned int ipmi_device_type_modifier_io_port_specific_device_not_specified_max_index = 0x00;

const char * const ipmi_device_type_modifier_ad_converter_specific_device_not_specified[] =
  {
    "unspecified",
    NULL
  };
unsigned int ipmi_device_type_modifier_ad_converter_specific_device_not_specified_max_index = 0x00;

const char * const ipmi_device_type_modifier_da_converter_specific_device_not_specified[] =
  {
    "unspecified",
    NULL
  };
unsigned int ipmi_device_type_modifier_da_converter_specific_device_not_specified_max_index = 0x00;

const char * const ipmi_device_type_modifier_ad_da_converter_specific_device_not_specified[] =
  {
    "unspecified",
    NULL
  };
unsigned int ipmi_device_type_modifier_ad_da_converter_specific_device_not_specified_max_index = 0x00;

const char * const ipmi_device_type_modifier_lcd_controller_driver_specific_device_not_specified[] =
  {
    "unspecified",
    NULL
  };
unsigned int ipmi_device_type_modifier_lcd_controller_driver_specific_device_not_specified_max_index = 0x00;

const char * const ipmi_device_type_modifier_core_logic_device_specific_device_not_specified[] =
  {
    "unspecified",
    NULL
  };
unsigned int ipmi_device_type_modifier_core_logic_device_specific_device_not_specified_max_index = 0x00;

const char * const ipmi_device_type_modifier_lmc6874_intelligent_battery_controller_or_equivalent[] =
  {
    "unspecified",
    NULL
  };
unsigned int ipmi_device_type_modifier_lmc6874_intelligent_battery_controller_or_equivalent_max_index = 0x00;

const char * const ipmi_device_type_modifier_intelligent_battery_controller_specific_device_not_specified[] =
  {
    "unspecified",
    NULL
  };
unsigned int ipmi_device_type_modifier_intelligent_battery_controller_specific_device_not_specified_max_index = 0x00;

const char * const ipmi_device_type_modifier_combo_management_asic_specific_device_not_specified[] =
  {
    "unspecified",
    NULL
  };
unsigned int ipmi_device_type_modifier_combo_management_asic_specific_device_not_specified_max_index = 0x00;

const char * const ipmi_device_type_modifier_maxim_1617_temperature_sensor[] =
  {
    "unspecified",
    NULL
  };
unsigned int ipmi_device_type_modifier_maxim_1617_temperature_sensor_max_index = 0x00;

const char * const ipmi_device_type_modifier_other_unspecified_device[] =
  {
    "unspecified",
    NULL
  };
unsigned int ipmi_device_type_modifier_other_unspecified_device_max_index = 0x00;
