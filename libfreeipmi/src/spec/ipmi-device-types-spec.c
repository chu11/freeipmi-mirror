/*
  Copyright (C) 2003-2009 FreeIPMI Core Team

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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>

#include "freeipmi/spec/ipmi-device-types-spec.h"

#include "freeipmi-portability.h"

const char *const ipmi_device_types[] =
  {
    "reserved",                 /* 0x00 */
    "reserved",                 /* 0x01 */
    "DS1624 temperature sensor / EEPROM or equivalent" /* 0x02 */
    "DS1621 temperature sensor or equivalent" /* 0x03 */
    "LM75 Temperature Sensor or equivalent" /* 0x04 */
    "'Heceta' ASIC or similar" /* 0x05 */
    "EEPROM, 24C01 or equivalent" /* 0x08 */
    "EEPROM, 24C02 or equivalent" /* 0x09 */
    "EEPROM, 24C04 or equivalent" /* 0x0a */
    "EEPROM, 24C08 or equivalent" /* 0x0b */
    "EEPROM, 24C16 or equivalent" /* 0x0c */
    "EEPROM, 24C17 or equivalent" /* 0x0d */
    "EEPROM, 24C32 or equivalent" /* 0x0e */
    "EEPROM, 24C64 or equivalent" /* 0x0f */
    "FRU Inventory Device behind management controller" /* 0x10 */
    "reserved",                 /* 0x11 */
    "reserved",                 /* 0x12 */
    "reserved",                 /* 0x13 */
    "PCF 8570 256 byte RAM or equivalent" /* 0x14 */
    "PCF 8573 clock/calendar or equivalent" /* 0x15 */
    "PCF 8574a 'i/o port' or equivalent" /* 0x16 */
    "PCF 8583 clock/calendar or equivalent" /* 0x17 */
    "PCF 8593 clock/calendar or equivalent" /* 0x18 */
    "Clock calendar, type not specified" /* 0x19 */
    "PCF 8591 AD, D/A Converter or equivalent" /* 0x1a */
    "i/o port, specific device not specified" /* 0x1b */
    "A/D Converter, specific device not specified" /* 0x1c */
    "D/A Converter, specific device not specified" /* 0x1d */
    "A/D, D/A Converter, specific device not specified" /* 0x1e */
    "LCD controller / Driver, specific device not specified" /* 0x1f */
    "Core Logic (chip set) Device, specific device not specified" /* 0x20 */
    "LMC6874 Intelligent Battery controller, or equivalent" /* 0x21 */
    "Intelligent Battery controller, specific device not specified" /* 0x22 */
    "Combo Management ASIC, specific device not specified" /* 0x23 */
    "Maxim 1617 Temperature Sensor" /* 0x24 */
    "Other / unspecified device" /* 0xbf */
    NULL
  };
