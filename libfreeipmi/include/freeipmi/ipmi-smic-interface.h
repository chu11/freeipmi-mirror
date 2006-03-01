/* 
   ipmi-smic-interface.h - IPMI SMIC SMS Interface

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
#ifndef _IPMI_SMIC_INTERFACE_H
#define	_IPMI_SMIC_INTERFACE_H	1

#ifdef __cplusplus
extern "C" {
#endif

/* Config items */
#define IPMI_SMIC_SLEEP_USECS 500

/* SMIC I/O Port */
#define IPMI_SMIC_SMS_IO_BASE_DEFAULT    0x0CA9
#define IPMI_SMIC_SMS_IO_BASE_CDC1620    0x0CA9
#define IPMI_SMIC_SMS_IO_BASE_CDC9416    0x0CA9
#define IPMI_SMIC_SMS_IO_BASE_SR870BN4   0x08A9 
#define IPMI_SMIC_SMS_IO_BASE_CDC6440    0x08A9

/* IPMI KCS SMS Interface Registers */
#define IPMI_SMIC_REG_DATA(sms_io_base)    (sms_io_base)
#define IPMI_SMIC_REG_CONTROL(sms_io_base) (sms_io_base+1)
#define IPMI_SMIC_REG_STATUS(sms_io_base)  (sms_io_base+1)
#define IPMI_SMIC_REG_FLAGS(sms_io_base)   (sms_io_base+2)

/* Flag register definitions */
#define IPMI_SMIC_RX_DATA_RDY       0x80
#define IPMI_SMIC_TX_DATA_RDY       0x40
#define IPMI_SMIC_SMI               0x10
#define IPMI_SMIC_EVT_ATN           0x08
#define IPMI_SMIC_SMS_ATN           0x04
#define IPMI_SMIC_BUSY              0x01

/* SMS Stream control codes */
#define IPMI_SMIC_CC_SMS_GET_STATUS  0x40
#define IPMI_SMIC_CC_SMS_WR_START    0x41
#define IPMI_SMIC_CC_SMS_WR_NEXT     0x42
#define IPMI_SMIC_CC_SMS_WR_END      0x43
#define IPMI_SMIC_CC_SMS_RD_START    0x44
#define IPMI_SMIC_CC_SMS_RD_NEXT     0x45
#define IPMI_SMIC_CC_SMS_RD_END      0x46

/* SMS Stream status codes */
#define IPMI_SMIC_SC_SMS_RDY         0xc0
#define IPMI_SMIC_SC_SMS_WR_START    0xc1
#define IPMI_SMIC_SC_SMS_WR_NEXT     0xc2
#define IPMI_SMIC_SC_SMS_WR_END      0xc3
#define IPMI_SMIC_SC_SMS_RD_START    0xc4
#define IPMI_SMIC_SC_SMS_RD_NEXT     0xc5
#define IPMI_SMIC_SC_SMS_RD_END      0xc6

#ifdef __cplusplus
}
#endif

#endif /* ipmi-smic-interface.h */
