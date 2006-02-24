/* 
   ipmi-ssif-interface.h: IPMI - SMBus System Interface - SMS Interface

   Copyright (C) 2005 FreeIPMI Core Team

   Based on ipmitool.c provided by Amitoj Singh <amitoj@fnal.gov> and 
   Don Holmgren <djholm@fnal.gov>

   Under GNU/Linux, requires i2c-dev, i2c-i801, i2c-core drivers version >= 2.8.7

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

#ifndef IPMI_SSIF_INTERFACE_H
#define IPMI_SSIF_INTERFACE_H

#include <stdint.h>

#define IPMI_DEFAULT_I2C_DEVICE    "/dev/i2c-0"
#define IPMI_DEFAULT_IPMB_ADDRESS  0x42
#define IPMI_SSIF_SMBUS_SLAVE_ADDR 0x20
/* function error codes */
#define IPMI_SSIF_SUCCESS         0x00
#define IPMI_SSIF_UNDEF_ERROR     0xFFFF

#define IPMI_SSIF_UNKNOWN_INTERFACE       0x1000
#define IPMI_SSIF_ISA_MESSAGE_OVERFLOW    0x1100
#define IPMI_SSIF_BAD_ISA_STATE           0x1200
#define IPMI_SSIF_BMC_WRITE_STATE_FAIL    0x1300
#define IPMI_SSIF_BMC_READ_STATE_FAIL     0x1400
#define IPMI_SSIF_SMS_READY_TIMEOUT       0x1500
#define IPMI_SSIF_ERROR_READING_SMS       0x1600
#define IPMI_SSIF_PACKET_NUMBER_MISMATCH  0x1700
#define IPMI_SSIF_PACKET_SIZE_MISMATCH    0x1800
#define IPMI_SSIF_I2C_RETRY_ERROR         0x1900
#define IPMI_SSIF_SMS_SEND_ERROR_ON_FLUSH 0x2000
#define IPMI_SSIF_ISA_TIMEOUT_IN_IBF      0x2100
#define IPMI_SSIF_ISA_TIMEOUT_IN_OBF      0x2200

/* Response Packet Offsets */
#define IPMI_SSIF_RSP_OFFSET_COMPCODE 0x03
 
/* SMBus */
#define IPMI_SSIF_SMB_IPMI_REQUEST    2
#define IPMI_SSIF_SMB_IPMI_RESPONSE   3

/* this is for i2c-dev.c	*/
#define IPMI_I2C_SLAVE       0x0703  /* Change slave address                 */
			             /* Attn.: Slave address is 7 or 10 bits */
#define IPMI_I2C_SLAVE_FORCE 0x0706  /* Change slave address                 */
                                     /* Attn.: Slave address is 7 or 10 bits */
				     /* This changes the address, even if it */
				     /* is already taken!                    */

#define IPMI_I2C_SMBUS       0x0720  /* SMBus-level access */

/* SMBus transaction types (size parameter in the above functions) 
   Note: these no longer correspond to the (arbitrary) PIIX4 internal codes! */
#define IPMI_I2C_SMBUS_QUICK                0
#define IPMI_I2C_SMBUS_BYTE                 1
#define IPMI_I2C_SMBUS_BYTE_DATA            2 
#define IPMI_I2C_SMBUS_WORD_DATA            3
#define IPMI_I2C_SMBUS_PROC_CALL            4
#define IPMI_I2C_SMBUS_BLOCK_DATA           5
#define IPMI_I2C_SMBUS_I2C_BLOCK_DATA       6
#define IPMI_I2C_SMBUS_BLOCK_PROC_CALL      7     /* SMBus 2.0 */
#define IPMI_I2C_SMBUS_BLOCK_DATA_PEC       8     /* SMBus 2.0 */
#define IPMI_I2C_SMBUS_PROC_CALL_PEC        9     /* SMBus 2.0 */
#define IPMI_I2C_SMBUS_BLOCK_PROC_CALL_PEC  10    /* SMBus 2.0 */
#define IPMI_I2C_SMBUS_WORD_DATA_PEC        11    /* SMBus 2.0 */

/* 
 * Data for SMBus Messages 
 */
#define IPMI_I2C_SMBUS_BLOCK_MAX	32	/* As specified in SMBus standard */	
#define IPMI_I2C_SMBUS_I2C_BLOCK_MAX	32	/* Not specified but we use same structure */

/* smbus_access read or write markers */
#define IPMI_I2C_SMBUS_READ	1
#define IPMI_I2C_SMBUS_WRITE	0


union ipmi_i2c_smbus_data {
  uint8_t byte;
  uint16_t word;
  uint8_t block[IPMI_I2C_SMBUS_BLOCK_MAX + 3]; /* block[0] is used for length */
                          /* one more for read length in block process call */
	                                            /* and one more for PEC */
};

/* Note: 10-bit addresses are NOT supported! */
/* This is the structure as used in the I2C_SMBUS ioctl call */
struct ipmi_i2c_smbus_ioctl_data {
	char read_write;
	uint8_t command;
	int size;
	union ipmi_i2c_smbus_data *data;
};

#define IPMI_SSIF_CTX_ERR_SUCCESS         0
#define IPMI_SSIF_CTX_ERR_NULL            1
#define IPMI_SSIF_CTX_ERR_INVALID         2
#define IPMI_SSIF_CTX_ERR_PARAMETERS      3
#define IPMI_SSIF_CTX_ERR_PERMISSION      4
#define IPMI_SSIF_CTX_ERR_IO_PARAMETERS   5
#define IPMI_SSIF_CTX_ERR_IO_INIT         6
#define IPMI_SSIF_CTX_ERR_OVERFLOW        7
#define IPMI_SSIF_CTX_ERR_BUSY            8
#define IPMI_SSIF_CTX_ERR_OUTMEM          9
#define IPMI_SSIF_CTX_ERR_INTERNAL        10
#define IPMI_SSIF_CTX_ERR_ERRNUMRANGE     11

#define IPMI_SSIF_MODE_BLOCKING    0
#define IPMI_SSIF_MODE_NONBLOCKING 1
#define IPMI_SSIF_MODE_DEFAULT     IPMI_SSIF_MODE_BLOCKING

typedef struct ipmi_ssif_ctx *ipmi_ssif_ctx_t;

/* Notes:
 *
 * IPMBAddress - slave address of the BMC on the SMBus (usually 0x42)
 *
 */
ipmi_ssif_ctx_t ipmi_ssif_ctx_create(void);
int8_t ipmi_ssif_ctx_destroy(ipmi_ssif_ctx_t ctx);
int32_t ipmi_ssif_ctx_errnum(ipmi_ssif_ctx_t ctx);
char *ipmi_ssif_ctx_strerror(int32_t errnum);

int8_t ipmi_ssif_ctx_get_i2c_device(ipmi_ssif_ctx_t ctx, char **i2c_device);
int8_t ipmi_ssif_ctx_get_ipmb_addr(ipmi_ssif_ctx_t ctx, uint8_t *ipmb_addr);
int8_t ipmi_ssif_ctx_get_mode(ipmi_ssif_ctx_t ctx, uint8_t *mode);

int8_t ipmi_ssif_ctx_set_i2c_device(ipmi_ssif_ctx_t ctx, char *i2c_device);
int8_t ipmi_ssif_ctx_set_ipmb_addr(ipmi_ssif_ctx_t ctx, uint8_t ipmb_addr);
int8_t ipmi_ssif_ctx_set_mode(ipmi_ssif_ctx_t ctx, uint8_t mode);

int8_t ipmi_ssif_ctx_io_init(ipmi_ssif_ctx_t ctx);

int32_t ipmi_ssif_write (ipmi_ssif_ctx_t ctx,
			 uint8_t *buf,
			 uint32_t buf_len);

int32_t ipmi_ssif_read (ipmi_ssif_ctx_t ctx,
			uint8_t* buf,
			uint32_t buf_len);

#endif /* IPMI_SSIF_INTERFACE_H */
