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
   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  

*/

#ifndef _IPMI_SSIF_INTERFACE_H
#define _IPMI_SSIF_INTERFACE_H

#define IPMI_DEFAULT_I2C_DEVICE        "/dev/i2c-0"
#define IPMI_DEFAULT_SSIF_IPMB_ADDR    0x42

#define IPMI_SSIF_SINGLE_PART_WRITE_SMBUS_CMD          0x02
#define IPMI_SSIF_MULTI_PART_WRITE_START_SMBUS_CMD     0x06
#define IPMI_SSIF_MULTI_PART_WRITE_MIDDLE_SMBUS_CMD    0x07
#define IPMI_SSIF_MULTI_PART_WRITE_END_SMBUS_CMD       0x07

#define IPMI_SSIF_SINGLE_PART_READ_SMBUS_CMD           0x03
#define IPMI_SSIF_MULTI_PART_READ_START_SMBUS_CMD      0x03
#define IPMI_SSIF_MULTI_PART_READ_MIDDLE_SMBUS_CMD     0x09
#define IPMI_SSIF_MULTI_PART_READ_END_SMBUS_CMD        0x09
#define IPMI_SSIF_MULTI_PART_READ_RETRY_SMBUS_CMD      0x0A

#define IPMI_SSIF_MULTI_PART_READ_START_SIZE        30
#define IPMI_SSIF_MULTI_PART_READ_START_PATTERN1    0x0
#define IPMI_SSIF_MULTI_PART_READ_START_PATTERN2    0x1
#define IPMI_SSIF_MULTI_PART_READ_END_PATTERN       0xFF

/* START: copied from <linux/i2c.h> and <linux/i2c-dev.h>, */
/*        and prefixed IPMI.                               */
#define IPMI_I2C_SLAVE               0x0703
#define IPMI_I2C_SMBUS               0x0720
#define IPMI_I2C_SMBUS_BLOCK_DATA    5
#define IPMI_I2C_SMBUS_BLOCK_MAX     32
#define IPMI_I2C_SMBUS_READ          1
#define IPMI_I2C_SMBUS_WRITE         0

union ipmi_i2c_smbus_data
{
  uint8_t  byte;
  uint16_t word;
  uint8_t  block[IPMI_I2C_SMBUS_BLOCK_MAX + 3];
};

struct ipmi_i2c_smbus_ioctl_data
{
  uint8_t  read_write;
  uint8_t  command;
  uint32_t size;
  union ipmi_i2c_smbus_data *data;
};
/* END: copied from <linux/i2c.h> and <linux/i2c-dev.h>, */
/*      and prefixed IPMI.                               */

int ipmi_ssif_io_init (char *i2c_device, 
		       uint8_t ipmb_addr, 
		       int *i2c_fd);

ssize_t ipmi_ssif_write (int dev_fd, 
			 uint8_t *buf, 
			 size_t buf_len);

ssize_t ipmi_ssif_read (int dev_fd, 
			uint8_t *buf, 
			size_t buf_len);

int ipmi_ssif_close (int i2c_fd);

int ipmi_ssif_cmd2 (ipmi_device_t *dev, 
		    fiid_obj_t obj_cmd_rq, 
		    fiid_template_t tmpl_cmd_rq, 
		    fiid_obj_t obj_cmd_rs, 
		    fiid_template_t tmpl_cmd_rs);

int ipmi_ssif_cmd_raw2 (ipmi_device_t *dev, 
			uint8_t *buf_rq, 
			size_t buf_rq_len, 
			uint8_t *buf_rs, 
			size_t *buf_rs_len);

#endif

