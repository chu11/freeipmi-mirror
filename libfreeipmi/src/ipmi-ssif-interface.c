/* 
   ipmi-ssif-interface.c: IPMI - SMBus System Interface - SMS Interface

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

#include "freeipmi.h"

#if 0
/* Typedefs */
typedef unsigned char BYTE;
typedef unsigned int  WORD;
typedef unsigned long DWORD;
typedef unsigned int  UINT;
typedef unsigned long ULONG;
#endif

extern int errno;

static inline int32_t
ipmi_i2c_smbus_access (int file, char read_write, uint8_t command, int size, 
		       union ipmi_i2c_smbus_data *data)
{
	struct ipmi_i2c_smbus_ioctl_data args;

	args.read_write = read_write;
	args.command = command;
	args.size = size;
	args.data = data;
	return ioctl (file, IPMI_I2C_SMBUS, &args);
}

/* ipmi_i2c_smbus_read_block_data is based on
linux/i2c-dev.h:i2c_smbus_read_block_data. It is duplicated here to
reduce dependencies. -- Anand Babu */
static inline int32_t
ipmi_i2c_smbus_read_block_data (int file, uint8_t command, uint8_t *values)
{
	union ipmi_i2c_smbus_data data;
	int i;
	if (ipmi_i2c_smbus_access (file, IPMI_I2C_SMBUS_READ, command,
				   IPMI_I2C_SMBUS_BLOCK_DATA, &data))
		return -1;
	else {
		for (i = 1; i <= data.block[0]; i++)
			values[i-1] = data.block[i];
		return data.block[0];
	}
}

/* ipmi_i2c_smbus_write_block_data is based on
linux/i2c-dev.h:i2c_smbus_write_block_data. It is duplicated here to
reduce dependencies. -- Anand Babu */
static inline int32_t
ipmi_i2c_smbus_write_block_data (int file, uint8_t command, uint8_t length, uint8_t *values)
{
	union ipmi_i2c_smbus_data data;
	int i;
	if (length > 32)
		length = 32;
	for (i = 1; i <= length; i++)
		data.block[i] = values[i-1];
	data.block[0] = length;
	return ipmi_i2c_smbus_access (file, IPMI_I2C_SMBUS_WRITE, command,
				      IPMI_I2C_SMBUS_BLOCK_DATA, &data);
}

int
ipmi_ssif_io_init (char *i2c_device, uint8_t ipmb_addr, int *i2c_fd)
{
  int fd;

  if (!(i2c_device && i2c_fd))
    {
      errno = EINVAL;
      return (-1);
    }

   if ((fd = open (i2c_device, O_RDWR)) < 0)
     return (-1);

   /* zresearch webserver ipmb_addr: 0x341A */
   if (ioctl (fd, IPMI_I2C_SLAVE, ipmb_addr) < 0)
     return (-1);

   *i2c_fd = fd;
   return (0);
}

size_t
ipmi_ssif_write (int i2c_fd, char *buf, size_t len)
{ 
  int bytes_written;

  bytes_written = ipmi_i2c_smbus_write_block_data (i2c_fd, IPMI_SSIF_SMB_IPMI_REQUEST, len, (uint8_t *)buf);
  if (bytes_written == 0)
    return (-1);
  else
    return (bytes_written);
}

size_t
ipmi_ssif_read (int i2c_fd, char *buf, size_t *len)
{ 
  int bytes_read;
  
  bytes_read = ipmi_i2c_smbus_read_block_data (i2c_fd, IPMI_SSIF_SMB_IPMI_RESPONSE, (uint8_t *)buf);
  if (bytes_read == 0)
    return (-1);
  else
    return (bytes_read);
}

int
ipmi_ssif_exit (int i2c_fd)
{
  return (close (i2c_fd));
}

