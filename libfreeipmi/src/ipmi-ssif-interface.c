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

static inline int 
ipmi_i2c_smbus_access (int dev_fd, 
		       char read_write, 
		       uint8_t command, 
		       union ipmi_i2c_smbus_data *data)
{
  struct ipmi_i2c_smbus_ioctl_data args;
  
  args.read_write = read_write;
  args.command = command;
  args.size = IPMI_I2C_SMBUS_BLOCK_DATA;
  args.data = data;
  
  return (ioctl (dev_fd, IPMI_I2C_SMBUS, &args));
}

static inline ssize_t 
ipmi_ssif_single_part_write (int dev_fd, 
			     uint8_t *buf, 
			     size_t buf_len)
{

  union ipmi_i2c_smbus_data data;
  int i;
  
  data.block[0] = buf_len;
  for (i = 0; i < buf_len; i++)
    {
      data.block[i + 1] = buf[i];
    }
  
  return (ipmi_i2c_smbus_access (dev_fd, 
				 IPMI_I2C_SMBUS_WRITE, 
				 IPMI_SSIF_SINGLE_PART_WRITE_SMBUS_CMD, 
				 &data));
}

static inline ssize_t 
ipmi_ssif_multi_part_write (int dev_fd, 
			    uint8_t *buf, 
			    size_t buf_len)
{
  union ipmi_i2c_smbus_data data;
  int middle_parts;
  int i;
  int mpart;
  int index;
  
  if (buf_len % IPMI_I2C_SMBUS_BLOCK_MAX == 0)
    {
      fprintf (stderr, "%s:%s(): "
	       "PECULIAR IPMI COMMAND: As of this writing, "
	       "there are no standard IPMI messages to the "
	       "BMC that are exact multiples of %d.  This "
	       "command can be OEM/group network functions "
	       "(network function codes 2Ch:3Fh) in your "
	       "BMC implementation.  Please report to "
	       "FreeIPMI mailing list <freeipmi-devel@gnu.org>\n", 
	       __FILE__, __PRETTY_FUNCTION__, 
	       IPMI_I2C_SMBUS_BLOCK_MAX);
      return (-1);
    }
  
  middle_parts = (buf_len / IPMI_I2C_SMBUS_BLOCK_MAX) - 1;
  
  data.block[0] = IPMI_I2C_SMBUS_BLOCK_MAX;
  for (i = 0; i < IPMI_I2C_SMBUS_BLOCK_MAX; i++)
    {
      data.block[i + 1] = buf[i];
    }
  if (ipmi_i2c_smbus_access (dev_fd, 
			     IPMI_I2C_SMBUS_WRITE, 
			     IPMI_SSIF_MULTI_PART_WRITE_START_SMBUS_CMD, 
			     &data) == -1)
    {
      return (-1);
    }
  
  for (mpart = 1; mpart <= middle_parts; mpart++)
    {
      index = mpart * IPMI_I2C_SMBUS_BLOCK_MAX;
      data.block[0] = IPMI_I2C_SMBUS_BLOCK_MAX;
      for (i = 0; i < IPMI_I2C_SMBUS_BLOCK_MAX; i++)
	{
	  data.block[i + 1] = buf[index + i];
	}
      if (ipmi_i2c_smbus_access (dev_fd, 
				 IPMI_I2C_SMBUS_WRITE, 
				 IPMI_SSIF_MULTI_PART_WRITE_MIDDLE_SMBUS_CMD, 
				 &data) == -1)
	{
	  return (-1);
	}
    }
  
  index = (middle_parts + 1) * IPMI_I2C_SMBUS_BLOCK_MAX;
  data.block[0] = buf_len % IPMI_I2C_SMBUS_BLOCK_MAX;
  for (i = 0; i < data.block[0]; i++)
    {
      data.block[i + 1] = buf[index + i];
    }
  return (ipmi_i2c_smbus_access (dev_fd, 
				 IPMI_I2C_SMBUS_WRITE, 
				 IPMI_SSIF_MULTI_PART_WRITE_END_SMBUS_CMD, 
				 &data));
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
  
  if (ioctl (fd, IPMI_I2C_SLAVE, ipmb_addr) < 0)
    return (-1);
  
  *i2c_fd = fd;
  return (0);
}

ssize_t 
ipmi_ssif_write (int dev_fd, 
		 uint8_t *buf, 
		 size_t buf_len)
{
  if (buf == NULL || buf_len <= 0)
    {
      errno = EINVAL;
      return (-1);
    }
  
  if (buf_len <= IPMI_I2C_SMBUS_BLOCK_MAX)
    {
      return (ipmi_ssif_single_part_write (dev_fd, 
					   buf, 
					   buf_len));
    }
  else 
    {
      return (ipmi_ssif_multi_part_write (dev_fd, 
					  buf, 
					  buf_len));
    }
}

ssize_t 
ipmi_ssif_read (int dev_fd, 
		uint8_t *buf, 
		size_t buf_len)
{
  union ipmi_i2c_smbus_data data;
  int bytes_read = 0;
  int bytes_copied = 0;
  int length;
  int block_number;
  int index;
  int sindex;
  int multi_read_start = 0;
  int i;
  
  if (buf == NULL || buf_len <= 0)
    {
      errno = EINVAL;
      return (-1);
    }
  
  if (ipmi_i2c_smbus_access (dev_fd, 
			     IPMI_I2C_SMBUS_READ, 
			     IPMI_SSIF_SINGLE_PART_READ_SMBUS_CMD, 
			     &data) == -1)
    {
      return (-1);
    }
  
  if (data.block[0] == IPMI_SSIF_MULTI_PART_READ_START_SIZE && 
      data.block[1] == IPMI_SSIF_MULTI_PART_READ_START_PATTERN1 && 
      data.block[2] == IPMI_SSIF_MULTI_PART_READ_START_PATTERN2)
    {
      sindex = 3;
      multi_read_start = 1;
    }
  else
    {
      sindex = 1;
    }
  
  length = data.block[0];
  bytes_read = length;
  
  if (bytes_read > buf_len)
    {
      length = buf_len;
    }
  
  for (i = 0; i < length; i++)
    {
      buf[i] = data.block[sindex + i];
    }
  
  bytes_copied = length;
  
  while (multi_read_start)
    {
      if (ipmi_i2c_smbus_access (dev_fd, 
				 IPMI_I2C_SMBUS_READ, 
				 IPMI_SSIF_MULTI_PART_READ_MIDDLE_SMBUS_CMD, 
				 &data) == -1)
	{
	  return (-1);
	}
      
      length = data.block[0];
      block_number = data.block[1];
      bytes_read += length;
      
      if ((bytes_copied + length) > buf_len)
	{
	  length = buf_len - bytes_copied;
	}
      
      for (i = 0; i < length; i++)
	{
	  buf[bytes_copied + i] = data.block[i + 2];
	}
      
      bytes_copied += length;
      
      if (block_number == IPMI_SSIF_MULTI_PART_READ_END_PATTERN)
	{
	  break;
	}
    }
  
  return bytes_read;
}

int 
ipmi_ssif_close (int i2c_fd)
{
  return (close (i2c_fd));
}

int 
ipmi_ssif_cmd2 (ipmi_device_t *dev, 
		fiid_obj_t obj_cmd_rq, 
		fiid_template_t tmpl_cmd_rq, 
		fiid_obj_t obj_cmd_rs, 
		fiid_template_t tmpl_cmd_rs)
{
  if (!(dev && tmpl_cmd_rq && obj_cmd_rq && tmpl_cmd_rs && obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }
  
  { 
    uint8_t *pkt;
    uint32_t pkt_len;
    
    pkt_len = fiid_obj_len_bytes (*(dev->io.inband.rq.tmpl_hdr_ptr)) + 
      fiid_obj_len_bytes (tmpl_cmd_rq);
    pkt = alloca (pkt_len);
    memset (pkt, 0, pkt_len);
    ERR (pkt);
    
    ERR (fill_hdr_ipmi_kcs (dev->lun, 
			    dev->net_fn, 
			    dev->io.inband.rq.obj_hdr) == 0);
    ERR (assemble_ipmi_kcs_pkt (dev->io.inband.rq.obj_hdr, 
				obj_cmd_rq, 
				tmpl_cmd_rq, 
				pkt, 
				pkt_len) > 0);
    
    ERR (ipmi_ssif_write (dev->io.inband.dev_fd, pkt, pkt_len) != -1);
  }
  
  { 
    uint8_t *pkt;
    size_t pkt_max_size = 1024;
    uint32_t pkt_len;
    size_t read_len;
    size_t bytes_read = 0;
    
    pkt_len = fiid_obj_len_bytes (*(dev->io.inband.rs.tmpl_hdr_ptr)) + 
      fiid_obj_len_bytes (tmpl_cmd_rs);
    if (pkt_len > pkt_max_size)
      pkt_max_size *= 2;
    pkt = alloca (pkt_max_size);
    memset (pkt, 0, pkt_max_size);
    ERR (pkt);
    
    ERR ((bytes_read = ipmi_ssif_read (dev->io.inband.dev_fd, pkt, pkt_max_size)) != -1);
    if (bytes_read != pkt_len)
      {
	int i;
	
	fprintf (stderr, 
		 "%s(): received invalid packet.  "
		 "expected size = %d, received size = %d\n", 
		 __PRETTY_FUNCTION__, 
		 pkt_len, 
		 bytes_read);
	fprintf (stderr, "packet data:\n");
	for (i = 0; i < bytes_read; i++)
	  fprintf (stderr, "%02X ", pkt[i]);
	fprintf (stderr, "\n");
	
	fprintf (stderr, "please report to <freeipmi-devel@gnu.org>\n");
	
	return (-1);
      }
    ERR (unassemble_ipmi_kcs_pkt (pkt, 
				  bytes_read, 
				  dev->io.inband.rs.obj_hdr, 
				  obj_cmd_rs, 
				  tmpl_cmd_rs) != -1);
  }
  
  return (0);
}

int  
ipmi_ssif_cmd_raw2 (ipmi_device_t *dev, 
		    uint8_t *buf_rq, 
		    size_t buf_rq_len, 
		    uint8_t *buf_rs, 
		    size_t *buf_rs_len)
{
  if (!(dev && buf_rq && buf_rq_len > 0 
        && buf_rs && buf_rs_len && *buf_rs_len > 0))
    {
      errno = EINVAL;
      return (-1);
    }
  
  { 
    /* Request Block */
    ERR (ipmi_ssif_write (dev->io.inband.dev_fd, (char *)buf_rq, buf_rq_len) != -1);
  }
  
  { 
    /* Response Block */
    uint32_t bytes_read = 0;
    
    ERR ((bytes_read = ipmi_ssif_read (dev->io.inband.dev_fd, 
				       (char *)buf_rs, *buf_rs_len)) != -1);
    if (bytes_read > *buf_rs_len)
      {
	fprintf (stderr, 
		 "%s(): received packet is too big.  "
		 "expected size = %d, received size = %d\n", 
		 __PRETTY_FUNCTION__, 
		 buf_rs_len, 
		 bytes_read);
	fprintf (stderr, "please report to <freeipmi-devel@gnu.org>\n");
      }
    else 
      *buf_rs_len = bytes_read;
  }
  
  return (0);
}
