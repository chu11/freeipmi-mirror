/* 
   dmi-locate.c -  locate IPMI interfaces by dmidecode.

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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#ifdef STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <limits.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#if HAVE_FCNTL_H
#include <fcntl.h>
#endif /* HAVE_FCNTL_H */
#include <errno.h>

#include "freeipmi/ipmi-locate.h"
#include "freeipmi/fiid.h"
#include "freeipmi/ipmi-ssif-api.h"

#include "err-wrappers.h"
#include "freeipmi-portability.h"
#include "xmalloc.h"

#ifdef __ia64__
#define USE_EFI
#endif /* __ia64__ */

#define DEFAULT_MEM_DEV "/dev/mem"

typedef uint8_t  u8;
typedef uint16_t u16;
typedef uint32_t u32;

/*
 * These macros help us solve problems on systems that don't support
 * non-aligned memory access. This isn't a big issue IMHO, since the tools
 * in this package are intended mainly for Intel and compatible systems,
 * which are little-endian and support non-aligned memory access. Anyway,
 * you may use the following defines to control the way it works:
 * - Define BIGENDIAN on big-endian systems.
 * - Define ALIGNMENT_WORKAROUND if your system doesn't support
 *   non-aligned memory access. In this case, we use a slower, but safer,
 *   memory access method.
 * You most probably will have to define none or the two of them.
 */

#ifdef BIGENDIAN
typedef struct 
{
  u32 h;
  u32 l;
} u64;
#else
typedef struct
{
  u32 l;
  u32 h;
} u64;
#endif

#ifdef ALIGNMENT_WORKAROUND
static u64 U64 (u32 low, u32 high)
{
  u64 self;
  
  self.l=low;
  self.h=high;
  
  return self;
}
#endif

#ifdef ALIGNMENT_WORKAROUND
#	ifdef BIGENDIAN
#	define WORD(x) (u16)((x)[1]+((x)[0]<<8))
#	define DWORD(x) (u32)((x)[3]+((x)[2]<<8)+((x)[1]<<16)+((x)[0]<<24))
#	define QWORD(x) (U64(DWORD(x+4), DWORD(x)))
#	else /* BIGENDIAN */
#	define WORD(x) (u16)((x)[0]+((x)[1]<<8))
#	define DWORD(x) (u32)((x)[0]+((x)[1]<<8)+((x)[2]<<16)+((x)[3]<<24))
#	define QWORD(x) (U64(DWORD(x), DWORD(x+4)))
#	endif /* BIGENDIAN */
#else /* ALIGNMENT_WORKAROUND */
#define WORD(x) (u16)(*(const u16 *)(x))
#define DWORD(x) (u32)(*(const u32 *)(x))
#define QWORD(x) (*(const u64 *)(x))
#endif /* ALIGNMENT_WORKAROUND */

struct dmi_header
{
  u8 type;
  u8 length;
  u16 handle;
};

#ifndef HAVE_MMAP
static int 
myread (int fd, u8 *buf, size_t count, const char *prefix)
{
  ssize_t r = 1;
  size_t r2 = 0;
  
  while (r2 != count && r != 0)
    {
      r = read (fd, buf + r2, count - r2);
      if (r == -1)
	{
	  if (errno != EINTR)
	    {
	      close (fd);
	      perror (prefix);
	      return -1;
	    }
	}
      else
	r2 += r;
    }
  
  if (r2 != count)
    {
      close (fd);
      fprintf (stderr, "%s: Unexpected end of file\n", prefix);
      return -1;
    }
  
  return 0;
}
#endif

static int 
checksum (const u8 *buf, size_t len)
{
  u8 sum = 0;
  size_t a;
  
  for (a = 0; a < len; a++)
    sum += buf[a];
  return (sum == 0);
}

/*
 * Copy a physical memory chunk into a memory buffer.
 * This function allocates memory.
 */
static void *
mem_chunk (size_t base, size_t len, const char *devmem)
{
  void *p;
  int fd;
#ifdef HAVE_MMAP
  size_t mmoffset;
  void *mmp;
#endif
  
  if ((fd = open (devmem, O_RDONLY)) == -1)
    {
      perror (devmem);
      return NULL;
    }
  
  if ((p = malloc (len)) == NULL)
    {
      perror ("malloc");
      return NULL;
    }
  
#ifdef HAVE_MMAP
#ifdef _SC_PAGESIZE
  mmoffset = base % sysconf (_SC_PAGESIZE);
#else
  mmoffset = base % getpagesize ();
#endif /* _SC_PAGESIZE */
  /*
   * Please note that we don't use mmap() for performance reasons here,
   * but to workaround problems many people encountered when trying
   * to read from /dev/mem using regular read() calls.
   */
  mmp = mmap (0, mmoffset + len, PROT_READ, MAP_SHARED, fd, base - mmoffset);
  if (mmp == ((void *) -1))
    {
      fprintf (stderr, "%s: ", devmem);
      perror ("mmap");
      free (p);
      return NULL;
    }
  
  memcpy (p, (u8 *) mmp + mmoffset, len);
  
  if (munmap (mmp, mmoffset + len) == -1)
    {
      fprintf (stderr, "%s: ", devmem);
      perror ("munmap");
    }
#else /* HAVE_MMAP */
  if (lseek (fd, base, SEEK_SET) == -1)
    {
      fprintf (stderr, "%s: ", devmem);
      perror ("lseek");
      free (p);
      return NULL;
    }
  
  if (myread (fd, p, len, devmem) == -1)
    {
      free (p);
      return NULL;
    }
#endif /* HAVE_MMAP */
  
  if (close (fd) == -1)
    perror (devmem);
  
  return p;
}

static int 
dmi_table (u32 base, u16 len, u16 num, u16 ver, const char *devmem, ipmi_interface_type_t interface_type, ipmi_locate_info_t *locate_info)
{
  u8 *buf;
  u8 *data;
  int i = 0;
  
  if ((buf = mem_chunk (base, len, devmem)) == NULL)
    {
      return (-1);
    }
  
  data = buf;
  while ((i < num) && 
	 ((data + sizeof (struct dmi_header)) <= (buf + len)))
    {
      u8 *next;
      struct dmi_header *h = (struct dmi_header *) data;
      
      /* look for the next handle */
      next = data + h->length;
      while ((next - buf + 1 < len) && 
	     (next[0] != 0 || next[1] != 0))
	next++;
      next += 2;
      
      if ((next - buf <= len) && (h->type == 38) && (h->length >= 0x10))
	{
	  locate_info->locate_driver_type = IPMI_LOCATE_DRIVER_DMIDECODE;
	  locate_info->interface_type = data[0x04];
	  locate_info->ipmi_ver_major = data[0x05] >> 4;
	  locate_info->ipmi_ver_minor = data[0x05] & 0x0F;
	  
	  if (locate_info->interface_type == IPMI_INTERFACE_SSIF)
	    {
	      locate_info->base_address.bmc_smbus_slave_address = data[0x06] >> 1;
	      locate_info->address_space_id = IPMI_ADDRESS_SPACE_ID_SMBUS;
	      locate_info->reg_space = 0x01;
	      locate_info->bmc_i2c_dev_name = strdup (IPMI_DEFAULT_I2C_DEVICE);
	    }
	  else 
	    {
	      u8 *ptr = NULL;
	      u8 lsb;
	      uint64_t base_addr;
	      u64 address;
	      
	      ptr = data + 0x08;
	      if (h->length < 0x12)
		{
		  lsb = 0;
		}
	      else 
		{
		  lsb = (data[0x10] >> 5) & 1;
		}
	      
	      address = QWORD (ptr);
	      
	      base_addr = address.h;
	      base_addr = (base_addr << 32) | ((address.l & ~1) | lsb);
	      
	      if (address.l & 1)
		{
		  locate_info->base_address.bmc_iobase_address = base_addr;
		  locate_info->address_space_id = IPMI_ADDRESS_SPACE_ID_SYSTEM_IO;
		}
	      else
		{
		  locate_info->base_address.bmc_membase_address = base_addr;
		  locate_info->address_space_id = IPMI_ADDRESS_SPACE_ID_SYSTEM_MEMORY;
		}
	      
	      switch (data[0x10] >> 6)
		{
		case 0x0:
		  locate_info->reg_space = 0x01;
		  break;
		case 0x1:
		  locate_info->reg_space = 0x04;
		  break;
		case 0x2:
		  locate_info->reg_space = 0x10;
		  break;
		}
	    }
	  
#if 0
	  printf ("ipmi_locate_info.ipmi_ver_major = [%d]\n", 
		  locate_info->ipmi_ver_major);
	  printf ("ipmi_locate_info.ipmi_ver_minor = [%d]\n", 
		  locate_info->ipmi_ver_minor);
	  printf ("ipmi_locate_info.locate_driver_type = [%d]\n", 
		  locate_info->locate_driver_type);
	  printf ("ipmi_locate_info.locate_driver = [%d]\n", 
		  locate_info->locate_driver);
	  printf ("ipmi_locate_info.interface_type = [%d]\n", 
		  locate_info->interface_type);
	  printf ("ipmi_locate_info.bmc_i2c_dev_name = [%s]\n", 
		  locate_info->bmc_i2c_dev_name);
	  printf ("ipmi_locate_info.addr_space_id = [%X]\n", 
		  locate_info->address_space_id);
	  printf ("ipmi_locate_info.base_addr.bmc_iobase_addr = [%LX]\n", 
		  locate_info->base_address.bmc_iobase_address);
	  printf ("ipmi_locate_info.base_addr.bmc_membase_addr = [%LX]\n", 
		  locate_info->base_address.bmc_membase_address);
	  printf ("ipmi_locate_info.base_addr.bmc_smbus_slave_addr = [%X]\n", 
		  locate_info->base_address.bmc_smbus_slave_address);
	  printf ("ipmi_locate_info.reg_space = [%X]\n", 
		  locate_info->reg_space);
#endif
	  
	  if (locate_info->interface_type == interface_type)
	    {
	      free (buf);
	      return 0;
	    }
	}
      
      data = next;
      i++;
    }
  
  free (buf);
  
  return (-1);
}

static int 
smbios_decode (u8 *buf, const char *devmem, ipmi_interface_type_t interface_type, ipmi_locate_info_t *locate_info)
{
  if (checksum (buf, buf[0x05]) && 
      (memcmp (buf + 0x10, "_DMI_", 5) == 0) && 
      checksum (buf + 0x10, 0x0F))
    {
      return dmi_table (DWORD (buf + 0x18), WORD (buf + 0x16), WORD (buf + 0x1C), 
			(buf[0x06] << 8) + buf[0x07], devmem, 
			interface_type, locate_info);
    }
  
  return (-1);
}

#ifndef USE_EFI
static int 
legacy_decode (u8 *buf, const char *devmem, ipmi_interface_type_t interface_type, ipmi_locate_info_t *locate_info)
{
  if (checksum (buf, 0x0F))
    {
      return dmi_table (DWORD (buf + 0x08), WORD (buf + 0x06), WORD (buf + 0x0C), 
			((buf[0x0E] & 0xF0) << 4) + (buf[0x0E] & 0x0F), devmem, 
			interface_type, locate_info);
    }
	
  return (-1);
}
#endif /* USE_EFI */

ipmi_locate_info_t *
ipmi_locate_dmidecode_get_dev_info (ipmi_interface_type_t type)
{
  ipmi_locate_info_t locate_info;
  ipmi_locate_info_t *pinfo;
  int found = 0;
  size_t fp;
#ifdef USE_EFI
  FILE *efi_systab;
  const char *filename;
  char linebuf[64];
#endif /* USE_EFI */
  u8 *buf;
  
  ERR_EINVAL_NULL_RETURN (IPMI_INTERFACE_TYPE_VALID(type));

  memset (&locate_info, 0, sizeof (ipmi_locate_info_t));
#ifdef USE_EFI
  /*
   * Linux up to 2.6.6-rc2: /proc/efi/systab
   * Linux 2.6.6-rc3 and up: /sys/firmware/efi/systab
   */
  if (((efi_systab = fopen (filename = "/proc/efi/systab", "r")) == NULL) && 
      ((efi_systab = fopen (filename = "/sys/firmware/efi/systab", "r")) == NULL))
    {
      return NULL;
    }
  
  fp = 0;
  while ((fgets (linebuf, sizeof (linebuf) - 1, efi_systab)) != NULL)
    {
      char *addr = memchr (linebuf, '=', strlen (linebuf));
      *(addr++) = '\0';
      if (strcmp (linebuf, "SMBIOS") == 0)
	{
	  fp = strtoul (addr, NULL, 0);
	}
    }
  if (fclose (efi_systab) != 0)
    perror (filename);
  if (fp == 0)
    {
      return NULL;
    }
  
  if ((buf = mem_chunk (fp, 0x20, DEFAULT_MEM_DEV)) == NULL)
    {
      return NULL;
    }
  
  if (smbios_decode (buf, DEFAULT_MEM_DEV, type, &locate_info))
    {
      found++;
    }
  
  free (buf);
#else /* USE_EFI */
  if ((buf = mem_chunk (0xF0000, 0x10000, DEFAULT_MEM_DEV)) == NULL)
    {
      return NULL;
    }
  
  for (fp = 0; fp <= 0xFFF0; fp += 16)
    {
      if ((memcmp (buf + fp, "_SM_", 4) == 0) && (fp <= 0xFFE0))
	{
	  if (smbios_decode (buf + fp, DEFAULT_MEM_DEV, type, &locate_info) == 0)
	    {
	      found++;
	      break;
	    }
	  fp += 16;
	}
      else if (memcmp (buf + fp, "_DMI_", 5) == 0)
	{
	  if (legacy_decode (buf + fp, DEFAULT_MEM_DEV, type, &locate_info) == 0)
	    {
	      found++;
	      break;
	    }
	}
    }
  
  free (buf);
#endif /* USE_EFI */
  
  if (found)
    {
      pinfo = (ipmi_locate_info_t *) malloc (sizeof (struct ipmi_locate_info));
      if (pinfo)
	*pinfo = locate_info;
      return pinfo;
    }
  
  return NULL;
}
