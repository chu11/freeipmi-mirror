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
#include <assert.h>
#include <errno.h>

#include "freeipmi/locate/ipmi-locate.h"
#include "freeipmi/driver/ipmi-ssif-driver.h"

#include "ipmi-locate-defs.h"
#include "ipmi-locate-trace.h"
#include "ipmi-locate-util.h"

#include "freeipmi-portability.h"

#define DEFAULT_MEM_DEV "/dev/mem"

typedef uint8_t fipmiu8;
typedef uint16_t fipmiu16;
typedef uint32_t fipmiu32;

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
  fipmiu32 h;
  fipmiu32 l;
} fipmiu64;
#else
typedef struct
{
  fipmiu32 l;
  fipmiu32 h;
} fipmiu64;
#endif

#ifdef ALIGNMENT_WORKAROUND
static fipmiu64 U64 (fipmiu32 low, fipmiu32 high)
{
  fipmiu64 self;

  self.l=low;
  self.h=high;

  return (self);
}
#endif

#ifdef ALIGNMENT_WORKAROUND
#       ifdef BIGENDIAN
#       define WORD(x) (fipmiu16)((x)[1]+((x)[0]<<8))
#       define DWORD(x) (fipmiu32)((x)[3]+((x)[2]<<8)+((x)[1]<<16)+((x)[0]<<24))
#       define QWORD(x) (U64 (DWORD (x+4), DWORD (x)))
#       else /* BIGENDIAN */
#       define WORD(x) (fipmiu16)((x)[0]+((x)[1]<<8))
#       define DWORD(x) (fipmiu32)((x)[0]+((x)[1]<<8)+((x)[2]<<16)+((x)[3]<<24))
#       define QWORD(x) (U64 (DWORD (x), DWORD (x+4)))
#       endif /* BIGENDIAN */
#else /* ALIGNMENT_WORKAROUND */
#define WORD(x) (fipmiu16)(*(const fipmiu16 *)(x))
#define DWORD(x) (fipmiu32)(*(const fipmiu32 *)(x))
#define QWORD(x) (*(const fipmiu64 *)(x))
#endif /* ALIGNMENT_WORKAROUND */

struct dmi_header
{
  fipmiu8 type;
  fipmiu8 length;
  fipmiu16 handle;
};

#ifndef HAVE_MMAP
static int
_myread (ipmi_locate_ctx_t ctx,
         int fd,
         fipmiu8 *buf,
         size_t count)
{
  ssize_t r = 1;
  size_t r2 = 0;

  assert (ctx);
  assert (ctx->magic == IPMI_LOCATE_CTX_MAGIC);
  assert (buf);

  while (r2 != count && r != 0)
    {
      if ((r = read (fd, buf + r2, count - r2)) < 0)
        {
          if (errno != EINTR)
            {
              /* ignore potential error, error path */
              close (fd);
              LOCATE_ERRNO_TO_LOCATE_ERRNUM (ctx, errno);
              return (-1);
            }
        }
      else
        r2 += r;
    }

  if (r2 != count)
    {
      LOCATE_SET_ERRNUM (ctx, IPMI_LOCATE_ERR_SYSTEM_ERROR);
      /* ignore potential error, error path */
      close (fd);
      return (-1);
    }

  return (0);
}
#endif

static int
_checksum (const fipmiu8 *buf, size_t len)
{
  fipmiu8 sum = 0;
  size_t a;

  assert (buf);

  for (a = 0; a < len; a++)
    sum += buf[a];
  return (sum == 0);
}

/*
 * Copy a physical memory chunk into a memory buffer.
 * This function allocates memory.
 */
static void *
_mem_chunk (ipmi_locate_ctx_t ctx,
            size_t base,
            size_t len,
            const char *devmem)
{
  void *p = NULL;
  void *rv = NULL;
  int fd = -1;
#ifdef HAVE_MMAP
  size_t mmoffset;
  void *mmp;
#ifdef _SC_PAGESIZE
  long pagesize;
#endif
#endif

  assert (ctx);
  assert (ctx->magic == IPMI_LOCATE_CTX_MAGIC);
  assert (devmem);

  if ((fd = open (devmem, O_RDONLY)) < 0)
    {
      LOCATE_ERRNO_TO_LOCATE_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (!(p = malloc (len)))
    {
      LOCATE_SET_ERRNUM (ctx, IPMI_LOCATE_ERR_OUT_OF_MEMORY);
      goto cleanup;
    }

#ifdef HAVE_MMAP
#ifdef _SC_PAGESIZE
  {
    if ((pagesize = sysconf (_SC_PAGESIZE)) < 0)
      {
        LOCATE_ERRNO_TO_LOCATE_ERRNUM (ctx, errno);
        goto cleanup;
      }
    mmoffset = base % pagesize;
  }
#else
  mmoffset = base % getpagesize ();
#endif /* _SC_PAGESIZE */
  /*
   * Please note that we don't use mmap() for performance reasons here,
   * but to workaround problems many people encountered when trying
   * to read from /dev/mem using regular read() calls.
   */
  if ((mmp = mmap (0,
                   mmoffset + len,
                   PROT_READ,
                   MAP_SHARED,
                   fd,
                   base - mmoffset)) == MAP_FAILED)
    {
      LOCATE_ERRNO_TO_LOCATE_ERRNUM (ctx, errno);
      goto cleanup;
    }

  memcpy (p, (fipmiu8 *) mmp + mmoffset, len);
  rv = p;
  /* ignore potential error, just return result */
  munmap (mmp, mmoffset + len);
#else /* HAVE_MMAP */

  if (lseek (fd, base, SEEK_SET) < 0)
    {
      LOCATE_ERRNO_TO_LOCATE_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (_myread (ctx, fd, p, len) < 0)
    goto cleanup;

  rv = p;
#endif /* HAVE_MMAP */

 cleanup:
  /* ignore potential error, cleanup path */
  close (fd);
  if (!rv)
    free (p);
  return (rv);
}

static int
_dmi_table (ipmi_locate_ctx_t ctx,
            fipmiu32 base,
            fipmiu16 len,
            fipmiu16 num,
            fipmiu16 ver,
            const char *devmem,
            ipmi_interface_type_t type,
            struct ipmi_locate_info *locate_info)
{
  fipmiu8 *buf = NULL;
  fipmiu8 *data;
  int i = 0;
  int rv = -1;

  assert (ctx);
  assert (ctx->magic == IPMI_LOCATE_CTX_MAGIC);

  if (!(buf = _mem_chunk (ctx, base, len, devmem)))
    return (-1);

  data = buf;
  while ((i < num) &&
         ((data + sizeof (struct dmi_header)) <= (buf + len)))
    {
      fipmiu8 *next;
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
          locate_info->ipmi_version_major = data[0x05] >> 4;
          locate_info->ipmi_version_minor = data[0x05] & 0x0F;

          if (locate_info->interface_type == IPMI_INTERFACE_SSIF)
            {
              locate_info->driver_address = data[0x06] >> 1;
              locate_info->address_space_id = IPMI_ADDRESS_SPACE_ID_SMBUS;
              locate_info->register_spacing = 0x01;
              strncpy (locate_info->driver_device, IPMI_DEFAULT_I2C_DEVICE, IPMI_LOCATE_PATH_MAX);
              locate_info->driver_device[IPMI_LOCATE_PATH_MAX - 1] = '\0';
            }
          else
            {
              fipmiu8 *ptr = NULL;
              fipmiu8 lsb;
              uint64_t base_addr;
              fipmiu64 address;

              ptr = data + 0x08;
              if (h->length < 0x12)
                lsb = 0;
              else
                lsb = (data[0x10] >> 5) & 1;

              address = QWORD (ptr);

              base_addr = address.h;
              base_addr = (base_addr << 32) | ((address.l & ~1) | lsb);

              if (address.l & 1)
                {
                  locate_info->address_space_id = IPMI_ADDRESS_SPACE_ID_SYSTEM_IO;
                  locate_info->driver_address = base_addr;
                }
              else
                {
                  locate_info->address_space_id = IPMI_ADDRESS_SPACE_ID_SYSTEM_MEMORY;
                  locate_info->driver_address = base_addr;
                }

              switch (data[0x10] >> 6)
                {
                case 0x0:
                  locate_info->register_spacing = 0x01;
                  break;
                case 0x1:
                  locate_info->register_spacing = 0x04;
                  break;
                case 0x2:
                  locate_info->register_spacing = 0x10;
                  break;
                }
            }

#if 0
          printf ("ipmi_locate_info.ipmi_version_major = [%d]\n",
                  locate_info->ipmi_version_major);
          printf ("ipmi_locate_info.ipmi_version_minor = [%d]\n",
                  locate_info->ipmi_version_minor);
          printf ("ipmi_locate_info.locate_driver_type = [%d]\n",
                  locate_info->locate_driver_type);
          printf ("ipmi_locate_info.interface_type = [%d]\n",
                  locate_info->interface_type);
          printf ("ipmi_locate_info.driver_device = [%s]\n",
                  locate_info->driver_device);
          printf ("ipmi_locate_info.address_space_id = [%X]\n",
                  locate_info->address_space_id);
          printf ("ipmi_locate_info.driver_address = [%X]\n",
                  locate_info->driver_address);
          printf ("ipmi_locate_info.register_spacing = [%X]\n",
                  locate_info->register_spacing);
#endif

          if (locate_info->interface_type == type)
            {
              rv = 0;
              goto cleanup;
            }
        }

      data = next;
      i++;
    }

 cleanup:
  free (buf);
  return (rv);
}

static int
_smbios_decode (ipmi_locate_ctx_t ctx,
                fipmiu8 *buf,
                const char *devmem,
                ipmi_interface_type_t type,
                struct ipmi_locate_info *locate_info)
{
  assert (ctx);
  assert (ctx->magic == IPMI_LOCATE_CTX_MAGIC);
  assert (devmem);
  assert (IPMI_INTERFACE_TYPE_VALID (type));
  assert (locate_info);

  if (_checksum (buf, buf[0x05]) &&
      (memcmp (buf + 0x10, "_DMI_", 5) == 0) &&
      _checksum (buf + 0x10, 0x0F))
    return (_dmi_table (ctx,
                        DWORD (buf + 0x18),
                        WORD (buf + 0x16),
                        WORD (buf + 0x1C),
                        (buf[0x06] << 8) + buf[0x07],
                        devmem,
                        type,
                        locate_info));

  return (-1);
}

static int
_legacy_decode (ipmi_locate_ctx_t ctx,
                fipmiu8 *buf,
                const char *devmem,
                ipmi_interface_type_t type,
                struct ipmi_locate_info *locate_info)
{
  assert (ctx);
  assert (ctx->magic == IPMI_LOCATE_CTX_MAGIC);
  assert (devmem);
  assert (IPMI_INTERFACE_TYPE_VALID (type));
  assert (locate_info);

  if (_checksum (buf, 0x0F))
    return (_dmi_table (ctx,
                        DWORD (buf + 0x08),
                        WORD (buf + 0x06),
                        WORD (buf + 0x0C),
                        ((buf[0x0E] & 0xF0) << 4) + (buf[0x0E] & 0x0F),
                        devmem,
                        type,
                        locate_info));

  return (-1);
}

int
ipmi_locate_dmidecode_get_device_info (ipmi_locate_ctx_t ctx,
                                       ipmi_interface_type_t type,
                                       struct ipmi_locate_info *info)
{
  struct ipmi_locate_info locate_info;
  int found = 0;
  size_t fp;
  FILE *efi_systab;
  const char *filename;
  char linebuf[64];
  fipmiu8 *buf = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_LOCATE_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_locate_ctx_errormsg (ctx), ipmi_locate_ctx_errnum (ctx));
      return (-1);
    }

  if (!IPMI_INTERFACE_TYPE_VALID (type) || !info)
    {
      LOCATE_SET_ERRNUM (ctx, IPMI_LOCATE_ERR_PARAMETERS);
      return (-1);
    }

  memset (&locate_info, '\0', sizeof (struct ipmi_locate_info));
  /*
   * Linux up to 2.6.6-rc2: /proc/efi/systab
   * Linux 2.6.6-rc3 and up: /sys/firmware/efi/systab
   */
  if (((efi_systab = fopen (filename = "/proc/efi/systab", "r")))
      || ((efi_systab = fopen (filename = "/sys/firmware/efi/systab", "r"))))
    {
      fp = 0;
      while ((fgets (linebuf, sizeof (linebuf) - 1, efi_systab)))
	{
	  char *addr =  memchr (linebuf, '=', strlen (linebuf));
	  *(addr++) = '\0';
	  if (!strcmp (linebuf, "SMBIOS"))
	    {
	      char *endptr;
	  
	      errno = 0;
	      fp = strtoul (addr, &endptr, 0);
	      if (errno
		  || ((endptr[0] != '\0') && (endptr[0] != 0x0a)))
		{
		  LOCATE_SET_ERRNUM (ctx, IPMI_LOCATE_ERR_SYSTEM_ERROR);
		  return (-1);
		}
	    }
	}
      fclose (efi_systab);

      if (!fp)
	{
	  LOCATE_SET_ERRNUM (ctx, IPMI_LOCATE_ERR_SYSTEM_ERROR);
	  return (-1);
	}

      if (!(buf = _mem_chunk (ctx, fp, 0x20, DEFAULT_MEM_DEV)))
	return (-1);

      if (!(_smbios_decode (ctx, buf, DEFAULT_MEM_DEV, type, &locate_info) < 0))
	found++;

      free (buf);
    }
#if !defined(__arm__) && !defined(__aarch64__)
  else
    {
      if (!(buf = _mem_chunk (ctx, 0xF0000, 0x10000, DEFAULT_MEM_DEV)))
	return (-1);

      for (fp = 0; fp <= 0xFFF0; fp += 16)
	{
	  if ((memcmp (buf + fp, "_SM_", 4) == 0) && (fp <= 0xFFE0))
	    {
	      if (!(_smbios_decode (ctx,
				    buf + fp,
				    DEFAULT_MEM_DEV,
				    type,
				    &locate_info) < 0))
		{
		  found++;
		  break;
		}
	      fp += 16;
	    }
	  else if (memcmp (buf + fp, "_DMI_", 5) == 0)
	    {
	      if (!(_legacy_decode (ctx,
				    buf + fp,
				    DEFAULT_MEM_DEV,
				    type,
				    &locate_info) < 0))
		{
		  found++;
		  break;
		}
	    }
	}

      free (buf);
    }
#endif
  
  if (found)
    {
      memcpy (info, &locate_info, sizeof (struct ipmi_locate_info));
      rv = 0;
    }
  else
    LOCATE_SET_ERRNUM (ctx, IPMI_LOCATE_ERR_SYSTEM_ERROR);
  
  return (rv);
}
