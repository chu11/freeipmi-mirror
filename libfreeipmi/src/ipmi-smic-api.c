/* 
   ipmi-smic-interface.c: IPMI - System Management Interface Chip - SMS Interface

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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#ifdef STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#ifdef HAVE_LINUX_TYPES_H
#include <linux/types.h>
#endif
#include <err.h>
#include <errno.h>
#include <fcntl.h>

#include "freeipmi/fiid.h"
#include "freeipmi/ipmi-smic-api.h"

#include "ipmi-inband.h"

#include "ipmi-common.h"
#include "freeipmi-portability.h"

#if defined(__FreeBSD__) && !defined(USE_IOPERM)
static int ipmi_smic_dev_io_fd = -1;
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

static uint64_t smic_poll_count;
static unsigned long smic_sleep_usecs = IPMI_SMIC_SLEEP_USECS;

fiid_template_t tmpl_hdr_smic =
  {
    {2, "lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {6, "net_fn", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

#if 0
static int
ipmi_smic_print_flags (int fd, uint8_t state)
{
  ipmi_dprintf (fd, "Current SMIC flags: %#x : ", state);
  if(state & IPMI_SMIC_RX_DATA_RDY) 
    ipmi_dprintf (fd, "RX_DATA_RDY ");
  if(state & IPMI_SMIC_TX_DATA_RDY)
    ipmi_dprintf (fd, "TX_DATA_RDY ");
  if(state & IPMI_SMIC_SMI)
    ipmi_dprintf (fd, "SMI ");
  if(state & IPMI_SMIC_EVT_ATN) 
    ipmi_dprintf (fd, "EVT_ATN ");
  if(state & IPMI_SMIC_SMS_ATN)
    ipmi_dprintf (fd, "SMS_ATN ");
  if(state & IPMI_SMIC_BUSY)
    ipmi_dprintf (fd, "BUSY ");
  ipmi_dprintf (fd, "\n");
  return (0);
}
#endif /* 0x */

uint64_t 
ipmi_smic_get_poll_count ()
{
  return smic_poll_count;
}

int
ipmi_smic_io_init (uint8_t sms_io_base, unsigned long sleep_usecs)
{
  smic_sleep_usecs = sleep_usecs;

#ifdef __FreeBSD__
#ifdef USE_IOPERM
  /* i386_set_ioperm has known problems on FBSD 5.x (bus errors). */
  return (i386_set_ioperm(sms_io_base, 0x02, 0x01));
#else
  /* Opening /dev/io raises IOPL bits for that process. */
  /* XXX This fd will remain open until exit as there is no
   * uninitialization routine. */
  ipmi_smic_dev_io_fd = open("/dev/io", O_RDONLY);
  return (ipmi_smic_dev_io_fd == -1 ? -1 : 0);
#endif
#else
  return (iopl (3));
#endif
}

/* Examine flags register. */
uint8_t 
ipmi_smic_get_flags (uint16_t sms_io_base)
{
  return _INB (IPMI_SMIC_REG_FLAGS (sms_io_base));
}

uint8_t
ipmi_smic_get_status (uint16_t sms_io_base)
{
  return _INB (IPMI_SMIC_REG_STATUS (sms_io_base));
}

/* Basic spin loops. */
void 
ipmi_smic_wait_for_not_busy (uint16_t sms_io_base) 
{
  while(ipmi_smic_get_flags(sms_io_base) & IPMI_SMIC_BUSY)
    {
      usleep (IPMI_SMIC_SLEEP_USECS);
    }
}

void 
ipmi_smic_wait_for_rx_ready (uint16_t sms_io_base)
{
  while(!(ipmi_smic_get_flags (sms_io_base) & IPMI_SMIC_RX_DATA_RDY))
    {
      usleep (IPMI_SMIC_SLEEP_USECS);
    }
}

void 
ipmi_smic_wait_for_tx_ready (uint16_t sms_io_base)
{
  while(!(ipmi_smic_get_flags(sms_io_base) & IPMI_SMIC_TX_DATA_RDY))
    {
      usleep (IPMI_SMIC_SLEEP_USECS);
    }
}

void 
ipmi_smic_wait_for_idle (uint16_t sms_io_base)
{
  while (ipmi_smic_get_flags (sms_io_base) & IPMI_SMIC_BUSY)
    {
        usleep (IPMI_SMIC_SLEEP_USECS);
    }
}

/* Flag manipulations. */

void 
ipmi_smic_set_busy (uint16_t sms_io_base)
{
    _OUTB (IPMI_SMIC_BUSY, IPMI_SMIC_REG_FLAGS (sms_io_base));
}

/* Basic functions for writing bytes. */

void 
ipmi_smic_write_start (uint16_t sms_io_base, uint8_t data)
{
    ipmi_smic_wait_for_idle (sms_io_base);
    _OUTB (IPMI_SMIC_CC_SMS_WR_START, IPMI_SMIC_REG_CONTROL (sms_io_base));
    _OUTB (data, IPMI_SMIC_REG_DATA (sms_io_base));
    ipmi_smic_set_busy (sms_io_base);
    ipmi_smic_wait_for_idle (sms_io_base);
}

void 
ipmi_smic_write_next (uint16_t sms_io_base, uint8_t data)
{
    ipmi_smic_wait_for_idle (sms_io_base);
    ipmi_smic_wait_for_tx_ready (sms_io_base);
    _OUTB (IPMI_SMIC_CC_SMS_WR_NEXT, IPMI_SMIC_REG_CONTROL (sms_io_base));
    _OUTB (data, IPMI_SMIC_REG_DATA (sms_io_base));
    ipmi_smic_set_busy (sms_io_base);
    ipmi_smic_wait_for_idle (sms_io_base);
}
void 
ipmi_smic_write_end (uint16_t sms_io_base, uint8_t data)
{
    ipmi_smic_wait_for_idle (sms_io_base);
    ipmi_smic_wait_for_tx_ready (sms_io_base);
    _OUTB (IPMI_SMIC_CC_SMS_WR_END, IPMI_SMIC_REG_CONTROL (sms_io_base));
    _OUTB (data, IPMI_SMIC_REG_DATA (sms_io_base));
    ipmi_smic_set_busy (sms_io_base);
    ipmi_smic_wait_for_idle (sms_io_base);
}

/* Basic functions for doing reads. */

uint8_t
ipmi_smic_read_start (uint16_t sms_io_base)
{
    ipmi_smic_wait_for_idle (sms_io_base);
    ipmi_smic_wait_for_rx_ready (sms_io_base);
    _OUTB (IPMI_SMIC_CC_SMS_RD_START, IPMI_SMIC_REG_CONTROL (sms_io_base));
    ipmi_smic_set_busy (sms_io_base);
    ipmi_smic_wait_for_idle (sms_io_base);
    return _INB (IPMI_SMIC_REG_DATA (sms_io_base));
}

uint8_t
ipmi_smic_read_next (uint16_t sms_io_base)
{
    ipmi_smic_wait_for_idle (sms_io_base);
    ipmi_smic_wait_for_rx_ready (sms_io_base);
    _OUTB (IPMI_SMIC_CC_SMS_RD_NEXT, IPMI_SMIC_REG_CONTROL (sms_io_base));
    ipmi_smic_set_busy (sms_io_base);
    ipmi_smic_wait_for_idle (sms_io_base);
    return _INB (IPMI_SMIC_REG_DATA (sms_io_base));
}

void
ipmi_smic_read_end (uint16_t sms_io_base)
{
    ipmi_smic_wait_for_idle (sms_io_base);
    _OUTB (IPMI_SMIC_CC_SMS_RD_END, IPMI_SMIC_REG_CONTROL (sms_io_base));
    ipmi_smic_set_busy (sms_io_base);
    ipmi_smic_wait_for_idle (sms_io_base);
}

/* API read function. */

int
ipmi_smic_write (uint16_t sms_io_base, uint8_t* data, int len)
{
    int outlen=0;
    int x=0;
    uint8_t* p=data;
    
    /* In case someone isn't paying attention */
    if(len < 2)
    {
        warnx("ipmi_smic_write: Impossibly short message\n");
        return -1;
    }
    
    /* Start up write with first byte. */
    ipmi_smic_write_start(sms_io_base, *p);
    p++; outlen++;
    
    x = ipmi_smic_get_status (sms_io_base);
    if(x != IPMI_SMIC_SC_SMS_WR_START) 
    {
        warnx("ipmi_smic_write: Error writing starting byte (%d)\n", x);
        return -1;
    }
    
    /* Loop over rest of bytes. */
    for(; outlen < (len - 1); outlen++) 
    {
        ipmi_smic_write_next (sms_io_base, *p);
        p++;
        outlen++;
        x = ipmi_smic_get_status (sms_io_base);
    
        if(x != IPMI_SMIC_SC_SMS_WR_NEXT) 
        {
            warnx("ipmi_smic_write: Error writing byte (%d)\n", x);
            return -1;
        }

    }
    
    /* End with final byte. */
    ipmi_smic_write_end (sms_io_base, *p);
    x = ipmi_smic_get_status (sms_io_base);
    if(x != IPMI_SMIC_SC_SMS_WR_END) 
    {
        warnx("ipmi_smic_write: Error writing ending byte (%d)\n", x);
        return -1;
    }
    
    warnx("ipmi_smic_write: Write return code %#x", _INB (IPMI_SMIC_REG_DATA (sms_io_base)));

    outlen++;
    
    return outlen;
}

/* API read function. */

int
ipmi_smic_read (uint16_t sms_io_base, uint8_t* data, int len)
{
    int outlen=0;
    int x=0;
    uint8_t* p=data;

    /* In case someone isn't paying attention */
    if(len < 2)
    {
        warnx("ipmi_smic_read: Impossibly small buffer\n");
        return -1;
    }

    *p = ipmi_smic_read_start (sms_io_base);
    p++;
    outlen++;
    x = ipmi_smic_get_status (sms_io_base);
    if(x == IPMI_SMIC_SC_SMS_RD_END)
    {
        /* only one byte to grab */
        return outlen;
    }
    
    if(x != IPMI_SMIC_SC_SMS_RD_START) 
    {
        warnx("ipmi_smic_read: Error reading starting byte (%d)\n", x);
        return -1;
    }

    while(outlen < len) 
    {
        *p = ipmi_smic_read_next (sms_io_base);
        p++;
        outlen++;
        x = ipmi_smic_get_status (sms_io_base);
        warnx("ipmi_smic_read: Read byte %#x, outlen = %d, len = %d, status = %#x", *(p-1), outlen, len, x);
        if(x != IPMI_SMIC_SC_SMS_RD_NEXT) break;
    }
    
    if(outlen < len && x != IPMI_SMIC_SC_SMS_RD_END)
    {
        warnx("ipmi_smic_read: Error reading byte (%d)\n", x);
        return -1;
    }
    
    ipmi_smic_read_end (sms_io_base);
    x = ipmi_smic_get_status (sms_io_base);
    
    if(x != IPMI_SMIC_SC_SMS_RDY) 
    {
        warnx("ipmi_smic_read: Error returning to RDY state (%d)\n", x);
        return -1;
    }
    
    return outlen;
}

