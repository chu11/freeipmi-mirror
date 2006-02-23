/* 
   ipmi-inband-macros.h - macros for inband communication

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

#ifndef _IPMI_INBAND_MACROS_H
#define	_IPMI_INBAND_MACROS_H 1

#ifdef __cplusplus
extern "C" {
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifndef _OS2_
#  if (defined(__GLIBC__) && __GLIBC__ >= 2)
#    include <sys/io.h>
#  elif defined (__OpenBSD__) || defined (__NetBSD__)
#    include <machine/pio.h>/* inb/outb */
#    include <machine/sysarch.h>/* sysarch call */
#  elif defined (__FreeBSD__)
#    include <machine/cpufunc.h>
#    include <machine/sysarch.h>
#  elif defined (PPC)
#    include <asm/io.h>
#  else
#    ifdef _AXP_
#       include <sys/io.h>
#    endif
#  endif   
#endif

#if defined(__FreeBSD__)
# define _INB(port)  inb (port)
# define _OUTB(data, port)  outb (port, data)
#else
# define _INB(port)  inb (port)
# define _OUTB(data, port)  outb (data, port)
#endif

#ifdef __cplusplus
}
#endif

#endif /* ipmi-inband-macros.h */

