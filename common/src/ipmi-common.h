/*
ipmi-common.h: common code for freeipmi tools.
Copyright (C) 2005 FreeIPMI Core Team

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA
*/

#ifndef _IPMI_COMMON_H
#define _IPMI_COMMON_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifndef __cplusplus

#include <freeipmi/freeipmi.h>

#if !defined(__STDC_VERSION__) || (__STDC_VERSION__ < 199901L)
# if !defined(__GNUC__) ||(__GNUC__ < 3)
  typedef char _Bool;           /* For C compilers without _Bool */
# endif
#endif
 
#define bool  _Bool
#define true  1
#define false 0
 
#else
 
  /* C++ */
#define bool  bool
#define true  true
#define false false
#endif
#define __bool_true_false_are_defined 1

#if  __WORDSIZE == 64
#define FI_64 "%l"
#else
#define FI_64 "%ll"
#endif

# if ENABLE_NLS
#  include <libintl.h>
#  define _(Text) gettext (Text)
# else
#  define textdomain(Domain)
#  define _(Text) Text
# endif
# define N_(Text) Text

int ipmi_is_root ();

/* Portable version of the extremely unportable Linux dprintf() */
int ipmi_dprintf(int fd, char *fmt, ...);

/* From David Wheeler's Secure Programming Guide */
void *guaranteed_memset(void *s, int c, size_t n);

/* Turn an input string into a 20-byte binary k_g key */
int parse_kg(unsigned char *outbuf, int outsz, char *instr);

/* Turn a 20-byte binary k_g key into an output string */
char *format_kg(char *outstr, int outsz, unsigned char *k_g);

#endif
