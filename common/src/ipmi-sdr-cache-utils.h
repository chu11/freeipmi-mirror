/*
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

#ifndef _IPMI_SDR_CACHE_UTILS_H
#define _IPMI_SDR_CACHE_UTILS_H

#define STRING_WHITESPACES    " \f\n\r\t\v"

char *stripwhite (char *string);
char *get_token (char **line);
int remove_newline (char *str);
char *strsep_noempty (char **strp, char *delims);
int str2long (char *str, int base, long *l);
int str2ulong (char *str, int base, unsigned long *ul);
int str2int (char *str, int base, int *i);
int str2uint (char *str, int base, unsigned int *ui);
int str2double (char *str, double *d);

#endif
