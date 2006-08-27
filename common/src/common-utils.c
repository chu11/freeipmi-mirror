/*
common-utils.c: commonly used utility functions.
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

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <ctype.h>
#include <errno.h>
#include <limits.h>

char *
stripwhite (char *string)
{
  register char *s, *t;

  for (s = string; isspace (*s); s++)
    ;

  if (*s == 0)
    return s;

  t = s + strlen (s) - 1;
  while (t > s && isspace (*t))
    t--;
  *++t = '\0';

  return s;
}

char *
get_token (char **line)
{
  char *command;
  while (1)
    {
      command = (char *) strsep (line, " ,");
      if (!command)
        break;
      if (*(command))
        break;
    }
  
  if (command)
    return strdup (stripwhite (command));
  return command;
}

int 
str2long (char *str, int base, long *l)
{
  long value;
  char *tail = NULL;
  int errnum;
  
  errno = 0;
  value = strtol (str, &tail, base);
  errnum = errno;
  
  if (errnum)
    return (-1); // overflow
  
  if (tail[0] != '\0')
    return (-1); // invalid integer format
  
  *l = value;
  
  return 0;
}

int 
str2ulong (char *str, int base, unsigned long *ul)
{
  long l;
  unsigned long value;
  char *tail = NULL;
  int errnum;
  
  errno = 0;
  l = strtol (str, &tail, base);
  errnum = errno;
  
  if (tail[0] != '\0')
    return (-1); // invalid integer format
  
  if (!errnum)
    {
      if (l < 0)
	return (-1); // minus sign is present
    }
  
  errno = 0;
  value = strtoul (str, &tail, base);
  errnum = errno;
  
  if (errnum)
    return (-1); // overflow
  
  if (tail[0] != '\0')
    return (-1); // invalid integer format
  
  *ul = value;
  
  return 0;
}

int 
str2int (char *str, int base, int *i)
{
  long l;
  
  if (!str2long (str, base, &l))
    {
      if (l >= INT_MIN && l <= INT_MAX)
	{
	  *i = l;
	  return 0;
	}
    }
  
  return (-1);
}

int 
str2uint (char *str, int base, unsigned int *ui)
{
  unsigned long ul;
  
  if (!str2ulong (str, base, &ul))
    {
      if (ul <= UINT_MAX)
	{
	  *ui = ul;
	  return 0;
	}
    }
  
  return (-1);
}

int 
str2double (char *str, double *d)
{
  double value;
  char *tail = NULL;
  int errnum;
  
  if (!(str && d && str[0]))
    return (-1);
  
  errno = 0;
  value = strtod (str, &tail);
  errnum = errno;
  
  if (errnum)
    return (-1);
  
  if (tail[0] != '\0')
    return (-1); // invalid format
  
  *d = value;
  
  return 0;
}

