/*
  Copyright (C) 2003-2008 FreeIPMI Core Team

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

#ifndef _HOSTMAP_H
#define _HOSTMAP_H

enum hostmap_errnum
  {
    HOSTMAP_ERR_SUCCESS = 0,
    HOSTMAP_ERR_FILE_NOT_FOUND = 1,
    HOSTMAP_ERR_OUT_OF_MEMORY = 2,
    HOSTMAP_ERR_PARAMETERS = 3,
    HOSTMAP_ERR_PARSE = 4,
    HOSTMAP_ERR_HOSTNAME_INVALID = 5,
    HOSTMAP_ERR_HOSTNAME_COUNT_INVALID = 6,
    HOSTMAP_ERR_DUPLICATE_ENTRY = 7,
    HOSTMAP_ERR_HOST_NOT_FOUND = 8,
    HOSTMAP_ERR_SYSTEM_ERROR = 9,
    HOSTMAP_ERR_INTERNAL_ERROR = 10,
    HOSTMAP_ERR_ERRNUMRANGE = 11,
  };

typedef struct hostmap *hostmap_t;

/* return 0 success, -1 error */
typedef int (*hostmap_for_each_f)(const char *althost, const char *ipmihost, void *arg);

hostmap_t hostmap_create(void);

void hostmap_destroy(hostmap_t hmap);

int hostmap_errnum(hostmap_t hmap);

char *hostmap_strerror(int errnum);

int hostmap_parse(hostmap_t hmap, const char *filename);

/* if error HOSTMAP_ERR_PARSE or HOSTMAP_ERR_DUPLICATE_ENTRY get line of error */
int hostmap_line(hostmap_t hmap);

char *hostmap_map_althost(hostmap_t hmap, const char *althost);

int hostmap_open(hostmap_t *hmapptr, const char *filename);

#endif
