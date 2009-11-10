/*
   Copyright (C) 2003-2008 FreeIPMI Core Team

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
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.
*/

#ifndef _TOOL_HOSTRANGE_COMMON_H
#define _TOOL_HOSTRANGE_COMMON_H

/* Returns number of hosts setup for, -1 on error */
int pstdout_setup(char **hosts,
                  int buffer_output,
                  int consolidate_output,
                  int fanout,
                  int eliminate,
                  int always_prefix);

#endif /* _TOOL_HOSTRANGE_COMMON_H */
