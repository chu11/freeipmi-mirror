/*
 * Copyright (C) 2003-2012 FreeIPMI Core Team
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

#ifndef TOOL_HOSTRANGE_COMMON_H
#define TOOL_HOSTRANGE_COMMON_H

/* Returns number of hosts setup for, -1 on error */
int pstdout_setup (char **hosts,
                   int buffer_output,
                   int consolidate_output,
                   int fanout,
                   int eliminate,
                   int always_prefix);

#endif /* TOOL_HOSTRANGE_COMMON_H */
