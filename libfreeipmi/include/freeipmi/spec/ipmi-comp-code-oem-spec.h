/*
   Copyright (C) 2003-2010 FreeIPMI Core Team

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

#ifndef _IPMI_COMP_CODE_OEM_SPEC_H
#define _IPMI_COMP_CODE_OEM_SPEC_H

#ifdef __cplusplus
extern "C" {
#endif

/******************************************* 
 * Fujitsu                                 *
 *******************************************/

/*
 * Fujitsu RX100 S5
 *
 * http://manuals.ts.fujitsu.com/file/4390/irmc_s2-en.pdf
 */
/* IPMI_CMD_OEM_FUJITSU_BIOS - w/ GET_CPU_INFO Command Specifier */
#define IPMI_COMP_CODE_OEM_FUJITSU_UNPOPULATED_CPU_SOCKET 0x01
#define IPMI_COMP_CODE_OEM_FUJITSU_UNPOPULATED_CPU_SOCKET_STR \
  "Unpopulated CPU Socket"

#ifdef __cplusplus
}
#endif

#endif /* _IPMI_COMP_CODE_OEM_SPEC_H */

