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

#ifndef _IPMI_SOL_PARAMETER_SPEC_H
#define _IPMI_SOL_PARAMETER_SPEC_H

#ifdef __cplusplus
extern "C" {
#endif

#define IPMI_SOL_PARAMETER_SET_IN_PROGRESS                                  0
#define IPMI_SOL_PARAMETER_SOL_ENABLE                                       1
#define IPMI_SOL_PARAMETER_SOL_AUTHENTICATION                               2
#define IPMI_SOL_PARAMETER_CHARACTER_ACCUMULATE_INTERVAL_AND_SEND_THRESHOLD 3
#define IPMI_SOL_PARAMETER_SOL_RETRY                                        4
#define IPMI_SOL_PARAMETER_SOL_NON_VOLATILE_BIT_RATE                        5
#define IPMI_SOL_PARAMETER_SOL_VOLATILE_BIT_RATE                            6 
#define IPMI_SOL_PARAMETER_SOL_PAYLOAD_CHANNEL                              7
#define IPMI_SOL_PARAMETER_SOL_PAYLOAD_PORT_NUMBER                          8

#ifdef __cplusplus
}
#endif

#endif /* _IPMI_SOL_PARAMETER_SPEC_H */
