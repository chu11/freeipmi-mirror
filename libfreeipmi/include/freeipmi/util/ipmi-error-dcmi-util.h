/*
   Copyright (C) 2003-2009 FreeIPMI Core Team

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
/*****************************************************************************\
 *  $Id: ipmi-error-dcmi-util.h,v 1.1 2009-11-25 15:47:40 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2009 Lawrence Livermore National Security, LLC.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-155698
 *
 *  This file is part of Ipmi-Dcmi, tools and libraries to support the
 *  data center manageability interface (DCMI).  For details, see
 *  http://www.llnl.gov/linux/.
 *
 *  Ipmi-Dcmi is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 2 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmi-Dcmi is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmi-Dcmi.  If not, see <http://www.gnu.org/licenses/>.
\*****************************************************************************/

#ifndef _IPMI_ERROR_DCMI_UTIL_H
#define _IPMI_ERROR_DCMI_UTIL_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <sys/types.h>
#include <freeipmi/fiid/fiid.h>

/* returns 0 on success, -1 on error */
int ipmi_completion_code_dcmi_strerror_r (uint8_t cmd,
                                          uint8_t netfn,
                                          uint8_t comp_code,
                                          char *errstr,
                                          size_t len);

/* returns 0 on success, -1 on error */
int ipmi_completion_code_dcmi_strerror_cmd_r (fiid_obj_t obj_cmd,
                                              uint8_t netfn,
                                              char *errstr,
                                              size_t len);

#ifdef __cplusplus
}
#endif

#endif /* ipmi-error-dcmi-util.h */

