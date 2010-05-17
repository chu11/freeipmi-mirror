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
/*****************************************************************************\
 *  Copyright (C) 2009-2010 Lawrence Livermore National Security, LLC.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  LLNL-CODE-413270
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

#ifndef _IPMI_COMP_CODE_DCMI_SPEC_H
#define _IPMI_COMP_CODE_DCMI_SPEC_H

#ifdef __cplusplus
extern "C" {
#endif

/* IPMI_CMD_DCMI_GET_POWER_LIMIT */

#define IPMI_COMP_CODE_DCMI_NO_SET_POWER_LIMIT                        0x80
#define IPMI_COMP_CODE_DCMI_NO_SET_POWER_LIMIT_STR \
  "No Set Power Limit"

/* IPMI_CMD_DCMI_SET_POWER_LIMIT */

#define IPMI_COMP_CODE_DCMI_POWER_LIMIT_OUT_OF_RANGE                  0x84
#define IPMI_COMP_CODE_DCMI_POWER_LIMIT_OUT_OF_RANGE_STR \
  "Power Limit out of range"

#define IPMI_COMP_CODE_DCMI_CORRECTION_TIME_OUT_OF_RANGE              0x85
#define IPMI_COMP_CODE_DCMI_CORRECTION_TIME_OUT_OF_RANGE_STR \
  "Correction Time out of range"

#define IPMI_COMP_CODE_DCMI_STATISTICS_REPORTING_PERIOD_OUT_OF_RANGE  0x89 /* not a typo, is 0x89 */
#define IPMI_COMP_CODE_DCMI_STATISTICS_REPORTING_PERIOD_OUT_OF_RANGE_STR \
  "Statistics Reporting Period out of range"

#ifdef __cplusplus
}
#endif

#endif /* _IPMI_COMP_CODE_DCMI_SPEC_H */

