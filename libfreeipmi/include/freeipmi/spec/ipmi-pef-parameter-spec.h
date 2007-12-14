/* Copyright (C) 2003, 2004, 2005 FreeIPMI Core Team */

/* This file is free software; you can redistribute it and/or modify */
/* it under the terms of the GNU General Public License as published by */
/* the Free Software Foundation; either version 2, or (at your option) */
/* any later version. */

/* This file is distributed in the hope that it will be useful, */
/* but WITHOUT ANY WARRANTY; without even the implied warranty of */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the */
/* GNU General Public License for more details. */

/* You should have received a copy of the GNU General Public License */
/* along with GNU Emacs; see the file COPYING.  If not, write to */
/* the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, */
/* Boston, MA 02110-1301, USA. */

/* $Id: ipmi-pef-parameter-spec.h,v 1.1.2.1 2007-12-14 05:45:35 chu11 Exp $ */

#ifndef IPMI_PEF_PARAMETER_SPEC_H
#define IPMI_PEF_PARAMETER_SPEC_H

#ifdef __cplusplus
extern "C" {
#endif

#define IPMI_PEF_PARAMETER_PEF_CONTROL			          1
#define IPMI_PEF_PARAMETER_PEF_ACTION_GLOBAL_CONTROL              2
#define IPMI_PEF_PARAMETER_PEF_STARTUP_DELAY                      3
#define IPMI_PEF_PARAMETER_PEF_ALERT_STARTUP_DELAY                4
#define IPMI_PEF_PARAMETER_NUMBER_OF_EVENT_FILTERS                5
#define IPMI_PEF_PARAMETER_EVENT_FILTER_TABLE                     6
#define IPMI_PEF_PARAMETER_EVENT_FILTER_TABLE_DATA_1              7
#define IPMI_PEF_PARAMETER_NUMBER_OF_ALERT_POLICY_ENTRIES         8
#define IPMI_PEF_PARAMETER_ALERT_POLICY_TABLE                     9
#define IPMI_PEF_PARAMETER_SYSTEM_GUID                           10
#define IPMI_PEF_PARAMETER_NUMBER_OF_ALERT_STRINGS               11
#define IPMI_PEF_PARAMETER_ALERT_STRING_KEYS                     12
#define IPMI_PEF_PARAMETER_ALERT_STRINGS                         13
#define IPMI_PEF_PARAMETER_NUMBER_OF_GROUP_CONTROL_TABLE_ENTRIES 14
#define IPMI_PEF_PARAMETER_GROUP_CONTROL_TABLE                   15

#ifdef __cplusplus
}
#endif

#endif
