/* 
   $Id: common.h,v 1.8 2006-02-25 02:44:00 chu11 Exp $ 

   common.h - Common header definitions.

   Copyright (C) 2005 FreeIPMI Core Team

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
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.  
*/

#ifndef _COMMON_H
#define _COMMON_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <argp.h>
#include <pwd.h>
#include <termios.h>
#include <guile/gh.h>
#include <readline/readline.h>
#include <readline/history.h>
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#if HAVE_FCNTL_H
#include <fcntl.h>
#endif /* HAVE_FCNTL_H */
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/resource.h>
#if TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else /* !TIME_WITH_SYS_TIME */
#if HAVE_SYS_TIME_H
#include <sys/time.h>
#else /* !HAVE_SYS_TIME_H */
#ifdef __FreeBSD__
#include <sys/time.h>
#else  /* !__FreeBSD */
#include <time.h>
#endif /* !__FreeBSD */
#endif /* !HAVE_SYS_TIME_H */
#endif  /* !TIME_WITH_SYS_TIME */
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>

#include "ipmi-udm.h"
#include "ipmi-lan-interface.h"
#include "ipmi-device-global-cmds.h"
#include "ipmi-device-global-cmds-udm.h"
#include "ipmi-messaging-support-cmds.h"
#include "ipmi-messaging-support-cmds-udm.h"
#include "ipmi-lan-cmds.h"
#include "ipmi-lan-cmds-udm.h"
#include "ipmi-pef-and-alerting-cmds.h"
#include "ipmi-pef-and-alerting-cmds-udm.h"
#include "ipmi-serial-modem-cmds.h"
#include "ipmi-serial-modem-cmds-udm.h"
#include "ipmi-sensor-api.h"
#include "ipmi-sel-api.h"
#include "ipmi-sel-cmds.h"
#include "ipmi-sel-cmds-udm.h"
#include "ipmi-sensor-types-spec.h"
#include "ipmi-sensor-units-spec.h"
#include "ipmi-sdr-record-types.h"
#include "ipmi-sdr-repository-cmds.h"
#include "ipmi-sdr-repository-cmds-udm.h"
#include "ipmi-sdr-repository-cache-api.h"
#include "rmcp.h"
#include "ipmi-cmd-spec.h"
#include "ipmi-netfn-spec.h"
#include "ipmi-lan-param-spec.h"
#include "ipmi-ipmb-interface.h"
#include "ipmi-chassis-cmds.h"
#include "ipmi-chassis-cmds-udm.h"

#include "argp-common.h"
#include "ipmi-common.h"

#include "xmalloc.h"
#include "fish-argp.h"
#include "fi-utils.h"
#include "guile-wrapper.h"
#include "extension.h"
#include "interpreter.h"
#include "ipmi-wrapper.h"
#include "bmc-conf2.h"
#include "scm-procedures.h"
#include "fi-commands.h"
#include "fish.h"

#define SET_SELECTOR      0x0
#define BLOCK_SELECTOR    0x0

#endif
