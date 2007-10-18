/*****************************************************************************\
 *  $Id: ipmiconsole_config.h,v 1.16 2007-10-18 16:18:46 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007 Lawrence Livermore National Security, LLC.
 *  Copyright (C) 2006-2007 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-221226
 *  
 *  This file is part of Ipmiconsole, a set of IPMI 2.0 SOL libraries
 *  and utilities.  For details, see http://www.llnl.gov/linux/.
 *  
 *  Ipmipower is free software; you can redistribute it and/or modify 
 *  it under the terms of the GNU General Public License as published by the 
 *  Free Software Foundation; either version 2 of the License, or (at your 
 *  option) any later version.
 *  
 *  Ipmipower is distributed in the hope that it will be useful, but 
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License 
 *  for more details.
 *  
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmipower.  If not, see <http://www.gnu.org/licenses/>.
\*****************************************************************************/

#ifndef _IPMICONSOLE_CONFIG_H
#define _IPMICONSOLE_CONFIG_H

#if HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <sys/param.h>
#include <freeipmi/freeipmi.h>

#ifndef MAXHOSTNAMELEN
#define MAXHOSTNAMELEN 64
#endif /* MAXHOSTNAMELEN */

#define IPMICONSOLE_CONFIG_FILE_DEFAULT "/etc/ipmiconsole.conf"

struct ipmiconsole_config
{
  int debug;
#ifndef NDEBUG
  int debugfile;
  int noraw;
#endif /* NDEBUG */
  char *config_file;

  char hostname[MAXHOSTNAMELEN+1];
  char username[IPMI_MAX_USER_NAME_LENGTH+1];
  char password[IPMI_2_0_MAX_PASSWORD_LENGTH+1];
  unsigned char k_g[IPMI_MAX_K_G_LENGTH+1];
  unsigned int k_g_len;
  int privilege;
  int cipher_suite_id;
  int dont_steal;
  int deactivate;
  int lock_memory;
  unsigned int workaround_flags;

  int hostname_set_on_cmdline;
  int username_set_on_cmdline;
  int password_set_on_cmdline;
  int k_g_set_on_cmdline;
  int privilege_set_on_cmdline;
  int cipher_suite_id_set_on_cmdline;
  int dont_steal_set_on_cmdline;
  int deactivate_set_on_cmdline;
  int lock_memory_set_on_cmdline;
  int workaround_flags_set_on_cmdline;
};

void ipmiconsole_config_setup(int argc, char **argv);

#endif /* _IPMICONSOLE_CONFIG_H */
