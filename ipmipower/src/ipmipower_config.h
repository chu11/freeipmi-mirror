/*****************************************************************************\
 *  $Id: ipmipower_config.h,v 1.7 2007-10-17 23:13:04 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2003-2007 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-155698
 *  
 *  This file is part of Ipmipower, a remote power control utility.
 *  For details, see http://www.llnl.gov/linux/.
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
 *  with Ipmipower; if not, write to the Free Software Foundation, Inc.,
 *  51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.
\*****************************************************************************/

#ifndef _IPMIPOWER_CONFIG_H
#define _IPMIPOWER_CONFIG_H

#include "ipmipower.h"

/* ipmipower_config_default_logfile
 * - Create default logfile name and store in buffer
 */
void ipmipower_config_default_logfile(char *buf, int buflen);

/* ipmipower_config_setup
 * - setup and initialization config defaults
 */ 
void ipmipower_config_setup(void);

/* ipmipower_config_cmdline_parse
 * - parse command line options
 */
void ipmipower_config_cmdline_parse(int argc, char **argv);

/* ipmipower_config_conffile_parse
 * - read and store info from config file
 */
void ipmipower_config_conffile_parse(char *conffile);

/* ipmipower_config_check_values
 * - check values stored in conf
 */
void ipmipower_config_check_values(void);

#endif /* _IPMIPOWER_CONFIG_H */
