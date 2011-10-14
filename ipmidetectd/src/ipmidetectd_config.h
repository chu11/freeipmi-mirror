/*****************************************************************************\
 *  $Id: ipmidetectd_config.h,v 1.11 2010-02-08 22:02:30 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2011 Lawrence Livermore National Security, LLC.
 *  Copyright (C) 2007 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-228523
 *
 *  This file is part of Ipmidetect, tools and libraries for detecting
 *  IPMI nodes in a cluster. For details, see http://www.llnl.gov/linux/.
 *
 *  Ipmidetect is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 3 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmidetect is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmidetect.  If not, see <http://www.gnu.org/licenses/>.
\*****************************************************************************/

#ifndef _IPMIDETECTD_CONFIG_H
#define _IPMIDETECTD_CONFIG_H

#if HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include "hostlist.h"

#define IPMIDETECTD_DEBUG_DEFAULT                        0
#define IPMIDETECTD_IPMIPING_PERIOD                      15000
#define IPMIDETECTD_SERVER_PORT_DEFAULT                  9225

struct ipmidetectd_config
{
#ifndef NDEBUG
  int debug;
#endif /* NDEBUG */
  char *config_file;

  int ipmiping_period;
  int ipmidetectd_server_port;
  hostlist_t hosts;
};

void ipmidetectd_config_setup (int argc, char **argv);

#endif /* _IPMIDETECTD_CONFIG_H */
