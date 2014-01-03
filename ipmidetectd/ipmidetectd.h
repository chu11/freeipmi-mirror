/*****************************************************************************\
 *  $Id: ipmidetectd.h,v 1.11 2010-02-08 22:02:30 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2014 Lawrence Livermore National Security, LLC.
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

#ifndef IPMIDETECTD_H
#define IPMIDETECTD_H

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <freeipmi/freeipmi.h>

#include "hostlist.h"

enum ipmidetectd_argp_option_keys
  {
    IPMIDETECTD_CONFIG_FILE_KEY = 'c',
    IPMIDETECTD_LEGACY_CONFIG_FILE_KEY = 160,	/* legacy */
    IPMIDETECTD_DEBUG_KEY = 'd',
  };

struct ipmidetectd_config
{
  int ipmiping_period;
  int ipmidetectd_server_port;
  hostlist_t hosts;
};

struct ipmidetectd_arguments
{
  int debug;
  char *config_file;
};

#endif /* IPMIDETECTD_H */
