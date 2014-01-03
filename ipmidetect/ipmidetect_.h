/*****************************************************************************\
 *  $Id: ipmidetect.h,v 1.11 2010-02-08 22:02:30 chu11 Exp $
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

/* file is named ipmidetect_.h to differentiate itself from the
 * library ipmidetect.h.
 *
 * I am scared of the portability of the #include_next directive, so
 * that's why I'm doing it this way.
 */

/* file is "ipmidetect_.h", so double underscore */
#ifndef IPMIDETECT__H
#define IPMIDETECT__H

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include "hostlist.h"

enum ipmidetect_argp_option_keys
  {
    IPMIDETECT_HOSTNAME_KEY = 'h',
    IPMIDETECT_LEGACY_HOSTNAME_KEY = 'o',	/* legacy */
    IPMIDETECT_PORT_KEY = 'p',
    IPMIDETECT_DETECTED_KEY = 'd',
    IPMIDETECT_UNDETECTED_KEY = 'u',
    IPMIDETECT_HOSTRANGE_KEY = 'q',
    IPMIDETECT_COMMA_KEY = 'c',
    IPMIDETECT_NEWLINE_KEY = 'n',
    IPMIDETECT_SPACE_KEY = 's',
  };

enum ipmidetect_output_type
  {
    IPMIDETECT_DETECTED_NODES,
    IPMIDETECT_UNDETECTED_NODES,
    IPMIDETECT_DETECTED_AND_UNDETECTED_NODES,
  };

struct ipmidetect_arguments
{
  char *hostname;
  int port;
  int output_type;
  char output_format;
  hostlist_t inputted_nodes;
};

#endif /* IPMIDETECT__H */
