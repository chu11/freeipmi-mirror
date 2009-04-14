/*****************************************************************************\
 *  $Id: ipmi-fru-parse-common.h,v 1.1.2.5 2009-04-14 20:47:13 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2009 Lawrence Livermore National Security, LLC.
 *  Copyright (C) 2007 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-232183
 *
 *  This file is part of Ipmi-fru, a tool used for retrieving
 *  motherboard field replaceable unit (FRU) information. For details,
 *  see http://www.llnl.gov/linux/.
 *
 *  Ipmi-fru is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 2 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmi-fru is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmi-fru.  If not, see <http://www.gnu.org/licenses/>.
\*****************************************************************************/

#ifndef _IPMI_FRU_PARSE_COMMON_H
#define _IPMI_FRU_PARSE_COMMON_H

#if 0
int ipmi_fru_parse_output_type_length_field (ipmi_fru_parse_ctx_t ctx,
                                             uint8_t *frubuf,
                                             unsigned int frubuflen,
                                             unsigned int offset_in_bytes,
                                             uint8_t *language_code,
                                             unsigned int *len_parsed,
                                             char *str);
#endif

#endif
