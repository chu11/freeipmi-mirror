/*
  Copyright (C) 2006 FreeIPMI Core Team

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2, or (at your option)
  any later version.
  
  This program is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.
  
  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA
*/

#ifndef _IPMI_SDR_CACHE_READS_H
#define _IPMI_SDR_CACHE_READS_H

#include "ipmi-sdr-cache.h"

int sdr_cache_read_repository_info_timestamps (sdr_cache_ctx_t ctx,
                                               char *cache_record,
                                               unsigned int *addition_timestamp,
                                               unsigned int *erase_timestamp);

int sdr_cache_read_record (sdr_cache_ctx_t ctx,
                           char *cache_record, 
                           sdr_record_t *record);

#endif
