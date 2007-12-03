/*
ipmi-sdr-cache-writes.h: SDR cache creation and management apis.
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

#ifndef _IPMI_SDR_CACHE_WRITES_H
#define _IPMI_SDR_CACHE_WRITES_H

#include <stdio.h>

#include "ipmi-sdr-cache.h"

int sdr_cache_write_repository_info (sdr_cache_ctx_t ctx,
                                     ipmi_ctx_t ipmi_ctx,
                                     FILE *fp,
                                     unsigned int *sdr_record_count);

int sdr_cache_write_record (sdr_cache_ctx_t ctx,
                            FILE *fp,
                            sdr_record_t *record);

#endif
