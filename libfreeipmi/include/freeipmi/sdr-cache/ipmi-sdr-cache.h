/*****************************************************************************\
 *  $Id: ipmi-sdr-cache.h,v 1.2 2007-12-29 17:20:33 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2006-2007 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-222073
 *
 *  This file is part of Ipmimonitoring, an IPMI sensor monitoring
 *  library.  For details, see http://www.llnl.gov/linux/.
 *
 *  Ipmimonitoring is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 2 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmimonitoring is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmimonitoring.  If not, see <http://www.gnu.org/licenses/>.
\*****************************************************************************/

#ifndef _IPMI_SDR_CACHE_H
#define _IPMI_SDR_CACHE_H

#include <stdint.h>
#include <freeipmi/api/ipmi-api.h>

#define IPMI_SDR_CACHE_CTX_ERR_SUCCESS                                      0
#define IPMI_SDR_CACHE_CTX_ERR_CONTEXT_NULL                                 1
#define IPMI_SDR_CACHE_CTX_ERR_CONTEXT_INVALID                              2
#define IPMI_SDR_CACHE_CTX_ERR_PARAMETERS                                   3
#define IPMI_SDR_CACHE_CTX_ERR_OUT_OF_MEMORY                                4
#define IPMI_SDR_CACHE_CTX_ERR_FILENAME_INVALID                             5
#define IPMI_SDR_CACHE_CTX_ERR_FILESYSTEM                                   6
#define IPMI_SDR_CACHE_CTX_ERR_PERMISSION                                   7
#define IPMI_SDR_CACHE_CTX_ERR_CACHE_CREATE_CACHE_EXISTS                    8
#define IPMI_SDR_CACHE_CTX_ERR_CACHE_CREATE_CTX_SET_TO_READ                 9
#define IPMI_SDR_CACHE_CTX_ERR_CACHE_CREATE_DUPLICATE_RECORD_ID             10
#define IPMI_SDR_CACHE_CTX_ERR_CACHE_CREATE_DUPLICATE_SENSOR_NUMBER         11
#define IPMI_SDR_CACHE_CTX_ERR_CACHE_CREATE_INVALID_RECORD_LENGTH           12
#define IPMI_SDR_CACHE_CTX_ERR_CACHE_CREATE_INVALID_RECORD_COUNT            13
#define IPMI_SDR_CACHE_CTX_ERR_CACHE_READ_ALREADY_INITIALIZED               14
#define IPMI_SDR_CACHE_CTX_ERR_CACHE_READ_INITIALIZATION                    15
#define IPMI_SDR_CACHE_CTX_ERR_CACHE_READ_CACHE_DOES_NOT_EXIST              16
#define IPMI_SDR_CACHE_CTX_ERR_CACHE_DELETE_CTX_SET_TO_READ                 17
#define IPMI_SDR_CACHE_CTX_ERR_CACHE_INVALID                                18
#define IPMI_SDR_CACHE_CTX_ERR_CACHE_OUT_OF_DATE                            19
#define IPMI_SDR_CACHE_CTX_ERR_NOT_FOUND                                    20
#define IPMI_SDR_CACHE_CTX_ERR_IPMI_ERROR                                   21
#define IPMI_SDR_CACHE_CTX_ERR_SYSTEM_ERROR                                 22
#define IPMI_SDR_CACHE_CTX_ERR_OVERFLOW                                     23
#define IPMI_SDR_CACHE_CTX_ERR_INTERNAL_ERROR                               24
#define IPMI_SDR_CACHE_CTX_ERR_ERRNUMRANGE                                  25

#define IPMI_SDR_CACHE_CREATE_FLAGS_DEFAULT            0x0
/* During cache creation, overwrite any previously created cache.  Default
 * is to return an error that the cache already exists.
 */
#define IPMI_SDR_CACHE_CREATE_FLAGS_OVERWRITE          0x1

#define IPMI_SDR_CACHE_VALIDATION_FLAGS_DEFAULT                 0x0
/* During cache creation, check for duplicate record ids and return error if
 * one is found.
 */
#define IPMI_SDR_CACHE_VALIDATION_FLAGS_DUPLICATE_RECORD_ID     0x1
/* During cache creation, check for duplicate sensor numbers and
 * return error if one is found.
 */
#define IPMI_SDR_CACHE_VALIDATION_FLAGS_DUPLICATE_SENSOR_NUMBER 0x2

typedef struct ipmi_sdr_cache_ctx *ipmi_sdr_cache_ctx_t;

/* Callback between every record that is cached */
typedef void (*Sdr_Create_Callback)(uint8_t sdr_version,
                                    uint16_t record_count,
                                    uint32_t most_recent_addition_timestamp,
                                    uint32_t most_recent_erase_timestamp,
                                    uint16_t record_id,
                                    void *data);

/* SDR Cache Context Functions */
ipmi_sdr_cache_ctx_t ipmi_sdr_cache_ctx_create(void);
void ipmi_sdr_cache_ctx_destroy(ipmi_sdr_cache_ctx_t ctx);
int ipmi_sdr_cache_ctx_errnum(ipmi_sdr_cache_ctx_t ctx);
char * ipmi_sdr_cache_ctx_strerror(int errnum);

/* SDR Cache Creation Functions */
int ipmi_sdr_cache_create(ipmi_sdr_cache_ctx_t ctx, 
                          ipmi_ctx_t ipmi_ctx,
			  char *filename, 
			  int create_flags,
			  int validation_flags,
                          Sdr_Create_Callback create_callback,
                          void *create_callback_data);

/* SDR Cache Reading Functions */
int ipmi_sdr_cache_open(ipmi_sdr_cache_ctx_t ctx, 
                        ipmi_ctx_t ipmi_ctx,
                        char *filename);

int ipmi_sdr_cache_sdr_version(ipmi_sdr_cache_ctx_t ctx, uint8_t *sdr_version);
int ipmi_sdr_cache_record_count(ipmi_sdr_cache_ctx_t ctx, uint16_t *record_count);
int ipmi_sdr_cache_most_recent_addition_timestamp(ipmi_sdr_cache_ctx_t ctx,
                                                  uint32_t *most_recent_addition_timestamp);
int ipmi_sdr_cache_most_recent_erase_timestamp(ipmi_sdr_cache_ctx_t ctx, 
                                               uint32_t *most_recent_erase_timestamp);

int ipmi_sdr_cache_first(ipmi_sdr_cache_ctx_t ctx);
int ipmi_sdr_cache_next(ipmi_sdr_cache_ctx_t ctx);
int ipmi_sdr_cache_seek(ipmi_sdr_cache_ctx_t ctx, unsigned int index);
int ipmi_sdr_cache_search_record_id(ipmi_sdr_cache_ctx_t ctx, uint16_t record_id);

int ipmi_sdr_cache_record_read(ipmi_sdr_cache_ctx_t ctx, 
			       uint8_t *buf,
			       unsigned int buflen);

int ipmi_sdr_cache_close(ipmi_sdr_cache_ctx_t ctx);

/* SDR Cache Delete Functions */
int ipmi_sdr_cache_delete(ipmi_sdr_cache_ctx_t ctx, char *filename);


#endif /* _IPMI_SDR_CACHE_H */
