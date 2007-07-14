/*****************************************************************************\
 *  $Id: ipmi_monitoring_ipmi_communication.c,v 1.3.8.9 2007-07-14 01:29:46 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2006 The Regents of the University of California.
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
 *  with Ipmimonitoring; if not, write to the Free Software Foundation, Inc.,
 *  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA.
\*****************************************************************************/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <sys/types.h>
#include <assert.h>
#include <errno.h>
#include <freeipmi/freeipmi.h>

#include "ipmi_monitoring.h"
#include "ipmi_monitoring_defs.h"
#include "ipmi_monitoring_debug.h"
#include "ipmi_monitoring_fiid_wrappers.h"
#include "ipmi_monitoring_ipmi_communication.h"

extern uint32_t _ipmi_monitoring_flags;

static void
_ipmi_communication_cleanup(ipmi_monitoring_ctx_t c)
{
  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);

  if (c->comm.dev)
    {
      ipmi_close_device(c->comm.dev);
      ipmi_device_destroy(c->comm.dev);
      c->comm.dev = NULL;
    }
}

static int
_inband_init(ipmi_monitoring_ctx_t c,
             struct ipmi_monitoring_ipmi_config *config)
{
  unsigned int workaround_flags;
  unsigned int flags;

  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);
  assert(c->comm.dev);

  if (config
      && (config->driver_type >= 0
          && (config->driver_type != IPMI_MONITORING_DRIVER_TYPE_KCS
              && config->driver_type != IPMI_MONITORING_DRIVER_TYPE_SSIF
              && config->driver_type != IPMI_MONITORING_DRIVER_TYPE_OPENIPMI)))
    {
      c->errnum = IPMI_MONITORING_ERR_PARAMETERS;
      return -1;
    }
  

  workaround_flags = 0;
  if (config && config->workaround_flags)
    {
      /* No inband workarounds supported right now */
    }
  
  if ((_ipmi_monitoring_flags & IPMI_MONITORING_FLAGS_DEBUG)
      && (_ipmi_monitoring_flags & IPMI_MONITORING_FLAGS_DEBUG_IPMI_PACKETS))
    flags = IPMI_FLAGS_DEBUG_DUMP;
  else
    flags = IPMI_FLAGS_DEFAULT;

  if (!config || config->driver_type < 0)
    {
      if (ipmi_open_inband (c->comm.dev,
                            IPMI_DEVICE_OPENIPMI,
                            (config) ? config->disable_auto_probe : 0,
                            (config) ? config->driver_address : 0,
                            (config) ? config->register_spacing : 0,
                            (config) ? config->driver_device : NULL,
                            workaround_flags,
                            flags) < 0)
        {
          IPMI_MONITORING_DEBUG(("ipmi_cmd: %s", ipmi_device_strerror(ipmi_device_errnum(c->comm.dev))));
          
          if (ipmi_open_inband (c->comm.dev,
                                IPMI_DEVICE_KCS,
                                (config) ? config->disable_auto_probe : 0,
                                (config) ? config->driver_address : 0,
                                (config) ? config->register_spacing : 0,
                                (config) ? config->driver_device : NULL,
                                workaround_flags,
                                flags) < 0)
            {
              IPMI_MONITORING_DEBUG(("ipmi_cmd: %s", ipmi_device_strerror(ipmi_device_errnum(c->comm.dev))));
              
              if (ipmi_open_inband (c->comm.dev,
                                    IPMI_DEVICE_SSIF,
                                    (config) ? config->disable_auto_probe : 0,
                                    (config) ? config->driver_address : 0,
                                    (config) ? config->register_spacing : 0,
                                    (config) ? config->driver_device : NULL,
                                    workaround_flags,
                                    flags) < 0)
                {
                  IPMI_MONITORING_DEBUG(("ipmi_cmd: %s", ipmi_device_strerror(ipmi_device_errnum(c->comm.dev))));
                  
                  if (ipmi_device_errnum(c->comm.dev) == IPMI_ERR_BMC_BUSY)
                    c->errnum = IPMI_MONITORING_ERR_BMC_BUSY;
                  else if (ipmi_device_errnum(c->comm.dev) == IPMI_ERR_BAD_COMPLETION_CODE_INVALID_COMMAND
                           || ipmi_device_errnum(c->comm.dev) == IPMI_ERR_BAD_COMPLETION_CODE_REQUEST_DATA_INVALID
                           || ipmi_device_errnum(c->comm.dev) == IPMI_ERR_BAD_COMPLETION_CODE
                           || ipmi_device_errnum(c->comm.dev) == IPMI_ERR_IPMI_ERROR)
                    c->errnum = IPMI_MONITORING_ERR_IPMI_ERROR;
                  else if (ipmi_device_errnum(c->comm.dev) == IPMI_ERR_OUT_OF_MEMORY)
                    c->errnum = IPMI_MONITORING_ERR_OUT_OF_MEMORY;
                  else if (ipmi_device_errnum(c->comm.dev) == IPMI_ERR_SYSTEM_ERROR)
                    c->errnum = IPMI_MONITORING_ERR_SYSTEM_ERROR;
                  else if (ipmi_device_errnum(c->comm.dev) == IPMI_ERR_DRIVER_PATH_REQUIRED)
                    c->errnum = IPMI_MONITORING_ERR_PARAMETERS;
                  else
                    c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
                  return -1;
                }
            }
        } 
    }
  else
    {
      unsigned int driver_type;

      if (config->driver_type == IPMI_MONITORING_DRIVER_TYPE_KCS)
        driver_type = IPMI_DEVICE_KCS;
      else if (config->driver_type == IPMI_MONITORING_DRIVER_TYPE_SSIF)
        driver_type = IPMI_DEVICE_SSIF;
      else
        driver_type = IPMI_DEVICE_OPENIPMI;

      if (ipmi_open_inband (c->comm.dev,
                            driver_type,
                            (config) ? config->disable_auto_probe : 0,
                            (config) ? config->driver_address : 0,
                            (config) ? config->register_spacing : 0,
                            (config) ? config->driver_device : NULL,
                            workaround_flags,
                            flags) < 0)
        {
          IPMI_MONITORING_DEBUG(("ipmi_cmd: %s", ipmi_device_strerror(ipmi_device_errnum(c->comm.dev))));

          if (ipmi_device_errnum(c->comm.dev) == IPMI_ERR_BMC_BUSY)
            c->errnum = IPMI_MONITORING_ERR_BMC_BUSY;
          else if (ipmi_device_errnum(c->comm.dev) == IPMI_ERR_BAD_COMPLETION_CODE_INVALID_COMMAND
                   || ipmi_device_errnum(c->comm.dev) == IPMI_ERR_BAD_COMPLETION_CODE_REQUEST_DATA_INVALID
                   || ipmi_device_errnum(c->comm.dev) == IPMI_ERR_BAD_COMPLETION_CODE
                   || ipmi_device_errnum(c->comm.dev) == IPMI_ERR_IPMI_ERROR)
            c->errnum = IPMI_MONITORING_ERR_IPMI_ERROR;
          else if (ipmi_device_errnum(c->comm.dev) == IPMI_ERR_OUT_OF_MEMORY)
            c->errnum = IPMI_MONITORING_ERR_OUT_OF_MEMORY;
          else if (ipmi_device_errnum(c->comm.dev) == IPMI_ERR_SYSTEM_ERROR)
            c->errnum = IPMI_MONITORING_ERR_SYSTEM_ERROR;
          else if (ipmi_device_errnum(c->comm.dev) == IPMI_ERR_DRIVER_PATH_REQUIRED)
            c->errnum = IPMI_MONITORING_ERR_PARAMETERS;
          else
            c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
          return -1;
        }
    }

  return 0;
}
             
static int
_ipmi_1_5_init(ipmi_monitoring_ctx_t c, 
               const char *hostname,
               struct ipmi_monitoring_ipmi_config *config)
{
  uint8_t privilege_level;
  uint8_t authentication_type;
  unsigned int session_timeout_len;
  unsigned int retransmission_timeout_len;
  unsigned int workaround_flags;
  unsigned int flags;
  unsigned int workaround_flags_mask = (IPMI_MONITORING_WORKAROUND_FLAGS_ACCEPT_SESSION_ID_ZERO
                                        | IPMI_MONITORING_WORKAROUND_FLAGS_FORCE_PERMSG_AUTHENTICATION
                                        | IPMI_MONITORING_WORKAROUND_FLAGS_CHECK_UNEXPECTED_AUTHCODE
                                        | IPMI_MONITORING_WORKAROUND_FLAGS_BIG_ENDIAN_SEQUENCE_NUMBER);

  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);
  assert(c->comm.dev);
  assert(hostname);

  if (strlen(hostname) > MAXHOSTNAMELEN
      || (config
          && ((config->username && strlen(config->username) > IPMI_MAX_USER_NAME_LENGTH)
              || (config->password && strlen(config->password) > IPMI_1_5_MAX_PASSWORD_LENGTH)
              || (config->privilege_level >= 0
                  && (config->privilege_level != IPMI_MONITORING_PRIVILEGE_USER
                      && config->privilege_level != IPMI_MONITORING_PRIVILEGE_OPERATOR
                      && config->privilege_level != IPMI_MONITORING_PRIVILEGE_ADMIN))
              || (config->authentication_type >= 0
                  && (config->authentication_type != IPMI_MONITORING_AUTHENTICATION_TYPE_NONE
                      && config->authentication_type != IPMI_MONITORING_AUTHENTICATION_TYPE_STRAIGHT_PASSWORD_KEY
                      && config->authentication_type != IPMI_MONITORING_AUTHENTICATION_TYPE_MD2
                      && config->authentication_type != IPMI_MONITORING_AUTHENTICATION_TYPE_MD5))
              || (config->workaround_flags & ~workaround_flags_mask))))
    {
      c->errnum = IPMI_MONITORING_ERR_PARAMETERS;
      return -1;
    }

  if (config && config->privilege_level >= 0)
    {
      if (config->privilege_level == IPMI_MONITORING_PRIVILEGE_USER)
        privilege_level = IPMI_PRIVILEGE_LEVEL_USER;
      else if (config->privilege_level == IPMI_MONITORING_PRIVILEGE_OPERATOR)
        privilege_level = IPMI_PRIVILEGE_LEVEL_OPERATOR;
      else
        privilege_level = IPMI_PRIVILEGE_LEVEL_ADMIN;
    }
  else
    privilege_level = IPMI_MONITORING_PRIVILEGE_LEVEL_DEFAULT;

  if (config && config->authentication_type >= 0)
    {
      if (config->authentication_type == IPMI_MONITORING_AUTHENTICATION_TYPE_NONE)
        authentication_type = IPMI_AUTHENTICATION_TYPE_NONE;
      else if (config->authentication_type == IPMI_MONITORING_AUTHENTICATION_TYPE_STRAIGHT_PASSWORD_KEY)
        authentication_type = IPMI_AUTHENTICATION_TYPE_STRAIGHT_PASSWORD_KEY;
      else if (config->authentication_type == IPMI_MONITORING_AUTHENTICATION_TYPE_MD2)
        authentication_type = IPMI_AUTHENTICATION_TYPE_MD2;
      else
        authentication_type = IPMI_AUTHENTICATION_TYPE_MD5;
    }
  else
    authentication_type = IPMI_MONITORING_AUTHENTICATION_TYPE_DEFAULT;

  if (config && config->session_timeout_len > 0)
    session_timeout_len = config->session_timeout_len;
  else
    session_timeout_len = IPMI_MONITORING_SESSION_TIMEOUT_LENGTH_DEFAULT;

  if (config && config->retransmission_timeout_len > 0)
    retransmission_timeout_len = config->retransmission_timeout_len;
  else
    retransmission_timeout_len = IPMI_MONITORING_RETRANSMISSION_TIMEOUT_LENGTH_DEFAULT;

   if (retransmission_timeout_len > session_timeout_len)
     {
       c->errnum = IPMI_MONITORING_ERR_PARAMETERS;
       return -1;
     }

  workaround_flags = 0;
  if (config && config->workaround_flags)
    {
      if (config->workaround_flags & IPMI_MONITORING_WORKAROUND_FLAGS_ACCEPT_SESSION_ID_ZERO)
        workaround_flags |= IPMI_OUTOFBAND_WORKAROUND_FLAGS_ACCEPT_SESSION_ID_ZERO;
      else if (config->workaround_flags & IPMI_MONITORING_WORKAROUND_FLAGS_FORCE_PERMSG_AUTHENTICATION)
        workaround_flags |= IPMI_OUTOFBAND_WORKAROUND_FLAGS_FORCE_PERMSG_AUTHENTICATION;
      else if (config->workaround_flags & IPMI_MONITORING_WORKAROUND_FLAGS_CHECK_UNEXPECTED_AUTHCODE)
        workaround_flags |= IPMI_OUTOFBAND_WORKAROUND_FLAGS_CHECK_UNEXPECTED_AUTHCODE;
      else if (config->workaround_flags & IPMI_MONITORING_WORKAROUND_FLAGS_BIG_ENDIAN_SEQUENCE_NUMBER)
        workaround_flags |= IPMI_OUTOFBAND_WORKAROUND_FLAGS_BIG_ENDIAN_SEQUENCE_NUMBER;
    }
  
  if ((_ipmi_monitoring_flags & IPMI_MONITORING_FLAGS_DEBUG)
      && (_ipmi_monitoring_flags & IPMI_MONITORING_FLAGS_DEBUG_IPMI_PACKETS))
    flags = IPMI_FLAGS_DEBUG_DUMP;
  else
    flags = IPMI_FLAGS_DEFAULT;
  
  if (ipmi_open_outofband (c->comm.dev,
                           IPMI_DEVICE_LAN,
                           hostname,
                           (config) ? config->username : NULL,
                           (config) ? config->password : NULL,
                           authentication_type,
                           privilege_level,
                           session_timeout_len,
                           retransmission_timeout_len,
                           workaround_flags,
                           flags) < 0)
    {
      IPMI_MONITORING_DEBUG(("ipmi_open_outofband: %s", ipmi_device_strerror(ipmi_device_errnum(c->comm.dev))));
      if (ipmi_device_errnum(c->comm.dev) == IPMI_ERR_USERNAME_INVALID)
        c->errnum = IPMI_MONITORING_ERR_USERNAME_INVALID;
      else if (ipmi_device_errnum(c->comm.dev) == IPMI_ERR_PASSWORD_INVALID)
        c->errnum = IPMI_MONITORING_ERR_PASSWORD_INVALID;
      else if (ipmi_device_errnum(c->comm.dev) == IPMI_ERR_PRIVILEGE_LEVEL_INSUFFICIENT)
        c->errnum = IPMI_MONITORING_ERR_PRIVILEGE_LEVEL_INSUFFICIENT;
      else if (ipmi_device_errnum(c->comm.dev) == IPMI_ERR_AUTHENTICATION_TYPE_UNAVAILABLE)
        c->errnum = IPMI_MONITORING_ERR_AUTHENTICATION_TYPE_UNAVAILABLE;
      else if (ipmi_device_errnum(c->comm.dev) == IPMI_ERR_PASSWORD_VERIFICATION_TIMEOUT)
        c->errnum = IPMI_MONITORING_ERR_PASSWORD_VERIFICATION_TIMEOUT;
      else if (ipmi_device_errnum(c->comm.dev) == IPMI_ERR_SESSION_TIMEOUT)
        c->errnum = IPMI_MONITORING_ERR_SESSION_TIMEOUT;
      else if (ipmi_device_errnum(c->comm.dev) == IPMI_ERR_BAD_COMPLETION_CODE_INVALID_COMMAND
               || ipmi_device_errnum(c->comm.dev) == IPMI_ERR_BAD_COMPLETION_CODE_REQUEST_DATA_INVALID
               || ipmi_device_errnum(c->comm.dev) == IPMI_ERR_BAD_COMPLETION_CODE
               || ipmi_device_errnum(c->comm.dev) == IPMI_ERR_IPMI_ERROR)
        c->errnum = IPMI_MONITORING_ERR_IPMI_ERROR;
      else if (ipmi_device_errnum(c->comm.dev) == IPMI_ERR_BMC_BUSY)
        c->errnum = IPMI_MONITORING_ERR_BMC_BUSY;
      else if (ipmi_device_errnum(c->comm.dev) == IPMI_ERR_BAD_COMPLETION_CODE_INSUFFICIENT_PRIVILEGE)
        c->errnum = IPMI_MONITORING_ERR_PRIVILEGE_LEVEL_INSUFFICIENT;
      else if (ipmi_device_errnum(c->comm.dev) == IPMI_ERR_OUT_OF_MEMORY)
        c->errnum = IPMI_MONITORING_ERR_OUT_OF_MEMORY;
      else if (ipmi_device_errnum(c->comm.dev) == IPMI_ERR_HOSTNAME_INVALID)
        c->errnum = IPMI_MONITORING_ERR_HOSTNAME_INVALID;
      else if (ipmi_device_errnum(c->comm.dev) == IPMI_ERR_PARAMETERS)
        c->errnum = IPMI_MONITORING_ERR_PARAMETERS;
      else if (ipmi_device_errnum(c->comm.dev) == IPMI_ERR_SYSTEM_ERROR)
        c->errnum = IPMI_MONITORING_ERR_SYSTEM_ERROR;
      else
        c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
      return -1;
    }

  return 0;
}

static int
_ipmi_2_0_init(ipmi_monitoring_ctx_t c, 
               const char *hostname,
               struct ipmi_monitoring_ipmi_config *config)
{
  uint8_t privilege_level;
  uint8_t cipher_suite_id;
  unsigned int session_timeout_len;
  unsigned int retransmission_timeout_len;
  unsigned int workaround_flags;
  unsigned int flags;
  unsigned workaround_flags_mask = (IPMI_MONITORING_WORKAROUND_FLAGS_INTEL_2_0_SESSION
                                    | IPMI_MONITORING_WORKAROUND_FLAGS_SUPERMICRO_2_0_SESSION
                                    | IPMI_MONITORING_WORKAROUND_FLAGS_SUN_2_0_SESSION);
  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);
  assert(c->comm.dev);
  assert(hostname);

  if (strlen(hostname) > MAXHOSTNAMELEN
      || (config
          && ((config->username && strlen(config->username) > IPMI_MAX_USER_NAME_LENGTH)
              || (config->password && strlen(config->password) > IPMI_2_0_MAX_PASSWORD_LENGTH)
              || (config->k_g && config->k_g_len > IPMI_MAX_K_G_LENGTH)
              || (config->privilege_level >= 0
                  && (config->privilege_level != IPMI_MONITORING_PRIVILEGE_USER
                      && config->privilege_level != IPMI_MONITORING_PRIVILEGE_OPERATOR
                      && config->privilege_level != IPMI_MONITORING_PRIVILEGE_ADMIN))
              || (config->cipher_suite_id >= 0
                  && (config->cipher_suite_id != 0
                      && config->cipher_suite_id != 1
                      && config->cipher_suite_id != 2
                      && config->cipher_suite_id != 3
                      && config->cipher_suite_id != 6
                      && config->cipher_suite_id != 7
                      && config->cipher_suite_id != 8
                      && config->cipher_suite_id != 11
                      && config->cipher_suite_id != 12))
              || (config->workaround_flags & ~workaround_flags_mask))))
    {
      c->errnum = IPMI_MONITORING_ERR_PARAMETERS;
      return -1;
    }

  if (config && config->privilege_level >= 0)
    {
      if (config->privilege_level == IPMI_MONITORING_PRIVILEGE_USER)
        privilege_level = IPMI_PRIVILEGE_LEVEL_USER;
      else if (config->privilege_level == IPMI_MONITORING_PRIVILEGE_OPERATOR)
        privilege_level = IPMI_PRIVILEGE_LEVEL_OPERATOR;
      else
        privilege_level = IPMI_PRIVILEGE_LEVEL_ADMIN;
    }
  else
    privilege_level = IPMI_MONITORING_PRIVILEGE_LEVEL_DEFAULT;

  if (config && config->cipher_suite_id > 0)
    cipher_suite_id = config->cipher_suite_id;
  else
    cipher_suite_id = 3;

  if (config && config->session_timeout_len > 0)
    session_timeout_len = config->session_timeout_len;
  else
    session_timeout_len = IPMI_MONITORING_SESSION_TIMEOUT_LENGTH_DEFAULT;

  if (config && config->retransmission_timeout_len > 0)
    retransmission_timeout_len = config->retransmission_timeout_len;
  else
    retransmission_timeout_len = IPMI_MONITORING_RETRANSMISSION_TIMEOUT_LENGTH_DEFAULT;

   if (retransmission_timeout_len > session_timeout_len)
     {
       c->errnum = IPMI_MONITORING_ERR_PARAMETERS;
       return -1;
     }

  workaround_flags = 0;
  if (config && config->workaround_flags)
    {
      if (config->workaround_flags & IPMI_MONITORING_WORKAROUND_FLAGS_INTEL_2_0_SESSION)
        workaround_flags |= IPMI_OUTOFBAND_2_0_WORKAROUND_FLAGS_INTEL_2_0_SESSION;
      else if (config->workaround_flags & IPMI_MONITORING_WORKAROUND_FLAGS_SUPERMICRO_2_0_SESSION)
        workaround_flags |= IPMI_OUTOFBAND_2_0_WORKAROUND_FLAGS_SUPERMICRO_2_0_SESSION;
      else if (config->workaround_flags & IPMI_MONITORING_WORKAROUND_FLAGS_SUN_2_0_SESSION)
        workaround_flags |= IPMI_OUTOFBAND_2_0_WORKAROUND_FLAGS_SUN_2_0_SESSION;
    }
  
  if ((_ipmi_monitoring_flags & IPMI_MONITORING_FLAGS_DEBUG)
      && (_ipmi_monitoring_flags & IPMI_MONITORING_FLAGS_DEBUG_IPMI_PACKETS))
    flags = IPMI_FLAGS_DEBUG_DUMP;
  else
    flags = IPMI_FLAGS_DEFAULT;

  if (ipmi_open_outofband_2_0 (c->comm.dev,
                               IPMI_DEVICE_LAN_2_0,
                               hostname,
                               (config) ? config->username : NULL,
                               (config) ? config->password : NULL,
                               (config) ? config->k_g : NULL,
                               (config && config->k_g) ? config->k_g_len : 0,
                               privilege_level,
                               cipher_suite_id,
                               session_timeout_len,
                               retransmission_timeout_len,
                               workaround_flags,
                               flags) < 0)
    {
      IPMI_MONITORING_DEBUG(("ipmi_open_outofband_2_0: %s", ipmi_device_strerror(ipmi_device_errnum(c->comm.dev))));
      if (ipmi_device_errnum(c->comm.dev) == IPMI_ERR_USERNAME_INVALID)
        c->errnum = IPMI_MONITORING_ERR_USERNAME_INVALID;
      else if (ipmi_device_errnum(c->comm.dev) == IPMI_ERR_PASSWORD_INVALID)
        c->errnum = IPMI_MONITORING_ERR_PASSWORD_INVALID;
      else if (ipmi_device_errnum(c->comm.dev) == IPMI_ERR_PRIVILEGE_LEVEL_INSUFFICIENT)
        c->errnum = IPMI_MONITORING_ERR_PRIVILEGE_LEVEL_INSUFFICIENT;
      else if (ipmi_device_errnum(c->comm.dev) == IPMI_ERR_K_G_INVALID)
        c->errnum = IPMI_MONITORING_ERR_K_G_INVALID;
      else if (ipmi_device_errnum(c->comm.dev) == IPMI_ERR_CIPHER_SUITE_ID_UNAVAILABLE)
        c->errnum = IPMI_MONITORING_ERR_CIPHER_SUITE_ID_UNAVAILABLE;
      else if (ipmi_device_errnum(c->comm.dev) == IPMI_ERR_PASSWORD_VERIFICATION_TIMEOUT)
        c->errnum = IPMI_MONITORING_ERR_PASSWORD_VERIFICATION_TIMEOUT;
      else if (ipmi_device_errnum(c->comm.dev) == IPMI_ERR_IPMI_2_0_UNAVAILABLE)
        c->errnum = IPMI_MONITORING_ERR_IPMI_2_0_UNAVAILABLE;
      else if (ipmi_device_errnum(c->comm.dev) == IPMI_ERR_SESSION_TIMEOUT)
        c->errnum = IPMI_MONITORING_ERR_SESSION_TIMEOUT;
      else if (ipmi_device_errnum(c->comm.dev) == IPMI_ERR_BAD_COMPLETION_CODE_INVALID_COMMAND
               || ipmi_device_errnum(c->comm.dev) == IPMI_ERR_BAD_COMPLETION_CODE_REQUEST_DATA_INVALID
               || ipmi_device_errnum(c->comm.dev) == IPMI_ERR_BAD_COMPLETION_CODE
               || ipmi_device_errnum(c->comm.dev) == IPMI_ERR_IPMI_ERROR)
        c->errnum = IPMI_MONITORING_ERR_IPMI_ERROR;
      else if (ipmi_device_errnum(c->comm.dev) == IPMI_ERR_BMC_BUSY)
        c->errnum = IPMI_MONITORING_ERR_BMC_BUSY;
      else if (ipmi_device_errnum(c->comm.dev) == IPMI_ERR_BAD_COMPLETION_CODE_INSUFFICIENT_PRIVILEGE)
        c->errnum = IPMI_MONITORING_ERR_PRIVILEGE_LEVEL_INSUFFICIENT;
      else if (ipmi_device_errnum(c->comm.dev) == IPMI_ERR_OUT_OF_MEMORY)
        c->errnum = IPMI_MONITORING_ERR_OUT_OF_MEMORY;
      else if (ipmi_device_errnum(c->comm.dev) == IPMI_ERR_HOSTNAME_INVALID)
        c->errnum = IPMI_MONITORING_ERR_HOSTNAME_INVALID;
      else if (ipmi_device_errnum(c->comm.dev) == IPMI_ERR_PARAMETERS)
        c->errnum = IPMI_MONITORING_ERR_PARAMETERS;
      else if (ipmi_device_errnum(c->comm.dev) == IPMI_ERR_SYSTEM_ERROR)
        c->errnum = IPMI_MONITORING_ERR_SYSTEM_ERROR;
      else
        c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
      return -1;
    }

  return 0;
}

static int
_outofband_init(ipmi_monitoring_ctx_t c, 
                const char *hostname,
                struct ipmi_monitoring_ipmi_config *config)
{
  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);
  assert(c->comm.dev);
  assert(hostname);

  if (strlen(hostname) > MAXHOSTNAMELEN
      || (config
          && ((config->protocol_version >= 0
               && (config->protocol_version != IPMI_MONITORING_PROTOCOL_VERSION_1_5
                   && config->protocol_version != IPMI_MONITORING_PROTOCOL_VERSION_2_0)))))
    {
      c->errnum = IPMI_MONITORING_ERR_PARAMETERS;
      return -1;
    }

  if (!config 
      || config->protocol_version < 0
      || config->protocol_version == IPMI_MONITORING_PROTOCOL_VERSION_1_5)
    {
      if (_ipmi_1_5_init(c,
                         hostname,
                         config) < 0)
        return -1;
    }
  else
    {
      if (_ipmi_2_0_init(c,
                         hostname,
                         config) < 0)
        return -1;
    }

  return 0;
}

int 
ipmi_monitoring_ipmi_communication_init(ipmi_monitoring_ctx_t c,
                                        const char *hostname,
                                        struct ipmi_monitoring_ipmi_config *config)
{

  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);
  assert(!c->comm.dev);

  memset(&(c->comm), '\0', sizeof(struct ipmi_monitoring_communication));
 
  if (!(c->comm.dev = ipmi_device_create()))
    {
      IPMI_MONITORING_DEBUG(("ipmi_device_create: %s", strerror(errno)));
      c->errnum = IPMI_MONITORING_ERR_OUT_OF_MEMORY;
      goto cleanup;
    }
  
  if (!hostname)
    {
      if (_inband_init(c, config) < 0)
        goto cleanup;
    }
  else
    {
      if (_outofband_init(c, hostname, config) < 0)
        goto cleanup;
    }

  return 0;

 cleanup:
  _ipmi_communication_cleanup(c);
  return -1;
}

int 
ipmi_monitoring_ipmi_sendrecv(ipmi_monitoring_ctx_t c,
                              uint8_t lun,
                              uint8_t net_fn,
                              fiid_obj_t obj_cmd_rq,
                              fiid_obj_t obj_cmd_rs)
{
  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);
  assert(c->comm.dev);
  assert(fiid_obj_valid(obj_cmd_rq));
  assert(fiid_obj_valid(obj_cmd_rs));

  if (ipmi_cmd (c->comm.dev,
                lun,
                net_fn,
                obj_cmd_rq,
                obj_cmd_rs) < 0)
    {
      IPMI_MONITORING_DEBUG(("ipmi_cmd: %s", ipmi_device_strerror(ipmi_device_errnum(c->comm.dev))));
      if (ipmi_device_errnum(c->comm.dev) == IPMI_ERR_SESSION_TIMEOUT)
        c->errnum = IPMI_MONITORING_ERR_SESSION_TIMEOUT;
      else if (ipmi_device_errnum(c->comm.dev) == IPMI_ERR_BAD_COMPLETION_CODE_INVALID_COMMAND
               || ipmi_device_errnum(c->comm.dev) == IPMI_ERR_BAD_COMPLETION_CODE_REQUEST_DATA_INVALID
               || ipmi_device_errnum(c->comm.dev) == IPMI_ERR_BAD_COMPLETION_CODE
               || ipmi_device_errnum(c->comm.dev) == IPMI_ERR_IPMI_ERROR)
        c->errnum = IPMI_MONITORING_ERR_IPMI_ERROR;
      else if (ipmi_device_errnum(c->comm.dev) == IPMI_ERR_BMC_BUSY)
        c->errnum = IPMI_MONITORING_ERR_BMC_BUSY;
      else if (ipmi_device_errnum(c->comm.dev) == IPMI_ERR_BAD_COMPLETION_CODE_INSUFFICIENT_PRIVILEGE)
        c->errnum = IPMI_MONITORING_ERR_PRIVILEGE_LEVEL_INSUFFICIENT;
      else if (ipmi_device_errnum(c->comm.dev) == IPMI_ERR_OUT_OF_MEMORY)
        c->errnum = IPMI_MONITORING_ERR_OUT_OF_MEMORY;
      else if (ipmi_device_errnum(c->comm.dev) == IPMI_ERR_SYSTEM_ERROR)
        c->errnum = IPMI_MONITORING_ERR_SYSTEM_ERROR;
      else
        c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
      return -1;
    }

  return 0;
}

int 
ipmi_monitoring_ipmi_communication_cleanup(ipmi_monitoring_ctx_t c)
{
  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);

  _ipmi_communication_cleanup(c);
  return 0;
}
