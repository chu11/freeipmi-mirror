/*
 * Copyright (C) 2003-2014 FreeIPMI Core Team
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */

/* 2's complement checksum of preceding bytes in the connection header
   or between the previous checksum. 8-bit checksum algorithm:
   Initialize checksum to 0.
   For each byte, checksum = (checksum + byte) modulo 256. Then find
   1's compliment of checksum and add one to it.
   To verify add all the bytes and the checksum and then % 256 should
   yield 0.
*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#ifdef STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#if HAVE_FCNTL_H
#include <fcntl.h>
#endif /* HAVE_FCNTL_H */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */

#include "freeipmi/util/ipmi-outofband-util.h"
#include "freeipmi/fiid/fiid.h"
#include "freeipmi/cmds/ipmi-messaging-support-cmds.h"
#include "freeipmi/interface/ipmi-rmcpplus-interface.h"
#include "freeipmi/interface/rmcp-interface.h"
#include "freeipmi/spec/ipmi-authentication-type-spec.h"
#include "freeipmi/spec/ipmi-privilege-level-spec.h"

#include "libcommon/ipmi-fiid-util.h"
#include "libcommon/ipmi-trace.h"

#include "freeipmi-portability.h"

#define IPMI_SEQUENCE_NUMBER_MAX            0xFFFFFFFF
#define IPMI_SEQUENCE_NUMBER_WINDOW_DEFAULT          8
#define IPMI_SEQUENCE_NUMBER_WINDOW_MAX             32
#define IPMI_SEQUENCE_NUMBER_WINDOW_MIN              1

int
ipmi_is_ipmi_1_5_packet (const void *pkt, unsigned int pkt_len)
{
  int rmcp_hdr_len;
  uint8_t auth_type;

  if ((rmcp_hdr_len = fiid_template_len_bytes (tmpl_rmcp_hdr)) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  if (pkt_len <= rmcp_hdr_len)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  auth_type = *((uint8_t *)(pkt + rmcp_hdr_len));
  auth_type &= 0x0F;
  return ((auth_type != IPMI_AUTHENTICATION_TYPE_RMCPPLUS) ? 1 : 0);
}

int
ipmi_is_ipmi_2_0_packet (const void *pkt, unsigned int pkt_len)
{
  int rmcp_hdr_len;
  uint8_t auth_type;

  if ((rmcp_hdr_len = fiid_template_len_bytes (tmpl_rmcp_hdr)) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  if (pkt_len <= rmcp_hdr_len)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  auth_type = *((uint8_t *)(pkt + rmcp_hdr_len));
  auth_type &= 0x0F;
  return ((auth_type == IPMI_AUTHENTICATION_TYPE_RMCPPLUS) ? 1 : 0);
}

int
ipmi_check_session_sequence_number_1_5_init (uint32_t *highest_received_sequence_number,
                                             uint32_t *previously_received_list)
{
  if (!highest_received_sequence_number
      || !previously_received_list)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  (*highest_received_sequence_number) = 0;
  (*previously_received_list) = 0xFF;

  return (0);
}

int
ipmi_check_session_sequence_number_2_0_init (uint32_t *highest_received_sequence_number,
                                             uint32_t *previously_received_list)
{
  if (!highest_received_sequence_number
      || !previously_received_list)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  (*highest_received_sequence_number) = 0;
  (*previously_received_list) = 0xFF;

  return (0);
}

static int
_check_session_sequence_number (uint32_t session_sequence_number,
                                uint32_t *highest_received_sequence_number,
                                uint32_t *previously_received_list,
                                unsigned int sequence_number_window,
                                int ipmi_2_0_flag)
{
  uint32_t shift_num, wrap_val;

  if (!highest_received_sequence_number
      || !previously_received_list)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (sequence_number_window > IPMI_SEQUENCE_NUMBER_WINDOW_MAX)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (!sequence_number_window)
    sequence_number_window = IPMI_SEQUENCE_NUMBER_WINDOW_DEFAULT;

  /* achu: This algorithm is more or less from Appendix A of the IPMI
   * spec.  I know that technically I could remove a lot of code here
   * if I just let unsigned ints wrap around.  I dunno, I like to see
   * all of the code actually written out b/c it makes more sense to
   * the casual code reviewer.  Maybe I'll change it later.
   */

  /* Drop duplicate packet */
  if (session_sequence_number == (*highest_received_sequence_number))
    return (0);

  /* In IPMI 2.0, sequence number 0 is special, and shouldn't happen */
  if (ipmi_2_0_flag && !session_sequence_number)
    return (0);

  /* Check if sequence number is greater than highest received and is
   * within range
   */
  if ((*highest_received_sequence_number) > (IPMI_SEQUENCE_NUMBER_MAX - IPMI_SEQUENCE_NUMBER_WINDOW_DEFAULT))
    {
      wrap_val = IPMI_SEQUENCE_NUMBER_WINDOW_DEFAULT - (IPMI_SEQUENCE_NUMBER_MAX - (*highest_received_sequence_number)) - 1;

      /* In IPMI 2.0, sequence number 0 isn't possible, so adjust wrap_val */
      if (ipmi_2_0_flag)
        wrap_val++;

      if ((session_sequence_number > (*highest_received_sequence_number))
          || (session_sequence_number <= wrap_val))
        {
          if (session_sequence_number > (*highest_received_sequence_number) && session_sequence_number <= IPMI_SEQUENCE_NUMBER_MAX)
            shift_num = session_sequence_number - (*highest_received_sequence_number);
          else
            {
              /* IPMI 2.0 Special Case b/c 0 isn't a legit sequence number */
              if (ipmi_2_0_flag)
                shift_num = session_sequence_number + (IPMI_SEQUENCE_NUMBER_MAX - (*highest_received_sequence_number));
              else
                shift_num = session_sequence_number + (IPMI_SEQUENCE_NUMBER_MAX - (*highest_received_sequence_number)) + 1;
            }

          (*highest_received_sequence_number) = session_sequence_number;
          (*previously_received_list) <<= shift_num;
          (*previously_received_list) |= (0x1 << (shift_num - 1));
          return (1);
        }
    }
  else
    {
      if (session_sequence_number > (*highest_received_sequence_number)
          && (session_sequence_number - (*highest_received_sequence_number)) <= IPMI_SEQUENCE_NUMBER_WINDOW_DEFAULT)
        {
          shift_num = (session_sequence_number - (*highest_received_sequence_number));
          (*highest_received_sequence_number) = session_sequence_number;
          (*previously_received_list) <<= shift_num;
          (*previously_received_list) |= (0x1 << (shift_num - 1));
          return (1);
        }
    }

  /* Check if sequence number is lower than highest received, is
   * within range, and hasn't been seen yet
   */
  if ((*highest_received_sequence_number) < IPMI_SEQUENCE_NUMBER_WINDOW_DEFAULT)
    {
      wrap_val = IPMI_SEQUENCE_NUMBER_MAX - (IPMI_SEQUENCE_NUMBER_WINDOW_DEFAULT - (*highest_received_sequence_number)) + 1;

      /* In IPMI 2.0, sequence number 0 isn't possible, so adjust wrap_val */
      if (ipmi_2_0_flag)
        wrap_val--;

      if (session_sequence_number < (*highest_received_sequence_number) || session_sequence_number >= wrap_val)
        {
          if (session_sequence_number > (*highest_received_sequence_number) && session_sequence_number <= IPMI_SEQUENCE_NUMBER_MAX)
            {
              /* IPMI 2.0 Special Case b/c 0 isn't a legit sequence number */
              if (ipmi_2_0_flag)
                shift_num = (*highest_received_sequence_number) + (IPMI_SEQUENCE_NUMBER_MAX - session_sequence_number);
              else
                shift_num = (*highest_received_sequence_number) + (IPMI_SEQUENCE_NUMBER_MAX - session_sequence_number) + 1;
            }
          else
            shift_num = (*highest_received_sequence_number) - session_sequence_number;

          /* Duplicate packet check*/
          if ((*previously_received_list) & (0x1 << (shift_num - 1)))
            return (0);

          (*previously_received_list) |= (0x1 << (shift_num - 1));
          return (1);
        }
    }
  else
    {
      if (session_sequence_number < (*highest_received_sequence_number)
          && session_sequence_number >= ((*highest_received_sequence_number) - IPMI_SEQUENCE_NUMBER_WINDOW_DEFAULT))
        {
          shift_num = (*highest_received_sequence_number) - session_sequence_number;

          /* Duplicate packet check*/
          if ((*previously_received_list) & (0x1 << (shift_num - 1)))
            return (0);

          (*previously_received_list) |= (0x1 << (shift_num - 1));
          return (1);
        }
    }
  
  return (0);
}

int
ipmi_check_session_sequence_number_1_5 (uint32_t session_sequence_number,
                                        uint32_t *highest_received_sequence_number,
                                        uint32_t *previously_received_list,
                                        unsigned int sequence_number_window)
{
  return (_check_session_sequence_number (session_sequence_number,
                                          highest_received_sequence_number,
                                          previously_received_list,
                                          sequence_number_window,
                                          0));
}

int
ipmi_check_session_sequence_number_2_0 (uint32_t session_sequence_number,
                                        uint32_t *highest_received_sequence_number,
                                        uint32_t *previously_received_list,
                                        unsigned int sequence_number_window)
{
  return (_check_session_sequence_number (session_sequence_number,
                                          highest_received_sequence_number,
                                          previously_received_list,
                                          sequence_number_window,
                                          1));
}

int
ipmi_check_authentication_capabilities_authentication_type (uint8_t authentication_type,
                                                            fiid_obj_t obj_cmd)
{
  uint8_t supported_authentication_type = 0;
  uint64_t val;

  if (!IPMI_1_5_AUTHENTICATION_TYPE_VALID (authentication_type)
      || !fiid_obj_valid (obj_cmd))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd, tmpl_cmd_get_channel_authentication_capabilities_rs) < 0)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  switch (authentication_type)
    {
    case IPMI_AUTHENTICATION_TYPE_NONE:
      if (FIID_OBJ_GET (obj_cmd,
                        "authentication_type.none",
                        &val) < 0)
        {
          FIID_OBJECT_ERROR_TO_ERRNO (obj_cmd);
          return (-1);
        }
      supported_authentication_type = val;
      break;
    case IPMI_AUTHENTICATION_TYPE_MD2:
      if (FIID_OBJ_GET (obj_cmd,
                        "authentication_type.md2",
                        &val) < 0)
        {
          FIID_OBJECT_ERROR_TO_ERRNO (obj_cmd);
          return (-1);
        }
      supported_authentication_type = val;
      break;
    case IPMI_AUTHENTICATION_TYPE_MD5:
      if (FIID_OBJ_GET (obj_cmd,
                        "authentication_type.md5",
                        &val) < 0)
        {
          FIID_OBJECT_ERROR_TO_ERRNO (obj_cmd);
          return (-1);
        }
      supported_authentication_type = val;
      break;
    case IPMI_AUTHENTICATION_TYPE_STRAIGHT_PASSWORD_KEY:
      if (FIID_OBJ_GET (obj_cmd,
                        "authentication_type.straight_password_key",
                        &val) < 0)
        {
          FIID_OBJECT_ERROR_TO_ERRNO (obj_cmd);
          return (-1);
        }
      supported_authentication_type = val;
      break;
    case IPMI_AUTHENTICATION_TYPE_OEM_PROP:
      if (FIID_OBJ_GET (obj_cmd,
                        "authentication_type.oem_prop",
                        &val) < 0)
        {
          FIID_OBJECT_ERROR_TO_ERRNO (obj_cmd);
          return (-1);
        }
      supported_authentication_type = val;
      break;
    }

  if (supported_authentication_type)
    return (1);

  return (0);
}

int
ipmi_check_authentication_capabilities_username (const char *username,
                                                 const char *password,
                                                 fiid_obj_t obj_cmd)
{
  uint8_t authentication_status_anonymous_login;
  uint8_t authentication_status_null_username;
  uint8_t authentication_status_non_null_username;
  uint64_t val;

  if (!fiid_obj_valid (obj_cmd))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd, tmpl_cmd_get_channel_authentication_capabilities_rs) < 0)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_GET (obj_cmd,
                    "authentication_status.anonymous_login",
                    &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_cmd);
      return (-1);
    }
  authentication_status_anonymous_login = val;

  if (FIID_OBJ_GET (obj_cmd,
                    "authentication_status.null_username",
                    &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_cmd);
      return (-1);
    }
  authentication_status_null_username = val;

  if (FIID_OBJ_GET (obj_cmd,
                    "authentication_status.non_null_username",
                    &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_cmd);
      return (-1);
    }
  authentication_status_non_null_username = val;

  if ((!username && !password && !authentication_status_anonymous_login)
      || (!username && password && !authentication_status_null_username)
      || (username  && !authentication_status_non_null_username))
    return (0);

  return (1);
}

int
ipmi_check_authentication_capabilities_ipmi_2_0 (fiid_obj_t obj_cmd)
{
  uint8_t ipmi_v20_extended_capabilities_available;
  uint8_t channel_supports_ipmi_v20_connections;
  uint64_t val;

  if (!fiid_obj_valid (obj_cmd))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd, tmpl_cmd_get_channel_authentication_capabilities_rs) < 0)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_GET (obj_cmd,
                    "authentication_type.ipmi_v2.0_extended_capabilities_available",
                    &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_cmd);
      return (-1);
    }
  ipmi_v20_extended_capabilities_available = val;
  
  if (FIID_OBJ_GET (obj_cmd,
                    "channel_supports_ipmi_v2.0_connections",
                    &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_cmd);
      return (-1);
    }
  channel_supports_ipmi_v20_connections = val;

  if (!ipmi_v20_extended_capabilities_available
      || !channel_supports_ipmi_v20_connections)
    return (0);

  return (1);
}

int
ipmi_check_authentication_capabilities_k_g (const void *k_g,
                                            fiid_obj_t obj_cmd)
{
  uint8_t authentication_status_k_g;
  uint64_t val;

  if (!fiid_obj_valid (obj_cmd))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd, tmpl_cmd_get_channel_authentication_capabilities_rs) < 0)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_GET (obj_cmd,
                    "authentication_status.k_g",
                    &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_cmd);
      return (-1);
    }
  authentication_status_k_g = val;

  if ((!k_g && authentication_status_k_g)
      || (k_g && !authentication_status_k_g))
    return (0);

  return (1);
}

int
ipmi_check_open_session_maximum_privilege (uint8_t privilege_level,
                                           fiid_obj_t obj_cmd)
{
  uint8_t maximum_privilege_level;
  uint64_t val;

  if (!IPMI_PRIVILEGE_LEVEL_VALID (privilege_level)
      || !fiid_obj_valid (obj_cmd))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd, tmpl_rmcpplus_open_session_response) < 0)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_GET (obj_cmd,
                    "maximum_privilege_level",
                    &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_cmd);
      return (-1);
    }
  maximum_privilege_level = val;

  if (privilege_level == IPMI_PRIVILEGE_LEVEL_USER
      && (maximum_privilege_level == IPMI_PRIVILEGE_LEVEL_USER
          || maximum_privilege_level == IPMI_PRIVILEGE_LEVEL_OPERATOR
          || maximum_privilege_level == IPMI_PRIVILEGE_LEVEL_ADMIN
          || maximum_privilege_level == IPMI_PRIVILEGE_LEVEL_OEM))
    return (1);
  else if (privilege_level == IPMI_PRIVILEGE_LEVEL_OPERATOR
           && (maximum_privilege_level == IPMI_PRIVILEGE_LEVEL_OPERATOR
                   || maximum_privilege_level == IPMI_PRIVILEGE_LEVEL_ADMIN
               || maximum_privilege_level == IPMI_PRIVILEGE_LEVEL_OEM))
    return (1);
  else if (privilege_level == IPMI_PRIVILEGE_LEVEL_ADMIN
           && (maximum_privilege_level == IPMI_PRIVILEGE_LEVEL_ADMIN
               || maximum_privilege_level == IPMI_PRIVILEGE_LEVEL_OEM))
    return (1);
  else if (privilege_level == IPMI_PRIVILEGE_LEVEL_OEM
           && maximum_privilege_level == IPMI_PRIVILEGE_LEVEL_OEM)
    return (1);

  return (0);
}
