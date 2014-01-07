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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#ifdef STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <errno.h>

#include "freeipmi/util/ipmi-error-dcmi-util.h"
#include "freeipmi/util/ipmi-error-util.h"
#include "freeipmi/fiid/fiid.h"
#include "freeipmi/spec/ipmi-cmd-dcmi-spec.h"
#include "freeipmi/spec/ipmi-comp-code-dcmi-spec.h"
#include "freeipmi/spec/ipmi-netfn-spec.h"

#include "libcommon/ipmi-fiid-util.h"
#include "libcommon/ipmi-trace.h"

#include "freeipmi-portability.h"

#define SNPRINTF_RETURN(arg...)                 \
  do                                            \
    {                                           \
      snprintf (errstr, len, arg);              \
      return (0);                               \
    } while (0)

int
ipmi_completion_code_dcmi_strerror_r (uint8_t cmd,
                                      uint8_t netfn,
                                      uint8_t comp_code,
                                      char *errstr,
                                      size_t len)
{
  if (!errstr)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (netfn == IPMI_NET_FN_GROUP_EXTENSION_RQ
      || netfn == IPMI_NET_FN_GROUP_EXTENSION_RS)
    {
      switch (cmd)
        {
        case IPMI_CMD_DCMI_GET_POWER_LIMIT:
          switch (comp_code)
            {
            case IPMI_COMP_CODE_DCMI_NO_SET_POWER_LIMIT:
              SNPRINTF_RETURN (IPMI_COMP_CODE_DCMI_NO_SET_POWER_LIMIT_STR);
            }
          break;
        case IPMI_CMD_DCMI_SET_POWER_LIMIT:
          switch (comp_code)
            {
            case IPMI_COMP_CODE_DCMI_POWER_LIMIT_OUT_OF_RANGE:
              SNPRINTF_RETURN (IPMI_COMP_CODE_DCMI_POWER_LIMIT_OUT_OF_RANGE_STR);
            case IPMI_COMP_CODE_DCMI_CORRECTION_TIME_OUT_OF_RANGE:
              SNPRINTF_RETURN (IPMI_COMP_CODE_DCMI_CORRECTION_TIME_OUT_OF_RANGE_STR);
            case IPMI_COMP_CODE_DCMI_STATISTICS_REPORTING_PERIOD_OUT_OF_RANGE:
              SNPRINTF_RETURN (IPMI_COMP_CODE_DCMI_STATISTICS_REPORTING_PERIOD_OUT_OF_RANGE_STR);
            }
          break;
	case IPMI_CMD_DCMI_GET_ASSET_TAG:
	  switch (comp_code)
	    {
	    case IPMI_COMP_CODE_DCMI_ENCODING_TYPE_IN_FRU_IS_BINARY_UNSPECIFIED:
	      SNPRINTF_RETURN (IPMI_COMP_CODE_DCMI_ENCODING_TYPE_IN_FRU_IS_BINARY_UNSPECIFIED_STR);
	    case IPMI_COMP_CODE_DCMI_ENCODING_TYPE_IN_FRU_IS_BCD_PLUS:
	      SNPRINTF_RETURN (IPMI_COMP_CODE_DCMI_ENCODING_TYPE_IN_FRU_IS_BCD_PLUS_STR);
	    case IPMI_COMP_CODE_DCMI_ENCODING_TYPE_IN_FRU_IS_6BIT_ASCII_PACKED:
	      SNPRINTF_RETURN (IPMI_COMP_CODE_DCMI_ENCODING_TYPE_IN_FRU_IS_6BIT_ASCII_PACKED_STR);
	    case IPMI_COMP_CODE_DCMI_ENCODING_TYPE_IN_FRU_IS_SET_TO_ASCII_LATIN1_NOT_ENGLISH:
	      SNPRINTF_RETURN (IPMI_COMP_CODE_DCMI_ENCODING_TYPE_IN_FRU_IS_SET_TO_ASCII_LATIN1_NOT_ENGLISH_STR);
	    }
	  break;
        }
    }

  return ipmi_completion_code_strerror_r (cmd,
                                          netfn,
                                          comp_code,
                                          errstr,
                                          len);
}

int
ipmi_completion_code_dcmi_strerror_cmd_r (fiid_obj_t obj_cmd,
                                          uint8_t netfn,
                                          char *errstr,
                                          size_t len)
{
  uint8_t cmd, comp_code;
  uint64_t val;
  
  /* The netfn need not be valid */
  if (!fiid_obj_valid (obj_cmd)
      || !errstr)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }
  
  if (FIID_OBJ_FIELD_LOOKUP (obj_cmd, "cmd") < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_cmd);
      return (-1);
    }
  
  if (FIID_OBJ_FIELD_LOOKUP (obj_cmd, "comp_code") < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_cmd);
      return (-1);
    }
  
  if (FIID_OBJ_GET (obj_cmd, "cmd", &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_cmd);
      return (-1);
    }
  cmd = val;
  
  if (FIID_OBJ_GET (obj_cmd, "comp_code", &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_cmd);
      return (-1);
    }
  comp_code = val;
  
  return (ipmi_completion_code_dcmi_strerror_r (cmd, netfn, comp_code, errstr, len));
}
