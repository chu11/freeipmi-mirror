/*
  Copyright (C) 2008-2009 FreeIPMI Core Team

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

#if HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#include <ctype.h>
#endif /* STDC_HEADERS */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <assert.h>

#include <freeipmi/freeipmi.h>

#include "ipmi-oem.h"
#include "ipmi-oem-argp.h"
#include "ipmi-oem-common.h"
#include "ipmi-oem-fujitsu.h"

#include "freeipmi-portability.h"
#include "pstdout.h"

#define IPMI_OEM_CONFIG_BMC_NETWORK_NAME         0x1430
#define IPMI_OEM_CONFIG_BMC_USE_NETWORK_NAME     0x1431
#define IPMI_OEM_CONFIG_BMC_NAME_EXTENSION       0x1432
#define IPMI_OEM_CONFIG_BMC_ADD_SERIAL_NUMBER    0x1433
#define IPMI_OEM_CONFIG_BMC_ADD_NAME_EXTENSION   0x1434
#define IPMI_OEM_CONFIG_BMC_REGISTER_DHCP_IN_DNS 0x144A

#define IPMI_OEM_CONFIG_BMC_READ  0x01
#define IPMI_OEM_CONFIG_BMC_WRITE 0x02

#define IPMI_OEM_CONFIG_BMC_YES 0x01
#define IPMI_OEM_CONFIG_BMC_NO  0x00

#define IPMI_OEM_CONFIG_BMC_NAME_MAX 16

/* field length 1 byte long, so max should be 256 */
#define IPMI_OEM_CONFIG_BMC_BUFFER_LENGTH        256 

#define IPMI_CMD_OEM_FUJITSU_CONFIG_BMC 0xE0

#define IPMI_NET_FN_OEM_GROUP_RQ         0x2E
#define IPMI_NET_FN_OEM_GROUP_RS         0x2F

static int
_ipmi_oem_fujitsu_config_bmc_read (ipmi_oem_state_data_t *state_data,
                                   uint16_t config_id,
                                   uint8_t *buf,
                                   unsigned int *buflen)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  int rs_len;
  int rv = -1;

  assert (state_data);
  assert (buf);
  assert (buflen);
  assert (*buflen);

  /* Fujitsu OEM
   *
   * Config BMC Request
   *
   * 0x2E - OEM network function
   * 0xE0 - OEM cmd
   * 0x80 - ???
   * 0x28 - ???
   * 0x00 - ??? (maybe and object ID?)
   * 0x01 | 0x02 - 0x01 = read, 0x02 = write
   * 0x00 - ??? (maybe and object ID?)
   * 0x?? - config id LSB
   * 0x?? - config id MSB
   * 0x?? - length of write (if read, not relevant)
   * 0x?? - bytes to write (if read, not relevant)
   *
   * Config BMC Response
   *
   * 0x2E - OEM network function
   * 0xE0 - OEM cmd
   * 0x?? - completion code
   * 0x80 - ???
   * 0x28 - ???
   * 0x00 - ??? (maybe and object ID?)
   * 0x?? - length of read (if write, not relevant)
   * 0x?? - bytes of read (if write, not relevant)
   */
  
  bytes_rq[0] = IPMI_CMD_OEM_FUJITSU_CONFIG_BMC;
  bytes_rq[1] = 0x80;
  bytes_rq[2] = 0x28;
  bytes_rq[3] = 0x00;
  bytes_rq[4] = IPMI_OEM_CONFIG_BMC_READ;
  bytes_rq[5] = 0x00;
  bytes_rq[6] = (config_id & 0x00FF);
  bytes_rq[7] = ((config_id & 0xFF00) >> 8);

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_GROUP_RQ, /* network function */
                              bytes_rq, /* data */
                              8, /* num bytes */
                              bytes_rs,
                              IPMI_OEM_MAX_BYTES)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_raw: %s\n",
                       ipmi_ctx_strerror (ipmi_ctx_errnum (state_data->ipmi_ctx)));
      goto cleanup;
    }

  if (ipmi_oem_check_response_and_completion_code (state_data,
                                                   bytes_rs,
                                                   rs_len,
                                                   5,
                                                   IPMI_CMD_OEM_FUJITSU_CONFIG_BMC,
                                                   IPMI_NET_FN_OEM_GROUP_RS) < 0)
    goto cleanup;

  /* achu
   *
   * I don't know if it's possible for there to be no return value,
   * i.e. a string is set to NULL.  I will assume it's possible.
   */
  memset (buf, '\0', (*buflen));
  if (rs_len >= 6)
    {
      /* 5 that are expected, 1 for length byte, and remainder */
      if (rs_len != (5 + 1 + bytes_rq[5]))
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "invalid length returned: %u\n",
                           bytes_rq[5]);
          goto cleanup;
        }

      if ((*buflen) < bytes_rq[5])
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "internal buffer overflow\n");
          goto cleanup;
        }

      memcpy (buf, &bytes_rq[6], bytes_rq[5]);
      (*buflen) = bytes_rq[5];
    }
  else
    (*buflen) = 0;

  rv = 0;
 cleanup:
  return (rv);
}

static int
_ipmi_oem_fujitsu_config_bmc_write (ipmi_oem_state_data_t *state_data,
                                    uint16_t config_id,
                                    uint8_t *buf,
                                    uint8_t buflen)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  unsigned int rq_len;
  int rs_len;
  int rv = -1;

  assert (state_data);
  assert (buf);
  assert (buflen);

  /* Fujitsu OEM
   *
   * Config BMC Request
   *
   * 0x2E - OEM network function
   * 0xE0 - OEM cmd
   * 0x80 - ???
   * 0x28 - ???
   * 0x00 - ??? (maybe and object ID?)
   * 0x01 | 0x02 - 0x01 = read, 0x02 = write
   * 0x00 - ??? (maybe and object ID?)
   * 0x?? - config id LSB
   * 0x?? - config id MSB
   * 0x?? - length of write (if read, not relevant)
   * 0x?? - bytes to write (if read, not relevant)
   *
   * Config BMC Response
   *
   * 0x2E - OEM network function
   * 0xE0 - OEM cmd
   * 0x?? - completion code
   * 0x80 - ???
   * 0x28 - ???
   * 0x00 - ??? (maybe and object ID?)
   * 0x?? - length of read (if write, not relevant)
   * 0x?? - bytes of read (if write, not relevant)
   */
  
  bytes_rq[0] = IPMI_CMD_OEM_FUJITSU_CONFIG_BMC;
  bytes_rq[1] = 0x80;
  bytes_rq[2] = 0x28;
  bytes_rq[3] = 0x00;
  bytes_rq[4] = IPMI_OEM_CONFIG_BMC_WRITE;
  bytes_rq[5] = 0x00;
  bytes_rq[6] = (config_id & 0x00FF);
  bytes_rq[7] = ((config_id & 0xFF00) >> 8);
  bytes_rq[8] = buflen;

  rq_len = 9;

  /* achu: I don't know if you can clear this by specifying 0 length,
   * but lets assume you can.
   */
  if (buflen)
    {
      memcpy (&bytes_rq[7], buf, buflen);
      rq_len += buflen;
    }

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_GROUP_RQ, /* network function */
                              bytes_rq, /* data */
                              rq_len, /* num bytes */
                              bytes_rs,
                              IPMI_OEM_MAX_BYTES)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_raw: %s\n",
                       ipmi_ctx_strerror (ipmi_ctx_errnum (state_data->ipmi_ctx)));
      goto cleanup;
    }

  if (ipmi_oem_check_response_and_completion_code (state_data,
                                                   bytes_rs,
                                                   rs_len,
                                                   5,
                                                   IPMI_CMD_OEM_FUJITSU_CONFIG_BMC,
                                                   IPMI_NET_FN_OEM_GROUP_RS) < 0)
    goto cleanup;

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_fujitsu_get_network_name (ipmi_oem_state_data_t *state_data)
{
  uint8_t buf[IPMI_OEM_CONFIG_BMC_BUFFER_LENGTH + 1];
  unsigned int buflen = IPMI_OEM_CONFIG_BMC_BUFFER_LENGTH;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  memset (buf, '\0', IPMI_OEM_CONFIG_BMC_BUFFER_LENGTH + 1);
  if (_ipmi_oem_fujitsu_config_bmc_read (state_data,
                                         IPMI_OEM_CONFIG_BMC_NETWORK_NAME,
                                         buf,
                                         &buflen) < 0)
    goto cleanup;

  if (buflen)
    pstdout_printf (state_data->pstate,
                    "%s\n",
                    buf);
  else
    pstdout_printf (state_data->pstate,
                    "\n");

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_fujitsu_set_network_name (ipmi_oem_state_data_t *state_data)
{
  uint8_t buf[IPMI_OEM_CONFIG_BMC_BUFFER_LENGTH + 1];
  uint8_t buflen;
  int rv = -1;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 1);
  
  if (strlen (state_data->prog_data->args->oem_options[0]) > IPMI_OEM_CONFIG_BMC_NAME_MAX)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s OEM option argument '%s' invalid length, max %u\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[0],
                       IPMI_OEM_CONFIG_BMC_NAME_MAX);
      goto cleanup;
    }

  buflen = strlen (state_data->prog_data->args->oem_options[0]);
  memset (buf, '\0', IPMI_OEM_CONFIG_BMC_BUFFER_LENGTH + 1);
  memcpy (buf, state_data->prog_data->args->oem_options[0], buflen);

  if (_ipmi_oem_fujitsu_config_bmc_write (state_data,
                                          IPMI_OEM_CONFIG_BMC_NETWORK_NAME,
                                          buf,
                                          buflen) < 0)
    goto cleanup;

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_fujitsu_get_use_network_name (ipmi_oem_state_data_t *state_data)
{
  uint8_t buf[IPMI_OEM_CONFIG_BMC_BUFFER_LENGTH + 1];
  unsigned int buflen = IPMI_OEM_CONFIG_BMC_BUFFER_LENGTH;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  memset (buf, '\0', IPMI_OEM_CONFIG_BMC_BUFFER_LENGTH + 1);
  if (_ipmi_oem_fujitsu_config_bmc_read (state_data,
                                         IPMI_OEM_CONFIG_BMC_USE_NETWORK_NAME,
                                         buf,
                                         &buflen) < 0)
    goto cleanup;

  if (buflen != 1)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "invalid length returned: %u\n",
                       buflen);
      goto cleanup;
    }

  pstdout_printf (state_data->pstate,
                  "%s\n",
                  buf[0] ? "Yes" : "No");

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_fujitsu_set_use_network_name (ipmi_oem_state_data_t *state_data)
{
  uint8_t buf[IPMI_OEM_CONFIG_BMC_BUFFER_LENGTH + 1];
  uint8_t buflen;
  int rv = -1;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 1);
  
  if (strcasecmp (state_data->prog_data->args->oem_options[0], "yes")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "no"))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[0]);
      goto cleanup;
    }

  if (!strcasecmp (state_data->prog_data->args->oem_options[0], "yes"))
    buf[0] = IPMI_OEM_CONFIG_BMC_YES;
  else
    buf[0] = IPMI_OEM_CONFIG_BMC_NO;
  buflen = 1;
  
  if (_ipmi_oem_fujitsu_config_bmc_write (state_data,
                                          IPMI_OEM_CONFIG_BMC_USE_NETWORK_NAME,
                                          buf,
                                          buflen) < 0)
    goto cleanup;
  
  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_fujitsu_get_name_extension (ipmi_oem_state_data_t *state_data)
{
  uint8_t buf[IPMI_OEM_CONFIG_BMC_BUFFER_LENGTH + 1];
  unsigned int buflen = IPMI_OEM_CONFIG_BMC_BUFFER_LENGTH;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  memset (buf, '\0', IPMI_OEM_CONFIG_BMC_BUFFER_LENGTH + 1);
  if (_ipmi_oem_fujitsu_config_bmc_read (state_data,
                                         IPMI_OEM_CONFIG_BMC_NAME_EXTENSION,
                                         buf,
                                         &buflen) < 0)
    goto cleanup;

  if (buflen)
    pstdout_printf (state_data->pstate,
                    "%s\n",
                    buf);
  else
    pstdout_printf (state_data->pstate,
                    "\n");

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_fujitsu_set_name_extension (ipmi_oem_state_data_t *state_data)
{
  uint8_t buf[IPMI_OEM_CONFIG_BMC_BUFFER_LENGTH + 1];
  uint8_t buflen;
  int rv = -1;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 1);
  
  if (strlen (state_data->prog_data->args->oem_options[0]) > IPMI_OEM_CONFIG_BMC_NAME_MAX)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s OEM option argument '%s' invalid length, max %u\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[0],
                       IPMI_OEM_CONFIG_BMC_NAME_MAX);
      goto cleanup;
    }
  
  buflen = strlen (state_data->prog_data->args->oem_options[0]);
  memset (buf, '\0', IPMI_OEM_CONFIG_BMC_BUFFER_LENGTH + 1);
  memcpy (buf, state_data->prog_data->args->oem_options[0], buflen);
  
  if (_ipmi_oem_fujitsu_config_bmc_write (state_data,
                                          IPMI_OEM_CONFIG_BMC_NAME_EXTENSION,
                                          buf,
                                          buflen) < 0)
    goto cleanup;
  
  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_fujitsu_get_add_name_extension (ipmi_oem_state_data_t *state_data)
{
  uint8_t buf[IPMI_OEM_CONFIG_BMC_BUFFER_LENGTH + 1];
  unsigned int buflen = IPMI_OEM_CONFIG_BMC_BUFFER_LENGTH;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  memset (buf, '\0', IPMI_OEM_CONFIG_BMC_BUFFER_LENGTH + 1);
  if (_ipmi_oem_fujitsu_config_bmc_read (state_data,
                                         IPMI_OEM_CONFIG_BMC_ADD_NAME_EXTENSION,
                                         buf,
                                         &buflen) < 0)
    goto cleanup;

  if (buflen != 1)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "invalid length returned: %u\n",
                       buflen);
      goto cleanup;
    }

  pstdout_printf (state_data->pstate,
                  "%s\n",
                  buf[0] ? "Yes" : "No");

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_fujitsu_set_add_name_extension (ipmi_oem_state_data_t *state_data)
{
  uint8_t buf[IPMI_OEM_CONFIG_BMC_BUFFER_LENGTH + 1];
  uint8_t buflen;
  int rv = -1;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 1);
  
  if (strcasecmp (state_data->prog_data->args->oem_options[0], "yes")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "no"))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[0]);
      goto cleanup;
    }

  if (!strcasecmp (state_data->prog_data->args->oem_options[0], "yes"))
    buf[0] = IPMI_OEM_CONFIG_BMC_YES;
  else
    buf[0] = IPMI_OEM_CONFIG_BMC_NO;
  buflen = 1;
  
  if (_ipmi_oem_fujitsu_config_bmc_write (state_data,
                                          IPMI_OEM_CONFIG_BMC_ADD_NAME_EXTENSION,
                                          buf,
                                          buflen) < 0)
    goto cleanup;
  
  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_fujitsu_get_add_serial_number (ipmi_oem_state_data_t *state_data)
{
  uint8_t buf[IPMI_OEM_CONFIG_BMC_BUFFER_LENGTH + 1];
  unsigned int buflen = IPMI_OEM_CONFIG_BMC_BUFFER_LENGTH;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  memset (buf, '\0', IPMI_OEM_CONFIG_BMC_BUFFER_LENGTH + 1);
  if (_ipmi_oem_fujitsu_config_bmc_read (state_data,
                                         IPMI_OEM_CONFIG_BMC_ADD_SERIAL_NUMBER,
                                         buf,
                                         &buflen) < 0)
    goto cleanup;

  if (buflen != 1)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "invalid length returned: %u\n",
                       buflen);
      goto cleanup;
    }

  pstdout_printf (state_data->pstate,
                  "%s\n",
                  buf[0] ? "Yes" : "No");

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_fujitsu_set_add_serial_number (ipmi_oem_state_data_t *state_data)
{
  uint8_t buf[IPMI_OEM_CONFIG_BMC_BUFFER_LENGTH + 1];
  uint8_t buflen;
  int rv = -1;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 1);
  
  if (strcasecmp (state_data->prog_data->args->oem_options[0], "yes")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "no"))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[0]);
      goto cleanup;
    }

  if (!strcasecmp (state_data->prog_data->args->oem_options[0], "yes"))
    buf[0] = IPMI_OEM_CONFIG_BMC_YES;
  else
    buf[0] = IPMI_OEM_CONFIG_BMC_NO;
  buflen = 1;
  
  if (_ipmi_oem_fujitsu_config_bmc_write (state_data,
                                          IPMI_OEM_CONFIG_BMC_ADD_SERIAL_NUMBER,
                                          buf,
                                          buflen) < 0)
    goto cleanup;
  
  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_fujitsu_get_register_dhcp_in_dns (ipmi_oem_state_data_t *state_data)
{
  uint8_t buf[IPMI_OEM_CONFIG_BMC_BUFFER_LENGTH + 1];
  unsigned int buflen = IPMI_OEM_CONFIG_BMC_BUFFER_LENGTH;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  memset (buf, '\0', IPMI_OEM_CONFIG_BMC_BUFFER_LENGTH + 1);
  if (_ipmi_oem_fujitsu_config_bmc_read (state_data,
                                         IPMI_OEM_CONFIG_BMC_REGISTER_DHCP_IN_DNS,
                                         buf,
                                         &buflen) < 0)
    goto cleanup;
  
  if (buflen != 1)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "invalid length returned: %u\n",
                       buflen);
      goto cleanup;
    }

  pstdout_printf (state_data->pstate,
                  "%s\n",
                  buf[0] ? "Yes" : "No");

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_fujitsu_set_register_dhcp_in_dns (ipmi_oem_state_data_t *state_data)
{
  uint8_t buf[IPMI_OEM_CONFIG_BMC_BUFFER_LENGTH + 1];
  uint8_t buflen;
  int rv = -1;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 1);
  
  if (strcasecmp (state_data->prog_data->args->oem_options[0], "yes")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "no"))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[0]);
      goto cleanup;
    }

  if (!strcasecmp (state_data->prog_data->args->oem_options[0], "yes"))
    buf[0] = IPMI_OEM_CONFIG_BMC_YES;
  else
    buf[0] = IPMI_OEM_CONFIG_BMC_NO;
  buflen = 1;
  
  if (_ipmi_oem_fujitsu_config_bmc_write (state_data,
                                          IPMI_OEM_CONFIG_BMC_REGISTER_DHCP_IN_DNS,
                                          buf,
                                          buflen) < 0)
    goto cleanup;
  
  rv = 0;
 cleanup:
  return (rv);
}


