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
#endif /* STDC_HEADERS */
#include <limits.h>
#include <assert.h>

#include <freeipmi/freeipmi.h>

#include "ipmi-oem.h"
#include "ipmi-oem-argp.h"
#include "ipmi-oem-common.h"

#include "freeipmi-portability.h"
#include "pstdout.h"

int
ipmi_oem_check_response_and_completion_code (ipmi_oem_state_data_t *state_data,
                                             const void *bytes_rs,
                                             unsigned int bytes_rs_len,
                                             unsigned int expected_bytes_rs_len,
                                             uint8_t cmd,
                                             uint8_t netfn)
{
  const uint8_t *bytes_rs_ptr = bytes_rs;

  assert (state_data);
  assert (bytes_rs);

  if (bytes_rs_len < expected_bytes_rs_len)
    {
      if (bytes_rs_len >= 2 && bytes_rs_ptr[1] != IPMI_COMP_CODE_COMMAND_SUCCESS)
        goto output_comp_code_error;

      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid response length: %u, expected %u\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       bytes_rs_len,
                       expected_bytes_rs_len);
      return (-1);
    }

 output_comp_code_error:  
  if (bytes_rs_ptr[1] != IPMI_COMP_CODE_COMMAND_SUCCESS)
    {
      char errbuf[IPMI_OEM_ERR_BUFLEN];
      
      memset (errbuf, '\0', IPMI_OEM_ERR_BUFLEN);
      if (ipmi_completion_code_strerror_r (cmd, /* cmd */
                                           netfn, /* network function */
                                           bytes_rs_ptr[1], /* completion code */
                                           errbuf,
                                           IPMI_OEM_ERR_BUFLEN) < 0)
        {
          pstdout_perror (state_data->pstate, "ipmi_completion_code_strerror_r");
          snprintf (errbuf, IPMI_OEM_ERR_BUFLEN, "completion-code = 0x%X", bytes_rs_ptr[1]);
        }
      
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s failed: %s\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       errbuf);
      return (-1);
    }

  return (0);
}

int 
ipmi_oem_parse_key_value (ipmi_oem_state_data_t *state_data,
                          unsigned int option_num,
                          char **key,
                          char **value)
{  
  char *tempstr = NULL;
  char *tempptr = NULL;
  char *tempkey = NULL;
  char *tempvalue = NULL;
  int rv = -1;
   
  assert (state_data);
  assert (key);
  assert (value);
   
  if (!(tempstr = strdup (state_data->prog_data->args->oem_options[option_num])))
    {
      pstdout_perror (state_data->pstate,
                      "strdup");
      goto cleanup;
    }
   
  tempptr = strchr (tempstr, '=');
  if (!tempptr)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s' : no equal sign\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[option_num]);
      goto cleanup;
    }

  (*tempptr) = '\0'; 
  tempptr++;

  if (!(tempkey = strdup (tempstr)))
    {
      pstdout_perror (state_data->pstate,
                      "strdup");
      goto cleanup;
    }

  if (!(tempvalue = strdup (tempptr)))
    {
      pstdout_perror (state_data->pstate,
                      "strdup");
      goto cleanup; 
    }
  
  (*key) = tempkey;
  (*value) = tempvalue;
  
  rv = 0;
 cleanup:
  free (tempstr);
  if (rv < 0)
    {
      free (tempkey);
      free (tempvalue);
    }
  return (rv);
}

int
ipmi_oem_parse_enable (ipmi_oem_state_data_t *state_data,
                       unsigned int option_num,
                       const char *value,
                       uint8_t *enable)
{
  assert (state_data);
  assert (value);
  assert (enable);

  if (strcasecmp (value, "enable") && strcasecmp (value, "disable"))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s' : invalid value\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[option_num]);
      return (-1);
    }
  
  if (!strcasecmp (value, "enable"))
    (*enable) = 1;
  else
    (*enable) = 0;
  
  return (0);
}

int
ipmi_oem_parse_1_byte_field (ipmi_oem_state_data_t *state_data,
			     unsigned int option_num,
			     const char *value,
			     uint8_t *value_out)
{
  unsigned int temp;
  char *ptr = NULL;
  
  assert (state_data);
  assert (value);
  assert (value_out);
  
  errno = 0;
  
  temp = strtoul (value, &ptr, 10);
  
  if (errno
      || temp > UCHAR_MAX
      || ptr[0] != '\0')
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s' : invalid value\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[option_num]);
      return (-1);
    }
  
  (*value_out) = temp;
  return (0);
}

int
ipmi_oem_parse_2_byte_field (ipmi_oem_state_data_t *state_data,
			     unsigned int option_num,
			     const char *value,
			     uint16_t *value_out)
{
  unsigned int temp;
  char *ptr = NULL;
  
  assert (state_data);
  assert (value);
  assert (value_out);
  
  errno = 0;
  
  temp = strtoul (value, &ptr, 10);
  
  if (errno
      || temp > USHRT_MAX
      || ptr[0] != '\0')
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s' : invalid value\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[option_num]);
      return (-1);
    }
  
  (*value_out) = temp;
  return (0);
}

int
ipmi_oem_parse_4_byte_field (ipmi_oem_state_data_t *state_data,
			     unsigned int option_num,
			     const char *value,
			     uint32_t *value_out)
{ 
  unsigned int temp;
  char *ptr = NULL;
  
  assert (state_data);
  assert (value);
  assert (value_out);
  
  errno = 0;
  
  temp = strtoul (value, &ptr, 10);
  
  if (errno
      || ptr[0] != '\0')
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s' : invalid value\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[option_num]);
      return (-1);
    }
  
  (*value_out) = temp;
  return (0);
}

int
ipmi_oem_parse_unsigned_int_range (ipmi_oem_state_data_t *state_data,
                                   unsigned int option_num,
                                   const char *value,
                                   uint32_t *value_out,
                                   unsigned int min,
                                   unsigned int max)
{
  unsigned int temp;
  char *ptr = NULL;
  
  assert (state_data);
  assert (value);
  assert (value_out);
  assert (min < max);
  
  errno = 0;
  
  temp = strtoul (value, &ptr, 10);
  
  if (errno
      || ptr[0] != '\0')
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s' : invalid value\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[option_num]);
      return (-1);
    }
  
  if (temp < min
      || temp > max)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s' : out of range\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[option_num]);
      return (-1);
    }

  (*value_out) = temp;
  return (0);
}

int
ipmi_oem_parse_string (ipmi_oem_state_data_t *state_data,
                       unsigned int option_num,
                       const char *value,
                       uint8_t *string_length,
                       char *stringbuf,
                       unsigned int stringbuflen)
{
  assert (state_data);
  assert (value);
  assert (string_length);
  assert (stringbuf);
  assert (stringbuflen);
  
  if (strlen (value) > stringbuflen)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s' : string length too long\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[option_num]);
      return (-1);
    }

  (*string_length) = strlen (value);

  /* use memcpy, do not need NULL termination */
  if ((*string_length))
    memcpy (stringbuf,
	    value,
	    (*string_length));
  
  return (0);
}

