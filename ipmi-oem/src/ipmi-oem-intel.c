/*
 * Copyright (C) 2008-2011 FreeIPMI Core Team
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

#if HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#if TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else  /* !TIME_WITH_SYS_TIME */
#if HAVE_SYS_TIME_H
#include <sys/time.h>
#else /* !HAVE_SYS_TIME_H */
#include <time.h>
#endif  /* !HAVE_SYS_TIME_H */
#endif /* !TIME_WITH_SYS_TIME */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <limits.h>		/* UCHAR_MAX */
#include <assert.h>

#include <freeipmi/freeipmi.h>

#include "ipmi-oem.h"
#include "ipmi-oem-argp.h"
#include "ipmi-oem-common.h"
#include "ipmi-oem-intel.h"

#include "freeipmi-portability.h"
#include "pstdout.h"
#include "tool-oem-common.h"
#include "tool-sdr-cache-common.h"
#include "tool-sensor-common.h"

/* Intel S5500WB/Penguin Computing Relion 700
 *
 * Set Fault Indication
 *
 * "Satellite controllers on the IPMB or software can indicate to the
 * BMC that they have detectd a fault assertion or de-assertion using
 * this command.  The BMC generates a composite fault state from this
 * information and its own internal fault state, and uses this to
 * drive front panel indicators.  The fault indication state
 * contributes to the system status state, as reported by the fron
 * tpanel system status. ... This command also supports additional
 * parameters to control BMC-owned component fault LEDs associated
 * with teh specified fault condition."
 * 
 * Set Fault Indication Request
 *
 * 0x30 - OEM network function
 * 0x57 - OEM cmd
 * 0x?? - source id
 *      - 0x00 - unspecified source
 *      - 0x01 - hot-swap controller 0
 *      - 0x02 - hot-swap controller 1
 *      - 0x03 - bios
 * 0x?? - fault type
 *      - 0x00 - fan
 *      - 0x01 - temp
 *      - 0x02 - power
 *      - 0x03 - drive slot
 *      - 0x04 - software
 *      - 0x05 - memory
 * 0x?? - state to set
 *      - 0x00 - ok
 *      - 0x01 - degraded
 *      - 0x02 - non-critical
 *      - 0x03 - critical
 *      - 0x04 - non-recoverable
 * 0x?? - component fault LED group ID
 *      - 0xFF - if not used
 * 0x?? - LED state
 *      - LSbyte first, 1 bit will turn on LED.
 *
 * Set Fault Indication Response
 *
 * 0x57 - OEM cmd
 * 0x?? - Completion Code
 */

#define IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_ENABLE_SMTP            0x00
/* MS byte first */
#define IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_SMTP_SERVER_ADDRESS    0x01
#define IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_SMTP_USER_NAME         0x02
/* write only */
#define IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_USER_PASSWORD          0x03
/* read only */
#define IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_NUMBER_OF_DESTINATIONS 0x04
/* max 2 blocks - doc says 64 bytes, but 2 blocks appears to be correct */
#define IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_EMAIL_ADDRESS          0x05
/* max 2 blocks */
#define IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_SUBJECT                0x06
/* max 4 blocks */
#define IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_MESSAGE_CONTENT        0x07
/* max 4 blocks according to doc, but according to firmware max 1 block when you write */
#define IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_SENDER_EMAIL_ADDRESS   0x08
/* max 2 blocks */
#define IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_SMTP_HOST_NAME         0x09

/* To avoid gcc warnings, add +1 in comparison */
#define IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_VALID(__value)	\
  (((__value + 1) >= (IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_ENABLE_SMTP + 1) \
    && (__value) <= IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_SMTP_HOST_NAME) ? 1 : 0)

#define IPMI_OEM_INTEL_SMTP_CONFIGURATION_NO_SET_SELECTOR   0x0
#define IPMI_OEM_INTEL_SMTP_CONFIGURATION_NO_BLOCK_SELECTOR 0x0

#define IPMI_OEM_INTEL_SMTP_ENABLE_BITMASK 0x01
#define IPMI_OEM_INTEL_SMTP_ENABLE_SHIFT   0

#define IPMI_OEM_INTEL_SMTP_ENABLE  0x1
#define IPMI_OEM_INTEL_SMTP_DISABLE 0x0

#define IPMI_OEM_INTEL_CHANNEL_NUMBERS_MAX 16

#define IPMI_OEM_INTEL_SMTP_STRING_BLOCK_LENGTH_MAX 16

#define IPMI_OEM_INTEL_SMTP_STRING_LENGTH_MAX       (4*IPMI_OEM_INTEL_SMTP_STRING_BLOCK_LENGTH_MAX)

#define IPMI_OEM_INTEL_RESTORE_CONFIGURATION_OPERATION_INITIATE_RESTORE   0xAA
#define IPMI_OEM_INTEL_RESTORE_CONFIGURATION_OPERATION_GET_RESTORE_STATUS 0x00

#define IPMI_OEM_INTEL_RESTORE_CONFIGURATION_RESTORE_PROGRESS_RESTORE_IN_PROGRESS 0x00
#define IPMI_OEM_INTEL_RESTORE_CONFIGURATION_RESTORE_PROGRESS_RESTORE_COMPLETED   0x01

static int
_get_smtp_configuration_data (ipmi_oem_state_data_t *state_data,
			      uint8_t channel_number,
			      uint8_t parameter_selector,
			      uint8_t set_selector,
			      uint8_t block_selector,
			      uint8_t *buf,
			      unsigned int *buflen)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  int rs_len;
  int rv = -1;

  assert (state_data);
  assert (IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_VALID (parameter_selector));
  assert (buf);
  assert (buflen);
  assert ((*buflen));

  /* Intel S5500WB/Penguin Computing Relion 700
   *
   * Get SMTP Configuration Request
   *
   * 0x32 - OEM network function
   * 0x38 - OEM cmd
   * 0x?? - channel number
   * 0x?? - parameter selector
   * 0x?? - set selector
   * 0x?? - block selector
   *
   * Get SMTP Configuration Response
   *
   * 0x38 - OEM cmd
   * 0x?? - Completion Code
   * 0x?? - 0x?? - configuration data
   */

  bytes_rq[0] = IPMI_CMD_OEM_INTEL_GET_SMTP_CONFIGURATION;
  bytes_rq[1] = channel_number;
  bytes_rq[2] = parameter_selector;
  bytes_rq[3] = set_selector;
  bytes_rq[4] = block_selector;
  
  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_INTEL_CONFIG_RQ, /* network function */
                              bytes_rq, /* data */
                              5, /* num bytes */
                              bytes_rs,
                              IPMI_OEM_MAX_BYTES)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_raw: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  if (ipmi_oem_check_response_and_completion_code (state_data,
                                                   bytes_rs,
                                                   rs_len,
                                                   2,
						   IPMI_CMD_OEM_INTEL_GET_SMTP_CONFIGURATION,
						   IPMI_NET_FN_OEM_INTEL_CONFIG_RS,
                                                   NULL) < 0)
    goto cleanup;

  if (rs_len > 2)
    memcpy (buf, &bytes_rs[2], rs_len - 2);

  (*buflen) = rs_len - 2;

  rv = 0;
 cleanup:
  return (rv);
}

static int
_get_smtp_configuration_value (ipmi_oem_state_data_t *state_data,
			       uint8_t channel_number,
			       uint8_t parameter_selector,
			       uint32_t *value)
{
  uint8_t buf[IPMI_OEM_MAX_BYTES];
  unsigned int buflen = IPMI_OEM_MAX_BYTES;
  int rv = -1;

  assert (state_data);
  assert (parameter_selector == IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_ENABLE_SMTP
	  || parameter_selector == IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_SMTP_SERVER_ADDRESS
	  || parameter_selector == IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_NUMBER_OF_DESTINATIONS);
  assert (value);

  if (_get_smtp_configuration_data (state_data,
				    channel_number,
				    parameter_selector,
				    IPMI_OEM_INTEL_SMTP_CONFIGURATION_NO_SET_SELECTOR,
				    IPMI_OEM_INTEL_SMTP_CONFIGURATION_NO_BLOCK_SELECTOR,
				    buf,
				    &buflen) < 0)
    goto cleanup;

  if (!buflen)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "zero configuration data bytes returned\n");
      goto cleanup;
    }

  if ((parameter_selector == IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_ENABLE_SMTP
       || parameter_selector == IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_NUMBER_OF_DESTINATIONS)
      && buflen != 1)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
		       "%u configuration data bytes returned, expected 1\n",
		       buflen);
      goto cleanup;
    }
  else if (parameter_selector == IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_SMTP_SERVER_ADDRESS
	   && buflen != 4)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%u configuration data bytes returned, expected 4\n",
		       buflen);
      
      goto cleanup;
    }

  if (parameter_selector == IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_ENABLE_SMTP
      || parameter_selector == IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_NUMBER_OF_DESTINATIONS)
    (*value) = buf[0];
  else /* parameter_selector == IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_SMTP_SERVER_ADDRESS */
    {
      /* MS byte first */
      (*value) = 0;
      (*value) |= (buf[0] << 24);
      (*value) |= (buf[1] << 16);
      (*value) |= (buf[2] << 8);
      (*value) |= buf[3];
    }

  rv = 0;
 cleanup:
  return (rv);
}

static int
_get_smtp_configuration_string (ipmi_oem_state_data_t *state_data,
				uint8_t channel_number,
				uint8_t parameter_selector,
				char *string,
				unsigned int stringlen)
{
  unsigned int max_blocks = 0;
  int rv = -1;
  unsigned int i;
  unsigned int string_count = 0;

  assert (state_data);
  assert (parameter_selector == IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_SMTP_USER_NAME
	  || parameter_selector == IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_EMAIL_ADDRESS
	  || parameter_selector == IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_SUBJECT
	  || parameter_selector == IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_MESSAGE_CONTENT
	  || parameter_selector == IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_SENDER_EMAIL_ADDRESS
	  || parameter_selector == IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_SMTP_HOST_NAME);
  assert (string);
  assert (stringlen);

  if (parameter_selector == IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_SMTP_USER_NAME)
    max_blocks = 1;
  else if (parameter_selector == IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_EMAIL_ADDRESS
	   || parameter_selector == IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_SUBJECT
	   || parameter_selector == IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_SMTP_HOST_NAME)
    max_blocks = 2;
  else /* parameter_selector == IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_MESSAGE_CONTENT
	  || parameter_selector == IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_SENDER_EMAIL_ADDRESS */
    max_blocks = 4;

  assert (stringlen >= (max_blocks * IPMI_OEM_INTEL_SMTP_STRING_BLOCK_LENGTH_MAX));

  for (i = 0; i < max_blocks; i++)
    {
      uint8_t buf[IPMI_OEM_MAX_BYTES];
      unsigned int buflen = IPMI_OEM_MAX_BYTES;
      unsigned int j;
      int nul_found = 0;

      memset (buf, '\0', IPMI_OEM_MAX_BYTES);

      if (_get_smtp_configuration_data (state_data,
					channel_number,
					parameter_selector,
					IPMI_OEM_INTEL_SMTP_CONFIGURATION_NO_SET_SELECTOR,
					i,
					buf,
					&buflen) < 0)
	goto cleanup;
      
      if (!buflen)
	{
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "zero configuration data bytes returned\n");
	  goto cleanup;
	}
      
      if ((buflen + string_count) > stringlen)
	{
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "buffer overflow\n");
	  goto cleanup;
	}

      memcpy (string + string_count, buf, buflen);

      string_count += buflen;

      /* strings are NUL terminated, so if there is a NUL found, we can break out */
      for (j = 0; j < buflen; j++)
	{
	  if (buf[j] == '\0')
	    {
	      nul_found++;
	      break;
	    }
	}

      if (nul_found)
	break;
    }

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_intel_get_smtp_config (ipmi_oem_state_data_t *state_data)
{
  uint8_t channel_numbers[IPMI_OEM_INTEL_CHANNEL_NUMBERS_MAX];
  unsigned int channel_numbers_count = 0;
  int rv = -1;
  unsigned int i;

  assert (state_data);

  /* achu: with nothing else to go on, I'm assuming the channel number is the lan channel number */

  if (state_data->prog_data->args->oem_options_count > 1)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid number of options specified\n");
      goto cleanup;
    }

  if (state_data->prog_data->args->oem_options_count)
    {
      char *ptr = NULL;
      unsigned int temp;

      errno = 0;
      temp = strtoul (state_data->prog_data->args->oem_options[0], &ptr, 10);
      if (errno
          || temp > UCHAR_MAX
          || ptr[0] != '\0'
          || !temp)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "%s:%s invalid OEM option argument '%s'\n",
                           state_data->prog_data->args->oem_id,
                           state_data->prog_data->args->oem_command,
                           state_data->prog_data->args->oem_options[0]);
          goto cleanup;
        }
      
      channel_numbers[0] = temp;
      channel_numbers_count = 1;
    }
  else
    {
      int ret;

      if ((ret = ipmi_get_channel_numbers (state_data->ipmi_ctx,
					   IPMI_CHANNEL_MEDIUM_TYPE_LAN_802_3,
					   channel_numbers,
					   IPMI_OEM_INTEL_CHANNEL_NUMBERS_MAX)) < 0)
	{
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "ipmi_get_channel_numbers: %s\n",
			   ipmi_ctx_errormsg (state_data->ipmi_ctx));
	  goto cleanup;
	}
      
      if (!ret)
	{
	  pstdout_fprintf (state_data->pstate,
                           stderr,
                           "ipmi_get_channel_numbers: no LAN channels discovered\n",
                           ipmi_ctx_errormsg (state_data->ipmi_ctx));
          goto cleanup;
	}

      channel_numbers_count = ret;
    }

  for (i = 0; i < channel_numbers_count; i++)
    {
      uint8_t smtpenable;
      uint32_t smtpserveraddress;
      char smtp_user_name[IPMI_OEM_INTEL_SMTP_STRING_LENGTH_MAX + 1];
      char email_address[IPMI_OEM_INTEL_SMTP_STRING_LENGTH_MAX + 1];
      char subject[IPMI_OEM_INTEL_SMTP_STRING_LENGTH_MAX + 1];
      char message_content[IPMI_OEM_INTEL_SMTP_STRING_LENGTH_MAX + 1];
      char sender_email_address[IPMI_OEM_INTEL_SMTP_STRING_LENGTH_MAX + 1];
      char smtp_host_name[IPMI_OEM_INTEL_SMTP_STRING_LENGTH_MAX + 1];
      uint32_t value;
      
      if (channel_numbers_count > 1)
	pstdout_printf (state_data->pstate,
			"LAN Channel %u\n",
			channel_numbers[i]);

      if (_get_smtp_configuration_value (state_data,
					 channel_numbers[i],
					 IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_ENABLE_SMTP,
					 &value) < 0)
	goto cleanup;

      smtpenable = (value & IPMI_OEM_INTEL_SMTP_ENABLE_BITMASK);
      smtpenable >>= IPMI_OEM_INTEL_SMTP_ENABLE_SHIFT;

      if (_get_smtp_configuration_value (state_data,
					 channel_numbers[i],
					 IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_SMTP_SERVER_ADDRESS,
					 &value) < 0)
	goto cleanup;

      smtpserveraddress = value;

      memset (smtp_user_name, '\0', IPMI_OEM_INTEL_SMTP_STRING_LENGTH_MAX + 1);

      if (_get_smtp_configuration_string (state_data,
					  channel_numbers[i],
					  IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_SMTP_USER_NAME,
					  smtp_user_name,
					  IPMI_OEM_INTEL_SMTP_STRING_LENGTH_MAX) < 0)
	goto cleanup;
      
      memset (email_address, '\0', IPMI_OEM_INTEL_SMTP_STRING_LENGTH_MAX + 1);

      if (_get_smtp_configuration_string (state_data,
					  channel_numbers[i],
					  IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_EMAIL_ADDRESS,
					  email_address,
					  IPMI_OEM_INTEL_SMTP_STRING_LENGTH_MAX) < 0)
	goto cleanup;

      memset (subject, '\0', IPMI_OEM_INTEL_SMTP_STRING_LENGTH_MAX + 1);

      if (_get_smtp_configuration_string (state_data,
					  channel_numbers[i],
					  IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_SUBJECT,
					  subject,
					  IPMI_OEM_INTEL_SMTP_STRING_LENGTH_MAX) < 0)
	goto cleanup;

      memset (message_content, '\0', IPMI_OEM_INTEL_SMTP_STRING_LENGTH_MAX + 1);

      if (_get_smtp_configuration_string (state_data,
					  channel_numbers[i],
					  IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_MESSAGE_CONTENT,
					  message_content,
					  IPMI_OEM_INTEL_SMTP_STRING_LENGTH_MAX) < 0)
	goto cleanup;

      memset (sender_email_address, '\0', IPMI_OEM_INTEL_SMTP_STRING_LENGTH_MAX + 1);

      if (_get_smtp_configuration_string (state_data,
					  channel_numbers[i],
					  IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_SENDER_EMAIL_ADDRESS,
					  sender_email_address,
					  IPMI_OEM_INTEL_SMTP_STRING_LENGTH_MAX) < 0)
	goto cleanup;

      memset (smtp_host_name, '\0', IPMI_OEM_INTEL_SMTP_STRING_LENGTH_MAX + 1);

      if (_get_smtp_configuration_string (state_data,
					  channel_numbers[i],
					  IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_SMTP_HOST_NAME,
					  smtp_host_name,
					  IPMI_OEM_INTEL_SMTP_STRING_LENGTH_MAX) < 0)
	goto cleanup;

      pstdout_printf (state_data->pstate,
		      "SMTP                 : %s\n",
		      (smtpenable == IPMI_OEM_INTEL_SMTP_ENABLE) ? "enabled" : "disabled");

      pstdout_printf (state_data->pstate,
		      "SMTP Server Address  : %u.%u.%u.%u\n",
		      (smtpserveraddress & 0x000000FF),
		      (smtpserveraddress & 0x0000FF00) >> 8,
		      (smtpserveraddress & 0x00FF0000) >> 16,
		      (smtpserveraddress & 0xFF000000) >> 24);
      
      pstdout_printf (state_data->pstate,
		      "SMTP User Name       : %s\n",
		      smtp_user_name);

      pstdout_printf (state_data->pstate,
		      "Email Address        : %s\n",
		      email_address);

      pstdout_printf (state_data->pstate,
		      "Subject              : %s\n",
		      subject);

      pstdout_printf (state_data->pstate,
		      "Message Content      : %s\n",
		      message_content);

      pstdout_printf (state_data->pstate,
		      "Sender Email Address : %s\n",
		      sender_email_address);

      pstdout_printf (state_data->pstate,
		      "SMTP Host Name       : %s\n",
		      smtp_host_name);

      if (channel_numbers_count > 1
	  && i != (channel_numbers_count - 1))
	pstdout_printf (state_data->pstate, "\n");
    }

  rv = 0;
 cleanup:
  return (rv);
}

static int
_set_smtp_configuration_data (ipmi_oem_state_data_t *state_data,
			      uint8_t channel_number,
			      uint8_t parameter_selector,
			      uint8_t set_selector,
			      uint8_t block_selector,
			      uint8_t *buf,
			      unsigned int buflen)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  int rs_len;
  int rv = -1;

  assert (state_data);
  assert (IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_VALID (parameter_selector));
  assert (buf);
  assert (buflen);
  assert (buflen <= (IPMI_OEM_MAX_BYTES - 5));

  /* Intel S5500WB/Penguin Computing Relion 700
   *
   * Set SMTP Configuration Request
   *
   * 0x32 - OEM network function
   * 0x38 - OEM cmd
   * 0x?? - channel number
   * 0x?? - parameter selector
   * 0x?? - set selector
   * 0x?? - block selector
   * 0x?? - 0x?? - configuration data
   *
   * Set SMTP Configuration Response
   *
   * 0x38 - OEM cmd
   * 0x?? - Completion Code
   */

  bytes_rq[0] = IPMI_CMD_OEM_INTEL_SET_SMTP_CONFIGURATION;
  bytes_rq[1] = channel_number;
  bytes_rq[2] = parameter_selector;
  bytes_rq[3] = set_selector;
  bytes_rq[4] = block_selector;
  memcpy (&bytes_rq[5], buf, buflen);
  
  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_INTEL_CONFIG_RQ, /* network function */
                              bytes_rq, /* data */
                              5 + buflen, /* num bytes */
                              bytes_rs,
                              IPMI_OEM_MAX_BYTES)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_raw: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }
  
  if (ipmi_oem_check_response_and_completion_code (state_data,
                                                   bytes_rs,
                                                   rs_len,
                                                   2,
						   IPMI_CMD_OEM_INTEL_GET_SMTP_CONFIGURATION,
						   IPMI_NET_FN_OEM_INTEL_CONFIG_RS,
                                                   NULL) < 0)
    goto cleanup;

  rv = 0;
 cleanup:
  return (rv);
}

static int
_set_smtp_configuration_value (ipmi_oem_state_data_t *state_data,
			       uint8_t *channel_numbers,
			       unsigned int channel_numbers_count,
			       uint8_t parameter_selector,
			       uint32_t value)
{
  uint8_t buf[IPMI_OEM_MAX_BYTES];
  unsigned int buflen = IPMI_OEM_MAX_BYTES;
  int rv = -1;
  unsigned int i;

  assert (state_data);
  assert (channel_numbers);
  assert (channel_numbers_count);
  assert (parameter_selector == IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_ENABLE_SMTP
	  || parameter_selector == IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_SMTP_SERVER_ADDRESS
	  || parameter_selector == IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_NUMBER_OF_DESTINATIONS);

  if (parameter_selector == IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_ENABLE_SMTP
      || parameter_selector == IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_NUMBER_OF_DESTINATIONS)
    {
      buf[0] = value;
      buflen = 1;
    }
  else if (parameter_selector == IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_SMTP_SERVER_ADDRESS)
    {
      /* MS byte first */
      buf[0] = (value & 0xFF000000) >> 24;
      buf[1] = (value & 0x00FF0000) >> 16;
      buf[2] = (value & 0x0000FF00) >> 8;
      buf[3] = (value & 0x000000FF);
      buflen = 4;
    }

  for (i = 0; i < channel_numbers_count; i++)
    {
      if (_set_smtp_configuration_data (state_data,
					channel_numbers[i],
					parameter_selector,
					IPMI_OEM_INTEL_SMTP_CONFIGURATION_NO_SET_SELECTOR,
					IPMI_OEM_INTEL_SMTP_CONFIGURATION_NO_BLOCK_SELECTOR,
					buf,
					buflen) < 0)
	goto cleanup;
    }

  rv = 0;
 cleanup:
  return (rv);
}

static int
_set_smtp_configuration_string (ipmi_oem_state_data_t *state_data,
				uint8_t *channel_numbers,
				unsigned int channel_numbers_count,
				uint8_t parameter_selector,
				char *key,
				char *string)
{
  uint8_t buf[IPMI_OEM_INTEL_SMTP_STRING_LENGTH_MAX];
  unsigned int max_blocks = 0;
  int rv = -1;
  unsigned int i;

  assert (state_data);
  assert (channel_numbers);
  assert (channel_numbers_count);
  assert (parameter_selector == IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_SMTP_USER_NAME
	  || parameter_selector == IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_USER_PASSWORD
          || parameter_selector == IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_EMAIL_ADDRESS
          || parameter_selector == IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_SUBJECT
          || parameter_selector == IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_MESSAGE_CONTENT
          || parameter_selector == IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_SENDER_EMAIL_ADDRESS
          || parameter_selector == IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_SMTP_HOST_NAME);
  assert (key);
  assert (string);

  if (parameter_selector == IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_SMTP_USER_NAME)
    max_blocks = 1;
  else if (parameter_selector == IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_EMAIL_ADDRESS
	   || parameter_selector == IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_SUBJECT)
    max_blocks = 2;
  else if (parameter_selector == IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_SMTP_HOST_NAME)
    /* XXX: document indicates 2 blocks, but firmware has errors on > 1 block, so we use 1 block */
    max_blocks = 1;
  else if (parameter_selector == IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_SENDER_EMAIL_ADDRESS)
    /* XXX: document indicates 4 blocks, but firmware has errors on > 1 block, so we use 1 block */
    max_blocks = 1;
  else /* parameter_selector == IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_MESSAGE_CONTENT */
    max_blocks = 4;
  
  if (strlen (string) > (max_blocks * IPMI_OEM_INTEL_SMTP_STRING_BLOCK_LENGTH_MAX))
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "value for key %s too long\n",
		       key);
      goto cleanup;
    }

  /* according to docs, must pad 00h to max blocks */
  memset (buf, '\0', IPMI_OEM_INTEL_SMTP_STRING_LENGTH_MAX);
  memcpy (buf, string, strlen (string));

  for (i = 0; i < channel_numbers_count; i++)
    {
      unsigned int j;

      for (j = 0; j < max_blocks; j++)
	{
	  if (_set_smtp_configuration_data (state_data,
					    channel_numbers[i],
					    parameter_selector,
					    IPMI_OEM_INTEL_SMTP_CONFIGURATION_NO_SET_SELECTOR,
					    j,
					    buf + j * IPMI_OEM_INTEL_SMTP_STRING_BLOCK_LENGTH_MAX,
					    IPMI_OEM_INTEL_SMTP_STRING_BLOCK_LENGTH_MAX) < 0)
	    goto cleanup;
	}
    }

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_intel_set_smtp_config (ipmi_oem_state_data_t *state_data)
{
  uint8_t channel_numbers[IPMI_OEM_INTEL_CHANNEL_NUMBERS_MAX];
  unsigned int channel_numbers_count = 0;
  uint8_t smtpenable = 0;
  uint32_t smtpserveraddress = 0;
  char smtp_user_name[IPMI_OEM_INTEL_SMTP_STRING_LENGTH_MAX + 1];
  char user_password[IPMI_OEM_INTEL_SMTP_STRING_LENGTH_MAX + 1];
  char email_address[IPMI_OEM_INTEL_SMTP_STRING_LENGTH_MAX + 1];
  char subject[IPMI_OEM_INTEL_SMTP_STRING_LENGTH_MAX + 1];
  char message_content[IPMI_OEM_INTEL_SMTP_STRING_LENGTH_MAX + 1];
  char sender_email_address[IPMI_OEM_INTEL_SMTP_STRING_LENGTH_MAX + 1];
  char smtp_host_name[IPMI_OEM_INTEL_SMTP_STRING_LENGTH_MAX + 1];
  int rv = -1;
  unsigned int i;
  int load_channel_numbers = 1;

  assert (state_data);

  if (!state_data->prog_data->args->oem_options_count)
    {
      pstdout_printf (state_data->pstate,
		      "Option: smtp=enable|disable\n"
		      "Option: smtpserveraddress=ipaddress\n"
		      "Option: smtpusername=string\n"
		      "Option: userpassword=string\n"
		      "Option: emailaddress=string\n"
		      "Option: subject=string\n"
		      "Option: messagecontent=string\n"
		      "Option: senderemailaddress=string\n"
		      "Option: smtphostname=string\n");
      return (0); 
    }

  /* Check if user selected a channel number */
  if (state_data->prog_data->args->oem_options_count)
    {
      char *ptr = NULL;
      unsigned int temp;

      errno = 0;
      temp = strtoul (state_data->prog_data->args->oem_options[0], &ptr, 10);
      if (!(errno
	    || temp > UCHAR_MAX
	    || ptr[0] != '\0'
	    || !temp))
        {
	  channel_numbers[0] = temp;
	  channel_numbers_count = 1;
	  load_channel_numbers = 0;
	}
    }

  if (load_channel_numbers)
    {
      int ret;
      
      if ((ret = ipmi_get_channel_numbers (state_data->ipmi_ctx,
					   IPMI_CHANNEL_MEDIUM_TYPE_LAN_802_3,
					   channel_numbers,
					   IPMI_OEM_INTEL_CHANNEL_NUMBERS_MAX)) < 0)
	{
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "ipmi_get_channel_numbers: %s\n",
			   ipmi_ctx_errormsg (state_data->ipmi_ctx));
	  goto cleanup;
	}
      
      if (!ret)
	{
	  pstdout_fprintf (state_data->pstate,
                           stderr,
                           "ipmi_get_channel_numbers: no LAN channels discovered\n",
                           ipmi_ctx_errormsg (state_data->ipmi_ctx));
          goto cleanup;
	}

      channel_numbers_count = ret;
    }
 
  for (i = (load_channel_numbers ? 0 : 1); i < state_data->prog_data->args->oem_options_count; i++)
    {
      char *key = NULL;
      char *value = NULL;
      
      if (ipmi_oem_parse_key_value (state_data,
                                    i,
                                    &key,
                                    &value) < 0)
        goto cleanup;

      if (!strcasecmp (key, "smtp"))
        {
          if (ipmi_oem_parse_enable (state_data, i, value, &smtpenable) < 0)
            goto cleanup;

	  if (_set_smtp_configuration_value (state_data,
					     channel_numbers,
					     channel_numbers_count,
					     IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_ENABLE_SMTP,
					     smtpenable) < 0)
	    goto cleanup;
        }
      else if (!strcasecmp (key, "smtpserveraddress"))
        {
          if (ipmi_oem_parse_ip_address (state_data, i, value, &smtpserveraddress) < 0)
            goto cleanup;

	  if (_set_smtp_configuration_value (state_data,
					     channel_numbers,
					     channel_numbers_count,
					     IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_SMTP_SERVER_ADDRESS,
					     smtpserveraddress) < 0)
	    goto cleanup;
        }
      else if (!strcasecmp (key, "smtpusername"))
        {
	  uint8_t string_length = 0;

	  if (ipmi_oem_parse_string (state_data,
				     i,
				     value, 
				     &string_length,
				     smtp_user_name,
				     IPMI_OEM_INTEL_SMTP_STRING_LENGTH_MAX) < 0)
	    goto cleanup;

	  if (_set_smtp_configuration_string (state_data,
					      channel_numbers,
					      channel_numbers_count,
					      IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_SMTP_USER_NAME,
					      key,
					      smtp_user_name) < 0)
	    goto cleanup;
        }
      else if (!strcasecmp (key, "userpassword"))
        {
	  uint8_t string_length = 0;

	  if (ipmi_oem_parse_string (state_data,
				     i,
				     value, 
				     &string_length,
				     user_password,
				     IPMI_OEM_INTEL_SMTP_STRING_LENGTH_MAX) < 0)
	    goto cleanup;

	  /* clear out buffers for security */

	  memset (value, '\0', strlen (value));
	  
	  memset (state_data->prog_data->args->oem_options[i], '\0', strlen (state_data->prog_data->args->oem_options[i]));

	  if (_set_smtp_configuration_string (state_data,
					      channel_numbers,
					      channel_numbers_count,
					      IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_USER_PASSWORD,
					      key,
					      user_password) < 0)
	    goto cleanup;

	  /* clear out buffers for security */
	  
	  memset (user_password, '\0', IPMI_OEM_INTEL_SMTP_STRING_LENGTH_MAX + 1);
        }
      else if (!strcasecmp (key, "emailaddress"))
        {
	  uint8_t string_length = 0;

	  if (ipmi_oem_parse_string (state_data,
				     i,
				     value, 
				     &string_length,
				     email_address,
				     IPMI_OEM_INTEL_SMTP_STRING_LENGTH_MAX) < 0)
	    goto cleanup;

	  if (_set_smtp_configuration_string (state_data,
					      channel_numbers,
					      channel_numbers_count,
					      IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_EMAIL_ADDRESS,
					      key,
					      email_address) < 0)
	    goto cleanup;
        }
      else if (!strcasecmp (key, "subject"))
        {
	  uint8_t string_length = 0;

	  if (ipmi_oem_parse_string (state_data,
				     i,
				     value, 
				     &string_length,
				     subject,
				     IPMI_OEM_INTEL_SMTP_STRING_LENGTH_MAX) < 0)
	    goto cleanup;

	  if (_set_smtp_configuration_string (state_data,
					      channel_numbers,
					      channel_numbers_count,
					      IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_SUBJECT,
					      key,
					      subject) < 0)
	    goto cleanup;
        }
      else if (!strcasecmp (key, "messagecontent"))
        {
	  uint8_t string_length = 0;

	  if (ipmi_oem_parse_string (state_data,
				     i,
				     value, 
				     &string_length,
				     message_content,
				     IPMI_OEM_INTEL_SMTP_STRING_LENGTH_MAX) < 0)
	    goto cleanup;

	  if (_set_smtp_configuration_string (state_data,
					      channel_numbers,
					      channel_numbers_count,
					      IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_MESSAGE_CONTENT,
					      key,
					      message_content) < 0)
	    goto cleanup;
        }
      else if (!strcasecmp (key, "senderemailaddress"))
        {
	  uint8_t string_length = 0;

	  if (ipmi_oem_parse_string (state_data,
				     i,
				     value, 
				     &string_length,
				     sender_email_address,
				     IPMI_OEM_INTEL_SMTP_STRING_LENGTH_MAX) < 0)
	    goto cleanup;

	  if (_set_smtp_configuration_string (state_data,
					      channel_numbers,
					      channel_numbers_count,
					      IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_SENDER_EMAIL_ADDRESS,
					      key,
					      sender_email_address) < 0)
	    goto cleanup;
        }
      else if (!strcasecmp (key, "smtphostname"))
        {
	  uint8_t string_length = 0;

	  if (ipmi_oem_parse_string (state_data,
				     i,
				     value, 
				     &string_length,
				     smtp_host_name,
				     IPMI_OEM_INTEL_SMTP_STRING_LENGTH_MAX) < 0)
	    goto cleanup;

	  if (_set_smtp_configuration_string (state_data,
					      channel_numbers,
					      channel_numbers_count,
					      IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_SMTP_HOST_NAME,
					      key,
					      smtp_host_name) < 0)
	    goto cleanup;
        }
      else
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "%s:%s invalid OEM option argument '%s' : invalid key\n",
                           state_data->prog_data->args->oem_id,
                           state_data->prog_data->args->oem_command,
                           state_data->prog_data->args->oem_options[i]);
          goto cleanup;
        }

      free (key);
      free (value);
    }

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_intel_restore_configuration (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  int rs_len;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  /* Intel S5500WB/Penguin Computing Relion 700
   *
   * Restore Configuration Request
   *
   * 0x30 - OEM network function
   * 0x02 - OEM cmd
   * 0x43 - 'C'
   * 0x4C - 'L'
   * 0x52 - 'R'
   * 0x?? - Operation
   *      - 0x00 - restore status
   *      - 0xAA - initiate restore
   * 
   * Restore Configuration Response
   *
   * 0x02 - OEM cmd
   * 0x?? - Completion Code
   * 0x?? - Restore progress
   *      - 0x00 - restore in progress
   *      - 0x01 - restore completed
   */

  bytes_rq[0] = IPMI_CMD_OEM_INTEL_RESTORE_CONFIGURATION;
  bytes_rq[1] = 'C';
  bytes_rq[2] = 'L';
  bytes_rq[3] = 'R';
  bytes_rq[4] = IPMI_OEM_INTEL_RESTORE_CONFIGURATION_OPERATION_INITIATE_RESTORE;
   
  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_INTEL_GENERIC_RQ, /* network function */
                              bytes_rq, /* data */
                              5, /* num bytes */
                              bytes_rs,
                              IPMI_OEM_MAX_BYTES)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_raw: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }
  
  if (ipmi_oem_check_response_and_completion_code (state_data,
                                                   bytes_rs,
                                                   rs_len,
                                                   3,
                                                   IPMI_CMD_OEM_INTEL_RESTORE_CONFIGURATION,
                                                   IPMI_NET_FN_OEM_INTEL_GENERIC_RS,
                                                   NULL) < 0)
    goto cleanup;
  
  /* don't quit until it is done */
  while (1)
    {
      bytes_rq[4] = IPMI_OEM_INTEL_RESTORE_CONFIGURATION_OPERATION_GET_RESTORE_STATUS;
      
      if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                                  0, /* lun */
                                  IPMI_NET_FN_OEM_INTEL_GENERIC_RQ, /* network function */
                                  bytes_rq, /* data */
                                  5, /* num bytes */
                                  bytes_rs,
                                  IPMI_OEM_MAX_BYTES)) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "ipmi_cmd_raw: %s\n",
                           ipmi_ctx_errormsg (state_data->ipmi_ctx));
          goto cleanup;
        }
      
      if (ipmi_oem_check_response_and_completion_code (state_data,
                                                       bytes_rs,
                                                       rs_len,
                                                       3,
						       IPMI_CMD_OEM_INTEL_RESTORE_CONFIGURATION,
						       IPMI_NET_FN_OEM_INTEL_GENERIC_RS,
                                                       NULL) < 0)
        goto cleanup;
      
      if (bytes_rs[2] == IPMI_OEM_INTEL_RESTORE_CONFIGURATION_RESTORE_PROGRESS_RESTORE_COMPLETED)
        break;

      sleep (1);
    }
  
  rv = 0;
 cleanup:
  return (rv);
}
