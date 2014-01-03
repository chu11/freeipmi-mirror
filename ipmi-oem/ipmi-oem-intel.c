/*
 * Copyright (C) 2008-2014 FreeIPMI Core Team
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

/* Quanta QSSC-S4R/Appro GB812X-CN
 * (Quanta motherboard maintains Intel manufacturer ID)
 *
 * Set Fan Control Configuration Command
 *
 * Should really only be used by the BIOS, documented here.
 *
 * Set Fan Control Configuration Request
 *
 * 0x?? - OEM network function not indicated
 * 0x89 - OEM cmd
 * 0x?? - fan profile to enable
 *      - 0 - Fan profile 0 (default)
 *      - 1 - Fan profile 1
 *      - 2 - Fan profile 2
 *      - 3 - Fan profile 3
 *      - 4 - Fan profile 4 
 *      - 5 - Fan profile 5 (not valid for QSSC-S4R)
 *      - 6 - Fan profile 6 (not valid for QSSC-S4R)
 *      - 7 - Fan profile 7 (not valid for QSSC-S4R)
 *      - 0xff - none specified (do not change current setting)
 * 0x?? - flags
 *      - [7:3] reserved
 *      - [2] - Memory temp sensor and memory throttling configuration status
 *            - 0 - not started or in progress
 *            - 1 - completed
 *      = [1:0] Memory Throttling Mode
 *            - 0 - Non supported
 *            - 1 - Open-loop thermal throttlig (OTT) (not supported for QSSC-S4R)
 *            - 2 - Close-loop thermal throttling (CLTT)
 *            - 3 - None specified (do not change current setting)
 * 0x?? - Memory Device Group ID
 *      - 0 - cpu #1 group              
 *      - 1 - cpu #2 group              
 *      - 2 - cpu #3 group             
 *      - 3 - cpu #4 group             
 *      - 0xff - none specified
 *
 * bytes 4-11 - memory device presence bit map
 *      - 64-bit map forindicating the presence of a memory temp
 *        sensor for devices in the specified group ID.  Byte order is
 *        LSByte first.  Setting a bit to 1 indicates that the
 *        associated device is present and its temperature shoulde be
 *        monitored.  Device enumeration corresponds to bit-position
 *        in the bit-mask.  Valid only if Memory Device Group ID field
 *        is not set to FFh.
 *      - For QSSC S4R - 31:0 presence of DIMM temp device
 *                     - 39:32 presence of memory buffer (Mill Brook) temp device
 *                     - 63:40 - reserved
 *
 * Set Fan Control Configuration Response
 *
 * 0x89 - OEM cmd
 * 0x?? - Completion Code
 */

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

  assert (stringlen >= (max_blocks * IPMI_OEM_INTEL_SMTP_STRING_BLOCK_LENGTH));

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
  uint8_t channel_numbers[IPMI_CHANNEL_NUMBERS_MAX];
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
      char *endptr = NULL;
      unsigned int temp;

      errno = 0;
      temp = strtoul (state_data->prog_data->args->oem_options[0], &endptr, 10);
      if (errno
          || endptr[0] != '\0'
          || temp > UCHAR_MAX
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
					   IPMI_CHANNEL_NUMBERS_MAX)) < 0)
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
      char smtp_user_name[IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_STRING_LENGTH_MAX + 1];
      char email_address[IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_STRING_LENGTH_MAX + 1];
      char subject[IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_STRING_LENGTH_MAX + 1];
      char message_content[IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_STRING_LENGTH_MAX + 1];
      char sender_email_address[IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_STRING_LENGTH_MAX + 1];
      char smtp_host_name[IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_STRING_LENGTH_MAX + 1];
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

      memset (smtp_user_name, '\0', IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_STRING_LENGTH_MAX + 1);

      if (_get_smtp_configuration_string (state_data,
					  channel_numbers[i],
					  IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_SMTP_USER_NAME,
					  smtp_user_name,
					  IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_STRING_LENGTH_MAX) < 0)
	goto cleanup;
      
      memset (email_address, '\0', IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_STRING_LENGTH_MAX + 1);

      if (_get_smtp_configuration_string (state_data,
					  channel_numbers[i],
					  IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_EMAIL_ADDRESS,
					  email_address,
					  IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_STRING_LENGTH_MAX) < 0)
	goto cleanup;

      memset (subject, '\0', IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_STRING_LENGTH_MAX + 1);

      if (_get_smtp_configuration_string (state_data,
					  channel_numbers[i],
					  IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_SUBJECT,
					  subject,
					  IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_STRING_LENGTH_MAX) < 0)
	goto cleanup;

      memset (message_content, '\0', IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_STRING_LENGTH_MAX + 1);

      if (_get_smtp_configuration_string (state_data,
					  channel_numbers[i],
					  IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_MESSAGE_CONTENT,
					  message_content,
					  IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_STRING_LENGTH_MAX) < 0)
	goto cleanup;

      memset (sender_email_address, '\0', IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_STRING_LENGTH_MAX + 1);

      if (_get_smtp_configuration_string (state_data,
					  channel_numbers[i],
					  IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_SENDER_EMAIL_ADDRESS,
					  sender_email_address,
					  IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_STRING_LENGTH_MAX) < 0)
	goto cleanup;

      memset (smtp_host_name, '\0', IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_STRING_LENGTH_MAX + 1);

      if (_get_smtp_configuration_string (state_data,
					  channel_numbers[i],
					  IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_SMTP_HOST_NAME,
					  smtp_host_name,
					  IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_STRING_LENGTH_MAX) < 0)
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
  uint8_t buf[IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_STRING_LENGTH_MAX];
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

  switch (parameter_selector)
    {
    case IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_SMTP_USER_NAME:
      max_blocks = IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_SMTP_USER_NAME_MAX_BLOCKS;
      break;
    case IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_USER_PASSWORD:
      max_blocks = IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_USER_PASSWORD_MAX_BLOCKS;
      break;
    case IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_EMAIL_ADDRESS:
      max_blocks = IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_EMAIL_ADDRESS_MAX_BLOCKS;
      break;
    case IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_SUBJECT:
      max_blocks = IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_SUBJECT_MAX_BLOCKS;
      break;
    case IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_MESSAGE_CONTENT:
      max_blocks = IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_MESSAGE_CONTENT_MAX_BLOCKS;
      break;
    case IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_SENDER_EMAIL_ADDRESS:
      max_blocks = IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_SENDER_EMAIL_ADDRESS_MAX_BLOCKS;
      break;
    case IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_SMTP_HOST_NAME:
      max_blocks = IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_SMTP_HOST_NAME_MAX_BLOCKS;
      break;
    }
  
  if (strlen (string) > (max_blocks * IPMI_OEM_INTEL_SMTP_STRING_BLOCK_LENGTH))
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "value for key %s too long\n",
		       key);
      goto cleanup;
    }

  /* One extra check w/ passwords */
  if (parameter_selector == IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_USER_PASSWORD
      && strlen (string) > IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_USER_PASSWORD_LENGTH_MAX)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "value for key %s too long\n",
		       key);
      goto cleanup;
    }
  
  /* according to docs, must pad 00h to max blocks */
  memset (buf, '\0', IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_STRING_LENGTH_MAX);
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
					    buf + j * IPMI_OEM_INTEL_SMTP_STRING_BLOCK_LENGTH,
					    IPMI_OEM_INTEL_SMTP_STRING_BLOCK_LENGTH) < 0)
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
  uint8_t channel_numbers[IPMI_CHANNEL_NUMBERS_MAX];
  unsigned int channel_numbers_count = 0;
  uint8_t smtpenable = 0;
  uint32_t smtpserveraddress = 0;
  char smtp_user_name[IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_STRING_LENGTH_MAX + 1];
  char user_password[IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_STRING_LENGTH_MAX + 1];
  char email_address[IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_STRING_LENGTH_MAX + 1];
  char subject[IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_STRING_LENGTH_MAX + 1];
  char message_content[IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_STRING_LENGTH_MAX + 1];
  char sender_email_address[IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_STRING_LENGTH_MAX + 1];
  char smtp_host_name[IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_STRING_LENGTH_MAX + 1];
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
      char *endptr = NULL;
      unsigned int temp;

      errno = 0;
      temp = strtoul (state_data->prog_data->args->oem_options[0], &endptr, 10);
      if (!(errno
	    || endptr[0] != '\0'
	    || temp > UCHAR_MAX
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
					   IPMI_CHANNEL_NUMBERS_MAX)) < 0)
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
				     IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_STRING_LENGTH_MAX) < 0)
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
				     IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_STRING_LENGTH_MAX) < 0)
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
	  
	  memset (user_password, '\0', IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_STRING_LENGTH_MAX + 1);
        }
      else if (!strcasecmp (key, "emailaddress"))
        {
	  uint8_t string_length = 0;

	  if (ipmi_oem_parse_string (state_data,
				     i,
				     value, 
				     &string_length,
				     email_address,
				     IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_STRING_LENGTH_MAX) < 0)
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
				     IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_STRING_LENGTH_MAX) < 0)
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
				     IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_STRING_LENGTH_MAX) < 0)
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
				     IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_STRING_LENGTH_MAX) < 0)
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
				     IPMI_OEM_INTEL_SMTP_CONFIGURATION_PARAMETER_STRING_LENGTH_MAX) < 0)
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
ipmi_oem_intel_get_power_restore_delay (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  uint16_t delay = 0; 
  int rs_len;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  /* Intel S2600JF/Appro 512X
   *
   * Request 
   *
   * 0x30 - OEM network function
   * 0x55 - OEM cmd
   *
   * Response 
   *
   * 0x55 - OEM cmd
   * 0x?? - Completion Code
   * 0x?? - delay setting (MSB)
   *      - [7:3] - reserved
   *      - [2:0] - most significant 3 bits
   * 0x?? - delay setting (LSB)
   *
   * delay setting is 11 bits total.
   */

  bytes_rq[0] = IPMI_CMD_OEM_INTEL_GET_POWER_RESTORE_DELAY;

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_INTEL_GENERIC_RQ, /* network function */
                              bytes_rq, /* data */
                              1, /* num bytes */
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
                                                   4,
                                                   IPMI_CMD_OEM_INTEL_GET_POWER_RESTORE_DELAY,
                                                   IPMI_NET_FN_OEM_INTEL_GENERIC_RS,
                                                   NULL) < 0)
    goto cleanup;

  delay = ((bytes_rs[2] & IPMI_OEM_INTEL_POWER_RESTORE_DELAY_MSB_MASK) << 8) | bytes_rs[3];

  pstdout_printf (state_data->pstate, "%u\n", delay);

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_intel_set_power_restore_delay (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  unsigned int tmp;
  char *endptr;
  uint16_t delay = 0; 
  int rs_len;
  int rv = -1;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 1);

  errno = 0;
  
  tmp = strtoul (state_data->prog_data->args->oem_options[0],
		 &endptr,
		 10);
  if (errno
      || endptr[0] != '\0'
      || tmp > IPMI_OEM_INTEL_POWER_RESTORE_DELAY_MAX)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "%s:%s invalid OEM option argument '%s'\n",
		       state_data->prog_data->args->oem_id,
		       state_data->prog_data->args->oem_command,
		       state_data->prog_data->args->oem_options[0]);
      goto cleanup;
    }
  
  delay = tmp;
  
  /* Intel S2600JF/Appro 512X
   *
   * Request 
   *
   * 0x30 - OEM network function
   * 0x54 - OEM cmd
   * 0x?? - delay setting (MSB)
   *      - [7:3] - reserved
   *      - [2:0] - most significant 3 bits
   * 0x?? - delay setting (LSB)
   *
   * Response 
   *
   * 0x55 - OEM cmd
   * 0x?? - Completion Code
   *
   * delay setting is 11 bits total.
   */

  bytes_rq[0] = IPMI_CMD_OEM_INTEL_SET_POWER_RESTORE_DELAY;
  bytes_rq[1] = (delay >> 8) & IPMI_OEM_INTEL_POWER_RESTORE_DELAY_MSB_MASK;
  bytes_rq[2] = (delay & IPMI_OEM_INTEL_POWER_RESTORE_DELAY_LSB_MASK);

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_INTEL_GENERIC_RQ, /* network function */
                              bytes_rq, /* data */
                              3, /* num bytes */
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
                                                   IPMI_CMD_OEM_INTEL_SET_POWER_RESTORE_DELAY,
                                                   IPMI_NET_FN_OEM_INTEL_GENERIC_RS,
                                                   NULL) < 0)
    goto cleanup;

  rv = 0;
 cleanup:
  return (rv);
}

#if 0
/* Cannot verify - need newer firmware from Intel */ 
int
ipmi_oem_intel_get_bmc_service_status (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  int rs_len;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  /* Intel S2600JF/Appro 512X
   *
   * Request 
   *
   * 0x30 - OEM network function
   * 0xB2 - OEM cmd
   *
   * Response 
   *
   * 0xB2 - OEM cmd
   * 0x?? - Completion Code
   * 0x?? - Standard Services bit pattern
   *      - bit 7 - 0 - ssh service is not running
   *                1 - ssh service is running
   *        bit 6 - reserved
   *        bit 5 - 0 - http/https services not running
   *                1 - http/https services running
   *        bit 4:0 - reserved
   * 0x?? - OEM specific Services bit pattern
   *      - bit 7 - 0 - kvm service is not running
   *                1 - kvm service is running
   *        bit 6:0 - reserved 
   */

  bytes_rq[0] = IPMI_CMD_OEM_INTEL_GET_BMC_SERVICE_STATUS;

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_INTEL_GENERIC_RQ, /* network function */
                              bytes_rq, /* data */
                              2, /* num bytes */
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
                                                   4,
                                                   IPMI_CMD_OEM_INTEL_GET_BMC_SERVICE_STATUS,
                                                   IPMI_NET_FN_OEM_INTEL_GENERIC_RS,
                                                   NULL) < 0)
    goto cleanup;

  pstdout_printf (state_data->pstate, "SSH: %s\n",
		  (bytes_rs[2] & IPMI_OEM_INTEL_STANDARD_SERVICES_SSH) ? "Enabled" : "Disabled");
  
  pstdout_printf (state_data->pstate, "HTTP/HTTPS: %s\n",
		  (bytes_rs[2] & IPMI_OEM_INTEL_STANDARD_SERVICES_HTTP) ? "Enabled" : "Disabled");
  
  pstdout_printf (state_data->pstate, "KVM: %s\n",
		  (bytes_rs[2] & IPMI_OEM_INTEL_OEM_SPECIFIC_SERVICES_KVM) ? "Enabled" : "Disabled");

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_intel_set_bmc_service_status (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  uint8_t standard_services = 0;
  uint8_t oem_specific_services = 0;
  int rs_len;
  int rv = -1;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 2);

  if (strcasecmp (state_data->prog_data->args->oem_options[0], "enable")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "disable"))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[0]);
      goto cleanup;
    }
  
  if (strcasecmp (state_data->prog_data->args->oem_options[1], "ssh")
      && strcasecmp (state_data->prog_data->args->oem_options[1], "http")
      && strcasecmp (state_data->prog_data->args->oem_options[1], "kvm"))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[1]);
      goto cleanup;
    }

  /* Intel S2600JF/Appro 512X
   *
   * Request 
   *
   * 0x30 - OEM network function
   * 0xB1 - OEM cmd
   * 0x?? - 00h - disable given services
   *        01h - enable given services
   * 0x?? - Standard Services bit pattern
   *      - bit 7 - 0 - ssh service is not running
   *                1 - ssh service is running
   *        bit 6 - reserved
   *        bit 5 - 0 - http/https services not running
   *                1 - http/https services running
   *        bit 4:0 - reserved
   * 0x?? - OEM specific Services bit pattern
   *      - bit 7 - 0 - kvm service is not running
   *                1 - kvm service is running
   *        bit 6:0 - reserved 
   *
   * Response 
   *
   * 0xB1 - OEM cmd
   * 0x?? - Completion Code
   */

  bytes_rq[0] = IPMI_CMD_OEM_INTEL_CONTROL_BMC_SERVICES;
  if (!strcasecmp (state_data->prog_data->args->oem_options[0], "enable"))
    bytes_rq[1] = IPMI_OEM_INTEL_ENABLE_SERVICES;
  else
    bytes_rq[1] = IPMI_OEM_INTEL_DISABLE_SERVICES;

  if (!strcasecmp (state_data->prog_data->args->oem_options[1], "ssh"))
    {
      bytes_rq[2] = IPMI_OEM_INTEL_STANDARD_SERVICES_SSH;
      bytes_rq[3] = 0;
    }
  else if (!strcasecmp (state_data->prog_data->args->oem_options[1], "http"))
    {
      bytes_rq[2] = IPMI_OEM_INTEL_STANDARD_SERVICES_HTTP;
      bytes_rq[3] = 0;
    }
  else if (!strcasecmp (state_data->prog_data->args->oem_options[1], "kvm"))
    {
      bytes_rq[2] = 0;
      bytes_rq[3] = IPMI_OEM_INTEL_OEM_SPECIFIC_SERVICES_KVM;
    }

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_INTEL_GENERIC_RQ, /* network function */
                              bytes_rq, /* data */
                              4, /* num bytes */
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
                                                   IPMI_CMD_OEM_INTEL_CONTROL_BMC_SERVICES,
                                                   IPMI_NET_FN_OEM_INTEL_GENERIC_RS,
                                                   NULL) < 0)
    goto cleanup;
  
  rv = 0;
 cleanup:
  return (rv);
}
#endif

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
   * Intel S2600JF/Appro 512X
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
