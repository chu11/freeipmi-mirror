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

#if HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <assert.h>

#include "ipmi-config.h"
#include "ipmi-config-map.h"
#include "ipmi-config-section.h"
#include "ipmi-config-utils.h"
#include "ipmi-config-validate.h"

#include "freeipmi-portability.h"
#include "pstdout.h"

/* Convenience struct */
struct get_power_limit_data
{
  uint8_t exception_actions;
  uint16_t power_limit_requested;
  uint32_t correction_time_limit;
  uint16_t management_application_statistics_sampling_period;
};

static ipmi_config_err_t
asset_tag_checkout (ipmi_config_state_data_t *state_data,
		    const char *section_name,
		    struct ipmi_config_keyvalue *kv)
{
  fiid_obj_t obj_cmd_rs = NULL;
  char asset_tag_data[IPMI_DCMI_MAX_ASSET_TAG_LENGTH + 1];
  int data_len;
  unsigned int offset = 0;
  uint8_t total_asset_tag_length = 0;
  uint8_t bytes_to_read = IPMI_DCMI_ASSET_TAG_NUMBER_OF_BYTES_TO_READ_MAX;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_dcmi_get_asset_tag_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  memset (asset_tag_data, '\0', IPMI_DCMI_MAX_ASSET_TAG_LENGTH + 1);

  while (1)
    {
      uint64_t val;

      if (!offset
          || ((total_asset_tag_length - offset) >= IPMI_DCMI_ASSET_TAG_NUMBER_OF_BYTES_TO_READ_MAX))
        bytes_to_read = IPMI_DCMI_ASSET_TAG_NUMBER_OF_BYTES_TO_READ_MAX;
      else 
        bytes_to_read = total_asset_tag_length - offset;
      
      if (ipmi_cmd_dcmi_get_asset_tag (state_data->ipmi_ctx,
                                       offset,
                                       bytes_to_read,
                                       obj_cmd_rs) < 0)
        {
	  if (ipmi_config_param_errnum_is_non_fatal (state_data,
						     obj_cmd_rs,
						     &ret))
	    rv = ret;

	  if (rv == IPMI_CONFIG_ERR_FATAL_ERROR
	      || state_data->prog_data->args->common_args.debug)
	    pstdout_fprintf (state_data->pstate,
			     stderr,
			     "ipmi_cmd_dcmi_get_asset_tag: %s\n",
			     ipmi_ctx_errormsg (state_data->ipmi_ctx));
	  
	  goto cleanup;
        }

      if (FIID_OBJ_GET (obj_cmd_rs,
                        "total_asset_tag_length",
                        &val) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_get: 'total_asset_tag_length': %s\n",
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }
      total_asset_tag_length = val;

      if (!total_asset_tag_length)
        break;

      if ((data_len = fiid_obj_get_data (obj_cmd_rs,
                                         "data",
                                         asset_tag_data + offset,
                                         IPMI_DCMI_MAX_ASSET_TAG_LENGTH - offset)) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_get_data: 'data': %s\n",
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }
      offset += data_len;

      if (offset >= total_asset_tag_length)
        break;
    }

  /* Handle special case UTF-8 encoding w/ BOM prefix */
  if (asset_tag_data[0] == IPMI_DCMI_ASSET_TAG_UTF8_BOM_BYTE0
      && asset_tag_data[1] == IPMI_DCMI_ASSET_TAG_UTF8_BOM_BYTE1
      && asset_tag_data[2] == IPMI_DCMI_ASSET_TAG_UTF8_BOM_BYTE2)
    {
      /* achu: I think this is right for UTF-8 in libc and is
       * portable, but I would bet some systems won't like this.
       */
      if (ipmi_config_section_update_keyvalue_output (state_data,
						      kv,
						      &asset_tag_data[3]) < 0)
	return (IPMI_CONFIG_ERR_FATAL_ERROR);
    }
  else
    {
      if (ipmi_config_section_update_keyvalue_output (state_data,
						      kv,
						      asset_tag_data) < 0)
	return (IPMI_CONFIG_ERR_FATAL_ERROR);
    }

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
asset_tag_commit (ipmi_config_state_data_t *state_data,
                         const char *section_name,
                         const struct ipmi_config_keyvalue *kv)
{
  fiid_obj_t obj_cmd_rs = NULL;
  unsigned int offset = 0;
  char data_buf[IPMI_DCMI_MAX_ASSET_TAG_LENGTH + 1];
  unsigned int data_len;
  uint8_t bytes_to_write = IPMI_DCMI_ASSET_TAG_NUMBER_OF_BYTES_TO_WRITE_MAX;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  /* achu:
   *
   * DCMI v1.1 spec is unclear if the entire buffer needs to be
   * written or just the amount you desire.
   *
   * DCMI v1.5 spec strongly suggests you don't write the entire
   * buffer due to the results of the "total_asset_tag_length_written"
   * field.
   *
   * "Total Asset Tag Length. This is the length in bytes of the stored
   * Asset Tag after the Set operation has completed. The Asset Tag
   * length shall be set to the sum of the offset to write plus bytes
   * to write. For example, if offset to write is 32 and bytes to
   * write is 4, the Total Asset Tag Length returned will be 36."
   */

  data_len = strlen (kv->value_input);
  
  /* Write empty buffer */
  if (!data_len)
    data_len = IPMI_DCMI_MAX_ASSET_TAG_LENGTH;

  memset (data_buf, '\0', IPMI_DCMI_MAX_ASSET_TAG_LENGTH + 1);

  memcpy (data_buf, kv->value_input, strlen (kv->value_input));

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_dcmi_set_asset_tag_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  while (1)
    {
      uint64_t val;

      if ((data_len - offset) >= IPMI_DCMI_ASSET_TAG_NUMBER_OF_BYTES_TO_WRITE_MAX)
        bytes_to_write = IPMI_DCMI_ASSET_TAG_NUMBER_OF_BYTES_TO_WRITE_MAX;
      else 
        bytes_to_write = data_len - offset;
      
      if (ipmi_cmd_dcmi_set_asset_tag (state_data->ipmi_ctx,
                                       offset,
                                       bytes_to_write,
                                       data_buf + offset,
                                       data_len,
                                       obj_cmd_rs) < 0)
        {
	  if (ipmi_config_param_errnum_is_non_fatal (state_data,
						     obj_cmd_rs,
						     &ret))
	    rv = ret;
	  
	  if (rv == IPMI_CONFIG_ERR_FATAL_ERROR
	      || state_data->prog_data->args->common_args.debug)
	    pstdout_fprintf (state_data->pstate,
			     stderr,
			     "ipmi_cmd_dcmi_set_asset_tag: %s\n",
			     ipmi_ctx_errormsg (state_data->ipmi_ctx));
	  
	  goto cleanup;
        }

      if (FIID_OBJ_GET (obj_cmd_rs,
                        "total_asset_tag_length_written",
                        &val) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_get: 'total_asset_tag_length': %s\n",
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }

      /* DCMI 1.1 spec is unclear on "total_length_written", is it the
       * number of bytes just written or total bytes written so far?
       * 
       * DCMI 1.5 spec makes it clear that this is the number of bytes
       * written in total.  To defend against vendor mistakes, we
       * handle both situations.
       */
      if (val > bytes_to_write)
        offset += bytes_to_write;
      else
        offset += val;

      if (offset >= data_len)
        break;
    }

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_validate_t
asset_tag_validate (ipmi_config_state_data_t *state_data,
                           const char *section_name,
                           const char *key_name,
                           const char *value)
{
  assert (state_data);
  assert (section_name);
  assert (key_name);
  assert (value);

  if (!value || strlen (value) > IPMI_DCMI_MAX_ASSET_TAG_LENGTH)
    return (IPMI_CONFIG_VALIDATE_INVALID_VALUE);
  return (IPMI_CONFIG_VALIDATE_VALID_VALUE);
}
    
static ipmi_config_err_t
management_controller_identifier_string_checkout (ipmi_config_state_data_t *state_data,
						  const char *section_name,
						  struct ipmi_config_keyvalue *kv)
{
  fiid_obj_t obj_cmd_rs = NULL;
  char management_controller_identifier_string_data[IPMI_DCMI_MAX_MANAGEMENT_CONTROLLER_IDENTIFIER_STRING_LENGTH];
  int data_len;
  unsigned int offset = 0;
  uint8_t total_length = 0;
  uint8_t bytes_to_read = IPMI_DCMI_MANAGEMENT_CONTROLLER_IDENTIFIER_STRING_NUMBER_OF_BYTES_TO_READ_MAX;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_dcmi_get_management_controller_identifier_string_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  memset (management_controller_identifier_string_data, '\0', IPMI_DCMI_MAX_MANAGEMENT_CONTROLLER_IDENTIFIER_STRING_LENGTH);

  while (1)
    {
      uint64_t val;

      if (!offset
          || ((total_length - offset) >= IPMI_DCMI_MANAGEMENT_CONTROLLER_IDENTIFIER_STRING_NUMBER_OF_BYTES_TO_READ_MAX))
        bytes_to_read = IPMI_DCMI_MANAGEMENT_CONTROLLER_IDENTIFIER_STRING_NUMBER_OF_BYTES_TO_READ_MAX;
      else 
        bytes_to_read = total_length - offset;
      
      if (ipmi_cmd_dcmi_get_management_controller_identifier_string (state_data->ipmi_ctx,
								     offset,
								     bytes_to_read,
								     obj_cmd_rs) < 0)
        {
	  if (ipmi_config_param_errnum_is_non_fatal (state_data,
						     obj_cmd_rs,
						     &ret))
	    rv = ret;
	  
	  if (rv == IPMI_CONFIG_ERR_FATAL_ERROR
	      || state_data->prog_data->args->common_args.debug)
	    pstdout_fprintf (state_data->pstate,
			     stderr,
			     "ipmi_cmd_dcmi_get_management_controller_identifier_string: %s\n",
			     ipmi_ctx_errormsg (state_data->ipmi_ctx));

          goto cleanup;
        }
      
      if (FIID_OBJ_GET (obj_cmd_rs,
                        "total_length",
                        &val) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_get: 'total_length': %s\n",
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }
      total_length = val;

      if (!total_length)
        break;

      if ((data_len = fiid_obj_get_data (obj_cmd_rs,
                                         "data",
                                         management_controller_identifier_string_data + offset,
                                         IPMI_DCMI_MAX_MANAGEMENT_CONTROLLER_IDENTIFIER_STRING_LENGTH - offset)) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_get_data: 'data': %s\n",
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }
      offset += data_len;
      
      if (offset >= total_length)
        break;
    }
  
  if (ipmi_config_section_update_keyvalue_output (state_data,
						  kv,
						  management_controller_identifier_string_data) < 0)
	return (IPMI_CONFIG_ERR_FATAL_ERROR);

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
management_controller_identifier_string_commit (ipmi_config_state_data_t *state_data,
						const char *section_name,
						const struct ipmi_config_keyvalue *kv)
{
  fiid_obj_t obj_cmd_rs = NULL;
  unsigned int offset = 0;
  char data_buf[IPMI_DCMI_MAX_MANAGEMENT_CONTROLLER_IDENTIFIER_STRING_LENGTH];
  unsigned int data_len;
  uint8_t bytes_to_write = IPMI_DCMI_MANAGEMENT_CONTROLLER_IDENTIFIER_STRING_NUMBER_OF_BYTES_TO_WRITE_MAX;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  /* achu:
   *
   * According to DCMI v1.5 draft
   * 
   * "The presence of the null terminator among bytes to shall be
   * considered as indicating the last transfer of the Management
   * Controller Identifier string"
   *
   * So I am assuming we don't need to write the entire buffer.  But
   * we must include the NUL byte at the end.
   */

  /* +1 for NUL char */
  data_len = strlen (kv->value_input) + 1;

  /* Write NUL char */
  if (!data_len)
    data_len = 1;

  memset (data_buf, '\0', IPMI_DCMI_MAX_MANAGEMENT_CONTROLLER_IDENTIFIER_STRING_LENGTH);

  memcpy (data_buf, kv->value_input, strlen (kv->value_input));

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_dcmi_set_management_controller_identifier_string_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  while (1)
    {
      uint64_t val;

      if ((data_len - offset) >= IPMI_DCMI_MANAGEMENT_CONTROLLER_IDENTIFIER_STRING_NUMBER_OF_BYTES_TO_WRITE_MAX)
        bytes_to_write = IPMI_DCMI_MANAGEMENT_CONTROLLER_IDENTIFIER_STRING_NUMBER_OF_BYTES_TO_WRITE_MAX;
      else 
        bytes_to_write = data_len - offset;
      
      if (ipmi_cmd_dcmi_set_management_controller_identifier_string (state_data->ipmi_ctx,
                                                                     offset,
                                                                     bytes_to_write,
                                                                     data_buf + offset,
                                                                     data_len,
                                                                     obj_cmd_rs) < 0)
        {
	  if (ipmi_config_param_errnum_is_non_fatal (state_data,
						     obj_cmd_rs,
						     &ret))
	    rv = ret;
	  
	  if (rv == IPMI_CONFIG_ERR_FATAL_ERROR
	      || state_data->prog_data->args->common_args.debug)
	    pstdout_fprintf (state_data->pstate,
			     stderr,
			     "ipmi_cmd_dcmi_set_management_controller_identifier_string: %s\n",
			     ipmi_ctx_errormsg (state_data->ipmi_ctx));
          goto cleanup;
        }

      if (FIID_OBJ_GET (obj_cmd_rs,
                        "total_length_written",
                        &val) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_get: 'total_management_controller_identifier_string_length': %s\n",
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }

      /* DCMI 1.1 spec is unclear on "total_length_written", is it the
       * number of bytes just written or total bytes written so far?
       * 
       * DCMI 1.5 spec makes it clear that this is the number of bytes
       * written in total.  To defend against vendor mistakes, we
       * handle both situations.
       */
      if (val > bytes_to_write)
        offset += bytes_to_write;
      else
        offset += val;

      if (offset >= data_len)
        break;
    }
  
  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_validate_t
management_controller_identifier_string_validate (ipmi_config_state_data_t *state_data,
						  const char *section_name,
						  const char *key_name,
						  const char *value)
{
  assert (state_data);
  assert (section_name);
  assert (key_name);
  assert (value);

  /* IPMI_DCMI_MAX_MANAGEMENT_CONTROLLER_IDENTIFIER_STRING_LENGTH includes NUL char, so subtract 1 in check */
  if (!value || strlen (value) > (IPMI_DCMI_MAX_MANAGEMENT_CONTROLLER_IDENTIFIER_STRING_LENGTH - 1))
    return (IPMI_CONFIG_VALIDATE_INVALID_VALUE);
  return (IPMI_CONFIG_VALIDATE_VALID_VALUE);
}

static ipmi_config_err_t
_get_power_limit (ipmi_config_state_data_t *state_data,
		  struct get_power_limit_data *gpld,
		  int *no_set_power_limit_flag)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  int no_set_power_limit_error_flag = 0;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;

  assert (state_data);
  assert (gpld);

  if (no_set_power_limit_flag)
    (*no_set_power_limit_flag) = 0;

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_dcmi_get_power_limit_rs)))
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "fiid_obj_create: %s",
		       strerror (errno));
      goto cleanup;
    }

  /* IPMI Workaround/Interpretation
   *
   * The DCMI spec indicates a potential completion code for the "Get
   * Power Limit" command as "No Set Power Limit" (0x80).  FreeIPMI
   * originally interpreted this to mean the "Set Power Limit" command
   * was not available.  Atleast one vendor interpreted this to mean
   * "No Power Limit Set".  One can consider this an English
   * interpretation issue of 'No set POWER LIMIT' vs. 'No SET POWER
   * LIMIT' (i.e. is "set" a verb or part of a proper noun referencing
   * the DCMI command).  Confounding this issue is the fact that the
   * example implementation in Intel's DCMItool implements the former,
   * while the DCMI Conformance test suite implements the latter.  In
   * addition to this, with the latter interpretation, it need not be
   * an indication of an error, but rather a flag.  So the rest of the
   * packet can be completely full of legitimate data.
   *
   * So how do we handle this?
   *
   * If we hit "No Set Power Limit", try to read data.  If we can't
   * read data (b/c it's not set), fail out, but preserve the "No Set
   * Power Limit" error message.
   */

  if (ipmi_cmd_dcmi_get_power_limit (state_data->ipmi_ctx,
				     obj_cmd_rs) < 0)
    {
      ipmi_config_err_t ret;

      if (ipmi_ctx_errnum (state_data->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE
	  && ipmi_check_completion_code (obj_cmd_rs,
					 IPMI_COMP_CODE_DCMI_NO_SET_POWER_LIMIT) == 1)
	{
	  if (state_data->prog_data->args->common_args.debug)
	    pstdout_fprintf (state_data->pstate,
			     stderr,
			     "ipmi_cmd_dcmi_get_power_limit: %s",
			     IPMI_COMP_CODE_DCMI_NO_SET_POWER_LIMIT_STR);

	  if (no_set_power_limit_flag)
	    (*no_set_power_limit_flag) = 1;
 
	  no_set_power_limit_error_flag++;
	  goto read_data;
	}

      if (ipmi_config_param_errnum_is_non_fatal (state_data,
						 obj_cmd_rs,
						 &ret))
	rv = ret;

      if (rv == IPMI_CONFIG_ERR_FATAL_ERROR
	  || state_data->prog_data->args->common_args.debug)
	pstdout_fprintf (state_data->pstate,
			 stderr,
			 "ipmi_cmd_get_pef_configuration_parameters_event_filter_table: %s\n",
			 ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

 read_data:

  if (FIID_OBJ_GET (obj_cmd_rs,
		    "exception_actions",
		    &val) < 0)
    {
      if (!no_set_power_limit_error_flag
	  || fiid_obj_errnum (obj_cmd_rs) != FIID_ERR_DATA_NOT_AVAILABLE)
	pstdout_fprintf (state_data->pstate,
			 stderr,
			 "fiid_obj_get: 'exception_actions': %s",
			 fiid_obj_errormsg (obj_cmd_rs));

      if (no_set_power_limit_error_flag)
	rv = IPMI_CONFIG_ERR_NON_FATAL_ERROR;

      goto cleanup;
    }
  gpld->exception_actions = val;
  
  if (FIID_OBJ_GET (obj_cmd_rs,
		    "power_limit_requested",
		    &val) < 0)
    {
      if (!no_set_power_limit_error_flag
	  || fiid_obj_errnum (obj_cmd_rs) != FIID_ERR_DATA_NOT_AVAILABLE)
	pstdout_fprintf (state_data->pstate,
			 stderr,
			 "fiid_obj_get: 'power_limit_requested': %s",
			 fiid_obj_errormsg (obj_cmd_rs));

      if (no_set_power_limit_error_flag)
	rv = IPMI_CONFIG_ERR_NON_FATAL_ERROR;

      goto cleanup;
    }
  gpld->power_limit_requested = val;

  if (FIID_OBJ_GET (obj_cmd_rs,
		    "correction_time_limit",
		    &val) < 0)
    {
      if (!no_set_power_limit_error_flag
	  || fiid_obj_errnum (obj_cmd_rs) != FIID_ERR_DATA_NOT_AVAILABLE)
	pstdout_fprintf (state_data->pstate,
			 stderr,
			 "fiid_obj_get: 'correction_time_limit': %s",
			 fiid_obj_errormsg (obj_cmd_rs));

      if (no_set_power_limit_error_flag)
	rv = IPMI_CONFIG_ERR_NON_FATAL_ERROR;

      goto cleanup;
    }
  gpld->correction_time_limit = val;

  if (FIID_OBJ_GET (obj_cmd_rs,
		    "management_application_statistics_sampling_period",
		    &val) < 0)
    {
      if (!no_set_power_limit_error_flag
	  || fiid_obj_errnum (obj_cmd_rs) != FIID_ERR_DATA_NOT_AVAILABLE)
	pstdout_fprintf (state_data->pstate,
			 stderr,
			 "fiid_obj_get: 'management_application_statistics_sampling_period': %s",
			 fiid_obj_errormsg (obj_cmd_rs));

      if (no_set_power_limit_error_flag)
	rv = IPMI_CONFIG_ERR_NON_FATAL_ERROR;

      goto cleanup;
    }
  gpld->management_application_statistics_sampling_period = val;

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
_set_power_limit (ipmi_config_state_data_t *state_data,
		  const char *section_name,
		  struct get_power_limit_data *gpld,
		  int no_set_power_limit_flag)
{
  fiid_obj_t obj_cmd_rs = NULL;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;

  assert (state_data);
  assert (section_name);
  assert (gpld);

  /* IPMI Workaround/Interpretation
   *
   * The DCMI spec indicates a potential completion code for the
   * "Get Power Limit" command as "No Set Power Limit" (0x80).
   * FreeIPMI originally interpreted this to mean the "Set Power
   * Limit" command was not available.  Atleast one vendor
   * interpreted this to mean "No Power Limit Set".  One can
   * consider this an English interpretation issue of 'No set
   * POWER LIMIT' vs. 'No SET POWER LIMIT' (i.e. is "set" a verb
   * or part of a proper noun referencing the DCMI command).
   * Confounding this issue is the fact that the example
   * implementation in Intel's DCMItool implements the former,
   * while the DCMI Conformance test suite implements the latter.
   * In addition to this, with the latter interpretation, it need
   * not be an indication of an error, but rather a flag.  So the
   * rest of the packet can be completely full of legitimate data.
   * 
   * So we will do the following.
   *
   * If the "No Set Power Limit" completion code is returned and 
   * we were able to read all of the fields, _get_power_limit() will
   * return normally, so we don't need to worry about this case. 
   *
   * If the "No Set Power Limit", completion code is returned and
   * we were *not* able to read all of the fields, we won't have
   * values from "Get Power Limit" and won't know how to do the
   * configuration properly in "Set Power Limit".  So we will
   * require that the user input all fields for "Set Power Limit".
   */

  if (no_set_power_limit_flag)
    {
      struct ipmi_config_section *section;
      struct ipmi_config_keyvalue *kv;

      section = state_data->sections;
      while (section)
	{
	  if (!strcasecmp (section_name, section->section_name))
	    break;
	  section = section->next;
	}

      /* shouldn't be possible */
      if (!section)
	goto cleanup;

      if ((kv = ipmi_config_find_keyvalue (section, "Policy_Type")))
	gpld->power_limit_requested = atoi (kv->value_input);

      if ((kv = ipmi_config_find_keyvalue (section, "Policy_Enabled")))
	gpld->correction_time_limit = strtoul (kv->value_input, NULL, 0);

      if ((kv = ipmi_config_find_keyvalue (section, "Management_Application_Statistics_Sampling_Period")))
	gpld->management_application_statistics_sampling_period = atoi (kv->value_input);

      if ((kv = ipmi_config_find_keyvalue (section, "Exception_Actions")))
	{
	  int num = exception_actions_number (kv->value_input);

	  if (num < 0)
	    /* previously checked for correctness, so no error check */
	    gpld->exception_actions = strtol (kv->value_input, NULL, 0);
	  else
	    gpld->exception_actions = num;
	}
    }

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_dcmi_set_power_limit_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }
  
  if (ipmi_cmd_dcmi_set_power_limit (state_data->ipmi_ctx,
				     gpld->exception_actions,
				     gpld->power_limit_requested,
				     gpld->correction_time_limit,
				     gpld->management_application_statistics_sampling_period,
				     obj_cmd_rs) < 0)
    {
      ipmi_config_err_t ret;

      if ((ipmi_check_completion_code (obj_cmd_rs,
				       IPMI_COMP_CODE_DCMI_POWER_LIMIT_OUT_OF_RANGE) == 1)
	  || (ipmi_check_completion_code (obj_cmd_rs,
                                              IPMI_COMP_CODE_DCMI_CORRECTION_TIME_OUT_OF_RANGE) == 1)
	  || (ipmi_check_completion_code (obj_cmd_rs,
					  IPMI_COMP_CODE_DCMI_STATISTICS_REPORTING_PERIOD_OUT_OF_RANGE) == 1))
	rv = IPMI_CONFIG_ERR_NON_FATAL_ERROR;
      else
	{
	  if (ipmi_config_param_errnum_is_non_fatal (state_data,
						     obj_cmd_rs,
						     &ret))
	    rv = ret;
	}

      if (rv == IPMI_CONFIG_ERR_FATAL_ERROR
	  || state_data->prog_data->args->common_args.debug)
	{
	  if (ipmi_ctx_errnum (state_data->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE
	      && ipmi_check_completion_code (obj_cmd_rs,
					     IPMI_COMP_CODE_DCMI_POWER_LIMIT_OUT_OF_RANGE) == 1)
	    pstdout_fprintf (state_data->pstate,
			     stderr,
			     "ipmi_cmd_dcmi_set_power_limit: %s\n",
			     IPMI_COMP_CODE_DCMI_POWER_LIMIT_OUT_OF_RANGE_STR);
	  else if (ipmi_ctx_errnum (state_data->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE
		   && ipmi_check_completion_code (obj_cmd_rs,
						  IPMI_COMP_CODE_DCMI_CORRECTION_TIME_OUT_OF_RANGE) == 1)
	    pstdout_fprintf (state_data->pstate,
			     stderr,
			     "ipmi_cmd_dcmi_set_power_limit: %s\n",
			     IPMI_COMP_CODE_DCMI_CORRECTION_TIME_OUT_OF_RANGE_STR);
	  else if (ipmi_ctx_errnum (state_data->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE
		   && ipmi_check_completion_code (obj_cmd_rs,
						  IPMI_COMP_CODE_DCMI_STATISTICS_REPORTING_PERIOD_OUT_OF_RANGE) == 1)
	    pstdout_fprintf (state_data->pstate,
			     stderr,
			     "ipmi_cmd_dcmi_set_power_limit: %s\n",
			     IPMI_COMP_CODE_DCMI_STATISTICS_REPORTING_PERIOD_OUT_OF_RANGE_STR);
	  else
	    pstdout_fprintf (state_data->pstate,
			     stderr,
			     "ipmi_cmd_dcmi_set_power_limit: %s\n",
			     ipmi_ctx_errormsg (state_data->ipmi_ctx));
	}

      goto cleanup;
    }

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
power_limit_requested_checkout (ipmi_config_state_data_t *state_data,
				const char *section_name,
				struct ipmi_config_keyvalue *kv)
{
  ipmi_config_err_t ret;
  struct get_power_limit_data gpld;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_power_limit (state_data,
			       &gpld,
			       NULL)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);
  
  if (ipmi_config_section_update_keyvalue_output_unsigned_int (state_data,
							       kv,
							       gpld.power_limit_requested) < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);
  
  return (IPMI_CONFIG_ERR_SUCCESS);
}

static ipmi_config_err_t
power_limit_requested_commit (ipmi_config_state_data_t *state_data,
			      const char *section_name,
			      const struct ipmi_config_keyvalue *kv)
{
  ipmi_config_err_t ret;
  struct get_power_limit_data gpld;
  int no_set_power_limit_flag = 0;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_power_limit (state_data,
			       &gpld,
			       &no_set_power_limit_flag)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      if (no_set_power_limit_flag)
	return (_set_power_limit (state_data, section_name, &gpld, 1));
      else
	return (ret);
    }

  gpld.power_limit_requested = atoi (kv->value_input);

  return (_set_power_limit (state_data, section_name, &gpld, 0));
}

static ipmi_config_err_t
correction_time_limit_checkout (ipmi_config_state_data_t *state_data,
				const char *section_name,
				struct ipmi_config_keyvalue *kv)
{
  ipmi_config_err_t ret;
  struct get_power_limit_data gpld;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_power_limit (state_data,
			       &gpld,
			       NULL)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);
  
  if (ipmi_config_section_update_keyvalue_output_unsigned_int (state_data,
							       kv,
							       gpld.correction_time_limit) < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  return (IPMI_CONFIG_ERR_SUCCESS);
}

static ipmi_config_err_t
correction_time_limit_commit (ipmi_config_state_data_t *state_data,
			      const char *section_name,
			      const struct ipmi_config_keyvalue *kv)
{
  ipmi_config_err_t ret;
  struct get_power_limit_data gpld;
  int no_set_power_limit_flag = 0;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_power_limit (state_data,
			       &gpld,
			       &no_set_power_limit_flag)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      if (no_set_power_limit_flag)
	return (_set_power_limit (state_data, section_name, &gpld, 1));
      else
	return (ret);
    }

  /* already validated, no need to check for errors */
  gpld.correction_time_limit = strtoul (kv->value_input, NULL, 0);

  return (_set_power_limit (state_data, section_name, &gpld, 0));
}

static ipmi_config_err_t
management_application_statistics_sampling_period_checkout (ipmi_config_state_data_t *state_data,
							    const char *section_name,
							    struct ipmi_config_keyvalue *kv)
{
  ipmi_config_err_t ret;
  struct get_power_limit_data gpld;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_power_limit (state_data,
			       &gpld,
			       NULL)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);
  
  if (ipmi_config_section_update_keyvalue_output_unsigned_int (state_data,
							       kv,
							       gpld.management_application_statistics_sampling_period) < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  return (IPMI_CONFIG_ERR_SUCCESS);
}

static ipmi_config_err_t
management_application_statistics_sampling_period_commit (ipmi_config_state_data_t *state_data,
							  const char *section_name,
							  const struct ipmi_config_keyvalue *kv)
{
  ipmi_config_err_t ret;
  struct get_power_limit_data gpld;
  int no_set_power_limit_flag = 0;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_power_limit (state_data,
			       &gpld,
			       &no_set_power_limit_flag)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      if (no_set_power_limit_flag)
	return (_set_power_limit (state_data, section_name, &gpld, 1));
      else
	return (ret);
    }

  gpld.management_application_statistics_sampling_period = atoi (kv->value_input);

  return (_set_power_limit (state_data, section_name, &gpld, 0));
}

static ipmi_config_err_t
exception_actions_checkout (ipmi_config_state_data_t *state_data,
							    const char *section_name,
							    struct ipmi_config_keyvalue *kv)
{
  ipmi_config_err_t ret;
  struct get_power_limit_data gpld;
  char *str;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_power_limit (state_data,
			       &gpld,
			       NULL)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);
  
  str = exception_actions_string (gpld.exception_actions);

  if (str && strlen (str))
    {
      if (ipmi_config_section_update_keyvalue_output (state_data,
                                                      kv,
                                                      str) < 0)
        return (IPMI_CONFIG_ERR_FATAL_ERROR);
    }
  else
    {
      if (ipmi_config_section_update_keyvalue_output_hex (state_data,
							  kv,
							  gpld.exception_actions) < 0)
        return (IPMI_CONFIG_ERR_FATAL_ERROR);
    }

  return (IPMI_CONFIG_ERR_SUCCESS);
}

static ipmi_config_err_t
exception_actions_commit (ipmi_config_state_data_t *state_data,
							  const char *section_name,
							  const struct ipmi_config_keyvalue *kv)
{
  ipmi_config_err_t ret;
  struct get_power_limit_data gpld;
  int no_set_power_limit_flag = 0;
  int num;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_power_limit (state_data,
			       &gpld,
			       &no_set_power_limit_flag)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      if (no_set_power_limit_flag)
	return (_set_power_limit (state_data, section_name, &gpld, 1));
      else
	return (ret);
    }

  num = exception_actions_number (kv->value_input);

  if (num < 0)
    /* previously checked for correctness, so no error check */
    gpld.exception_actions = strtol (kv->value_input, NULL, 0);
  else
    gpld.exception_actions = num;

  return (_set_power_limit (state_data, section_name, &gpld, 0));
}

struct ipmi_config_section *
ipmi_config_dcmi_dcmi_conf_section_get (ipmi_config_state_data_t *state_data)
{
  struct ipmi_config_section *section = NULL;

  assert (state_data);

  if (!(section = ipmi_config_section_create (state_data,
					      "DCMI_Conf",
					      NULL,
					      NULL,
					      0,
					      NULL,
					      NULL)))
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "Asset_Tag",
                                   "Give valid string",
                                   0,
                                   asset_tag_checkout,
                                   asset_tag_commit,
                                   asset_tag_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "Management_Controller_Identifier_String",
                                   "Give valid string",
                                   0,
                                   management_controller_identifier_string_checkout,
                                   management_controller_identifier_string_commit,
                                   management_controller_identifier_string_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "Power_Limit_Requested",
                                   "Give valid number in Watts",
                                   0,
                                   power_limit_requested_checkout,
                                   power_limit_requested_commit,
				   number_range_two_bytes_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "Correction_Time_Limit",
                                   "Give valid number in milliseconds",
                                   0,
                                   correction_time_limit_checkout,
                                   correction_time_limit_commit,
				   number_range_four_bytes_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "Management_Application_Statistics_Sampling_Period",
                                   "Give valid number in seconds",
                                   0,
                                   management_application_statistics_sampling_period_checkout,
                                   management_application_statistics_sampling_period_commit,
				   number_range_two_bytes_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "Exception_Actions",
                                   "Give valid hex or NO_ACTION, HARD_POWER_OFF_SYSTEM, or LOG_EVENT_TO_SEL_ONLY",
                                   0,
                                   exception_actions_checkout,
                                   exception_actions_commit,
				   exception_actions_validate) < 0)
    goto cleanup;

  return (section);

 cleanup:
  if (section)
    ipmi_config_section_destroy (section);
  return (NULL);
}

