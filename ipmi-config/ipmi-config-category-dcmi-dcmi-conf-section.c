/*
 * Copyright (C) 2003-2013 FreeIPMI Core Team
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

  return (section);

 cleanup:
  if (section)
    ipmi_config_section_destroy (section);
  return (NULL);
}

