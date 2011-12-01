/*
 * Copyright (C) 2007-2012 FreeIPMI Core Team
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

#include "ipmi-pef-config.h"
#include "ipmi-pef-config-map.h"
#include "ipmi-pef-config-utils.h"
#include "ipmi-pef-config-validate.h"

#include "freeipmi-portability.h"
#include "pstdout.h"

/* achu: presumably there is no maximum.  We could read/write blocks
   forever based on block numbers.  However, we need to have some
   artificial max for the sake of ipmi-pef-config.
*/
#define PEF_ALERT_STRING_MAX_LEN 64

#define PEF_ALERT_STRING_BLOCK_SIZE 16

struct alert_string_keys {
  uint8_t event_filter_number;
  uint8_t alert_string_set;
};

static config_err_t
_get_alert_string_keys (ipmi_pef_config_state_data_t *state_data,
                        const char *section_name,
                        struct alert_string_keys *ask)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t string_selector;
  uint64_t val = 0;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;

  assert (state_data);
  assert (section_name);
  assert (ask);

  string_selector = atoi (section_name + strlen ("Alert_String_"));

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_pef_configuration_parameters_alert_string_keys_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_get_pef_configuration_parameters_alert_string_keys (state_data->ipmi_ctx,
                                                                   IPMI_GET_PEF_PARAMETER,
                                                                   string_selector,
                                                                   IPMI_PEF_CONFIGURATION_PARAMETERS_NO_BLOCK_SELECTOR,
                                                                   obj_cmd_rs) < 0)
    {
      config_err_t ret;

      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_get_pef_configuration_parameters_alert_string_keys: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      if (config_is_config_param_non_fatal_error (state_data->ipmi_ctx,
                                                  obj_cmd_rs,
                                                  &ret))
        rv = ret;

      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs, "filter_number", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'filter_number': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  ask->event_filter_number = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "set_number_for_string", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'set_number_for_string': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  ask->alert_string_set = val;

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static config_err_t
_set_alert_string_keys (ipmi_pef_config_state_data_t *state_data,
                        const char *section_name,
                        struct alert_string_keys *ask)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t string_selector;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;

  assert (state_data);
  assert (section_name);
  assert (ask);

  string_selector = atoi (section_name + strlen ("Alert_String_"));

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_set_pef_configuration_parameters_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_set_pef_configuration_parameters_alert_string_keys (state_data->ipmi_ctx,
                                                                   string_selector,
                                                                   ask->event_filter_number,
                                                                   ask->alert_string_set,
                                                                   obj_cmd_rs) < 0)
    {
      config_err_t ret;

      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_set_pef_configuration_parameters_alert_string_keys: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      if (config_is_config_param_non_fatal_error (state_data->ipmi_ctx,
                                                  obj_cmd_rs,
                                                  &ret))
        rv = ret;

      goto cleanup;
    }

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static config_err_t
event_filter_number_checkout (const char *section_name,
                              struct config_keyvalue *kv,
                              void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  struct alert_string_keys ask;
  config_err_t ret;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if ((ret = _get_alert_string_keys (state_data,
                                     section_name,
                                     &ask)) != CONFIG_ERR_SUCCESS)
    return (ret);

  if (config_section_update_keyvalue_output_unsigned_int (state_data->pstate,
                                                          kv,
                                                          ask.event_filter_number) < 0)
    return (CONFIG_ERR_FATAL_ERROR);

  return (CONFIG_ERR_SUCCESS);
}

static config_err_t
event_filter_number_commit (const char *section_name,
                            const struct config_keyvalue *kv,
                            void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  struct alert_string_keys ask;
  config_err_t ret;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if ((ret = _get_alert_string_keys (state_data,
                                     section_name,
                                     &ask)) != CONFIG_ERR_SUCCESS)
    return (ret);

  ask.event_filter_number = atoi (kv->value_input);

  return (_set_alert_string_keys (state_data,
                                  section_name,
                                  &ask));
}

static config_err_t
alert_string_set_checkout (const char *section_name,
                           struct config_keyvalue *kv,
                           void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  struct alert_string_keys ask;
  config_err_t ret;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if ((ret = _get_alert_string_keys (state_data,
                                     section_name,
                                     &ask)) != CONFIG_ERR_SUCCESS)
    return (ret);

  if (config_section_update_keyvalue_output_unsigned_int (state_data->pstate,
                                                          kv,
                                                          ask.alert_string_set) < 0)
    return (CONFIG_ERR_FATAL_ERROR);

  return (CONFIG_ERR_SUCCESS);
}

static config_err_t
alert_string_set_commit (const char *section_name,
                         const struct config_keyvalue *kv,
                         void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  struct alert_string_keys ask;
  config_err_t ret;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if ((ret = _get_alert_string_keys (state_data,
                                     section_name,
                                     &ask)) != CONFIG_ERR_SUCCESS)
    return (ret);

  ask.alert_string_set = atoi (kv->value_input);

  return (_set_alert_string_keys (state_data,
                                  section_name,
                                  &ask));
}

static config_err_t
alert_string_checkout (const char *section_name,
                       struct config_keyvalue *kv,
                       void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  char alert_string[PEF_ALERT_STRING_MAX_LEN+1];
  uint8_t string_selector;
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  unsigned int blocks;
  unsigned int i;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  string_selector = atoi (section_name + strlen ("Alert_String_"));

  memset (alert_string, '\0', PEF_ALERT_STRING_MAX_LEN+1);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_pef_configuration_parameters_alert_strings_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (!((PEF_ALERT_STRING_MAX_LEN) % PEF_ALERT_STRING_BLOCK_SIZE))
    blocks = (PEF_ALERT_STRING_MAX_LEN)/PEF_ALERT_STRING_BLOCK_SIZE;
  else
    blocks = (PEF_ALERT_STRING_MAX_LEN)/PEF_ALERT_STRING_BLOCK_SIZE + 1;

  for (i = 0; i < blocks; i++)
    {
      int j;

      if (fiid_obj_clear (obj_cmd_rs) < 0)
        {
          pstdout_perror (state_data->pstate, "fiid_obj_clear");
          goto cleanup;
        }

      if (ipmi_cmd_get_pef_configuration_parameters_alert_string (state_data->ipmi_ctx,
                                                                  IPMI_GET_PEF_PARAMETER,
                                                                  string_selector,
                                                                  i + 1,
                                                                  obj_cmd_rs) < 0)
        {
          config_err_t ret;

          if (state_data->prog_data->args->config_args.common.debug)
            pstdout_fprintf (state_data->pstate,
                             stderr,
                             "ipmi_cmd_get_pef_configuration_parameters_alert_string: %s\n",
                             ipmi_ctx_errormsg (state_data->ipmi_ctx));

          if (config_is_config_param_non_fatal_error (state_data->ipmi_ctx,
                                                      obj_cmd_rs,
                                                      &ret))
            rv = ret;

          goto cleanup;
        }

      /* XXX: Be lazy for now, assume no strings will overflow
       * whatever is passed in, so don't check for overflow errors
       * from fiid_obj_get_data.
       */
      if (fiid_obj_get_data (obj_cmd_rs,
                             "string_data",
                             alert_string + (i * PEF_ALERT_STRING_BLOCK_SIZE),
                             PEF_ALERT_STRING_MAX_LEN - (i * PEF_ALERT_STRING_BLOCK_SIZE)) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_get_data: 'string_data': %s\n",
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }

      /* Check if we've found a nul character */
      for (j = 0; j < PEF_ALERT_STRING_BLOCK_SIZE; j++)
        {
          if (!((alert_string + (i * PEF_ALERT_STRING_BLOCK_SIZE))[j]))
            goto done;
        }
    }

 done:
  if (config_section_update_keyvalue_output (state_data->pstate,
                                             kv,
                                             alert_string) < 0)
    return (CONFIG_ERR_FATAL_ERROR);

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);

}

static config_err_t
alert_string_commit (const char *section_name,
                     const struct config_keyvalue *kv,
                     void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  uint8_t string_selector;
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  uint8_t *alert_string_buf = NULL;
  unsigned int alert_string_len = 0;
  unsigned int alert_string_buf_len = 0;
  unsigned int blocks;
  unsigned int i;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  string_selector = atoi (section_name + strlen ("Alert_String_"));

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_set_pef_configuration_parameters_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  alert_string_len = strlen (kv->value_input);

  /* We need to write a nul char, so count it as part of the buflen */
  alert_string_buf_len = alert_string_len + 1;

  if (!(alert_string_buf = (uint8_t *)malloc (alert_string_buf_len)))
    {
      pstdout_perror (state_data->pstate, "malloc");
      goto cleanup;
    }
  memset (alert_string_buf, '\0', alert_string_buf_len);

  if (alert_string_len)
    memcpy (alert_string_buf, kv->value_input, alert_string_len);

  if (!((alert_string_buf_len) % PEF_ALERT_STRING_BLOCK_SIZE))
    blocks = (alert_string_buf_len)/PEF_ALERT_STRING_BLOCK_SIZE;
  else
    blocks = (alert_string_buf_len)/PEF_ALERT_STRING_BLOCK_SIZE + 1;

  for (i = 0; i < blocks; i++)
    {
      uint8_t len_to_write;

      if ((alert_string_buf_len - (i * PEF_ALERT_STRING_BLOCK_SIZE)) < PEF_ALERT_STRING_BLOCK_SIZE)
        len_to_write = alert_string_buf_len - (i * PEF_ALERT_STRING_BLOCK_SIZE);
      else
        len_to_write = PEF_ALERT_STRING_BLOCK_SIZE;

      if (ipmi_cmd_set_pef_configuration_parameters_alert_strings (state_data->ipmi_ctx,
                                                                   string_selector,
                                                                   i+1,
                                                                   alert_string_buf + (i * PEF_ALERT_STRING_BLOCK_SIZE),
                                                                   len_to_write,
                                                                   obj_cmd_rs) < 0)
        {
          config_err_t ret;
      
          if (state_data->prog_data->args->config_args.common.debug)
            pstdout_fprintf (state_data->pstate,
                             stderr,
                             "ipmi_cmd_set_pef_configuration_parameters_alert_strings: %s\n",
                             ipmi_ctx_errormsg (state_data->ipmi_ctx));

          if (config_is_config_param_non_fatal_error (state_data->ipmi_ctx,
                                                      obj_cmd_rs,
                                                      &ret))
            rv = ret;

          goto cleanup;
        }
    }

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  free (alert_string_buf);
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);

}

static config_validate_t
alert_string_validate (const char *section_name,
                       const char *key_name,
                       const char *value,
                       void *arg)
{
  if (strlen (value) <= PEF_ALERT_STRING_MAX_LEN)
    return (CONFIG_VALIDATE_VALID_VALUE);
  return (CONFIG_VALIDATE_INVALID_VALUE);
}

struct config_section *
ipmi_pef_config_alert_string_section_get (ipmi_pef_config_state_data_t *state_data, int num)
{
  struct config_section *section = NULL;
  char buf[CONFIG_MAX_SECTION_NAME_LEN];

  if (num <= 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "Invalid Num = %d\n", num);
      return (NULL);
    }

  snprintf (buf, CONFIG_MAX_SECTION_NAME_LEN, "Alert_String_%d", num);

  if (!(section = config_section_create (state_data->pstate,
                                         buf,
                                         NULL,
                                         NULL,
                                         0,
                                         NULL,
                                         NULL)))
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Event_Filter_Number",
                              "Give valid number",
                              0,
                              event_filter_number_checkout,
                              event_filter_number_commit,
                              config_number_range_seven_bits) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Alert_String_Set",
                              "Give valid number",
                              0,
                              alert_string_set_checkout,
                              alert_string_set_commit,
                              config_number_range_seven_bits) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Alert_String",
                              "Give string. Max 64 chars.",
                              0,
                              alert_string_checkout,
                              alert_string_commit,
                              alert_string_validate) < 0)
    goto cleanup;

  return (section);

 cleanup:
  if (section)
    config_section_destroy (section);
  return (NULL);
}

