#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */

#include "pef-config.h"
#include "pef-config-map.h"
#include "pef-config-wrapper.h"
#include "pef-config-utils.h"

static config_err_t
community_string_checkout (const char *section_name,
                           struct config_keyvalue *kv,
                           void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  char community_string[IPMI_MAX_COMMUNITY_STRING_LENGTH+1] = { 0, };
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;

  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_get_lan_configuration_parameters_community_string_rs)))
    goto cleanup;
  
  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }
  
  if (ipmi_cmd_get_lan_configuration_parameters_community_string (state_data->ipmi_ctx,
                                                                  channel_number,
                                                                  IPMI_GET_LAN_PARAMETER,
                                                                  SET_SELECTOR,
                                                                  BLOCK_SELECTOR,
                                                                  obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_get_lan_configuration_parameters_community_string: %s\n",
                ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  memset(community_string,'\0', IPMI_MAX_COMMUNITY_STRING_LENGTH+1);
  if (Fiid_obj_get_data (obj_cmd_rs,
                         "community_string",
                         (uint8_t *)community_string,
                         IPMI_MAX_COMMUNITY_STRING_LENGTH+1) < 0)
    goto cleanup;

  if (config_section_update_keyvalue_output(kv, community_string) < 0)
    return CONFIG_ERR_FATAL_ERROR;

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  Fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

static config_err_t
community_string_commit (const char *section_name,
                         const struct config_keyvalue *kv,
                         void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_set_lan_configuration_parameters_rs)))
    goto cleanup;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }
  
  if (ipmi_cmd_set_lan_configuration_parameters_community_string (state_data->ipmi_ctx,
                                                                  channel_number,
                                                                  kv->value_input,
                                                                  strlen(kv->value_input),
                                                                  obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_set_lan_configuration_parameters_community_string: %s\n",
                ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  Fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

static config_validate_t
community_string_validate (const char *section_name,
                           const char *key_name,
                           const char *value)
{
  if (!value || strlen (value) > IPMI_MAX_COMMUNITY_STRING_LENGTH)
    return CONFIG_VALIDATE_INVALID_VALUE;
  return CONFIG_VALIDATE_VALID_VALUE;
}

struct config_section *
pef_config_community_string_section_get (pef_config_state_data_t *state_data)
{
  struct config_section *section = NULL;

  if (!(section = config_section_create ("Community_String",
                                         NULL, 
                                         NULL, 
                                         0)))
    goto cleanup;

  if (config_section_add_key (section,
                              "Community_String",
                              "Give valid string",
                              0,
                              community_string_checkout,
                              community_string_commit,
                              community_string_validate) < 0) 
    goto cleanup;

  return section;

 cleanup:
  if (section)
    config_section_destroy(section);
  return NULL;
}

