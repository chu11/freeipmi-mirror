#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <errno.h>
#include <assert.h>

#include "bmc-config.h"
#include "bmc-config-common.h"
#include "bmc-config-sections.h"
#include "bmc-config-user-sections.h"
#include "bmc-config-lan-channel-section.h"
#include "bmc-config-lan-conf-section.h"
#include "bmc-config-lan-conf-auth-section.h"
#include "bmc-config-lan-conf-security-keys-section.h"
#include "bmc-config-lan-conf-misc-section.h"
#include "bmc-config-pef-conf-section.h"
#include "bmc-config-rmcpplus-conf-privilege-section.h"
#include "bmc-config-serial-channel-section.h"
#include "bmc-config-serial-conf-section.h"
#include "bmc-config-sol-conf-section.h"
#include "bmc-config-misc-section.h"
#include "bmc-config-wrapper.h"

#include "config-section.h"

static int
_get_num_users (bmc_config_state_data_t *state_data)
{
  uint8_t users = 0;
  config_err_t ret;

  if ((ret = get_bmc_max_users (state_data, &users)) != CONFIG_ERR_SUCCESS)
    return -1;
  return (int)users;
}

struct config_section *
bmc_config_config_sections_create(bmc_config_state_data_t *state_data)
{
  struct config_section *sections = NULL;
  struct config_section *s = NULL;
  int num_users, i;

  if ((num_users = _get_num_users (state_data)) < 0)
    goto cleanup;
  
  for (i = 0; i < num_users; i++)
    {
      if (!(s = bmc_config_user_section_get(state_data, i+1)))
	goto cleanup;
      if (config_section_append (&sections, s) < 0)
	goto cleanup;
    }
  
  if (!(s = bmc_config_lan_channel_section_get (state_data)))
    goto cleanup;
  if (config_section_append(&sections, s) < 0)
    {
      fprintf(stderr,
              "config_section_append error\n");
      goto cleanup;
    }

  if (!(s = bmc_config_lan_conf_section_get (state_data)))
    goto cleanup;
  if (config_section_append(&sections, s) < 0)
    {
      fprintf(stderr,
              "config_section_append error\n");
      goto cleanup;
    }

  if (!(s = bmc_config_lan_conf_auth_section_get (state_data)))
    goto cleanup;
  if (config_section_append(&sections, s) < 0)
    {
      fprintf(stderr,
              "config_section_append error\n");
      goto cleanup;
    }

  if (!(s = bmc_config_lan_conf_security_keys_section_get (state_data)))
    goto cleanup;
  if (config_section_append(&sections, s) < 0)
    {
      fprintf(stderr,
              "config_section_append error\n");
      goto cleanup;
    }

  if (!(s = bmc_config_lan_conf_misc_section_get (state_data)))
    goto cleanup;
  if (config_section_append(&sections, s) < 0)
    {
      fprintf(stderr,
              "config_section_append error\n");
      goto cleanup;
    }

  if (!(s = bmc_config_rmcpplus_conf_privilege_section_get (state_data)))
    goto cleanup;
  if (config_section_append(&sections, s) < 0)
    {
      fprintf(stderr,
              "config_section_append error\n");
      goto cleanup;
    }

  if (!(s = bmc_config_serial_channel_section_get (state_data)))
    goto cleanup;
  if (config_section_append(&sections, s) < 0)
    {
      fprintf(stderr,
              "config_section_append error\n");
      goto cleanup;
    }

  if (!(s = bmc_config_serial_conf_section_get (state_data)))
    goto cleanup;
  if (config_section_append(&sections, s) < 0)
    {
      fprintf(stderr,
              "config_section_append error\n");
      goto cleanup;
    }

  if (!(s = bmc_config_pef_conf_section_get (state_data)))
    goto cleanup;
  if (config_section_append(&sections, s) < 0)
    {
      fprintf(stderr,
              "config_section_append error\n");
      goto cleanup;
    }

  if (!(s = bmc_config_sol_conf_section_get (state_data)))
    goto cleanup;
  if (config_section_append(&sections, s) < 0)
    {
      fprintf(stderr,
              "config_section_append error\n");
      goto cleanup;
    }

  if (!(s = bmc_config_misc_section_get (state_data)))
    goto cleanup;
  if (config_section_append(&sections, s) < 0)
    {
      fprintf(stderr,
              "config_section_append error\n");
      goto cleanup;
    }

  return sections;

 cleanup:
  config_sections_destroy(sections);
  return NULL;
}
