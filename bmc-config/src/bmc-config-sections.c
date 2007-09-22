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
#include "bmc-config-utils.h"

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

static int
_add_section(struct section **sections, struct section *sect)
{
  if (!sections || !sect)
    return -1;
  
  if (*sections)
    {
      struct section *trav = *sections;
      while (trav->next)
	trav = trav->next;
      trav->next = sect;
    }
  else
    *sections = sect;

  return 0;
}

struct section *
bmc_config_sections_list_create (bmc_config_state_data_t *state_data)
{
  struct section *sections = NULL;
  struct section *sect = NULL;
  uint8_t number_of_users;
  int i;

  if (get_number_of_users(state_data, &number_of_users) != CONFIG_ERR_SUCCESS)
    {
      if (state_data->prog_data->args->verbose)
        fprintf (stderr, "## FATAL: Unable to get Number of Users\n");
      return NULL;
    }

  for (i = 0; i < number_of_users; i++)
    {
      if (!(sect = bmc_config_user_section_get(state_data, i+1)))
	goto cleanup;
      if (_add_section (&sections, sect) < 0)
	goto cleanup;
    }
  
  if (!(sect = bmc_config_lan_channel_section_get (state_data)))
    goto cleanup;
  if (_add_section (&sections, sect) < 0)
    goto cleanup;

  if (!(sect = bmc_config_lan_conf_section_get (state_data)))
    goto cleanup;
  if (_add_section (&sections, sect) < 0)
    goto cleanup;

  if (!(sect = bmc_config_lan_conf_auth_section_get (state_data)))
    goto cleanup;
  if (_add_section (&sections, sect) < 0)
    goto cleanup;

  if (!(sect = bmc_config_lan_conf_security_keys_section_get (state_data)))
    goto cleanup;
  if (_add_section (&sections, sect) < 0)
    goto cleanup;

  if (!(sect = bmc_config_lan_conf_misc_section_get (state_data)))
    goto cleanup;
  if (_add_section (&sections, sect) < 0)
    goto cleanup;

  if (!(sect = bmc_config_rmcpplus_conf_privilege_section_get (state_data)))
    goto cleanup;
  if (_add_section (&sections, sect) < 0)
    goto cleanup;

  if (!(sect = bmc_config_serial_channel_section_get (state_data)))
    goto cleanup;
  if (_add_section (&sections, sect) < 0)
    goto cleanup;

  if (!(sect = bmc_config_serial_conf_section_get (state_data)))
    goto cleanup;
  if (_add_section (&sections, sect) < 0)
    goto cleanup;

  if (!(sect = bmc_config_pef_conf_section_get (state_data)))
    goto cleanup;
  if (_add_section (&sections, sect) < 0)
    goto cleanup;

  if (!(sect = bmc_config_sol_conf_section_get (state_data)))
    goto cleanup;
  if (_add_section (&sections, sect) < 0)
    goto cleanup;

  if (!(sect = bmc_config_misc_section_get (state_data)))
    goto cleanup;
  if (_add_section (&sections, sect) < 0)
    goto cleanup;

  return sections;

 cleanup:
  bmc_config_sections_list_destroy(state_data, sections);
  return NULL;
}

void 
bmc_config_sections_list_destroy(bmc_config_state_data_t *state_data,
                                 struct section *sections)
{
  if (sections)
    {
      while (sections)
	{
	  struct section *sections_next = sections->next;
	  bmc_config_section_destroy(state_data, sections);
	  sections = sections_next;
	}
    }
}

struct section * 
bmc_config_section_create (bmc_config_state_data_t *state_data,
                           char *section_name,
                           char *section_comment_section_name,
                           char *section_comment,
                           unsigned int flags)
{
  struct section *section = NULL;

  if (!section_name)
    return NULL;

  if (!(section = (struct section *) calloc (1, sizeof(*section))))
    {
      perror("calloc");
      goto cleanup;
    }

  if (!(section->section_name = strdup(section_name)))
    {
      perror("strdup");
      goto cleanup;
    }

  if (section_comment_section_name)
    {
      if (!(section->section_comment_section_name = strdup(section_comment_section_name)))
        {
          perror("strdup");
          goto cleanup;
        }
    }

  if (section_comment)
    {
      if (!(section->section_comment = strdup(section_comment)))
        {
          perror("strdup");
          goto cleanup;
        }
    }

  section->flags = flags;

  return section;
 cleanup:
  if (section)
    bmc_config_section_destroy (state_data, section);
  return NULL;
}

void 
bmc_config_section_destroy (bmc_config_state_data_t *state_data,
                            struct section *section)
{
  if (section)
    {
      if (section->section_name)
	free(section->section_name);

      if (section->section_comment_section_name)
        free(section->section_comment_section_name);

      if (section->section_comment)
        free(section->section_comment);
      
      while (section->keyvalues)
	{
	  struct keyvalue *keyvalue_next = section->keyvalues->next;

	  if (section->keyvalues->value)
	    free(section->keyvalues->value);

	  free(section->keyvalues);
	  section->keyvalues = keyvalue_next;
	}

      free(section);
    }
}

int 
bmc_config_section_add_keyvalue (bmc_config_state_data_t *state_data,
                                 struct section *section,
                                 const char *key,
                                 const char *desc,
                                 unsigned int flags,
                                 Keyvalue_Checkout checkout,
                                 Keyvalue_Commit commit,
                                 Keyvalue_Diff diff,
                                 Keyvalue_Validate validate)
{
  struct keyvalue *kv;

  if (!state_data
      || !section
      || !key
      || !desc
      || !checkout
      || !commit
      || !diff
      || !validate)
    return -1;

  if (!(kv = (struct keyvalue *) calloc (1, sizeof(*kv))))
    {
      perror ("calloc");
      goto cleanup;
    }

  kv->key = key;
  kv->desc = desc;
  kv->flags = flags;
  kv->checkout = checkout;
  kv->commit = commit;
  kv->diff = diff;
  kv->validate = validate;

  if (section->keyvalues)
    {
      struct keyvalue *trav = section->keyvalues;
      while (trav->next)
	trav = trav->next;
      trav->next = kv;
    }
  else
    section->keyvalues = kv;

  return 0;
 cleanup:
  if (kv)
    free(kv);
  return -1;
}

static struct section *
bmc_config_section_find_section (bmc_config_state_data_t *state_data,
                                 const char *section_name)
{
  const struct section *sect;

  sect = state_data->sections;

  while (sect) 
    {
      if (same (section_name, sect->section_name))
        break;
      sect = sect->next;
    }

  return (struct section *)sect;
}

struct keyvalue *
bmc_config_section_find_keyvalue (bmc_config_state_data_t *state_data,
                                  const char *section_name,
                                  const char *key_name)
{

  const struct section *sect;
  struct keyvalue *kv = NULL;

  if (!(sect = bmc_config_section_find_section (state_data,
                                                section_name)))
    {
      fprintf (stderr, "Unknown section `%s'\n", section_name);
      return NULL;
    }

  kv = sect->keyvalues;

  while (kv) 
    {
      if (same (key_name, kv->key))
        break;
      kv = kv->next;
    }

  if (!kv) 
    {
      fprintf (stderr, "Unknown key `%s' in section `%s'\n",
               key_name, section_name);
      return NULL;
    }

  return kv;
}

int
bmc_config_section_set_value (bmc_config_state_data_t *state_data,
                              const char *section_name,
                              const char *key_name,
                              const char *value)
{
  struct section *sect;
  struct keyvalue *kv;

  if (!(sect = bmc_config_section_find_section (state_data, section_name)))
    {
      fprintf (stderr, "Unknown section `%s'\n", section_name);
      return -1;
    }

  if (!(kv = bmc_config_section_find_keyvalue (state_data, section_name, key_name)))
    {
      fprintf (stderr, "Unknown key `%s' in section `%s'\n", key_name, section_name);
      return -1;
    }

  if (kv->validate)
    {
      config_validate_t v;

      if ((v = kv->validate (section_name, key_name, value)) == CONFIG_VALIDATE_FATAL_ERROR)
        return -1;
      
      if (v == CONFIG_VALIDATE_INVALID_VALUE)
        {
          fprintf (stderr, "Invalid value `%s' for key `%s'\n",
                   value, key_name);
          return -1;
        }
    }

  if (kv->value)
    free (kv->value);
  
  if (!(kv->value = strdup (value)))
    {
      perror("strdup");
      return -1;
    }

  return 0;
}

config_err_t
bmc_config_section_commit_value (bmc_config_state_data_t *state_data,
                                 const char *section_name,
                                 const char *key_name,
                                 const char *value)
{
  struct section *sect;
  struct keyvalue *kv;

  if (!(sect = bmc_config_section_find_section (state_data, section_name)))
    return CONFIG_ERR_FATAL_ERROR;

  if (!(kv = bmc_config_section_find_keyvalue (state_data, section_name, key_name)))
    return CONFIG_ERR_FATAL_ERROR;

  if (kv->validate)
    {
      config_validate_t v;

      if ((v = kv->validate (section_name, key_name, value)) == CONFIG_VALIDATE_FATAL_ERROR)
        return CONFIG_ERR_FATAL_ERROR;
      
      if (v == CONFIG_VALIDATE_INVALID_VALUE)
        {
          fprintf (stderr, "Invalid value `%s' for key `%s'\n",
                   value, key_name);
          return CONFIG_ERR_NON_FATAL_ERROR;
        }
    }

  if (kv->value)
    free (kv->value);

  if (!(kv->value = strdup (value)))
    {
      perror("strdup");
      return CONFIG_ERR_FATAL_ERROR;
    }

  return kv->commit (state_data, sect, kv);
}

int
bmc_config_section_diff_value (bmc_config_state_data_t *state_data,
                               const char *section_name,
                               const char *key_name,
                               const char *value)
{
  struct section *sect;
  struct keyvalue *kv;

  if (!(sect = bmc_config_section_find_section (state_data, section_name)))
    return CONFIG_ERR_FATAL_ERROR;

  if (!(kv = bmc_config_section_find_keyvalue (state_data, section_name, key_name)))
    return CONFIG_ERR_FATAL_ERROR;

  if (kv->validate)
    {
      config_validate_t v;

      if ((v = kv->validate (section_name, key_name, value)) == CONFIG_VALIDATE_FATAL_ERROR)
        return CONFIG_ERR_FATAL_ERROR;
      
      if (v == CONFIG_VALIDATE_INVALID_VALUE)
        {
          fprintf (stderr, "Invalid value `%s' for key `%s'\n",
                   value, key_name);
          return CONFIG_ERR_NON_FATAL_ERROR;
        }
    }

  if (kv->value)
    free (kv->value);

  if (!(kv->value = strdup (value)))
    {
      perror("strdup");
      return CONFIG_ERR_FATAL_ERROR;
    }

  return kv->diff (state_data, sect, kv);
}

config_err_t 
bmc_config_sections_list (bmc_config_state_data_t *state_data)
{
  struct section *sect;

  sect = state_data->sections;

  while (sect)
    {
      if (!(sect->flags & BMC_DO_NOT_CHECKOUT))
	printf("%s\n", sect->section_name); 
      sect = sect->next;
    }

  return CONFIG_ERR_SUCCESS;
}
