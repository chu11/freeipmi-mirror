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

struct config_section *
bmc_config_sections_create (bmc_config_state_data_t *state_data)
{
  struct config_section *sections = NULL;
  struct config_section *section = NULL;
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
      if (!(section = bmc_config_user_section_get(state_data, i+1)))
	goto cleanup;
      if (config_section_append (&sections, section) < 0)
	goto cleanup;
    }
  
  if (!(section = bmc_config_lan_channel_section_get (state_data)))
    goto cleanup;
  if (config_section_append (&sections, section) < 0)
    goto cleanup;

  if (!(section = bmc_config_lan_conf_section_get (state_data)))
    goto cleanup;
  if (config_section_append (&sections, section) < 0)
    goto cleanup;

  if (!(section = bmc_config_lan_conf_auth_section_get (state_data)))
    goto cleanup;
  if (config_section_append (&sections, section) < 0)
    goto cleanup;

  if (!(section = bmc_config_lan_conf_security_keys_section_get (state_data)))
    goto cleanup;
  if (config_section_append (&sections, section) < 0)
    goto cleanup;

  if (!(section = bmc_config_lan_conf_misc_section_get (state_data)))
    goto cleanup;
  if (config_section_append (&sections, section) < 0)
    goto cleanup;

  if (!(section = bmc_config_rmcpplus_conf_privilege_section_get (state_data)))
    goto cleanup;
  if (config_section_append (&sections, section) < 0)
    goto cleanup;

  if (!(section = bmc_config_serial_channel_section_get (state_data)))
    goto cleanup;
  if (config_section_append (&sections, section) < 0)
    goto cleanup;

  if (!(section = bmc_config_serial_conf_section_get (state_data)))
    goto cleanup;
  if (config_section_append (&sections, section) < 0)
    goto cleanup;

  if (!(section = bmc_config_pef_conf_section_get (state_data)))
    goto cleanup;
  if (config_section_append (&sections, section) < 0)
    goto cleanup;

  if (!(section = bmc_config_sol_conf_section_get (state_data)))
    goto cleanup;
  if (config_section_append (&sections, section) < 0)
    goto cleanup;

  if (!(section = bmc_config_misc_section_get (state_data)))
    goto cleanup;
  if (config_section_append (&sections, section) < 0)
    goto cleanup;

  return sections;

 cleanup:
  config_sections_destroy(sections);
  return NULL;
}

int
config_section_append(struct config_section **sections, 
                      struct config_section *section)
{
  assert(sections);
  assert(section);
  
  if (*sections)
    {
      struct config_section *s = *sections;
      while (s->next)
        s = s->next;
      s->next = section;
    }
  else
    *sections = section;

  return 0;
}

void 
config_sections_destroy(struct config_section *sections)
{
  if (sections)
    {
      while (sections)
	{
	  struct config_section *sections_next = sections->next;
	  config_section_destroy(sections);
	  sections = sections_next;
	}
    }
}

struct config_section * 
config_section_create (char *section_name,
                       char *section_comment_section_name,
                       char *section_comment,
                       unsigned int flags)
{
  struct config_section *section = NULL;

  assert(section_name);

  if (!(section = (struct config_section *)malloc(sizeof(struct config_section))))
    {
      perror("malloc");
      goto cleanup;
    }
  memset(section, '\0', sizeof(struct config_section));

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
    config_section_destroy(section);
  return NULL;
}

static void
_config_keyvalue_destroy(struct config_keyvalue *keyvalue)
{
  if (keyvalue)
    {
      if (keyvalue->key_name)
        free(keyvalue->key_name);
      if (keyvalue->description)
        free(keyvalue->description);
      if (keyvalue->value)
        free(keyvalue->value);
      free(keyvalue);
    }
}

void 
config_section_destroy (struct config_section *section)
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
	  struct config_keyvalue *keyvalue_next = section->keyvalues->next;
          _config_keyvalue_destroy(section->keyvalues);
	  section->keyvalues = keyvalue_next;
	}

      free(section);
    }
}

int 
config_section_add_keyvalue (struct config_section *section,
                             const char *key_name,
                             const char *description,
                             unsigned int flags,
                             Key_Checkout checkout,
                             Key_Commit commit,
                             Key_Diff diff,
                             Key_Validate validate)
{
  struct config_keyvalue *kv = NULL;

  assert(section);
  assert(key_name);
  assert(description);
  assert(checkout);
  assert(commit);
  assert(validate);

  if (!(kv = (struct config_keyvalue *)malloc(sizeof(struct config_keyvalue))))
    {
      perror("malloc");
      goto cleanup;
    }
  memset(kv, '\0', sizeof(struct config_keyvalue));

  if (!(kv->key_name = strdup(key_name)))
    {
      perror("strdup");
      goto cleanup;
    }
  if (!(kv->description = strdup(description)))
    {
      perror("strdup");
      goto cleanup;
    }
  kv->flags = flags;
  kv->checkout = checkout;
  kv->commit = commit;
  kv->diff = diff;
  kv->validate = validate;

  if (section->keyvalues)
    {
      struct config_keyvalue *trav = section->keyvalues;
      while (trav->next)
	trav = trav->next;
      trav->next = kv;
    }
  else
    section->keyvalues = kv;

  return 0;
 cleanup:
  _config_keyvalue_destroy(kv);
  return -1;
}

static struct config_section *
config_section_find_section (struct config_section *sections,
                             const char *section_name)
{
  const struct config_section *section;

  section = sections;

  while (section) 
    {
      if (same (section_name, section->section_name))
        break;
      section = section->next;
    }

  return (struct config_section *)section;
}

struct config_keyvalue *
config_section_find_keyvalue (struct config_section *sections,
                              const char *section_name,
                              const char *key_name)
{
  const struct config_section *section;
  struct config_keyvalue *kv = NULL;

  if (!(section = config_section_find_section (sections,
                                               section_name)))
    {
      fprintf (stderr, "Unknown section `%s'\n", section_name);
      return NULL;
    }

  kv = section->keyvalues;

  while (kv) 
    {
      if (same (key_name, kv->key_name))
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
config_section_set_value (struct config_section *sections,
                          const char *section_name,
                          const char *key_name,
                          const char *value)
{
  struct config_section *section;
  struct config_keyvalue *kv;

  if (!(section = config_section_find_section (sections, section_name)))
    {
      fprintf (stderr, "Unknown section `%s'\n", section_name);
      return -1;
    }

  if (!(kv = config_section_find_keyvalue (sections, section_name, key_name)))
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
config_section_commit_value (struct config_section *sections,
                             const char *section_name,
                             const char *key_name,
                             const char *value,
                             void *arg)
{
  struct config_section *section;
  struct config_keyvalue *kv;

  if (!(section = config_section_find_section (sections, section_name)))
    return CONFIG_ERR_FATAL_ERROR;

  if (!(kv = config_section_find_keyvalue (sections, section_name, key_name)))
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

  return kv->commit (section, kv, arg);
}

int
config_section_diff_value (struct config_section *sections,
                           const char *section_name,
                           const char *key_name,
                           const char *value,
                           void *arg)
{
  struct config_section *section;
  struct config_keyvalue *kv;

  if (!(section = config_section_find_section (sections, section_name)))
    return CONFIG_ERR_FATAL_ERROR;

  if (!(kv = config_section_find_keyvalue (sections, section_name, key_name)))
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

  return kv->diff (section, kv, arg);
}

config_err_t 
config_output_sections_list (struct config_section *sections)
{
  struct config_section *s;

  s = sections;
  while (s)
    {
      if (!(s->flags & CONFIG_DO_NOT_CHECKOUT))
	printf("%s\n", s->section_name); 
      s = s->next;
    }

  return CONFIG_ERR_SUCCESS;
}
