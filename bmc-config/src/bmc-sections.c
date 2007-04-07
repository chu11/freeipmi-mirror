/* 

   bmc-sections - sections of bmc parameters

   Copyright (C) 2006 FreeIPMI Core Team

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.  
*/

#include "bmc-config.h"
#include "bmc-common.h"
#include "bmc-sections.h"
#include "bmc-user-sections.h"
#include "bmc-lan-channel-section.h"
#include "bmc-lan-conf-section.h"
#include "bmc-lan-conf-alert-section.h"
#include "bmc-lan-conf-auth-section.h"
#include "bmc-lan-conf-security-keys-section.h"
#include "bmc-lan-conf-misc-section.h"
#include "bmc-rmcpplus-conf-privilege-section.h"
#include "bmc-serial-channel-section.h"
#include "bmc-serial-conf-section.h"
#include "bmc-pef-conf-section.h"
#include "bmc-sol-conf-section.h"
#include "bmc-misc-section.h"

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
bmc_config_sections_create (bmc_config_state_data_t *state_data)
{
  struct section *sections = NULL;
  struct section *sect = NULL;
  int num_users, i;

  if ((num_users = bmc_get_num_users (state_data)) < 0)
    return NULL;

  for (i = 0; i < num_users; i++)
    {
      if (!(sect = bmc_user_section_get(state_data, i)))
	goto cleanup;
      if (_add_section (&sections, sect) < 0)
	goto cleanup;
    }
  
  if (!(sect = bmc_lan_channel_section_get (state_data)))
    goto cleanup;
  if (_add_section (&sections, sect) < 0)
    goto cleanup;

  if (!(sect = bmc_lan_conf_section_get (state_data)))
    goto cleanup;
  if (_add_section (&sections, sect) < 0)
    goto cleanup;

  if (!(sect = bmc_lan_conf_alert_section_get (state_data)))
    goto cleanup;
  if (_add_section (&sections, sect) < 0)
    goto cleanup;

  if (!(sect = bmc_lan_conf_auth_section_get (state_data)))
    goto cleanup;
  if (_add_section (&sections, sect) < 0)
    goto cleanup;

  if (!(sect = bmc_lan_conf_security_keys_section_get (state_data)))
    goto cleanup;
  if (_add_section (&sections, sect) < 0)
    goto cleanup;

  if (!(sect = bmc_lan_conf_misc_section_get (state_data)))
    goto cleanup;
  if (_add_section (&sections, sect) < 0)
    goto cleanup;

  if (!(sect = bmc_rmcpplus_conf_privilege_section_get (state_data)))
    goto cleanup;
  if (_add_section (&sections, sect) < 0)
    goto cleanup;

  if (!(sect = bmc_serial_channel_section_get (state_data)))
    goto cleanup;
  if (_add_section (&sections, sect) < 0)
    goto cleanup;

  if (!(sect = bmc_serial_conf_section_get (state_data)))
    goto cleanup;
  if (_add_section (&sections, sect) < 0)
    goto cleanup;

  if (!(sect = bmc_pef_conf_section_get (state_data)))
    goto cleanup;
  if (_add_section (&sections, sect) < 0)
    goto cleanup;

  if (!(sect = bmc_sol_conf_section_get (state_data)))
    goto cleanup;
  if (_add_section (&sections, sect) < 0)
    goto cleanup;

  if (!(sect = bmc_misc_section_get (state_data)))
    goto cleanup;
  if (_add_section (&sections, sect) < 0)
    goto cleanup;

  return sections;

 cleanup:
  bmc_config_sections_destroy(state_data, sections);
  return NULL;
}

void 
bmc_config_sections_destroy(bmc_config_state_data_t *state_data,
                            struct section *sections)
{
  if (sections)
    {
      while (sections)
	{
	  struct section *sections_next = sections->next;
	  bmc_section_destroy(state_data, sections);
	  sections = sections_next;
	}
    }
}

struct section * 
bmc_section_create (bmc_config_state_data_t *state_data,
                    char *section_name)
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

  return section;
 cleanup:
  if (section)
    bmc_section_destroy (state_data, section);
  return NULL;
}

void 
bmc_section_destroy (bmc_config_state_data_t *state_data,
                     struct section *section)
{
  if (section)
    {
      if (section->section_name)
	free(section->section_name);
      
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
bmc_section_add_keyvalue (bmc_config_state_data_t *state_data,
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
bmc_section_find_section (bmc_config_state_data_t *state_data,
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
bmc_section_find_keyvalue (bmc_config_state_data_t *state_data,
                           const char *section_name,
			   const char *key_name)
{

  const struct section *sect;
  struct keyvalue *kv = NULL;

  if (!(sect = bmc_section_find_section (state_data,
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
bmc_section_set_value (bmc_config_state_data_t *state_data,
                       const char *section_name,
		       const char *key_name,
		       const char *value)
{
  struct section *sect;
  struct keyvalue *kv;

  if (!(sect = bmc_section_find_section (state_data, section_name)))
    return -1;

  if (!(kv = bmc_section_find_keyvalue (state_data, section_name, key_name)))
    return -1;

  if (kv->validate)
    {
      bmc_validate_t v;

      if ((v = kv->validate (state_data, sect, value)) == BMC_VALIDATE_FATAL_ERROR)
        return -1;
      
      if (v == BMC_VALIDATE_INVALID_VALUE)
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

bmc_err_t
bmc_section_commit_value (bmc_config_state_data_t *state_data,
                          const char *section_name,
			  const char *key_name,
			  const char *value)
{
  struct section *sect;
  struct keyvalue *kv;

  if (!(sect = bmc_section_find_section (state_data, section_name)))
    return BMC_ERR_FATAL_ERROR;

  if (!(kv = bmc_section_find_keyvalue (state_data, section_name, key_name)))
    return BMC_ERR_FATAL_ERROR;

  if (kv->validate)
    {
      bmc_validate_t v;

      if ((v = kv->validate (state_data, sect, value)) == BMC_VALIDATE_FATAL_ERROR)
        return BMC_ERR_FATAL_ERROR;
      
      if (v == BMC_VALIDATE_INVALID_VALUE)
        {
          fprintf (stderr, "Invalid value `%s' for key `%s'\n",
                   value, key_name);
          return BMC_ERR_NON_FATAL_ERROR;
        }
    }

  if (kv->value)
    free (kv->value);

  if (!(kv->value = strdup (value)))
    {
      perror("strdup");
      return BMC_ERR_FATAL_ERROR;
    }

  return kv->commit (state_data, sect, kv);
}

int
bmc_section_diff_value (bmc_config_state_data_t *state_data,
                        const char *section_name,
			const char *key_name,
			const char *value)
{
  struct section *sect;
  struct keyvalue *kv;

  if (!(sect = bmc_section_find_section (state_data, section_name)))
    return BMC_ERR_FATAL_ERROR;

  if (!(kv = bmc_section_find_keyvalue (state_data, section_name, key_name)))
    return BMC_ERR_FATAL_ERROR;

  if (kv->validate)
    {
      bmc_validate_t v;

      if ((v = kv->validate (state_data, sect, value)) == BMC_VALIDATE_FATAL_ERROR)
        return BMC_ERR_FATAL_ERROR;
      
      if (v == BMC_VALIDATE_INVALID_VALUE)
        {
          fprintf (stderr, "Invalid value `%s' for key `%s'\n",
                   value, key_name);
          return BMC_ERR_NON_FATAL_ERROR;
        }
    }

  if (kv->value)
    free (kv->value);

  if (!(kv->value = strdup (value)))
    {
      perror("strdup");
      return BMC_ERR_FATAL_ERROR;
    }

  return kv->diff (state_data, sect, kv);
}

int 
bmc_sections_list (bmc_config_state_data_t *state_data)
{
  struct section *sect;

  sect = state_data->sections;

  while (sect)
    {
      printf("%s\n", sect->section_name); 
      sect = sect->next;
    }

  return 0;
}
