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

#include "bmc-common.h"
#include "bmc-sections.h"

#include "bmc-user-sections.h"

#include "bmc-lan-channel-section.h"
#include "bmc-lan-conf-section.h"
#include "bmc-lan-conf-auth-section.h"
#include "bmc-lan-conf-security-keys-section.h"
#include "bmc-lan-conf-misc-section.h"
#include "bmc-rmcpplus-conf-privilege-section.h"
#include "bmc-serial-channel-section.h"
#include "bmc-serial-conf-section.h"
#include "bmc-pef-conf-section.h"
#include "bmc-sol-conf-section.h"
#include "bmc-misc-section.h"

struct section *
bmc_sections_init (struct bmc_config_arguments *args)
{
  struct section *sections = NULL;

  add_section (sections, bmc_user_sections_get (args));
  add_section (sections, bmc_lan_channel_section_get (args));
  add_section (sections, bmc_lan_conf_section_get (args));
  add_section (sections, bmc_lan_conf_auth_section_get (args));
  add_section (sections, bmc_lan_conf_security_keys_section_get (args));
  add_section (sections, bmc_lan_conf_misc_section_get (args));
  add_section (sections, bmc_rmcpplus_conf_privilege_section_get (args));
  add_section (sections, bmc_serial_channel_section_get (args));
  add_section (sections, bmc_serial_conf_section_get (args));
  add_section (sections, bmc_pef_conf_section_get (args));
  add_section (sections, bmc_sol_conf_section_get (args));
  add_section (sections, bmc_misc_section_get (args));

  return sections;
}

static struct section *
bmc_section_find_section (const char *section_name,
			  const struct section *sections)
{
  const struct section *sect = sections;

  while (sect) 
    {
      if (same (section_name, sect->section))
        break;
      sect = sect->next;
    }
  return (struct section *)sect;
}

struct keyvalue *
bmc_section_find_keyvalue (const char *section_name,
			   const char *key_name,
			   const struct section *sections)
{

  const struct section *sect = bmc_section_find_section (section_name,
							 sections);
  struct keyvalue *kv = NULL;

  if (!sect) 
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
bmc_section_set_value (const char *section_name,
		       const char *key_name,
		       const char *value,
		       struct bmc_config_arguments *args,
		       struct section *sections)
{

  struct section *sect = bmc_section_find_section (section_name,
						    sections);
  struct keyvalue *kv = bmc_section_find_keyvalue (section_name,
						   key_name,
						   sections);

  if (!kv)
    return -1;

  if (kv->validate && (kv->validate (args, sect, value) != 0)) 
    {
      fprintf (stderr, "Invalid value `%s' for key `%s'\n",
               value, key_name);
      return -1;
    }

  if (kv->value)
    free (kv->value);
  
  if (!(kv->value = strdup (value)))
    {
      perror("strdup");
      exit(1);
    }

  return 0;
}

int
bmc_section_commit_value (const char *section_name,
			  const char *key_name,
			  const char *value,
			  struct bmc_config_arguments *args,
			  struct section *sections)
{
  struct section *sect = bmc_section_find_section (section_name,
						    sections);
  struct keyvalue *kv = bmc_section_find_keyvalue (section_name,
						   key_name,
						   sections);

  if (!kv)
    return -1;

  if (kv->validate && (kv->validate (args, sect, value) != 0)) 
    {
      fprintf (stderr, "Invalid value `%s' for key `%s'\n",
               value, key_name);
      return -1;
    }

  if (kv->value)
    free (kv->value);

  if (!(kv->value = strdup (value)))
    {
      perror("strdup");
      exit(1);
    }

  return kv->commit (args, sect, kv);
}

int
bmc_section_diff_value (const char *section_name,
			const char *key_name,
			const char *value,
			struct bmc_config_arguments *args,
			struct section *sections)
{
  struct section *sect = bmc_section_find_section (section_name,
						    sections);
  struct keyvalue *kv = bmc_section_find_keyvalue (section_name,
						   key_name,
						   sections);

  if (!kv)
    return -1;

  if (kv->validate && (kv->validate (args, sect, value) != 0)) 
    {
      fprintf (stderr, "Invalid value `%s' for key `%s'\n",
               value, key_name);
      return -1;
    }

  if (kv->value)
    free (kv->value);

  if (!(kv->value = strdup (value)))
    {
      perror("strdup");
      exit(1);
    }

  return kv->diff (args, sect, kv);
}

int 
bmc_sections_list (struct bmc_config_arguments *args, 
                   struct section *sections)
{
  struct section *sect = sections;

  while (sect)
    {
      printf("%s\n", sect->section); 
      sect = sect->next;
    }

  return 0;
}
