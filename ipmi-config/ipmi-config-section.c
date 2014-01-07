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
#include <errno.h>
#include <assert.h>

#include "ipmi-config-section.h"
#include "ipmi-config-utils.h"

#include "freeipmi-portability.h"

int
ipmi_config_section_append (struct ipmi_config_section **sections,
                            struct ipmi_config_section *section)
{
  assert (sections);
  assert (section);

  if (*sections)
    {
      struct ipmi_config_section *s = *sections;
      while (s->next)
        s = s->next;
      s->next = section;
    }
  else
    *sections = section;

  return (0);
}

void
ipmi_config_sections_destroy (struct ipmi_config_section *sections)
{
  while (sections)
    {
      struct ipmi_config_section *sections_next = sections->next;
      ipmi_config_section_destroy (sections);
      sections = sections_next;
    }
}

struct ipmi_config_section *
ipmi_config_section_create (ipmi_config_state_data_t *state_data,
                            const char *section_name,
                            const char *section_comment_section_name,
                            const char *section_comment,
                            unsigned int flags,
                            Section_Pre_Commit section_pre_commit,
                            Section_Post_Commit section_post_commit)
{
  struct ipmi_config_section *section = NULL;

  assert (state_data);
  assert (section_name);

  if (!(section = (struct ipmi_config_section *)malloc (sizeof (struct ipmi_config_section))))
    {
      pstdout_perror (state_data->pstate, "malloc");
      goto cleanup;
    }
  memset (section, '\0', sizeof (struct ipmi_config_section));

  if (!(section->section_name = strdup (section_name)))
    {
      pstdout_perror (state_data->pstate, "strdup");
      goto cleanup;
    }

  if (section_comment_section_name)
    {
      if (!(section->section_comment_section_name = strdup (section_comment_section_name)))
        {
          pstdout_perror (state_data->pstate, "strdup");
          goto cleanup;
        }
    }

  if (section_comment)
    {
      if (!(section->section_comment = strdup (section_comment)))
        {
          pstdout_perror (state_data->pstate, "strdup");
          goto cleanup;
        }
    }

  section->flags = flags;
  section->section_pre_commit = section_pre_commit;
  section->section_post_commit = section_post_commit;
  section->category = 0;
  section->line_length = 0;

  return (section);

 cleanup:
  ipmi_config_section_destroy (section);
  return (NULL);
}

struct ipmi_config_section *
ipmi_config_section_multi_channel_create (ipmi_config_state_data_t *state_data,
                                          const char *section_name_base_str,
                                          const char *section_comment,
                                          Section_Pre_Commit section_pre_commit,
                                          Section_Post_Commit section_post_commit,
                                          unsigned int config_flags,
                                          int channel_index,
                                          uint8_t *channel_numbers,
                                          unsigned int channel_numbers_count)
{
  struct ipmi_config_section *section = NULL;

  assert (state_data);
  assert (section_name_base_str);
  assert (channel_numbers);
  assert (channel_index < (int)channel_numbers_count);

  if (channel_index < 0)
    {
      if (!(section = ipmi_config_section_create (state_data,
                                                  section_name_base_str,
                                                  section_name_base_str,
                                                  section_comment,
                                                  config_flags,
                                                  section_pre_commit,
                                                  section_post_commit)))
        goto cleanup;
    }
  else
    {
      char section_name[IPMI_CONFIG_MAX_SECTION_NAME_LEN];
      char section_comment_section_name[IPMI_CONFIG_MAX_SECTION_NAME_LEN];
      char *section_comment_section_name_ptr = NULL;

      snprintf (section_name,
                IPMI_CONFIG_MAX_SECTION_NAME_LEN,
                "%s_Channel_%u",
                section_name_base_str,
                channel_numbers[channel_index]);

      if (section_comment)
        {
          snprintf (section_comment_section_name,
                    IPMI_CONFIG_MAX_SECTION_NAME_LEN,
                    "%s_Channel_X",
                    section_name_base_str);
          section_comment_section_name_ptr = section_comment_section_name;
        }

      if (!channel_index)
        {
          if (!(section = ipmi_config_section_create (state_data,
                                                      section_name,
                                                      section_comment_section_name_ptr,
                                                      section_comment,
                                                      config_flags,
                                                      section_pre_commit,
                                                      section_post_commit)))
            goto cleanup;
        }
      else
        {
          if (!(section = ipmi_config_section_create (state_data,
                                                      section_name,
                                                      NULL,
                                                      NULL,
                                                      config_flags,
                                                      section_pre_commit,
                                                      section_post_commit)))
            goto cleanup;
        }
    }

  return (section);

 cleanup:
  ipmi_config_section_destroy (section);
  return (NULL);
}

static void
_config_key_destroy (struct ipmi_config_key *key)
{
  if (key)
    {
      free (key->key_name);
      free (key->description);
      free (key);
    }
}

static void
_config_keyvalue_destroy (struct ipmi_config_keyvalue *keyvalue)
{
  if (keyvalue)
    {
      free (keyvalue->value_input);
      free (keyvalue->value_output);
      free (keyvalue);
    }
}

void
ipmi_config_section_destroy (struct ipmi_config_section *section)
{
  if (section)
    {
      free (section->section_name);
      free (section->section_comment_section_name);
      free (section->section_comment);

      while (section->keys)
        {
          struct ipmi_config_key *key_next = section->keys->next;
          _config_key_destroy (section->keys);
          section->keys = key_next;
        }

      while (section->keyvalues)
        {
          struct ipmi_config_keyvalue *keyvalue_next = section->keyvalues->next;
          _config_keyvalue_destroy (section->keyvalues);
          section->keyvalues = keyvalue_next;
        }

      free (section);
    }
}

int
ipmi_config_set_category (struct ipmi_config_section *sections, unsigned int category)
{
  struct ipmi_config_section *s;

  assert (sections);
  assert (IPMI_CONFIG_CATEGORY_VALID (category));

  s = sections;
  while (s)
    {
      s->category = category;
      s = s->next;
    }

  return (0);
}

int
ipmi_config_set_line_length (struct ipmi_config_section *sections, unsigned int line_length)
{
  struct ipmi_config_section *s;

  assert (sections);

  s = sections;
  while (s)
    {
      s->line_length = line_length;
      s = s->next;
    }

  return (0);
}

int
ipmi_config_section_add_key (ipmi_config_state_data_t *state_data,
                             struct ipmi_config_section *section,
                             const char *key_name,
                             const char *description,
                             unsigned int flags,
                             Key_Checkout checkout,
                             Key_Commit commit,
                             Key_Validate validate)
{
  struct ipmi_config_key *k = NULL;

  assert (state_data);
  assert (section);
  assert (key_name);
  assert (description);
  assert (checkout);
  assert (commit);
  assert (validate);

  if (!(k = (struct ipmi_config_key *)malloc (sizeof (struct ipmi_config_key))))
    {
      pstdout_perror (state_data->pstate, "malloc");
      goto cleanup;
    }
  memset (k, '\0', sizeof (struct ipmi_config_key));

  if (!(k->key_name = strdup (key_name)))
    {
      pstdout_perror (state_data->pstate, "strdup");
      goto cleanup;
    }
  if (!(k->description = strdup (description)))
    {
      pstdout_perror (state_data->pstate, "strdup");
      goto cleanup;
    }
  k->flags = flags;
  k->checkout = checkout;
  k->commit = commit;
  k->validate = validate;

  if (section->keys)
    {
      struct ipmi_config_key *trav = section->keys;
      while (trav->next)
        trav = trav->next;
      trav->next = k;
    }
  else
    section->keys = k;

  return (0);

 cleanup:
  _config_key_destroy (k);
  return (-1);
}

int
ipmi_config_section_multi_channel_add_key (ipmi_config_state_data_t *state_data,
                                           struct ipmi_config_section *section,
                                           const char *key_name_base_str,
                                           const char *description,
                                           unsigned int flags,
                                           Key_Checkout checkout,
                                           Key_Commit commit,
                                           Key_Validate validate,
                                           int channel_index,
                                           uint8_t *channel_numbers,
                                           unsigned int channel_numbers_count)
{
  assert (state_data);
  assert (section);
  assert (key_name_base_str);
  assert (description);
  assert (checkout);
  assert (commit);
  assert (validate);
  assert (channel_numbers);
  assert (channel_index < (int)channel_numbers_count);

  if (channel_index < 0)
    {
      if (ipmi_config_section_add_key (state_data,
                                       section,
                                       key_name_base_str,
                                       description,
                                       flags,
                                       checkout,
                                       commit,
                                       validate) < 0)
        return (-1);
    }
  else
    {
      char key_name[IPMI_CONFIG_MAX_KEY_NAME_LEN];

      snprintf (key_name,
                IPMI_CONFIG_MAX_KEY_NAME_LEN,
                "%s_Channel_%u",
                key_name_base_str,
                channel_numbers[channel_index]);

      if (ipmi_config_section_add_key (state_data,
                                       section,
                                       key_name,
                                       description,
                                       flags,
                                       checkout,
                                       commit,
                                       validate) < 0)
        return (-1);
    }

  return (0);
}

int
ipmi_config_section_add_keyvalue (ipmi_config_state_data_t *state_data,
                                  struct ipmi_config_section *section,
                                  struct ipmi_config_key *key,
                                  const char *value_input,
                                  const char *value_output)
{
  struct ipmi_config_keyvalue *kv = NULL;

  assert (state_data);
  assert (section);
  assert (key);

  if (!(kv = malloc (sizeof (struct ipmi_config_keyvalue))))
    {
      pstdout_perror (state_data->pstate, "malloc");
      goto cleanup;
    }
  memset (kv, '\0', sizeof (struct ipmi_config_keyvalue));

  /* back pointer */
  kv->key = key;

  if (value_input)
    {
      if (!(kv->value_input = strdup (value_input)))
        {
          pstdout_perror (state_data->pstate, "strdup");
          goto cleanup;
        }
    }

  if (value_output)
    {
      if (!(kv->value_output = strdup (value_output)))
        {
          pstdout_perror (state_data->pstate, "strdup");
          goto cleanup;
        }
    }

  kv->next = NULL;

  if (section->keyvalues)
    {
      struct ipmi_config_keyvalue *trav = section->keyvalues;
      while (trav->next)
        trav = trav->next;
      trav->next = kv;
    }
  else
    section->keyvalues = kv;

  return (0);

 cleanup:
  _config_keyvalue_destroy (kv);
  return (-1);
}

int
ipmi_config_section_update_keyvalue_input (ipmi_config_state_data_t *state_data,
                                           struct ipmi_config_keyvalue *keyvalue,
                                           const char *value_input)
{
  assert (state_data);
  assert (keyvalue);

  if (value_input)
    {
      /* overwrite values, user can specify something else on the
       * command line
       */
      free (keyvalue->value_input);

      if (!(keyvalue->value_input = strdup (value_input)))
        {
          pstdout_perror (state_data->pstate, "strdup");
          return (-1);
        }
    }

  return (0);
}

int
ipmi_config_section_update_keyvalue_output (ipmi_config_state_data_t *state_data,
                                            struct ipmi_config_keyvalue *keyvalue,
                                            const char *value_output)
{
  assert (state_data);
  assert (keyvalue);
  assert (!keyvalue->value_output);

  if (value_output)
    {
      if (!(keyvalue->value_output = strdup (value_output)))
        {
          pstdout_perror (state_data->pstate, "strdup");
          return (-1);
        }
    }

  return (0);
}

int
ipmi_config_section_update_keyvalue_output_unsigned_int (ipmi_config_state_data_t *state_data,
                                                         struct ipmi_config_keyvalue *keyvalue,
                                                         unsigned int value_output)
{
  char buf[IPMI_CONFIG_PARSE_BUFLEN];

  assert (state_data);
  assert (keyvalue);
  assert (!keyvalue->value_output);

  snprintf (buf, IPMI_CONFIG_PARSE_BUFLEN, "%u", value_output);

  if (!(keyvalue->value_output = strdup (buf)))
    {
      pstdout_perror (state_data->pstate, "strdup");
      return (-1);
    }

  return (0);
}

int
ipmi_config_section_update_keyvalue_output_hex (ipmi_config_state_data_t *state_data,
                                                struct ipmi_config_keyvalue *keyvalue,
                                                unsigned int value_output)
{
  char buf[IPMI_CONFIG_PARSE_BUFLEN];

  assert (state_data);
  assert (keyvalue);
  assert (!keyvalue->value_output);

  snprintf (buf, IPMI_CONFIG_PARSE_BUFLEN, "0x%X", value_output);

  if (!(keyvalue->value_output = strdup (buf)))
    {
      pstdout_perror (state_data->pstate, "strdup");
      return (-1);
    }

  return (0);
}

int
ipmi_config_section_update_keyvalue_output_double (ipmi_config_state_data_t *state_data,
                                                   struct ipmi_config_keyvalue *keyvalue,
                                                   double value_output)
{
  char buf[IPMI_CONFIG_PARSE_BUFLEN];

  assert (state_data);
  assert (keyvalue);
  assert (!keyvalue->value_output);

  snprintf (buf, IPMI_CONFIG_PARSE_BUFLEN, "%f", value_output);

  if (!(keyvalue->value_output = strdup (buf)))
    {
      pstdout_perror (state_data->pstate, "strdup");
      return (-1);
    }

  return (0);
}

int
ipmi_config_sections_validate_keyvalue_inputs (ipmi_config_state_data_t *state_data)
{
  struct ipmi_config_section *s;
  int nonvalid_count = 0;
  int rv = -1;

  assert (state_data);

  s = state_data->sections;
  while (s)
    {
      struct ipmi_config_keyvalue *kv;

      kv = s->keyvalues;
      while (kv)
        {
          if (!kv->value_input)
            {
              pstdout_fprintf (state_data->pstate,
                               stderr,
                               "Value not specified for key '%s' in section '%s'\n",
                               kv->key->key_name,
                               s->section_name);
              nonvalid_count++;
              goto next_kv;
            }

          if (kv->value_input)
            {
              ipmi_config_validate_t v;

              if (!strcasecmp (kv->value_input, IPMI_CONFIG_USERNAME_NOT_SET_YET_STR))
                {
                  pstdout_fprintf (state_data->pstate,
                                   stderr,
                                   "Invalid value '%s' for key '%s' in section '%s'\n",
                                   kv->value_input,
                                   kv->key->key_name,
                                   s->section_name);
                  nonvalid_count++;
                  goto next_kv;
                }

              if ((v = kv->key->validate (state_data,
					  s->section_name,
                                          kv->key->key_name,
                                          kv->value_input)) == IPMI_CONFIG_VALIDATE_FATAL_ERROR)
                goto cleanup;

              if (v == IPMI_CONFIG_VALIDATE_NON_FATAL_ERROR)
                {
                  pstdout_fprintf (state_data->pstate,
                                   stderr,
                                   "Error validating value '%s' for key '%s' in section '%s'\n",
                                   kv->value_input,
                                   kv->key->key_name,
                                   s->section_name);
                  nonvalid_count++;
                }
              if (v == IPMI_CONFIG_VALIDATE_VALUE_CANNOT_BE_ENCODED_ACCURATELY)
                {
                  pstdout_fprintf (state_data->pstate,
                                   stderr,
                                   "Value '%s' for key '%s' in section '%s' cannot be encoded accurately, try another value\n",
                                   kv->value_input,
                                   kv->key->key_name,
                                   s->section_name);
                  nonvalid_count++;
                }
              if (v == IPMI_CONFIG_VALIDATE_OUT_OF_RANGE_VALUE)
                {
                  pstdout_fprintf (state_data->pstate,
                                   stderr,
                                   "Out of Range value '%s' for key '%s' in section '%s'\n",
                                   kv->value_input,
                                   kv->key->key_name,
                                   s->section_name);
                  nonvalid_count++;
                }
              if (v == IPMI_CONFIG_VALIDATE_INVALID_VALUE)
                {
                  pstdout_fprintf (state_data->pstate,
                                   stderr,
                                   "Invalid value '%s' for key '%s' in section '%s'\n",
                                   kv->value_input,
                                   kv->key->key_name,
                                   s->section_name);
                  nonvalid_count++;
                }
            }
        next_kv:
          kv = kv->next;
        }

      s = s->next;
    }

  rv = nonvalid_count;
 cleanup:
  return (rv);
}

int
ipmi_config_sections_insert_keyvalues (ipmi_config_state_data_t *state_data,
                                       struct ipmi_config_keypair *keypairs)
{
  struct ipmi_config_section *s;
  struct ipmi_config_key *k;
  struct ipmi_config_keyvalue *kv;
  struct ipmi_config_keypair *kp;
  int rv = 0;

  assert (state_data);
  assert (keypairs);

  kp = keypairs;
  while (kp)
    {
      if (!(s = ipmi_config_find_section (state_data, kp->section_name)))
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "Unknown section `%s'\n",
                           kp->section_name);
          rv = -1;
          goto next_keypair;
        }

      if (!(k = ipmi_config_find_key (s, kp->key_name)))
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "Unknown key `%s' in section `%s'\n",
                           kp->key_name,
                           kp->section_name);
          rv = -1;
          goto next_keypair;
        }

      if ((kv = ipmi_config_find_keyvalue (s, kp->key_name)))
        {
          if (ipmi_config_section_update_keyvalue_input (state_data,
                                                         kv,
                                                         kp->value_input) < 0)
            {
              rv = -1;
              goto cleanup;
            }
        }
      else
        {
          if (ipmi_config_section_add_keyvalue (state_data,
                                                s,
                                                k,
                                                kp->value_input,
                                                NULL) < 0)
            {
              rv = -1;
              goto cleanup;
            }
        }

    next_keypair:
      kp = kp->next;
    }

 cleanup:
  return (rv);
}

ipmi_config_err_t
ipmi_config_output_sections_list (ipmi_config_state_data_t *state_data)
{
  struct ipmi_config_section *s;

  assert (state_data);

  s = state_data->sections;
  while (s)
    {
      if (!(s->flags & IPMI_CONFIG_DO_NOT_LIST))
        pstdout_printf (state_data->pstate,
                        "%s\n",
                        s->section_name);
      s = s->next;
    }

  return (IPMI_CONFIG_ERR_SUCCESS);
}
