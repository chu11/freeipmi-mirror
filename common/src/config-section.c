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

#include "config-section.h"

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
          struct config_section *s_next = sections->next;
          config_section_destroy(sections);
          sections = s_next;
        }
    }
}

struct config_section *
config_section_create(char *section_name,
                      char *section_comment_section_name,
                      char *section_comment,
                      unsigned int flags,
                      Section_Checkout checkout,
                      Section_Commit commit)
{
  struct config_section *section = NULL;

  assert(section_name);
  assert(checkout);
  assert(commit);

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

  section->checkout = checkout;
  section->commit = commit;

  return section;
 cleanup:
  if (section)
    config_section_destroy (section);
  return NULL;
}

static void
_config_key_destroy(struct config_key *key)
{
  if (key)
    {
      if (key->key_name)
        free(key->key_name);
      if (key->description)
        free(key->description);
      free(key);
    }
}

static void
_config_keyvalue_destroy(struct config_keyvalue *keyvalue)
{
  if (keyvalue)
    {
      if (keyvalue->value_input)
        free(keyvalue->value_input);
      if (keyvalue->value_output)
        free(keyvalue->value_output);
      free(keyvalue);
    }
}

void
config_section_destroy(struct config_section *section)
{
  if (section)
    {
      if (section->section_name)
        free(section->section_name);

      if (section->section_comment_section_name)
        free(section->section_comment_section_name);

      if (section->section_comment)
        free(section->section_comment);

      while (section->keys)
        {
          struct config_key *k_next = (section->keys)->next;
          _config_key_destroy(section->keys);
          section->keys = k_next;
        }

      while (section->keyvalues)
        {
          struct config_keyvalue *kv_next = (section->keyvalues)->next;
          _config_keyvalue_destroy(section->keyvalues);
          section->keyvalues = kv_next;
        }

      free(section);
    }
}

int 
config_section_add_key(struct config_section *section,
                       const char *key_name,
                       const char *description,
                       unsigned int flags,
                       Key_Validate validate)
{
  struct config_key *key = NULL;

  assert(section);
  assert(key_name);
  assert(description);
  assert(validate);

  if (!(key = (struct config_key *)malloc(sizeof(struct config_key))))
    {
      perror("malloc");
      goto cleanup;
    }

  if (!(key->key_name = strdup(key_name)))
    {
      perror("strdup");
      goto cleanup;
    }
  if (!(key->description = strdup(description)))
    {
      perror("strdup");
      goto cleanup;
    }
  key->flags = flags;
  key->validate = validate;

  if (section->keys)
    {
      struct config_key *k = section->keys;
      while (k->next)
        k = k->next;
      k->next = key;
    }
  else
    section->keys = key;

  return 0;
 cleanup:
  _config_key_destroy(key);
  return -1;
}

int
config_section_add_keyvalue(struct config_section *section,
                            struct config_key *key,
                            const char *value_input,
                            const char *value_output)
{
  struct config_keyvalue *kv = NULL;
  
  assert(section);
  assert(key);

  if (!(kv = malloc(sizeof(struct config_keyvalue))))
    {
      perror("malloc");
      goto cleanup;
    }
  memset(kv, '\0', sizeof(struct config_keyvalue));

  /* back pointer */
  kv->key = key;

  if (value_input)
    {
      if (!(kv->value_input = strdup(value_input)))
        {
          perror("strdup");
          goto cleanup;
        }
    }

  if (value_output)
    {
      if (!(kv->value_output = strdup(value_output)))
        {
          perror("strdup");
          goto cleanup;
        }
    }
   
  kv->next = NULL;
  
  if (section->keyvalues)
    {
      struct config_keyvalue *kv = section->keyvalues;
      while (kv->next)
        kv = kv->next;
      kv->next = kv;
    }
  else
    section->keyvalues = kv;

  return 0;

 cleanup:
  _config_keyvalue_destroy(kv);
  return -1;
}

config_err_t 
config_sections_output_list(struct config_section *sections)
{
  struct config_section *s;

  assert(sections);

  s = sections;
  while (s)
    {
      if (!(s->flags & CONFIG_DO_NOT_CHECKOUT))
        printf("%s\n", s->section_name);
      s = s->next;
    }

  return CONFIG_ERR_SUCCESS;
}




