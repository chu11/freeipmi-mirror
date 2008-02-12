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
#include "config-utils.h"

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
config_section_add_key (struct config_section *section,
                        const char *key_name,
                        const char *description,
                        unsigned int flags,
                        Key_Checkout checkout,
                        Key_Commit commit,
                        Key_Validate validate)
{
  struct config_key *k = NULL;

  assert(section);
  assert(key_name);
  assert(description);
  assert(checkout);
  assert(commit);
  assert(validate);

  if (!(k = (struct config_key *)malloc(sizeof(struct config_key))))
    {
      perror("malloc");
      goto cleanup;
    }
  memset(k, '\0', sizeof(struct config_key));

  if (!(k->key_name = strdup(key_name)))
    {
      perror("strdup");
      goto cleanup;
    }
  if (!(k->description = strdup(description)))
    {
      perror("strdup");
      goto cleanup;
    }
  k->flags = flags;
  k->checkout = checkout;
  k->commit = commit;
  k->validate = validate;

  if (section->keys)
    {
      struct config_key *trav = section->keys;
      while (trav->next)
	trav = trav->next;
      trav->next = k;
    }
  else
    section->keys = k;

  return 0;
 cleanup:
  _config_key_destroy(k);
  return -1;
}

int
config_section_add_keyvalue (struct config_section *section,
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

int
config_section_update_keyvalue_input(struct config_keyvalue *keyvalue,
                                     const char *value_input)
{
  assert(keyvalue);
  
  if (value_input)
    {
      /* overwrite values, user can specify something else on the
       * command line 
       */
      if (keyvalue->value_input)
        free(keyvalue->value_input);

      if (!(keyvalue->value_input = strdup(value_input)))
        {
          perror("strdup");
          return -1;
        }
    }

  return 0;
}

int
config_section_update_keyvalue_output(struct config_keyvalue *keyvalue,
                                      const char *value_output)
{
  assert(keyvalue);
  assert(!keyvalue->value_output);

  if (value_output)
    {
      
      if (!(keyvalue->value_output = strdup(value_output)))
        {
          perror("strdup");
          return -1;
        }
    }

  return 0;
}

int
config_section_update_keyvalue_output_int(struct config_keyvalue *keyvalue,
                                          unsigned int value_output)
{
  char buf[CONFIG_PARSE_BUFLEN];

  assert(keyvalue);
  assert(!keyvalue->value_output);

  snprintf(buf, CONFIG_PARSE_BUFLEN, "%u", value_output);
  
  if (!(keyvalue->value_output = strdup(buf)))
    {
      perror("strdup");
      return -1;
    }

  return 0;
}

int
config_section_update_keyvalue_output_double(struct config_keyvalue *keyvalue,
                                             double value_output)
{
  char buf[CONFIG_PARSE_BUFLEN];

  assert(keyvalue);
  assert(!keyvalue->value_output);

  snprintf(buf, CONFIG_PARSE_BUFLEN, "%.2f", value_output);
  
  if (!(keyvalue->value_output = strdup(buf)))
    {
      perror("strdup");
      return -1;
    }

  return 0;
}

int
config_sections_validate_keyvalue_inputs(struct config_section *sections,
                                         int value_input_required)
{
  struct config_section *s;
  int nonvalid_count = 0;
  int rv = -1;

  assert(sections);

  s = sections;
  while (s)
    {
      struct config_keyvalue *kv;

      kv = s->keyvalues;
      while (kv)
        {
          if (value_input_required && !kv->value_input)
            {
              fprintf(stderr,
                      "Value not specified for key '%s' in section '%s'\n",
                      kv->key->key_name,
                      s->section_name);
              nonvalid_count++;
              goto next_kv;
            }

          if (kv->value_input)
            {
              config_validate_t v;

              if ((v = kv->key->validate(s->section_name,
                                         kv->key->key_name,
                                         kv->value_input)) == CONFIG_VALIDATE_FATAL_ERROR)
                goto cleanup;

              if (v == CONFIG_VALIDATE_INVALID_VALUE)
                {
                  fprintf(stderr,
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
  return rv;
}

int
config_sections_insert_keyvalues(struct config_section *sections,
                                 struct config_keypair *keypairs)
{
  struct config_section *s;
  struct config_key *k;
  struct config_keyvalue *kv;
  struct config_keypair *kp;
  int rv = 0;

  assert(sections);
  assert(keypairs);

  kp = keypairs;
  while (kp)
    {
      if (!(s = config_find_section(sections, kp->section_name)))
        {
          fprintf(stderr, "Unknown section `%s'\n", kp->section_name);
          rv = -1;
          goto next_keypair;
        }

      if (!(k = config_find_key(s, kp->key_name)))
        {
          fprintf(stderr,
                  "Unknown key `%s' in section `%s'\n",
                  kp->key_name,
                  kp->section_name);
          rv = -1;
          goto next_keypair;
        }

      if ((kv = config_find_keyvalue(s, kp->key_name)))
        {
          if (config_section_update_keyvalue_input(kv,
                                                   kp->value_input) < 0)
            {
              rv = -1;
              goto cleanup;
            }
        }
      else
        {
          if (config_section_add_keyvalue(s,
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
  return rv;
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
