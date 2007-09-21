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
#include "config-util.h"

struct config_section_str *
config_section_str_create(char *section_name)
{
  struct config_section_str *s = NULL;
  
  if (!(s = (struct config_section_str *)malloc(sizeof(struct config_section_str))))
    {
      perror("malloc");
      goto cleanup;
    }

  if (!(s->section_name = strdup(section_name)))
    {
      perror("strdup");
      goto cleanup;
    }
  s->next = NULL;

  return s;

 cleanup:
  if (s)
    {
      if (s->section_name)
        free(s->section_name);
      free(s);
    }
  return NULL;
}

int
config_section_str_append(struct config_section_str **section_strs,
                          struct config_section_str *section_str)
{
  assert(section_strs);
  assert(section_str);

  if (*section_strs)
    {
      struct config_section_str *sstr;

      sstr = *section_strs;
      while (sstr)
        {
          if (!strcasecmp(sstr->section_name, section_str->section_name))
            {
              fprintf(stderr,
                      "Duplicate section '%s' specified\n",
                      sstr->section_name);
              return -1;
            }
          sstr = sstr->next;
        }

      sstr = *section_strs;
      while (sstr->next)
        sstr = sstr->next;
      sstr->next = section_str;
    }
  else
    *section_strs = section_str;

  return 0;
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
          struct config_section *s_next = sections->next;
          config_section_destroy(sections);
          sections = s_next;
        }
    }
}

struct config_section *
config_section_create(const char *section_name,
                      const char *section_comment_section_name,
                      const char *section_comment,
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

int
config_section_update_keyvalue(struct config_keyvalue *keyvalue,
                               const char *value_input,
                               const char *value_output)
{
  assert(keyvalue);

  if (value_input)
    {
      /* overwrite values, usually user specifies something on command line */
      if (keyvalue->value_input)
        free(keyvalue->value_input);

      if (!(keyvalue->value_input = strdup(value_input)))
        {
          perror("strdup");
          return -1;
        }
    }

  if (value_output)
    {
      if (keyvalue->value_output)
        free(keyvalue->value_output);

      if (!(keyvalue->value_output = strdup(value_output)))
        {
          perror("strdup");
          return -1;
        }
    }
   
  return 0;
}

int
config_sections_validate_keyvalue_inputs(struct config_section *sections,
                                         int value_input_required,
                                         int debug,
                                         void *arg)
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
                                 struct config_keyinput *keyinputs,
                                 int debug)
{
  struct config_section *s;
  struct config_key *k;
  struct config_keyvalue *kv;
  struct config_keyinput *ki;
  int rv = 0;

  assert(sections);
  assert(keyinputs);

  ki = keyinputs;
  while (ki)
    {
      if (!(s = config_find_section(sections, ki->section_name)))
        {
          fprintf(stderr, "Unknown section `%s'\n", ki->section_name);
          rv = -1;
          goto next_keyinput;
        }

      if (!(k = config_find_key(s, ki->key_name)))
        {
          fprintf(stderr,
                  "Unknown key `%s' in section `%s'\n",
                  ki->key_name,
                  ki->section_name);
          rv = -1;
          goto next_keyinput;
        }

      if ((kv = config_find_keyvalue(s, ki->key_name)))
        {
          if (config_section_update_keyvalue(kv,
                                             ki->value_input,
                                             NULL) < 0)
            {
              rv = -1;
              goto cleanup;
            }
        }
      else
        {
          if (config_section_add_keyvalue(s,
                                          k,
                                          ki->value_input,
                                          NULL) < 0)
            {
              rv = -1;
              goto cleanup;
            }
        }

    next_keyinput:
      ki = ki->next;
    }
  
 cleanup:
  return rv;
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




