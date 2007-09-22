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

#include "config-util.h"

struct config_section *
config_find_section(struct config_section *sections, 
                    const char *section_name)
{
  struct config_section *s = NULL;

  assert(sections);
  assert(section_name);

  s = sections;
  while (s)
    {
      if (!strcasecmp(section_name, s->section_name))
        break;
      s = s->next;
    }

  return s;
}

struct config_key *
config_find_key(struct config_section *section, 
                const char *key_name)
{
  struct config_key *k = NULL;

  assert(section);
  assert(key_name);

  k = section->keys;
  while (k)
    {
      if (!strcasecmp(key_name, k->key_name))
        break;
      k = k->next;
    }

  return k;
}

struct config_keyvalue *
config_find_keyvalue(struct config_section *section, 
                     const char *key_name)
{
  struct config_keyvalue *kv = NULL;

  assert(section);
  assert(key_name);

  kv = section->keyvalues;
  while (kv)
    {
      if (!strcasecmp(key_name, kv->key->key_name))
        break;
      kv = kv->next;
    }

  return kv;
}
