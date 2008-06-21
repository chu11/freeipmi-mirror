/* 
   Copyright (C) 2003-2008 FreeIPMI Core Team

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
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.  
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

#include "config-tool-utils.h"

#include "freeipmi-portability.h"
#include "pstdout.h"

int
config_keypair_parse_string(char *str,
                            char **section_name,
                            char **key_name,
                            char **value)
{
  char *str_temp = NULL;
  char *section_name_tok = NULL;
  char *key_name_tok = NULL;
  char *value_tok = NULL;
  char *ptr;
  char *buf;
  int rv = -1;

  assert(str);
  assert(section_name);
  assert(key_name);
  assert(value);

  *section_name = NULL;
  *key_name = NULL;
  *value = NULL;

  if (!(str_temp = strdup(str)))
    {
      perror("strdup");
      goto cleanup;
    }

  section_name_tok = strtok_r(str_temp, ":", &buf);
  key_name_tok = strtok_r(NULL, "=", &buf);
  value_tok = strtok_r(NULL, "\0", &buf);

  if (!(section_name_tok && key_name_tok))
    {
      fprintf(stderr,
              "Improperly input keypair '%s'\n",
              str);
      goto cleanup;
    }

  /* get rid of spaces stuck in the string */
  if (section_name_tok)
    section_name_tok = strtok_r(section_name_tok, " \t", &buf);
  if (key_name_tok)
    key_name_tok = strtok_r(key_name_tok, " \t", &buf);
  if (value_tok)
    value_tok = strtok_r(value_tok, " \t", &buf);

  if (section_name_tok)
    {
      if (!(ptr = strdup(section_name_tok)))
        {
          perror("strdup");
          goto cleanup;
        }
      *section_name = ptr;
    }
  if (key_name_tok)
    {
      if (!(ptr = strdup(key_name_tok)))
        {
          perror("strdup");
          goto cleanup;
        }
      *key_name = ptr;
    }
  if (value_tok)
    {
      if (!(ptr = strdup(value_tok)))
        {
          perror("strdup");
          goto cleanup;
        }
      *value = ptr;
    }

  rv = 0;
 cleanup:
  if (str_temp)
    free(str_temp);
  if (rv < 0)
    {
      if (*section_name)
        {
          free(*section_name);
          *section_name = NULL;
        }
      if (*key_name)
        {
          free(*key_name);
          *key_name = NULL;
        }
      if (*value)
        {
          free(*value);
          *value = NULL;
        }
    }
  return rv;
}

int
config_keypair_append(struct config_keypair **keypairs,
                      struct config_keypair *keypair)
{
  assert(keypairs);
  assert(keypair);

  if (*keypairs)
    {
      struct config_keypair *kp;

      kp = *keypairs;
      while (kp)
        {
          if (!strcasecmp(kp->section_name, keypair->section_name)
              && !strcasecmp(kp->key_name, keypair->key_name))
            {
              fprintf(stderr,
                      "Duplicate section:key pair '%s:%s' specified\n",
                      kp->section_name, kp->key_name);
              return -1;
            }
          kp = kp->next;
        }

      kp = *keypairs;
      while (kp->next)
        kp = kp->next;
      kp->next = keypair;
    }
  else
    *keypairs = keypair;

  return 0;
}

void
config_keypairs_destroy(struct config_keypair *keypairs)
{
  if (keypairs)
    {
      while (keypairs)
        {
          struct config_keypair *kp_next = keypairs->next;
          config_keypair_destroy(keypairs);
          keypairs = kp_next;
        }
    }
}

struct config_keypair *
config_keypair_create(const char *section_name,
                      const char *key_name,
                      const char *value_input)
{
  struct config_keypair *keypair = NULL;

  assert(section_name);
  assert(key_name);

  if (!(keypair = (struct config_keypair *)malloc(sizeof(struct config_keypair))))
    {
      perror("malloc");
      goto cleanup;
    }
  memset(keypair, '\0', sizeof(struct config_keypair));

  if (!(keypair->section_name = strdup(section_name)))
    {
      perror("strdup");
      goto cleanup;
    }

  if (!(keypair->key_name = strdup(key_name)))
    {
      perror("strdup");
      goto cleanup;
    }

  if (value_input)
    {
      if (!(keypair->value_input = strdup(value_input)))
        {
          perror("strdup");
          goto cleanup;
        }
    }

  return keypair;

 cleanup:
  if (keypair)
    config_keypair_destroy(keypair);
  return NULL;
}

void
config_keypair_destroy(struct config_keypair *keypair)
{
  if (keypair)
    {
      if (keypair->section_name)
        free(keypair->section_name);
      if (keypair->key_name)
        free(keypair->key_name);
      if (keypair->value_input)
        free(keypair->value_input);
      free(keypair);
    }
}

struct config_section_str *
config_section_str_create(char *section_name)
{
  struct config_section_str *sstr = NULL;

  if (!(sstr = (struct config_section_str *)malloc(sizeof(struct config_section_str))))
    {
      perror("malloc");
      goto cleanup;
    }

  if (!(sstr->section_name = strdup(section_name)))
    {
      perror("strdup");
      goto cleanup;
    }
  sstr->next = NULL;

  return sstr;

 cleanup:
  config_section_str_destroy(sstr);
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

void 
config_section_str_destroy(struct config_section_str *section_str)
{
  if (section_str)
    {
      if (section_str->section_name)
        free(section_str->section_name);
      free(section_str);
    }
}

int8_t
config_ipv4_address_string2int(pstdout_state_t pstate,
                               char *src, 
                               uint32_t *dest)
{
  unsigned int b1, b2, b3, b4;
  uint64_t val;
  int rv;

  assert(src && dest);

  if ((rv = sscanf (src, 
                    "%u.%u.%u.%u", 
                    &b1,
                    &b2, 
                    &b3, 
                    &b4)) < 0)
    {
      PSTDOUT_PERROR(pstate,
                     "sscanf");
      return (-1);
    }

  if (rv != 4)
    {
      PSTDOUT_FPRINTF(pstate,
                      stderr, 
                      "config_ipv4_address_string2int: Invalid src input: %s\n",
                      src);
      return (-1);
    }

  val = 0;
  val |= (uint64_t)b1;
  val |= ((uint64_t)b2 << 8);
  val |= ((uint64_t)b3 << 16);
  val |= ((uint64_t)b4 << 24);

  *dest = val;
  return (0);
}

int8_t
config_mac_address_string2int(pstdout_state_t pstate,
                              char *src, 
                              uint64_t *dest)
{
  unsigned int b1, b2, b3, b4, b5, b6;
  uint64_t val;
  int rv;

  assert(src && dest);

  if ((rv = sscanf (src,
                    "%02X:%02X:%02X:%02X:%02X:%02X", 
                    &b1,
                    &b2, 
                    &b3, 
                    &b4,
                    &b5, 
                    &b6)) < 0)
    {
      PSTDOUT_PERROR(pstate,
                     "sscanf");
      return (-1);
    }

  if (rv != 6)
    {
      PSTDOUT_FPRINTF(pstate,
                      stderr, 
                      "config_mac_address_string2int: Invalid src input: %s\n",
                      src);
      return (-1);
    }

  val = 0;
  val |= (uint64_t)b1;
  val |= ((uint64_t)b2 << 8);
  val |= ((uint64_t)b3 << 16);
  val |= ((uint64_t)b4 << 24);
  val |= ((uint64_t)b5 << 32);
  val |= ((uint64_t)b6 << 40);

  *dest = val;
  return (0);
}

struct config_section *
config_find_section(pstdout_state_t pstate,
                    struct config_section *sections, 
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
config_find_key(pstdout_state_t pstate,
                struct config_section *section, 
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
config_find_keyvalue(pstdout_state_t pstate,
                     struct config_section *section, 
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
