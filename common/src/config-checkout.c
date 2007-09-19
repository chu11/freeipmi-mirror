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

#include "config-checkout.h"
#include "config-comment.h"
#include "config-section.h"

config_err_t
config_checkout_section(struct config_section *section, 
                        FILE *fp,
                        int debug,
                        void *arg)
{
  struct config_keyvalue *kv;
  config_err_t rv = CONFIG_ERR_SUCCESS;

  assert(section);
  assert(fp);

  if (section->flags & CONFIG_DO_NOT_CHECKOUT)
    return CONFIG_ERR_SUCCESS;

  /* if no keyvalues specified by user, we want to checkout all keys,
   * so build keyvalues list appropriately 
   */
  if (!section->keyvalues)
    {
      struct config_key *k;

      k = section->keys;
      while (k)
        {
          if (!(k->flags & CONFIG_DO_NOT_CHECKOUT))
            {
              if (config_section_add_keyvalue(section,
                                              k,
                                              NULL, 
                                              NULL) < 0)
                return CONFIG_ERR_FATAL_ERROR;
            }
          k = k->next;
        }
    }

  if (section->checkout(section->section_name,
                        section->keyvalues,
                        fp,
                        arg) == CONFIG_ERR_FATAL_ERROR)
    return CONFIG_ERR_FATAL_ERROR;

  if (section->section_comment_section_name
      && section->section_comment)
    {
      if (config_section_comments(section->section_comment_section_name,
                                  section->section_comment,
                                  fp) < 0)
        {
          if (debug)
            fprintf(stderr, "Comment output error\n");
          rv = CONFIG_ERR_NON_FATAL_ERROR;
        }
    }

  fprintf(fp, "Section %s\n", section->section_name);

  kv = section->keyvalues;
  while (kv)
    { 
      int key_len = 0;

      fprintf(fp, "\t## %s\n", kv->key->description);
      
      /* If a value was not checked out, don't output the field 
       * 
       * Note that a checked out value can be an empty string
       * (i.e. "").
       */
      if (kv->value_output)
        {
          /* achu: Certain keys should have their checked out
           * value automatically commented out.  Sometimes (in the
           * case of passwords) they cannot be checked out, so the
           * default is for value to be empty.  We do not want the
           * user accidently commiting this checked out file,
           * which (in this example) clears the password.
           *
           * Some other keys may or may not have a value, depending on
           * the IPMI version or the implementation.
           */
          if (kv->key->flags & CONFIG_CHECKOUT_KEY_COMMENTED_OUT)
            key_len = fprintf(fp, "\t## %s", kv->key->key_name);
          else if (kv->key->flags & CONFIG_CHECKOUT_KEY_COMMENTED_OUT_IF_VALUE_EMPTY)
            {
              if (kv->value_output && strlen(kv->value_output))
                key_len = fprintf(fp, "\t%s", kv->key->key_name);
              else
                key_len = fprintf(fp, "\t## %s", kv->key->key_name);
            }
          else
            key_len = fprintf(fp, "\t%s", kv->key->key_name);
          
          while (key_len <= 45)
            {
              fprintf(fp, " ");
              key_len++;
            }
          
          fprintf(fp, "%s\n", kv->value_output);
        }

      kv = kv->next;
    }

  fprintf(fp, "EndSection\n");
  return rv;
}

config_err_t 
config_checkout_all(struct config_section *sections, 
                    FILE *fp,
                    int debug,
                    void *arg)
{
  struct config_section *s;
  config_err_t rv = CONFIG_ERR_SUCCESS;
  config_err_t ret;

  assert(sections);
  assert(fp);

  s = sections;
  while (s)
    {
      if ((ret = config_checkout_section(s, fp, debug, arg)) != CONFIG_ERR_SUCCESS)
        {
          if (ret == CONFIG_ERR_FATAL_ERROR)
            {
              rv = CONFIG_ERR_FATAL_ERROR;
              break;
            }
          rv = ret;
        }
      s = s->next;
    }

  return rv;
}
