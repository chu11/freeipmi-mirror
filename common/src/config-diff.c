#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <assert.h>

#include "config-diff.h"
#include "config-checkout.h"

config_err_t
config_diff (struct config_section *sections,
             struct config_arguments *cmd_args,
             void *arg)
{
  struct config_section *s;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret = CONFIG_ERR_SUCCESS;
  config_err_t this_ret;

  assert(sections);
  assert(cmd_args);

  s = sections;
  while (s) 
    {
      struct config_keyvalue *kv = s->keyvalues;
      while (kv) 
        {
          assert(kv->value_input);

          if ((this_ret = kv->key->checkout (s->section_name,
                                             kv,
                                             arg)) == CONFIG_ERR_FATAL_ERROR)
            goto cleanup;
          
          if (this_ret == CONFIG_ERR_SUCCESS)
            {
              if (!same (kv->value_input, kv->value_output))
                printf ("%s:%s - input=`%s':actual=`%s'\n",
                        s->section_name,
                        kv->key->key_name,
                        kv->value_input,
                        kv->value_output);
            }
          else
            {
              printf("\t## ERROR: Unable to checkout %s:%s\n",
                     s->section_name,
                     kv->key->key_name);
              ret = this_ret;
            }
          kv = kv->next;
        }

      if (cmd_args->verbose)
        fprintf (stderr, "Completed diff of Section: %s\n",
                 section->section_name);

      s = s->next;
    }
  
  rv = ret;
 cleanup:
  return rv;
}


