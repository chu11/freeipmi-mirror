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
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <errno.h>
#include <assert.h>

#include "config-tool-checkout.h"
#include "config-tool-comment.h"
#include "config-tool-section.h"

config_err_t
config_checkout_section(struct config_section *section,
                        struct config_arguments *cmd_args,
                        int all_keys_if_none_specified,
                        FILE *fp,
                        void *arg)
{
  struct config_keyvalue *kv;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret = CONFIG_ERR_SUCCESS;
  config_err_t this_ret;

  assert(section);
  assert(cmd_args);
  assert(fp);

  if (section->flags & CONFIG_DO_NOT_CHECKOUT)
    return CONFIG_ERR_SUCCESS;

  /* if no keyvalues specified by user, we want to checkout all keys,
   * so build keyvalues list appropriately
   */
  if (all_keys_if_none_specified && !section->keyvalues)
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
                goto cleanup;
            }
          k = k->next;
        }
    }

  if (!section->keyvalues)
    return CONFIG_ERR_SUCCESS;

  /* no need to output if fp NULL, for example if 'diff' is calling
   * us.
   */
  if (section->section_comment_section_name
      && section->section_comment)
    {
      if (config_section_comments(section->section_comment_section_name,
                                  section->section_comment,
                                  fp) < 0)
        {
          if (cmd_args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
            fprintf(stderr, "## Error: Comment output error\n");
          ret = CONFIG_ERR_NON_FATAL_ERROR;
        }
    }
  
  fprintf(fp, "Section %s\n", section->section_name);

  kv = section->keyvalues;
  while (kv)
    {
      int key_len = 0;

      if (kv->key->flags & CONFIG_UNDEFINED)
        {
          if (config_section_update_keyvalue_output(kv, "Undefined") < 0)
            this_ret = CONFIG_ERR_FATAL_ERROR;
        }
      else
        {
          if ((this_ret = kv->key->checkout (section->section_name,
                                             kv,
                                             arg)) == CONFIG_ERR_FATAL_ERROR)
            goto cleanup;
        }

      if (this_ret == CONFIG_ERR_NON_FATAL_ERROR)
        {
          if (cmd_args->verbose)
            fprintf (fp, "\t## ERROR: Unable to checkout %s:%s\n",
                     section->section_name,
                     kv->key->key_name);
          ret = CONFIG_ERR_NON_FATAL_ERROR;
        } 
      else
        {
          assert(kv->value_output);

          fprintf(fp, "\t## %s\n", kv->key->description);

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
          if (kv->key->flags & CONFIG_CHECKOUT_KEY_COMMENTED_OUT
              || kv->key->flags & CONFIG_UNDEFINED)
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

          while (key_len <= CONFIG_CHECKOUT_LINE_LEN)
            {
              fprintf(fp, " ");
              key_len++;
            }

          fprintf(fp, "%s\n", kv->value_output);
        }

      kv = kv->next;
    }

  fprintf(fp, "EndSection\n");
  rv = ret;
 cleanup:
  return rv;
}

config_err_t
config_checkout (struct config_section *sections,
                 struct config_arguments *cmd_args,
                 int all_keys_if_none_specified,
                 FILE *fp,
                 void *arg)
{
  struct config_section *s;
  config_err_t rv = CONFIG_ERR_SUCCESS;
  config_err_t ret;

  assert(sections);
  assert(cmd_args);
  assert(fp);

  s = sections;
  while (s)
    {
      if ((ret = config_checkout_section(s,
                                         cmd_args,
                                         all_keys_if_none_specified,
                                         fp,
                                         arg)) != CONFIG_ERR_SUCCESS)
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
