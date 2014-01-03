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

#include "ipmi-config-checkout.h"
#include "ipmi-config-comment.h"
#include "ipmi-config-section.h"
#include "ipmi-config-utils.h"

#include "freeipmi-portability.h"
#include "pstdout.h"

ipmi_config_err_t
ipmi_config_checkout_section (ipmi_config_state_data_t *state_data,
                              struct ipmi_config_section *section,
                              int all_keys_if_none_specified,
                              FILE *fp)
{
  struct ipmi_config_keyvalue *kv;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret = IPMI_CONFIG_ERR_SUCCESS;
  ipmi_config_err_t this_ret;
  unsigned int line_length;

  assert (state_data);
  assert (section);
  assert (fp);

  /* if no keyvalues specified by user, we want to checkout all keys,
   * so build keyvalues list appropriately
   */
  if (all_keys_if_none_specified && !section->keyvalues)
    {
      struct ipmi_config_key *k;

      k = section->keys;
      while (k)
        {
          if (!(k->flags & IPMI_CONFIG_DO_NOT_CHECKOUT))
            {
              if (ipmi_config_section_add_keyvalue (state_data,
                                                    section,
                                                    k,
                                                    NULL,
                                                    NULL) < 0)
                goto cleanup;
            }
          k = k->next;
        }
    }

  if (!section->keyvalues)
    return (IPMI_CONFIG_ERR_SUCCESS);

  /* no need to output if fp NULL, for example if 'diff' is calling
   * us.
   */
  if (section->section_comment_section_name
      && section->section_comment
      && all_keys_if_none_specified)
    {
      if (ipmi_config_section_comments (state_data,
                                        section->section_comment_section_name,
                                        section->section_comment,
                                        fp) < 0)
        {
          if (state_data->prog_data->args->common_args.debug)
            pstdout_fprintf (state_data->pstate,
                             stderr,
                             "## Error: Comment output error\n");
          ret = IPMI_CONFIG_ERR_NON_FATAL_ERROR;
        }
    }

  if (!section->line_length)
    line_length = IPMI_CONFIG_CHECKOUT_LINE_LEN;
  else
    line_length = section->line_length;

  ipmi_config_pstdout_fprintf (state_data,
                               fp,
                               "Section %s\n",
                               section->section_name);


  kv = section->keyvalues;
  while (kv)
    {
      int key_len = 0;

      if (kv->key->flags & IPMI_CONFIG_UNDEFINED)
        {
          if (ipmi_config_section_update_keyvalue_output (state_data,
                                                          kv,
                                                          "Undefined") < 0)
            this_ret = IPMI_CONFIG_ERR_FATAL_ERROR;
          else
            this_ret = IPMI_CONFIG_ERR_SUCCESS;
        }
      else
        {
          if ((this_ret = kv->key->checkout (state_data,
					     section->section_name,
                                             kv)) == IPMI_CONFIG_ERR_FATAL_ERROR)
            goto cleanup;
        }

      if (IPMI_CONFIG_IS_NON_FATAL_ERROR (this_ret))
        {
          if (state_data->prog_data->args->verbose_count > 1)
            {
              if (this_ret == IPMI_CONFIG_ERR_NON_FATAL_ERROR_NOT_SUPPORTED)
                ipmi_config_pstdout_fprintf (state_data,
                                             fp,
                                             "\t## Unable to checkout %s:%s : Not Supported\n",
                                             section->section_name,
                                             kv->key->key_name);
              else
                ipmi_config_pstdout_fprintf (state_data,
                                             fp,
                                             "\t## Unable to checkout %s:%s\n",
                                             section->section_name,
                                             kv->key->key_name);
            }
          ret = IPMI_CONFIG_ERR_NON_FATAL_ERROR;
        }
      else
        {
          char obuf[IPMI_CONFIG_OUTPUT_BUFLEN];

          assert (kv->value_output);

          if (strchr (kv->key->description, '\n'))
            {
              char *cptr;

              cptr = kv->key->description;
              ipmi_config_pstdout_fprintf (state_data,
                                           fp,
                                           "\t## ");
              while (*cptr)
                {
                  if (*cptr == '\n')
                    ipmi_config_pstdout_fprintf (state_data,
                                                 fp,
                                                 "\n\t## ");
                  else
                    ipmi_config_pstdout_fprintf (state_data,
                                                 fp,
                                                 "%c",
                                                 *cptr);
                  cptr++;
                }
              ipmi_config_pstdout_fprintf (state_data,
                                           fp,
                                           "\n");
            }
          else
            ipmi_config_pstdout_fprintf (state_data,
                                         fp,
                                         "\t## %s\n",
                                         kv->key->description);

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

          /* achu:
           *
           * The pstdout library does not return string lengths like
           * printf/fprintf do.  So we have to calculate it via
           * snprintf.
           */
          if (kv->key->flags & IPMI_CONFIG_CHECKOUT_KEY_COMMENTED_OUT
              || kv->key->flags & IPMI_CONFIG_UNDEFINED
              || (kv->key->flags & IPMI_CONFIG_CHECKOUT_KEY_COMMENTED_OUT_IF_VALUE_EMPTY
                  && !strlen (kv->value_output))
              || (kv->key->flags & IPMI_CONFIG_USERNAME_NOT_SET_YET
                  && !strcasecmp (kv->value_output, IPMI_CONFIG_USERNAME_NOT_SET_YET_STR)))
            key_len = snprintf (obuf,
                                IPMI_CONFIG_OUTPUT_BUFLEN,
                                "\t## %s",
                                kv->key->key_name);
          else
            key_len = snprintf (obuf,
                                IPMI_CONFIG_OUTPUT_BUFLEN,
                                "\t%s",
                                kv->key->key_name);

          ipmi_config_pstdout_fprintf (state_data,
                                       fp,
                                       "%s",
                                       obuf);

          while (key_len <= line_length)
            {
              ipmi_config_pstdout_fprintf (state_data,
                                           fp,
                                           " ");
              key_len++;
            }

          ipmi_config_pstdout_fprintf (state_data,
                                       fp,
                                       " %s\n",
                                       kv->value_output);
        }

      kv = kv->next;
    }

  ipmi_config_pstdout_fprintf (state_data,
                               fp,
                               "EndSection\n");
  rv = ret;
 cleanup:
  return (rv);
}

ipmi_config_err_t
ipmi_config_checkout (ipmi_config_state_data_t *state_data,
                      int all_keys_if_none_specified,
                      FILE *fp)
{
  struct ipmi_config_section *s;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_SUCCESS;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (fp);

  s = state_data->sections;
  while (s)
    {
      if (!(s->flags & IPMI_CONFIG_DO_NOT_CHECKOUT)
          || !all_keys_if_none_specified)
        {
          if ((ret = ipmi_config_checkout_section (state_data,
                                                   s,
                                                   all_keys_if_none_specified,
                                                   fp)) != IPMI_CONFIG_ERR_SUCCESS)
            {
              if (ret == IPMI_CONFIG_ERR_FATAL_ERROR)
                {
                  rv = IPMI_CONFIG_ERR_FATAL_ERROR;
                  break;
                }
              rv = ret;
            }
        }
      s = s->next;
    }

  return (rv);
}
