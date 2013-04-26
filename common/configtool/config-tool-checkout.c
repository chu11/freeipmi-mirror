/*
 * Copyright (C) 2003-2013 FreeIPMI Core Team
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

#include "config-tool-checkout.h"
#include "config-tool-comment.h"
#include "config-tool-section.h"
#include "config-tool-utils.h"

#include "freeipmi-portability.h"
#include "pstdout.h"

config_err_t
config_checkout_section (pstdout_state_t pstate,
                         struct config_section *section,
                         struct config_arguments *cmd_args,
                         int all_keys_if_none_specified,
                         FILE *fp,
                         unsigned int line_length,
                         void *arg)
{
  struct config_keyvalue *kv;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret = CONFIG_ERR_SUCCESS;
  config_err_t this_ret;

  assert (section);
  assert (cmd_args);
  assert (fp);

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
              if (config_section_add_keyvalue (pstate,
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
    return (CONFIG_ERR_SUCCESS);

  /* no need to output if fp NULL, for example if 'diff' is calling
   * us.
   */
  if (section->section_comment_section_name
      && section->section_comment
      && all_keys_if_none_specified)
    {
      if (config_section_comments (pstate,
                                   section->section_comment_section_name,
                                   section->section_comment,
                                   fp) < 0)
        {
          if (cmd_args->common_args.debug)
            pstdout_fprintf (pstate,
                             stderr,
                             "## Error: Comment output error\n");
          ret = CONFIG_ERR_NON_FATAL_ERROR;
        }
    }

  if (!line_length)
    line_length = CONFIG_CHECKOUT_LINE_LEN;

  config_pstdout_fprintf (pstate,
			  fp,
			  "Section %s\n",
			  section->section_name);


  kv = section->keyvalues;
  while (kv)
    {
      int key_len = 0;

      if (kv->key->flags & CONFIG_UNDEFINED)
        {
          if (config_section_update_keyvalue_output (pstate,
                                                     kv,
                                                     "Undefined") < 0)
            this_ret = CONFIG_ERR_FATAL_ERROR;
          else
            this_ret = CONFIG_ERR_SUCCESS;
        }
      else
        {
          if ((this_ret = kv->key->checkout (section->section_name,
                                             kv,
                                             arg)) == CONFIG_ERR_FATAL_ERROR)
            goto cleanup;
        }

      if (CONFIG_IS_NON_FATAL_ERROR (this_ret))
        {
          if (cmd_args->verbose_count > 1)
            {
              if (this_ret == CONFIG_ERR_NON_FATAL_ERROR_NOT_SUPPORTED)
                config_pstdout_fprintf (pstate,
					fp,
					"\t## Unable to checkout %s:%s : Not Supported\n",
					section->section_name,
					kv->key->key_name);
              else
                config_pstdout_fprintf (pstate,
					fp,
					"\t## Unable to checkout %s:%s\n",
					section->section_name,
					kv->key->key_name);
            }
          ret = CONFIG_ERR_NON_FATAL_ERROR;
        }
      else
        {
          char obuf[CONFIG_OUTPUT_BUFLEN];

          assert (kv->value_output);

          if (strchr (kv->key->description, '\n'))
            {
              char *cptr;

              cptr = kv->key->description;
              config_pstdout_fprintf (pstate,
				      fp,
				      "\t## ");
              while (*cptr)
                {
                  if (*cptr == '\n')
                    config_pstdout_fprintf (pstate,
					    fp,
					    "\n\t## ");
                  else
                    config_pstdout_fprintf (pstate,
					    fp,
					    "%c",
					    *cptr);
                  cptr++;
                }
              config_pstdout_fprintf (pstate,
				      fp,
				      "\n");
            }
          else
            config_pstdout_fprintf (pstate,
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
          if (kv->key->flags & CONFIG_CHECKOUT_KEY_COMMENTED_OUT
              || kv->key->flags & CONFIG_UNDEFINED
              || (kv->key->flags & CONFIG_CHECKOUT_KEY_COMMENTED_OUT_IF_VALUE_EMPTY
                  && !strlen (kv->value_output))
              || (kv->key->flags & CONFIG_USERNAME_NOT_SET_YET
                  && !strcasecmp (kv->value_output, CONFIG_USERNAME_NOT_SET_YET_STR)))
            key_len = snprintf (obuf,
                                CONFIG_OUTPUT_BUFLEN,
                                "\t## %s",
                                kv->key->key_name);
          else
            key_len = snprintf (obuf,
                                CONFIG_OUTPUT_BUFLEN,
                                "\t%s",
                                kv->key->key_name);

          config_pstdout_fprintf (pstate,
				  fp,
				  "%s",
				  obuf);

          while (key_len <= line_length)
            {
              config_pstdout_fprintf (pstate,
				      fp,
				      " ");
              key_len++;
            }

          config_pstdout_fprintf (pstate,
				  fp,
				  " %s\n",
				  kv->value_output);
        }

      kv = kv->next;
    }

  config_pstdout_fprintf (pstate,
			  fp,
			  "EndSection\n");
  rv = ret;
 cleanup:
  return (rv);
}

config_err_t
config_checkout (pstdout_state_t pstate,
                 struct config_section *sections,
                 struct config_arguments *cmd_args,
                 int all_keys_if_none_specified,
                 FILE *fp,
                 unsigned int line_length,
                 void *arg)
{
  struct config_section *s;
  config_err_t rv = CONFIG_ERR_SUCCESS;
  config_err_t ret;

  assert (sections);
  assert (cmd_args);
  assert (fp);

  s = sections;
  while (s)
    {
      if (!(s->flags & CONFIG_DO_NOT_CHECKOUT)
	  || !all_keys_if_none_specified)
	{
	  if ((ret = config_checkout_section (pstate,
					      s,
					      cmd_args,
					      all_keys_if_none_specified,
					      fp,
					      line_length,
					      arg)) != CONFIG_ERR_SUCCESS)
	    {
	      if (ret == CONFIG_ERR_FATAL_ERROR)
		{
		  rv = CONFIG_ERR_FATAL_ERROR;
		  break;
		}
	      rv = ret;
	    }
	}
      s = s->next;
    }

  return (rv);
}
