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

#include "bmc-config.h"
#include "bmc-config-checkout.h"
#include "bmc-config-common.h"
#include "bmc-config-sections.h"

#include "config-comment.h"

static config_err_t
bmc_checkout_keyinput (bmc_config_state_data_t *state_data,
                       struct config_keyinput *ki)
{
  struct section *sect;
  struct keyvalue *kv;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret = CONFIG_ERR_SUCCESS;

  sect = state_data->sections;
  while (sect) 
    {
      if (same (ki->section_name, sect->section_name)) 
        break;
      sect = sect->next;
    }
  
  if (!sect) 
    {
      fprintf (stderr, "Unknown section `%s'\n", ki->section_name);
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  kv = sect->keyvalues;

  while (kv) 
    {
      if (same (ki->key_name, kv->key))
        break;
      kv = kv->next;
    }

  if (!kv) 
    {
      fprintf (stderr, "Unknown key `%s' in section `%s'\n",
               ki->key_name, ki->section_name);
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  if ((ret = kv->checkout (state_data, sect, kv)) == CONFIG_ERR_FATAL_ERROR)
    goto cleanup;

  if (ret == CONFIG_ERR_SUCCESS) 
    printf ("%s:%s=%s\n", ki->section_name, ki->key_name, kv->value);
  else
    printf ("Error fetching value for %s in %s\n", ki->key_name, ki->section_name);

  rv = ret;
 cleanup:
  return rv;
}

static config_err_t
bmc_checkout_keyinputs (bmc_config_state_data_t *state_data)
{
  struct bmc_config_arguments *args;
  struct config_keyinput *ki;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret = CONFIG_ERR_SUCCESS;

  args = state_data->prog_data->args;

  ki = args->keyinputs;
  while (ki)
    {
      config_err_t this_ret;

      if ((this_ret = bmc_checkout_keyinput(state_data,
                                            ki)) == CONFIG_ERR_FATAL_ERROR)
        goto cleanup;
      
      if (this_ret == CONFIG_ERR_NON_FATAL_ERROR)
        ret = CONFIG_ERR_NON_FATAL_ERROR;

      ki = ki->next;
    }

  rv = ret;
 cleanup:
  return rv;
}

static config_err_t
bmc_checkout_section_common (bmc_config_state_data_t *state_data,
                             struct section *sect, 
                             FILE *fp)
{
  struct bmc_config_arguments *args;
  struct keyvalue *kv;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret = CONFIG_ERR_SUCCESS;
  config_err_t this_ret;

  if (sect->flags & CONFIG_DO_NOT_CHECKOUT)
    return ret;

  if (sect->section_comment_section_name
      && sect->section_comment)
    {
      if (config_section_comments(sect->section_comment_section_name,
                                  sect->section_comment,
                                  fp) < 0)
        {
          if (args->verbose)
            fprintf (fp, "\t## FATAL: Comment output error\n");
          ret = CONFIG_ERR_NON_FATAL_ERROR;
        }
    }

  fprintf (fp, "Section %s\n", sect->section_name);

  args = state_data->prog_data->args;
  kv = sect->keyvalues;
  
  while (kv) 
    { 
      /* achu: Certain keys should be "hidden" and not be checked out.
       * They only linger for backwards compatability to FreeIPMI's
       * 0.2.0 bmc-config, which have several options with typoed
       * names.
       */
      if (kv->flags & CONFIG_DO_NOT_CHECKOUT)
        {
          kv = kv->next;
          continue;
        }

      if ((this_ret = kv->checkout (state_data, 
                                    sect,
                                    kv)) == CONFIG_ERR_FATAL_ERROR)
        goto cleanup;
      
      if (this_ret == CONFIG_ERR_NON_FATAL_ERROR)
        {
          if (args->verbose)
            fprintf (fp, "\t## FATAL: Unable to checkout %s:%s\n",
                     sect->section_name,
                     kv->key);
          ret = CONFIG_ERR_NON_FATAL_ERROR;
        } 
      else 
        {
          int key_len = 0;
                  
          fprintf (fp, "\t## %s\n", kv->desc);
                  
          /* beauty comes at a cost */
          
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
          if (kv->flags & CONFIG_CHECKOUT_KEY_COMMENTED_OUT)
            key_len = fprintf(fp, "\t## %s", kv->key);
          else if (kv->flags & CONFIG_CHECKOUT_KEY_COMMENTED_OUT_IF_VALUE_EMPTY)
            {
              if (kv->value && strlen(kv->value))
                key_len = fprintf (fp, "\t%s", kv->key);
              else
                key_len = fprintf(fp, "\t## %s", kv->key);
            }
          else
            key_len = fprintf (fp, "\t%s", kv->key);
          
          while (key_len <= 45) 
            {
              fprintf (fp, " ");
              key_len++;
            }
          
          fprintf (fp, "%s\n", kv->value);
        }
      
      kv = kv->next;
    }

  fprintf (fp, "EndSection\n");
  return ret;

 cleanup:
  return rv;
}

static config_err_t
bmc_checkout_section (bmc_config_state_data_t *state_data)
{
  struct bmc_config_arguments *args;
  struct config_section_str *sstr;
  FILE *fp;
  int file_opened = 0;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret = CONFIG_ERR_SUCCESS;

  args = state_data->prog_data->args;

  if (args->filename && strcmp (args->filename, "-"))
    {
      if (!(fp = fopen (args->filename, "w")))
        {
          perror("fopen");
          goto cleanup;
        }
      file_opened++;
    }
  else
    fp = stdout;

  sstr = args->section_strs;
  while (sstr)
    {
      struct section *sect = state_data->sections;
      int found = 0;
      
      while (sect) 
        {
          if (!strcasecmp(sect->section_name, sstr->section_name))
            {
              config_err_t this_ret;
              
              found++;

              if ((this_ret = bmc_checkout_section_common (state_data, 
                                                           sect, 
                                                           fp)) == CONFIG_ERR_FATAL_ERROR)
                goto cleanup;
              
              if (this_ret == CONFIG_ERR_NON_FATAL_ERROR)
                ret = CONFIG_ERR_NON_FATAL_ERROR;
              
              break;
            }

          sect = sect->next;
        }

      if (!found)
        {
          fprintf(stderr, "Invalid section `%s'\n", sstr->section_name);
          ret = 0;
        } 

      sstr = sstr->next;
    }
  
  rv = ret;
 cleanup:
  if (file_opened)
    fclose(fp);
  return rv;
}

static config_err_t
bmc_checkout_file (bmc_config_state_data_t *state_data)
{
  struct bmc_config_arguments *args;
  struct section *sect;
  FILE *fp;
  int file_opened = 0;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret = CONFIG_ERR_SUCCESS;

  args = state_data->prog_data->args;

  if (args->filename && strcmp (args->filename, "-"))
    {
      if (!(fp = fopen (args->filename, "w")))
        {
          perror("fopen");
          goto cleanup;
        }
      file_opened++;
    }
  else
    fp = stdout;

  sect = state_data->sections;
  while (sect) 
    {
      config_err_t this_ret;
      
      if ((this_ret = bmc_checkout_section_common (state_data,
                                                   sect,
                                                   fp)) == CONFIG_ERR_FATAL_ERROR)
        goto cleanup;

      if (this_ret == CONFIG_ERR_NON_FATAL_ERROR)
        ret = CONFIG_ERR_NON_FATAL_ERROR;

      if (args->verbose)
        fprintf (stderr, "Completed checkout of Section: %s\n",
                 sect->section_name);

      sect = sect->next;
    }

  rv = ret;
 cleanup:
  if (file_opened)
    fclose(fp);
  return rv;
}

config_err_t
bmc_checkout (bmc_config_state_data_t *state_data)
{
  struct bmc_config_arguments *args;
  config_err_t ret;

  args = state_data->prog_data->args;

  if (args->keyinputs) 
    ret = bmc_checkout_keyinputs (state_data);
  else if (args->section_strs)
    ret = bmc_checkout_section (state_data);
  else
    ret = bmc_checkout_file (state_data);

  return ret;
}
