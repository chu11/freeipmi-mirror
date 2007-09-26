#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */

#include "config-diff.h"
#include "config-parse.h"
#include "config-section.h"

static void 
_report_diff (const char *section, 
              const char *key, 
              const char *input_value,
              const char *actual_value)
{
  printf ("%s:%s - input=`%s':actual=`%s'\n",
	  section, key, input_value, actual_value);
}

static config_err_t
config_diff_keypairs (struct config_section *sections,
                      struct config_arguments *cmd_args,
                      void *arg)
{
  struct config_keypair *kp;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret = CONFIG_ERR_SUCCESS;

  kp = cmd_args->keypairs;
  while (kp)
    {
      struct config_section *section;
      struct config_keyvalue *kv;
      config_err_t this_ret;

      /* XXX - use common func later */
      section = sections;
      while (section)
        {
          if (same (kp->section_name, section->section_name))
            break;
          section = section->next;
        }
      
      /* XXX check earlier */
      if (!section)
        {
          fprintf (stderr, "Unknown section `%s'\n", kp->section_name);
          rv = CONFIG_ERR_NON_FATAL_ERROR;
          goto cleanup;
        }

      kv = section->keyvalues;
      while (kv)
        {
          if (same (kp->key_name, kv->key_name))
            break;
          kv = kv->next;
        }

      if (!kv)
        {
          fprintf (stderr, "Unknown key `%s' in section `%s'\n",
                   kp->key_name, kp->section_name);
          rv = CONFIG_ERR_NON_FATAL_ERROR;
          goto cleanup;
        }

      if ((this_ret = kv->checkout (section->section_name,
                                    kv,
                                    arg)) == CONFIG_ERR_FATAL_ERROR)
        goto cleanup;

      if (this_ret == CONFIG_ERR_SUCCESS)
        {
          _report_diff (section->section_name,
                        kv->key_name,
                        kv->value_input,
                        kv->value_output);
        }
      else
        ret = this_ret;

      kp = kp->next;
    }

  rv = ret;
 cleanup:
  return rv;
}

static config_err_t
config_diff_file (struct config_section *sections,
                  struct config_arguments *cmd_args,
                  void *arg)
{
  FILE *fp;
  int file_opened = 0;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret = CONFIG_ERR_SUCCESS;
  config_err_t this_ret;

  if (cmd_args->filename && strcmp (cmd_args->filename, "-"))
    {
      if (!(fp = fopen (cmd_args->filename, "r")))
        {
          perror("fopen");
          goto cleanup;
        }
      file_opened++;
    }
  else
    fp = stdin;

  /* 1st pass */
  if ((this_ret = config_parse (sections, cmd_args, fp)) == CONFIG_ERR_FATAL_ERROR)
    goto cleanup;

  if (this_ret == CONFIG_ERR_NON_FATAL_ERROR)
    ret = CONFIG_ERR_NON_FATAL_ERROR;

  if (ret == CONFIG_ERR_SUCCESS) 
    {
      /* 2nd pass if 1st pass was successful */
      struct config_section *section = sections;
      while (section) 
        {
          struct config_keyvalue *kv = section->keyvalues;
          while (kv) 
            {
              /* XXX remove later */
              if (kv->value_input)
                {
                  if ((this_ret = kv->checkout (section->section_name,
                                                kv,
                                                arg)) == CONFIG_ERR_FATAL_ERROR)
                    goto cleanup;
                  
                  if (this_ret == CONFIG_ERR_SUCCESS)
                    {
                      if (!same (kv->value_input, kv->value_output))
                        _report_diff (section->section_name,
                                      kv->key_name,
                                      kv->value_input,
                                      kv->value_output);
                    }
                  else
                    ret = this_ret;
                }
              kv = kv->next;
            }
          section = section->next;
        }
    }

  rv = ret;
 cleanup:
  if (file_opened)
    fclose(fp);
  return rv;
}

config_err_t
config_diff (struct config_section *sections,
             struct config_arguments *cmd_args,
             void *arg)
{
  config_err_t ret;

  if (cmd_args->keypairs)
    ret = config_diff_keypairs (sections, cmd_args, arg);
  else
    ret = config_diff_file (sections, cmd_args, arg);

  return ret;
}


