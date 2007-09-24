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

static config_diff_t
config_diff_keypairs (struct config_section *sections,
                      struct config_arguments *cmd_args,
                      void *arg)
{
  struct config_keypair *kp;
  config_diff_t rv = CONFIG_DIFF_FATAL_ERROR;
  config_diff_t ret = CONFIG_DIFF_SAME;

  kp = cmd_args->keypairs;
  while (kp)
    {
      config_diff_t this_ret;

      if ((this_ret = config_section_diff_value (sections,
                                                     kp->section_name, 
                                                     kp->key_name,
                                                     kp->value_input,
                                                     arg)) == CONFIG_DIFF_FATAL_ERROR)
        goto cleanup;

      if (this_ret == CONFIG_DIFF_NON_FATAL_ERROR)
        ret = CONFIG_DIFF_NON_FATAL_ERROR;

      kp = kp->next;
    }

  rv = ret;
 cleanup:
  return rv;
}

static config_diff_t
config_diff_file (struct config_section *sections,
                  struct config_arguments *cmd_args,
                  void *arg)
{
  FILE *fp;
  int file_opened = 0;
  config_diff_t rv = CONFIG_DIFF_FATAL_ERROR;
  config_diff_t ret = CONFIG_DIFF_SAME;
  config_diff_t this_ret;

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
  if ((this_ret = config_parse (sections, cmd_args, fp)) == CONFIG_DIFF_FATAL_ERROR)
    goto cleanup;

  if (this_ret == CONFIG_DIFF_NON_FATAL_ERROR)
    ret = CONFIG_DIFF_NON_FATAL_ERROR;

  if (ret == CONFIG_DIFF_SAME) 
    {
      /* 2nd pass if 1st pass was successful */
      struct config_section *section = sections;
      while (section) 
        {
          struct config_keyvalue *kv = section->keyvalues;
          while (kv) 
            {
              if (kv->value) 
                {
                  if ((this_ret = kv->diff (section, kv, arg)) == CONFIG_DIFF_FATAL_ERROR)
                    goto cleanup;

                  if (this_ret == CONFIG_DIFF_NON_FATAL_ERROR)
                    ret = CONFIG_DIFF_NON_FATAL_ERROR;
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
  config_diff_t ret;

  if (cmd_args->keypairs)
    ret = config_diff_keypairs (sections, cmd_args, arg);
  else
    ret = config_diff_file (sections, cmd_args, arg);

  if (ret == CONFIG_DIFF_SAME)
    return CONFIG_ERR_SUCCESS;
  if (ret == CONFIG_DIFF_DIFFERENT || ret == CONFIG_DIFF_NON_FATAL_ERROR)
    return CONFIG_ERR_NON_FATAL_ERROR;
  return CONFIG_ERR_FATAL_ERROR;
}

void 
report_diff (const char *section, 
	     const char *key, 
	     const char *input_value,
	     const char *actual_value)
{
  printf ("%s:%s - input=`%s':actual=`%s'\n",
	  section, key, input_value, actual_value);
}

