#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */

#include "pef-config.h"
#include "pef-config-common.h"
#include "pef-config-diff.h"
#include "pef-config-parser.h"
#include "pef-config-sections.h"

static config_diff_t
config_diff_keypair (pef_config_state_data_t *state_data,
                  struct config_keypair *kp)
{
  char *keypair = NULL;
  char *section_name;
  char *key_name;
  char *value;
  config_diff_t rv = CONFIG_DIFF_FATAL_ERROR;

  if (!(keypair = strdup (kp->keypair)))
    {
      perror("strdup");
      goto cleanup;
    }

  section_name = strtok (keypair, ":");
  key_name = strtok (NULL, "=");
  value = strtok (NULL, "");

  if (!(section_name && key_name && value))
    {
      fprintf (stderr, "Invalid KEY-PAIR spec `%s'\n", kp->keypair);
      rv = CONFIG_DIFF_NON_FATAL_ERROR;
      goto cleanup;
    }

  section_name = strtok (section_name, " \t");
  key_name = strtok (key_name, " \t");
  value = strtok (value, " \t");

  rv = pef_config_section_diff_value (state_data,
                                      section_name,
                                      key_name,
                                      value);
 cleanup:
  if (keypair)
    free (keypair);
  return rv;
}

static config_diff_t
config_diff_keypairs (pef_config_state_data_t *state_data)
{
  struct config_arguments *args;
  struct config_keypair *kp;
  config_diff_t rv = CONFIG_DIFF_FATAL_ERROR;
  config_diff_t ret = CONFIG_DIFF_SAME;

  args = state_data->prog_data->args;

  kp = args->keypairs;
  while (kp)
    {
      config_diff_t this_ret;

      if ((this_ret = config_diff_keypair (state_data,
                                        kp)) == CONFIG_DIFF_FATAL_ERROR)
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
config_diff_file (pef_config_state_data_t *state_data)
{
  struct config_arguments *args;
  FILE *fp;
  int file_opened = 0;
  config_diff_t rv = CONFIG_DIFF_FATAL_ERROR;
  config_diff_t ret = CONFIG_DIFF_SAME;
  config_diff_t this_ret;

  args = state_data->prog_data->args;

  if (args->filename && strcmp (args->filename, "-"))
    {
      if (!(fp = fopen (args->filename, "r")))
        {
          perror("fopen");
          goto cleanup;
        }
      file_opened++;
    }
  else
    fp = stdin;

  /* 1st pass */
  if ((this_ret = pef_config_parser (state_data, fp)) == CONFIG_DIFF_FATAL_ERROR)
    goto cleanup;

  if (this_ret == CONFIG_DIFF_NON_FATAL_ERROR)
    ret = CONFIG_DIFF_NON_FATAL_ERROR;

  if (ret == CONFIG_DIFF_SAME) 
    {
      /* 2nd pass if 1st pass was successful */
      struct config_section *section = state_data->sections;
      while (section) 
        {
          struct config_keyvalue *kv = section->keyvalues;
          while (kv) 
            {
              if (kv->value) 
                {
                  if ((this_ret = kv->diff (state_data, section, kv)) == CONFIG_DIFF_FATAL_ERROR)
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
config_diff (pef_config_state_data_t *state_data)
{
  struct config_arguments *args;
  config_diff_t ret;

  args = state_data->prog_data->args;

  if (args->keypairs)
    ret = config_diff_keypairs (state_data);
  else
    ret = config_diff_file (state_data);

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

