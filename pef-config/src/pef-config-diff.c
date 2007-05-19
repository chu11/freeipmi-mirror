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

static pef_diff_t
pef_diff_file (pef_config_state_data_t *state_data)
{
  struct pef_config_arguments *args;
  FILE *fp;
  int file_opened = 0;
  pef_diff_t rv = PEF_DIFF_FATAL_ERROR;
  pef_diff_t ret = PEF_DIFF_SAME;
  pef_diff_t this_ret;

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
  if ((this_ret = pef_config_parser (state_data, fp)) == PEF_DIFF_FATAL_ERROR)
    goto cleanup;

  if (this_ret == PEF_DIFF_NON_FATAL_ERROR)
    ret = PEF_DIFF_NON_FATAL_ERROR;

  if (ret == PEF_DIFF_SAME) 
    {
      /* 2nd pass if 1st pass was successful */
      struct section *sect = state_data->sections;
      while (sect) 
        {
          struct keyvalue *kv = sect->keyvalues;
          while (kv) 
            {
              if (kv->value) 
                {
                  if ((this_ret = kv->diff (state_data, sect, kv)) == PEF_DIFF_FATAL_ERROR)
                    goto cleanup;

                  if (this_ret == PEF_DIFF_NON_FATAL_ERROR)
                    ret = PEF_DIFF_NON_FATAL_ERROR;
                }
              kv = kv->next;
            }
          sect = sect->next;
        }
    }

  rv = ret;
 cleanup:
  if (file_opened)
    fclose(fp);
  return rv;
}

pef_err_t
pef_diff (pef_config_state_data_t *state_data)
{
  struct pef_config_arguments *args;
  pef_diff_t ret;

  args = state_data->prog_data->args;

  ret = pef_diff_file (state_data);

  if (ret == PEF_DIFF_SAME)
    return PEF_ERR_SUCCESS;
  if (ret == PEF_DIFF_DIFFERENT || ret == PEF_DIFF_NON_FATAL_ERROR)
    return PEF_ERR_NON_FATAL_ERROR;
  return PEF_ERR_FATAL_ERROR;
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

