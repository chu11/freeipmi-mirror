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

#include "fiid-wrappers.h"

#include "ipmi-pef.h"
#include "ipmi-pef-commit.h"
#include "ipmi-pef-wrapper.h"

int 
commit_pef_community_string (ipmi_pef_state_data_t *state_data, FILE *fp)
{
  uint8_t community_string[IPMI_MAX_COMMUNITY_STRING_LENGTH+1] = { 0, };
  int rv = -1;

  if (get_community_string (state_data,
			    fp,
			    community_string,
			    IPMI_MAX_COMMUNITY_STRING_LENGTH+1) < 0)
    {
      if (state_data->prog_data->args->verbose_wanted)
	fprintf (fp, "## FATAL: Unable to set community string\n");
      goto cleanup;
    }
  
  if (set_bmc_community_string (state_data, 
				community_string) < 0)
    {
      if (state_data->prog_data->args->verbose_wanted)
	fprintf (fp, "## FATAL: Unable to set community string\n");
      goto cleanup;
    }

  rv = 0;
 cleanup:
  return rv;
}

int 
commit_pef_lan_alert_destination (ipmi_pef_state_data_t *state_data, FILE *fp)
{
  lan_alert_destination_t *lad_list = NULL;
  int count = 0;
  int i = 0;
  int rv = 0;
  
  get_lan_alert_destination_list (fp, &lad_list, &count);
  
  for (i = 0; i < count; i++)
    {
      if (set_lan_alert_destination (state_data, &lad_list[i]) != 0)
	{
          if (state_data->prog_data->args->verbose_wanted)
            fprintf (fp, "## FATAL: Unable to set LAN Alert Destination #%d\n", 
                     lad_list[i].destination_selector);
	  rv = -1;
	  continue;
	}
    }
  
  free(lad_list);
  return rv;
}

int 
commit_pef_alert_policy_table (ipmi_pef_state_data_t *state_data, FILE *fp)
{
  pef_alert_policy_table_t *apt_list = NULL;
  int count = 0;
  int i = 0;
  int rv = 0;
  
  get_alert_policy_table_list (fp, &apt_list, &count);
  
  for (i = 0; i < count; i++)
    {
      if (set_alert_policy_table (state_data, &apt_list[i]) != 0)
	{
          if (state_data->prog_data->args->verbose_wanted)
            fprintf (fp, "## FATAL: Unable to set alert policy table #%d\n", 
                     apt_list[i].alert_policy_number);
	  rv = -1;
	  continue;
	}
    }
  
  free(apt_list);
  return rv;
}

int 
commit_pef_event_filter_table (ipmi_pef_state_data_t *state_data, FILE *fp)
{
  pef_event_filter_table_t *eft_list = NULL;
  int count = 0;
  int i = 0;
  int rv = 0;
  
  get_event_filter_table_list (fp, &eft_list, &count);
  
  for (i = 0; i < count; i++)
    {
      if (set_event_filter_table (state_data, &eft_list[i]) != 0)
	{
          if (state_data->prog_data->args->verbose_wanted)
            fprintf (fp, "## FATAL: Unable to set event filter table #%d\n", 
                     eft_list[i].filter_number);
	  rv = -1;
	  continue;
	}
    }
  
  free(eft_list);
  return rv;
}

pef_err_t
pef_commit (ipmi_pef_state_data_t *state_data)
{
  FILE *fp = NULL;
  int file_opened = 0;
  pef_err_t rv = PEF_ERR_FATAL_ERROR;
  struct ipmi_pef_arguments *args;

  args = state_data->prog_data->args;

  if (args->filename && strcmp (args->filename, "-"))
    {
      if ((fp = fopen (args->filename, "r")) == NULL)
        {
          fprintf (stderr, "Unable to open file [%s] for reading: %s\n", 
                   args->filename,
                   strerror(errno));
          goto cleanup;
        }
      file_opened++;
    }
  else
    fp = stdin;
  
  /* XXX come back to this later when the sectional stuff works */
  
  if (args->community_string_wanted)
    {
      if (commit_pef_community_string (state_data, fp) < 0)
        goto cleanup;
    }
  else if (args->alert_destinations_wanted)
    {
      if (commit_pef_lan_alert_destination (state_data, fp) < 0)
        goto cleanup;
    }
  else if (args->alert_policy_table_wanted)
    {
      if (commit_pef_alert_policy_table (state_data, fp) < 0)
        goto cleanup;
    }
  else if (args->event_filter_table_wanted)
    {
      if (commit_pef_event_filter_table (state_data, fp) < 0)
        goto cleanup;
    }
  
  rv = PEF_ERR_SUCCESS;
 cleanup:
  if (file_opened)
    fclose (fp);
  return rv;
}
