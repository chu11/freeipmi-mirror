#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */

#include "ipmi-pef.h"
#include "ipmi-pef-common.h"
#include "ipmi-pef-diff.h"
#include "ipmi-pef-map.h"
#include "ipmi-pef-sections.h"
#include "ipmi-pef-wrapper.h"

struct section *
ipmi_pef_alert_policy_table_section_get (ipmi_pef_state_data_t *state_data, int num)
{
  struct section *sect = NULL;
  char buf[64];

  if (num <= 0)
    {
      fprintf(stderr, "Invalid Num = %d\n", num);
      return NULL;
    }

  snprintf(buf, 64, "Alert_Policy_%d", num);

  if (!(sect = ipmi_pef_section_create (state_data, buf)))
    goto cleanup;

#if 0
  /* XXX come back to this */
  if (ipmi_pef_section_add_keyvalue (state_data,
                                     sect,
                                     "Alert_Policy",
                                     "Give valid string",
                                     0,
                                     alert_policy_checkout,
                                     alert_policy_commit,
                                     alert_policy_diff,
                                     alert_policy_validate) < 0) 
    goto cleanup;
#endif

  return sect;

 cleanup:
  if (sect)
    ipmi_pef_section_destroy(state_data, sect);
  return NULL;
}

