#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <assert.h>

#include "bmc-config.h"
#include "bmc-config-channel-common.h"

#include "config-common.h"
#include "config-section.h"

struct config_section *
bmc_config_lan_channel_section_get (bmc_config_state_data_t *state_data)
{
  struct config_section *lan_channel_section = NULL;
  char *section_comment = 
    "In the Lan_Channel section, general IPMI over LAN can be enabled for "
    "disabled.  In the below, \"Volatile\" configurations are immediately "
    "configured onto the BMC and will have immediate effect on the system.  "
    "\"Non_Volatile\" configurations are only available after the next "
    "system reset.  Generally, both the \"Volatile\" and \"Non_Volatile\" "
    "equivalent fields should be configured identically."
    "\n"
    "To enable IPMI over LAN, typically \"Access_Mode\" "
    "should be set to \"Always_Available\".  "
    "\"Channel_Privilege_Limit\" should be set to the highest privilege "
    "level any username was configured with.  Typically, this "
    "is set to \"Administrator\"."
    "\n"
    "\"User_Level_Auth\" and \"Per_Message_Auth\" are typically set to "
    "\"Yes\" for additional security.";

  if (!(lan_channel_section = config_section_create ("Lan_Channel",
                                                     "Lan_Channel",
                                                     section_comment,
                                                     0,
                                                     channel_checkout,
                                                     channel_commit)))
    goto cleanup;

  if (channel_section_get(state_data, lan_channel_section) < 0)
    goto cleanup;

  return lan_channel_section;

 cleanup:
  if (lan_channel_section)
    config_section_destroy(lan_channel_section);
  return NULL;
}

