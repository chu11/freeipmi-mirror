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
bmc_config_serial_channel_section_get (bmc_config_state_data_t *state_data)
{
  struct config_section *serial_channel_section = NULL;
  char *section_comment = 
    "In the Serial_Channel section, IPMI over Serial communication can be "
    "enabled or disabled.  "
    "In the below, \"Volatile\" configurations are immediately "
    "configured onto the BMC and will have immediate effect on the system.  "
    "\"Non_Volatile\" configurations are only available after the next "
    "system reset.  Generally, both the \"Volatile\" and \"Non_Volatile\" "
    "equivalent fields should be configured identically."
    "\n"
    "Most users will only be interested in IPMI over LAN, therefore serial "
    "communication can be disabled.  This can be done by setting "
    "\"Access_Mode\" to \"Disabled\".";

  if (!(serial_channel_section = config_section_create ("Serial_Channel",
                                                        "Serial_Channel",
                                                        section_comment,
                                                        0,
                                                        channel_checkout,
                                                        channel_commit)))
    goto cleanup;

  if (channel_section_get(state_data, serial_channel_section) < 0)
    goto cleanup;

  return serial_channel_section;

 cleanup:
  if (serial_channel_section)
    config_section_destroy(serial_channel_section);
  return NULL;
}

