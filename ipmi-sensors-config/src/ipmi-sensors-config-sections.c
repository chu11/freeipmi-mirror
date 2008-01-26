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

#include "ipmi-sensors-config.h"
#include "ipmi-sensors-config-sections.h"

struct config_section *
ipmi_sensors_config_sections_create (ipmi_sensors_config_state_data_t *state_data)
{
  struct config_section *sections = NULL;
  struct config_section *section = NULL;

#if 0
  if (!(section = ipmi_sensors_config_community_string_section_get (state_data)))
    goto cleanup;
#endif
  if (config_section_append (&sections, section) < 0)
    goto cleanup;

  return sections;

 cleanup:
  config_sections_destroy(sections);
  return NULL;
}
