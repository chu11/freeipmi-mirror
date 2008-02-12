#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <assert.h>

#include "ipmi-sensors-config-utils.h"

config_err_t 
convert_id_string (ipmi_sensors_config_state_data_t *state_data, 
                   char *id_string)
{
  char *ptr;

  assert(state_data);
  assert(id_string);

  /* Convert stuff to underscore.  I will not convert +/-/. for now.
   * I think they are fine.
   */
  while ((ptr = strchr(id_string, ' ')))
    *ptr = '_';

  while ((ptr = strchr(id_string, '/')))
    *ptr = '_';

  return CONFIG_ERR_SUCCESS;
}
