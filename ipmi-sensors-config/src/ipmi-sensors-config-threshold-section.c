#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <assert.h>

#include "ipmi-sensors-config.h"
#if 0
#include "ipmi-sensors-config-map.h"
#include "ipmi-sensors-config-validate.h"
#endif
#include "ipmi-sensors-config-utils.h"

#include "tool-sdr-cache-common.h"

config_err_t
ipmi_sensors_config_threshold_section (ipmi_sensors_config_state_data_t *state_data,
                                       uint8_t *sdr_record,
                                       unsigned int sdr_record_len,
                                       struct config_section **section_ptr)
{
  struct config_section *section = NULL;
  char section_name[CONFIG_MAX_SECTION_NAME_LEN];
  char id_string[IPMI_SDR_CACHE_MAX_ID_STRING + 1];
  uint16_t record_id;
  uint8_t lower_non_critical_threshold = 0;
  uint8_t lower_critical_threshold = 0;
  uint8_t lower_non_recoverable_threshold = 0;
  uint8_t upper_non_critical_threshold = 0;
  uint8_t upper_critical_threshold = 0;
  uint8_t upper_non_recoverable_threshold = 0;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;

  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);
  assert(section_ptr);

  if (sdr_cache_get_record_id_and_type (NULL,
                                        sdr_record,
                                        sdr_record_len,
                                        &record_id,
                                        NULL) < 0)
    goto cleanup;

  memset(id_string, '\0', IPMI_SDR_CACHE_MAX_ID_STRING + 1);

  if (sdr_cache_get_id_string (NULL,
                               sdr_record,
                               sdr_record_len,
                               id_string,
                               IPMI_SDR_CACHE_MAX_ID_STRING) < 0)
    goto cleanup;

  if ((ret = convert_id_string (state_data, id_string)) != CONFIG_ERR_SUCCESS)
    {
      if (state_data->prog_data->args->config_args.common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr, 
                "convert_id_string: %s\n",
                strerror(errno));
      rv = ret;
      goto cleanup;
    }

  /* We will name sections by record_id then name, since id_strings
   * could be identical.
   */
  /* achu: I know CONFIG_MAX_SECTION_NAME_LEN is much larger than
   * IPMI_SDR_CACHE_MAX_ID_STRING.  We should probably do some math
   * instead of just using macros flat out though.
   */
  if (strlen(id_string) > 0)
    snprintf(section_name,
             CONFIG_MAX_SECTION_NAME_LEN,
             "%u_%s",
             record_id,
             id_string);
  else
    /* I guess its conceivable the sensor won't have a name, so we
     * make one up.
     */
    snprintf(section_name,
             CONFIG_MAX_SECTION_NAME_LEN,
             "%u_%s",
             record_id,
             "Unknown_Sensor_Name");

  if (!(section = config_section_create (section_name,
                                         NULL,
                                         NULL,
                                         0)))
    goto cleanup;

  

#if 0

  if (config_section_add_key (section,
                              "Lower_Non_Critical_Threshold",
                              "",
                              0,
                              ,
                              ,
                              ) < 0)
    goto cleanup;

  if (config_section_add_key (section,
                              "Lower_Critical_Threshold",
                              "",
                              0,
                              ,
                              ,
                              ) < 0)
    goto cleanup;

  if (config_section_add_key (section,
                              "Lower_Non_Recoverable_Threshold",
                              "",
                              0,
                              ,
                              ,
                              ) < 0)
    goto cleanup;

  if (config_section_add_key (section,
                              "Upper_Non_Critical_Threshold",
                              "",
                              0,
                              ,
                              ,
                              ) < 0)
    goto cleanup;

  if (config_section_add_key (section,
                              "Upper_Critical_Threshold",
                              "",
                              0,
                              ,
                              ,
                              ) < 0)
    goto cleanup;

  if (config_section_add_key (section,
                              "Upper_Non_Recoverable_Threshold",
                              "",
                              0,
                              ,
                              ,
                              ) < 0)
    goto cleanup;

#endif

  *section_ptr = section;
  return CONFIG_ERR_SUCCESS;

 cleanup:
  if (section)
    config_section_destroy(section);
  return rv;
}
