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

static config_err_t
threshold_checkout (const char *section_name,
                    struct config_keyvalue *kv,
                    void *arg)
{
#if 0
  ipmi_sensors_config_state_data_t *state_data = (ipmi_sensors_config_state_data_t *)arg;
#endif
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;

  rv = CONFIG_ERR_SUCCESS;
#if 0
 cleanup:
#endif
  return (rv);
}

static config_err_t
threshold_commit (const char *section_name,
                  const struct config_keyvalue *kv,
                  void *arg)
{
#if 0
  ipmi_sensors_config_state_data_t *state_data = (ipmi_sensors_config_state_data_t *)arg;
#endif
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;

  rv = CONFIG_ERR_SUCCESS;
#if 0
 cleanup:
#endif
  return (rv);
}

static config_validate_t
threshold_validate (const char *section_name,
                    const char *key_name,
                    const char *value)
{
  return CONFIG_VALIDATE_VALID_VALUE;
}

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
  uint8_t lower_non_critical_threshold_settable = 0;
  uint8_t lower_critical_threshold_settable = 0;
  uint8_t lower_non_recoverable_threshold_settable = 0;
  uint8_t upper_non_critical_threshold_settable = 0;
  uint8_t upper_critical_threshold_settable = 0;
  uint8_t upper_non_recoverable_threshold_settable = 0;
  uint8_t lower_non_critical_threshold_readable = 0;
  uint8_t lower_critical_threshold_readable = 0;
  uint8_t lower_non_recoverable_threshold_readable = 0;
  uint8_t upper_non_critical_threshold_readable = 0;
  uint8_t upper_critical_threshold_readable = 0;
  uint8_t upper_non_recoverable_threshold_readable = 0;
  unsigned int flags;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t sensor_type, sensor_unit;
  char *desc;

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

  if (sdr_cache_get_threshold_settable (NULL,
                                        sdr_record,
                                        sdr_record_len,
                                        &lower_non_critical_threshold_readable,
                                        &lower_critical_threshold_readable,
                                        &lower_non_recoverable_threshold_readable,
                                        &upper_non_critical_threshold_readable,
                                        &upper_critical_threshold_readable,
                                        &upper_non_recoverable_threshold_readable) < 0)
    goto cleanup;

  if (sdr_cache_get_threshold_settable (NULL,
                                        sdr_record,
                                        sdr_record_len,
                                        &lower_non_critical_threshold_settable,
                                        &lower_critical_threshold_settable,
                                        &lower_non_recoverable_threshold_settable,
                                        &upper_non_critical_threshold_settable,
                                        &upper_critical_threshold_settable,
                                        &upper_non_recoverable_threshold_settable) < 0)
    goto cleanup;

  if (sdr_cache_get_sensor_type (NULL,
                                 sdr_record,
                                 sdr_record_len,
                                 &sensor_type) < 0)
    goto cleanup;

  if (sdr_cache_get_sensor_unit (NULL,
                                 sdr_record,
                                 sdr_record_len,
                                 &sensor_unit) < 0)
    goto cleanup;

  /* achu: I'm not handling every possibility, will update as needed */
  if (sensor_type == IPMI_SENSOR_TYPE_TEMPERATURE)
    {
      if (sensor_unit == IPMI_SENSOR_UNIT_DEGREES_C)
        desc = "Give a valid temperature (in Celsius)";
      else if (sensor_unit == IPMI_SENSOR_UNIT_DEGREES_F)
        desc = "Give a valid temperature (in Fahrenheit)";
      else
        desc = "Unknown units";
    }
  else if (sensor_type == IPMI_SENSOR_TYPE_VOLTAGE)
    {
      if (sensor_unit == IPMI_SENSOR_UNIT_VOLTS)
        desc = "Give a valid voltage";
      else
        desc = "Unknown units";
    }
  else if (sensor_type == IPMI_SENSOR_TYPE_FAN)
    {
      if (sensor_unit == IPMI_SENSOR_UNIT_RPM)
        desc = "Give a valid RPM";
      else
        desc = "Unknown units";
    }

  /* If a threshold is not-readable, it isn't up for consideration, so
   * don't "register" it.
   */

  if (lower_non_critical_threshold_readable)
    {
      flags = 0;

      if (!lower_non_critical_threshold_settable)
        {
          flags |= CONFIG_CHECKOUT_KEY_COMMENTED_OUT;
          flags |= CONFIG_READABLE_ONLY;
        }

      if (config_section_add_key (section,
                                  "Lower_Non_Critical_Threshold",
                                  desc,
                                  flags,
                                  threshold_checkout,
                                  threshold_commit,
                                  threshold_validate) < 0)
        goto cleanup;
    }

  if (lower_critical_threshold_readable)
    {
      flags = 0;

      if (!lower_critical_threshold_settable)
        {
          flags |= CONFIG_CHECKOUT_KEY_COMMENTED_OUT;
          flags |= CONFIG_READABLE_ONLY;
        }

      if (config_section_add_key (section,
                                  "Lower_Critical_Threshold",
                                  desc,
                                  flags,
                                  threshold_checkout,
                                  threshold_commit,
                                  threshold_validate) < 0)
        goto cleanup;
    }

  if (lower_non_recoverable_threshold_readable)
    {
      flags = 0;

      if (!lower_non_recoverable_threshold_settable)
        {
          flags |= CONFIG_CHECKOUT_KEY_COMMENTED_OUT;
          flags |= CONFIG_READABLE_ONLY;
        }

      if (config_section_add_key (section,
                                  "Lower_Non_Recoverable_Threshold",
                                  desc,
                                  flags,
                                  threshold_checkout,
                                  threshold_commit,
                                  threshold_validate) < 0)
        goto cleanup;
    }

  *section_ptr = section;
  return CONFIG_ERR_SUCCESS;

 cleanup:
  if (section)
    config_section_destroy(section);
  return rv;
}
