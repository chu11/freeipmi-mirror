/*
 * Copyright (C) 2003-2012 FreeIPMI Core Team
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */

#ifndef IPMI_SDR_PARSE_H
#define IPMI_SDR_PARSE_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>

#define IPMI_SDR_PARSE_ERR_SUCCESS                                 0
#define IPMI_SDR_PARSE_ERR_CONTEXT_NULL                            1
#define IPMI_SDR_PARSE_ERR_CONTEXT_INVALID                         2
#define IPMI_SDR_PARSE_ERR_PARAMETERS                              3
#define IPMI_SDR_PARSE_ERR_OUT_OF_MEMORY                           4
#define IPMI_SDR_PARSE_ERR_INVALID_SDR_RECORD                      5
#define IPMI_SDR_PARSE_ERR_INCOMPLETE_SDR_RECORD                   6
#define IPMI_SDR_PARSE_ERR_CANNOT_PARSE_OR_CALCULATE               7
#define IPMI_SDR_PARSE_ERR_SYSTEM_ERROR                            8
#define IPMI_SDR_PARSE_ERR_INTERNAL_ERROR                          9
#define IPMI_SDR_PARSE_ERR_ERRNUMRANGE                            10

#define IPMI_SDR_PARSE_FLAGS_DEFAULT                              0x0000

typedef struct ipmi_sdr_parse_ctx *ipmi_sdr_parse_ctx_t;

/* SDR Parse Context Functions */
ipmi_sdr_parse_ctx_t ipmi_sdr_parse_ctx_create (void);
void ipmi_sdr_parse_ctx_destroy (ipmi_sdr_parse_ctx_t ctx);
int ipmi_sdr_parse_ctx_errnum (ipmi_sdr_parse_ctx_t ctx);
char * ipmi_sdr_parse_ctx_strerror (int errnum);
char * ipmi_sdr_parse_ctx_errormsg (ipmi_sdr_parse_ctx_t ctx);

/* SDR Parse flag functions */
int ipmi_sdr_parse_ctx_get_flags (ipmi_sdr_parse_ctx_t ctx, unsigned int *flags);
int ipmi_sdr_parse_ctx_set_flags (ipmi_sdr_parse_ctx_t ctx, unsigned int flags);

/* For all SDR records */
int ipmi_sdr_parse_record_id_and_type (ipmi_sdr_parse_ctx_t ctx,
                                       const void *sdr_record,
                                       unsigned int sdr_record_len,
                                       uint16_t *record_id,
                                       uint8_t *record_type);

/* For Full, Compact, Event SDR records */
int ipmi_sdr_parse_sensor_owner_id (ipmi_sdr_parse_ctx_t ctx,
                                    const void *sdr_record,
                                    unsigned int sdr_record_len,
                                    uint8_t *sensor_owner_id_type,
                                    uint8_t *sensor_owner_id);

/* For Full, Compact, Event SDR records */
int ipmi_sdr_parse_sensor_owner_lun (ipmi_sdr_parse_ctx_t ctx,
                                     const void *sdr_record,
                                     unsigned int sdr_record_len,
                                     uint8_t *sensor_owner_lun,
                                     uint8_t *channel_number);

/* For Full, Compact, Event SDR records */
int ipmi_sdr_parse_sensor_number (ipmi_sdr_parse_ctx_t ctx,
                                  const void *sdr_record,
                                  unsigned int sdr_record_len,
                                  uint8_t *sensor_number);

/* For Full, Compact, Event SDR, Generic Device Locator, Management Controller Device Locator SDR records */
int ipmi_sdr_parse_entity_id_instance_type (ipmi_sdr_parse_ctx_t ctx,
                                            const void *sdr_record,
                                            unsigned int sdr_record_len,
                                            uint8_t *entity_id,
                                            uint8_t *entity_instance,
                                            uint8_t *entity_instance_type);

/* For Full, Compact, Event SDR records */
int ipmi_sdr_parse_sensor_type (ipmi_sdr_parse_ctx_t ctx,
                                const void *sdr_record,
                                unsigned int sdr_record_len,
                                uint8_t *sensor_type);

/* For Full, Compact, Event SDR records */
int ipmi_sdr_parse_event_reading_type_code (ipmi_sdr_parse_ctx_t ctx,
                                            const void *sdr_record,
                                            unsigned int sdr_record_len,
                                            uint8_t *event_reading_type_code);

/* For Full, Compact, Event SDR records */
/* return length of data read into buffer on success, -1 on error */
int ipmi_sdr_parse_id_string (ipmi_sdr_parse_ctx_t ctx,
                              const void *sdr_record,
                              unsigned int sdr_record_len,
                              char *id_string,
                              unsigned int id_string_len);

/* For Full, Compact SDR records */
int ipmi_sdr_parse_sensor_units (ipmi_sdr_parse_ctx_t ctx,
                                 const void *sdr_record,
                                 unsigned int sdr_record_len,
                                 uint8_t *sensor_units_percentage,
                                 uint8_t *sensor_units_modifier,
                                 uint8_t *sensor_units_rate,
                                 uint8_t *sensor_base_unit_type,
                                 uint8_t *sensor_modifier_unit_type);

/* For Full, Compact SDR records */
int ipmi_sdr_parse_sensor_capabilities (ipmi_sdr_parse_ctx_t ctx,
                                        const void *sdr_record,
                                        unsigned int sdr_record_len,
                                        uint8_t *event_message_control_support,
                                        uint8_t *threshold_access_support,
                                        uint8_t *hysteresis_support,
                                        uint8_t *auto_re_arm_support,
                                        uint8_t *entity_ignore_support);

/* For Full, Compact SDR records */
int ipmi_sdr_parse_sensor_direction (ipmi_sdr_parse_ctx_t ctx,
                                     const void *sdr_record,
                                     unsigned int sdr_record_len,
                                     uint8_t *sensor_direction);

/* For Full, Compact SDR records */
/* event reading type must indicate a discrete sensor */
int ipmi_sdr_parse_assertion_supported (ipmi_sdr_parse_ctx_t ctx,
                                        const void *sdr_record,
                                        unsigned int sdr_record_len,
                                        uint8_t *event_state_0,
                                        uint8_t *event_state_1,
                                        uint8_t *event_state_2,
                                        uint8_t *event_state_3,
                                        uint8_t *event_state_4,
                                        uint8_t *event_state_5,
                                        uint8_t *event_state_6,
                                        uint8_t *event_state_7,
                                        uint8_t *event_state_8,
                                        uint8_t *event_state_9,
                                        uint8_t *event_state_10,
                                        uint8_t *event_state_11,
                                        uint8_t *event_state_12,
                                        uint8_t *event_state_13,
                                        uint8_t *event_state_14);

/* For Full, Compact SDR records */
/* event reading type must indicate a discrete sensor */
int ipmi_sdr_parse_deassertion_supported (ipmi_sdr_parse_ctx_t ctx,
                                          const void *sdr_record,
                                          unsigned int sdr_record_len,
                                          uint8_t *event_state_0,
                                          uint8_t *event_state_1,
                                          uint8_t *event_state_2,
                                          uint8_t *event_state_3,
                                          uint8_t *event_state_4,
                                          uint8_t *event_state_5,
                                          uint8_t *event_state_6,
                                          uint8_t *event_state_7,
                                          uint8_t *event_state_8,
                                          uint8_t *event_state_9,
                                          uint8_t *event_state_10,
                                          uint8_t *event_state_11,
                                          uint8_t *event_state_12,
                                          uint8_t *event_state_13,
                                          uint8_t *event_state_14);

/* For Full SDR records */
/* event reading type must indicate a threshold sensor */
int ipmi_sdr_parse_threshold_assertion_supported (ipmi_sdr_parse_ctx_t ctx,
                                                  const void *sdr_record,
                                                  unsigned int sdr_record_len,
                                                  uint8_t *lower_non_critical_going_low,
                                                  uint8_t *lower_non_critical_going_high,
                                                  uint8_t *lower_critical_going_low,
                                                  uint8_t *lower_critical_going_high,
                                                  uint8_t *lower_non_recoverable_going_low,
                                                  uint8_t *lower_non_recoverable_going_high,
                                                  uint8_t *upper_non_critical_going_low,
                                                  uint8_t *upper_non_critical_going_high,
                                                  uint8_t *upper_critical_going_low,
                                                  uint8_t *upper_critical_going_high,
                                                  uint8_t *upper_non_recoverable_going_low,
                                                  uint8_t *upper_non_recoverable_going_high);

/* For Full SDR records */
/* event reading type must indicate a threshold sensor */
int ipmi_sdr_parse_threshold_deassertion_supported (ipmi_sdr_parse_ctx_t ctx,
                                                    const void *sdr_record,
                                                    unsigned int sdr_record_len,
                                                    uint8_t *lower_non_critical_going_low,
                                                    uint8_t *lower_non_critical_going_high,
                                                    uint8_t *lower_critical_going_low,
                                                    uint8_t *lower_critical_going_high,
                                                    uint8_t *lower_non_recoverable_going_low,
                                                    uint8_t *lower_non_recoverable_going_high,
                                                    uint8_t *upper_non_critical_going_low,
                                                    uint8_t *upper_non_critical_going_high,
                                                    uint8_t *upper_critical_going_low,
                                                    uint8_t *upper_critical_going_high,
                                                    uint8_t *upper_non_recoverable_going_low,
                                                    uint8_t *upper_non_recoverable_going_high);

/* For Full SDR records */
/* event reading type must indicate a threshold sensor */
int ipmi_sdr_parse_threshold_readable (ipmi_sdr_parse_ctx_t ctx,
                                       const void *sdr_record,
                                       unsigned int sdr_record_len,
                                       uint8_t *lower_non_critical_threshold,
                                       uint8_t *lower_critical_threshold,
                                       uint8_t *lower_non_recoverable_threshold,
                                       uint8_t *upper_non_critical_threshold,
                                       uint8_t *upper_critical_threshold,
                                       uint8_t *upper_non_recoverable_threshold);

/* For Full SDR records */
/* event reading type must indicate a threshold sensor */
int ipmi_sdr_parse_threshold_settable (ipmi_sdr_parse_ctx_t ctx,
                                       const void *sdr_record,
                                       unsigned int sdr_record_len,
                                       uint8_t *lower_non_critical_threshold,
                                       uint8_t *lower_critical_threshold,
                                       uint8_t *lower_non_recoverable_threshold,
                                       uint8_t *upper_non_critical_threshold,
                                       uint8_t *upper_critical_threshold,
                                       uint8_t *upper_non_recoverable_threshold);

/* For Full SDR records */
/* b_exponent - sometimes documented as k1 */
/* r_exponent - sometimes documented as k2 */
int ipmi_sdr_parse_sensor_decoding_data (ipmi_sdr_parse_ctx_t ctx,
                                         const void *sdr_record,
                                         unsigned int sdr_record_len,
                                         int8_t *r_exponent,
                                         int8_t *b_exponent,
                                         int16_t *m,
                                         int16_t *b,
                                         uint8_t *linearization,
                                         uint8_t *analog_data_format);

/* For Full SDR records */
int ipmi_sdr_parse_sensor_reading_ranges_specified (ipmi_sdr_parse_ctx_t ctx,
                                                    const void *sdr_record,
                                                    unsigned int sdr_record_len,
                                                    uint8_t *nominal_reading_specified,
                                                    uint8_t *normal_maximum_specified,
                                                    uint8_t *normal_minimum_specified);

/* For Full SDR records */
/* Results must be freed by user */
int ipmi_sdr_parse_sensor_reading_ranges (ipmi_sdr_parse_ctx_t ctx,
                                          const void *sdr_record,
                                          unsigned int sdr_record_len,
                                          double **nominal_reading,
                                          double **normal_maximum,
                                          double **normal_minimum,
                                          double **sensor_maximum_reading,
                                          double **sensor_minimum_reading);

/* For Full SDR records */
/* Results must be freed by user */
int ipmi_sdr_parse_thresholds (ipmi_sdr_parse_ctx_t ctx,
                               const void *sdr_record,
                               unsigned int sdr_record_len,
                               double **lower_non_critical_threshold,
                               double **lower_critical_threshold,
                               double **lower_non_recoverable_threshold,
                               double **upper_non_critical_threshold,
                               double **upper_critical_threshold,
                               double **upper_non_recoverable_threshold);


/* For Full SDR records */
int ipmi_sdr_parse_thresholds_raw (ipmi_sdr_parse_ctx_t ctx,
                                   const void *sdr_record,
                                   unsigned int sdr_record_len,
                                   uint8_t *lower_non_critical_threshold,
                                   uint8_t *lower_critical_threshold,
                                   uint8_t *lower_non_recoverable_threshold,
                                   uint8_t *upper_non_critical_threshold,
                                   uint8_t *upper_critical_threshold,
                                   uint8_t *upper_non_recoverable_threshold);

/* For Full SDR records */
/* Results must be freed by user */
int ipmi_sdr_parse_tolerance (ipmi_sdr_parse_ctx_t ctx,
                              const void *sdr_record,
                              unsigned int sdr_record_len,
                              double **tolerance);

/* For Full SDR records */
/* Result returned is in percentage */
/* Results must be freed by user */
int ipmi_sdr_parse_accuracy (ipmi_sdr_parse_ctx_t ctx,
                             const void *sdr_record,
                             unsigned int sdr_record_len,
                             double **tolerance);

/* For Full SDR records */
int ipmi_sdr_parse_hysteresis (ipmi_sdr_parse_ctx_t ctx,
                               const void *sdr_record,
                               unsigned int sdr_record_len,
                               uint8_t *positive_going_threshold_hysteresis,
                               uint8_t *negative_going_threshold_hysteresis);

/* For Compact SDR records */
int ipmi_sdr_parse_sensor_record_sharing (ipmi_sdr_parse_ctx_t ctx,
                                          const void *sdr_record,
                                          unsigned int sdr_record_len,
                                          uint8_t *share_count,
                                          uint8_t *id_string_instance_modifier_type,
                                          uint8_t *id_string_instance_modifier_offset,
                                          uint8_t *entity_instance_sharing);

/* For Entity Association, Device Relative Entity Association SDR
   records */
int ipmi_sdr_parse_container_entity (ipmi_sdr_parse_ctx_t ctx,
                                     const void *sdr_record,
                                     unsigned int sdr_record_len,
                                     uint8_t *container_entity_id,
                                     uint8_t *container_entity_instance);

/* For Generic Device Locator, FRU Device Locator, Management
   Controller Device Locator SDR records */
/* return length of data read into buffer on success, -1 on error */
int ipmi_sdr_parse_device_id_string (ipmi_sdr_parse_ctx_t ctx,
                                     const void *sdr_record,
                                     unsigned int sdr_record_len,
                                     char *device_id_string,
                                     unsigned int device_id_string_len);

/* For Generic Device Locator, FRU Device Locator SDR records */
int ipmi_sdr_parse_device_type (ipmi_sdr_parse_ctx_t ctx,
                                const void *sdr_record,
                                unsigned int sdr_record_len,
                                uint8_t *device_type,
                                uint8_t *device_type_modifier);

/* For Generic Device Locator SDR records */
int ipmi_sdr_parse_generic_device_locator_parameters (ipmi_sdr_parse_ctx_t ctx,
                                                      const void *sdr_record,
                                                      unsigned int sdr_record_len,
                                                      uint8_t *device_access_address,
                                                      uint8_t *channel_number,
                                                      uint8_t *device_slave_address,
                                                      uint8_t *private_bus_id,
                                                      uint8_t *lun_for_master_write_read_command,
                                                      uint8_t *address_span,
                                                      uint8_t *oem);

/* For FRU Device Locator SDR records */
int ipmi_sdr_parse_fru_device_locator_parameters (ipmi_sdr_parse_ctx_t ctx,
                                                  const void *sdr_record,
                                                  unsigned int sdr_record_len,
                                                  uint8_t *device_access_address,
                                                  uint8_t *logical_fru_device_device_slave_address,
                                                  uint8_t *private_bus_id,
                                                  uint8_t *lun_for_master_write_read_fru_command,
                                                  uint8_t *logical_physical_fru_device,
                                                  uint8_t *channel_number);

/* For FRU Device Locator SDR records */
int ipmi_sdr_parse_fru_entity_id_and_instance (ipmi_sdr_parse_ctx_t ctx,
                                               const void *sdr_record,
                                               unsigned int sdr_record_len,
                                               uint8_t *fru_entity_id,
                                               uint8_t *fru_entity_instance);

/* For Management Controller Device Locator SDR records */
int ipmi_sdr_parse_management_controller_device_locator_parameters (ipmi_sdr_parse_ctx_t ctx,
                                                                    const void *sdr_record,
                                                                    unsigned int sdr_record_len,
                                                                    uint8_t *device_slave_address,
                                                                    uint8_t *channel_number);


/* For Management Controller Confirmation, OEM SDR records */
int ipmi_sdr_parse_manufacturer_id (ipmi_sdr_parse_ctx_t ctx,
                                    const void *sdr_record,
                                    unsigned int sdr_record_len,
                                    uint32_t *manufacturer_id);

/* For Management Controller Confirmation SDR records */
int ipmi_sdr_parse_product_id (ipmi_sdr_parse_ctx_t ctx,
                               const void *sdr_record,
                               unsigned int sdr_record_len,
                               uint16_t *product_id);

/* For OEM SDR records */
/* return length of data read into buffer on success, -1 on error */
int ipmi_sdr_parse_oem_data (ipmi_sdr_parse_ctx_t ctx,
                             const void *sdr_record,
                             unsigned int sdr_record_len,
                             void *oem_data,
                             unsigned int oem_data_len);

#ifdef __cplusplus
}
#endif

#endif /* IPMI_SDR_PARSE_H */
