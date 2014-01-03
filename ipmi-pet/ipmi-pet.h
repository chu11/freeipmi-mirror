/*
 * Copyright (C) 2011-2014 FreeIPMI Core Team
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

#ifndef IPMI_PET_H
#define IPMI_PET_H

#include <freeipmi/freeipmi.h>

#include "tool-cmdline-common.h"
#include "tool-oem-common.h"
#include "tool-sdr-cache-common.h"
#include "tool-sensor-common.h"

/* seems like a nice size */
#define IPMI_PET_MAX_ARGS 1024

enum ipmi_pet_argp_option_keys
  {
    VERBOSE_KEY = 'v',
    PET_ACKNOWLEDGE_KEY = 160,
    CMD_FILE_KEY = 161,
    OUTPUT_EVENT_SEVERITY_KEY = 162,
    OUTPUT_EVENT_STATE_KEY = 163,
    EVENT_STATE_CONFIG_FILE_KEY = 164,
    MANUFACTURER_ID_KEY = 165,
    PRODUCT_ID_KEY = 166,
    INTERPRET_OEM_DATA_KEY = 167,
    ENTITY_SENSOR_NAMES_KEY = 168,
    NO_SENSOR_TYPE_OUTPUT_KEY = 169,
    COMMA_SEPARATED_OUTPUT_KEY = 170,
    NO_HEADER_OUTPUT_KEY = 171,
    NON_ABBREVIATED_UNITS_KEY = 172,
  };

struct ipmi_pet_arguments
{
  struct common_cmd_args common_args;
  unsigned int verbose_count;
  int pet_acknowledge;
  char *cmd_file;
  int output_event_severity;
  int output_event_state;
  char *event_state_config_file;
  uint32_t manufacturer_id;
  int manufacturer_id_set;
  uint16_t product_id;
  int product_id_set;
  int interpret_oem_data;
  int entity_sensor_names;
  int no_sensor_type_output;
  int comma_separated_output;
  int no_header_output;
  int non_abbreviated_units;
  uint32_t specific_trap;
  int specific_trap_na_specified;
  int specific_trap_set;
  uint8_t variable_bindings[IPMI_PET_MAX_ARGS];
  unsigned int variable_bindings_length;
};

typedef struct ipmi_pet_prog_data
{
  char *progname;
  struct ipmi_pet_arguments *args;
} ipmi_pet_prog_data_t;

typedef struct ipmi_pet_state_data
{
  ipmi_pet_prog_data_t *prog_data;
  ipmi_ctx_t ipmi_ctx;
  char *hostname;
  ipmi_sdr_ctx_t sdr_ctx;
  ipmi_sel_ctx_t sel_ctx;
  ipmi_fru_ctx_t fru_ctx;
  ipmi_interpret_ctx_t interpret_ctx;
  int output_headers;
  struct sensor_column_width column_width;
  struct ipmi_oem_data oem_data;
} ipmi_pet_state_data_t;

#endif /* IPMI_PET_H */
