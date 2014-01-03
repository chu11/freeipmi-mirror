/*
 * Copyright (C) 2003-2014 FreeIPMI Core Team
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

#ifndef IPMI_INTERPRET_CONFIG_COMMON_H
#define IPMI_INTERPRET_CONFIG_COMMON_H

#include "freeipmi/interpret/ipmi-interpret.h"

#include "ipmi-interpret-defs.h"

#include "conffile.h"

#define IPMI_INTERPRET_CONFIG_FILE_OPTIONS_MAX 1024

#define IPMI_INTERPRET_CONFIG_FILE_MANUFACTURER_ID_MAX 64

#define IPMI_INTERPRET_CONFIG_FILE_PRODUCT_ID_MAX      1024

struct ipmi_interpret_config_file_ids {
  uint32_t manufacturer_id;
  uint16_t product_ids[IPMI_INTERPRET_CONFIG_FILE_PRODUCT_ID_MAX];
  unsigned product_ids_count;
};

int interpret_config_parse_state (conffile_t cf,
				  char *option_string);

int interpret_config_parse_strtoul (conffile_t cf,
				    const char *str,
				    uint32_t max,
				    uint32_t *value);

int interpret_config_parse_manufactuer_id_product_id (conffile_t cf,
						      const char *str,
						      struct ipmi_interpret_config_file_ids ids[IPMI_INTERPRET_CONFIG_FILE_MANUFACTURER_ID_MAX],
						      unsigned int *ids_count);

#endif /* IPMI_INTERPRET_CONFIG_COMMON_H */
