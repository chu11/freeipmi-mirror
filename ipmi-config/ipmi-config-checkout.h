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

#ifndef IPMI_CONFIG_CHECKOUT_H
#define IPMI_CONFIG_CHECKOUT_H

#include "ipmi-config.h"

ipmi_config_err_t ipmi_config_checkout_section (ipmi_config_state_data_t *state_data,
                                                struct ipmi_config_section *section,
                                                int all_keys_if_none_specified,
                                                FILE *fp);

ipmi_config_err_t ipmi_config_checkout (ipmi_config_state_data_t *state_data,
                                        int all_keys_if_none_specified,
                                        FILE *fp);

#endif /* IPMI_CONFIG_CHECKOUT_H */
