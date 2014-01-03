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

#if HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <assert.h>

#include "ipmi-config.h"
#include "ipmi-config-section.h"
#include "ipmi-config-validate.h"
#include "ipmi-config-category-common-pef-conf-section.h"

#include "freeipmi-portability.h"

struct ipmi_config_section *
ipmi_config_core_pef_conf_section_get (ipmi_config_state_data_t *state_data)
{
  struct ipmi_config_section *section;

  assert (state_data);

  if (!(section = ipmi_config_section_create (state_data,
                                              "PEF_Conf",
                                              NULL,
                                              NULL,
                                              IPMI_CONFIG_DO_NOT_CHECKOUT | IPMI_CONFIG_DO_NOT_LIST,
                                              NULL,
                                              NULL)))
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "Enable_PEF",
                                   "Possible values: Yes/No",
                                   0,
                                   enable_pef_checkout,
                                   enable_pef_commit,
                                   yes_no_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "Enable_PEF_Event_Messages",
                                   "Possible values: Yes/No",
                                   0,
                                   enable_pef_event_messages_checkout,
                                   enable_pef_event_messages_commit,
                                   yes_no_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "Enable_PEF_Startup_Delay",
                                   "Possible values: Yes/No",
                                   0,
                                   enable_pef_startup_delay_checkout,
                                   enable_pef_startup_delay_commit,
                                   yes_no_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "Enable_PEF_Alert_Startup_Delay",
                                   "Possible values: Yes/No",
                                   0,
                                   enable_pef_alert_startup_delay_checkout,
                                   enable_pef_alert_startup_delay_commit,
                                   yes_no_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "Enable_Alert_Action",
                                   "Possible values: Yes/No",
                                   0,
                                   enable_alert_action_checkout,
                                   enable_alert_action_commit,
                                   yes_no_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "Enable_Power_Down_Action",
                                   "Possible values: Yes/No",
                                   0,
                                   enable_power_down_action_checkout,
                                   enable_power_down_action_commit,
                                   yes_no_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "Enable_Reset_Action",
                                   "Possible values: Yes/No",
                                   0,
                                   enable_reset_action_checkout,
                                   enable_reset_action_commit,
                                   yes_no_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "Enable_Power_Cycle_Action",
                                   "Possible values: Yes/No",
                                   0,
                                   enable_power_cycle_action_checkout,
                                   enable_power_cycle_action_commit,
                                   yes_no_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "Enable_OEM_Action",
                                   "Possible values: Yes/No",
                                   0,
                                   enable_oem_action_checkout,
                                   enable_oem_action_commit,
                                   yes_no_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "Enable_Diagnostic_Interrupt",
                                   "Possible values: Yes/No",
                                   0,
                                   enable_diagnostic_interrupt_checkout,
                                   enable_diagnostic_interrupt_commit,
                                   yes_no_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "PEF_Startup_Delay",
                                   "Give value in seconds",
                                   0,
                                   pef_startup_delay_checkout,
                                   pef_startup_delay_commit,
                                   number_range_one_byte_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "PEF_Alert_Startup_Delay",
                                   "Give value in seconds",
                                   0,
                                   pef_alert_startup_delay_checkout,
                                   pef_alert_startup_delay_commit,
                                   number_range_one_byte_validate) < 0)
    goto cleanup;

  return (section);

 cleanup:
  if (section)
    ipmi_config_section_destroy (section);
  return (NULL);
}

