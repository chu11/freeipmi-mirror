#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */

#include "bmc-config.h"
#include "bmc-config-common.h"
#include "bmc-config-wrapper.h"
#include "bmc-config-diff.h"
#include "bmc-config-map.h"
#include "bmc-config-sections.h"
#include "bmc-config-validate.h"

static config_err_t
serial_conf_checkout (bmc_config_state_data_t *state_data,
		      uint8_t *basic_mode,
		      uint8_t *ppp_mode,
		      uint8_t *terminal_mode,
		      uint8_t *connect_mode)
{
  uint8_t tmp_basic_mode;
  uint8_t tmp_ppp_mode;
  uint8_t tmp_terminal_mode;
  uint8_t tmp_connect_mode;
  config_err_t ret;

  if ((ret = get_bmc_serial_conf_connection_mode (state_data,
                                                  &tmp_basic_mode,
                                                  &tmp_ppp_mode,
                                                  &tmp_terminal_mode,
                                                  &tmp_connect_mode)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (basic_mode)
    *basic_mode = tmp_basic_mode;

  if (ppp_mode)
    *ppp_mode = tmp_ppp_mode;

  if (terminal_mode)
    *terminal_mode = tmp_terminal_mode;

  if (connect_mode)
    *connect_mode = tmp_connect_mode;

  return CONFIG_ERR_SUCCESS;
}


static config_err_t
serial_conf_commit (bmc_config_state_data_t *state_data,
		    uint8_t *basic_mode,
		    uint8_t *ppp_mode,
		    uint8_t *terminal_mode,
		    uint8_t *connect_mode)
{
  uint8_t tmp_basic_mode;
  uint8_t tmp_ppp_mode;
  uint8_t tmp_terminal_mode;
  uint8_t tmp_connect_mode;
  config_err_t ret;

  if ((ret = get_bmc_serial_conf_connection_mode (state_data,
                                                  &tmp_basic_mode,
                                                  &tmp_ppp_mode,
                                                  &tmp_terminal_mode,
                                                  &tmp_connect_mode)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (basic_mode)
    tmp_basic_mode = *basic_mode;

  if (ppp_mode)
    tmp_ppp_mode = *ppp_mode;

  if (terminal_mode)
    tmp_terminal_mode = *terminal_mode;

  if (connect_mode)
    tmp_connect_mode = *connect_mode;

  return set_bmc_serial_conf_connection_mode (state_data,
                                              tmp_basic_mode,
                                              tmp_ppp_mode,
                                              tmp_terminal_mode,
                                              tmp_connect_mode);
}

static config_err_t
enable_basic_mode_checkout (const struct config_section *section,
			    struct config_keyvalue *kv,
                            void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  uint8_t value;
  config_err_t ret;

  if ((ret = serial_conf_checkout (state_data,
                                   &value,
                                   NULL,
                                   NULL,
                                   NULL)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (!(kv->value = strdup (value ? "Yes" : "No")))
    {
      perror("strdup");
      return CONFIG_ERR_FATAL_ERROR;
    }

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
enable_basic_mode_commit (const struct config_section *section,
			  const struct config_keyvalue *kv,
                          void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  uint8_t value;
  value = (same (kv->value, "yes") ? 1 : 0);

  return serial_conf_commit (state_data,
			     &value, NULL, NULL, NULL);
}

static config_diff_t
enable_basic_mode_diff (const struct config_section *section,
			const struct config_keyvalue *kv,
                        void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  uint8_t get_value;
  uint8_t passed_value;
  config_err_t rc;
  config_diff_t ret;
  
  if ((rc = serial_conf_checkout (state_data,
                                   &get_value,
                                   NULL,
                                   NULL,
                                   NULL)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return CONFIG_DIFF_NON_FATAL_ERROR;
      return CONFIG_DIFF_FATAL_ERROR;
    }

  passed_value = same (kv->value, "yes");

  if (passed_value == get_value)
    ret = CONFIG_DIFF_SAME;
  else 
    {
      ret = CONFIG_DIFF_DIFFERENT;
      report_diff (section->section_name,
                   kv->key_name,
                   kv->value,
                   get_value ? "Yes" : "No");
    }
  return ret;
}

static config_err_t
enable_ppp_mode_checkout (const struct config_section *section,
			  struct config_keyvalue *kv,
                          void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  uint8_t value;
  config_err_t ret;

  if ((ret = serial_conf_checkout (state_data,
                                   NULL,
                                   &value,
                                   NULL,
                                   NULL)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (!(kv->value = strdup (value ? "Yes" : "No")))
    {
      perror("strdup");
      return CONFIG_ERR_FATAL_ERROR;
    }

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
enable_ppp_mode_commit (const struct config_section *section,
			const struct config_keyvalue *kv,
                        void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  uint8_t value;
  value = (same (kv->value, "yes") ? 1 : 0);

  return serial_conf_commit (state_data,
			     NULL, &value, NULL, NULL);
}

static config_diff_t
enable_ppp_mode_diff (const struct config_section *section,
		      const struct config_keyvalue *kv,
                      void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  uint8_t get_value;
  uint8_t passed_value;
  config_err_t rc;
  config_diff_t ret;

  if ((rc = serial_conf_checkout (state_data,
                                  NULL,
                                  &get_value,
                                  NULL,
                                  NULL)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return CONFIG_DIFF_NON_FATAL_ERROR;
      return CONFIG_DIFF_FATAL_ERROR;
    }

  passed_value = same (kv->value, "yes");

  if (passed_value == get_value)
    ret = CONFIG_DIFF_SAME;
  else 
    {
      ret = CONFIG_DIFF_DIFFERENT;
      report_diff (section->section_name,
                   kv->key_name,
                   kv->value,
                   get_value ? "Yes" : "No");
    }
  return ret;
}

static config_err_t
enable_terminal_mode_checkout (const struct config_section *section,
			       struct config_keyvalue *kv,
                               void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  uint8_t value;
  config_err_t ret;

  if ((ret = serial_conf_checkout (state_data,
                                   NULL,
                                   NULL,
                                   &value,
                                   NULL)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (!(kv->value = strdup (value ? "Yes" : "No")))
    {
      perror("strdup");
      return CONFIG_ERR_FATAL_ERROR;
    }

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
enable_terminal_mode_commit (const struct config_section *section,
			     const struct config_keyvalue *kv,
                             void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  uint8_t value;
  value = (same (kv->value, "yes") ? 1 : 0);

  return serial_conf_commit (state_data,
			     NULL, NULL, &value, NULL);
}

static config_diff_t
enable_terminal_mode_diff (const struct config_section *section,
			   const struct config_keyvalue *kv,
                           void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  uint8_t get_value;
  uint8_t passed_value;
  config_err_t rc;
  config_diff_t ret;

  if ((rc = serial_conf_checkout (state_data,
                                  NULL,
                                  NULL,
                                  &get_value,
                                  NULL)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return CONFIG_DIFF_NON_FATAL_ERROR;
      return CONFIG_DIFF_FATAL_ERROR;
    }

  passed_value = same (kv->value, "yes");

  if (passed_value == get_value)
    ret = CONFIG_DIFF_SAME;
  else 
    {
      ret = CONFIG_DIFF_DIFFERENT;
      report_diff (section->section_name,
                   kv->key_name,
                   kv->value,
                   get_value ? "Yes" : "No");
    }
  return ret;
}

static config_err_t
connect_mode_checkout (const struct config_section *section,
		       struct config_keyvalue *kv,
                       void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  uint8_t value;
  config_err_t ret;

  if ((ret = serial_conf_checkout (state_data,
                                   NULL,
                                   NULL,
                                   NULL,
                                   &value)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (!(kv->value = strdup (connect_mode_string (value))))
    {
      perror("strdup");
      return CONFIG_ERR_FATAL_ERROR;
    }

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
connect_mode_commit (const struct config_section *section,
		     const struct config_keyvalue *kv,
                     void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  uint8_t value;
  value = connect_mode_number (kv->value);

  return serial_conf_commit (state_data,
			     NULL, NULL, NULL, &value);
}

static config_diff_t
connect_mode_diff (const struct config_section *section,
		   const struct config_keyvalue *kv,
                   void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  uint8_t get_value;
  uint8_t passed_value;
  config_err_t rc;
  config_diff_t ret;

  if ((rc = serial_conf_checkout (state_data,
                                  NULL,
                                  NULL,
                                  NULL,
                                  &get_value)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return CONFIG_DIFF_NON_FATAL_ERROR;
      return CONFIG_DIFF_FATAL_ERROR;
    }

  passed_value = connect_mode_number (kv->value);
  if (passed_value == get_value)
    ret = CONFIG_DIFF_SAME;
  else 
    {
      ret = CONFIG_DIFF_DIFFERENT;
      report_diff (section->section_name,
                   kv->key_name,
                   kv->value,
                   connect_mode_string (get_value));
    }
  return ret;
}

static config_err_t
page_blackout_interval_checkout (const struct config_section *section,
				 struct config_keyvalue *kv,
                                 void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  uint8_t interval;
  config_err_t ret;

  if ((ret = get_bmc_serial_conf_page_blackout_interval (state_data,
                                                         &interval)) != CONFIG_ERR_SUCCESS)
    return ret;
  
  if (asprintf (&kv->value, "%d", interval) < 0)
    {
      perror("asprintf");
      return CONFIG_ERR_FATAL_ERROR;
    }

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
page_blackout_interval_commit (const struct config_section *section,
			       const struct config_keyvalue *kv,
                               void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  return set_bmc_serial_conf_page_blackout_interval (state_data,
						     atoi (kv->value));
}

static config_diff_t
page_blackout_interval_diff (const struct config_section *section,
			     const struct config_keyvalue *kv,
                             void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  uint8_t interval;
  int passed_interval;
  config_err_t rc;
  config_diff_t ret;

  if ((rc = get_bmc_serial_conf_page_blackout_interval (state_data,
                                                        &interval)) != CONFIG_ERR_SUCCESS) 
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return CONFIG_DIFF_NON_FATAL_ERROR;
      return CONFIG_DIFF_FATAL_ERROR;
    }

  passed_interval = atoi (kv->value);

  if (passed_interval == interval)
    ret = CONFIG_DIFF_SAME;
  else 
    {
      char num[32];
      ret = CONFIG_DIFF_DIFFERENT;
      sprintf (num, "%d", interval);
      report_diff (section->section_name,
                   kv->key_name,
                   kv->value,
                   num);
    }
  return ret;
}

static config_err_t
call_retry_interval_checkout (const struct config_section *section,
			      struct config_keyvalue *kv,
                              void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  uint8_t interval;
  config_err_t ret;

  if ((ret = get_bmc_serial_conf_call_retry_interval (state_data,
                                                      &interval)) != CONFIG_ERR_SUCCESS)
    return ret;
       
  if (asprintf (&kv->value, "%d", interval) < 0)
    {
      perror("asprintf");
      return CONFIG_ERR_FATAL_ERROR;
    }

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
call_retry_interval_commit (const struct config_section *section,
			    const struct config_keyvalue *kv,
                            void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  return set_bmc_serial_conf_call_retry_interval (state_data,
                                                  atoi (kv->value));
}

static config_diff_t
call_retry_interval_diff (const struct config_section *section,
			  const struct config_keyvalue *kv,
                          void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  uint8_t interval;
  int passed_interval;
  config_err_t rc;
  config_diff_t ret;

  if ((rc = get_bmc_serial_conf_call_retry_interval (state_data,
                                                     &interval)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return CONFIG_DIFF_NON_FATAL_ERROR;
      return CONFIG_DIFF_FATAL_ERROR;
    }

  passed_interval = atoi (kv->value);

  if (passed_interval == interval)
    ret = CONFIG_DIFF_SAME;
  else 
    {
      char num[32];
      ret = CONFIG_DIFF_DIFFERENT;
      sprintf (num, "%d", interval);
      report_diff (section->section_name,
                   kv->key_name,
                   kv->value,
                   num);
    }
  return ret;
}

static config_err_t
serial_conf_comm_checkout (bmc_config_state_data_t *state_data,
			   uint8_t *dtr_hangup,
			   uint8_t *flow_control,
			   uint8_t *bit_rate)
{
  uint8_t tmp_dtr_hangup;
  uint8_t tmp_flow_control;
  uint8_t tmp_bit_rate;
  config_err_t ret;

  if ((ret = get_bmc_serial_conf_ipmi_messaging_comm_settings (state_data,
                                                               &tmp_dtr_hangup,
                                                               &tmp_flow_control,
                                                               &tmp_bit_rate)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (dtr_hangup)
    *dtr_hangup = tmp_dtr_hangup;

  if (flow_control)
    *flow_control = tmp_flow_control;

  if (bit_rate)
    *bit_rate = tmp_bit_rate;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
serial_conf_comm_commit (bmc_config_state_data_t *state_data,
			 uint8_t *dtr_hangup,
			 uint8_t *flow_control,
			 uint8_t *bit_rate)
{
  uint8_t tmp_dtr_hangup;
  uint8_t tmp_flow_control;
  uint8_t tmp_bit_rate;
  config_err_t ret;

  if ((ret = get_bmc_serial_conf_ipmi_messaging_comm_settings (state_data,
                                                               &tmp_dtr_hangup,
                                                               &tmp_flow_control,
                                                               &tmp_bit_rate)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (dtr_hangup)
    tmp_dtr_hangup = *dtr_hangup;
  if (flow_control)
    tmp_flow_control = *flow_control;
  if (bit_rate)
    tmp_bit_rate = *bit_rate;

  return set_bmc_serial_conf_ipmi_messaging_comm_settings (state_data,
							   tmp_dtr_hangup,
							   tmp_flow_control,
							   tmp_bit_rate);
}

static config_err_t
enable_dtr_hangup_checkout (const struct config_section *section,
			    struct config_keyvalue *kv,
                            void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  uint8_t value;
  config_err_t ret;
  
  if ((ret = serial_conf_comm_checkout (state_data,
                                        &value,
                                        NULL,
                                        NULL)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (!(kv->value = strdup (value ? "Yes" : "No")))
    {
      perror("strdup");
      return CONFIG_ERR_FATAL_ERROR;
    }

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
enable_dtr_hangup_commit (const struct config_section *section,
			  const struct config_keyvalue *kv,
                          void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  uint8_t value = same (kv->value, "yes");

  return serial_conf_comm_commit (state_data,
				  &value,
				  NULL,
				  NULL);
}

static config_diff_t
enable_dtr_hangup_diff (const struct config_section *section,
			const struct config_keyvalue *kv,
                        void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  uint8_t passed_value;
  uint8_t got_value;
  config_err_t rc;
  config_diff_t ret;

  if ((rc = serial_conf_comm_checkout (state_data,
                                       &got_value,
                                       NULL,
                                       NULL)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return CONFIG_DIFF_NON_FATAL_ERROR;
      return CONFIG_DIFF_FATAL_ERROR;
    }

  passed_value = same (kv->value, "yes") ? 1 : 0;

  if (passed_value == got_value)
    ret = CONFIG_DIFF_SAME;
  else 
    {
      ret = CONFIG_DIFF_DIFFERENT;
      report_diff (section->section_name,
                   kv->key_name,
                   kv->value,
                   got_value ? "Yes" : "No");
    }
  return ret;
}

static config_err_t
flow_control_checkout (const struct config_section *section,
		       struct config_keyvalue *kv,
                       void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  uint8_t value;
  config_err_t ret;
  
  if ((ret = serial_conf_comm_checkout (state_data,
                                        NULL,
                                        &value,
                                        NULL)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (!(kv->value = strdup (flow_control_string (value))))
    {
      perror("strdup");
      return CONFIG_ERR_FATAL_ERROR;
    }

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
flow_control_commit (const struct config_section *section,
		     const struct config_keyvalue *kv,
                     void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  uint8_t value = flow_control_number (kv->value);
  return serial_conf_comm_commit (state_data,
				  NULL,
				  &value,
				  NULL);
}

static config_diff_t
flow_control_diff (const struct config_section *section,
		   const struct config_keyvalue *kv,
                   void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  uint8_t passed_value;
  uint8_t got_value;
  config_err_t rc;
  config_diff_t ret;

  if ((rc = serial_conf_comm_checkout (state_data,
                                       NULL,
                                       &got_value,
                                       NULL)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return CONFIG_DIFF_NON_FATAL_ERROR;
      return CONFIG_DIFF_FATAL_ERROR;
    }

  passed_value = flow_control_number (kv->value);

  if (passed_value == got_value)
    ret = CONFIG_DIFF_SAME;
  else 
    {
      ret = CONFIG_DIFF_DIFFERENT;
      report_diff (section->section_name,
                   kv->key_name,
                   kv->value,
                   flow_control_string (got_value));
    }
  return ret;
}

static config_err_t
bit_rate_checkout (const struct config_section *section,
		   struct config_keyvalue *kv,
                   void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  uint8_t value;
  config_err_t ret;
  
  if ((ret = serial_conf_comm_checkout (state_data,
                                        NULL,
                                        NULL,
                                        &value)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (!(kv->value = strdup (bit_rate_string (value))))
    {
      perror("strdup");
      return CONFIG_ERR_FATAL_ERROR;
    }

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
bit_rate_commit (const struct config_section *section,
		 const struct config_keyvalue *kv,
                 void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  uint8_t value = bit_rate_number (kv->value);
  return serial_conf_comm_commit (state_data,
				  NULL,
				  NULL,
				  &value);
}

static config_diff_t
bit_rate_diff (const struct config_section *section,
	       const struct config_keyvalue *kv,
               void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  uint8_t passed_value;
  uint8_t got_value;
  config_err_t rc;
  config_diff_t ret;

  if ((rc = serial_conf_comm_checkout (state_data,
                                       NULL,
                                       NULL,
                                       &got_value)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return CONFIG_DIFF_NON_FATAL_ERROR;
      return CONFIG_DIFF_FATAL_ERROR;
    }

  passed_value = bit_rate_number (kv->value);

  if (passed_value == got_value)
    ret = CONFIG_DIFF_SAME;
  else 
    {
      ret = CONFIG_DIFF_DIFFERENT;
      report_diff (section->section_name,
                   kv->key_name,
                   kv->value,
                   bit_rate_string (got_value));
    }
  return ret;
}

struct config_section *
bmc_config_serial_conf_section_get (bmc_config_state_data_t *state_data)
{
  struct config_section *bmc_serial_conf_section = NULL;
  char *section_comment = 
    "In the Serial_Conf section, typical serial communication configuration "
    "is setup.  Most users will only be interested in IPMI over LAN, "
    "therefore this section can generally be ignored.";

  if (!(bmc_serial_conf_section = bmc_config_section_create(state_data, 
                                                            "Serial_Conf", 
                                                            "Serial_Conf", 
                                                            section_comment,
                                                            0)))
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       bmc_serial_conf_section,
                                       "Enable_Basic_Mode",
                                       "Possible values: Yes/No",
                                       0,
                                       enable_basic_mode_checkout,
                                       enable_basic_mode_commit,
                                       enable_basic_mode_diff,
                                       config_yes_no_validate) < 0)
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       bmc_serial_conf_section,
                                       "Enable_PPP_Mode",
                                       "Possible values: Yes/No",
                                       0,
                                       enable_ppp_mode_checkout,
                                       enable_ppp_mode_commit,
                                       enable_ppp_mode_diff,
                                       config_yes_no_validate) < 0)
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       bmc_serial_conf_section,
                                       "Enable_Terminal_Mode",
                                       "Possible values: Yes/No",
                                       0,
                                       enable_terminal_mode_checkout,
                                       enable_terminal_mode_commit,
                                       enable_terminal_mode_diff,
                                       config_yes_no_validate) < 0)
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       bmc_serial_conf_section,
                                       "Connect_Mode",
                                       "Possible values: Modem_Connect/Direct_Mode",
                                       0,
                                       connect_mode_checkout,
                                       connect_mode_commit,
                                       connect_mode_diff,
                                       connect_mode_number_validate) < 0)
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       bmc_serial_conf_section,
                                       "Page_Blackout_Interval",
                                       "Give a valid number",
                                       0,
                                       page_blackout_interval_checkout,
                                       page_blackout_interval_commit,
                                       page_blackout_interval_diff,
                                       config_number_range_one_byte) < 0)
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       bmc_serial_conf_section,
                                       "Call_Retry_Interval",
                                       "Give a valid number",
                                       0,
                                       call_retry_interval_checkout,
                                       call_retry_interval_commit,
                                       call_retry_interval_diff,
                                       config_number_range_one_byte) < 0)
    goto cleanup;

  /* achu: For backwards compatability to bmc-config in 0.2.0 */
  if (bmc_config_section_add_keyvalue (state_data,
                                       bmc_serial_conf_section,
                                       "Call_Retry_Time",
                                       "Give a valid number",
                                       CONFIG_DO_NOT_CHECKOUT,
                                       call_retry_interval_checkout,
                                       call_retry_interval_commit,
                                       call_retry_interval_diff,
                                       config_number_range_one_byte) < 0)
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       bmc_serial_conf_section,
                                       "Enable_DTR_Hangup",
                                       "Possible values: Yes/No",
                                       0,
                                       enable_dtr_hangup_checkout,
                                       enable_dtr_hangup_commit,
                                       enable_dtr_hangup_diff,
                                       config_yes_no_validate) < 0)
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       bmc_serial_conf_section,
                                       "Flow_Control",
                                       "Possible values: No_Flow_Control/RTS_CTS/XON_XOFF",
                                       0,
                                       flow_control_checkout,
                                       flow_control_commit,
                                       flow_control_diff,
                                       flow_control_number_validate) < 0)
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       bmc_serial_conf_section,
                                       "Bit_Rate",
                                       "Possible values: 9600/19200/38400/57600/115200",
                                       0,
                                       bit_rate_checkout,
                                       bit_rate_commit,
                                       bit_rate_diff,
                                       bit_rate_number_validate) < 0)
    goto cleanup;

  return bmc_serial_conf_section;

 cleanup:
  if (bmc_serial_conf_section)
    bmc_config_section_destroy(state_data, bmc_serial_conf_section);
  return NULL;
}
