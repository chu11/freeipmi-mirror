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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#ifdef STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <errno.h>
#include <assert.h>

#include "freeipmi/api/ipmi-serial-modem-cmds-api.h"
#include "freeipmi/cmds/ipmi-serial-modem-cmds.h"
#include "freeipmi/fiid/fiid.h"
#include "freeipmi/spec/ipmi-ipmb-lun-spec.h"
#include "freeipmi/spec/ipmi-netfn-spec.h"
#include "freeipmi/spec/ipmi-serial-modem-configuration-parameters-spec.h"

#include "ipmi-api-defs.h"
#include "ipmi-api-trace.h"
#include "ipmi-api-util.h"

#include "libcommon/ipmi-fiid-util.h"

#include "freeipmi-portability.h"

int
ipmi_cmd_set_serial_modem_configuration (ipmi_ctx_t ctx,
                                         uint8_t channel_number,
                                         uint8_t parameter_selector,
                                         const void *configuration_parameter_data,
                                         unsigned int configuration_parameter_data_len,
                                         fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  /* remaining parameter checks in fill function */
  if (!fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_cmd_set_serial_modem_configuration_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_set_serial_modem_configuration_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_set_serial_modem_configuration (channel_number,
                                               parameter_selector,
                                               configuration_parameter_data,
                                               configuration_parameter_data_len,
                                               obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }
  
  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_TRANSPORT_RQ,
                    obj_cmd_rq,
                    obj_cmd_rs) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}

int
ipmi_cmd_set_serial_modem_configuration_set_in_progress (ipmi_ctx_t ctx,
                                                         uint8_t channel_number,
                                                         uint8_t state,
                                                         fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  /* remaining parameter checks in fill function */
  if (!fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_cmd_set_serial_modem_configuration_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_set_serial_modem_configuration_set_in_progress_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_set_serial_modem_configuration_set_in_progress (channel_number,
                                                               state,
                                                               obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_TRANSPORT_RQ,
                    obj_cmd_rq,
                    obj_cmd_rs) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}

int
ipmi_cmd_set_serial_modem_configuration_connection_mode (ipmi_ctx_t ctx,
                                                         uint8_t channel_number,
                                                         uint8_t basic_mode,
                                                         uint8_t ppp_mode,
                                                         uint8_t terminal_mode,
                                                         uint8_t connect_mode,
                                                         fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  /* remaining parameter checks in fill function */
  if (!fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_cmd_set_serial_modem_configuration_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_set_serial_modem_configuration_connection_mode_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_set_serial_modem_configuration_connection_mode (channel_number,
                                                               basic_mode,
                                                               ppp_mode,
                                                               terminal_mode,
                                                               connect_mode,
                                                               obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_TRANSPORT_RQ,
                    obj_cmd_rq,
                    obj_cmd_rs) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}

int
ipmi_cmd_set_serial_modem_configuration_ipmi_messaging_comm_settings (ipmi_ctx_t ctx,
                                                                      uint8_t channel_number,
                                                                      uint8_t dtr_hangup,
                                                                      uint8_t flow_control,
                                                                      uint8_t bit_rate,
                                                                      fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  /* remaining parameter checks in fill function */
  if (!fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_cmd_set_serial_modem_configuration_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_set_serial_modem_configuration_ipmi_messaging_comm_settings_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_set_serial_modem_configuration_ipmi_messaging_comm_settings (channel_number,
                                                                            dtr_hangup,
                                                                            flow_control,
                                                                            bit_rate,
                                                                            obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_TRANSPORT_RQ,
                    obj_cmd_rq,
                    obj_cmd_rs) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}

int
ipmi_cmd_set_serial_modem_configuration_page_blackout_interval (ipmi_ctx_t ctx,
                                                                uint8_t channel_number,
                                                                uint8_t page_blackout_interval,
                                                                fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  /* remaining parameter checks in fill function */
  if (!fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_cmd_set_serial_modem_configuration_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_set_serial_modem_configuration_page_blackout_interval_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_set_serial_modem_configuration_page_blackout_interval (channel_number,
                                                                      page_blackout_interval,
                                                                      obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_TRANSPORT_RQ,
                    obj_cmd_rq,
                    obj_cmd_rs) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}

int
ipmi_cmd_set_serial_modem_configuration_call_retry_interval (ipmi_ctx_t ctx,
                                                             uint8_t channel_number,
                                                             uint8_t call_retry_interval,
                                                             fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  /* remaining parameter checks in fill function */
  if (!fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_cmd_set_serial_modem_configuration_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_set_serial_modem_configuration_call_retry_interval_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_set_serial_modem_configuration_call_retry_interval (channel_number,
                                                                   call_retry_interval,
                                                                   obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_TRANSPORT_RQ,
                    obj_cmd_rq,
                    obj_cmd_rs) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}

static int
_ipmi_cmd_get_serial_modem_configuration_parameters_common (ipmi_ctx_t ctx,
							    uint8_t channel_number,
							    uint8_t get_parameter,
							    uint8_t set_selector,
							    uint8_t block_selector,
							    fiid_obj_t obj_cmd_rs,
							    fiid_field_t *tmpl_cmd_rs_expected,
							    uint8_t parameter_selector)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  assert (tmpl_cmd_rs_expected);

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  /* remaining parameter checks in fill function */
  if (!fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs, tmpl_cmd_rs_expected) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_get_serial_modem_configuration_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_get_serial_modem_configuration (channel_number,
                                               get_parameter,
                                               parameter_selector,
                                               set_selector,
                                               block_selector,
                                               obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_TRANSPORT_RQ,
                    obj_cmd_rq,
                    obj_cmd_rs) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}

int
ipmi_cmd_get_serial_modem_configuration (ipmi_ctx_t ctx,
                                         uint8_t channel_number,
                                         uint8_t get_parameter,
                                         uint8_t parameter_selector,
                                         uint8_t set_selector,
                                         uint8_t block_selector,
                                         fiid_obj_t obj_cmd_rs)
{
  if (_ipmi_cmd_get_serial_modem_configuration_parameters_common (ctx,
								  channel_number,
								  get_parameter,
								  set_selector,
								  block_selector,
								  obj_cmd_rs,
								  tmpl_cmd_get_serial_modem_configuration_rs,
								  parameter_selector) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }
  
  return (0);
}

int
ipmi_cmd_get_serial_modem_configuration_set_in_progress (ipmi_ctx_t ctx,
                                                         uint8_t channel_number,
                                                         uint8_t get_parameter,
                                                         uint8_t set_selector,
                                                         uint8_t block_selector,
                                                         fiid_obj_t obj_cmd_rs)
{
  if (_ipmi_cmd_get_serial_modem_configuration_parameters_common (ctx,
								  channel_number,
								  get_parameter,
								  set_selector,
								  block_selector,
								  obj_cmd_rs,
								  tmpl_cmd_get_serial_modem_configuration_set_in_progress_rs,
								  IPMI_SERIAL_MODEM_CONFIGURATION_PARAMETER_SET_IN_PROGRESS) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  return (0);
}

int
ipmi_cmd_get_serial_modem_configuration_connection_mode (ipmi_ctx_t ctx,
                                                         uint8_t channel_number,
                                                         uint8_t get_parameter,
                                                         uint8_t set_selector,
                                                         uint8_t block_selector,
                                                         fiid_obj_t obj_cmd_rs)
{
  if (_ipmi_cmd_get_serial_modem_configuration_parameters_common (ctx,
								  channel_number,
								  get_parameter,
								  set_selector,
								  block_selector,
								  obj_cmd_rs,
								  tmpl_cmd_get_serial_modem_configuration_connection_mode_rs,
								  IPMI_SERIAL_MODEM_CONFIGURATION_PARAMETER_CONNECTION_MODE) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  return (0);
}

int
ipmi_cmd_get_serial_modem_configuration_ipmi_messaging_comm_settings (ipmi_ctx_t ctx,
                                                                      uint8_t channel_number,
                                                                      uint8_t get_parameter,
                                                                      uint8_t set_selector,
                                                                      uint8_t block_selector,
                                                                      fiid_obj_t obj_cmd_rs)
{
  if (_ipmi_cmd_get_serial_modem_configuration_parameters_common (ctx,
								  channel_number,
								  get_parameter,
								  set_selector,
								  block_selector,
								  obj_cmd_rs,
								  tmpl_cmd_get_serial_modem_configuration_ipmi_messaging_comm_settings_rs,
								  IPMI_SERIAL_MODEM_CONFIGURATION_PARAMETER_IPMI_MESSAGING_COMM_SETTINGS) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  return (0);
}

int
ipmi_cmd_get_serial_modem_configuration_call_retry_interval (ipmi_ctx_t ctx,
                                                             uint8_t channel_number,
                                                             uint8_t get_parameter,
                                                             uint8_t set_selector,
                                                             uint8_t block_selector,
                                                             fiid_obj_t obj_cmd_rs)
{
  if (_ipmi_cmd_get_serial_modem_configuration_parameters_common (ctx,
								  channel_number,
								  get_parameter,
								  set_selector,
								  block_selector,
								  obj_cmd_rs,
								  tmpl_cmd_get_serial_modem_configuration_call_retry_interval_rs,
								  IPMI_SERIAL_MODEM_CONFIGURATION_PARAMETER_CALL_RETRY_INTERVAL) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  return (0);
}

int
ipmi_cmd_get_serial_modem_configuration_page_blackout_interval (ipmi_ctx_t ctx,
                                                                uint8_t channel_number,
                                                                uint8_t get_parameter,
                                                                uint8_t set_selector,
                                                                uint8_t block_selector,
                                                                fiid_obj_t obj_cmd_rs)
{
  if (_ipmi_cmd_get_serial_modem_configuration_parameters_common (ctx,
								  channel_number,
								  get_parameter,
								  set_selector,
								  block_selector,
								  obj_cmd_rs,
								  tmpl_cmd_get_serial_modem_configuration_page_blackout_interval_rs,
								  IPMI_SERIAL_MODEM_CONFIGURATION_PARAMETER_PAGE_BLACKOUT_INTERVAL) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  return (0);
}

