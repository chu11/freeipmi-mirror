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
id_checkout (const struct config_section *section,
	     struct config_keyvalue *kv,
             void *arg,
	     int id)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  uint8_t priv;
  config_err_t ret;

  if ((ret = get_rmcpplus_cipher_suite_id_privilege (state_data,
                                                     id,
                                                     &priv)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (!(kv->value = strdup (rmcpplus_priv_string (priv))))
    {
      perror("strdup");
      return CONFIG_ERR_FATAL_ERROR;
    }

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
id_commit (const struct config_section *section,
	   const struct config_keyvalue *kv,
           void *arg,
	   int id)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  return set_rmcpplus_cipher_suite_id_privilege (state_data,
						 id,
						 rmcpplus_priv_number (kv->value));
}

static config_diff_t
id_diff (const struct config_section *section,
	 const struct config_keyvalue *kv,
         void *arg,
	 int id)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  uint8_t priv;
  config_err_t rc;
  config_diff_t ret;

  if ((rc = get_rmcpplus_cipher_suite_id_privilege (state_data,
                                                    id,
                                                    &priv)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return CONFIG_DIFF_NON_FATAL_ERROR;
      return CONFIG_DIFF_FATAL_ERROR;
    }

  if (same (kv->value, rmcpplus_priv_string (priv)))
    ret = CONFIG_DIFF_SAME;
  else 
    {
      ret = CONFIG_DIFF_DIFFERENT;
      report_diff (section->section_name,
                   kv->key_name,
                   kv->value,
                   rmcpplus_priv_string (priv));
    }
  return ret;
}

static config_err_t
id_checkout_cb (const struct config_section *section,
                struct config_keyvalue *kv,
                void *arg)
{
  uint8_t id = atoi (kv->key_name + strlen("Maximum_Privilege_Cipher_Suite_Id_"));
  return id_checkout (section, kv, arg, id);
}

static config_err_t
id_commit_cb (const struct config_section *section,
              const struct config_keyvalue *kv,
              void *arg)
{
  uint8_t id = atoi (kv->key_name + strlen("Maximum_Privilege_Cipher_Suite_Id_"));
  return id_commit (section, kv, arg, id);
}

static config_diff_t
id_diff_cb (const struct config_section *section,
            const struct config_keyvalue *kv,
            void *arg)
{ 
  uint8_t id = atoi (kv->key_name + strlen("Maximum_Privilege_Cipher_Suite_Id_"));
  return id_diff (section, kv, arg, id);
}

struct config_section *
bmc_config_rmcpplus_conf_privilege_section_get (bmc_config_state_data_t *state_data)
{
  struct config_section *rmcpplus_conf_privilege_section = NULL;
  char *section_comment = 
    "If your system supports IPMI 2.0 and Serial-over-LAN (SOL),"
    "cipher suite IDs may be configurable below.  In the "
    "Rmcpplus_Conf_Privilege section, maximum user privilege levels "
    "allowed for authentication under IPMI 2.0 (including Serial-over-LAN) "
    "are set for each supported cipher suite ID.  Each cipher suite ID "
    "supports different sets of authentication, integrity, and encryption "
    "algorithms for IPMI 2.0.  Typically, the highest privilege level any "
    "username configured should set for support under a cipher suite ID. "
    "This is typically \"Administrator\".";

  if (!(rmcpplus_conf_privilege_section = config_section_create ("Rmcpplus_Conf_Privilege",
                                                                     "Rmcpplus_Conf_Privilege",
                                                                     section_comment,
                                                                     0)))
    goto cleanup;

  if (config_section_add_keyvalue (rmcpplus_conf_privilege_section,
                                       "Maximum_Privilege_Cipher_Suite_Id_0",
                                       "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                                       0,
                                       id_checkout_cb,
                                       id_commit_cb,
                                       id_diff_cb,
                                       rmcpplus_priv_number_validate) < 0)
    goto cleanup;

  if (config_section_add_keyvalue (rmcpplus_conf_privilege_section,
                                       "Maximum_Privilege_Cipher_Suite_Id_1",
                                       "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                                       0,
                                       id_checkout_cb,
                                       id_commit_cb,
                                       id_diff_cb,
                                       rmcpplus_priv_number_validate) < 0)
    goto cleanup;

  if (config_section_add_keyvalue (rmcpplus_conf_privilege_section,
                                       "Maximum_Privilege_Cipher_Suite_Id_2",
                                       "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                                       0,
                                       id_checkout_cb,
                                       id_commit_cb,
                                       id_diff_cb,
                                       rmcpplus_priv_number_validate) < 0)
    goto cleanup;

  if (config_section_add_keyvalue (rmcpplus_conf_privilege_section,
                                       "Maximum_Privilege_Cipher_Suite_Id_3",
                                       "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                                       0,
                                       id_checkout_cb,
                                       id_commit_cb,
                                       id_diff_cb,
                                       rmcpplus_priv_number_validate) < 0)
    goto cleanup;

  if (config_section_add_keyvalue (rmcpplus_conf_privilege_section,
                                       "Maximum_Privilege_Cipher_Suite_Id_4",
                                       "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                                       0,
                                       id_checkout_cb,
                                       id_commit_cb,
                                       id_diff_cb,
                                       rmcpplus_priv_number_validate) < 0)
    goto cleanup;

  if (config_section_add_keyvalue (rmcpplus_conf_privilege_section,
                                       "Maximum_Privilege_Cipher_Suite_Id_5",
                                       "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                                       0,
                                       id_checkout_cb,
                                       id_commit_cb,
                                       id_diff_cb,
                                       rmcpplus_priv_number_validate) < 0)
    goto cleanup;

  if (config_section_add_keyvalue (rmcpplus_conf_privilege_section,
                                       "Maximum_Privilege_Cipher_Suite_Id_6",
                                       "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                                       0,
                                       id_checkout_cb,
                                       id_commit_cb,
                                       id_diff_cb,
                                       rmcpplus_priv_number_validate) < 0)
    goto cleanup;

  if (config_section_add_keyvalue (rmcpplus_conf_privilege_section,
                                       "Maximum_Privilege_Cipher_Suite_Id_7",
                                       "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                                       0,
                                       id_checkout_cb,
                                       id_commit_cb,
                                       id_diff_cb,
                                       rmcpplus_priv_number_validate) < 0)
    goto cleanup;

  if (config_section_add_keyvalue (rmcpplus_conf_privilege_section,
                                       "Maximum_Privilege_Cipher_Suite_Id_8",
                                       "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                                       0,
                                       id_checkout_cb,
                                       id_commit_cb,
                                       id_diff_cb,
                                       rmcpplus_priv_number_validate) < 0)
    goto cleanup;

  if (config_section_add_keyvalue (rmcpplus_conf_privilege_section,
                                       "Maximum_Privilege_Cipher_Suite_Id_9",
                                       "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                                       0,
                                       id_checkout_cb,
                                       id_commit_cb,
                                       id_diff_cb,
                                       rmcpplus_priv_number_validate) < 0)
    goto cleanup;

  if (config_section_add_keyvalue (rmcpplus_conf_privilege_section,
                                       "Maximum_Privilege_Cipher_Suite_Id_10",
                                       "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                                       0,
                                       id_checkout_cb,
                                       id_commit_cb,
                                       id_diff_cb,
                                       rmcpplus_priv_number_validate) < 0)
    goto cleanup;

  if (config_section_add_keyvalue (rmcpplus_conf_privilege_section,
                                       "Maximum_Privilege_Cipher_Suite_Id_11",
                                       "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                                       0,
                                       id_checkout_cb,
                                       id_commit_cb,
                                       id_diff_cb,
                                       rmcpplus_priv_number_validate) < 0)
    goto cleanup;

  if (config_section_add_keyvalue (rmcpplus_conf_privilege_section,
                                       "Maximum_Privilege_Cipher_Suite_Id_12",
                                       "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                                       0,
                                       id_checkout_cb,
                                       id_commit_cb,
                                       id_diff_cb,
                                       rmcpplus_priv_number_validate) < 0)
    goto cleanup;

  if (config_section_add_keyvalue (rmcpplus_conf_privilege_section,
                                       "Maximum_Privilege_Cipher_Suite_Id_13",
                                       "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                                       0,
                                       id_checkout_cb,
                                       id_commit_cb,
                                       id_diff_cb,
                                       rmcpplus_priv_number_validate) < 0)
    goto cleanup;

  if (config_section_add_keyvalue (rmcpplus_conf_privilege_section,
                                       "Maximum_Privilege_Cipher_Suite_Id_14",
                                       "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                                       0,
                                       id_checkout_cb,
                                       id_commit_cb,
                                       id_diff_cb,
                                       rmcpplus_priv_number_validate) < 0)
    goto cleanup;

  return rmcpplus_conf_privilege_section;

 cleanup:
  if (rmcpplus_conf_privilege_section)
    config_section_destroy(rmcpplus_conf_privilege_section);
  return NULL;
}

