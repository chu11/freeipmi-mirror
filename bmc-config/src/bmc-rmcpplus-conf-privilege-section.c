#include "bmc-config.h"
#include "bmc-common.h"
#include "bmc-config-api.h"
#include "bmc-diff.h"
#include "bmc-map.h"
#include "bmc-sections.h"
#include "bmc-validate.h"

static bmc_err_t
id_checkout (const struct bmc_config_arguments *args,
	     const struct section *sect,
	     struct keyvalue *kv,
	     int id)
{
  uint8_t priv;
  bmc_err_t ret;

  if ((ret = get_rmcpplus_cipher_suite_id_privilege (args->dev,
                                                     id,
                                                     &priv)) != BMC_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (!(kv->value = strdup (rmcpplus_priv_string (priv))))
    {
      perror("strdup");
      return BMC_ERR_FATAL_ERROR;
    }

  return BMC_ERR_SUCCESS;
}

static bmc_err_t
id_commit (const struct bmc_config_arguments *args,
	   const struct section *sect,
	   const struct keyvalue *kv,
	   int id)
{
  return set_rmcpplus_cipher_suite_id_privilege (args->dev,
						 id,
						 rmcpplus_priv_number (kv->value));
}

static bmc_diff_t
id_diff (const struct bmc_config_arguments *args,
	 const struct section *sect,
	 const struct keyvalue *kv,
	 int id)
{
  uint8_t priv;
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_rmcpplus_cipher_suite_id_privilege (args->dev,
                                                    id,
                                                    &priv)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  if (same (kv->value, rmcpplus_priv_string (priv)))
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   rmcpplus_priv_string (priv));
    }
  return ret;
}

static bmc_err_t
id_0_checkout (const struct bmc_config_arguments *args,
	       const struct section *sect,
	       struct keyvalue *kv)
{
  return id_checkout (args, sect, kv, 0);
}

static bmc_err_t
id_0_commit (const struct bmc_config_arguments *args,
	     const struct section *sect,
	     const struct keyvalue *kv)
{
  return id_commit (args, sect, kv, 0);
}

static bmc_diff_t
id_0_diff (const struct bmc_config_arguments *args,
	   const struct section *sect,
	   const struct keyvalue *kv)
{
  return id_diff (args, sect, kv, 0);
}

static bmc_err_t
id_1_checkout (const struct bmc_config_arguments *args,
	       const struct section *sect,
	       struct keyvalue *kv)
{
  return id_checkout (args, sect, kv, 1);
}

static bmc_err_t
id_1_commit (const struct bmc_config_arguments *args,
	     const struct section *sect,
	     const struct keyvalue *kv)
{
  return id_commit (args, sect, kv, 1);
}

static bmc_diff_t
id_1_diff (const struct bmc_config_arguments *args,
	   const struct section *sect,
	   const struct keyvalue *kv)
{
  return id_diff (args, sect, kv, 1);
}


static bmc_err_t
id_2_checkout (const struct bmc_config_arguments *args,
	       const struct section *sect,
	       struct keyvalue *kv)
{
  return id_checkout (args, sect, kv, 2);
}

static bmc_err_t
id_2_commit (const struct bmc_config_arguments *args,
	     const struct section *sect,
	     const struct keyvalue *kv)
{
  return id_commit (args, sect, kv, 2);
}

static bmc_diff_t
id_2_diff (const struct bmc_config_arguments *args,
	   const struct section *sect,
	   const struct keyvalue *kv)
{
  return id_diff (args, sect, kv, 2);
}



static bmc_err_t
id_3_checkout (const struct bmc_config_arguments *args,
	       const struct section *sect,
	       struct keyvalue *kv)
{
  return id_checkout (args, sect, kv, 3);
}

static bmc_err_t
id_3_commit (const struct bmc_config_arguments *args,
	     const struct section *sect,
	     const struct keyvalue *kv)
{
  return id_commit (args, sect, kv, 3);
}

static bmc_diff_t
id_3_diff (const struct bmc_config_arguments *args,
	   const struct section *sect,
	   const struct keyvalue *kv)
{
  return id_diff (args, sect, kv, 3);
}

static bmc_err_t
id_4_checkout (const struct bmc_config_arguments *args,
	       const struct section *sect,
	       struct keyvalue *kv)
{
  return id_checkout (args, sect, kv, 4);
}

static bmc_err_t
id_4_commit (const struct bmc_config_arguments *args,
	     const struct section *sect,
	     const struct keyvalue *kv)
{
  return id_commit (args, sect, kv, 4);
}

static bmc_diff_t
id_4_diff (const struct bmc_config_arguments *args,
	   const struct section *sect,
	   const struct keyvalue *kv)
{
  return id_diff (args, sect, kv, 4);
}



static bmc_err_t
id_5_checkout (const struct bmc_config_arguments *args,
	       const struct section *sect,
	       struct keyvalue *kv)
{
  return id_checkout (args, sect, kv, 5);
}

static bmc_err_t
id_5_commit (const struct bmc_config_arguments *args,
	     const struct section *sect,
	     const struct keyvalue *kv)
{
  return id_commit (args, sect, kv, 5);
}

static bmc_diff_t
id_5_diff (const struct bmc_config_arguments *args,
	   const struct section *sect,
	   const struct keyvalue *kv)
{
  return id_diff (args, sect, kv, 5);
}

static bmc_err_t
id_6_checkout (const struct bmc_config_arguments *args,
	       const struct section *sect,
	       struct keyvalue *kv)
{
  return id_checkout (args, sect, kv, 6);
}

static bmc_err_t
id_6_commit (const struct bmc_config_arguments *args,
	     const struct section *sect,
	     const struct keyvalue *kv)
{
  return id_commit (args, sect, kv, 6);
}

static bmc_diff_t
id_6_diff (const struct bmc_config_arguments *args,
	   const struct section *sect,
	   const struct keyvalue *kv)
{
  return id_diff (args, sect, kv, 6);
}

static bmc_err_t
id_7_checkout (const struct bmc_config_arguments *args,
	       const struct section *sect,
	       struct keyvalue *kv)
{
  return id_checkout (args, sect, kv, 7);
}

static bmc_err_t
id_7_commit (const struct bmc_config_arguments *args,
	     const struct section *sect,
	     const struct keyvalue *kv)
{
  return id_commit (args, sect, kv, 7);
}

static bmc_diff_t
id_7_diff (const struct bmc_config_arguments *args,
	   const struct section *sect,
	   const struct keyvalue *kv)
{
  return id_diff (args, sect, kv, 7);
}


static bmc_err_t
id_8_checkout (const struct bmc_config_arguments *args,
	       const struct section *sect,
	       struct keyvalue *kv)
{
  return id_checkout (args, sect, kv, 8);
}

static bmc_err_t
id_8_commit (const struct bmc_config_arguments *args,
	     const struct section *sect,
	     const struct keyvalue *kv)
{
  return id_commit (args, sect, kv, 8);
}

static bmc_diff_t
id_8_diff (const struct bmc_config_arguments *args,
	   const struct section *sect,
	   const struct keyvalue *kv)
{
  return id_diff (args, sect, kv, 8);
}

static bmc_err_t
id_9_checkout (const struct bmc_config_arguments *args,
	       const struct section *sect,
	       struct keyvalue *kv)
{
  return id_checkout (args, sect, kv, 9);
}

static bmc_err_t
id_9_commit (const struct bmc_config_arguments *args,
	     const struct section *sect,
	     const struct keyvalue *kv)
{
  return id_commit (args, sect, kv, 9);
}

static bmc_diff_t
id_9_diff (const struct bmc_config_arguments *args,
	   const struct section *sect,
	   const struct keyvalue *kv)
{
  return id_diff (args, sect, kv, 9);
}

static bmc_err_t
id_10_checkout (const struct bmc_config_arguments *args,
		const struct section *sect,
		struct keyvalue *kv)
{
  return id_checkout (args, sect, kv, 10);
}

static bmc_err_t
id_10_commit (const struct bmc_config_arguments *args,
	      const struct section *sect,
	      const struct keyvalue *kv)
{
  return id_commit (args, sect, kv, 10);
}

static bmc_diff_t
id_10_diff (const struct bmc_config_arguments *args,
	    const struct section *sect,
	    const struct keyvalue *kv)
{
  return id_diff (args, sect, kv, 10);
}

static bmc_err_t
id_11_checkout (const struct bmc_config_arguments *args,
		const struct section *sect,
		struct keyvalue *kv)
{
  return id_checkout (args, sect, kv, 11);
}

static bmc_err_t
id_11_commit (const struct bmc_config_arguments *args,
	      const struct section *sect,
	      const struct keyvalue *kv)
{
  return id_commit (args, sect, kv, 11);
}

static bmc_diff_t
id_11_diff (const struct bmc_config_arguments *args,
	    const struct section *sect,
	    const struct keyvalue *kv)
{
  return id_diff (args, sect, kv, 11);
}

static bmc_err_t
id_12_checkout (const struct bmc_config_arguments *args,
		const struct section *sect,
		struct keyvalue *kv)
{
  return id_checkout (args, sect, kv, 12);
}

static bmc_err_t
id_12_commit (const struct bmc_config_arguments *args,
	      const struct section *sect,
	      const struct keyvalue *kv)
{
  return id_commit (args, sect, kv, 12);
}

static bmc_diff_t
id_12_diff (const struct bmc_config_arguments *args,
	    const struct section *sect,
	    const struct keyvalue *kv)
{
  return id_diff (args, sect, kv, 12);
}

static bmc_err_t
id_13_checkout (const struct bmc_config_arguments *args,
		const struct section *sect,
		struct keyvalue *kv)
{
  return id_checkout (args, sect, kv, 13);
}

static bmc_err_t
id_13_commit (const struct bmc_config_arguments *args,
	      const struct section *sect,
	      const struct keyvalue *kv)
{
  return id_commit (args, sect, kv, 13);
}

static bmc_diff_t
id_13_diff (const struct bmc_config_arguments *args,
	    const struct section *sect,
	    const struct keyvalue *kv)
{
  return id_diff (args, sect, kv, 13);
}

static bmc_err_t
id_14_checkout (const struct bmc_config_arguments *args,
		const struct section *sect,
		struct keyvalue *kv)
{
  return id_checkout (args, sect, kv, 14);
}

static bmc_err_t
id_14_commit (const struct bmc_config_arguments *args,
	      const struct section *sect,
	      const struct keyvalue *kv)
{
  return id_commit (args, sect, kv, 14);
}

static bmc_diff_t
id_14_diff (const struct bmc_config_arguments *args,
	    const struct section *sect,
	    const struct keyvalue *kv)
{
  return id_diff (args, sect, kv, 14);
}

struct section *
bmc_rmcpplus_conf_privilege_section_get (struct bmc_config_arguments *args)
{
  struct section *rmcpplus_conf_privilege_section = NULL;

  if (!(rmcpplus_conf_privilege_section = bmc_section_create ("Rmcpplus_Conf_Privilege")))
    goto cleanup;

  if (bmc_section_add_keyvalue (rmcpplus_conf_privilege_section,
				"Maximum_Privilege_Cipher_Suite_Id_0",
				"Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
				0,
				id_0_checkout,
				id_0_commit,
				id_0_diff,
				rmcpplus_priv_number_validate) < 0)
    goto cleanup;

  if (bmc_section_add_keyvalue (rmcpplus_conf_privilege_section,
				"Maximum_Privilege_Cipher_Suite_Id_1",
				"Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
				0,
				id_1_checkout,
				id_1_commit,
				id_1_diff,
				rmcpplus_priv_number_validate) < 0)
    goto cleanup;

  if (bmc_section_add_keyvalue (rmcpplus_conf_privilege_section,
				"Maximum_Privilege_Cipher_Suite_Id_2",
				"Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
				0,
				id_2_checkout,
				id_2_commit,
				id_2_diff,
				rmcpplus_priv_number_validate) < 0)
    goto cleanup;

  if (bmc_section_add_keyvalue (rmcpplus_conf_privilege_section,
				"Maximum_Privilege_Cipher_Suite_Id_3",
				"Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
				0,
				id_3_checkout,
				id_3_commit,
				id_3_diff,
				rmcpplus_priv_number_validate) < 0)
    goto cleanup;

  if (bmc_section_add_keyvalue (rmcpplus_conf_privilege_section,
				"Maximum_Privilege_Cipher_Suite_Id_4",
				"Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
				0,
				id_4_checkout,
				id_4_commit,
				id_4_diff,
				rmcpplus_priv_number_validate) < 0)
    goto cleanup;

  if (bmc_section_add_keyvalue (rmcpplus_conf_privilege_section,
				"Maximum_Privilege_Cipher_Suite_Id_5",
				"Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
				0,
				id_5_checkout,
				id_5_commit,
				id_5_diff,
				rmcpplus_priv_number_validate) < 0)
    goto cleanup;

  if (bmc_section_add_keyvalue (rmcpplus_conf_privilege_section,
				"Maximum_Privilege_Cipher_Suite_Id_6",
				"Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
				0,
				id_6_checkout,
				id_6_commit,
				id_6_diff,
				rmcpplus_priv_number_validate) < 0)
    goto cleanup;

  if (bmc_section_add_keyvalue (rmcpplus_conf_privilege_section,
				"Maximum_Privilege_Cipher_Suite_Id_7",
				"Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
				0,
				id_7_checkout,
				id_7_commit,
				id_7_diff,
				rmcpplus_priv_number_validate) < 0)
    goto cleanup;

  if (bmc_section_add_keyvalue (rmcpplus_conf_privilege_section,
				"Maximum_Privilege_Cipher_Suite_Id_8",
				"Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
				0,
				id_8_checkout,
				id_8_commit,
				id_8_diff,
				rmcpplus_priv_number_validate) < 0)
    goto cleanup;

  if (bmc_section_add_keyvalue (rmcpplus_conf_privilege_section,
				"Maximum_Privilege_Cipher_Suite_Id_9",
				"Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
				0,
				id_9_checkout,
				id_9_commit,
				id_9_diff,
				rmcpplus_priv_number_validate) < 0)
    goto cleanup;

  if (bmc_section_add_keyvalue (rmcpplus_conf_privilege_section,
				"Maximum_Privilege_Cipher_Suite_Id_10",
				"Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
				0,
				id_10_checkout,
				id_10_commit,
				id_10_diff,
				rmcpplus_priv_number_validate) < 0)
    goto cleanup;

  if (bmc_section_add_keyvalue (rmcpplus_conf_privilege_section,
				"Maximum_Privilege_Cipher_Suite_Id_11",
				"Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
				0,
				id_11_checkout,
				id_11_commit,
				id_11_diff,
				rmcpplus_priv_number_validate) < 0)
    goto cleanup;

  if (bmc_section_add_keyvalue (rmcpplus_conf_privilege_section,
				"Maximum_Privilege_Cipher_Suite_Id_12",
				"Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
				0,
				id_12_checkout,
				id_12_commit,
				id_12_diff,
				rmcpplus_priv_number_validate) < 0)
    goto cleanup;

  if (bmc_section_add_keyvalue (rmcpplus_conf_privilege_section,
				"Maximum_Privilege_Cipher_Suite_Id_13",
				"Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
				0,
				id_13_checkout,
				id_13_commit,
				id_13_diff,
				rmcpplus_priv_number_validate) < 0)
    goto cleanup;

  if (bmc_section_add_keyvalue (rmcpplus_conf_privilege_section,
				"Maximum_Privilege_Cipher_Suite_Id_14",
				"Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
				0,
				id_14_checkout,
				id_14_commit,
				id_14_diff,
				rmcpplus_priv_number_validate) < 0)
    goto cleanup;

  return rmcpplus_conf_privilege_section;

 cleanup:
  if (rmcpplus_conf_privilege_section)
    bmc_section_destroy (rmcpplus_conf_privilege_section);
  return NULL;
}

