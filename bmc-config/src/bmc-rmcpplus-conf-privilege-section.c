#include "bmc-config.h"
#include "bmc-common.h"
#include "bmc-config-api.h"
#include "bmc-diff.h"
#include "bmc-map.h"
#include "bmc-sections.h"

static int
id_checkout (const struct bmc_config_arguments *args,
	     const struct section *sect,
	     struct keyvalue *kv,
	     int id)
{
  int ret;
  uint8_t priv;

  ret = get_rmcpplus_cipher_suite_id_privilege (args->dev,
						id,
						&priv);
  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);

  if (!(kv->value = strdup (rmcpplus_priv_string (priv))))
    {
      perror("strdup");
      return -1;
    }
  return 0;
}


static int
id_commit (const struct bmc_config_arguments *args,
	   const struct section *sect,
	   const struct keyvalue *kv,
	   int id)
{
  return set_rmcpplus_cipher_suite_id_privilege (args->dev,
						 id,
						 rmcpplus_priv_number (kv->value));
}

static int
id_diff (const struct bmc_config_arguments *args,
	 const struct section *sect,
	 const struct keyvalue *kv,
	 int id)
{
  int ret;
  uint8_t priv;

  ret = get_rmcpplus_cipher_suite_id_privilege (args->dev,
						id,
						&priv);
  if (ret != 0)
    return -1;

  if (same (kv->value, rmcpplus_priv_string (priv)))
    ret = 0;
  else 
    {
      ret = 1;
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   rmcpplus_priv_string (priv));
    }
  return ret;
}

static int
id_validate (const struct bmc_config_arguments *args,
	       const struct section *sect,
	       const char *value)
{
  return (rmcpplus_priv_number (value) >= 0) ? 0 : 1;
}


static int
id_0_checkout (const struct bmc_config_arguments *args,
	       const struct section *sect,
	       struct keyvalue *kv)
{
  return id_checkout (args, sect, kv, 0);
}

static int
id_0_commit (const struct bmc_config_arguments *args,
	     const struct section *sect,
	     const struct keyvalue *kv)
{
  return id_commit (args, sect, kv, 0);
}

static int
id_0_diff (const struct bmc_config_arguments *args,
	   const struct section *sect,
	   const struct keyvalue *kv)
{
  return id_diff (args, sect, kv, 0);
}



static int
id_1_checkout (const struct bmc_config_arguments *args,
	       const struct section *sect,
	       struct keyvalue *kv)
{
  return id_checkout (args, sect, kv, 1);
}

static int
id_1_commit (const struct bmc_config_arguments *args,
	     const struct section *sect,
	     const struct keyvalue *kv)
{
  return id_commit (args, sect, kv, 1);
}

static int
id_1_diff (const struct bmc_config_arguments *args,
	   const struct section *sect,
	   const struct keyvalue *kv)
{
  return id_diff (args, sect, kv, 1);
}


static int
id_2_checkout (const struct bmc_config_arguments *args,
	       const struct section *sect,
	       struct keyvalue *kv)
{
  return id_checkout (args, sect, kv, 2);
}

static int
id_2_commit (const struct bmc_config_arguments *args,
	     const struct section *sect,
	     const struct keyvalue *kv)
{
  return id_commit (args, sect, kv, 2);
}

static int
id_2_diff (const struct bmc_config_arguments *args,
	   const struct section *sect,
	   const struct keyvalue *kv)
{
  return id_diff (args, sect, kv, 2);
}



static int
id_3_checkout (const struct bmc_config_arguments *args,
	       const struct section *sect,
	       struct keyvalue *kv)
{
  return id_checkout (args, sect, kv, 3);
}

static int
id_3_commit (const struct bmc_config_arguments *args,
	     const struct section *sect,
	     const struct keyvalue *kv)
{
  return id_commit (args, sect, kv, 3);
}

static int
id_3_diff (const struct bmc_config_arguments *args,
	   const struct section *sect,
	   const struct keyvalue *kv)
{
  return id_diff (args, sect, kv, 3);
}



static int
id_4_checkout (const struct bmc_config_arguments *args,
	       const struct section *sect,
	       struct keyvalue *kv)
{
  return id_checkout (args, sect, kv, 4);
}

static int
id_4_commit (const struct bmc_config_arguments *args,
	     const struct section *sect,
	     const struct keyvalue *kv)
{
  return id_commit (args, sect, kv, 4);
}

static int
id_4_diff (const struct bmc_config_arguments *args,
	   const struct section *sect,
	   const struct keyvalue *kv)
{
  return id_diff (args, sect, kv, 4);
}



static int
id_5_checkout (const struct bmc_config_arguments *args,
	       const struct section *sect,
	       struct keyvalue *kv)
{
  return id_checkout (args, sect, kv, 5);
}

static int
id_5_commit (const struct bmc_config_arguments *args,
	     const struct section *sect,
	     const struct keyvalue *kv)
{
  return id_commit (args, sect, kv, 5);
}

static int
id_5_diff (const struct bmc_config_arguments *args,
	   const struct section *sect,
	   const struct keyvalue *kv)
{
  return id_diff (args, sect, kv, 5);
}



static int
id_6_checkout (const struct bmc_config_arguments *args,
	       const struct section *sect,
	       struct keyvalue *kv)
{
  return id_checkout (args, sect, kv, 6);
}

static int
id_6_commit (const struct bmc_config_arguments *args,
	     const struct section *sect,
	     const struct keyvalue *kv)
{
  return id_commit (args, sect, kv, 6);
}

static int
id_6_diff (const struct bmc_config_arguments *args,
	   const struct section *sect,
	   const struct keyvalue *kv)
{
  return id_diff (args, sect, kv, 6);
}


static int
id_7_checkout (const struct bmc_config_arguments *args,
	       const struct section *sect,
	       struct keyvalue *kv)
{
  return id_checkout (args, sect, kv, 7);
}

static int
id_7_commit (const struct bmc_config_arguments *args,
	     const struct section *sect,
	     const struct keyvalue *kv)
{
  return id_commit (args, sect, kv, 7);
}

static int
id_7_diff (const struct bmc_config_arguments *args,
	   const struct section *sect,
	   const struct keyvalue *kv)
{
  return id_diff (args, sect, kv, 7);
}


static int
id_8_checkout (const struct bmc_config_arguments *args,
	       const struct section *sect,
	       struct keyvalue *kv)
{
  return id_checkout (args, sect, kv, 8);
}

static int
id_8_commit (const struct bmc_config_arguments *args,
	     const struct section *sect,
	     const struct keyvalue *kv)
{
  return id_commit (args, sect, kv, 8);
}

static int
id_8_diff (const struct bmc_config_arguments *args,
	   const struct section *sect,
	   const struct keyvalue *kv)
{
  return id_diff (args, sect, kv, 8);
}


static int
id_9_checkout (const struct bmc_config_arguments *args,
	       const struct section *sect,
	       struct keyvalue *kv)
{
  return id_checkout (args, sect, kv, 9);
}

static int
id_9_commit (const struct bmc_config_arguments *args,
	     const struct section *sect,
	     const struct keyvalue *kv)
{
  return id_commit (args, sect, kv, 9);
}

static int
id_9_diff (const struct bmc_config_arguments *args,
	   const struct section *sect,
	   const struct keyvalue *kv)
{
  return id_diff (args, sect, kv, 9);
}



static int
id_10_checkout (const struct bmc_config_arguments *args,
		const struct section *sect,
		struct keyvalue *kv)
{
  return id_checkout (args, sect, kv, 10);
}

static int
id_10_commit (const struct bmc_config_arguments *args,
	      const struct section *sect,
	      const struct keyvalue *kv)
{
  return id_commit (args, sect, kv, 10);
}

static int
id_10_diff (const struct bmc_config_arguments *args,
	    const struct section *sect,
	    const struct keyvalue *kv)
{
  return id_diff (args, sect, kv, 10);
}



static int
id_11_checkout (const struct bmc_config_arguments *args,
		const struct section *sect,
		struct keyvalue *kv)
{
  return id_checkout (args, sect, kv, 11);
}

static int
id_11_commit (const struct bmc_config_arguments *args,
	      const struct section *sect,
	      const struct keyvalue *kv)
{
  return id_commit (args, sect, kv, 11);
}

static int
id_11_diff (const struct bmc_config_arguments *args,
	    const struct section *sect,
	    const struct keyvalue *kv)
{
  return id_diff (args, sect, kv, 11);
}



static int
id_12_checkout (const struct bmc_config_arguments *args,
		const struct section *sect,
		struct keyvalue *kv)
{
  return id_checkout (args, sect, kv, 12);
}

static int
id_12_commit (const struct bmc_config_arguments *args,
	      const struct section *sect,
	      const struct keyvalue *kv)
{
  return id_commit (args, sect, kv, 12);
}

static int
id_12_diff (const struct bmc_config_arguments *args,
	    const struct section *sect,
	    const struct keyvalue *kv)
{
  return id_diff (args, sect, kv, 12);
}



static int
id_13_checkout (const struct bmc_config_arguments *args,
		const struct section *sect,
		struct keyvalue *kv)
{
  return id_checkout (args, sect, kv, 13);
}

static int
id_13_commit (const struct bmc_config_arguments *args,
	      const struct section *sect,
	      const struct keyvalue *kv)
{
  return id_commit (args, sect, kv, 13);
}

static int
id_13_diff (const struct bmc_config_arguments *args,
	    const struct section *sect,
	    const struct keyvalue *kv)
{
  return id_diff (args, sect, kv, 13);
}


static int
id_14_checkout (const struct bmc_config_arguments *args,
		const struct section *sect,
		struct keyvalue *kv)
{
  return id_checkout (args, sect, kv, 14);
}

static int
id_14_commit (const struct bmc_config_arguments *args,
	      const struct section *sect,
	      const struct keyvalue *kv)
{
  return id_commit (args, sect, kv, 14);
}

static int
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
  
  if (!(rmcpplus_conf_privilege_section = (void *) calloc (1, sizeof (struct section))))
    {
      perror("calloc");
      return NULL;
    }
  if (!(rmcpplus_conf_privilege_section->section = strdup ("Rmcpplus_Conf_Privilege")))
    {
      perror("strdup");
      return NULL;
    }

  add_keyvalue (rmcpplus_conf_privilege_section,
		"Maximum_Privilege_Cipher_Suite_Id_0",
		"Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                0,
		id_0_checkout,
		id_0_commit,
		id_0_diff,
		id_validate);

  add_keyvalue (rmcpplus_conf_privilege_section,
		"Maximum_Privilege_Cipher_Suite_Id_1",
		"Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                0,
		id_1_checkout,
		id_1_commit,
		id_1_diff,
		id_validate);

  add_keyvalue (rmcpplus_conf_privilege_section,
		"Maximum_Privilege_Cipher_Suite_Id_2",
		"Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                0,
		id_2_checkout,
		id_2_commit,
		id_2_diff,
		id_validate);

  add_keyvalue (rmcpplus_conf_privilege_section,
		"Maximum_Privilege_Cipher_Suite_Id_3",
		"Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                0,
		id_3_checkout,
		id_3_commit,
		id_3_diff,
		id_validate);

  add_keyvalue (rmcpplus_conf_privilege_section,
		"Maximum_Privilege_Cipher_Suite_Id_4",
		"Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                0,
		id_4_checkout,
		id_4_commit,
		id_4_diff,
		id_validate);

  add_keyvalue (rmcpplus_conf_privilege_section,
		"Maximum_Privilege_Cipher_Suite_Id_5",
		"Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                0,
		id_5_checkout,
		id_5_commit,
		id_5_diff,
		id_validate);

  add_keyvalue (rmcpplus_conf_privilege_section,
		"Maximum_Privilege_Cipher_Suite_Id_6",
		"Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                0,
		id_6_checkout,
		id_6_commit,
		id_6_diff,
		id_validate);

  add_keyvalue (rmcpplus_conf_privilege_section,
		"Maximum_Privilege_Cipher_Suite_Id_7",
		"Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                0,
		id_7_checkout,
		id_7_commit,
		id_7_diff,
		id_validate);

  add_keyvalue (rmcpplus_conf_privilege_section,
		"Maximum_Privilege_Cipher_Suite_Id_8",
		"Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                0,
		id_8_checkout,
		id_8_commit,
		id_8_diff,
		id_validate);

  add_keyvalue (rmcpplus_conf_privilege_section,
		"Maximum_Privilege_Cipher_Suite_Id_9",
		"Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                0,
		id_9_checkout,
		id_9_commit,
		id_9_diff,
		id_validate);

  add_keyvalue (rmcpplus_conf_privilege_section,
		"Maximum_Privilege_Cipher_Suite_Id_10",
		"Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                0,
		id_10_checkout,
		id_10_commit,
		id_10_diff,
		id_validate);

  add_keyvalue (rmcpplus_conf_privilege_section,
		"Maximum_Privilege_Cipher_Suite_Id_11",
		"Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                0,
		id_11_checkout,
		id_11_commit,
		id_11_diff,
		id_validate);

  add_keyvalue (rmcpplus_conf_privilege_section,
		"Maximum_Privilege_Cipher_Suite_Id_12",
		"Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                0,
		id_12_checkout,
		id_12_commit,
		id_12_diff,
		id_validate);

  add_keyvalue (rmcpplus_conf_privilege_section,
		"Maximum_Privilege_Cipher_Suite_Id_13",
		"Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                0,
		id_13_checkout,
		id_13_commit,
		id_13_diff,
		id_validate);

  add_keyvalue (rmcpplus_conf_privilege_section,
		"Maximum_Privilege_Cipher_Suite_Id_14",
		"Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                0,
		id_14_checkout,
		id_14_commit,
		id_14_diff,
		id_validate);

  return rmcpplus_conf_privilege_section;
}

