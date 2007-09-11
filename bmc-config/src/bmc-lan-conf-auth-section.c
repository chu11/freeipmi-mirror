#include "bmc-config.h"
#include "bmc-config-common.h"
#include "bmc-config-wrapper.h"
#include "bmc-config-diff.h"
#include "bmc-config-sections.h"
#include "bmc-config-validate.h"

struct bmc_authentication_level {
  uint8_t callback_level_none;
  uint8_t callback_level_md2;
  uint8_t callback_level_md5;
  uint8_t callback_level_straight_password;
  uint8_t callback_level_oem_proprietary;
  uint8_t user_level_none;
  uint8_t user_level_md2;
  uint8_t user_level_md5;
  uint8_t user_level_straight_password;
  uint8_t user_level_oem_proprietary;
  uint8_t operator_level_none;
  uint8_t operator_level_md2;
  uint8_t operator_level_md5;
  uint8_t operator_level_straight_password;
  uint8_t operator_level_oem_proprietary;
  uint8_t admin_level_none;
  uint8_t admin_level_md2;
  uint8_t admin_level_md5;
  uint8_t admin_level_straight_password;
  uint8_t admin_level_oem_proprietary;
  uint8_t oem_level_none;
  uint8_t oem_level_md2;
  uint8_t oem_level_md5;
  uint8_t oem_level_straight_password;
  uint8_t oem_level_oem_proprietary;
};

static bmc_err_t
_authentication_level_checkout (bmc_config_state_data_t *state_data,
                                const struct section *sect,
                                struct keyvalue *kv,
                                struct bmc_authentication_level *al,
                                uint8_t *desired_authentication_level)
{
  bmc_err_t ret;
  
  if ((ret = get_bmc_lan_conf_authentication_type_enables (state_data,
                                                           &(al->callback_level_none),
                                                           &(al->callback_level_md2),
                                                           &(al->callback_level_md5),
                                                           &(al->callback_level_straight_password),
                                                           &(al->callback_level_oem_proprietary),
                                                           &(al->user_level_none),
                                                           &(al->user_level_md2),
                                                           &(al->user_level_md5),
                                                           &(al->user_level_straight_password),
                                                           &(al->user_level_oem_proprietary),
                                                           &(al->operator_level_none),
                                                           &(al->operator_level_md2),
                                                           &(al->operator_level_md5),
                                                           &(al->operator_level_straight_password),
                                                           &(al->operator_level_oem_proprietary),
                                                           &(al->admin_level_none),
                                                           &(al->admin_level_md2),
                                                           &(al->admin_level_md5),
                                                           &(al->admin_level_straight_password),
                                                           &(al->admin_level_oem_proprietary),
                                                           &(al->oem_level_none),
                                                           &(al->oem_level_md2),
                                                           &(al->oem_level_md5),
                                                           &(al->oem_level_straight_password),
                                                           &(al->oem_level_oem_proprietary))) != BMC_ERR_SUCCESS)
    return ret;
  
  if (kv->value)
    free (kv->value);
  
  if (*desired_authentication_level)
    {
      if (!(kv->value = strdup ("Yes")))
        {
          perror("strdup");
          return BMC_ERR_FATAL_ERROR;
        }
    }
  else
    {
      if (!(kv->value = strdup ("No")))
        {
          perror("strdup");
          return BMC_ERR_FATAL_ERROR;
        }
    }

  return BMC_ERR_SUCCESS;
}

static bmc_err_t
_authentication_level_commit (bmc_config_state_data_t *state_data,
                              const struct section *sect,
                              const struct keyvalue *kv,
                              struct bmc_authentication_level *al,
                              uint8_t *desired_authentication_level)
{
  bmc_err_t ret;
  
  if ((ret = get_bmc_lan_conf_authentication_type_enables (state_data,
                                                           &(al->callback_level_none),
                                                           &(al->callback_level_md2),
                                                           &(al->callback_level_md5),
                                                           &(al->callback_level_straight_password),
                                                           &(al->callback_level_oem_proprietary),
                                                           &(al->user_level_none),
                                                           &(al->user_level_md2),
                                                           &(al->user_level_md5),
                                                           &(al->user_level_straight_password),
                                                           &(al->user_level_oem_proprietary),
                                                           &(al->operator_level_none),
                                                           &(al->operator_level_md2),
                                                           &(al->operator_level_md5),
                                                           &(al->operator_level_straight_password),
                                                           &(al->operator_level_oem_proprietary),
                                                           &(al->admin_level_none),
                                                           &(al->admin_level_md2),
                                                           &(al->admin_level_md5),
                                                           &(al->admin_level_straight_password),
                                                           &(al->admin_level_oem_proprietary),
                                                           &(al->oem_level_none),
                                                           &(al->oem_level_md2),
                                                           &(al->oem_level_md5),
                                                           &(al->oem_level_straight_password),
                                                           &(al->oem_level_oem_proprietary))) != BMC_ERR_SUCCESS)
    return ret;
  
  *desired_authentication_level = same (kv->value, "yes");

  if ((ret = set_bmc_lan_conf_authentication_type_enables (state_data,
                                                           al->callback_level_none,
                                                           al->callback_level_md2,
                                                           al->callback_level_md5,
                                                           al->callback_level_straight_password,
                                                           al->callback_level_oem_proprietary,
                                                           al->user_level_none,
                                                           al->user_level_md2,
                                                           al->user_level_md5,
                                                           al->user_level_straight_password,
                                                           al->user_level_oem_proprietary,
                                                           al->operator_level_none,
                                                           al->operator_level_md2,
                                                           al->operator_level_md5,
                                                           al->operator_level_straight_password,
                                                           al->operator_level_oem_proprietary,
                                                           al->admin_level_none,
                                                           al->admin_level_md2,
                                                           al->admin_level_md5,
                                                           al->admin_level_straight_password,
                                                           al->admin_level_oem_proprietary,
                                                           al->oem_level_none,
                                                           al->oem_level_md2,
                                                           al->oem_level_md5,
                                                           al->oem_level_straight_password,
                                                           al->oem_level_oem_proprietary)) != BMC_ERR_SUCCESS)
    return ret;

  return BMC_ERR_SUCCESS;
}

static bmc_diff_t
_authentication_level_diff (bmc_config_state_data_t *state_data,
                            const struct section *sect,
                            const struct keyvalue *kv,
                            struct bmc_authentication_level *al,
                            uint8_t *desired_authentication_level)
{
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_bmc_lan_conf_authentication_type_enables (state_data,
                                                          &(al->callback_level_none),
                                                          &(al->callback_level_md2),
                                                          &(al->callback_level_md5),
                                                          &(al->callback_level_straight_password),
                                                          &(al->callback_level_oem_proprietary),
                                                          &(al->user_level_none),
                                                          &(al->user_level_md2),
                                                          &(al->user_level_md5),
                                                          &(al->user_level_straight_password),
                                                          &(al->user_level_oem_proprietary),
                                                          &(al->operator_level_none),
                                                          &(al->operator_level_md2),
                                                          &(al->operator_level_md5),
                                                          &(al->operator_level_straight_password),
                                                          &(al->operator_level_oem_proprietary),
                                                          &(al->admin_level_none),
                                                          &(al->admin_level_md2),
                                                          &(al->admin_level_md5),
                                                          &(al->admin_level_straight_password),
                                                          &(al->admin_level_oem_proprietary),
                                                          &(al->oem_level_none),
                                                          &(al->oem_level_md2),
                                                          &(al->oem_level_md5),
                                                          &(al->oem_level_straight_password),
                                                          &(al->oem_level_oem_proprietary))) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }
  
  if (*desired_authentication_level == same (kv->value, "yes")) 
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   *desired_authentication_level ? "Yes" : "No");
    }

  return ret;
}

/* callback_none */

static bmc_err_t
callback_none_checkout (bmc_config_state_data_t *state_data,
			const struct section *sect,
			struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_checkout (state_data,
                                         sect,
                                         kv,
                                         &auth,
                                         &(auth.callback_level_none));
}

static bmc_err_t
callback_none_commit (bmc_config_state_data_t *state_data,
		      const struct section *sect,
		      const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_commit (state_data,
                                       sect,
                                       kv,
                                       &auth,
                                       &(auth.callback_level_none));
}

static bmc_diff_t
callback_none_diff (bmc_config_state_data_t *state_data,
		    const struct section *sect,
		    const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_diff (state_data,
                                     sect,
                                     kv,
                                     &auth,
                                     &(auth.callback_level_none));
}

/* callback_md2 */

static bmc_err_t
callback_md2_checkout (bmc_config_state_data_t *state_data,
                       const struct section *sect,
                       struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_checkout (state_data,
                                         sect,
                                         kv,
                                         &auth,
                                         &(auth.callback_level_md2));
}

static bmc_err_t
callback_md2_commit (bmc_config_state_data_t *state_data,
                     const struct section *sect,
                     const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_commit (state_data,
                                       sect,
                                       kv,
                                       &auth,
                                       &(auth.callback_level_md2));
}

static bmc_diff_t
callback_md2_diff (bmc_config_state_data_t *state_data,
                   const struct section *sect,
                   const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_diff (state_data,
                                     sect,
                                     kv,
                                     &auth,
                                     &(auth.callback_level_md2));
}

/* callback_md5 */

static bmc_err_t
callback_md5_checkout (bmc_config_state_data_t *state_data,
                       const struct section *sect,
                       struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_checkout (state_data,
                                         sect,
                                         kv,
                                         &auth,
                                         &(auth.callback_level_md5));
}

static bmc_err_t
callback_md5_commit (bmc_config_state_data_t *state_data,
                     const struct section *sect,
                     const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_commit (state_data,
                                       sect,
                                       kv,
                                       &auth,
                                       &(auth.callback_level_md5));
}

static bmc_diff_t
callback_md5_diff (bmc_config_state_data_t *state_data,
                   const struct section *sect,
                   const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_diff (state_data,
                                     sect,
                                     kv,
                                     &auth,
                                     &(auth.callback_level_md5));
}

/* callback_straight_password */

static bmc_err_t
callback_straight_password_checkout (bmc_config_state_data_t *state_data,
				     const struct section *sect,
				     struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_checkout (state_data,
                                         sect,
                                         kv,
                                         &auth,
                                         &(auth.callback_level_straight_password));
}

static bmc_err_t
callback_straight_password_commit (bmc_config_state_data_t *state_data,
				   const struct section *sect,
				   const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_commit (state_data,
                                       sect,
                                       kv,
                                       &auth,
                                       &(auth.callback_level_straight_password));
}

static bmc_diff_t
callback_straight_password_diff (bmc_config_state_data_t *state_data,
				 const struct section *sect,
				 const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_diff (state_data,
                                     sect,
                                     kv,
                                     &auth,
                                     &(auth.callback_level_straight_password));
}

/* callback_oem_proprietary */

static bmc_err_t
callback_oem_proprietary_checkout (bmc_config_state_data_t *state_data,
				   const struct section *sect,
				   struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_checkout (state_data,
                                         sect,
                                         kv,
                                         &auth,
                                         &(auth.callback_level_oem_proprietary));
}

static bmc_err_t
callback_oem_proprietary_commit (bmc_config_state_data_t *state_data,
				 const struct section *sect,
				 const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_commit (state_data,
                                       sect,
                                       kv,
                                       &auth,
                                       &(auth.callback_level_oem_proprietary));
}

static bmc_diff_t
callback_oem_proprietary_diff (bmc_config_state_data_t *state_data,
			       const struct section *sect,
			       const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_diff (state_data,
                                     sect,
                                     kv,
                                     &auth,
                                     &(auth.callback_level_oem_proprietary));
}

/* user */

/* user_none */

static bmc_err_t
user_none_checkout (bmc_config_state_data_t *state_data,
		    const struct section *sect,
		    struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_checkout (state_data,
                                         sect,
                                         kv,
                                         &auth,
                                         &(auth.user_level_none));
}

static bmc_err_t
user_none_commit (bmc_config_state_data_t *state_data,
		  const struct section *sect,
		  const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_commit (state_data,
                                       sect,
                                       kv,
                                       &auth,
                                       &(auth.user_level_none));
}

static bmc_diff_t
user_none_diff (bmc_config_state_data_t *state_data,
		const struct section *sect,
		const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_diff (state_data,
                                     sect,
                                     kv,
                                     &auth,
                                     &(auth.user_level_none));
}

/* user_md2 */

static bmc_err_t
user_md2_checkout (bmc_config_state_data_t *state_data,
		   const struct section *sect,
		   struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_checkout (state_data,
                                         sect,
                                         kv,
                                         &auth,
                                         &(auth.user_level_md2));
}

static bmc_err_t
user_md2_commit (bmc_config_state_data_t *state_data,
		 const struct section *sect,
		 const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_commit (state_data,
                                       sect,
                                       kv,
                                       &auth,
                                       &(auth.user_level_md2));
}

static bmc_diff_t
user_md2_diff (bmc_config_state_data_t *state_data,
	       const struct section *sect,
	       const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_diff (state_data,
                                     sect,
                                     kv,
                                     &auth,
                                     &(auth.user_level_md2));
}

/* user_md5 */

static bmc_err_t
user_md5_checkout (bmc_config_state_data_t *state_data,
		   const struct section *sect,
		   struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_checkout (state_data,
                                         sect,
                                         kv,
                                         &auth,
                                         &(auth.user_level_md5));
}

static bmc_err_t
user_md5_commit (bmc_config_state_data_t *state_data,
		 const struct section *sect,
		 const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_commit (state_data,
                                       sect,
                                       kv,
                                       &auth,
                                       &(auth.user_level_md5));
}

static bmc_diff_t
user_md5_diff (bmc_config_state_data_t *state_data,
	       const struct section *sect,
	       const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_diff (state_data,
                                     sect,
                                     kv,
                                     &auth,
                                     &(auth.user_level_md5));
}

/* user_straight_password */

static bmc_err_t
user_straight_password_checkout (bmc_config_state_data_t *state_data,
				 const struct section *sect,
				 struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_checkout (state_data,
                                         sect,
                                         kv,
                                         &auth,
                                         &(auth.user_level_straight_password));
}

static bmc_err_t
user_straight_password_commit (bmc_config_state_data_t *state_data,
			       const struct section *sect,
			       const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_commit (state_data,
                                       sect,
                                       kv,
                                       &auth,
                                       &(auth.user_level_straight_password));
}

static bmc_diff_t
user_straight_password_diff (bmc_config_state_data_t *state_data,
			     const struct section *sect,
			     const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_diff (state_data,
                                     sect,
                                     kv,
                                     &auth,
                                     &(auth.user_level_straight_password));
}

/* user_oem_proprietary */

static bmc_err_t
user_oem_proprietary_checkout (bmc_config_state_data_t *state_data,
			       const struct section *sect,
			       struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_checkout (state_data,
                                         sect,
                                         kv,
                                         &auth,
                                         &(auth.user_level_oem_proprietary));
}

static bmc_err_t
user_oem_proprietary_commit (bmc_config_state_data_t *state_data,
			     const struct section *sect,
			     const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_commit (state_data,
                                       sect,
                                       kv,
                                       &auth,
                                       &(auth.user_level_oem_proprietary));
}

static bmc_diff_t
user_oem_proprietary_diff (bmc_config_state_data_t *state_data,
			   const struct section *sect,
			   const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_diff (state_data,
                                     sect,
                                     kv,
                                     &auth,
                                     &(auth.user_level_oem_proprietary));
}

/* operator */

/* operator_none */

static bmc_err_t
operator_none_checkout (bmc_config_state_data_t *state_data,
			const struct section *sect,
			struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_checkout (state_data,
                                         sect,
                                         kv,
                                         &auth,
                                         &(auth.operator_level_none));
}

static bmc_err_t
operator_none_commit (bmc_config_state_data_t *state_data,
		      const struct section *sect,
		      const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_commit (state_data,
                                       sect,
                                       kv,
                                       &auth,
                                       &(auth.operator_level_none));
}

static bmc_diff_t
operator_none_diff (bmc_config_state_data_t *state_data,
		    const struct section *sect,
		    const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_diff (state_data,
                                     sect,
                                     kv,
                                     &auth,
                                     &(auth.operator_level_none));
}

/* operator_md2 */

static bmc_err_t
operator_md2_checkout (bmc_config_state_data_t *state_data,
		       const struct section *sect,
		       struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_checkout (state_data,
                                         sect,
                                         kv,
                                         &auth,
                                         &(auth.operator_level_md2));
}

static bmc_err_t
operator_md2_commit (bmc_config_state_data_t *state_data,
		     const struct section *sect,
		     const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_commit (state_data,
                                       sect,
                                       kv,
                                       &auth,
                                       &(auth.operator_level_md2));
}

static bmc_diff_t
operator_md2_diff (bmc_config_state_data_t *state_data,
		   const struct section *sect,
		   const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_diff (state_data,
                                     sect,
                                     kv,
                                     &auth,
                                     &(auth.operator_level_md2));
}

/* operator_md5 */

static bmc_err_t
operator_md5_checkout (bmc_config_state_data_t *state_data,
		       const struct section *sect,
		       struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_checkout (state_data,
                                         sect,
                                         kv,
                                         &auth,
                                         &(auth.operator_level_md5));
}

static bmc_err_t
operator_md5_commit (bmc_config_state_data_t *state_data,
		     const struct section *sect,
		     const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_commit (state_data,
                                       sect,
                                       kv,
                                       &auth,
                                       &(auth.operator_level_md5));
}

static bmc_diff_t
operator_md5_diff (bmc_config_state_data_t *state_data,
		   const struct section *sect,
		   const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_diff (state_data,
                                     sect,
                                     kv,
                                     &auth,
                                     &(auth.operator_level_md5));
}

/* operator_straight_password */

static bmc_err_t
operator_straight_password_checkout (bmc_config_state_data_t *state_data,
				     const struct section *sect,
				     struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_checkout (state_data,
                                         sect,
                                         kv,
                                         &auth,
                                         &(auth.operator_level_straight_password));
}

static bmc_err_t
operator_straight_password_commit (bmc_config_state_data_t *state_data,
				   const struct section *sect,
				   const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_commit (state_data,
                                       sect,
                                       kv,
                                       &auth,
                                       &(auth.operator_level_straight_password));
}

static bmc_diff_t
operator_straight_password_diff (bmc_config_state_data_t *state_data,
				 const struct section *sect,
				 const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_diff (state_data,
                                     sect,
                                     kv,
                                     &auth,
                                     &(auth.operator_level_straight_password));
}

/* operator_oem_proprietary */

static bmc_err_t
operator_oem_proprietary_checkout (bmc_config_state_data_t *state_data,
				   const struct section *sect,
				   struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_checkout (state_data,
                                         sect,
                                         kv,
                                         &auth,
                                         &(auth.operator_level_oem_proprietary));
}

static bmc_err_t
operator_oem_proprietary_commit (bmc_config_state_data_t *state_data,
				 const struct section *sect,
				 const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_commit (state_data,
                                       sect,
                                       kv,
                                       &auth,
                                       &(auth.operator_level_oem_proprietary));
}

static bmc_diff_t
operator_oem_proprietary_diff (bmc_config_state_data_t *state_data,
			       const struct section *sect,
			       const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_diff (state_data,
                                     sect,
                                     kv,
                                     &auth,
                                     &(auth.operator_level_oem_proprietary));
}

/* admin */


/* admin_none */

static bmc_err_t
admin_none_checkout (bmc_config_state_data_t *state_data,
		     const struct section *sect,
		     struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_checkout (state_data,
                                         sect,
                                         kv,
                                         &auth,
                                         &(auth.admin_level_none));
}

static bmc_err_t
admin_none_commit (bmc_config_state_data_t *state_data,
		   const struct section *sect,
		   const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_commit (state_data,
                                       sect,
                                       kv,
                                       &auth,
                                       &(auth.admin_level_none));
}

static bmc_diff_t
admin_none_diff (bmc_config_state_data_t *state_data,
		 const struct section *sect,
		 const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_diff (state_data,
                                     sect,
                                     kv,
                                     &auth,
                                     &(auth.admin_level_none));
}

/* admin_md2 */

static bmc_err_t
admin_md2_checkout (bmc_config_state_data_t *state_data,
		    const struct section *sect,
		    struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_checkout (state_data,
                                         sect,
                                         kv,
                                         &auth,
                                         &(auth.admin_level_md2));
}

static bmc_err_t
admin_md2_commit (bmc_config_state_data_t *state_data,
		  const struct section *sect,
		  const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_commit (state_data,
                                       sect,
                                       kv,
                                       &auth,
                                       &(auth.admin_level_md2));
}

static bmc_diff_t
admin_md2_diff (bmc_config_state_data_t *state_data,
		const struct section *sect,
		const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_diff (state_data,
                                     sect,
                                     kv,
                                     &auth,
                                     &(auth.admin_level_md2));
}

/* admin_md5 */

static bmc_err_t
admin_md5_checkout (bmc_config_state_data_t *state_data,
		    const struct section *sect,
		    struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_checkout (state_data,
                                         sect,
                                         kv,
                                         &auth,
                                         &(auth.admin_level_md5));
}

static bmc_err_t
admin_md5_commit (bmc_config_state_data_t *state_data,
		  const struct section *sect,
		  const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_commit (state_data,
                                       sect,
                                       kv,
                                       &auth,
                                       &(auth.admin_level_md5));
}

static bmc_diff_t
admin_md5_diff (bmc_config_state_data_t *state_data,
		const struct section *sect,
		const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_diff (state_data,
                                     sect,
                                     kv,
                                     &auth,
                                     &(auth.admin_level_md5));
}

/* admin_straight_password */

static bmc_err_t
admin_straight_password_checkout (bmc_config_state_data_t *state_data,
				  const struct section *sect,
				  struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_checkout (state_data,
                                         sect,
                                         kv,
                                         &auth,
                                         &(auth.admin_level_straight_password));
}

static bmc_err_t
admin_straight_password_commit (bmc_config_state_data_t *state_data,
				const struct section *sect,
				const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_commit (state_data,
                                       sect,
                                       kv,
                                       &auth,
                                       &(auth.admin_level_straight_password));
}

static bmc_diff_t
admin_straight_password_diff (bmc_config_state_data_t *state_data,
			      const struct section *sect,
			      const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_diff (state_data,
                                     sect,
                                     kv,
                                     &auth,
                                     &(auth.admin_level_straight_password));
}

/* admin_oem_proprietary */

static bmc_err_t
admin_oem_proprietary_checkout (bmc_config_state_data_t *state_data,
				const struct section *sect,
				struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_checkout (state_data,
                                         sect,
                                         kv,
                                         &auth,
                                         &(auth.admin_level_oem_proprietary));
}

static bmc_err_t
admin_oem_proprietary_commit (bmc_config_state_data_t *state_data,
			      const struct section *sect,
			      const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_commit (state_data,
                                       sect,
                                       kv,
                                       &auth,
                                       &(auth.admin_level_oem_proprietary));
}

static bmc_diff_t
admin_oem_proprietary_diff (bmc_config_state_data_t *state_data,
			    const struct section *sect,
			    const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_diff (state_data,
                                     sect,
                                     kv,
                                     &auth,
                                     &(auth.admin_level_oem_proprietary));
}

/* oem */

/* oem_none */

static bmc_err_t
oem_none_checkout (bmc_config_state_data_t *state_data,
		   const struct section *sect,
		   struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_checkout (state_data,
                                         sect,
                                         kv,
                                         &auth,
                                         &(auth.oem_level_none));
}

static bmc_err_t
oem_none_commit (bmc_config_state_data_t *state_data,
		 const struct section *sect,
		 const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_commit (state_data,
                                       sect,
                                       kv,
                                       &auth,
                                       &(auth.oem_level_none));
}

static bmc_diff_t
oem_none_diff (bmc_config_state_data_t *state_data,
	       const struct section *sect,
	       const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_diff (state_data,
                                     sect,
                                     kv,
                                     &auth,
                                     &(auth.oem_level_none));
}

/* oem_md2 */

static bmc_err_t
oem_md2_checkout (bmc_config_state_data_t *state_data,
		  const struct section *sect,
		  struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_checkout (state_data,
                                         sect,
                                         kv,
                                         &auth,
                                         &(auth.oem_level_md2));
}

static bmc_err_t
oem_md2_commit (bmc_config_state_data_t *state_data,
		const struct section *sect,
		const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_commit (state_data,
                                       sect,
                                       kv,
                                       &auth,
                                       &(auth.oem_level_md2));
}

static bmc_diff_t
oem_md2_diff (bmc_config_state_data_t *state_data,
	      const struct section *sect,
	      const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_diff (state_data,
                                     sect,
                                     kv,
                                     &auth,
                                     &(auth.oem_level_md2));
}

/* oem_md5 */

static bmc_err_t
oem_md5_checkout (bmc_config_state_data_t *state_data,
		  const struct section *sect,
		  struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_checkout (state_data,
                                         sect,
                                         kv,
                                         &auth,
                                         &(auth.oem_level_md5));
}

static bmc_err_t
oem_md5_commit (bmc_config_state_data_t *state_data,
		const struct section *sect,
		const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_commit (state_data,
                                       sect,
                                       kv,
                                       &auth,
                                       &(auth.oem_level_md5));
}

static bmc_diff_t
oem_md5_diff (bmc_config_state_data_t *state_data,
	      const struct section *sect,
	      const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_diff (state_data,
                                     sect,
                                     kv,
                                     &auth,
                                     &(auth.oem_level_md5));
}

/* oem_straight_password */

static bmc_err_t
oem_straight_password_checkout (bmc_config_state_data_t *state_data,
				const struct section *sect,
				struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_checkout (state_data,
                                         sect,
                                         kv,
                                         &auth,
                                         &(auth.oem_level_straight_password));
}

static bmc_err_t
oem_straight_password_commit (bmc_config_state_data_t *state_data,
			      const struct section *sect,
			      const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_commit (state_data,
                                       sect,
                                       kv,
                                       &auth,
                                       &(auth.oem_level_straight_password));
}

static bmc_diff_t
oem_straight_password_diff (bmc_config_state_data_t *state_data,
			    const struct section *sect,
			    const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_diff (state_data,
                                     sect,
                                     kv,
                                     &auth,
                                     &(auth.oem_level_straight_password));
}

/* oem_oem_proprietary */

static bmc_err_t
oem_oem_proprietary_checkout (bmc_config_state_data_t *state_data,
			      const struct section *sect,
			      struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_checkout (state_data,
                                         sect,
                                         kv,
                                         &auth,
                                         &(auth.oem_level_oem_proprietary));
}

static bmc_err_t
oem_oem_proprietary_commit (bmc_config_state_data_t *state_data,
			    const struct section *sect,
			    const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_commit (state_data,
                                       sect,
                                       kv,
                                       &auth,
                                       &(auth.oem_level_oem_proprietary));
}

static bmc_diff_t
oem_oem_proprietary_diff (bmc_config_state_data_t *state_data,
			  const struct section *sect,
			  const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;

  return _authentication_level_diff (state_data,
                                     sect,
                                     kv,
                                     &auth,
                                     &(auth.oem_level_oem_proprietary));
}

static bmc_err_t
section_lan_conf_auth_comments(bmc_config_state_data_t *state_data,
                               char *section_name,
                               FILE *fp)
{
  char *str =
    "In the Lan_Conf_Auth section, allowable authentication mechanisms for "
    "IPMI 1.5 is configured.  Most users will want to set all \"MD5\" "
    "authentication to \"Yes\" and the rest to \"No\".  If you have "
    "configured a NULL username and a NULL password, you "
    "will also want to configure some of the \"None\" fields to \"Yes\" "
    "to allow \"None\" authentication to work.";

  if (format_section_comments(section_name,
                              str,
                              fp) < 0)
    return BMC_ERR_NON_FATAL_ERROR;

  return BMC_ERR_SUCCESS;
}

struct section *
bmc_lan_conf_auth_section_get (bmc_config_state_data_t *state_data)
{
  struct section *lan_conf_auth_section = NULL;

  if (!(lan_conf_auth_section = bmc_config_section_create(state_data, 
                                                          "Lan_Conf_Auth",
                                                          section_lan_conf_auth_comments,
                                                          0)))
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_auth_section,
                                       "Callback_Enable_Auth_Type_None",
                                       "Possible values: Yes/No",
                                       0,
                                       callback_none_checkout,
                                       callback_none_commit,
                                       callback_none_diff,
                                       yes_no_validate) < 0)
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_auth_section,
                                       "Callback_Enable_Auth_Type_MD2",
                                       "Possible values: Yes/No",
                                       0,
                                       callback_md2_checkout,
                                       callback_md2_commit,
                                       callback_md2_diff,
                                       yes_no_validate) < 0)
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_auth_section,
                                       "Callback_Enable_Auth_Type_MD5",
                                       "Possible values: Yes/No",
                                       0,
                                       callback_md5_checkout,
                                       callback_md5_commit,
                                       callback_md5_diff,
                                       yes_no_validate) < 0)
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_auth_section,
                                       "Callback_Enable_Auth_Type_Straight_Password",
                                       "Possible values: Yes/No",
                                       0,
                                       callback_straight_password_checkout,
                                       callback_straight_password_commit,
                                       callback_straight_password_diff,
                                       yes_no_validate) < 0)
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_auth_section,
                                       "Callback_Enable_Auth_Type_OEM_Proprietary",
                                       "Possible values: Yes/No",
                                       0,
                                       callback_oem_proprietary_checkout,
                                       callback_oem_proprietary_commit,
                                       callback_oem_proprietary_diff,
                                       yes_no_validate) < 0)
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_auth_section,
                                       "User_Enable_Auth_Type_None",
                                       "Possible values: Yes/No",
                                       0,
                                       user_none_checkout,
                                       user_none_commit,
                                       user_none_diff,
                                       yes_no_validate) < 0)
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_auth_section,
                                       "User_Enable_Auth_Type_MD2",
                                       "Possible values: Yes/No",
                                       0,
                                       user_md2_checkout,
                                       user_md2_commit,
                                       user_md2_diff,
                                       yes_no_validate) < 0)
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_auth_section,
                                       "User_Enable_Auth_Type_MD5",
                                       "Possible values: Yes/No",
                                       0,
                                       user_md5_checkout,
                                       user_md5_commit,
                                       user_md5_diff,
                                       yes_no_validate) < 0)
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_auth_section,
                                       "User_Enable_Auth_Type_Straight_Password",
                                       "Possible values: Yes/No",
                                       0,
                                       user_straight_password_checkout,
                                       user_straight_password_commit,
                                       user_straight_password_diff,
                                       yes_no_validate) < 0)
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_auth_section,
                                       "User_Enable_Auth_Type_OEM_Proprietary",
                                       "Possible values: Yes/No",
                                       0,
                                       user_oem_proprietary_checkout,
                                       user_oem_proprietary_commit,
                                       user_oem_proprietary_diff,
                                       yes_no_validate) < 0)
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_auth_section,
                                       "Operator_Enable_Auth_Type_None",
                                       "Possible values: Yes/No",
                                       0,
                                       operator_none_checkout,
                                       operator_none_commit,
                                       operator_none_diff,
                                       yes_no_validate) < 0)
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_auth_section,
                                       "Operator_Enable_Auth_Type_MD2",
                                       "Possible values: Yes/No",
                                       0,
                                       operator_md2_checkout,
                                       operator_md2_commit,
                                       operator_md2_diff,
                                       yes_no_validate) < 0)
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_auth_section,
                                       "Operator_Enable_Auth_Type_MD5",
                                       "Possible values: Yes/No",
                                       0,
                                       operator_md5_checkout,
                                       operator_md5_commit,
                                       operator_md5_diff,
                                       yes_no_validate) < 0)
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_auth_section,
                                       "Operator_Enable_Auth_Type_Straight_Password",
                                       "Possible values: Yes/No",
                                       0,
                                       operator_straight_password_checkout,
                                       operator_straight_password_commit,
                                       operator_straight_password_diff,
                                       yes_no_validate) < 0)
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_auth_section,
                                       "Operator_Enable_Auth_Type_OEM_Proprietary",
                                       "Possible values: Yes/No",
                                       0,
                                       operator_oem_proprietary_checkout,
                                       operator_oem_proprietary_commit,
                                       operator_oem_proprietary_diff,
                                       yes_no_validate) < 0)
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_auth_section,
                                       "Admin_Enable_Auth_Type_None",
                                       "Possible values: Yes/No",
                                       0,
                                       admin_none_checkout,
                                       admin_none_commit,
                                       admin_none_diff,
                                       yes_no_validate) < 0)
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_auth_section,
                                       "Admin_Enable_Auth_Type_MD2",
                                       "Possible values: Yes/No",
                                       0,
                                       admin_md2_checkout,
                                       admin_md2_commit,
                                       admin_md2_diff,
                                       yes_no_validate) < 0)
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_auth_section,
                                       "Admin_Enable_Auth_Type_MD5",
                                       "Possible values: Yes/No",
                                       0,
                                       admin_md5_checkout,
                                       admin_md5_commit,
                                       admin_md5_diff,
                                       yes_no_validate) < 0)
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_auth_section,
                                       "Admin_Enable_Auth_Type_Straight_Password",
                                       "Possible values: Yes/No",
                                       0,
                                       admin_straight_password_checkout,
                                       admin_straight_password_commit,
                                       admin_straight_password_diff,
                                       yes_no_validate) < 0)
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_auth_section,
                                       "Admin_Enable_Auth_Type_OEM_Proprietary",
                                       "Possible values: Yes/No",
                                       0,
                                       admin_oem_proprietary_checkout,
                                       admin_oem_proprietary_commit,
                                       admin_oem_proprietary_diff,
                                       yes_no_validate) < 0)
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_auth_section,
                                       "OEM_Enable_Auth_Type_None",
                                       "Possible values: Yes/No",
                                       0,
                                       oem_none_checkout,
                                       oem_none_commit,
                                       oem_none_diff,
                                       yes_no_validate) < 0)
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_auth_section,
                                       "OEM_Enable_Auth_Type_MD2",
                                       "Possible values: Yes/No",
                                       0,
                                       oem_md2_checkout,
                                       oem_md2_commit,
                                       oem_md2_diff,
                                       yes_no_validate) < 0)
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_auth_section,
                                       "OEM_Enable_Auth_Type_MD5",
                                       "Possible values: Yes/No",
                                       0,
                                       oem_md5_checkout,
                                       oem_md5_commit,
                                       oem_md5_diff,
                                       yes_no_validate) < 0)
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_auth_section,
                                       "OEM_Enable_Auth_Type_Straight_Password",
                                       "Possible values: Yes/No",
                                       0,
                                       oem_straight_password_checkout,
                                       oem_straight_password_commit,
                                       oem_straight_password_diff,
                                       yes_no_validate) < 0)
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_auth_section,
                                       "OEM_Enable_Auth_Type_OEM_Proprietary",
                                       "Possible values: Yes/No",
                                       0,
                                       oem_oem_proprietary_checkout,
                                       oem_oem_proprietary_commit,
                                       oem_oem_proprietary_diff,
                                       yes_no_validate) < 0)
    goto cleanup;

  return lan_conf_auth_section;

 cleanup:
  if (lan_conf_auth_section)
    bmc_config_section_destroy(state_data, lan_conf_auth_section);
  return NULL;
}
