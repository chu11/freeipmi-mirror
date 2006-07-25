#include "bmc-common.h"
#include "bmc-config-api.h"
#include "bmc-diff.h"
#include "bmc-map.h"
#include "bmc-sections.h"
#include "bmc-types.h"

static int
sol_auth_checkout (ipmi_device_t dev,
		   uint8_t *sol_privilege_level,
		   uint8_t *force_sol_payload_authentication,
		   uint8_t *force_sol_payload_encryption)
{
  int ret;
  uint8_t tmp_sol_privilege_level;
  uint8_t tmp_force_sol_payload_authentication;
  uint8_t tmp_force_sol_payload_encryption;

  ret = get_sol_sol_authentication (dev,
				    &tmp_sol_privilege_level,
				    &tmp_force_sol_payload_authentication,
				    &tmp_force_sol_payload_encryption);

  if (ret != 0)
    return -1;

  if (sol_privilege_level)
    *sol_privilege_level = tmp_sol_privilege_level;

  if (force_sol_payload_authentication)
    *force_sol_payload_authentication = tmp_force_sol_payload_authentication;

  if (force_sol_payload_encryption)
    *force_sol_payload_encryption = tmp_force_sol_payload_encryption;

  return 0;
}


static int
sol_auth_commit (ipmi_device_t dev,
		 uint8_t *sol_privilege_level,
		 uint8_t *force_sol_payload_authentication,
		 uint8_t *force_sol_payload_encryption)
{
  int ret;
  uint8_t tmp_sol_privilege_level;
  uint8_t tmp_force_sol_payload_authentication;
  uint8_t tmp_force_sol_payload_encryption;

  ret = get_sol_sol_authentication (dev,
				    &tmp_sol_privilege_level,
				    &tmp_force_sol_payload_authentication,
				    &tmp_force_sol_payload_encryption);

  if (ret != 0)
    return -1;

  if (sol_privilege_level)
    tmp_sol_privilege_level = *sol_privilege_level;

  if (force_sol_payload_authentication)
    tmp_force_sol_payload_authentication = *force_sol_payload_authentication;

  if (force_sol_payload_encryption)
    tmp_force_sol_payload_encryption = *force_sol_payload_encryption;

  return set_sol_sol_authentication (dev,
				     tmp_sol_privilege_level,
				     tmp_force_sol_payload_authentication,
				     tmp_force_sol_payload_encryption);
}

static int
enable_sol_checkout (const struct arguments *args,
		     const struct section *sect,
		     struct keyvalue *kv)
{
  uint8_t enable;
  int ret;

  ret = get_sol_sol_enable (args->dev,
			    &enable);

  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);

  kv->value = strdup (enable? "Yes" : "No");

  return 0;
}

static int
enable_sol_commit (const struct arguments *args,
		   const struct section *sect,
		   const struct keyvalue *kv)
{
  return set_sol_sol_enable (args->dev,
			     same (kv->value, "yes"));
}

static int
enable_sol_diff (const struct arguments *args,
		 const struct section *sect,
		 const struct keyvalue *kv)
{
  uint8_t got_value;
  uint8_t passed_value;
  int ret;

  ret = get_sol_sol_enable (args->dev,
			    &got_value);
  if (ret != 0)
    return -1;

  passed_value = same (kv->value, "yes") ? 1 : 0;

  if (passed_value == got_value)
    ret = 0;
  else 
    {
      ret = 1;
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   got_value ? "Yes" : "No");
    }
  return ret;
}

static int
enable_sol_validate (const struct arguments *args,
		     const struct section *sect,
		     const char *value)
{
  return (value && (same (value, "yes") || same (value, "no"))) ? 0 : 1;
}

static int
sol_privilege_level_checkout (const struct arguments *args,
			      const struct section *sect,
			      struct keyvalue *kv)
{
  int ret;
  uint8_t value;

  ret = sol_auth_checkout (args->dev,
			   &value,
			   NULL,
			   NULL);

  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);

  kv->value = strdup (privilege_level_string (value));
  return 0;
}

static int
sol_privilege_level_commit (const struct arguments *args,
			    const struct section *sect,
			    const struct keyvalue *kv)
{
  uint8_t value = privilege_level_number (kv->value);
  return sol_auth_commit (args->dev,
			  &value,
			  NULL,
			  NULL);
}

static int
sol_privilege_level_diff (const struct arguments *args,
			  const struct section *sect,
			  const struct keyvalue *kv)
{
  int ret;
  uint8_t passed_value;
  uint8_t got_value;

  ret = sol_auth_checkout (args->dev,
			   &got_value,
			   NULL,
			   NULL);

  if (ret != 0)
    return -1;

  passed_value = privilege_level_number (kv->value);

  if (passed_value == got_value)
    ret = 0;
  else 
    {
      ret = 1;
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   privilege_level_string (got_value));
    }
  return ret;
}


static int
sol_privilege_level_validate (const struct arguments *args,
			      const struct section *sect,
			      const char *value)
{
  int num = privilege_level_number (value);

  return (num == -1) ? 1 : 0;
}


/* force_sol_payload_authentication */
static int
force_sol_payload_authentication_checkout (const struct arguments *args,
					   const struct section *sect,
					   struct keyvalue *kv)
{
  int ret;
  uint8_t value;

  ret = sol_auth_checkout (args->dev,
			   NULL,
			   &value,
			   NULL);

  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);

  kv->value = strdup (value ? "Yes" : "No");
  return 0;
}

static int
force_sol_payload_authentication_commit (const struct arguments *args,
					 const struct section *sect,
					 const struct keyvalue *kv)
{
  uint8_t value = same (kv->value, "yes") ? 1 : 0;
  return sol_auth_commit (args->dev,
			  NULL,
			  &value,
			  NULL);
}

static int
force_sol_payload_authentication_diff (const struct arguments *args,
				       const struct section *sect,
				       const struct keyvalue *kv)
{
  int ret;
  uint8_t passed_value;
  uint8_t got_value;

  ret = sol_auth_checkout (args->dev,
			   NULL,
			   &got_value,
			   NULL);

  if (ret != 0)
    return -1;

  passed_value = same (kv->value, "yes") ? 1 : 0;

  if (passed_value == got_value)
    ret = 0;
  else 
    {
      ret = 1;
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   got_value ? "Yes" : "No");
    }
  return ret;
}


static int
force_sol_payload_authentication_validate (const struct arguments *args,
					   const struct section *sect,
					   const char *value)
{
  return (value && (same (value, "yes") || same (value, "no"))) ? 0 : 1;
}

/* force_sol_payload_encryption */

static int
force_sol_payload_encryption_checkout (const struct arguments *args,
					   const struct section *sect,
					   struct keyvalue *kv)
{
  int ret;
  uint8_t value;

  ret = sol_auth_checkout (args->dev,
			   NULL,
			   NULL,
			   &value);

  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);

  kv->value = strdup (value ? "Yes" : "No");
  return 0;
}

static int
force_sol_payload_encryption_commit (const struct arguments *args,
				     const struct section *sect,
				     const struct keyvalue *kv)
{
  uint8_t value = same (kv->value, "yes") ? 1 : 0;
  return sol_auth_commit (args->dev,
			  NULL,
			  NULL,
			  &value);
}

static int
force_sol_payload_encryption_diff (const struct arguments *args,
				   const struct section *sect,
				   const struct keyvalue *kv)
{
  int ret;
  uint8_t passed_value;
  uint8_t got_value;

  ret = sol_auth_checkout (args->dev,
			   NULL,
			   NULL,
			   &got_value);

  if (ret != 0)
    return -1;

  passed_value = same (kv->value, "yes") ? 1 : 0;

  if (passed_value == got_value)
    ret = 0;
  else 
    {
      ret = 1;
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   got_value ? "Yes" : "No");
    }
  return ret;
}


static int
force_sol_payload_encryption_validate (const struct arguments *args,
				       const struct section *sect,
				       const char *value)
{
  return (value && (same (value, "yes") || same (value, "no"))) ? 0 : 1;
}

/* character_accumulate_interval */

static int
character_accumulate_interval_checkout (const struct arguments *args,
					const struct section *sect,
					struct keyvalue *kv)
{
  int ret;
  uint8_t interval;
  uint8_t threshold;

  ret = get_sol_character_accumulate_interval_and_send_threshold (args->dev,
								  &interval,
								  &threshold);

  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);

  asprintf (&kv->value, "%d", interval);
  return 0;
}

static int
character_accumulate_interval_commit (const struct arguments *args,
				      const struct section *sect,
				      const struct keyvalue *kv)
{
  int ret;
  uint8_t interval;
  uint8_t threshold;

  ret = get_sol_character_accumulate_interval_and_send_threshold (args->dev,
								  &interval,
								  &threshold);

  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);

  interval = atoi (kv->value);

  return set_sol_character_accumulate_interval_and_send_threshold (args->dev,
								   interval,
								   threshold);
}

static int
character_accumulate_interval_diff (const struct arguments *args,
				    const struct section *sect,
				    const struct keyvalue *kv)
{
  int ret;
  uint8_t got_value;
  uint8_t passed_value;

  uint8_t interval;
  uint8_t threshold;

  ret = get_sol_character_accumulate_interval_and_send_threshold (args->dev,
								  &interval,
								  &threshold);

  got_value = interval;
  passed_value = atoi (kv->value);

  if (passed_value == got_value)
    ret = 0;
  else 
    {
      char num[32];
      ret = 1;
      sprintf (num, "%d", got_value);
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   num);
    }
  return ret;
}

static int
character_accumulate_interval_validate (const struct arguments *args,
					const struct section *sect,
					const char *value)
{
  int num;
  char *endptr;

  num = strtol (value, &endptr, 0);

  if (*endptr)
    return 1;

  if (num < 0 || num > 255)
    return 1;

  return 0;
}

/* character_send_threshold */

static int
character_send_threshold_checkout (const struct arguments *args,
				   const struct section *sect,
				   struct keyvalue *kv)
{
  int ret;
  uint8_t interval;
  uint8_t threshold;

  ret = get_sol_character_accumulate_interval_and_send_threshold (args->dev,
								  &interval,
								  &threshold);

  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);

  asprintf (&kv->value, "%d", threshold);
  return 0;
}

static int
character_send_threshold_commit (const struct arguments *args,
				 const struct section *sect,
				 const struct keyvalue *kv)
{
  int ret;
  uint8_t interval;
  uint8_t threshold;

  ret = get_sol_character_accumulate_interval_and_send_threshold (args->dev,
								  &interval,
								  &threshold);

  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);

  threshold = atoi (kv->value);

  return set_sol_character_accumulate_interval_and_send_threshold (args->dev,
								   interval,
								   threshold);
}

static int
character_send_threshold_diff (const struct arguments *args,
			       const struct section *sect,
			       const struct keyvalue *kv)
{
  int ret;
  uint8_t got_value;
  uint8_t passed_value;

  uint8_t interval;
  uint8_t threshold;

  ret = get_sol_character_accumulate_interval_and_send_threshold (args->dev,
								  &interval,
								  &threshold);

  got_value = threshold;
  passed_value = atoi (kv->value);

  if (passed_value == got_value)
    ret = 0;
  else 
    {
      char num[32];
      ret = 1;
      sprintf (num, "%d", got_value);
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   num);
    }
  return ret;
}

static int
character_send_threshold_validate (const struct arguments *args,
				   const struct section *sect,
				   const char *value)
{
  int num;
  char *endptr;

  num = strtol (value, &endptr, 0);

  if (*endptr)
    return 1;

  if (num < 0 || num > 255)
    return 1;
  return 0;
}

/* sol_retry_count */

static int
sol_retry_count_checkout (const struct arguments *args,
			  const struct section *sect,
			  struct keyvalue *kv)
{
  uint8_t count;
  uint8_t interval;
  int ret;

  ret = get_sol_sol_retry (args->dev,
			   &count,
			   &interval);

  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);

  asprintf (&kv->value, "%d", count);
  return 0;
}


static int
sol_retry_count_commit (const struct arguments *args,
			const struct section *sect,
			const struct keyvalue *kv)
{
  uint8_t count;
  uint8_t interval;
  int ret;

  ret = get_sol_sol_retry (args->dev,
			   &count,
			   &interval);

  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);

  count = atoi (kv->value);

  return set_sol_sol_retry (args->dev,
			    count,
			    interval);
}

static int
sol_retry_count_diff (const struct arguments *args,
		      const struct section *sect,
		      const struct keyvalue *kv)
{
  uint8_t passed_value;
  uint8_t got_value;

  int ret;
  uint8_t count;
  uint8_t interval;

  ret = get_sol_sol_retry (args->dev,
			   &count,
			   &interval);

  if (ret != 0)
    return -1;

  got_value = count;
  passed_value = atoi (kv->value);

  if (passed_value == got_value)
    ret = 0;
  else 
    {
      char num[32];
      ret = 1;
      sprintf (num, "%d", got_value);
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   num);
    }
  return ret;
}

static int
sol_retry_count_validate (const struct arguments *args,
			  const struct section *sect,
			  const char *value)
{
  int num;
  char *endptr;

  num = strtol (value, &endptr, 0);

  if (*endptr)
    return -1;

  if (num < 0 || num > 255)
    return 1;

  return 0;
}

/* sol_retry_interval */


static int
sol_retry_interval_checkout (const struct arguments *args,
			     const struct section *sect,
			     struct keyvalue *kv)
{
  uint8_t count;
  uint8_t interval;
  int ret;

  ret = get_sol_sol_retry (args->dev,
			   &count,
			   &interval);

  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);

  asprintf (&kv->value, "%d", interval);
  return 0;
}


static int
sol_retry_interval_commit (const struct arguments *args,
			   const struct section *sect,
			   const struct keyvalue *kv)
{
  uint8_t count;
  uint8_t interval;
  int ret;

  ret = get_sol_sol_retry (args->dev,
			   &count,
			   &interval);

  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);

  interval = atoi (kv->value);

  return set_sol_sol_retry (args->dev,
			    count,
			    interval);
}

static int
sol_retry_interval_diff (const struct arguments *args,
			 const struct section *sect,
			 const struct keyvalue *kv)
{
  uint8_t passed_value;
  uint8_t got_value;

  int ret;
  uint8_t count;
  uint8_t interval;

  ret = get_sol_sol_retry (args->dev,
			   &count,
			   &interval);

  if (ret != 0)
    return -1;

  got_value = interval;
  passed_value = atoi (kv->value);

  if (passed_value == got_value)
    ret = 0;
  else 
    {
      char num[32];
      ret = 1;
      sprintf (num, "%d", got_value);
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   num);
    }
  return ret;
}

static int
sol_retry_interval_validate (const struct arguments *args,
			     const struct section *sect,
			     const char *value)
{
  int num;
  char *endptr;

  num = strtol (value, &endptr, 0);

  if (*endptr)
    return -1;

  if (num < 0 || num > 255)
    return 1;

  return 0;
}

static int
non_volatile_bit_rate_checkout (const struct arguments *args,
				const struct section *sect,
				struct keyvalue *kv)
{
  int ret;
  uint8_t bitrate;

  ret = get_sol_sol_non_volatile_bit_rate (args->dev,
					   &bitrate);

  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);

  kv->value = strdup (sol_bit_rate_string (bitrate));
  return 0;
}

static int
non_volatile_bit_rate_commit (const struct arguments *args,
			      const struct section *sect,
			      const struct keyvalue *kv)
{
  return set_sol_sol_non_volatile_bit_rate (args->dev,
					    sol_bit_rate_number (kv->value));
}

static int
non_volatile_bit_rate_diff (const struct arguments *args,
			    const struct section *sect,
			    const struct keyvalue *kv)
{
  int ret;
  uint8_t got_value;
  uint8_t passed_value;

  ret = get_sol_sol_non_volatile_bit_rate (args->dev,
					   &got_value);
  if (ret != 0)
    return -1;

  passed_value = sol_bit_rate_number (kv->value);

  if (passed_value == got_value)
    ret = 0;
  else 
    {
      ret = 1;
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   sol_bit_rate_string (got_value));
    }
  return ret;
}

static int
non_volatile_bit_rate_validate (const struct arguments *args,
				const struct section *sect,
				const char *value)
{
  return sol_bit_rate_number (value) == -1 ? 1 : 0;
}

/* volatile_bit_rate */

static int
volatile_bit_rate_checkout (const struct arguments *args,
			    const struct section *sect,
			    struct keyvalue *kv)
{
  int ret;
  uint8_t bitrate;

  ret = get_sol_sol_volatile_bit_rate (args->dev,
				       &bitrate);

  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);

  kv->value = strdup (sol_bit_rate_string (bitrate));
  return 0;
}

static int
volatile_bit_rate_commit (const struct arguments *args,
			  const struct section *sect,
			  const struct keyvalue *kv)
{
  return set_sol_sol_volatile_bit_rate (args->dev,
					sol_bit_rate_number (kv->value));
}

static int
volatile_bit_rate_diff (const struct arguments *args,
			const struct section *sect,
			const struct keyvalue *kv)
{
  int ret;
  uint8_t got_value;
  uint8_t passed_value;

  ret = get_sol_sol_volatile_bit_rate (args->dev,
					   &got_value);
  if (ret != 0)
    return -1;

  passed_value = sol_bit_rate_number (kv->value);


  if (passed_value == got_value)
    ret = 0;
  else 
    {
      ret = 1;
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   sol_bit_rate_string (got_value));
    }
  return ret;
}

static int
volatile_bit_rate_validate (const struct arguments *args,
			    const struct section *sect,
			    const char *value)
{
  return sol_bit_rate_number (value) == -1 ? 1 : 0;
}

static int
port_checkout (const struct arguments *args,
	       const struct section *sect,
	       struct keyvalue *kv)
{
  int ret;
  uint16_t port;

  ret = get_sol_sol_payload_port_number (args->dev,
					 &port);
  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);

  asprintf (&kv->value, "%d", port);
  return 0;
}


static int
port_commit (const struct arguments *args,
	     const struct section *sect,
	     const struct keyvalue *kv)
{
  return set_sol_sol_payload_port_number (args->dev,
					  atoi (kv->value));
}

static int
port_diff (const struct arguments *args,
	   const struct section *sect,
	   const struct keyvalue *kv)
{
  uint16_t got_value;
  uint16_t passed_value;
  int ret;

  ret = get_sol_sol_payload_port_number (args->dev,
					 &got_value);
  if (ret != 0)
    return -1;

  passed_value = atoi (kv->value);

  if (passed_value == got_value)
    ret = 0;
  else 
    {
      char num[32];
      ret = 1;
      sprintf (num, "%d", got_value);
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   num);
    }
  return ret;
}

static int
port_validate (const struct arguments *args,
	       const struct section *sect,
	       const char *value)
{
  int num;
  char *endptr;

  num = strtol (value, &endptr, 0);

  if (*endptr)
    return -1;
  if (num < -1 || num > 65535)
    return 1;
  return 0;
}

struct section *
bmc_sol_conf_section_get (struct arguments *args)
{
  struct section * sol_conf_section = NULL;
  
  sol_conf_section = (void *) calloc (1, sizeof (struct section));
  sol_conf_section->section = strdup ("SOL_Conf");

  add_keyvalue (sol_conf_section,
		"Enable_SOL",
		"Possible values: Yes/No",
                0,
		enable_sol_checkout,
		enable_sol_commit,
		enable_sol_diff,
		enable_sol_validate);

  add_keyvalue (sol_conf_section,
		"SOL_Privilege_Level",
		"Possible values: Callback/User/Operator/Administrator/OEM_Proprietary",
                0,
		sol_privilege_level_checkout,
		sol_privilege_level_commit,
		sol_privilege_level_diff,
		sol_privilege_level_validate);

  add_keyvalue (sol_conf_section,
		"Force_SOL_Payload_Authentication",
		"Possible values: Yes/No",
                0,
		force_sol_payload_authentication_checkout,
		force_sol_payload_authentication_commit,
		force_sol_payload_authentication_diff,
		force_sol_payload_authentication_validate);

  add_keyvalue (sol_conf_section,
		"Force_SOL_Payload_Encryption",
		"Possible values: Yes/No",
                0,
		force_sol_payload_encryption_checkout,
		force_sol_payload_encryption_commit,
		force_sol_payload_encryption_diff,
		force_sol_payload_encryption_validate);

  add_keyvalue (sol_conf_section,
		"Character_Accumulate_Interval",
		"Give a valid integer. Each unit is 5ms",
                0,
		character_accumulate_interval_checkout,
		character_accumulate_interval_commit,
		character_accumulate_interval_diff,
		character_accumulate_interval_validate);

  add_keyvalue (sol_conf_section,
		"Character_Send_Threshold",
		"Give a valid number",
                0,
		character_send_threshold_checkout,
		character_send_threshold_commit,
		character_send_threshold_diff,
		character_send_threshold_validate);

  add_keyvalue (sol_conf_section,
		"SOL_Retry_Count",
		"Give a valid integer",
                0,
		sol_retry_count_checkout,
		sol_retry_count_commit,
		sol_retry_count_diff,
		sol_retry_count_validate);

  add_keyvalue (sol_conf_section,
		"SOL_Retry_Interval",
		"Give a valid integer. Interval unit is 10ms",
                0,
		sol_retry_interval_checkout,
		sol_retry_interval_commit,
		sol_retry_interval_diff,
		sol_retry_interval_validate);

  add_keyvalue (sol_conf_section,
		"Non_Volatile_Bit_Rate",
		"Possible values: Serial/9600/19200/38400/57600/115200",
                0,
		non_volatile_bit_rate_checkout,
		non_volatile_bit_rate_commit,
		non_volatile_bit_rate_diff,
		non_volatile_bit_rate_validate);

  add_keyvalue (sol_conf_section,
		"Volatile_Bit_Rate",
		"Possible values: Serial/9600/19200/38400/57600/115200",
                0,
		volatile_bit_rate_checkout,
		volatile_bit_rate_commit,
		volatile_bit_rate_diff,
		volatile_bit_rate_validate);

  add_keyvalue (sol_conf_section,
		"SOL_Payload_Port_Number",
		"Give a valid port number",
                BMC_CHECKOUT_KEY_COMMENTED_OUT,
		port_checkout,
		port_commit,
		port_diff,
		port_validate);
  return sol_conf_section;
}
