#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <freeipmi.h>
#include <stdio.h>
#include <stdlib.h>

#ifdef STDC_HEADERS
#include <string.h>
#else
# include <sys/types.h>
# ifndef HAVE_MEMCPY
static void*
memcpy (void *dest, const void *src, size_t n)
{
  while (0 <= --n) ((unsigned char*)dest) [n] = ((unsigned char*)src) [n];
  return dest;
}
# endif
# ifndef HAVE_MEMSET
static void*
memset (void *s, int c, size_t n)
{
  while (0 <= --n) ((unsigned char*)s) [n] = (unsigned char) c;
  return s;
}
# endif
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "fish.h"
#include "fi-utils.h"
#include "bmc-conf-utils.h"
#include "bmc-conf-checkout.h"
#include "bmc-conf-key-utils.h"

int 
check_user_password (int user_id, char *password)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  char password_data[IPMI_USER_PASSWORD_MAX_LENGTH];
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_set_user_password_rs));
  
  memset (password_data, 0, IPMI_USER_PASSWORD_MAX_LENGTH);
  if (strlen (password) < IPMI_USER_PASSWORD_MAX_LENGTH)
    memcpy (password_data, password, strlen (password));
  else 
    memcpy (password_data, password, IPMI_USER_PASSWORD_MAX_LENGTH);
  
  status = ipmi_kcs_set_user_password (fi_get_sms_io_base (), 
				       user_id, 
				       IPMI_PASSWORD_OPERATION_TEST_PASSWORD, 
				       password_data, 
				       obj_data_rs);
  
  if (status != 0)
    {
      perror ("ipmi_kcs_set_user_password");
      return (-1); /* failed */
    }
  
  if (IPMI_COMP_CODE (obj_data_rs) == IPMI_COMMAND_SUCCESS)
    return 1; /* true */
  
  if (IPMI_COMP_CODE (obj_data_rs) == IPMI_PASSWORD_OPERATION_TEST_FAILED)
    return 0; /* false */
  
  {
    char err_msg[IPMI_ERR_STR_MAX_LEN];
    ipmi_strerror_cmd_r (obj_data_rs, err_msg, IPMI_ERR_STR_MAX_LEN);
    fprintf (stderr, 
	     "error: ipmi_kcs_set_user_password: %d: %s\n", 
	     IPMI_COMP_CODE(obj_data_rs), err_msg);
  }
  
  return (-1); /* failed */
}


int 
edit_key_pair_bmc_config_file (char *filename, char *key, char *value)
{
  FILE *fp = NULL;
  FILE *tmp_fp = NULL;
  
  char *line = NULL;
  char *tmp_filename = NULL;
  char *key_string = NULL;
  int key_length = 0;
  
  size_t n = 0;
  
  if (filename == NULL || *filename == '\0')
    return (-1);
  
  if (key == NULL || *key == '\0')
    return (-1);
  
  if ((fp = fopen (filename, "r")) == NULL)
    return (-1);
  
  tmp_filename = alloca (strlen (filename) + 5);
  strcpy (tmp_filename, filename);
  strcat (tmp_filename, ".tmp");
  if ((tmp_fp = fopen (tmp_filename, "w")) == NULL)
    {
      fclose (fp);
      return (-1);
    }
  
  key_length = strlen (key) + 1;
  key_string = alloca (key_length);
  strcpy (key_string, key);
  strcat (key_string, " ");
  
  while (1)
    {
      if (getline (&line, &n, fp) == -1)
	break;
      
      if (strncmp (line, key_string, key_length) == 0)
	fprintf (tmp_fp, "%s %s\n", key, value);
      else 
	fprintf (tmp_fp, "%s", line);
      
      free (line);
      line = NULL;
      n = 0;
    }
  
  fclose (fp);
  fclose (tmp_fp);
  
  if (unlink (filename) != 0)
    return (-1);
  
  if (rename (tmp_filename, filename) != 0)
    return (-1);
  
  return (0);
}


int 
bmc_config_diff_key_pair (char *bmc_filename, char *user_key, char *user_value)
{
  FILE *fp = NULL;
  
  char *line = NULL;
  
  char *bmc_key = NULL;
  char *bmc_value = NULL;
  
  int status = 0;
  
  if (bmc_filename == NULL || *bmc_filename == '\0')
    return (-1);
  
  if (user_key == NULL || *user_key == '\0')
    return (-1);
  
  if (bmc_config_validate_value (user_key, user_value) == false)
    return 1; /* failed */

  /* check password */
  {
    int is_password_key = 0;
    int user_id = 0;
    
    if (strcmp (user_key, "user1_password") == 0)
      {
	user_id = 1;
	is_password_key = 1;
      }
    if (strcmp (user_key, "user2_password") == 0)
      {
	user_id = 2;
	is_password_key = 1;
      }
    if (strcmp (user_key, "user3_password") == 0)
      {
	user_id = 3;
	is_password_key = 1;
      }
    if (strcmp (user_key, "user4_password") == 0)
      {
	user_id = 4;
	is_password_key = 1;
      }
  
    if (is_password_key)
      {
	if (check_user_password (user_id, user_value))
	  return 0; /* same value */
      
	printf ("user%d_password differs\n", user_id);
	return 1; /* value differs */
      }
  }
  
  if ((fp = fopen (bmc_filename, "r")) == NULL)
    return (-1);
  
  while (1)
    {
      if (line)
	free (line);
      if (bmc_key)
	free (bmc_key);
      if (bmc_value)
	free (bmc_value);
      
      line = NULL;
      bmc_key = NULL; 
      bmc_value = NULL;
      
      if ((line = fi_getline (fp)) == NULL)
	break;
      
      if (bmc_config_get_key_value (line, &bmc_key,  &bmc_value) != 0)
	{
	  fprintf (stderr, 
		   "error: invalid line [%s] in bmc-config file\n", 
		   line);
	  continue;
	}
      
      if (strcmp (bmc_key, user_key) != 0)
	continue;
      
      if (strcmp (user_value, bmc_value) != 0)
	{
	  printf ("BMC:  %s %s\n", bmc_key, bmc_value);
	  printf ("USER: %s %s\n", user_key, user_value);
	  status = 1;
	  break;
	}
    }
  fclose (fp);
  
  if (line)
    free (line);
  if (bmc_key)
    free (bmc_key);
  if (bmc_value)
    free (bmc_value);
  
  return (status);
}


int 
bmc_config_diff_file (char *bmc_filename, char *user_filename)
{
  FILE *bmc_fp = NULL;
  char *bmc_line = NULL;
  char *bmc_key = NULL;
  char *bmc_value = NULL;
  
  FILE *user_fp = NULL;
  char *user_line = NULL;
  char *user_key = NULL;
  char *user_value = NULL;
  
  int status = 0;
  
  if (bmc_filename == NULL || *bmc_filename == '\0' || 
      user_filename == NULL || *user_filename == '\0')
    return (-1);
  
  if ((bmc_fp = fopen (bmc_filename, "r")) == NULL)
    return (-1);
  
  if ((user_fp = fopen (user_filename, "r")) == NULL)
    {
      fclose (bmc_fp);
      return (-1);
    }
  
  while (1)
    {
      if (bmc_line) 
	free (bmc_line);
      if (bmc_key)
	free (bmc_key);
      if (bmc_value)
	free (bmc_value);
      
      if (user_line) 
	free (user_line);
      if (user_key)
	free (user_key);
      if (user_value)
	free (user_value);
      
      bmc_line = NULL;
      bmc_key = NULL;
      bmc_value = NULL;
      
      user_line = NULL;
      user_key = NULL;
      user_value = NULL;
      
      if ((bmc_line = fi_getline (bmc_fp)) == NULL)
	break;
      
      if ((user_line = fi_getline (user_fp)) == NULL)
	break;
      
      if (bmc_config_get_key_value (bmc_line, &bmc_key,  &bmc_value) != 0)
	{
	  fprintf (stderr, 
		   "error: invalid line [%s] in bmc-config file\n", 
		   bmc_line);
	  continue;
	}
      
      if (bmc_config_get_key_value (user_line, &user_key,  &user_value) != 0)
	{
	  fprintf (stderr, 
		   "error: invalid line [%s] in bmc-config file\n", 
		   user_line);
	  continue;
	}
      
      if (bmc_config_validate_value (user_key, user_value) == false)
	{
	  status = 1; /* failed */
	  continue;
	}
      
      /* check password */
      {
	int is_password_key = 0;
	int user_id = 0;
	
	if (strcmp (user_key, "user1_password") == 0)
	  {
	    user_id = 1;
	    is_password_key = 1;
	  }
	if (strcmp (user_key, "user2_password") == 0)
	  {
	    user_id = 2;
	    is_password_key = 1;
	  }
	if (strcmp (user_key, "user3_password") == 0)
	  {
	    user_id = 3;
	    is_password_key = 1;
	  }
	if (strcmp (user_key, "user4_password") == 0)
	  {
	    user_id = 4;
	    is_password_key = 1;
	  }
	
	if (is_password_key)
	  {
	    if (strcmp (user_value, PASSWORD_MASK) == 0)
	      continue;
	    
	    if (check_user_password (user_id, user_value))
	      continue; /* same value */
	    
	    printf ("user%d_password differs\n", user_id);
	    status = 1; /* value differs */
	    continue;
	  }
      }
      
      if (strcmp (bmc_key, user_key) != 0)
	continue;
      
      if (strcmp (user_value, bmc_value) != 0)
	{
	  printf ("BMC:  %s %s\n", bmc_key, bmc_value);
	  printf ("USER: %s %s\n", user_key, user_value);
	  status = 1;
	}
    }
  
  fclose (bmc_fp);
  fclose (user_fp);
  
  return (status);
}


int 
check_bmc_config_file_key_orig (char *filename, char *key)
{
  FILE *fp = NULL;
  
  char *line = NULL;
  char *line_ptr = NULL;
  char *bmc_key = NULL;
  
  int status = 0;
  
  if (filename == NULL || *filename == '\0')
    return (-1);
  
  if (key == NULL || *key == '\0')
    return (-1);
  
  if ((fp = fopen (filename, "r")) == NULL)
    return (-1);
  
  while (1)
    {
      if (line)
	free (line);
      
      if ((line = fi_getline (fp)) == NULL)
	{
	  status = 1;
	  break;
	}
      
      line_ptr = line;
      bmc_key = get_token (&line_ptr);
      if (bmc_key == NULL)
	continue;
      
      if (strcmp (bmc_key, key) == 0)
	{
	  status = 0;
	  free (line);
	  break;
	}
    }
  
  fclose (fp);
  
  return status;
}


int 
diff_key_pair_bmc_config_file_orig (char *filename, char *key, char *value)
{
  FILE *fp = NULL;
  
  char *line = NULL;
  char *line_ptr = NULL;
  char *bmc_key = NULL;
  char *bmc_value = NULL;
  int status = 0;
  int is_password_key = 0;
  int user_id = 0;
  
  if (filename == NULL || *filename == '\0')
    return (-1);
  
  if (key == NULL || *key == '\0')
    return (-1);
  
  /* check ip validity */
  if (strcmp (key, "ip_addr") == 0)
    {
      if (!is_valid_ip (value))
	{
	  fprintf (stderr, 
		   "error: Invalid IP address [%s]\n", 
		   value);
	  return (-1);
	}
    }
  
  if (strcmp (key, "gw1_ip_addr") == 0)
    {
      if (!is_valid_ip (value))
	{
	  fprintf (stderr, 
		   "error: Invalid IP address [%s]\n", 
		   value);
	  return (-1);
	}
    }
  
  if (strcmp (key, "gw2_ip_addr") == 0)
    {
      if (!is_valid_ip (value))
	{
	  fprintf (stderr, 
		   "error: Invalid IP address [%s]\n", 
		   value);
	  return (-1);
	}
    }
  
  if (strcmp (key, "subnet_mask") == 0)
    {
      if (!is_valid_ip (value))
	{
	  fprintf (stderr, 
		   "error: Invalid subnet mask [%s]\n", 
		   value);
	  return (-1);
	}
    }
  
  /* check mac address */
  if (strcmp (key, "mac_addr") == 0)
    {
      if (!is_valid_mac_address (value))
	{
	  fprintf (stderr, 
		   "error: Invalid MAC address [%s]\n", 
		   value);
	  return (-1);
	}
    }
  
  if (strcmp (key, "gw1_mac_addr") == 0)
    {
      if (!is_valid_mac_address (value))
	{
	  fprintf (stderr, 
		   "error: Invalid MAC address [%s]\n", 
		   value);
	  return (-1);
	}
    }
  
  if (strcmp (key, "gw2_mac_addr") == 0)
    {
      if (!is_valid_mac_address (value))
	{
	  fprintf (stderr, 
		   "error: Invalid MAC address [%s]\n", 
		   value);
	  return (-1);
	}
    }
  
  /* check password*/
  if (strcmp (key, "user1_password") == 0)
    {
      user_id = 1;
      is_password_key = 1;
    }
  if (strcmp (key, "user2_password") == 0)
    {
      user_id = 2;
      is_password_key = 1;
    }
  if (strcmp (key, "user3_password") == 0)
    {
      user_id = 3;
      is_password_key = 1;
    }
  if (strcmp (key, "user4_password") == 0)
    {
      user_id = 4;
      is_password_key = 1;
    }
  
  if (is_password_key)
    {
      if (check_user_password (user_id, value) == 0)
	{
	  printf ("user%d_password differs\n", user_id);
	  status = 1;
	}
      return (status);
    }
  
  if ((fp = fopen (filename, "r")) == NULL)
    return (-1);
  
  
  while (1)
    {
      if ((line = fi_getline (fp)) == NULL)
	break;
      line_ptr = line;

      bmc_key = get_token (&line_ptr);
      if (bmc_key)
	bmc_key = stripwhite (bmc_key);
      if (strcmp (key, bmc_key) == 0)
	{
	  bmc_value = get_token (&line_ptr);
	  if (strcmp (value, bmc_value) != 0)
	    {
	      printf ("BMC:  %s %s\n", bmc_key, bmc_value);
	      printf ("USER: %s %s\n", key, value);
	      status = 1;
	    }
	}
      free (line);
    }
  
  fclose (fp);
  
  return (status);
}


int 
diff_file_bmc_config_file_orig (char *bmc_filename, char *filename)
{
  FILE *bmc_fp = NULL;
  FILE *fp = NULL;
  
  char *bmc_line = NULL;
  char *line = NULL;
  char *line_ptr = NULL;
  int status = 0;
  
  char *key = NULL;
  char *value = NULL;
  int is_password_key = 0;
  int user_id = 0;
  
  if (bmc_filename == NULL || *bmc_filename == '\0' || 
      filename == NULL || *filename == '\0')
    return (-1);
  
  if ((bmc_fp = fopen (bmc_filename, "r")) == NULL)
    return (-1);
  
  if ((fp = fopen (filename, "r")) == NULL)
    {
      fclose (bmc_fp);
      return (-1);
    }
  
  while (1)
    {
      is_password_key = 0;
      user_id = 0;
      
      if (bmc_line)
	free (bmc_line);
      if (line)
	free (line);
      
      if ((bmc_line = fi_getline (bmc_fp)) == NULL)
	break;
      
      if ((line = fi_getline (fp)) == NULL)
	{
	  free (bmc_line);
	  break;
	}
      line_ptr = line;
      
      /* check password*/
      if (strncmp (line_ptr, "user1_password", strlen ("user1_password")) == 0)
	{
	  user_id = 1;
	  is_password_key = 1;
	}
      if (strncmp (line_ptr, "user2_password", strlen ("user2_password")) == 0)
	{
	  user_id = 2;
	  is_password_key = 1;
	}
      if (strncmp (line_ptr, "user3_password", strlen ("user3_password")) == 0)
	{
	  user_id = 3;
	  is_password_key = 1;
	}
      if (strncmp (line_ptr, "user4_password", strlen ("user4_password")) == 0)
	{
	  user_id = 4;
	  is_password_key = 1;
	}
      
      if (is_password_key)
	{
	  key = get_token (&line_ptr);
	  if (key)
	    key = stripwhite (key);
	  value = get_token (&line_ptr);
	  
	  if (value == NULL)
	    value = strdupa ("");
	  
	  if (strcmp (value, PASSWORD_MASK) == 0)
	    continue;
	  
	  if (check_user_password (user_id, value) == 0)
	    {
	      printf ("user%d_password differs\n", user_id);
	      status = 1;
	    }
	  continue;
	}
      
      if (strcmp (bmc_line, line) != 0)
	{
	  printf ("BMC:  %s\n", bmc_line);
	  printf ("USER: %s\n", line);
	  status = 1;
	}
    }
  
  fclose (bmc_fp);
  fclose (fp);
  
  return (status);
}


