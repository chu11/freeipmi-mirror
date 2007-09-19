#ifndef _CONFIG_COMMON_H_
#define _CONFIG_COMMON_H_

#include <stdio.h>

#define CONFIG_CHECKOUT_KEY_COMMENTED_OUT                  0x01
#define CONFIG_CHECKOUT_KEY_COMMENTED_OUT_IF_VALUE_EMPTY   0x02
#define CONFIG_DO_NOT_CHECKOUT                             0x04

typedef enum
  {
    CONFIG_ERR_FATAL_ERROR = -2,
    CONFIG_ERR_NON_FATAL_ERROR = -1,
    CONFIG_ERR_SUCCESS = 0,
  } config_err_t;

typedef enum
  {
    CONFIG_VALIDATE_FATAL_ERROR = -2,
    CONFIG_VALIDATE_INVALID_VALUE = -1,
    CONFIG_VALIDATE_VALID_VALUE = 0,
  } config_validate_t;

/* validate procedure finds if value is suitable to be set as kv->value */
typedef config_validate_t (*Key_Validate) (const char *section_name,
                                           const char *key,
                                           const char *value,
                                           int debug,
                                           void *arg);

struct config_key
{
  char *key_name;
  char *description;
  unsigned int flags;
  Key_Validate validate;
  struct config_key *next;
};

struct config_keyvalue {
  struct config_key *key;
  char *value_input;            /* value input by user for commit and diff */
  char *value_output;           /* value read on bmc for checkout and diff */
  struct config_keyvalue *next;
};

/* checkout procedure fills the value into kv->value as printable string */
typedef config_err_t (*Section_Checkout) (const char *section_name,
                                          struct config_keyvalue *keyvalues,
                                          FILE *fp,
                                          int debug,
                                          void *arg);

/* commit procedure takes string value from kv->value and converts and
   does ipmi calls to set it */
typedef config_err_t (*Section_Commit) (const char *section_name,
                                        struct config_keyvalue *keyvalues,
                                        int debug,
                                        void *arg);

struct config_section {
  char *section_name;
  char *section_comment_section_name;
  char *section_comment;
  unsigned int flags;
  Section_Checkout checkout;
  Section_Commit commit;
  /* no need for diff callback, diff is a checkout and then comparison of values */
  /* keys in this section */
  struct config_key *keys;
  /* key and values read for checkout/commit/diff */
  struct config_keyvalue *keyvalues;
  struct config_section *next;
};

#endif /* _CONFIG_COMMON_H_ */
