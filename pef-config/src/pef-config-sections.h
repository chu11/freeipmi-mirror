#ifndef _PEF_CONFIG_SECTIONS
#define _PEF_CONFIG_SECTIONS

#include "pef-config.h"
#include "pef-config-common.h"

#define PEF_CHECKOUT_KEY_COMMENTED_OUT                  0x1
#define PEF_CHECKOUT_KEY_COMMENTED_OUT_IF_VALUE_EMPTY   0x2
#define PEF_DO_NOT_CHECKOUT                             0x4

/* Output a comment/instructions for a particular section */
typedef pef_err_t (*Section_Comment) (pef_config_state_data_t *state_data,
                                      char *section_name,
                                      FILE *fp);

struct section {
  struct section *next;
  char *section_name;
  Section_Comment comment;
  unsigned int flags;
  struct keyvalue *keyvalues;
};

/* checkout procedure fills the value into kv->value as printable string */
typedef pef_err_t (*Keyvalue_Checkout) (pef_config_state_data_t *state_data,
                                        const struct section *sect,
                                        struct keyvalue *kv);

/* commit procedure takes string value from kv->value and converts and
   does ipmi calls to set it */
typedef pef_err_t (*Keyvalue_Commit) (pef_config_state_data_t *state_data,
                                      const struct section *sect,
                                      const struct keyvalue *kv);

/* diff procedure finds the difference with the ipmi actual value
   and kv->value */
typedef pef_diff_t (*Keyvalue_Diff) (pef_config_state_data_t *state_data,
                                     const struct section *sect,
                                     const struct keyvalue *kv);

/* validate procedure finds if value is suitable to be set as kv->value */
typedef pef_validate_t (*Keyvalue_Validate) (pef_config_state_data_t *state_data,
                                             const struct section *sect,
                                             const char *value);

struct keyvalue {
  struct keyvalue *next;
  const char *key;
  const char *desc;
  unsigned int flags;
  char *value;
  Keyvalue_Checkout checkout;
  Keyvalue_Commit commit;
  Keyvalue_Diff diff;
  Keyvalue_Validate validate;
};

struct section * pef_config_sections_list_create (pef_config_state_data_t *state_data);

void pef_config_sections_list_destroy (pef_config_state_data_t *state_data,
                                       struct section *sections);

struct section * pef_config_section_create (pef_config_state_data_t *state_data, 
                                            char *section_name,
                                            Section_Comment comment,
                                            unsigned int flags);

void pef_config_section_destroy (pef_config_state_data_t *state_data, 
                                 struct section *section);

int pef_config_section_add_keyvalue (pef_config_state_data_t *state_data,
                                     struct section *section,
                                     const char *key,
                                     const char *desc,
                                     unsigned int flags,
                                     Keyvalue_Checkout checkout,
                                     Keyvalue_Commit commit,
                                     Keyvalue_Diff diff,
                                     Keyvalue_Validate validate);

struct keyvalue * pef_config_section_find_keyvalue (pef_config_state_data_t *state_data,
                                                    const char *section_name,
                                                    const char *key_name);

int pef_config_section_set_value (pef_config_state_data_t *state_data,
                                  const char *section_name,
                                  const char *key_name,
                                  const char *value);

pef_err_t pef_config_section_commit_value (pef_config_state_data_t *state_data,
                                           const char *section_name,
                                           const char *key_name,
                                           const char *value);

int pef_config_section_diff_value (pef_config_state_data_t *state_data,
                                   const char *section_name,
                                   const char *key_name,
                                   const char *value);

pef_err_t pef_config_sections_list (pef_config_state_data_t *state_data);

#endif /* _PEF_CONFIG_SECTIONS */
