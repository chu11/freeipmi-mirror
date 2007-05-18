#ifndef _IPMI_PEF_SECTIONS
#define _IPMI_PEF_SECTIONS

#include "ipmi-pef.h"
#include "ipmi-pef-common.h"

#define PEF_CHECKOUT_KEY_COMMENTED_OUT                  0x1
#define PEF_CHECKOUT_KEY_COMMENTED_OUT_IF_VALUE_EMPTY   0x2
#define PEF_DO_NOT_CHECKOUT                             0x4

struct section {
  struct section *next;
  char *section_name;
  struct keyvalue *keyvalues;
};

/* checkout procedure fills the value into kv->value as printable string */
typedef pef_err_t (*Keyvalue_Checkout) (ipmi_pef_state_data_t *state_data,
                                        const struct section *sect,
                                        struct keyvalue *kv);

/* commit procedure takes string value from kv->value and converts and
   does ipmi calls to set it */
typedef pef_err_t (*Keyvalue_Commit) (ipmi_pef_state_data_t *state_data,
                                      const struct section *sect,
                                      const struct keyvalue *kv);

/* diff procedure finds the difference with the ipmi actual value
   and kv->value */
typedef pef_diff_t (*Keyvalue_Diff) (ipmi_pef_state_data_t *state_data,
                                     const struct section *sect,
                                     const struct keyvalue *kv);

/* validate procedure finds if value is suitable to be set as kv->value */
typedef pef_validate_t (*Keyvalue_Validate) (ipmi_pef_state_data_t *state_data,
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

struct section * ipmi_pef_sections_list_create (ipmi_pef_state_data_t *state_data);

void ipmi_pef_sections_list_destroy (ipmi_pef_state_data_t *state_data,
                                     struct section *sections);

struct section * ipmi_pef_section_create (ipmi_pef_state_data_t *state_data, 
                                          char *section_name);

void ipmi_pef_section_destroy (ipmi_pef_state_data_t *state_data, 
                               struct section *section);

int ipmi_pef_section_add_keyvalue (ipmi_pef_state_data_t *state_data,
                                   struct section *section,
                                   const char *key,
                                   const char *desc,
                                   unsigned int flags,
                                   Keyvalue_Checkout checkout,
                                   Keyvalue_Commit commit,
                                   Keyvalue_Diff diff,
                                   Keyvalue_Validate validate);

struct keyvalue * ipmi_pef_section_find_keyvalue (ipmi_pef_state_data_t *state_data,
                                                  const char *section_name,
                                                  const char *key_name);

int ipmi_pef_section_set_value (ipmi_pef_state_data_t *state_data,
                                const char *section_name,
                                const char *key_name,
                                const char *value);

pef_err_t ipmi_pef_section_commit_value (ipmi_pef_state_data_t *state_data,
                                         const char *section_name,
                                         const char *key_name,
                                         const char *value);

int ipmi_pef_section_diff_value (ipmi_pef_state_data_t *state_data,
                                 const char *section_name,
                                 const char *key_name,
                                 const char *value);

pef_err_t ipmi_pef_sections_list (ipmi_pef_state_data_t *state_data);

#endif /* _IPMI_PEF_SECTIONS */
