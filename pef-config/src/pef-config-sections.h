#ifndef _PEF_CONFIG_SECTIONS
#define _PEF_CONFIG_SECTIONS

#include "pef-config.h"
#include "pef-config-common.h"

struct config_section {
  char *section_name;
  char *section_comment_section_name;
  char *section_comment;
  unsigned int flags;
  struct config_keyvalue *keyvalues;
  struct config_section *next;
};

/* checkout procedure fills the value into kv->value as printable string */
typedef config_err_t (*Key_Checkout) (pef_config_state_data_t *state_data,
                                        const struct config_section *section,
                                        struct config_keyvalue *kv);

/* commit procedure takes string value from kv->value and converts and
   does ipmi calls to set it */
typedef config_err_t (*Key_Commit) (pef_config_state_data_t *state_data,
                                      const struct config_section *section,
                                      const struct config_keyvalue *kv);

/* diff procedure finds the difference with the ipmi actual value
   and kv->value */
typedef config_diff_t (*Key_Diff) (pef_config_state_data_t *state_data,
                                     const struct config_section *section,
                                     const struct config_keyvalue *kv);

/* validate procedure finds if value is suitable to be set as kv->value */
typedef config_validate_t (*Key_Validate) (const char *section_name,
                                                const char *key_name,
                                                const char *value);

struct config_keyvalue {
  const char *key_name;
  const char *description;
  unsigned int flags;
  char *value;
  Key_Checkout checkout;
  Key_Commit commit;
  Key_Diff diff;
  Key_Validate validate;
  struct config_keyvalue *next;
};

struct config_section * pef_config_sections_list_create (pef_config_state_data_t *state_data);

void pef_config_sections_list_destroy (pef_config_state_data_t *state_data,
                                       struct config_section *sections);

struct config_section * pef_config_section_create (pef_config_state_data_t *state_data, 
                                            char *section_name,
                                            char *section_comment_section_name,
                                            char *section_comment,
                                            unsigned int flags);

void pef_config_section_destroy (pef_config_state_data_t *state_data, 
                                 struct config_section *section);

int pef_config_section_add_keyvalue (pef_config_state_data_t *state_data,
                                     struct config_section *section,
                                     const char *key_name,
                                     const char *description,
                                     unsigned int flags,
                                     Key_Checkout checkout,
                                     Key_Commit commit,
                                     Key_Diff diff,
                                     Key_Validate validate);

struct config_keyvalue * pef_config_section_find_keyvalue (pef_config_state_data_t *state_data,
                                                    const char *section_name,
                                                    const char *key_name);

int pef_config_section_set_value (pef_config_state_data_t *state_data,
                                  const char *section_name,
                                  const char *key_name,
                                  const char *value);

config_err_t pef_config_section_commit_value (pef_config_state_data_t *state_data,
                                           const char *section_name,
                                           const char *key_name,
                                           const char *value);

int pef_config_section_diff_value (pef_config_state_data_t *state_data,
                                   const char *section_name,
                                   const char *key_name,
                                   const char *value);

config_err_t pef_config_sections_list (pef_config_state_data_t *state_data);

#endif /* _PEF_CONFIG_SECTIONS */
