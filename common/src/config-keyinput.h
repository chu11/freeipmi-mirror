#ifndef _CONFIG_KEYINPUT_H_
#define _CONFIG_KEYINPUT_H_

#include <stdio.h>

#include "config-common.h"

int config_keyinput_parse_string(char *str,
                                 char **section_name,
                                 char **key_name,
                                 char **value);

int config_keyinput_append(struct config_keyinput **keyinputs,
                           struct config_keyinput *keyinput);

void config_keyinputs_destroy(struct config_keyinput *keyinputs);

struct config_keyinput *config_keyinput_create(const char *section_name,
                                               const char *key_name,
                                               const char *value_input);

void config_keyinput_destroy(struct config_keyinput *keyinput);


#endif /* _CONFIG_KEYINPUT_H_ */
