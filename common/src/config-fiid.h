#ifndef _CONFIG_FIID_H_
#define _CONFIG_FIID_H_

#include <stdio.h>
#include <stdint.h>

#include "config-common.h"

fiid_obj_t Fiid_obj_create(fiid_template_t tmpl);

int8_t Fiid_obj_get(fiid_obj_t obj, char *field, uint64_t *val);

int32_t Fiid_obj_get_data(fiid_obj_t obj,
                          char *field,
                          uint8_t *data,
                          uint32_t data_len);

int32_t Fiid_obj_set_data (fiid_obj_t obj,
                           char *field,
                           uint8_t *data,
                           uint32_t data_len);

int Fiid_obj_clear(fiid_obj_t obj);

void Fiid_obj_destroy(fiid_obj_t obj);

#endif /* _CONFIG_FIID_H_ */
