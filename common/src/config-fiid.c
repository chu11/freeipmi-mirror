#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <errno.h>
#include <assert.h>

#include "config-fiid.h"

fiid_obj_t 
Fiid_obj_create(fiid_template_t tmpl)
{
  fiid_obj_t obj;

  assert(tmpl);

  if (!(obj = fiid_obj_create(tmpl)))
    perror("fiid_obj_create");

  return obj;
}

int8_t
Fiid_obj_get(fiid_obj_t obj, char *field, uint64_t *val)
{
  int8_t ret;

  assert(obj);
  assert(field);
  assert(val);

  if ((ret = fiid_obj_get (obj, field, val)) < 0)
    fprintf(stderr, "fiid_obj_get: %s\n", fiid_strerror(fiid_obj_errnum(obj)));

  return ret;
}

int32_t 
Fiid_obj_get_data (fiid_obj_t obj,
                   char *field,
                   uint8_t *data,
                   uint32_t data_len)
{
  int32_t ret;

  assert(obj);
  assert(field);
  assert(data);
  assert(data_len);

  if ((ret = fiid_obj_get_data (obj, field, data, data_len)) < 0)
    fprintf(stderr, "fiid_obj_get_data: %s\n", fiid_strerror(fiid_obj_errnum(obj)));

  return ret;
}

int32_t 
Fiid_obj_set_data (fiid_obj_t obj,
                   char *field,
                   uint8_t *data,
                   uint32_t data_len)
{
  int32_t ret;

  assert(obj);
  assert(field);
  assert(data);
  assert(data_len);

  if ((ret = fiid_obj_set_data (obj, field, data, data_len)) < 0)
    fprintf(stderr, "fiid_obj_set_data: %s\n", fiid_strerror(fiid_obj_errnum(obj)));

  return ret;
}


int 
Fiid_obj_clear(fiid_obj_t obj)
{
  int8_t ret;

  assert(obj);

  if ((ret = fiid_obj_clear(obj)) < 0)
    fprintf(stderr, "fiid_obj_clear: %s\n", fiid_strerror(fiid_obj_errnum(obj)));

  return ret;
}

void
Fiid_obj_destroy (fiid_obj_t obj)
{
  if (obj)
    fiid_obj_destroy(obj);
}
