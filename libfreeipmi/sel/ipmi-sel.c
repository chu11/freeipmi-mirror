/*
 * Copyright (C) 2003-2014 FreeIPMI Core Team
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <assert.h>
#include <errno.h>

#include "freeipmi/sel/ipmi-sel.h"

#include "freeipmi/api/ipmi-sel-cmds-api.h"
#include "freeipmi/cmds/ipmi-sel-cmds.h"
#include "freeipmi/debug/ipmi-debug.h"
#include "freeipmi/fiid/fiid.h"
#include "freeipmi/interpret/ipmi-interpret.h"
#include "freeipmi/record-format/ipmi-sel-record-format.h"
#include "freeipmi/sdr/ipmi-sdr.h"
#include "freeipmi/spec/ipmi-comp-code-spec.h"
#include "freeipmi/util/ipmi-sensor-and-event-code-tables-util.h"
#include "freeipmi/util/ipmi-timestamp-util.h"
#include "freeipmi/util/ipmi-util.h"

#include "ipmi-sel-common.h"
#include "ipmi-sel-defs.h"
#include "ipmi-sel-string.h"
#include "ipmi-sel-trace.h"
#include "ipmi-sel-util.h"

#include "libcommon/ipmi-fiid-util.h"

#include "freeipmi-portability.h"
#include "debug-util.h"

static char *ipmi_sel_errmsgs[] =
  {
    "success",
    "context null",
    "context invalid",
    "invalid parameters",
    "out of memory",
    "sdr cache error",
    "no sel entries loaded",
    "no sel entries available",
    "invalid sel entry",
    "end of sel entries list reached",
    "not found",
    "reservation canceled",
    "interpret error",
    "callback error",
    "internal IPMI error",
    "internal system error",
    "buffer overflow",
    "internal error",
    "errnum out of range",
    NULL
  };

ipmi_sel_ctx_t
ipmi_sel_ctx_create (ipmi_ctx_t ipmi_ctx, ipmi_sdr_ctx_t sdr_ctx)
{
  struct ipmi_sel_ctx *ctx = NULL;

  /* check that ipmi_ctx is open for use if supplied */
  if (ipmi_ctx)
    {
      if (ipmi_ctx_get_target (ipmi_ctx, NULL, NULL) < 0)
	{
	  SET_ERRNO (EINVAL);
	  return (NULL);
	}
    }

  /* check that sdr_ctx is open for reading if supplied */
  if (sdr_ctx)
    {
      uint8_t tmp;

      if (ipmi_sdr_cache_sdr_version (sdr_ctx, &tmp) < 0)
	{
	  SET_ERRNO (EINVAL);
	  return (NULL);
	}
    }

  if (!(ctx = (ipmi_sel_ctx_t)malloc (sizeof (struct ipmi_sel_ctx))))
    {
      ERRNO_TRACE (errno);
      return (NULL);
    }
  memset (ctx, '\0', sizeof (struct ipmi_sel_ctx));
  ctx->magic = IPMI_SEL_CTX_MAGIC;
  ctx->flags = IPMI_SEL_FLAGS_DEFAULT;
  ctx->manufacturer_id = 0;
  ctx->product_id = 0;
  ctx->ipmi_version_major = 0;
  ctx->ipmi_version_minor = 0;
  ctx->debug_prefix = NULL;
  ctx->separator = NULL;

  ctx->reservation_id = 0;
  ctx->reservation_id_registered = 0;

  ctx->ipmi_ctx = ipmi_ctx;
  ctx->sdr_ctx = sdr_ctx;
  ctx->interpret_ctx = NULL;
  ctx->utc_offset = 0;

  ctx->sel_entries_loaded = 0;

  if (!(ctx->sel_entries = list_create ((ListDelF)free)))
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  return (ctx);

 cleanup:
  if (ctx)
    {
      if (ctx->sel_entries)
        list_destroy (ctx->sel_entries);
      free (ctx);
    }
  return (NULL);
}

static int
_sel_entries_delete_all (void *x, void *y)
{
  return (1);
}

static void
_sel_entries_clear (ipmi_sel_ctx_t ctx)
{
  assert (ctx);
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);

  list_delete_all (ctx->sel_entries, _sel_entries_delete_all, "dummyvalue");

  if (ctx->sel_entries_itr)
    {
      list_iterator_destroy (ctx->sel_entries_itr);
      ctx->sel_entries_itr = NULL;
    }
  ctx->sel_entries_loaded = 0;

  ctx->current_sel_entry = NULL;
  ctx->callback_sel_entry = NULL;
}

void
ipmi_sel_ctx_destroy (ipmi_sel_ctx_t ctx)
{
  if (!ctx || ctx->magic != IPMI_SEL_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_sel_ctx_errormsg (ctx), ipmi_sel_ctx_errnum (ctx));
      return;
    }

  free (ctx->debug_prefix);
  free (ctx->separator);
  _sel_entries_clear (ctx);
  list_destroy (ctx->sel_entries);
  ctx->magic = ~IPMI_SEL_CTX_MAGIC;
  free (ctx);
}

int
ipmi_sel_ctx_errnum (ipmi_sel_ctx_t ctx)
{
  if (!ctx)
    return (IPMI_SEL_ERR_CONTEXT_NULL);
  else if (ctx->magic != IPMI_SEL_CTX_MAGIC)
    return (IPMI_SEL_ERR_CONTEXT_INVALID);
  else
    return (ctx->errnum);
}

char *
ipmi_sel_ctx_strerror (int errnum)
{
  if (errnum >= IPMI_SEL_ERR_SUCCESS && errnum <= IPMI_SEL_ERR_ERRNUMRANGE)
    return (ipmi_sel_errmsgs[errnum]);
  else
    return (ipmi_sel_errmsgs[IPMI_SEL_ERR_ERRNUMRANGE]);
}

char *
ipmi_sel_ctx_errormsg (ipmi_sel_ctx_t ctx)
{
  return (ipmi_sel_ctx_strerror (ipmi_sel_ctx_errnum (ctx)));
}

int
ipmi_sel_ctx_get_flags (ipmi_sel_ctx_t ctx, unsigned int *flags)
{
  if (!ctx || ctx->magic != IPMI_SEL_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_sel_ctx_errormsg (ctx), ipmi_sel_ctx_errnum (ctx));
      return (-1);
    }

  if (!flags)
    {
      SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_PARAMETERS);
      return (-1);
    }

  *flags = ctx->flags;
  ctx->errnum = IPMI_SEL_ERR_SUCCESS;
  return (0);
}

int
ipmi_sel_ctx_set_flags (ipmi_sel_ctx_t ctx, unsigned int flags)
{
  if (!ctx || ctx->magic != IPMI_SEL_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_sel_ctx_errormsg (ctx), ipmi_sel_ctx_errnum (ctx));
      return (-1);
    }

  if (flags & ~IPMI_SEL_FLAGS_MASK)
    {
      SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_PARAMETERS);
      return (-1);
    }

  ctx->flags = flags;

  if (ctx->interpret_ctx)
    {
      unsigned int interpret_flags;

      if (ipmi_interpret_ctx_get_flags (ctx->interpret_ctx, &interpret_flags) < 0)
	{
	  SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_INTERPRET_ERROR);
	  return (-1);
	}

      if (flags & IPMI_SEL_FLAGS_ASSUME_SYTEM_EVENT_RECORDS)
	interpret_flags |= IPMI_INTERPRET_FLAGS_SEL_ASSUME_SYSTEM_EVENT_RECORDS;
      else
	interpret_flags &= ~IPMI_INTERPRET_FLAGS_SEL_ASSUME_SYSTEM_EVENT_RECORDS;
      
      if (ipmi_interpret_ctx_set_flags (ctx->interpret_ctx, interpret_flags) < 0)
	{
	  SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_INTERPRET_ERROR);
	  return (-1);
	}
    }

  ctx->errnum = IPMI_SEL_ERR_SUCCESS;
  return (0);
}

int
ipmi_sel_ctx_get_manufacturer_id (ipmi_sel_ctx_t ctx, uint32_t *manufacturer_id)
{
  if (!ctx || ctx->magic != IPMI_SEL_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_sel_ctx_errormsg (ctx), ipmi_sel_ctx_errnum (ctx));
      return (-1);
    }

  if (!manufacturer_id)
    {
      SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_PARAMETERS);
      return (-1);
    }

  *manufacturer_id = ctx->manufacturer_id;
  ctx->errnum = IPMI_SEL_ERR_SUCCESS;
  return (0);
}

int
ipmi_sel_ctx_set_manufacturer_id (ipmi_sel_ctx_t ctx, uint32_t manufacturer_id)
{
  if (!ctx || ctx->magic != IPMI_SEL_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_sel_ctx_errormsg (ctx), ipmi_sel_ctx_errnum (ctx));
      return (-1);
    }

  ctx->manufacturer_id = manufacturer_id;

  if (ctx->interpret_ctx)
    {
      if (ipmi_interpret_ctx_set_manufacturer_id (ctx->interpret_ctx,
						  ctx->manufacturer_id) < 0)
	{
	  SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_INTERNAL_ERROR);
	  return (-1);
	}
    }

  ctx->errnum = IPMI_SEL_ERR_SUCCESS;
  return (0);
}

int
ipmi_sel_ctx_get_product_id (ipmi_sel_ctx_t ctx, uint16_t *product_id)
{
  if (!ctx || ctx->magic != IPMI_SEL_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_sel_ctx_errormsg (ctx), ipmi_sel_ctx_errnum (ctx));
      return (-1);
    }

  if (!product_id)
    {
      SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_PARAMETERS);
      return (-1);
    }

  *product_id = ctx->product_id;
  ctx->errnum = IPMI_SEL_ERR_SUCCESS;
  return (0);
}

int
ipmi_sel_ctx_set_product_id (ipmi_sel_ctx_t ctx, uint16_t product_id)
{
  if (!ctx || ctx->magic != IPMI_SEL_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_sel_ctx_errormsg (ctx), ipmi_sel_ctx_errnum (ctx));
      return (-1);
    }

  ctx->product_id = product_id;

  if (ctx->interpret_ctx)
    {
      if (ipmi_interpret_ctx_set_product_id (ctx->interpret_ctx,
					     ctx->product_id) < 0)
	{
	  SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_INTERNAL_ERROR);
	  return (-1);
	}
    }

  ctx->errnum = IPMI_SEL_ERR_SUCCESS;
  return (0);
}

int
ipmi_sel_ctx_get_ipmi_version (ipmi_sel_ctx_t ctx,
			       uint8_t *ipmi_version_major,
			       uint8_t *ipmi_version_minor)
{
  if (!ctx || ctx->magic != IPMI_SEL_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_sel_ctx_errormsg (ctx), ipmi_sel_ctx_errnum (ctx));
      return (-1);
    }

  if (!ipmi_version_major || !ipmi_version_minor)
    {
      SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_PARAMETERS);
      return (-1);
    }

  *ipmi_version_major = ctx->ipmi_version_major;
  *ipmi_version_minor = ctx->ipmi_version_minor;
  ctx->errnum = IPMI_SEL_ERR_SUCCESS;
  return (0);
}

int
ipmi_sel_ctx_set_ipmi_version (ipmi_sel_ctx_t ctx,
			       uint8_t ipmi_version_major,
			       uint8_t ipmi_version_minor)
{
  if (!ctx || ctx->magic != IPMI_SEL_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_sel_ctx_errormsg (ctx), ipmi_sel_ctx_errnum (ctx));
      return (-1);
    }

  ctx->ipmi_version_major = ipmi_version_major;
  ctx->ipmi_version_minor = ipmi_version_minor;
  ctx->errnum = IPMI_SEL_ERR_SUCCESS;
  return (0);
}

int
ipmi_sel_ctx_get_parameter (ipmi_sel_ctx_t ctx,
			    unsigned int parameter,
			    void *ptr)
{
  if (!ctx || ctx->magic != IPMI_SEL_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_sel_ctx_errormsg (ctx), ipmi_sel_ctx_errnum (ctx));
      return (-1);
    }

  if (!ptr)
    {
      SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_PARAMETERS);
      return (-1);
    }

  switch (parameter)
    {
    case IPMI_SEL_PARAMETER_INTERPRET_CONTEXT:
      (*(ipmi_interpret_ctx_t *)ptr) = ctx->interpret_ctx;
      break;
    case IPMI_SEL_PARAMETER_UTC_OFFSET:
      (*(int *)ptr) = ctx->utc_offset;
      break;
    default:
      SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_PARAMETERS);
      return (-1);
    }
  
  ctx->errnum = IPMI_SEL_ERR_SUCCESS;
  return (0);
} 

int
ipmi_sel_ctx_set_parameter (ipmi_sel_ctx_t ctx,
			    unsigned int parameter,
			    const void *ptr)
{
  if (!ctx || ctx->magic != IPMI_SEL_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_sel_ctx_errormsg (ctx), ipmi_sel_ctx_errnum (ctx));
      return (-1);
    }

  switch (parameter)
    {
    case IPMI_SEL_PARAMETER_INTERPRET_CONTEXT:
      if (ptr)
	{
	  ipmi_interpret_ctx_t interpret_ctx;
	  uint16_t tmp;
	  
	  interpret_ctx = *((ipmi_interpret_ctx_t *)ptr);

	  /* test to make sure interpret_ctx legit */
	  if (ipmi_interpret_ctx_get_product_id (interpret_ctx, &tmp) < 0)
	    {
	      SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_PARAMETERS);
	      return (-1);
	    }
	  ctx->interpret_ctx = interpret_ctx;
	}
      else
	ctx->interpret_ctx = NULL;
      break;
    case IPMI_SEL_PARAMETER_UTC_OFFSET:
      if (ptr)
	{
	  int tmp = *((int *)ptr);
	  
	  if (!IPMI_UTC_OFFSET_VALID (tmp))
	    {
	      SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_PARAMETERS);
	      return (-1);
	    }
	  ctx->utc_offset = tmp;
	}
      else
	ctx->utc_offset = 0;
      break;
    default:
      SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_PARAMETERS);
      return (-1);
    }
  
  ctx->errnum = IPMI_SEL_ERR_SUCCESS;
  return (0);
} 

char *
ipmi_sel_ctx_get_debug_prefix (ipmi_sel_ctx_t ctx)
{
  if (!ctx || ctx->magic != IPMI_SEL_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_sel_ctx_errormsg (ctx), ipmi_sel_ctx_errnum (ctx));
      return (NULL);
    }

  ctx->errnum = IPMI_SEL_ERR_SUCCESS;
  return (ctx)->debug_prefix;
}

int
ipmi_sel_ctx_set_debug_prefix (ipmi_sel_ctx_t ctx, const char *debug_prefix)
{
  if (!ctx || ctx->magic != IPMI_SEL_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_sel_ctx_errormsg (ctx), ipmi_sel_ctx_errnum (ctx));
      return (-1);
    }

  free (ctx->debug_prefix);
  ctx->debug_prefix = NULL;

  if (debug_prefix)
    {
      if (!(ctx->debug_prefix = strdup (debug_prefix)))
        {
          SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_OUT_OF_MEMORY);
          return (-1);
        }
    }

  ctx->errnum = IPMI_SEL_ERR_SUCCESS;
  return (0);
}

char *
ipmi_sel_ctx_get_separator (ipmi_sel_ctx_t ctx)
{
  if (!ctx || ctx->magic != IPMI_SEL_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_sel_ctx_errormsg (ctx), ipmi_sel_ctx_errnum (ctx));
      return (NULL);
    }

  ctx->errnum = IPMI_SEL_ERR_SUCCESS;
  return (ctx)->separator;
}

int
ipmi_sel_ctx_set_separator (ipmi_sel_ctx_t ctx, const char *separator)
{
  if (!ctx || ctx->magic != IPMI_SEL_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_sel_ctx_errormsg (ctx), ipmi_sel_ctx_errnum (ctx));
      return (-1);
    }

  free (ctx->separator);
  ctx->separator = NULL;

  if (separator)
    {
      if (!(ctx->separator = strdup (separator)))
        {
          SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_OUT_OF_MEMORY);
          return (-1);
        }

      if (ipmi_event_message_separator (ctx->separator) < 0)
        {
          SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_INTERNAL_ERROR);
          return (-1);
        }
    }

  ctx->errnum = IPMI_SEL_ERR_SUCCESS;
  return (0);
}

int
ipmi_sel_ctx_register_reservation_id (ipmi_sel_ctx_t ctx, uint16_t *reservation_id)
{
  if (!ctx || ctx->magic != IPMI_SEL_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_sel_ctx_errormsg (ctx), ipmi_sel_ctx_errnum (ctx));
      return (-1);
    }

  if (!ctx->ipmi_ctx)
    {
      SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_IPMI_ERROR);
      return (-1);
    }

  if (sel_get_reservation_id (ctx, &ctx->reservation_id, NULL) < 0)
    return (-1);

  /* Possible reservation ID not supported */
  if (!ctx->reservation_id)
    {
      SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_IPMI_ERROR);
      return (-1);
    }
    
  if (reservation_id)
    *reservation_id = ctx->reservation_id;

  ctx->reservation_id_registered = 1;

  ctx->errnum = IPMI_SEL_ERR_SUCCESS;
  return (0);
}

int
ipmi_sel_ctx_clear_reservation_id (ipmi_sel_ctx_t ctx)
{
  if (!ctx || ctx->magic != IPMI_SEL_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_sel_ctx_errormsg (ctx), ipmi_sel_ctx_errnum (ctx));
      return (-1);
    }

  ctx->reservation_id = 0;
  ctx->reservation_id_registered = 0;

  ctx->errnum = IPMI_SEL_ERR_SUCCESS;
  return (0);
}

static void
_sel_entry_dump (ipmi_sel_ctx_t ctx, struct ipmi_sel_entry *sel_entry)
{
  fiid_obj_t obj_sel_record = NULL;
  char hdrbuf[DEBUG_UTIL_HDR_BUFLEN];
  uint8_t record_type;
  int record_type_class;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);
  assert (sel_entry);

  if (!(ctx->flags & IPMI_SEL_FLAGS_DEBUG_DUMP))
    return;

  if (sel_get_record_header_info (ctx,
				  sel_entry,
				  NULL,
				  &record_type) < 0)
    goto cleanup;

  record_type_class = ipmi_sel_record_type_class (record_type);

  if (record_type_class == IPMI_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD)
    {
      struct ipmi_sel_system_event_record_data system_event_record_data;
      int event_type_code_class;

      if (sel_get_system_event_record (ctx,
				       sel_entry,
				       &system_event_record_data) < 0)
        {
          if (ctx->errnum == IPMI_SEL_ERR_INVALID_SEL_ENTRY)
            {
              if (!(obj_sel_record = fiid_obj_create (tmpl_sel_system_event_record)))
                {
                  SEL_ERRNO_TO_SEL_ERRNUM (ctx, errno);
                  goto cleanup;
                }
              goto output;
            }
          goto cleanup;
        }

      event_type_code_class = ipmi_event_reading_type_code_class (system_event_record_data.event_type_code);

      if (event_type_code_class == IPMI_EVENT_READING_TYPE_CODE_CLASS_THRESHOLD)
        {
          if (!(obj_sel_record = fiid_obj_create (tmpl_sel_system_event_record_event_fields)))
            {
              SEL_ERRNO_TO_SEL_ERRNUM (ctx, errno);
              goto cleanup;
            }
        }
      else if (event_type_code_class == IPMI_EVENT_READING_TYPE_CODE_CLASS_GENERIC_DISCRETE
               || event_type_code_class == IPMI_EVENT_READING_TYPE_CODE_CLASS_SENSOR_SPECIFIC_DISCRETE)
        {
          if (system_event_record_data.event_data2_flag == IPMI_SEL_EVENT_DATA_PREVIOUS_STATE_OR_SEVERITY)
            {
              if (!(obj_sel_record = fiid_obj_create (tmpl_sel_system_event_record_discrete_previous_state_severity)))
                {
                  SEL_ERRNO_TO_SEL_ERRNUM (ctx, errno);
                  goto cleanup;
                }
            }
          else
            {
              if (!(obj_sel_record = fiid_obj_create (tmpl_sel_system_event_record_event_fields)))
                {
                  SEL_ERRNO_TO_SEL_ERRNUM (ctx, errno);
                  goto cleanup;
                }
            }
        }
      else
        {
          if (!(obj_sel_record = fiid_obj_create (tmpl_sel_system_event_record)))
            {
              SEL_ERRNO_TO_SEL_ERRNUM (ctx, errno);
              goto cleanup;
            }
        }
    }
  else if (record_type_class == IPMI_SEL_RECORD_TYPE_CLASS_TIMESTAMPED_OEM_RECORD)
    {
      if (!(obj_sel_record = fiid_obj_create (tmpl_sel_timestamped_oem_record)))
        {
          SEL_ERRNO_TO_SEL_ERRNUM (ctx, errno);
          goto cleanup;
        }
    }
  else if (record_type_class == IPMI_SEL_RECORD_TYPE_CLASS_NON_TIMESTAMPED_OEM_RECORD)
    {
      if (!(obj_sel_record = fiid_obj_create (tmpl_sel_non_timestamped_oem_record)))
        {
          SEL_ERRNO_TO_SEL_ERRNUM (ctx, errno);
          goto cleanup;
        }
    }
  else /* record_type_class == SEL_RECORD_TYPE_UNKNOWN */
    {
      if (!(obj_sel_record = fiid_obj_create (tmpl_sel_system_event_record)))
        {
          SEL_ERRNO_TO_SEL_ERRNUM (ctx, errno);
          goto cleanup;
        }
    }

  if (fiid_obj_set_all (obj_sel_record,
                        sel_entry->sel_event_record,
                        sel_entry->sel_event_record_len) < 0)
    {
      SEL_FIID_OBJECT_ERROR_TO_SEL_ERRNUM (ctx, obj_sel_record);
      goto cleanup;
    }

 output:
  debug_hdr_str (DEBUG_UTIL_TYPE_NONE,
                 DEBUG_UTIL_DIRECTION_NONE,
		 DEBUG_UTIL_FLAGS_DEFAULT,
                 "SEL Event Record",
                 hdrbuf,
                 DEBUG_UTIL_HDR_BUFLEN);

  if (ipmi_obj_dump (STDERR_FILENO,
                     ctx->debug_prefix,
                     hdrbuf,
                     NULL,
                     obj_sel_record) < 0)
    {
      SEL_ERRNO_TO_SEL_ERRNUM (ctx, errno);
      goto cleanup;
    }

#if WITH_RAWDUMPS
  /* For those vendors that get confused when they see the nice output
   * and want the hex output
   */
  if (ipmi_dump_hex (STDERR_FILENO,
		     ctx->debug_prefix,
		     hdrbuf,
		     NULL,
		     sel_entry->sel_event_record,
		     sel_entry->sel_event_record_len) < 0)
    {
      SEL_ERRNO_TO_SEL_ERRNUM (ctx, errno);
      goto cleanup;
    }
#endif

 cleanup:
  fiid_obj_destroy (obj_sel_record);
}

static int
_get_sel_entry (ipmi_sel_ctx_t ctx,
                fiid_obj_t obj_cmd_rs,
                uint16_t *reservation_id,
                int *reservation_id_initialized,
                uint16_t record_id)
{
  unsigned int reservation_id_retry_count = 0;
  unsigned int reservation_canceled = 0;
  unsigned int is_insufficient_privilege_level = 0;
  int rv = -1;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);
  assert (ctx->ipmi_ctx);
  assert (fiid_obj_valid (obj_cmd_rs) == 1);
  assert (fiid_obj_template_compare (obj_cmd_rs, tmpl_cmd_get_sel_entry_rs) == 1);
  assert (reservation_id);

  /*
   * IPMI spec states in section 31.4.1:
   *
   * "A Requester must issue a 'Reserve SEL' command prior to issuing
   * any of the following SEL commands. Note that the 'Reserve SEL'
   * command only needs to be reissued if the reservation is
   * canceled. ... Get SEL Entry command (if 'get' is from an offset
   * other than 00h)".
   *
   * Since we always use an offset of 00h, presumably we should never
   * need reserve the SEL before the get_sel_entry call.
   *
   * However, some machines may need it due to compliance issues.
   * I don't think using a reservation ID all of the time hurts
   * anything, so we'll just use it all of the time.
   */

  while (1)
    {
      if (!(*reservation_id_initialized) || reservation_canceled)
        {
	  if (ctx->reservation_id_registered)
	    (*reservation_id) = ctx->reservation_id;
	  else
	    {
	      if (sel_get_reservation_id (ctx, reservation_id, &is_insufficient_privilege_level) < 0)
		{
		  /* IPMI Workaround (achu)
		   *
		   * Discovered on Supermicro H8QME with SIMSO daughter card.
		   *
		   * For some reason motherboard requires Operator
		   * privilege instead of User privilege.  If
		   * IPMI_COMP_CODE_INSUFFICIENT_PRIVILEGE_LEVEL was
		   * received, just use reservation ID 0. For the reasons
		   * listed above, it shouldn't matter.
		   */
		  if (is_insufficient_privilege_level)
		    (*reservation_id) = 0;
		  else
		    goto cleanup;
		}
	    }
          (*reservation_id_initialized)++;
        }
      
      if (ipmi_cmd_get_sel_entry (ctx->ipmi_ctx,
                                  (*reservation_id),
                                  record_id,
                                  0,
                                  IPMI_SEL_READ_ENTIRE_RECORD_BYTES_TO_READ,
                                  obj_cmd_rs) < 0)
        {
          if (ipmi_ctx_errnum (ctx->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE
              && ipmi_check_completion_code (obj_cmd_rs,
                                             IPMI_COMP_CODE_RESERVATION_CANCELLED) == 1)
            {
	      if (ctx->reservation_id_registered)
		{
		  SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_RESERVATION_CANCELED);
		  goto cleanup;
		}
              else
		{
		  reservation_id_retry_count++;
		  reservation_canceled++;
		  (*reservation_id_initialized) = 0;
		  
		  if (reservation_id_retry_count > IPMI_SEL_RESERVATION_ID_RETRY)
		    {
		      SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_IPMI_ERROR);
		      goto cleanup;
		    }
		  
		  continue;
		}
            }
          else
            {
              SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_IPMI_ERROR);
              goto cleanup;
            }
        }
      
      break;
    }
  
  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_sel_parse (ipmi_sel_ctx_t ctx,
                uint16_t record_id_start,
                uint16_t record_id_last,
                Ipmi_Sel_Parse_Callback callback,
                void *callback_data)
{
  struct ipmi_sel_entry *sel_entry = NULL;
  uint16_t reservation_id = 0;
  int reservation_id_initialized = 0;
  uint16_t record_id = 0;
  uint16_t next_record_id = 0;
  int parsed_atleast_one_entry = 0;
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  int len;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_SEL_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_sel_ctx_errormsg (ctx), ipmi_sel_ctx_errnum (ctx));
      return (-1);
    }

  if (!ctx->ipmi_ctx)
    {
      SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_IPMI_ERROR);
      return (-1);
    }

  if (record_id_start > record_id_last)
    {
      SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_PARAMETERS);
      return (-1);
    }

  _sel_entries_clear (ctx);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_sel_entry_rs)))
    {
      SEL_ERRNO_TO_SEL_ERRNUM (ctx, errno);
      goto cleanup;
    }

  /* if caller requests a range, get last record_id and check against
   * input so we don't spin
   */
  if (record_id_last != IPMI_SEL_GET_RECORD_ID_LAST_ENTRY)
    {
      struct ipmi_sel_entry tmp_sel_entry;
      uint16_t tmp_record_id_last = 0;

      if (_get_sel_entry (ctx,
                          obj_cmd_rs,
                          &reservation_id,
                          &reservation_id_initialized,
                          IPMI_SEL_GET_RECORD_ID_LAST_ENTRY) < 0)
	{
          if (ipmi_ctx_errnum (ctx->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE
              && ipmi_check_completion_code (obj_cmd_rs,
                                             IPMI_COMP_CODE_REQUESTED_SENSOR_DATA_OR_RECORD_NOT_PRESENT) == 1)
            {
              /* If the sel is empty it's not really an error */
              goto out;
            }
	  goto cleanup;
	}

      memset (&tmp_sel_entry, '\0', sizeof (struct ipmi_sel_entry));
          
      if ((len = fiid_obj_get_data (obj_cmd_rs,
                                    "record_data",
                                    tmp_sel_entry.sel_event_record,
                                    IPMI_SEL_RECORD_LENGTH)) < 0)
        {
          SEL_FIID_OBJECT_ERROR_TO_SEL_ERRNUM (ctx, obj_cmd_rs);
          goto cleanup;
        }
      
      tmp_sel_entry.sel_event_record_len = len;

      if (sel_get_record_header_info (ctx,
				      &tmp_sel_entry,
				      &tmp_record_id_last,
				      NULL) < 0)
        goto cleanup;

      if (record_id_last > tmp_record_id_last)
        record_id_last = tmp_record_id_last;
    }

  /* special case, need only get the last record */
  if (record_id_start == IPMI_SEL_GET_RECORD_ID_LAST_ENTRY)
    {
      if (_get_sel_entry (ctx,
                          obj_cmd_rs,
                          &reservation_id,
                          &reservation_id_initialized,
                          record_id_start) < 0)
	{
          if (ipmi_ctx_errnum (ctx->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE
              && ipmi_check_completion_code (obj_cmd_rs,
                                             IPMI_COMP_CODE_REQUESTED_SENSOR_DATA_OR_RECORD_NOT_PRESENT) == 1)
            {
              /* If the sel is empty it's not really an error */
              goto out;
            }
	  goto cleanup;
	}

      if (!(sel_entry = (struct ipmi_sel_entry *)malloc (sizeof (struct ipmi_sel_entry))))
        {
          SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_OUT_OF_MEMORY);
          goto cleanup;
        }

      if ((len = fiid_obj_get_data (obj_cmd_rs,
                                    "record_data",
                                    sel_entry->sel_event_record,
                                    IPMI_SEL_RECORD_LENGTH)) < 0)
        {
          SEL_FIID_OBJECT_ERROR_TO_SEL_ERRNUM (ctx, obj_cmd_rs);
          goto cleanup;
        }
      
      sel_entry->sel_event_record_len = len;
     
      _sel_entry_dump (ctx, sel_entry);
      
      /* achu: should come before list_append to avoid having a freed entry on the list */
      if (callback)
        {
          ctx->callback_sel_entry = sel_entry;
          if ((*callback)(ctx, callback_data) < 0)
            {
              SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_CALLBACK_ERROR);
              goto cleanup;
            }
        }

      if (!list_append (ctx->sel_entries, sel_entry))
        {
          SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_INTERNAL_ERROR);
          goto cleanup;
        }
      sel_entry = NULL;

      goto out;
    }

  for (record_id = record_id_start;
       record_id <= record_id_last && record_id != IPMI_SEL_GET_RECORD_ID_LAST_ENTRY;
       record_id = next_record_id)
    {
      if (_get_sel_entry (ctx,
                          obj_cmd_rs,
                          &reservation_id,
                          &reservation_id_initialized,
                          record_id) < 0)
        {
          if (record_id == IPMI_SEL_GET_RECORD_ID_FIRST_ENTRY
              && ipmi_ctx_errnum (ctx->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE
              && ipmi_check_completion_code (obj_cmd_rs,
                                             IPMI_COMP_CODE_REQUESTED_SENSOR_DATA_OR_RECORD_NOT_PRESENT) == 1)
            {
              /* If the sel is empty it's not really an error */
              goto out;
            }
          else if (record_id_start != IPMI_SEL_GET_RECORD_ID_FIRST_ENTRY
                   && !parsed_atleast_one_entry
                   && ipmi_ctx_errnum (ctx->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE
                   && ipmi_check_completion_code (obj_cmd_rs,
                                                  IPMI_COMP_CODE_REQUESTED_SENSOR_DATA_OR_RECORD_NOT_PRESENT) == 1)
            {
              /* user input a starting record id, we didn't find something yet, so iterate until we do */
              next_record_id = record_id + 1;
              continue;
            }
          /* else */
          goto cleanup;
        }

      if (!parsed_atleast_one_entry)
        parsed_atleast_one_entry++;
      
      if (FIID_OBJ_GET (obj_cmd_rs, "next_record_id", &val) < 0)
        {
          SEL_FIID_OBJECT_ERROR_TO_SEL_ERRNUM (ctx, obj_cmd_rs);
          goto cleanup;
        }
      next_record_id = val;

      if (!(sel_entry = (struct ipmi_sel_entry *)malloc (sizeof (struct ipmi_sel_entry))))
        {
          SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_OUT_OF_MEMORY);
          goto cleanup;
        }

      if ((len = fiid_obj_get_data (obj_cmd_rs,
                                    "record_data",
                                    sel_entry->sel_event_record,
                                    IPMI_SEL_RECORD_LENGTH)) < 0)
        {
          SEL_FIID_OBJECT_ERROR_TO_SEL_ERRNUM (ctx, obj_cmd_rs);
          goto cleanup;
        }
      
      sel_entry->sel_event_record_len = len;
      
      _sel_entry_dump (ctx, sel_entry);
      
      /* achu: should come before list_append to avoid having a freed entry on the list */
      if (callback)
        {
          ctx->callback_sel_entry = sel_entry;
          if ((*callback)(ctx, callback_data) < 0)
            {
              SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_CALLBACK_ERROR);
              goto cleanup;
            }
        }

      if (!list_append (ctx->sel_entries, sel_entry))
        {
          SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_INTERNAL_ERROR);
          goto cleanup;
        }
      sel_entry = NULL;
    }

 out:

  if ((rv = list_count (ctx->sel_entries)) > 0)
    {
      if (!(ctx->sel_entries_itr = list_iterator_create (ctx->sel_entries)))
        {
          SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_INTERNAL_ERROR);
          goto cleanup;
        }
      ctx->current_sel_entry = list_next (ctx->sel_entries_itr);
    }
  ctx->sel_entries_loaded = 1; 

  ctx->errnum = IPMI_SEL_ERR_SUCCESS;
 cleanup:
  ctx->callback_sel_entry = NULL;
  free (sel_entry);
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

int
ipmi_sel_parse_record_ids (ipmi_sel_ctx_t ctx,
                           uint16_t *record_ids,
                           unsigned int record_ids_len,
                           Ipmi_Sel_Parse_Callback callback,
                           void *callback_data)
{
  struct ipmi_sel_entry *sel_entry = NULL;
  uint16_t reservation_id = 0;
  int reservation_id_initialized = 0;
  unsigned int i;
  fiid_obj_t obj_cmd_rs = NULL;
  int len;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_SEL_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_sel_ctx_errormsg (ctx), ipmi_sel_ctx_errnum (ctx));
      return (-1);
    }

  if (!ctx->ipmi_ctx)
    {
      SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_IPMI_ERROR);
      return (-1);
    }

  if (!record_ids || !record_ids_len)
    {
      SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_PARAMETERS);
      return (-1);
    }

  _sel_entries_clear (ctx);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_sel_entry_rs)))
    {
      SEL_ERRNO_TO_SEL_ERRNUM (ctx, errno);
      goto cleanup;
    }

  for (i = 0; i < record_ids_len; i++)
    {
      if (_get_sel_entry (ctx,
                          obj_cmd_rs,
                          &reservation_id,
                          &reservation_id_initialized,
                          record_ids[i]) < 0)
        {
          if (ipmi_ctx_errnum (ctx->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE
              && ipmi_check_completion_code (obj_cmd_rs,
                                             IPMI_COMP_CODE_REQUESTED_SENSOR_DATA_OR_RECORD_NOT_PRESENT) == 1)
            {
              /* record not available, ok continue on */
              continue;
            }
          /* else */
          goto cleanup;
        }

      if (!(sel_entry = (struct ipmi_sel_entry *)malloc (sizeof (struct ipmi_sel_entry))))
        {
          SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_OUT_OF_MEMORY);
          goto cleanup;
        }

      if ((len = fiid_obj_get_data (obj_cmd_rs,
                                    "record_data",
                                    sel_entry->sel_event_record,
                                    IPMI_SEL_RECORD_LENGTH)) < 0)
        {
          SEL_FIID_OBJECT_ERROR_TO_SEL_ERRNUM (ctx, obj_cmd_rs);
          goto cleanup;
        }
      
      sel_entry->sel_event_record_len = len;
      
      _sel_entry_dump (ctx, sel_entry);
      
      /* achu: should come before list_append to avoid having a freed entry on the list */
      if (callback)
        {
          ctx->callback_sel_entry = sel_entry;
          if ((*callback)(ctx, callback_data) < 0)
            {
              SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_CALLBACK_ERROR);
              goto cleanup;
            }
        }

      if (!list_append (ctx->sel_entries, sel_entry))
        {
          SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_INTERNAL_ERROR);
          goto cleanup;
        }
      sel_entry = NULL;
    }

  if ((rv = list_count (ctx->sel_entries)) > 0)
    {
      if (!(ctx->sel_entries_itr = list_iterator_create (ctx->sel_entries)))
        {
          SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_INTERNAL_ERROR);
          goto cleanup;
        }
      ctx->current_sel_entry = list_next (ctx->sel_entries_itr);
    }
  ctx->sel_entries_loaded = 1; 

  ctx->errnum = IPMI_SEL_ERR_SUCCESS;
 cleanup:
  ctx->callback_sel_entry = NULL;
  free (sel_entry);
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

int
ipmi_sel_parse_first (ipmi_sel_ctx_t ctx)
{
  if (!ctx || ctx->magic != IPMI_SEL_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_sel_ctx_errormsg (ctx), ipmi_sel_ctx_errnum (ctx));
      return (-1);
    }

  if (!ctx->sel_entries_loaded)
    {
      SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_SEL_ENTRIES_NOT_LOADED);
      return (-1);
    }

  if (!ctx->sel_entries_itr)
    {
      SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_NO_SEL_ENTRIES);
      return (-1);
    }

  list_iterator_reset (ctx->sel_entries_itr);
  ctx->current_sel_entry = list_next (ctx->sel_entries_itr);
  return (0);
}

int
ipmi_sel_parse_next (ipmi_sel_ctx_t ctx)
{
  if (!ctx || ctx->magic != IPMI_SEL_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_sel_ctx_errormsg (ctx), ipmi_sel_ctx_errnum (ctx));
      return (-1);
    }

  if (!ctx->sel_entries_loaded)
    {
      SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_SEL_ENTRIES_NOT_LOADED);
      return (-1);
    }

  if (!ctx->sel_entries_itr)
    {
      SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_NO_SEL_ENTRIES);
      return (-1);
    }

  ctx->current_sel_entry = list_next (ctx->sel_entries_itr);
  return ((ctx->current_sel_entry) ? 1 : 0);
}

int
ipmi_sel_parse_sel_entry_count (ipmi_sel_ctx_t ctx)
{
  if (!ctx || ctx->magic != IPMI_SEL_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_sel_ctx_errormsg (ctx), ipmi_sel_ctx_errnum (ctx));
      return (-1);
    }

  if (!ctx->sel_entries_loaded)
    {
      SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_SEL_ENTRIES_NOT_LOADED);
      return (-1);
    }

  if (!ctx->sel_entries_itr)
    {
      SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_NO_SEL_ENTRIES);
      return (-1);
    }

  return (list_count (ctx->sel_entries));
}

static int
_ipmi_sel_parse_find_record_id (ipmi_sel_ctx_t ctx,
                                uint16_t record_id,
                                unsigned int exact_match_flag)
{
  struct ipmi_sel_entry *sel_entry;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_SEL_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_sel_ctx_errormsg (ctx), ipmi_sel_ctx_errnum (ctx));
      return (-1);
    }

  if (!ctx->sel_entries_loaded)
    {
      SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_SEL_ENTRIES_NOT_LOADED);
      return (-1);
    }

  if (!ctx->sel_entries_itr)
    {
      SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_NO_SEL_ENTRIES);
      return (-1);
    }

  list_iterator_reset (ctx->sel_entries_itr);

  while ((sel_entry = list_next (ctx->sel_entries_itr)))
    {
      uint16_t current_record_id;

      if (sel_get_record_header_info (ctx,
				      sel_entry,
				      &current_record_id,
				      NULL) < 0)
        {
          /* if it was an invalid SEL entry, continue on */
          if (ctx->errnum == IPMI_SEL_ERR_INVALID_SEL_ENTRY)
            continue;
          goto cleanup;
        }

      if ((exact_match_flag
           && current_record_id == record_id)
          || (!exact_match_flag
              && current_record_id >= record_id))
        {
          rv = 0;
          ctx->errnum = IPMI_SEL_ERR_SUCCESS;
          ctx->current_sel_entry = sel_entry;
          goto cleanup;
        }
    }

  SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_NOT_FOUND);
  list_iterator_reset (ctx->sel_entries_itr);
 cleanup:
  return (rv);
}

int
ipmi_sel_parse_seek_record_id (ipmi_sel_ctx_t ctx, uint16_t record_id)
{
  return (_ipmi_sel_parse_find_record_id (ctx,
                                          record_id,
                                          0));
}

int
ipmi_sel_parse_search_record_id (ipmi_sel_ctx_t ctx, uint16_t record_id)
{
  return (_ipmi_sel_parse_find_record_id (ctx,
                                          record_id,
                                          1));
}

int
_get_parse_sel_entry_common (ipmi_sel_ctx_t ctx,
			     struct ipmi_sel_entry **sel_entry)
{
  assert (ctx);
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);
  assert (sel_entry);

  if (ctx->callback_sel_entry)
    *sel_entry = ctx->callback_sel_entry;
  else
    {
      if (!ctx->sel_entries_loaded)
	{
	  SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_SEL_ENTRIES_NOT_LOADED);
	  return (-1);
	}
      
      if (!ctx->sel_entries_itr)
	{
	  SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_NO_SEL_ENTRIES);
	  return (-1);
	}

      *sel_entry = ctx->current_sel_entry;
    }

  if (!(*sel_entry))
    {
      SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_SEL_ENTRIES_LIST_END);
      return (-1);
    }

  return (0);
}

int
ipmi_sel_parse_read_record (ipmi_sel_ctx_t ctx,
                            void *buf,
                            unsigned int buflen)
{
  struct ipmi_sel_entry *sel_entry = NULL;
  int rv = 0;

  if (!ctx || ctx->magic != IPMI_SEL_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_sel_ctx_errormsg (ctx), ipmi_sel_ctx_errnum (ctx));
      return (-1);
    }

  if (!buf || !buflen)
    {
      SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_PARAMETERS);
      return (-1);
    }

  if (_get_parse_sel_entry_common (ctx, &sel_entry) < 0)
    return (-1);

  if (buflen < sel_entry->sel_event_record_len)
    {
      SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_OVERFLOW);
      return (-1);
    }

  memcpy (buf,
          sel_entry->sel_event_record,
          sel_entry->sel_event_record_len);

  rv = sel_entry->sel_event_record_len;

  ctx->errnum = IPMI_SEL_ERR_SUCCESS;
  return (rv);
}

static int
_sel_parse_read_common (ipmi_sel_ctx_t ctx,
			const void *sel_record,
			unsigned int sel_record_len,
			struct ipmi_sel_entry **sel_entry_ptr,
			struct ipmi_sel_entry *sel_entry_buf)
{
  if (!ctx || ctx->magic != IPMI_SEL_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_sel_ctx_errormsg (ctx), ipmi_sel_ctx_errnum (ctx));
      return (-1);
    }

  if (!sel_record && !sel_record_len)
    {
      if (_get_parse_sel_entry_common (ctx, sel_entry_ptr) < 0)
	return (-1);
    }
  else
    {
      if (!sel_record
	  || !sel_record_len)
	{
	  SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_PARAMETERS);
	  return (-1);
	}

      memset (sel_entry_buf, '\0', sizeof (struct ipmi_sel_entry));
      sel_entry_buf->sel_event_record_len = sel_record_len > IPMI_SEL_RECORD_LENGTH ? IPMI_SEL_RECORD_LENGTH : sel_record_len;
      memcpy (sel_entry_buf->sel_event_record,
	      sel_record,
	      sel_entry_buf->sel_event_record_len);
      *sel_entry_ptr = sel_entry_buf;
    }

  return (0);
}

int
ipmi_sel_parse_read_record_id (ipmi_sel_ctx_t ctx,
			       const void *sel_record,
			       unsigned int sel_record_len,
			       uint16_t *record_id)
{
  struct ipmi_sel_entry *sel_entry_ptr = NULL;
  struct ipmi_sel_entry sel_entry;

  if (_sel_parse_read_common (ctx,
			      sel_record,
			      sel_record_len,
			      &sel_entry_ptr,
			      &sel_entry) < 0)
    return (-1);

  if (sel_get_record_header_info (ctx,
				  sel_entry_ptr,
				  record_id,
				  NULL) < 0)
    return (-1);

  ctx->errnum = IPMI_SEL_ERR_SUCCESS;
  return (0);
}

int
ipmi_sel_parse_read_record_type (ipmi_sel_ctx_t ctx,
                                 const void *sel_record,
                                 unsigned int sel_record_len,
				 uint8_t *record_type)
{
  struct ipmi_sel_entry *sel_entry_ptr = NULL;
  struct ipmi_sel_entry sel_entry;

  if (_sel_parse_read_common (ctx,
			      sel_record,
			      sel_record_len,
			      &sel_entry_ptr,
			      &sel_entry) < 0)
    return (-1);

  if (sel_get_record_header_info (ctx,
				  sel_entry_ptr,
				  NULL,
				  record_type) < 0)
    return (-1);

  ctx->errnum = IPMI_SEL_ERR_SUCCESS;
  return (0);
}

int
ipmi_sel_parse_read_timestamp (ipmi_sel_ctx_t ctx,
			       const void *sel_record,
			       unsigned int sel_record_len,
			       uint32_t *timestamp)
{
  struct ipmi_sel_entry *sel_entry_ptr = NULL;
  struct ipmi_sel_entry sel_entry;

  if (_sel_parse_read_common (ctx,
			      sel_record,
			      sel_record_len,
			      &sel_entry_ptr,
			      &sel_entry) < 0)
    return (-1);

  if (sel_get_timestamp (ctx,
			 sel_entry_ptr,
			 timestamp) < 0)
    return (-1);

  ctx->errnum = IPMI_SEL_ERR_SUCCESS;
  return (0);
}

int
_sel_parse_system_event_common (ipmi_sel_ctx_t ctx,
				const void *sel_record,
				unsigned int sel_record_len,
				struct ipmi_sel_system_event_record_data *system_event_record_data)
{
  struct ipmi_sel_entry *sel_entry_ptr = NULL;
  struct ipmi_sel_entry sel_entry_buf;

  assert (system_event_record_data);
  
  if (!ctx || ctx->magic != IPMI_SEL_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_sel_ctx_errormsg (ctx), ipmi_sel_ctx_errnum (ctx));
      return (-1);
    }

  if (!sel_record && !sel_record_len)
    {
      if (_get_parse_sel_entry_common (ctx, &sel_entry_ptr) < 0)
	return (-1);
    }
  else
    {
      if (!sel_record
	  || !sel_record_len)
	{
	  SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_PARAMETERS);
	  return (-1);
	}

      memset (&sel_entry_buf, '\0', sizeof (struct ipmi_sel_entry));
      sel_entry_buf.sel_event_record_len = sel_record_len > IPMI_SEL_RECORD_LENGTH ? IPMI_SEL_RECORD_LENGTH : sel_record_len;
      memcpy (sel_entry_buf.sel_event_record,
	      sel_record,
	      sel_entry_buf.sel_event_record_len);
      sel_entry_ptr = &sel_entry_buf;
    }
  
  if (sel_get_system_event_record (ctx,
				   sel_entry_ptr,
				   system_event_record_data) < 0)
    return (-1);
  
  return (0);
}

int
ipmi_sel_parse_read_generator_id (ipmi_sel_ctx_t ctx,
				  const void *sel_record,
				  unsigned int sel_record_len,
				  uint8_t *generator_id)
{
  struct ipmi_sel_system_event_record_data system_event_record_data;

  if (_sel_parse_system_event_common (ctx,
				      sel_record,
				      sel_record_len,
				      &system_event_record_data) < 0)
    return (-1);

  if (generator_id)
    *generator_id = system_event_record_data.generator_id;

  ctx->errnum = IPMI_SEL_ERR_SUCCESS;
  return (0);
}

int
ipmi_sel_parse_read_ipmb_device_lun (ipmi_sel_ctx_t ctx,
				     const void *sel_record,
				     unsigned int sel_record_len,
				     uint8_t *ipmb_device_lun)
{
  struct ipmi_sel_system_event_record_data system_event_record_data;

  if (_sel_parse_system_event_common (ctx,
				      sel_record,
				      sel_record_len,
				      &system_event_record_data) < 0)
    return (-1);

  if (ipmb_device_lun)
    *ipmb_device_lun = system_event_record_data.ipmb_device_lun;

  ctx->errnum = IPMI_SEL_ERR_SUCCESS;
  return (0);
}

int
ipmi_sel_parse_read_channel_number (ipmi_sel_ctx_t ctx,
				    const void *sel_record,
				    unsigned int sel_record_len,
				    uint8_t *channel_number)
{
  struct ipmi_sel_system_event_record_data system_event_record_data;

  if (_sel_parse_system_event_common (ctx,
				      sel_record,
				      sel_record_len,
				      &system_event_record_data) < 0)
    return (-1);

  /* special case */
  if (system_event_record_data.event_message_format_version == IPMI_V1_0_EVENT_MESSAGE_FORMAT)
    {
      SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_INVALID_SEL_ENTRY);
      return (-1);
    }

  if (channel_number)
    *channel_number = system_event_record_data.channel_number;

  ctx->errnum = IPMI_SEL_ERR_SUCCESS;
  return (0);
}

int
ipmi_sel_parse_read_event_message_format_version (ipmi_sel_ctx_t ctx,
						  const void *sel_record,
						  unsigned int sel_record_len,
						  uint8_t *event_message_format_version)
{
  struct ipmi_sel_system_event_record_data system_event_record_data;

  if (_sel_parse_system_event_common (ctx,
				      sel_record,
				      sel_record_len,
				      &system_event_record_data) < 0)
    return (-1);

  if (event_message_format_version)
    *event_message_format_version = system_event_record_data.event_message_format_version;

  ctx->errnum = IPMI_SEL_ERR_SUCCESS;
  return (0);
}

int
ipmi_sel_parse_read_sensor_type (ipmi_sel_ctx_t ctx,
				 const void *sel_record,
				 unsigned int sel_record_len,
				 uint8_t *sensor_type)
{
  struct ipmi_sel_system_event_record_data system_event_record_data;

  if (_sel_parse_system_event_common (ctx,
				      sel_record,
				      sel_record_len,
				      &system_event_record_data) < 0)
    return (-1);

  if (sensor_type)
    *sensor_type = system_event_record_data.sensor_type;

  ctx->errnum = IPMI_SEL_ERR_SUCCESS;
  return (0);
}

int
ipmi_sel_parse_read_sensor_number (ipmi_sel_ctx_t ctx,
				   const void *sel_record,
				   unsigned int sel_record_len,
				   uint8_t *sensor_number)
{
  struct ipmi_sel_system_event_record_data system_event_record_data;

  if (_sel_parse_system_event_common (ctx,
				      sel_record,
				      sel_record_len,
				      &system_event_record_data) < 0)
    return (-1);

  if (sensor_number)
    *sensor_number = system_event_record_data.sensor_number;

  ctx->errnum = IPMI_SEL_ERR_SUCCESS;
  return (0);
}

int
ipmi_sel_parse_read_event_direction (ipmi_sel_ctx_t ctx,
				     const void *sel_record,
				     unsigned int sel_record_len,
				     uint8_t *event_direction)
{
  struct ipmi_sel_system_event_record_data system_event_record_data;

  if (_sel_parse_system_event_common (ctx,
				      sel_record,
				      sel_record_len,
				      &system_event_record_data) < 0)
    return (-1);

  if (event_direction)
    *event_direction = system_event_record_data.event_direction;

  ctx->errnum = IPMI_SEL_ERR_SUCCESS;
  return (0);
}

int
ipmi_sel_parse_read_event_type_code (ipmi_sel_ctx_t ctx,
				     const void *sel_record,
				     unsigned int sel_record_len,
				     uint8_t *event_type_code)
{
  struct ipmi_sel_system_event_record_data system_event_record_data;

  if (_sel_parse_system_event_common (ctx,
				      sel_record,
				      sel_record_len,
				      &system_event_record_data) < 0)
    return (-1);

  if (event_type_code)
    *event_type_code = system_event_record_data.event_type_code;

  ctx->errnum = IPMI_SEL_ERR_SUCCESS;
  return (0);
}

int
ipmi_sel_parse_read_event_data1 (ipmi_sel_ctx_t ctx,
				 const void *sel_record,
				 unsigned int sel_record_len,
				 uint8_t *event_data1)
{
  struct ipmi_sel_system_event_record_data system_event_record_data;

  if (_sel_parse_system_event_common (ctx,
				      sel_record,
				      sel_record_len,
				      &system_event_record_data) < 0)
    return (-1);

  /* note: not a mistake, event_data3 is << 4 and event_data2 is << 6, see spec */
  if (event_data1)
    *event_data1 = (system_event_record_data.offset_from_event_reading_type_code
		    | system_event_record_data.event_data3_flag << 4
		    | system_event_record_data.event_data2_flag << 6);
  
  ctx->errnum = IPMI_SEL_ERR_SUCCESS;
  return (0);
}

int
ipmi_sel_parse_read_event_data1_offset_from_event_reading_type_code (ipmi_sel_ctx_t ctx,
								     const void *sel_record,
								     unsigned int sel_record_len,
                                                                     uint8_t *event_data1_offset)
{
  struct ipmi_sel_system_event_record_data system_event_record_data;

  if (_sel_parse_system_event_common (ctx,
				      sel_record,
				      sel_record_len,
				      &system_event_record_data) < 0)
    return (-1);

  if (event_data1_offset)
    *event_data1_offset = system_event_record_data.offset_from_event_reading_type_code;

  ctx->errnum = IPMI_SEL_ERR_SUCCESS;
  return (0);
}

int
ipmi_sel_parse_read_event_data1_event_data2_flag (ipmi_sel_ctx_t ctx,
						  const void *sel_record,
						  unsigned int sel_record_len,
                                                  uint8_t *event_data2_flag)
{
  struct ipmi_sel_system_event_record_data system_event_record_data;

  if (_sel_parse_system_event_common (ctx,
				      sel_record,
				      sel_record_len,
				      &system_event_record_data) < 0)
    return (-1);

  if (event_data2_flag)
    *event_data2_flag = system_event_record_data.event_data2_flag;

  ctx->errnum = IPMI_SEL_ERR_SUCCESS;
  return (0);
}

int
ipmi_sel_parse_read_event_data1_event_data3_flag (ipmi_sel_ctx_t ctx,
						  const void *sel_record,
						  unsigned int sel_record_len,
                                                  uint8_t *event_data3_flag)
{
  struct ipmi_sel_system_event_record_data system_event_record_data;

  if (_sel_parse_system_event_common (ctx,
				      sel_record,
				      sel_record_len,
				      &system_event_record_data) < 0)
    return (-1);

  if (event_data3_flag)
    *event_data3_flag = system_event_record_data.event_data3_flag;

  ctx->errnum = IPMI_SEL_ERR_SUCCESS;
  return (0);
}

int
ipmi_sel_parse_read_event_data2 (ipmi_sel_ctx_t ctx,
				 const void *sel_record,
				 unsigned int sel_record_len,
				 uint8_t *event_data2)
{
  struct ipmi_sel_system_event_record_data system_event_record_data;

  if (_sel_parse_system_event_common (ctx,
				      sel_record,
				      sel_record_len,
				      &system_event_record_data) < 0)
    return (-1);

  if (event_data2)
    *event_data2 = system_event_record_data.event_data2;

  ctx->errnum = IPMI_SEL_ERR_SUCCESS;
  return (0);
}

int
ipmi_sel_parse_read_event_data3 (ipmi_sel_ctx_t ctx,
				 const void *sel_record,
				 unsigned int sel_record_len,
				 uint8_t *event_data3)
{
  struct ipmi_sel_system_event_record_data system_event_record_data;

  if (_sel_parse_system_event_common (ctx,
				      sel_record,
				      sel_record_len,
				      &system_event_record_data) < 0)
    return (-1);

  if (event_data3)
    *event_data3 = system_event_record_data.event_data3;

  ctx->errnum = IPMI_SEL_ERR_SUCCESS;
  return (0);
}

int
ipmi_sel_parse_read_manufacturer_id (ipmi_sel_ctx_t ctx,
				     const void *sel_record,
				     unsigned int sel_record_len,
				     uint32_t *manufacturer_id)
{
  struct ipmi_sel_entry *sel_entry_ptr = NULL;
  struct ipmi_sel_entry sel_entry;
  
  if (_sel_parse_read_common (ctx,
			      sel_record,
			      sel_record_len,
			      &sel_entry_ptr,
			      &sel_entry) < 0)
    return (-1);

  if (sel_get_manufacturer_id (ctx,
			       sel_entry_ptr,
			       manufacturer_id) < 0)
    return (-1);

  ctx->errnum = IPMI_SEL_ERR_SUCCESS;
  return (0);
}

int
ipmi_sel_parse_read_oem (ipmi_sel_ctx_t ctx,
			 const void *sel_record,
			 unsigned int sel_record_len,
			 void *buf,
			 unsigned int buflen)
{
  struct ipmi_sel_entry *sel_entry_ptr = NULL;
  struct ipmi_sel_entry sel_entry;
  int rv = 0;

  if (_sel_parse_read_common (ctx,
			      sel_record,
			      sel_record_len,
			      &sel_entry_ptr,
			      &sel_entry) < 0)
    return (-1);

  if (!buf || !buflen)
    {
      SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_PARAMETERS);
      return (-1);
    }

  if ((rv = sel_get_oem (ctx,
			 sel_entry_ptr,
			 buf,
			 buflen)) < 0)
    return (-1);

  ctx->errnum = IPMI_SEL_ERR_SUCCESS;
  return (rv);
}

int
ipmi_sel_parse_read_record_string (ipmi_sel_ctx_t ctx,
                                   const char *fmt,
				   const void *sel_record,
				   unsigned int sel_record_len,
                                   char *buf,
                                   unsigned int buflen,
                                   unsigned int flags)
{
  struct ipmi_sel_entry *sel_entry = NULL;
  void *sel_record_to_use;
  unsigned int sel_record_len_to_use;

  if (!ctx || ctx->magic != IPMI_SEL_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_sel_ctx_errormsg (ctx), ipmi_sel_ctx_errnum (ctx));
      return (-1);
    }

  if (!fmt
      || !buf
      || !buflen
      || (flags & ~IPMI_SEL_STRING_FLAGS_MASK))
    {
      SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_PARAMETERS);
      return (-1);
    }

  if (!sel_record && !sel_record_len)
    {
      if (_get_parse_sel_entry_common (ctx, &sel_entry) < 0)
	return (-1);

      sel_record_to_use = sel_entry->sel_event_record;
      sel_record_len_to_use = sel_entry->sel_event_record_len;
    }
  else
    {
      if (!sel_record
	  || !sel_record_len)
	{
	  SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_PARAMETERS);
	  return (-1);
	}

      sel_record_to_use = (void *)sel_record;
      sel_record_len_to_use = sel_record_len;
    }

  if (sel_record_len_to_use < IPMI_SEL_RECORD_LENGTH)
    {
      SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_INVALID_SEL_ENTRY);
      return (-1);
    }

  return (sel_format_record_string (ctx,
				    fmt,
				    sel_record_to_use,
				    sel_record_len_to_use,
				    buf,
				    buflen,
				    flags));
}

int
ipmi_sel_clear_sel (ipmi_sel_ctx_t ctx)
{
  unsigned int reservation_id_retry_count = 0;
  uint16_t reservation_id;
  fiid_obj_t obj_cmd_rs = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_SEL_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_sel_ctx_errormsg (ctx), ipmi_sel_ctx_errnum (ctx));
      return (-1);
    }

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_clear_sel_rs)))
    {
      SEL_ERRNO_TO_SEL_ERRNUM (ctx, errno);
      goto cleanup;
    }

  while (1)
    {
      if (ctx->reservation_id_registered)
	reservation_id = ctx->reservation_id;
      else
	{
	  if (sel_get_reservation_id (ctx, &reservation_id, NULL) < 0)
	    goto cleanup;
	}

      if (ipmi_cmd_clear_sel (ctx->ipmi_ctx,
                              reservation_id,
                              IPMI_SEL_CLEAR_OPERATION_INITIATE_ERASE,
                              obj_cmd_rs) < 0)
        {
          if (ipmi_ctx_errnum (ctx->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE
              && ipmi_check_completion_code (obj_cmd_rs,
                                             IPMI_COMP_CODE_RESERVATION_CANCELLED) == 1)
            {
	      if (ctx->reservation_id_registered)
		{
		  SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_RESERVATION_CANCELED);
		  goto cleanup;
		}
	      else
		{
		  reservation_id_retry_count++;
		  
		  if (reservation_id_retry_count > IPMI_SEL_RESERVATION_ID_RETRY)
		    {
		      SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_IPMI_ERROR);
		      goto cleanup;
		    }
		  
		  if (sel_get_reservation_id (ctx, &reservation_id, NULL) < 0)
		    goto cleanup;

		  continue;
		}
            }
          else
            {
              SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_IPMI_ERROR);
              goto cleanup;
            }
        }

      break;
    }

  rv = 0;
  ctx->errnum = IPMI_SEL_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

int
ipmi_sel_delete_sel_entry (ipmi_sel_ctx_t ctx, uint16_t record_id)
{
  unsigned int reservation_id_retry_count = 0;
  uint16_t reservation_id;
  fiid_obj_t obj_cmd_rs = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_SEL_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_sel_ctx_errormsg (ctx), ipmi_sel_ctx_errnum (ctx));
      return (-1);
    }

  if (!(record_id > IPMI_SEL_GET_RECORD_ID_FIRST_ENTRY
        && record_id < IPMI_SEL_GET_RECORD_ID_LAST_ENTRY))
    {
      SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_PARAMETERS);
      return (-1);
    }

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_delete_sel_entry_rs)))
    {
      SEL_ERRNO_TO_SEL_ERRNUM (ctx, errno);
      goto cleanup;
    }

  while (1)
    {
      if (ctx->reservation_id_registered)
	reservation_id = ctx->reservation_id;
      else
	{
	  if (sel_get_reservation_id (ctx, &reservation_id, NULL) < 0)
	    goto cleanup;
	}

      if (ipmi_cmd_delete_sel_entry (ctx->ipmi_ctx,
                                     reservation_id,
                                     record_id,
                                     obj_cmd_rs) < 0)
        {
          if ((ipmi_ctx_errnum (ctx->ipmi_ctx) == IPMI_ERR_COMMAND_INVALID_OR_UNSUPPORTED
	       && ipmi_check_completion_code (obj_cmd_rs, IPMI_COMP_CODE_INVALID_DATA_FIELD_IN_REQUEST) == 1)
	      || (ipmi_ctx_errnum (ctx->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE
		  && ipmi_check_completion_code (obj_cmd_rs, IPMI_COMP_CODE_REQUESTED_SENSOR_DATA_OR_RECORD_NOT_PRESENT) == 1))
            {
              SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_NOT_FOUND);
              goto cleanup;
            }
          else if (ipmi_ctx_errnum (ctx->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE
                   && ipmi_check_completion_code (obj_cmd_rs,
                                                  IPMI_COMP_CODE_RESERVATION_CANCELLED) == 1)
            {
	      if (ctx->reservation_id_registered)
		{
		  SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_RESERVATION_CANCELED);
		  goto cleanup;
		}
	      else
		{
		  reservation_id_retry_count++;
		  
		  if (reservation_id_retry_count > IPMI_SEL_RESERVATION_ID_RETRY)
		    {
		      SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_IPMI_ERROR);
		      goto cleanup;
		    }

		  if (sel_get_reservation_id (ctx, &reservation_id, NULL) < 0)
		    goto cleanup;

		  continue;
		}
            }
          else
            {
              SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_IPMI_ERROR);
              goto cleanup;
            }
        }

      break;
    }

  rv = 0;
  ctx->errnum = IPMI_SEL_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

int
ipmi_sel_record_type_class (uint8_t record_type)
{
  if (IPMI_SEL_RECORD_TYPE_IS_EVENT (record_type))
    return (IPMI_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD);

  if (IPMI_SEL_RECORD_TYPE_IS_TIMESTAMPED_OEM (record_type))
    return (IPMI_SEL_RECORD_TYPE_CLASS_TIMESTAMPED_OEM_RECORD);

  if (IPMI_SEL_RECORD_TYPE_IS_NON_TIMESTAMPED_OEM (record_type))
    return (IPMI_SEL_RECORD_TYPE_CLASS_NON_TIMESTAMPED_OEM_RECORD);

  return (IPMI_SEL_RECORD_TYPE_CLASS_UNKNOWN);
}
