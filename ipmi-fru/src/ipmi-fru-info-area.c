/*****************************************************************************\
 *  $Id: ipmi-fru-info-area.c,v 1.21.4.3 2009-04-17 00:15:26 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2009 Lawrence Livermore National Security, LLC.
 *  Copyright (C) 2007 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-232183
 *
 *  This file is part of Ipmi-fru, a tool used for retrieving
 *  motherboard field replaceable unit (FRU) information. For details,
 *  see http://www.llnl.gov/linux/.
 *
 *  Ipmi-fru is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 2 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmi-fru is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmi-fru.  If not, see <http://www.gnu.org/licenses/>.
\*****************************************************************************/

#if HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#if TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else  /* !TIME_WITH_SYS_TIME */
#if HAVE_SYS_TIME_H
#include <sys/time.h>
#else /* !HAVE_SYS_TIME_H */
#include <time.h>
#endif  /* !HAVE_SYS_TIME_H */
#endif /* !TIME_WITH_SYS_TIME */
#include <assert.h>

#include <freeipmi/freeipmi.h>

#include "ipmi-fru.h"
#include "ipmi-fru-info-area.h"
#include "ipmi-fru-util.h"

#include "freeipmi-portability.h"

/* haven't seen a motherboard with more than 2-3 so far, 64 should be more than enough */
#define IPMI_FRU_CUSTOM_FIELDS 64

#define IPMI_FRU_STR_BUFLEN    1024

int
ipmi_fru_output_chassis_info_area (ipmi_fru_state_data_t *state_data,
                                   uint8_t *areabuf,
                                   unsigned int area_length)
{
  uint8_t chassis_type;
  ipmi_fru_parse_field_t chassis_part_number;
  ipmi_fru_parse_field_t chassis_serial_number;
  ipmi_fru_parse_field_t chassis_custom_fields[IPMI_FRU_CUSTOM_FIELDS];
  int i;

  assert (state_data);
  assert (areabuf);
  assert (area_length);

  memset (&chassis_part_number, '\0', sizeof (ipmi_fru_parse_field_t));
  memset (&chassis_serial_number, '\0', sizeof (ipmi_fru_parse_field_t));
  memset (&chassis_custom_fields[0],
          '\0',
          sizeof (ipmi_fru_parse_field_t) * IPMI_FRU_CUSTOM_FIELDS);

  if (ipmi_fru_parse_chassis_info_area (state_data->fru_parse_ctx,
                                        areabuf,
                                        area_length,
                                        &chassis_type,
                                        &chassis_part_number,
                                        &chassis_serial_number,
                                        chassis_custom_fields,
                                        IPMI_FRU_CUSTOM_FIELDS) < 0)
    {
      if (IPMI_FRU_PARSE_ERRNUM_IS_NON_FATAL_ERROR (state_data->fru_parse_ctx))
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "  FRU Chassis Error: %s\n",
                           ipmi_fru_parse_ctx_errormsg (state_data->fru_parse_ctx));
          return (0);
        }
      
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_fru_parse_chassis_info_area: %s\n",
                       ipmi_fru_parse_ctx_errormsg (state_data->fru_parse_ctx));
      return (-1);
    }

  if (IPMI_FRU_CHASSIS_TYPE_VALID (chassis_type))
    pstdout_printf (state_data->pstate,
                    "  FRU Chassis Type: %s\n",
                    ipmi_fru_chassis_types[chassis_type]);
  else
    pstdout_printf (state_data->pstate,
                    "  FRU Chassis Type: %s\n",
                    ipmi_fru_chassis_types[IPMI_FRU_CHASSIS_TYPE_UNKNOWN]);

  /* achu: Chassis Info Area has no language code, assume English. */

  if (ipmi_fru_output_field (state_data,
                             IPMI_FRU_LANGUAGE_CODE_ENGLISH,
                             &chassis_part_number,
                             "Chassis Part Number") < 0)
    return (-1);

  if (ipmi_fru_output_field (state_data,
                             IPMI_FRU_LANGUAGE_CODE_ENGLISH,
                             &chassis_serial_number,
                             "Chassis Serial Number") < 0)
    return (-1);

  for (i = 0; i < IPMI_FRU_CUSTOM_FIELDS; i++)
    {
      if (ipmi_fru_output_field (state_data,
                                 IPMI_FRU_LANGUAGE_CODE_ENGLISH,
                                 &chassis_custom_fields[i],
                                 "Chassis Custom Info") < 0)
        return (-1);
    }

  return (0);
}

int
ipmi_fru_output_board_info_area (ipmi_fru_state_data_t *state_data,
                                 uint8_t *areabuf,
                                 unsigned int area_length)
{
  uint8_t language_code;
  uint32_t mfg_date_time;
  ipmi_fru_parse_field_t board_manufacturer;
  ipmi_fru_parse_field_t board_product_name;
  ipmi_fru_parse_field_t board_serial_number;
  ipmi_fru_parse_field_t board_part_number;
  ipmi_fru_parse_field_t board_fru_file_id;
  ipmi_fru_parse_field_t board_custom_fields[IPMI_FRU_CUSTOM_FIELDS];
  time_t timetmp;
  struct tm mfg_date_time_tm;
  char mfg_date_time_buf[IPMI_FRU_STR_BUFLEN + 1];
  int i;

  assert (state_data);
  assert (areabuf);
  assert (area_length);

  memset (&board_manufacturer, '\0', sizeof (ipmi_fru_parse_field_t));
  memset (&board_product_name, '\0', sizeof (ipmi_fru_parse_field_t));
  memset (&board_serial_number, '\0', sizeof (ipmi_fru_parse_field_t));
  memset (&board_fru_file_id, '\0', sizeof (ipmi_fru_parse_field_t));
  memset (&board_custom_fields[0],
          '\0',
          sizeof (ipmi_fru_parse_field_t) * IPMI_FRU_CUSTOM_FIELDS);

  if (ipmi_fru_parse_board_info_area (state_data->fru_parse_ctx,
                                      areabuf,
                                      area_length,
                                      &language_code,
                                      &mfg_date_time,
                                      &board_manufacturer,
                                      &board_product_name,
                                      &board_serial_number,
                                      &board_part_number,
                                      &board_fru_file_id,
                                      board_custom_fields,
                                      IPMI_FRU_CUSTOM_FIELDS) < 0)
    {
      if (IPMI_FRU_PARSE_ERRNUM_IS_NON_FATAL_ERROR (state_data->fru_parse_ctx))
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "  FRU Board Error: %s\n",
                           ipmi_fru_parse_ctx_errormsg (state_data->fru_parse_ctx));
          return (0);
        }
      
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_fru_parse_board_info_area: %s\n",
                       ipmi_fru_parse_ctx_errormsg (state_data->fru_parse_ctx));
      return (-1);
    }

  if (state_data->prog_data->args->verbose_count)
    {
      if (IPMI_FRU_LANGUAGE_CODE_VALID (language_code))
        pstdout_printf (state_data->pstate,
                        "  FRU Board Language: %s\n",
                        ipmi_fru_language_codes[language_code]);
      else
        pstdout_printf (state_data->pstate,
                        "  FRU Board Language Code: %02Xh\n",
                        language_code);
    }

  timetmp = mfg_date_time;
  localtime_r (&timetmp, &mfg_date_time_tm);
  memset (mfg_date_time_buf, '\0', IPMI_FRU_STR_BUFLEN + 1);
  strftime (mfg_date_time_buf, IPMI_FRU_STR_BUFLEN, "%D - %T", &mfg_date_time_tm);

  pstdout_printf (state_data->pstate,
                  "  FRU Board Manufacturing Date/Time: %s\n",
                  mfg_date_time_buf);

  if (ipmi_fru_output_field (state_data,
                             language_code,
                             &board_manufacturer,
                             "Board Manufacturer") < 0)
    return (-1);

  if (ipmi_fru_output_field (state_data,
                             language_code,
                             &board_product_name,
                             "Board Product Name") < 0)
    return (-1);


  if (ipmi_fru_output_field (state_data,
                             language_code,
                             &board_serial_number,
                             "Board Serial Number") < 0)
    return (-1);

  if (ipmi_fru_output_field (state_data,
                             language_code,
                             &board_part_number,
                             "Board Part Number") < 0)
    return (-1);

  if (ipmi_fru_output_field (state_data,
                             language_code,
                             &board_fru_file_id,
                             "FRU File ID") < 0)
    return (-1);

  for (i = 0; i < IPMI_FRU_CUSTOM_FIELDS; i++)
    {
      if (ipmi_fru_output_field (state_data,
                                 language_code,
                                 &board_custom_fields[i],
                                 "Board Custom Info") < 0)
        return (-1);
    }

  return (0);
}

int
ipmi_fru_output_product_info_area (ipmi_fru_state_data_t *state_data,
                                   uint8_t *areabuf,
                                   unsigned int area_length)
{
  uint8_t language_code;
  ipmi_fru_parse_field_t product_manufacturer_name;
  ipmi_fru_parse_field_t product_name;
  ipmi_fru_parse_field_t product_part_model_number;
  ipmi_fru_parse_field_t product_version;
  ipmi_fru_parse_field_t product_serial_number;
  ipmi_fru_parse_field_t product_asset_tag;
  ipmi_fru_parse_field_t product_fru_file_id;
  ipmi_fru_parse_field_t product_custom_fields[IPMI_FRU_CUSTOM_FIELDS];
  int i;

  assert (state_data);
  assert (areabuf);
  assert (area_length);
  
  memset (&product_manufacturer_name, '\0', sizeof (ipmi_fru_parse_field_t));
  memset (&product_name, '\0', sizeof (ipmi_fru_parse_field_t));
  memset (&product_part_model_number, '\0', sizeof (ipmi_fru_parse_field_t));
  memset (&product_version, '\0', sizeof (ipmi_fru_parse_field_t));
  memset (&product_serial_number, '\0', sizeof (ipmi_fru_parse_field_t));
  memset (&product_asset_tag, '\0', sizeof (ipmi_fru_parse_field_t));
  memset (&product_fru_file_id, '\0', sizeof (ipmi_fru_parse_field_t));
  memset (&product_custom_fields[0],
          '\0',
          sizeof (ipmi_fru_parse_field_t) * IPMI_FRU_CUSTOM_FIELDS);
  
  if (ipmi_fru_parse_product_info_area (state_data->fru_parse_ctx,
                                        areabuf,
                                        area_length,
                                        &language_code,
                                        &product_manufacturer_name,
                                        &product_name,
                                        &product_part_model_number,
                                        &product_version,
                                        &product_serial_number,
                                        &product_asset_tag,
                                        &product_fru_file_id,
                                        product_custom_fields,
                                        IPMI_FRU_CUSTOM_FIELDS) < 0)
    {
      if (IPMI_FRU_PARSE_ERRNUM_IS_NON_FATAL_ERROR (state_data->fru_parse_ctx))
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "  FRU Product Error: %s\n",
                           ipmi_fru_parse_ctx_errormsg (state_data->fru_parse_ctx));
          return (0);
        }
      
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_fru_parse_product_info_area: %s\n",
                       ipmi_fru_parse_ctx_errormsg (state_data->fru_parse_ctx));
      return (-1);
    }

  if (state_data->prog_data->args->verbose_count)
    {
      if (IPMI_FRU_LANGUAGE_CODE_VALID (language_code))
        pstdout_printf (state_data->pstate,
                        "  FRU Product Language: %s\n",
                        ipmi_fru_language_codes[language_code]);
      else
        pstdout_printf (state_data->pstate,
                        "  FRU Product Language Code: %02Xh\n",
                        language_code);
    }

  if (ipmi_fru_output_field (state_data,
                             language_code,
                             &product_manufacturer_name,
                             "Product Manufacturer Name") < 0)
    return (-1);

  if (ipmi_fru_output_field (state_data,
                             language_code,
                             &product_name,
                             "Product Name") < 0)
    return (-1);


  if (ipmi_fru_output_field (state_data,
                             language_code,
                             &product_part_model_number,
                             "Product Part/Model Number") < 0)
    return (-1);

  if (ipmi_fru_output_field (state_data,
                             language_code,
                             &product_version,
                             "Product Version") < 0)
    return (-1);

  if (ipmi_fru_output_field (state_data,
                             language_code,
                             &product_serial_number,
                             "Product Serial Number") < 0)
    return (-1);

  if (ipmi_fru_output_field (state_data,
                             language_code,
                             &product_asset_tag,
                             "Product Asset Tag") < 0)
    return (-1);

  if (ipmi_fru_output_field (state_data,
                             language_code,
                             &product_fru_file_id,
                             "FRU File ID") < 0)
    return (-1);

  for (i = 0; i < IPMI_FRU_CUSTOM_FIELDS; i++)
    {
      if (ipmi_fru_output_field (state_data,
                                 language_code,
                                 &product_custom_fields[i],
                                 "Product Custom Info") < 0)
        return (-1);
    }

  return (0);
}

