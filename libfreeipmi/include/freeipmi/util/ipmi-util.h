/*
 * Copyright (C) 2003-2015 FreeIPMI Core Team
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

#ifndef IPMI_UTIL_H
#define IPMI_UTIL_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <freeipmi/fiid/fiid.h>

uint8_t ipmi_checksum (const void *buf, unsigned int buflen);

/* Call first time w/ checksum_initial 0, pass in result for subsequent calls */
uint8_t ipmi_checksum_incremental (const void *buf, unsigned int buflen, uint8_t checksum_initial);
/* Can pass NULL/0 for final buf/buflen */
uint8_t ipmi_checksum_final (const void *buf, unsigned int buflen, uint8_t checksum_initial);

/* returns 1 on pass, 0 on fail, -1 on error */
int ipmi_check_cmd (fiid_obj_t obj_cmd, uint8_t cmd);

/* returns 1 on pass, 0 on fail, -1 on error */
int ipmi_check_completion_code (fiid_obj_t obj_cmd, uint8_t completion_code);

/* returns 1 on pass, 0 on fail, -1 on error */
int ipmi_check_completion_code_success (fiid_obj_t obj_cmd);

/* returns number of bytes stored in buf on success (0 if buflen is
 * 0), -1 on error.  Fails with EPERM if the library was built without
 * /dev/urandom or /dev/random support.
 */
int ipmi_get_random (void *buf, unsigned int buflen);

const char *ipmi_cmd_str (uint8_t net_fn, uint8_t cmd);

#ifdef __cplusplus
}
#endif

#endif /* IPMI_UTIL_H */
