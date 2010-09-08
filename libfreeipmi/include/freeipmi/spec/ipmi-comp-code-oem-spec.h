/*
 * Copyright (C) 2003-2010 FreeIPMI Core Team
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

#ifndef _IPMI_COMP_CODE_OEM_SPEC_H
#define _IPMI_COMP_CODE_OEM_SPEC_H

#ifdef __cplusplus
extern "C" {
#endif

/******************************************* 
 * Fujitsu                                 *
 *******************************************/

/*
 * Fujitsu RX100 S5
 *
 * http://manuals.ts.fujitsu.com/file/4390/irmc_s2-en.pdf
 */
/* IPMI_CMD_OEM_FUJITSU_BIOS - w/ GET_CPU_INFO Command Specifier */
#define IPMI_COMP_CODE_OEM_FUJITSU_BIOS_UNPOPULATED_CPU_SOCKET 0x01
#define IPMI_COMP_CODE_OEM_FUJITSU_BIOS_UNPOPULATED_CPU_SOCKET_STR \
  "Unpopulated CPU Socket"

/*******************************************
 * Intel                                   *
 *******************************************/

/*
 * Intel S5500WB/Penguin Computing Relion 700
 */

/* IPMI_CMD_OEM_INTEL_SET_SMTP_CONFIGURATION */

#define IPMI_COMP_CODE_SET_SMTP_CONFIGURATION_PARAMETER_NOT_SUPPORTED                    0x80
#define IPMI_COMP_CODE_SET_SMTP_CONFIGURATION_PARAMETER_NOT_SUPPORTED_STR \
  "parameter not supported."

#define IPMI_COMP_CODE_SET_SMTP_CONFIGURATION_WRITE_READ_ONLY_PARAMETER                  0x82
#define IPMI_COMP_CODE_SET_SMTP_CONFIGURATION_WRITE_READ_ONLY_PARAMETER_STR \
  "attempt to write read-only parameter"

/* IPMI_CMD_OEM_INTEL_GET_SMTP_CONFIGURATION */

/* achu: document from Intel also sites a "write read-only parameter"
 * error code, but I assume that is a cut and paste typo.  Shouldn't
 * be possible for the "get" command
 */

#define IPMI_COMP_CODE_GET_SMTP_CONFIGURATION_PARAMETER_NOT_SUPPORTED                    0x80
#define IPMI_COMP_CODE_GET_SMTP_CONFIGURATION_PARAMETER_NOT_SUPPORTED_STR \
  "parameter not supported."

#ifdef __cplusplus
}
#endif

#endif /* _IPMI_COMP_CODE_OEM_SPEC_H */

