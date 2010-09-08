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

#ifndef _IPMI_PRODUCT_ID_SPEC_H
#define _IPMI_PRODUCT_ID_SPEC_H

#ifdef __cplusplus
extern "C" {
#endif

/******************************************* 
 * Dell                                    *
 *******************************************/

/* achu: I believe 256 to be the poweredge "family" of products, but
 * I'm not sure.  At minimum, it covers the 2900, 2950, R610, and
 * R710.
 */
#define __IPMI_DELL_PRODUCT_ID_POWEREDGE    256
#define IPMI_DELL_PRODUCT_ID_POWEREDGE_2900 __IPMI_DELL_PRODUCT_ID_POWEREDGE
#define IPMI_DELL_PRODUCT_ID_POWEREDGE_2950 __IPMI_DELL_PRODUCT_ID_POWEREDGE
#define IPMI_DELL_PRODUCT_ID_POWEREDGE_R610 __IPMI_DELL_PRODUCT_ID_POWEREDGE
#define IPMI_DELL_PRODUCT_ID_POWEREDGE_R710 __IPMI_DELL_PRODUCT_ID_POWEREDGE

/*******************************************
 * Intel                                   *
 *******************************************/

#define IPMI_INTEL_PRODUCT_ID_SR870BN4 256
#define IPMI_INTEL_PRODUCT_ID_TIGER4   IPMI_INTEL_PRODUCT_ID_SR870BN4

#define IPMI_INTEL_PRODUCT_ID_S5500WB  62

/* 
 * Intel derived
 */

#define IPMI_CALIFORNIA_DIGITAL_PRODUCT_ID_6440 IPMI_INTEL_PRODUCT_ID_SR870BN4

#define IPMI_PENGUIN_COMPUTING_PRODUCT_ID_RELION_700 IPMI_INTEL_PRODUCT_ID_S5500WB

/*******************************************
 * IBM                                     *
 *******************************************/

#define IPMI_IBM_PRODUCT_ID_X3455 20566
#define IPMI_IBM_PRODUCT_ID_X3755 14

/******************************************* 
 * Inventec                                *
 *******************************************/

#define IPMI_INVENTEC_PRODUCT_ID_5441 51
#define IPMI_INVENTEC_PRODUCT_ID_5442 52

/* 
 * Inventec derived
 */

#define IPMI_DELL_PRODUCT_ID_XANADU_II  IPMI_INVENTEC_PRODUCT_ID_5441
#define IPMI_DELL_PRODUCT_ID_XANADU_III IPMI_INVENTEC_PRODUCT_ID_5442

/******************************************* 
 * Quanta                                *
 *******************************************/
 
#define IPMI_QUANTA_PRODUCT_ID_S99Q 21401

/* 
 * Quanta derived
 */

#define IPMI_DELL_PRODUCT_ID_FS12_TY IPMI_QUANTA_PRODUCT_ID_S99Q

/******************************************* 
 * Sun Microsystems                        *
 *******************************************/

#define IPMI_SUN_MICROSYSTEMS_PRODUCT_ID_X4140 18177

/******************************************* 
 * Supermicro                              *
 *******************************************/

/* achu: I believe 43707 to be the x8dt "family" of products */

#define __IPMI_SUPERMICRO_PRODUCT_ID_X8DT  43707
#define IPMI_SUPERMICRO_PRODUCT_ID_X8DTH   __IPMI_SUPERMICRO_PRODUCT_ID_X8DT
#define IPMI_SUPERMICRO_PRODUCT_ID_X8DTG   __IPMI_SUPERMICRO_PRODUCT_ID_X8DT
#define IPMI_SUPERMICRO_PRODUCT_ID_X8DTU   __IPMI_SUPERMICRO_PRODUCT_ID_X8DT

#ifdef __cplusplus
}
#endif

#endif
