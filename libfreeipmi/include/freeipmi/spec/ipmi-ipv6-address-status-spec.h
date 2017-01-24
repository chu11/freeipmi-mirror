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

#ifndef IPMI_IPV6_ADDRESS_STATUS_SPEC_H
#define IPMI_IPV6_ADDRESS_STATUS_SPEC_H

#ifdef __cplusplus
extern "C" {
#endif

#define IPMI_IPV6_ADDRESS_SOURCE_STATIC      0x00
#define IPMI_IPV6_ADDRESS_SOURCE_SLAAC       0x01
#define IPMI_IPV6_ADDRESS_SOURCE_DHCPV6      0x02

#define IPMI_IPV6_STATIC_ADDRESS_SOURCE_VALID(__address_status) \
  (((__address_status) == IPMI_IPV6_ADDRESS_SOURCE_STATIC) ? 1 : 0)

#define IPMI_IPV6_DYNAMIC_ADDRESS_SOURCE_VALID(__address_status) \
  (((__address_status) == IPMI_IPV6_ADDRESS_SOURCE_SLAAC         \
  || (__address_status) == IPMI_IPV6_ADDRESS_SOURCE_DHCPV6) ? 1 : 0)


#define IPMI_IPV6_ADDRESS_STATUS_ACTIVE      0x00
#define IPMI_IPV6_ADDRESS_STATUS_DISABLED    0x01
#define IPMI_IPV6_ADDRESS_STATUS_PENDING     0x02
#define IPMI_IPV6_ADDRESS_STATUS_FAILED      0x03
#define IPMI_IPV6_ADDRESS_STATUS_DEPRECATED  0x04
#define IPMI_IPV6_ADDRESS_STATUS_INVALID     0x05

#define IPMI_IPV6_ADDRESS_STATUS_VALID(__address_status)           \
  (((__address_status) == IPMI_IPV6_ADDRESS_STATUS_ACTIVE          \
    || (__address_status) == IPMI_IPV6_ADDRESS_STATUS_DISABLED     \
    || (__address_status) == IPMI_IPV6_ADDRESS_STATUS_PENDING      \
    || (__address_status) == IPMI_IPV6_ADDRESS_STATUS_FAILED       \
    || (__address_status) == IPMI_IPV6_ADDRESS_STATUS_DEPRECATED   \
    || (__address_status) == IPMI_IPV6_ADDRESS_STATUS_INVALID) ? 1 : 0)

#ifdef __cplusplus
}
#endif

#endif /* IPMI_IPV6_ADDRESS_STATUS_SPEC_H */
