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
#include <config.h>
#endif /* HAVE_CONFIG_H */

#include <sys/types.h>

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <errno.h>

#include "ipmi-bit-ops.h"

#include "freeipmi-portability.h"

/* Return the integer composed of the START (inclusive) through END
   (exclusive) bits of N.  The STARTth bit becomes the 0-th bit in the result.

   (number->string (bit-extract #b1101101010 0 4) 2)
   => "1010"
   (number->string (bit-extract #b1101101010 4 9) 2)
   => "10110"
*/
int
bits_extract (uint64_t bits, uint8_t start, uint8_t end, uint64_t *result)
{
  if (start > end || start > 64 || end > 64 || !result)
    {
      errno = EINVAL;
      return (-1);
    }

  bits >>= start;
  bits <<= ((63 - (end - 1)) + start);
  bits >>= ((63 - (end - 1)) + start);
  *result = bits;
  return (0);
}

/* Merges the val composed of the START (inclusive) through END
   (exclusive) bits of N.  The STARTth bit becomes the 0-th bit in the result.
*/
int
bits_merge (uint64_t bits, uint8_t start, uint8_t end, uint64_t val, uint64_t *result)
{
  uint64_t lsb_ones = 0xFFFFFFFFFFFFFFFFULL;
  uint64_t msb_ones = 0xFFFFFFFFFFFFFFFFULL;

  if (start > end || start > 64 || end > 64 || !result)
    {
      errno = EINVAL;
      return (-1);
    }

  if (start)
    {
      lsb_ones <<= (64 - start);
      lsb_ones >>= (64 - start);
    }
  else
    lsb_ones = 0x0;

  msb_ones >>= (end - start);
  msb_ones <<= (end - start);

  msb_ones <<= start;

  msb_ones |= lsb_ones;
  bits     |= ~msb_ones;
  msb_ones |= (val << start);
  *result = (bits & msb_ones);
  return (0);
}

