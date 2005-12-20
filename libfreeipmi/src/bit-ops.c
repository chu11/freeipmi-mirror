/* 
   bit-ops.c - bit level manipulators

   Copyright (C) 2003, 2004, 2005 FreeIPMI Core Team

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  

*/

#include "freeipmi.h"

/* Return the integer composed of the START (inclusive) through END
   (exclusive) bits of N.  The STARTth bit becomes the 0-th bit in the result.
   
   (number->string (bit-extract #b1101101010 0 4) 2)
             => "1010"
   (number->string (bit-extract #b1101101010 4 9) 2)
             => "10110"
*/
uint64_t
bits_extract (uint64_t bits, uint8_t start, uint8_t end)
{
  bits >>= start;
  bits <<= ((63 - (end - 1)) + start);
  bits >>= ((63 - (end - 1)) + start);
  return (bits);
}

/* Merges the val composed of the START (inclusive) through END
   (exclusive) bits of N.  The STARTth bit becomes the 0-th bit in the result.
*/
uint64_t
bits_merge (uint64_t bits, uint8_t start, uint8_t end, uint64_t val)
{
  uint64_t lsb_ones = 0xFFFFFFFFFFFFFFULL;
  uint64_t msb_ones = 0xFFFFFFFFFFFFFFULL;
  
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
  return (bits & msb_ones);
}

