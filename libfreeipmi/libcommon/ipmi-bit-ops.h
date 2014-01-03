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

#ifndef IPMI_BIT_OPS_H
#define IPMI_BIT_OPS_H

#define BIT_0 0x01
#define BIT_1 0x02
#define BIT_2 0x04
#define BIT_3 0x08
#define BIT_4 0x10
#define BIT_5 0x20
#define BIT_6 0x40
#define BIT_7 0x80
#define BIT(n) ((uint64_t) powl (2, n))

#define TOBOOL(arg) (!(!(arg)))
#define BIT_SET(arg, posn) (arg | (1L << posn))
#define BIT_CLR(arg, posn) (arg & ~(1L << posn))
#define BIT_FLP(arg, posn) (arg ^ (1L << posn))
#define BIT_TST(arg, posn) TOBOOL ((arg) & (1L << posn))

#define BITS_ZERO(arg) (arg ^ arg)
#define BITS_0(arg)    (bits_extract (arg,  0, 8))
#define BITS_1(arg)    (bits_extract (arg,  8, 16))
#define BITS_2(arg)    (bits_extract (arg, 16, 24))
#define BITS_3(arg)    (bits_extract (arg, 24, 32))
#define BITS_4(arg)    (bits_extract (arg, 32, 40))
#define BITS_5(arg)    (bits_extract (arg, 40, 48))
#define BITS_6(arg)    (bits_extract (arg, 48, 56))
#define BITS_7(arg)    (bits_extract (arg, 56, 64))
#define BITS_SET(arg, bits) (arg | (bits))
#define BITS_CLR(arg, bits) (arg & ~(bits))
#define BITS_ROUND_BYTES(bits_count) ((bits_count / 8) + ((bits_count % 8) ? 1 : 0))

typedef uint8_t bitstr_t;

/* internal macros */
/* byte of the bitstring bit is in */
#define BITSTR_BYTE(bit)                        \
  ((bit) >> 3)

/* mask for the bit within its byte */
#define BITSTR_MASK(bit)                        \
  (1 << ((bit)&0x7))

/* external macros */
/* bytes in a bitstring of nbits bits */
#define BITSTR_SIZE(nbits)                      \
  ((((nbits) - 1) >> 3) + 1)

/* allocate a bitstring */
#define BITSTR_ALLOC(nbits)                                             \
  (bitstr_t *)calloc (1,                                                \
                      (size_t)bitstr_size (nbits) * sizeof (bitstr_t))

/* allocate a bitstring on the stack */
#define BITSTR_DECL(name, nbits)                        \
  (name)[bitstr_size (nbits)]

/* is bit N of bitstring name set? */
#define BITSTR_TEST(name, bit)                          \
  ((name)[BITSTR_BYTE (bit)] & BITSTR_MASK (bit))

/* set bit N of bitstring name */
#define BITSTR_SET(name, bit)                           \
  (name)[BITSTR_BYTE (bit)] |= BITSTR_MASK (bit)

/* clear bit N of bitstring name */
#define BITSTR_CLEAR(name, bit)                         \
  (name)[BITSTR_BYTE (bit)] &= ~BITSTR_MASK(bit)

/* clear bits start ... stop in bitstring */
#define BITSTR_NCLEAR(name, start, stop) {                      \
    register bitstr_t *_name = name;                            \
    register int _start = start, _stop = stop;                  \
    register int _startbyte = BITSTR_BYTE (_start);             \
    register int _stopbyte = BITSTR_BYTE (_stop);               \
    if (_startbyte == _stopbyte) {                              \
      _name[_startbyte] &= ((0xff >> (8 - (_start&0x7))) |      \
                            (0xff << ((_stop&0x7) + 1)));       \
    } else {                                                    \
      _name[_startbyte] &= 0xff >> (8 - (_start&0x7));          \
      while (++_startbyte < _stopbyte)                          \
        _name[_startbyte] = 0;                                  \
      _name[_stopbyte] &= 0xff << ((_stop&0x7) + 1);            \
    }                                                           \
  }

/* set bits start ... stop in bitstring */
#define BITSTR_NSET(name, start, stop) {                        \
    register bitstr_t *_name = name;                            \
    register int _start = start, _stop = stop;                  \
    register int _startbyte = BITSTR_BYTE (_start);             \
    register int _stopbyte = BITSTR_BYTE (_stop);               \
    if (_startbyte == _stopbyte) {                              \
      _name[_startbyte] |= ((0xff << (_start&0x7)) &            \
                            (0xff >> (7 - (_stop&0x7))));       \
    } else {                                                    \
      _name[_startbyte] |= 0xff << ((_start)&0x7);              \
      while (++_startbyte < _stopbyte)                          \
        _name[_startbyte] = 0xff;                               \
      _name[_stopbyte] |= 0xff >> (7 - (_stop&0x7));            \
    }                                                           \
  }

/* find first bit clear in name */
#define BITSTR_FFC(name, nbits, value) {                                \
    register bitstr_t *_name = name;                                    \
    register int _byte, _nbits = nbits;                                 \
    register int _stopbyte = BITSTR_BYTE (_nbits), _value = -1;         \
    for (_byte = 0; _byte <= _stopbyte; ++_byte)                        \
      if (_name[_byte] != 0xff) {                                       \
        _value = _byte << 3;                                            \
        for (_stopbyte = _name[_byte]; (_stopbyte&0x1);                 \
             ++_value, _stopbyte >>= 1) ;                               \
        break;                                                          \
      }                                                                 \
    *(value) = _value;                                                  \
  }

/* find first bit set in name */
#define BITSTR_FFS(name, nbits, value) {                                \
    register bitstr_t *_name = name;                                    \
    register int _byte, _nbits = nbits;                                 \
    register int _stopbyte = BITSTR_BYTE (_nbits), _value = -1;         \
    for (_byte = 0; _byte <= _stopbyte; ++_byte)                        \
      if (_name[_byte]) {                                               \
        _value = _byte << 3;                                            \
        for (_stopbyte = _name[_byte]; !(_stopbyte&0x1);                \
             ++_value, _stopbyte >>= 1) ;                               \
        break;                                                          \
      }                                                                 \
    *(value) = _value;                                                  \
  }

int bits_extract (uint64_t bits, uint8_t start, uint8_t end, uint64_t *result);
int bits_merge (uint64_t bits, uint8_t start, uint8_t end, uint64_t val, uint64_t *result);

#endif /* IPMI_BIT_OPS_H */
