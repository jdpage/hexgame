// Hex helper library private declarations
// Copyright (C) 2018  Jonathan David Page <jonathan@sleepingcyb.org>
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

#ifndef HEX_PRIVATE_H
#define HEX_PRIVATE_H

#include <hex/hex.h>

struct hex_board_s {
  int size;
  hex_color data[0];
};

#define __HEX_GENSYM2(x,y) x##y
#define __HEX_GENSYM1(x,y) __HEX_GENSYM2(x,y)
#define HEX_GENSYM(x) __HEX_GENSYM1(x,__COUNTER__)

#define __HEX_TRY(expr, e) do {                 \
    hex_err e = (expr);                         \
    if (e != HEX_OK) { return e; }              \
  } while (0)
#define HEX_TRY(expr) __HEX_TRY(expr, HEX_GENSYM(__hex_try_e))

#define HEX_TRYC(e, expr, cleanup) do {         \
    (e) = (expr);                               \
    if ((e) != HEX_OK) { goto cleanup; }        \
  } while (0)

#endif
