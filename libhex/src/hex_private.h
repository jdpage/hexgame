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
