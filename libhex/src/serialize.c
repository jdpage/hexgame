// Hex board serialization routines
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

#include <hex_private.h>


hex_err hex_board_dump(const hex_board *board, char *buf, size_t buf_len)
{
  if (buf_len < hex_board_dumpsize(board)) {
    return HEX_ESHORTBUFFER;
  }

  int len = board->size * board->size;
  for (int k = 0; k < len; k++) {
    switch (board->data[k]) {
    case HEX_COLOR_RED:
      buf[k] = 'r';
      break;
    case HEX_COLOR_BLUE:
      buf[k] = 'b';
      break;
    default:
      buf[k] = '.';
      break;
    }
  }

  buf[len] = '\0';
  return HEX_OK;
}


size_t hex_board_dumpsize(const hex_board *board)
{
  return sizeof(char) * (board->size * board->size + 1);
}


hex_err hex_board_scan(hex_board *board, const char *buf)
{
  int remaining = board->size * board->size;
  hex_color *ptr = board->data;
  while (remaining --> 0) {
    switch (*(buf++)) {
    case 'r':
    case 'R':
      *(ptr++) = HEX_COLOR_RED;
      break;
    case 'b':
    case 'B':
      *(ptr++) = HEX_COLOR_BLUE;
      break;
    case '\0':
      return HEX_ESIZEMISMATCH;
    default:
      *(ptr++) = HEX_COLOR_NONE;
      break;
    }
  }

  if (*buf != '\0') {
    return HEX_ESIZEMISMATCH;
  }

  return HEX_OK;
}
