// Hex helper library interface
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

#ifndef HEX_HEX_H
#define HEX_HEX_H

#include <stdlib.h>

enum hex_err_e {
  HEX_OK,
  HEX_ENOMEM,
  HEX_EBOUNDS,
  HEX_EBADSPACE,
  HEX_ESIZEMISMATCH,
  HEX_ESHORTBUFFER,
  HEX_ERRS_MAX
};

enum hex_color_e {
  HEX_COLOR_NONE,
  HEX_COLOR_RED,
  HEX_COLOR_BLUE,
  HEX_COLORS_MAX
} __attribute__ ((__packed__));

typedef enum hex_err_e hex_err;
typedef enum hex_color_e hex_color;
typedef struct hex_board_s hex_board;

typedef void *hex_alloc(size_t nmemb, size_t size);

// Creates a new hex board of the given size. In the case of hex_board_init, the
// pointer returned can be freed as usual for memory allocated with the given
// allocator function. Returns HEX_ENOMEM if allocation fails, and HEX_EBOUNDS
// if the size is invalid. The allocator is assumed to return pre-zeroed memory
// (as calloc(3)). The hex_board_initat variant does not assume zeroed memory,
// while hex_board_initatz does.
hex_err hex_board_init(hex_board **board, int size, hex_alloc *alloc);
hex_err hex_board_initat(hex_board *board, int size);
hex_err hex_board_initatz(hex_board *board, int size);

// Gets the number of bytes necessary to store a board of the given size. Can be
// used to allocate memory for use with hex_board_initat.
size_t hex_board_sizeof(int size);

// Gets the size of the board.
int hex_board_size(const hex_board *board);

// Gets a pointer to the board data, in column-major order, i.e. columns are
// contiguous.
hex_color *hex_board_data(hex_board *board, size_t *data_len);
const hex_color *hex_board_rodata(const hex_board *board, size_t *data_len);

// Dumps the string representation of the board into the given buffer as a
// null-terminated string. If the buffer is not long enough to hold the entire
// board and a null byte, HEX_ESHORTBUFFER is returned. hex_board_dumpsize
// returns the number of bytes required to hold a board dump, including the null
// terminator. The string representation consists of each space in column-major
// order where the characters 'r', 'b', and '.' denote red, blue, and empty
// spaces respectively.
hex_err hex_board_dump(const hex_board *board, char *buf, size_t buf_len);
size_t hex_board_dumpsize(const hex_board *board);

// Loads the string representation into the board. If the string representation
// is the wrong size, HEX_ESIZEMISMATCH is returned, and the board data may have
// been partially overwritten. The accepted string representation is as above,
// and is handled case-insensitively, and any unrecognized character is assumed
// to be an empty space.
hex_err hex_board_scan(hex_board *board, const char *buf);

// Gets a pointer to a space on the board. Returns HEX_EBOUNDS if row, col are
// out-of-bounds.
hex_err hex_board_space(
  hex_board *board,
  int row, int col,
  hex_color **space);
hex_err hex_board_rospace(
  const hex_board *board,
  int row, int col,
  const hex_color **space);

// Gets the coordinates of the given space. Returns HEX_EBADSPACE if the given
// space is not part of the given board.
hex_err hex_board_coords(
  const hex_board *board,
  const hex_color *space,
  int *row, int *col);

// Given a space on a board, gets the corresponding space on another board.
// Returns HEX_EBADSPACE if src_space is not from src_board.
hex_err hex_board_correlate(
  hex_board *dest_board,
  hex_color **dest_space,
  const hex_board *src_board,
  const hex_color *src_space);
hex_err hex_board_rocorrelate(
  const hex_board *dest_board,
  const hex_color **dest_space,
  const hex_board *src_board,
  const hex_color *src_space);

// Populates the 'neighbors' array with the coordinates of the neighbors of the
// given square, with alternating row and column values, and stores the number
// of neighbors. The neighbors array is assumed to have enough room to store six
// neighbors, i.e. it should be of length 12. Returns HEX_EBOUNDS if row, col
// are out-of-bounds.
hex_err hex_board_neighborcoords(
  const hex_board *board,
  int row, int col,
  int *neighbors,
  int *neighbor_count);

// Populates the 'neighbors' array with pointers to the spaces neighboring the
// given space. Returns HEX_EBADSPACE if the given space is not part of the
// given board. The neighbors array should be of length 6.
hex_err hex_board_neighbors(
  hex_board *board,
  hex_color *space,
  hex_color **neighbors,
  int *neighbor_count);
hex_err hex_board_roneighbors(
  const hex_board *board,
  const hex_color *space,
  const hex_color **neighbors,
  int *neighbor_count);

// Clears the given board, resetting it to its initial state.
void hex_board_clear(hex_board *board);

// Copies the data from one board to another board. Returns HEX_ESIZEMISMATCH if
// the boards are not the same size.
hex_err hex_board_copy(hex_board *dest, const hex_board *src);

// Gets the winner of the board, if any. Result is HEX_COLOR_NONE if there is no
// winner. Assumes that the top and bottom edges of the board belong to red, and
// the left and right edges belong to blue.
hex_err hex_board_getwinner(const hex_board *board, hex_color *winner);

#endif
