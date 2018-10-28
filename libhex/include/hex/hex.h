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

#if defined(__GNUC__)
  #define HEX_PACKED __attribute__ ((__packed__))
#else
  #define HEX_PACKED
#endif

#if defined(_WIN32) && defined(hex_EXPORTS)
#   define HEX_EXPORT extern __declspec(dllexport)
#else
#   define HEX_EXPORT
#endif

enum hex_err_e {
  HEX_OK,
  HEX_ENOMEM,
  HEX_EBOUNDS,
  HEX_ESIZEMISMATCH,
  HEX_ESHORTBUFFER,
  HEX_ERRS_MAX
};

enum hex_color_e {
  HEX_COLOR_NONE,
  HEX_COLOR_RED,
  HEX_COLOR_BLUE,
  HEX_COLORS_MAX
} HEX_PACKED;

typedef enum hex_err_e hex_err;
typedef enum hex_color_e hex_color;
typedef struct hex_tile_s hex_tile;
typedef struct hex_board_s hex_board;

typedef void *hex_alloc(size_t nmemb, size_t size);

// Creates a new hex board of the given size. In the case of hex_board_init, the
// pointer returned can be freed as usual for memory allocated with the given
// allocator function. Returns HEX_ENOMEM if allocation fails, and HEX_EBOUNDS
// if the size is invalid. The allocator is assumed to return pre-zeroed memory
// (as calloc(3)). The hex_board_initat variant does not assume zeroed memory,
// while hex_board_initatz does.
HEX_EXPORT hex_err hex_board_init(
  hex_board **board,
  int size,
  hex_alloc *alloc);
HEX_EXPORT hex_err hex_board_initat(hex_board *board, int size);
HEX_EXPORT hex_err hex_board_initatz(hex_board *board, int size);

// Gets the number of bytes necessary to store a board of the given size. Can be
// used to allocate memory for use with hex_board_initat.
HEX_EXPORT size_t hex_board_sizeof(int size);

// Gets the size of the board.
HEX_EXPORT int hex_board_size(const hex_board *board);

// Gets a pointer to the board data. Use hex_board_index to get individual tiles
// within the buffer.
HEX_EXPORT hex_tile *hex_board_data(hex_board *board, size_t *data_len);
HEX_EXPORT const hex_tile *hex_board_rodata(const hex_board *board, size_t *data_len);

// Converts a row, column pair into an index. Returns HEX_EBOUNDS if the given
// coordinates are out of bounds.
HEX_EXPORT hex_err hex_board_index(
  const hex_board *board,
  int row, int col,
  int *index);
HEX_EXPORT int hex_board_unsafe_index(
  const hex_board *board,
  int row, int col);
HEX_EXPORT hex_err hex_board_sindex(
  int size, int row, int col, int *index);
HEX_EXPORT int hex_board_unsafe_sindex(
  int size, int row, int col);

// Returns a number one larger than the largest index returned by
// hex_board_index. This is the same value placed into data_len when calling
// hex_board_data.
HEX_EXPORT int hex_board_index_count(const hex_board *board);
HEX_EXPORT int hex_board_sindex_count(int size);

// Dumps the string representation of the board into the given buffer as a
// null-terminated string. If the buffer is not long enough to hold the entire
// board and a null byte, HEX_ESHORTBUFFER is returned. hex_board_dumpsize
// returns the number of bytes required to hold a board dump, including the null
// terminator. The string representation consists of each space in column-major
// order where the characters 'r', 'b', and '.' denote red, blue, and empty
// spaces respectively.
HEX_EXPORT hex_err hex_board_dump(const hex_board *board, char *buf, size_t buf_len);
HEX_EXPORT size_t hex_board_dumpsize(const hex_board *board);

// Loads the string representation into the board. If the string representation
// is the wrong size, HEX_ESIZEMISMATCH is returned, and the board data may have
// been partially overwritten. The accepted string representation is as above,
// and is handled case-insensitively, and any unrecognized character is assumed
// to be an empty space.
HEX_EXPORT hex_err hex_board_scan(hex_board *board, const char *buf);

// Returns 0 if the board is empty, and nonzero otherwise.
HEX_EXPORT int hex_board_is_empty(const hex_board *board);

// Gets a pointer to a tile on the board. Returns HEX_EBOUNDS if the given tile
// location specifiers are out of bounds. The unsafe variants do not perform
// bounds-checking.
HEX_EXPORT hex_err hex_board_rctile(
  hex_board *board,
  int row, int col,
  hex_tile **tile);
HEX_EXPORT hex_tile *hex_board_unsafe_rctile(
  hex_board *board,
  int row, int col);
HEX_EXPORT hex_err hex_board_rorctile(
  const hex_board *board,
  int row, int col,
  const hex_tile **tile);
HEX_EXPORT const hex_tile *hex_board_unsafe_rorctile(
  const hex_board *board,
  int row, int col);
HEX_EXPORT hex_err hex_board_itile(
  hex_board *board,
  int index,
  hex_tile **tile);
HEX_EXPORT hex_tile *hex_board_unsafe_itile(
  hex_board *board, int index);
HEX_EXPORT hex_err hex_board_roitile(
  const hex_board *board,
  int index,
  const hex_tile **tile);
HEX_EXPORT const hex_tile *hex_board_unsafe_roitile(
  const hex_board *board,
  int index);

// Gets the coordinates of the given tile or tile index. Returns HEX_EBOUNDS if
// the given tile is not part of the given board or the tile index is out of
// range.
HEX_EXPORT hex_err hex_board_tcoords(
  const hex_board *board,
  const hex_tile *tile,
  int *row, int *col);
HEX_EXPORT hex_err hex_board_icoords(
  const hex_board *board,
  int index, int *row, int *col);

// Given a tile on a board, gets the corresponding tile on another board.
// Returns HEX_EBOUNDS if src_tile is not from src_board, and HEX_ESIZEMISMATCH
// if src_board and dest_board are not the same size.
HEX_EXPORT hex_err hex_board_correlate(
  hex_board *dest_board,
  hex_tile **dest_tile,
  const hex_board *src_board,
  const hex_tile *src_tile);
HEX_EXPORT hex_err hex_board_rocorrelate(
  const hex_board *dest_board,
  const hex_tile **dest_tile,
  const hex_board *src_board,
  const hex_tile *src_tile);

// Populates the 'neighbors' array with the coordinates of the neighbors of the
// given tile, with alternating row and column values, and stores the number of
// neighbors. The neighbors array is assumed to have enough room to store six
// neighbors, i.e. it should be of length 12. Returns HEX_EBOUNDS if row, col
// are out-of-bounds.
HEX_EXPORT hex_err hex_board_rcneighbors(
  const hex_board *board,
  int row, int col,
  int *neighbors,
  int *neighbor_count);

// Populates the 'neighbors' array with the indices of the neighbors of the
// given tile. The neighbors array is assumed to have enough room to store six
// neighbors, i.e. it should be of length 6. Returns HEX_EBOUNDS if index is
// out-of-bounds.
HEX_EXPORT hex_err hex_board_ineighbors(
  const hex_board *board,
  int index,
  int *neighbors,
  int *neighbor_count);

// Populates the 'neighbors' array with pointers to the tiles neighboring the
// given tile. Returns HEX_EBOUNDS if the given space is not part of the given
// board. The neighbors array should be of length 6.
HEX_EXPORT hex_err hex_board_tneighbors(
  hex_board *board,
  hex_tile *tile,
  hex_tile **neighbors,
  int *neighbor_count);
HEX_EXPORT hex_err hex_board_rotneighbors(
  const hex_board *board,
  const hex_tile *tile,
  const hex_tile **neighbors,
  int *neighbor_count);

// Clears the given board, resetting it to its initial state.
HEX_EXPORT void hex_board_clear(hex_board *board);

// Copies the data from one board to another board. Returns HEX_ESIZEMISMATCH if
// the boards are not the same size. If src and dest point to the same board,
// this is a no-op.
HEX_EXPORT hex_err hex_board_copy(hex_board *dest, const hex_board *src);

// Updates dest to be a flipped copy of src, i.e. the row/column axes have been
// exchanged, and red/blue have been switched. Returns HEX_ESIZEMISMATCH if the
// boards are not the same size, and HEX_EBOUNDS if src and dest are the same
// pointer.
HEX_EXPORT hex_err hex_board_flipcopy(hex_board *dest, const hex_board *src);

// Gets the winner of the board, if any. Result is HEX_COLOR_NONE if there is no
// winner. Assumes that the top and bottom edges of the board belong to red, and
// the left and right edges belong to blue.
HEX_EXPORT hex_err hex_board_getwinner(
  const hex_board *board,
  hex_color *winner);

// Gets a pointer to the color of the given tile.
HEX_EXPORT hex_color *hex_tile_color(hex_tile *tile);
HEX_EXPORT const hex_color *hex_tile_rocolor(const hex_tile *tile);

// True if a given tile is free
#define hex_tile_free(t) (*hex_tile_color(t) == HEX_COLOR_NONE)

#endif
