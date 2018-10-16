// Hex board general functions
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
#include <limits.h>
#include <stddef.h>
#include <string.h>


static inline hex_err hex_check_size(int size)
{
  if (size < 0 || size > INT_MAX / size) {
    return HEX_EBOUNDS;
  }

  return HEX_OK;
}


static inline hex_err hex_check_sbounds(int size, int row, int col)
{
  if (row < 0 || row >= size || col < 0 || col >= size) {
    return HEX_EBOUNDS;
  }

  return HEX_OK;
}


static inline hex_err hex_check_ibounds(int size, int index)
{
  if (index < 0 || index > size * size) {
    return HEX_EBOUNDS;
  }

  return HEX_OK;
}


static inline hex_err hex_check_tile(
  const hex_board *board,
  const hex_tile *tile,
  int *index)
{
  ptrdiff_t offset = tile - board->data;
  if (offset < 0 || offset >= board->size * board->size) {
    return HEX_EBOUNDS;
  }

  // during initialization we already established that all space offsets fit
  // into an int, so this cast is ok.
  if (index != NULL) {
    *index = (int) offset;
  }
  return HEX_OK;
}


hex_err hex_board_init(hex_board **board, int size, hex_alloc *alloc)
{
  HEX_TRY(hex_check_size(size));
  *board = alloc(1, hex_board_sizeof(size));
  if (*board == NULL) {
    return HEX_ENOMEM;
  }

  (*board)->size = size;
  return HEX_OK;
}


hex_err hex_board_initat(hex_board *board, int size)
{
  HEX_TRY(hex_check_size(size));
  memset(board, 0, hex_board_sizeof(size));
  board->size = size;
  return HEX_OK;
}


hex_err hex_board_initatz(hex_board *board, int size)
{
  HEX_TRY(hex_check_size(size));
  board->size = size;
  return HEX_OK;
}


size_t hex_board_sizeof(int size)
{
  return sizeof(hex_board) + sizeof(hex_tile) * ((size_t) (size * size));
}


int hex_board_size(const hex_board *board)
{
  return board->size;
}


hex_tile *hex_board_data(hex_board *board, size_t *data_len)
{
  // board->size is > 0
  *data_len = (size_t) (board->size * board->size);
  return board->data;
}


const hex_tile *hex_board_rodata(const hex_board *board, size_t *data_len)
{
  // board->size is > 0
  *data_len = (size_t) (board->size * board->size);
  return board->data;
}


hex_err hex_board_index(
  const hex_board *board,
  int row, int col,
  int *index)
{
  HEX_TRY(hex_check_sbounds(board->size, row, col));
  *index = hex_board_unsafe_sindex(board->size, row, col);
  return HEX_OK;
}


int hex_board_unsafe_index(
  const hex_board *board,
  int row, int col)
{
  return hex_board_unsafe_sindex(board->size, row, col);
}


hex_err hex_board_sindex(int size, int row, int col, int *index)
{
  HEX_TRY(hex_check_sbounds(size, row, col));
  *index = hex_board_unsafe_sindex(size, row, col);
  return HEX_OK;
}


int hex_board_unsafe_sindex(int size, int row, int col)
{
  return row + col * size;
}


int hex_board_index_count(const hex_board *board)
{
  return hex_board_sindex_count(board->size);
}


int hex_board_sindex_count(int size) { return size * size; }


hex_err hex_board_rctile(
  hex_board *board,
  int row, int col,
  hex_tile **tile)
{
  int index;
  HEX_TRY(hex_board_index(board, row, col, &index));
  *tile = &board->data[index];
  return HEX_OK;
}


hex_tile *hex_board_unsafe_rctile(hex_board *board, int row, int col)
{
  int index = hex_board_unsafe_index(board, row, col);
  return &board->data[index];
}


hex_err hex_board_rorctile(
  const hex_board *board,
  int row, int col,
  const hex_tile **tile)
{
  int index;
  HEX_TRY(hex_board_index(board, row, col, &index));
  *tile = &board->data[index];
  return HEX_OK;
}


const hex_tile *hex_board_unsafe_rorctile(
  const hex_board *board, int row, int col)
{
  int index = hex_board_unsafe_index(board, row, col);
  return &board->data[index];
}


hex_err hex_board_itile(
  hex_board *board, int index, hex_tile **tile)
{
  HEX_TRY(hex_check_ibounds(board->size, index));
  *tile = &board->data[index];
  return HEX_OK;
}


hex_tile *hex_board_unsafe_itile(hex_board *board, int index)
{
  return &board->data[index];
}


hex_err hex_board_roitile(
  const hex_board *board, int index, const hex_tile **tile)
{
  HEX_TRY(hex_check_ibounds(board->size, index));
  *tile = &board->data[index];
  return HEX_OK;
}


const hex_tile *hex_board_unsafe_roitile(
  const hex_board *board, int index)
{
  return &board->data[index];
}


hex_err hex_board_tcoords(
  const hex_board *board,
  const hex_tile *tile,
  int *row, int *col)
{
  int index;
  HEX_TRY(hex_check_tile(board, tile, &index));
  *row = index % board->size;
  *col = index / board->size;
  return HEX_OK;
}


hex_err hex_board_icoords(
  const hex_board *board,
  int index, int *row, int *col)
{
  HEX_TRY(hex_check_ibounds(board->size, index));
  *row = index % board->size;
  *col = index / board->size;
  return HEX_OK;
}


hex_err hex_board_correlate(
  hex_board *dest_board,
  hex_tile **dest_tile,
  const hex_board *src_board,
  const hex_tile *src_tile)
{
  if (dest_board->size != src_board->size) {
    return HEX_ESIZEMISMATCH;
  }

  int index;
  HEX_TRY(hex_check_tile(src_board, src_tile, &index));
  *dest_tile = &dest_board->data[index];
  return HEX_OK;
}


hex_err hex_board_rocorrelate(
  const hex_board *dest_board,
  const hex_tile **dest_tile,
  const hex_board *src_board,
  const hex_tile *src_tile)
{
  if (dest_board->size != src_board->size) {
    return HEX_ESIZEMISMATCH;
  }

  int index;
  HEX_TRY(hex_check_tile(src_board, src_tile, &index));
  *dest_tile = &dest_board->data[index];
  return HEX_OK;
}


hex_err hex_board_rcneighbors(
  const hex_board *board,
  int row, int col,
  int *neighbors,
  int *neighbor_count)
{
  HEX_TRY(hex_check_sbounds(board->size, row, col));

  *neighbor_count = 0;
  int *ptr = neighbors;
  for (int c = -1; c <= 1; c++) {
    for (int r = -1; r <= 1; r++) {
      // eliminate the center square and one of the diagonals
      if (r == c) { continue; }

      int nr = row + r, nc = col + c;
      if (hex_check_sbounds(board->size, nr, nc) != HEX_OK) {
        continue;
      }

      *(ptr++) = nr;
      *(ptr++) = nc;
      (*neighbor_count)++;
    }
  }

  return HEX_OK;
}


hex_err hex_board_ineighbors(
  const hex_board *board,
  int index,
  int *neighbors,
  int *neighbor_count)
{
  int row, col;
  int rcneighbors[12];
  HEX_TRY(hex_board_icoords(board, index, &row, &col));
  HEX_TRY(hex_board_rcneighbors(
            board, row, col,
            rcneighbors, neighbor_count));

  for (int k = 0; k < *neighbor_count; k++) {
    neighbors[k] = hex_board_unsafe_index(
      board, rcneighbors[2*k], rcneighbors[2*k+1]);
  }

  return HEX_OK;
}


hex_err hex_board_tneighbors(
  hex_board *board,
  hex_tile *tile,
  hex_tile **neighbors,
  int *neighbor_count)
{
  int row, col;
  int rcneighbors[12];
  HEX_TRY(hex_board_tcoords(board, tile, &row, &col));
  HEX_TRY(hex_board_rcneighbors(
            board, row, col,
            rcneighbors, neighbor_count));

  for (int k = 0; k < *neighbor_count; k++) {
    neighbors[k] = hex_board_unsafe_rctile(
      board, rcneighbors[2*k], rcneighbors[2*k + 1]);
  }

  return HEX_OK;
}


hex_err hex_board_rotneighbors(
  const hex_board *board,
  const hex_tile *tile,
  const hex_tile **neighbors,
  int *neighbor_count)
{
  int row, col;
  int rcneighbors[12];
  HEX_TRY(hex_board_tcoords(board, tile, &row, &col));
  HEX_TRY(hex_board_rcneighbors(
            board, row, col,
            rcneighbors, neighbor_count));

  for (int k = 0; k < *neighbor_count; k++) {
    neighbors[k] = hex_board_unsafe_rorctile(
      board, rcneighbors[2*k], rcneighbors[2*k + 1]);
  }

  return HEX_OK;
}


void hex_board_clear(hex_board *board)
{
  memset(board->data, 0, sizeof(hex_color) * ((size_t) (board->size * board->size)));
}


hex_err hex_board_copy(hex_board *dest, const hex_board *src)
{
  if (dest->size != src->size) {
    return HEX_ESIZEMISMATCH;
  }

  if (src != dest) {
    memcpy(dest->data, src->data, sizeof(hex_color) * ((size_t) (dest->size * dest->size)));
  }

  return HEX_OK;
}


hex_color *hex_tile_color(hex_tile *tile)
{
  return &tile->color;
}


const hex_color *hex_tile_rocolor(const hex_tile *tile)
{
  return &tile->color;
}
