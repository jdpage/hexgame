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


static inline hex_err hex_check_bounds(
  const hex_board *board,
  int row, int col)
{
  if (row < 0 || row >= board->size || col < 0 || col >= board->size) {
    return HEX_EBOUNDS;
  }

  return HEX_OK;
}


static inline hex_err hex_check_space(
  const hex_board *board,
  const hex_color *space,
  int *index)
{
  ptrdiff_t offset = space - board->data;
  if (offset < 0 || offset >= board->size * board->size) {
    return HEX_EBADSPACE;
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
  return sizeof(hex_board) + sizeof(hex_color) * size * size;
}


int hex_board_size(const hex_board *board)
{
  return board->size;
}


hex_color *hex_board_data(hex_board *board, size_t *data_len)
{
  // board->size is > 0
  *data_len = (size_t) (board->size * board->size);
  return board->data;
}


const hex_color *hex_board_rodata(const hex_board *board, size_t *data_len)
{
  // board->size is > 0
  *data_len = (size_t) (board->size * board->size);
  return board->data;
}


hex_err hex_board_space(
  hex_board *board,
  int row, int col,
  hex_color **space)
{
  HEX_TRY(hex_check_bounds(board, row, col));
  *space = &(board->data[row + col * board->size]);
  return HEX_OK;
}


hex_err hex_board_rospace(
  const hex_board *board,
  int row, int col,
  const hex_color **space)
{
  HEX_TRY(hex_check_bounds(board, row, col));
  *space = &(board->data[row + col * board->size]);
  return HEX_OK;
}


hex_err hex_board_coords(
  const hex_board *board,
  const hex_color *space,
  int *row, int *col)
{
  int index;
  HEX_TRY(hex_check_space(board, space, &index));
  *row = index % board->size;
  *col = index / board->size;
  return HEX_OK;
}


hex_err hex_board_correlate(
  hex_board *dest_board,
  hex_color **dest_space,
  const hex_board *src_board,
  const hex_color *src_space)
{
  if (dest_board->size != src_board->size) {
    return HEX_ESIZEMISMATCH;
  }

  int index;
  HEX_TRY(hex_check_space(src_board, src_space, &index));
  *dest_space = &(dest_board->data[index]);
  return HEX_OK;
}


hex_err hex_board_rocorrelate(
  const hex_board *dest_board,
  const hex_color **dest_space,
  const hex_board *src_board,
  const hex_color *src_space)
{
  if (dest_board->size != src_board->size) {
    return HEX_ESIZEMISMATCH;
  }

  int index;
  HEX_TRY(hex_check_space(src_board, src_space, &index));
  *dest_space = &(dest_board->data[index]);
  return HEX_OK;
}


hex_err hex_board_neighborcoords(
  const hex_board *board,
  int row, int col,
  int *neighbors,
  int *neighbor_count)
{
  HEX_TRY(hex_check_bounds(board, row, col));

  *neighbor_count = 0;
  int *ptr = neighbors;
  for (int c = -1; c <= 1; c++) {
    for (int r = -1; r <= 1; r++) {
      // eliminate the center square and one of the diagonals
      if (r == c) { continue; }

      int nr = row + r, nc = col + c;
      if (hex_check_bounds(board, nr, nc) != HEX_OK) {
        continue;
      }

      *(ptr++) = nr;
      *(ptr++) = nc;
      (*neighbor_count)++;
    }
  }

  return HEX_OK;
}


hex_err hex_board_neighbors(
  hex_board *board,
  hex_color *space,
  hex_color **neighbors,
  int *neighbor_count)
{
  int row, col;
  int neighborcoords[12];
  HEX_TRY(hex_board_coords(board, space, &row, &col));
  HEX_TRY(hex_board_neighborcoords(
            board,
            row, col,
            neighborcoords, neighbor_count));

  for (int k = 0; k < *neighbor_count; k++) {
    HEX_TRY(hex_board_space(
              board,
              neighborcoords[2*k], neighborcoords[2*k + 1],
              &(neighbors[k])));
  }

  return HEX_OK;
}



hex_err hex_board_roneighbors(
  const hex_board *board,
  const hex_color *space,
  const hex_color **neighbors,
  int *neighbor_count)
{
  int row, col;
  int neighborcoords[12];
  HEX_TRY(hex_board_coords(board, space, &row, &col));
  HEX_TRY(hex_board_neighborcoords(
            board,
            row, col,
            neighborcoords, neighbor_count));

  for (int k = 0; k < *neighbor_count; k++) {
    HEX_TRY(hex_board_rospace(
              board,
              neighborcoords[2*k], neighborcoords[2*k + 1],
              &(neighbors[k])));
  }

  return HEX_OK;
}


void hex_board_clear(hex_board *board)
{
  memset(board->data, 0, sizeof(hex_color) * board->size * board->size);
}


hex_err hex_board_copy(hex_board *dest, const hex_board *src)
{
  if (dest->size != src->size) {
    return HEX_ESIZEMISMATCH;
  }

  if (src != dest) {
    memcpy(dest->data, src->data, sizeof(hex_color) * dest->size * dest->size);
  }

  return HEX_OK;
}
