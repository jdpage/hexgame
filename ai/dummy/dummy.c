// Example AI implementation which plays particularly badly
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

#include <hex/hex.h>
#include <hex/hexmon.h>
#include <stdio.h>

#ifdef __GNUC__
#  define UNUSED(x) UNUSED_ ## x __attribute__((__unused__))
#else
#  define UNUSED(x) UNUSED_ ## x
#endif

#if defined(_WIN32) && defined(dummy_EXPORTS)
#   define DUMMY_EXPORT extern __declspec(dllexport)
#else
#   define DUMMY_EXPORT
#endif

typedef struct {
  hex_host_info *host;
} dummy_state;


hex_board *make_board(dummy_state *state)
{
  hex_err err;
  hex_board *board;

  if ((err = hex_board_init(&board, state->host->size, calloc)) != HEX_OK) {
    switch (err) {
    case HEX_EBOUNDS:
      fprintf(stderr, "%s: invalid board size %d\n",
              state->host->optv[0], state->host->size);
      exit(2);

    default:
      fprintf(stderr, "%s: unknown error in board init\n",
              state->host->optv[0]);
      exit(3);

    case HEX_ENOMEM:
      exit(3);
    }
  }

  return board;
}


void dummy_move(void *data, char *board_state, int *row, int *col)
{
  dummy_state *state = data;
  hex_err err;
  hex_tile *tile;
  hex_board *board = make_board(state);
  int size = hex_board_size(board);

  if ((err = hex_board_scan(board, board_state)) != HEX_OK) {
    fprintf(stderr, "%s: board size mismatch\n", state->host->optv[0]);
    exit(2);
  }

  // let's pretend that we're playing as red
  if (state->host->color == 'b') {
    hex_board *flipped = make_board(state);
    hex_board_flipcopy(flipped, board);
    free(board);
    board = flipped;
  }

  // find the first empty space and play in it
  for (int c = 0; c < size; c++) {
    for (int r = 0; r < size; r++) {
      hex_board_rctile(board, r, c, &tile);
      if (hex_tile_free(tile)) {
        *row = r;
        *col = c;
        goto done;
      }
    }
  }

  fprintf(stderr, "%s: no free spaces; expected quit?\n",
          state->host->optv[0]);
  exit(2);

 done:
  // if we were actually blue, we need to swap our choice
  if (state->host->color == 'b') {
    int t = *row;
    *row = *col;
    *col = t;
  }
}


void dummy_destroy(void *data, char * UNUSED(board))
{
  dummy_state *state = data;
  free(state);
}


DUMMY_EXPORT void dummy_ai_init(hex_host_info *host, hex_ai_info *ai)
{
  dummy_state *state;

  // For debugging purposes, print the provided options to stderr.
  for (int k = 0; k < host->optc; k++) {
    fprintf(stderr, "optv[%d]: %s\n", k, host->optv[k]);
  }

  state = malloc(sizeof(dummy_state));
  state->host = host;

  ai->data = state;
  ai->move_callback = &dummy_move;
  ai->destroy_callback = &dummy_destroy;
}
