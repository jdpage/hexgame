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


typedef struct {
  hex_host_info *host;
  hex_board *board;
} dummy_state;


void dummy_move(void *data, char *board, int *row, int *col)
{
  dummy_state *state = data;
  hex_err err;
  hex_color *space;
  int size = hex_board_size(state->board);

  if ((err = hex_board_scan(state->board, board)) != HEX_OK) {
    fprintf(stderr, "%s: board size mismatch\n", state->host->optv[0]);
    exit(2);
  }

  // find the first empty space and play in it
  for (int c = 0; c < size; c++) {
    for (int r = 0; r < size; r++) {
      hex_board_space(state->board, r, c, &space);
      if (*space == HEX_COLOR_NONE) {
        *row = r;
        *col = c;
        return;
      }
    }
  }
}


void dummy_destroy(void *data, char * UNUSED(board))
{
  dummy_state *state = data;
  free(state->board);
  free(state);
}


void dummy_ai_init(hex_host_info *host, hex_ai_info *ai)
{
  hex_err err;
  dummy_state *state = malloc(sizeof(dummy_state));
  state->host = host;

  if ((err = hex_board_init(&state->board, host->size, calloc)) != HEX_OK) {
    switch (err) {
    case HEX_EBOUNDS:
      fprintf(stderr, "%s: invalid board size %d\n",
              host->optv[0], host->size);
      exit(2);

    default:
      fprintf(stderr, "%s: unknown error in board init\n",
              host->optv[0]);
      exit(3);

    case HEX_ENOMEM:
      exit(3);
    }
  }

  ai->data = state;
  ai->move_callback = &dummy_move;
  ai->destroy_callback = &dummy_destroy;
}
