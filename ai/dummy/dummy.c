#include <hex/hex.h>
#include <hex/hexmon.h>
#include <stdio.h>


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

  if ((err = hex_board_load(state->board, board)) != HEX_OK) {
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


void dummy_destroy(void *data, char *board)
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

    case HEX_ENOMEM:
      exit(3);
    }
  }

  ai->data = state;
  ai->move_callback = &dummy_move;
  ai->destroy_callback = &dummy_destroy;
}
