// Hex helper library winner checking
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


hex_err hex_board_getwinner(const hex_board *board, hex_color *winner)
{
  hex_err err = HEX_OK;
  const hex_color **search_queue;
  int search_index = -1;
  int *visited;
  int space_count = board->size * board->size;
  const hex_color *space_zero;
  const hex_color *neighbors[6];
  int neighbor_count;

#define SEARCH_PUSH(spc) (search_queue[++search_index] = (spc))
#define SEARCH_POP() (search_queue[search_index--])
#define VISITED(spc) (visited[(spc) - space_zero])

  search_queue = calloc(space_count, sizeof(const hex_color *));
  if (search_queue == NULL) {
    goto cleanup_done;
  }

  visited = calloc(space_count, sizeof(int));
  if (visited == NULL) {
    goto cleanup_search_queue;
  }

  HEX_TRYC(err, hex_board_rospace(board, 0, 0, &space_zero), cleanup_all);

  // We can start the search from the top and left sides, since if there's a win
  // we'll reach the other side, by definition. Left is blue, top is red.
  for (int k = 0; k < board->size; k++) {
    const hex_color *space;

    HEX_TRYC(err, hex_board_rospace(board, k, 0, &space), cleanup_all);
    if (*space == HEX_COLOR_BLUE) {
      SEARCH_PUSH(space);
    }

    HEX_TRYC(err, hex_board_rospace(board, 0, k, &space), cleanup_all);
    if (*space == HEX_COLOR_RED) {
      SEARCH_PUSH(space);
    }
  }

  // Now we can do a DFS traversal of the board.
  *winner = HEX_COLOR_NONE;
  while (search_index >= 0) {
    int row, col;
    const hex_color *space = SEARCH_POP();
    VISITED(space) = 1;

    // Check for a win. A blue space which is on the right edge of the board is
    // a win for blue, and a red space on the bottom edge of the board is a win
    // for red. It's impossible for both to have one, so early-exit is ok.
    HEX_TRYC(err, hex_board_coords(board, space, &row, &col), cleanup_all);
    if ((*space == HEX_COLOR_BLUE && col == board->size - 1)
        || (*space == HEX_COLOR_RED && row == board->size - 1)) {
      *winner = *space;
      break;
    }

    // Add the neighbors to the search queue if they're the same color as the
    // current space, and we haven't visited them yet.
    HEX_TRYC(err,
             hex_board_roneighbors(
               board, space,
               neighbors, &neighbor_count),
             cleanup_all);
    for (int k = 0; k < neighbor_count; k++) {
      if (!VISITED(neighbors[k]) && *(neighbors[k]) == *space) {
        SEARCH_PUSH(neighbors[k]);
      }
    }
  }

 cleanup_all:
 cleanup_visited:
  free(visited);

 cleanup_search_queue:
  free(search_queue);

 cleanup_done:
  return err;

#undef SEARCH_PUSH
#undef SEARCH_POP
}
