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
  int *search_queue;
  int *visited;
  int neighbors[6];
  int neighbor_count;
  int search_index = -1;
  int index_count = hex_board_index_count(board);
  hex_err err = HEX_OK;

#define SEARCH_PUSH(spc) (search_queue[++search_index] = (spc))
#define SEARCH_POP() (search_queue[search_index--])
#define COLOR(i) (board->data[i].color)

  search_queue = calloc((size_t) index_count, sizeof(int));
  if (search_queue == NULL) {
    goto cleanup_done;
  }

  visited = calloc((size_t) index_count, sizeof(int));
  if (visited == NULL) {
    goto cleanup_search_queue;
  }

  // We can start the search from the top and left sides, since if there's a win
  // we'll reach the other side, by definition. Left is blue, top is red.
  for (int k = 0; k < board->size; k++) {
    int index = hex_board_unsafe_index(board, k, 0);
    if (COLOR(index) == HEX_COLOR_BLUE) {
      SEARCH_PUSH(index);
    }

    index = hex_board_unsafe_index(board, 0, k);
    if (COLOR(index) == HEX_COLOR_RED) {
      SEARCH_PUSH(index);
    }
  }

  // Now we can do a DFS traversal of the board.
  *winner = HEX_COLOR_NONE;
  while (search_index >= 0) {
    int row, col, index = SEARCH_POP();
    visited[index] = 1;

    // Check for a win. A blue space which is on the right edge of the board is
    // a win for blue, and a red space on the bottom edge of the board is a win
    // for red. It's impossible for both to have one, so early-exit is ok.
    HEX_TRYC(err, hex_board_icoords(board, index, &row, &col), cleanup_all);
    if ((COLOR(index) == HEX_COLOR_BLUE && col == board->size - 1)
        || (COLOR(index) == HEX_COLOR_RED && row == board->size - 1)) {
      *winner = COLOR(index);
      break;
    }

    // Add the neighbors to the search queue if they're the same color as the
    // current space, and we haven't visited them yet. Ignoring the error 
    HEX_TRYC(err,
             hex_board_ineighbors(
               board, index,
               neighbors, &neighbor_count),
             cleanup_all);
    for (int k = 0; k < neighbor_count; k++) {
      if (!visited[neighbors[k]] && COLOR(neighbors[k]) == COLOR(index)) {
        SEARCH_PUSH(neighbors[k]);
      }
    }
  }

 cleanup_all:
  free(visited);

 cleanup_search_queue:
  free(search_queue);

 cleanup_done:
  return err;

#undef SEARCH_PUSH
#undef SEARCH_POP
#undef COLOR
}
