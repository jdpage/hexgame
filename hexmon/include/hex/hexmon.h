// Interface for hexmon-hosted Hex AIs
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

#ifndef HEX_HEXMON_H
#define HEX_HEXMON_H


// Information about the host process and current game. The pointer passed to
// the init function is valid for the entire life of the process, so AIs may
// store the pointer for later use if they wish.
typedef struct hex_host_info_s {
  // Additional arguments passed on command line, starting with the path to the
  // module.
  char **optv;
  int optc;

  // The board size.
  int size;

  // The color the AI is playing as.
  char color;
} hex_host_info;


// Information about how to call the AI. The init function is expected to
// populate this structure.
typedef struct hex_ai_info_s {
  // A user-defined pointer for storing state data. Will be passed as the data
  // argument to callbacks. May be left NULL.
  void *data;

  // Called when requesting a move. Must not be NULL. The board argument
  // contains the current board state, and it is expected to populate row and
  // col before returning.
  void (*move_callback)(
    void *data,
    char *board,
    int *row, int *col);

  // Called when the game ends. May be NULL. The board argument contains the
  // final board state.
  void (*destroy_callback)(
    void *data,
    char *board);
} hex_ai_info;


// When loading an AI from e.g. libfoo.so, hexmon will look for a symbol named
// foo_ai_init, and call it with the following signature. It is expected to
// populate the structure passed to the ai argument and perform any other
// initialization necessary.
typedef void hex_ai_init(
  hex_host_info *host,
  hex_ai_info *ai);


#endif
