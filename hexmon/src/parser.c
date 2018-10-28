// Command parser for Hexmon
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

#include <hexmon_private.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#ifdef _WIN32
#include <malloc.h>
#endif


enum state_e {
  S_ERROR, // has to be zero
  S_LINE_START_ERROR,
  S_LINE_START,
  S_MOVE_M,
  S_MOVE_O,
  S_MOVE_V,
  S_MOVE_E,
  S_MOVE_SPC,
  S_MOVE_DATA,
  S_MOVE_OK, // has to be after DATA
  S_QUIT_Q,
  S_QUIT_U,
  S_QUIT_I,
  S_QUIT_T,
  S_QUIT_SPC,
  S_QUIT_DATA,
  S_QUIT_OK, // has to be after DATA
  STATE_COUNT,
};


struct parser_s {
  size_t capacity;
  size_t pos;
  enum state_e state;
  char data[0];
};


#define CHAR_COUNT 128

static int state_table[STATE_COUNT][CHAR_COUNT];


void parse_table_init(void)
{
  // Our error state is 0, so most states are ready.

  // Handle premature newlines. Some of these will get overwritten later, for
  // valid newlines.
  for (int s = 0; s < STATE_COUNT; s++) {
    state_table[s]['\n'] = S_LINE_START_ERROR;
  }

  // Wait for input
  state_table[S_LINE_START][' '] = S_LINE_START;
  state_table[S_LINE_START]['\n'] = S_LINE_START;
  state_table[S_LINE_START_ERROR][' '] = S_LINE_START;
  state_table[S_LINE_START_ERROR]['\n'] = S_LINE_START;

  // Handle the "move" command
  state_table[S_LINE_START]['m'] = S_MOVE_M;
  state_table[S_LINE_START]['M'] = S_MOVE_M;
  state_table[S_LINE_START_ERROR]['m'] = S_MOVE_M;
  state_table[S_LINE_START_ERROR]['M'] = S_MOVE_M;
  state_table[S_MOVE_M]['o'] = S_MOVE_O;
  state_table[S_MOVE_M]['O'] = S_MOVE_O;
  state_table[S_MOVE_O]['v'] = S_MOVE_V;
  state_table[S_MOVE_O]['V'] = S_MOVE_V;
  state_table[S_MOVE_V]['e'] = S_MOVE_E;
  state_table[S_MOVE_V]['E'] = S_MOVE_E;
  state_table[S_MOVE_E][' '] = S_MOVE_SPC;
  state_table[S_MOVE_SPC][' '] = S_MOVE_SPC;

  // Handle the "quit" command
  state_table[S_LINE_START]['q'] = S_QUIT_Q;
  state_table[S_LINE_START]['Q'] = S_QUIT_Q;
  state_table[S_LINE_START_ERROR]['q'] = S_QUIT_Q;
  state_table[S_LINE_START_ERROR]['Q'] = S_QUIT_Q;
  state_table[S_QUIT_Q]['u'] = S_QUIT_U;
  state_table[S_QUIT_Q]['U'] = S_QUIT_U;
  state_table[S_QUIT_U]['i'] = S_QUIT_I;
  state_table[S_QUIT_U]['I'] = S_QUIT_I;
  state_table[S_QUIT_I]['t'] = S_QUIT_T;
  state_table[S_QUIT_I]['T'] = S_QUIT_T;
  state_table[S_QUIT_T][' '] = S_QUIT_SPC;
  state_table[S_QUIT_SPC][' '] = S_QUIT_SPC;

  // Handle board data
  for (int c = 0; c < CHAR_COUNT; c++) {
    if (isgraph(c)) {
      state_table[S_MOVE_SPC][c] = S_MOVE_DATA;
      state_table[S_MOVE_DATA][c] = S_MOVE_DATA;
      state_table[S_QUIT_SPC][c] = S_QUIT_DATA;
      state_table[S_QUIT_DATA][c] = S_QUIT_DATA;
    }
  }

  // Special logic causes the transition to S_MOVE_OK, S_QUIT_OK
  // Handle newline and line trailer
  state_table[S_MOVE_OK][' '] = S_MOVE_OK;
  state_table[S_QUIT_OK][' '] = S_QUIT_OK;
  state_table[S_MOVE_OK]['\n'] = S_LINE_START;
  state_table[S_QUIT_OK]['\n'] = S_LINE_START;

  // Error recovery
  state_table[S_ERROR]['\n'] = S_LINE_START_ERROR;
}


parser *make_parser(int capacity)
{
  size_t datasize = sizeof(char) * ((size_t) (capacity + 1));
  parser *p = calloc(1, sizeof(parser) + datasize);
  p->capacity = (size_t) capacity;
  p->state = S_LINE_START;
  return p;
}


void destroy_parser(parser *p)
{
  free(p);
}


parse_event parser_receive(parser *p, int c)
{
  enum state_e oldst;

  // Normally we expect to get a quit command and exit ourselves, but if stdin
  // gets closed we want to handle that gracefully.
  if (c == EOF) {
    return PARSE_EOF;
  }

  // We might get non-ASCII characters as input. These are valid inside the data
  // part of the command, but nowhere else, and even inside the data command,
  // they're just going to be ignored later. So we go ahead and turn them into
  // ?s, which will trigger an error in the approriate places.
  if (c >= CHAR_COUNT) {
    c = '?';
  }

  // This functions largely as a state machine, but has some special cases for
  // data collection and error recovery.
  oldst = p->state;
  p->state = state_table[oldst][c];

  switch (p->state) {
  case S_LINE_START_ERROR:
    // The last line was an error, so go ahead and signal that.
    return PARSE_ERROR;

  case S_LINE_START:
    // transitioning to line start from an OK state sends the command.
    if (oldst == S_MOVE_OK) { return PARSE_CMD_MOVE; }
    if (oldst == S_QUIT_OK) { return PARSE_CMD_QUIT; }
    return PARSE_CONTINUE;

  case S_MOVE_DATA:
  case S_QUIT_DATA:
    // going into a data state from a non-data state clears the buffer.
    if (oldst != S_MOVE_DATA && oldst != S_QUIT_DATA) {
      memset(p->data, 0, p->capacity);
      p->pos = 0;
    }

    // copy the character into the buffer
    p->data[p->pos++] = (char) c;

    // reaching the end of the buffer triggers the transition to an OK state.
    // Further non-space characters will cause an error.
    if (p->pos == p->capacity) { p->state++; }

    return PARSE_CONTINUE;

  default:
    return PARSE_CONTINUE;
  }
}


char *parser_data(parser *p)
{
  return p->data;
}
