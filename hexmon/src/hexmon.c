// Monitor program for Hex AIs
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

int load_ai(cnstring path, hex_host_info *host, hex_ai_info *ai)
{
  hex_ai_init *init;
  nstring name, symnames[2];
  int err = 0;

  if ((name = get_ai_name(path)) == NULL) {
    err = 1;
    goto cleanup_none;
  }

  if ((symnames[0] = nconcat(name, NTEXT("_ai_init"))) == NULL) {
    err = 1;
    goto cleanup_name;
  }

  symnames[1] = NTEXT("ai_init");

  if ((init = bind_init_symbol(path, symnames, 2)) == NULL) {
    err = 1;
    goto cleanup_all;
  }

  init(host, ai);

 cleanup_all:
  free(symnames[0]);

 cleanup_name:
  free(name);

 cleanup_none:
  return err;
}

int run_monitor(int capacity, hex_ai_info *ai)
{
  parser *p;
  int row, col;

  parse_table_init();
  p = make_parser(capacity);

  while (1) {
    switch (parser_scan(p, stdin)) {
    case PARSE_EOF:
      goto cleanup;

    case PARSE_CMD_MOVE:
      ai->move_callback(ai->data, parser_data(p), &row, &col);
      announce_move(stdout, row, col);
      break;

    case PARSE_CMD_QUIT:
      if (ai->destroy_callback != NULL) {
        ai->destroy_callback(ai->data, parser_data(p));
      }
      goto cleanup;

    default:
      break;
    }
  }

 cleanup:
  destroy_parser(p);
  return 0;
}
