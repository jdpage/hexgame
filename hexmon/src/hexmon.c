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

#include <hex/hexmon.h>
#include <dlfcn.h>
#include <errno.h>
#include <limits.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <libgen.h>


static const char *symsuffix = "_ai_init";
#define symsuffixlen 8


int load_ai(hex_host_info *host, hex_ai_info *ai) {
  void *lib;
  hex_ai_init *init;
  char *path, *stem, *symname;
  size_t symlen, stemlen;
  int err = 0;

  // Irritatingly, basename may modify its argument.
  if ((path = strdup(host->optv[0])) == NULL) {
    err = 1;
    goto cleanup_none;
  }
  stem = basename(path);
  for (char *p = stem; *p != '\0'; p++) {
    if (*p == '.') {
      *p = '\0';
      break;
    }
  }

  if (strncmp(stem, "lib", 3) == 0) {
    stem += 3;
  }

  stemlen = strlen(stem);
  symlen = stemlen + symsuffixlen;
  if ((symname = calloc(symlen + 1, sizeof(char))) == NULL) {
    err = 1;
    goto cleanup_path;
  }
  memcpy(symname, stem, stemlen);
  memcpy(symname + stemlen, symsuffix, symsuffixlen);
  symname[symlen] = '\0';

  if ((lib = dlopen(host->optv[0], RTLD_NOW)) == NULL) {
    fprintf(stderr, "error loading \"%s\": %s\n",
            host->optv[0], dlerror());
    err = 1;
    goto cleanup;
  }

  if ((init = dlsym(lib, symname)) == NULL) {
    fprintf(stderr, "error binding \"%s\": %s\n",
            symname, dlerror());
    err = 1;
    goto error_cleanup_lib;
  }

  init(host, ai);

  goto cleanup;

 error_cleanup_lib:
  dlclose(lib);

 cleanup:
 cleanup_symname:
  free(symname);

 cleanup_path:
  // stem doesn't need to be freed
  free(path);

 cleanup_none:
  return err;
}


int run_monitor(int size, hex_ai_info *ai)
{
  char *board;
  int row, col;
  while (1) {
    if (scanf(" move %ms", &board) > 0) {
      ai->move_callback(ai->data, board, &row, &col);
      free(board);
      printf("%d %d\n", row, col);
      fflush(stdout);
    } else if (scanf(" quit %ms", &board) > 0) {
      if (ai->destroy_callback != NULL) {
        ai->destroy_callback(ai->data, board);
      }
      free(board);
      return 0;
    } else {
      return 2;
    }
  }
}


int main(int argc, char *argv[])
{
  if (argc < 4) {
    fprintf(stderr, "usage: %s size color path [opts...]\n", argv[0]);
    return 1;
  }

  errno = 0;
  int size = strtol(argv[1], NULL, 10);
  if (errno != 0) {
    fprintf(stderr, "size argument must be an integer\n");
    return 1;
  }

  hex_host_info host = {
    .optv = argv + 3,
    .optc = argc - 3,
    .size = size,
    .color = argv[2][0],
  };

  if (host.color != 'r' && host.color != 'b') {
    fprintf(stderr, "color argument \"%s\" should be \"red\" or \"blue\"\n",
            argv[2]);
  }

  if (host.size < 1 || host.size > INT_MAX / host.size) {
    fprintf(stderr, "size argument %d out of range\n", host.size);
    return 1;
  }

  hex_ai_info ai = { 0 };
  int err;
  if ((err = load_ai(&host, &ai)) != 0) {
    return err;
  }

  return run_monitor(size, &ai);
}
