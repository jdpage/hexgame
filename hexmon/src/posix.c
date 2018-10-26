// Hexmon POSIX implementations
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
#include <dlfcn.h>
#include <errno.h>
#include <libgen.h>
#include <limits.h>
#include <string.h>

void announce_move(FILE *channel, int row, int col)
{
  fprintf(channel, "%d %d\n", row, col);
  fflush(channel);
}

parse_event parser_scan(parser *p, FILE *input)
{
  parse_event e;

  do {
    e = parser_receive(p, fgetc(input));
    if (e == PARSE_ERROR) {
      fprintf(stderr, "invalid command\n");
    }
  } while (e == PARSE_ERROR || e == PARSE_CONTINUE);

  return e;
}

char *nconcat(const char *left, const char *right)
{
  char *conc;
  size_t llen, rlen, clen;
  llen = strlen(left);
  rlen = strlen(right);
  clen = llen + rlen;

  if ((conc = calloc(clen + 1, sizeof(char))) == NULL) {
    return NULL;
  }

  memcpy(conc, left, llen);
  memcpy(conc + llen, right, rlen);
  conc[clen] = 0;

  return conc;
}

char *get_ai_name(const char *cpath) {
  char *stem, *rstem, *path;

  // Irritatingly, basename may modify its argument, so make a duplicate.
  if ((path = strdup(cpath)) == NULL) {
    return NULL;
  }

  stem = basename(path);

  // Remove the extension
  for (char *p = stem; *p != '\0'; p++) {
    if (*p == '.') {
      *p = '\0';
      break;
    }
  }

  // Remove the library prefix
  if (strncmp(stem, "lib", 3) == 0) {
    stem += 3;
  }

  // stem is a slice into path, so we can't return it directly because it'll be
  // impossible to free. Note that strdup might return NULL, but that's what
  // we'd return ourselves anyway if it did, so no need for special error
  // handling.
  rstem = strdup(stem);
  free(path);
  return rstem;
}

hex_ai_init *bind_init_symbol(const char *path, const char **syms, int count)
{
  void *lib;
  hex_ai_init *init = NULL;

  if ((lib = dlopen(path, RTLD_NOW)) == NULL) {
    fprintf(stderr, "error loading \"%s\": %s\n",
            path, dlerror());
    return NULL;
  }

  for (int k = 0; k < count; k++) {
    if ((init = dlsym(lib, syms[k])) != NULL) {
      break;
    }

    fprintf(stderr, "couldn't bind \"%s\": %s\n",
            syms[k], dlerror());
  }

  if (init == NULL) {
    fprintf(stderr, "error binding all given symbols\n");
    dlclose(lib);
  }

  return init;
}

int main(int argc, char *argv[])
{
  if (argc < 4) {
    fprintf(stderr, MSG_USAGE, argv[0]);
    return 1;
  }

  errno = 0;
  long size = strtol(argv[1], NULL, 10);
  if (errno != 0 || size < INT_MIN || size > INT_MAX) {
    fprintf(stderr, MSG_BADSIZE);
    return 1;
  }

  hex_host_info host = {
    .optv = argv + 3,
    .optc = argc - 3,
    .size = (int) size,
    .color = argv[2][0],
  };

  if (host.color != CCOLOR_BLUE && host.color != CCOLOR_RED) {
    fprintf(stderr, MSG_BADCOLOR, argv[2]);
    return 1;
  }

  if (host.size < 1 || host.size > INT_MAX / host.size) {
    fprintf(stderr, MSG_SIZERANGE, host.size);
    return 1;
  }

  hex_ai_info ai = { 0 };
  int err;
  if ((err = load_ai(argv[3], &host, &ai)) != 0) {
    return err;
  }

  return run_monitor((int) (size * size), &ai);
}
