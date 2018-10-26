// Hexmon Win32 implementations
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
#include <malloc.h>
#include <wchar.h>

void announce_move(FILE *channel, int row, int col) {
  fwprintf(channel, L"%d %d\n", row, col);
  fflush(channel);
}

parse_event parser_scan(parser *p, FILE *input)
{
  parse_event e;

  do {
    e = parser_receive(p, fgetwc(input));
    if (e == PARSE_ERROR) {
      fwprintf(stderr, L"invalid command\n");
    }
  } while (e == PARSE_ERROR || e == PARSE_CONTINUE);

  return e;
}

wchar_t *nconcat(const wchar_t *left, const wchar_t *right)
{
  wchar_t *conc;
  size_t llen, rlen, clen;
  llen = wcslen(left);
  rlen = wcslen(right);
  clen = llen + rlen;

  if ((conc = calloc(clen + 1, sizeof(wchar_t))) == NULL) {
    return NULL;
  }

  wmemcpy_s(conc, clen, left, llen);
  wmemcpy_s(conc + llen, clen - llen, right, rlen);
  conc[clen] = 0;

  return conc;
}

wchar_t *get_ai_name(const wchar_t *cpath)
{
  wchar_t *stem, *rstem;
  errno_t e;
  size_t pathlen = wcslen(cpath);

  // Stem definitely won't be larger than the path.
  if ((stem = calloc(pathlen, sizeof(wchar_t))) == NULL) {
    return NULL;
  }

  e = _wsplitpath_s(
    cpath,
    NULL, 0, // don't care about the drive
    NULL, 0, // don't care about the directory
    stem, pathlen,
    NULL, 0); // don't care about the extension
  if (e != 0) {
    free(stem);
    return NULL;
  }

  // Remove the library prefix
  if (wcsncmp(stem, L"lib", 3) == 0) {
    rstem = stem + 3;
  } else {
    rstem = stem;
  }

  // rstem is a slice into stem, so we can't return it directly because it'll be
  // impossible to free. Note that wstrdup might return NULL, but that's what
  // we'd return ourselves anyway if it did, so no need for special error
  // handling.
  rstem = _wcsdup(rstem);
  free(stem);
  return rstem;
}

char *mbconvert(const wchar_t *s)
{
  int len = WideCharToMultiByte(CP_UTF8, 0, s, -1, NULL, 0, NULL, NULL);
  char *mbstr = calloc(len, sizeof(char));
  WideCharToMultiByte(CP_UTF8, 0, s, -1, mbstr, len, NULL, NULL);
  return mbstr;
}

hex_ai_init *bind_init_symbol(
  const wchar_t *path,
  const wchar_t **syms,
  int count)
{
  HINSTANCE lib;
  hex_ai_init *init = NULL;

  if ((lib = LoadLibrary(path)) == NULL) {
    fwprintf(stderr, L"error loading \"%s\"\n", path);
    return NULL;
  }

  for (int k = 0; k < count; k++) {
    char *mbsym = mbconvert(syms[k]);
    init = (hex_ai_init *) GetProcAddress(lib, mbsym);
    free(mbsym);
    if (init != NULL) {
      break;
    }

    fwprintf(stderr, L"couldn't bind \"%s\"\n", syms[k]);
  }

  if (init == NULL) {
    fwprintf(stderr, L"error binding all given symbols\n");
    FreeLibrary(lib);
  }

  return init;
}

int wmain(int argc, wchar_t *argv[])
{
  if (argc < 4) {
    fwprintf(stderr, TEXT(MSG_USAGE), argv[0]);
    return 1;
  }

  errno = 0;
  long size = wcstol(argv[1], NULL, 10);
  if (errno != 0 || size < INT_MIN || size > INT_MAX) {
    fwprintf(stderr, TEXT(MSG_BADSIZE));
    return 1;
  }

  hex_host_info host = {
    .size = (int) size,
    .color = (char) argv[2][0],
  };

  if (argv[2][0] != host.color
      || (host.color != CCOLOR_BLUE && host.color != CCOLOR_RED)) {
    fwprintf(stderr, TEXT(MSG_BADCOLOR), argv[2]);
    return 1;
  }

  if (host.size < 1 || host.size > INT_MAX / host.size) {
    fwprintf(stderr, TEXT(MSG_SIZERANGE), host.size);
    return 1;
  }

  // AIs expect UTF8 optv, so go ahead and do the conversion.
  host.optc = argc - 3;
  host.optv = calloc(host.optc, sizeof(char *));
  for (int k = 0; k < host.optc; k++) {
    host.optv[k] = mbconvert(argv[k + 3]);
  }

  hex_ai_info ai = { 0 };
  int err;
  if ((err = load_ai(argv[3], &host, &ai)) != 0) {
    goto cleanup;
  }

  err = run_monitor((int) (size * size), &ai);

 cleanup:
  for (int k = 0; k < argc - 3; k++) {
    if (host.optv[k] != NULL) {
      free(host.optv[k]);
    }
  }
  free(host.optv);

  return err;
}
