// Hexmon private declarations
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

#ifdef _WIN32

#define UNICODE
#define WIN32_LEAN_AND_MEAN
#include <windows.h>

typedef LPTSTR nstring;
typedef LPCTSTR cnstring;
typedef TCHAR nchar;

#define NTEXT(s) TEXT(s)

#else

typedef char *nstring;
typedef const char *cnstring;
typedef char nchar;

#define NTEXT(s) s

#endif

#include <hex/hexmon.h>
#include <stdio.h>


/*
 * ===============
 * Common messages
 * ===============
 */

#define CCOLOR_BLUE 'b'
#define CCOLOR_RED 'r'

#define MSG_USAGE "usage: %s size color path [opts...]\n"
#define MSG_BADSIZE "size argument must be an integer\n"
#define MSG_BADCOLOR "color argument \"%s\" should be \"red\" or \"blue\"\n"
#define MSG_SIZERANGE "size argument %d out of range\n"


/*
 * ====================
 * Defines for parser.c
 * ====================
 */

// Parser object
typedef struct parser_s parser;

// Parser events
typedef enum parse_event_e {
  PARSE_EOF,
  PARSE_ERROR,
  PARSE_CONTINUE,
  PARSE_CMD_MOVE,
  PARSE_CMD_QUIT,
  PARSE_MAX
} parse_event;

// Initializes the parse table. Must be called before other parser function.
void parse_table_init(void);

// Creates a new command parser for boards of the given data size.
parser *make_parser(int capacity);

// Destroys the given parser, freeing all memory.
void destroy_parser(parser *p);

// Sends the character c to the parser p. Returns a parse event.
parse_event parser_receive(parser *p, int c);

// Returns the current board data for the parser. Only valid if the last parse
// event was PARSE_CMD_MOVE or PARSE_CMD_QUIT.
char *parser_data(parser *p);


/*
 * ====================
 * Defines for hexmon.c
 * ====================
 */

// Runs the monitor loop.
int run_monitor(int capacity, hex_ai_info *ai);

// Loads an AI from the given path, populating the ai struct.
int load_ai(cnstring path, hex_host_info *host, hex_ai_info *ai);


/*
 * =====================
 * Defines for platforms
 * =====================
 */

// Scans data from the given input stream. Returns the same set of parse events
// as parser_receive, but handles both errors and continues. Note that this
// function is defined in posix.c and windows.c, not in parser.c.
parse_event parser_scan(parser *p, FILE *input);

// Announces the given move over the given channel.
void announce_move(FILE *channel, int row, int col);

// Tries to bind an init symbol from the dynamic library at the given path.
hex_ai_init *bind_init_symbol(cnstring path, cnstring *symname, int symcount);

// Derives the AI name from the dynamic library path.
nstring get_ai_name(cnstring path);

// Concatenates two native strings.
nstring nconcat(cnstring left, cnstring right);
