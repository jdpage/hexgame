Hex Game
========

An in-progress implementation of
[Hex](https://en.wikipedia.org/wiki/Hex_(board_game)) for Linux supporting
pluggable AIs. Write your own and submit a PR!


hextk
-----

A Tk-based GUI program for playing Hex with zero, one, or two AI players. Uses
the *hexmon* program to actually play games.


hexmon
------

A host program for Hex AIs, used as a backend for *hextk*.


libhex
------

A C library / Tcl extension providing an implementation of a Hex board. This is
the implementation used by the game host, but AI implementations may also choose
to use it.


Architecture
------------

*hextk* is implemented as a Tcl/Tk program which, after providing an opportunity
to configure the game, launches an instance of *hexmon* as a child process, and
then communicates with it over its stdin/stdout.

The command line provided to *hexmon* is as follows:

    hexmon size red[:redopt] blue[:blueopt]

where `size` is the board size, `red` and `blue` are paths to shared objects
implementing the AI ABI or the string `human`, and `redopt` and `blueopt` are
options passed to the AI implementation. Options passed to `human` players are
ignored. The blue player goes first.

To avoid symbol clashes, *hexmon* only depends on libc, and forks itself before
loading the AI implementations. It communicates with *hextk* in a line-oriented
fashion. It accepts the following commands:

 - `blue row col` where `row` and `col` are integer row and column numbers. The
   command is echoed back on stdout.
   
 - `red row col` where `row` and `col` are integer row and column numbers. The
   command is echoed back on stdout.
      
 - `move color` to trigger an AI move, where `color` is `red` or `blue`. Will
   respond on stdout with either a `red` or `blue` command as described above.
    
 - `quit` to exit the program. No response is returned.
    
*hexmon* keeps track of the board state, but not turn order or move legality.
Upon launch, it does the following:

 1. Create a shared memory area big enough to hold the board state and response
    info, and fill it with zeros.

 2. For each AI provided, fork. The child process will be responsible for
    loading the AI implementation, and entering the stopped state when ready.
    
 3. Wait for all child processes to enter the stopped state.

 4. Wait for commands on stdin.
   - For `red` and `blue` commands, update the board state and respond.
   - For the `move` command, send SIGCONT to the appropriate child, wait
     for it to enter the stopped state, and then respond.
   - For the `quit` command, send SIGTERM and then SIGCONT to the child
     processes, then wait for them to exit.
     
The child processes work as follows:

 1. Load the AI implementation, and tell it to initialize itself. Once this is
    done, enter the stopped state.
    
 2. On continue, examine the shared memory, and ask the AI for its next move,
    passing the board state. Once it has provided a response, place it into the
    shared memory and enter the stopped state. Repeat until SIGTERM is received.
    
 3. On SIGTERM, tell the AI to deinitialize itself, then exit.
 

The interface to the AI is as follows. Consider the following structure:

    typedef struct {
      char *opt;
      char color;
      int size;
    } hex_host_info;

    typedef struct {
      void *data;
      void (*move_callback)(
          void *data,
          char *board,
          int *row, int *col);
      void (*destroy_callback)(
          void *data,
          char *board);
    } hex_ai_info;
    
    typedef void hex_ai_init(hex_host_info *host, hex_ai_info *ai);

The AI loader given an AI with filename e.g. `libfoo.so`, will look for a symbol
called `hex_ai_init_foo`, which it will call as per `hex_ai_init` above. The
symbol is expected to initialize the AI, and populate the `hex_ai_info`
structure. 

The optional `data` field may be used to store an opaque pointer, It may safely
be left `NULL` if unused. Its contents will be passed as the `data` argument to
the callbacks.

The optional `destroy_callback` field will be called before process exit with
the final state of the board. It may be left `NULL` if unused.

The required `move` field will be called when requesting a move. It must not be
`NULL`. It is passed the board data and side length of the board, and is
expected to populate `row` and `col` with the requested move.

The board data consists of a null-terminated string of `size*size` characters
representing the board state in column-major order. Empty spaces are represented
by the space character, red spaces by the `r` character, and blue spaces by the
`b` character.

The `hex_host_info` structure contains information about the environment, such
as the option string passed on the command line and the color the AI is playing
as. The struct is available for the life of program, so the AI may store the
pointer if it wishes.
