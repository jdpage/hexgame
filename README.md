Hex Game
========

An in-progress implementation of
[Hex](https://en.wikipedia.org/wiki/Hex_(board_game)) for Linux supporting
pluggable AIs. Write your own and submit a PR!


Core Components
---------------

### hextk

A Tk-based GUI program for playing Hex with zero, one, or two AI players. Uses
the *hexmon* program to actually play games.


### hexmon

A host program for Hex AIs, used as a backend for *hextk*.


### libhex

A C library / Tcl extension providing an implementation of a Hex board. This is
the implementation used by the game host, but AI implementations may also choose
to use it.


AI Implementations
------------------

Listed in alphabetical order.


### disruptor

Doesn't play especially well, as it only does one move of lookahead. At least
it's better than dummy. Written by Jonathan Page in Chicken Scheme.


### dummy

Plays particularly badly. Originally written to test *hexmon*, before there were
any other implementations. Anybody could have written it and sadly, someone did,
in C.


Architecture
------------

*hextk* is implemented as a Tcl/Tk program which, after providing an opportunity
to configure the game, launches an instance of *hexmon* as a child process, and
then communicates with it over its stdin/stdout.

The command line provided to *hexmon* is as follows:

    hexmon size color path [opts...]

where `size` is the board size, `color` is `red` or `blue`, `path` is the path
to a shared object implementing the AI, and `opts` are additional options which
will be passed to the AI init function.

To avoid symbol clashes, *hexmon* only depends on libc and ldl, It communicates
with *hextk* in a line-oriented fashion. It accepts the following commands:

 - `move boardstate` where `boardstate` is a dump of the board. The coordinates
   to play in are echoed back on stdout.
   
 - `quit boardstate` where `boardstate` is a dump of the board. No response is
   returned and the program exits.
    
See **hexmon.h** for the AI ABI.
