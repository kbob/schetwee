# Schetwee - Scheme #3

Pronounced *skee-TWEE*.  It's not exactly a lisp, but it's some kind of
speech impediment.

This is my third serious attempt to implement Scheme.  The basic
strategy here is very different from
[Schetoo](url=https://github.com/kbob/schetoo)] (Scheme #2).

Schetoo started with an interpreter written in C that built up the
runtime from scratch.  It became a reasonably complete Scheme, except
that it had no macro capabilities.  It was too hard to simultaneously
learn how Scheme libraries and macros work and implement them in a
macro-free Scheme subset.

So this time, I am writing in full Scheme, and building my system
piecemeal.  At some point, Schetwee should be able to compile itself
into a standalone system, and I can separate from the host Scheme.
The details are in [Strategy.md](Strategy.md).

And just to make it interesting, the end goal is Scheme running on an
FPGA.  That's not until the last phase, though, so I may never get
there.
