# Schetwee - Strategy

How to get from nothing to a useful Scheme implementation?

Last time (schetoo), I started with C.  This time, I'm going to start
with another working Scheme.


# Phase 1

Implement this much.

  - environment

    - initial (global) environment
    - linked list of (name, type, value)

  - eval.  Eval is simply
  
        (define (eval exp env)
          (run (compile exp env)))

  - compile and run.  Cmpile is a no-op, and run is a simple
    S-expression interpreter that implements a few forms.
  
  - some primitive procedures.  Just import them from the host Scheme.

If I complete that much, I can run "core scheme" -- a full environment
for a very limited subset of the language.


# Phase 2

For phase two, implement macros.

  - syntax-object and identifier types
  
  - syntax-case
  
  - `expand` as outlined in
    [bc-syntax-case](url=http://www.cs.indiana.edu/~dyb/pubs/bc-syntax-case.pdf]bc-syntax-case),
    using the environment from Phase 1.

  - implement various system syntax, e.g., `cond`, `and`, `letrec*`,
    in terms of `syntax-case`.

If I complete that much, I can run interesting Scheme programs from a
single file.


# Phase 3

  - `read`
  
  - A REPL and the interactive stuff.

  - library system
  
If I complete that much, I will have a full Scheme implementation
running atop the host Scheme implementation.


# Phase 4

  - design a bytecode format.

  - write a VM for the bytecodes, including a memory manager.
  
  - compile to bytecode.

If I complete that much, I'll have a self-hosted Scheme.


# Phase 5

  - write a simulation of an SDRAM chip's pin interface.
  
  - reimplement the memory manager to use the SDRAM interface.
  
  - rewrite the bytecode interpreter in VHDL or myHDL.
  
  - write serial I/O in VHDL or myHDL.

If I complete that much, I'll have Scheme on an FPGA.  w00t!
  
