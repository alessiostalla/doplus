## Doplus ##

doplus (written DO+) is an iteration macro for Common Lisp. I wrote it because:

  * I want to code in Lisp, not in Pascal (LOOP, I'm looking at you!)
  * Code-walkers are evil and simple iteration should not require one (iterate, WTF?).

Doplus was originally one of my [tapulli](http://code.google.com/p/tapulli). It is quite stable now, so I promoted it to a stand-alone project.

## Overview and main features ##

doplus basically can be thought as a high-level DSL over CL:DO `[*]`. In spirit it's similar to iterate, but having implementation simplicity as a core goal, the syntax is necessarily a bit different to accomodate the lack of a code walker that can freely move pieces of code around. Also, doplus generally requires the user to be a bit more explicit than both LOOP and iterate.

doplus, like iterate, is meant to be extensible by simply writing macros. By contrast, LOOP is not portably extensible, and on those implementations where it is, it's not trivial to extend it. Most macros that extend doplus can be written combining built-in operators and producing code in doplus syntax, just like a user might have written it. This makes many possible doplus extensions very readable.

doplus is fully understood by SLIME, since it uses regular macros and defines dummy top-level macros for its body-local macrolets, to make symbol completion available for them as well.

doplus is, to my knowledge, the only advanced iteration construct in Lisp to support atomic initialization and stepping of iteration variables. Refer to the manual section [Initialization and stepping](manual#Initialization_and_stepping.md) for more information.

`[*]` doplus used to be actually written on top of `cl:do*`, but has been rewritten using lower-level constructs to have more control on the various steps of iteration.

## Manual ##

The manual is on a [separate page](manual.md).

## Licensing ##

doplus is distributed under the GPLv3 license; if you'd like a different license arrangement, please contact me. I'm very open about providing friendlier licenses to individuals and companies that I trust, but by default I won't allow complete strangers to profit from my code without giving anything back.