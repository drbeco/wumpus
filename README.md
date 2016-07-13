# Hunt The Wumpus

## The Wumpus World Simulator

Edited, Compiled, Modified by:

Author: 

- Ruben Carlo Benante (rcb@beco.cc)

Copyright: 2012 - 2016

License: GNU GPL Version 2.0

Based on:

- Original by Gregory Yob (1972)
- Larry Holder (accessed version Oct/2005)
- Walter Nauber 09/02/2001
- An Anonymous version of Hunt The Wumpus with menus (aeric, 2012?)

Special thanks to:

- Larry Holder (holder@wsu.edu) (version 1.0 and version 2.3)
- Walter Nauber (walter.nauber@tu-dresden.de) (swi-prolog version)

A very special thanks to Gregory Yob (in memoriam) and beloved wife,
who was kind enough to answer my call and share with me a picture to upload 
to his wikipedia page:

https://en.wikipedia.org/wiki/Gregory_Yob

## Agents

### Agent 001

Strategy: 

    - Lock a target and fulfill it. Targets can be 2 turns, 1 turn or 1 go forward
    - Grab gold on sight
    - Shoot as soon as it smells a Wumpus
    - If wumpus dead, stump the body (go forward)
    - If it smells and agent have no arrows, go around (safe mode)
    - If it fells breeze, go back
    - If wumpus dead or a piece of gold found, get out the cave
    - If actions reach a dangerous amount, use the last to get out
    - Nothing happening:
        + explore new squares
        + no new squares, explore random old squares

### Agent 002

Strategy:

A prefixed list of actions fine-tuned for the fixed map _Fig 62_. The list have the actions that will run in order:

    - lacoes([goforward,turnleft,goforward,goforward,turnleft,shoot,grab,turnleft,goforward,goforward,         turnright,goforward,climb]).

### Agent 003

Strategy:

Try to solve the Wumpus World without any use of memory.

    - To avoid cicles the agent have lists tunned for each situation from where it draw a random member.

### Agent 007

Strategy:

Headstrong: just go forward, stubborn as a mule, stiff-necked.

## References

* Original Wumpus version 1: http://www.atariarchives.org/bcc1/showpage.php?page=247
* Original Wumpus version 2: http://www.atariarchives.org/bcc2/showpage.php?page=244
* http://www.trs-80.org/hunt-the-wumpus/
* https://www.practicingruby.com/articles/wumpus
* A C code simulating BASIC code for the original Wumpus: https://www.daniweb.com/programming/computer-science/threads/424300/wumpus-recoded-in-c
* Larry Holder's dedicated web page: http://www.eecs.wsu.edu/~holder/courses/AI/wumpus/

