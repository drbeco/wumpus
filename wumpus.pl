%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%    Hunt The Wumpus - World Simulator                                          %
%    Copyright (C) 2012 - 2016  Ruben Carlo Benante <rcb at beco dot cc>        %
%                                                                               %
%    This program is free software; you can redistribute it and/or modify       %
%    it under the terms of the GNU General Public License as published by       %
%    the Free Software Foundation; either version 2 of the License, or          %
%    (at your option) any later version.                                        %
%                                                                               %
%    This program is distributed in the hope that it will be useful,            %
%    but WITHOUT ANY WARRANTY; without even the implied warranty of             %
%    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              %
%    GNU General Public License for more details.                               %
%                                                                               %
%    You should have received a copy of the GNU General Public License along    %
%    with this program; if not, write to the Free Software Foundation, Inc.,    %
%    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Hunt The Wumpus - World Simulator
%
%   Edited, Compiled, Modified by:
%   Author: 
%     - Ruben Carlo Benante (rcb@beco.cc)
%   Copyright: 2012 - 2016
%   License: GNU GPL Version 2.0
%
%   Based on:
%     - Original by Gregory Yob (1972)
%     - Larry Holder (accessed version Oct/2005)
%     - Walter Nauber 09/02/2001
%     - An Anonymous version of Hunt The Wumpus with menus (aeric? 2012?)
%
%
%   Special thanks to:
%     - Larry Holder (holder@cse.uta.edu) (version 1.0 and version 2.3)
%     - Walter Nauber (walter.nauber@tu-dresden.de) (swi-prolog version)
%
% A Prolog implementation of the Wumpus world described in Russell and
% Norvig's "Artificial Intelligence: A Modern Approach", Section 6.2.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% original file name: wumpus2.pl
%
% Wumpus World Simulator v2.3
%
% Written by Larry Holder (holder@cse.uta.edu)
%
% A few enhancements have been added:
%   - the wumpus can move according to simple rules (e.g., move towards the
%     gold location)
%   - the percept includes a binary image of the square the agent is
%     facing containing bitmaps of a wumpus, gold and\or pit
%   - the percept includes a natural language "hint"
%   - random wumpus world generator
%
% See comments on the following interface procedures:
%
%   evaluate_agent(Trials,Score,Time)
%   evaluate_agent2(Trials,Score,Time)
%   initialize(World,Percept)
%   restart(Percept)

:- load_files([
  library(random),   % Quintus Prologs random library
  math,              % My own math library (couldnt load Quintus)
  utils,             % Basic utilities
  image,             % Image percept generation
  move_wumpus,       % Wumpus movement
  nl_hint            % Natural language hint percept generation
  ]).

:- dynamic([
  ww_initial_state/1,
  wumpus_world_extent/1,
  wumpus_location/2,
  wumpus_health/1,
  wumpus_orientation/1,
  wumpus_last_action/1,
  wumpus_movement_rule/1,
  gold/2,
  pit/2,
  agent_location/2,
  agent_orientation/1,
  agent_in_cave/1,
  agent_health/1,
  agent_gold/1,
  agent_arrows/1,
  agent_score/1
  ]).


gold_probability(0.10).  % Probability that a location has gold
pit_probability(0.20).   % Probability that a non-(1,1) location has a pit
max_agent_actions(64).   % Maximum actions per trial allowed by agent
max_agent_tries(10).     % Maximum agent tries (climb or die) per world


% evaluate_agent(Trials,Score,Time): Performs Trials trials, where each
%   trial involves generating a random wumpus world, initializing the
%   agent, running the agent until it dies or leaves the cave, and then
%   recording the score and time spent running the agent.  The total
%   score and time are returned in Score and Time (millisecs).
%
%   This procedure requires the external definition of two procedures:
%
%     init_agent: Called after new world is initialized.  Should perform
%                 any needed agent initialization.
%
%     run_agent(Percept,Action): Given the current Percept, this procedure
%                 should return an appropriate Action, which is then
%                 executed.

evaluate_agent(Trials,Score,Time) :-
  run_agent_trials(Trials,1,Score,Time).


% run_agent_trials(Trials,NextTrial,Score,Time): Runs trials from NextTrial
%   to Trial and returns the total Score and Time (millisecs) spent inside
%   calls to init_agent and run_agent.

run_agent_trials(Trials,NextTrial,0,0) :-
  NextTrial > Trials.

run_agent_trials(Trials,NextTrial,Score,Time) :-
  NextTrial =< Trials,
  format("Trial ~d~n",[NextTrial]),
  initialize(random,Percept),
  statistics(runtime,[T1|_]),
  init_agent,                         % needs to be defined externally
  statistics(runtime,[T2|_]),
  run_agent_trial(1,Percept,Time1),
  agent_score(Score1),
  NextTrial1 is NextTrial + 1,
  run_agent_trials(Trials,NextTrial1,Score2,Time2),
  Score is Score1 + Score2,
  Time is Time1 + Time2 + (T2 - T1).


% run_agent_trial(NumActions,Percept,Time):  Continues to ask for and
%   execute actions from run_agent(Percept,Action) until either the
%   agent dies, leaves the cave or executes the maximum M actions as
%   defined by max_agent_actions(M).  In any case, the total time
%   spent during calls to run_agent is returned in Time (millisecs).

run_agent_trial(_,_,0) :-             % trial over when agent dies or
  ( agent_health(dead) ;              %   leaves cave
    agent_in_cave(no) ),
  !.

run_agent_trial(NumActions,_,0) :-    % agent allowed only N actions as
  max_agent_actions(N),               %   defined by max_agent_actions(N)
  NumActions > N,
  !.

run_agent_trial(NumActions,Percept,Time) :-
  statistics(runtime,[T1|_]),
  run_agent(Percept,Action),          % needs to be defined externally
  statistics(runtime,[T2|_]),
  execute(Action,Percept1),
  NumActions1 is NumActions + 1,
  run_agent_trial(NumActions1,Percept1,Time1),
  Time is Time1 + (T2 - T1).


% evaluate_agent2(Trials,Score,Time): Performs Trials trials, where each
%   trial involves calling the externally-defined procedure
%   navigate(Actions,Score1,Time1) described below and returning the
%   accumulated Score and Time (in millisecs) over all trials.
%
%   This procedure requires the external definition of the procedure:
%
%     navigate(Actions,Score,Time): Initializes a wumpus world with a
%       call to initialize(World,Percept) and then makes at most
%       max_agent_tries attempts to solve the generated world, calling
%       restart(Percept) whenever the agent dies or exceeds the maximum
%       number of actions per try as defined by max_agent_actions.

evaluate_agent2(Trials,Score,Time) :-
  run_agent_trials2(Trials,1,Score,Time).


% run_agent_trials2(Trials,NextTrial,Score,Time): Runs trials from NextTrial
%   to Trial and returns the total Score and Time (millisecs) spent inside
%   calls to init_agent and run_agent.

run_agent_trials2(Trials,NextTrial,0,0) :-
  NextTrial > Trials.

run_agent_trials2(Trials,NextTrial,Score,Time) :-
  NextTrial =< Trials,
  format("Trial ~d~n",[NextTrial]),
  navigate(Actions,Score1,Time1),
  format("  Actions = ~w~n",[Actions]),
  NextTrial1 is NextTrial + 1,
  run_agent_trials2(Trials,NextTrial1,Score2,Time2),
  Score is Score1 + Score2,
  Time is Time1 + Time2.


% initialize(World,Percept): initializes the Wumpus world and our fearless
%   agent according to the given World and returns the Percept from square
%   1,1.  World can be either 'fig62' for Figure 6.2 of Russell and Norvig,
%   or 'random' to generate a random world.

initialize(World,[Stench,Breeze,Glitter,no,no,NLHint,Image]) :-
  initialize_world(World),
  initialize_agent,
  stench(Stench),
  breeze(Breeze),
  glitter(Glitter),
  nl_hint(NLHint),
  generate_image(Image),
  display_action(initialize,NLHint).


% restart(Percept): Restarts the current world from scratch and returns
%   the initial Percept.

restart([Stench,Breeze,Glitter,no,no,NLHint,Image]) :-
  ww_retractall,
  ww_initial_state(L),
  assert_list(L),
  initialize_agent,
  stench(Stench),
  breeze(Breeze),
  glitter(Glitter),
  nl_hint(NLHint),
  generate_image(Image),
  display_action(restart,NLHint).


% initialize_world(World): Initializes the Wumpus world.  World is either
%   fig62, which generates the wumpus world in Figure 6.2 of [Russell &
%   Norvig], or World=random, which generates a random world according to
%   the following guidelines:
%
%   Size: The size of the wumpus world is fixed at 4x4, but can be set
%         arbitrarily using different values for wumpus_world_extent(E).
%
%   Wumpus Location: The initial wumpus location is chosen at random
%                    anywhere in the cave except location (1,1).
%
%   Wumpus Movement: A wumpus-movement rule is chosen at random and
%                    is fixed until a new world is generated.
%
%   Pit Location: Each square has a pit with probability P set by
%                 pit_probability(P), except location (1,1), which
%                 will never have a pit.
%
%   Gold Location: Each square has gold with probability P set by
%                  gold_probability(P).  At least one square will have
%                  gold; no more than one gold piece per square.
%
% wumpus_world_extent(E): defines world to be E by E
% wumpus_location(X,Y): the Wumpus is in square X,Y
% wumpus_orientation(A): one of {0,90,180,270), initially 0.
% wumpus_health(H): H is 'dead' or 'alive'
% wumpus_last_action(A): one of {nil,goforward,turnleft,turnright},
%                        initially nil.
% wumpus_movement_rule(R): R is a randomly-selected rule from
%                          wumpus_movement_rules(Rules) 
% gold(X,Y): there is gold in square X,Y
% pit(X,Y): there is a pit in square X,Y

initialize_world(fig62) :-
  ww_retractall,
  retractall(ww_initial_state(_)),
  assert(ww_initial_state([])),
  addto_ww_init_state(wumpus_world_extent(4)),
  addto_ww_init_state(wumpus_location(1,3)),
  addto_ww_init_state(wumpus_orientation(0)),
  addto_ww_init_state(wumpus_health(alive)),
  addto_ww_init_state(wumpus_last_action(nil)),
  wumpus_movement_rules(Rules),
  random_member(Rule,Rules),
  addto_ww_init_state(wumpus_movement_rule(Rule)),
  addto_ww_init_state(gold(2,3)),
  addto_ww_init_state(pit(3,1)),
  addto_ww_init_state(pit(3,3)),
  addto_ww_init_state(pit(4,4)),
  ww_initial_state(L),
  assert_list(L).

initialize_world(random) :-
  ww_retractall,
  retractall(ww_initial_state(_)),
  assert(ww_initial_state([])),
  addto_ww_init_state(wumpus_world_extent(4)),
  all_squares(4,AllSqrs),
  gold_probability(PG),             % place gold
  place_objects(gold,PG,AllSqrs),
  at_least_one_gold(4),
  del([1,1],AllSqrs,AllSqrs1),
  pit_probability(PP),              % place pits
  place_objects(pit,PP,AllSqrs1),
  random_member([WX,WY],AllSqrs1),  % initialize wumpus
  addto_ww_init_state(wumpus_location(WX,WY)),
  addto_ww_init_state(wumpus_orientation(0)),
  addto_ww_init_state(wumpus_health(alive)),
  addto_ww_init_state(wumpus_last_action(nil)),
  wumpus_movement_rules(Rules),
  random_member(Rule,Rules),
  addto_ww_init_state(wumpus_movement_rule(Rule)),
  ww_initial_state(L),
  assert_list(L).


% initialize_agent: agent is initially alive, destitute (except for one
%   arrow), in grid 1,1 and facing to the right (0 degrees).

initialize_agent :-
  retractall(agent_location(_,_)),
  retractall(agent_orientation(_)),
  retractall(agent_in_cave(_)),
  retractall(agent_health(_)),
  retractall(agent_gold(_)),
  retractall(agent_arrows(_)),
  retractall(agent_score(_)),
  assert(agent_location(1,1)),
  assert(agent_orientation(0)),
  assert(agent_in_cave(yes)),
  assert(agent_health(alive)),
  assert(agent_gold(0)),
  assert(agent_arrows(1)),
  assert(agent_score(0)).


% ww_retractall: Retract all wumpus world information, except about the
%   agent.

ww_retractall :-
  retractall(wumpus_world_extent(_)),
  retractall(wumpus_location(_,_)),
  retractall(wumpus_orientation(_)),
  retractall(wumpus_health(_)),
  retractall(wumpus_last_action(_)),
  retractall(wumpus_movement_rule(_)),
  retractall(gold(_,_)),
  retractall(pit(_,_)).


% addto_ww_init_state(Fact): Adds Fact to the list L stored in
%   ww_initial_state(L).

addto_ww_init_state(Fact) :-
  retract(ww_initial_state(L)),
  assert(ww_initial_state([Fact|L])).


% assert_list(L): Assert all facts on list L.

assert_list([]).

assert_list([Fact|Facts]) :-
  assert(Fact),
  assert_list(Facts).


% all_squares(Extent,AllSqrs): AllSqrs is the list of all possible
%   squares [X,Y] in a wumpus world of size Extent by Extent.

all_squares(Extent,AllSqrs) :-
  all_squares_1(Extent,1,1,AllSqrs).

all_squares_1(Extent,Extent,Extent,[[Extent,Extent]]).

all_squares_1(Extent,Row,Extent,[[Row,Extent]|RestSqrs]) :-
  Row < Extent,
  Row1 is Row + 1,
  all_squares_1(Extent,Row1,1,RestSqrs).

all_squares_1(Extent,Row,Col,[[Row,Col]|RestSqrs]) :-
  Col < Extent,
  Col1 is Col + 1,
  all_squares_1(Extent,Row,Col1,RestSqrs).


% place_objects(Object,P,Squares): For each square in Squares, place
%   Object at square with probability P.

place_objects(_,_,[]).

place_objects(Object,P,[Square|Squares]) :-
  maybe(P),   % succeeds with probability P
  !,
  Fact =.. [Object|Square],
  addto_ww_init_state(Fact),
  place_objects(Object,P,Squares).

place_objects(Object,P,[_|Squares]) :-
  place_objects(Object,P,Squares).


% at_least_one_gold(Extent): Ensures that at least on gold piece is
%   somewhere in the wumpus world.

at_least_one_gold(_) :-
  ww_initial_state(L),
  member(gold(_,_),L),
  !.

at_least_one_gold(E) :-
  E1 is E + 1,
  random(1,E1,X),
  random(1,E1,Y),
  addto_ww_init_state(gold(X,Y)).


%------------------------------------------------------------------------
% execute(Action,Percept): executes Action and returns Percept
%
%   Action is one of:
%     goforward: move one square along current orientation if possible
%     turnleft:  turn left 90 degrees
%     turnright: turn right 90 degrees
%     grab:      pickup gold if in square
%     shoot:     shoot an arrow along orientation, killing wumpus if
%                in that direction
%     climb:     if in square 1,1, leaves the cave and adds 1000 points
%                for each piece of gold
%
%   Percept = [Stench,Breeze,Glitter,Bump,Scream,NLHint,Image]
%             The first five are either 'yes' or 'no'.  NLHint is a list
%             of ascii codes representing an English sentence giving some
%             information about the wumpus world.  Image is a 25x25 binary
%             image of the square or wall the agent is facing containing
%             bitmaps of the wumpus, pit and\or gold.

execute(_,[no,no,no,no,no,[],[]]) :-
  agent_health(dead), !,         % agent must be alive to execute actions
  format("You are dead!~n",[]).

execute(_,[no,no,no,no,no,[],[]]) :-
  agent_in_cave(no), !,         % agent must be in the cave
  format("You have left the cave.~n",[]).

execute(goforward,[Stench,Breeze,Glitter,Bump,no,NLHint,Image]) :-
  decrement_score,
  goforward(Bump),        % update location and check for bump
  move_wumpus,
  update_agent_health,    % check for wumpus or pit
  stench(Stench),         % update rest of percept
  breeze(Breeze),
  glitter(Glitter),
  nl_hint(NLHint),
  generate_image(Image),
  display_action(goforward,NLHint).

execute(turnleft,[Stench,Breeze,Glitter,no,no,NLHint,Image]) :-
  decrement_score,
  agent_orientation(Angle),
  NewAngle is (Angle + 90) mod 360,
  retract(agent_orientation(Angle)),
  assert(agent_orientation(NewAngle)),
  move_wumpus,
  stench(Stench),
  breeze(Breeze),
  glitter(Glitter),
  nl_hint(NLHint),
  generate_image(Image),
  display_action(turnleft,NLHint).

execute(turnright,[Stench,Breeze,Glitter,no,no,NLHint,Image]) :-
  decrement_score,
  agent_orientation(Angle),
  NewAngle is (Angle + 270) mod 360,
  retract(agent_orientation(Angle)),
  assert(agent_orientation(NewAngle)),
  move_wumpus,
  stench(Stench),
  breeze(Breeze),
  glitter(Glitter),
  nl_hint(NLHint),
  generate_image(Image),
  display_action(turnright,NLHint).

execute(grab,[Stench,Breeze,no,no,no,NLHint,Image]) :-
  decrement_score,
  get_the_gold,
  move_wumpus,
  stench(Stench),
  breeze(Breeze),
  nl_hint(NLHint),
  generate_image(Image),
  display_action(grab,NLHint).

execute(shoot,[Stench,Breeze,Glitter,no,Scream,NLHint,Image]) :-
  decrement_score,
  shoot_arrow(Scream),
  move_wumpus,  % wumpus will move only if alive
  stench(Stench),
  breeze(Breeze),
  glitter(Glitter),
  nl_hint(NLHint),
  generate_image(Image),
  display_action(shoot,NLHint).

execute(climb,[no,no,no,no,no,[],[]]) :-  % climb works, no wumpus movement
  agent_location(1,1), !,
  decrement_score,
  agent_gold(G),
  retract(agent_score(S)),
  S1 is (S + (1000 * G)),
  assert(agent_score(S1)),
  retract(agent_in_cave(yes)),
  assert(agent_in_cave(no)),
  display_action(climb,[]),
  format("I am outta here.~n",[]).

execute(climb,[Stench,Breeze,Glitter,no,no,NLHint,Image]) :-
  decrement_score,
  move_wumpus,
  stench(Stench),
  breeze(Breeze),
  glitter(Glitter),
  nl_hint(NLHint),
  generate_image(Image),
  display_action(climb,NLHint),
  format("You cannot leave the cave from here.~n",[]).


% decrement_score: subtracts one from agent_score for each move

decrement_score :-
  retract(agent_score(S)),
  S1 is S - 1,
  assert(agent_score(S1)).


% stench(Stench): Stench = yes if wumpus (dead or alive) is in a square
%   directly up, down, left, or right of the current agent location.

stench(yes) :-
  agent_location(X,Y),
  X1 is X + 1,
  X0 is X - 1,
  Y1 is Y + 1,
  Y0 is Y - 1,
  ( wumpus_location(X1,Y) ;
    wumpus_location(X0,Y) ;
    wumpus_location(X,Y1) ;
    wumpus_location(X,Y0) ;
    wumpus_location(X,Y) ),
  !.

stench(no).


% breeze(Breeze): Breeze = yes if a pit is in a square directly up, down,
%   left, or right of the current agent location.

breeze(yes) :-
  agent_location(X,Y),
  X1 is X + 1,
  X0 is X - 1,
  Y1 is Y + 1,
  Y0 is Y - 1,
  ( pit(X1,Y) ;
    pit(X0,Y) ;
    pit(X,Y1) ;
    pit(X,Y0) ;
    pit(X,Y)  ),
  !.

breeze(no).


% glitter(Glitter): Glitter = yes if there is gold in the current agent
%   location.

glitter(yes) :-
  agent_location(X,Y),
  gold(X,Y),
  !.

glitter(no).


% kill_wumpus: pretty obvious

kill_wumpus :-
  retract(wumpus_health(alive)),
  assert(wumpus_health(dead)).


% goforward(Bump): Attempts to move agent forward one unit along
%   its current orientation.

goforward(no) :-
  agent_orientation(Angle),
  agent_location(X,Y),
  new_location(X,Y,Angle,X1,Y1),
  wumpus_world_extent(E),         % check if agent off world
  X1 > 0,
  X1 =< E,
  Y1 > 0,
  Y1 =< E,
  !,
  retract(agent_location(X,Y)),   % update location
  assert(agent_location(X1,Y1)).

goforward(yes).     % Ran into wall, Bump = yes


% new_location(X,Y,Orientation,X1,Y1): returns new coordinates X1,Y1
%   after moving from X,Y along Orientation: 0, 90, 180, 270 degrees.

new_location(X,Y,0,X1,Y) :-
  X1 is X + 1.

new_location(X,Y,90,X,Y1) :-
  Y1 is Y + 1.

new_location(X,Y,180,X1,Y) :-
  X1 is X - 1.

new_location(X,Y,270,X,Y1) :-
  Y1 is Y - 1.


% update_agent_health: kills agent if in a room with a live wumpus or a
%   pit.

update_agent_health :-
  agent_health(alive),
  agent_location(X,Y),
  wumpus_health(alive),
  wumpus_location(X,Y),
  !,
  retract(agent_health(alive)),
  assert(agent_health(dead)),
  retract(agent_score(S)),
  S1 is S - 10000,
  assert(agent_score(S1)),
  format("You are Wumpus food!~n",[]).

update_agent_health :-
  agent_health(alive),
  agent_location(X,Y),
  pit(X,Y),
  !,
  retract(agent_health(alive)),
  assert(agent_health(dead)),
  retract(agent_score(S)),
  S1 is S - 10000,
  assert(agent_score(S1)),
  format("Aaaaaaaaaaaaaaaaaaa!~n",[]).

update_agent_health.


% get_the_gold: adds gold to agents loot if any gold in the square

get_the_gold :-
  agent_location(X,Y),
  gold(X,Y), !,                   % theres gold in this square!
  agent_gold(NGold),              %   add to agents loot
  NGold1 is NGold + 1,
  retract(agent_gold(NGold)),
  assert(agent_gold(NGold1)),
  format("You now have ~d piece(s) of gold!~n",NGold1),
  retract(gold(X,Y)).             %   delete gold from square

get_the_gold.


% shoot_arrow(Scream): If agent has an arrow, then shoot it in the
%   direction the agent is facing and listen for Scream.

shoot_arrow(Scream) :-
  agent_arrows(Arrows),
  Arrows > 0, !,                  % agent has an arrow and will use it!
  Arrows1 is Arrows - 1,          %   update number of arrows
  retract(agent_arrows(Arrows)),
  assert(agent_arrows(Arrows1)),
  format("You now have ~d arrow(s).~n",Arrows1),
  agent_location(X,Y),
  agent_orientation(Angle),
  propagate_arrow(X,Y,Angle,Scream).

shoot_arrow(no).


% propagate_arrow(X,Y,Angle,Scream): If wumpus is at X,Y then hear its
%   woeful scream as you vanquish the creature.  If not, then move arrow
%   one square along Angle and try again.  If arrow hits a wall, then
%   you missed.

propagate_arrow(X,Y,_,yes) :-
  wumpus_location(X,Y), !,
  kill_wumpus.

propagate_arrow(X,Y,0,Scream) :-
  X1 is X + 1,
  wumpus_world_extent(E),
  X1 =< E,
  !,
  propagate_arrow(X1,Y,0,Scream).

propagate_arrow(X,Y,90,Scream) :-
  Y1 is Y + 1,
  wumpus_world_extent(E),
  Y1 =< E,
  !,
  propagate_arrow(X,Y1,90,Scream).

propagate_arrow(X,Y,180,Scream) :-
  X1 is X - 1,
  X1 > 0,
  !,
  propagate_arrow(X1,Y,180,Scream).

propagate_arrow(X,Y,270,Scream) :-
  Y1 is Y - 1,
  Y1 > 0,
  !,
  propagate_arrow(X,Y1,270,Scream).

propagate_arrow(_,_,_,no).


% display_world: Displays everything known about the wumpus world,

display_world :-
  nl,
  wumpus_world_extent(E),
  display_rows(E,E),
  wumpus_orientation(WA),
  wumpus_health(WH),
  wumpus_last_action(WAct),
  wumpus_movement_rule(Rule),
  agent_orientation(AA),
  agent_health(AH),
  agent_arrows(N),
  agent_gold(G),
  format('wumpus_orientation(~d)~n',[WA]),
  format('wumpus_health(~w)~n',[WH]),
  format('wumpus_last_action(~w)~n',[WAct]),
  format('wumpus_movement_rule(~w)~n',[Rule]),
  format('agent_orientation(~d)~n',[AA]),
  format('agent_health(~w)~n',[AH]),
  format('agent_arrows(~d)~n',[N]),
  format('agent_gold(~d)~n',[G]).


display_rows(0,E) :-
  !,
  display_dashes(E).

display_rows(Row,E) :-
  display_dashes(E),
  display_row(Row,E),
  Row1 is Row - 1,
  display_rows(Row1,E).

display_row(Row,E) :-
  display_square(1,Row,E).

display_square(X,_,E) :-
  X > E,
  !,
  format('|~n',[]).

display_square(X,Y,E) :-
  format('| ',[]),
  display_info(X,Y),
  X1 is X + 1,
  display_square(X1,Y,E).

display_info(X,Y) :-
  display_location_fact(wumpus_location,X,Y,'W'),
  display_location_fact(agent_location,X,Y,'A'),
  display_location_fact(pit,X,Y,'P'),
  display_location_fact(gold,X,Y,'G').

display_location_fact(Functor,X,Y,Atom) :-
  Fact =.. [Functor,X,Y],
  Fact,
  !,
  format('~w ',[Atom]).

display_location_fact(_,_,_,_) :-
  format('  ',[]).

display_dashes(E) :-
  RowLen is (E * 10) + 1,
  name('-',[Dash]),
  format('~*c~n',[RowLen,Dash]).


% display_action(Action,NLHint): Updates display after Action taken and
%   new percept (including NLHint) generated.

display_action(Action,NLHint) :-
  format("~nExecuting ~w~n",[Action]),
  display_world,
  format("NL Hint = ~s~n",[NLHint]).
