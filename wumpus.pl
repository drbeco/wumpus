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
%   Special thanks to:
%     - Larry Holder (holder@cse.uta.edu) (version 1.0 and version 2.3)
%     - Walter Nauber (walter.nauber@tu-dresden.de) (swi-prolog version)
%
% A Prolog implementation of the Wumpus world described in Russell and
% Norvig's "Artificial Intelligence: A Modern Approach", Section 6.2.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Versin pit3, by Beco
%
% World Models: fig62, random, pit3
%
% fig62:
%   gold: [2,3]
%   pits: [3,1], [3,3], [3,4]
%   wumpus: [1,3], fixed location
%
% pit3:
%   size: from 3 to 9
%   gold: only one, not in [1,1]
%   pits: exact 3, random location, not in [1,1], [2,1] and [1,2]
%   wumpus: only one, random fixed location, not in [1,1]
%
% random:
%   size: from 2 to 9
%   gold: random quantity (probability 0.1), not in [1,1]
%   pits: random quantity (probability 0.2), random location, not in [1,1], [2,1] and [1,2]
%   wumpus: only one, random fixed location, not in [1,1]
%
% TODO:
%
% walker:
%   size: from 2 to 9
%   gold: random quantity (probability 0.1), not in [1,1]
%   pits: random quantity (probability 0.2), random location, not in [1,1], [2,1] and [1,2]
%   wumpus: only one, random, move at will, not started in [1,1]
%
% New hazard: bats! Move you to a random location.
% Make every room with 3 tunnels
% Enough with the grids! Lets see the original topology
%
% 1000 points for each gold AFTER climbing alive
% 500 points for killing the Wumpus
% -500 for dying
%

:- dynamic([
  wumpus_world_extent/1,
  wumpus_location/2,
  wumpus_health/1,
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
%max_agent_tries(10).     % Maximum agent tries (climb or die) per world
max_agent_actions(64).   % Maximum actions per trial allowed by agent

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

start :-
	evaluate_agent(1,S),
	format("Score: ~d~n", S).

evaluate_agent(Trials,Score) :-
  run_agent_trials(Trials,1,Score).


% run_agent_trials(Trials,NextTrial,Score,Time): Runs trials from NextTrial
%   to Trial and returns the total Score and Time (millisecs) spent inside
%   calls to init_agent and run_agent.

run_agent_trials(Trials,NextTrial,0) :-
  NextTrial > Trials.

run_agent_trials(Trials,NextTrial,Score) :-
  NextTrial =< Trials,
  format("Trial ~d~n",[NextTrial]),
  wumpusworld(Type,_), % types: random, fig62, pit3
%   initialize(random,Percept),
%   initialize(fig62,Percept),
%   initialize(pit3,Percept),
  initialize(Type,Percept),
  format("External init_agent...~n"),
  init_agent,                         % needs to be defined externally
  display_world,
  run_agent_trial(1,Percept),
  agent_score(Score1),
  NextTrial1 is NextTrial + 1,
  run_agent_trials(Trials,NextTrial1,Score2),
  Score is Score1 + Score2.


% run_agent_trial(NumActions,Percept,Time):  Continues to ask for and
%   execute actions from run_agent(Percept,Action) until either the
%   agent dies, leaves the cave or executes the maximum M actions as
%   defined by max_agent_actions(M).  In any case, the total time
%   spent during calls to run_agent is returned in Time (millisecs).

run_agent_trial(_,_) :-             % trial over when agent dies or
  ( agent_health(dead) ;              %   leaves cave
    agent_in_cave(no) ),
  !.

run_agent_trial(NumActions,_) :-    % agent allowed only N actions as
  max_agent_actions(N),               %   defined by max_agent_actions(N)
  NumActions > N,
  !.

run_agent_trial(NumActions,Percept) :-
  run_agent(Percept,Action),          % needs to be defined externally
  format("~nExternal run_agent(~w,~w)~n", [Percept, Action]),
  execute(Action,Percept1),
  display_world,
  NumActions1 is NumActions + 1,
  run_agent_trial(NumActions1,Percept1).


% initialize(World,Percept): initializes the Wumpus world and our fearless
%   agent according to the given World and returns the Percept from square
%   1,1.  
%   World can be:
%   fig62: for Figure 6.2 of Russell and Norvig,
%   random: to generate a random world,
%   pit3: a random world with only 3 pits.

initialize(World,[Stench,Breeze,Glitter,no,no]) :-
  initialize_world(World),
  initialize_agent,
  stench(Stench),
  breeze(Breeze),
  glitter(Glitter).


% restart: restarts the current world from scratch.  For now, since we only
%   have one world fig62, restart just reinitializes this world.

restart([Stench,Breeze,Glitter,no,no]) :-
  initialize_world(fig62),
  initialize_agent,
  stench(Stench),
  breeze(Breeze),
  glitter(Glitter).


% initialize_world(World): Initializes the Wumpus world in Figure 6.2 of Russell & Norvig
%
% wumpus_world_extent(E): defines world to be E by E
% wumpus_location(X,Y): the Wumpus is in square X,Y
% wumpus_health(H): H is 'dead' or 'alive'
% gold(X,Y): there is gold in square X,Y
% pit(X,Y): there is a pit in square X,Y

initialize_world(fig62) :-
  ww_retractall, %retract wumpus, gold and pit
  %retractall(wumpus_world_extent(_)),
  %retractall(wumpus_location(_,_)),
  %retractall(wumpus_health(_)),
  %retractall(gold(_,_)),
  %retractall(pit(_,_)),
  retractall(ww_initial_state(_)),
  assert(ww_initial_state([])),
  %assert(wumpus_world_extent(4)),
  %assert(wumpus_location(1,3)),
  %assert(wumpus_health(alive)),
  wumpusworldsize(E), % size extension
  addto_ww_init_state(wumpus_world_extent(E)), % fig62 size 4
  addto_ww_init_state(wumpus_location(1,3)),
  addto_ww_init_state(wumpus_orientation(0)),
  addto_ww_init_state(wumpus_health(alive)),
  addto_ww_init_state(wumpus_last_action(nil)),
  %assert(gold(2,3)),
  addto_ww_init_state(gold(2,3)),
  %assert(pit(3,1)),
  %assert(pit(3,3)),
  %assert(pit(4,4)).
  addto_ww_init_state(pit(3,1)),
  addto_ww_init_state(pit(3,3)),
  addto_ww_init_state(pit(4,4)),
  ww_initial_state(L),
  assert_list(L).

initialize_world(pit3) :-
  ww_retractall,
  retractall(ww_initial_state(_)),
  assert(ww_initial_state([])),
  wumpusworldsize(E), % size extension, pit3, range [3, 9]
  addto_ww_init_state(wumpus_world_extent(E)),
  all_squares(E,AllSqrs),
  delete(AllSqrs, [1,1], AllSqrs1),  % all squares but [1,1]
  subtract(AllSqrs1, [[1,2],[2,1]], AllSqrs3), % all squares but [1,1],[2,1],[1,2]
  random_member([GX,GY], AllSqrs1),  % gold position (only one), not in [1,1]
  addto_ww_init_state(gold(GX,GY)), % Fact =.. [Object|Square], % Fact = pit([X,Y])
  random_member([PX1,PY1], AllSqrs3),
  delete(AllSqrs3, [PX1, PY1], Pit1Sqrs),
  random_member([PX2,PY2], Pit1Sqrs),
  delete(Pit1Sqrs, [PX2, PY2], Pit2Sqrs),
  random_member([PX3,PY3], Pit2Sqrs),
  addto_ww_init_state(pit(PX1,PY1)), % pit 1
  addto_ww_init_state(pit(PX2,PY2)), % pit 2
  addto_ww_init_state(pit(PX3,PY3)), % pit 3
  random_member([WX,WY],AllSqrs1),   % initialize wumpus (not [1,1])
  addto_ww_init_state(wumpus_location(WX,WY)),
  addto_ww_init_state(wumpus_orientation(0)),
  addto_ww_init_state(wumpus_health(alive)),
  addto_ww_init_state(wumpus_last_action(nil)),
  ww_initial_state(L),
  assert_list(L).

initialize_world(random) :-
  ww_retractall,
  retractall(ww_initial_state(_)),
  assert(ww_initial_state([])),
  wumpusworldsize(E), % size extension, random, range [2, 9]
  addto_ww_init_state(wumpus_world_extent(E)),
  all_squares(E,AllSqrs),
  gold_probability(PG),             % place gold (not [1,1])
  place_objects(gold,PG,AllSqrs1),
  at_least_one_gold(E),
  delete(AllSqrs, [1,1], AllSqrs1), % all squares but [1,1]
  subtract(AllSqrs1, [[1,2],[2,1]], AllSqrs3), % all squares but [1,1],[2,1],[1,2]
  pit_probability(PP),              % place pits
  place_objects(pit,PP,AllSqrs3),   % AllSqrs3
  random_member([WX,WY],AllSqrs1),  % initialize wumpus (not [1,1])
  addto_ww_init_state(wumpus_location(WX,WY)),
  addto_ww_init_state(wumpus_orientation(0)),
  addto_ww_init_state(wumpus_health(alive)),
  addto_ww_init_state(wumpus_last_action(nil)),
%   wumpus_movement_rules(Rules),
%   random_member(Rule,Rules),
%   addto_ww_init_state(wumpus_movement_rule(Rule)),
  ww_initial_state(L),
  assert_list(L).

% addto_ww_init_state(Fact): Adds Fact to the list L stored in
%   ww_initial_state(L).

addto_ww_init_state(Fact) :-
  retract(ww_initial_state(L)),
  assert(ww_initial_state([Fact|L])).

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
  Fact =.. [Object|Square], % Fact = pit([X,Y])
  addto_ww_init_state(Fact),
  place_objects(Object,P,Squares).

place_objects(Object,P,[_|Squares]) :-
  place_objects(Object,P,Squares).

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

% retract wumpus, gold and pit
ww_retractall :-
  retractall(wumpus_world_extent(_)),
  retractall(wumpus_location(_,_)),
  retractall(wumpus_orientation(_)),
  retractall(wumpus_health(_)),
  retractall(wumpus_last_action(_)),
  retractall(wumpus_movement_rule(_)),
  retractall(gold(_,_)),
  retractall(pit(_,_)).

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
%   Percept = [Stench,Breeze,Glitter,Bump,Scream] each having
%     a value of either 'yes' or 'no'.

execute(_,[no,no,no,no,no]) :-
  agent_health(dead), !,         % agent must be alive to execute actions
  format("You are dead!~n",[]).

execute(_,[no,no,no,no,no]) :-
  agent_in_cave(no), !,         % agent must be in the cave
  format("You have left the cave.~n",[]).

execute(goforward,[Stench,Breeze,Glitter,Bump,no]) :-
  decrement_score,
  goforward(Bump),        % update location and check for bump
  update_agent_health,    % check for wumpus or pit
  stench(Stench),         % update rest of percept
  breeze(Breeze),
  glitter(Glitter).

execute(turnleft,[Stench,Breeze,Glitter,no,no]) :-
  decrement_score,
  agent_orientation(Angle),
  NewAngle is (Angle + 90) mod 360,
  retract(agent_orientation(Angle)),
  assert(agent_orientation(NewAngle)),
  stench(Stench),
  breeze(Breeze),
  glitter(Glitter).

execute(turnright,[Stench,Breeze,Glitter,no,no]) :-
  decrement_score,
  agent_orientation(Angle),
  NewAngle is (Angle + 270) mod 360,
  retract(agent_orientation(Angle)),
  assert(agent_orientation(NewAngle)),
  stench(Stench),
  breeze(Breeze),
  glitter(Glitter).

execute(grab,[Stench,Breeze,no,no,no]) :-
  decrement_score,
  stench(Stench),
  breeze(Breeze),
  get_the_gold.

execute(shoot,[Stench,Breeze,Glitter,no,Scream]) :-
  decrement_score,
  stench(Stench),
  breeze(Breeze),
  glitter(Glitter),
  shoot_arrow(Scream).

execute(climb,[no,no,no,no,no]) :-
  agent_location(1,1), !,
  decrement_score,
  agent_gold(G),
  retract(agent_score(S)),
  S1 is (S + (1000 * G)),
  assert(agent_score(S1)),
  retract(agent_in_cave(yes)),
  assert(agent_in_cave(no)),
  format("I am outta here.~n",[]).

execute(climb,[Stench,Breeze,Glitter,no,no]) :-
  decrement_score,
  stench(Stench),
  breeze(Breeze),
  glitter(Glitter),
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
    pit(X,Y) ),
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
  assert(wumpus_health(dead)),
  retract(agent_score(S)),
  S1 is S + 500, % 500 point for killing the wumpus
  assert(agent_score(S1)).

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
  agent_location(X,Y),
  wumpus_health(alive),
  wumpus_location(X,Y),
  !,
  retract(agent_health(alive)),
  assert(agent_health(dead)),
  retract(agent_score(S)),
  S1 is S - 500,
  assert(agent_score(S1)),
  format("You are Wumpus food!~n",[]).

update_agent_health :-
  agent_location(X,Y),
  pit(X,Y),
  !,
  retract(agent_health(alive)),
  assert(agent_health(dead)),
  retract(agent_score(S)),
  S1 is S - 500,
  assert(agent_score(S1)),
  format("Aaaaaaaaaaaaaaaaaaa!~n",[]).

update_agent_health.


% get_the_gold: adds gold to agents loot if any gold in the square

get_the_gold :-
  agent_location(X,Y),
  gold(X,Y), !,                   % there's gold in this square!
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
%  nl,
  wumpus_world_extent(E),
  display_rows(E,E),
%  wumpus_orientation(WA),
  wumpus_health(WH),
%  wumpus_last_action(WAct),
%  wumpus_movement_rule(Rule),
  agent_orientation(AA),
  agent_health(AH),
  agent_arrows(N),
  agent_gold(G),
  agent_location(X,Y),
%  format('wumpus_orientation(~d)~n',[WA]),
  format('wumpus_health(~w)~n',[WH]),
%  format('wumpus_last_action(~w)~n',[WAct]),
%  format('wumpus_movement_rule(~w)~n',[Rule]),
  format('agent_location(~d,~d)~n',[X, Y]),
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

% success with probability P
maybe(P):-
    random(N),
    N<P.
maybe :- maybe(0.5).

% assert_list(L): Assert all facts on list L.
assert_list([]).

assert_list([Fact|Facts]) :-
  assert(Fact),
  assert_list(Facts).

% some sizes cannot work
wumpusworldsize(4) :-
    wumpusworld(fig62, _).

wumpusworldsize(E) :-
    wumpusworld(pit3, E),
    E>=3, E=<9.

wumpusworldsize(4) :-
    wumpusworld(pit3, _).

wumpusworldsize(E) :-
    wumpusworld(random, E),
    E>=2, E=<9. % 2 to 9, ok

wumpusworldsize(4) :-
    wumpusworld(random, _).

