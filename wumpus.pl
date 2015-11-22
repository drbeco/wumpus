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
%     - An Anonymous version of Hunt The Wumpus with menus (2012?)
%
%   Special thanks to:
%     - Larry Holder (holder@cse.uta.edu) (version 1.0 and version 2.3)
%     - Walter Nauber (walter.nauber@tu-dresden.de) (swi-prolog version)
%
% A Prolog implementation of the Wumpus world described in Russell and
% Norvig's "Artificial Intelligence: A Modern Approach", Section 6.2.

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


% initialize(Percept): initializes the Wumpus world and our fearless
%   agent according to the given World and returns the percept from square
%   1,1.  For now, the only world available is 'fig62' from Fig 6.2 of
%   Russell and Norvig, but you should assume that initialize creates
%   a random world.

initialize([Stench,Breeze,Glitter,no,no]) :-
  initialize_world(fig62),
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


% initialize_world(World): Initializes the Wumpus world in Figure 6.2 of
%                          [Russell & Norvig]
%
% wumpus_world_extent(E): defines world to be E by E
% wumpus_location(X,Y): the Wumpus is in square X,Y
% wumpus_health(H): H is 'dead' or 'alive'
% gold(X,Y): there is gold in square X,Y
% pit(X,Y): there is a pit in square X,Y

initialize_world(fig62) :-
  retractall(wumpus_world_extent(_)),
  retractall(wumpus_location(_,_)),
  retractall(wumpus_health(_)),
  retractall(gold(_,_)),
  retractall(pit(_,_)),
  assert(wumpus_world_extent(4)),
  assert(wumpus_location(1,3)),
  assert(wumpus_health(alive)),
  assert(gold(2,3)),
  assert(pit(3,1)),
  assert(pit(3,3)),
  assert(pit(4,4)).


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
