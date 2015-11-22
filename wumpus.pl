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

/******************************************************************* 
  Problem Set 3
  =============
  HUNT THE WUMPUS GAME
  Start by
  ?- start.
*******************************************************************/

:- dynamic
  pit/2,                % pit location
  wumpus_location/2,    % wumpus location
  wumpus/1,             % wumpus's health - alive/dead
  agent_location/2,     % agent location
  agent/1,              % agent's health - alive/dead
  agent_orientation/1,  % agents is facing which direction
  arrows/1.             % number of arrows left

welcome :- 
  nl,nl,
  write('Welcome to "Hunt the Wumpus" simulation game'),
  nl,
  write('============================================'),
  nl,
  write('Go forward : execute(goforward,[Stench,Breeze,Wall,no]).'),
  nl,
  write('Turn left  : execute(turnleft,[Stench,Breeze,no,no]).'),
  nl,
  write('Turn right : execute(turnright,[Stench,Breeze,no,no]).'),
  nl,
  write('Shoot arrow: execute(shoot,[Stench,Breeze,no,Scream]).'),
  nl,
  write('Check player location:    agent_location(X,Y).'),
  nl,
  write('Check player orientation: agent_orientation(O).'),
  nl,
  write('Check player health:      agent(H).'),
  nl,
  write('Check arrows left:        arrows(L).'),
  nl,
  write('Simulation: simulation([Stench,Breeze,Wall,Scream]).'),
  nl.

% Initialize Everything and Start Game
start :-
  welcome,nl,
  initialize([Stench,Breeze,no,no]),
  stench(Stench),
  breeze(Breeze),
  write('Press Enter to continue'),
  nl.

% 
start([Stench,Breeze,no,no]) :-
  welcome,nl,
  initialize([Stench,Breeze,no,no]),
  stench(Stench),
  breeze(Breeze),
  write('Press Enter to continue'),
  nl.

% initializes the Wumpus world and agent.
initialize([Stench,Breeze,no,no]) :-
  initialize_environment,
  initialize_agent.
  % stench(Stench),
  % breeze(Breeze).

% Maze is set with wumpus alive in square (5,1) 
% and pits in position (3,1), (6,2), (4,3) and (2,4)
initialize_environment :-
  retractall(wumpus_location(_,_)),
  retractall(wumpus(_)),
  retractall(pit(_,_)),
  assert(wumpus_location(5,1)),     % wumpus is in square (5,1)
  assert(wumpus(alive)),            % wumpus is alive
  assert(pit(3,1)),                 % 4 locations of pit
  assert(pit(6,2)),
  assert(pit(4,3)),
  assert(pit(2,4)).

% Agent is initially alive in square (1,1) 
% facing right (0 degree) with 1 arrow
initialize_agent :-
  retractall(agent_location(_,_)),
  retractall(agent_orientation(_)),
  retractall(agent(_)),
  retractall(arrows(_)),
  assert(agent_location(1,1)),    % start in square (1,1)
  assert(agent_orientation(0)),   % facing to the right (0 degrees)
  assert(agent(alive)),           % agent is alive
  assert(arrows(1)).              % agent has only 1 arrow

% dead agent cannot execute any actions
execute(_,[no,no,no,no]) :-
  agent(dead), !,
  format("You are dead!~n",[]).

% agent moves forward
execute(goforward,[Stench,Breeze,Wall,no]) :-
  goforward(Wall),        % update location and check if wall
  update_agent_health,    % check whether same location as wumpus or pit
  stench(Stench),         % check if smell Stench
  breeze(Breeze),         % check if feel Breeze
  agent_location(X,Y),
  format("You now in ~d,",X),format("~d .~n",Y).

% agent turns left
execute(turnleft,[Stench,Breeze,no,no]) :-
  agent_orientation(Angle),
  NewAngle is (Angle + 90) mod 360,     % turn 90 degree counter-clockwise
  retract(agent_orientation(Angle)),
  assert(agent_orientation(NewAngle)),  % update agent orientation
  stench(Stench),
  breeze(Breeze),
  write('You turned Left'),nl.

% agent turns right
execute(turnright,[Stench,Breeze,no,no]) :-
  agent_orientation(Angle),
  NewAngle is (Angle + 270) mod 360,    % turn 90 degree clockwise
  retract(agent_orientation(Angle)),
  assert(agent_orientation(NewAngle)),  % update agent orientation
  stench(Stench),
  breeze(Breeze),
  write('You turned Right'),nl.

% shoot action
execute(shoot,[Stench,Breeze,no,Scream]) :-
  stench(Stench),
  breeze(Breeze),
  shoot_arrow(Scream).  % check if wumpus screams after arrow is shot

% return Stench = yes if wumpus is next to agent location.
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
    wumpus_location(X,Y) ),!.

stench(no).

% return Breeze = yes if a pit is next to agent location.
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
    pit(X,Y) ),!.

breeze(no).

% slay the wumpus and win the game
kill_wumpus :-
  retract(wumpus(alive)),
  assert(wumpus(dead)),    % update wumpus health
  format("Grrrrrrrrr...!~n",[]),
  format("You have killed Wumpus!~n",[]).

% Try to move agent one square towards its orientation
% if agent does not move over the grid
goforward(no) :-
  agent_orientation(Angle),
  agent_location(X,Y),
  new_location(X,Y,Angle,X1,Y1),
  X1 > 0,
  X1 =< 6,
  Y1 > 0,
  Y1 =< 4,
  !,
  retract(agent_location(X,Y)),   
  assert(agent_location(X1,Y1)).  % update agent location

goforward(yes).     % Ran into wall, Wall = yes

% return new coordinates X1,Y1
% after moing from X,Y along Orientation: 0, 90, 180, 270 degrees.

new_location(X,Y,0,X1,Y) :-
  X1 is X + 1.

new_location(X,Y,90,X,Y1) :-
  Y1 is Y + 1.

new_location(X,Y,180,X1,Y) :-
  X1 is X - 1.

new_location(X,Y,270,X,Y1) :-
  Y1 is Y - 1.

% kill agent if in a room with a live wumpus or a pit.

% eaten by wumpus
update_agent_health :-
  agent_location(X,Y),
  wumpus(alive),
  wumpus_location(X,Y),
  !,
  retract(agent(alive)),
  assert(agent(dead)),     % update agent health
  format("You are Wumpus food!~n",[]).

% fall into pit
update_agent_health :-
  agent_location(X,Y),
  pit(X,Y),
  !,
  retract(agent(alive)),
  assert(agent(dead)),     % update agent health
  format("Aaaaaaaaaaaaaaaaaaa!~n",[]),
  format("You fall into a pit.~n",[]).

update_agent_health.

% If agent has an arrow, then shoot it in the direction 
% the agent is facing and listen if wumpus Screams.
shoot_arrow(Scream) :-
  arrows(Arrows),
  Arrows > 0, !,                  % agent has an arrow and will use it!
  Arrows1 is Arrows - 1,          % update number of arrows
  retract(arrows(Arrows)),
  assert(arrows(Arrows1)),
  write('You shot an arrow. '),
  format("You now have ~d arrow(s).~n",Arrows1),
  agent_location(X,Y),
  agent_orientation(Angle),
  propagate_arrow(X,Y,Angle,Scream).

shoot_arrow(no).

% If wumpus is at X,Y then wumpus is killed.
% If arrow hits a wall, then you missed.
propagate_arrow(X,Y,_,yes) :-
  wumpus_location(X,Y), !,
  kill_wumpus.

% Let the arrow travels along the direction
propagate_arrow(X,Y,0,Scream) :-
  X1 is X + 1,
  X1 =< 6,
  !,
  propagate_arrow(X1,Y,0,Scream).

propagate_arrow(X,Y,90,Scream) :-
  Y1 is Y + 1,
  Y1 =< 4,
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

% Simulation of agent's actions

simulation([Stench,Breeze,Wall,Scream]) :- 
nl,write('This is a simulation'),nl,
write('===================='),nl,

agent_location(X0,Y0),
format("You start in ~d,",X0),format("~d .~n",Y0),

execute(turnleft,[Stench,Breeze,no,no]),

execute(goforward,[Stench,Breeze,Wall,no]),

execute(goforward,[Stench,Breeze,Wall,no]),

execute(turnright,[Stench,Breeze,no,no]),

execute(goforward,[Stench,Breeze,Wall,no]),

execute(goforward,[Stench,Breeze,Wall,no]),

execute(turnleft,[Stench,Breeze,no,no]),

execute(goforward,[Stench,Breeze,Wall,no]),

execute(turnright,[Stench,Breeze,no,no]),

execute(goforward,[Stench,Breeze,Wall,no]),

execute(goforward,[Stench,Breeze,Wall,no]),

execute(turnright,[Stench,Breeze,no,no]),

execute(goforward,[Stench,Breeze,Wall,no]),

execute(goforward,[Stench,Breeze,Wall,no]),

execute(shoot,[_,_,_,_]).

% execute(shoot,[Stench,Breeze,no,Scream]).
