%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%    Hunt The Wumpus - World Simulator                                          %
%    Copyright (C) 2012 - 2016  Ruben Carlo Benante <rcb at beco dot cc>        %
%                                                                               %
%    This program is free software; you can redistribute it and/or modify       %
%    it under the terms of the GNU General Public License as published by       %
%    the Free Software Foundation; version 2 of the License.             %
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
%     - Walter Nauber (09/02/2001)
%     - An Anonymous version of Hunt The Wumpus with menus (aeric? 2012?)
%
%   Special thanks to:
%     - Larry Holder (holder@cse.uta.edu) (version 1.0 and version 2.3)
%     - Walter Nauber (walter.nauber@tu-dresden.de) (swi-prolog version)
%
% A Prolog implementation of the Wumpus world described in Russell and
% Norvig's "Artificial Intelligence: A Modern Approach", Section 6.2.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Version pit3, by Beco
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
% External:
%   init_agent.
%   restart_agent.
%   run_agent(Perception, Action).
%   world_setup(Randomness, Topology, Size, Movement, Actions, Tries, Gold, Pit, Bat)
%       Randomness: - fig62 (implies Topology=grid, Size=4, Movement=stander)
%                   - random (implies Size range [2-9] or 20) (default)
%                   - pit3 (implies Size range [3-9] or 20)
%       Topology:   - grid (default)
%                   - dodeca (aka dodecahedron original map)
%       Size:       - 4, grid, fig62
%                   - [2-9] grid, random (default 4)
%                   - [3-9] grid, pit3
%                   - 20, dodeca
%       Movement:   - stander (does not move at all) (default)
%                   - walker (moves when hears shoot)
%                   - runner (moves all the time)
%       Actions:    - 2 to 200, number of maximum agent actions allowed (default 64)
%       Tries:      - Number of trials (default 1)
%       Gold:       - Gold probability per square. When fig62 or pit3, only one gold. (default 0.1)
%       Pit:        - Pit probability per square. When fig62 or pit3, only 3 pits. (default 0.2)
%       Bat:        - yes or no. When fig62, no. (default no)
%
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

% Protect all predicates (make private), except the ones listed bellow:
:- module(wumpus,   % module name WUMPUS
    [start/0,       % start an automatic agent
    manual_setup/1, % manually configure the world
    manual_init/0,  % manually initialize the world
    go/0,           % shortcut for goforward
    goforward/0,    % manually moves the agent forward
    turnright/0,    % manually turns to right / clockwise
    turn/0,         % shortcut for turnright
    turnr/0,        % shortcut for turnright
    turnleft/0,     % manually turns to left / anti-clockwise
    turnl/0,        % shortcut for turnleft
    grab/0,         % manually grabs something (gold, arrow, ...)
    shoot/0,        % manually shoots an arrow
    climb/0]).      % manually climbs out the cave

:- dynamic([
    get_setup/1,            % get world setup (from world_setup or from default setup)
    world_extent/1,         % ww World extent size
    wumpus_location/2,      % ww Wumpus location: (X,Y) on grid; (CaveNumber,Level) on dodeca;
    wumpus_health/1,        % ww Wumpus health: alive/dead
    gold/2,                 % ww Gold positions
    pit/2,                  % ww Pit positions
    agent_location/2,       % ww (X,Y) on grid; (CaveNumber, Level) on dodeca;
    agent_orientation/1,    % ww Agent orientation: 0/East/Right, 90/North/Up, 180/West/Left, 270/South/Down
    agent_in_cave/1,        % ww Agent is inside cave: yes/no
    agent_health/1,         % ww Agent health: alive/dead
    agent_gold/1,           % ww Number of golds that the agent has (start with 0)
    agent_arrows/1,         % ww Number of arrows that the agent has (start with 1)
    agent_score/1,          % ww Game Score
    agent_num_actions/1,    % ww Number of the current agent action
    gold_probability/1,     % ww Probability that a location has gold (default 0.10)
    pit_probability/1,      % ww Probability that a non-(1,1) location has a pit (default 0.20)
    max_agent_lifes/1,      % ww Maximum agent tries (climb or die) per world (default 1)
    max_agent_actions/1,    % ww Maximum actions per trial allowed by agent (default 64)
    ww_initial_state/1      % list of ww configuration values
  ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Run by hand
%

%L=[Random, Topology, Size, Move, Actions, Tries, Gold, Pit, Bat]
manual_setup(L0) :- 
    check_setup(L0, L1), 
    retractall(get_setup(_)),
    assert(get_setup(L1)), % [random, grid, 4, stander, 64, 1, 0.1, 0.2, no])), % default definition
    !. 

manual_init :-
    initialize(P), % also sets get_setup to default in case of no manual_setup
    get_setup(L),
    format("~nWorld Setup: ~w~n", [L]),
    format("First Impression: ~w~n", [P]),
    display_world,
    !.

go :- goforward.
goforward :- manual_execute(goforward).
turn :- turnright.
turnr :- turnright.
turnright :- manual_execute(turnright).
turnl :- turnleft.
turnleft :- manual_execute(turnleft).
grab :- manual_execute(grab).
shoot :- manual_execute(shoot).
climb :- manual_execute(climb).

manual_execute(A) :-
    execute(A, P),
    format("~nAction: ~w~nPerception: ~w~n", [A, P]),
    display_world,
    agent_score(S),
	format("Score: ~d~n", S),
    !.

% evaluate_agent(Trials,Score,Time): Performs Trials trials, where each
%   trial involves generating a random wumpus world, initializing the
%   agent, running the agent until it dies or leaves the cave, and then
%   recording the score and time spent running the agent.  The total
%   score and time are returned in Score and Time (millisecs).
%
%   This procedure requires the external definition of three procedures:
%
%     init_agent: Called after new world is initialized.  Should perform
%                 any needed agent initialization.
%
%     run_agent(Percept,Action): Given the current Percept, this procedure
%                 should return an appropriate Action, which is then
%                 executed.
%
%     world_setup() % TODO - explain

start :-
	evaluate_agent(1,S), % only one trial
	format("Score: ~d~n", S).

evaluate_agent(Trials,Score) :-
  run_agent_trials(Trials,1,Score).


% run_agent_trials(Trials,NextTrial,Score,Time): Runs trials from NextTrial
%   to Trial and returns the total Score and Time (millisecs) spent inside
%   calls to init_agent and run_agent.

run_agent_trials(Trials,NextTrial,0) :-
  NextTrial > Trials,
  !.

run_agent_trials(Trials,NextTrial,Score) :-
  NextTrial =< Trials,
  format("Trial ~d~n",[NextTrial]),
  initialize(Percept),  % world and agent
  format("External init_agent...~n"),
  init_agent,           % needs to be defined externally
  display_world,
  !,
  retractall(agent_num_actions(_)),
  assert(agent_num_actions(1)),
  run_agent_action(Percept),
  agent_score(Score1),
  NextTrial1 is NextTrial + 1,
  run_agent_trials(Trials,NextTrial1,Score2),
  Score is Score1 + Score2.


% run_agent_action(NumActions,Percept,Time):  Continues to ask for and
%   execute actions from run_agent(Percept,Action) until either the
%   agent dies, leaves the cave or executes the maximum M actions as
%   defined by max_agent_actions(M).  In any case, the total time
%   spent during calls to run_agent is returned in Time (millisecs).

run_agent_action(_) :-              % trial over when agent dies or
  ( agent_health(dead) ;            %   leaves cave
    agent_in_cave(no) ),
  !.

run_agent_action(_) :-              % agent allowed only N actions as
  agent_num_actions(NumActions),    % current action
  max_agent_actions(N),             %   defined by max_agent_actions(N)
  NumActions > N,
  !.

run_agent_action(Percept) :-
  run_agent(Percept,Action),          % needs to be defined externally
  check_agent_action(Action),         % check for goforward, turnright, turnleft, shoot, grab or climb.
  format("~nExternal run_agent(~w,~w)~n", [Percept, Action]),
  execute(Action,Percept1),
  display_world,
  agent_num_actions(NumActions),             % current action
  retractall(agent_num_actions(_)),
  NumActions1 is NumActions + 1,
  assert(agent_num_actions(NumActions1)),    % new current action number
  !,
  run_agent_action(Percept1).

run_agent_action(Percept) :-
    format("External function run_agent(~w, Nop) failed miserably!~n", [Percept]),
    !, fail.

% initialize(World,Percept): initializes the Wumpus world and our fearless
%   agent according to the given World and returns the Percept from square
%   1,1.  
%   World can be:
%   fig62: for Figure 6.2 of Russell and Norvig,
%   random: to generate a random world,
%   pit3: a random world with only 3 pits.

initialize([Stench,no,no,no,no]) :-
  initialize_world,
  initialize_agent,
  stench(Stench).
  % breeze(Breeze), % no pit on [1,1], [1,2] and [2,1]
  % glitter(Glitter). % not on [1,1]


% restart: restarts the current world from scratch.  For now, since we only
%   have one world fig62, restart just reinitializes this world.

restart([Stench,no,no,no,no]) :-
    initialize_world,
    initialize_agent,
    stench(Stench).


% initialize_world: gather information
initialize_world :-
    ww_retractall, %retract wumpus, gold and pit
    assert_setup, % assert user or default setup
    get_setup(L), %[Size, Type, Move, Gold, Pit, Bat]), 
    L=[Size, _, Move, Gold, Pit, Bat],
    addto_ww_init_state(world_extent(Size)),
    addto_ww_init_state(wumpus_orientation(0)),
    addto_ww_init_state(wumpus_health(alive)),
    addto_ww_init_state(wumpus_last_action(nil)),
    addto_ww_init_state(gold_probability(Gold)),     % Probability that a location has gold
    addto_ww_init_state(pit_probability(Pit)),       % Probability that a non-(1,1) location has a pit
    addto_ww_init_state(bat_probability(Bat)),       % Probability that a location has bats
    addto_ww_init_state(max_agent_lifes(1)),         % Maximum agent lifes (climb or die) per world
    Actions is Size * Size * 4,                      % 4 actions per square average (fig62 is 2.875 moves per square)
    addto_ww_init_state(max_agent_actions(Actions)), % Maximum actions per trial allowed by agent
    addto_ww_init_state(wumpus_move(Move)),          % Wumpus move style
    initialize_world(L).
    %ww_initial_state(L),
    %assert_list(L).

% initialize_world(World): Initializes the Wumpus world in Figure 6.2 of Russell & Norvig
%
% world_extent(E): defines world to be E by E
% wumpus_location(X,Y): the Wumpus is in square X,Y
% wumpus_health(H): H is 'dead' or 'alive'
% gold(X,Y): there is gold in square X,Y
% pit(X,Y): there is a pit in square X,Y

initialize_world([_,fig62,_,_,_,_]) :-
    %get_setup(L), %[Size, Type, Move, Gold, Pit, Bat]), 
    addto_ww_init_state(wumpus_location(1,3)), % wumpus location
    addto_ww_init_state(gold(2,3)), % gold position
    addto_ww_init_state(pit(3,1)),  % pit 1 
    addto_ww_init_state(pit(3,3)),  % pit 2
    addto_ww_init_state(pit(4,4)),  % pit 3
    ww_initial_state(L),
    assert_list(L).

initialize_world([E, Type, _, PG, PP, PB]) :-
    %all_squares(grid, E, AllSqrs),% E = size extension, random, range [2, 9]
    %delete(AllSqrs, [1,1], AllSqrs1), % all squares but [1,1]
    %subtract(AllSqrs1, [[1,2],[2,1]], AllSqrs3), % all squares but [1,1],[2,1],[1,2]
    gold_squares(E, Type, GS),
    hazard_squares(E, Type, HS),
    place_it(gold, PG, GS), % place gold (not [1,1])
    %at_least_one_gold(grid, E),
    place_it(pit, PP, HS),   % AllSqrs3
    place_it(bat, PB, HS),   % place some bats not near the entrance
    place_it(wumpus_location, 1, GS), % initialize wumpus (not [1,1])
    %random_member([WX,WY], GS),  % initialize wumpus (not [1,1])
    %addto_ww_init_state(wumpus_location(WX,WY)),
    %   wumpus_movement_rules(Rules),
    %   random_member(Rule,Rules),
    %   addto_ww_init_state(wumpus_movement_rule(Rule)),
    ww_initial_state(L),
    assert_list(L).

%initialize_world([pit3,grid,E,_,_,_,_,_,_]) :-
%initialize_world([E, grid, _, PG, PP, PB]) :-
    %gold_squares(grid, GSqrs),
    %hazard_squares(grid, HSqrs),
    %
    %random_member([GX,GY], AllSqrs1),  % gold position (only one), not in [1,1]
    %addto_ww_init_state(gold(GX,GY)),
    %random_member([PX1,PY1], AllSqrs3),
    %delete(AllSqrs3, [PX1, PY1], Pit1Sqrs),
    %random_member([PX2,PY2], Pit1Sqrs),
    %delete(Pit1Sqrs, [PX2, PY2], Pit2Sqrs),
    %random_member([PX3,PY3], Pit2Sqrs),
    %addto_ww_init_state(pit(PX1,PY1)), % pit 1
    %addto_ww_init_state(pit(PX2,PY2)), % pit 2
    %addto_ww_init_state(pit(PX3,PY3)), % pit 3
    %random_member([WX,WY],AllSqrs1),   % initialize wumpus (not [1,1])
    %addto_ww_init_state(wumpus_location(WX,WY)),
    %ww_initial_state(L),
    %assert_list(L).
    


%initialize_world([pit3,dodeca,E,_,_,_,_,_,_]) :-
    %all_squares(dodeca, E, AllSqrs),
    %delete(AllSqrs, [1,1], AllSqrs1),  % all squares but [1,1]
    %subtract(AllSqrs1, [[2,2],[5,2],[6,2]], AllSqrs4), % all squares but [2,2],[5,2],[6,2]
    %random_member([GX,GY], AllSqrs1),  % gold position (only one), not in [1,1]
    %addto_ww_init_state(gold(GX,GY)),
    %random_member([PX1,PY1], AllSqrs4),
    %delete(AllSqrs4, [PX1, PY1], Pit1Sqrs),
    %random_member([PX2,PY2], Pit1Sqrs),
    %delete(Pit1Sqrs, [PX2, PY2], Pit2Sqrs),
    %random_member([PX3,PY3], Pit2Sqrs),
    %addto_ww_init_state(pit(PX1,PY1)), % pit 1
    %addto_ww_init_state(pit(PX2,PY2)), % pit 2
    %addto_ww_init_state(pit(PX3,PY3)), % pit 3
    %random_member([WX,WY],AllSqrs4),   % initialize wumpus (not [1,1],[2,2],[5,2],[6,2])
    %addto_ww_init_state(wumpus_location(WX,WY)),
    %ww_initial_state(L),
    %assert_list(L).

% No need, setup checked
%initialize_world(_) :- % default
%    writeln('Error, this world setup combination is not implemented yet! Sorry about that.'),
%    !, fail.
%    %halt(1). % error 1

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
  assert(agent_orientation(0)),
  assert(agent_in_cave(yes)),
  assert(agent_health(alive)),
  assert(agent_gold(0)),
  assert(agent_arrows(1)), % TODO: setup choose number of arrows
  assert(agent_score(0)),
  assert(agent_location(1,1)).
  %world_setup([Random, Topology, Size, Move, Actions, Tries, Gold, Pit, Bat]),
  %world_setup([_, Top, _, _, _, _, _, _, _]),
  %initialize_agent(Top).

%initialize_agent(grid) :-
%   assert(agent_location(1,1)).

%initialize_agent(dodeca) :-
%   assert(agent_location(1,0)).

gold_squares(E, grid, GS) :-
    all_squares(grid, E, All),
    delete(All, [1,1], GS).  % all squares but [1,1]

gold_squares(E, dodeca, GS) :-
    all_squares(dodeca, E, All),
    delete(All, [1,1], GS).  % all squares but [1,1]

hazard_squares(E, grid, HS) :-
    gold_squares(E, grid, GS),
    subtract(GS, [[1,2],[2,1]], HS). % all squares but [1,1],[2,1],[1,2]

hazard_squares(E, dodeca, HS) :-
    gold_squares(E, dodeca, GS), % all squares but [1,1] 
    subtract(GS, [[2,2],[5,2],[6,2]], HS). % all squares but [2,2],[5,2],[6,2]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% dodeca_map : mapping dodecahedron tunnels. 
% For each Cave from 1 to 20, there are only 3 tunnels Tn not 0
% [Cave1, [ T1, T2, T3, T4]], ..., [Cave20, ...]
% T1 = North / up    / 90
% T2 = South / down  / 270
% T3 = East  / right / 0
% T4 = West  / left  / 180

dodeca_map([[1, [6, 0, 2, 5]], [2, [3, 1, 0, 7]], [3, [0, 8, 2, 4]], [4, [0, 9, 3, 5]], [5, [4, 1, 10, 0]], [6, [0, 1, 11, 15]], [7, [12, 11, 2, 0]], [8, [3, 12, 0, 13]], [9, [4, 14, 13, 0]], [10, [14, 15, 0, 5]], [11, [16, 6, 7, 0]], [12, [8, 7, 0, 17]], [13, [0, 18, 8, 9]], [14, [9, 10, 19, 0]], [15, [20, 6, 0, 10]], [16, [17, 11, 0, 20]], [17, [18, 16, 12, 0]], [18, [13, 0, 17, 19]], [19, [18, 20, 0, 14]], [20, [19, 15, 16, 0]]]).

% all_squares(Extent,AllSqrs): AllSqrs is the list of all possible
%   squares [X,Y] in a wumpus world of 
%   grid: size Extent by Extent.
%   dodeca: 20 rooms

all_squares(dodeca, 20, [[1,1],[2,2],[5,2],[6,2],[3,3],[7,3],[4,3],[10,3],[11,3],[15,3],[8,4],[12,4],[9,4],[14,4],[16,4],[20,4],[13,5],[17,5],[19,5],[18,6]]).

all_squares(grid, Extent, AllSqrs) :-
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

place_it(_, _, []).

place_it(gold, Qt, Sq) :-
    float(Qt),
    place_objects_det(gold, 1, Sq),   % put one for sure
    place_objects_prob(gold, Qt, Sq). % and lets see how many others
    %ww_initial_state(L),
    %member(gold(_,_),L),
    %!.

%at_least_one_gold(_,_).
%place_it(gold, Qt, Sq) :-
%    float(Qt),
%    place_objects_det(gold, 1, Sq).

place_it(Ob, Qt, Sq) :-
    float(Qt),
    place_objects_prob(Ob, Qt, Sq).

place_it(Ob, Qt, Sq) :-
    integer(Qt),
    place_objects_det(Ob, Qt, Sq).

place_objects_prob(_, _, []).

place_objects_prob(Object, P, [Sq|Squares]) :-
  maybe(P),   % succeeds with probability P
  !,
  Fact =.. [Object|Sq], % Fact = pit(X,Y)
  addto_ww_init_state(Fact),
  place_objects_prob(Object, P, Squares).

place_objects_prob(Object, P, [_|Squares]) :-
  place_objects_prob(Object, P, Squares).

place_objects_det(_, _, []).

place_objects_det(_, 0, [_|_]).

place_objects_det(Obj, Qtd, [H|T]) :-
    Qtd>0,
    random_member(Sq, [H|T]),
    delete([H|T], Sq, S1),
    Fact =.. [Obj | Sq], % Fact = pit(X,Y)
    addto_ww_init_state(Fact),
    Q1 is Qtd - 1,
    place_objects_det(Obj, Q1, S1).


% at_least_one_gold(Topology, Extent): Ensures that at least on gold piece is
%   somewhere in the wumpus world.

%at_least_one_gold(_,_) :-
%  ww_initial_state(L),
%  member(gold(_,_),L),
%  !.

%at_least_one_gold(Top, Ext) :-
%    gold_squares(Top, GS)
%    all_squares(Top, Ext, AllSqrs),
%    delete(AllSqrs, [1,1], AllSqrs1), % all but [1,1]
%    random_member([GX,GY], AllSqrs1),
%    addto_ww_init_state(gold(GX,GY)).  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
  update_agent_health,    % check for wumpus, pit or max actions
  stench(Stench),         % update rest of percept
  breeze(Breeze),
  glitter(Glitter).

execute(turnleft,[Stench,Breeze,Glitter,no,no]) :-
  decrement_score,
  update_agent_health,    % check for wumpus, pit or max actions
  agent_orientation(Angle),
  NewAngle is (Angle + 90) mod 360,
  retract(agent_orientation(Angle)),
  assert(agent_orientation(NewAngle)),
  stench(Stench),
  breeze(Breeze),
  glitter(Glitter).

execute(turnright,[Stench,Breeze,Glitter,no,no]) :-
  decrement_score,
  update_agent_health,    % check for wumpus, pit or max actions
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
  get_the_gold,
  update_agent_health.    % check for wumpus, pit or max actions

execute(shoot,[Stench,Breeze,Glitter,no,Scream]) :-
  decrement_score,
  stench(Stench),
  breeze(Breeze),
  glitter(Glitter),
  shoot_arrow(Scream),
  update_agent_health.    % check for wumpus, pit or max actions

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
  format("You cannot leave the cave from here.~n",[]),
  update_agent_health.    % check for wumpus, pit or max actions

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% decrement_score: subtracts one from agent_score for each move

decrement_score :-
  retract(agent_score(S)),
  S1 is S - 1,
  assert(agent_score(S1)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Perceptions

% stench(Stench): Stench = yes if wumpus (dead or alive) is in a square
%   directly up, down, left, or right of the current agent location.

stench(yes) :-
    agent_location(X,Y),
    wumpus_location(X,Y),
    !.

stench(yes) :-
    agent_location(X,Y),
    is_adjacent(X, Y, wumpus_location),
    !.

stench(no).

% breeze(Breeze): Breeze = yes if a pit is in a square directly up, down,
%   left, or right of the current agent location.

breeze(yes) :-
  agent_location(X,Y),
  is_adjacent(X, Y, pit),
  !.

breeze(no).

% F is wumpus_location(X,Y) or pit(X,Y).
is_adjacent(X, Y, F) :-
    get_setup([_,Type|_]),
    is_adjacent_type(X, Y, F, Type).

is_adjacent_type(X, Y, F, Type) :-
    (Type == grid ; Type == fig62),
    X1 is X + 1,
    X0 is X - 1,
    Y1 is Y + 1,
    Y0 is Y - 1,
    ( call(F, X1, Y) ;
      call(F, X0, Y) ;
      call(F, X, Y1) ;
      call(F, X, Y0) ;
      call(F, X, Y) ).

is_adjacent_top(X, _, F, dodeca) :-
    dodeca_map(L),
    member([X,[C1, C2, C3, C4]], L),
    ( call(F, C1, _) ;
      call(F, C2, _) ;
      call(F, C3, _) ;
      call(F, C4, _) ).

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
  !,
  retract(agent_location(X,Y)),   % update location
  assert(agent_location(X1,Y1)).

goforward(yes).     % Ran into wall, Bump = yes


% new_location(X,Y,Orientation,X1,Y1): returns new coordinates X1,Y1
%   after moving from X,Y along Orientation: 0, 90, 180, 270 degrees.

new_location(X, Y, A, X1, Y1) :-
    get_setup([E,Type|_]),
    new_location_type(X, Y, A, X1, Y1, Type, E).

new_location_type(X, Y, 0, X1, Y, Type, E) :-
    (Type == grid ; Type == fig62),
    X1 is X + 1,
    X1 =< E.

new_location_type(X, Y, 90, X, Y1, Type, E) :-
    (Type == grid ; Type == fig62),
    Y1 is Y + 1,
    Y1 =< E.

new_location_type(X, Y, 180, X1, Y, Type, _) :-
    (Type == grid ; Type == fig62),
    X1 is X - 1,
    X1 > 0.

new_location_type(X, Y, 270, X, Y1, Type, _) :-
    (Type == grid ; Type == fig62),
    Y1 is Y - 1,
    Y1 > 0.

new_location_type(X, _, 0, X1, Y1, dodeca, _) :-
    dodeca_map(L),
    member([X,[_, _, C3, _]], L),
    C3 =\= 0, % East valid
    X1 is C3,
    all_squares(dodeca, 20, S),
    member([X1, Y1], S).

new_location_type(X, _, 90, X1, Y1, dodeca, _) :-
    dodeca_map(L),
    member([X,[C1, _, _, _]], L),
    C1 =\= 0, % North valid
    X1 is C1,
    all_squares(dodeca, 20, S),
    member([X1, Y1], S).

new_location_type(X, _, 180, X1, Y1, dodeca, _) :-
    dodeca_map(L),
    member([X,[_, _, _, C4]], L),
    C4 =\= 0, % West valid
    X1 is C4,
    all_squares(dodeca, 20, S),
    member([X1, Y1], S).

new_location_type(X, _, 270, X1, Y1, dodeca, _) :-
    dodeca_map(L),
    member([X,[_, C2, _, _]], L),
    C2 =\= 0, % South valid
    X1 is C2,
    all_squares(dodeca, 20, S),
    member([X1, Y1], S).


% update_agent_health: 
% kills agent if in a room with a live wumpus or a pit.
% TODO: agent starve if no more actions

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

update_agent_health :-
  agent_num_actions(N), % current action
  max_agent_actions(M), % max allowed actions
  N >= M,
  !,
  retract(agent_health(alive)),
  assert(agent_health(dead)),
  retract(agent_score(S)),
  S1 is S - 500,
  assert(agent_score(S1)),
  format("You've starved to death inside this faultfinding cave!~n",[]).

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

propagate_arrow(X,Y,A,S) :-
    get_setup([E,Top|_]),
    propagate_arrow_top(X, Y, A, S, Top, E).

propagate_arrow_top(X,Y,_,yes,_,_) :-
  wumpus_location(X,Y), !,
  kill_wumpus.

% To right / east
propagate_arrow_top(X,Y,0,Scream,grid,E) :-
  X1 is X + 1,
  X1 =< E,
  !,
  propagate_arrow_top(X1,Y,0,Scream,grid,E).

% To up / north
propagate_arrow_top(X,Y,90,Scream,grid,E) :-
  Y1 is Y + 1,
  Y1 =< E,
  !,
  propagate_arrow_top(X,Y1,90,Scream,grid,E).

% To left / west
propagate_arrow_top(X,Y,180,Scream,grid,_) :-
  X1 is X - 1,
  X1 > 0,
  !,
  propagate_arrow_top(X1,Y,180,Scream,grid,_).

% To down / south
propagate_arrow_top(X,Y,270,Scream,grid,_) :-
  Y1 is Y - 1,
  Y1 > 0,
  !,
  propagate_arrow_top(X,Y1,270,Scream,grid,_).

% To right / east
propagate_arrow_top(X,_,0,Scream,dodeca,_) :-
    dodeca_map(L),
    member([X,[_, _, C3, _]], L),
    C3 =\= 0, % East valid
    X1 is C3,
    all_squares(dodeca, 20, S),
    member([X1, Y1], S),
    !,
    propagate_arrow_top(X1,Y1,0,Scream,dodeca,_).

% To up / north
propagate_arrow_top(X,_,90,Scream,dodeca,_) :-
    dodeca_map(L),
    member([X,[C1, _, _, _]], L),
    C1 =\= 0, % North valid
    X1 is C1,
    all_squares(dodeca, 20, S),
    member([X1, Y1], S),
    !,
    propagate_arrow_top(X1,Y1,90,Scream,dodeca,_).

% To left / west
propagate_arrow_top(X,_,180,Scream,dodeca,_) :-
    dodeca_map(L),
    member([X,[_, _, _, C4]], L),
    C4 =\= 0, % West valid
    X1 is C4,
    all_squares(dodeca, 20, S),
    member([X1, Y1], S),
    !,
    propagate_arrow_top(X1,Y1,180,Scream,dodeca,_).

% To down / south
propagate_arrow_top(X,_,270,Scream,dodeca,_) :-
    dodeca_map(L),
    member([X,[_, C2, _, _]], L),
    C2 =\= 0, % South valid
    X1 is C2,
    all_squares(dodeca, 20, S),
    member([X1, Y1], S),
    !,
    propagate_arrow_top(X1,Y1,270,Scream,dodeca,_).

propagate_arrow_top(_,_,_,no,_,_).

% display_world: Displays everything known about the wumpus world,

display_world :-
%  nl,
    display_board,
    %  wumpus_orientation(WA),
    wumpus_health(WH),
    wumpus_location(WLX, WLY),
    %  wumpus_last_action(WAct),
    %  wumpus_movement_rule(Rule),
    agent_orientation(AA),
    agent_health(AH),
    agent_arrows(N),
    agent_gold(G),
    agent_location(X,Y),
    %  format('wumpus_orientation(~d)~n',[WA]),
    format('wumpus_health(~w)~n',[WH]),
    format('wumpus_location(~d,~d)~n',[WLX, WLY]),
    %  format('wumpus_last_action(~w)~n',[WAct]),
    %  format('wumpus_movement_rule(~w)~n',[Rule]),
    format('agent_location(~d,~d)~n',[X, Y]),
    format('agent_orientation(~d)~n',[AA]),
    format('agent_health(~w)~n',[AH]),
    format('agent_arrows(~d)~n',[N]),
    format('agent_gold(~d)~n',[G]).

display_board :-
    get_setup([E,Type|_]),
    (Type == grid ; Type == fig62),
    display_rows(E, E).

display_board.

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Dynamic facts

% addto_ww_init_state(Fact): Adds Fact to the list L stored in
%   ww_initial_state(L).

addto_ww_init_state(Fact) :-
  retract(ww_initial_state(L)),
  list_to_set([Fact|L],S), % avoid duplicates
  assert(ww_initial_state(S)).

%assert_once(Fact):-
%    \+( Fact ), !,
%    assert(Fact).
%
%assert_once(_).

% assert_list(L): Assert all facts on list L.
assert_list([]).

assert_list([Fact|Facts]) :-
  assert(Fact), % assert_once(Fact),
  assert_list(Facts).

% retract wumpus, gold and pit
ww_retractall :-
    retractall(world_extent(_)),
    retractall(wumpus_orientation(_)),
    retractall(wumpus_health(_)),
    retractall(wumpus_last_action(_)),
    retractall(wumpus_location(_,_)),
    retractall(wumpus_movement_rule(_)),
    retractall(gold(_,_)),
    retractall(pit(_,_)),
    retractall(bats(_)),
    retractall(pit_probability(_)),
    retractall(gold_probability(_)),
    retractall(max_agent_lifes(_)), 
    retractall(max_agent_actions(_)),
    retractall(wumpus_move(_)),
    retractall(ww_initial_state(_)),
    assert(ww_initial_state([])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% check for goforward, turnright, turnleft, shoot, grab or climb.
check_agent_action(A) :- nonvar(A), !, check_agent_action_which(A), !.
check_agent_action(_) :- format("Agent gave no actions!~n"), !, fail.
check_agent_action_which(goforward).
check_agent_action_which(turnright).
check_agent_action_which(turnleft).
check_agent_action_which(shoot).
check_agent_action_which(grab).
check_agent_action_which(climb).
check_agent_action_which(_) :- format("Agent gave unknow action!~n"), !, fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% check_setup : check for user definition, or use default values
%
%   world_setup(Randomness, Topology, Size, Movement, Actions, Tries, Gold, Pit, Bat)
%       Randomness: - fig62 (implies Topology=grid, Size=4, Movement=stander)
%                   - random (implies Size range [2-9] or 20) (default)
%                   - pit3 (implies Size range [3-9] or 20)
%       Topology:   - grid (default)
%                   - dodeca (aka dodecahedron original map)
%       Size:       - 4, grid, fig62
%                   - [2-9] grid, random (default 4)
%                   - [3-9] grid, pit3
%                   - 20, dodeca
%       Movement:   - stander (does not move at all) (default)
%                   - walker (moves when hears shoot)
%                   - runner (moves all the time)
%       Actions:    - 2 to 200, number of maximum agent actions allowed (default 64)
%       Tries:      - Number of trials (default 1)
%       Gold:       - Gold probability per square. When fig62 or pit3, only one gold. (default 0.1)
%       Pit:        - Pit probability per square. When fig62 or pit3, only 3 pits. (default 0.2)
%       Bat:        - yes or no. When fig62, no. (default no)
% old [random, Topology, Size, Move, Actions, Tries, Gold, Pit, Bat]=Lin,

% New configuration:
% world_setup
%    1.   Size: 2..20, with some constrictions: [2-9] grid; 20, dodeca; 4, fig62
%    2.   Type: fig62, grid or dodeca
%    3.   Move: stander, walker, runner (wumpus movement)
%    4.   Gold: Integer is deterministic number, float from 0.0<G<1.0 is probabilistic
%    5.   Pits: Idem, 0 is no pits.
%    6.   Bats: Idem, 0 is no bats.
%
%       Actions: 2..400, agent actions allowed (not in this version. Now its 4 actions per square))
%       Tries: fixed at 1 now (for future versions)


% example: world_setup([5, grid, stander, 1, 3, 1]). % size 5, 1 gold, 3 pits and 1 bat
% world_setup([4, grid, stander, 0.1, 0.2, 0.1])). % default

%get_setup([Size, Type, Move, Gold, Pit, Bat]) 
assert_setup :-
    current_predicate(world_setup, world_setup(_)),
    world_setup(Lin), % user definition 
    check_setup(Lin, Lout),
    !,
    format("User defined setup: Size=~w, Type=~w, Move=~w, Gold=~w, Pit=~w, Bat=~w~n", Lout),
    retractall(get_setup(_)),
    assert(get_setup(Lout)).

assert_setup :-
    current_predicate(get_setup, get_setup(_)), % case manual_setup asserted
    get_setup(Lout),
    !,
    format("Reusing setup: Size=~w, Type=~w, Move=~w, Gold=~w, Pit=~w, Bat=~w~n", Lout).

assert_setup :-
    Lout=[4, grid, stander, 0.1, 0.2, 0.1], % defaulf
    format("Default setup: Size=~w, Type=~w, Move=~w, Gold=~w, Pit=~w, Bat=~w~n", Lout),
    retractall(get_setup(_)),
    assert(get_setup(Lout)). % default
    %assert(get_setup([random, grid, 4, stander, 64, 1, 0.1, 0.2, no])). % old default

% correct wrong combinations
% [Size, Type, Move, Gold, Pit, Bat]) 
% fig62
check_setup([_, fig62|_], [4, fig62, stander, 1, 3, 0]). % Size 4, fig62, wumpus standing still, 1 gold, 3 pits, no bats 
    %[_, fig62, _, _, _, _]=Lin,
    %check_setup_actions(Actions),
    %check_setup_tries(Tries).

% grid
check_setup([Size, grid, Move, Gold, Pit, Bat], [S1, grid, M1, G1, P1, B1]) :-
    %[Size, grid, Move, Gold, Pit, Bat]=Lin,
    %check_setup_topology(Topology),
    check_setup_size(grid, Size, S1), 
    check_setup_move(Move, M1),
    %check_setup_actions(Actions),
    %check_setup_lifes(Lifes),
    check_setup_gold(Gold, S1, G1),
    %check_setup_pit(Pit, P1),
    %check_setup_bat(Bat, B1),
    check_setup_hazard(Pit, S1, P1), % Qtd pits, size, Qtd Pits validated
    check_setup_hazard(Bat, S1, B1).

% dodeca
check_setup([_, dodeca, Move, Gold, Pit, Bat], [S1, dodeca, M1, G1, P1, B1]) :-
    %[pit3, Topology, Size, Move, Actions, Tries, Gold, _, Bat]=Lin,
    %check_setup_topology(Topology),
    check_setup_size(dodeca, 20, S1), 
    check_setup_move(Move, M1),
    %check_setup_actions(Actions),
    %check_setup_lifes(Lifes),
    check_setup_gold(Gold, S1, G1),
    %check_setup_pit(Pit, P1),
    %check_setup_bat(Bat, B1),
    check_setup_hazard(Pit, S1, P1),
    check_setup_hazard(Bat, S1, B1). % BUG conferir


% check_setup_topology(grid). check_setup_topology(dodeca). % Topology (grid, dodecahedron)
% Map size (or extension)
check_setup_size(grid, S0, S0) :- S0>=2, S0=<9.
check_setup_size(grid, _, 4).
check_setup_size(dodeca, _, 20). 
% Types of Wumpus Movement 
check_setup_move(walker, walker).
check_setup_move(runner, runner).
check_setup_move(_, stander).

%check_setup_actions(A) :- A>=2, A=<400. % Maximum agent actions 2<=A<=400
%check_setup_lifes(1).  %check_setup_lifes(T) :- T>=1, T=<5. % Lifes per labirinth
% Gold, Pit and Bat : integer, fixed number; float, probability
check_setup_gold(G0, _, G0) :- 
    float(G0), 
    check_setup_prob(G0). % Gold Probability P

%check_setup_gold(G0, _, 0.1) :- % default probability 0.1 if out of range
%    float(G0).

check_setup_gold(G0, S1, G0) :- 
    integer(G0),
    G0>=0, % at least one gold (BUG G0>0)
    MX is S1 * S1 - 1,
    G0=<MX.

check_setup_gold(_, _, 1). % default, one piece of gold

%check_setup_pit(P0, S1, P1) :-
%    float(P0),
%    check_setup_prob(P0),  % Pit Probability P
%    P1=P0.

check_setup_hazard(H0, _, H0) :-
    float(H0),
    check_setup_prob(H0).

%check_setup_hazard(G0, _, 0.2) :- % default probability 0.2 if out of range
%    float(G0).

check_setup_hazard(H0, S1, H0) :-
    integer(H0),
    H0>=0,
    MX is S1 * S1 - 3,
    H0=<MX.

check_setup_hazard(_, 2, 1). % Default 1 hazard, size = 2x2

check_setup_hazard(_, 3, 2). % Default 2 hazards, size = 3x3

check_setup_hazard(_, _, 3). % Default 3 hazards, size >= 4x4

check_setup_prob(P) :- P>0.0, P<1.0.        % Probability 0.0<P<1.0


