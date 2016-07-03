%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%    Hunt The Wumpus - World Simulator                                          %
%    Copyright (C) 2012 - 2016  Ruben Carlo Benante <rcb at beco dot cc>        %
%                                                                               %
%    This program is free software; you can redistribute it and/or modify       %
%    it under the terms of the GNU General Public License as published by       %
%    the Free Software Foundation; version 2 of the License.                    %
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
% Hunt The Wumpus - World Simulator - Version 5.0, by Dr. Beco
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
%
% Agent actions:
%
% 1. goforward,    % moves the agent forward
% 2. turnright,    % turns to right / clockwise
% 3. turnleft,     % turns to left / anti-clockwise
% 4. grab,         % grabs something (gold, arrow, ...)
% 5. shoot,        % shoots an arrow
% 6. sit,          % sit and do nothing
% 7. climb,        % climbs out the cave from square [1,1]
%
% World Setup:
%
% world_setup([Size, Type, Move, Gold, Pit, Bat, [RandS, RandA]]).
% 1. Size: 2..20, with some constrictions: [2-9] grid; 20, dodeca; 4, fig62
% 2. Type: fig62, grid or dodeca
% 3. Move: walker, runner, wanderer, spinner, hoarder, spelunker, stander, trapper and bulldozer
% 4. Gold: Integer is deterministic number, float from 0.0<G<1.0 is probabilistic
% 5. Pits: Idem, 0 is no pits.
% 6. Bats: Idem, 0 is no bats.
% Optional, advanced setup:
%   7. RandS: yes/no, random agent start position
%   8. RandA: yes/no, random agent start angle (not implemented)
%
% fig62:
%   gold: [2,3]
%   pits: [3,1], [3,3], [3,4]
%   wumpus: [1,3], fixed location (stander)
%
% grid:
%   size: from 2 to 9; 0 = random size
%   gold: free to choose from 1 to NxN-1, not in [1,1]
%   pits: any number, random location, not in [1,1], [2,1] and [1,2]
%   wumpus: only one, random location, not in [1,1]
%
% dodeca:
%   size: 20
%   gold: random quantity not in [1,1]
%   pits: random quantity not in [1,1], [2,1] and [1,2]
%   wumpus: only one, not in [1,1]
%
% External:
%   init_agent.
%   run_agent(Perception, Action).
%   world_setup([Size, Type, Move, Golds, Pits, Bats, [RandS, RandA]]).
%   Types of Wumpus Movement
%       walker    : original: moves when it hears a shoot, or you enter its cave
%       runner    : go forward and turn left or right on bumps, maybe on pits
%       wanderer  : arbitrarily choses an action from [sit,turnleft,turnright,goforward]
%       spinner   : goforward, turnleft, repeat.
%       hoarder   : go to one of the golds and sit
%       spelunker : go to a pit and sit
%       stander   : do not move (default)
%       trapper   : goes hunting agent as soon as it leaves [1,1]; goes home otherwise
%       bulldozer : hunt the agent as soon as it smells him
%
%      Actions:     - 4 per square
%      Tries:       - Number of trials
%
%      Gold:        - Gold probability per square. When fig62 or pit3, only one gold. (default 0.1)
%      Hazard: Pit: - Pit probability per square. When fig62 or pit3, only 3 pits. (default 0.2)
%      Hazard: Bat: - Bat probability per square. When fig62, no bats. (default 0.1)
%
%      Hazards and gold:
%               - An integer number sets total.
%               - A float number from 0.0 < P < 1.0 sets the probability.
%
% Score:
%   * +1000 points for each gold AFTER climbing alive
%   * +500 points for killing the Wumpus
%   * -500 for dying (1. eaten alive by wumpus, 2. falling into a pit, 3. walking until exhausted)
%   * -1 for action (sit, turnright, turnleft, goforward, grab, shoot, climb)
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
    tr/0,           % shortcut for turnright
    turnleft/0,     % manually turns to left / anti-clockwise
    turnl/0,        % shortcut for turnleft
    tl/0,           % shortcut for turnleft
    grab/0,         % manually grabs something (gold, arrow, ...)
    gr/0,           % shortcut for grab
    shoot/0,        % manually shoots an arrow
    sh/0,           % shortcut for shoot
    sit/0,          % sit and do nothing
    si/0,           % shortcut for sit
    climb/0,        % manually climbs out the cave
    cl/0]).         % shortcut for climb

:- dynamic([
    get_setup/1,            % get world setup (from world_setup or from default setup)
    world_extent/1,         % ww World extent size
    wumpus_location/2,      % ww Wumpus location: (X,Y) on grid; (CaveNumber,Level) on dodeca;
    wumpus_health/1,        % ww Wumpus health: alive/dead
    wumpus_orientation/1,   % ww Wumpus orientation: 0, 90, 180, 270
    wumpus_last_action/1,   % ww Wumpus last action
    wumpus_move_rule/1,     % ww Wumpus movement rule: walker, runner, wanderer, spinner, hoarder, spelunker, stander, trapper, bulldozer
    gold/2,                 % ww Gold positions
    pit/2,                  % ww Pit positions
    bat/2,                  % ww Bat positions
    gold_probability/1,     % ww Probability that a location has gold (default 0.10)
    pit_probability/1,      % ww Probability that a non-(1,1) location has a pit (default 0.20)
    bat_probability/1,      % ww Probability that a non-(1,1) location has a bat (default 0.10)
    max_agent_actions/1,    % ww Maximum actions per trial allowed by agent (default 4*squardes)
    agent_location/2,       % Agent location: (X,Y) on grid; (CaveNumber, Level) on dodeca;
    agent_orientation/1,    % Agent orientation: 0/East/Right, 90/North/Up, 180/West/Left, 270/South/Down
    agent_in_cave/1,        % Agent is inside cave: yes/no
    agent_health/1,         % Agent health: alive/dead
    agent_gold/1,           % Number of golds that the agent has (start with 0)
    agent_arrows/1,         % Number of arrows that the agent has (start with 1)
    agent_score/1,          % Agent Game Score
    agent_num_actions/1,    % Number of the current agent action
    ww_initial_state/1      % list of ww configuration values
  ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Run by hand
%
%L=[Size, Type, Move, Gold, Pit, Bat, [RandS, RandA]]
%L=[4, grid, stander, 0.1, 0.2, 0.1, [no, no]] % default
manual_setup(L0) :-
    check_setup(L0, L1),
    retractall(get_setup(_)),
    assert(get_setup(L1)),
    !.

manual_init :-
    initialize(P), % also sets get_setup to default in case of no manual_setup
    get_setup(L),
    format("~nWorld Setup: ~w~n", [L]),
    format("First Impression: ~w~n", [P]),
    display_world,
    !.

% Manual actions to play directly at the swipl PROLOG prompt
go :- goforward.
goforward :- manual_execute(goforward).
turn :- turnright.
turnr :- turnright.
tr :- turnright.
turnright :- manual_execute(turnright).
turnl :- turnleft.
tl :- turnleft.
turnleft :- manual_execute(turnleft).
gr :- grab.
grab :- manual_execute(grab).
sh :- shoot.
shoot :- manual_execute(shoot).
cl :- climb.
climb :- manual_execute(climb).
si :- sit.
sit :- manual_execute(sit).

manual_execute(A) :-
    agent_num_actions(N), % current action
    execute(A, P),
    format("~nAction #~w: ~w~n", [N, A]),
    display_world,
    format("Perception: ~w~n", [P]),
    retractall(agent_num_actions(_)),
    N1 is N + 1,
    assert(agent_num_actions(N1)),
    agent_score(S),
	format("Score: ~d~n", S),
    !.

% evaluate_agent(Trials,Score,Time): Performs Trials trials, where each
%   trial involves generating a random wumpus world, initializing the
%   agent, running the agent until it dies or leaves the cave, and then
%   recording the score and time spent running the agent.  The total
%   score and time are returned in Score and Time (millisecs).
%
%   This procedure requires the external definition of 2 procedures:
%
%     init_agent: Called after new world is initialized.  Should perform
%                 any needed agent initialization.
%
%     run_agent(Percept,Action): Given the current Percept, this procedure
%                 should return an appropriate Action, which is then
%                 executed.

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

run_agent_action(_) :-
    ( agent_health(dead) ;          % trial over when agent dies or
      agent_in_cave(no) ),          %   leaves cave
    !.

run_agent_action(_) :-               % agent allowed only N actions as
    max_agent_actions(N),            %   defined by max_agent_actions(N)
    agent_num_actions(NumActions),   % current action
    NumActions > N,
    !.

run_agent_action(Percept) :-
    run_agent(Percept,Action),          % needs to be defined externally
    check_agent_action(Action),         % check for goforward, turnright, turnleft, shoot, grab, sit or climb.
    agent_num_actions(NumActions),      % current action
    format("~nExternal action #~w: run_agent(~w,~w)~n", [NumActions, Percept, Action]),
    execute(Action,Percept1),
    display_world,
    retractall(agent_num_actions(_)),
    NumActions1 is NumActions + 1,
    assert(agent_num_actions(NumActions1)),
    !,
    run_agent_action(Percept1).

run_agent_action(Percept) :-
    format("External function run_agent(~w, Nop) failed miserably!~n", [Percept]),
    !, fail.

% initialize(Percept): initializes the Wumpus world and our fearless
%   agent and returns the Percept from square 1,1.
%   Percept = [Stench,Breeze,Glitter,Bump,Scream,Rustle]

initialize([Stench, Breeze, Glitter, Bump, no, Rustle]) :-
    initialize_world,
    initialize_agent,
    stench(Stench),        % #1 stench(Stench) Wumpus may be nearby (no Wumpus on [1,1])
    breeze(Breeze),        % #2 no pit on [1,1] and grid: [1,2],[2,1] or dodeca: [2,2],[5,2],[8,2]
    glitter(Glitter),      % #3 no gold on [1,1]
    will_bump(Bump),       % #4 check if agent is facing a wall at start
    rustle(Rustle).        % #6 no bat on [1,1], and grid: [1,2],[2,1] or dodeca: [2,2],[5,2],[8,2]

% initialize_world: gather information
initialize_world :-
    ww_retract_all, % retract ww list (wumpus, gold, pit and bat, and all initial state variables)
    assert_setup,   % assert user or default setup
    get_setup(L),   % L=[Size, Type, Move, Gold, Pit, Bat, Adv], Adv=[RandS, RandA]
    L=[Size, _Type, Move, Gold, Pit, Bat|_],
    ww_addto_init_state(world_extent(Size)),
    random(0, 4, WAngN),
    WAng is WAngN * 90,
    ww_addto_init_state(wumpus_orientation(WAng)),   % Random Wumpus start angle
    ww_addto_init_state(wumpus_health(alive)),       % Wumpus is alive and well (and hungry)
    ww_addto_init_state(wumpus_last_action(sit)),    % Wumpus last move to use as basic AI memory
    ww_addto_init_state(gold_probability(Gold)),     % Probability that a location has gold
    ww_addto_init_state(pit_probability(Pit)),       % Probability that a non-(1,1) location has a pit
    ww_addto_init_state(bat_probability(Bat)),       % Probability that a location has bats
    Actions is Size * Size * 4,                      % 4 actions per square average (fig62 is 2.875 moves per square)
    ww_addto_init_state(max_agent_actions(Actions)), % Maximum actions per trial allowed by agent
    ww_addto_init_state(wumpus_move_rule(Move)),     % Wumpus move style
    initialize_world_type(L).

% initialize_world_type(World): Initializes the Wumpus world
% World = [Size, Type, Move, Gold, Pit, Bat, [Ax, Ay]]
%
% initialize_world_type(World): Initializes the Wumpus world in Figure 6.2 of Russell & Norvig
initialize_world_type([_,fig62,_,_,_,_|_]) :-
    ww_addto_init_state(wumpus_location(1,3)), % wumpus location
    ww_addto_init_state(gold(2,3)), % gold position
    ww_addto_init_state(pit(3,1)),  % pit 1
    ww_addto_init_state(pit(3,3)),  % pit 2
    ww_addto_init_state(pit(4,4)),  % pit 3
    ww_assert_all.

initialize_world_type([E, Type, _, PG, PP, PB|_]) :-
    gold_squares(E, Type, GS),
    hazard_squares(E, Type, HS),
    ww_place_it(gold, PG, GS),           % place gold (not [1,1])
    ww_place_it(pit, PP, HS),            % AllSqrs3
    ww_place_it(bat, PB, HS),            % place some bats not near the entrance
    ww_place_it(wumpus_location, 1, GS), % exactly one wumpus, initialize it not in [1,1]
    ww_assert_all.

% initialize_agent: agent is initially alive, destitute (except for one
%   arrow), in grid 1,1 and facing to the right (0 degrees).

initialize_agent :-
    retractall(agent_orientation(_)),
    retractall(agent_in_cave(_)),
    retractall(agent_health(_)),
    retractall(agent_gold(_)),
    retractall(agent_arrows(_)),
    retractall(agent_score(_)),
    retractall(agent_num_actions(_)),
    assert(agent_orientation(0)),
    assert(agent_in_cave(yes)),
    assert(agent_health(alive)),
    assert(agent_gold(0)),
    assert(agent_arrows(1)),
    assert(agent_score(0)),
    assert(agent_num_actions(1)),
    initialize_agent_advanced.

initialize_agent_advanced :-
    get_setup(L),   % [Size, Type, Move, Gold, Pit, Bat, Adv], Adv=[RandS, RandA]
    L=[Size, Type, _Move, _Gold, _Pit, _Bat, Adv], % [Ax, Ay]],
    initialize_agent_location(Size, Type, Adv).

initialize_agent_location(S, T, [yes|_]) :- % RandS = yes, random agent start location
    !,
    all_squares_type(T, S, All),
    findall([Xb, Yb], bat(Xb, Yb), AllBats),    % find all bats
    findall([Xp, Yp], pit(Xp, Yp), AllPits),    % find all pits
    findall([Xg, Yg], gold(Xg, Yg), AllGolds),  % find all golds
    wumpus_location(Wx, Wy),                    % find Wumpus
    subtract(All, AllBats, AllButBats),         % All but bats
    subtract(AllButBats, AllPits, AllBBPits),   % All but bats and pits
    subtract(AllBBPits, AllGolds, AllBBPGolds), % All but bats, pits and golds
    delete(AllBBPGolds, [Wx, Wy], AllButs),     % All but bats, pits, golds and Wumpus
    random_member([Ax, Ay], AllButs),           % Pick one
    retractall(agent_location(_,_)),
    assert(agent_location(Ax, Ay)).

initialize_agent_location(_, _, [no|_]) :- % RandS = no, agent at [1,1]
    retractall(agent_location(_,_)),
    assert(agent_location(1, 1)), !.

initialize_agent_location(_, _, [_]) :- % RandS = no, agent at [1,1]
    writeln('Bug L391, take a look at check_setup').

gold_squares(E, T, GS) :- % not used by fig62
    all_squares_type(T, E, All),
    delete(All, [1,1], GS).  % all squares but [1,1]

hazard_squares(E, grid, HS) :- % not used by fig62
    gold_squares(E, grid, GS),
    subtract(GS, [[1,2],[2,1]], HS). % all squares but [1,1],[2,1],[1,2]

hazard_squares(E, dodeca, HS) :-
    gold_squares(E, dodeca, GS), % all squares but [1,1]
    subtract(GS, [[2,2],[5,2],[8,2]], HS). % all squares but [2,2],[5,2],[8,2]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% dodeca_map : mapping dodecahedron tunnels.
% For each Cave from 1 to 20, there are exactly 3 tunnels Cn =\= 0
% [Cave1, [ C0, C1, C2, C3]], ..., [Cave20, [C0, C1, C2, C3]]
%
% C0 = East  / right / 0
% C1 = North / up    / 90
% C2 = West  / left  / 180
% C3 = South / down  / 270

dodeca_map([
    [1, [2, 8, 5, 0]],      % Cave  1: east 2,    north 8,    west 5,    south none
    [2, [0, 3, 10, 1]],     % Cave  2: east none, north 3,    west 10,   south 1
    [3, [2, 0, 4, 12]],     % Cave  3: east 2,    north none, west 4,    south 12
    [4, [3, 0, 5, 14]],     % Cave  4: east 3,    north none, west 5,    south 14
    [5, [6, 4, 0, 1]],      % Cave  5: east 6,    north 4,    west none, south 1
    [6, [0, 15, 5, 7]],     % Cave  6: east none, north 15,   west 5,    south 7
    [7, [0, 17, 6, 8]],     % Cave  7: east none, north 17,   west 6,    south 8
    [8, [9, 0, 7, 1]],      % Cave  8: east 9,    north none, west 7,    south 1
    [9, [10, 18, 0, 8]],    % Cave  9: east 10,   north 18,   west none, south 8
    [10, [2, 11, 0, 9]],    % Cave 10: east 2,    north 11,   west none, south 9
    [11, [0, 12, 19, 10]],  % Cave 11: east none, north 12,   west 19,   south 10
    [12, [0, 3, 13, 11]],   % Cave 12: east none, north 3,    west 13,   south 11
    [13, [12, 0, 14, 20]],  % Cave 13: east 12,   north none, west 14,   south 20
    [14, [13, 4, 0, 15]],   % Cave 14: east 13,   north 4,    west none, south 15
    [15, [16, 14, 0, 6]],   % Cave 15: east 16,   north 14,   west none, south 6
    [16, [0, 20, 15, 17]],  % Cave 16: east none, north 20,   west 15,   south 17
    [17, [18, 16, 0, 7]],   % Cave 17: east 18,   north 16,   west none, south 7
    [18, [0, 19, 17, 9]],   % Cave 18: east none, north 19,   west 17,   south 9
    [19, [11, 20, 0, 18]],  % Cave 19: east 11,   north 20,   west none, south 18
    [20, [19, 13, 16, 0]]   % Cave 20: east 19,   north 13,   west 16,   south none
    ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% execute(Action,Percept): executes Action and returns Percept
%
%  Action is one of:
%   1. goforward: move one square along current orientation if possible
%   2. turnleft:  turn left +90 degrees
%   3. turnright: turn right -90 degrees
%   4. grab:      pickup gold if in square
%   5. shoot:     shoot an arrow along orientation, killing wumpus if
%                 in that direction
%   6. climb:     if in square 1,1, leaves the cave and adds 1000 points
%                 for each piece of gold
%   7. sit:       do nothing, costs one action and -1 score
%
%   Percept = [Stench,Breeze,Glitter,Bump,Scream,Rustle]
%   each having a value of either 'yes' or 'no'.

execute(_,[no,no,no,no,no,no]) :-
    agent_health(dead), !,         % agent must be alive to execute actions
    format("You are dead!~n",[]).

execute(_,[no,no,no,no,no,no]) :-
    agent_in_cave(no), !,         % agent must be in the cave
    format("You have left the cave.~n",[]).

execute(goforward,[Stench,Breeze,Glitter,Bump,no,Rustle]) :-
    decrement_score,
    goforward(Bump),        % update location and check for bump
    move_wumpus(goforward), % move wumpus according to the rule set, before bats grab him
    elsewhereville(Rustle), % check for bats nearby or in the current square
    update_agent_health,    % check for wumpus, pit or max actions
    stench(Stench),         % update rest of percept
    breeze(Breeze),
    glitter(Glitter).

execute(turnleft,[Stench,Breeze,Glitter,no,no,Rustle]) :-
    decrement_score,
    agent_orientation(Angle),
    NewAngle is (Angle + 90) mod 360,
    retract(agent_orientation(Angle)),
    assert(agent_orientation(NewAngle)),
    move_wumpus(turnleft),  % move wumpus according to the rule set
    update_agent_health,    % check for wumpus, pit or max actions
    stench(Stench),
    breeze(Breeze),
    glitter(Glitter),
    rustle(Rustle).

execute(turnright,[Stench,Breeze,Glitter,no,no,Rustle]) :-
    decrement_score,
    agent_orientation(Angle),
    NewAngle is (Angle + 270) mod 360,
    retract(agent_orientation(Angle)),
    assert(agent_orientation(NewAngle)),
    move_wumpus(turnright), % move wumpus according to the rule set
    update_agent_health,    % check for wumpus, pit or max actions
    stench(Stench),
    breeze(Breeze),
    glitter(Glitter),
    rustle(Rustle).

execute(grab,[Stench,Breeze,no,no,no,Rustle]) :-
    decrement_score,
    get_the_gold,
    move_wumpus(grab),      % move wumpus according to the rule set
    update_agent_health,    % check for wumpus, pit or max actions
    stench(Stench),
    breeze(Breeze),
    rustle(Rustle).

execute(shoot,[Stench,Breeze,Glitter,no,Scream,Rustle]) :-
    decrement_score,
    move_wumpus(shoot),     % move wumpus according to the rule set
    shoot_arrow(Scream),    % shoot after wumpus move, as it may dodge the arrow
    update_agent_health,    % check for wumpus, pit or max actions
    stench(Stench),
    breeze(Breeze),
    glitter(Glitter),
    rustle(Rustle).

execute(climb,[no,no,no,no,no,no]) :-
    agent_location(1,1), !,
    decrement_score,
    agent_gold(G),
    retract(agent_score(S)),
    S1 is (S + (1000 * G)),
    assert(agent_score(S1)),
    retract(agent_in_cave(yes)),
    assert(agent_in_cave(no)),
    format("I am outta here.~n",[]).

execute(climb,[Stench,Breeze,Glitter,no,no,Rustle]) :-
    decrement_score,
    format("You cannot leave the cave from here.~n",[]),
    move_wumpus(climb),     % move wumpus according to the rule set
    update_agent_health,    % check for wumpus, pit or max actions
    stench(Stench),
    breeze(Breeze),
    glitter(Glitter),
    rustle(Rustle).

execute(sit,[Stench,Breeze,Glitter,no,no,Rustle]) :-
    decrement_score,
    move_wumpus(sit),       % move wumpus according to the rule set
    update_agent_health,    % check for wumpus, pit or max actions
    stench(Stench),
    breeze(Breeze),
    glitter(Glitter),
    rustle(Rustle).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Perceptions [Stench,Breeze,Glitter,Bump,Scream]

% stench(Stench): Stench = yes if wumpus (dead or alive) is in a square
%   directly up, down, left, or right of the current agent location.

stench(yes) :-
    agent_location(X,Y),
    has_hazard_perception(X, Y, wumpus_location),
    !.

stench(no).

% breeze(Breeze): Breeze = yes if a pit is in a square directly up, down,
%   left, or right of the current agent location.

breeze(yes) :-
    agent_location(X,Y),
    has_hazard_perception(X, Y, pit),
    !.

breeze(no).

% glitter(Glitter): Glitter = yes if there is gold in the current agent
%   location.

glitter(yes) :-
    agent_location(X,Y),
    gold(X,Y),
    !.

glitter(no).

% goforward(Bump): Attempts to move agent forward one unit along
%   its current orientation.

goforward(no) :-
    agent_orientation(Angle),
    agent_location(X,Y),
    new_location(X,Y,Angle,X1,Y1),  % fail if bump
    !,
    retract(agent_location(X,Y)),   % update location
    assert(agent_location(X1,Y1)).  % if it has bats, it will update again

goforward(yes).     % Ran into wall, Bump = yes
    format("Not possible! Bumped a wall!", []).

% agent will bump if he walks
will_bump(yes) :-
    agent_orientation(A),
    agent_location(X, Y),
    facing_wall(X, Y, A), !.

will_bump(no).


% rustle(Rustle): Rustle = yes if bats are nearby (adjacent)
% Rustle = no if no bats or
% agent is in the bat's square (for a period short of time of course)

rustle(yes) :-
    agent_location(X, Y),
    %\+ bat(X, Y),
    has_hazard_perception(X, Y, bat),
    !.

rustle(no).

% check for bats nearby or in the current square

%elsewhereville(yes) :-
%    rustle(yes), % just adjacent, not in the current square
%    !.

elsewhereville(Rustle) :-
    agent_location(X, Y),
    bat(X, Y),
    !,
    all_squares(All),
    delete(All, [X, Y], All1),  % all squares but the current one
    findall([BX,BY], bat(BX, BY), AllBats), % find all bats
    subtract(All1, AllBats, BatList), % remove all bats from possible new square
    random_member([AX, AY], BatList), % chose a new square [AX, AY]
    retractall(agent_location(_,_)),
    assert(agent_location(AX, AY)),
    rustle(Rustle),
    format("Zap! Super bat snatch! Elsewhereville for you!~n", []).

elsewhereville(Rustle) :- % no rustle, no bat move
    rustle(Rustle).

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% update_agent_health:
% kills agent if in a room with a live wumpus or a pit.

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
    format("... Oops! You are Wumpus food!~n",[]).

update_agent_health :-
    agent_location(X,Y),
    pit(X,Y),
    !,
    retract(agent_health(alive)),
    assert(agent_health(dead)),
    retract(agent_score(S)),
    S1 is S - 500,
    assert(agent_score(S1)),
    format("Yyiiiieeee... Fell in pit!~n",[]).

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Display the World
%
% display_world: Displays everything known about the wumpus world

display_world :-
    display_board,
    wumpus_orientation(WA),
    wumpus_health(WH),
    wumpus_location(WLX, WLY),
    wumpus_last_action(WAct),
    wumpus_move_rule(Rule),
    agent_orientation(AA),
    agent_health(AH),
    agent_arrows(N),
    agent_gold(G),
    agent_location(X,Y),
    format('wumpus_move_rule(~w)~n',[Rule]),
    format('wumpus_health(~w)~n',[WH]),
    format('wumpus_orientation(~d)~n',[WA]),
    format('wumpus_location(~d,~d)~n',[WLX, WLY]),
    format('wumpus_last_action(~w)~n~n',[WAct]),
    format('agent_health(~w)~n',[AH]),
    format('agent_orientation(~d)~n',[AA]),
    format('agent_location(~d,~d)~n',[X, Y]),
    format('agent_arrows(~d)~n',[N]),
    format('agent_gold(~d)~n',[G]).

display_board :- % squared board
    get_setup([E,Type|_]),
    (Type == grid ; Type == fig62),
    display_rows(E, E).

display_board :- % dodecahedron board
    get_setup([_,dodeca|_]),
    display_dodeca.

display_rows(0, E) :-
    !,
    display_dashes(E).

display_rows(Row, E) :-
    display_dashes(E),
    display_square(1, Row, E),
    Row1 is Row - 1,
    display_rows(Row1 ,E).

display_square(X, _, E) :-
    X > E,
    !,
    format('|~n',[]).

display_square(X, Y, E) :-
    format('|', []),
    display_info(X, Y),
    X1 is X + 1,
    display_square(X1, Y, E).

% display the letters 'W ', 'A ', 'P ' or 'G ', or spaces
display_info(X, Y) :-
    display_location_fact(wumpus_location, X, Y, 'W'), % Wumpus
    display_location_fact(agent_location, X, Y, 'A'),  % Agent
    display_location_fact(pit, X, Y, 'P'),             % Pit
    display_location_fact(bat, X, Y, 'B'),             % Bat
    display_location_fact(gold, X, Y, 'G').            % Gold

display_location_fact(Functor, X, Y, C) :-
    Fact =.. [Functor, X, Y], % Fact = gold(X, Y)
    Fact, % is Fact true?
    !,
    format('~w',[C]).

display_location_fact(_, _, _, _) :-
    format(' ',[]).

% display ------ * E + -
display_dashes(E) :-
    RowLen is (E * 6) + 1,
    name('-', [Dash]),
    format('~*c~n', [RowLen, Dash]).

% display dodeca 3 x 3 context near agent
display_dodeca :-
    display_dodeca_rows(3).

display_dodeca_rows(0) :-
    !,
    display_dashes(3).

display_dodeca_rows(R) :-
    display_dashes(3),
    display_dodeca_1row(R),
    R1 is R - 1,
    display_dodeca_rows(R1).

display_dodeca_1row(3) :-
    dodeca_map(L),
    agent_location(X, _),
    member([X, [_, C1, _, _]], L), % North
    D = [0, C1, 0], % C1 may be zero
    display_dodeca_squares(D).

display_dodeca_1row(2) :-
    dodeca_map(L),
    agent_location(X, _),
    member([X, [C0, _, C2, _]], L), % East + West
    D = [C2, X, C0], % Ci may be zero
    display_dodeca_squares(D).

display_dodeca_1row(1) :-
    dodeca_map(L),
    agent_location(X, _),
    member([X, [_, _, _, C3]], L), % South
    D = [0, C3, 0], % C3 may be zero
    display_dodeca_squares(D).

display_dodeca_squares([]) :-
    !,
    format('|~n',[]).

display_dodeca_squares([0|T]) :-
    format('|     ', []),
    display_dodeca_squares(T).

display_dodeca_squares([X|T]) :-
    format('|', []),
    all_squares_type(dodeca, 20, S),
    member([X, Y], S),
    display_info(X, Y),
    display_dodeca_squares(T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Dynamic facts

% ww_addto_init_state(Fact): Adds Fact to the list L stored in
%   ww_initial_state(L).

ww_addto_init_state(Fact) :-
    (ww_initial_state(L) ; L = []), !,
    retractall(ww_initial_state(_)),
    list_to_set([Fact|L],S), % avoid duplicates
    assert(ww_initial_state(S)).

% ww_assert_all :- assert all facts on ww list

ww_assert_all :-
    (ww_initial_state(L) ; L = []), !,
    ww_retract_all,
    ww_assert_list(L),
    assert(ww_initial_state(L)).

% assert_list(L): Assert all facts on list L.
ww_assert_list([]).

ww_assert_list([Fact|Facts]) :-
    assert(Fact), % assert_once(Fact),
    ww_assert_list(Facts).

% retract wumpus, gold, pit and bat
ww_retract_all :-
    (ww_initial_state(L) ; L = []), !,
    retractall(ww_initial_state(_)),
    maplist(ww_par, L, Pl), % Pl = [[gold, 2], [pit, 2], ...]
    list_to_set(Pl, S),
    ww_retract_list(S).

ww_retract_list([]).

ww_retract_list([[N, A]|Fs]) :-
    functor(U, N, A),    % functor(U, gold, 2), U=gold(_,_)
    retractall(U),       % retract all facts in the list
    ww_retract_list(Fs).

ww_par(F, P) :- % F=gold(1,3), P=[gold, 2]
    functor(F, N, A),
    P = [N, A].

% place_objects(Object,P,Squares): For each square in Squares, place
%   Object at square with probability P.

ww_place_it(_, _, []).

ww_place_it(gold, Qt, Sq) :-
    float(Qt),
    ww_place_objects_det(gold, 1, Sq),   % put one for sure
    ww_place_objects_prob(gold, Qt, Sq). % and lets see how many others

ww_place_it(Ob, Qt, Sq) :-
    float(Qt),
    ww_place_objects_prob(Ob, Qt, Sq).

ww_place_it(Ob, Qt, Sq) :-
    integer(Qt),
    ww_place_objects_det(Ob, Qt, Sq).

ww_place_objects_prob(_, _, []).

ww_place_objects_prob(Object, P, [Sq|Squares]) :-
    maybe(P),   % succeeds with probability P
    !,
    Fact =.. [Object|Sq], % Fact = pit(X,Y)
    ww_addto_init_state(Fact),
    ww_place_objects_prob(Object, P, Squares).

ww_place_objects_prob(Object, P, [_|Squares]) :-
    ww_place_objects_prob(Object, P, Squares).

ww_place_objects_det(_, _, []).

ww_place_objects_det(_, 0, [_|_]).

ww_place_objects_det(Obj, Qtd, [H|T]) :-
    Qtd>0,
    random_member(Sq, [H|T]),
    delete([H|T], Sq, S1),
    Fact =.. [Obj | Sq], % Fact = pit(X,Y)
    ww_addto_init_state(Fact),
    Q1 is Qtd - 1,
    ww_place_objects_det(Obj, Q1, S1).

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
check_agent_action_which(sit).
check_agent_action_which(_) :- format("Agent gave unknow action!~n"), !, fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% check_setup : check for user definition, or use default values
%
% world_setup [Size, Type, Move, Gold, Pit, Bat, [RandS, RandA]] 
%    1.  Size: 2..20, with some constrictions: [2-9] grid; 20, dodeca; 4, fig62. Zero for random size.
%    2.  Type: fig62, grid or dodeca
%    3.  Move: walker, runner, wanderer, spinner, hoarder, spelunker, stander, trapper, bulldozer
%    4.  Gold: Integer is deterministic number, float from 0.0<G<1.0 is probabilistic
%    5.  Pits: Idem, 0 is no pits.
%    6.  Bats: Idem, 0 is no bats.
%    Optional Advanced list: list with advanced setup
%           7. RandS: yes/no, random agent start location
%           8. RandA: yes/no, random agent angle
%           9. Other items reserved for future use
%
%   L = [Size, Type, Move, Gold, Pit, Bat, Adv]
%   Adv = [RandS, RandA|_]
%
% examples:
% world_setup([5, grid, stander, 1, 3, 1). % size 5, 1 gold, 3 pits and 1 bat
% world_setup([5, grid, stander, 1, 3, 1, []]). % size 5, 1 gold, 3 pits and 1 bat
% world_setup([4, grid, stander, 0.1, 0.2, 0.1, [no, no]]). % default
% world_setup([4, grid, stander, 0.1, 0.2, 0.1, [yes, no]]).
% world_setup([0, grid, stander, 0.1, 0.1, 0.1, [yes]]):
%   - random size, random golds, pits and bats
%   - grid, wumpus movement 'walker'
%   - [yes]: RandS, random agent start location
%

%get_setup([Size, Type, Move, Gold, Pit, Bat, [RandS, RandA]])
assert_setup :-
    current_predicate(world_setup, world_setup(_)),
    world_setup(Lin), % user definition
    check_setup(Lin, Lout),
    !,
    format("User defined setup: Size=~w, Type=~w, Move=~w, Gold=~w, Pit=~w, Bat=~w, Adv=~w~n", Lout),
    retractall(get_setup(_)),
    assert(get_setup(Lout)).

assert_setup :-
    current_predicate(get_setup, get_setup(_)), % case manual_setup asserted
    get_setup(Lout),
    !,
    format("Reusing setup: Size=~w, Type=~w, Move=~w, Gold=~w, Pit=~w, Bat=~w, Adv=~w~n", Lout).

assert_setup :-
    Lout=[4, grid, stander, 0.1, 0.2, 0.1, [no, no]], % defaulf
    format("Default setup: Size=~w, Type=~w, Move=~w, Gold=~w, Pit=~w, Bat=~w, Adv=~w~n", Lout),
    retractall(get_setup(_)),
    assert(get_setup(Lout)). % default

% correcting wrong combinations
% [Size, Type, Move, Gold, Pit, Bat, [Adv]]

check_setup([Size, Type, Move, Gold, Pit, Bat | TAdv], [S1, T1, M1, G1, P1, B1, A1]) :-
    check_setup_basic([Size, Type, Move, Gold, Pit, Bat], [S1, T1, M1, G1, P1, B1]),
    check_setup_advanced(TAdv, A1).

% fig62
% Size 4, wumpus stander, 1 gold, 3 pits, no bats, and agent_location may be random or not
check_setup_basic([_, fig62, Move, _, _, _], [4, fig62, M1, 1, 3, 0]) :- 
    !,
    check_setup_move(Move, M1).

% grid and dodeca

check_setup_basic([Size, Type, Move, Gold, Pit, Bat], [S1, T1, M1, G1, P1, B1]) :-
    check_setup_type(Type, T1),
    check_setup_size(T1, Size, S1), !, % ! fix alternatives (debug A:alternatives)
    check_setup_move(Move, M1),
    check_setup_gold_type(Gold, S1, G1, T1),
    check_setup_hazard_type(Pit, S1, P1, T1), % Qtd pits, size, Qtd Pits validated
    check_setup_hazard_type(Bat, S1, B1, T1).

% Check advanced list

check_setup_advanced([], [no, no]).

check_setup_advanced([Adv], A1) :-
    check_setup_advanced_rands(Adv, Rs), % agent random start location
    check_setup_advanced_randa(Adv, Ra), % agent random start angle of orientation
    A1 = [Rs, Ra].

check_setup_advanced_rands([yes|_], yes).
check_setup_advanced_rands(_, no).
check_setup_advanced_randa([_, yes|_], yes).
check_setup_advanced_randa(_, no).

% Check map type
check_setup_type(grid, grid).
check_setup_type(dodeca, dodeca).
check_setup_type(_, grid).

% Map size (or extension)
% if 0, random size and random agent start position
check_setup_size(grid, 0, S0) :- random(2, 10, S0).
check_setup_size(grid, S0, S0) :- S0>=2, S0=<9.
check_setup_size(grid, _, 4).
check_setup_size(dodeca, _, 20).

% Types of Wumpus Movement
check_setup_move(walker, walker). % original: moves when it hears a shoot, or you enter its cave
check_setup_move(runner, runner). % go forward and turn left or right on bumps, maybe on pits
check_setup_move(wanderer, wanderer). % arbitrarily choses an action from [sil,turnleft,turnright,goforward]
check_setup_move(spinner, spinner). % goforward, turnleft, repeat.
check_setup_move(hoarder, hoarder). % go to one of the golds and sit
check_setup_move(spelunker, spelunker). % go to a pit and sit
check_setup_move(trapper, trapper). % hunt the agent from distance
check_setup_move(bulldozer, bulldozer). % hunt the agent if smell it
check_setup_move(_, stander). % do not move (default)

% Gold, Pit and Bat : integer, fixed number; float, probability
check_setup_gold_type(G0, _, G0, _) :-
    float(G0),
    check_setup_prob(G0). % Gold Probability P

check_setup_gold_type(G0, S1, G0, grid) :-
    integer(G0),
    G0>0, % at least one gold
    MX is S1 * S1 - 1,
    G0=<MX.

check_setup_gold_type(G0, _, G0, dodeca) :-
    integer(G0),
    G0>0,
    G0=<19.

check_setup_gold_type(_, _, 1, _). % default, one piece of gold

check_setup_hazard_type(H0, _, H0, _) :-
    float(H0),
    check_setup_prob(H0).

check_setup_hazard_type(H0, S1, H0, grid) :-
    integer(H0),
    H0>=0,
    MX is S1 * S1 - 3,
    H0=<MX.

check_setup_hazard_type(H0, _, H0, dodeca) :-
    integer(H0),
    H0>=0,
    H0=<16.

check_setup_hazard_type(_, 2, 1, _). % Default 1 hazard, size = grid 2x2
check_setup_hazard_type(_, 3, 2, _). % Default 2 hazards, size = grid 3x3
check_setup_hazard_type(_, _, 3, _). % Default 3 hazards, size grid >= 4x4 or dodeca = 20

check_setup_prob(P) :- P>0.0, P<1.0.  % Probability 0.0<P<1.0

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% move the wumpus
%
% Types of Wumpus Movement
% walker    % original: moves when it hears a shoot, or you enter its cave
% runner    % go forward and turn left or right on bumps, maybe on pits
% wanderer  % arbitrarily choses an action from [sit,turnleft,turnright,goforward]
% spinner   % goforward, turnleft, repeat.
% hoarder   % go to one of the golds and sit
% spelunker % go to a pit and sit
% stander   % do not move (default)
% trapper   % goes hunting agent as soon as it leaves [1,1]; goes home otherwise
% bulldozer % hunt the agent as soon as it smells him
%

% move_wumpus: Moves the wumpus according to a pre-selected
% Rule as defined by wumpus_move_rule(Rule).

move_wumpus(_) :-
    wumpus_health(dead),
    !.

move_wumpus(AAct) :-
    wumpus_health(alive),
    wumpus_move_rule(Rule),
    wumpus_select_action(Rule, AAct, WAct),
    execute_wumpus_action(WAct),
    retract(wumpus_last_action(_)),
    assert(wumpus_last_action(WAct)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% wumpus_select_action(Rule,Action):  Uses wumpus movement Rule to
%   determine the next action for the wumpus (goforward, turnleft or
%   turnright).  When adding new rules, be sure to add their rule names
%   to the  wumpus_movement_rules list above.

% stander: do not move (default)

wumpus_select_action(stander, _, sit).

% wanderer: arbitrarily choses an action from [sit, turnleft, turnright and 2x goforward]

wumpus_select_action(wanderer, _, WAct) :-
    random_member(WAct, [sit, turnleft, turnright, goforward, goforward]).

% runner: go forward and turn left or right on bumps, maybe on pits

wumpus_select_action(runner, _, WAct) :-
    wumpus_location(X, Y),
    wumpus_orientation(WAng),
    (facing_wall(X, Y, WAng);
    facing_home(X, Y, WAng)),
    !,
    random_member(WAct, [turnleft, turnright]).

wumpus_select_action(runner, _, WAct) :-
    wumpus_location(X, Y),
    pit(X, Y),
    !,
    random_member(WAct, [turnleft, turnright, goforward, goforward]).

wumpus_select_action(runner, _, goforward).

% spinner: goforward, turnright, repeat.

wumpus_select_action(spinner, _, turnright) :-
    wumpus_last_action(goforward),
    !.

wumpus_select_action(spinner, _, goforward).

% hoarder: go to one of the golds and sit

wumpus_select_action(hoarder, _, sit) :- % no gold
    \+ gold(_,_),
    !.

wumpus_select_action(hoarder, _, sit) :- % already on top of a gold piece
    wumpus_location(X, Y),
    gold(X, Y),
    !.

wumpus_select_action(hoarder, _, WAct) :- % move towards gold
    gold(GX, GY),
    !,           % prevent backtracking on other gold pieces
    wumpus_location(WX, WY),
    wumpus_orientation(WAng),
    move_towards(WX, WY, WAng, GX, GY, WAct).

% spelunker: go to a pit and sit

wumpus_select_action(spelunker, _, sit) :- % no pits
    \+ pit(_,_),
    !.

wumpus_select_action(spelunker, _, sit) :- % already in a pit
    wumpus_location(X, Y),
    pit(X, Y),
    !.

wumpus_select_action(spelunker, _, WAct) :- % move towards a pit
    pit(PX, PY),
    !,           % prevent backtracking on other pits
    wumpus_location(WX, WY),
    wumpus_orientation(WAng),
    move_towards(WX, WY, WAng, PX, PY, WAct).

% walker : (original) moves when it hears a shoot, or you enter its cave

wumpus_select_action(walker, shoot, WAct) :- % move if hear a shoot
    agent_arrows(A),
    A > 0, !,  % agent has an arrow and he is using it right now!
    random_member(WAct, [turnleft, turnright, goforward, goforward]).

wumpus_select_action(walker, _, WAct) :- % or if the agent comes in
    wumpus_location(X, Y),
    agent_location(X, Y),
    !,
    random_member(WAct, [sit, turnleft, turnright, goforward]).

wumpus_select_action(walker, _, sit). % otherwise, don't move

% bulldozer : once it smells the Agent, it keeps following him

wumpus_select_action(bulldozer, _, WAct) :- % if agent smelling and not at [1,1], hunt!
    agent_location(AX, AY),
    [AX, AY] \== [1, 1],
    stench(yes),
    !,
    wumpus_location(WX, WY),
    wumpus_orientation(WAng),
    move_towards(WX, WY, WAng, AX, AY, WAct).

wumpus_select_action(bulldozer, _, WAct) :- % if agent smells decaying, maybe sit, maybe hunt, hunt, hunt!
    agent_location(AX, AY),
    [AX, AY] \== [1, 1],
    wumpus_location(WX, WY),
    (distance2(AX, AY, WX, WY) ; diagonal(AX, AY, WX, WY)),
    !,
    wumpus_orientation(WAng),
    move_towards(WX, WY, WAng, AX, AY, Tow),
    random_member(WAct, [sit, Tow, Tow, Tow]).

wumpus_select_action(bulldozer, _, WAct) :- % if agent at [1,1] or not stench random
    random_member(WAct, [sit, sit, turnleft, turnright, goforward]).

% trapper : goes for the kill even from distance. The only safe place is [1,1]

wumpus_select_action(trapper, _, WAct) :- % agent at [1,1] go home
    agent_location(1, 1),
    !,
    wumpus_location(WX, WY),
    wumpus_orientation(WAng),
    wumpus_home(HX, HY),
    move_towards(WX, WY, WAng, HX, HY, WAct).

wumpus_select_action(trapper, _, WAct) :- % agent not at [1,1], go for the kill
    agent_location(AX, AY),
    wumpus_location(WX, WY),
    wumpus_orientation(WAng),
    move_towards(WX, WY, WAng, AX, AY, WAct).

wumpus_home(HX, HY) :-
    get_setup([E,Type|_]),
    wumpus_home_type(HX, HY, Type, E).

wumpus_home_type(1, 3, fig62, 4).
wumpus_home_type(E, E, grid, E).
wumpus_home_type(20, 6, dodeca, 20).

diagonal(X1, Y1, X2, Y2) :- % only grid/fig62 makes sense
    get_setup([_,T|_]),
    diagonal_type(X1, Y1, X2, Y2, T).

diagonal_type(X1, Y1, X2, Y2, T) :- % only grid/fig62 makes sense
    (T == grid ; T == fig62),
    Xi is X1 - 1,
    Xf is X1 + 1,
    Yi is Y1 - 1,
    Yf is Y1 + 1,
    ([Xi, Yi] == [X2, Y2] ;
     [Xi, Yf] == [X2, Y2] ;
     [Xf, Yi] == [X2, Y2] ;
     [Xf, Yf] == [X2, Y2] ).

distance2(X1, Y1, X2, Y2) :-
    get_setup([_,T|_]),
    distance2_type(X1, Y1, X2, Y2, T).

distance2_type(X1, Y1, X2, Y2, T) :-
    (T == grid ; T == fig62),
    Xi is X1 - 2,
    Xf is X1 + 2,
    Yi is Y1 - 2,
    Yf is Y1 + 2,
    ([X1, Yi] == [X2, Y2] ;
     [X1, Yf] == [X2, Y2] ;
     [Xi, Y1] == [X2, Y2] ;
     [Xf, Y1] == [X2, Y2] ).

distance2_type(X1, _, X2, _, dodeca) :-
    dodeca_map(L),
    member([X1, Zero], L),
    delete(Zero, 0, [T1, T2, T3]),
    member([T1, Adj1], L),
    member([T2, Adj2], L),
    member([T3, Adj3], L),
    (member(X2, Adj1) ;
     member(X2, Adj2) ;
     member(X2, Adj3) ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% execute_wumpus_action(Action):  Similar to agent movement, where Action is
%   one of goforward, turnleft or turnright.  Wumpus cannot goforward into
%   location (1,1).  If goforward succeeds, then check agent's health.

execute_wumpus_action(goforward) :-
    wumpus_orientation(WAng),
    wumpus_location(X, Y),
    new_location(X, Y, WAng, X1, Y1),
    (X1 > 1 ; Y1 > 1),                % can't go into 1,1
    !,
    retract(wumpus_location(X, Y)),   % update location
    assert(wumpus_location(X1, Y1)).

execute_wumpus_action(goforward).     % unsuccessfully

execute_wumpus_action(turnleft) :-
    retract(wumpus_orientation(WAng)),
    WAng1 is (WAng + 90) mod 360,
    assert(wumpus_orientation(WAng1)).

execute_wumpus_action(turnright) :-
    retract(wumpus_orientation(WAng)),
    WAng1 is (WAng + 270) mod 360,
    assert(wumpus_orientation(WAng1)).

execute_wumpus_action(sit). % do nothing


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Support predicates
%

% facing_wall(X,Y,Orient): True if location (X,Y) is next to a wall, and orientation is facing wall.

facing_wall(X, Y, A) :-
    get_setup([E, Type|_]),
    facing_wall_type(X, Y, A, Type, E).

facing_wall_type(1, _, 180, fig62, _).
facing_wall_type(_, 1, 270, fig62, _).
facing_wall_type(4, _, 0, fig62, _).
facing_wall_type(_, 4, 90, fig62, _).

facing_wall_type(1, _, 180, grid, _).
facing_wall_type(_, 1, 270, grid, _).
facing_wall_type(E, _, 0, grid, E).
facing_wall_type(_, E, 90, grid, E).

facing_wall_type(X, _, 0, dodeca, _) :-
    dodeca_map(L),
    member([X,[0, _, _, _]], L). % To East is a wall
facing_wall_type(X, _, 90, dodeca, _) :-
    dodeca_map(L),
    member([X,[_, 0, _, _]], L). % To North is a wall
facing_wall_type(X, _, 180, dodeca, _) :-
    dodeca_map(L),
    member([X,[_, _, 0, _]], L). % To West is a wall
facing_wall_type(X, _, 270, dodeca, _) :-
    dodeca_map(L),
    member([X,[_, _, _, 0]], L). % To South is a wall

% facing_home(X,Y,Orient):  True if location (X,Y) is next to (1,1), and orientation is facing (1,1).

facing_home(X, Y, A) :-
    get_setup([_, Type|_]),
    facing_home_type(X, Y, A, Type).

facing_home_type(1, 2, 270, T) :-
    (T == grid ; T == fig62).

facing_home_type(2, 1, 180, T) :-
    (T == grid ; T == fig62).

facing_home_type(_, 2, 270, dodeca).

% move_towards(X1, Y1, Ang, X2, Y2, Action):
% If (X1, Y1) == (X2, Y2), action is sit.
% If (X1, Y1) =\= (X2, Y2),
% action is turnleft, turnright or goforward.

move_towards(X1, Y1, _, X1, Y1, sit).

move_towards(X1, Y1, Ang1, X2, Y2, Act) :-
    get_setup([_, Type|_]),
    move_towards_type(X1, Y1, Ang1, X2, Y2, Act, Type).

% grid or fig62
move_towards_type(X1, Y1, Ang1, X2, Y2, Act, T) :-
    (T == grid ; T == fig62),
    DX is X2 - X1,
    DY is Y2 - Y1,
    AngR is atan(DY, DX),
    rad2deg(AngR, Ang2),
    nearest_orientation(Ang2, Ang3),
    direction_action(Ang1, Ang3, Act).

% dodeca: adjacent cave
move_towards_type(X1, _, Ang1, X2, _, Act, dodeca) :-
    dodeca_map(L),
    member([X1, Adj], L), % member([X1, Adj=[C0, C1, C2, C3]], L),
    nth0(I, Adj, X2), % calculates the index from 0 to 3
    !,
    Ang2 is I * 90,
    direction_action(Ang1, Ang2, Act).

% dodeca: distant cave
move_towards_type(X1, _, Ang1, X2, _, Act, dodeca) :-
    move_dodeca_adjacent(X1, Xadj, X2),
    !,
    move_towards_type(X1, _, Ang1, Xadj, _, Act, dodeca).

% dodeca: distant cave, increasing
move_dodeca_adjacent(X1, Xadj, X2) :-
    X1 < X2,
    dodeca_map(L),
    member([X1, Adj], L),
    exclude(<(X2), Adj, Opt), % exclude caves greater than X2
    max_list(Opt, Xadj).

% dodeca: distant cave, decreasing
move_dodeca_adjacent(X1, Xadj, X2) :-
    X1 > X2,
    dodeca_map(L),
    member([X1, Adj], L),
    exclude(>(X2), Adj, Opt), % exclude caves smaller than X2
    min_list(Opt, Xadj).

% nearest_orientation(Angle,Orient):  Orient is the nearest orientation
%   (0, 90, 180, 270) to angle (in degrees), where 0 <= A < 360.

nearest_orientation(A, 0) :- (A =< 45 ; A > 315 ), !.
nearest_orientation(A, 90) :- A > 45, A =< 135, !.
nearest_orientation(A, 180) :- A > 135, A =< 225, !.
nearest_orientation(_, 270).

% direction_action(Orient1,Orient2,Action): Action = goforward if
%   Orient1=Orient2.  Otherwise, Action is turnleft or turnright
%   according to the difference between Orient1 and Orient2.

direction_action(270, 0, turnleft) :- !.
direction_action(0, 270, turnright) :- !.
direction_action(A1, A2, turnleft) :- A1 < A2, !.
direction_action(A1, A2, turnright) :- A1 > A2, !.
direction_action(_, _, goforward).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% converts radians to degrees

rad2deg(R, D) :-
    R >= 0.0,
    !,
    Dp is R * 180.0 / pi,
    D is integer(Dp).

rad2deg(R, D) :-
    R < 0.0,
    Rp is R + 2.0 * pi,
    rad2deg(Rp, D).

% has_hazard_perception/3 : true if square is adjacent or contains a hazard
% F is wumpus_location(X,Y) or pit(X,Y).
% The square test for Stench, Breeze or Rustling
has_hazard_perception(X, Y, F) :-
    get_setup([_,Type|_]),
    has_hazard_type(X, Y, F, Type), !.

has_hazard_type(X, Y, F, Type) :-
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

has_hazard_type(X, _, F, dodeca) :-
    dodeca_map(L),
    member([X, [C0, C1, C2, C3]], L),
    ( call(F, C0, _) ;
      call(F, C1, _) ;
      call(F, C2, _) ;
      call(F, C3, _) ;
      call(F, X, _) ).

% maybe/1: success with probability P
maybe(P) :-
    random(N),
    N<P.
% maybe/0: success with probability 0.5
maybe :- maybe(0.5).

% propagate_arrow(X,Y,Angle,Scream): If wumpus is at X,Y then hear its
%   woeful scream as you vanquish the creature.  If not, then move arrow
%   one square along Angle and try again.  If arrow hits a wall, then
%   you missed.

propagate_arrow(X, Y, A, S) :-
    get_setup([E, T|_]),
    propagate_arrow_type(X, Y, A, S, T, E).

propagate_arrow_type(X, Y, _, yes, _, _) :-
  wumpus_location(X, Y), !,
  kill_wumpus.

% To right / east
propagate_arrow_type(X, Y, 0, Scream, T, E) :-
    ( T == grid ; T == fig62 ),
    X1 is X + 1,
    X1 =< E,
    !,
    propagate_arrow_type(X1, Y, 0, Scream, T, E).

% To up / north
propagate_arrow_type(X, Y, 90, Scream, T, E) :-
    ( T == grid ; T == fig62 ),
    Y1 is Y + 1,
    Y1 =< E,
    !,
    propagate_arrow_type(X, Y1, 90, Scream, T, E).

% To left / west
propagate_arrow_type(X, Y, 180, Scream, T, _) :-
    ( T == grid ; T == fig62 ),
    X1 is X - 1,
    X1 > 0,
    !,
    propagate_arrow_type(X1, Y, 180, Scream, T, _).

% To down / south
propagate_arrow_type(X, Y, 270, Scream, T, _) :-
    ( T == grid ; T == fig62 ),
    Y1 is Y - 1,
    Y1 > 0,
    !,
    propagate_arrow_type(X, Y1, 270, Scream, T, _).

% To right / east
propagate_arrow_type(X, _, 0, Scream, dodeca, _) :-
    dodeca_map(L),
    member([X, [C0, _, _, _]], L),
    C0 =\= 0, % East valid
    !,
    propagate_arrow_type(C0, _, 0, Scream, dodeca, _).

% To up / north
propagate_arrow_type(X, _, 90, Scream, dodeca, _) :-
    dodeca_map(L),
    member([X, [_, C1, _, _]], L),
    C1 =\= 0, % North valid
    !,
    propagate_arrow_type(C1, _, 90, Scream, dodeca, _).

% To left / west
propagate_arrow_type(X, _, 180, Scream, dodeca, _) :-
    dodeca_map(L),
    member([X,[_, _, C3, _]], L),
    C3 =\= 0, % West valid
    !,
    propagate_arrow_type(C3, _, 180, Scream, dodeca, _).

% To down / south
propagate_arrow_type(X, _, 270, Scream, dodeca, _) :-
    dodeca_map(L),
    member([X,[_, _, _, C4]], L),
    C4 =\= 0, % South valid
    !,
    propagate_arrow_type(C4, _, 270, Scream, dodeca, _).

propagate_arrow_type(_, _, _, no, _, _).

% new_location(X,Y,Orientation,X1,Y1): returns new coordinates X1,Y1
%   after moving from X,Y along Orientation: 0, 90, 180, 270 degrees.
%   or FALSE if a bump occurs

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

new_location_type(X, _, 0, C0, Y1, dodeca, _) :-
    dodeca_map(L),
    member([X, [C0, _, _, _]], L),
    C0 =\= 0, % East valid
    all_squares_type(dodeca, 20, S),
    member([C0, Y1], S).

new_location_type(X, _, 90, C1, Y1, dodeca, _) :-
    dodeca_map(L),
    member([X, [_, C1, _, _]], L),
    C1 =\= 0, % North valid
    all_squares_type(dodeca, 20, S),
    member([C1, Y1], S).

new_location_type(X, _, 180, C2, Y1, dodeca, _) :-
    dodeca_map(L),
    member([X, [_, _, C2, _]], L),
    C2 =\= 0, % West valid
    all_squares_type(dodeca, 20, S),
    member([C2, Y1], S).

new_location_type(X, _, 270, C3, Y1, dodeca, _) :-
    dodeca_map(L),
    member([X, [_, _, _, C3]], L),
    C3 =\= 0, % South valid
    all_squares_type(dodeca, 20, S),
    member([C3, Y1], S).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% decrement_score: subtracts one from agent_score for each move

decrement_score :-
    retract(agent_score(S)),
    S1 is S - 1,
    assert(agent_score(S1)).

% kill_wumpus: pretty obvious

kill_wumpus :-
    retract(wumpus_health(alive)),
    assert(wumpus_health(dead)),
    retract(agent_score(S)),
    S1 is S + 500, % 500 point for killing the wumpus
    assert(agent_score(S1)).

% all_squares(AllSqrs): AllSqrs is the list of all possible
%   squares [X,Y] in a wumpus world of
%   grid: size Extent by Extent.
%   dodeca: 20 rooms, [Room Number, Room Level]

all_squares(AllSqrs) :-
    get_setup([E,T|_]), %[Size, Type, Move, Gold, Pit, Bat, [Ax, Ay]]),
    all_squares_type(T, E, AllSqrs).

all_squares_type(dodeca, 20, [[1,1],[2,2],[3,3],[4,3],[5,2],[6,3],[7,3],[8,2],[9,3],[10,3],[11,4],[12,4],[13,5],[14,4],[15,4],[16,5],[17,4],[18,4],[19,5],[20,6]]).

all_squares_type(Type, Extent, AllSqrs) :-
    (Type == grid ; Type == fig62),
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

