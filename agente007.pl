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
%
% Strategy: goes only forward, do not turn, do not grab gold, do not come back
% Performance: it does not go very well as you can imagine
%
% To define an agent within the navigate.pl scenario, define:
%   init_agent
%   restart_agent
%   run_agent
%   wumpusworld(Type, Size)
%
%       +--------+-----------+
%       |  Type  |    Size   |
%       +--------+-----------+
%       | fig62  | 4 (fixed) |
%       | random | 2 ... 9   |
%       | pit3   | 3 ... 9   |
%       +--------+-----------+
%
% Currently set up to solve the wumpus world in Figure 6.2 of Russell and
% Norvig.  You can enforce generation of this world by changing the
% initialize(random,Percept) to initialize(fig62,Percept) in the
% navigate(Actions,Score,Time) procedure in file navigate.pl and then run
% navigate(Actions,Score,Time).

% initialize([Stench,Breeze,Glitter,Bump,Scream])
% initialize([Fedor,Vento,Brilho,Trombada,Grito])
% initialize([F,V,B,T,G])

% goforward
% turnright
% turnleft
% grab
% climb
% shoot

:- load_files([wumpus]).

% world_setup([Random, Topology, Size, Move, Actions, Tries, Gold, Pit, Bat]).
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
%       Actions:    - 2 to 400, number of maximum agent actions allowed (default 64)
%       Tries:      - Number of trials (default 1)
%       Gold:       - Gold probability per square. When fig62 or pit3, only one gold. (default 0.1)
%       Pit:        - Pit probability per square. When fig62 or pit3, only 3 pits. (default 0.2)
%       Bat:        - yes or no. When fig62, no. (default no)

%world_setup([random, grid, 4, stander, 64, 1, 0.1, 0.2, no]).  % (default)
%world_setup([random, grid, 9, stander, 10, 1, 0.3, 0.3, no]).
%world_setup([random, grid, 1, stander, 10, 1, 0.1, 0.1, no]).  % error
%world_setup([random, grid, 2, stander, 10, 1, 0.1, 0.2, no]). 
%world_setup([random, grid, 10, stander, 10, 1, 0.1, 0.1, no]). % error
%world_setup([pit3, grid, 2, stander, 10, 1, 0.1, 0.2, no]).    % error
%world_setup([pit3, grid, 3, stander, 10, 1, 0.1, 0.2, no]).
%world_setup([pit3, grid, 4, stander, 10, 1, 0.1, 0.2, no]).
%world_setup([pit3, grid, 9, stander, 10, 1, 0.1, 0.2, no]).
%world_setup([pit3, grid, 10, stander, 10, 1, 0.1, 0.2, no]).   % error
%world_setup([pit3, dodeca, 20, stander, 100, 1, 0.1, 0.2, yes]).

%world_setup([pit3, dodeca, 20, stander, 100, 1, 0.1, 0.2, no]).
world_setup([pit3, grid, 4, stander, 64, 1, 0.1, 0.2, no]).

init_agent.

restart_agent :- init_agent.

run_agent(Percepcao, Acao) :-
  cabeca_dura(Percepcao, Acao).

cabeca_dura(_,goforward).

