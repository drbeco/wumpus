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

% Lista de Percepcao: [Stench,Breeze,Glitter,Bump,Scream]
% Traducao: [Fedor,Vento,Brilho,Trombada,Grito]
% Acoes possiveis:
% goforward - andar
% turnright - girar sentido horario
% turnleft - girar sentido anti-horario
% grab - pegar o ouro
% climb - sair da caverna
% shoot - atirar a flecha

% Copie wumpus1.pl e agenteXX.pl onde XX eh o numero do seu agente (do grupo)
% para a pasta rascunhos e depois de pronto para trabalhos
% Todos do grupo devem copiar para sua pasta trabalhos, 
% com o mesmo NUMERO, o arquivo identico.

% Para rodar o exemplo, inicie o prolog com:
% swipl -s agente007.pl
% e faca a consulta (query) na forma:
% ?- start.


%:- module(agente003, [wumpusworld/2, init_agent/0, restart_agent/0, run_agent/2]).  % errado: agente nao eh modulo
%:- load_files([wumpus]).  % tanto faz load_files ou use_module

:- use_module(wumpus, [start/0]). % agente usa modulo simulador
 

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
%world_setup([pit3, dodeca, 20, stander, 100, 1, 0.1, 0.2, no]).
%world_setup([pit3, grid, 5, stander, 64, 1, 0.1, 0.2, no]).

%New configuration format:
% world_setup(Type,
%    1.   Size: 2..20, with some constrictions: [2-9] grid; 20, dodeca; 4, fig62
%    2.   Type: fig62, grid or dodeca
%    3.   Move: stander, walker, runner (wumpus movement)
%    4.   Gold: Integer is deterministic number, float from 0.0<G<1.0 is probabilistic
%    5.   Pits: Idem, 0 is no pits.
%    6.   Bats: Idem, 0 is no bats.
%
%       Actions: 2..400, agent actions allowed (not in this version. Now its 4 actions per square))
%       Tries: fixed at 1 now (for future versions)


% world_setup([4, grid, stander, 0.1, 0.2, 0.1])). % default
%world_setup([2, grid, stander, 1, 3, 1]). % size 5, 1 gold, 3 pits and 1 bat
world_setup([3, grid, stander, 0.1, 0.2, 0]). % size 5, 1 gold, 3 pits and 1 bat

init_agent :-
    retractall(flecha(_)),
    assert(flecha(1)).

% Funcao a implementar.
%restart_agent :-
%    init_agent.

% esta e a funcao chamada pelo simulador. Nao altere a "cabeca" da funcao. Apenas o corpo.
% Funcao recebe Percepcao, uma lista conforme descrito acima.
% Deve retornar uma Acao, dentre as acoes validas descritas acima.
run_agent(Percepcao, Acao) :-
  write('percebi: '), 
  writeln(Percepcao),
  wumpus:agent_arrows(1),
  acao(Percepcao, Acao). 

acao([_,_,no,yes,_], turnleft) :- %vira para a esquerda sempre que bater na parede
    writeln('acao: esquerda').
acao([_,_,yes,_,_], grab) :- %pega o ouro se sentir o brilho
    writeln('acao: pega').
acao([yes,_,no,_,_], shoot):-  %atira em linha reta se sentir fedor e tiver uma flecha
    flecha(1),
    writeln('acao: atira').
acao(_, goforward) :- %vai pra frente caso default
    writeln('acao: frente').

