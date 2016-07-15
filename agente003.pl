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
% Strategy: try to solve the wumpus world without memory
% Performance: it does not go very well as you can imagine
%
% To define an agent within the navigate.pl scenario, define:
%   init_agent
%   run_agent
%   world_setup([Size, Type, Move, Gold, Pit, Bat])
%
%       +--------+-----------+
%       |  Type  |    Size   |
%       +--------+-----------+
%       | fig62  | 4 (fixed) |
%       | grid   | 2 ... 9   |
%       | dodeca | 20 (fixed)|
%       +--------+-----------+
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Lista de Percepcao: [Stench,Breeze,Glitter,Bump,Scream]
% Traducao: [Fedor,Vento,Brilho,Trombada,Grito]
% Acoes possiveis (abreviacoes):
% goforward (go)            - andar
% turnright (turn ou turnr) - girar sentido horario
% turnleft (turnl)          - girar sentido anti-horario
% grab                      - pegar o ouro
% climb                     - sair da caverna
% shoot                     - atirar a flecha
%
% Custos:
% Andar/Girar/Pegar/Sair/Atirar: -1
% Morrer: -500 (buraco, wumpus ou fadiga)
% Matar Wumpus: +500
% Sair com ouro: +1000 para cada pepita
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
% Para rodar o exemplo, inicie o prolog com:
% swipl -s agente003.pl
% e faca a consulta (query) na forma:
% ?- start.

:- use_module(wumpus, [start/0]). % agente usa modulo simulador

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% world_setup([Size, Type, Move, Gold, Pit, Bat])
%
%       Size:       - fig62, 4
%                   - grid, [2-9] (default 4)
%                   - dodeca, 20
%       
%       Type,       - fig62: 4x4, 1 gold, 3 pits. Not random, all set by book.
%       Topology &  - grid: can have random or determined variables 
%       Randomness: - dodeca: (aka dodecahedron original map)
%
%       Wumpus      - stander (does not move at all) (default)
%       Movement:   - walker (moves when hears shoot)
%                   - runner (moves all the time)
%
%       Gold:       - Gold probability per square. If fig62, 1 gold. (default 1 or 0.1)
%       Pit:        - Pit probability per square. If fig62, 3 pits. (default 1 for 2x2, 2 for 3x3, 3 if Size>=4x4, or 0.2)
%       Bat:        - Bat probability per square. If fig62, no bats. (default, idem)
%
% Configuration:
%    1.   Size: 0,2..9,20, where: grid is [2-9] or 0 for random, dodeca is 20, fig62 is 4.
%    2.   Type: fig62, grid or dodeca
%    3.   Move: stander, walker, runner (wumpus movement)
%    4.   Gold: Integer is deterministic number, float from 0.0<G<1.0 is probabilistic
%    5.   Pits: Idem, 0 is no pits.
%    6.   Bats: Idem, 0 is no bats.
%
%   Actions: Maximum actions allowed to agent: 4 actions per square in total
%   Tries: Number of trials for successive runs (default fixed 1)
%
% world_setup([Size, Type, Move, Gold, Pit, Bat])
%
% examples: 
% world_setup([4, grid, stander, 0.1, 0.2, 0.1])). % default
% world_setup([5, grid, stander, 1, 3, 0.1]). % size 5, 1 gold, 3 pits and maybe some bats with prob. 0.1
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Nao mexa nestas duas linhas:
:- load_files([wumpus]).
% Mundo: 4x4, quadrado, wumpus nao anda, 1 ouro, probabilidade de buracos 0.2, sem morcegos.
% Acoes totais: 64
world_setup([4, grid, stander, 1, 0.2, 0]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Inicie aqui seu programa.

init_agent.

% esta e a funcao chamada pelo simulador. Nao altere a "cabeca" da funcao. Apenas o corpo.
% Funcao recebe Percepcao, uma lista conforme descrito acima.
% Deve retornar uma Acao, dentre as acoes validas descritas acima.
run_agent(Percepcao, Acao) :-
    write('percebi: '), 
    writeln(Percepcao),
    acao(Percepcao, Acao). 

% Lista de Percepcao: [Fedor,Vento,Brilho,Trombada,Grito]
acao([_,_,yes,_,_], grab) :- %pega o ouro se sentir o brilho
    writeln('acao: pega').
acao([_,_,_,yes,_], turnright) :- %vira para a direita sempre que bater na parede
    writeln('acao: esquerda').
acao([yes,_,_,_,no], A):-  %atira, andar, girar ou sair em caso de fedor sem grito
    random_member(A, [shoot, shoot, shoot, shoot, turnright, turnright, turnright, climb, climb, goforward]),
    write('acao aleatoria: '),
    writeln(A).
acao([_,no,_,_,yes], goforward) :- %anda se escutar grito e sem brisa
    writeln('acao: frente').
acao([_,yes,_,_,yes], A):-  %grito com brisa
    random_member(A, [climb, turnright]),
    write('acao aleatoria: '),
    writeln(A).
acao([_,yes,_,_,_], A):-  %brisa
    random_member(A, [climb, climb, climb, turnright, turnright, goforward]),
    write('acao aleatoria: '),
    writeln(A).
acao(_, A) :- % sorteia default
    random_member(A, [turnright, goforward, climb]),
    write('acao aleatoria: '),
    writeln(A).


/* ----------------------------------------------------------------------- */
/* vi: set ai et ts=4 sw=4 tw=0 wm=0 fo=croql : PL config for Vim modeline */
/* Template by Dr. Beco <rcb at beco dot cc>       Version 20150620.224740 */

