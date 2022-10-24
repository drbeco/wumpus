%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%    Hunt The Wumpus - World Simulator                                          %
%    Copyright (C) 2012 - 2022  Ruben Carlo Benante <rcb at beco dot cc>        %
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
%   Copyright: 2012 - 2022
%   License: GNU GPL Version 2.0
%
%   Special thanks to:
%     - Original by Gregory Yob (1972)
%     - Larry Holder (accessed version Oct/2005)
%     - Walter Nauber 09/02/2001
%     - An Anonymous version of Hunt The Wumpus with menus (aeric? 2012?)
%
% A Prolog implementation of the Wumpus world invented by Gregory Yob
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Strategy: 
%       - Lock a target and fulfill it. Targets can be 2 turns, 1 turn or 1 go forward
%       - Grab gold on sight
%       - Shoot as soon as it smells a Wumpus
%       - If wumpus dead, stump the body (go forward)
%       - If it smells and agent have no arrows, go around (safe mode)
%       - If it fells breeze, go back
%       - If wumpus dead or a piece of gold found, get out the cave
%       - If actions reach a dangerous amount, use the last to get out
%       - Nothing happening:
%           - explore new squares
%           - no new squares, explore random old squares
%
% Performance:
%       - Pretty good damn agent!

:- use_module(wumpus, [start/0]). % agente usa modulo simulador

:- dynamic([safe/1, mpos/1, lastpos/1, angle/1, arrows/1, action/1, wumpus/1, target/1, gold/1]).

% todo: world size 0 means random
% todo: random start position of agent
%world_setup([20, dodeca, stander, 1, 3, 0]). % size 4, 1 gold, 3 pits and 0 bat
%world_setup([4, grid, stander, 1, 3, 0]). % size 4, 1 gold, 3 pits and 0 bat
%world_setup([4, grid, stander, 1, 3, 1]). % size 4, 1 gold, 3 pits and 0 bat

world_setup([5, grid, walker, 0.2, 0.1, 0.1, [no]]). % size?, golds?, pits? and bats?

% debug: wumpus location, list of golds, list of pits, list of bats
debug([[1,2],[[2,1]],[[3,1],[3,2],[3,3]],[]]).

init_agent :-
    retractall(safe(_)),
    retractall(mpos(_)),
    retractall(lastpos(_)),
    retractall(angle(_)),
    retractall(arrows(_)),
    retractall(action(_)),
    retractall(wumpus(_)),
    retractall(target(_)),
    retractall(gold(_)),
    assert(safe([])), 
    assert(mpos([1,1])),
    assert(lastpos([0,0])),
    assert(angle(0)),
    assert(arrows(1)),
    assert(action(0)),
    assert(wumpus(alive)),
    assert(target([])),
    assert(gold(0)).

restart_agent :- % in case of lifes
    init_agent.

run_agent(Per, Act) :-
    mpos(P),
    update_safe(P),
    update_action,
    select(Per, Act),
    target(T),
    format("Next Target: ~w~n", [T]),
    format("Next Action: ~w~n", [Act]).

% atualiza seguras e visitadas apenas
% update secure and visited only
update_safe(P) :-
    safe(S),
    retractall(safe(_)),
    union(S, [P], S1),
    assert(safe(S1)),
    format("~nSafe squares: ~w~n", [S1]).

% se indo embora, vai cadastrando todas as seguras do caminho
% if leaving, mark all secures in the way
add_safe([no,no|_], P) :-
    near_grid(P, Adj),
    safe(S),
    retractall(safe(_)),
    union(S, Adj, S1),
    assert(safe(S1)),
    format("~nAdded safe squares: ~w~n", [S1]).
add_safe([yes,no|_], P) :-
    wumpus(dead),
    near_grid(P, Adj),
    safe(S),
    retractall(safe(_)),
    union(S, Adj, S1),
    assert(safe(S1)),
    format("~nAdded safe squares: ~w~n", [S1]).
add_safe(_, _). % nothing to add

update_action :-
    action(A),
    retractall(action(_)),
    B is A + 1,
    assert(action(B)),
    format("Number of actions: ~w~n", [B]),
    !.

%shoot: Stench=yes, Arrow>0, Wumpus=alive
%ignore: Wumpus=dead
%avoid: Stench=yes, Wumpus=alive, Arrow==0
% fedor, vento, brilho, trombada, grito

%---------------------------------------------------------------
% Tem alvo? Continua cumprindo.
% Does it still have a target? If so, keep complying.
select(_, Act) :-
    mpos(Mpos),
    target(Targ),
    Targ \== [],
    angle(Ang),
    format("Fulfilling locked target.~n"),
    go_grid_adj(Mpos, Targ, Ang, Act).
    %retractall(target(_)),
    %assert(target([])).

% brilho? pega
% If it shines, it is gold, grab.
select([_,_,yes|_], grab) :-
    gold(G),
    retractall(gold(_)),
    G1 is G + 1,
    assert(gold(G1)),
    format("Gimme that gold! (~w)~n", [G1]).

% fedor, wumpus vivo e tenho flecha? atira
% If it stinks, and I have an arrow? shoot
select(Per, shoot) :-
    Per=[yes|_],
    wumpus(alive),
    arrows(1),
    retractall(arrows(_)),
    assert(arrows(0)),
    format("Shoot the bastard!~n"),
    add_safe_by_arrow(Per).

% grito e sem brisa? eba, matei! pisa no cadaver
% if it cries, and there is no breeze, walks over the dead body
select([_, no, _, _, yes|_], goforward) :-
    retractall(wumpus(_)),
    assert(wumpus(dead)),
    format("Wumpus dead! Stumping the body!~n"),
    %fail. % ainda nao sei
    mpos(Mpos),
    angle(Ang),
    go_grid_adj(Mpos, _, Ang, goforward). % Stump his body!

% grito com brisa? eba, matei! Mas nao sei o que fazer... 
% if it cries, but there is a breeze, I don't know what to do
% TODO: add a default action here, if needed
select([_, yes, _, _, yes|_], _) :- %goforward) :-
    retractall(wumpus(_)),
    assert(wumpus(dead)),
    format("Wumpus dead!~n"),
    fail. % ainda nao sei

%---------------------------------------------------------------

% tenho ouro OU wumpus morto e longe da saida? missao cumprida! volta
% I have the gold OR the wumpus is dead, and I'm far away from the exit? Mission accomplished, lets go back
select(Per, Act) :-
    mpos(Mpos),
    Mpos \== [1,1],
    gold(G),
    (wumpus(dead) ; G > 0), % ouro OU wumpus morto
    add_safe(Per, Mpos),
    new_target(back,T),
    %new_target(home,T),
    angle(Ang),
    print_what,
    format("Going home!~n"),
    go_grid_adj(Mpos, T, Ang, Act).

% tenho ouro OU wumpus morto e na saida? missao cumprida! escalar
% I have the gold OR the wumpus is dead, and I'm at the exit? Mission accomplished, lets climb
select(_, climb) :-
    mpos([1,1]),
    gold(G),
    (wumpus(dead) ; G > 0), % ouro OU wumpus morto
    print_what,
    format("Getting out!~n").

%---------------------------------------------------------------

% Acabando as acoes, to na saida? escalar
% I have few actions, but I'm at the exit point? climb
select(_, climb) :-
    action_overflow,
    mpos([1,1]),
    format("Climbing out of this filth place!~n").

% Acabando as acoes? voltar
% Few action, not at the exit point? Target to go back
select(Per, Act) :-
    action_overflow,
    mpos(Mpos),
    add_safe(Per, Mpos),
    new_target(back, Targ),
    %new_target(home, Targ),
    angle(Ang),
    format("I've spent too much time in this unholy world!~n"),
    go_grid_adj(Mpos, Targ, Ang, Act).

%---------------------------------------------------------------

% fedor, wumpus vivo ainda, e nao tenho flecha? vai na seguranca
% Stinks, Wumpus alive, I do not have arrows? Go only in safe caves
select([yes|_], Act) :-
    action_ok,
    wumpus(alive),
    arrows(0),
    new_target(safe,T),
    mpos(Mpos),
    angle(Ang),
    format("Smelly place and no arrows! Going safe...~n"),
    go_grid_adj(Mpos, T, Ang, Act).

% brisa? volta
% A breeze? Go back
select([_, yes|_], Act) :-
    action_ok,
    new_target(back,T),
    mpos(Mpos),
    angle(Ang),
    format("Too cold here for me! Going back!~n"),
    go_grid_adj(Mpos, T, Ang, Act).

% brisa, e nao volta? escalar
% A breeze, and I can't go back? Climb
select([_, yes|_], climb) :-
    action_ok,
    mpos([1,1]),
    format("This cave gives me the chills!~n").

%---------------------------------------------------------------

% nada? explorar o mundo
% Nothing here? Let's explore the world
select([no, no | _], Act) :-
    new_target(explore, T),
    mpos(Mpos),
    angle(Ang),
    format("I fell like exploring this place a bit...~n"),
    go_grid_adj(Mpos, T, Ang, Act).

%---------------------------------------------------------------

max_actions(M) :-
    world_setup([E|_]), %BUG E=0 mapa tamanho aleatorio, max_actions M==0
    S is E * 4, % numero de acoes que preciso
    T is E * E * 4, % numero de acoes totais
    M is T - S. % total - preciso
    %format("~nDebug: S=~w, T=~w, D=~w, C=~w~n", [S, T, M]).

action_overflow :- % true se estourar
    action(C),
    max_actions(M),
    C > M. % alcancei o limite

action_ok :- % true se ainda tenho acoes
    action(C),
    max_actions(M),
    C =< M. % nao alcancei o limite

% fedor sem brisa, casa segura apos tiro
add_safe_by_arrow([yes, no | _]) :-
    mpos(Mpos),
    angle(Ang),
    forwarding(Mpos, Ang, For),
    update_safe(For).

% fedor com brisa, deixa pra la
add_safe_by_arrow([yes, yes | _]). % not safe

print_what :-
    print_wumpus,
    print_gold.

print_wumpus :-
    wumpus(dead),
    format("Got the smelly creature! ").
print_wumpus.

print_gold :-
    gold(G),
    G > 0,
    format("Got the bucks! ").
print_gold.

% explore new
new_target(explore, Targ) :-
    mpos(Mpos),
    near_grid(Mpos, Adj),
    safe(S),
    subtract(Adj, S, [Targ|_]),
    retractall(target(_)),
    assert(target(Targ)).

% explore random
new_target(explore, Targ) :-
    mpos(Mpos),
    near_grid(Mpos, Adj),
    random_member(Targ, Adj),
    retractall(target(_)),
    assert(target(Targ)).

% explore safe random
new_target(safe, Targ) :-
    mpos(Mpos),
    near_grid(Mpos, Adj),
    safe(S),
    intersection(Adj, S, R),
    random_member(Targ, R),
    retractall(target(_)),
    assert(target(Targ)).

% Know bug: Mpos=[1,3], e nao visitou [1,2]. Entre [2,3] e [1,4], prefere errado [1,4]
% Can give empty target intersection([[2,1],[1,2]], [[1,1]], []) (case pit on [2,1] for example)
new_target(back, Targ) :-
    mpos(Mpos),
    near_grid(Mpos, Adj),
    safe(V),
    intersection(Adj, V, It),
    sort(It, [Targ|_]),
    retractall(target(_)),
    assert(target(Targ)).

new_target(home, Targ) :-
    mpos(Mpos),
    lastpos(Lpos),
    near_grid(Mpos, Adj),
    safe(V),
    intersection(Adj, V, It),
    delete(It, Lpos, Opts), % do not repeat last square
    %sort(Opts, [Targ1, Targ2|_]),
    sort(Opts, Sor),
    random_member(Targ, Sor),
    retractall(target(_)),
    assert(target(Targ)).

new_target(force, Targ) :-
    nonvar(Targ),
    mpos(Mpos),
    near_grid(Mpos, Adj),
    member(Targ, Adj), % Alvo tem que ser adjacente
    retractall(target(_)),
    assert(target(Targ)).

% Dada posicao P, retorna lista com casas adjacentes validas
% Given P position, return the list of valid adjacent caves
near_grid(P, L) :-
    near_grid_e(P, [], Le),
    near_grid_n(P, Le, Ln),
    near_grid_w(P, Ln, Lw),
    near_grid_s(P, Lw, L),
    !.

near_grid_e(P, L, Le) :- % [X1, Y]) :-
    forwarding(P, 0, Q),
    P \== Q,
    append(L, [Q], Le).
near_grid_e(_, L, L).

near_grid_n(P, L, Ld) :- %[X, Y1]) :-
    forwarding(P, 90, Q),
    P \== Q,
    append(L, [Q], Ld).
near_grid_n(_, L, L).

near_grid_w(P, L, Lw) :- %[X1, Y]) :-
    forwarding(P, 180, Q),
    P \== Q,
    append(L, [Q], Lw).
near_grid_w(_, L, L).

near_grid_s(P, L, Ls) :- %[X, Y1]) :-
    forwarding(P, 270, Q),
    P \== Q,
    append(L, [Q], Ls).
near_grid_s(_, L, L).

forwarding([X, Y], 0, [X1, Y]) :-
    world_setup([E|_]),
    X1 is X + 1,
    X1 =< E.
forwarding([X, Y], 0, [X, Y]). % trombou, fica

forwarding([X, Y], 90, [X, Y1]) :-
    world_setup([E|_]),
    Y1 is Y + 1,
    Y1 =< E.
forwarding([X, Y], 90, [X, Y]). % trombou, fica

forwarding([X, Y], 180, [X1, Y]) :-
    X1 is X - 1,
    X1 > 0.
forwarding([X, Y], 180, [X, Y]). % trombou, fica

forwarding([X, Y], 270, [X, Y1]) :- % trombou, fica
    Y1 is Y - 1,
    Y1 > 0.
forwarding([X, Y], 270, [X, Y]). % trombou, fica

go_grid_adj(P0, P1, A, goforward) :-
    forwarding(P0, A, P1),
    update_pos(P1).

go_grid_adj(P0, P1, A, turnright) :-
    B is (A - 90) mod 360,
    forwarding(P0, B, P1),
    retractall(angle(_)),
    assert(angle(B)),
    format("New angle: ~w~n", [B]).

go_grid_adj(_, _, A, turnleft) :-
    B is (A + 90) mod 360,
    retractall(angle(_)),
    assert(angle(B)),
    format("New angle: ~w~n", [B]).

update_pos(P1) :-
    mpos(P0),
    retractall(mpos(_)),
    retractall(lastpos(_)),
    assert(mpos(P1)),
    assert(lastpos(P0)),
    retractall(target(_)),
    assert(target([])),
    format("New position: ~w -> ~w~n", [P0, P1]).

/* ----------------------------------------------------------------------- */
/* vi: set ai et ts=4 sw=4 tw=0 wm=0 fo=croql : PL config for Vim modeline */
/* Template by Dr. Beco <rcb at beco dot cc>       Version 20150620.224740 */

