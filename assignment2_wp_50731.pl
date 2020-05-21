candidate_number(50731).

% Find hidden identity by repeatedly calling agent_ask_oracle(oscar,o(1),link,L)
% find_identity(-A)
find_identity(A):-
  (part_module(2)   -> find_identity_2(A)
  ; otherwise -> find_identity_o(A)
  ).

current_energy(E) :-
  my_agent(Agent),
  query_world( agent_current_energy, [Agent,E] ).

agent_ask_oracle(o(X),link,L) :-
  my_agent(Agent),
  query_world( agent_ask_oracle, [Agent,o(X),link,L]).

topup(C) :-
  my_agent(Agent), 
  query_world( agent_topup_energy, [Agent,C] ).

current_position(P) :-
  my_agent(Agent),
  query_world( agent_current_position, [Agent,P]). 

% For ASS2 Part 2
find_identity_2(A) :-
  bagof(Ac,actor(Ac), As), 
  eliminate(As,A). 

zip([], [], []).
zip([X|Xs], [Y|Ys], [[X,Y]|Zs]) :- zip(Xs,Ys,Zs).

% Given list of actors, ask oracle for link, keep all actors whose wikipedia pages contain the link 
% When there is only one element in the list, that is your actor 
% eliminate(+As, -Sss) 
eliminate(As,X):-
  agent_ask_oracle(oscar, o(1),link,L), 
  include(isAMember(L), As, Xs),
  ( oneElem(Xs, X),!
  ; otherwise -> eliminate(Xs, X)).

% Given a list of seen oracles, get the nearest unseen oracle, the cost to get there and the shortest path to it 
% nearest(+SeenOs,-o(X),-Cost,-Path)
nearest(SeenOs,o(X),Cost,Path) :-
  current_position(P), 
  solve_task_astar(SeenOs, P, find(o(X)), [c(0, 0, 0, P, P)], [], Path, Cost, _NewPos), !. 

% Get the nearest energy station, the cost to get there and the shortest path to it 
% nearest(-c(X),-Cost,-Path)
nearest(c(X),Cost,Path) :-
  current_position(P), 
  solve_task_astar(P, find(c(X)), [c(0, 0, 0, P, P)], [], Path, Cost, _NewPos), !. 

% Given a list of actors, an oracle, seen oracles, get link from oracle and elminate all actors in the list of actors
%   whose wikipedia page does not contain the link. If there's only one actor left, that is the identity of the agent
%   otherwise call getidentity(+Xs,+SeenOs,-A)
% eliminate(+As,+o(X),+SeenOs,-A)
eliminate(As,o(X),SeenOs,A):-
  agent_ask_oracle(o(X), link, L), 
  include(isAMember(L), As, Xs),
  writeln("Remaining actors: "), writeln(Xs),
  ( oneElem(Xs, A), !
  ; otherwise -> getidentity(Xs,SeenOs,A)).

% If there's only one element in a list, return true 
oneElem([Xs], Xs).

% Given a link L, check that if it belongs to a list of links of the actor
isAMember(L,X):-
  bagof(AL, (wp(X, WT), wt_link(WT, AL)), Ls), 
  member(L, Ls).

% ASS2 Part 3 and 4 
find_identity_o(A):-
  % Get a list of actors I could be
  bagof(Ac, actor(Ac), As), 
  getidentity(As,[],A).

% getidentity(+Ac,+SeenOs,-A)
getidentity(Ac,SeenOs,A) :- 
  % Find nearest oracle 
  % If my energy is not enough, find nearest energy station
    % If energy is not enough, mission failed
    % Else go to energy station and charge up, find nearest oracle again 
  % Else go to oracle and ask question
  nearest(SeenOs,o(X),cost(C),Path), current_energy(E), EnergyNeeded is C + 10, 
  ( (E @=< EnergyNeeded ; E @=< 60)  -> % writeln("Finding nearest energy station"),
                             nearest(c(Z),cost(D),EPath),
              ( E @=< D   -> writeln("out of energy!"), false 
              ; otherwise -> ( move(EPath), current_position(P), map_adjacent(P,_,c(Z)) -> topup(c(Z)), getidentity(Ac,SeenOs,A)
                             ; otherwise -> getidentity(Ac,SeenOs,A) ) 
              )
  ; otherwise -> ( move(Path), current_position(P), map_adjacent(P,_,o(X)) -> eliminate(Ac,o(X),[o(X)|SeenOs],A)
                 ; otherwise -> getidentity(Ac,SeenOs,A) )
  ). 

energyneeded(C,F) :- F is C + 10. 
head([H|T],H,T).
isEmpty([]).

move([]).
move(Path) :-
  my_agent(Agent),
  head(Path, P, Tail),
  ( query_world(agent_do_moves,[Agent,[P]]) -> move(Tail)
  ; otherwise -> get_alternative_path(Tail) 
  ). 

% Can't get to the original position, return false 
get_alternative_path([]) :- false. 
get_alternative_path(Tail) :-
  current_position(P1), 
  head(Tail,P2,Tail1),
  ( solve_task_astar(P1, find(P2), [c(0, 0, 0, P1, P1)], [], AlternativePath, _Cost, _NewPos), !-> writeln("Alternative path: "), writeln(AlternativePath), 
                                                                                                   append(AlternativePath,Tail1,NewPath),
                                                                                                   move(NewPath)
  ; otherwise -> get_alternative_path(Tail1)
  ).

