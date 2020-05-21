candidate_number(50731).

solve_task(Task,Cost):-
  my_agent(Agent),
  query_world( agent_current_position, [Agent,P] ),
  solve_task_astar(P, Task, [c(0, 0, 0, P, P)], [], Path, Cost, _NewPos), !, 
  query_world( agent_do_moves, [Agent,Path] ).

% Case of finding nearest energy station or a position on the board
% solve_task_astar(+Start,+Task,:OList,:CList,-Path,-cost(Cost),-NewPos)
solve_task_astar(Start,Task,OList,CList,Path,cost(Cost),NewPos) :- 
  achieved(Start, Task, OList, CList, Path, Cost, NewPos).

% Case of finding nearest unseen oracle, need to keep track of seen oracles
% solve_task_astar(+Seen,+Start,+Task,:OList,:CList,-Path,-cost(Cost),-NewPos)
solve_task_astar(Seen,Start,Task,OList,CList,Path,cost(Cost),NewPos) :- 
  achieved(Seen, Start, Task, OList, CList, Path, Cost, NewPos).

achieved(P, go(Exit), OList, CList, Path, Cost, NewPos) :-
  pop(OList, c(F,Cost,H,Parent,Pos),_T),
  ( Exit = none -> true
  ; otherwise -> Exit = Pos, getpath(P,Parent,Pos,CList,[],Path)
  ). 

achieved(P,find(X),OList,CList,Path,Cost,NewPos) :-
  pop(OList,c(F,Cost,H,Parent,Pos),_),
  ( X = none -> true
  ; otherwise -> map_adjacent(Pos,_,X),
    getpath(P,Parent,Pos,CList,[],Path)
  ).  

achieved(P,find(P1),OList,CList,Path,Cost,NewPos) :-
  pop(OList,c(F,Cost,H,Parent,Pos),_),
  ( P1 = none -> true
  ; otherwise -> P1 = Pos, getpath(P,Parent,Pos,CList,[],Path)
  ). 

achieved(Seen,P,find(O),OList,CList,Path,Cost,NewPos) :-
  pop(OList,c(F,Cost,H,Parent,Pos),_),
  ( O = none -> true
  ; otherwise -> map_adjacent(Pos,_,O),
    \+ member(O,Seen), 
    getpath(P,Parent,Pos,CList,[],Path)
  ).  

getpath(Start,Parent,Start,CList,TP,TP).
getpath(Start,Parent,Pos,CList,TP,Path) :-
  append([Pos],TP,NewTP),
  memberchk(c(_,_,_,P,Parent),CList), 
  getpath(Start,P,Parent,CList,NewTP,Path).

solve_task_astar(P, Task, OList, CList, Path, Cost, NewPos) :-
  % Find node with the least f on the OList
  pop(OList, c(F,G,H,Parent,Pos), Tail),
  search(Task, G, Pos, OList, CList, Successors), 
  merge(Tail, Successors, NewOList),
  merge([c(F,G,H,Parent,Pos)], CList, NewCList), 
  solve_task_astar(P, Task, NewOList, NewCList, Path, Cost, NewPos). 

solve_task_astar(Seen,P, Task, OList, CList, Path, Cost, NewPos) :-
  % Find node with the least f on the OList
  pop(OList, c(F,G,H,Parent,Pos), Tail),
  search(Task, G, Pos, OList, CList, Successors), 
  merge(Tail, Successors, NewOList),
  merge([c(F,G,H,Parent,Pos)], CList, NewCList), 
  solve_task_astar(Seen, P, Task, NewOList, NewCList, Path, Cost, NewPos). 

pop([H|T], H, T). 

search(go(Goal), G, Pos, OList, CList, Successors) :- 
  findall( c(F,Gc,D,Pos,A),
            (map_adjacent(Pos, A, empty),
             map_distance(A, Goal, D),
             Gc is G+1,
             F is D+Gc, 
            \+ (memberchk(c(F1,_,_,_,A),OList) -> (F1 @=< F)), 
            \+ (memberchk(c(F2,_,_,_,A),CList) -> (F2 @=< F))
            ),
           Temp ),
  sort(Temp, Successors).

search(find(O),G,Pos,OList,CList,Successors) :-
  findall( c(0,Gc,0,Pos,A),
            (map_adjacent(Pos, A, empty), 
             Gc is G+1, 
            \+ memberchk(c(_,_,_,_,A),OList), 
            \+ memberchk(c(_,_,_,_,A),CList)
            ),
           Temp ),
  sort(Temp, Successors).

unpack(go(Exit), Exit).
unpack(find(O), O). 

