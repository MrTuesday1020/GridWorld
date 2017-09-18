%% 17S1 9414 Assignment3
%% Group 214
%% z5104857 Shi Xiaoyun
%% z5092923 Wang Jintao


% q1
% procedure:	trigger(Events, Goals)
% input:		Events (list) -> truffle(X,Y,S) or restaurant(X,Y,S)
% return:	 	Goals -> goals(Goals_rest,Goals_truff)		
% 			Goals_truff,Goals_rest (list) -> goal(X,Y,S) 

% base case 
trigger([],goals([],[])).

% truffle(X,Y,S) -> add into Goals_truff
trigger([truffle(X,Y,S)|Tail],goals(GoalRes,[goal(X,Y,S)|Gtail])) :-
	trigger(Tail,goals(GoalRes,Gtail)).

% restaurant(X,Y,S) -> add into Goals_rest
trigger([restaurant(X,Y,S)|Tail],goals([goal(X,Y,S)|Gtail],GoalTru)) :-
	trigger(Tail,goals(Gtail,GoalTru)).


% q2
% procedure:	incorporate_goals(Goals, Beliefs, Intentions, Intentions1)
% input:		Goals -> goals(Goals_rest,Goals_truff) -> q1
% 				Beliefs -> beliefs(at(X,Y),stock(T))
%				Intentions -> intents(Int_sell,Int_pick)
%				Int_sell, Int_pick (list) -> [goal(X,Y,S), Plan]
% return: 		Intentions1
incorporate_goals(goals(Goals_rest,Goals_truff), beliefs(at(Bx,By),stock(_)), intents(Int_sell,Int_pick), Intentions1) :-
	extract(Int_sell,Sell),
	extract(Int_pick,Pick),
	insert(Goals_rest,New_sell,Int_sell,Sell,Bx,By),
	insert(Goals_truff,New_pick,Int_pick,Pick,Bx,By),
	Intentions1 = intents(New_sell,New_pick),!.
%% insert the goals into the list
insert([],Int_sell,Int_sell,_,_,_).
%% if the goal has already in the list, do nothing.
insert([goal(X,Y,S)|Tail],Insert_int,Int_sell,Sell,Bx,By) :-
	member([X,Y,S],Sell),
	insert(Tail,Insert_int,Int_sell,Sell,Bx,By).
%% if the goal is not in the list, insert it into the list and then order the list
insert([goal(X,Y,S)|Tail],Insert_int,Int_sell,Sell,Bx,By) :-
	order([goal(X,Y,S),[]],Int_sell,New_sell,Bx,By),
	insert(Tail,Insert_int,New_sell,Sell,Bx,By).

order([goal(X,Y,S),[]],[],[[goal(X,Y,S),[]]],_,_).
%% sort the list by comparing the number of truffles
order([goal(X1,Y1,S1),[]],[[goal(X2,Y2,S2),C2]|Tail],New_sell,_,_) :-
	S1 > S2,
	New_sell = [[goal(X1,Y1,S1),[]],[goal(X2,Y2,S2),C2]|Tail].
order([goal(X1,Y1,S1),[]],[[goal(X2,Y2,S2),C2]|Tail],New_sell,Bx,By) :-
	S1 < S2,
	New_sell = [[goal(X2,Y2,S2),C2]|Tail0],
	order([goal(X1,Y1,S1),[]],Tail,Tail0,Bx,By).
%% if number of truffles are equal than compare the Manhattan distance
order([goal(X1,Y1,S1),[]],[[goal(X2,Y2,S2),C2]|Tail],New_sell,Bx,By) :-
	S1 = S2,
	distance((X1,Y1),(Bx,By),D1),
	distance((X2,Y2),(Bx,By),D2),
	D1 > D2,
	New_sell = [[goal(X1,Y1,S1),[]],[goal(X2,Y2,S2),C2]|Tail].
order([goal(X1,Y1,S1),[]],[[goal(X2,Y2,S2),C2]|Tail],New_sell,Bx,By) :-
	S1 = S2,
	distance((X1,Y1),(Bx,By),D1),
	distance((X2,Y2),(Bx,By),D2),
	D1 =< D2,
	New_sell = [[goal(X2,Y2,S2),C2]|Tail0],
	order([goal(X1,Y1,S1),[]],Tail,Tail0,Bx,By).

% base case
extract([],[]).
% get all (X,Y,S)
extract([[goal(X,Y,S),_]|Tail],[[X,Y,S]|Gtail]) :-
	extract(Tail,Gtail).


% q3
% procedure:	get_action(Beliefs, Intentions, Intentions1, Action)
% input:		Beliefs -> belief(at(X,Y),stock(T))
%				Intentions -> intents(Int_sell,Int_pick) -> q2
%					Int_sell, Int_pick (list) -> [goal(X,Y,S), Plan]
% return:		Intentions1 -> intents(Int_sell,Int_pick)
%					Int_sell, Int_pick (list) -> [goal(X,Y,S), Plan]
%					Plan -> from current position to goal location then pick or sell, any convenient way
%				Action -> the first action in plan
%% sell intention not empty and S =< T, choose this intention (Plan is empty)
get_action(Beliefs, Intentions1, Intentions2, Action):-
	Beliefs = beliefs(at(Px,Py),stock(T)),
	Intentions1 = intents(Int_sell,Int_pick),
	not(Int_sell = []),
	Int_sell = [First_Goal|Rest_Goal],
	First_Goal = [goal(X,Y,S),[]],
	S =< T,
	plan_sell((Px,Py),(X,Y),List),
	List = [Selected_action|Rest_action],
	applicable(Selected_action),
	copy_list(Rest_action,New_Plan),
	Intentions2 = intents([[goal(X,Y,S),New_Plan]|Rest_Goal],Int_pick),
	Action = Selected_action,!.
%% sell intention not empty and S =< T, choose this intention (Plan is not empty)
get_action(Beliefs, Intentions1, Intentions2, Action):-
	Beliefs = beliefs(at(_,_),stock(T)),
	Intentions1 = intents(Int_sell,Int_pick),
	not(Int_sell = []),
	Int_sell = [First_Goal|Rest_Goal],
	First_Goal = [goal(X,Y,S),Plan],
	S =< T,
	Plan = [Selected_action|Rest_action],
	applicable(Selected_action),
	Intentions2 = intents([[goal(X,Y,S),Rest_action]|Rest_Goal],Int_pick),
	Action = Selected_action,!.
%% sell intentions is empty or S > T, choose this intention (Plan is empty)
get_action(Beliefs, Intentions1, Intentions2, Action):-
	Beliefs = beliefs(at(Px,Py),stock(_)),
	Intentions1 = intents(Int_sell,Int_pick),
	not(Int_pick = []),
	Int_pick = [First_Goal|Rest_Goal],
	First_Goal = [goal(X,Y,S),[]],
	plan_pick((Px,Py),(X,Y),List),
	List = [Selected_action|Rest_action],
	applicable(Selected_action),
	copy_list(Rest_action,New_Plan),
	Intentions2 = intents(Int_sell,[[goal(X,Y,S),New_Plan]|Rest_Goal]),
	Action = Selected_action,!.
%% sell intentions is empty or S > T, choose this intention (Plan is not empty)
get_action(Beliefs, Intentions1, Intentions2, Action):-
	Beliefs = beliefs(at(_,_),stock(_)),
	Intentions1 = intents(Int_sell,Int_pick),
	not(Int_pick = []),
	Int_pick = [First_Goal|Rest_Goal],
	First_Goal = [goal(X,Y,S),Plan],
	Plan = [Selected_action|Rest_action],
	applicable(Selected_action),
	Intentions2 = intents(Int_sell,[[goal(X,Y,S),Rest_action]|Rest_Goal]),
	Action = Selected_action,!.
%% stand still, action is move(X,Y)
get_action(Beliefs, Intentions1, Intentions2, Action):-
	Beliefs = beliefs(at(X,Y),stock(_)),
	Intentions2 = Intentions1,
	Action = move(X,Y),!.

copy_list([],[]).
copy_list([H|T1],[H|T2]) :- copy_list(T1,T2).

%% make the selling plan
plan_sell((X,Y),(X,Y),[sell(X,Y)]).
plan_sell((Px,Py),(X,Y),List) :-
	Px - X > 0 ,
	Nx is Px - 1,
	List = [move(Nx,Py)|Glist], 
	plan_sell((Nx,Py),(X,Y),Glist).
plan_sell((Px,Py),(X,Y),List) :-
	Px - X < 0,
	Nx is Px + 1, 
	List = [move(Nx,Py)|Glist], 
	plan_sell((Nx,Py),(X,Y),Glist).
plan_sell((Px,Py),(X,Y),List) :-
	Px =:= X,
	Py - Y > 0,
	Ny is Py - 1,
	List = [move(X,Ny)|Glist], 
	plan_sell((X,Ny),(X,Y),Glist).
plan_sell((Px,Py),(X,Y),List) :-
	Px =:= X,
	Py - Y < 0,
	Ny is Py + 1,
	List = [move(X,Ny)|Glist],
	plan_sell((X,Ny),(X,Y),Glist).
%% make the picking plan
plan_pick((X,Y),(X,Y),[pick(X,Y)]).
plan_pick((Px,Py),(X,Y),List) :-
	Px - X > 0 ,
	Nx is Px - 1,
	List = [move(Nx,Py)|Glist], 
	plan_pick((Nx,Py),(X,Y),Glist).
plan_pick((Px,Py),(X,Y),List) :-
	Px - X < 0,
	Nx is Px + 1, 
	List = [move(Nx,Py)|Glist], 
	plan_pick((Nx,Py),(X,Y),Glist).
plan_pick((Px,Py),(X,Y),List) :-
	Px =:= X,
	Py - Y > 0,
	Ny is Py - 1,
	List = [move(X,Ny)|Glist], 
	plan_pick((X,Ny),(X,Y),Glist).
plan_pick((Px,Py),(X,Y),List) :-
	Px =:= X,
	Py - Y < 0,
	Ny is Py + 1,
	List = [move(X,Ny)|Glist],
	plan_pick((X,Ny),(X,Y),Glist).


%% Q4
%% if at(X,Y) - the agent should believe it is at(X,Y)
%% if picked(X,Y,S) - stock(T) changes to stock(T1) where T1 is T+S
%% if sold(X,Y,S) - stock(T) changes to stock(T1) where T1 is T-S
update_beliefs(Observation, Beliefs, Beliefs1):-
	Observation = at(X,Y),
	Beliefs = beliefs(at(_,_),stock(T)),
	Beliefs1 = beliefs(at(X,Y),stock(T)).

update_beliefs(Observation, Beliefs, Beliefs1):-
	Observation = picked(X,Y,S),
	Beliefs = beliefs(at(_,_),stock(T)),
	T1 is T + S,
	Beliefs1 = beliefs(at(X,Y),stock(T1)).

update_beliefs(Observation, Beliefs, Beliefs1):-
	Observation = sold(X,Y,S),
	Beliefs = beliefs(at(_,_),stock(T)),
	T1 is T - S,
	Beliefs1 = beliefs(at(X,Y),stock(T1)).


%% Q5
%% at (x,y) do not change the agent's intentions
%% picked(x,y,s) or sold(x,y,s),remove the corresponding plan from the list of intentions
update_intentions(Observation, Intentions2, Intentions3):-
	Observation = at(_,_),
	Intentions3 = Intentions2.

update_intentions(Observation, Intentions2, Intentions3):-
	Observation = picked(X,Y,S),
	Intentions2 = intents(Int_sell,Int_pick),
	delete(Int_pick,[goal(X,Y,S),[]],New_Int_pick),
	Intentions3 = intents(Int_sell,New_Int_pick).

update_intentions(Observation, Intentions2, Intentions3):-
	Observation = sold(X,Y,S),
	Intentions2 = intents(Int_sell,Int_pick),
	delete(Int_sell,[goal(X,Y,S),[]],New_Int_sell),
	Intentions3 = intents(New_Int_sell,Int_pick).

