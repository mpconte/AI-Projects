
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%1
%Question 1a
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Primitive control actions

% go to location y from location x in the same room.
primitive_action(go(X,Y)).   

% push box B from location X to location Y in same room
primitive_action(push(B, X, Y)). %write('pushin '), write(B), write(' from '), write(X), write(' to '), write(Y)		 				   				 

% climb up on top of Box B
primitive_action(climbUp(B)). % write('goin up '), write(B), nl.    		 

% climb down on top of Box B
primitive_action(climbDown(B)).% write('goin down '), write(B), nl.  		

% turn on switch S
primitive_action(turnOn(S)). %, write('turnin on '), write(S), nl.	 

% turn off switch S
primitive_action(turnOff(S)). %, write('turnin off '), write(S), nl.	 



							 

% PreConditions for Primitive Actions


%%%%%%%%%%%%%
% go(X,Y)
%%%%%%%%%%%%%
poss(go(X,Y), State) :- in(X,R), in(Y,R), at(shakey, X, State),		
			%can't go if onTop of a box	
			((at(B,X,State), box(B), onTop(B, State), !, fail); true).		


%%%%%%%%%%%%%
% push(B,X,Y) 
%%%%%%%%%%%%%
poss(push(B,X,Y), State) :-     in(X,R), in(Y,R), at(shakey, X, State),	box(B), at(B,X,State),		
				%can't push if onTop of a box
				((onTop(B, State), !, fail); true),		

								
				%can't push box to another box's location
		  		( (box(B2), at(B2,Y,State), !, fail); true).
					

%%%%%%%%%%%%%%%
% climbUp(Box):
%%%%%%%%%%%%%%%
poss(climbUp(B), State) :- box(B), at(B,Location,State), at(shakey,Location,State), not(onTop(B,State)).


%%%%%%%%%%%%%%%%%%
% climbDown(Box):
%%%%%%%%%%%%%%%%%%
poss(climbDown(B), State) :- box(B), at(B,Location,State), at(shakey,Location,State), onTop(B,State).



%%%%%%%%%%%%%%%%%%
%turnOn(Switch)
%%%%%%%%%%%%%%%%%%
poss(turnOn(S), State) :- controls(L, S), box(B),
			  at(B,Location,State), at(shakey,Location,State), at(S,Location,State), 						  
			  onTop(B, State), off(L,State).

%%%%%%%%%%%%%%%%%%
%turnOff(Switch)
%%%%%%%%%%%%%%%%%%
poss(turnOff(S), State) :- controls(L, S), box(B),
			   at(B,Location,State), at(shakey,Location,State), at(S,Location,State),
			   onTop(B, State), on(L,State), up(S,State). 






				   
% Successor State Axioms for Primitive Fluents.


%%%%%%%%%%%%%%%%%%%%%%%%%
%% at(object, Location)
%%%%%%%%%%%%%%%%%%%%%%%%%
  
at(B, Location, do(Action,State)) :- box(B),
				     (at(B, Start, State), in(Start,R), in(Location,R), Action = push(B, Start, Location)) ; 
				     (at(B, Location, State)).
								
						
at(shakey, Location, do(Action, State)) :- (at(shakey,X,State), in(X,R), in(Location,R), box(B), Action = push(B,X,Location)); 
					   (at(shakey,X,State), in(X,R), in(Location,R), Action = go(X,Location)) ;
					   (at(shakey, Location, State)).
					   												   						 
					   	
%%%%%%%%%%%%%%%%%%%%%%
%%up/down(Switch)	
%%%%%%%%%%%%%%%%%%%%%%
	
up(Switch, do(Action,State)) :- controls(Light, Switch), at(Switch,Location,State), 
				at(Box,Location,State), 									
				(onTop(Box, State), off(Light, State), Action = turnOn(Switch)) ; 
				(onTop(Box, State), on(Light, State), up(Switch, State), not(Action = turnOff(Switch)));
				(on(Light, State), up(Switch, State)).
							
					
					
%%%%%%%%%%%%%%%%%%%%%%
%% on/off(Light)					
%%%%%%%%%%%%%%%%%%%%%%
on(Light, do(Action,State)) :- 	controls(Light, Switch), at(Switch,Location,State), 
				at(Box,Location,State), box(B),
				(onTop(Box, State), off(Light, State), Action = turnOn(Switch)) ; 
				(onTop(Box, State), on(Light, State), up(Switch,State), not(Action = turnOff(Switch)));
				(on(Light, State)).				
				
off(Light, do(Action,State)) :- controls(Light, Switch), at(Switch,Location,State), 
				at(Box,Location,State), box(B), 									
				(onTop(Box, State), on(Light, State), up(Switch, State), Action = turnOff(Switch)) ; 
				(onTop(Box, State), off(Light, State), not(Action = turnOn(Switch)));
				(off(Light, State)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%onTop(Box)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
onTop(Box, do(Action,State)) :- box(Box), 
				(onTop(Box, State), not(Action = climbDown(Box)) )								
				;
				(at(Box, Location, State), at(shakey,Location, State), Action = climbUp(Box))
				;
				(onTop(Box, State)).



%Light/Switch Initializations

up(s1, s0).
on(l1, s0).
up(s4, s0).
on(l4, s0).

off(l2, s0).
off(l3, s0).


%box constants

box(b1).
box(b2).
box(b3).
box(b4).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%ROOM LAYOUT IS AS FOLLOWS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*
________________________
|56|57|58|59|60|	|
|51|52|53|54|55|<(door) |
|46|47|48|49|50|      	|	
_____ROOM_4____|	|	
|41|42|43|44|45|	|
|36|37|38|39|40|<(door) |
|31|32|33|34|35|      	|	
_____ROOM_3____|	|	
|26|27|28|29|30|	|
|21|22|23|24|25|<(door) |
|16|17|18|19|20|      	|	
_____ROOM_2____|	|	
|11|12|13|14|15|	|
|6 |7 |8 |9 |10|<(door) |
|1 |2 |3 |4 |5 |      	|	
_____ROOM_1____|________|

switch at 15,30,45,60
*/


%shakey location implementation
locInitRobot(36).	

%initialize object locations
at(b3, 13, s0).
at(b2, 14, s0).
at(b4, 1, s0).
at(b1, 5, s0). 

at(s1, 15, _).
at(s2, 30, _).
at(s3, 45, _).
at(s4, 60, _).


at(shakey, X, s0) :- locInitRobot(X).


%map lights to switches
controls(l1, s1).
controls(l2, s2).
controls(l3, s3).
controls(l4, s4).


%maps door location for a room
room_door(r1,10).
room_door(r2,25).
room_door(r3,40).
room_door(r4,55).


%map location to room

%door locations
in(10, corridor).
in(25, corridor).
in(40, corridor).
in(55, corridor).

%other locations
in(1, r1). in(2, r1). in(3, r1). in(4, r1). in(5, r1). in(6, r1). 
in(7, r1). in(8, r1). in(9, r1). in(10, r1). in(11, r1). in(12, r1). 
in(13, r1). in(14, r1). in(15, r1). 


in(16, r2). in(17, r2). in(18, r2). in(19, r2). in(20, r2). in(21, r2). 
in(22, r2). in(23, r2). in(24, r2). in(25, r2). in(26, r2). in(27, r2). 
in(28, r2). in(29, r2). in(30, r2). 

in(31, r3). in(32, r3). in(33, r3). in(34, r3). in(35, r3). in(36, r3). 
in(37, r3). in(38, r3). in(39, r3). in(40, r3). in(41, r3). in(42, r3). 
in(43, r3). in(44, r3). in(45, r3). 

in(46, r4). in(47, r4). in(48, r4). in(49, r4). in(50, r4). in(51, r4). 
in(52, r4). in(53, r4). in(54, r4). in(55, r4). in(56, r4). in(57, r4). 
in(58, r4). in(59, r4). in(60, r4). 






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Question 2 b,c
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 				
%%%% goal situation calculus sentence: %%%%%
% box 2 in room 2
%
%[at(b2,Location) & in(Location,r2)]
%

%sequence for putting box2 in room2, check if executable and if goal

q2b :-  ifExecutable, ifGoal.

ifExecutable :- executable(do(push(b2,10,25), do(push(b2,14,10), do(go(10,14), do(go(40,10), do(go(36,40), s0)))))).

ifGoal:-	do(go(36,40) : go(40,10) : go(10,14) : push(b2,14,10) : push(b2,10,25), s0, S),
		goal(S).										



%goal predicate:
goal(State) :- 	at(b2,L,State), in(L,r2).
 

%procedure to put box2 in room2 
proc(box2InRoom2,     
		      %if shakey is not on top of a box
   		      if( at(shakey,L1) & in(L1,R1) & room_door(R1,D1) &
 		      	at(b2, L2) & in(L2,R2) & room_door(R2,D2) & room_door(r2,D3) & -onTop(B),
		      
		      % <action> go to room with box 2 and push it to room 2
		      	go(L1,D1) : go(D1,D2) : go(D2, L2) : push(b2,L2,D2) : push(b2,D2,D3),
		      
		      % else, climbDown then <action>
		      	climbDown(B) : go(L1,D1) : go(D1,D2) : go(D2, L2) : push(b2,L2,D2) : push(b2,D2,D3)
		      )
	). 


%execute procedure then show action sequence and check if result is goal
q2c :- do(box2InRoom2,s0,S), executable(S), show_act_seq(S), goal(S).
 

 
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Question 2d
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% allLightsOn procedure
% if shakey is not onTop of a box go to a box's location and turn on all lights; 
% otherwise, climbDown the box first then turn on all lights

proc(allLightsOn, if(-onTop(B), findRoomWithBox : turnOnLights, climbDown(B) : turnOnLights) ).



% turnOnLights
% support procedure -> invokes necessary actions to turn on the lights

proc(turnOnLights, 

		%while a light is off....			
		while(off(Light) & controls(Light,Switch) & at(Switch,L1) & in(L1,R1) & room_door(R1,D1) & 
		      at(shakey,L2) & in(L2,R2) & room_door(R2,D2) 
		      & at(B,L2) & box(B), 
		
			%push box to the respective off Light Switches location, climbUp, turnOn Switch then climbDown
																						
			push(B,L2,D2) : 
			push(B,D2,D1) : 
			push(B,D1,L1) : 
			climbUp(B) : 
			turnOn(Switch) : 
			climbDown(B)
		)									
).



% findRoomWithBox
% support procedure -> shakey goes to a box's location

proc(findRoomWithBox, 

		%select location of a box
		?(at(shakey, L1) & in(L1, R)  & 
		room_door(R,D) 					
		& at(B,L2) & box(B) & in(L2, R2) & 
		room_door(R2,DNext) ) 									
		: 		
		%go to box's location																							
		go(L1,D) : go(D,DNext) : go(DNext,L2) 		
).






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Question 2e
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
				
%goal (see q2b)																		
								
%acceptable Action in State:
%  if        shakey not onTop of Box and at box 2 location in State, Action = push
%  else if   shakey not onTop of Box in State, Action = go
%  else      shakey onTop of box in State, Action = climbDown
	
acceptable(Action,State) :- (at(shakey,L,State), at(b2,L,State), box(B), not(onTop(B,State)), in(L,R), in(M,R), Action = push(b2,L,M)) ; 
			    (at(shakey,L,State), not(onTop(B,State)), box(B), in(L,R), in(M,R), Action = go(L,M)) ;
			    (onTop(B,State), box(B), Action = climbDown(B)).





restoreSitArg(at(O,L), S, at(O,L,S)).
restoreSitArg(up(Switch), S, up(Switch,S)).
restoreSitArg(on(L), S, on(L,S)).
restoreSitArg(off(L), S, off(L,S)).
restoreSitArg(onTop(B), S, onTop(B,S)).

restoreSitArg(acceptable(A), S, acceptable(A,S)).
restoreSitArg(goal, S, goal(S)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% added by Yves Lesperance

show_act_seq(s0).
show_act_seq(do(A,S)):- show_act_seq(S), write(A), nl.

run:- do(control,s0,S), show_act_seq(S).

% definition of executable (legal) situation

executable(s0).
executable(do(A,S)) :- poss(A,S), executable(S).
