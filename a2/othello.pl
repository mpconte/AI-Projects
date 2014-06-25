/* ----------------------------------------------------------
    CSE 3402 W07 Assignment 2 file

% Surname: 	  Conte
% First Name:	  Matthew
% Student Number: 207333230

  ------------------------------------------------------ */

%do not chagne the follwoing line!
%:- ensure_loaded('abplay.pl').

% /* ------------------------------------------------------ */
%               IMPORTANT! PLEASE READ THIS SUMMARY:
%       This files gives you some useful helpers (set & get).
%       Your job is to implement several predicates using
%       these helpers (feel free to add your own helpers if needed,
%       MAKE SURE to write comments for all your helpers, marks will
%       be deducted for bad style!).
%
%       Implement the following predicates at their designated space
%       in this file (we suggest to have a look at file ttt.pl  to
%       see how the implementations is done for game tic-tac-toe.
%
%          * initialize(InitialState,InitialPlyr).
%          * winner(State,Plyr) 
%          * tie(State)
%          * terminal(State) 
%          * moves(Plyr,State,MvList)
%          * nextState(Plyr,Move,State,NewState,NextPlyr)
%          * validmove(Plyr,State,Proposed)
%          * h(State,Val)  (see question 2 in the handout)
%          * lowerBound(B)
%          * upperBound(B)
% /* ------------------------------------------------------ */







% /* ------------------------------------------------------ */

% We use the following State Representation: 
% [Row0, Row1 ... Rown] (ours is 6x6 so n = 5 ).
% each Rowi is a LIST of 6 elements '.' or '1' or '2' as follows: 
%    . means the position is  empty
%    1 means player one has a stone in this position
%    2 means player two has a stone in this position. 



% given helper: Inital state of the board 
initBoard([ [.,.,.,.,.,.], 
            [.,.,.,.,.,.],
	        [.,.,1,2,.,.], 
	        [.,.,2,1,.,.], 
            [.,.,.,.,.,.], 
	        [.,.,.,.,.,.] ]).
 
fullBoard1([
			[.,1,1,1,2,1],
			[1,2,2,2,1,1],
			[1,1,1,1,1,1],
			[2,2,2,2,2,2],
			[2,2,2,2,1,1],	    
			[1,1,1,1,2,2] 
			]).
			
			
	    
%%%%%%%%%%%%%%%%%% IMPLEMENT: initialize(...)%%%%%%%%%%%%%%%%%%%%%
%%% Using initBoard define initialize(InitialState,InitialPlyr). 
%%%  holds iff InitialState is the initial state and 
%%%  InitialPlyr is the player who moves first. 

initialize(Init, 1) :- initBoard(Init).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%winner(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define winner(State,Plyr) here.  
%     - returns winning player if State is a terminal position and
%     Plyr has a higher score than the other player 

winner(State, Playr) :- winner(State, Playr, [0,0], 0, 0).

%finished scanning and all spaces are filled
winner(_ , 1, [6,5], P1, P2) :-  P2 > P1.
winner(_ , 2, [6,5], P1, P2) :-  !, P1 > P2.

%search next row
winner(State, P, [6,Y], P1, P2) :- NewY is Y + 1, !, winner(State, P, [0,NewY], P1, P2).


%if P1 stone is found, add to P1 Stone accumulator
winner(State, P, [X,Y], P1, P2) :- get(State,[X,Y],1),  NewP1 is P1 + 1, !, NewX is X + 1, 
				winner(State, P, [NewX,Y], NewP1, P2).
			    
%if P2 stone is found, add to P2 Stone accumulator
winner(State, P, [X,Y], P1, P2) :- get(State,[X,Y],2),  NewP2 is P2 + 1, !, NewX is X + 1, 
				winner(State, P, [NewX,Y], P1, NewP2).

%else, no stone was found, exit and fail predicate (not terminal state)
winner(State, _, [X,Y], _, _) :- get(State,[X,Y],V), V = '.', !, fail.
			    	




%%%%%%%%%%%%%%%%%%%%%%%%%%%%tie(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define tie(State) here. 
%    - true if terminal State is a "tie" (no winner) 

tie(State) :- tie(State, [0,0], 0, 0).

%finished scanning and all spaces are filled
tie(_ , [6,5], P1, P2) :-  !, P2 = P1.

%search next row
tie(State, [6,Y], P1, P2) :- NewY is Y + 1, !, tie(State, [0,NewY], P1, P2).


%if P1 stone is found, add to P1 Stone accumulator
tie(State, [X,Y], P1, P2) :- get(State,[X,Y],1),  NewP1 is P1 + 1, !, NewX is X + 1, 
						tie(State, [NewX,Y], NewP1, P2).
			    
%if P2 stone is found, add to P2 Stone accumulator
tie(State, [X,Y], P1, P2) :- get(State,[X,Y],2),  NewP2 is P2 + 1, !, NewX is X + 1, 
						tie(State, [NewX,Y], P1, NewP2).

%else, no stone was found, exit and fail predicate (not terminal)
tie(State, [X,Y], _, _) :- get(State,[X,Y],V), V = '.', !, fail.





%%%%%%%%%%%%%%%%%%%%%%%%%%%%terminal(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define terminal(State). 
%   - true if State is a terminal   

%terminal must be either a winning or tie state
terminal(State) :- ( winner(State, _) ; tie(State) ).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%showState(State)%%%%%%%%%%%%%%%%%%%%%%%%%%
%% given helper. DO NOT  change this. It's used by play.pl
%% 
showState( G ) :- 
	printRows( G ). 
 
printRows( [] ). 
printRows( [H|L] ) :- 
	printList(H),
	nl,
	printRows(L). 

printList([]).
printList([H | L]) :-
	write(H),
	write(' '),
	printList(L).


	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%moves(Plyr,State,MvList)%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define moves(Plyr,State,MvList). 
%   - returns list MvList of all legal moves Plyr can make in State
%


%if no moves found, return [n]
moves(Plyr,State,MvList) :- moves(Plyr,State,List,[0,0]), List = [], !, 
							  MvList = [n].
															
%else, return list given
moves(Plyr,State,MvList) :- moves(Plyr,State,MvList,[0,0]).
		
				
	
%(base case) Stop searching here
moves(_,_,[],[5,6]) :- !.

%begin scanning next column 	
moves(Plyr,State,A,[X,6]) :- 	!, NewX is X + 1, !, moves(Plyr,State,A,[NewX,0]).
																 

%find opponent stone to the left of current blank spot																 								 
moves(Plyr,State,[A|B],[X,Y]) :-  	get(State, [X,Y], E), E = '.', CheckX is X - 1, 
					get(State, [CheckX, Y], U), 
					not(U = '.'), not(U = Plyr), A = [X,Y], !, 
					NewY is Y + 1, 
					moves(Plyr,State,B,[X, NewY]).
									
%find opponent stone to the right of current blank spot									
moves(Plyr,State,[A|B],[X,Y]) :-  	get(State, [X,Y], E), E = '.', CheckX is X + 1, 
					get(State, [CheckX, Y], U), 
					not(U = '.'), not(U = Plyr), A = [X,Y], !, 
					NewY is Y + 1, 
					moves(Plyr,State,B,[X, NewY]).
																												
%find opponent stone to the bottom of current blank spot										 
moves(Plyr,State,[A|B],[X,Y]) :-  	get(State, [X,Y], E), E = '.', CheckY is Y + 1, 
					get(State, [X, CheckY], U), 
					not(U = '.'), not(U = Plyr), A = [X,Y], !, 
					NewY is Y + 1, 
					moves(Plyr,State,B,[X, NewY]).	
																		   
%find opponent stone to the top of current blank spot
moves(Plyr,State,[A|B],[X,Y]) :-  	get(State, [X,Y], E), E = '.', CheckY is Y - 1, 
					get(State, [X, CheckY], U), 
					not(U = '.'), not(U = Plyr), A = [X,Y], !, 
					NewY is Y + 1, 
					moves(Plyr,State,B,[X, NewY]).

%if no opponent stone found adjacent to current spot, continue searching
moves(Plyr,State,B,[X,Y]) :- !, NewY is Y + 1, moves(Plyr,State,B,[X, NewY]).																																																				  
																  




%%%%%%%%%%%%%%nextState(Plyr,Move,State,NewState,NextPlyr)%%%%%%%%%%%%%%%%%%%%
%% 
%% define nextState(Plyr,Move,State,NewState,NextPlyr). 
%   - given that Plyr makes Move in State, it determines NewState (i.e. the next 
%     state) and NextPlayer (i.e. the next player who will move).
%

%First Case:    if no moves are possible, NewState is unchanged State and 
%				NextPlyr is opposite of Plyr otherwise, Second Case, 
nextState(1,_,State,NewState,2) :- moves(1, State, L), L = [n], !, NewState = State.
nextState(2,_,State,NewState,1) :- moves(2, State, L), L = [n], !, NewState = State.


%Second Case:    NewState is State after Move and NextPlyr is opposite of Plyr
nextState(1,Move,State,NewState,2) :- !, set(State,New,Move,1), !, 
					 alter(1, New, Move, NewState).
					 
nextState(2,Move,State,NewState,1) :- !, set(State,New,Move,2), !, 
					 alter(2, New, Move, NewState).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%alter(Plyr, State, [X,Y], NewState)%%%%%
%%
%% define alter(Plyr, State, [X,Y], NewState)
%%
%%-helper predicate; NewState is State with move [X,Y] by Plyr
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

alter(Plyr, State, [X,Y], F7) :- YDown is Y + 1, YUp is Y - 1, XRight is X + 1, XLeft is X - 1,
											
					(
					%check all valid "turnable" stones above inserted stone and update
					(checkUp(Plyr,State,[X,YUp],S1), T1 = S1) ;
					T1 = []
					),
					!, 
					update(Plyr, T1, State, _, F),											 											 											 
					(		
								 									 
					 %check all valid "turnable" stones below inserted stone and update
					 (checkDown(Plyr,F,[X,YDown],S2), T2 = S2) ;
					 T2 = []
					 ),											 
					 !,
					 update(Plyr, T2, F, _, F1),									 																					 																					 
					 (											 
									 
					 %check all valid "turnable" stones right of inserted stone and update
					 (checkRight(Plyr,F1,[XRight,Y],S3), T3 = S3) ;
					 T3 = []
					 ),
					 !,
					 update(Plyr, T3, F1, _, F2),											 																						 
					 (											 
								 
					 %check all valid "turnable" stones left of inserted stone and update
					 (checkLeft(Plyr,F2,[XLeft,Y],S4), T4 = S4) ;
					 T4 = []
					 ),
					 !,
					 update(Plyr, T4, F2, _, F3),												 
					 (											 											 
											 
					 %check all valid "turnable" stones Up-Left of inserted stone and update
					 (checkUpLeft(Plyr,F3,[XLeft,YUp],S5), T5 = S5) ;
					 T5 = []
					 ),
					 !,
					 update(Plyr, T5, F3, _, F4),											 																						 
					 (											 
									 
					 %check all valid "turnable" stones Down-Left of inserted stone and update
					 (checkDownLeft(Plyr,F4,[XLeft,YDown],S6), T6 = S6) ;
					 T6 = []
					 ),
					 !,
					 update(Plyr, T6, F4, _, F5),											 																					 																						 
					 (
									 
					 %check all valid "turnable" stones Up-Right of inserted stone and update											 
					 (checkUpRight(Plyr,F5,[XRight,YUp],S7), T7 = S7) ;
					 T7 = []
					 ),
					 !,
					 update(Plyr, T7, F5, _, F6),											 																						 
					 (											 
								 
					 %check all valid "turnable" stones Down-Right of inserted stone and update
					 (checkDownRight(Plyr,F6,[XRight,YDown],S8), T8 = S8) ;
					 T8 = []
					 ),											 
					 !, update(Plyr, T8,F6, _, F7).								 						 											 								
											
											 
%%%%%%%%%%%%%%CheckZ(Plyr. State, [X,Y], Stock)%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%define CheckZ(Plyr, State, [X,Y], Stock)
%%-helper predicate; stores all "turnable" opponent stones located Z from spot [X,Y] 
%%%%%%%%%%%%%%%%%%%% and stores them in Stock fails if a blank spot is found
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%										 

											 											 
checkUp(Plyr, State, [X,Y], []) :- 	    get(State, [X,Y], E), E = Plyr.
checkUp(_, State, [X,Y], _) :- 		    get(State, [X,Y], E), E = '.', !, fail.
checkUp(Plyr, State, [X,Y], [A|B]) :- 	    get(State, [X,Y], _), A = [X,Y], NewY is Y - 1, 
					    checkUp(Plyr,State,[X,NewY],B).											 
											 
checkDown(Plyr, State, [X,Y], []) :- 	    get(State, [X,Y], E), E = Plyr.
checkDown(_, State, [X,Y], _) :- 	    get(State, [X,Y], E), E = '.', !, fail.
checkDown(Plyr, State, [X,Y], [A|B]) :-     get(State, [X,Y], _), A = [X,Y], NewY is Y + 1, 
					    checkDown(Plyr,State,[X,NewY],B).											 
										 
checkRight(Plyr, State, [X,Y], []) :- 	    get(State, [X,Y], E), E = Plyr.
checkRight(_, State, [X,Y], _) :-           get(State, [X,Y], E), E = '.', !, fail.
checkRight(Plyr, State, [X,Y], [A|B]) :-    get(State, [X,Y], _), A = [X,Y], NewX is X + 1, 
					    checkRight(Plyr,State,[NewX,Y],B).	
											 											 
checkLeft(Plyr, State, [X,Y], []) :- 	    get(State, [X,Y], E), E = Plyr.
checkLeft(_, State, [X,Y], _) :- 	    get(State, [X,Y], E), E = '.', !, fail.
checkLeft(Plyr, State, [X,Y], [A|B]) :-     get(State, [X,Y], _), A = [X,Y], NewX is X - 1, 
					    checkLeft(Plyr,State,[NewX,Y],B).

											 
checkUpRight(Plyr, State, [X,Y], []) :-     get(State, [X,Y], E), E = Plyr.
checkUpRight(_, State, [X,Y], _) :-         get(State, [X,Y], E), E = '.', !, fail.
checkUpRight(Plyr, State, [X,Y], [A|B]) :-  get(State, [X,Y], _), A = [X,Y], 
					    NewY is Y - 1, NewX is X + 1,
					    checkUpRight(Plyr,State,[NewX,NewY],B).											 
											 
checkDownRight(Plyr, State, [X,Y], []) :-    get(State, [X,Y], E), E = Plyr.
checkDownRight(_, State, [X,Y], _) :-        get(State, [X,Y], E), E = '.', !, fail.
checkDownRight(Plyr, State, [X,Y], [A|B]) :- get(State, [X,Y], _), A = [X,Y], 
					     NewY is Y + 1, NewX is X + 1,
					     checkDownRight(Plyr,State,[NewX,NewY],B).											 
										 
checkUpLeft(Plyr, State, [X,Y], []) :- 	    get(State, [X,Y], E), E = Plyr.
checkUpLeft(_, State, [X,Y], _) :- 	    get(State, [X,Y], E), E = '.', !, fail.
checkUpLeft(Plyr, State, [X,Y], [A|B]) :-   get(State, [X,Y], _), A = [X,Y], 
					    NewY is Y - 1, NewX is X - 1,
					    checkUpLeft(Plyr,State,[NewX,NewY],B).		
											 											 
checkDownLeft(Plyr, State, [X,Y], []) :-    get(State, [X,Y], E), E = Plyr.
checkDownLeft(_, State, [X,Y], _) :- 	    get(State, [X,Y], E), E = '.', !, fail.
checkDownLeft(Plyr, State, [X,Y], [A|B]) :- get(State, [X,Y], _), A = [X,Y], 
					    NewY is Y + 1, NewX is X - 1,
					    checkDownLeft(Plyr,State,[NewX,NewY],B).											 

											
											
											 

%%%%%%%%%%%%%%%%%%update(Plyr,Stock,State,NewState,Final)%%%%%%%%%%%%%%%%%%%%
%%define update(Plyr,Stock,State,NewState,Final)
%%-helper predicate; Final is State after setting all points in Stock
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
update(_, [], New, _, New).										  
update(Plyr, [A|B], State, NewState, Final) :- set(State,NewState,A,Plyr), !, 
  					       update(Plyr, B, NewState, _, Final).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%validmove(Plyr,State,Proposed)%%%%%%%%%%%%%%%%%%%%
%% 
%% define validmove(Plyr,State,Proposed). 
%   - true if Proposed move by Plyr is valid in State.

validmove(Plyr, State, Proposed) :- moves(Plyr, State, L),  find(Proposed, L).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%find(X, List).
%helper predicate; true if X exists in List
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
find(_, []) :- !, fail.
find(X, [X|_]) :- !.
find(X, [_|Other]) :- !, find(X, Other).

																												


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%lowerBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define lowerBound(B).  
%   - returns a value B that is less than the actual or heuristic value
%     of all states.

lowerBound(-100).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%upperBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define upperBound(B). 
%   - returns a value B that is greater than the actual or heuristic value
%     of all states.

upperBound(100).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                       %
%                                                                       %
%                Given   UTILITIES                                      %
%                   do NOT change these!                                %
%                                                                       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get(Board, Point, Element)
%    : get the contents of the board at position column X and row Y
% set(Board, NewBoard, [X, Y], Value):
%    : set Value at column X row Y in Board and bind resulting grid to NewBoard
%
% The origin of the board is in the upper left corner with an index of
% [0,0], the upper right hand corner has index [5,0], the lower left
% hand corner has index [0,5], the lower right hand corner has index
% [5,5] (on a 6x6 board).
%
% Example
% ?- initBoard(B), showState(B), get(B, [2,3], Value). 
%. . . . . . 
%. . . . . . 
%. . 1 2 . . 
%. . 2 1 . . 
%. . . . . . 
%. . . . . . 
%
%B = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], 
%     ['.', '.', 1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], 
%     ['.', '.', '.', '.'|...], ['.', '.', '.'|...]]
%Value = 2 
%Yes
%?- 
%
% Setting values on the board
% ?- initBoard(B),  showState(B),set(B, NB1, [2,4], 1), set(NB1, NB2, [2,3], 1),  showState(NB2). 
%
% . . . . . . 
% . . . . . . 
% . . 1 2 . . 
% . . 2 1 . . 
% . . . . . . 
% . . . . . .
% 
% . . . . . . 
% . . . . . . 
% . . 1 2 . . 
% . . 1 1 . . 
% . . 1 . . . 
% . . . . . .
%
%B = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.', 
%1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], ['.', '.', '.', '.'|...], ['.', '.',
% '.'|...]]
%NB1 = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.'
%, 1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], ['.', '.', 1, '.'|...], ['.', '.
%', '.'|...]]
%NB2 = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.'
%, 1, 2, '.', '.'], ['.', '.', 1, 1, '.'|...], ['.', '.', 1, '.'|...], ['.', 
%'.', '.'|...]]

% get(Board, Point, Element): get the value of the board at position
% column X and row Y (indexing starts at 0).
get( Board, [X, Y], Value) :- 
	nth0( Y, Board, ListY), 
	nth0( X, ListY, Value).
 
% set( Board, NewBoard, [X, Y], Value) 

set( [Row|RestRows], [NewRow|RestRows], [X, 0], Value)
    :- setInList(Row, NewRow, X, Value). 

set( [Row|RestRows], [Row|NewRestRows], [X, Y], Value) :- 
	Y > 0, 
	Y1 is Y-1, 
	set( RestRows, NewRestRows, [X, Y1], Value). 

% setInList( List, NewList, Index, Value) 

setInList( [_|RestList], [Value|RestList], 0, Value). 

setInList( [Element|RestList], [Element|NewRestList], Index, Value) :- 
	Index > 0, 
	Index1 is Index-1, 
	setInList( RestList, NewRestList, Index1, Value). 
 






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%h(State,Val)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define h(State,Val). 
%   - given State, returns heuristic Val of that state
%   - larger values are good for Max, smaller values are good for Min
%   NOTE1. If State is terminal h should return its true value.
%   NOTE2. If State is not terminal h should be an estimate of
%          the value of state (see handout on ideas about
%          good heuristics.


%%%%%%terminal states%%%%%%%%%%%%%%%%%%%%%%
%assign true value to the winner or
%assign 0 if tie occurs 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
h(State, Val) :- winner(State, 1), !, Val = 36.  
h(State, Val) :- winner(State, 2), !, Val = -36.  
h(State, Val) :- tie(State), !, Val = 0.  


%%%%%%%%%%%%%%%%%%%%%%%%
%for non-terminal states, heuristic returns NumOpponentStones - NumPlayerStones
%%%%%%%%%%%%%%%%%%%%%%%%
h(State,Val) :- !, h(State, [0,0], 0, Val).

%finished scanning (base case)
h(_ , [6,5], P, P) :- !.

%search next row
h(State, [6,Y], P1, P2) :- NewY is Y + 1, !, h(State, [0,NewY], P1, P2).


%if P1 stone is found
h(State, [X,Y], P1, P2) :- get(State,[X,Y],1),  NewP1 is P1 - 1, !, NewX is X + 1, 
						h(State, [NewX,Y], NewP1, P2).
			    
%if P2 stone is found
h(State, [X,Y], P1, P2) :- get(State,[X,Y],2),  NewP1 is P1 + 1, !, NewX is X + 1, 
						h(State, [NewX,Y], NewP1, P2).

%if blank is found
h(State, [X,Y], P1,P2) :- NewX is X + 1, !, h(State, [NewX,Y], P1, P2).
