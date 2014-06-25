/** ---------------------------------------------------------

CSE 3402 Assignment 1

Family name: Conte

Given name: Matthew

Student number: 207333230



---------------------------------------------------------- */

/* load the three search algorithms */
:- ensure_loaded('astar.pl').
:- ensure_loaded('astarCC.pl').
:- ensure_loaded('idastar.pl').

/* ------------------------------------------------------- */

/* successors( +State, -Neighbors)

   Neighbors is a list of elements (Cost, NewState) where
   NewState is a state reachable from State by one action and
   Cost is the cost for that corresponding action (=1 in our
   case)
*/   

successors(State, Z) :- 	successors(State , Y, -1), removeLastEmpty(Y, Z), !. % 


%find a left slide, 
successors(State, [X|Y], -1) :-	successors(State, V, 0, _, -1), !, 
				((not(State = V), X = (1, V), !, successors(State, Y, 1));
				 !, successors(State, [X|Y], 1)).							
%find an up slide
successors(State, [X|Y], 1)	:-  successors(State, V, 0, _, 1), !, 
				((not(State = V), X = (1, V), !, successors(State, Y, -2));
				!, successors(State, [X|Y], -2)).
%find an down slide						
successors(State, [X|Y], -2) :- successors(State, V, 0, _, -2), !, 
				((not(State = V), X = (1, V), !, successors(State, Y, 2));
				!, successors(State, [X|Y], 2)).
								
%find a right slide	
successors(State, X, 2) :- successors(State, V, 0, _, 2), !,      
				((not(State = V), X = [Y|Z], Y = (1, V), Z = []);															
			         !, X = [[]]).

								       
	% removeLastEmpty(X, Y)
	% Y = X with [] element removed at the end, if any, 
	%since certain cases above may be in this situation
	removeLastEmpty([], []) :- !.
	removeLastEmpty([X|Y], B) :- X = [], Y = [], !, removeLastEmpty(Y, B).
	removeLastEmpty([X|Y], [X|B]) :- !, removeLastEmpty(Y, B).




/*							
successors(State, X, N, V, C)
 
	helper successors predicate, 
	
	N specifies whether a spot has been found; 0 =  not found, 1 = found   
	V is the value to contain if a slide is found and 
	  need to adjust when we meet the blank, to change V
	
	C find if a certain slide is possible,  C = -1, find left slide
						       C =  1, find up slide	
						       C = -2, find down slide
						       C =  2, find right slide
 	 if it's possible, X = state of resulting slide
	 else X = State (original unmoved)					
*/							
				
%base case 				
successors([], [], _, _, _).
successors([], [], _, _).

%reached end of a row, continue to next row
successors([[]| X], [[]| Y], C, V, C1) :- successors(X,Y,C,V,C1).
successors([[]| X], [[]| Y], C, V) :- successors(X,Y,C,V).


%%%%%%Cases where blank is encountered%%%%%%%%%%%%%%%%%%%%%%

%check if value right of blank
successors([X|Y], [A|B], 0, _, -1) :- 	X = [x|X2], not(X2 = []), X2 = [M|_], A = [M|A2], 
					
					successors([X2|Y], [A2|B], 1, M).
										

%check for value below blank				
successors([X|Y], [A|B], 0, _, 1) :- 	X = [x|X2], length(X, N1), not(Y = []), Y = [Y1|_], A = [A1|A2], 
										
					checkBlank(N1, Y1, Value), not(Value = -1), A1 = Value, 
					
					successors([X2|Y], [A2|B], 1, Value).						
										
										
	
%%%%%%Cases where left to or above blank is encountered%%%%%%%%%%							
							
%check blank right of value
successors([X|Y], [A|B], 0, _, 2) :- X = [X1|X2], not(X2 = []), X2 = [x|_], A = [x|A2], 
								  
				    successors([X2|Y], [A2|B], 1, X1).																																																						
														
											
%check for a blank below value
successors([X|Y], [A|B], 0, _, -2) :- X = [X1|X2], not(X1 = x), length(X, N), 
				     not(Y = []), Y = [Y1|_], A = [A1|A2],																										 												
							
												
						%if blank is found below current element						
						not(checkBlank(N, Y1, X1)),
						A1 = x, successors([X2|Y], [A2|B], 1, X1).  														

						

%else							
successors([X|Y], [A|B], 0, V, C) :-  X = [X1|X2], A = [X1|A2], 
				
				      successors([X2|Y], [A2|B], 0, V, C). 
							
						


%%When a slide is found, invoke this predicate									
successors([X|Y], [A|B], 1, V) :-    X = [X1|X2], A = [A1|A2], 	
					
				(								 																
					 (X1 = x, !, A1 = V, successors([X2|Y], [A2|B], 1, V));
								 
					 (X1 = V, !, A1 = x, successors([X2|Y], [A2|B], 1, V));
								 
					 (A1 = X1, !, successors([X2|Y], [A2|B], 1, V))
				).
								
											

			
/*
checkBlank(X, A, C)

for finding a blank above a below and finding a value below a found blank.

%return false if a blank is found below element.
%return true otherwise
*/
checkBlank(_, [], _).
				
checkBlank(N, X, _) :-  length(X, N), X = [x|_],  !, fail.

checkBlank(N, X, Value) :-  length(X, N), X = [X1|_], not(X1 = x), Value = X1.  														

checkBlank(N, [_|Y], Value) :- checkBlank(N, Y, Value).
					

/* ------------------------------------------------------- */

/* equality(+S1, +S2)

   holds if and only S1 and S2 describe the same state
*/      
equality(S,S).


/* ------------------------------------------------------- */

/* hfn_null( +State, -V)

   V is the null heuristic for State (=0 irrelevant of the state)
*/
hfn_null(_State, 0).



/* hfn_misplaced( +State, -V)

   V is the number of misplaced tiles in State   
*/

hfn_misplaced(State, V) :-  !, hfn_misplaced(State, V, 0, 0).

/*
hfn_misplaced(State, V, N, C).

	helper predicate for hfn_misplaced(State, V)
	
	N = counter to find correct value of square
	C = heuristic counter, increments for each misplaced square found
	V = C
*/
%base case
hfn_misplaced([], V, _, X) :- !, V is X - 1.

%end of row is reached, continue to next row
hfn_misplaced([[]|X], V, N, C) :- !, hfn_misplaced(X,V,N,C).


%if a blank is found, increment counter and continue scanning
hfn_misplaced([X|Y], V, N, C) :- X = [x|X2], N1 is N + 1, C1 is C + 1, !, hfn_misplaced([X2|Y],V,N1,C1). 

%if correct square is found in current spot, continue scanning
hfn_misplaced([X|Y], V, N, C) :- X = [X1|X2], N1 is N + 1, X1 = N1, !, hfn_misplaced([X2|Y], V, N1, C).
								 
%if incorrect square is found in current spot, increment counter and continue scanning								
hfn_misplaced([X|Y], V, N, C) :- X = [_|X2], N1 is N + 1, C1 is C + 1, !, hfn_misplaced([X2|Y], V, N1, C1). 






/* hfn_manhattan( +State, -V)

   V is the sum over the manhattan distances between the current
   and the designated position of each tile
*/

hfn_manhattan( State, V ) :-  length(State, L), hfn_manhattan(State, V, 0, 0, L, 1, 0).


hfn_manhattan([], X, _, X, _, _, 0).

hfn_manhattan([[]|X], V, N, T, L, RowNum, 0) :- NextRow is RowNum + 1, !, hfn_manhattan(X, V, N, T, L, NextRow, 0).



%used to traverse down each row to find the values current location
hfn_manhattan(X1, V, N, T, L, RowNum, -1) :-	

				
				C is (RowNum * L) + 1, D is (L * (RowNum - 1)) + 1, 
	
				(												
				X1 >= D, X1 < C, 																																		
					(
				 	  (X1 >= N , NewT is T + (X1 - N), !, hfn_manhattan([], V, N, NewT, L, RowNum, 0) ) 											
					; (X1 <  N,  NewT is T + (N - X1), !, hfn_manhattan([], V, N, NewT, L, RowNum, 0) )
					)
				;
						
				NewT is T + 1, NewRow is RowNum - 1, N1 is N - L,
				!, hfn_manhattan(X1, V, N1, NewT, L, NewRow, -1) 																																				 												  												
				).

												

%used to traverse up each row to find the values current location
hfn_manhattan(X1, V, N, T, L, RowNum, 1) :-		
																											
				C is (RowNum * L) + 1, D is (L * (RowNum - 1)) + 1, 
										
				(												
				X1 >= D, X1 < C, 
					(
				  	  (X1 >= N , NewT is T + (X1 - N), !, hfn_manhattan([], V, N, NewT, L, RowNum, 0) ) 
					; (X1 <  N,  NewT is T + (N - X1), !, hfn_manhattan([], V, N, NewT, L, RowNum, 0) ) 
					)
										
				;
												
				NewT is T + 1, NewRow is RowNum + 1, N1 is N + L,
				!, hfn_manhattan(X1, V, N1, NewT, L, NewRow, 1)																																				  
				).


											
hfn_manhattan([X|Y], V, N, T, L, RowNum, 0) :- 	X = [x|X2], N1 is N + 1, !, hfn_manhattan([X2|Y], V, N1, T, L, RowNum, 0).						
							

hfn_manhattan([X|Y], V, N, T, L, RowNum, 0) :- 																																	
						X = [X1|X2], not(X1 = x), N1 is N + 1, 
						C is (RowNum * L) + 1, D is (L * (RowNum - 1)) + 1,											  	  																
																															
								
				(							   
				        %if block is in correct row
		   			    (
	 				    X1 >= D, X1 < C, 																									
							
							    (									 											
					    (X1 >= N1 , T1 is T + (X1 - N1),  !,  hfn_manhattan([X2|Y], V, N1, T1, L, RowNum, 0) )
						    
					   ;(X1 < N1, T1 is T + (N1 - X1), !, hfn_manhattan([X2|Y], V, N1, T1, L, RowNum, 0) )															  															      																						
							    )
					    )					
						    ;
							    
			    	    	%if block is not in current row
				    																				
							    	
							%if block is vertically higher in goal								
							(
							X1 < D, !, hfn_manhattan(X1, T1, N1, T, L, RowNum, -1),
																		
							!,	   hfn_manhattan([X2|Y], V, N1, T1, L, RowNum, 0)	
							);
										
									
										
							%if block is vertically lower in goal
							(						
							X1 >= C, !, hfn_manhattan(X1, T1, N1, T, L, RowNum, 1),
											
							!, 	    hfn_manhattan([X2|Y], V, N1, T1, L, RowNum, 0)		
							)
							    
							    
							    
				 ).

			
				
																									
											
											
/* ------------------------------------------------------- */


/* init( +Name, -State)

   State is the initial state for problem Name.
   State is represented by a list of lists where each list
   is a row of the grid read as left-to-right & top-to-bottom
   x is the blank square.
*/

init(a, [[1,2,3], [4,8,5], [x,7,6]] ).

init(b, [[8,2,6], [4,1,5], [x,7,3]] ).

init(c, [[x,2,6], [4,1,5], [8,7,3]] ).

init(d, [[1,2,3,4], [5,6,7,8], [9,10,x,15], [13,12,11,14]] ).





/* ------------------------------------------------------- */

/* goal( +State )

   holds if and only if State is a goal state
*/   

goal(S) :- goal(S, 0).

goal([], _) :- !.

goal([[]|X], C) :- !, goal(X, C).

goal([X|Y], C) :- X = [X1|X2], not(X1 = x), (
					     (X1 is C + 1, !, goal([X2|Y], X1));
					     (!, fail)
					    ).		

goal([X|Y], C) :- X = [X1|X2], X1 = x, C1 is C + 1, !, goal([X2|Y], C1).		
/* ------------------------------------------------------- */






/** ---------------------------------------------------------
  calling the search algorithms
  ---------------------------------------------------------- */

go(ProblemName, HFN) :-
	init(ProblemName, Init),
	astar(Init, successors, goal, HFN, Path, equality),
	writeln(Path).

goCC(ProblemName, HFN) :-
	init(ProblemName, Init),
	astarCC(Init, successors, goal, HFN, Path, equality),
	writeln(Path).

goIDA(ProblemName, HFN) :-
	init(ProblemName, Init),
	idastar(Init, successors, goal, HFN, Path, equality),
	writeln(Path).


