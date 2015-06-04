:-include(sud22).
:-dynamic(varNumber/3).
symbolicOutput(0). % set to 1 to see symbolic output only; 0 otherwise.

%% to solve a sudoku we must impose many different types of constraints:
%% we must enforce that there is ALO (at most one) and AMO (at most one) for every sub square
%% also, we must set AMO for every row and col of the board. Finally we just have to make sure
%% that we do not allow repetitions of numbers among the different sub squares of 3x3 that conform the boad.

writeClauses:-
	enforce_given_cell,
	at_least_one_per_quad,
	at_most_one_per_quad,
	at_most_one_per_row,
	at_most_one_per_col,
	avoid_repetitions_at_quads.
	

%% first, we must ensure that the given cell clauses that enforce a number are preserved with the solution.
enforce_given_cell:- 
	filled(I,J,K),
	writeClause( [c-I-J-K] ), 
	fail.
enforce_given_cell.

%% set at least 1 number for evry cell
at_least_one_per_quad:-
	between(1,9,I), 
	between(1,9,J),
	findall(c-I-J-K, between(1,9,K), C),
	writeClause(C), 
	fail.
at_least_one_per_quad.

%% set at most 1 number for evry cell
at_most_one_per_quad:-
	between(1,9,I),
	between(1,9,J),
	between(1,9,K), 
	between(1,9,AUX), 
	K < AUX,
	writeClause([ \+c-I-J-K, \+c-I-J-AUX ]),
	fail.
at_most_one_per_quad.

%% add constraints to forbid repetitions of numbers at each one of the 9 rows 
at_most_one_per_row:-
	between(1,9,I),
	between(1,9,K),
	between(1,9,J),
	between(1,9,AUX),
	J < AUX,
	writeClause([ \+c-I-J-K, \+c-I-AUX-K ]),
	fail.
at_most_one_per_row.

%% add constraints to forbid repetitions of numbers at each one of the 9 cols
at_most_one_per_col:-
	between(1,9,J),
	between(1,9,K),
	between(1,9,I),
	between(1,9,AUX),
	I < AUX,
	writeClause([ \+c-I-J-K, \+c-AUX-J-K ]),
	fail.
at_most_one_per_col.


%% check that there are no repeated numbers at each of the 9 sub-squares that conform the sudoku
avoid_repetitions_at_quads:-
	between(0,2,IF),
	between(0,2,JF),
	between(1,3,I),
	between(1,3,J), 
	%% num value 
	between(1,9,K),	
	between(I,3,I2),
	between(0,3,J2),
	(I>I2; J2 > J),	
	AUX1 is (I+IF*3),		%% first i's literals of AMO clause
	AUX2 is (J+JF*3),		%% first j's literals of AMO clause
	AUX3 is (I2+IF*3),		%% second i's of AMO clause
	AUX4 is (J2+JF*3),		%% second j's of AMO clause
	writeClause([ \+c-AUX1-AUX2-K, \+c-AUX3-AUX4-K ]),
	fail.
avoid_repetitions_at_quads.

displaySol(S):- between(1,9,I), nl, between(1,9,J), member(V,S), num2var(V,c-I-J-K), write(K), write(' '), fail.
displaySol(_):- nl.


% ========== No need to change the following: =====================================

main:- symbolicOutput(1), !, writeClauses, halt. % escribir bonito, no ejecutar
main:-  assert(numClauses(0)), assert(numVars(0)),
	tell(clauses), writeClauses, told,
	tell(header),  writeHeader,  told,
	unix('cat header clauses > infile.cnf'),
	unix('picosat -v -o model infile.cnf'),
	unix('cat model'),
	see(model), readModel(M), seen, displaySol(M),
	halt.

var2num(T,N):- hash_term(T,Key), varNumber(Key,T,N),!.
var2num(T,N):- retract(numVars(N0)), N is N0+1, assert(numVars(N)), hash_term(T,Key),
	assert(varNumber(Key,T,N)), assert( num2var(N,T) ), !.

writeHeader:- numVars(N),numClauses(C),write('p cnf '),write(N), write(' '),write(C),nl.

countClause:-  retract(numClauses(N)), N1 is N+1, assert(numClauses(N1)),!.
writeClause([]):- symbolicOutput(1),!, nl.
writeClause([]):- countClause, write(0), nl.
writeClause([Lit|C]):- w(Lit), writeClause(C),!.
w( Lit ):- symbolicOutput(1), write(Lit), write(' '),!.
w(\+Var):- var2num(Var,N), write(-), write(N), write(' '),!.
w(  Var):- var2num(Var,N),           write(N), write(' '),!.
unix(Comando):-shell(Comando),!.
unix(_).

readModel(L):- get_code(Char), readWord(Char,W), readModel(L1), addIfPositiveInt(W,L1,L),!.
readModel([]).

addIfPositiveInt(W,L,[N|L]):- W = [C|_], between(48,57,C), number_codes(N,W), N>0, !.
addIfPositiveInt(_,L,L).

readWord(99,W):- repeat, get_code(Ch), member(Ch,[-1,10]), !, get_code(Ch1), readWord(Ch1,W),!.
readWord(-1,_):-!, fail. %end of file
readWord(C,[]):- member(C,[10,32]), !. % newline or white space marks end of word
readWord(Char,[Char|W]):- get_code(Char1), readWord(Char1,W), !.
%========================================================================================
