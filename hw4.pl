/* Homework Assignment 4 - Prolog 2
   Programming Languages
   CS471, Spring 2017
   Binghamton University */

/* Instructions */

/* This section deals with general submission instructions.
First, grab this assignment from the site. BEFORE MOVING ON RENAME hw4S17.asn
to hw4.pl.
You will be able to code in and run the file in the Prolog interpreter directly.
I recommend reading this assignment directly from the source file.

We will be using swipl for our Prolog environment: To load/reload this file,
cd to its directory and run swipl. Then, in the prompt, type [hw4].

cd PATH_TO_FILE
swipl
[hw4].

From then on you may execute queries (goals) in the prompt. As usual, you
should provide your answers in the designated spot. Once you have added some
code to the file, rerun [hw4]. in the swipl prompt to reload.

In addition, there are unit tests for each problem. These are there to help you
better understand what the question asks for, as well as check your code. They
are included in our "database" as queries and are initially commented out
-- % is a Prolog line comment.

%:- member_times(4,[3,3,2,3],0). % SUCCEED
%:- member_times(4,[1,2,3],3).   % FAIL

After you have finished a problem and are ready to test, remove the initial %
for each test for the associated problem and reload the assignment file
([hw4].). Each SUCCEED line should silently load and succeed, and each FAIL
line should throw a WARNING. If a SUCCEED line throws a WARNING, or a FAIL line
fails to, then you solution is not correct. If you pass the tests there is a
good chance that your code is correct, but not guaranteed; the tests are meant
as guided feedback and are not a check for 100% correctness. */

/* Submission */

/* For this assignment - and the remaining Prolog assignments - you must submit
only the source (template) file. There is no need to tar anything as all coding
should be done directly in hw4.pl. */

/* Homework 4 */

/* Due: Wednesday, 2/24 at 11:59 PM */

/* Purpose: To get comfortable with backtracking, recursion,
   become familar with reflective mechanism of Prolog,
   and Prolog as a symbolic programming language.
*/

my_append([],Ys,Ys).
my_append([X|Xs],Ys,[X|Zs]) :- my_append(Xs,Ys,Zs).

my_prefix(_,[]).
my_prefix([X|Xs], [X|Ys]) :- my_prefix(Xs,Ys).



/* Problem 0A (NOT GRADED):
Using the preceding predicates, draw the execution trees for the following
queries (goals). You should also enter the queries in swipl to test.

my_append([a,b],[c],Z).
my_append(Xs,[c],[a,b,c]).
my_append(Xs,Ys,[a,b,c]).
my_prefix([a,b,c],Z).

After drawing the execution trees, enable tracking on my_append and my_prefix
by running (two separate queries)

trace(my_append).
trace(my_prefix).

in swipl. Now, execute the above queries and try and match what you drew to
the way the actual query is executed in swipl. To turn off 'trace', type the
query 'nodebug'.

If you prefer a graphical debugger/trace, enter the query 'manpce.' . You will
see a small XPCE manual window. Under the 'Tools' menu select: "Prolog
graphical tracer".

*/




/* Problem 1:
   Write a predicate sumlist(List,Sum) which succeeds if Sum is the total value
   of all the elements of List. This will be a top down recursion.
   The recursion clause will add the current value to the result of the sum
   of the rest of the list.
   We have already provided the base case for this predicate underneath
   'Problem 1 Answer'. You just need to add the recursive clause.
*/

/* Problem 1 Answer */

sumlist([], 0).
sumlist([H|Ts],Sx) :- sumlist(Ts,Sy),Sx is Sy+H.

/* Problem 1 Test */
/* There should be no warnings when compiling,
   tests which are supposed to fail are written as such */

 :- sumlist([], 0).
 :- sumlist([], 1) -> fail ; true.
 :- sumlist([1,2,3,4], 10).
 :- sumlist([1], 1).

/* Problem 2:
   Write the predicate sumlist2(List,Sum) which succeeds if Sum is the sum total
   of all the elements of List. Instead of adding the current value to the
   result of the sum of the tail, you will calculate the partial sum of the all
   the elements you have reached so far. You will need an extra argument to
   store the partial sum, so you will write an auxilitary predicate sumlist2/3
   to handle the extra argument.

   Underneath 'Problem 2 Answer' we have provided sumlist2/2, which calls the
   auxiliary predicate sumlist2/3. We have also provided the base case for the
   auxiliary predicate. You just need to add the recursive clause for
   sumlist2/3.

*/

/* Problem 2 Answer */

sumlist2(List,Sum) :- sumlist2(List, 0, Sum).
sumlist2([H|Ts], X, Sum) :- Y is X+H,sumlist2(Ts, Y, Sum).
sumlist2([], Sum, Sum).

/* Problem 2 Test */

 :- sumlist2([], 0).
 :- sumlist2([], 1) -> fail ; true.
 :- sumlist2([1,2,3,4], 10).
 :- sumlist2([1], 1).

/* Problem 3:
   Write the predicate sumPartialR(N, SumLst), which succeeds as follows:
   given a number N, SumLst is a sequence of sums such that first number in
   S is the sum of all the numbers from N to 1, the second number in S the sum
   of all the numbers from N-1 down to 1, and so on.
   In other words, SumLst = [N+(N-1)+..+1, (N-1)+(N-2)+..+1, ..., 1].
   For example:

     ?- sumPartialR(6,S).
     S = [21, 15, 10, 6, 3, 1] .

   This problem can be solved in 2 clauses.
*/


/* Problem 3 Answer */

sumPartialR(1, [1]).
sumPartialR(N, [H|Ts]) :- Nx is N-1,sumPartialR(Nx, Ts),[Hx|_]=Ts,H is Hx+N.

/* Problem 3 Test */

 :- sumPartialR(1, [1]).
 :- sumPartialR(1, []) -> fail ; true.
 :- sumPartialR(2, [3, 1]).
 :- sumPartialR(6, [21, 15, 10, 6, 3, 1]).



/* Problem 4:
   Write the predicate sumPartialL(N, SumLst). This problem is very similar to
   Problem 3, but has one key difference. The sum totals accumulate from left
   to right, so the SumLst generated will be different. For example, the first
   value in S will be N, the second value will be N + (N-1), and so on.
   In other words, SumLst = [N, N+(N-1), ..., N+(N-1)+(N-2)+...+1].
   For example,

     ?- sumPartialL(6,S).
     S = [6, 11, 15, 18, 20, 21]

   It would be helpful to follow the idea used in problem 2. So your first
   clause should be:

       sumPartialL(N,Lst):-sumPartialL(N,N,Lst).

   You need to add 2 additional clauses.*/

/* Problem 4 Answer */
sumPartialL(N,Lst):-sumPartialL(N,N,Lst).
sumPartialL(0,_,[]).
sumPartialL(N,X,[X|Ts]) :- Nx is N-1,Y is X+N-1,sumPartialL(Nx,Y,Ts).

/* Problem 4 Test */

 :- sumPartialL(1, [1]).
 :- sumPartialL(1, []) -> fail ; true.
 :- sumPartialL(6, [6, 11, 15, 18, 20, 21]).


/* Problem 5:
   A) What is the mathematical definition of:
     a) a relation?
     b) a function?
   B) Is every function a relation? If false, give a counter example.
   C) Is every relation a function? If false, give a counter example. */

/* Problem 5 Answer: */
/*
A)
a). A relation is a A rule which associates each element of set (A) with at least one element in set (B).
b). a function is a rule that each element in set (A) maps to only one element in set (B).

B). True
C). False. For example. {(a,b),(a,c),(d,e)} is a relation but it is not one to one since a will be mapped to b and c. So it is not a function.
*/
/* Problem 0B (NOT GRADED):
   Each line is an individual Prolog query; it's a good idea type them in your
   prompt (not the file itself) to get a feel for the way Prolog works. You
   should think about whether or not each query will succeed, and if so how the
   variables will be initialized (unified). It will help in doing some of the
   problems.

?- number(A), A = 5.6.
?- A = 5.6, number(A).
?- integer(4).
?- help(functor).
?- functor(foo(a,b,c),F,N).
?- functor(T,foo,3).
?- help(arg).
?- arg(3, foo(a,b,c),A).
?- help('=..').
?- T =.. [foo,x, y, z].
?- E =.. ['+',2,3], R is E.
?- foo(who, what) =.. T.
?- foo(who, what) =.. [A, B,C].
?- clause(ack(M,0,B),C).
?- clause(H,(B is 2*0)).
*/



/* Problem 6:
   We will use a predicate edge(X,Y) to encode a graph.
   edge(X,Y) is true if there is a directed edge from X to Y.
   The following is a mini graph encoded in Prolog. */

edge(a,b).
edge(a,e).
edge(a,c).
edge(b,a).
edge(b,c).
edge(b,d).
edge(c,e).
edge(f,e).

/* Using your knowledge of backtracking and the findall predicate, write
   predicates outgoing/2 and incoming/2.

   outgoing(X,Y) should succeed if Y is a list of all immediate vertices
   reached from X's outgoing edges. incoming(X,Y) should succeed if Y is a
   list of all vertices that have outgoing edges to X.
*/

/* Problem 6 Answer */

outgoing(X,Y) :- findall(C,edge(X,C),Y).
incoming(X,Y) :- findall(C,edge(C,X),Y).

/* Problem 6 Test */
 :- outgoing(a,X), X = [b,e,c].
 :- outgoing(e,X), X = [].
 :- outgoing(a,X), X = [b,e,c].
 :- incoming(a,X), X = [b].
 :- incoming(f,X), X = [].

 :- outgoing(e,X), X = [a] -> fail ; true.
 :- incoming(e,X), X = [] -> fail ; true.



/* Problem 7:
   Define homoiconic.
   Is Prolog homoiconic?
   What does it mean to say a language is fully reflective?
   Is Prolog fully reflective?

   (See page 584 and Chapter 12)
*/

/* Problem 7 Answer: */
/*
A homoiconic language can simplify metacomputing by eliminating the need to translate between internal/external (data structure/syntactic) representations of the program
Prolog is homoiconic.
Fully reflective means that the language can inspect every aspect of its structure and current state
Prolog is not fully reflective.
*/


/* Problem 8:
   Write a predicate computeS/4. computeS(Op, Arg1, Arg2, Result) succeeds if
   Result is the value after computing Arg1 Op Arg2. Use the insight you gained
   in Problem 0B. Op must be a builtin Prolog operator.
*/

/* Problem 8 Answer: */

computeS(Op, Arg1, Arg2, Result) :- E=..[Op,Arg1,Arg2], R is E, R=Result.

/* Problem 8 Test: */
 :- computeS(-, 19, 7, 12).
 :- computeS(div, 19, 7, 2).
 :- computeS(div, 19, 7, R), R = 2.

 :- computeS(/, 19, 7, 2) -> fail ; true.
 :- catch((computeS(sin, 90, 1, _), fail), error(_Err, _Context), true).



/* Problem 9:
   In class we discussed the 'is' predicate for evaluating expressions. Write a
   predicate results/2.
   result(Elst,RLst) succeeds if Rlst is unifies with the values computed from
   the list of expressions, Elst.
*/

/* Problem 9 Answer: */

result([],[]).
result([Ha|Tas],[Hb|Tbs]) :- Hb is Ha,result(Tas,Tbs).

/* Problem 9 Test */
 :- result([],[]).
 :- result([+(3,7), mod(104,7),-(5)],[10, 6, -5]).
 :- result([+(3,7), +(15, -(3,11))],X), X = [10, 7].

 :- result([+(3,7), mod(104,7)],[10,13]) -> fail ; true.



/* Problem 10:
   A good example of symbolic computation is symbolic differentiation. Below
   are the rules for symbolic differentiation where U, V are mathematical
   expressions, C is a number constant, N is an integer constant and x is a
   variable:

        dx/dx = 1
        d(C)/dx = 0.
        d(Cx)/dx = C
        d(-U)/dx = -(dU/dx)
        d(U+V)/dx = dU/dx + dV/dx
        d(U-V)/dx = dU/dx - dV/dx
        d(U*V)/dx = U*(dV/dx) + V*(dU/dx)
        d(U^N)/dx = N*U^(N-1)*(dU/dx)

   Translate these rules into Prolog. (Please keep the order of the rules the
   same for testing purposes).
*/

/* Problem 10 Answer: */

d(x,x,1).
d(X,x,0) :- number(X).
d(X*x,x,X) :- number(X).
d(-U,x,R) :- d(U,x,Rx), R = -Rx.
d(X+Y,x,R) :- d(X,x,Rx),d(Y,x,Ry),R = Rx+Ry.
d(X-Y,x,R) :- d(X,x,Rx),d(Y,x,Ry),R = Rx-Ry.
d(X*Y,x,R) :- d(X,x,Rx),d(Y,x,Ry),R = X*Ry+Y*Rx.
d(U^N,x,R) :- integer(N),d(U,x,Rx),Tmp is N-1,R = N*U^Tmp*Rx.

/* Problem 10 Test: */
 :- d(x,x,R), R = 1 .
 :- d(7*x,x,R), R = 7 .
 :- d(x +2*(x^3 + x*x),x,Result), Result = 1+ (2* (3*x^2*1+ (x*1+x*1))+ (x^3+x*x)*0) .
 :- d(-(1.24*x -x^3),x,Result), Result = - (1.24-3*x^2*1) .
 :- d(-(1.24*x -2*x^3),x,Result), Result = - (1.24- (2* (3*x^2*1)+x^3*0)) .

% Pay careful attention to why this fails.
 :- d(x +2*(x^3 + x*x),x,Result), Result = 1+ (2* (3*x^(3-1)*1+ (x*1+x*1))+ (x^3+x*x)*0) -> fail ; true.



/* Problem 11:
   (Exercise 3.5 from Learn Prolog Now!)
   Binary trees are trees where all internal nodes have exactly two children.
   The smallest binary trees consist of only one leaf node. We will represent
   leaf nodes as leaf(Label) . For instance, leaf(3) and leaf(7) are leaf
   nodes, and therefore small binary trees.

   Given two binary trees B1 and B2 we can combine them into one binary tree
   using the functor tree/2 as follows: tree(B1,B2) .
   So, from the leaves leaf(1) and leaf(2) we can build the binary tree
   tree(leaf(1),leaf(2)) .
   From the binary trees tree(leaf(1),leaf(2)) and leaf(4) we can build
   tree( tree(leaf(1), leaf(2)), leaf(4)) .

   Define a predicate swap/2 , which produces the mirror image of the binary
   tree that is its first argument. For example:

   ?-  swap( tree( tree(leaf(1), leaf(2)), leaf(4)), T).
   T  =  tree( leaf(4), tree(leaf(2), leaf(1))).
*/

/* Problem 11 Answer: */


swap(leaf(X),leaf(X)).
swap(tree(X,Y),T) :- swap(Y,M),swap(X,N),T=tree(M,N).

/* Problem 11 Test: */
 :- swap( tree( tree(leaf(1), leaf(2)), leaf(4)), T), T  =  tree( leaf(4), tree(leaf(2), leaf(1))).
 :- swap(leaf(1), leaf(1)).
 :- swap(tree(leaf(1), leaf(2)), tree(leaf(1), leaf(2))) -> fail ; true.


/* Problem 12:
   In class we will discuss how Prolog can be used for solving puzzles.
   We will do a well know puzzle: "Send more money".
   Each of the letters  D,E,M,N,O,R,S and Y represents a different digit.
   Moreover, when each letter is mapped to its corresponding digit
   the equation SEND + MORE = MONEY holds.

   Our solution was very naive using Prolog ability to search for the solution.
   The search is very slow because 8 letters to be solved, it simply explores
   the 10*9*...*3 mappings of letters to digits.

   You can speed up the search by implementing the following insights:
      Clearly, SEND < 9999 and MORE < 9999. Thus MONEY < 19998 and hence M = 1.
      Now we have SEND + 1ORE = 1ONEY.
      Again SEND < 9999 and now 1ORE < 1999 so 1ONEY < 11998.
      Since M is already bound to 1, O must be bound to 0.
      A little more thought shows that S must be bound to 8 or 9, and that
      N = E + 1.

   Using these insights to reduce the number of solutions that must be explored,
   write a Prolog predicate sendMoreMoney([D,E,M,N,O,R,S,Y]) that solves this
   puzzle by binding the correct digits to each of the variables in the list.
*/

/* Problem 12 Answer: */

sendMoreMoney( [D,E,M,N,O,R,S,Y]) :-
	O=0,
	M=1,
	Lst = [E,D,R,Y],
	Digits = [2,3,4,5,6,7,8,9],
	assign_digits(Lst, Digits),
	assign_digits([S],[8,9]),
	N is E+1,
	N\=S,N\=D,N\=R,N\=Y,
	E\=S,S\=D,S\=R,S\=Y,
	1000*S + 100*E + 10*N + D +
	1000*M + 100*O + 10*R + E =:=
	10000*M + 1000*O + 100*N + 10*E + Y.
	
assign_digits([], _List).
assign_digits([D|Ds], List):-
	select(D, List, NewList),
	assign_digits(Ds, NewList).

/* Problem 12 Test: */

 :- M = 1, sendMoreMoney( [D,E,M,N,O,R,S,Y]), M = 1, D = 7, E = 5, N = 6, O = 0, R = 8, S = 9, Y = 2.



/* Problem 13:
   In class we discussed green and red cuts (!). A green cut is a cut that DOES
   NOT change correctness (the answer returned) but simply improves efficiency
   by preventing unnecessary backtracking. Red cuts change correctness - if a
   predicate is correct and contains a cut that, when removed, is no longer
   correct, it is a red cut.

   Insert cuts into the following 2 predicates. The my_max/3 are already correct, 
   but using a cut (green) will improve their efficiency. 
   The last, my_max1/3 is wrong, but inserting a cut (red) will
   make it correct.
*/

/* Problem 13 Answer */


my_max(X,Y,Y) :- X =< Y,!.
my_max(X,Y,X) :- X > Y.

my_max1(X,Y,Z) :- X =< Y, Y = Z,!.
my_max1(X,_,X).

/* Problem 13 Test */
% You're own your own for this one :) */

 :- my_max(1,2,2).
 :- my_max(3,2,3).
 :- my_max(2,2,3) -> fail;true.
 :- my_max1(1,2,2).
 :- my_max1(3,2,3).
 :- my_max1(2,2,3) -> fail;true.
