/* Homework Assignment 3
   Programming Languages
   CS471, Spring 2017
   Binghamton University */

/* Instructions */

/*

This section deals with general submission instructions. Use this
source file. BEFORE MOVING ON RENAME hw3S17.asn to hw3.pl. You will be
able to code in and run the file in the prolog interpreter directly. I
recommend reading this assignment directly from the source file.

We will be using swipl for our prolog environment:
 To load/reload this file, cd to its directory and run swipl. Then, in the prompt, type [hw3].

cd PATH_TO_FILE
swipl
[hw3].

From then on you may execute queries (goals) in the prompt. As usual,
you should provide your answers in the designated spot. Once you have
added some code to the file, rerun [hw3]. in the swipl prompt to
reload.

In addition, there are unit tests for each problem. These are there to
help you better understand what the question asks for, as well as
check your code. They are included in our knowledge base as queries
and are initially commented out -- % is a Prolog line comment.

%:- member_times(4,[3,3,2,3],0). % SUCCEED
%:- member_times(4,[1,2,3],3).   % FAIL

After you have finished a problem and are ready to test, remove the
initial % for each test for the associated problem and reload the
assignment file ([hw3].). Each SUCCEED line should silently load and
succeed, and each FAIL line should throw a WARNING. If a SUCCEED line
throws a WARNING, or a FAIL line fails to, then you solution is not
correct. If you pass the tests there is a good chance that your code
is correct, but not guaranteed; the tests are meant as guided feedback
and are not a check for 100% correctness.

*/

/* Submission */

/*
For this assignment -- and the remaing Prolog assignments -- you must
submit only the source file. There is no need to tar anything as all
coding should be done directly in hw3.pl.
*/

/* Homework 3 */

/* Due: Next Wednesday, 11:59 PM */

/*
Purpose: To get comfortable with Logic programming, and get a good
grasp on list manipulation in Prolog.
*/

/* Problem 0A (NOT GRADED):
   Programming with matching.  A line can be defined by 2 points. A point has an
   x and y coordinate.   A line is vertical if both points have the same x value.
   A line is horizontal if both points have the same y values.  The following
   is a knowledge base which specify what is meant for a line to be vertical
   or horizontal respectively. This example is due to Ivan Bratko.
*/

vertical(line(point(X,Y),point(X,Z))).
horizontal(line(point(X,Y),point(Z,Y))).

/* 1. Name the clauses, predicates, rules, and facts.
   2. Name the constants, variables, and complex structures. */
   
   1.
   clauses:
   point(X,Y)
   point(X,Z)
   point(Z,Y)
   line(point(X,Y),point(X,Z))
   line(point(X,Y),point(Z,Y))
   vertical(line(point(X,Y),point(X,Z)))
   horizontal(line(point(X,Y),point(Z,Y)))
   
   predicates:
   point
   line
   vertical
   horizontal
   
   no rules
   
   facts:
   point(X,Y)
   point(X,Z)
   point(Z,Y)
   line(point(X,Y),point(X,Z))
   line(point(X,Y),point(Z,Y))
   vertical(line(point(X,Y),point(X,Z)))
   horizontal(line(point(X,Y),point(Z,Y)))
   
   2.
   no constant
   
   variables: X, Y, Z
   
   complex structures:
   point(X,Y)
   point(X,Z)
   point(Z,Y)
   line(point(X,Y),point(X,Z))
   line(point(X,Y),point(Z,Y))
   vertical(line(point(X,Y),point(X,Z)))
   horizontal(line(point(X,Y),point(Z,Y)))

/* Problem 0B (NOT GRADED):
    A way of writing numerals, which is sometimes used in mathematical logic, makes
    use of just four symbols: 0, succ , and the left and right parentheses.
    The following is the knowledge base for this representation of a numeral.
    The predicate 'add' is the definition of adding this representation of numbers.

    Use this base you should use to answer the question.
    (Reference 3.1 example 3 Learn Prolog Now!
      http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse9)
    */

numeral(0).
numeral(succ(X))  :-  numeral(X).

add(0,Y,Y).
add(succ(X),Y,succ(Z))  :-
                    add(X,Y,Z).


/* 1. Name the clauses, predicates, rules, and facts.
   2. Name the constants, variables, and complex structures. */
   
   1.
   clauses:
   numeral(0).
   succ(X).
   numeral(succ(X))  :-  numeral(X).

   add(0,Y,Y).
   add(succ(X),Y,succ(Z)) :- add(X,Y,Z).
					   
   predicates:
   numeral
   succ
   add
   
   rules:
   numeral(succ(X))  :-  numeral(X).
   add(succ(X),Y,succ(Z)) :- add(X,Y,Z).
   
   facts:
   numeral(0).
   succ(X).
   succ(Z).
   
   2.
   constants: 0
   
   variables: X, Y, Z
   
   complex structures:
   numeral(0).
   numeral(succ(X))  :-  numeral(X).

   add(0,Y,Y).
   add(succ(X),Y,succ(Z)) :- add(X,Y,Z).

/* Problem 0C (NOT GRADED):
Each line is an individual Prolog query; it's a good idea type them in your prompt (not the file itself) to get a feel for the way Prolog works. You should think about whether or not each query will succeed, and if so how the variables will be instantiated (as result of unification). You can expect these sort of questions on the test.

X = 1+2.
X is 1+2.
1+2=1+2.
1+2=2+1.
1+2=3.
1+2 is 3.
3 is 1+2.
3 =< 6.
6 =< 3.
X = Y, Y = Z, Z = 3.
X = Y, Y = Z, Z = 3, X = 4.
T = (X,Y), X = a, Y = b.
X = [1,2,3], [H|T] = X.
X = [1,2,3], [H1,H2|T] = X.
X = [1], [H|T] = X.
X = [1], Y = [2|X].
add(0,succ(succ(0)),Y).

help(member).
X = 3, member(X, [Y]).
*/

/* Problem 1:

The following are two basic predicates for list manipulation:
my_first/2 and my_last/2. We may refer to a predicate by writings it
as name/arity; hence, my_first/2 means a predicate named my_first with
two arguments.

my_first(X,Y) succeeds if X is the first element of list Y.
my_last(X,Y) succeeds if X is the last element of list Y.
*/

my_first(X,[X|_]).

my_last(X,[X]).
my_last(X,[_|Ys]) :- my_last(X,Ys).

/*
Note my use of the _ (wildcard). It is used to say to Prolog "I don't
care, match anything" and should be used to avoid singleton warnings
(a variable that is not unified). We will discuss this during lab.

Try the following queries before moving on. Note that they are
included in the comment section for a reason: They would be
interpreted as facts in the database otherwise.

my_first(X,[3,a,dd]).
my_last(X,[3,a,dd]).
my_first(3,[3,a,dd]).
my_first(a,[3,a,dd]).
my_last(dd,[3,a,dd]).
my_last(a,[3,a,dd]).

Now, write a predicate my_member(X,Y) that succeeds if X is a member
of the list Y.

NOTE: my_first/2 and my_last/2 are provided as examples for
manipulating lists. You should not use them in definition of
my_member/2.

*/

/* Problem 1 Answer: */

my_member(X,[X]).
my_member(X,[H|Ts]) :- my_member(X,Ts);H=X.


/* Problem 1 Test: */
:- my_member((1,3),[(1,2),(1,3)]).     % SUCCEED
:- my_member(3,[3]).         % SUCCEED
:- my_member(4,[1,2,3]).     % FAIL

/* Problem 2:
 Write a predicate init(All, BLst) that succeeds if Blst has all the items of ALL
 except the last item.  The items in BLst are in the same order as ALL.
*/

/* Problem 2 Answer: */

init([_],[]).
init([X|Tas],[X|Tbs]) :- init(Tas,Tbs).
init([Ha|Tas],[Hb|Tbs]) :- Ha\=Hb,init([Ha|Tas],Tbs).

/* Problem 2 Test: */
 :- init([1], []).       % SUCCEED
 :- init([1,2,3], [1,2]).% SUCCEED
 :- init([1,2], [1,2]).  % FAIL
 :- init([1,2], [2]).	% FAIL

/* Problem 3:
Write a predicate is_decreasing(X) that succeeds if X is a list of decreasing numbers -- Each number is either the same or lower than the preceding number.

NOTE: You may match two elements at a time against a list: [X,Y|Xs] = List. It's preferable to do it in the rule head however...
some_rule([X,Y|Xs]) :- ...  */

/* Problem 3 Answer: */

is_decreasing([]).
is_decreasing([_]).
is_decreasing([X,Y]) :- X>=Y.
is_decreasing([X,Y|Ts]) :- X>=Y,is_decreasing([Y|Ts]).

/* Problem 3 Test: */
:- is_decreasing([]).            % SUCCEED
:- is_decreasing([10]).          % SUCCEED
:- is_decreasing([10,9]).        % SUCCEED
:- is_decreasing([10,9,7]).      % SUCCEED
:- is_decreasing([10,9,7,7,2]).  % SUCCEED
:- is_decreasing([1,1,1,1,1]).   % SUCCEED

:- is_decreasing([10,9,7,9]).    % FAIL
:- is_decreasing([2,3,1]).       % FAIL
:- is_decreasing([1,2,3]).       % FAIL
:- is_decreasing([7,19]).        % FAIL

/* Problem 4:
Write a predicate element_at(X,Y,N) that succeeds if X is the Nth element of list Y. Y is 0-indexed.

NOTE: Don't worry about the error cases: i.e, N greater than the length of Y.  */

/* Problem 4 Answer: */

element_at(X,[X],0).
element_at(X,[X|_],0).
element_at(X,[_|Ts],N) :- element_at(X,Ts,Nx),N is Nx+1.


/* Problem 4 Test: */
:- element_at(3,[1,2,3],2).   % SUCCEED
:- element_at(1,[1,2,3],0).   % SUCCEED

:- element_at(1,[1,2,3],1).     % FAIL

/* Problem 5:
Write a predicate insert_at(X,Y,N,Z) that succeeds if Z is the list Y with X inserted at index N -- Insert X at index N in Y.

NOTE: Don't worry about the error cases: i.e, N greater than the length of Y.  */

/* Problem 5 Answer: */

insert_at(X,Y,0,[X|Y]).
insert_at(X,[_|Tys],N,[_|Tzs]) :- insert_at(X,Tys,Nx,Tzs),N is Nx+1.


/* Problem 5 Test: */
:- insert_at(3,[1,2,3],2,[1,2,3,3]).  % SUCCEED
:- insert_at(1,[1,2,3],0,[1,1,2,3]).  % SUCCEED
:- insert_at(a,[1,2,3],1,[1,a,2,3]).  % SUCCEED

:- insert_at(1,[1,2,3],0,[1,2,3]).    % FAIL

/* Problem 6:
Write a predicate zip(Xs,Ys,Zs) that succeeds if Zs is a list where each element is a tuple, (X,Y), with Xs and Ys paired together.

For example...
zip([1,2,3],[a,b,c],Zs) should give Zs = [(1,a),(2,b),(3,c)]
zip([1],[a],Zs) should give Zs = [(1,a)]

NOTE: You may assume X and Y have the same length. */

/* Problem 6 Answer: */
zip([],[],[]).
zip([Hx|Txs],[Hy|Tys],[Hz|Tzs]) :- Hz=(Hx,Hy),zip(Txs,Tys,Tzs).

/* Problem 6 Test: */
:- zip([1,2,3],[a,b,c],[(1,a),(2,b),(3,c)]). % SUCCEED
:- zip([],[],[]).                      % SUCCEED
:- zip([1],[2],[(1,2)]).               % SUCCEED

:- zip([1],[2],[(2,3)]).               % FAIL
:- zip([1],[2,3],[(1,2)]).             % FAIL

/* Problem 7:

Write a predicate zip2(Xs,Ys,Zs) that succeeds if Zs is a list where each element is a tuple, (X,Y), with Xs and Ys paired together. However, the length of Zs will be equal to the length of Xs or Ys which ever is less.

For example...
zip2([1,2,3,4],[a,b,c],Zs) should give Zs = [(1,a),(2,b),(3,c)]
zip2([1],[a,b],Zs) should give Zs = [(1,a)] */

/* Problem 7 Answer: */
zip2([],[],[]).
zip2([],[_|_],[]).
zip2([_|_],[],[]).
zip2([Hx|Txs],[Hy|Tys],[Hz|Tzs]) :- Hz=(Hx,Hy),zip2(Txs,Tys,Tzs).

/* Problem 7 Test: */
:- zip2([1,2,3],[a,b,c],[(1,a),(2,b),(3,c)]). % SUCCEED
:- zip2([],[a,b,c],[]).                  % SUCCEED
:- zip2([1,3],[],[]).                    % SUCCEED
:- zip2([1,3],[2],[(1,2)]).              % SUCCEED

:- zip2([1],[2],[(2,3)]).                 % FAIL
:- zip2([1],[a,b],[(1,a),(1,b)]).         % FAIL

/* Problem 8:
Write a predicate merge(A,B,M) that succeed if the list M has all the items from lists A and B in decreasing order.  Assume that A and B are sorted in decreasing order.  Items do not need to be unique.

For example:
merge([10,3,2], [11,5,2], M) should give M =[11,10,5,3,2,2].

 */

/* Problem 8 Answer: */

merge([],[],[]).
merge([],X,X).
merge(X,[],X).
merge([X|Tas],[Hb|Tbs],[X|Tms]) :- merge(Tas,[Hb|Tbs],Tms),(X>=Hb).
merge([Ha|Tas],[X|Tbs],[X|Tms]) :- merge([Ha|Tas],Tbs,Tms),(Ha=<X).

/* Problem 8 Test: */
:- merge([10,3,2],[11,5,2],[11,10,5,3,2,2]) .       % SUCCEED
:- merge([0],[],[0]).                               % SUCCEED
:- merge([],[3],[3]).                               % SUCCEED

:- merge([4,3],[3],[3]).                            % FAIL

/* Problem 9:
   See Problem 0B above for the knowledge base used for defining greater_than/2 .
   Exercise  3.4 (http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse11)
   Define a predicate greater_than/2 that takes two numerals in the notation
   that we introduced in the text (that is, 0, succ(0), succ(succ(0)), and so on)
   as arguments and decides whether the first one is greater than the second one.
*/

/* Problem 9 Answer: */

greater_than(succ(_),0).
greater_than(succ(X),succ(Y)) :- greater_than(X,Y).

/* Problem 9 Test: */
 :- greater_than(succ(succ(succ(0))),succ(0)).        % SUCCEED
 :- greater_than(succ(succ(0)),succ(succ(succ(0)))).  % FAIL




/* Problem 10:
   See Problem 0B above for the knowledge base used for defining minus/3 .
   Define substract(Num1,Num2,Result) to succeed if Result is the result of
   Num1 - Num2.  Num1, Num2 and Result use four symbols: 0, succ , and the left and right parentheses
   to represent numbers.

   Use the add/3, from problem 0B, definition to define minus/3.  Do not
   write a recursive definition for minus/3.
*/


/* Problem 10 Answer: */
minus(Z,0,Z).
minus(succ(X),succ(Y),Z) :- minus(X,Y,Z).

/* Problem 10 Test: */
 :- minus(succ(succ(0)), succ(0), succ(0)).       % SUCCEED
 :- minus(succ(succ(0)), 0, succ(succ(0))).       % SUCCEED
 :- minus(succ(succ(0)), succ(succ(0)), 0).       % SUCCEED
 :- minus(succ(succ(0)), 0, 0).	            % FAIL
 :- minus(succ(succ(0)), succ(0), succ(succ(0))). % FAIL

/* Problem 11:
(From Learn Prolog NOW!) Binary trees are trees where all internal nodes have exactly two children. The smallest binary trees consist of only one leaf node. We will represent leaf nodes as leaf(Label). For instance, leaf(3) and leaf(7) are leaf nodes, and therefore small binary trees. Given two binary trees B1 and B2 we can combine them into one binary tree using the predicate tree: tree(B1,B2). So, from the leaves leaf(1) and leaf(2) we can build the binary tree tree(leaf(1), leaf(2)). And from the binary trees tree(leaf(1), leaf(2)) and leaf(4) we can build the binary tree tree(tree(leaf(1), leaf(2)), leaf(4)).

Now define a predicate isBinaryTree(+BT) which succeeds if BT is a binary tree. The "+" indicates that it is assumed BT is instantiate in the query.
For example:
If BT = tree( leaf(1), tree( leaf(2),leaf(4)) ), then isBinaryTree(BT) succeeds.

*/

/* Problem 11 Answer: */

isBinaryTree(leaf(_)).
isBinaryTree(tree(X,Y)) :- isBinaryTree(X),isBinaryTree(Y).

/* Problem 11 Test: */
:- isBinaryTree(leaf(1)).                                           %SUCCEED
:- isBinaryTree(tree(leaf(a),leaf(b))).                             %SUCCEED
:- BT = tree( leaf(b), tree( leaf(x),leaf(y)) ), isBinaryTree(BT).  %SUCCEED
:- BT = tree(tree(leaf(1), leaf(2)), tree(leaf(10), tree(leaf(4), leaf(11)))), isBinaryTree(BT).  %SUCCEED

:- isBinaryTree( tree(leaf(1)) ).                                   % FAIL
%:- isBinaryTree( tree() ).                                          % FAIL
