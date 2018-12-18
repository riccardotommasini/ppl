e[1]

Both Floats and Integers are supported when dealing with arithmetic.
They are almost the only ones handled transparently by Erlang's operators.

To express integer in other basis use the notation Base#Value

2#101010. (42)
8#052.
16#2A.

## Variables
Tw
- Variables start with capital letter.
- Variables can also start with underscore, but only for non important ones.
- 
One.
*1: variable 'One' is unbounded

One = 1.
1
Un = Uno = One = 1.
1

Two = One + One.

Two = 2.

Two = Two + 1
** exception error: no match of right hand side value 3

two = 2.
** exception error: no match of right hand side value 2

Variables can be assigned just once (Fake assignment can be done only if the vaue is the same).
`=` is a comparison operator, which return the value if the two sides present the same value. 
If the left side term is an Unbounded varirable, Erlang binds it with the value of the 
right side term.

f(Two) erases the variable, cleaning its name.
f() cleans all variables.


# Atoms

lower case names are atoms, i.e., literals , constans with their own name for value. If it doesn't begin with lower case letter it has to be enclosed in single quotes ('').
atoms are referred into an atom table which is not garbadge collected, so do not use to much. DO NOT GENERATE ATOMS DYNAMICALLY

Boolean operators ALWAYS evaluate both their arguments.
Erlang cares about float and integers in comparison

=:= test equality
=/= test inequality

== and /= do not care about float and integer (Inexact comparison)

0 == false. -> False.
1 < false. -> true.

true and false are atoms, integrated in the language so you can use them.
there is a total ordering between types. The correct ordering of each element in a comparison is the following:
                                 
number < atom < reference < fun < port < pid < tuple < list < bit string.

# Tuples

organizing data into finite groups of elements. 
{Element1, Elem2...ElementN}.

Point = {4,2}.
{X,Y} = Point.
{X,_} = Point.
X.

PreciseTemperature = {celsius, 23.213}.
{kelvin, T} = PreciseTemperature.

Atoms are not the same, Erlang raises an error.

#Lists

[Element1,..., Element2].

String are lists, automatically converted (Bleah!).
That is that at least one element should not be comparable.

[97,98,99,4,5,6].

[233].

++ lists concatenation
-- list difference
(Right associative)

[1,2,3] -- [1,2] -- [3].

[1,2,3] -- [1,2] -- [2].

car -> hd(List).
cdr -> tl(List).
q
pattern match = [Head|Tail]

| is the cons operator

#List Comprehnsions

Similar to Haskell [2*N || N <- [1,2,3,4]].

constraints,

[2*N || N <- [1,2,3,4], N rem 2 =:= 0].

NewList = [Expression || (Pattern <- List)+, Condition1*]







      
