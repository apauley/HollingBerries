How to run this Erlang version
==============================

*   Change to this directory in a terminal

*   Ensure that all the beams are compiled

    > $ erlc *.beam

*   Fire up an erl vm

    > $ erl

*   Run prices:start().

    > $ erl -noshell -s prices start -s init stop

*   Look at the contents of pricefile.txt

About the Code
==============

I tried a very simplified Erlang version, not too far off from my Ruby
attempt, to see what I would use instead of a class hierarchy.

It turns out that I used Erlang's pattern matching almost exclusively
in the same places where an OO model would use subtype polymorphism.
It felt very natural.

I'm also intrigued that I didn't feel the need to use higher order
functions in this attempt.
Does this mean that the example is simple enough not to bring out
those guns, or that I didn't spot the places were I could use it?
