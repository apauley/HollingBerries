How to run this Ruby version
============================

*   Change to this directory in a terminal

*   Run prices.rb

    > $ ruby prices.rb

*   Look at the contents of pricefile.txt


About the Code
==============

The example lends itself to a simple Product/Fruit object hierarchy,
which is what I've used here.
The extra supplier logic is something which does not fit in with the
above hierarchy, which is probably why I used an if-statement there.
I'd like to get rid of it, I just haven't given it enough thought.

I find it interesting that the Erlang attempt (using pattern matching)
had no need for an if- or case-statement in the supplier logic.
