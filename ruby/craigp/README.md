This README stolen from tobykurien's version and tweaked.

How to run this Ruby version
============================

*   Change to this directory in a terminal

*   Run:

    > $ ruby prices.rb

*   Look at the contents of pricefile.txt

*   Verify test passed:

    > $ diff -u ../../pricefile.txt pricefile.txt # Expect no output


About the Code
==============

I tried three different approaches to solve the same problem.

The first (prices.rb) is written as I would probably write it if I was solving the
problem for myself, no changes were likely, and no one else needed to change/understand
the code. The objectives were simplicity and brevity.

The second (prices_fiber.rb) is a rather pointless use of ruby 1.9 fibers to handle the parsing done in
the first example.

The third (prices_dsl.rb) is a weak attempt to build a simple DSL around the problem, allowing
different products to quickly and easily be added in the future.
