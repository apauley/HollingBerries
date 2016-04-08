Rust implementation of Hollingberries problem. 
How to run this Rust version
============================

*   Change to this directory in a terminal

*   Run:

    > $ cargo run

*   Look at the contents of pricefile.txt

*   Verify test passed: 
    
    > $ diff -u ../../pricefile.txt pricefile.txt # Expect no output


About the Code
==============

This is a similar approach to the [Ruby implementation](../../ruby/tobykurien/). This time I tried to extract all business rules and data to the top, using the powerful pattern matching abilities of Rust to act as a sort of DSL for defining some of the rules.

> NOTE: This is simply an implementation in Rust and not necessarily idiomatic Rust, since I'm still learning Rust.
