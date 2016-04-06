Introduction
============

This is the Xtend implementation of the HollingBerries problem. Now I know you're rolling your eyes thinking: "Oh gawd, another obscure language I've never heard of!", but hold on a sec and hear me out. Xtend is different because it is a *transpiler*. Here are the highlights:

* Xtend is essentially a pre-processor for Java. So it compiles the code to Java source files, which are then compiled by the Java compiler.

* Xtend source is mostly Java with some boilerplate removed and a lot of syntactic sugar added.

* The learning curve is thus not steep (~1 hour for a Java dev). You simply use the bits of Xtend that make things easier for you.

* Xtend is an Eclipse project and thus has deep Eclipse integration and tooling support (IntelliJ 15+ is also supported). You can even debug Xtend code by stepping through Xtend code.

* Read more here: http://xtend-lang.org

How to run this Xtend version
=============================

* Install [gradle][]

* Change to this directory in a terminal

* Run:

    > $ gradle run

* Verify test passed by running this in the command-line from the project root: 
    
    > $ diff -u ../../pricefile.txt pricefile.txt # Expect no output


About the Code
==============

This is a similar implementation to my Ruby implementation, but instead of the elaborate config data, I used the powerful switch functionality of Xtend to map the product codes to markups and sell_by dates. Also, unlike the Ruby version, as is common in Java, I created a data object to hold the CSV data. This allows code-completion and other advantages of statically-typed languages to come through.

I made use of Xtend's functional syntax, extention methods, and string templating, which makes the code look like Ruby or some other dynamic language. Despite this, everything is statically typed, so it's like the best of both worlds. This is one of the attractions of Xtend. 
