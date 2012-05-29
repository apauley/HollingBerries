Usage
=====

* Install SBT from http://www.scala-sbt.org/
* Compile with "sbt compile"
* Run with "sbt run"
* Optionally run tests with "sbt test"

Note: with some minor changes this can also be converted to a standalone
Scala script, but SBT makes the code easier to organize and test.


Motivation
==========

Although the point of this problem is to explore non-object oriented solutions,
and Scala requires the use of objects, it does offer a few interesting twists:

* Combinator parsing
* Implicits (see the Int -> IntID conversion that allows the "between" operator)
* A sealed Fruit trait with case objects (similar to algebraic data types)
* Singleton companion objects instead of static methods
* Immutability (not required, but vals are used exclusively over vars here)
* Unit testing (also not required, but pleasant to work with and maintain)
