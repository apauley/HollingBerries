# HollingBerries Solution in Clojure

A couple of comments:
*   I am very new to clojure so any comments on doing things better would be welcomed.
*   I re-used(read copied...) quite a few things from mschristiansen's implementation. 
*   The approach I used here is similar to the my example in ABAP where I placed the Supplier and Product rules in records. I believe this will give a bit more flexibility should the business rules(e.g. vary markups, expiry dates, etc by either product or supplier).

## Usage

You'll need [Leiningen](https://github.com/technomancy/leiningen), a
build tool for Clojure.

Leiningen will fetch project dependencies described in the project.clj
file.
> lein deps

Allows you to run the program, which will generate the pricefile.txt.
> lein run

Or start a repl (is it called a prompt in Python?)
> lein repl

To run the test:
> lein test

## License

Distributed under the Eclipse Public License, the same as Clojure.
