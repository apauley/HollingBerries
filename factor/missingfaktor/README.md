# HollingBerries Solution in Factor

This is a translation of ArtyomKazak's Haskell solution.

## Installing Factor

Download and install Factor - stable release 0.94 from [this site](http://factorcode.org).

Note that Factor is still in its infancy and breaking changes occur quite often. As such, attempting to run the program with a newer version of Factor (to be released soon) is likely to result in errors. If that happens, please notify me, and I will try to update the code to the most recent release of the language. 

## Running the solution

In your Factor installation directory, there should be an executable file named "factor". That's "Listener", Factor's REPL. Run that, and type the following:

    ! Enter appropriate paths below.
    "path/to/holling-berries.factor" run-file
    USE: holling-berries
    "path/to/produce.csv" "path/to/output/file" holling-berries

You should see an output file created at the specified output path.