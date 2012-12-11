1. Run the Erlang shell.
2. In the Erlang shell, type:

1> c("hollingberries.erl").

3. You may also need to run the following:

2> c("edate.erl").
3> c("csv_luke.erl").

4. Once everything has compiled, run the program like so:

4> hollingberries:process_produce("produce.csv", "pricefile.txt").

The first parameter is the input file, the second is the output file.

This is the first Erlang code I ever wrote, part of the interview processes
for my job was when Andreas sent me this problem to solve. I solved it and
got the job! :-) This solution has been revised because of an interpretation
problem with the requirements for my original solution, and the result is a
bit more compact code. I have never worked in a functional language before
solving this problem, so I am sure this solution could be improved.
Nevertheless, here it is!
