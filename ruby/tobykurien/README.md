This README stolen from marcheiligers version and tweaked.

How to run this Ruby version
============================

*   Change to this directory in a terminal

*   Run:

    > $ ruby hollingberries.rb

*   Look at the contents of pricefile.txt

*   Verify test passed: 
    
    > $ diff -u ../../pricefile.txt pricefile.txt # Expect no output


About the Code
==============

I went for an evolutionary approach to coding this. Because there is no indication 
of how often (or if) the rules might change, or if the CSV format might change, etc. 
I decided to keep it as simple as possible (no abstractions). The CSV rows map directly 
into function calls (to avoid creating objects and mapping columns to names). Also, I decided to 
define all the rules up at the top so that common modifications (like adding or removing 
premium suppliers, product markups, etc. will be simple even for a non-programmer). I 
hardcoded the rules for trouble/premium suppliers as there was no indication this would 
change, but if in future these needed to be modificable, I would have added them to the 
configuration code at the top.

The solution started by assuming that there are no exceptions - just work out a 
markup and sell-by date. Thereafter, I modified and refactored the code to add in the 
exceptions. I think the result is a simple and easy-to-follow (and modify) code. I only 
created data mappings where it was necessary - for example, I didn't map product codes 
to product names, as that is irrelevant to the code and infact harder to maintain, as you 
would have to create new names for new product codes should there be more product types, 
whereas here you simply add another range and mapping to the @markups_for_codes hashmap.

