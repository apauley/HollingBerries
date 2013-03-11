Introduction
============
Meet Mr. and Mrs. Hollingberry. Recently retired, Mr. Hollingberry has decided
to move to sunny South Africa and open up a small local convenience store to
keep him and Mrs. Hollingberry out of mischief.

Alas, it turned out not be such a laid-back job as he had hoped...

The Problem
===========
One of their current problems is printing price tags on their fresh produce.
Every morning, as soon as the various produce have been delivered,
Mrs. Hollingberry enters it into a program her nephew had written.

The result is a comma-seperated file that includes, among other
fields, the cost price (in cents) and delivery date of each product.

The Task
========
Your job is to write a program that reads the csv file and then creates
a new file that will be used to print out the price tags.

The Input File
===============
An example csv file is in this directory (produce.csv).
We use the following fields:

*   Supplier ID.

    All suppliers are equal, but some are more equal than others.

*   Product code.

    This tells us what kind of produce we're dealing with.

*   Description.

    We can print part of this on the price tag.

*   Delivery date.

    YYYY/MM/DD. We use this to calculate the sell-by date.

*   Cost price.

    In cents. We use this to calculate the selling price.

*   Unit count.

    We need to print a price tag for each item delivered.

The Output File
===============
An example output file generated from the example input file is in this directory (pricefile.txt).
The price file has 3 fields on each line:
the selling price, the sell-by date and a product description.

The price file is in fixed-width format, because the label printer has
limited space (50 characters) for each price tag. Each line in the price file will
cause one price tag to be printed.

The selling price takes up 9 characters. One currency symbol (R), 5
digits before the decimal (a dot) and 2 digits afer the decimal.
Like this: R99999.99

Mr Hollingberry says we shouldn't worry about larger amounts. If he
ever sells something for a 100 grand he will have to retire again, and
he can't take that kind of stress again.

The sell-by date, just like the delivery date in the input file, is in
YYYY/MM/DD format (10 characters).

The remaining 31 characters is used for the product description.

A typical line in the price file will look like this:

> R   19.922012/05/26Apples 1kg Green. They are very

The Business Rules
==================
You have to calculate the selling price and the sell-by date. Luckily
we can use the description just as it is in the csv file. Well, the
first 31 characters of it anyway.

## Markup Rules
* The markup for apples  is 40%.
* The markup for bananas is 35%.
* The markup for berries is 55%.
* The markup for anything else 50%.

## Sell-by Dates
* Apples have to be sold 2 weeks after the date of delivery.
* Bananas have to be sold 5 days after the date of delivery.
* All other types of fruit has to be sold 1 week after the date of
delivery.

## Product Codes
* Fruit has product codes ranging from 1000 to 1999.
* Apples specifically have product codes ranging from 1100 to 1199.
* Bananas have product codes ranging from 1200 to 1299.
* Berries have product codes ranging from 1300 to 1399.

## Supplier Troubles
One of the suppliers, Susan Windler (Supplier ID 32), has been known to deliver
fruit that is not quite as fresh as that of the other suppliers.
Mr. Hollingberry has decided to handle this quietly, by:
*   ensuring that the sell-by date for anything delivered by Susan is always 3
    days earlier than normal.
*   making the price R2 less than usual.

Come to think of it, Togetherness Tshabalala (Supplier ID 101), also
needs to be on this list.

## Premium Produce (tm)
Some suppliers are dedicated Premium Produce (tm) suppliers.
The customer has to believe that they are buying something better than
usual.
Mr Hollingberry does this by making the packaging look nice, and by increasing the
price.
The suppliers already took care of the nice packaging, you now have to
make it expensive. Anything supplied by a Premium Produce (tm)
supplier gets an additional 10% markup, and then the price is rounded
up to the nearest Rand.

For example, a product that would usually have a 50% markup would now have a 60% markup.
(It would be incorrect to first apply a 50% markup and then apply another 10% on top of that).
If a product costs R25.11 after the extra 10% markup has
been applied, you need to round it up to R26.

The Premium Produce suppliers currently are:
*   Promise Mashangu (Supplier ID 219)
*   Karel Visser (Supplier ID 204)

How do I contribute my solution?
================================

*   Fork https://github.com/apauley/HollingBerries on github.

*   Make a subdirectory for your language, if it isn't already there

*   Put your code in a subdirectory named with your github username

    > eg. ocaml/yminsky

*   If you want, put a description after your github username

    > eg. ruby/apauley-functionalruby

*   Make sure that your solution generates exactly the same output
    file when given the sample input file:

    > $ diff -u ../../pricefile.txt pricefile.txt # Expect no output

*   Include a README file with instructions on how to build/run your
    code. A Makefile or something similar will be nice. Also, tell us
    a bit about your solution if you feel like it.

*   Ensure that your repository is up to date:

    > $ git pull --rebase

*   Send me a pull request so that your solution is included with the
    others. By doing this you agree that your code may be used by
    anyone for any purpose. If you use someone elses code it will be
    nice to give him/her credit. Email me if you have a suggestion for an informal
    license that fits these code comparisons. MIT? Creative Commons?

Why this example?
=================

Programmers with an Object Oriented background that are interested in
Functional Programming (like myself) often have the following
concerns:

"How do I design/model a problem when I don't have my trusted classes
and objects available?"

If you're used to the "everything must be an object" philosophy, you
might feel that your hands are cut off.

I wanted a scenario that a typical OO programmer would solve using an
object hierarchy with some polymorphism. And then have solutions for
the same problem in functional programming languages so that it can be
easily compared.

Of course it will also be very interesting to see paradigms other than
OO and FP :-)
