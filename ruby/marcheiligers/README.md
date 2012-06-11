This README stolen from apauly's version and tweaked.

How to run this Ruby version
============================

*   Change to this directory in a terminal

*   Run prices.rb

    > $ ruby prices.rb

*   Look at the contents of pricefile.txt


About the Code
==============

The example lends itself to a simple Product/Fruit object hierarchy,
which is totally not what I've used here. Unlike _apauly_, instead I've 
decided that products are products, but there are Rules which change 
how a Product's markup and shelf life is calculated.

The extra supplier logic is something which does not fit in with the
_apualy_'s hierarchy, doesn't exist in this example. Instead all Products
are tested against all Rules and those rules that match a product are
applied. There are the basic fruit rules which set markup % and shelf
life in days, and then the special Supplier rules which actually
override the way the calculations are done. All the rules use some mild
meta-programming, which I like.

Some interesting features:
* Rules are never actually instantiated. 
* It's really easy to add additional Rules, both for new types of Fruit
and for new Supplier "issues". But, there's no support for Supplier rules
that apply to the same product. I haven't thought about how one might 
implement that using this solution.
* The actual LabelPrinter class just pulls functionality from the 3
Modules. It's really easy to split the responsibilities, add rules, even
have multiple different label printers, perhaps for different shops.
* In a very non-Rubyist approach, I've done no testing what-so-ever :(
