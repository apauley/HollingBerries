Introduction
============
I did this version out of curiosity to see what it would look like in a procedural language such as SAP's ABAP language. 

ABAP started as a procedural language but now also supports object oriented programming and most of the constructs which you would find in other OO languages such as Java.

In this example I went for a very procedural approach, although I made some calls to ABAP Objects(e.g. uploading the files). 

I was really surprised by how verbose this example turned out. That said, the example is not typical of the processing which would be done in SAP. If the source file and rules were stored in the database this program would have been a lot simpler. I also took what I believe would have been a typical ABAP approach and coded the Supplier and Produce rules as if it would've been stored in a database table(which added significantly to the number or lines).


Running this Example
====================
Well, first you will need access to an ABAP Server or install one. If you don't have access to one you can download a trial version from here: http://www.sdn.sap.com/irj/scn/nw-downloads

This program is a simple ABAP report and should run on most systems(I used bare NW 7.02). 

Steps to Install:
*   Create a new program in transaction SE38 and call it ZHOLLING.
*   Paste the code into the Editor.
*   Save, Activate and Execute.



