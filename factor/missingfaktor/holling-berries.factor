USING: kernel io accessors prettyprint alien.syntax csv io.encodings.utf8 grouping
       present sequences splitting calendar formatting locals combinators assocs
       combinators continuations lexer parser math math.functions math.order
       math.parser io.files ;

IN: holling-berries

<< SYNTAX: alist{ \ } [ 2 group >alist ] parse-literal ; >>

ENUM: kind apple banana berry other ;

CONSTANT: markup-table alist{
    apple 1.4
    banana 1.35
    berry 1.55
    other 1.5
}

CONSTANT: shelf-table alist{
    apple 14
    banana 5
    berry 7
    other 7
}

CONSTANT: code-table alist{
    apple {
        { 1100 1199 }
    }
    banana {
        { 1200 1299 }
    }
    berry {
        { 1300 1399 }
    }
    other {
        { 1000 1099 }
        { 1400 1999 }
    }
}

: apply ( seq quot -- x ) 
    with-datastack first ;

: repeat ( n seq -- new-seq ) 
    [ ] curry replicate concat ;

: trouble? ( x -- ? ) 
    { 32 101 } member? ;

: premium? ( x -- ? ) 
    { 204 219 } member? ;

: read-date ( str -- date ) 
    "/" split [ string>number ] map [ <date> ] apply ;

: present-date ( date -- str ) 
    "%Y/%m/%d" strftime ; 

TUPLE: item supplier code descript day price units ;

C: <item> item

: present-item ( item -- str )
    [ units>> ]
    [
        [ price>> ]
        [ day>> present-date ]
        [ descript>> 31 head  ] tri "R%8.2f%10s%31s\n" sprintf
    ] bi repeat ;

ALIAS: >n string>number

: read-item ( seq -- item ) 
    { [ >n ] [ >n ] [ ] [ read-date ] [ >n 100.0 / ] [ >n ] } 
    [ call( x -- x ) ] 2map 
    [ <item> ] apply ;

: between?' ( range x -- ? ) 
    swap [ first ] [ second ] bi between? ;

:: calculate ( item -- item' )
    code-table [ second [ item code>> between?' ] any? ] find first :> kind
    item supplier>> :> supplier
    item price>> :> price
    supplier trouble? :> tr
    supplier premium? :> pr
    kind markup-table at :> markup
    kind shelf-table at :> shelf
    pr [
        markup 0.1 + price * ceiling 
    ] [
        markup price * tr 2 0 ? -  
    ] if 0 max :> price'
    item day>> shelf tr 3 0 ? - days time+ :> day'
    item clone price' >>price day' >>day nip ;

: process ( csv -- str  )
    rest [ length 6 = ] filter [ read-item calculate present-item ] map concat  ;

: main ( -- )
    "pricefile.txt" utf8 
    "produce.csv" utf8 file>csv process [ print ] curry 
    with-file-writer ;
    
MAIN: main