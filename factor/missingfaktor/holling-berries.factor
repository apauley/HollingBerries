USING: kernel io accessors prettyprint locals combinators assocs continuations 
       sequences splitting calendar formatting io.files io.encodings.utf8 fry
       alien.syntax csv grouping lexer parser math math.functions math.order 
       math.parser ;

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

: trouble? ( item -- ? ) 
    supplier>> { 32 101 } member? ;

: premium? ( item -- ? ) 
    supplier>> { 204 219 } member? ;

: between?' ( range x -- ? ) 
    swap [ first ] [ second ] bi between? ;

: item-kind ( item -- kind )
    '[ second [ _ code>> between?' ] any? ] code-table swap find first nip ;

:: calculate ( item -- item' )
    item clone :> item'
    item' { 
        [ item-kind ] [ price>> ] [ trouble? ] [ premium? ] [ day>> ] 
    } cleave :> ( k pr tr pm d )
    k [ markup-table at ] [ shelf-table at ] bi :> ( mk sh )
    item' pm [ mk 0.1 + pr * ceiling ] [ mk pr * tr 2 0 ? - ] if 0 max >>price
          d sh tr 3 0 ? - days time+ >>day ;
    
: process ( csv -- str  )
    rest [ length 6 = ] filter [ read-item calculate present-item ] map concat  ;

: main ( -- )
    "pricefile.txt" utf8 
    "produce.csv" utf8 file>csv process [ print ] curry 
    with-file-writer ;
    
MAIN: main