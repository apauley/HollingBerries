USING: kernel io accessors prettyprint locals combinators assocs continuations 
       sequences splitting calendar formatting io.files io.encodings.utf8 fry
       alien.syntax csv grouping lexer parser math math.functions math.order 
       math.parser sequences.generalizations ;

IN: holling-berries

<< SYNTAX: alist{ \ } [ 2 group >alist ] parse-literal ; >>

ALIAS: >n string>number

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

: repeat ( n seq -- new-seq ) 
    [ ] curry replicate concat ;

: read-date ( str -- date ) 
    "/" split [ >n ] map first3 <date> ;

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

: read-item ( seq -- item ) 
    6 firstn
    item new
    swap >n >>units
    swap >n 100.0 / >>price
    swap read-date >>day
    swap >>descript
    swap >n >>code
    swap >n >>supplier ;

: trouble? ( item -- ? ) 
    supplier>> { 32 101 } member? ;

: premium? ( item -- ? ) 
    supplier>> { 204 219 } member? ;

: between?' ( range x -- ? ) 
    swap first2 between? ;

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
    rest [ length 6 = ] filter [ read-item calculate present-item ] map concat but-last ;

:: holling-berries ( in out -- )
    out utf8 [
      in utf8 file>csv process "%s" printf
    ] with-file-writer ;