#!/usr/bin/awk -f
#   Usage: Hollingberries.awk <produce.csv >pricefile.txt

#   Problems.

# 1 CSV technically allows newlines inside quoted strings, which this
#   code does not.  https://github.com/apauley/HollingBerries does not
#   say whether descriptions can include newlines.
#   Quick hack: do not allow newlines.

# 2 I'm _guessing_ that supplier IDs are to be treated as numbers,
#   so that 1, 01, 001, are all the same supplier.
#   Quick hack: $x + 0 forces field x to be treated as a number.

# 3 What should we do for product IDs outside the fruit range?
#   Quick hack: complain and stop the program.

# 4 What should we do if a description is shorter than 31 characters?
#   Quick hack: use %31.13s.

# 5 What is the range of delivery dates?
#   Quick hack: the code used here can handle a much larger range of
#   dates than I think the problem calls for.

# 6 https://github.com/apauley/HollingBerries says that the input
#   dates are YYYY-MM-DD; they are in fact YYYY/MM/DD.
#   Quick hack: treat all non-digit characters alike.

# 7 What should we do if the date is invalid?
#   Quick hack: ignore gross errors, accept February 31.

# 8 https://github.com/apauley/HollingBerries says that
#   the price has 8 digits, but there is no room for "R" and "."
#   and 8 digits in a 9 column field.
#   Quick hack: assume 5 digits before the point were wanted.

# 9 What if the price reduction for troubled suppliers results
#   in a negative price?
#   Quick hack: clamp the price at 0.  (Checked pricefile.txt!)

# This is 82 SLOC of AWK.  The trickiest code is that dealing
# with dates and CSV files, neither of which comes with AWK.
# Without those, it would be 45 SLOC.
# As for the rest, it is very difficult to see any role for objects here.

function err(msg) {
    print msg >"/dev/stderr"
    exit(1)
}

#   The following functions for converting between calendar dates
#   and day numbers are adapted from C code written by James Bielak.
#   See Computer Language Magazine, December 1990, page 57.

function date_to_julian(date	,y,m,d,a,b,c) {
    n_fields = split(date, fields, /[^0-9]+/)
    if (n_fields != 3) err("bad date 1 " delivery)
    y = fields[1]
    m = fields[2]
    d = fields[3]
    if (y < 2000 || m < 1 || m > 12 || d < 1 || d > 31) 
	err("bad date 2 " delivery)
    c = y > 0 ? 0.0 : 0.75;
    if (m <= 2) { y--; m += 12 }
    b = 0
    if (y*10000 + m*100 + d >= 15821015) {
	a = int(y / 100)
	b = 2 - a + int(a / 4)
    }
    return int(365.25 * y - c) + int(30.6001 * (m+1)) + d + 1720994 + b
}

function julian_to_date(julian	,a,b,c,d,e,z,g,y,m) {
    z = julian + 1
    if (z < 2299161) {
        a = z
    } else {      
        g = int((z - 1867216.25) / 36524.25)
        a = z + 1 + g - int(g / 4)
    }
    b = a + 1524
    c = int((b - 122.1) / 365.25)
    d = int(365.25 * c)
    e = int((b - d) / 30.6001);
    d = b - d - int(30.6001 * e)
    m = e <= 13 ? e - 1    : e - 13
    y = m >= 3  ? c - 4716 : c - 4715
    return sprintf("%4d/%02d/%02d", y, m, d)
}

BEGIN {
    type[ 32] = "troubled"	# Susan Windler
    type[101] = "troubled"	# Togetherness Tshabalala
    type[204] = "premium"	# Promise Mashangu
    type[219] = "premium"	# Karel Visser
}

NR > 1 {
    line = "," $0
    n_fields = 0
    while (match(line, /^,([^,"]|"([^"]|"")*")*/)) {
	field = substr(line, RSTART+1, RLENGTH-1) # skip the comma
	line = substr(line, RSTART+RLENGTH)
	gsub(/""/, "\034", field) # \034 is Field Separator
	gsub(/"/, "", field)
	gsub(/\034/, "\"", field)
	fields[++n_fields] = field
    }
    if (n_fields != 6) err("wrong number of fields")
    supplier    = fields[1]+0
    product     = fields[2]+0
    description = fields[3]
    delivery    = fields[4]
    base_price  = fields[5]+0
    count       = fields[6]+0
    julian      = date_to_julian(delivery)

 ## Product rules

    if (product >= 1100 && product <= 1199) {	# Apples
        markup = 40
	life   = 14
    } else
    if (product >= 1200 && product <= 1299) {	# Bananas
        markup = 35
        life   = 5
    } else
    if (product >= 1300 && product <= 1399) {	# Berries
        markup = 55
        life   = 7
    } else
    if (product >= 1000 && product <= 1999) {	# Other fruit
        markup = 50
        life   = 7
    } else {
	err("bad product code " product)
    }

 ## Supplier rules

    if (type[supplier] == "premium") {
	price = base_price * (110 + markup) / 100  # in cents
	price = (int(price/100) + (price > int(price))) * 100
    } else
    if (type[supplier] == "troubled") {
	price = base_price * (100 + markup) / 100  # in cents	
	price = int(price + 0.5) - 200
	if (price < 0) price = 0
	life -= 3
    } else {
	price = base_price * (100 + markup) / 100  # in cents	
	price = int(price + 0.5)
    }

 ## Output

    output = sprintf("R%5d.%02d%s%31.31s", \
	int(price/100), price % 100, 	   \
	julian_to_date(julian+life), description)
    for (i = 1; i <= count; i++) print output
}

