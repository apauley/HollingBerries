Meet Mr. Hollingberry. Recently retired, Mr. Hollingberry has decided
to move to sunny South Africa and open up a local convenience store to
keep him and Mrs. Hollingberry out of mischief.

Alas, it turned out not be such a laid-back job as he had hoped...

One of their current problems is printing price tags on their fresh produce.
Every morning as soon, as the various produce have been delivered,
Mrs. Hollingberry enters it into a program her nephew had written.

The result is a comma-seperated file that includes, among other
fields, the cost price (in cents) and delivery date of each product.

Your job is to write a program that reads this file and then creates
a new file that will be used to print out the price tags.
The price file has 3 fields on each line: the selling price, the
sell-by date and a product description.

The price file is in fixed-width format, because the label printer has
limited space for each price tag. Each line in the price file will
cause one price tag to be printed.

The selling price takes up 8 characters. One currency symbol (R) and 7
digits where Rands and cents are seperated by a period: R9999.99

The sell-by date, just like the delivery date in the input file, is in
YYYY/MM/DD format (10 characters).

The remaining 22 characters is used for the product description.

A typical line in the price file will look like this:
R  17.952012/05/19Apples 1kg Golden Deli

You have to calculate the selling price and the sell-by date. Luckily
we can use the description just as it is in the csv file.

The markup for apples golden delicious apples is 75%.
The markup for apples both red and green apples is 70%.
The markup for bananas is 85%.
The markup for milk is 20%.
The markup for meat is 25%.

Apples have to be sold 2 weeks after the date of delivery.
Bananas have to be sold 5 days after the date of delivery.
All other types of fruit has to be sold 1 week after the date of
delivery.

Milk has to be sold 2 days after the date of delivery.
Meat has to be sold 1 week after the date of delivery.
