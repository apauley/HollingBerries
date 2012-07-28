=========================
Postgresql implementation
=========================

How to
------

To run the script, you need access to a  postgresql server in an existing database. In a shell, change to the project's directory where the csv file lies and enter

::

    psql database -U user -h host -X -q -o my_pricelist.txt < sql/chanmix51/prices.sql

You can omit the database, user and host if you use the according environment variables (see psql manpage for details).

The script will create a new schema named ``import`` in the database and create a ``produce`` table from the ``produce.csv`` file. Once the output has been sent, the schema and all it contains is destroyed.

How does it work ?
------------------

#. Create a transaction so if something fails the whole thing will be rollback'ed.
#. Create ``import`` schema in which the objects & functions are going to be stored.
#. Create all business oriented functions.
#. Load the CSV file in a tabled named ``import.produce``.
#. Query the table and output the price list as expected.
#. Drop the schema and all it contains.

The SQL statement to output the results works as follow. The starting structure is the ``produce`` table::

    Table "import.produce"
        Column     │       Type
    ───────────────┼────────────────────
     supplier_id   │ integer
     product_code  │ product_type
     description   │ character varying
     delivery_date │ date
     cost_price    │ integer
     unit_count    │ integer

The first step is to generate on line per item with all information related to product type like markup and max sell date:

::

    produce_fruit (supplier_id, markup, price, max_sell_date, description, item_no) AS (
        SELECT 
            supplier_id,
            get_markup(product_code) AS markup,
            cost_price AS price,
            get_delivery_date(product_code, delivery_date) AS max_sell_date,
            description,
            generate_series(1, unit_count) AS item_no
        FROM
            import.produce
        )

The ``generate_series`` is here to expand as many lines as unit count.

From this can be applied to rules related to the suppliers, hence the final prices of the products::

    produce_supplier (price, max_sell_date, description) AS (
        SELECT
            CASE
                WHEN is_trouble_supplier(supplier_id) THEN get_price_trouble_supplier(price, markup)
                WHEN is_premium_supplier(supplier_id) THEN get_price_premium_supplier(price, markup)
                ELSE get_price_normal_supplier(price, markup)
            END AS price,
            CASE 
                WHEN is_trouble_supplier(supplier_id) THEN adjust_trouble_seller_price(max_sell_date)
                ELSE max_sell_date
            END AS max_sell_date,
            description
        FROM
            produce_fruit
        )

The result is a set containing all we need for the output string: final price, max sell date and description. It just needs to be formatted:

    ::

    SELECT 'R'||substr(to_char(price, '99990.00'), 2)||to_char(max_sell_date, 'YYYY/MM/DD')||substr(description,1 ,31) FROM produce_supplier;

Note the ``substr(to_char(price...`` is needed only because it seems Postgres is keeping an extra space for the +/- sign in front of the formatted string.
