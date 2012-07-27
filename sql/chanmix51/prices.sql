-- price calculus SQL script
\a
\t

BEGIN;
CREATE SCHEMA import;
SET search_path TO import, public;

CREATE DOMAIN product_type AS integer CHECK ( VALUE BETWEEN 1000 AND 1999 );

CREATE OR REPLACE FUNCTION import.is_apple(product_type) RETURNS boolean AS $_$
    SELECT $1 BETWEEN 1100 AND 1199;
$_$ LANGUAGE sql IMMUTABLE;

CREATE OR REPLACE FUNCTION import.is_banana(product_type) RETURNS boolean AS $_$
    SELECT $1 BETWEEN 1200 AND 1299;
$_$ LANGUAGE sql IMMUTABLE;

CREATE OR REPLACE FUNCTION import.is_berry(product_type) RETURNS boolean AS $_$
    SELECT $1 BETWEEN 1300 AND 1399;
$_$ LANGUAGE sql IMMUTABLE;

CREATE OR REPLACE FUNCTION import.is_trouble_supplier(integer) RETURNS boolean AS $_$
    SELECT $1 IN (32, 101);
$_$ LANGUAGE sql IMMUTABLE;

CREATE OR REPLACE FUNCTION import.is_premium_supplier(integer) RETURNS boolean AS $_$
    SELECT $1 IN (204, 219);
$_$ LANGUAGE sql IMMUTABLE;

CREATE OR REPLACE FUNCTION import.get_markup(product_type) RETURNS numeric AS $_$
    SELECT 
        CASE 
            WHEN import.is_apple($1)  THEN 1.4
            WHEN import.is_banana($1) THEN 1.35
            WHEN import.is_berry($1)  THEN 1.55
            ELSE 1.5
        END;
$_$ LANGUAGE sql IMMUTABLE;

CREATE OR REPLACE FUNCTION import.get_delivery_date(product_type, date) RETURNS date AS $_$
    SELECT
        CASE
            WHEN import.is_apple($1)  THEN CAST($2 + interval '2 weeks' AS date)
            WHEN import.is_banana($1) THEN CAST($2 + interval '5 days' AS date)
            ELSE CAST($2 + interval '1 week' AS date)
        END;
$_$ LANGUAGE sql IMMUTABLE;

CREATE OR REPLACE FUNCTION get_price_trouble_supplier(integer, numeric) RETURNS numeric AS $_$
    SELECT greatest(0, ($1 * $2 * 0.01) - 2);
$_$ LANGUAGE sql IMMUTABLE;

CREATE OR REPLACE FUNCTION get_price_premium_supplier(integer, numeric) RETURNS numeric AS $_$
    SELECT ceil($1 * ($2 + 0.1) * 0.01);
$_$ LANGUAGE sql IMMUTABLE;

CREATE OR REPLACE FUNCTION get_price_normal_supplier(integer, numeric) RETURNS numeric AS $_$
    SELECT $1 * $2 * 0.01;
$_$ LANGUAGE sql IMMUTABLE;

CREATE OR REPLACE FUNCTION adjust_trouble_seller_price(date) RETURNS date AS $_$
    SELECT CAST($1 - interval '3 days' AS date);
$_$ LANGUAGE sql IMMUTABLE;

CREATE TABLE import.produce (
      supplier_id integer NOT NULL, 
      product_code product_type NOT NULL, 
      description varchar, 
      delivery_date date NOT NULL, 
      cost_price integer NOT NULL CHECK (cost_price >= 0), 
      unit_count integer NOT NULL CHECK (unit_count >= 0), 
      PRIMARY KEY (product_code, delivery_date, supplier_id)
      )
    ;

\copy import.produce FROM 'produce.csv' WITH CSV HEADER QUOTE '"';

WITH
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
    ),
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
SELECT 'R'||substr(to_char(price, '99990.00'), 2)||to_char(max_sell_date, 'YYYY/MM/DD')||substr(description,1 ,31) FROM produce_supplier;

DROP SCHEMA import CASCADE;
COMMIT;
