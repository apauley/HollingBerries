import csv
import math
import os
from datetime import datetime, timedelta

BASE_PATH = os.path.join(os.path.dirname(__file__), '..', '..')

PRODUCT_CATEGORIES = {
    'Apples':  range(1100, 1200),
    'Bananas': range(1200, 1300),
    'Berries': range(1300, 1400),
}

DEFAULT_MARKUP = 50
PRODUCT_MARKUPS = {'Apples':  40, 'Bananas': 35, 'Berries': 55, }

DEFAULT_SELLBY = 7
PRODUCT_SELLBYS = {'Apples':  14, 'Bananas': 5, }

PREMIUM_SUPPLIERS = [204, 219]
UNFRESH_SUPPLIERS = [32, 101]

class Product(object):
    def __init__(self, *args, **kwargs):
        self.__dict__.update(**kwargs)

def parse_products(csv_file):
    def clean(row):
        return Product(**dict(supplier=int(row['Supplier ID']),
            code=int(row['Product Code']),
            description=row['Product Description'],
            delivery=datetime.strptime(row["Delivery Date"], "%Y/%m/%d"),
            price=int(row["Unit Price"]), units=int(row["Number of Units"])))
    return filter(lambda p: p.units > 0,
                  map(clean, csv.DictReader(csv_file)))

def category(product):
    return [category for (category, codes) in PRODUCT_CATEGORIES.iteritems()
            if product.code in codes][0]

def price(product):
    markup = PRODUCT_MARKUPS.get(category(product), DEFAULT_MARKUP)
    markup += 10 if product.supplier in PREMIUM_SUPPLIERS else 0
    price = product.price * (1 + markup / 100.0)
    price -= 200 if product.supplier in UNFRESH_SUPPLIERS else 0
    price = price / 100.0 if price > 0 else 0
    return math.ceil(price) if product.supplier in PREMIUM_SUPPLIERS else price

def sell_by(product):
    delta = PRODUCT_SELLBYS.get(category(product), DEFAULT_SELLBY)
    delta -= 3 if product.supplier in UNFRESH_SUPPLIERS else 0
    return ((product.delivery + timedelta(days=delta)).strftime("%Y/%m/%d"))

def get_lines(product):
    line = lambda p: "R% 8.2f%s%s" % (price(p), sell_by(p), p.description[:31])
    return [line(product) for _ in xrange(product.units)]

def process(input_file, output_file):
    output_file.write('\n'.join(['\n'.join(get_lines(p))
                      for p in parse_products(input_file)]) + '\n')
    input_file.close()
    output_file.close()

if __name__ == '__main__':
    process(open(os.path.join(BASE_PATH, 'produce.csv'), 'rb'),
            open('pricefile.txt', 'w'))
