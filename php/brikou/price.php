<?php

function getRows()
{

    $handle = fopen(__DIR__.'/../../produce.csv', 'r');

    $rows = array();

    while (($columns = fgetcsv($handle))) {
        $rows[] = $columns;
    }
    fclose($handle);

    $jeys = array_shift($rows);

    for ($j = 0, $ji = count($rows); $j < $ji; $j++) {
        $rows[$j] = array_combine($jeys, $rows[$j]);
    }

    return $rows;
};

function isApple($productCode)
{
    return ($productCode >= 1100 && $productCode < 1200);
}

function isBanana($productCode)
{
    return ($productCode >= 1200 && $productCode < 1300);
}

function isBerry($productCode)
{
    return ($productCode >= 1300 && $productCode < 1400);
}

function getSellingPrice($productCode, $supplierId, $unitPrice)
{
    switch (true) {
        case isApple($productCode):
            $markup = 1.40;
            break;
        case isBanana($productCode):
            $markup = 1.35;
            break;
        case isBerry($productCode):
            $markup = 1.55;
            break;
        default:
            $markup = 1.50;
            break;
    }

    if (in_array($supplierId, array(219, 204)))  {
        $sellingPrice = ceil($unitPrice * ($markup + 0.10));
    } else {
        $sellingPrice = $unitPrice * $markup;
    }

    if (in_array($supplierId, array(32, 101)))  {
        $sellingPrice = max(0, $sellingPrice - 2);
    }

    return $sellingPrice;
}

function getSellByDate($productCode, $supplierId, $deliveryDate)
{
    switch (true) {
        case isApple($productCode):
            $time = '2 weeks';
            break;
        case isBanana($productCode):
            $time = '5 days';
            break;
        default:
            $time = '1 week';
            break;
    }

    $sellByDate = DateTime::createFromFormat('Y/m/d', $deliveryDate);
    $sellByDate->add(DateInterval::createFromDateString($time));

    if (in_array($supplierId, array(32, 101)))  {
        $sellByDate->sub(DateInterval::createFromDateString('3 days'));
    }

    return $sellByDate->format('Y/m/d');
}

ini_set('date.timezone', 'UTC');

$rows = getRows();

ob_start();

$lines = file(__DIR__.'/../../pricefile.txt');

foreach($rows as $row) {

    $sellingPrice = getSellingPrice($row['Product Code'], $row['Supplier ID'], $row['Unit Price'] / 100);
    $sellByDate = getSellByDate($row['Product Code'], $row['Supplier ID'], $row['Delivery Date']);
    $productDescription = substr($row['Product Description'], 0, 31);

    echo str_repeat(sprintf("R%8.2f%s%s\n", $sellingPrice, $sellByDate, $productDescription), $row['Number of Units']);
}

$data = ob_get_clean();

file_put_contents(__DIR__.'/pricefile.txt', $data);
