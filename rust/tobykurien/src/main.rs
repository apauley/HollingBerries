extern crate csv;
extern crate rustc_serialize;
extern crate chrono;

use chrono::*;
use std::fs::File;
use std::io::prelude::*;

// ******** Business rules data ********
enum Produce { Apple, Banana, Berry, Other }

const MARKUP: [(Produce, f32); 4] = [ 
	(Produce::Apple, 1.4),
	(Produce::Banana, 1.35),
	(Produce::Berry, 1.55),
	(Produce::Other, 1.5),			 
];

const SHELF_LIFE: [(Produce, u32); 4] = [ 
	(Produce::Apple, 14),
	(Produce::Banana, 5),
	(Produce::Berry, 7),
	(Produce::Other, 7),			 
];

const TROUBLE_SUPPLIERS: [u32; 2] = [ 32, 101 ];
const PREMIUM_SUPPLIERS: [u32; 2] = [ 204, 219 ];

// Map product code to Produce type using pattern matching
fn produce_type(produce: &ProduceRecord) -> usize {
	match produce.product_code {
		1100...1199 => Produce::Apple as usize,
		1200...1299 => Produce::Banana as usize,
		1300...1399 => Produce::Berry as usize,
		_ => Produce::Other as usize
	}
}

// Delivery times for various produce
fn delivery_days(produce: &ProduceRecord) -> i64 {
	match produce.product_code {
		1100...1199 => 14,
		1200...1299 => 5,
		_ => 7
	}
}


// ******** Application code ********

#[derive(RustcDecodable)]
struct ProduceRecord {
    supplier_id: u32,
    product_code: u32,
    product_desc: String,
    delivery_day: String,
    unit_price: u32,
    units: u32,
}

fn getSellPrice(produce: &ProduceRecord) -> f32 {
	let price = produce.unit_price as f32;
	let prod_type = produce_type(&produce);
	let ret: f32 = if TROUBLE_SUPPLIERS.iter().any(|i| produce.supplier_id == *i) {
		(price * MARKUP[prod_type].1 - 200.0)/ 100.0
	} else if PREMIUM_SUPPLIERS.iter().any(|i| produce.supplier_id == *i) {
		((price * (MARKUP[prod_type].1 + 0.1))/ 100.0).ceil()
	} else {
		(price * MARKUP[prod_type].1)/ 100.0
	};
	
	ret.max(0.0)
}

fn getSellBy(produce: &ProduceRecord) -> String {
	let mut adj = delivery_days(produce);

	if TROUBLE_SUPPLIERS.iter().any(|i| produce.supplier_id == *i) {
		adj = adj - 3
	};
	
	let day = produce.delivery_day.clone();
	let date: String = " 00:00:00 +00:00".chars().enumerate().fold(day, |res, (i, ch)| {
        res + &format!("{}", ch)
    });
	let dt = DateTime::parse_from_str(&date, "%Y/%m/%d %H:%M:%S %z")
				.expect("Invalid date");
	(dt + Duration::days(adj)).format("%Y/%m/%d").to_string()
}

fn processProduce(produce: ProduceRecord) -> String {
	let lines: Vec<String> = (0..produce.units).map(|a| {
		let mut desc: String = produce.product_desc.clone();
		desc.truncate(31);
		format!("R{:8.2}{}{}", getSellPrice(&produce), getSellBy(&produce), desc)
	}).collect();
	
	lines.as_slice().join("\n")
}

fn main() {
    let mut f: File = File::create("pricefile.txt").expect("Can't open output file!");
    let mut rdr = csv::Reader::from_file("../../produce.csv").unwrap();
    for record in rdr.decode() {
        let record: ProduceRecord = record.unwrap();
        let output = processProduce(record);
        if output.len() > 0 { 
        	f.write(output.as_bytes()); 
        	f.write(b"\n");
        }
    }
}
