extern crate csv;
extern crate rustc_serialize;
extern crate core;

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
	let price = (produce.unit_price as f32) / 100.0;
	let prod_type = produce_type(&produce);
	price * MARKUP[prod_type].1
}

fn getSellBy(produce: &ProduceRecord) -> String {
	produce.delivery_day.clone()
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
    let mut rdr = csv::Reader::from_file("../../produce.csv").unwrap();
    for record in rdr.decode() {
        let record: ProduceRecord = record.unwrap();
        let output = processProduce(record);
        if output.len() > 0 { println!("{}", output); }        
    }
}
