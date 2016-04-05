extern crate csv;
extern crate rustc_serialize;

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

// Map product code to Product type using pattern matching
fn produce(product_code: u32) -> Produce {
	match product_code {
		1100...1199 => Produce::Apple,
		1200...1299 => Produce::Banana,
		1300...1399 => Produce::Berry,
		_ => Produce::Other
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
    units: u32
}

fn main() {
    let mut rdr = csv::Reader::from_file("../../produce.csv").unwrap();

    for record in rdr.decode() {
        let record: ProduceRecord = record.unwrap();
        println!("({}, {}): {}", record.supplier_id, record.product_code, record.product_desc);
    }
}
