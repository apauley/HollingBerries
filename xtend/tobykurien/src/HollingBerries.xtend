import java.io.FileReader
import java.io.FileWriter
import java.text.SimpleDateFormat
import java.util.Date
import org.eclipse.xtend.lib.annotations.Data

import static extension com.google.common.io.CharStreams.*

@Data class Product {
    int supplierId
    int productCode
    String description
    Date deliveryDate
    int price
    int quantity
}

@Data class ProduceData {
    IntegerRange codes
    double markup
    int shelfLifeDays
}

// Main class
class HollingBerries {
    // Data for each type of produce: code range, markup, shelf life in days
    val produceData = #{
        "Apples" ->  new ProduceData((1100 .. 1199), 1.40, 14),
        "Bananas" -> new ProduceData((1200 .. 1299), 1.35, 5),
        "Berries" -> new ProduceData((1300 .. 1399), 1.55, 7),
        "Other" ->   new ProduceData(null, 1.5, 7)
    }

    // supplier ratings
    val troubleSuppliers = #[32, 101]
    val premiumSuppliers = #[219, 204]

    val inDateFormat = new SimpleDateFormat("\"yyyy/MM/dd\"")
    val outdateFormat = new SimpleDateFormat("yyyy/MM/dd")
        
    def getProduceData(Product p) {
        produceData.values.findFirst[ codes == null || codes.contains(p.productCode) ]
    }
        
    // compute the selling price based on product type and supplier
    def getSellPrice(Product p) {
        var markup = p.produceData.markup

        // adjust for suppliers
        switch p {
            case troubleSuppliers.contains(p.supplierId):
                (p.price * markup - 200.0) / 100.0
            case premiumSuppliers.contains(p.supplierId):
                Math::ceil(p.price * (markup + 0.1) / 100.0)
            default:
                p.price * markup / 100.0
        }
    }

    // compute sell by date based on product type and supplier
    def getSellBy(Product p) {
        var adj = p.produceData.shelfLifeDays

        // adjust for suppliers
        if (troubleSuppliers.contains(p.supplierId)) {
            adj = adj - 3
        }

        new Date(p.deliveryDate.time + adj * 1000 * 60 * 60 * 24)
    }

    // generate the label printer lines for each product
    def String processProduct(Product p) {
        if (p.quantity <= 0) return ""
        (0 .. p.quantity - 1).join("", [
            var sellPrice = Math.max(p.getSellPrice, 0.0)
            var sellBy = p.getSellBy
            var desc = p.description.substring(1, 32)
            '''R«String::format("% 8.2f", sellPrice)»«outdateFormat.format(sellBy)»«desc»
            '''.toString
        ])
    }

    // processing loop to read CSV and write label printer file
    def process(String sourceCsv, String outputCsv) {
        val out = new FileWriter(outputCsv)
        new FileReader(sourceCsv).readLines.drop(1).forEach [
            val cols = it.split(",(?=([^\"]*\"[^\"]*\")*[^\"]*$)", -1).iterator
            val product = new Product(
                Integer::parseInt(cols.next),
                Integer::parseInt(cols.next),
                cols.next,
                inDateFormat.parse(cols.next),
                Integer::parseInt(cols.next),
                Integer::parseInt(cols.next)
            )
            out.write(processProduct(product))
        ]
        out.close
    }

    def static void main(String[] args) {
        new HollingBerries().process("../../produce.csv", "pricefile.txt")
    }
}
