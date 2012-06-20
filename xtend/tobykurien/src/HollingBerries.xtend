import java.io.FileReader
import java.text.SimpleDateFormat
import java.util.Date
import org.eclipse.xtend.lib.Data
import static HollingBerries.*
import static extension com.google.common.io.CharStreams.*
import java.io.FileWriter
import java.util.Calendar

// Class to hold product data
@Data class Product {
  int supplierId
  int productCode
  String description
  Date deliveryDate
  int price
  int quantity
}

// Main class
class HollingBerries {
   // supplier ratings
   val troubleSuppliers = newArrayList(32, 101)
   val premiumSuppliers = newArrayList(219, 204)

   // compute the selling price based on product type and supplier
   def getSellPrice(Product p) { 
      var markup = switch p {
         case (1100..1199).contains(p.productCode): 40
         case (1200..1299).contains(p.productCode): 35
         case (1300..1399).contains(p.productCode): 55
         default: 50      
      }
      
      // adjust for suppliers
      sanePrice(switch p {
         case troubleSuppliers.contains(p.supplierId):
            (p.price * (1 + markup/100.0) - 200) / 100.0
         case premiumSuppliers.contains(p.supplierId):
            Math::ceil(p.price * (1 + (markup+10)/100.0) / 100.0)
         default:
            p.price * (1 + markup/100.0) / 100.0
      })
   }

   // compute sell by date based on product type and supplier
   def getSellBy(Product p) {
      var adj = switch p {
         case (1100..1199).contains(p.productCode): 14
         case (1200..1299).contains(p.productCode): 5
         default: 7
      }
      
      // adjust for suppliers
      if (troubleSuppliers.contains(p.supplierId)) {
            adj = adj - 3
      }
      
      new Date(p.deliveryDate.time + adj*1000*60*60*24)
   }

   // make sure prices are sane
   def sanePrice(double price) {
      if (price < 0) 0 else price
   }

   // generate the label printer lines for each product
   def String processProduct(Product p) {
      if (p.quantity == 0) return ""
      (0..p.quantity-1).join("", [
         var sellPrice = p.getSellPrice
         var sellBy = p.getSellBy
         var desc = p.description.substring(1,32)
         
         var sdf = new SimpleDateFormat("yyyy/MM/dd")
         '''R«String::format("% 8.2f", sellPrice)»«sdf.format(sellBy)»«desc»
         '''.toString             
      ])
   }
   
   // processing loop to read CSV and write label printer file
   def process(String sourceCsv, String outputCsv) {
      val out = new FileWriter(outputCsv)
      new FileReader(sourceCsv).readLines.drop(1).forEach[
         val cols = it.split(",(?=([^\"]*\"[^\"]*\")*[^\"]*$)", -1).iterator
         val product = new Product(
            Integer::parseInt(cols.next),
            Integer::parseInt(cols.next),
            cols.next,
            new SimpleDateFormat("\"yyyy/MM/dd\"").parse(cols.next),
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
