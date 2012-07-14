/*
 *This is a semi-modularized implementation of the problem written with different
 * custom implementations in mind. Implementation in here can easily be replaced 
 * though they procedure based rather than object oriented using interfaces.
 * 
 * However, it works well as the implementations would be direct code refactoring
 * in the methods of this class.
 * 
 * APIs to do some of the stuff written in a custom way here were not used due
 * to the fact that I also wanted to see how huge and difficult some of the 
 * activities are and in how much code this could be done - mainly in a custom 
 * manner. Also to reduce dependencies when testing this code. This piece of code 
 * can be executed without depending on any external libraries/APIs.
 * 
 * 
 * Brian Maunde
 */
package hollingberries;

import java.io.*;
import java.text.DateFormat;
import java.text.DecimalFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;

/**
 *
 * @author Brian Maunde
 */
public class HollingBerries {

    private String inProdLine;
    //Collection of all the products from the input file
    private ArrayList<Product> products = new ArrayList<>();
    
    //hashmaps for the special rules
    private HashMap markup = new HashMap();
    private HashMap sellByDurations = new HashMap();
    private HashMap badSuppliers = new HashMap();
    private HashMap primeSuppliers = new HashMap();
    
    //array for the generic structure
    private String[] productArr = new String[6];
    private String outputStr;
    
    //input and output files
    private String produceFile = System.getProperty("user.dir") + File.separator+ "produce.csv";
    private String priceFile = System.getProperty("user.dir") + File.separator+ "prices.csv";
    
    //formatting objects
    DecimalFormat df = new DecimalFormat("0.00");
    GregorianCalendar gc = new GregorianCalendar();
    DateFormat fmt = new SimpleDateFormat("yyyy/MM/dd");

    //main execution block
    public static void main(String[] args) {
        HollingBerries hb = new HollingBerries();
        hb.readAndPopulate();
        hb.printPrices();
    }

    //read from the file, create the product objects and 
    //add to the collection
    public void readAndPopulate() {

        char separator = ',';
        char replacement = '`';
        
        //start with the statics
        staticFills();

        //begin file reads
        try (BufferedReader reader = new BufferedReader(new FileReader(produceFile))) {
            
            //just do a read to get rid of the headings
            reader.readLine();
            
            //now for the actual data
            inProdLine = reader.readLine();
            
            while (inProdLine != null) {

                //split
                inProdLine = removeExtraSeparators(inProdLine, separator, replacement);
                productArr = inProdLine.split(",");

                //create the product and add to array list
                products.add(doFill(productArr));

                inProdLine = reader.readLine();

            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    //separated this as it could have different implementations
    //But the hashmaps have to be field
    public void staticFills() {

        //Markups
        markup.put(new Integer(1), new Integer(40)); //apples
        markup.put(new Integer(2), new Integer(35)); //bananas
        markup.put(new Integer(3), new Integer(55)); //berries
        markup.put(new Integer(4), new Integer(50)); //other

        //sell By dates
        sellByDurations.put(new Integer(1), new Integer(14)); //apples
        sellByDurations.put(new Integer(2), new Integer(5)); //bananas
        sellByDurations.put(new Integer(3), new Integer(7)); //other
        sellByDurations.put(new Integer(4), new Integer(7)); //other

        //bad Supliers
        badSuppliers.put(new Integer(1), "32");
        badSuppliers.put(new Integer(2), "101");

        //Prime Suppliers
        primeSuppliers.put("One", "219");
        primeSuppliers.put("Two", "204");
    }

    //create the product objects and do manipulations for the selling price
    //and also shelf life
    public Product doFill(String[] prodAttributes) {

        Product produceProd;
        int supID;
        int prodCode;
        int units;
        double rate;
        double costPrice;
        String description;
        String devDate;
        Date deliveryDate = null;
        Date sellByDate;
        
        char separator = ',';
        char replacement = '`';
        //separate the array into more defined attributes of the produce
        supID = Integer.parseInt(prodAttributes[0].replace("\"", ""));
        prodCode = Integer.parseInt(prodAttributes[1].replace("\"", ""));
        
        //just because we replaced only characters in the description, we have to restore them
        //all other need no replacement
        description = prodAttributes[2].replace("\"", "");
        description = description.replace(replacement, separator);
        
        devDate = prodAttributes[3].replace("\"", "");
        costPrice = Double.parseDouble(prodAttributes[4].replace("\"", ""));
        units = Integer.parseInt(prodAttributes[5].replace("\"", ""));


        //convert the string to date formatting at the same time
        try {
            deliveryDate = (Date) fmt.parse(devDate);
        } catch (ParseException ex) {
            ex.printStackTrace();
        }

        // what about prime rate
        Integer key = getKey(prodCode);
        rate = ((Integer) markup.get(key)).doubleValue();
        if (primeSuppliers.containsValue(Integer.toString(supID))) {
            rate += 10;
        }

        //what about dates - shelf life
        int dur = ((Integer) sellByDurations.get(key)).intValue();
        gc.setTime(deliveryDate);
        gc.add(Calendar.DAY_OF_YEAR, dur);
        sellByDate = gc.getTime();

        //get the selling price
        Double sellingPrice = costPrice * (1 + (rate / 100));
        sellingPrice /= 100;

        //format it
        sellingPrice = Double.parseDouble(df.format(sellingPrice));

        //what about prime rate rules
        if (primeSuppliers.containsValue(Integer.toString(supID))) {
            df.format(Math.ceil(sellingPrice));
        }

        //what abt the bad supplier rules?
        if (badSuppliers.containsValue(Integer.toString(supID))) {
            Date tmp = sellByDate;
            gc.setTime(tmp);
            gc.add(Calendar.DAY_OF_YEAR, - 3);
            sellByDate = gc.getTime();

            sellingPrice = sellingPrice >= 2.00? sellingPrice-2.00:0.00;

        }

        //now lets create the product
        produceProd = new Product(supID, prodCode, description, sellByDate, sellingPrice, units);

        return produceProd;

    }

    //do the printing - also could have a different implementation
    //writing to file
    public void printPrices() {

        //FileWriter out = new FileWriter(priceFile);
        Product tmpProd;

        int n = 0;
        int p;

        //initilize output - since we are printing to a file
        outputStr = "";

        //loop through products
        while (n < products.size()) {
            tmpProd = products.get(n);
            int len = tmpProd.getDescription().length();
                 //changed formatting - remove spacing between R and amount
                //put a space between all the attributes
            p = (len >= 29)? 29:len;
               
            if (tmpProd.getUnits() > 0 && tmpProd != null) {
                //now for the actual formatting
                for (int k = 0; k < tmpProd.getUnits(); k++) {
                   outputStr += "R" + df.format(tmpProd.getSellingPrice()) + " " + fmt.format(tmpProd.getSellByDate()) + " " + tmpProd.getDescription().substring(0, p) + "\n";
                }
            }
            n = n + 1;
        }

        //now write to the file
        try (PrintWriter print = new PrintWriter(new FileWriter(priceFile))) {

            print.println(outputStr);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    //can have different implementations - key to be used in hashMaps for markups 
    //shelf lives
    public Integer getKey(int prodCode) {

        if (prodCode > 1100 && prodCode < 1200) {
            return new Integer(1);
        } else if (prodCode >= 1200 && prodCode < 1300) {
            return new Integer(2);
        } else if (prodCode >= 1300) {
            return new Integer(3);
        } else {
            return new Integer(4);
        }
    }

    //A customized implementation for skipping separators in quotes
    //replace them with a unique character that is rarely or never used in texts
    //e.g the ` character.
    public String removeExtraSeparators(String input, char separator, char replacement) {
        char quote = '\"';
        char [] replStr = new char[input.length()];
        boolean inQuotes = false;
        for (int k = 0; k < input.length(); k++) {
            replStr[k] = input.charAt(k);
            if (inQuotes) {
                if (input.charAt(k) == separator) {
                    replStr[k] = replacement;
                }
            }
            if(input.charAt(k) == quote){
                inQuotes = ! inQuotes;
            }
                    
        }
        return String.valueOf(replStr);
    }
    
    public String restoreReplacements(String input, char separator, char replacement ){
        input.replaceAll(String.valueOf(replacement),String.valueOf(separator) );
        return input;
    }
}

//just to put everything together
//Will add the product object here with some getters and setter that are not
//necessarily being used
class Product {

    private int supplierID;
    private int productCode;
    private String description;
    private Date sellByDate;
    private Double sellingPrice;
    private int units;

    public Product(int supplierID, int productCode, String description, Date sellByDate, Double sellingPrice, int units) {
        this.supplierID = supplierID;
        this.productCode = productCode;
        this.description = description;
        this.sellByDate = sellByDate;
        this.sellingPrice = sellingPrice;
        this.units = units;
    }

    public Double getSellingPrice() {
        return sellingPrice;
    }

    public void setSellingPrice(Double sellingPrice) {
        this.sellingPrice = sellingPrice;
    }

    public Date getSellByDate() {
        return sellByDate;
    }

    public void setSellByDate(Date sellByDate) {
        this.sellByDate = sellByDate;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public int getProductCode() {
        return productCode;
    }

    public void setProductCode(int productCode) {
        this.productCode = productCode;
    }

    public int getSupplierID() {
        return supplierID;
    }

    public void setSupplierID(int supplierID) {
        this.supplierID = supplierID;
    }

    public int getUnits() {
        return units;
    }

    public void setUnits(int units) {
        this.units = units;
    }
}
