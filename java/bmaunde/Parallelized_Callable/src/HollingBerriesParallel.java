/*
 * Brian Maunde
 */
package Parallelized;

import java.io.*;
import java.text.DateFormat;
import java.text.DecimalFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.concurrent.*;

//Class
public class HollingBerriesParallel {

    private String inProdLine;
    
    private HashMap<Integer,Integer> markup = new HashMap<>();
    private HashMap<Integer,Integer> sellByDurations = new HashMap<>();
    private HashMap<String,String> badSuppliers = new HashMap<>();
    private HashMap<String,String> primeSuppliers = new HashMap<>();
    
    private String outputStr;
    
    private String produceFile = System.getProperty("user.dir") + File.separator+ "produce.csv";
    private String priceFile = System.getProperty("user.dir") + File.separator+ "prices.csv";
    

    DecimalFormat df = new DecimalFormat("0.00");
    GregorianCalendar gc = new GregorianCalendar();
    DateFormat fmt = new SimpleDateFormat("yyyy/MM/dd");

    public static void main(String[] args) {

        HollingBerriesParallel hb = new HollingBerriesParallel();  
        hb.printPrices(hb.readAndPopulate());
    }

    private List<Future<String>> readAndPopulate() {

        List<do_Fill> tList = new ArrayList<>();
        List<Future<String>> results = new ArrayList<>();

        staticFills();

        try (BufferedReader reader = new BufferedReader(new FileReader(produceFile))) {
        
            reader.readLine();
            inProdLine = reader.readLine();
           
            while (inProdLine != null) {
                
                tList.add(new do_Fill(inProdLine, markup, sellByDurations, badSuppliers, primeSuppliers));
                inProdLine = reader.readLine();
            }
            
            ExecutorService executor = Executors.newCachedThreadPool(Executors.defaultThreadFactory());
            results = executor.invokeAll(tList);
            
            executor.shutdown();

        } catch (IOException | InterruptedException e) {
            
            e.printStackTrace();
            
        }
        
        return results;
    }

    private void staticFills() {

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
        badSuppliers.put("One", "32");
        badSuppliers.put("Two", "101");

        //Prime Suppliers
        primeSuppliers.put("One", "219");
        primeSuppliers.put("Two", "204");
    }

    private void printPrices(List<Future<String>> results) {
        
        outputStr = "";
        
        try (PrintWriter print = new PrintWriter(new FileWriter(priceFile))) {
            

            for (Future<String> result : results) {
                
                outputStr += result.get();
                
            }
            print.println(outputStr);
            
        } catch (IOException | InterruptedException | ExecutionException e) {
            
            e.printStackTrace();
            
        }
    }
}

class do_Fill implements Callable<String> {

    private final String produceLine;
    
    private final DecimalFormat df = new DecimalFormat("0.00");
    private GregorianCalendar gc = new GregorianCalendar();
    private final DateFormat fmt = new SimpleDateFormat("yyyy/MM/dd");
    
    private final HashMap markup;
    private final HashMap sellByDurations;
    private final HashMap badSuppliers;
    private final HashMap primeSuppliers;

    public do_Fill(String pLine, HashMap markup, HashMap sellByDurations, HashMap badSuppliers, HashMap primeSuppliers) {
        this.produceLine = pLine;
        this.badSuppliers = badSuppliers;
        this.sellByDurations = sellByDurations;
        this.markup = markup;
        this.primeSuppliers = primeSuppliers;
    }

    @Override
    public String call() {
        
        char separator = ',';
        char replacement = '`';
        
        Product produceProd;
        String outputStr;
        
        String pLine = removeExtraSeparators(produceLine, separator, replacement);
        
        String[] prodAtttr = pLine.split(",");
        
        produceProd = doFill(prodAtttr);

        int len = produceProd.getDescription().length();
        
        int p = (len >= 29) ? 29 : len;
        outputStr = "";
        
        for (int k = 0; k < produceProd.getUnits(); k++) {
            
            outputStr += "R" + df.format(produceProd.getSellingPrice()) + " " + fmt.format(produceProd.getSellByDate()) + 
                    " " + produceProd.getDescription().substring(0, p) + "\n";
            
        }

        return outputStr;
    }

    private Product doFill(String[] prodAttributes) {

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

        supID = Integer.parseInt(prodAttributes[0].replace("\"", ""));
        prodCode = Integer.parseInt(prodAttributes[1].replace("\"", ""));

        description = prodAttributes[2].replace("\"", "");
        description = description.replace(replacement, separator);

        devDate = prodAttributes[3].replace("\"", "");
        costPrice = Double.parseDouble(prodAttributes[4].replace("\"", ""));
        units = Integer.parseInt(prodAttributes[5].replace("\"", ""));

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

        int dur = ((Integer) sellByDurations.get(key)).intValue();
        gc.setTime(deliveryDate);
        gc.add(Calendar.DAY_OF_YEAR, dur);
        sellByDate = gc.getTime();

        Double sellingPrice = costPrice * (1 + (rate / 100));
        sellingPrice /= 100;

        sellingPrice = Double.parseDouble(df.format(sellingPrice));

        if (primeSuppliers.containsValue(Integer.toString(supID))) {
            df.format(Math.ceil(sellingPrice));
        }

        if (badSuppliers.containsValue(Integer.toString(supID))) {
            Date tmp = sellByDate;
            gc.setTime(tmp);
            gc.add(Calendar.DAY_OF_YEAR, - 3);
            sellByDate = gc.getTime();

            sellingPrice = sellingPrice >= 2.00 ? sellingPrice - 2.00 : 0.00;
        }

        produceProd = new Product(supID, prodCode, description, sellByDate, sellingPrice, units);

        return produceProd;

    }

    private String removeExtraSeparators(String input, char separator, char replacement) {
        char quote = '\"';
        char[] replStr = new char[input.length()];
        boolean inQuotes = false;
        for (int k = 0; k < input.length(); k++) {
            replStr[k] = input.charAt(k);
            if (inQuotes) {
                if (input.charAt(k) == separator) {
                    replStr[k] = replacement;
                }
            }
            if (input.charAt(k) == quote) {
                inQuotes = !inQuotes;
            }

        }
        return String.valueOf(replStr);
    }

    private Integer getKey(int prodCode) {

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
    
}

//Product
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

    public Date getSellByDate() {
        return sellByDate;
    }

    public String getDescription() {
        return description;
    }

    public int getProductCode() {
        return productCode;
    }

    public int getSupplierID() {
        return supplierID;
    }

    public int getUnits() {
        return units;
    }
}
