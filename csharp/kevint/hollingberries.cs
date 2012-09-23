using System;
using System.Collections.Generic;
using System.IO;
using Microsoft.VisualBasic.FileIO;
using System.Linq;

namespace HollingBerries
{
    class Program
    {
        static void Main(string[] args)
        {
            var produceFile = args[0];
            var pricefileTxt = args[1];

            var premiumSuppliers = new List<int> { 219, 204 };
            var troubleSuppliers = new List<int> { 32, 101 };
            var products = new Dictionary<string, Product>
                {
                    {"Apples", new Product(1100, 1199, .40, 14)}, 
                    {"Bananas", new Product(1200, 1299, .35, 5)}, 
                    {"Berries", new Product(1300, 1399, .55, 7)}
                };
            var premiumSupplierMarkup = .1;
            var troubleSupplierDiscountInRands = 2;

            var productList = new ProductList(
                premiumSuppliers,
                troubleSuppliers,
                premiumSupplierMarkup,
                troubleSupplierDiscountInRands,
                products
            );

            var generator = new PriceFileGenerator(new ProduceFileParser(), productList);

            generator.CreatePriceFile(pricefileTxt, produceFile);

        }
    }
	
	public class PriceFileGenerator
    {
        private readonly IProduceFileParser _produceFileParser;
        private readonly IProductList _productList;

        public PriceFileGenerator(IProduceFileParser produceFileParser, IProductList productList)
        {
            _produceFileParser = produceFileParser;
            _productList = productList;
        }

        public void CreatePriceFile(string priceFile, string produceCsv)
        {
            var writer = new StreamWriter(priceFile);

            foreach (var stockItem in _produceFileParser.Parse(produceCsv))
            {
                for (int i = 0; i < stockItem.NumberOfUnits; i++)
                {
                    writer.WriteLine(
                        string.Format("R{0,8:###0.00}", _productList.SellingPriceOf(stockItem)) +
                        _productList.SellByDateOf(stockItem).ToString("yyyy/MM/dd") +
                        stockItem.ProductDescription.Substring(0,31)
                        );
                }
            }

            writer.Close();
            writer.Dispose();
        }
    }
	
	public interface IProduceFileParser
    {
        IEnumerable<ProduceItem> Parse(string path);
    }

	public interface IProductList
    {
        double MarkupOf(ProduceItem produceItem);
        double SellingPriceOf(ProduceItem produceItem);
        DateTime SellByDateOf(ProduceItem produceItem);
        int DaysToKeep(ProduceItem produceItem);
    }
	
    public class ProduceFileParser : IProduceFileParser
    {
        public IEnumerable<ProduceItem> Parse(string path)
        {
            var parser = new TextFieldParser(path); 
            parser.TextFieldType = FieldType.Delimited;
            parser.SetDelimiters(",");
            var ignoreHeaders = true;

            while(!parser.EndOfData)
            {
                var fields = parser.ReadFields();

                if (ignoreHeaders) { ignoreHeaders = false; continue; }

                yield return new ProduceItem(
                    int.Parse( fields[0] ),
                    int.Parse( fields[1] ),
                    fields[2],
                    DateTime.Parse(fields[3]),
                    double.Parse( fields[4] )/100, 
                    int.Parse( fields[5] ) 
                    );
                
            }

            parser.Close();
        }
    }
	
	 public class ProduceItem
    {
        public ProduceItem(int supplierId, int productCode, string productDescription, DateTime deliveryDate, double unitPrice,  int numberOfUnits)
        {
            SupplierId = supplierId;
            ProductCode = productCode;
            ProductDescription = productDescription;
            DeliveryDate = deliveryDate;
            UnitPrice = unitPrice;
            NumberOfUnits = numberOfUnits;
        }

        public int SupplierId { get; private set; }
        public int ProductCode { get; private set; }
        public string ProductDescription { get; private set; }
        public DateTime DeliveryDate { get; private set; }
        public double UnitPrice { get; private set; }
        public int NumberOfUnits { get; private set; }
    }
	
	public class ProductList : SortedList<string, Product>, IProductList
    {
        private readonly List<int> _premiumSuppliers;
        private readonly List<int> _troubleSuppliers;
        private readonly double _premiumSupplierMarkup;
        private readonly double _troubleSupplierDiscountInRands;

        public ProductList(List<int> premiumSuppliers, List<int> troubleSuppliers, double premiumSupplierMarkup, double troubleSupplierDiscountInRands, IDictionary<string, Product> dictionary) : base(dictionary)
        {
            _premiumSuppliers = premiumSuppliers;
            _troubleSuppliers = troubleSuppliers;
            _premiumSupplierMarkup = premiumSupplierMarkup;
            _troubleSupplierDiscountInRands = troubleSupplierDiscountInRands;
        }

        public double MarkupOf(ProduceItem produceItem)
        {
            // could be replaced with binary search for better performance
            return 1 + Values
                .Where(product => product.Has(produceItem.ProductCode))
                .Select(product => product.Markup).FirstOrDefault();
        }

        public double SellingPriceOf(ProduceItem produceItem)
        {
            if (produceItem.UnitPrice == 0.0) return 0;

            var markup = MarkupOf(produceItem);
            if (isPremiumSupplier(produceItem)) markup += _premiumSupplierMarkup;

            var price = Math.Round(produceItem.UnitPrice * markup, 2);

            if (isTroubleSupplier(produceItem)) price -= _troubleSupplierDiscountInRands;
            if (isPremiumSupplier(produceItem)) price =  Math.Ceiling(price);

            return price;
        }

        private bool isPremiumSupplier(ProduceItem produceItem)
        {
            return _premiumSuppliers.Contains(produceItem.SupplierId);
        }

        private bool isTroubleSupplier(ProduceItem produceItem)
        {
            return _troubleSuppliers.Contains(produceItem.SupplierId);
        }

        public DateTime SellByDateOf(ProduceItem produceItem)
        {
            var sellBy = produceItem.DeliveryDate.AddDays(DaysToKeep(produceItem));

            if (isTroubleSupplier(produceItem))
                sellBy = sellBy.AddDays(-3);

            return sellBy;
        }

        public int DaysToKeep(ProduceItem produceItem)
        {
            // could be replaced with binary search for better performance
            return Values
                .Where(product => product.Has(produceItem.ProductCode))
                .Select(product => product.DaysToKeep).FirstOrDefault(); 
        }
    }
	
	public class Product
    {
        private int _codeFrom;
        private int _codeTo;

        public double Markup { get; private set; }
        public int DaysToKeep { get; private set; }

        public Product(int codeFrom, int codeTo, double markup, int daysToKeep)
        {
            _codeFrom = codeFrom;
            _codeTo = codeTo;
            Markup = markup;
            DaysToKeep = daysToKeep;
        }

        public bool Has(int productCode) { return productCode >= _codeFrom && productCode <= _codeTo; }
    }
}
