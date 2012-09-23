using System;
using System.Collections.Generic;
using System.IO;
using NUnit.Framework;

namespace HollingBerries.Tests
{
    [TestFixture]
    public class PriceFileGeneratorTests
    {
        [Test]
        public void CanCreateOutputFile()
        {
            var pricefileTxt = "pricefile.txt";

            if (File.Exists(pricefileTxt)) File.Delete(pricefileTxt);

            var premiumSuppliers = new List<int> {219, 204};
            var troubleSuppliers = new List<int> {32, 101};
            var products = new Dictionary<string, Product>
                {
                    {"Apples", new Product(1100, 1199, .40, 14)}, 
                    {"Bananas", new Product(1200, 1299, .35, 5)}, 
                    {"Berries", new Product(1300, 1399, .55, 7)}
                };

            var productCollection = new ProductList(
                premiumSuppliers,
                troubleSuppliers,
                .1,
                2,
                products
            );

            var generator = new PriceFileGenerator(new ProduceFileParser(), productCollection);

            generator.CreatePriceFile(pricefileTxt, @".\Data\produce.csv");

            assertFileContentsAreSame("..\..\pricefile.txt", pricefileTxt);
        }

        private void assertFileContentsAreSame(string samplepricefileTxt, string pricefileTxt)
        {
            var sample = File.ReadAllText(samplepricefileTxt);
            var actual = File.ReadAllText(pricefileTxt);

            Assert.That(sample.Replace("\n", "\r\n"), Is.EqualTo(actual)); // cater for *nix line endings
        }

        [Test]
        public void CanParseProduceFile()
        {
            var parser = new ProduceFileParser();
            ProduceItem firstItem = null;
            var totalItems = 0;

            foreach(var item in parser.Parse(@".\Data\produce.csv"))
            {
                if (firstItem == null) firstItem = item;
                totalItems++;
            }
            
            Assert.That(totalItems, Is.EqualTo(12));
            Assert.That(firstItem.SupplierId, Is.EqualTo(15));
            Assert.That(firstItem.ProductCode, Is.EqualTo(1101));
            Assert.That(firstItem.ProductDescription, Is.EqualTo("Apples 1kg Golden Delicious. The sweetest Apples! Always a favourite. Love, Mrs. Hollingberry"));
            Assert.That(firstItem.DeliveryDate, Is.EqualTo(new DateTime(2012,02,15)));
            Assert.That(firstItem.UnitPrice, Is.EqualTo(15.05));
            Assert.That(firstItem.NumberOfUnits, Is.EqualTo(5));
        }

    }
	
	[TestFixture]
    public class ProductTests
    {
        private ProductList _products;
        
        [SetUp]
        public void Given()
        {
            var premiumSupplierMarkup = .1;
            var troubleSupplierDiscountInRands = 2;
            var sample = new Product(100, 200, .1, 14);

            _products = new ProductList(
                new List<int> { 1 }, 
                new List<int> { 2 }, 
                premiumSupplierMarkup,
                troubleSupplierDiscountInRands,
                new Dictionary<string, Product> { 
                   {"SampleProductName", sample}
                });
        }

        [Test]
        public void CanCalculateMarkupFromProductCode()
        {
            var produceItem = new ProduceItem(0, 150, "", DateTime.Now.Date, 100, 1);

            Assert.That(_products.MarkupOf(produceItem), Is.EqualTo(1.1));
        }

        [Test]
        public void CanCalculatePriceFromCostAndProductCode()
        {
            var produceItem = new ProduceItem(0, 150, "", DateTime.Now.Date, 100, 1);
            
            Assert.That(_products.SellingPriceOf(produceItem), Is.EqualTo(110));
        }

        [Test]
        public void CanDetermineSellByDate()
        {
            var currentDate = DateTime.Now.Date;
            var produceItem = new ProduceItem(0, 150, "", DateTime.Now.Date, 100, 1);

            Assert.That(_products.SellByDateOf(produceItem), Is.EqualTo(currentDate.AddDays(14)));
        }

        [Test]
        public void CanAdjustSellByForProblemSuppliers()
        {
            var item = new ProduceItem(2, 150, "", DateTime.Now.Date, 100, 1);
            var currentDate = DateTime.Now.Date;

            Assert.That(_products.SellByDateOf(item), Is.EqualTo(currentDate.AddDays(14 - 3)));
        }

        [Test]
        public void CanAdjustPriceForProblemSuppliers()
        {
            var item = new ProduceItem(2, 150, "", new DateTime(), 10.00, 1);

            Assert.That(_products.SellingPriceOf(item), Is.EqualTo(11.0 - 2));
        }

        [Test]
        public void CanAdjustPriceForPremiumSuppliers()
        {
            var item = new ProduceItem(1, 150, "", new DateTime(), 10.00, 1);

            Assert.That(_products.SellingPriceOf(item), Is.EqualTo(12.0));
        }

        [Test]
        public void CanRoundPriceUpForPremiumSuppliers()
        {
            var item = new ProduceItem(1, 150, "", new DateTime(), 5.3, 1);

            Assert.That(_products.SellingPriceOf(item), Is.EqualTo(7.0));
        }
    }
}
