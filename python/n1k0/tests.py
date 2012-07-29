import os
import unittest
import hollingberries


class TestHollingBerries(unittest.TestCase):
    def setUp(self):
        self.input_file = open(os.path.join(hollingberries.BASE_PATH, 'produce.csv'), 'rb')
        self.output_file = open('test_pricelist.txt', 'w')

    def tearDown(self):
        os.remove('test_pricelist.txt')

    def test_process(self):
        hollingberries.process(self.input_file, self.output_file)
        reference_lines = open(os.path.join(hollingberries.BASE_PATH, 'pricefile.txt')).read().split('\n')
        output_lines = open('test_pricelist.txt', 'rb').read().split('\n')
        self.assertEquals(output_lines, reference_lines)

if __name__ == '__main__':
    unittest.main()
