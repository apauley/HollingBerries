How to run the code (Windows (assumes .NET Framework 4.0 is installed))
=======================================================================

    cd csharp\kevint\
    build.windows.bat
    hollingberries.exe ..\..\products.csv pricefile.txt
	# diff will fail unless you ignore line endings (\n vs \r\n)

How to run the code (Other (assumes Mono Framework 4.0 is installed))
=======================================================================

    cd csharp/kevint/
    sh  build.other.sh
    mono hollingberries.exe ../../products.csv pricefile.txt
    diff -u ../../pricefile.txt pricefile.txt