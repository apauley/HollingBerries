# A Java Implementation of the HollingBerries Scenario
## The Parallelized Version - By Brian Maunde

**Notes:**

*   There are two implementations of this parallelized version. The two can be alternated by commenting out one of the lines in the code - in the regions of lines 73 - 75 in the method **readAndPopulate**. The two lines are as follows:  

		ExecutorService executor = Executors.newCachedThreadPool(Executors.defaultThreadFactory())
		//ExecutorService executor = Executors.newFixedThreadPool(1,Executors.defaultThreadFactory());
  
	The first allows a faster run - depending on machine specs - but distorts the output file on a product type to product type basis but products represented by a single line in the input file will be arranged together.  
	The second one is a slower than the first but provides output that is arranged in accordance with the sequence in the input file.

	
*	I broke the rules a bit and in this way - my ouput is arranged as follows using 29 characters from the description, combining the curreny indicator(R) and the selling price whilst putting spaces between the three components selling price, sellby-date and description. Also, the output is in csv format:

    	R21.07 2012/02/29 Apples 1kg Golden Delicious.
 

##Requirements

**Java 7**   
>There were a couple of cool features introduced in Java 7 that were used in this program.  

**ANT**  
>To build using the provided file, you will need ant - preferably version 1.6 or later.

**Location of the Produce.csv file**   
>You need to put the **produce.csv** file in the directory with this README.md file, the build file and src folder file (the Parallelized folder according to the setup in GIT). The root directory of the application. If you get the *FileNotFoundException*, the most probable reason is that you have put this file in the wrong directory.

## Building the Application    

All the commands listed below should be executed in your command line tool. If you want to run straight away, use the **One Stop Command To Run** below

###Just Cleaning   

If you have already built and need to rebuild the application, use the command  

		ant clean

###Just Compiling   

To just compile, use the command:
     
				ant compile     

To clean and compile please use:           

				ant clean compile	    	

###Running Application
      
To run the application, use the command     
     
				ant run    

OR        

				ant clean run   

###One Stop Command To Run    

To simply do all the above with one command, use:   
    
				ant     

OR       

				ant run 
    
OR       

				ant clean run  



## License

Distributed under the Eclipse Public License.