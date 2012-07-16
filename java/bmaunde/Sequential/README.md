# A Java Implementation of the HollingBerries Scenario
## The Parallelized Version - By Brian Maunde
 

##Requirements

**Java 7**   
>There were a couple of cool features introduced in Java 7 that were used in this program.  

**ANT**  
>To build using the provided file, you will need ant - preferably version 1.6 or later.

**Location of the Produce.csv file**   
>You need to put the **produce.csv** file in the directory with this README.md file, the build file and src folder file (the Sequential folder according to the setup in GIT). The root directory of the application. If you get the *FileNotFoundException*, the most probable reason is that you have put this file in the wrong directory.

## Building the Application    

All the commands listed below should be executed in your command line tool with your current context being the directory with the **build.xml** file.


###Clean and Build   

To clean and build, use the command:
     
				ant clean-build    

###Build:     

To build without cleaning, use command      

				ant build    	

###Running Application
      
To run the application, use the command     
     
				ant run    

OR (with cleaning)       

				ant clean run 





## License

Distributed under the Eclipse Public License.