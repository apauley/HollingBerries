# A Java Implementation of the HollingBerries Scenario
## Versions Home - By Brian Maunde

## Version Descriptions

There are currently 3 versions provided in here. They are as follows:

1. Parallelized_Runnable.

>This version is the fastest - might vary from system to system - of the three. In it the Runnable objects are used to share the workload of doing the calculations and formatting the output. Within this version, two implementation can be derived by commenting one of the two lines in the code as specified in the README file under the Parallelized_Runnable project.

2. Parallelized_Callable.

>This version utilizes the Callable object to share the work amongst several cached pools of threads. This is the second fastest of the three - this however that Runnable objects are better than Callable objects as Callable objects, with simple tweaking, may be caused to mimick Runnable objects. With this version the output is formatted in a FIFO manner with respect to the input file - the first record in the input file will be the first in the output file - the same with the last. See the README for this project under Parallelized_Callable

3. Sequential

>This is the sequential implementation of the the scenario. As should be normally expected, it is the slowest of the three versions.


##Requirements

**Java 7**   
>There were a couple of cool features introduced in Java 7 that were used in this program.  These include automatic resource management, multiple exception handling, and Collection declaration.

**ANT**  
>To build using the provided file, you will need ant - preferably version 1.6 or later.

**Location of the Produce.csv file**   
>You need to put the **produce.csv** file in the directory with the README.md file, the build file and src folder file. The root directory of the application. If you get the *FileNotFoundException*, the most probable reason is that you have put this file in the wrong directory.


## License

Distributed under the Eclipse Public License.