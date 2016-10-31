# Digital Marketing Project

The Python and R files have different functions
	- .ipynb file - To perform Data cleaning and exploration operations.  The updated data is then exported to the root directory.
	- .R file - To create models on top of the updated data created in the former step.
	
Tools Versions Used -
Python Version 2.7.12 | Anaconda 4.1.1 (packages individual versions is mentioned in the ipynb file)
R - 3.3.1 | R Studio 0.99.903

Prerequisites -
1. Ensure all the three files - data file (csv), ipynb and R files in the same location and .
	- To run all the cells in the python notebook, follow the work flow - Click 'Cell' --> 'Run All'

2. Python code uses the following packages - pandas, numpy, tabulate, seaborn, matplotlib and sklearn.  If any package is missing, use the following in anacond prompt to install the packages 
	-pip install "package_name" 

3. Python saves a csv file - "updated_data.csv" which would be used by R to create models on the updated data set.  

4. Before running the 'R' file, set the working directory (using "setwd" command ) to the folder that is created in step 1 and ensure that file in step 3 is generated is also in same location.  Note : 'R' would automatically install the required packages.

* Comments have been annotated on the code wherever necessary
