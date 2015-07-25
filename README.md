## run_analysis.R

This function isolates and summarizes specific data from within a dataset containing 
angular momentum and acceleration data collected from a variety of subjects performing a variety of activities.

(get data here: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip)

To run the script, simply:

1. Clone it into the same directory as the downloaded .zip file. Unzip the file using (on Linux):

>unzip getdata-projectfiles-UCI\ HAR\ Dataset.zip 
Alternatively, use a zip program of your choice, but make sure the data files are     unzipped to a subdirectory called "UCI HAR Dataset."

2. open R
3. call source("run_analysis.R")
4. summary <- run_analysis()

Running the script in this way will result in the creation of two files:
* main.txt - a consolidated dataset containing all the mean and standard deviation measurements
* averages.txt - a summary of the above dataset, which lists the mean of each variable by test subject and activity performed
Both files can be read into R with read.table().

The variable summary will be filled with a data.table containing the data written to averages.txt

