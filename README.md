# Programming_R assignment
#Original file for Jupyter is here: https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBM-DA0151EN-SkillsNetwork/labs/Final_project/IBM-DS-with-R-2-Data-Analysis-with-R.ipynb
# R Studio file is uploaded with the required code (adressing the 10 tasks)

Assignment Scenario
Congratulations! You have just been hired by a US Weather forecast firm as a data scientist.

The company is considering the weather condition to help predict the possibility of precipitations, which involves using various local climatological variables, including temperature, wind speed, humidity, dew point, and pressure. The data you will be handling was collected by a NOAA weather station located at the John F. Kennedy International Airport in Queens, New York.

Your task is to provide a high level analysis of weather data in JFK Airport. Your stakeholders want to understand the current and historical record of precipitations based on different variables. For now they are mainly interested in a macro-view of JFK Airport Weather, and how it relates to the possibility to rain because it will affect flight delays and etc.

Introduction
This project relates to the NOAA Weather Dataset - JFK Airport (New York). The original dataset contains 114,546 hourly observations of 12 local climatological variables (such as temperature and wind speed) collected at JFK airport. This dataset can be obtained for free from the IBM Developer Data Asset Exchange.

For this project, you will be using a subset dataset, which contains 5727 rows (about 5% or original rows) and 9 columns. The end goal will be to predict the precipitation using some of the available features. In this project, you will practice reading data files, preprocessing data, creating models, improving models and evaluating them to ultimately choose the best model.

Table of Contents:
Using this R notebook you will complete 10 tasks:

0. Import Modules
1. Download and Unzip NOAA Weather Dataset
2. Read Dataset into Project
3. Select Subset of Columns
4. Clean Up Columns
5. Convert Columns to Numerical Types
6. Rename Columns
7. Exploratory Data Analysis
8. Linear Regression
9. Improve the Model
10. Find Best Model

0. Import required modules
Tidymodels is a collection of packages that use tidyverse principles to easily do the entire modeling process from preprocessing initial data, to creating a model, to tunning hyperparameters. The tidymodels packages can be used to produce high quality statistical and machine learning models. Our Jupyter notebook platforms have a built-in Tidyverse, Tidymodels and rlang packages so we do not need to install these packages prior to loading library. However, if you decide to run this lab on your RStudio Desktop locally on your machine, you can remove the commented lines of code to install these packages before loading.

# Install tidymodels if you haven't done so
# install.packages("rlang")
# install.packages("tidymodels")
Note: After installing the packages, restart the kernel. Without installing the packages again, load them. Tidyverse and Tidymodels will be the two main packages you will use.

# Library for modeling
library(tidymodels)

# Load tidyverse
library(tidyverse)
Understand the Dataset
The original NOAA JFK dataset contains 114,546 hourly observations of various local climatological variables (including temperature, wind speed, humidity, dew point, and pressure).

In this project you will use a sample dataset, which is around 293 KB. Link to the sample dataset.

The sample contains 5727 rows (about 5% or original rows) and 9 columns, which are:

DATE
HOURLYDewPointTempF
HOURLYRelativeHumidity
HOURLYDRYBULBTEMPF
HOURLYWETBULBTEMPF
HOURLYPrecip
HOURLYWindSpeed
HOURLYSeaLevelPressure
HOURLYStationPressure
The original dataset is much bigger. Feel free to explore the original dataset. Link to the original dataset.

For more information about the dataset, checkout the preview of NOAA Weather - JFK Airport.


1. Download NOAA Weather Dataset
Use the download.file() function to download the sample dataset from the URL below.

URL = 'https://dax-cdn.cdn.appdomain.cloud/dax-noaa-weather-data-jfk-airport/1.1.4/noaa-weather-sample-data.tar.gz'


Untar the zipped file.



2. Extract and Read into Project
We start by reading in the raw dataset. You should specify the file name as "noaa-weather-sample-data/jfk_weather_sample.csv".


Next, display the first few rows of the dataframe.


Also, take a glimpse of the dataset to see the different column data types and make sure it is the correct subset dataset with about 5700 rows and 9 columns.



3. Select Subset of Columns
The end goal of this project will be to predict HOURLYprecip (precipitation) using a few other variables. Before you can do this, you first need to preprocess the dataset. Section 3 to section 6 focuses on preprocessing.

The first step in preprocessing is to select a subset of data columns and inspect the column types.

The key columns that we will explore in this project are:

HOURLYRelativeHumidity
HOURLYDRYBULBTEMPF
HOURLYPrecip
HOURLYWindSpeed
HOURLYStationPressure
Data Glossary:

'HOURLYRelativeHumidity' is the relative humidity given to the nearest whole percentage.
'HOURLYDRYBULBTEMPF' is the dry-bulb temperature and is commonly used as the standard air temperature reported. It is given here in whole degrees Fahrenheit.
'HOURLYPrecip' is the amount of precipitation in inches to hundredths over the past hour. For certain automated stations, precipitation will be reported at sub-hourly intervals (e.g. every 15 or 20 minutes) as an accumulated amount of all precipitation within the preceding hour. A “T” indicates a trace amount of precipitation.
'HOURLYWindSpeed' is the speed of the wind at the time of observation given in miles per hour (mph).
'HOURLYStationPressure' is the atmospheric pressure observed at the station during the time of observation. Given in inches of Mercury (in Hg).
Select those five columns and store the modified dataframe as a new variable.


Show the first 10 rows of this new dataframe.



4. Clean Up Columns
From the dataframe preview above, we can see that the column HOURLYPrecip - which is the hourly measure of precipitation levels - contains both NA and T values. T specifies trace amounts of precipitation (meaning essentially no precipitation), while NA means not available, and is used to denote missing values. Additionally, some values also have "s" at the end of them, indicating that the precipitation was snow.

Inspect the unique values present in the column HOURLYPrecip (with unique(dataframe$column)) to see these values.


Having characters in values (like the "T" and "s" that you see in the unique values) will cause problems when you create a model because values for precipitation should be numerical. So you need to fix these values that have characters.

Now, for the column HOURLYPrecip:

Replace all the T values with "0.0" and
Remove "s" from values like "0.02s". In R, you can use the method str_remove(column, pattern = "s$") to remove the character "s" from the end of values. The "$" tells R to match to the end of values. The pattern is a regex pattern. Look at here for more information about regex and matching to strings in R.
Remember that you can use tidyverse's mutate() to update columns.

You can check your work by checking if unique values of HOURLYPrecip still contain any T or s. Store the modified dataframe as a new variable.



5. Convert Columns to Numerical Types
Now that you have removed the characters in the HOURLYPrecip column, you can safely covert the column to a numeric type.

First, check the types of the columns. You will notice that all are dbl (double or numeric) except for HOURLYPrecip, which is chr (character or string). Use the glimpse function from Tidyverse.


Convert HOURLYPrecip to the numeric type and store the cleaned dataframe as a new variable.


We can now see that all fields have numerical data type.



6. Rename Columns
Let's rename the following columns as:

'HOURLYRelativeHumidity' to 'relative_humidity'
'HOURLYDRYBULBTEMPF' to 'dry_bulb_temp_f'
'HOURLYPrecip' to 'precip'
'HOURLYWindSpeed' to 'wind_speed'
'HOURLYStationPressure' to 'station_pressure'
You can use dplyr::rename(). Then, store the final dataframe as a new variable.



7. Exploratory Data Analysis
Now that you have finished preprocessing the dataset, you can can start exploring the columns more.

First, split the data into a training and testing set. Splitting a dataset is done randomly, so to have reproducible results set the seed = 1234. Also, use 80% of the data for training.


Next, looking at just the training set, plot histograms or box plots of the variables (relative_humidity, dry_bulb_temp_f, precip, wind_speed, station_pressure) for an intial look of their distributions using tidyverse's ggplot. Leave the testing set as is because it is good practice to not see the testing set until evaluating the final model.



8. Linear Regression
After exploring the dataset more, you are now ready to start creating models to predict the precipitation (precip).

Create simple linear regression models where precip is the response variable and each of relative_humidity, dry_bulb_temp_f,wind_speed or station_pressure will be a predictor variable, e.g. precip ~ relative_humidity, precip ~ dry_bulb_temp_f, etc. for a total of four simple models. Additionally, visualize each simple model with a scatter plot.






9. Improve the Model
Now, try improving the simple models you created in the previous section.

Create at least two more models, each model should use at least one of the different techniques:

Add more features/predictors
Add regularization (L1, L2 or a mix)
Add a polynomial component
Also, for each of the models you create, check the model performance using the training set and a metric like MSE, RMSE, or R-squared.

Consider using tidymodels if you choose to add regularization and tune lambda.



10. Find Best Model
Compare the regression metrics of each model from section 9 to find the best model overall. To do this,

Evaluate the models on the testing set using at least one metric (like MSE, RMSE or R-squared).
After calculating the metrics on the testing set for each model, print them out in as a table to easily compare. You can use something like:
model_names <- c("model_1", "model_2", "model_3")
train_error <- c("model_1_value", "model_2_value", "model_3_value")
test_error <- c("model_1_value", "model_2_value", "model_3_value")
comparison_df <- data.frame(model_names, train_error, test_error)
Finally, from the comparison table you create, conclude which model performed the best.
