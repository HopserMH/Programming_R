library(tidymodels)
library(tidyverse)

URL = 'https://dax-cdn.cdn.appdomain.cloud/dax-noaa-weather-data-jfk-airport/1.1.4/noaa-weather-sample-data.tar.gz'

#1-1 Use the download.file() function to download the sample dataset from the URL below.
download.file(URL, destfile = "file.tar.gz")

#1-2 Untar the zipped file.
untar("file.tar.gz")

#2-1 reading in the raw dataset. You should specify the file name as "noaa-weather-sample-data/jfk_weather_sample.csv".
noaa_weather_sample <-read_csv("noaa-weather-sample-data/jfk_weather_sample.csv",
                               col_types = cols(
                                 'DATE' = col_datetime(),
                                 'HOURLYDewPointTempF' = col_number(),
                                 'HOURLYRelativeHumidity' = col_number(),
                                 'HOURLYDRYBULBTEMPF' = col_number(),
                                 'HOURLYWETBULBTEMPF' = col_number(),
                                 # 'HOURLYPrecip' = col_number(),
                                 'HOURLYWindSpeed' = col_number(),
                                 'HOURLYSeaLevelPressure' = col_number(),
                                 'HOURLYStationPressure' = col_number(),
                               ))

#2-2 display the first few rows of the dataframe.
noaa_weather_sample[1:5,]

#2-3 take a glimpse of the dataset to see the different column data types 
glimpse (noaa_weather_sample)
str(noaa_weather_sample)

#3-1:Select those five columns and store the modified dataframe as a new variable
noaa_weather_sample_pruned <-noaa_weather_sample %>%
  select(HOURLYRelativeHumidity,HOURLYDRYBULBTEMPF,HOURLYPrecip,HOURLYWindSpeed,HOURLYStationPressure)

#3-2:Show the first 10 rows of this new dataframe.
noaa_weather_sample_pruned[1:10,]

#4-1 Inspect the unique values present in the column HOURLYPrecip (with unique(dataframe$column)) to see these values
unique(noaa_weather_sample_pruned$HOURLYPrecip)
glimpse(noaa_weather_sample_pruned$HOURLYPrecip)

#4-2 for the column HOURLYPrecip: Replace all the T values with "0.0"  and Remove "s" from values like "0.02s"
noaa_weather_sample_pruned$HOURLYPrecip[noaa_weather_sample_pruned$HOURLYPrecip=="T"]<-0.0
noaa_weather_sample_pruned$HOURLYPrecip<-str_remove(noaa_weather_sample_pruned$HOURLYPrecip, pattern = "s$")

#5-1 check the types of the columns
glimpse(noaa_weather_sample_pruned)

#5-2Convert HOURLYPrecip to the numeric type and store the cleaned dataframe as a new variable.
noaa_weather_cleaned <-noaa_weather_sample_pruned %>%
  mutate_all(type.convert) %>%
  mutate_if(is.character, as.numeric)

#5-3 see that all fields have numerical data type.
glimpse(noaa_weather_cleaned)


#6-1rename the following columns as:
#'HOURLYRelativeHumidity' to 'relative_humidity'
#'HOURLYDRYBULBTEMPF' to 'dry_bulb_temp_f'
#'HOURLYPrecip' to 'precip'
#'HOURLYWindSpeed' to 'wind_speed'
#'HOURLYStationPressure' to 'station_pressure'

names(noaa_weather_cleaned)<-c("relative_humidity","dry_bulb_temp_f","precip","wind_speed","station_pressure")
glimpse(noaa_weather_cleaned)


#7.1 split the data into a training and testing set. 
#Splitting a dataset is done randomly, so to have reproducible results set the seed = 1234. Also, use 80% of the data for training.
library(tidymodels)
set.seed(1234)
noaa_weather_split <-initial_split(noaa_weather_cleaned, prop = 0.8)
train_data <- training(noaa_weather_split)
test_data <-testing((noaa_weather_split))

#7.2 looking at just the training set, plot histograms or box plots of the variables 
#(relative_humidity, dry_bulb_temp_f, precip, wind_speed, station_pressure)
#for an intial look of their distributions using tidyverse's ggplot.
boxplot(train_data[,], names=names(train_data),main= "check vars")

ggplot(data=train_data, mapping= aes( x= precip))+ 
  geom_histogram(bins=10, color ="black", fill="red") +
  coord_cartesian(xlim=c(-0.2,1.5))

hist(train_data$relative_humidity, main="humidity")
hist(train_data$dry_bulb_temp_f, main="dry_bulb_temp_f")
hist(train_data$precip, ,main="precip")
hist(train_data$precip[which(train_data$precip!=0.0)], main="precip")
qplot(precip, data=train_data, geom="density", bin=5, main="precip")

hist(train_data$wind_speed, main="wind_speed")
hist(train_data$station_pressure, main="station_pressure")

#8.linear Regression: 
#simple linear regression models where 
#precip is the response variable and 
#each of relative_humidity, dry_bulb_temp_f,wind_speed or station_pressure 
#for a total of four simple models. Additionally, visualize each simple model with a scatter plot.

#8-1 Precip vs Humidity
anova_precipe_VS_humidity <- aov(precip ~relative_humidity ,data=train_data )
summary(anova_precipe_VS_humidity)

LM_precipe_VS_humidity <- lm(precip ~relative_humidity ,data=train_data )
summary(LM_precipe_VS_humidity)

ggplot(train_data, aes(relative_humidity,precip)) + geom_point()+geom_smooth(method="lm")

#8-2 Precip vs Bulb_temp         
anova_precipe_VS_dry_bulb_temp_f <- aov(precip ~dry_bulb_temp_f ,data=train_data )
summary(anova_precipe_VS_dry_bulb_temp_f)

LM_precipe_VS_dry_bulb_temp_f <- lm(precip ~dry_bulb_temp_f ,data=train_data )
summary(LM_precipe_VS_dry_bulb_temp_f)

ggplot(train_data, aes(dry_bulb_temp_f,precip)) + geom_point()+geom_smooth(method="lm")

#8-3 Precip vs Wind        
anova_precipe_VS_wind_speed <- aov(precip ~wind_speed ,data=train_data )
summary(anova_precipe_VS_wind_speed)

LM_precipe_VS_wind_speed <- lm(precip ~wind_speed ,data=train_data )
summary(LM_precipe_VS_wind_speed)

ggplot(train_data, aes(wind_speed,precip)) + geom_point()+geom_smooth(method="lm")

#8-4 Precip vs Pressure
anova_precipe_VS_station_pressure <- aov(precip ~station_pressure ,data=train_data )
summary(anova_precipe_VS_station_pressure)

LM_precipe_VS_station_pressure <- lm(precip ~station_pressure ,data=train_data )
summary(LM_precipe_VS_station_pressure)

ggplot(train_data, aes(station_pressure,precip)) + geom_point()+geom_smooth(method="lm")

#9 improve Model
#Create at least 2 more models, each model should use at least 1 of the different techniques:
#  Add more features/predictors
#    Add regularization (L1, L2 or a mix)
#   Add a polynomial component


#1 approach normalize the 4 independent vars 
noaa_weather_cleaned_sin_Na <-noaa_weather_cleaned %>%
  replace_na(list(
    relative_humidity = 0,     dry_bulb_temp_f= 0, precip= 0, wind_speed= 0, station_pressure= 0  ))


noaa_weather_cleaned_sin_Na$Z_Scale_Humidity <-(noaa_weather_cleaned_sin_Na$relative_humidity - mean(noaa_weather_cleaned_sin_Na$relative_humidity))/ sd(noaa_weather_cleaned_sin_Na$relative_humidity)
head(noaa_weather_cleaned_sin_Na$Z_Scale_Humidity)

noaa_weather_cleaned_sin_Na$Z_Scale_dry_bulb_temp_f <-(noaa_weather_cleaned_sin_Na$dry_bulb_temp_f - mean(noaa_weather_cleaned_sin_Na$dry_bulb_temp_f))/ sd(noaa_weather_cleaned_sin_Na$dry_bulb_temp_f)
noaa_weather_cleaned_sin_Na$Z_Scale_dry_wind_speed <-(noaa_weather_cleaned_sin_Na$wind_speed - mean(noaa_weather_cleaned_sin_Na$wind_speed))/ sd(noaa_weather_cleaned_sin_Na$wind_speed)
noaa_weather_cleaned_sin_Na$Z_Scale_dry_station_pressure <-(noaa_weather_cleaned_sin_Na$station_pressure - mean(noaa_weather_cleaned_sin_Na$station_pressure))/ sd(noaa_weather_cleaned_sin_Na$station_pressure)
glimpse (noaa_weather_cleaned_sin_Na)

noaa_weather_cleaned_sin_Na$Buckets_Wind<-cut(noaa_weather_cleaned_sin_Na$wind_speed,seq(0,55,by=5))

noaa_weather_split_sin_NA_and__Z_normalized <-initial_split(noaa_weather_cleaned_sin_Na, prop = 0.8)
train_data <- training(noaa_weather_split_sin_NA_and__Z_normalized)
test_data <-testing((noaa_weather_split_sin_NA_and__Z_normalized))

#9-1 1st model: 
lm_spec <-linear_reg()%>% set_engine (engine="lm")

train_fit1 <- lm_spec %>% fit (precip ~Z_Scale_Humidity + Z_Scale_dry_wind_speed+Z_Scale_dry_station_pressure,data=train_data)

test_results <-train_fit1 %>% predict (new_data= test_data) %>% mutate (truth=test_data$precip)
rmse(test_results, truth=truth, estimate= .pred)

train_results <-train_fit1 %>% predict (new_data= train_data) %>% mutate (truth=train_data$precip)
rmse(train_results, truth=truth, estimate= .pred)
rmse_model1_test <-rmse(test_results, truth=truth, estimate= .pred)[3]
names(rmse_model1_test)<-"rmse"
rmse_model1_train <-rmse(train_results, truth=truth, estimate= .pred)[3]
names(rmse_model1_train)<-"rmse"

#9-1 2nd model 
train_fit2 <- lm_spec %>% fit (precip ~ poly(Z_Scale_Humidity ,2 ,raw=TRUE)+Z_Scale_dry_station_pressure,data=train_data)
test_results2 <-train_fit2 %>% predict (new_data= test_data) %>% mutate (truth=test_data$precip)
rmse(test_results2, truth=truth, estimate= .pred)

train_results2 <-train_fit2 %>% predict (new_data= train_data) %>% mutate (truth=train_data$precip)
rmse(train_results2, truth=truth, estimate= .pred)

rmse_model2_test <-rmse(test_results2, truth=truth, estimate= .pred)[3]
names(rmse_model2_test)<-"rmse"
rmse_model2_train <-rmse(train_results2, truth=truth, estimate= .pred)[3]
names(rmse_model2_train)<-"rmse"

#9-1 3rd model 
#buckets wind-speed

train_fit3 <- lm_spec %>%  fit (precip ~ poly(Z_Scale_Humidity ,2 ,raw=TRUE)+Z_Scale_dry_station_pressure+Buckets_Wind,data=train_data )

test_results <- train_fit3 %>%
  predict (new_data = test_data) %>%
  mutate (truth = test_data$precip) #creates new column

train_results <- train_fit3 %>%
  predict (new_data = train_data) %>%
  mutate (truth = train_data$precip) #creates new column

rmse(test_results, truth=truth, estimate= .pred)
rmse(train_results, truth=truth, estimate= .pred)
rmse_model3_train<-rmse(train_results, truth=truth, estimate= .pred)[3]
names(rmse_model3_train)<-"rmse"


rmse_model3_test<-rmse(test_results, truth=truth, estimate= .pred)[3]
names(rmse_model3_test)<-"rmse"

#9-1 4th model 
#regularization

LM_recipe <-recipe (precip ~ Z_Scale_Humidity +Z_Scale_dry_station_pressure+Z_Scale_dry_wind_speed,data=train_data)

tune_spec <- linear_reg(penalty = tune(), mixture=0)%>%
  set_engine("glmnet")

lasso_wf <- workflow() %>%
  add_recipe (LM_recipe)

weather_CVfolds <-vfold_cv(train_data)

lambda_grid <- grid_regular(levels=50, penalty(range= c(-3,0.3)))

ridge_grid <- tune_grid(
  lasso_wf %>% add_model (tune_spec), 
  resamples = weather_CVfolds, grid=lambda_grid)
show_best(ridge_grid, metric="rmse")
#best model with lambda 0.00346 

ridge_spec <- linear_reg(penalty = 0.00346, mixture=0)%>%
  set_engine("glmnet")

ridge_wf <- workflow() %>%
  add_recipe (LM_recipe)

weather_CVfolds <-vfold_cv(train_data)

ridge_fit <-   ridge_wf %>% add_model(ridge_spec) %>% fit(data=train_data)
ridge_fit %>%
  pull_workflow_fit() %>%
  tidy()

ridge_fit <-fit_resamples(ridge_spec, precip ~ Z_Scale_Humidity +Z_Scale_dry_station_pressure+Z_Scale_dry_wind_speed,resamples=weather_CVfolds )
metrics_ridge<-ridge_fit %>% collect_metrics()
rmse_model4_train<-metrics_ridge[1,3]
names(rmse_model4_train)<-"rmse"

ridge_fit_test <- ridge_wf %>% add_model(ridge_spec) %>%fit(data=test_data) %>% predict(new_data=test_data) %>% mutate(truth=test_data$precip)
rmse_model4_test<-rmse(ridge_fit_test,truth=truth, estimate= .pred)[3]
names(rmse_model4_test)<-"rmse"


#10 Evaluate the models on the testing set using at least one metric (like MSE, RMSE or R-squared).
#After calculating the metrics on the testing set for each model, print them out in as a table to easily compare.
#model_names <- c("model_1", "model_2", "model_3")
#train_error <- c("model_1_value", "model_2_value", "model_3_value")
#test_error <- c("model_1_value", "model_2_value", "model_3_value")
#comparison_df <- data.frame(model_names, train_error, test_error)


model_names <- c("Model1","Model2","Model3","Model4")
model_content <-c("lm(precip ~.","precip ~ poly2(Humidity)+pressure","as Model2 +Buckets_Wind","Best ridge_fit precip ~ Humidity +pressure+wind")
train_error <- c(rmse_model1_train,rmse_model2_train,rmse_model3_train,rmse_model4_train)
test_error <- c(rmse_model1_test,rmse_model2_test,rmse_model3_test,rmse_model4_test)
comparison_df <- data.frame(model_names,model_content, train_error, test_error)

#preparing the output-table:
table <- matrix(c(train_error,test_error,model_content),ncol=3)
colnames(table)<- c("RMSE (train)","RMSE (test)","content")
rownames(table)<-model_names
table

#answering the best-model is:
print(paste0("best model=",model_content[2]))