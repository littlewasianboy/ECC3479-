
# ECC3479-

<!-- badges: start -->
<!-- badges: end -->

Step 1: Clone the repository onto your machine. 

You will need to install the following Git packages:

install.packages("usethis")
install.packages("readxl")
install.packages("zoo")
install.packages("writexl")
install.packages("gert")


Use the following code to clone the repo:

usethis::create_from_github(
  repo_spec = "littlewasianboy/ECC3479-",
  destdir = "~/Your desired file path location",
  fork = FALSE
)

(Desktop is always a nice place to save it )


2. Open the file:

Go to your saved file location, find the ECC3479- folder, and double click
ECC3479.Rproj to open in Rstudio. 

Before accessing the data you need to PULL the latest changes. You should 
have the latest version but do this just in case. 

gert::git_pull()

Everything is already loaded, to restart the process, clear your global 
environment:

rm(list = ls())

2. Cleaning the data

Great! Now lets clean the data. You can follow the code from here and 
these instructions for my thinking. 

I want to import the semi-cleaned data and turn this into cleaned data. 
For consistency lets create it as a separate variable:

raw_smoking_data = read_excel("ecc_data_raw.xlsx")
smoking_data = raw_smoking_data

smoking_data = smoking_data[order(smoking_data$year), ]

Hmm upon viewing the data we have missing values, lets set this up for 
interpolation:

missing_years <- data.frame(
  year = c(2006, 2008, 2009, 2023),
  smoking_total = NA,
  smoking_male = NA,
  smoking_female = NA,
  excise_tax = NA,
  illegal_market = NA,
  cancer_total = NA,
  cancer_male = NA,
  cancer_female = NA
)

Nice! but this code gives NA's for some ACTUAL values. Upon viewing the data
this happens with excise tax, lets fix this and bind the data frame to the 
original data set:

smoking_data <- rbind(smoking_data, missing_years)
smoking_data <- smoking_data[order(smoking_data$year), ]

smoking_data[smoking_data$year == 2006, "excise_tax"] <- 0.23259
smoking_data[smoking_data$year == 2008, "excise_tax"] <- 0.2545
smoking_data[smoking_data$year == 2009, "excise_tax"] <- 0.25833

Great! Binding was successful and actual values have been put back in. Viewing 
the data shows me that we are missing values for our cancer stats. Sadly 
we will have to extrapolate - this will be flagged as a model limitation:

last_value_total <- smoking_data[smoking_data$year == 2021, "cancer_total"]
second_last_total <- smoking_data[smoking_data$year == 2020, "cancer_total"]
annual_change_total <- last_value_total - second_last_total
smoking_data[smoking_data$year == 2022, "cancer_total"] <- last_value_total + annual_change_total

last_value_male <- smoking_data[smoking_data$year == 2021, "cancer_male"]
second_last_male <- smoking_data[smoking_data$year == 2020, "cancer_male"]
annual_change_male <- last_value_male - second_last_male
smoking_data[smoking_data$year == 2022, "cancer_male"] <- last_value_male + annual_change_male

last_value_female <- smoking_data[smoking_data$year == 2021, "cancer_female"]
second_last_female <- smoking_data[smoking_data$year == 2020, "cancer_female"]
annual_change_female <- last_value_female - second_last_female
smoking_data[smoking_data$year == 2022, "cancer_female"] <- last_value_female + annual_change_female

Those values have been extrapolated, however notice that there isnt a very
clear trend with lung cancer rates, we need to take extra care to disclose this.

Now we can interpolate some missing values for smoking incidence; 3-4 
observations per incidence variable is OK...still a limitation: 

smoking_data$smoking_total <- na.approx(smoking_data$smoking_total, na.rm = FALSE)
smoking_data$smoking_male <- na.approx(smoking_data$smoking_male, na.rm = FALSE)
smoking_data$smoking_female <- na.approx(smoking_data$smoking_female, na.rm = FALSE)
smoking_data$cancer_total <- na.approx(smoking_data$cancer_total, na.rm = FALSE)
smoking_data$cancer_male <- na.approx(smoking_data$cancer_male, na.rm = FALSE)
smoking_data$cancer_female <- na.approx(smoking_data$cancer_female, na.rm = FALSE)

Data looks amazing!! viewing the data shows us that 2023 is useless. 
I can also see that illegal market data is very limited. To avoid more 
model instability lets drop these both completely.

smoking_data$illegal_market <- NULL
smoking_data <- smoking_data[smoking_data$year != 2023, ]

Great! lets do a sanity check: 

View(smoking_data)
head(smoking_data)
str(smoking_data)











