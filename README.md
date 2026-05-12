
# ECC3479-

<!-- badges: start -->
<!-- badges: end -->

Step 1: Clone the repository onto your machine. 

In your blank R console, use the following code:

install.packages("usethis")
library(usethis)

Now use the following code to clone the repo:

usethis::create_from_github(
  repo_spec = "littlewasianboy/ECC3479",
  destdir = "C:/Users/yourname/Desktop",
  fork = FALSE
)

(alternatively instead of writing your name you can just use:
"~/Desktop" after destdir)


2. Open the file:

If your machine automatically opens the Rproj, then great. In the files section
on the right, click on NewECC.R and it will load. 

IF NOT

Go to your saved file location, find the ECC3479- folder, and double click
NewECC.Rto open in Rstudio. 

If repository is already cloned onto your machine from earlier, you will not need to repeat this entire process. 

If your global environment is full, clear your global environment:

rm(list = ls())

Otherwise DO NOT RUN THIS CODE. 

OR an even simpler version if you are having difficulties. You can copy and paste the raw code from the NewECC.R file into your R studio. The data is downloaded from Yahoo finance and the RBA respectively, using the quantmod and readrba packages. It is fully cleaned through R so no additional files need to be taken from the repository. 

2. Cleaning the data 

Run the code from line 4 to 197. This contains every single variable, and all merges into the regression data.  

3. Regression models

Run the code from line 395 to 425. This will print the raw regression results. 

4. Rmd file

Open the Rmd file. If it was cloned on your machine properly, you will see it in the tabs. If not, you can simply access it from the ECC3479- folder and open it with R studio. Knit the file at the top to view in html format.  

5. Robustness Analysis

Run the code from 435 to 635. This contains all robustness tests. 

6. Robustness Rmd file

Open the Robustness_Analysis.Rmd file. Similar to step 4, assuming cloning worked on your machine, you will see it in the tabs. If not, access it from the ECC3479- folder, or alternatively the GitHub if fatal issues occur. 


------

Everything here is from the old README.md. I have left it here in case you still require it for past assessments. 

install.packages("readxl")
install.packages("zoo")
install.packages("writexl")
install.packages("gert")


library(readxl)
library(zoo)
library(writexl)
library(gert)

Now run the code from line 46-99, and view the the clean data. The clean data
is named smoking_data. 

The rest of this document is my intuition for each line of code and a step by 
step. Feel free to read if necessary! 

-- 


Great! Now lets clean the data. You can follow the code from here and 
these instructions for my thinking. 

I want to import the semi-cleaned data and turn this into cleaned data. 
For consistency lets create it as a separate variable. Starting from line 46:

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

Hold on - there are duplicate rows from the binding process! Lets fix:

smoking_data <- smoking_data[!duplicated(smoking_data$year), ]

Great! lets do a final sanity check: 

View(smoking_data)
head(smoking_data)
str(smoking_data)











