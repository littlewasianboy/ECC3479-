install.packages("usethis")
install.packages("gitcreds")


usethis::use_git_config(user.name = "littlewasianboy", user.email = "mver0038@student.monash.edu")
usethis::create_github_token()
gitcreds::gitcreds_set()
usethis::git_sitrep()

#File conflict 
usethis::git_vaccinate()

usethis::create_from_github(
  repo_spec = "littlewasianboy/ECC3479-",
  destdir = "~/Desktop",  
  fork = FALSE
)

gert::git_add(".")
gert::git_commit("Created the hub")
gert::git_push()


usethis::use_readme_md()

#Importing and cleaning the data

gert::git_pull()


install.packages("readxl")
install.packages("zoo")
install.packages("writexl")
install.packages("gert")


library(readxl)
library(zoo)
library(writexl)
library(gert)

#This code will clear environment. Wont need to use this, do not RUN. 
rm(list = ls(all.names = TRUE))


raw_smoking_data = read_excel("ecc_data_raw.xlsx")
smoking_data = raw_smoking_data

smoking_data = smoking_data[order(smoking_data$year), ]

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


smoking_data <- rbind(smoking_data, missing_years)
smoking_data <- smoking_data[order(smoking_data$year), ]

smoking_data[smoking_data$year == 2006, "excise_tax"] <- 0.23259
smoking_data[smoking_data$year == 2008, "excise_tax"] <- 0.2545
smoking_data[smoking_data$year == 2009, "excise_tax"] <- 0.25833

#Linear extrapolation - LIMITATION 
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

#Linear interpolation -  

smoking_data$smoking_total <- na.approx(smoking_data$smoking_total, na.rm = FALSE)
smoking_data$smoking_male <- na.approx(smoking_data$smoking_male, na.rm = FALSE)
smoking_data$smoking_female <- na.approx(smoking_data$smoking_female, na.rm = FALSE)
smoking_data$cancer_total <- na.approx(smoking_data$cancer_total, na.rm = FALSE)
smoking_data$cancer_male <- na.approx(smoking_data$cancer_male, na.rm = FALSE)
smoking_data$cancer_female <- na.approx(smoking_data$cancer_female, na.rm = FALSE)

#Removing 2023 and illegal_smoking variable 

smoking_data$illegal_market <- NULL
smoking_data <- smoking_data[smoking_data$year != 2023, ]

#Removing duplicate rows

smoking_data <- smoking_data[!duplicated(smoking_data$year), ]

#Rounding 
cols_to_round <- setdiff(names(smoking_data), "year")
smoking_data[, cols_to_round] <- as.data.frame(lapply(smoking_data[, cols_to_round], round, 3))

#Final sanity check 
View(smoking_data)
head(smoking_data)
str(smoking_data)



gert::git_add(".")
gert::git_commit("Data cleaned and README completed")
gert::git_push()


# EDA 

# Summary stats for every variable

install.packages("moments")
library(moments)
summary(smoking_data)

sapply(smoking_data[, cols_to_round], sd, na.rm = TRUE)


#Histograms - Skewness 
par(mfrow = c(2, 3))  # arrange plots in a grid
hist(smoking_data$smoking_total, main = "Smoking Total", xlab = "")
hist(smoking_data$smoking_male, main = "Smoking Male", xlab = "")
hist(smoking_data$smoking_female, main = "Smoking Female", xlab = "")
hist(smoking_data$excise_tax, main = "Excise Tax", xlab = "")
hist(smoking_data$cancer_total, main = "Cancer Total", xlab = "")
hist(smoking_data$cancer_male, main = "Cancer Male", xlab = "")
par(mfrow = c(1, 1))  # reset

#Boxplots - Outliers 
par(mfrow = c(2, 3))
boxplot(smoking_data$smoking_total, main = "Smoking Total")
boxplot(smoking_data$smoking_male, main = "Smoking Male")
boxplot(smoking_data$smoking_female, main = "Smoking Female")
boxplot(smoking_data$excise_tax, main = "Excise Tax")
boxplot(smoking_data$cancer_total, main = "Cancer Total")
boxplot(smoking_data$cancer_male, main = "Cancer Male")
par(mfrow = c(1, 1))

#time series comparison 
par(mfrow = c(2, 3))
plot(smoking_data$year, smoking_data$smoking_total, type = "b", main = "Smoking Total", xlab = "Year", ylab = "")
plot(smoking_data$year, smoking_data$smoking_male, type = "b", main = "Smoking Male", xlab = "Year", ylab = "")
plot(smoking_data$year, smoking_data$smoking_female, type = "b", main = "Smoking Female", xlab = "Year", ylab = "")
plot(smoking_data$year, smoking_data$excise_tax, type = "b", main = "Excise Tax", xlab = "Year", ylab = "")
plot(smoking_data$year, smoking_data$cancer_total, type = "b", main = "Cancer Total", xlab = "Year", ylab = "")
plot(smoking_data$year, smoking_data$cancer_male, type = "b", main = "Cancer Male", xlab = "Year", ylab = "")
par(mfrow = c(1, 1))

#Correlation between variables 












