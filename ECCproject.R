install.packages("usethis")
install.packages("gitcreds")


usethis::use_git_config(user.name = "littlewasianboy", user.email = "mver0038@student.monash.edu")
usethis::create_github_token()
gitcreds::gitcreds_set()
usethis::git_sitrep()


usethis::create_from_github(
  repo_spec = "littlewasianboy/ECC3479-",
  destdir = "~/Desktop",  # or wherever you want it saved locally
  fork = FALSE
)

gert::git_add(".")
gert::git_commit("Created the hub")
gert::git_push()


usethis::use_readme_md()

#Importing and cleaning the data

gert::git_pull()

library(readxl)
library(zoo)
install.packages("writexl")
library(writexl)

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

#Final sanity check 
View(smoking_data)
head(smoking_data)
str(smoking_data)



gert::git_add(".")
gert::git_commit("Data cleaned")
gert::git_push()




#
