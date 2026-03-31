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

raw_smoking_data = read_excel("ecc_data_raw.xlsx")
smoking_data = raw_smoking_data

cleaned_smoking <- cleaned_smoking[order(cleaned_smoking$year), ]




list.files()

gert::git_add(".")
gert::git_commit("imported data into R")
gert::git_push()




#
