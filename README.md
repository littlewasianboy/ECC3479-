
# ECC3479-

<!-- badges: start -->
<!-- badges: end -->

Step 1: Clone the repository onto your machine. 

You will need to install the following Git packages:
install.packages("usethis")
install.packages("gitcreds")

Use the following code to clone the repo:

usethis::create_from_github(
  repo_spec = "littlewasianboy/ECC3479-",
  destdir = "~/Your desired file path location",
  fork = FALSE
)

Check status of git using this:

usethis::git_sitrep()

2. Cleaning the data

Before accessing the data you need to PULL the latest changes. 

gert::git_pull()

Great! Now lets clean the data. 



