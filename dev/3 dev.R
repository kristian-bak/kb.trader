
## Document
devtools::document()

## Load all functions
devtools::load_all()

## Add dependencies
usethis::use_package("kb.yahoo")
usethis::use_package("kb.utils")
usethis::use_package("dplyr")

## Add README
usethis::use_readme_rmd()

## Add NEWS file
usethis::use_news_md()

## Add pipe operator
usethis::use_pipe()
