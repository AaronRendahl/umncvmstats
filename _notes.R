# https://usethis.r-lib.org/articles/git-credentials.html
usethis::create_github_token()
gitcreds::gitcreds_set()

devtools::document()
devtools::spell_check()
devtools::document()
devtools::check_man()

devtools::build_readme() # slow, it installs package first
devtools::build_vignettes()

devtools::build_site()

## subsets of build_site
pkgdown::build_home()
pkgdown::build_reference()
pkgdown::build_articles()
pkgdown::build_article("setup")
pkgdown::build_article("data")
pkgdown::build_article("proportions")
pkgdown::build_article("graphics")

## uploads and checks elsewhere
devtools::check_mac_release()
devtools::check_win_release()
devtools::check_win_devel()

pkgload::dev_help('one_proportion_inference')

# https://github.com/ThinkR-open/prepare-for-cran
attachment::att_amend_desc()
tags <- checkhelper::find_missing_tags()
