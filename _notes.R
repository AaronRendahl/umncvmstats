devtools::spell_check()
devtools::document()
devtools::check_man()

devtools::build_readme() # slow, it installs package first
devtools::build_vignettes()

devtools::build_site()

## subsets of build_site
pkgdown::build_home()
pkgdown::build_articles()

## uploads and checks elsewhere
devtools::check_mac_release()
devtools::check_win_release()
