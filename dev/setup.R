# setup
usethis::use_build_ignore("dev")
usethis::use_rcpp_armadillo()
usethis::use_package("foreach")
usethis::use_package("doParallel")
usethis::use_package("ggplot2")
usethis::use_package("tidyr")
usethis::use_package("gridExtra")

usethis::use_description(
  list(
    Title = "Gradient",
    `Authors@R` = "c(
    person('Alice', 'Giampino', email = 'a.giampino@campus.unimib.it', role = c('cre', 'aut')),
    person('Vincenzo', 'Nardelli', email = 'v.nardelli2@campus.unimib.it', role = c('aut')))",
    Description = "A small package for efficient descent computation!",
    URL = "https://github.com/vanlog/shapes"
  )
)
usethis::use_lgpl_license( name = "Alice Giampino and Vincenzo Nardelli" )
usethis::use_tidy_description()

usethis::use_readme_md( open = FALSE )

usethis::use_testthat()
usethis::use_test("gd")


usethis::use_git_config(
  scope = "user",
  user.name = "Vincenzo Nardelli",
  user.email = "vincnardelli@gmail.com"
)
usethis::use_git()

usethis::use_travis()
usethis::use_coverage()


usethis::use_vignette(name="linear_model_with_gradient_package")
