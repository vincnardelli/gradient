# setup
usethis::use_build_ignore("dev")
usethis::use_rcpp_armadillo()

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


