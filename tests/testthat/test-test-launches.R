test_that(
  "app launches",{
    skip_on_cran()
    skip_on_travis()
    skip_on_appveyor()
    x <- processx::process$new(
      "R", 
      c(
        "-e", 
        "shiny::runApp()"
      )
    )
    Sys.sleep(1)
    expect_true(x$is_alive())
    x$kill()
  }
)