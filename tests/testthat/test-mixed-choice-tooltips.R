test_that("mixed-design contrast choices explain their analyses", {
  shiny::testServer(myserver, {
    session$setInputs(example_dataset = "rosenthal_tbl53")
    session$flushReact()
    invisible(output$variables)
    session$setInputs(
      dv_name = "dv", between_name = "between",
      within_name = "within", id_name = "id"
    )
    session$flushReact()

    html <- output$variables$html
    expect_match(
      html,
      "whether the within-subject pattern differs across groups",
      fixed = TRUE
    )
    expect_match(
      html,
      "Between-subjects contrast weights are not used",
      fixed = TRUE
    )
    expect_gte(
      lengths(regmatches(
        html,
        gregexpr("cofad-help-tooltip", html, fixed = TRUE)
      )),
      3L
    )
  })
})
