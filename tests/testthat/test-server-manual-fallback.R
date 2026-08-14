model_hot_input <- function(variables) {
  model <- data.frame(
    role = c(
      "Dependent variable", "Between-subjects factor",
      "Within-subjects factor", "Participant ID"
    ),
    variable = variables,
    stringsAsFactors = FALSE
  )
  hot <- rhandsontable::rhandsontable(model)
  list(
    data = lapply(seq_len(nrow(model)), function(i) {
      unname(as.list(model[i, , drop = FALSE]))
    }),
    changes = list(event = "afterChange"),
    params = hot$x
  )
}

test_that("inconclusive detection supports manual model-table selection", {
  ambiguous <- data.frame(
    outcome = c(4.1, 2.0, 7.3, 5.8, 9.1, 1.2, 6.4, 3.5, 8.2, 10.0),
    predictor = seq_len(10)
  )
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  utils::write.csv(ambiguous, path, row.names = FALSE)
  upload <- list(
    name = "ambiguous.csv", size = unname(file.info(path)$size),
    type = "text/csv", datapath = path
  )

  shiny::testServer(myserver, {
    session$setInputs(datafile = upload)
    session$flushReact()

    expect_identical(reactive$design_suggestion$design, "undetermined")
    expect_match(
      as.character(output$variables$html),
      "Automatic detection was inconclusive"
    )
    expect_match(output$hot_model, "NONE")

    session$setInputs(hot_model = model_hot_input(
      c("outcome", "predictor", "NONE", "NONE")
    ))
    session$flushReact()

    expect_identical(
      reactive$model_spec,
      c(
        dv_name = "outcome", between_name = "predictor",
        within_name = "", id_name = ""
      )
    )
  })
})
