test_that("app examples are grouped by design and can all be loaded", {
  info <- cofad_example_info()
  expect_setequal(
    unique(info$design), c("Between", "Within", "Mixed")
  )
  expect_true(all(vapply(info$name, function(name) {
    is.data.frame(load_cofad_example(name))
  }, logical(1))))

  choices <- cofad_example_choices()
  expect_identical(names(choices), c("Between", "Within", "Mixed"))
  expect_true("rosenthal_tbl54" %in% choices[["Within"]])

  selector <- as.character(cofad_example_select())
  expect_match(selector, '<optgroup label="Between">', fixed = TRUE)
  expect_lt(regexpr('label="Between"', selector, fixed = TRUE),
            regexpr('label="Within"', selector, fixed = TRUE))
  expect_lt(regexpr('label="Within"', selector, fixed = TRUE),
            regexpr('label="Mixed"', selector, fixed = TRUE))
  expect_match(selector, 'title="A real independent-groups example',
               fixed = TRUE)
  expect_match(
    selector,
    'value="rosenthal_tbl53" title="Age group by four repeated measures; includes Rosenthal&#39;s L and r examples (illustrative; not real data)." selected="selected"',
    fixed = TRUE
  )
  expect_true(all(info$data_type %in% c(
    "real data", "illustrative; not real data"
  )))
  expect_match(
    cofad_example_description("maraver"), "\\(real data\\)\\.$"
  )
  expect_match(
    cofad_example_description("furr_p4"),
    "\\(illustrative; not real data\\)\\.$"
  )
})

test_that("Rosenthal Table 6.8 has a stable mixed structure", {
  data(rosenthal_tbl68_mixed)
  expect_equal(nrow(rosenthal_tbl68_mixed), 32)
  expect_equal(length(unique(rosenthal_tbl68_mixed$id)), 8)
  expect_equal(as.integer(table(rosenthal_tbl68_mixed$between)), c(16L, 16L))
  expect_true(all(vapply(split(rosenthal_tbl68_mixed$between,
                               rosenthal_tbl68_mixed$id),
                         function(x) length(unique(x)) == 1, logical(1))))
})

test_that("the app loads a selected example in-process", {
  shiny::testServer(myserver, {
    session$setInputs(example_dataset = "")
    session$flushReact()
    session$setInputs(example_dataset = "rosenthal_tbl54")
    session$flushReact()
    expect_equal(nrow(reactive$data), 8)
    expect_identical(reactive$design_suggestion$design, "within")
    expect_match(output$example_description$html, "L and r")
  })
})

test_that("the default mixed example is analyzed at startup", {
  shiny::testServer(myserver, {
    session$flushReact()
    expect_identical(reactive$example_name, "rosenthal_tbl53")
    expect_identical(reactive$design_suggestion$design, "mixed")
    expect_identical(
      reactive$model_spec,
      cofad_example_spec("rosenthal_tbl53")$roles
    )
  })
})
