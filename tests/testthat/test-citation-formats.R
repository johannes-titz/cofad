test_that("the app offers both package citations in three copy formats", {
  plain <- cofad_citation_plain()
  html <- cofad_citation_html()
  bib <- cofad_citation_bibtex()

  for (citation in c(plain, html, bib)) {
    expect_match(citation, "Henninger")
    expect_match(citation, "Titz")
    expect_match(citation, "10.3758/s13428-025-02833-w", fixed = TRUE)
    expect_match(citation, "10.21105/joss.03822", fixed = TRUE)
  }
  expect_match(plain, "Behavior Research Methods, 57, Article 326", fixed = TRUE)
  expect_match(html, "<i>Behavior Research Methods, 57</i>", fixed = TRUE)
  expect_match(bib, "@article{henninger2025cofad", fixed = TRUE)

  panel <- as.character(cofad_citation_panel())
  expect_match(panel, "copy_citation_plain")
  expect_match(panel, "copy_citation_html")
  expect_match(panel, "copy_citation_bib")
  expect_false(grepl('citation("cofad")', panel, fixed = TRUE))
})

test_that("the R citation file contains both required references", {
  citation_file <- system.file("CITATION", package = "cofad")
  expect_true(nzchar(citation_file))
  citations <- utils::readCitationFile(citation_file)
  expect_length(citations, 2)
  dois <- vapply(citations, function(x) unname(x$doi), character(1))
  expect_setequal(
    dois,
    c("10.3758/s13428-025-02833-w", "10.21105/joss.03822")
  )
})
