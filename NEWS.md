# cofad (development version)

* place app copy controls beside their report, table, or citation content,
  move table explanations into compact side columns, clarify panel-versus-
  section heading sizes, and remove the duplicate example description below
  the selector while retaining its option tooltips
* add ordinary eta squared to the app F table, including the within-group/error
  SS share and calculation tooltips, display small p values rather than
  replacing them with a threshold, and use regular-weight Help and citation
  typography
* compare the total, between-group, and contrast-plus-error SS denominators in
  an interactive Plotly variance chart, with the component legend above the
  bars, labeled squared r effect-size measures, and hover details for every
  component
* show the package/webR build version in the app title, make every main panel
  collapsible, use a compact development commit label, start with the linear
  Rosenthal Table 3.1 example, and open Help and citation information by default
* use regular proportional type for the paper-ready report, switch very small
  report p values to scientific notation, report the effect as copy-safe rₑₛ,
  and document the guaranteed and conditional ordering of the three contrast
  effect-size magnitudes
* shorten the displayed squared-effect column heading to r² and attribute the
  app copyright to Johannes Titz et al.
* add GitHub-native copyable plain-text, HTML, and BibTeX citation blocks to the
  README
* make the Shiny report text, F table, and effect-size table directly copyable;
  reports now state whether repeated measures use magnitude-sensitive L scores
  or pattern-fit r scores, use signed t statistics with directional p values,
  and retain the opposite-direction warning in copied text
* use compact, content-sized model and result tables with right-aligned numeric
  output and booktabs styling, hide contrast tables for factors absent from the
  model, add example-data tooltips identifying real versus illustrative data,
  and consolidate documentation, issue links, and citation tools beneath the
  left model panel
* add participant-level `r` scores for within and mixed contrasts via
  `within_score = "r"`, alongside the default magnitude-sensitive L scores,
  with Rosenthal Table 5.3/5.4 examples and regression tests
* add a categorized example-data chooser to the Shiny and webR apps
* add APA 7 plain-text, HTML, and BibTeX citation-copy buttons to the app;
  `citation("cofad")` supplies both the tutorial and software references
* add corrected mixed-design data as `rosenthal_tbl68_mixed`, while retaining
  the historically packaged `rosenthal_tbl68` object for compatibility
* add the 2025 Behavior Research Methods tutorial as the primary citation,
  alongside the JOSS software paper
* replace radio-button model specification in the Shiny app with a stable,
  table-based interface
* add a detailed variance-decomposition table, squared effect-size measures,
  and a variance-partition figure to between-subjects and mixed-design app
  results; mixed output now clearly describes variation in derived
  within-contrast scores rather than raw repeated outcome variance
* correct sums of squares, pooled error variance, and effect sizes for
  between-subjects designs with unequal group sizes; raw and aggregated input
  now use the same formulas; aggregated input no longer requires an explicit
  `data = NULL`, and effect-size conversions preserve contrast direction
* improve file-import validation, example-data loading security, wording, and
  documentation
* add conservative automatic design suggestions based on replication, crossing,
  and nesting structure, with editable results in the Shiny model table
* add an optional Shinylive/webR static build, automatic GitHub Pages
  deployment, and in-process Shiny server tests
* make inconclusive automatic design detection explicitly fall back to the
  editable manual model table
* modernize GitHub Actions and add Dependabot updates for workflow actions
* reduce hard dependencies by replacing small uses of `dplyr`, `Hmisc`,
  `lifecycle`, `readr`, `rlang`, `stringr`, and `tibble` with base R or
  `foreign`; the pipe remains available for backwards compatibility
* verify from the books that Rosenthal, Rosnow, and Rubin was first published
  in 2000 and clarify Sedlmeier and Renkewitz's Table 16.1/16.2 distinction

# cofad 0.3.3

* report correct df for t test

# cofad 0.3.2

* change dv name for akan

# cofad 0.3.1

* fix github action for joss paper
* fix some typos
* add install instructions for cran
* remove rtools instructions (only relevant for dev version)
* change order of condition variable for akan data set
* add Maraver 2021 data set

# cofad 0.3.0

* several bugs were fixed including rare occasions, where the order of factors was not treated correctly
* included a helper function to calculate differences between two sets of lambdas for a competing contrast analysis, including documentation
* added data examples
* remove plyr as dependency
* spell check
* change maintainer to Johannes Titz, change order of authors, add contributors Mirka Henninger and Simone Malejka
* improve summary functions, content and display
* modify shiny GUI to use normal elements due to instability with moving elements (sortable)
* migrate to shinytest2
* include test for aggregated function, make it work with summary
* deduplicate code (reuse between for mixed)
* update summary for mixed

# cofad 0.2.1

* small improvements in documentation, references and paper for the official
publication at journal of open source software

# cofad 0.2.0

* Added a `NEWS.md` file to track changes to the package.
* Added Shiny GUI.
* Improved structure of the package.
* Fixed Bug with 0-variance conditions.
* Improved README.
* Improved examples.
* Improved documentation.
* Added and documented data sets.
* Added function for aggregated data.
