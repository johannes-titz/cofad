# cofad 0.4.0

## Analysis and API

* Added participant-level `r` scores for within-subjects and mixed contrasts via
  `within_score = "r"`. The existing `L` score remains the default: use `L`
  when response magnitude matters and `r` when agreement with the predicted
  pattern matters.
* Corrected sums of squares, pooled error variance, and effect sizes for
  between-subjects designs with unequal group sizes. Raw and aggregated input
  now use the same formulas, and effect-size conversions preserve contrast
  direction.
* Corrected the within-only `r_contrast` calculation to use the denominator
  degrees of freedom from its contrast test. The paper-ready report now uses
  the signed `r_contrast` consistently and relates its square to the
  contrast-specific eta-squared measures.
* Extended competing-contrast support to the app. Favored and rival weights are
  standardized before their difference is analyzed, matching `lambda_diff()`.
* Added conservative `detect_design()` suggestions based on replication,
  crossing, and nesting. Ambiguous data deliberately fall back to manual model
  selection.
* `calc_contrast_aggregated()` no longer requires an explicit `data = NULL`.
* Added the corrected mixed-design data set `rosenthal_tbl68_mixed`; the
  historical `rosenthal_tbl68` object remains available for compatibility.
* Reduced hard dependencies by replacing small uses of `dplyr`, `Hmisc`,
  `lifecycle`, `readr`, `rlang`, `stringr`, and `tibble` with base R or
  `foreign`. The pipe remains available for backward compatibility.

## Shiny app

* Restored stable, editable table-based model and contrast inputs. The model
  table shows automatic design suggestions but always permits manual changes.
* Added categorized between-subjects, within-subjects, and mixed examples. Each
  example loads the model roles and planned weights documented in its source;
  examples with published rival hypotheses open in competing mode.
* Mixed designs now explicitly offer either a between × within contrast or a
  within contrast averaged across groups. Pure designs follow the roles in the
  model table without redundant activation controls.
* Added detailed variance-decomposition F tables with ordinary and partial eta
  squared, calculation tooltips, directional paper-ready reports, and
  interactive Plotly partitions of variation. Mixed output is explicitly based
  on participants' derived within-contrast scores rather than raw repeated
  outcomes.
* Reports can be copied as rich HTML with a plain-text fallback. F tables can be
  copied as aligned text or HTML and downloaded as dependency-free DOCX files.
* Added APA 7 citation-copy controls for the tutorial and software paper. The
  package-level `citation("cofad")` command now returns both references.
* Improved file validation, example-loading security, small-p-value formatting,
  table sizing and alignment, tooltips, collapsible panels, accessible colors,
  version display, and responsive copy controls.

## Documentation, deployment, and testing

* Added the 2025 *Behavior Research Methods* tutorial as the primary citation,
  alongside the JOSS software paper.
* Expanded the README with current R, app, Docker, and webR examples; equations
  now use GitHub's native mathematical notation.
* Added a browser-only Shinylive/webR build with automatic GitHub Pages
  deployment and a containerized Shiny deployment through `Dockerfile`.
* Expanded numerical, validation, design-detection, example-preset, citation,
  export, and in-process Shiny server tests. GitHub Actions now check multiple R
  versions and platforms and publish test coverage.
* Verified the publication year and DOI metadata for Rosenthal, Rosnow, and
  Rubin (2000) and clarified the Sedlmeier and Renkewitz Table 16.1/16.2
  distinction.

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
