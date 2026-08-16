#' Curated example data sets for the Shiny app
#'
#' @return A data frame containing the data-set key, category, label, origin,
#'   and a short description.
#' @noRd
cofad_example_info <- function() {
  data.frame(
    name = c(
      "maraver", "furr_p4", "rosenthal_tbl31", "sedlmeier_p525",
      "testing_effect", "schwoebel",
      "akan", "rosenthal_tbl54", "sedlmeier_p537", "haans_within1by4",
      "rosenthal_tbl53", "rosenthal_tbl59", "rosenthal_p141",
      "rosenthal_chap5_q2", "rosenthal_tbl68_mixed"
    ),
    design = c(
      rep("Between", 6), rep("Within", 4), rep("Mixed", 5)
    ),
    label = c(
      "Henninger tutorial: Maraver et al. (2021)",
      "Furr (2004), p. 4", "Rosenthal et al. (2000), Table 3.1",
      "Sedlmeier & Renkewitz (2018), p. 525", "Testing effect (real data)",
      "Schwoebel et al. (2018)",
      "Henninger tutorial: Akan et al. (2018)",
      "Rosenthal et al. (2000), Table 5.4: L versus r",
      "Sedlmeier & Renkewitz (2018), p. 537", "Haans (2018), 1 x 4",
      "Rosenthal et al. (2000), Table 5.3", "Rosenthal et al. (2000), Table 5.9",
      "Rosenthal et al. (2000), p. 141", "Rosenthal et al. (2000), Ch. 5 Q2",
      "Rosenthal et al. (2000), Table 6.8"
    ),
    data_type = c(
      "real data", "illustrative; not real data",
      "illustrative; not real data", "illustrative; not real data",
      "real data", "real data", "real data",
      "illustrative; not real data", "illustrative; not real data",
      "illustrative; not real data", "illustrative; not real data",
      "illustrative; not real data", "illustrative; not real data",
      "illustrative; not real data", "illustrative; not real data"
    ),
    description = c(
      "A real independent-groups example used in the cofad tutorial.",
      "Four independent academic-major groups.",
      "Four independent groups with a linear contrast.",
      "Three independent training groups.",
      "Three independent learning conditions from a student research project.",
      "Four independent retrieval-practice conditions.",
      "A real three-condition repeated-measures example used in the tutorial.",
      "Two four-condition profiles showing why L and r answer different questions.",
      "Four repeated listening conditions for eight participants.",
      "Four repeated seating-row conditions.",
      "Age group by four repeated measures; includes Rosenthal's L and r examples.",
      "Psychotherapy group by medication condition.",
      "Unequal between-group sizes and two repeated medication conditions.",
      "Cognitive-complexity group by three repeated interaction levels.",
      "Gender group by four repeated time points."
    ),
    stringsAsFactors = FALSE
  )
}

cofad_example_choices <- function() {
  info <- cofad_example_info()
  design <- factor(info$design, levels = c("Between", "Within", "Mixed"))
  split(stats::setNames(info$name, info$label), design)
}

cofad_default_example <- function() {
  "rosenthal_tbl53"
}

#' Source-defined model roles and planned contrasts for app examples
#'
#' The examples should open with the hypotheses used in their books or papers,
#' rather than with weights inferred from the alphabetical order of factor
#' levels. A NULL contrast for a present between factor selects a within contrast
#' averaged across groups; the between factor is retained for grouping and error
#' pooling.
#'
#' @param name example data-set key
#' @return A list containing model roles and source-defined contrast weights.
#' @noRd
cofad_example_spec <- function(name) {
  spec <- switch(
    name,
    maraver = list(
      roles = c(dv_name = "prop_recalled", between_name = "condition",
                within_name = "", id_name = ""),
      between = c(imagine = 1, memorize = -0.5, pay_attention = -0.5),
      between_rival = c(imagine = 1, memorize = 0, pay_attention = -1)
    ),
    furr_p4 = list(
      roles = c(dv_name = "empathy", between_name = "major",
                within_name = "", id_name = ""),
      between = c(psychology = 1, education = -1,
                  business = 0, chemistry = 0)
    ),
    rosenthal_tbl31 = list(
      roles = c(dv_name = "dv", between_name = "between",
                within_name = "", id_name = ""),
      between = c(A = -3, B = -1, C = 1, D = 3)
    ),
    sedlmeier_p525 = list(
      roles = c(dv_name = "lsg", between_name = "between",
                within_name = "", id_name = ""),
      between = c(KT = -2, JT = 3, MT = -1),
      between_rival = c(KT = -2, JT = 1, MT = 1)
    ),
    testing_effect = list(
      roles = c(dv_name = "recalled", between_name = "condition",
                within_name = "", id_name = ""),
      between = c(itemtest = 1, restudy = -0.5, sourcetest = -0.5)
    ),
    schwoebel = list(
      roles = c(dv_name = "percent_recalled", between_name = "condition",
                within_name = "", id_name = ""),
      between = c(`massed-same` = -1, `massed-different` = -1,
                  `spaced-same` = 1, `spaced-different` = 1)
    ),
    akan = list(
      roles = c(dv_name = "contexts", between_name = "",
                within_name = "condition", id_name = "subject"),
      within = c(test = 1, restudy = -0.5, control = -0.5),
      within_rival = c(test = 0.5, restudy = 0.5, control = -1)
    ),
    rosenthal_tbl54 = list(
      roles = c(dv_name = "dv", between_name = "",
                within_name = "within", id_name = "id"),
      within = c(`1` = -3, `2` = -1, `3` = 1, `4` = 3)
    ),
    sedlmeier_p537 = list(
      roles = c(dv_name = "reading_test", between_name = "",
                within_name = "music", id_name = "participant"),
      # The book evaluates hypothesis 2 minus hypothesis 1.
      within = c(`without music` = 3, `white noise` = -1,
                 classic = -1, jazz = -1),
      within_rival = c(`without music` = 1.25, `white noise` = 0.25,
                       classic = -0.75, jazz = -0.75)
    ),
    haans_within1by4 = list(
      roles = c(dv_name = "value", between_name = "",
                within_name = "name", id_name = "person"),
      within = c(row1 = 3, row2 = 1, row3 = -1, row4 = -3)
    ),
    rosenthal_tbl53 = list(
      roles = c(dv_name = "dv", between_name = "between",
                within_name = "within", id_name = "id"),
      between = c(age8 = -1, age10 = 0, age12 = 1),
      within = c(`1` = -3, `2` = -1, `3` = 1, `4` = 3)
    ),
    rosenthal_tbl59 = list(
      roles = c(dv_name = "dv", between_name = "pt",
                within_name = "med", id_name = "id"),
      within = c(treatment = 1, placebo = -1)
    ),
    rosenthal_p141 = list(
      roles = c(dv_name = "dv", between_name = "bw",
                within_name = "med", id_name = "id"),
      within = c(treatment = -1, placebo = 1)
    ),
    rosenthal_chap5_q2 = list(
      roles = c(dv_name = "dv", between_name = "between",
                within_name = "within", id_name = "id"),
      between = c(high = 1, low = -1),
      within = c(low = -1, medium = 0, high = 1)
    ),
    rosenthal_tbl68_mixed = list(
      roles = c(dv_name = "dv", between_name = "between",
                within_name = "within", id_name = "id"),
      # The school-year theory is compared directly with the linear age theory.
      within = c(t1 = -1, t2 = 0, t3 = 0, t4 = 1),
      within_rival = c(t1 = -3, t2 = -1, t3 = 1, t4 = 3)
    ),
    NULL
  )
  if (is.null(spec)) return(NULL)
  spec$competing <- !is.null(spec$between_rival) || !is.null(spec$within_rival)
  spec
}

cofad_example_description <- function(name) {
  info <- cofad_example_info()
  match_row <- match(name, info$name)
  if (is.na(match_row)) return("")
  paste0(
    sub("[.]$", "", info$description[[match_row]]),
    " (", info$data_type[[match_row]], ")."
  )
}

cofad_example_select <- function(selected = cofad_default_example()) {
  info <- cofad_example_info()
  groups <- c("Between", "Within", "Mixed")
  shiny::tagList(
    shiny::tags$label(`for` = "example_dataset",
                      "Or load a categorized example"),
    shiny::tags$select(
      id = "example_dataset", class = "form-control",
      shiny::tags$option(value = "", "Choose an example"),
      lapply(groups, function(group) {
        rows <- info$design == group
        shiny::tags$optgroup(
          label = group,
          lapply(which(rows), function(i) {
            shiny::tags$option(
              value = info$name[[i]],
              title = cofad_example_description(info$name[[i]]),
              selected = if (identical(info$name[[i]], selected)) "selected",
              info$label[[i]]
            )
          })
        )
      })
    )
  )
}
