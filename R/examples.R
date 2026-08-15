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
  "rosenthal_tbl31"
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
