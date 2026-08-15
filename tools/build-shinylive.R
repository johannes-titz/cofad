#!/usr/bin/env Rscript

root <- normalizePath(getwd())
if (!file.exists(file.path(root, "DESCRIPTION")) ||
    !dir.exists(file.path(root, "R"))) {
  stop("Run this script from the cofad repository root.", call. = FALSE)
}
if (!requireNamespace("shinylive", quietly = TRUE)) {
  stop(
    "Package 'shinylive' is required. Install it with install.packages('shinylive').",
    call. = FALSE
  )
}

stage_dir <- file.path(root, "_shinylive_app")
site_dir <- file.path(root, "docs")
if (dir.exists(stage_dir)) unlink(stage_dir, recursive = TRUE)
if (dir.exists(site_dir)) unlink(site_dir, recursive = TRUE)

dir.create(file.path(stage_dir, "R"), recursive = TRUE)
dir.create(file.path(stage_dir, "data"), recursive = TRUE)
dir.create(file.path(stage_dir, "extdata"), recursive = TRUE)

description <- read.dcf(file.path(root, "DESCRIPTION"))
build_version <- unname(description[1, "Version"])
build_commit <- tryCatch(
  system2(
    "git", c("-C", root, "rev-parse", "--short=8", "HEAD"),
    stdout = TRUE, stderr = FALSE
  ),
  error = function(e) ""
)
writeLines(
  c(
    "options(",
    "  cofad.webR = TRUE,",
    paste0("  cofad.version = ", encodeString(build_version, quote = "\""), ","),
    paste0(
      "  cofad.commit = ",
      encodeString(substr(trimws(build_commit[1]), 1, 8), quote = "\"")
    ),
    ")"
  ),
  file.path(stage_dir, "build-info.R")
)

invisible(file.copy(
  file.path(root, "inst", "shinylive", "app.R"), stage_dir,
  overwrite = TRUE
))
files <- c(
  "calc_contrast.R", "calc_contrast_aggregated.R", "design_detection.R",
  "examples.R", "helper.R", "print_methods.R", "summary_methods.R", "ui.R",
  "server.R"
)
invisible(file.copy(
  file.path(root, "R", files), file.path(stage_dir, "R"), overwrite = TRUE
))
invisible(file.copy(
  list.files(file.path(root, "data"), full.names = TRUE),
  file.path(stage_dir, "data"), overwrite = TRUE
))
invisible(file.copy(
  file.path(
    root, "inst", "extdata",
    c("intro.html", "citation.txt", "cofad-copy.js")
  ),
  file.path(stage_dir, "extdata"), overwrite = TRUE
))

started <- proc.time()[["elapsed"]]
shinylive::export(stage_dir, site_dir)
elapsed <- proc.time()[["elapsed"]] - started
files_written <- list.files(site_dir, recursive = TRUE, full.names = TRUE)
size_mb <- sum(file.info(files_written)$size, na.rm = TRUE) / 1024^2
cleanup_status <- unlink(stage_dir, recursive = TRUE)
if (cleanup_status != 0 || dir.exists(stage_dir)) {
  warning("Could not remove temporary build directory: ", stage_dir)
}

message("Shinylive site written to: ", site_dir)
message(sprintf("Export time: %.1f seconds", elapsed))
message(sprintf("Static output size: %.1f MiB", size_mb))
message("Preview with: httpuv::runStaticServer('docs')")
