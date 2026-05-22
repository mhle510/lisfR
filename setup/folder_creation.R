repo_path <- "~/Documents/GitHub/lisfR"
setwd(repo_path)

# Create main package folders
dir.create("R", showWarnings = FALSE)
dir.create("examples", showWarnings = FALSE)
dir.create("inst", showWarnings = FALSE)
dir.create("inst/extdata", recursive = TRUE, showWarnings = FALSE)
dir.create("tests", showWarnings = FALSE)

# Create basic package files
if (!file.exists("DESCRIPTION")) {
  writeLines(
    c(
      "Package: lisfR",
      "Title: R Tools for LISF Workflows",
      "Version: 1.0.0",
      "Authors@R: person('Manh-Hung', 'Le', email = 'manh-hung.le@nasa.giv', role = c('aut')",
      "Description: R utilities for working with LISF",
      "License: MIT + file LICENSE",
      "Encoding: UTF-8",
      "Roxygen: list(markdown = TRUE)",
      "RoxygenNote: 7.3.2",
      "Imports:",
      "    ncdf4,",
      "    terra,",
      "    data.table,",
      "    lubridate,",
      "    stringr,",
      "    tidyverse,",
      "    sf"
    ),
    "DESCRIPTION"
  )
}

if (!file.exists("NAMESPACE")) {
  writeLines("", "NAMESPACE")
}

if (!file.exists("README.md")) {
  writeLines(
    c(
      "# lisfR",
      "",
      "R tools for supporting Land Information System Framework workflows.",
      "",
      "## Purpose",
      "",
      "- Check LDT output",
      "- Check LIS output",
      "- Convert LIS output to other formats",
      "- Support LISF post-processing workflows"
    ),
    "README.md"
  )
}

if (!file.exists("lisfR.Rproj")) {
  writeLines(
    c(
      "Version: 1.0",
      "",
      "RestoreWorkspace: Default",
      "SaveWorkspace: Default",
      "AlwaysSaveHistory: Default",
      "",
      "EnableCodeIndexing: Yes",
      "UseSpacesForTab: Yes",
      "NumSpacesForTab: 2",
      "Encoding: UTF-8",
      "",
      "RnwWeave: Sweave",
      "LaTeX: pdfLaTeX"
    ),
    "lisfR.Rproj"
  )
}