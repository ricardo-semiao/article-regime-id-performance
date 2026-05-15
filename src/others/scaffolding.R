
# For configuring VSCode's R features with renv. Musnt't be run

# See more in:
# https://github.com/REditorSupport/vscode-R/wiki/Working-with-renv-enabled-projects

library(languageserver)
library(box.lsp)
library(httpgd)
library(vscDebugger)

# Initial configuration:
if (FALSE) {
  renv::init()
  renv::settings$r.version("4.5.1")
  box.lsp::use_box_lsp(file_path = ".Rprofile")
}

# Manual installs:
if (FALSE) {
  renv::install("ManuelHentschel/vscDebugger")
  renv::install("katex")
}

# Continuous development:
if (FALSE) {
  renv::dependencies() |>
    dplyr::mutate(
      Source = gsub(".+/article-regime-id-performance/(.+)", "\\1", Source)
    ) |>
    with(unique(Package))
  status <- renv::status()
  renv::snapshot()
  renv::restore()
}

if (FALSE) {
  renv::diagnostics()
  renv::repair()
  renv::clean()
  renv::rebuild()
  renv::plan()
}
