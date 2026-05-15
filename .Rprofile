
# Activate renv (set renv/library as the project library):
source("renv/activate.R")

# Add box IDE hints if box.lsp is installed:
local({
  if (nzchar(system.file(package = "box.lsp"))) {
    options(
      languageserver.parser_hooks = list(
        "box::use" = box.lsp::box_use_parser
      )
    )
  }
})

# Set box paths:
local({
  # Set local box path
  wd <- getwd()
  options("box.path" = c(
    file.path(wd),
    file.path(wd, "src", "creators"),
    file.path(wd, "src", "options"),
    file.path(wd, "src", "diagnostics"),
    file.path(wd, "src", "results"),
    file.path(wd, "src", "others"),
    getOption("box.path")
  ))
})
