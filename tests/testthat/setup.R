python <- Sys.getenv("NORSE_RESEARCH_PYTHON", unset = "")

if (nzchar(python)) {
  reticulate::use_python(python, required = TRUE)
}
