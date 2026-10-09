# Helpers used only by the pipeline (_targets.R).

RAW_FILES <- c("matches_1516.csv", "shots_1516.csv", "stints_1516.csv",
               "segments_1516.csv", "players_1516.csv")

# Python to run the build with: the PYTHON environment variable if set (e.g. in
# ~/.Renviron), otherwise python3, otherwise python.
find_python <- function() {
  py <- Sys.getenv("PYTHON")
  if (nzchar(py)) return(py)
  for (cand in c("python3", "python")) {
    p <- Sys.which(cand)
    if (nzchar(p)) return(unname(p))
  }
  stop("No Python found. Install Python 3, or set PYTHON in ~/.Renviron to its full path.")
}

# Run the build script (uses the download cache under data/statsbomb_raw/) and
# return the paths of the CSVs it writes.
run_build <- function(script) {
  py <- find_python()
  status <- system2(py, shQuote(script))
  if (!identical(as.integer(status), 0L)) {
    stop("build_shots.py failed (exit status ", status, ") using ", py, ". ",
         "If that Python lacks the requests package, run `", py, " -m pip install requests`, ",
         "or set PYTHON in ~/.Renviron to the Python you normally use.")
  }
  paths <- file.path(DATA_DIR, RAW_FILES)
  missing <- paths[!file.exists(paths)]
  if (length(missing)) stop("Build finished but these files are missing: ", paste(missing, collapse = ", "))
  paths
}

raw_file <- function(files, name) {
  f <- files[basename(files) == name]
  if (length(f) != 1L) stop("Expected exactly one raw file named ", name, ".")
  f
}
