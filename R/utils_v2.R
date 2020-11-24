# utilities --------------------------------------------------------------------

get_rds_data_from_path <-
  function(path, filename) {
    filepath <- file.path(path, filename)
    if (tools::file_ext(filepath) != "rds") {
      stop(paste0("file does not have the appropriate 'rds' extension: ", filename))
    }
    if (!file.exists(filepath)) {
      stop(paste0("file does not exist: ", filepath))
    }
    tryCatch(
      dplyr::ungroup(dplyr::as_tibble(readRDS(filepath))),
      error = function(e) stop(paste0("file cannot be read with readRDS(): ", filepath))
    )
  }


fast_match <-
  function(x, dict) {
    if (inherits(x, "data.frame") && ncol(x) == 1L) { x <- x[[1]] }

    stopifnot(inherits(x, "character"))
    stopifnot(inherits(dict, "data.frame"))
    stopifnot(ncol(dict) == 2L)
    stopifnot(inherits(dict[[1]], "character"))

    xfctr <- factor(x)
    matchidxs <- match(levels(xfctr), dict[[1]])
    matches <- dict[[2]][matchidxs]
    matches[as.numeric(xfctr)]
  }


save_files_to <-
  function(path, ...) {
    if (!dir.exists(path)) { dir.create(path) }

    dots <- match.call(expand.dots = FALSE)$...

    lapply(dots, function(obj_sym) {
      obj_name <- deparse(obj_sym)
      if (exists(obj_name)) {
        filename <- paste0(obj_name, ".fst")
        fst::write_fst(base::get(obj_name), file.path(path, filename))
      }
    })

    if (any(file.size(list.files(path, full.names = T)) > 100e6)) {
      warning("File size exceeds what can be pushed to GitHub. Check before Committing")
    }

    invisible(TRUE)
  }
