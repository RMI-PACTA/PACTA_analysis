read_web_input_file <- function(input_file_path) {
  file_ext <- tools::file_ext(input_file_path)

  if (file_ext == "csv") {
    input_file <- read_csv(input_file_path, col_types = cols())
  }
  if (file_ext == "xlsx") {
    input_file <- read_xlsx(input_file_path)
  }
  if (file_ext == "txt") {
    enc <- guess_encoding(input_file_path)$encoding[1]
    input_file <- read.table(input_file_path, sep = ",", header = T, fileEncoding = enc)

    if (ncol(input_file) == 1) {
      input_file <- read.table(input_file_path, sep = "\t", header = T, fileEncoding = enc)
    }

    if (ncol(input_file) == 1) {
      input_file <- read.table(input_file_path, sep = ";", header = T, fileEncoding = enc)
    }
  }

  if (data_check(input_file) == FALSE) {
    warning("Input file not readable")
    ifelse(nrow(input_file) == 0,
           write_log(
             msg = "Input file has 0 rows. Please ensure the uploaded file is not empty.",
             file_path = log_path
           ),
           write_log(
             msg = "Input file could not be transformed into a data.frame. Please check the uploaded file has the correct format.",
             file_path = log_path
           )
    )
  }

  return(input_file)
}
