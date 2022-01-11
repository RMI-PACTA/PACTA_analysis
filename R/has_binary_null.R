has_binary_null <- function(path, n = 2048L) {
  con <- gzfile(path, open = "rb")
  on.exit(close(con))
  hdr <- readBin(con, what = "raw", n = n)
  any(hdr == 0)
}
