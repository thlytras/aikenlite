#' Output the answer key for an exam
#'
#' This takes an exam in list format and outputs the annswer key,
#' in CSV or XLSX format (only if package WriteXLS is installed)
#'
#' @param x An exam in list format, as read by \code{readAiken()}
#' @param file The file name to write (with .csv or .xls or .xlsx
#' extension) or \code{NULL} if no file is to be output.
#' @param nrow The number or rows output in a key. Default is 12.
#'
#' @return The answer key in a matrix format (with \code{nrow} rows).
#'   At EUC we use this to copy the exam key to Scantron paper forms,
#'   for bulk machine-grading of exam papers.
#'
#' @importFrom WriteXLS WriteXLS
#' @importFrom utils write.csv
#' @export
writeKey <- function(x, file=NULL, nrow=12) {
  ans <- sapply(x, function(xx) xx$correct)
  ans <- ans[1:(ceiling(length(ans)/nrow)*nrow)]
  key <- matrix(ans, nrow=nrow)
  if (!is.null(file) && require(WriteXLS)) {
    if (!grepl("\\.csv$", file) && requireNamespace("WriteXLS", quietly = TRUE)) {
      WriteXLS::WriteXLS(as.data.frame(key), file, "Key")
    } else {
      utils::write.csv(as.data.frame(key), file)
    }
  }
  key
}
