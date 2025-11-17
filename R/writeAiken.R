#' Write a MCQ exam (in list format) to Aiken format
#'
#' This takes an exam in list format and exports it to Aiken format.
#'
#' @param x An exam in list format, as read by \code{readAiken()} or \code{readNLM()}.
#' @param file The file to write to. If NULL, nothing will be written to disk.
#' @param numbering How to enumerate questions when exporting? Expressed as a format
#'   string for \code{sprintf()}, default \code{"[\%s] "}. Set to \code{""} to disable numbering.
#' @param ignoreComments If \code{TRUE} (the default) no comments will be exported,
#'   including feedback for correct or incorrect answers.
#'
#' @return A character vector (of length 1) containing the exam in Aiken format.
#'
#' @export
writeAiken <- function(x, file=NULL, numbering="[%s] ", ignoreComments=TRUE) {
  for (i in 1:length(x)) x[[i]]$i <- suppressWarnings(sprintf(numbering, i))
  out <- sapply(x, function(xx) {
    if (ignoreComments) {
      comments <- ""
    } else {
      if (xx$feedbackCorrect!="") xx$feedbackCorrect <- paste0("//feedbackCorrect ", xx$feedbackCorrect, "\n")
      if (xx$feedbackIncorrect!="") xx$feedbackIncorrect <- paste0("//feedbackCorrect ", xx$feedbackIncorrect, "\n")
      if (length(xx$comments)>0) xx$comments <- paste0("// ", xx$comments, "\n")
      comments <- paste0(c(xx$feedbackCorrect, xx$feedbackIncorrect, xx$comments), collapse="")
    }
    sprintf("%s%s\n%s\nANSWER: %s\n%s",
            xx$i, xx$Q,
            paste(LETTERS[1:length(xx$R)], xx$R, sep=". ", collapse="\n"),
            xx$correct, comments
    )
  })
  if (!is.null(file)) writeLines(out, file)
  paste(out, collapse="")
}
