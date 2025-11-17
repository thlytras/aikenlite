#' Shuffle the questions and/or items in a MCQ exam
#'
#' Shuffle the questions and/or items in a MCQ exam
#' (in list format)
#'
#' @param x An exam in list format, as read by \code{readAiken()} or \code{readNLM()}
#' @param q Shuffle questions? Default is \code{TRUE}
#' @param a Shuffle answers? Default is \code{TRUE}
#'
#' @return A list holding the shuffled question items.
#'
#' @export
shuffleExam <- function(x, q=TRUE, a=TRUE) {
  if (q) x <- x[sample(1:length(x))]
  if (a) {
    for (i in 1:length(x)) {
      s <- sample(1:length(x[[i]]$R))
      x[[i]]$R <- x[[i]]$R[s]
      x[[i]]$correct <- LETTERS[match(match(x[[i]]$correct, LETTERS), s)]
    }
  }
  return(x)
}
