#' Read questions in NotebookLM JSON format into an R list format
#'
#' This reads questions in Google NotebookLM JSON format into an R list,
#' suitable for further processing or export.
#'
#' @param input The exported questions from NotebookLM. A JSON string, URL or file.
#'
#' @details The "rationale" for the incorrect answers is combined into a single feedback.
#'  The "hint" for each quesion is included as an extra comment.
#'
#' @return A list holding the question items. Each question item is a list with elements
#' \code{Q} (the question), \code{R} (the vector of responses), \code{correct} (the correct
#' answer, in a capital letter), and possibly \code{feedbackCorrect} (feedback text if the
#' answer is correct) and \code{feedbackIncorrect} (feedback text if the answer is incorrect).
#'
#' @examples
#' exam <- readNLM("questions.json")
#'
#' @importFrom jsonlite fromJSON
#'
#' @export
readNLM <- function(input) {
  x <- jsonlite::fromJSON(input)
  if (length(x)!=1 && names(x)!="quiz" & !is.data.frame(x$quiz))
    stop("The provided JSON does not appear to contain a NotebookLM \"quiz\"")
  x <- x$quiz
  out <- mapply(function(q,a,h) {
    res <- list(
      Q = q,
      R = a$text,
      correct = LETTERS[which(a$isCorrect)],
      feedbackCorrect = a$rationale[a$isCorrect],
      feedbackIncorrect = paste(a$rationale[!a$isCorrect], collapse=" "),
      comments  = paste("Hint:", h)
    )
  }, q=x$question, a=x$answerOptions, h=x$hint, SIMPLIFY=FALSE, USE.NAMES=FALSE)
  return(out)
}
