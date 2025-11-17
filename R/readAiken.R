#' Read an exam in Aiken format into an R list format
#'
#' This reads an exam in Aiken format into an R list,
#' suitable for further processing or export.
#'
#' @param input Path to the Aiken file OR a character vector (one element per line)
#' @param bestOf Default: 4. Expect this many answers per question, and warn if fewer
#' (or more) are found.
#'
#' @details The following is expected in the Aiken file format:
#' \itemize{
#'  \item Questions are seperated by at least one empty line
#'  \item Questions can be across multiple lines; they are merged into a single line
#'  \item Answers start with a single letter (uppercase or lowercase) and a dot or parenthesis.
#'    A. or a. or a) or (a) are all valid.
#'  \item The letter prepending each answer is IGNORED. The answers are treated in order of appearance.
#'  \item Correct answer is indicated by ANSWER: followed by the correct letter. Case-insensitive.
#'  \item Comments start with \\ and are ignored
#'  \item However, lines starting with \\feedbackCorrect or \\feedbackIncorrect are NOT ignored.
#'    They are merged together into the feedback for a correct or incorrect answer respectively.
#' }
#'
#' This behaviour ensures the maximum degree of tolerance in reading Aiken files.
#' An error is only generated if a section of the Aiken file has no question, or no answers,
#' or no correct answer (and the corresponding line is indicated).
#'
#' @return A list holding the question items. Each question item is a list with elements
#' \code{Q} (the question), \code{R} (the vector of responses), \code{correct} (the correct
#' answer, in a capital letter), and possibly \code{feedbackCorrect} (feedback text if the
#' answer is correct) and \code{feedbackIncorrect} (feedback text if the answer is incorrect).
#'
#' @examples
#' exam <- readAiken("final.txt")
#'
#' @export
readAiken <- function(input, bestOf=4) {
  # Reading lines and trimming whitespace
  if (length(input)==1) {
    if (!file.exists(input)) stop(sprintf("File '%s' does not exist!", input))
    x <- readLines(input)
  }
  x <- trimws(x)
  x1 <- gsub("  ", " ", x, fixed=TRUE)
  while(!identical(x,x1)) { x <- x1; x1 <- gsub("  ", " ", x1, fixed=TRUE) }
  rm(x1)

  b <- c(0, which(x==""), length(x)+1) # Identifying line breaks
  x <- lapply(1:(length(b)-1), function(i) {
    L <- x[(b[i]+1):(b[i+1]-1)]
    L <- L[!is.na(L) & L!=""]
    comments <- trimws(gsub("^//", "", L[grepl("(?i)^//(?!feedback\\s*(C|Inc)orrect)", L, perl=TRUE)]))
    L <- L[!grepl("(?i)^//(?!feedback\\s*(C|Inc)orrect)", L, perl=TRUE)]
    if (length(L)==0) return(NULL)
    res <- list(
      Q = NA,
      R = gsub("^(\\()?[A-Za-z](\\.|\\))\\s+", "", L[grepl("^(\\()?[A-Za-z](\\.|\\))\\s+", L)]),
      correct = substr(gsub("^ANSWER: ", "", toupper(L[grepl("^ANSWER: ", toupper(L))])), 1, 1),
      feedbackCorrect = paste(gsub("(?i)^//feedback\\s*Correct ", "", L[grepl("(?i)^//feedback\\s*Correct ", L)]), collapse=" "),
      feedbackIncorrect = paste(gsub("(?i)^//feedback\\s*Incorrect ", "", L[grepl("(?i)^//feedback\\s*Incorrect ", L)]), collapse=" "),
      comments = comments
    )
    L <- L[!grepl("^(\\()?[A-Za-z](\\.|\\))\\s+", L)]
    L <- L[!grepl("(?i)^//feedback\\s*(C|Inc)orrect", L)]
    L <- L[!grepl("^ANSWER: ", toupper(L))]
    if (length(L)==0) stop(sprintf("On line %s, no question found!", b[i]+1))
    res$Q <- paste(L, collapse=" ") # If multiple lines found, treat them as a single question!
    if (length(res$R)==0) stop(sprintf("On line %s, no answers found!", b[i]+1))
    if (length(res$correct)==0) stop(sprintf("On line %s, no correct answer found!", b[i]+1))
    if (length(res$R)!=bestOf) warning(sprintf("On line %s, %s answers found rather than %s.", b[i]+1, length(res$R), bestOf))
    if (length(res$correct)>1) {
      warning(sprintf("On line %s, multiple correct answers found and only the first will be kept.", b[i]+1))
      res$correct <- res$correct[1]
    }
    res
  })
  x <- x[!sapply(x, is.null)] # Dropping NULL questions
  return(x)
}
