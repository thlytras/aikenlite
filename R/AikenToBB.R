#' Create a Blackboard question bank from an exam in Aiken format
#'
#' This functions creates a Blackboard question bank from an exam in Aiken
#' format, which is a simple text format.
#'
#' @param input Path to the Aiken file
#' @param title Title for the question bank. If not supplied, the name of the file is used.
#' @param rand.answers Randomize the ordering of the answers?
#' @param rand.questions Randomize the question order?
#'
#' @details This is an old function that will be deprecated. Use \code{readAiken()} instead.
#'
#' @return A list holding the question items. Also creates a zip file with the question bank.
#'
#' @examples
#' exam <- AikenToBB("final.txt", rand.questions=TRUE)
#'
#' @export
AikenToBB <- function(input, title=NULL, rand.answers=TRUE, rand.questions=FALSE) {
  if (is.null(title)) title <- gsub("\\.txt$", "", input)

  x <- readLines(input)
  b <- c(0, which(trimws(x)==""), length(x)+1)
  x <- lapply(1:(length(b)-1), function(i) {
    L <- x[(b[i]+1):(b[i+1]-1)]
    res <- list(
      Q = L[1],
      R = gsub("^[A-D]\\. ", "", trimws(L[-c(1,length(L))])),
      correct = gsub("^ANSWER: ", "", toupper(L[length(L)]))
    )
    res
  })

  if (rand.answers) {
    x <- lapply(x, function(y) {
      o <- sample(1:length(y$R))
      y$correct <- LETTERS[match(match(y$correct, LETTERS[1:length(y$R)]), o)]
      y$R <- y$R[o]
      y
    })
  }
  if (rand.questions) x <- x[sample(1:length(x))]

  tmp_manifest <- '<?xml version="1.0" encoding="UTF-8"?>
<manifest identifier="man00001"><organization default="toc00001"><tableofcontents identifier="toc00001"/></organization><resources><resource baseurl="res00001" file="res00001.dat" identifier="res00001" type="assessment/x-bb-pool"/></resources></manifest>'

  tmp_qfile <- '<?xml version="1.0" encoding="utf-8"?>
<POOL>
  <COURSEID value="IMPORT" />
  <TITLE value="%s" />
  <DESCRIPTION>
    <TEXT>%s</TEXT>
  </DESCRIPTION>
  <QUESTIONLIST>
%s
  </QUESTIONLIST>
%s
</POOL>'

  tmp_qitem <- '  <QUESTION_MULTIPLECHOICE id="q%s">
    <BODY>
      <TEXT>%s</TEXT>
      <FLAGS value="true">
        <ISHTML value="true" />
        <ISNEWLINELITERAL />
      </FLAGS>
    </BODY>
    <ANSWER id="q%s_a1" position="1">
      <TEXT>%s</TEXT>
    </ANSWER>
    <GRADABLE>
      <FEEDBACK_WHEN_CORRECT>Good work</FEEDBACK_WHEN_CORRECT>
      <FEEDBACK_WHEN_INCORRECT>That is not correct</FEEDBACK_WHEN_INCORRECT>
      <CORRECTANSWER answer_id="q%s_a%s" />
    </GRADABLE>
    <ANSWER id="q%s_a2" position="2">
      <TEXT>%s</TEXT>
    </ANSWER>
    <ANSWER id="q%s_a3" position="3">
      <TEXT>%s</TEXT>
    </ANSWER>
    <ANSWER id="q%s_a4" position="4">
      <TEXT>%s</TEXT>
    </ANSWER>
  </QUESTION_MULTIPLECHOICE>'

  outManifest <- tmp_manifest
  outFile <- sprintf(tmp_qfile, title, title,
                     do.call(paste, c(lapply(1:length(x), function(i)
                       sprintf('    <QUESTION id="q%s" class="QUESTION_MULTIPLECHOICE" />', i)
                     ), collapse="", sep="\n")),
                     do.call(paste, c(lapply(1:length(x), function(i) sprintf(tmp_qitem, i, x[[i]]$Q,
                                                                              i, x[[i]]$R[1], i, match(x[[i]]$correct, LETTERS),
                                                                              i, x[[i]]$R[2], i, x[[i]]$R[3], i, x[[i]]$R[4])), sep="\n", collapse="")))

  writeLines(outManifest, sprintf("%s/imsmanifest.xml", tempdir()))
  writeLines(outFile, sprintf("%s/res00001.dat", tempdir()))

  zip(sprintf("%s.zip", gsub(" ", "_", title, fixed=TRUE)),
      c(sprintf("%s/res00001.dat", tempdir()), sprintf("%s/imsmanifest.xml", tempdir())), flags="-qjFS")
  return(x)
}
