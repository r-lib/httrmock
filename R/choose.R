
#' @importFrom utils menu

choose <- function(partial = "", answers) {
  good_answers <- answers[startsWith(answers, partial)]
  sel <- menu(good_answers, title = "Choose current test")
  if (sel == 0) stop("No test selected")
  good_answers[sel]
}
