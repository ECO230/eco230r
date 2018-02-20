#' Return a list of 5 random words given a 4 digit numeric id
#'
#'This function is used during setup to validate that a student was able to install the eco230r package
#'
#' @param ID4 a 4 digit id this must be a number that is 4 digits long
#'
#' @return A string containing 5 random words and the ID that was passed into the function
#' @export
#'
#' @examples validation <- validate(1234)

validate <- function(ID4) {

if(!is.numeric(ID4))
  stop("You must use a 4 digit number")

words <- c("the",
           "be",
           "to",
           "of",
           "and",
           "a",
           "in",
           "that",
           "have",
           "I",
           "it",
           "for",
           "not",
           "on",
           "with",
           "he",
           "as",
           "you",
           "do",
           "at",
           "this",
           "but",
           "his",
           "by",
           "from",
           "they",
           "we",
           "say",
           "her",
           "she",
           "or",
           "an",
           "will",
           "my",
           "one",
           "all",
           "would",
           "there",
           "their",
           "what",
           "so",
           "up",
           "out",
           "if",
           "about",
           "who",
           "get",
           "which",
           "go",
           "me",
           "when",
           "make",
           "can",
           "like",
           "time",
           "no",
           "just",
           "him",
           "know",
           "take",
           "people",
           "into",
           "year",
           "your",
           "good",
           "some",
           "could",
           "them",
           "see",
           "other",
           "than",
           "then",
           "now",
           "look",
           "only",
           "come",
           "its",
           "over",
           "think",
           "also",
           "back",
           "after",
           "use",
           "two",
           "how",
           "our",
           "work",
           "first",
           "well",
           "way",
           "even",
           "new",
           "want",
           "because",
           "any",
           "these",
           "give",
           "day",
           "most",
           "us")

set.seed(ID4)
lst <- sample(words,5)

reply <- paste('Words for ', ID4, ':',lst[1], lst[2], lst[3], lst[4], lst[5])

}





