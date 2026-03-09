#' @description This package serves as an API client for reading and writing
#' to and from \href{https://www.wikidata.org/wiki/Wikidata:Main_Page}{Wikidata}, (including 
#' via the \href{https://quickstatements.toolforge.org/}{QuickStatements} format),
#' as well as for reading from \href{https://www.wikipedia.org}{Wikipedia}.
#' @seealso \code{\link{get_random}} for selecting a random item or property,
#' \code{\link{get_item}} for a /specific/ item or property, or \code{\link{find_item}}
#' for using search functionality to pull out item or property IDs where the descriptions
#' or aliases match a particular search term.
"_PACKAGE"

## usethis namespace: start
#' @import crayon
#' @import dplyr
#' @import pbapply
#' @import progress
#' @import stringr
#' @import tibble
#' @import utils
#' @import WikidataQueryServiceR
#' @importFrom vctrs vec_cbind vec_rbind
#' @importFrom httr POST
#' @importFrom httr user_agent
#' @importFrom jsonlite fromJSON
#' @importFrom readr format_delim
#' @importFrom readr format_tsv
#' @importFrom stats cor
#' @importFrom stats var
#' @importFrom WikipediR page_content random_page query
## usethis namespace: end
NULL
