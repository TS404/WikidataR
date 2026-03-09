# -------- Gets --------

#'@title Retrieve specific Wikidata items or properties
#'@description \code{get_item} and \code{get_property} allow you to retrieve the data associated
#'with individual Wikidata items and properties, respectively. As with
#'other \code{WikidataR} code, custom print methods are available; use \code{\link{str}}
#'to manipulate and see the underlying structure of the data.
#'
#'@param id the ID number(s) of the item or property you're looking for. This can be in
#'various formats; either a numeric value ("200"), the full name ("Q200") or
#'even with an included namespace ("Property:P10") - the function will format
#'it appropriately. This function is vectorized and will happily accept
#'multiple IDs.
#'
#'@inheritDotParams httr::GET
#'
#'@seealso \code{\link{get_random}} for selecting a random item or property,
#'or \code{\link{find_item}} for using search functionality to pull out
#'item or property IDs where the descriptions or aliases match a particular
#'search term.
#'
#'@examples
#'
#'#Retrieve a specific item
#'adams_metadata <- get_item("42")
#'
#'#Retrieve a specific property
#'object_is_child <- get_property("P40")
#'
#'@aliases get_item get_property
#'@rdname get_item
#'@export
get_item <- function(id, ...){
  id <- check_input(id, "Q")
  output <- (lapply(id, wd_query, ...))
  class(output) <- "wikidata"
  return(output)
}

#'@rdname get_item
#'@export
get_property <- function(id, ...){
  has_grep <- grepl("^P(?!r)",id, perl = TRUE)
  id[has_grep] <- paste0("Property:", id[has_grep])
  id <- check_input(id, "Property:P")
  
  output <- (lapply(id, wd_query, ...))
  class(output) <- "wikidata"
  return(output)
}

#'@title Retrieve randomly-selected Wikidata items or properties
#'@description \code{get_random_item} and \code{get_random_property} allow you to retrieve the data
#'associated with randomly-selected Wikidata items and properties, respectively. As with
#'other \code{WikidataR} code, custom print methods are available; use \code{\link{str}}
#'to manipulate and see the underlying structure of the data.
#'
#'@param limit how many random items to return. 1 by default, but can be higher.
#'
#'@inheritDotParams httr::GET
#'
#'@seealso \code{\link{get_item}} for selecting a specific item or property,
#'or \code{\link{find_item}} for using search functionality to pull out
#'item or property IDs where the descriptions or aliases match a particular
#'search term.
#'
#'@examples
#'\dontrun{
#'#Random item
#'random_item <- get_random_item()
#'
#'#Random property
#'random_property <- get_random_property()
#'}
#'@aliases get_random get_random_item get_random_property
#'@rdname get_random
#'@export
get_random_item <- function(limit = 1, ...){
  return(wd_rand_query(ns = 0, limit = limit, ...))
}

#'@rdname get_random
#'@export
get_random_property <- function(limit = 1, ...){
  return(wd_rand_query(ns = 120, limit = limit, ...))
}


#' @title Get an example SPARQL query from Wikidata
#' @description Gets the specified example(s) from
#'   [SPARQL query service examples page](https://www.wikidata.org/wiki/Wikidata:SPARQL_query_service/queries/examples)
#'   using [Wikidata's MediaWiki API](https://www.wikidata.org/w/api.php).
#' @details If you are planning on extracting multiple examples, please provide
#'   all the names as a single vector for efficiency.
#' @param example_name the names of the examples as they appear on
#'   [this page](https://www.wikidata.org/wiki/Wikidata:SPARQL_query_service/queries/examples)
#' @return The SPARQL query as a character vector.
#' @examples
#' \dontrun{
#' sparql_query <- extract_example(c("Cats", "Horses"))
#' query_wikidata(sparql_query)
#' # returns a named list with two data frames
#' # one called "Cats" and one called "Horses"
#' sparql_query <- extract_example("Largest cities with female mayor")
#' cat(sparql_query)
#' query_wikidata(sparql_query)
#' }
#' @seealso [query_wikidata]
#' @export
get_example <- function(example_name){
  content <- WikipediR::page_content(
    domain = "www.wikidata.org",
    page_name = "Wikidata:SPARQL query service/queries/examples",
    as_wikitext = TRUE
  )
  wiki <- strsplit(content$parse$wikitext$`*`, "\n")[[1]]
  wiki <- wiki[wiki != ""]
  return(vapply(example_name, function(example_name){
    heading_line <- which(grepl(paste0("^===\\s?", example_name, "\\s?===$"), wiki, fixed = FALSE))
    start_line <- which(grepl("{{SPARQL", wiki[(heading_line + 1):length(wiki)], fixed = TRUE))[1]
    end_line <- which(grepl("}}", wiki[(heading_line + start_line + 1):length(wiki)], fixed = TRUE))[1]
    query <- paste0(wiki[(heading_line + start_line):(heading_line + start_line + end_line - 1)], collapse = "\n")
    return(sub("^\\s*\\{\\{SPARQL2?\\n?\\|query\\=", "", query))
  }, ""))
}


# -------- Finds --------

#'@title Search for Wikidata items or properties that match a search term
#'@description \code{find_item} and \code{find_property} allow you to retrieve a set
#'of Wikidata items or properties where the aliases or descriptions match a particular
#'search term.  As with other \code{WikidataR} code, custom print methods are available;
#'use \code{\link{str}} to manipulate and see the underlying structure of the data.
#'
#'@param search_term A term to search for.
#'
#'@inheritParams searcher
#'@inheritDotParams httr::GET
#'
#'@seealso \code{\link{get_random}} for selecting a random item or property,
#'or \code{\link{get_item}} for selecting a specific item or property.
#'
#'@examples
#'
#'#Check for entries relating to Douglas Adams in some way
#'adams_items <- find_item("Douglas Adams")
#'
#'#Check for properties involving the peerage
#'peerage_props <- find_property("peerage")
#'
#'@return A list containing the result of the query.
#'@rdname find_item
#'@export
find_item <- function(search_term, 
                      language = "en", 
                      limit = 10, 
                      response_language = "en", 
                      ...){
  res <- searcher(search_term, language, limit, response_language, "item")
  class(res) <- "find_item"
  return(res)
}

#'@rdname find_item
#'@export
find_property <- function(search_term, 
                          language = "en", 
                          response_language = "en", 
                          limit = 10){
  res <- searcher(search_term, language, limit, response_language, "property")
  class(res) <- "find_property"
  return(res)
}

#Generic, direct access to Wikidata's search functionality.
#'@title Convert an input to a item QID
#'@description Convert an input string to the most likely item QID
#'@param search_term a term to search for.
#'@param language the language to conduct the search in; this should
#'consist of an ISO language code. Set to "en" by default.
#'@param response_language the language to return the labels and descriptions in; this should
#'consist of an ISO language code. Set to "en" by default.
#'@param limit the number of results to return; set to 10 by default.
#'@param type type of wikidata object to return (default = "item")
#'@param ... Additional parameters to supply to [httr::POST()]
#'@return If the inputted string matches an item label, return its QID.
#'If the inputted string matches multiple labels of multiple items, return the QID of the first hit.
#'If the inputted string is already a QID, return the string.
#'@examples
#'# if input string is a valid QID
#'as_qid("Q42")
#'# if input string matches multiple item labels
#'as_qid("Douglas Adams")
#'# if input string matches a single unique label
#'as_qid("Douglas Adams and the question of arterial blood pressure in mammals")
#'@export
searcher <- function(search_term, language, limit, response_language, type, ...){
  result <- WikipediR::query(url = "https://www.wikidata.org/w/api.php", out_class = "list", clean_response = FALSE,
                             query_param = list(
                               action   = "wbsearchentities", 
                               type     = type,
                               language = language,
                               uselang = response_language,
                               limit    = limit,
                               search   = search_term
                             ),
                             ...)
  result <- result$search
  return(result)
}
