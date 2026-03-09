#Generic queryin' function for direct Wikidata calls. Wraps around WikipediR::page_content. - Ironholds
#'@title Download a Wikidata item
#'@description Utility wrapper for Wikidata API to download item.
#'Used by \code{get_item} and \code{get_property}.
#'@param title The Wikidata item or property as a string.
#'@inheritDotParams httr::POST
#'@return A downloaded full wikidata object (item or property) formatted as a 
#'nested json list.
#'@export
wd_query <- function(title, ...){
  result <- WikipediR::page_content(domain = "wikidata.org", 
                                    page_name = title, 
                                    as_wikitext = TRUE,
                                    httr::user_agent("WikidataR - https://github.com/TS404/WikidataR"),
                                    ...)
  output <- jsonlite::fromJSON(result$parse$wikitext[[1]])
  return(output)
}

# Query for a random item in "namespace" (ns). Essentially a wrapper around WikipediR::random_page. - Ironholds
#'@title Download random Wikidata items
#'@description Utility wrapper for Wikidata API to download random items. 
#'Used by \code{random_item}.
#'@param ns string indicating namespace, most commonly "Main" for QID items, "Property" 
#'for PID properties.
#'@param limit How many random object to return.
#'@inheritDotParams httr::POST
#'@return Downloaded full wikidata objects (items or properties) formatted 
#'as nested json lists.
#'@export
wd_rand_query <- function(ns, limit, ...){
  result <- WikipediR::random_page(domain = "wikidata.org", 
                                   as_wikitext = TRUE, 
                                   namespaces = ns,
                                   httr::user_agent("WikidataR - https://github.com/TS404/WikidataR"),
                                   limit = limit, ...)
  output <- lapply(result, function(x){jsonlite::fromJSON(x$wikitext[[1]])})
  class(output) <- "wikidata"
  return(output)
}

#SPARQL query function for direct Wikidata calls.
#'@title Download full Wikidata items matching a SPARQL query 
#'@description Utility wrapper for wikidata spargl endpoint to download items.
#'Used by \code{get_geo_entity} and \code{get_geo_box}.
#'@param query The SPARQL query as a string
#'@inheritDotParams httr::POST
#'@return a download of the full wikidata objects formatted as a nested json list
#'@export
sparql_query <- function(query, ...){
  result <- httr::GET("https://query.wikidata.org/bigdata/namespace/wdq/sparql",
                      query = list(query = query),
                      httr::user_agent("WikidataR - https://github.com/TS404/WikidataR"),
                      ...)
  httr::stop_for_status(result)
  return(httr::content(result, as = "parsed", type = "application/json"))
}

#Wrapper around WikidataQueryServiceR::query_wikidata
#' @title Send one or more SPARQL queries to WDQS
#' @description Makes a POST request to Wikidata Query Service SPARQL endpoint.
#' @param sparql_query SPARQL query (can be a vector of queries)
#' @param format
#'   `tibble` (default) returns a pure character data frame,
#'   `simple` returns a pure character vector, while
#'   `smart` fetches JSON-formatted data and returns a tibble with datetime
#'   columns converted to `POSIXct`.
#'@inheritDotParams httr::POST
#' @return A `tibble` or `vector`. Note: QID values will be returned as QIDs, rather than URLs.
#' @section Query limits:
#' There is a hard query deadline configured which is set to 60 seconds. There
#' are also following limits:
#' - One client (user agent + IP) is allowed 60 seconds of processing time each
#'   60 seconds
#' - One client is allowed 30 error queries per minute
#' See \href{https://www.mediawiki.org/wiki/Wikidata_Query_Service/User_Manual#Query_limits}{query limits section}
#' in the Wikidata Query Service User Manual for more information.
#' @examples
#' # R's versions and release dates:
#' sparql_query <- 'SELECT DISTINCT
#'   ?softwareVersion ?publicationDate
#'   WHERE {
#'     BIND(wd:Q206904 AS ?R)
#'     ?R p:P348 [
#'       ps:P348 ?softwareVersion;
#'       pq:P577 ?publicationDate
#'     ] .
#' }'
#' query_wikidata(sparql_query)
#'
#' \dontrun{
#' # "smart" format converts all datetime columns to POSIXct
#' query_wikidata(sparql_query, format = "smart")
#' }
#' @export
query_wikidata <- function(sparql_query,format="tibble",...) {
  if(format=="simple"){simplify<-TRUE}else{simplify<-FALSE}
  if(format=="tibble"){format<-"simple"}
  output <- WikidataQueryServiceR::query_wikidata(sparql_query=sparql_query,
                                                  format=format, ...)
  output <- suppressWarnings(mapply(url_to_id,
                                    data.frame(output),
                                    SIMPLIFY=simplify))
  output <- tibble(data.frame(output))
  if(nrow(output)==0){output<-tibble(value=NA)}
  output
}

#' @title QID from identifier
#' @description Convert unique identifiers to QIDs (for items in Wikidata). 
#' @details The \href{https://www.wikidata.org/wiki/Q43649390}{Wikidata Q identifier} (QID) is the unique identifier (UID) 
#' used in Wikidata.
#' @param property The identifier property to search (for caveats, see \code{as_pid}.)
#' @param value The identifier value to match.
#' @return A vector of QIDs corresponding to identifiers submitted.
#' @examples
#' qid_from_identifier('ISBN-13','978-0-262-53817-6')
#' @export
qid_from_identifier <- function(property = 'DOI',
                                value    = c('10.15347/WJM/2019.001','10.15347/WJM/2020.002')){
  
  property <- as_pid(property)
  
  qid_from_property1 <- function(value,property){
    out <- paste('SELECT ?value WHERE {?value wdt:',
                 property,
                 ' "',
                 value,
                 '"}',
                 sep='')
    names(out)<-value
    return(out)
    }
  
  qid_from_property2 <- function(x){
    out <- as.character(query_wikidata(x)[[1]])
    names(out) <- names(x)
    return(out)
    }
  
  sparql_query <- lapply(value,property,FUN=qid_from_property1)
  
  if(length(value)>1){
    output <- unlist(pblapply(sparql_query,qid_from_property2))
  } else {
    output <- as.character(unlist(lapply(sparql_query,FUN=query_wikidata)))
    names(output) <- value
  }
  
  if(length(value)!=length(output)){
    message("Caution! Some supplied values returned more than one QID.")
    }
  
  return(output)
}

#' @title Identifier from identifier
#' @description Convert unique identifiers to other unique identifiers.
#' @param property The identifier property to search (for caveats, see \code{as_pid})
#' @param return The identifier property to convert to
#' @param value The identifier value to match.
#' @return A vector of identifiers corresponding to identifiers submitted.
#' @examples
#' \donttest{
#' identifier_from_identifier(property ='ORCID iD',
#'                            return = 'IMDb ID',
#'                            value = c('0000-0002-7865-7235','0000-0003-1079-5604')
#'                            )
#' }
#' @export
identifier_from_identifier <- function(property = 'ORCID iD',
                                       return   = 'IMDb ID',
                                       value    = "0000-0002-7865-7235"){
  
  property <- as_pid(property)
  return   <- as_pid(return)
  
  qid_from_property1 <- function(value,return,property){paste('SELECT ?return WHERE { ?value wdt:',
                                                       property,
                                                       ' "',
                                                       value,
                                                       '". ?value wdt:',
                                                       return,
                                                       ' ?return.}',
                                                       sep='')}
  sparql_query <- lapply(value,return,property,FUN=qid_from_property1)
  output       <- if(length(value)>1){
    unlist(pbapply::pblapply(sparql_query,function(x) as.character(query_wikidata(x)[[1]])))
  }else{
    as.character(unlist(lapply(sparql_query,FUN=query_wikidata)))
  }
  names(output) <- value
  return(output)
}
