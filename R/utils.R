# -------- Format checkers --------
# Simple tests of strings for whether they adhere to common wikidata formats
is.qid     <- function(x){grepl("^[Qq][0-9]+$",x)}
is.pid     <- function(x){gsub("S","P",x) %in% as.matrix(WD.globalvar$PID.datatype$property)}
is.sid     <- function(x){gsub("S","P",x) %in% as.matrix(WD.globalvar$SID.valid$Wikidata_property_to_indicate_a_source)}
is.date    <- function(x){grepl("[0-9]{1,4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}",x)}
is.quot    <- function(x){grepl("^\".+\"$",x)}
is.empty   <- function(x){x==""}
is.coord   <- function(x){grepl("@-?([1-8]?\\d(\\.\\d+)?|90(\\.0+)?)/-?(180(\\.0+)?|((1[0-7]\\d)|([1-9]?\\d))(\\.\\d+)?)$",x)}
is.wdURL   <- function(x){grepl("http://www.wikidata.org/entity/[PpQq][0-9]+$",x)}
is.create  <- function(x){grepl("^CREATE",x)}
is.createx <- function(x){grepl("^CREATE.+",x)}
is.create_x<- function(x){grepl("^CREATE_.+",x)}
is.last    <- function(x){grepl("^LAST$",x)}
is.special <- function(x){
  if(grepl("^[LAD]",x)){
    substr(x,2,100) %in% as.matrix(WD.globalvar$lang.abbrev)
  }else if(grepl("^S",x)){
    substr(x,2,100) %in% as.matrix(WD.globalvar$abbrev.wiki)
  }else{
    FALSE
  }
}

check.PID.WikibaseItem <- function(x){
  x %in% WD.globalvar$PID.datatype$property[WD.globalvar$PID.datatype$wbtype=="WikibaseItem"]}

check.PID.constraint <- function(x){
  check.PID.constraint.nest1 <- function(x){
    out <- as.character(WD.globalvar$PID.constraint$fmt[WD.globalvar$PID.constraint$Wikidata_property==x])
    if(length(out)!=0){out}else{NA}
    }
  sapply(x,check.PID.constraint.nest1)
}

#'@title Extract an identifier from a wikidata URL
#'@description Convert a URL ending in an identifier (returned by SPARQL queries) to just
#'the plain identifier (QID or PID).
#'@param x a strings representing a wikidata URL
#'@return if the URL ends in a QID or PID, return that PID or QID, else return the original string
#'@examples
#'url_to_id("http://www.wikidata.org/entity/42")
#'@export
url_to_id <- function (x){
  if(is.wdURL(x)){x <- sapply(sapply(x,pattern = "/",stringr::str_split),tail,1)}
  output <- x
  output
}

#Generic input checker. Needs additional stuff for property-based querying
#because namespaces are weird, yo. - Ironholds
#'@title Generic input checker
#'@description Utility function to handle namespaces. Used by \code{get_item} and \code{get_property}
#'@param input string to check
#'@param substitution string for what's been looked for
#'@return boolian indicating whether the checked string contains a match for the substitution string 
#'@export
check_input <- function(input, substitution){
  in_fit <- grepl("^\\d+$",input)
  if(any(in_fit)){
    input[in_fit] <- paste0(substitution, input[in_fit])
  }
  return(input)
}


# -------- Format converters --------
# Simple functions to convert plain text descriptions into their most likely QID/PIDs
#'@title Convert an input to a item QID
#'@description Convert an input string to the most likely item 
#'\href{https://www.wikidata.org/wiki/Q43649390}{QID}.
#'@param x a vector, data frame, or tibble of strings representing wikidata items
#'@return if the inputted string is a valid QID, return the string.
#'If the inputted string matches an item label, return its QID.
#'If the inputted string matches multiple labels of multiple items, return the QID of the first hit.
#'@examples
#'# if input string is a valid QID
#'as_qid("Q42")
#'# if input string matches multiple item labels
#'as_qid("Douglas Adams")
#'# if input string matches a single unique label
#'as_qid("Douglas Adams and the question of arterial blood pressure in mammals")
#'@export
as_qid <- function(x){
  as_qid_nest1 <- function(x){
    as_qid_nest2 <- function(x){
      if(is.qid(x)|is.date(x)|is.quot(x)|is.na(x)|is.null(x)|is.empty(x)|is.createx(x)|is.create(x)|is.last(x)){
        x
      }else{
        temp <- find_item(x,limit = 100)
        if(length(temp)==0){
          out <- NA
          message (paste0("no sufficiently close match for \"",x,"\". Returned \"NA\"."))
        }else{
          toinclude    <- sapply(temp,function(temp,x){temp$label==x},x)
          toinclude[1] <- TRUE
          temp         <- temp[toinclude]
          out          <- temp[[1]]$id
          names(out)   <- temp[[1]]$label
          if(x!=temp[[1]]$label){message(paste0(
            "Inexact match for \"",x,
            "\", closest match = ",temp[[1]]$label,
            " (",out,") "))}
          if(length(temp)>1){
            message(paste0(
            "Multiple exact matches for \"",x,"\""))
            message(paste0(
              "  match ",1:length(temp),
              " = (",sapply(temp,function(temp){temp$id}),
              ") ",sapply(temp,function(temp){temp$description}),
              "\n"))}
          }
        out
      }
    }
    out <- unlist(lapply(x,as_qid_nest2))
    out
  }
  output <- vec_cbind(lapply(tibble(x),as_qid_nest1))
  return(output)
}

#'@title Convert an input to a property PID
#'@description Convert an input string to the most likely property PID
#'@param x a vector, data frame, or tibble of strings representing Wikidata properties
#'@return If the inputted string is a valid PID, return the string.
#'If the inputted string matches a property label, return its PID.
#'If the inputted string matches multiple labels of multiple properties, return the PID of the first hit.
#'@examples
#'# if input string is a valid PID
#'as_pid("P50")
#'# if input string matches multiple item labels
#'as_pid("author")
#'# if input string matches a single unique label
#'as_pid("Scopus author ID")
#'@export
as_pid <- function(x){
  as_pid_nest1 <- function(x){
    as_pid_nest2 <- function(x){
      if(is.pid(x)|is.date(x)|is.quot(x)|is.na(x)|is.null(x)|is.empty(x)|is.special(x)){
        x
      }else{
        temp <- find_property(x,limit = 2)
        if(length(temp)==0){
            out <- NA
            message (paste0("no sufficiently close match for \"",x,"\". Returned \"NA\"."))
          }else{
            out        <- temp[[1]]$id
            names(out) <- temp[[1]]$label
            if(x!=temp[[1]]$label){message(paste0(
              "Inexact match for \"",x,
              "\", closest match = ",temp[[1]]$label,
              " (",out,")."))}
            }
        out
      }
    }
    out <- unlist(lapply(x,as_pid_nest2))
    out
  }
  output <- vec_cbind(lapply(tibble(x),as_pid_nest1))
  return(output)
}

#'@title Convert an input to a source property SID
#'@description Convert an input string to the most likely source SID
#' (equivalent to PID.)
#'@param x a vector, data frame, or tibble of strings representing Wikidata 
#'source properties.
#'@return if the inputted string is a valid SID, return the string.
#'If the inputted string matches a property label, return its SID.
#'If the inputted string matches multiple labels of multiple properties, 
#'return the SID of the first hit.
#'@examples
#'# if input string is a valid SID
#'as_pid("S854")
#'# if input string matches multiple item labels
#'as_pid("URL")
#'# if input string matches a single unique label
#'as_pid("Reference URL")
#'@export
as_sid <- function(x){
  as_sid_nest1 <- function(x){
    as_sid_nest2 <- function(x){
      if(is.sid(x)|is.date(x)|is.quot(x)|is.na(x)|is.null(x)|is.empty(x)){
        x
      }else if(all(is.pid(x))){
        gsub("P","S",x,ignore.case = 1)
      }else{
        gsub("P","S",find_property(x)[[1]]$id)
      }
    }
    out <- unlist(lapply(x,as_sid_nest2))
    out
  }
  output <- vec_cbind(lapply(tibble(x),as_sid_nest1))
  return(output)
}

#'@title Add quotations marks
#'@description Add escaped quotation marks around strings that need them ready for 
#'submission to an API.
#'@param x a vector, data frame, or tibble of strings
#'@param format either "tibble" / "csv" to use plain quotation marks (default), 
#'or "api" / "website" to use `'\%22'`
#'@return A tibble of items inside of escaped quotation marks
#'unless they are already in escaped quotation marks, is a 
#'\href{https://www.wikidata.org/wiki/Q43649390}{QID}, 
#'in which chase it is returned unchanged. 
#'@examples
#'as_quot("text")
#'@export
as_quot <- function(x,format="tibble"){
  if(is.null(x)){
    return(NULL)
  }else if(format=="api"|format=="website"){
    q_mark <- '%22'
  }else if(format=="tibble"|format=="csv"){
    q_mark <- '"'
  }
  as_quot_nest1 <- function(x){
    as_quot_nest2 <- function(x){
      if(!(is.qid(x)|is.quot(x)|is.date(x)|is.na(x)|is.empty(x)|is.numeric(x)))
      {paste0(q_mark,as.character(x),q_mark)}
      else
      {as.character(x)}
    }
    out <- unlist(lapply(x,as_quot_nest2))
    out
  }
  output <- vec_cbind(lapply(tibble(x),as_quot_nest1))
  return(output)
}

#'@title Extract an identifier from a Wikidata URL
#'@description Convert a URL ending in an identifier (returned by SPARQL queries)
#'to just the plan identifier (QID or PID).
#'@details The \href{https://www.wikidata.org/wiki/Q43649390}{Wikidata Q identifier} (QID) 
#'is the unique identifier (UID) used in Wikidata.
#'@param x A vector of strings representing Wikidata URLs.
#'@return QID or PID.
#'@examples
#'url_to_id("http://www.wikidata.org/Q42")
#'@export
url_to_id <- function(x){
  sapply(sapply(x,pattern = "/|:",stringr::str_split),tail,1)
}


# -------- Wikidata object manipulation --------
#'@title Extract claims from returned item data
#'@description extract claim information from data returned using
#'\code{\link{get_item}}.
#'@param items a list of one or more Wikidata items returned with
#'\code{\link{get_item}}.
#'@param claims a vector of claims (in the form "P321", "P12") to look for
#'and extract.
#'@return a list containing one sub-list for each entry in \code{items},
#'and (below that) the found data for each claim. In the event a claim
#'cannot be found for an item, an \code{NA} will be returned
#'instead.
#'@examples
#'# Get item data
#'adams_data <- get_item("42")
#'# Get claim data
#'claims <- extract_claims(adams_data, "P31")
#'@export
extract_claims <- function (items,
                            claims){
  claims <- sapply(claims,as_pid)
  output <- lapply(items, function(x, claims){
    return(lapply(claims, function(claim, obj){
      which_match <- which(names(obj$claims) == claim)
      if (!length(which_match)){
        return(NA)
      }
      return(obj$claims[[which_match[1]]])
    }, obj = x))
  }, claims = claims)
  return(output)
}

#'@title List properties of a Wikidata item
#'@description for a downloaded wikidata item, list the properties of all statements
#'@param item a list of one or more Wikidata items returned with
#'\code{\link{get_item}}.
#'@param names a boolian for whether to return property names, or just P numbers
#'and extract.
#'@return a list containing one sub-list for each entry in \code{items},
#'and (below that) the found data for each claim. In the event a claim
#'cannot be found for an item, an \code{NA} will be returned
#'instead.
#'@examples
#'# Get item data
#'adams_data <- get_item("42")
#'# Get claim data
#'claims <- extract_claims(adams_data, "P31")
#'@export
list_properties <- function (item,
                             names=FALSE){
  properties.p <- lapply(lapply(item,"[[","claims"),names)
  if(names){
    if(length(item)==1){
      names(properties.p) <- unlist(lapply(lapply(lapply(get_property(properties.p),"[[","labels"),"[[","en"),"[[","value"))
    }
  }
  return(properties.p)
}

#Note: This one isn't very well named. not really the property names, more the predicate names, but you get the idea
#'@title Get names of properties
#'@description For a claim or set of claims, return the names of the properties  
#'@param properties a claims list from \code{extract_claims}
#'@return tibble of labels for each property for a set of claims
#'@export
get_names_from_properties <- function(properties){
  get_names_from_properties_nest1 <- function(x){
    out <- lapply(lapply(lapply(lapply(x,"[[","mainsnak"),"[[","datavalue"),"[[","value"),"[[","id")
    names(out) <- lapply(lapply(lapply(x,"[[","mainsnak"),"[[","property"),"[[",1)
    return(out)
  }
  get_names_from_properties_nest2 <- function(x){
    out <- lapply(x,get_item)
    return(out)
  }
  get_names_from_properties_nest3.1 <- function(x){
    out <- lapply(lapply(lapply(x,"[[","labels"),"[[","en"),"[[","value")
    names(out) <- lapply(x,"[[","id")
    return(out)
  }
  get_names_from_properties_nest3 <- function(x){
    out <- lapply(x,get_names_from_properties_nest3.1)
    return(out)
  }
  
  property_values.qid <- lapply(properties,get_names_from_properties_nest1)
  property_values.q   <- lapply(property_values.qid,get_names_from_properties_nest2)
  property_names      <- lapply(property_values.q, get_names_from_properties_nest3)
  property_names      <- lapply(lapply(property_names,unlist),enframe,name = "QID") 
  return(property_names)
}


#'@title Filter QIDs
#'@description For a QID or vector of QIDs, remove ones that match a particular statement
#'(e.g. remove all that are instances of academic publications or books).
#'@details The \href{https://www.wikidata.org/wiki/Q43649390}{Wikidata Q identifier} (QID) 
#'is the unique identifier (UID) used in Wikidata.
#'@param ids QIDs to check
#'@param property property to check (default = P31 to filter on "instance of")
#'@param filter values of that property to use to filter out
#'(default = Q737498, Q5633421, Q7725634, Q13442814, and Q18918145 to remove academic
#'publications or books)
#'@param message message to return (useful for disambiguate_QIDs function)
#'@return a vector of QIDs that do not match the property filter
#'@examples 
#' \dontrun{
#' # Filter three items called "Earth Science" to show only those that aren't
#' # books, journals or journal articles
#' filter_qids(ids = c("Q96695546","Q8008","Q58966429"),
#'             property = "P31",
#'             filter = c("Q737498","Q5633421","Q7725634","Q13442814","Q18918145"))
#' }
#'@export
filter_qids <- function (ids,
                         property = "P31",
                         filter = c("Q737498",
                                    "Q5633421",
                                    "Q7725634",
                                    "Q13442814",
                                    "Q18918145"),
                         message=NULL){
  out <- NULL
  pb <- progress_bar$new(total  = length(ids),
                         format = paste0(message,":bar :percent eta::eta"),
                         width  = 75,
                         show_after = 0)
  if(is.null(property)|is.null(filter)){
    for (i in 1:length(ids)){
      pb$tick()
      qid   <- ids[i]
      item  <- find_item(qid,limit=1)
      label <- item[[1]]$label
      if(length(item[[1]]$description)>0){
        if(!is.null(item[[1]]$description)){
          desc <- item[[1]]$description
        }else{
          desc <- item[[1]]$description
        }
      }else{
        desc <- "no description"
      }
      out <- vec_rbind(out,tibble(qid=qid,label=label,desc=desc))
    }
  }else{
    for (i in 1:length(ids)){
      pb$tick()
      qid  <- ids[i]
      item <- get_item(qid)
      P31  <- item[[1]]$claims[[property]]$mainsnak$datavalue$value$id
      if(all(is.null(P31))){P31<-"other"}
      if(!any(P31 %in% filter)){
        label <- item[[1]]$labels[[1]]$value
        if(length(item[[1]]$descriptions)>0){
          if(!is.null(item[[1]]$descriptions$en$value)){
            desc <- item[[1]]$descriptions$en$value
          }else{
            desc <- item[[1]]$descriptions[[1]]$value
          }
        }else{
          desc <- "no description"
        }
        if(length(item[[1]]$labels)>0){
          if(!is.null(item[[1]]$labels$en$value)){
            label <- item[[1]]$labels$en$value
          }else{
            label <- item[[1]]$labels[[1]]$value
          }
        }else{
          label <- "no label"
        }
        out <- vec_rbind(out,tibble(qid=qid,label=label,desc=desc))
      }
    }
  }
  if(is.null(out)){
    out <- tibble(qid=NA,
                  label=NA,
                  desc="No current matching Wikidata item")
  }
  return(out)
}


# -------- Misc. string manipulation --------
#'@title Format short form person names
#'@description Converting names into first initial and surname, or just initials
#'@param x a vector of people's names as strings
#'@param format a vector of strings of either "FLast" or "FL" to indicate the output format
#'@return the inputted name strings with first names shortened based on the
#'selected format.
#'@export
initials <- function(x,format="FLast"){
  if (format=="FLast"){
    gsub("^([A-Za-z]).* ([A-Za-z]*)", "\\1 \\2", x)
  }else{
    gsub("(.)\\S* *", "\\1", x)
  }
}

#'@title Remove special characters
#'@description Special characters can otherwise mess up wikidata read-writes
#'@param x a vector of strings to check for special characters
#'@return the inputted strings with special characters replaced with
#'closest match plan characters.
#'@export
unspecial <- function(x){
  out <- x
  for(i in 1:ncol(x)){
    out[[i]] <- iconv(x[[i]],to = 'ASCII//TRANSLIT')
    if(all_numeric(x[[i]])){
      out[[i]] <- as.numeric(out[[i]])
    }else{
      out[[i]] <- as.factor(out[[i]])
    } 
  }
  return(as_tibble(out))
}

#'@title Extract a paragraph of text
#'@description Return the nth paragraph of a section of text
#'Useful for extracting information from Wikipedia or other wikimarkup text
#'@param text the input text as a string
#'@param para number indicating which paragraph(s) to return (default=1)
#'@param templ an optional string specifying a mediawikitemplate within
#'which to restrict the search restrict search 
#'@return the nth paragraph of the input text.
#'@export
extract_para <- function(text,
                         para=1,
                         templ=NULL){
  extract_para_nest1 <- function(x,y){
    out <- lapply(x,gsub,pattern=".*= *| *\\|",replacement="")
    names(out) <- y
    return(out)
  }
  templ <- gsub(" ","_",templ)
  tosearch <- gsub("( |\\\\n|\\\\t)+"," ",text)
  if(!is.null(templ)){
    templates <- regmatches(tosearch, gregexpr("\\{(?:[^{}]+|(?R))*+\\}",
                                               tosearch, perl=TRUE, ignore.case=TRUE))[[1]]
    name_lens <- regexpr(" *\\|| *\\}",templates) - 1 
    templates <- paste0(gsub(" ","_",substr(templates,1,regexpr(" *\\|| *\\}",templates)-1)),
                        substr(templates,regexpr("*\\||*\\}",templates),nchar(templates)))
    
    tosearch  <- unlist(str_extract_all(templates,
                                       paste0("(?i)\\{\\{ *?",templ,".*?\\}\\}")))
    names(tosearch) <- paste0(templ,"_",1:length(tosearch))
  }
  
  match_paras <- lapply(tosearch,
                        str_extract_all,
                        paste0("\\| *?",para," *?=.*?\\|"))
  
  match_exact <- lapply(match_paras,extract_para_nest1,para)
  
  return(match_exact)
}

#'@title "CREATE" rows 
#'@description Add in empty lines for QuickStatements CREATE rows that mint new QIDs.
#'This is a slightly messy quirk of the QuickStatements format that mints new QIDs via a line
#'containing only "CREATE", so this function is a way to approximate that behavior in a tibble
#'@param items a vector, data frame or tibble of items (which may or may not contain the keyword "CREATE")
#'@param vector a vector of properties or values which may be expanded based on the items vector
#'@return if the vector is NULL, return NULL. Otherwise, if the "CREATE" keyword appears in the
#'items vector, insert blank strings at those positions in the vector.
#'@export
createrows <- function(items,vector){
  if(is.null(vector)){
    return(NULL)
  }
  if(any(items=="CREATE",na.rm = 1)){
    #expand vector to full length if just intending to repeat a single value 
    if(length(unlist(vector))==1){
      vector <- rep(vector,sum(items!="CREATE"))
    }
    vector <- tibble(vector)
    
    newQID <- which(items=="CREATE")
    val    <- vec_rbind(vector,tibble(data.frame(array("",dim=c(length(newQID),ncol(vector)),dimnames = list(NULL,colnames(vector))))))
    id     <- c(1:nrow(vector), newQID-seq_along(newQID)+0.5)
    out    <- tibble(val[order(id),])
    return(out)
  }else{
    return(tibble(vector))
  }
}

#'@title "CREATE" rows from tidy format
#'@description Add in QuickStatements CREATE rows that mint new QIDs from tidy input data.
#'New items are created by any item starting that starts with the text "CREATE" followed
#'by any unique ID.
#'@param QS.tib a tibble of items, values and properties (optionally qualifiers and sources).
#'@return a tibble, with items that start with "CREATE" followed by any unique text causing the
#'addition of a "Create" line above, being replaced with "LAST" in the QuickStatements 
#'format to create new QIDs.
#'@export
createrows.tidy <- function(QS.tib){
  #insert 'CREATE' blankrows above first instance of 'CREATExyz'
  newQID <- which(!duplicated(QS.tib[,1])&sapply(QS.tib[,1],is.createx))
  val    <- rbind(QS.tib, array("",dim=c(length(newQID),ncol(QS.tib)),dimnames = list(newQID,names(QS.tib))) )
  id     <- c(seq_along(t(QS.tib)[1,]), newQID-0.5)
  out    <- val[order(id),]
  
  #replace 'CREATEXYZ' with 'LAST'
  out[sapply(out[,1],is.createx),1] <- "LAST"
  
  #replace new empty rows with 'CREATE' row
  out[apply(is.empty(out),all,MARGIN=1),1] <- "CREATE"
  return(out)
}


all_numeric <- function(x) {
  x <- sub("[[:space:]]+$", "", x)
  x <- sub("^[[:space:]]+", "", x)
  # remove empty 
  xs <- x[nzchar(x)]
  
  if (length(xs) == 0 || all(is.na(x))) {
    return(FALSE)
  }
  
  isnon <- suppressWarnings(!is.na(xs) & is.na(as.numeric(xs)))
  !any(isnon)
}
