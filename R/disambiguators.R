# -------- Disambiguator functions --------
#
#'@title Disambiguate QIDs
#'@description Interactive function that presents alternative possible QID matches for a list of text
#'strings and provides options for choosing between alternatives, rejecting all presented alternatives,
#'or creating new items. Useful in cases where a list of text strings may have either missing Wikidata items
#'or multiple alternative potential matches that need to be manually disambiguated. Can also used on
#'lists of lists (see examples). For long lists of items, the process can be stopped partway through and
#'the returned vector will indicate where the process was stopped. 
#'@param list a list or vector of text strings to find potential QID matches to.
#'            Can also be a list of lists (see examples)
#'@param variablename type of items in the list that are being disambiguated (used in messages)
#'@param variableinfo additional information about items that are being disambiguated (used in messages)
#'@param filter_property property to filter on (e.g. "P31" to filter on "instance of")
#'@param filter_variable values of that property to use to filter out (e.g. "Q571" to filter out books)
#'@param filter_firsthit apply filter to the first match presented or only if alternatives requested?
#'                       (default = FALSE, note: true is slower if filter not needed on most matches)
#'@param Q_min return only possible hits with QIDs above the provided value
#'@param auto_create if no match found, automatically assign "CREATE"
#'@param limit number of alternative possible Wikidata items to present if multiple potential matches
#'@return a vector of:
#' \describe{
#'   \item{QID}{Selected QID (for when an appropriate Wikidata match exists)}
#'   \item{CREATE}{Mark that a new Wikidata item should be created (for when no appropriate Wikidata match yet exists)}
#'   \item{NA}{Mark that no Wikidata item is needed}
#'   \item{STOP}{Mark that the process was halted at this point (so that output can be used as input to the function later)}
#' }
#'@examples
#'\dontrun{
#'#Disambiguating possible QID matches for these music genres
#'#Results should be:
#'# "Q22731" as the first match
#'# "Q147538" as the first match
#'# "Q3947" as the second alternative match
#'disambiguate_QIDs(list=c("Rock","Pop","House"),
#'                  variablename="music genre")
#'
#'#Disambiguating possible QID matches for these three words, but not the music genres
#'#This will take longer as the filtering step is slower
#'#Results should be:
#'# "Q22731" (the material) as the first match
#'# "Q147538" (the soft drink) as the second alternative match
#'# "Q3947" (the building) as the first match
#'disambiguate_QIDs(list=c("Rock","Pop","House"),
#'                  filter_property="instance of",
#'                  filter_variable="music genre",
#'                  filter_firsthit=TRUE,
#'                  variablename="concept, not the music genre")
#'
#'#Disambiguating possible QID matches for the multiple expertise of
#'#these three people as list of lists
#'disambiguate_QIDs(list=list(alice=list("physics","chemistry","maths"),
#'                            barry=list("history"),
#'                            clair=list("law","genetics","ethics")),
#'                  variablename="expertise")
#'}
#'@export
disambiguate_QIDs <- function(list,
                              variablename="variables",
                              variableinfo=NULL,
                              filter_property=NULL,
                              filter_variable=NULL,
                              filter_firsthit=FALSE,
                              Q_min=NULL,
                              auto_create=FALSE,
                              limit=10){
  #make list is formatted as a list (e.g. if vector)
  if(!all(class(list)=="list")){list <- as.list(list)}
  if(!is.null(filter_property)){filter_property <- as_pid(filter_property)[[1]][1]}
  if(!is.null(filter_variable)){filter_variable <- as_qid(filter_variable)[[1]][1]}
  
  #is the list the outut from a previous half-done run?
  if(any(unlist(lapply(list,function(x) x=="STOP")),na.rm = TRUE)){
    item_to_start_from    <- which(unlist(lapply(list,function(x) any(x=="STOP"))))
    subitem_to_start_from <- dplyr::first(which(list == "STOP"))
    output <- list
  }else{
    item_to_start_from    <- 1
    subitem_to_start_from <- 1
    output <- blank_output_list(list)
  }

  #create output
  pb_main <- progress_bar$new(total = sum(unlist(output,recursive = TRUE)==""|unlist(output,recursive = TRUE)=="STOP"),
                              format     = ":bar :percent eta::eta \n",
                              current    = "|",
                              width      = 90,
                              show_after = 0)
  for(item in item_to_start_from:length(list)){
    for(subitem in subitem_to_start_from:length(list[[item]])){
      #check item to search
      tosearch <- list[[item]][subitem]
      if(is.na(tosearch)){break}                                   #skip NAs
      if(tosearch=="STOP"|tosearch==""){tosearch<-names(tosearch)} #use name for items not done in previous run (stored as "STOP" and "")
      if(grepl("^[Qq][0-9]+$",tosearch)){break}                    #skip if already a QID
      if(is.null(tosearch)){break}                                 #skip nulls or empty items with no name (usually errors)
      list[[item]][subitem] <- tosearch                            #if no skips, place that text back in the list

      #announce choice to be made
      message_header(list,item,subitem,variablename,variableinfo)
      pb_main$tick()
      #execute search and record choice
      Q_min_hit=NULL
      if(!is.null(Q_min)){
        Q_min <- as.numeric(gsub("Q","",Q_min))
        Q_min_hit <- sapply(find_item(list[[item]][subitem],limit = limit),"[[",1)
        Q_min_hit <- Q_min_hit[as.numeric(gsub("Q","",Q_min_hit))>Q_min]
      }
      if(length(Q_min_hit)==1){
          choice <- Q_min_hit
      }else{
        if(filter_firsthit){
          first_hit_qid <- firsthit(list[[item]][subitem],filter_property,filter_variable)
        }else{
          first_hit_qid <- firsthit(list[[item]][subitem])
        }
        if(is.na(first_hit_qid) & auto_create){
          choice <- "CREATE"
        }else{
          choice <- makechoice(qid = first_hit_qid,
                               text= names(first_hit_qid),
                               filter_property=filter_property,
                               filter_variable=filter_variable,
                               limit=limit)
        }

      }
      output[[item]][[subitem]] <- choice
      names(output[[item]])[[subitem]] <- names(choice)

      #check if stop request made
      if(!is.na(output[[item]][[subitem]])){if(output[[item]][[subitem]]=="STOP"){
        done_so_far <- item
        message_stop(done_so_far,total = length(list))
        break
      }}
    }
    subitem_to_start_from <- 1 # reset the subitem to start from if completed a full item
    if(!is.na(output[[item]][[subitem]])){if(output[[item]][[subitem]]=="STOP"){break}}
  }
  return(output)
}

# When provided with a QID, interactively make a decision on whether the output should be that qid or some other value
makechoice <- function(qid=NULL,
                       text=NULL,
                       table=NULL,
                       filter_property=NULL,
                       filter_variable=NULL,
                       limit=10){
  if(is.null(text)){
    text <- names(qid)
  }

  # announce item for disambig
  suppressWarnings(invisible(selection <- readline()))
  if      (selection=="s"|selection=="stop"){                   #s = stop
    output <- "STOP"
    names(output) <- text

  }else if(selection=="y"|selection=="yes"){                    #y = accept
    output <- qid
    names(output) <- text

  }else if(selection=="n"|selection=="no"|selection==""){       #n = reject
    output <- NA
    names(output) <- text

  }else if(selection=="c"|selection=="create"){                 #c = create
    output <- "CREATE"
    names(output) <- text

  }else if(selection=="?"){                                     #? = loop up in browser
    browseURL(paste0("https://www.wikidata.org/wiki/",qid))
    output <- makechoice(qid,text,table,filter_property,filter_variable,limit)

  }else if(grepl("^[Qq][0-9]+$",selection)){                    #Q123 = id
    output <- selection
    names(output) <- paste0("-> ",selection)

  }else if(grepl("^[Qq][0-9]+?$",selection)){                   #Q123? = search that id
    browseURL(paste0("https://www.wikidata.org/wiki/",
                     gsub("\\?","",selection)))
    output <- makechoice(qid,text,table,filter_property,filter_variable,limit)

  }else if(grepl("^[0-9]+$",selection) & !is.null(table)){      #number = select row
    output <- table$qid[as.numeric(selection)]
    label  <- table$label[as.numeric(selection)]

  }else if(grepl("^[0-9]+\\?$",selection)& !is.null(table)){    #number? = loop up row in browser
    browseURL(paste0("https://www.wikidata.org/wiki/",
                     table$qid[as.numeric(gsub("\\?","",selection))]))
    output <- makechoice(qid,text,table,filter_property,filter_variable,limit)
    label  <- table$label[as.numeric(selection)]

  }else if((selection=="a"|selection=="alt") & !is.null(text)){ #a = alternative
    table  <- choices_alt(text,filter_property,filter_variable,limit)
    output <- makechoice(qid,text,table,filter_property,filter_variable,limit)
    if(!is.null(names(output)) & !is.null(text)){if(names(output)!=text){
      names(output) <- paste0(text," -> ",names(output))
    }}

  }else{                                                        #freetext = freetext to search
    table  <- choices_alt(selection,filter_property,filter_variable,limit)
    output <- makechoice(qid,selection,table,filter_property,filter_variable,limit)
    if(!is.null(names(output)) & !is.null(text)){if(names(output)!=text){
      names(output) <- paste0(text," -> ",names(output))
    }}
  }

  return(output)
}

# -------- Messages --------

# Clear console and show standard header for
message_header <- function(list,
                           i,
                           j,
                           variablename=NULL,
                           variableinfo=NULL){
  list         <- as.list(list)
  name         <- bold$cyan(names(list)[[i]])
  variables    <- list[[i]]
  variables[j] <- bold$white$underline(variables[j])
  variables    <- paste(variables,collapse = " | ")
  if(!is.null(variablename)){
    variablename <- paste0("the ",variablename," of ")
    if (length(variablename)>1){
      variablename <- variablename[i]
    }
  }else{
    variablename <- NULL
  }

  if(!is.null(variableinfo)){
    variableinfo <- paste0(variableinfo,"\n")
    if (length(variableinfo)>1){
      variableinfo <- variableinfo[i]
    }
  }else{
    variableinfo <- NULL
  }
  message("\014",
          "--------------------------------------------------------------------------- \n",
          "Let's disambiguate ",variablename,
          name, ": \n",
          variableinfo,
          variables)
}

message_choices <- function(){
  message(bold(" y    "),"-> accept the presented match \n",
          bold(" n    "),"-> reject the presented match and move on to the next \n",
          bold(" a    "),"-> request alternative possible matches \n",
          bold(" Q123 "),"-> use this as the wikidata QID \n",
          bold(" text "),"-> try this text as alternative search term \n",
          bold(" c    "),"-> create a new item for this later \n",
          bold(" s    "),"-> stop here, save those done so far and come back later \n",
          bold(" ?    "),"-> check the presented match in your browser")
}

message_choices_na <- function(){
  message(bold(" y/n  "),"-> leave as 'NA' \n",
          bold(" Q123 "),"-> use this as the wikidata QID \n",
          bold(" text "),"-> try this text as alternative search term \n",
          bold(" c    "),"-> create a new item for this later \n",
          bold(" s    "),"-> stop here, save those done so far and come back later")
}

message_choices_alt <- function(table){
  message("Are any of these appropriate?")
  print(data.frame(table),right=FALSE)
  message(bold(" number "),"-> select one of the matches presented (include ",bold("?")," to check an item in your browser) \n",
          bold(" Q123   "),"-> use this as the wikidata QID \n",
          bold(" text   "),"-> try this text as alternative search term \n",
          bold(" c      "),"-> create a new item for this later \n",
          bold(" s      "),"-> stop here, save those done so far and come back later")
}

message_stop <- function(done_so_far,total){
  message("Stopping. You've completed ",
          bold$white(done_so_far - 1),
          " so far (",
          bold$white(total - done_so_far + 1),
          " remaining). \n",
          "To restart from where you left off, use the output from this function as the list for disambiguate_QIDs()")
}


# -------- Misc. support --------

# pulling and formatting the first hit from wikidata
# and presenting appropriate choice text options in prep for makechoice()
firsthit <- function(text,
                     filter_property=NULL,
                     filter_variable=NULL,
                     limit=30){
  if(!is.null(filter_property) & !is.null(filter_variable)){
    filtered_items <- filter_qids(ids=sapply(find_item(text,limit = limit),"[[",1),
                                  property=filter_property,
                                  filter=filter_variable,
                                  message="Checking for item that doesn't match the filter ")
    if(!is.na(filtered_items$qid[1])){
      qid   <- filtered_items$qid[1]
      label <- filtered_items$label[1]
      desc  <- filtered_items$desc[1]
      message(white(qid,"   ",label,"   ",desc,sep=""))
      message_choices()
    }else{
      qid <- NA
      message(white("No good match found that matches filters"))
      message_choices_na()
    }
  }else{
    item <- find_item(text,limit = 1)
    if(length(item)>0){
      if(is.null(item[[1]]$description)){
        desc <- "no description"
      }else{
        desc <- item[[1]]$description
      }
      if(is.null(item[[1]]$label)){
        label <- "no label"
      }else{
        label <- item[[1]]$label
      }
      qid <- item[[1]]$id
      message(white(qid,"   ",label,"   ",desc,sep=""))
      message_choices()
    }else{
      qid <- NA
      message(white("No good match found"))
      message_choices_na()
    }
  }
  names(qid) <- text
  return(qid)
}

blank_output_list <- function(list){
  make_attr_names <- function(x){
    x1 <- list[[x]]
    attr(x1, 'names') <- x1
    x1
  }
  if(all(is.null(names(list)))){
    output <- list
    names(output) <- list
  }else{
    output <- lapply(names(list), make_attr_names)
    names(output) <- names(list)
  }
  output <- rapply(output,function(x) ifelse(is.na(x),NA,""),how = 'replace')
  return(output)
}


restarted_output_list <- function(list){
  make_attr_names_rev <- function(x){
    x1 <- list[[x]]
    x1 <- attr(x1, 'names')
    x1
  }
  listnames <- lapply(names(list), make_attr_names_rev)
  output <- rapply(output,function(x) ifelse(is.na(x),NA,""),how = 'replace')
  return(output)
}

choices_alt <- function(selection,filter_property,filter_variable,limit){
  altqids <- unlist(lapply(find_item(selection,limit=limit),function(x) x$id))
  if(is.null(altqids)){
    message("Searching for ",bold$white(selection)," as an alternative term")
    results <- tibble(qid=NA,
                      label=NA,
                      desc="No current matching Wikidata item")
  }else{
    message("Searching for ",bold$white(selection)," as an alternative term")
    results <- filter_qids(ids = altqids,
                           property = filter_property,
                           filter = filter_variable)
  }
  if(all(is.na(results$qid))){
    message(white("No good match found"))
    message_choices_na()
    return(NULL)
  }else{
    message_choices_alt(results)
    names(results$qid) <- results$label
    return(results)
  }
}
