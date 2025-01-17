WikidataR
=========

A combined R package for reading, writing and handling Wikidata semantic data (via APIs).

__Authors:__ [Thomas Shafee](https://github.com/TS404) (aut., maint.), [Os Keys](https://github.com/Ironholds) (aut., cre.)  
__License:__ [MIT](https://opensource.org/licenses/MIT)  
__Status:__ Stable

Description
======
WikidataR includes functions to:
- read from wikidata (single items, properties, or properties)
- query wikidata (retrieving all items that match a set of criterial via [Wikidata SPARQL query service](https://query.wikidata.org))
- write to Wikidata (adding new items or statements via [QuickStatements](https://tools.wmflabs.org/quickstatements)) 
- Handle and manipulate Wikidata objects (as lists and tibbles)
For details on how to best use it, see the examples below.

Installation
======

To download the stable version of WikidataR from CRAN:

    install.packages("WikidataR","WikidataQueryServiceR")
    
To get the most current development version from Github:

    install.packages("devtools")
    devtools::install_github("ts404/WikidataR")
    
Examples
======
### Search Wikidata to see if an item exists (example: pharmaceuticals)
For cases where you don't already know the QID of an item or the PID of a property, you can search wikidata by name. Note that some search terms will return multiple possible items. You can also specify a language (defaults to English).

``` r
find_item("Paracetamol")
find_property("medical condition treated")
```
Which returns the lists: 

```
    acetaminophen (Q57055) - common drug for pain and fever  
    Paracetamol (Q36716177) - scientific article published on July 1980  
    Paracetamol (Q54982056) - musical group  
    ...
```

and
```
    medical condition treated (P2175) - disease that this pharmaceutical drug, procedure, or therapy is used to treat 
```
Elements within those lists include basic information from wikidata (ID, description, labels). The QID or PID can then be used to get the full data for the item (see below).

### Convert between identifiers
Wikidata is an excellent thesaurus for different identifiers. For example it is possible to convert from any identifier to wikidata QIDs or between different identifiers
``` r
qid_from_identifier('ISBN-13','978-0-262-53817-6')
identifier_from_identifier('ORCID iD','IMDb ID',c('0000-0002-7865-7235','0000-0003-1079-5604'))
```
Which returns the lists: 
```
    978-0-262-53817-6 Q102035721 Wikipedia @ 20: Stories of an Incomplete Revolution
```
and
```
    # A tibble: 2 x 2
      value               return   
      <chr>               <fct>    
    1 0000-0002-7865-7235 nm2118834
    2 0000-0003-1079-5604 nm1821217
```

### Get full items from Wikidata (example: journal articles)
In this example, we search for three articles using their DOIs ([P356](https://www.wikidata.org/wiki/Property:P356)), find their QIDs, download their full wikidata entries, and then extract the "main topics" (note PID didn't have to be used).

``` r
article.qid      <- qid_from_DOI(c('10.15347/WJM/2017.007','10.15347/WJM/2019.001','10.15347/WJM/2019.007'))
article.q        <- get_item(article.qid)
article.topics.p <- extract_claims(article.q, "main topic")
get_names_from_properties(article.topics.p)
```
Which returns a tibble for each of the journal articles, listing the main topics of each and their QIDs.
```
    $`10.15347/WJM/2017.007`
    # A tibble: 1 x 2
      QID          value    
      <chr>        <chr>    
    1 P921.Q164778 rotavirus

    $`10.15347/WJM/2019.001`
    # A tibble: 2 x 2
      QID            value                               
      <chr>          <chr>                               
    1 P921.Q15989108 Western African Ebola virus epidemic
    2 P921.Q10538943 Ebola virus                         

    $`10.15347/WJM/2019.007`
    # A tibble: 2 x 2
      QID            value                          
      <chr>          <chr>                          
    1 P921.Q1820650  readability                    
    2 P921.Q16235120 health information on Wikipedia
```

### Query Wikidata with complex searches (example: movie genres)

In this example, we search Wikidata for any items that are an "instance of" ([P31](https://www.wikidata.org/wiki/Property:P31)) "film" ([Q11424](https://www.wikidata.org/wiki/Q11424)) that has the label "The Cabin in the Woods" ([Q45394](https://www.wikidata.org/wiki/Q45394)), and ask for the item's genres ([P136](https://www.wikidata.org/wiki/Property:P136)).

``` r
query_wikidata('SELECT DISTINCT
  ?genre ?genreLabel
WHERE {
  ?film wdt:P31 wd:Q11424.
  ?film rdfs:label "The Cabin in the Woods"@en.
  ?film wdt:P136 ?genre.
  SERVICE wikibase:label { bd:serviceParam wikibase:language "en". }
}')
```
Which returns a tibble:
```
    # A tibble: 6 x 2
      genre                                   genreLabel          
      <chr>                                   <chr>               
    1 http://www.wikidata.org/entity/Q3072049 zombie film         
    2 http://www.wikidata.org/entity/Q471839  science fiction film
    3 http://www.wikidata.org/entity/Q859369  comedy-drama        
    4 http://www.wikidata.org/entity/Q1342372 monster film        
    5 http://www.wikidata.org/entity/Q853630  slasher film        
    6 http://www.wikidata.org/entity/Q224700  comedy horror    
```

For more example SPARQL queries, see [this page](https://www.wikidata.org/wiki/Wikidata:SPARQL_query_service/queries/examples) on [Wikidata](https://www.wikidata.org/wiki/Wikidata:Main_Page).

`query_wikidata()` can accept multiple queries, returning a (potentially named) list of data frames. If the vector of SPARQL queries is named, the results will inherit those names.

#### Links for learning SPARQL  

-   [A beginner-friendly course for SPARQL](https://www.wikidata.org/wiki/Wikidata:A_beginner-friendly_course_for_SPARQL)
-   Building a SPARQL query: [Museums on Instagram](https://www.wikidata.org/wiki/Help:SPARQL/Building_a_query/Museums_on_Instagram)
-   [SPARQL Query Examples](https://www.wikidata.org/wiki/Wikidata:SPARQL_query_service/queries/examples) for WDQS
-   [Using SPARQL to access Linked Open Data](https://programminghistorian.org/lessons/graph-databases-and-SPARQL) by Matthew Lincoln
-   Interesting or illustrative [SPARQL queries](https://www.wikidata.org/wiki/Wikidata:SPARQL_query_service/queries) for Wikidata
-   Wikidata [2016 SPARQL Workshop](https://www.wikidata.org/wiki/Wikidata:SPARQL_query_service/2016_SPARQL_Workshop)
-   [Wikidata SPARQL Query video tutorial](https://www.youtube.com/watch?v=1jHoUkj_mKw) by Navino Evans
-   *[Learning SPARQL](http://www.learningsparql.com/)* by Bob DuCharme
-   [WDQS User Manual](https://www.mediawiki.org/wiki/Wikidata_query_service/User_Manual)

### Write to Wikidata (example: paintings)  
In this example we'll write directly to wikidata via the [QuickStatements](https://tools.wmflabs.org/quickstatements) format.
``` r
write_wikidata(items      = c("Q4115189","Q13406268"),
               properties = "author",
               values     = c("Q762","Q41406"),
               format     = "api",
               api.username = "myusername", # Enter your Wikimedia username here
               api.token  = "" #REDACTED# Find yours from https://tools.wmflabs.org/quickstatements/#/user
               )
```
Results in the statements being directly added to wikidata under your username via the API.  
> The Mona Lisa (Q12418) has the Creator (P170) of Leonardo da Vinci (Q762)  
> The Scream (Q471379) has the Creator (P170) of Edvard Munch (Q41406)  

Alternatively, you can print via <code>format=tibble</code> and paste into the [QuickStatements](https://tools.wmflabs.org/quickstatements) website.

### Combining all of the above (example: journal articles)
The example below finds all articles in a journal, works out the URL for their peer reviews, and writes those URLs into those articles' wikidata items.
``` r
sparql_query <- 'SELECT ?Article ?ArticleLabel ?JLabel ?T ?peer_review_URL WHERE {
  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en". }
  ?Article wdt:P1433 wd:Q24657325.
  OPTIONAL { ?Article wdt:P1433 ?J. }
  OPTIONAL { ?Article wdt:P1476 ?T. }
  OPTIONAL { ?Article wdt:P7347 ?peer_review_URL. }}
LIMIT 10000'
articles.qr <- as_tibble(query_wikidata(sparql_query))
articles.qr <- articles.qr[articles.qr$peer_review_URL=="",] #omit those with review URLs listed
review.URLs <- paste0('https://en.wikiversity.org/wiki/Talk:',
                      articles.qr$JLabel,
                      "/",
                      articles.qr$T
                     )
review.URLs <- gsub(" ","_",review.URLs)

write_wikidata(items      = sapply(sapply(articles.qr$Article,pattern = "/",stringr::str_split),tail,1),
               properties = "Peer review URL",
               values     = review.URLs,
               format     = "tibble",
               )
                  
write_wikidata(items        = sapply(sapply(articles.qr$Article,pattern = "/",stringr::str_split),tail,1),
               properties   = "Peer review URL",
               values       = review.URLs,
               format       = "api",
               api.username = "myusername", 
               api.token    = , #REDACTED# Find yours from https://tools.wmflabs.org/quickstatements/#/user
               )
```
### Acknowledgements
This package combines and builds on the utilities of Os Keyes' [WikidataR](https://github.com/Ironholds/WikidataR), Christian Graul's
[rwikidata](https://github.com/chgrl/rwikidata), Mikhail Popov's [WikidataQueryServiceR](https://github.com/wikimedia/WikidataQueryServiceR), and Serena Signorelli's [QueryWikidataR](https://github.com/serenasignorelli/QueryWikidataR) packages. It also uses the Magnus Manske's [QuickStatements](https://github.com/magnusmanske/quickstatements) tool.
