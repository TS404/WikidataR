% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{url_to_id}
\alias{url_to_id}
\title{Extract an identifier from a wikidata URL}
\usage{
url_to_id(x)

url_to_id(x)
}
\arguments{
\item{x}{A vector of strings representing Wikidata URLs.}
}
\value{
if the URL ends in a QID or PID, return that PID or QID, else return the original string

QID or PID.
}
\description{
Convert a URL ending in an identifier (returned by SPARQL queries) to just
the plain identifier (QID or PID).

Convert a URL ending in an identifier (returned by SPARQL queries)
to just the plan identifier (QID or PID).
}
\details{
The \href{https://www.wikidata.org/wiki/Q43649390}{Wikidata Q identifier} (QID) 
is the unique identifier (UID) used in Wikidata.
}
\examples{
url_to_id("http://www.wikidata.org/entity/42")
url_to_id("http://www.wikidata.org/Q42")
}
