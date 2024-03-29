% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api_utilities.R
\name{get_api_response}
\alias{get_api_response}
\title{Get API response}
\usage{
get_api_response(url, profile = myProfile)
}
\arguments{
\item{url}{a character of API URL}

\item{profile}{an object of class \code{\link[Rnssp]{Credentials}}.
Default is \code{myProfile}.}
}
\value{
An object of class \code{response}.
}
\description{
A wrapper to the \code{get_api_response} method defined in the
\code{\link[Rnssp]{Credentials}} class.
}
\examples{
\dontrun{
myProfile <- create_profile("", "")
url <- "http://httpbin.org/json"

api_response <- get_api_response(url)
names(api_response)
}
}
\seealso{
\code{\link[Rnssp]{Credentials}}
}
