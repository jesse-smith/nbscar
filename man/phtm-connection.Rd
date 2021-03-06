% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/phantom.R
\name{phtm-connection}
\alias{phtm-connection}
\alias{phtm_process}
\alias{phtm_host}
\alias{phtm_port}
\title{Get Info From an Object Returned by \code{\link[webdriver:run_phantomjs]{run_phantomjs()}}}
\usage{
phtm_process(x)

phtm_host(x)

phtm_port(x)
}
\arguments{
\item{phantom}{A length 2 list returned by
\code{\link[webdriver:run_phantomjs]{run_phantomjs()}}}
}
\value{
The \code{process}, \code{host}, or \code{port}
}
\description{
Get Info From an Object Returned by \code{\link[webdriver:run_phantomjs]{run_phantomjs()}}
}
\keyword{internal}
