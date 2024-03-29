% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/words.R
\name{words}
\alias{words}
\alias{words.default}
\alias{words.character}
\alias{nword}
\alias{lexicon}
\title{Extract or Count Words}
\usage{
words(x, ...)

\method{words}{default}(x, file, ...)

\method{words}{character}(x, ...)

nword(x, ...)

lexicon(x, ...)
}
\arguments{
\item{x}{Character vector or an object coercible to a text objexct.}

\item{...}{Further arguments (not used).}

\item{file}{Character vector specifying a file to be read.}
}
\description{
Return a list of words contained in a text.
}
\examples{
words("The punctuation, in this phrase; is ... really: 'terrible'.")
nword("The punctuation, in this phrase; is ... really: 'terrible'.")
}
