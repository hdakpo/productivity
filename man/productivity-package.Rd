% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/productivity-package.R
\docType{package}
\name{productivity-package}
\alias{productivity-package}
\alias{productivity}
\title{productivity: A package for estimating various productivity indices using DEA}
\description{
The \pkg{productivity} package allows estimating various productivity indices
using Data Envelopment Analysis (DEA).
}
\details{
Seven productivity indices are available. All the indices estimation and
decomposition use linear programming (more specifically DEA).
}
\section{fareprim}{
 \code{\link{fareprim}} estimates the transitive
Färe-Primont productivity index.
}

\section{fisher}{
 \code{\link{fisher}} estimates the Fisher productivity
index.
}

\section{hicksmoorsteen}{
 \code{\link{hicksmoorsteen}} estimates the
Hicks-Moorsteen productivity index.

@section laspeyres: \code{\link{laspeyres}}estimates the Laspeyres
productivity index.

@section lowe: \code{\link{lowe}} estimates the transitive Lowe
productivity index.

@section malm: \code{\link{malm}} estimates the Malmquist
productivity index.

@section paasche: \code{\link{paasche}} estimates the Paasche
productivity index.
}

\section{Bugreport}{
 Any bug or suggestion can be reported using the
\code{sfaR} tracker facilities at: \url{https://github.com/hdakpo/productivity/issues}
}

\author{
K Hervé Dakpo, Yann Desjeux, Laure Latruffe
}
