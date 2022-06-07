################################################################################
#                                                                              #
# productivity package doc                                                     #
#                                                                              #
################################################################################

#------------------------------------------------------------------------------#
# productivity package overview                                                #
# Models: -Färe-Primont productivity index                                     #
#         -Fisher productivity index                                           #
#         -Hicks-Moorsteen productivity index                                  #
#         -Laspeyres productivity index                                        #
#         -Lowe productivity index                                             #
#         -Malmquist productivity index                                        #
#         -Paasche productivity index                                          #
#------------------------------------------------------------------------------#

#' productivity: A package for estimating various productivity indices using DEA
#'
#' The \pkg{productivity} package allows estimating various productivity indices
#' using Data Envelopment Analysis (DEA).
#'
#' Seven productivity indices are available. All the indices estimation and
#' decomposition use linear programming (more specifically DEA).
#'
#' @name productivity-package
#'
#' @aliases productivity-package productivity
#'
#' @docType package
#'
#' @section fareprim: \code{\link{fareprim}} estimates the transitive
#' Färe-Primont productivity index.
#'
#' @section fisher: \code{\link{fisher}} estimates the Fisher productivity
#' index.
#'
#' @section hicksmoorsteen: \code{\link{hicksmoorsteen}} estimates the
#' Hicks-Moorsteen productivity index.
#'
#'  @section laspeyres: \code{\link{laspeyres}}estimates the Laspeyres
#' productivity index.
#'
#'  @section lowe: \code{\link{lowe}} estimates the transitive Lowe
#' productivity index.
#'
#'  @section malm: \code{\link{malm}} estimates the Malmquist
#' productivity index.
#'
#'  @section paasche: \code{\link{paasche}} estimates the Paasche
#' productivity index.
#'
#'@section Bugreport: Any bug or suggestion can be reported using the
#' \code{sfaR} tracker facilities at: \url{https://github.com/hdakpo/productivity/issues}
#'
#' @author K Hervé Dakpo, Yann Desjeux, Laure Latruffe
#' @importFrom foreach foreach %dopar% registerDoSEQ
#' @import lpSolveAPI
#' @importFrom parallel detectCores
#' @importFrom doParallel registerDoParallel stopImplicitCluster
#' @importFrom iterators iter nextElem
#' @importFrom utils globalVariables flush.console
#' @importFrom methods is
NULL
