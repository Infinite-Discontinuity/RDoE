#' An s4 Class to represent an experimental design.
#'
#' @slot design a data.frame containing the experimental design
#' @slot effects a list containing the un-collapsed effects - e.g. for a 2 factor interaction effect in index i, effects[[i]] returns a character vector [factor1:factor2]
#' @slot effects.collapsed a character vector containing the collapsed effects - e.g. for a 2 factor interaction effect in index i, effects[[i]] returns a string "factor1*factor2"
#' @slot generators a list containing the un-collapsed character vectors - confounding relations used to generate the design
#' @slot defining.relation a list containing the un-collapsed character vectors - the defining relation
#' @slot resolution a number - the resolution of the design
#' @slot aliasing a data frame - the aliasing structure of the design
#'
#' @return
#' @export
#'
#' @examples
setClass("design", slots = list(

  design = "data.frame",
  n.factors = "numeric",
  factors = "character",
  levels = "list",
  effects = "list",
  effects.collapsed = "character",
  generators.n = "list",
  generators.c = "list",
  defining.relation.c = "list",
  defining.relation.n = "list",
  resolution = "numeric",
  aliasing.c = "data.frame",
  aliasing.n = "data.frame",
  design.flat = "data.frame",
  design.summarised = "data.frame",
  design.original = "data.frame",
  design.original.flat = "data.frame",
  design.original.summarised = "data.frame"

))
