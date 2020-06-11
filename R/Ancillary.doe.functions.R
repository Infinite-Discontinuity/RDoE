#' Replicate the Runs in a design object
#'
#' @param .design a design object
#' @param n.reps the number of replicates
#'
#' @return a design object
#' @export
#'
#' @examples
Add.Replicates <- function(.design, n.reps) {
  design <- .design
  for(i in 1:n.reps) {
    design@design <- bind_rows(design@design,.design@design)
    design@design.original <- bind_rows(design@design.original,.design@design.original)
  }
  return(design)
}

#' Add center points to a design object
#'
#' @param .design a design object
#' @param n.center the number of center points
#'
#' @return a design object
#' @export
#'
#' @examples
Add.CenterPoints <- function(.design, n.center) {

  design <- .design
  facs <- design@factors
  lvl <- design@levels
  center <- data.frame()

  for(i in 1:length(facs)) {
    center[1,i] <- lvl[[i]][[1]] + (lvl[[i]][[2]] - lvl[[i]][[1]])/2
  }
  colnames(center) <- facs

  for (i in 1:n.center) {
    design@design <- bind_rows(design@design, center)
  }

  return(design)
}

#' Randomize the run order in a design object
#'
#' @param .design a design object
#'
#' @return a design object
#' @export
#'
#' @examples
Randomize.Runs <- function(.design) {
  design <- .design
  design@design <- design@design[sample(1:nrow(design@design)), ]
  t <- design@design.original
  for(i in 1:nrow(design@design)) {
    t[i,] <- design@design.original[design@design[i,] %>% row.names(),]
  }
  design@design.original <- t
  rownames(design@design) <- c(1:nrow(design@design))
  return(design)
}

Simulate.Response <- function(.design) {

  design <- .design
  lvl <- design@levels
  hidden.effect <- sample(1:design@n.factors, 1)

  design@design %>% mutate(Response = "")

  for(i in 1:nrow(design@design)) {
    if(design@design[i,hidden.effect] == lvl[[hidden.effect]][[1]]) {design@design$Response[[i]] <- rnorm(n = 1)}
    if(design@design[i,hidden.effect] == lvl[[hidden.effect]][[2]]) {design@design$Response[[i]] <- rnorm(n = 1, mean = 5)}
  }
  return(design)

}

Expand.Effects <- function(.design) {

  design <- .design
  effects <- design@effects
  nfacs <- length(colnames(design@design))

  for(i in (nfacs + 1):length(effects)) {

    design@design <- design@design %>%
      mutate(n = 1)

    for(j in 1:length(effects[[i]])) {
      design@design$n <- design@design$n * design@design[,effects[[i]][[j]]]
    }

    colnames(design@design)[[i]] <- effects[[i]] %>% str_c(collapse = "*")
  }

  return(design)

}
