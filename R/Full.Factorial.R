#' Generate a Full Factorial Design
#'
#' @param n.factors The number of factors
#' @param c.names Optional – A character vector of factor names
#'
#' @return A 'design' object
#' @export
#'
#' @examples
Full.Factorial <- function(n.factors, c.names, l.levels) {

  if(missing(n.factors)) {
    nfacs <- length(c.names)
  } else {nfacs <- n.factors}

  l <- list()
  for(i in 1:nfacs) {
    l[[i]] <- c(-1,1)
  }
  design <- expand.grid(l)
  if(!missing(c.names)) {
    colnames(design) <- c.names
  }

  effects <- list()
  effects.c <- c()
  facs <- colnames(design)

  effects <- effects %>% append(facs)
  effects.c <- effects.c %>% append(facs)
  for(j in 2:(nfacs)) {

    comb <- facs %>% combn(j)
    t <- list()
    for(i in 1:length(comb[1,])) {
      t[[i]] <- comb[,i] %>% as.character()
      effects.c <- effects.c %>% append(comb[,i] %>% as.character() %>% str_c(collapse = "*"))
    }
    effects <- effects %>% append(t)
  }

  rownames(design) <- c(1:nrow(design))

  des.org <- design

  if(!missing(l.levels)) {

    lvl <- l.levels

    for(j in 1:nfacs) {
      for(i in 1:nrow(design)) {
        if(design[i,j] == 0) {design[i,j] <- lvl[[j]][[1]] + (lvl[[j]][[2]] - lvl[[j]][[1]])/2}
        if(design[i,j] == -1) {design[i,j] <- lvl[[j]][[1]]}
        if(design[i,j] == 1) {design[i,j] <- lvl[[j]][[2]]}
      }
    }
  } else {
    lvl <- list()
    for(i in 1:nfacs) {
      lvl[[i]] <- c(-1,1)
    }
  }

  return(new("design",
             design = design,
             n.factors = nfacs,
             factors = colnames(design),
             levels = lvl,
             effects = effects,
             effects.collapsed = effects.c,
             generators.n = list(),
             generators.c = list(),
             defining.relation.n = list(),
             defining.relation.c = list(),
             resolution = Inf,
             aliasing.c = data.frame(),
             aliasing.n = data.frame(),
             design.flat = data.frame(),
             design.summarised = data.frame(),
             design.original = des.org,
             design.original.flat = data.frame(),
             design.original.summarised = data.frame()
   ))

  # ls <- list(design, effects, effects.c, effects.d)
  # names(ls) <- c("design", "effects", "effects(collapsed)", "effects(display)")
  # return(ls)
  # class(ls) <- "design"
}
