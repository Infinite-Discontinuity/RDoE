#' Generate a Full Factorial Design
#'
#' @param n.factors The number of factors
#' @param p The number of times design is fractionated
#' @param l.generators A list containing the un-simplified design generactors - e.g. for a $2^{3-1}$ design supply list(c("Var2", "Var3"))
#' @param c.names Optional – A character vector of factor names
#'
#' @return A 'design' object
#' @export
#'
#' @examples
Fractional.Factorial <- function(n.factors, p, l.generators, c.names, l.levels) {

  # Create the Base design with n-p factors
  if(missing(n.factors)) {
    nfacs <- length(c.names)
  } else {nfacs <- n.factors}

  l <- list()
  for(i in 1:(nfacs - p)) {
    l[[i]] <- c(-1,1)
  }
  design <- expand.grid(l)

  if(!missing(c.names)) {
    colnames(design) <- c.names[1:(nfacs-p)]
  }

  # add p new factors
  for (i in 1:p) {
    design <- design %>%
      mutate(new = 1)

    if(!missing(c.names)) {
      colnames(design)[[nfacs - p + i]] <- c.names[[nfacs - p + i]]
    } else {
      colnames(design)[n.factors - p + i] <- paste("Var", n.factors - p + i, sep = "")
    }
  }



  # Calculate levels for p factors using the generators
  for(i in 1:p) {
    for (j in 1:length(l.generators[[i]])) {
      design[, nfacs - p + i] <- design[, nfacs - p + i]*design %>% select(l.generators[[i]][[j]])
    }
  }

  factors <- colnames(design)

  generators.n <- list()
  generators.c <- list()
  # Converts the genereators to design words
  for(i in 1:length(l.generators)) {

    n <- c()
    c <- c()

    for(j in 1:length(l.generators[[i]])) {
      c <- c %>% append(l.generators[[i]][[j]])
      n <- n %>% append(str_which(colnames(design), l.generators[[i]][[j]] %>% as.character()) %>% as.character)
    }

    n <- n %>% append(nfacs - p + i)
    c <- c %>% append(colnames(design)[[nfacs - p + i]])

    generators.n[[i]] <- n
    generators.c[[i]] <- c
  }

  # Finds effects
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

  # Finds the defining relation
  n.gen <- length(generators.c)
  words.c <- list()
  words.n <- list()
  n.word <- 1

  if(p > 1) {
    for(i in 2:n.gen) {

      c <- gtools::combinations(length(generators.c), i, 1:n.gen)
      n <- gtools::combinations(length(generators.n), i, 1:n.gen)

      for(j in 1:length(c[,1])) {
        w.c <- c()
        w.n <- c()
        for (k in 1:length(c[j,])) {
          w.c <- append(w.c,generators.c[[c[j,k]]]) %>%
            unique() %>% sort()
          w.n <- append(w.n,generators.n[[n[j,k]]]) %>%
            unique() %>% sort()
        }
        words.c[[n.word]] <- w.c
        words.n[[n.word]] <- w.n
        n.word <- n.word + 1
      }
    }

    defining.relation.c <- append(generators.c, words.c)
    defining.relation.n <- append(generators.n, words.n)

    defining.relation.c <- defining.relation.c[order(sapply(defining.relation.c,length),decreasing=F)]
    defining.relation.n <- defining.relation.n[order(sapply(defining.relation.n,length),decreasing=F)]
  } else {
    defining.relation.c <- generators.c
    defining.relation.n <- generators.n
    }

  for(i in 1:length(defining.relation.c)) {
    defining.relation.c[[i]] <- defining.relation.c[[i]][order(match(defining.relation.c[[i]], effects.c))]
  }

  lengths <- c()
  for(i in 1:length(defining.relation.c)) {
    lengths <- append(lengths, length(defining.relation.c[[i]]))
  }
  resolution <- lengths %>% min()

  # aliasing
  aliasing.c <- matrix(nrow = length(effects.c), ncol = length(effects.c)) %>% as.data.frame()
  aliasing.c[is.na(aliasing.c)] <- ""
  colnames(aliasing.c) <- effects.c
  row.names(aliasing.c) <- effects.c
  for(i in 1:length(effects.c)) {aliasing.c[effects.c[[i]],effects.c[[i]]] <- "X"}
  for(i in 1:length(defining.relation.c)) {
    for(n in 1:(length(defining.relation.c[[i]]) %/% 2)) {
      c <- combn(defining.relation.c[[i]], n)
      for(j in 1:length(c[1,])) {
        t <- c[,j]
        s <- defining.relation.c[[i]]
        for(k in 1:length(t)) {s <- s[s != t[[k]]]}
        t <- t %>% as.character() %>% str_c(collapse = "*")
        s <- s %>% as.character() %>% str_c(collapse = "*")
        aliasing.c[s,t] <- "X"
        aliasing.c[t,s] <- "X"
      }
    }
  }

  aliasing.n <- aliasing.c

  e <- c()
  f <- c()
  for(i in 1:nfacs) {
    e <- e %>% append(i)
  }

  for(i in 1:(nfacs)) {
    t <- combn(e,i)
    for(j in 1:length(t[1,])) {
      f <- f %>% append(t[,j] %>% as.character() %>% str_c(collapse = ""))
    }
  }

  colnames(aliasing.n) <- f
  # rownames(aliasing.n) <- colnames(aliasing.n)

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
             generators.n = generators.n,
             generators.c = generators.c,
             defining.relation.c = defining.relation.c,
             defining.relation.n = defining.relation.n,
             resolution = resolution,
             aliasing.c = aliasing.c,
             aliasing.n = aliasing.n,
             design.flat = data.frame(),
             design.summarised = data.frame(),
             design.original = des.org,
             design.original.flat = data.frame(),
             design.original.summarised = data.frame()
  ))

}
