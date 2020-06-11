Analyze.Design <- function(.design) {
  design <- .design
  effects <- design@effects
  lvl <- design@levels
  nfacs <- design@n.factors
  design.flat <- data.frame()
  design.flat.o <- data.frame()

  response <- design@design$Response
  design@design <- design@design.original

  for(i in (nfacs + 1):length(effects)) {

    design@design <- design@design %>%
      mutate(n = 1)

    design@design.original <- design@design.original %>%
      mutate(n = 1)

    for(j in 1:length(effects[[i]])) {
      design@design$n <- design@design$n * design@design[,effects[[i]][[j]]]
      design@design.original$n <- design@design.original$n * design@design.original[,effects[[i]][[j]]]
    }

    colnames(design@design)[[i]] <- effects[[i]] %>% str_c(collapse = "*")
    colnames(design@design.original)[[i]] <- effects[[i]] %>% str_c(collapse = "*")
  }

  for(j in 1:nfacs) {
    for(i in 1:nrow(design@design)) {
      if(design@design[i,j] == 0) {design@design[i,j] <- lvl[[j]][[1]] + (lvl[[j]][[2]] - lvl[[j]][[1]])/2}
      if(design@design[i,j] == -1) {design@design[i,j] <- lvl[[j]][[1]]}
      if(design@design[i,j] == 1) {design@design[i,j] <- lvl[[j]][[2]]}
    }
  }

  design@design <- design@design %>%
    add_column(.before = 1, Response = response)

  design@design.original <- design@design.original %>%
    add_column(.before = 1, Response = response)

  for(i in 1:(length(effects))) {

    design@design %>%
      mutate(Level = design@design[,i+1]) %>%
      select(Response, Level) %>%
      mutate(Effect = design@effects.collapsed[[i]]) %>%
      mutate(`Effect Order` = length(effects[[i]])) -> t

    design@design.original %>%
      mutate(Level = design@design.original[,i+1]) %>%
      select(Response, Level) %>%
      mutate(Effect = design@effects.collapsed[[i]]) %>%
      mutate(`Effect Order` = length(effects[[i]])) -> t.o

    bind_rows(design.flat,t) -> design.flat
    bind_rows(design.flat.o,t.o) -> design.flat.o
  }

  design@design.flat <- design.flat
  design@design.original.flat <- design.flat.o

  design.flat %>%
    group_by(Effect, Level, `Effect Order`) %>%
    summarize(`Mean Response` = mean(Response), SD = sd(Response)) %>%
    mutate(x.i = ifelse(
      `Effect Order` < 3,
      ifelse(
        `Effect Order` < 2,
        Effect,
        (Effect %>% str_split("\\*") %>% unlist())[[1]]
      ),
      ""
    )
    ) %>%
    mutate(y.i = ifelse(
      `Effect Order` < 3,
      ifelse(
        `Effect Order` < 2,
        Effect,
        (Effect %>% str_split("\\*") %>% unlist())[[2]]
      ),
      ""
    )
    ) -> design@design.summarised

  design.flat.o %>%
    group_by(Effect, Level, `Effect Order`) %>%
    summarize(`Mean Response` = mean(Response), SD = sd(Response)) %>%
    mutate(x.i = ifelse(
      `Effect Order` < 3,
      ifelse(
        `Effect Order` < 2,
        Effect,
        (Effect %>% str_split("\\*") %>% unlist())[[1]]
      ),
      ""
    )
    ) %>%
    mutate(y.i = ifelse(
      `Effect Order` < 3,
      ifelse(
        `Effect Order` < 2,
        Effect,
        (Effect %>% str_split("\\*") %>% unlist())[[2]]
      ),
      ""
    )
    ) -> design@design.original.summarised

  design@design.flat$Effect <-  design@design.flat$Effect %>% factor(levels = design@effects)
  design@design.summarised$Effect <- design@design.summarised$Effect %>% factor(levels = design@effects)

  return(design)
}

Plot.Response <- function(.design) {

design <- .design

p1 <- design@design %>%
    ggplot(aes(Response)) +
    geom_histogram(
      aes(y=..density..),
      color="black", fill="lightblue"
    ) +
    geom_density(alpha=.2, fill="#FF6666") +
    theme_classic(base_size = 14) +
    labs(
      title = "Response Histogram"
    ) +
    theme(
      axis.title = element_text(colour = "grey30", face = "bold"),
      axis.text = element_text(colour = "grey60"),
      axis.line = element_line(colour = "grey70"),
      axis.ticks = element_blank(),
      axis.title.y = element_text(margin=margin(0,10,0,0)),
      axis.title.x = element_text(margin=margin(10,0,0,0)),
      plot.title = element_text(face = "bold", margin=margin(0,0,10,0))
    )

p2 <- design@design %>%
  ggplot(aes(x = 0, y = Response)) +
    geom_violin(trim = FALSE, fill = 'grey90') +
    geom_boxplot(width = 0.25) +
    geom_jitter(width = 0.05, size = 3, color = "skyblue3") +
    # geom_dotplot(binaxis = "y", stackdir='center', dotsize=0.5, color = "skyblue3", fill = "deepskyblue3") +
    theme_classic(base_size = 14) +
    coord_flip() +
    labs(title = "Response Box Plot") +
    theme(
      axis.title = element_text(colour = "grey30", face = "bold"),
      axis.text = element_text(colour = "grey60"),
      axis.line = element_line(colour = "grey70"),
      axis.ticks = element_blank(),
      axis.title.x = element_text(margin=margin(10,0,0,0)),
      plot.title = element_text(face = "bold", margin=margin(0,0,10,0)),
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      axis.line.y = element_blank()
    )

p3 <- design@design %>%
  ggplot(aes(x = as.numeric(rownames(.design@design)), y = Response)) +
  geom_point(size = 2, color = 'grey30') +
  geom_smooth(size = 1.5) +
  theme_classic(base_size = 14) +
  labs(
    title = "Response vs. Run Order",
    x = "Run"
    ) +
  theme(
    axis.title = element_text(colour = "grey30", face = "bold"),
    axis.text = element_text(colour = "grey60"),
    axis.line = element_line(colour = "grey70"),
    axis.ticks = element_blank(),
    axis.title.y = element_text(margin=margin(0,10,0,0)),
    axis.title.x = element_text(margin=margin(10,0,0,0)),
    plot.title = element_text(face = "bold", margin=margin(0,0,10,0))
  )

dat <- design@design %>%
  arrange(Response) %>%
  mutate(NOSM = 0)

u <- mean(dat$Response)
s <- sd(dat$Response)

for(i in 1:nrow(dat)) {
  ui <- if(i == 1) {
    1 - 0.5^(1/nrow(dat))
  } else if(i == nrow(dat)) {
    0.5^(1/nrow(dat))
  } else {
    (i - 0.3175)/(nrow(dat) + 0.365)
  }

  dat$NOSM[[i]] <- qnorm(ui, mean=u, sd=s)

}

p4t <- dat %>%
  ggplot(aes(x = NOSM, y = Response)) +
  geom_point() +
  theme_classic(base_size = 14) +
  labs(
    title = "Normal Quantile-Quantile Plot",
    x = "Normal Quantiles"
  ) +
  geom_abline(aes(intercept=0,slope=1), linetype = "dashed",
              alpha = 0.5) +
  theme(
    axis.title = element_text(colour = "grey30", face = "bold"),
    axis.text = element_text(colour = "grey60"),
    axis.line = element_line(colour = "grey70"),
    axis.ticks = element_blank(),
    axis.title.y = element_text(margin=margin(0,10,0,0)),
    axis.title.x = element_text(margin=margin(10,0,0,0)),
    plot.title = element_text(face = "bold", margin=margin(0,0,10,0))
  )

p4 <- ggMarginal(p4t, color = "grey60", fill = "tomato", alpha = 0.5, xparams = list(fill = "skyblue1"))

return(list(p1,p2,p3,p4))

}


Plot.Response.vs.Level <- function(.design.analyzed) {

  design <- .design.analyzed

  design@design.original.flat$Level <- as.factor(design@design.original.flat$Level)

  design@design.flat$Level <- as.factor(design@design.flat$Level)

design@design.flat %>%
  filter(`Effect Order` == 1) %>%
  group_by(Level %>% as.factor()) %>%
    ggplot(aes(x = Level, y = Response)) +
    geom_violin(
      trim = FALSE,
      fill = 'grey90'
    ) +
    geom_boxplot(width = 0.1, fill = "lightskyblue1") +
    theme_classic(base_size = 14) +
    facet_grid(cols=vars(Effect), scales = "free_x") +
    labs(title = "Response vs. Factor Levels") +
    theme(
      axis.title = element_text(colour = "grey30", face = "bold"),
      axis.text = element_text(colour = "grey60"),
      axis.line = element_line(colour = "grey70"),
      axis.ticks = element_blank(),
      axis.title.y = element_text(margin=margin(0,10,0,0)),
      axis.title.x = element_text(margin=margin(10,0,0,0)),
      plot.title = element_text(face = "bold", margin=margin(0,0,10,0)),
      legend.position="none"
    )
}

# Summarize.Design <- function(.dat) {
#   .dat %>%
#     group_by(Effect, Level, `Effect Order`) %>%
#     summarize(`Mean Response` = mean(Response), SD = sd(Response)) %>%
#     mutate(x.i = ifelse(
#       `Effect Order` < 3,
#       ifelse(
#         `Effect Order` < 2,
#         Effect,
#         (Effect %>% str_split("\\*") %>% unlist())[[1]]
#       ),
#       ""
#     )
#     ) %>%
#     mutate(y.i = ifelse(
#       `Effect Order` < 3,
#       ifelse(
#         `Effect Order` < 2,
#         Effect,
#         (Effect %>% str_split("\\*") %>% unlist())[[2]]
#       ),
#       ""
#     )
#     ) %>% return()
# }

Plot.Main.Effects <- function(.design.analyzed) {

  design <- .design.analyzed

design@design.summarised %>%
  filter(`Effect Order` == 1) %>%
    ggplot(aes(x = Level %>% as.character() %>% as.numeric(), y = `Mean Response`, color = Effect)) +
    geom_point(size = 3) +
    geom_line(size = 1.25) +
    geom_errorbar(aes(ymin = `Mean Response` - SD, ymax = `Mean Response` + SD), width=.1) +
    facet_grid(cols = vars(Effect), scales = "free_x") +
    # scale_x_continuous(breaks=seq(-1,1,2)) +
    geom_hline(yintercept = mean(design@design.summarised$`Mean Response`)) +
    theme_classic(base_size = 14) +
    scale_color_brewer(palette="Set1") +
    labs(title = "Main Effect Means", x = "Level") +
    theme(
      axis.title = element_text(colour = "grey30", face = "bold"),
      axis.text = element_text(colour = "grey60"),
      axis.line = element_line(colour = "grey70"),
      axis.ticks = element_blank(),
      axis.title.y = element_text(margin=margin(0,10,0,0)),
      axis.title.x = element_text(margin=margin(10,0,0,0)),
      plot.title = element_text(face = "bold", margin=margin(0,0,10,0)),
      legend.position="none"
    )
}

Plot.Half.Normal <- function(.design.analyzed) {

  design <- .design.analyzed

  design@design.original.summarised %>%
    ungroup() %>%
    filter(Level == 1) %>%
    mutate(`Estimated Effect` = `Mean Response` - (design@design.original.summarised %>% filter(Level == -1))$`Mean Response`) %>%
    arrange(`Estimated Effect`) %>%
    mutate(HNOSM = 0) -> estimated.effects

  u <- mean(estimated.effects$`Estimated Effect`)
  s <- sd(estimated.effects$`Estimated Effect`)

  for(i in 1:nrow(estimated.effects)) {
    ui <- if(i == 1) {
      1 - 0.5^(1/nrow(estimated.effects))
    } else if(i == nrow(estimated.effects)) {
      0.5^(1/nrow(estimated.effects))
    } else {
      (i - 0.3175)/(nrow(estimated.effects) + 0.365)
    }

    estimated.effects$HNOSM[[i]] <- fdrtool::qhalfnorm(ui, theta = 1/u)

  }

  estimated.effects %>%
    # filter(`Effect Order` < 3) %>%
    ggplot(aes(x = HNOSM, y = `Estimated Effect`, label = Effect)) +
    geom_point() +
    geom_text(
      # hjust = 0,
      nudge_x = -0.02,
      nudge_y = 0.3,
      angle = 45
    ) +
    geom_hline(yintercept = 0, linetype = "dashed",
               alpha = 0.5) +
    theme_classic(base_size = 14) +
    # annotate("text", x = max(estimated.effects$HNOSM) + 0.01, y = estimated.effects$`Estimated Effect`, label = estimated.effects$Effect, hjust = 0) +
    labs(
      title = "Half-Normal Probability Plot",
      x = "Half-Normal Order Statistical Median"
    ) +
    # xlim(0,0.8) +
    theme(
      axis.title = element_text(colour = "grey30", face = "bold"),
      axis.text = element_text(colour = "grey60"),
      axis.line = element_line(colour = "grey70"),
      axis.ticks = element_blank(),
      axis.title.y = element_text(margin=margin(0,10,0,0)),
      axis.title.x = element_text(margin=margin(10,0,0,0)),
      plot.title = element_text(face = "bold", margin=margin(0,0,10,0))
    )

}

Plot.Interactions <- function(.design.analyzed) {

  design <- .design.analyzed

  design@design.original.summarised %>%
    filter(`Effect Order` < 3) %>%
    ggplot(aes(x = Level, y = `Mean Response`, color = Effect)) +
    geom_point(size = 3) +
    geom_line(size = 1.25) +
    geom_errorbar(aes(ymin = `Mean Response` - SD, ymax = `Mean Response` + SD), width=.1) +
    facet_grid(rows = vars(x.i %>% factor(levels = design@factors)), cols = vars(y.i %>% factor(levels = design@factors))) +
    scale_x_continuous(breaks=seq(-1,1,2)) +
    geom_hline(aes(yintercept = mean(`Mean Response`))) +
    theme_classic(base_size = 14) +
    theme(
      axis.title = element_text(colour = "grey30", face = "bold"),
      axis.text = element_text(colour = "grey60"),
      axis.line = element_line(colour = "grey70"),
      axis.ticks = element_blank(),
      axis.title.y = element_text(margin=margin(0,10,0,0)),
      axis.title.x = element_text(margin=margin(10,0,0,0)),
      plot.title = element_text(face = "bold", margin=margin(0,0,10,0)),
      legend.position="none"
    ) +
    labs(title = "Interaction Plot")
}

Model.Effects <- function(.design) {

  design <- .design
  effects <- design@effects
  nfacs <- design@n.factors

  for(i in (nfacs + 1):length(effects)) {

    if(length(effects[[i]]) < 4) {

      design@design <- design@design %>%
        mutate(n = 1)

      for(j in 1:length(effects[[i]])) {
        design@design$n <- design@design$n * design@design[,effects[[i]][[j]]]
      }

      colnames(design@design)[[i+1]] <- effects[[i]] %>% str_c(collapse = "*")
    }
  }

  fit <- lm(Response ~., data = design@design)
  return(fit)

}

Plot.Model <- function(.lm) {
  fit <- .lm
  dat <- data.frame(fit$residuals,fit$fitted.values)

  p1 <- dat %>%
    ggplot(aes(fit.residuals)) +
      geom_histogram(
        aes(y=..density..),
        color="black", fill="lightblue"
      ) +
      geom_density(alpha=.2, fill="#FF6666") +
      theme_classic() +
      labs(
        title = "Residuals Histogram",
        x = "Fit Residuals"
      ) +
      theme(
        axis.title = element_text(colour = "grey30", face = "bold"),
        axis.text = element_text(colour = "grey60"),
        axis.line = element_line(colour = "grey70"),
        axis.ticks = element_blank(),
        axis.title.y = element_text(margin=margin(0,10,0,0)),
        axis.title.x = element_text(margin=margin(10,0,0,0)),
        plot.title = element_text(face = "bold", margin=margin(0,0,10,0))
      )

  p2 <- dat %>%
    ggplot(aes(x = fit.fitted.values, y = fit.residuals)) +
    geom_point(size = 2, color = 'grey30') +
    geom_smooth(size = 1.5) +
    theme_classic() +
    labs(
      title = "Residuals vs. Predictions",
      x = "Fit Prediction",
      y = "Residuals"
    ) +
    theme(
      axis.title = element_text(colour = "grey30", face = "bold"),
      axis.text = element_text(colour = "grey60"),
      axis.line = element_line(colour = "grey70"),
      axis.ticks = element_blank(),
      axis.title.y = element_text(margin=margin(0,10,0,0)),
      axis.title.x = element_text(margin=margin(10,0,0,0)),
      plot.title = element_text(face = "bold", margin=margin(0,0,10,0))
    )

  u <- mean(dat$fit.residuals)
  s <- sd(dat$fit.residuals)

  dat %>%
    arrange(fit.residuals) %>%
    mutate(NOSM = 0) -> dat

  for(i in 1:nrow(dat)) {
    ui <- if(i == 1) {
      1 - 0.5^(1/nrow(dat))
    } else if(i == nrow(dat)) {
      0.5^(1/nrow(dat))
    } else {
      (i - 0.3175)/(nrow(dat) + 0.365)
    }

    dat$NOSM[[i]] <- qnorm(ui, mean=u, sd=s)

  }

  p3t <- dat %>%
    ggplot(aes(x = NOSM, y = fit.residuals)) +
    geom_point() +
    theme_classic(base_size = 14) +
    labs(
      title = "Normal Quantile-Quantile Plot",
      x = "Normal Quantiles"
    ) +
    geom_abline(aes(intercept=0,slope=1), color = "steelblue2", size = 1.25) +
    theme(
      axis.title = element_text(colour = "grey30", face = "bold"),
      axis.text = element_text(colour = "grey60"),
      axis.line = element_line(colour = "grey70"),
      axis.ticks = element_blank(),
      axis.title.y = element_text(margin=margin(0,10,0,0)),
      axis.title.x = element_text(margin=margin(10,0,0,0)),
      plot.title = element_text(face = "bold", margin=margin(0,0,10,0))
    )

  p3 <- ggMarginal(p3t, color = "grey60", fill = "tomato", alpha = 0.5, xparams = list(fill = "skyblue1"))

  p4 <- fit %>%
    anova() %>%
    as.data.frame() %>%
    rownames_to_column(var = "Effect") %>%
    filter(Effect != "Residuals") %>%
    mutate(
      Effect = Effect %>% str_replace_all("`",""),
      Significance = 1 - `Pr(>F)`) %>%
    arrange(Significance) %>%
    mutate(Effect=factor(Effect, levels=Effect)) %>%
    ggplot(aes(x = Effect, y = `F value`)) +
    geom_bar(stat = "Identity",
             color="black",
             fill="lightblue") +
    coord_flip() +
    theme_classic() +
    theme(
      axis.title = element_text(colour = "grey30", face = "bold"),
      axis.text = element_text(colour = "grey60"),
      axis.text.y = element_text(colour = "grey30"),
      axis.line = element_line(colour = "grey70"),
      axis.line.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.y = element_text(margin=margin(0,10,0,0)),
      axis.title.x = element_text(margin=margin(10,0,0,0)),
      plot.title = element_text(face = "bold", hjust = -0.35, margin=margin(0,0,10,0))
    ) +
    scale_y_log10() +
    geom_hline(aes(yintercept = qf(.95, 1, 71)),
               # color = "steelblue3",
               linetype = "dashed",
               alpha = 0.5,
               size = 1) +
    labs(title="F Value vs. Effect")

  return(list(
    p1,p2,p3,p4
  ))

}

Plot.F.Values <- function(.lm) {
  anova(.lm) %>%
    as.data.frame() %>%
    rownames_to_column(var = "Effect") %>%
    filter(Effect != "Residuals") %>%
    mutate(
      Effect = Effect %>% str_replace_all("`",""),
      Significance = 1 - `Pr(>F)`) %>%
    arrange(Significance) %>%
    mutate(Effect=factor(Effect, levels=Effect)) %>%
    ggplot(aes(x = Effect, y = `F value`)) +
    geom_bar(stat = "Identity",
             color="black",
             fill="lightblue") +
    coord_flip() +
    theme_classic() +
    theme(
      axis.title = element_text(colour = "grey30", face = "bold"),
      axis.text = element_text(colour = "grey60"),
      axis.text.y = element_text(colour = "grey30"),
      axis.line = element_line(colour = "grey70"),
      axis.ticks = element_blank(),
      axis.title.y = element_text(margin=margin(0,10,0,0)),
      axis.title.x = element_text(margin=margin(10,0,0,0)),
      plot.title = element_text(face = "bold", hjust = -0.35, margin=margin(0,0,10,0))
    ) +
    scale_y_log10() +
    geom_hline(aes(yintercept = qf(.95, 1, 71)),
               # color = "steelblue3",
               linetype = "dashed",
               alpha = 0.5,
               size = 1) +
    labs(title="F Value vs. Effect")
}

Plot.Fit.Residuals <- function(.lm) {

}
