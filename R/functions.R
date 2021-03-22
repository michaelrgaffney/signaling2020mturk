
T1Belief_Action_dist <- function(d){

  dcor <-
    d %>%
    group_by(vignette) %>%
    summarise(
      r = signif(cor(T1Belief, T1Action), 2)
    ) %>%
    mutate(r = paste('r =', r))

  ggplot(d, aes(T1Belief, T1Action, colour = vignette)) +
    geom_density2d(alpha = 0.5, show.legend = F) +
    geom_count(alpha = 0.5) +
    geom_smooth(se=F, method = 'lm', show.legend = F) +
    geom_text(data = dcor, aes(label = r), x = .15, y = 0.95, size=5, colour='black') +
    coord_fixed() +
    labs(x = '\nT1 Belief', y = 'T1 Action\n') +
    guides(colour = 'none', size = guide_legend(title = 'Overlapping\npoints')) +
    facet_wrap(~vignette) +
    theme_minimal(15) +
    theme(axis.title.y = element_text(angle=0))

}

fit_models <- function(data, formulas, family = 'quasibinomial'){
  d <- tibble(
    Name = names(formulas),
    Formula = unname(formulas),
    Model = if(family == 'quasibinomial'){
      map(Formula, ~glm(formula = .x, family = quasibinomial, data = data))
    } else {
      map(Formula, ~lm(formula = .x, data = data))
      },
    TidyModel = map(Model, ~tidy(.x, conf.int = T)),
    Anova = map(Model, ~Anova(.x, type = 3)),
    TidyANOVA = map(Anova, ~tidy(.x, conf.int = T))
  )
  names(d$Model) <- d$Name
  names(d$TidyModel) <- d$Name
  names(d$TidyANOVA) <- d$Name
  return(d)
}

effect_plot <- function(m, ...){

  args <- list(...)
  args$partial <- F
  args$rug <- F
  args$gg <- T
  args$scale <- 'response'

  p <- exec('visreg', !!!args) +
    ylim(0, 1) +
    ylab(args$ylbl) +
    theme_bw(15) +
    theme(axis.title.y = element_text(angle = 0))

  d <- args$data
  vartype = class(d[[args$xvar]])
  if('numeric' %in% vartype) return(p)

  p +
    xlab('') +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
}

effect_plots <- function(models, data){
  list(
    m1plot = effect_plot(fit=models$m1, xvar='signal', ylbl='T2 Belief', data=data),
    m1bplot = effect_plot(fit=models$m1b, xvar='signal', by='T1Belief', ylbl='T2 Belief', data=data),
    m2plot = effect_plot(fit=models$m2, xvar='signal', by='vignette', ylbl='T2 Belief', data=data),
    m3plot = effect_plot(fit=models$m3, xvar='signal', ylbl='T2 Action', data=data),
    m3bplot = effect_plot(fit=models$m3b, xvar='signal', by='T1Action', ylbl='T2 Action', data=data),
    m4plot = effect_plot(fit=models$m4, xvar='signal', by='vignette', ylbl='T2 Action', data=data),
    m7plot = effect_plot(fit=models$m7, xvar='signal', by='T1Divide', ylbl='T2 Division', data=data),
    m8plot = effect_plot(fit=models$m8, xvar='Age', by='signal', ylbl='T2 Division', data=data),
    m12plot = effect_plot(fit=models$m12, xvar='Sex', by='signal', ylbl='T2 Belief', data=data),
    m13plot = effect_plot(fit=models$m13, xvar='Sex', by='vignette', ylbl='T2 Belief', data=data),
    m14plot = effect_plot(fit=models$m14, xvar='Sex', by='signal', ylbl='T2 Action', data=data),
    m15plot = effect_plot(fit=models$m15, xvar='Sex', by='vignette', ylbl='T2 Action', data=data),
    m16plot = effect_plot(fit=models$m16, xvar='Age', by='signal', ylbl='T2 Belief', data=data),
    m17plot = effect_plot(fit=models$m17, xvar='Age', by='signal', ylbl='T2 Action', data=data),
    m18plot = effect_plot(fit=models$m18, xvar='Age', by='vignette', ylbl='T2 Belief', data=data),
    m19plot = effect_plot(fit=models$m19, xvar='Age', by='vignette', ylbl='T2 Action', data=data),
    m21plot = effect_plot(fit=models$m21, xvar='signal', by='vignette', ylbl = "T2 Mentally ill", data=data),
    m24plotAge = visreg(fit=models$m24, xvar='Age', partial=F, rug=F, gg=T, scale='response', data=data) +
      theme_bw(15) + theme(axis.title.y = element_text(angle = 0)),
    m24plotSex = effect_plot(fit=models$m24, xvar='Sex', by = 'signal', ylbl='T2 Belief', data=data),
    m25plotAge = visreg(fit=models$m25, xvar='Age', partial=F, rug=F, gg=T, scale='response', data=data) +
      theme_bw(15) + theme(axis.title.y = element_text(angle = 0)),
    m25plotSex = effect_plot(fit=models$m25, xvar='Sex', ylbl='T2 Action', data=data),
    m26plot = effect_plot(fit=models$m26, xvar='T2Action', by='vignette', ylbl='T3 Action', data=data) +
      theme(axis.text.x = element_text(angle = 0)) + xlab('\nT2 Action'),
    m27plot = effect_plot(fit=models$m27, xvar='vignette', ylbl='T1 Mentally ill', data=data)
  )
}#"T2Belief ~ T1Belief + Age + Sex * signal + vignette"

signal_mediate <- function(
  data = NULL,
  control.value = 'Verbal request',
  treat.value = NULL,
  med_f = NULL,
  out_f = NULL,
  mediator = NULL,
  family = 'binomial',
  sims = 1000
){
  d <-
    data %>%
    dplyr::filter(signal %in% c(control.value, treat.value)) %>%
    mutate(
      signal = fct_relevel(as.character(signal), control.value)
      )

  if (family == 'binomial'){
    m_med <- glm(med_f, family = binomial, d)
    m_out <- glm(out_f, family = binomial, d)
  } else if (family == 'gaussian'){
    m_med <- lm(med_f, d)
    m_out <- lm(out_f, d)
  }
  out <- mediate(m_med, m_out, treat = "signal", mediator = 'T2Belief', control.value = control.value, treat.value = treat.value, robustSE = T, sims = sims)
  return(out)
}

emotion_plot <- function(d){

  mean_emotions <- function(d){
    d %>%
      group_by(vignette, signal) %>%
      dplyr::summarise(
        T1LowMood_mean = mean(T1LowMood),
        T1Manipulative_mean = mean(T1Manipulative),
        T2LowMood_mean = mean(T2LowMood),
        T2Manipulative_mean = mean(T2Manipulative),
        .groups='drop'
      )
  }

  d_full <- mean_emotions(d)

  n <- nrow(d)
  boot_emotions <- function(i){
    mean_emotions(d[sample.int(n, replace = T),])
  }

  d_boot <- map_df(1:500, boot_emotions)

  ggplot() +
    geom_segment(data = d_boot, aes(x = T1LowMood_mean, y = T1Manipulative_mean, xend = T2LowMood_mean, yend = T2Manipulative_mean, colour=signal), alpha = 0.02, arrow = arrow(length = unit(3, "mm"))) +
    geom_segment(data = d_full, aes(x = T1LowMood_mean, y = T1Manipulative_mean, xend = T2LowMood_mean, yend = T2Manipulative_mean, colour=signal), alpha = 1, size = 1, arrow = arrow(length = unit(3, "mm"))) +
    xlim(0, NA) +
    facet_wrap(~vignette) +
    labs(title = 'Change in mean emotions from T1 to T2', x = '\nLow mood', y = 'Manipulative\n') +
    theme_bw(15) +
    theme(axis.title.y = element_text(angle = 0))
}

plot_raw_data <- function(d, type){
  d <-
    d %>%
    dplyr::select(MTurkID, signal, vignette, starts_with('T1'), starts_with('T2')) %>%
    pivot_longer(starts_with('T')) %>%
    separate(col=name, into = c('Time', 'Variable'), sep = 2) %>%
    dplyr::rename(
      id = MTurkID
    )

  db <-
    d %>%
    dplyr::filter(Variable == type)

  dc <-
    db %>%
    group_by(signal, vignette, Time) %>%
    dplyr::summarise(value = mean(value))

  ggplot() +
    geom_line(data = db, aes(Time, value, group = id), alpha = 0.1) +
    geom_line(data = dc, aes(Time, value, group = 1), colour = 'red') +
    scale_x_discrete(expand = expansion(mult=0.1)) +
    facet_grid(vignette~signal) +
    labs(x = '', y = paste0(type, '\n')) +
    theme_minimal(15) +
    theme(
      strip.text.y = element_text(angle = 0, hjust = 0),
      panel.spacing.x = unit(1, 'lines')
    )
}

T2emotion_heatmap <- function(d){
  d %>%
    dplyr::select(signal, T2Angry:T2Violated) %>%
    pivot_longer(-signal) %>%
    mutate(name = stringr::str_remove(name, 'T2')) %>%
    group_by(name, signal) %>%
    summarise(n = sum(value)/n()) %>%
    pivot_wider(names_from = signal, values_from = n, values_fill = 0) %>%
    hagenheat(rotate_labels = F, seriation_method = 'PCA_angle', viridis_option = 'B') +
    theme_minimal(15)
}

T1emotion_heatmap <- function(d){
  d %>%
    dplyr::select(vignette, T1Angry:T1Violated) %>%
    pivot_longer(-vignette) %>%
    mutate(name = stringr::str_remove(name, 'T1')) %>%
    group_by(name, vignette) %>%
    summarise(n = sum(value)/n()) %>%
    pivot_wider(names_from = vignette, values_from = n, values_fill = 0) %>%
    hagenheat(rotate_labels = F, seriation_method = 'PCA_angle', viridis_option = 'B') +
    theme_minimal(15)
}

######### Alternate mediators ############

# tmp <-
#   d %>%
#   dplyr::select(T1Action, T2Action, T1Belief, T2Belief, signal, T1LowMood, T2LowMood, T1Manipulative, T2Manipulative, T1Jealous, T2Jealous) %>%
#   dplyr::filter(signal %in% c('Suicide attempt', 'Verbal request')) %>%
#   mutate(signal = ifelse(signal == 'Suicide attempt', 1, 0)) %>%
#   as.data.frame()
#
#
# m <-
#   multimed(
#     outcome = "T2Action",
#     med.main = "T2Belief",
#     med.alt = "T2Jealous",
#     treat = "signal",
#     covariates = c("T1Belief", "T1Jealous", "T1Action"),
#     data = tmp,
#     sims = 1000
#     )
