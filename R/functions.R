

fit_models2 <- function(d, d_thwarted, family = 'quasibinomial'){
  list(
    m1 = glm(T2Belief ~ T1Belief * signal, family = family, d),
    m2 = glm(T2Belief ~ T1Belief + signal * vignette, family = family, d),
    m3 = glm(T2Action ~ T1Action * signal, family = family, d),
    m4 = glm(T2Action ~ T1Action + signal * vignette, family = family, d),
    m5 = glm(T3Action ~ T2Action * signal, family = family, d),
    m6 = glm(T3Action ~ T2Action * vignette + signal, family = family, d),
    m7 = glm(T2Divide ~ T1Divide * signal, family = family, d_thwarted),
    m8 = glm(T2Divide ~ T1Divide + Age * signal, family = family, d_thwarted),
    m9 = glm(T2Jealous ~ T1Jealous * signal, family = family, d),
    m10 = glm(T2Devious ~ T1Devious + signal, family = family, d),
    m11 = glm(T2Angry ~ T1Angry + signal, family = family, d),
    m12 = glm(T2Belief ~ T1Belief + signal*Sex, family = family, d),
    m13 = glm(T2Belief ~ T1Belief + vignette*Sex, family = family, d),
    m14 = glm(T2Action ~ T1Action + signal*Sex, family = family, d),
    m15 = glm(T2Action ~ T1Action + vignette*Sex, family = family, d),
    m16 = glm(T2Belief ~ T1Belief + signal * Age, family = family, d),
    m17 = glm(T2Action ~ T1Action + signal * Age, family = family, d),
    m18 = glm(T2Belief ~ T1Belief + signal + vignette*Age, family = family, d),
    m19 = glm(T2Action ~ T1Action + signal + vignette*Age, family = family, d),
    m20 = glm(T2MentallyIll ~ T1MentallyIll + signal, family = family, d),
    m21 = glm(T2MentallyIll ~ T1MentallyIll + signal*vignette, family = family, d),
    m22 = glm(T2Belief ~ T1Belief * signal + PC1emotionT1 + PC2emotionT1, family = family, d),
    m23 = glm(T2Action ~ T1Action * signal + PC1emotionT1 + PC2emotionT1, family = family, d)
  )
}

fit_models <- function(data, formulas, family = 'quasibinomial'){
  tibble(
    Name = names(formulas),
    Formula = unname(formulas),
    Model = map(Formula, ~glm(formula = .x, family = family, data = data)),
    Anova = map(Model, ~Anova(.x, type = 3)),
    Tidy = map(Model, ~tidy(.x, conf.int = T))
  )
}

effect_plot <- function(m, ...){

  args <- list(...)
  args$partial <- F
  args$rug <- F
  args$gg <- T
  args$scale <- 'response'

  p <- exec('visreg', !!!args)
  p + ylim(c(0, 1)) +
    labs(x = '', y = args$ylbl) +
    theme_bw(15) +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
      axis.title.y = element_text(angle = 0)
    )
}

effect_plots <- function(models, data){
  list(
    m1plot = effect_plot(fit=models[[1]], xvar='signal', by='T1Belief', ylbl='T2 Belief', data=data),
    m2plot = effect_plot(fit=models[[2]], xvar='signal', by='vignette', ylbl='T2 Belief', data=data),
    m3plot = effect_plot(fit=models[[3]], xvar='signal', by='T1Action', ylbl='T2 Action', data=data),
    m4plot = effect_plot(fit=models[[4]], xvar='signal', by='vignette', ylbl='T2 Action', data=data),
    m12plot = effect_plot(fit=models[[12]], xvar='Sex', by='signal', ylbl='T2 Belief', data=data),
    m14plot = effect_plot(fit=models[[14]], xvar='Sex', by='signal', ylbl='T2 Action', data=data),
    m16plot = effect_plot(fit=models[[16]], xvar='Age', by='signal', ylbl='T2 Belief', data=data),
    m17plot = effect_plot(fit=models[[17]], xvar='Age', by='signal', ylbl='T2 Action', data=data),
    m21plot = effect_plot(fit=models[[21]], xvar='signal', by='vignette', ylbl = "Mentally ill", data=data),
    m24plotAge = visreg(fit=models[[24]], xvar='Age', partial=F, rug=F, gg=T, scale='response', data=data) +
      theme_bw(15) +
      theme(
        axis.title.y = element_text(angle = 0)
      ),
    m24plotSex = effect_plot(fit=models[[24]], xvar='Sex', by = 'signal', ylbl='T2 Belief', data=data),
    m25plotAge = visreg(fit=models[[25]], xvar='Age', partial=F, rug=F, gg=T, scale='response', data=data) +
      theme_bw(15) +
      theme(
        axis.title.y = element_text(angle = 0)
      ),
    m25plotSex = effect_plot(fit=models[[25]], xvar='Sex',ylbl='T2 Action', data=data)
  )
}#"T2Belief ~ T1Belief + Age + Sex * signal + vignette"

signal_mediate <- function(
  data = NULL,
  control.value = 'Verbal request',
  treat.value = NULL,
  med_f = NULL,
  out_f = NULL,
  mediator = NULL,
  sims = 1000
){
  d <-
    data %>%
    dplyr::filter(signal %in% c(control.value, treat.value)) %>%
    mutate(signal = fct_relevel(signal, control.value))

  m_med <- glm(med_f, family = binomial, d)
  m_out <- glm(out_f, family = binomial, d)
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
    facet_wrap(~vignette) +
    labs(title = 'Change in mean emotion from T1 to T2', x = '\nLow mood', y = 'Manipulative\n') +
    theme_bw()
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
