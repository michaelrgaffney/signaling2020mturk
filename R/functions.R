
prep_pilotdata <- function(d){

  var_dict <-
    c(
    'Control:Sister' = 'Control',
    'Schizophrenia:Sister' = 'Schizophrenia',
    'VerbalRequest:Sister' = 'Verbal request',
    'Anger:Sister' = 'Anger',
    'FacialSadnesswithCrying:Sister' = 'Mild depression',
    'Depression:Sister' = 'Depression',
    'DepressionwithSuicideThreat:Sister' = 'Depression + Suicide threat',
    'SuicideAttempt:Sister' = 'Suicide attempt'
  )

  d %>%
    mutate(
      signal = var_dict[signal],
      signal = factor(signal, levels = var_dict)
    )
}

pilotResults <- function(d){

  d <-
    d %>%
    mutate(
      MentallyIll = str_detect(MC2.3, 'Mentally ill')
    )

  rslts <- list(
    m_pilot_belief = glm(needsmoneyt2/100 ~ needsmoneyt1 + signal, family = quasibinomial, d),
    m_pilot_mentallyill = suppressWarnings(glm(MentallyIll ~ signal, family = binomial, d)),
    meanT1belief = mean(d$needsmoneyt1, na.rm = T),
    medianT1belief = median(d$needsmoneyt1, na.rm = T)
  )

  rslts$effectplot <-
    visreg(rslts$m_pilot_belief, xvar='signal', scale='response', rug=F, gg=T) +
    labs(title = 'Pilot study results: Belief', x = '', y = '\nT2 Belief') +
    coord_flip() +
    theme_bw(15)

  rslts$mentalplot <-
    visreg(rslts$m_pilot_mentallyill, xvar = 'signal', scale = 'response', rug = F, gg = T) +
    labs(title = 'Pilot study results: Mentally ill', x = '', y = '\nT2 Mentally ill') +
    coord_flip() +
    theme_bw(15)

  return(rslts)
}

T1Belief_Action_dist <- function(d){

  dcor <-
    d %>%
    group_by(vignette) %>%
    summarise(
      r = signif(cor(T1Belief, T1Action), 2)
    ) %>%
    mutate(r = paste('r =', r))

  dmean <-
    d %>%
    group_by(vignette) %>%
    summarise(T1Belief = mean(T1Belief, na.rm = T), T1Action = mean(T1Action, na.rm = T))

  ggplot(d, aes(T1Belief, T1Action, colour = vignette)) +
    # geom_density2d(alpha = 0.5, show.legend = F) +
    geom_count(alpha = 0.5) +
    geom_smooth(se=F, method = 'lm', show.legend = F) +
    geom_point(data = dmean, aes(T1Belief, T1Action), colour = 'black', size = 3) +
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
    Formulas = unname(formulas),
    Model = if(family == 'quasibinomial'){
      map(Formulas, ~glm(formula = .x, family = quasibinomial, data = data))
    } else {
      map(Formulas, ~lm(formula = .x, data = data))
      },
    TidyModel = map(Model, ~tidy(.x, conf.int = T)),
    Anova = map(Model, ~Anova(.x, type = 3)),
    TidyANOVA = map(Anova, ~tidy(.x, conf.int = T)),
    Margins = map(Model, ~margins(.x, type = 'response'))
  )

  names(d$Model) <- d$Name
  names(d$TidyModel) <- d$Name
  names(d$TidyANOVA) <- d$Name
  names(d$Margins) <- d$Name

  return(d)
}

fmt_margins <- function(m, var, by = NULL){
  m <- summary(m)
  ame <- 100*signif(m[m$factor == var, 'AME'], 2)
  lower <- 100*signif(m[m$factor == var, 'lower'], 2)
  upper <- 100*signif(m[m$factor == var, 'upper'], 2)
  glue("{ame} (95% CI: {lower}-{upper})")
}

fractional_model <- function(m, table = F){
  # Fit fractional regression model based on standard glm model
  x = model.matrix(m)[,-1] # remove intercept
  frm(m$y, x, linkfrac = 'logit', table = table)
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
    m12plot = effect_plot(fit=models$m12, xvar='Sex', ylbl='T2 Belief', data=data),
    m12bplot = effect_plot(fit=models$m12, xvar='Age', ylbl='T2 Belief', data=data),
    m13plot = effect_plot(fit=models$m13, xvar='Sex', ylbl='T2 Action', data=data),
    m13bplot = effect_plot(fit=models$m13, xvar='Age', ylbl='T2 Action', data=data),
    # m14plot = effect_plot(fit=models$m14, xvar='Sex', by='signal', ylbl='T2 Action', data=data),
    # m15plot = effect_plot(fit=models$m15, xvar='Sex', by='vignette', ylbl='T2 Action', data=data),
    # m16plot = effect_plot(fit=models$m16, xvar='Age', by='signal', ylbl='T2 Belief', data=data),
    # m17plot = effect_plot(fit=models$m17, xvar='Age', by='signal', ylbl='T2 Action', data=data),
    # m18plot = effect_plot(fit=models$m18, xvar='Age', by='vignette', ylbl='T2 Belief', data=data),
    # m19plot = effect_plot(fit=models$m19, xvar='Age', by='vignette', ylbl='T2 Action', data=data),
    m21plot = effect_plot(fit=models$m21, xvar='signal', by='vignette', ylbl = "T2 Mentally ill", data=data),
    # m24plotAge = visreg(fit=models$m24, xvar='Age', partial=F, rug=F, gg=T, scale='response', data=data) +
    #   theme_bw(15) + theme(axis.title.y = element_text(angle = 0)),
    # m24plotSex = effect_plot(fit=models$m24, xvar='Sex', by = 'signal', ylbl='T2 Belief', data=data),
    # m25plotAge = visreg(fit=models$m25, xvar='Age', partial=F, rug=F, gg=T, scale='response', data=data) +
    #   theme_bw(15) + theme(axis.title.y = element_text(angle = 0)),
    # m25plotSex = effect_plot(fit=models$m25, xvar='Sex', ylbl='T2 Action', data=data),
    m26plot = effect_plot(fit=models$m26, xvar='T2Action', by='vignette', ylbl='T3 Action', data=data) +
      theme(axis.text.x = element_text(angle = 0)) + xlab('\nT2 Action'),
    m27plot = effect_plot(fit=models$m27, xvar='vignette', ylbl='T1 Mentally ill', data=data)
  )
}

effect_plots_India <- function(models, data){
  list(
    m7plot = effect_plot(fit=models$m7, xvar='signal', by='T1Divide', ylbl='T2 Division', data=data),
    m8plot = effect_plot(fit=models$m8, xvar='Age', by='signal', ylbl='T2 Division', data=data),
    m12plot = effect_plot(fit=models$m12, xvar='years_education', ylbl='T2 Belief', data=data),
    m13plot = effect_plot(fit=models$m13, xvar='Sex', ylbl='T2 Action', data=data),
    m17plot = effect_plot(fit=models$m17, xvar='signal', by='Age', ylbl='T2 Action', data=data)
  )
}

visreg_diff <- function(m, vignette, signal1, signal2, d, sig = 2){
  obj <- visreg(m, xvar='signal', by = 'vignette', scale='response', data = d, plot = F)
  fit <- obj$fit
  v1 <- fit$visregFit[fit$signal==signal1 & fit$vignette==vignette]
  v2 <- fit$visregFit[fit$signal==signal2 & fit$vignette==vignette]
  signif(100*(v1 - v2), sig)
}

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
    m_med <- suppressWarnings(glm(med_f, family = binomial, d))
    m_out <- suppressWarnings(glm(out_f, family = binomial, d))
  } else if (family == 'gaussian'){
    m_med <- lm(med_f, d)
    m_out <- lm(out_f, d)
  }
  out <- mediate(m_med, m_out, treat = "signal", mediator = 'T2Belief', control.value = control.value, treat.value = treat.value, robustSE = T, sims = sims)
  return(out)
}

all_signals_mediate <- function(d){

  signals <- as.character(levels(d$signal)[-1]) # Omit base level
  names(signals) <- signals
  stuff <- c('d.avg', 'd.avg.ci', 'z.avg', 'z.avg.ci', 'tau.coef', 'tau.ci', 'n.avg')

  meds <-
    map_df(
      signals,
      ~as_tibble(
        signal_mediate(
          sims = 2000,
          data = d,
          treat.value = .x,
          med_f = 'T2Belief ~ signal + T1Belief',
          out_f = 'T2Action ~ signal + T1Belief + T1Action + T2Belief',
          mediator = 'T2Belief'
        )[stuff]
      ),
      .id = 'signal'
    ) %>%
    mutate(
      level = rep(c('low', 'high'), length(signals)),
      signal = factor(signal, levels = c('Crying', 'Mild depression', 'Depression', 'Suicide attempt'))
    ) %>%
    pivot_wider(names_from = level, values_from = c(d.avg.ci, z.avg.ci, tau.ci)) %>%
    rename(
      signal = signal,
      ACME.point = d.avg,
      ADE.point = z.avg,
      Total.point = tau.coef,
      n.avg = n.avg,
      ACME.low = d.avg.ci_low,
      ACME.high = d.avg.ci_high,
      ADE.low = z.avg.ci_low,
      ADE.high = z.avg.ci_high,
      Total.low = tau.ci_low,
      Total.high = tau.ci_high
    ) %>%
    pivot_longer(-c(signal, n.avg)) %>%
    separate(name, into = c("Stat", "type"), sep = "\\.") %>%
    pivot_wider(names_from = type, values_from = value) %>%
    mutate(Stat = factor(Stat, levels = c('ADE', 'ACME', 'Total')))

  p <-
    ggplot(meds, aes(point, signal, xmin = low, xmax = high, colour = Stat)) +
    geom_pointrange(lwd = 2.5, fatten = 1, alpha = 0.75, position = position_dodge(width = .4)) +
    geom_text(x = 0.32, aes(label = paste0(round(100*n.avg), '%')), colour='black') +
    scale_color_viridis_d(option = 'B', end = 0.8) +
    guides(color=guide_legend(reverse = TRUE, override.aes = list(size=1))) +
    xlim(-0.02, 0.33) +
    labs(x = "\nAction", y = "") +
    theme_minimal(15)

  list(results=meds, plot=p)
}


emotion_plot <- function(d){

  mean_emotions <- function(d){
    d %>%
      group_by(vignette, signal) %>%
      dplyr::summarise(
        T1LowMood_mean = mean(T1LowMood),
        T1Manipulative_mean = mean(T1Manipulative),
        T2LowMood_mean = mean(T2LowMood - T1LowMood),
        T2Manipulative_mean = mean(T2Manipulative - T1Manipulative),
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
    geom_segment(data = d_boot, aes(x = T1LowMood_mean, y = T1Manipulative_mean, xend = T1LowMood_mean + T2LowMood_mean, yend = T1Manipulative_mean + T2Manipulative_mean, colour=signal), alpha = 0.02, arrow = arrow(length = unit(3, "mm"))) +
    geom_segment(data = d_full, aes(x = T1LowMood_mean, y = T1Manipulative_mean, xend = T1LowMood_mean + T2LowMood_mean, yend = T1Manipulative_mean + T2Manipulative_mean, colour=signal), alpha = 1, size = 1, arrow = arrow(length = unit(3, "mm"))) +
    xlim(0, NA) +
    facet_wrap(~vignette) +
    labs(title = 'Change in mean emotions from T1 to T2', x = '\nLow mood', y = 'Manipulative\n') +
    theme_bw(15) +
    theme(axis.title.y = element_text(angle = 0))
}

plot_ecdf <- function(d){
  e <-
    d %>%
    dplyr::select(id, signal, vignette, T1Belief, T2Belief, T1Action, T2Action) %>%
    pivot_longer(c(T1Belief, T2Belief, T1Action, T2Action), names_to = c('Time', 'Type'), names_sep = 2) %>%
    mutate(
      vignette = factor(vignette, levels = c('Thwarted marriage', 'Basketball coach', 'Romantic partner', 'Brother-in-law')),
      signal = as.character(signal)
    ) %>%
    group_by(Type) %>%
    mutate(
      signal = ifelse(Time == 'T1', 'T1 Baseline', signal),
      signal = factor(signal, levels = (c('T1 Baseline', 'Verbal request', 'Crying', 'Mild depression', 'Depression', 'Suicide attempt')))
    )

  emedian <-
    e %>%
    group_by(Type, vignette, signal) %>%
    summarise(median = median(value))

  ggplot(e, aes(value, colour = signal)) +
    stat_ecdf() +
    geom_segment(data=emedian, aes(x=median, xend=median, colour = signal), y=0, yend=0.5, linetype = 'dotted') +
    geom_point(data=emedian, aes(x=median, colour = signal), y = 0.5) +
    geom_point(data=emedian, aes(x=median, colour = signal), y = 0) +
    guides(color=guide_legend(title=NULL, override.aes = list(size = 2))) +
    facet_grid(Type~vignette, as.table = F) +
    labs(x = '\nRating', y = 'Cummulative fraction\nof observations') +
    theme_bw(15) +
    theme(
      axis.title.y = element_text(angle=0, hjust=1),
      strip.text.y = element_text(angle=0, hjust=0),
      legend.position = 'top')
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

plot_raw_data2d <- function(d){

  d2 <-
    d %>%
    group_by(signal, vignette) %>%
    dplyr::summarise(
      meanT1Belief = mean(T1Belief),
      meanT1Action = mean(T1Action),
      meanT2Belief = mean(T2Belief),
      meanT2Action = mean(T2Action)
    )

  ggplot() +
    geom_segment(data = d, aes(x = T1Belief, y = T1Action, xend = T2Belief, yend = T2Action), alpha = 0.25, arrow = arrow(length = unit(2, "mm"), type = 'closed')) +
    geom_segment(data = d2, aes(x = meanT1Belief, y = meanT1Action, xend = meanT2Belief, yend = meanT2Action), colour='red', alpha = 1, size = 1, arrow = arrow(length = unit(3, "mm"), type = 'closed')) +
    scale_x_continuous(breaks = c(0, 0.5, 1.0)) +
    scale_y_continuous(breaks = c(0, 0.5, 1.0)) +
    facet_grid(signal~vignette, as.table = F) +
    labs(x = '\nBelief', y = 'Action\n') +
    coord_fixed() +
    theme_minimal(14) +
    theme(
      strip.text.y = element_text(angle = 0, hjust = 0),
      panel.spacing.x = unit(1, 'lines'),
      axis.title.y = element_text(angle = 0)
    )
}

T1emotion_vignette <- function(d){
  d %>%
    dplyr::select(vignette, T1Angry:T1Violated) %>%
    pivot_longer(-vignette) %>%
    mutate(name = stringr::str_remove(name, 'T1')) %>%
    group_by(name, vignette) %>%
    summarise(n = sum(value)/n()) %>%
    pivot_wider(names_from = vignette, values_from = n)
}

T1emotion_signal <- function(d){
  d %>%
    dplyr::select(signal, T1Angry:T1Violated) %>%
    pivot_longer(-signal) %>%
    mutate(name = stringr::str_remove(name, 'T2')) %>%
    group_by(signal, name) %>%
    summarise(n = sum(value)/n()) %>%
    pivot_wider(names_from = signal, values_from = n)
}

T2emotion_signal <- function(d){
  d %>%
    dplyr::select(signal, T2Angry:T2Violated) %>%
    pivot_longer(-signal) %>%
    mutate(name = stringr::str_remove(name, 'T2')) %>%
    group_by(signal, name) %>%
    summarise(n = sum(value)/n()) %>%
    pivot_wider(names_from = signal, values_from = n)
}


# Effect sizes ------------------------------------------------------------

eff_sizes <- function(d){
  signals <- as.character(unique(d$signal))[-1]
  vignettes <- unique(d$vignette)
  crs <- cross_df(list(signal=signals, vignette=vignettes))
  names(crs$signal) <- 1:length(crs$signal)

  ef <- function(s, v, d){
    tmp <- d %>%
      dplyr::filter(vignette == v, signal == s | signal == 'Verbal request') %>%
      dplyr::select(T2Belief, T2Action, signal, vignette)
    efs <- c(as.numeric(psych::cohen.d(tmp$T2Belief, tmp$signal)$cohen.d), as.numeric(psych::cohen.d(tmp$T2Action, tmp$signal)$cohen.d))
    names(efs) <- c('Belief_low', 'Belief_est', 'Belief_high', 'Action_low', 'Action_est', 'Action_high')
    efs
  }

  bind_cols(
    crs,
    map2_dfr(crs$signal, crs$vignette, ~ef(.x, .y, d))
  ) %>%
    mutate(
      signal = factor(signal, levels = c('Crying', 'Mild depression', 'Depression', 'Suicide attempt')),
      id = paste0(signal, ',', vignette)
    ) %>%
    pivot_longer(Belief_low:Action_high, names_to = c('Outcome', '.value'), names_sep = '_')
}

eff_sizes2 <- function(d){

  tmp1 <-
    d %>%
    dplyr::select(vignette, signal, T1Belief, T2Belief) %>%
    pivot_longer(cols = c(T1Belief, T2Belief)) %>%
    mutate(name = factor(name, levels = c('T2Belief', 'T1Belief'))) %>%
    group_by(vignette, signal) %>%
    rstatix::cohens_d(., value ~ name, paired=T, ci=T, nboot = 1000)

  tmp2 <-
    d %>%
    dplyr::select(vignette, signal, T1Action, T2Action) %>%
    pivot_longer(cols = c(T1Action, T2Action)) %>%
    mutate(name = factor(name, levels = c('T2Action', 'T1Action'))) %>%
    group_by(vignette, signal) %>%
    rstatix::cohens_d(., value ~ name, paired=T, ci=T, nboot = 1000)

  tmp <- bind_rows(list(Belief = tmp1, Action = tmp2), .id = 'Outcome')
  tmp$id <- paste0(tmp$signal, ',', tmp$vignette)
  tmp
}


# Power -------------------------------------------------------------------

pwr_curve0 <- function(signalingdata2018, control){

  e <-
    signalingdata2018 %>%
    dplyr::filter(signal == "Depression" | signal == control) %>%
    mutate(signal = factor(signal, levels = c(control, "Depression")))

  pwr <- function(sample_size){
    pvalues <- map_dbl(1:2000, ~summary(lm(needsmoneyt2 ~ needsmoneyt1 + signal, e[sample(1:nrow(e), sample_size, replace = T),]))$coefficients["signalDepression","Pr(>|t|)"])
    sum(pvalues < 0.05)/length(pvalues)
  }

  plan(multisession, workers = 3)
  tibble(
    sample_size = seq(20, 200, 5),
    power = future_map_dbl(sample_size, pwr, .options = furrr_options(seed = T))
    )
}

pwr_curve <- function(d){
  bind_rows(list('Control' = pwr_curve0(d, 'Control'), 'Verbal' = pwr_curve0(d, 'Verbal request')), .id='Base')
}

pwr_curve2 <- function(signalingdata2018){
  e <- signalingdata2018 %>% dplyr::filter(signal == "Depression")
  pwr <- function(sample_size){
    pvalues <- map_dbl(1:2000, ~{e2 <- e[sample(1:nrow(e), sample_size, replace = T),]; t.test(e2$needsmoneyt1, e2$needsmoneyt2, paired = T)$p.value})
    sum(pvalues < 0.05)/length(pvalues)
  }
  tibble(
    sample_size = seq(20, 200, 5),
    power = map_dbl(sample_size, pwr)
  )
}

# 2D densities ------------------------------------------------------------

density2d <- function(d, res=150){
  d %>%
    group_by(vignette, signal) %>%
    nest() %>%
    rowwise() %>%
    mutate(
      matT1 = list(matrix(c(data$T1Belief, data$T1Action), ncol = 2, dimnames = list(c(), c('T1Belief', 'T1Action')))),
      kdeT1 = list(kde.boundary(x=matT1, xmin=c(0,0), xmax=c(1,1), boundary.kernel="linear", gridsize = c(res,res))),
      matT2 = list(matrix(c(data$T2Belief, data$T2Action), ncol = 2, dimnames = list(c(), c('T2Belief', 'T2Action')))),
      kdeT2 = list(kde.boundary(x=matT2, xmin=c(0,0), xmax=c(1,1), boundary.kernel="linear", gridsize = c(res,res)))
    ) %>%
    ungroup() %>%
    arrange(vignette, signal)
}

plot_densities <- function(d_density2d){
  pdf(file = 'Figures/densities.pdf', width = 12, height = 6)
  for (i in 1:nrow(d_density2d)){
    title = paste0(d_density2d$vignette[i], ' ', d_density2d$signal[i])
    par(mfrow=c(1,2), oma = c(0, 0, 2, 0))
    plot(d_density2d$kdeT1[[i]], display='persp', theta=50, phi=20, main = 'T1')
    plot(d_density2d$kdeT2[[i]], display='persp', theta=50, phi=20, main = 'T2')
    mtext(title, outer = T)
  }
  dev.off()
  return('Figures/densities.pdf')
}

ggplot_densities <- function(d_density2d){
  pdf(file = 'Figures/ggdensities.pdf', width = 12, height = 6)
  for (i in 1:nrow(d_density2d)){
    print(ggT1T2(d_density2d[i,]))
  }
  dev.off()
  return('Figures/ggdensities.pdf')
}

ggT1T2 <- function(d){
  subtitle = d$vignette[[1]]
  title = as.character(d$signal[[1]])
  ggkde(d$kdeT1[[1]]) + (ggkde(d$kdeT2[[1]]) + theme(axis.title.y = element_blank(), axis.text.y = element_blank())) +
    plot_annotation(
      title = title,
      subtitle = subtitle,
      tag_levels = '1',
      tag_prefix = 'T'
    ) +
    plot_layout(guides = 'collect') &
    theme(legend.position='right') # , plot.margin = margin(0,0,0,0)
}

ggkde <- function(mkde){

  varnms <- dimnames(mkde$x)[[2]]
  d <- as_tibble(mkde$x)

  rows <- length(mkde$eval.points[[1]])
  cols <- length(mkde$eval.points[[2]])

  d2 <- tibble(
    x = rep(mkde$eval.points[[1]], times = rows),
    y = rep(mkde$eval.points[[2]], each = cols),
    z = c(mkde$estimate)
  )

  ggplot() +
    geom_raster(data = d2, aes(x, y, fill = z), show.legend=F) +
    geom_count(data = d, aes(.data[[varnms[1]]], .data[[varnms[2]]]), fill = 'white', colour = 'black', shape = 21) +
    scale_fill_viridis_c(option = 'B') +
    scale_size_area(limits = c(1, 15)) +
    labs(x = '\nBelief', y = 'Action\n') +
    theme_minimal(15)
}


# Model comparison plot ---------------------------------------------------

compare_plot <- function(mquasi, mboot, mfrac, title){

  df_mquasi <- mquasi[c(1,2,6,7)]

  df_mboot <-
    as_tibble(mboot[,c(1:3)]) %>%
    mutate(
      term = rownames(mboot)
    ) %>%
    rename(
      conf.low = `boot 2.5%`,
      conf.high = `boot 97.5%`
    ) %>%
    relocate(term, .before = 1)

  mfracSE <- sqrt(diag(mfrac$p.var))
  df_mfrac <-
    tibble(
      term = names(mfrac$p),
      estimate = mfrac$p,
      conf.low = estimate - 2*(mfracSE),
      conf.high = estimate + 2*(mfracSE)
    ) %>%
    mutate(
      term = ifelse(term == 'INTERCEPT', '(Intercept)', term)
    )

  d <-
    bind_rows(glm = df_mquasi, bootstrap = df_mboot, fractional = df_mfrac, .id = 'Model') %>%
    mutate(
      term = factor(term, levels = rev(df_mquasi$term))
    )

  ggplot(d, aes(estimate, term, xmin = conf.low, xmax = conf.high, colour = Model, shape = Model)) +
    geom_pointrange(position = position_dodge(width = 0.5)) +
    guides(colour = guide_legend(reverse = T), shape = guide_legend(reverse = T)) +
    labs(title=title, y = '') +
    theme_minimal(15)
}

# Feedback ----------------------------------------------------------------

feedback <- function(d){

  nocomment <- c(
    'no',
    'none',
    'nothing',
    'n/a',
    'na',
    'n/a - thanks',
    'n/a- thanks',
    'no comments',
    'no feedback',
    'nope',
    '-',
    "i don't have any additional feedback thank you",
    "i do not have any additional comments",
    "i have no additional feedback",
    "i have none",
    "none thank you",
    'no additional feedback by me',
    'no comment',
    'no comments at the current time',
    'no comments or feedback',
    "no comments thanks for asking though ;)",
    'no comments!',
    'no comments everything went well thank you',
    'no feedback about this study',
    'no feedback, but good luck woth the project!',
    'no thanks',
    'none, thanks',
    'nothing comes to mind',
    'nothing else',
    'nothing whatsoever',
    'xxx',
    'none at this time, thank you',
    "none, but thanks for asking for feedback!",
    "no, i wish you well with your study",
    "no issues",
    "no problems"
  )

  uninformative <- c(
    'cool beans',
    'happy holidays!!!',
    'stay safe out there',
    'best of luck in your research!',
    'this survey is very satisfaction'
  )

  goodstudy <- c(
    "just that everything was lucid and clearly presented",
    "i appreciate studies like this one, thank you",
    "thank you for this important study i hope i can participate in more in the future",
    "everything ran smoothly have a wonderful day!" ,
    "everything ran well i hope you find my input helpful",
    "loved this study, best of luck in your research!",
    "i had zero problems with the survey either technically or in understanding the questions good job! it was well done thank you for the opportunity",
    "everything was perfect and clear i wrongly input my partner worker id in the previous page this is my worker id:a2rwm6ybcomwrq  can you kindly consider this worker id",
    "everything worked fine, no problems have a good day!",
    "excellent survey i've enjoyed a lot",
    "this study was such a nice making experience",
    "good study,thanks for giving this opportunity for me",
    "i like this task  because i imagine i am in the story  that feeling is good experience for my life",
    "i love doing surveys that help me express myself",
    "good survey and i learnt about everything in this survey",
    "as like such good one survey",
    "everything was clear and worked smoothly",
    "i like the story",
    "i like this story very much"
  )

  interestingstudy <- c(
    "i like this study it was more interesting to make decision",
    "very interesting and i hope i give my best in the task without any distractions",
    "interesting and enjoyable thanks for the opportunity to contribute to your research best wishes, r",
    "i found this study to be very interesting and thought provoking thank you for the opportunity to participate in it",
    "this was an interesting study and leaves me thoughtful about my perspective good luck with your research thank you",
    "this was a very interesting study , it made me think and reflect a lot !",
    "i think participating in this study was important and i would like to participate in more",
    "this study was very interesting to work",
    "i thought the story was very well done and interesting thank you for an informative study",
    "the story is very interesting and make the survey feels good",
    "this study was such a nice decision making experience every think was very interesting and smart thoughts",
    "it is an interesting study i put myself in that situation and decide accordingly",
    "it is the very interesting and easy to the survey",
    "it is the very interesting in the survey",
    "the story in the study was interesting :)",
    "this study gives me lesson on making wise decision",
    "i like this survey very much it is very interesting to do please give more survey like this",
    "it was quite an engaging study"
  )

  thanks <- c(
    "thanks for the opportunity to participate, good luck with your research and stay safe!",
    "thank you for allowing me to participate",
    "thank you for letting me participate",
    "thank you, i enjoyed this study",
    "thanks good luck with your work"
  )

  study_criticism <- c(
    "please increase the pay",
    "the questionnaires seemed to be repetitive",
    "the sliders are not resetting on every page that made me a little confused",
    "if there were more details then i'd be able to make a better decision before having to see video evidence",
    'there is no reason to ask how many sons or daughters one has "do you have any children?" "yes", "no" that is all that needs to be asked',
    "this was kind of depressing, so you might want to add that little warning you normally do for content warnings about things about abuse, or if it's there, i don't think it was displayed by itself enough to notice",
    "i found it odd that my theoretical niece had fancy belongings after a house fire",
    "can’t imagine how this teenage girl could possibly have a video of bro-in-law groping her, seems implausible, but the text stated that she did have it, so i changed my view accordingly - thanks",
    "scenario was somewhat difficult to understand",
    'people don\'t like being treated like children cut your "attention checks" down significantly you just come across as extremely paranoid',
    "those attention checks were ridiculous",
    "hello, just wanted to say that i did not miss that attention check at the end of the survey before demographics the slider however seemed to be broken and i couldn't move it! i tried refreshing the page and using the keyboard instead of the mouse but it seemed stuck i hope this does not cause a rejection please check the slider for future turkers",
    "the last attention check seemed unnecessary",
    "just a minor comment, but you may receive some false positives with regards to the last attention check question qualtrics sometimes fails to register a response and reusing the last the question for the check may prompt participants to think that their response was not recorded at first glance at first glance, leading them to try clicking again i think changing the question text may be a better filter as it would still require careful reading but it does not leave the possibility of a participant thinking that there was a qualtrics error"
  )

  study_comment <- c(
    "the survey is very usefull",
    "thank you for this meaningful study!",
    'thank you for including the most accurate demographic descriptor of my current relationship than i have seen in any other surveys so frustrating to just be classified as simply "divorced" when in a long-term unmarried relationship after a divorce awesome job!',
    "good study on how we feel about evidence",
    "very interesting study overall thanks for conducting research on important topics such as this stay safe during this corona pandemic and keep up the great research",
    "very difficult survey",
    'i think you should add the following question in future surveys" how many granddaughters/grandsons do you have',
    "the questons are too short it was good",
    "the survey is clear and interesting there are different options that could be considered if one thinks that the brother in law should not be kicked out the options include having a talk with the brother in law, threatening to kick him out if it happens again, etc what option would participants select?",
    "i hope i didn't miss anything on the ac's, thank you for letting me take this!",
    "i think i may have missed an attention check toward the end, i am not sure",
    "attention checking questions are easy to answer",
    "i accidentally skipped an attention check because my internet is going so slow, i noticed it but it was too late to stop the next page loading",
    "it is easy to understand the story to answer the attention question and it was very interesting"
  )

  vignette_endorsement <- c(
    "it is like a current situation in every family",
    "real life",
    "this survey makes sense with the place of the word",
    "this survey makes sense with me",
    "the study is very good and describes an even which is happening frequently in india",
    "this survey just like my family two daughters are same as this survey like",
    "many family ,members have this type of troubles",
    "dowry is a major problems to women, and this situation happened to many family",
    "i think this was a great study i actually kicked my boyfriend out because of my daughters accusations she did get depressed and i will always believe my child",
    "this kind of thing did happen to my daughter and i it was horrible","i went through this exact scenario as a kid i wound up homeless",
    "that hit really close to home i tried to imagine myself as the person in the scenario outlined, but i think a lot of me got into it and that affected my feelings and decision-making",
    "thanks for asking challenging, thoughtful questions sexual abuse in families is more prevalent than many people think",
    "i was in this situation growing up no one believed me, not even social workers but i was very much telling the truth and it destroyed me i never got trusted professional help with it and for it today i am a barely functioning adult with depression and other emotional problems"
  )

  vignette_comment <- c(
    "dowry system needs to be abolished women should be made self-dependent, they should not be using their parents money if they want to get settled with the person they love, they should mutually share all the expenses",
    "man who wants more money really wants the money not the daughter",
    "the beginning of the survey said the event didn't happen, but then there's video that it did?",
    "this was an interesting scenario overall i honestly do not know how i would react if my partner started hitting out daughter",
    "this would depend more on you and your daughter's relationship my real daughter would never lie about this, so i would have believed her from the beginning",
    "the moment my daughter's grades dropped i would start considering getting rid of the partner no partner is worth ruining a person's life",
    "i would probably have set up a hidden camera in this situation to see what is going on when i am not around",
    "very disturbing to imagine myself in this situation!!",
    "i would have probably have been keeping a closer eye on the brother-in-law and my daughter, even having her sleep in the same room as me while they were there at her first accusation even if i was doubting she was telling the truth i wouldn't risk someone potentially harming her, and i know this has occurred many times before when strange men are brought into a home with other children",
    "i'd ask my mom to take my daughter for a bit",
    "i am not sure hat i would throw him out right away, but i would investigate 100%! also, i would put cams in every room of the home without letting him know so that i could see if he did do anything and i can get it on cam, i would also have him arrested i would also explain to my daughter that, crying wolf can make it so that people are concerned about whether you can be trusted later, too i would also tell her to come get me immediately, take video, etc to help me research"
  )

  decision_explanation <- c(
    "actually my decision is the man who really loves the woman does not expect money from her except her pure love  so it seems unfair to me that getting dowry for marriages i strongly denied that concept in my culture",
    "i questioned the student's honest throughout because she has exaggerated before it's tough to judge a person as honest when they have not been honest before",
    "unfortunate scenario with the star player, but to me, people are innocent until proven guilty",
    "this was interesting, as i thought i was more confident standing by women who accuses others or rape, but this story shows otherwise i should've stood by her at the beginning but chose not to because of her attitude and behavior this study reminded me that that should not matter when it comes to sexual assaults accusations",
    "i made my decision by clear evidence seriously, i concerned this study",
    "this definitely was a bit challenging! however, i err on the side of victims, because bad things can still happen to bad people, and i have never really seen someone putting out a rape accusation when it could harm their own career unless the situation is serious -- usually it's the exact opposite that occurs",
    "i know we are always supposed to believe the victim, but without any other information that what i had on hand, i'd make the same decision again",
    "without evidence it hard to believe someone",
    "the behavior and personality of the star athlete made it hard to believe her allegation",
    "i know that some children would lie to get what they wanted if my daughter came to me and said that someone i was with hit her there would be a little doubt in my mind but i would pack his stuff immediately and kick him out i don't think my daughter would lie about an issue like that, and i definitely wouldn't take any chances",
    "i would want to believe my daughter and my partner, but with the evidence the daughter took, i'd believe her over him and most likely kick him out to protect her",
    "interesting hit, thanks i'd always err on the side of caution and in the initial scenario, even if i wasn't fully sure she was telling the truth, i'd kick the brother in law out i'd rather be wrong than let him stay if he were actually molesting her",
    "despite of how passive aggressive my daughter is, any allegation of that nature would always warrant a reaction of my part i believe there is always truth behind bold allegation so that nature i would definitely kicked my brother in law only from the beginning, reason why i put the slider at 72 i would have asked my sister and niece to stay there should be a explanation box as to why the rating one gave to further explain",
    "considering the daughter's past incidences of lying, the video evidence would be necessary for me to believe her",
    "yes thank you my sister and her daughter could stay they be welcome her husband on the other hand would have been long gone he could have went to my mothers if she though he was telling the truth also in that circumstance i rather believe my daughter"
  )

  T3reaction <- c(
    "okay, now i would want to beat the snot out of that coach for all he put her through and for all the stress he put me through",
    "that last part was surprising",
    "i feel bad about not believing my fictional daughter",
    "that was a surprising twist to the story i thought the star player was lying until the video",
    "guess i should have believed her!",
    "i feel bad because i chose love over the child in the beginning",
    "if there was video proof of my girlfriend hitting my daughter than i would almost certainly break up with her",
    "i feel very guilty on not believing my hypothetical daughter the first time if this ever happens to me i will side with my child first thank you for the eye opener",
    "had me fooled for a little bit",
    "well now i feel awful for not believing my fake daughter!",
    "that twist in the scenario really messed with my head i feel bad about not believing the girl that was my daughter in the scenario"
  )

  d %>%
    mutate(
      Feedback = str_squish(Feedback),
      Feedback_original = Feedback,
      Feedback = str_remove_all(Feedback, '\\.'),
      Feedback = str_to_lower(Feedback),
      Feedback_raw = Feedback,
      Feedback_wc = str_count(Feedback, '\\w+'),
      Feedback2 = case_when(
        Feedback %in% nocomment ~ NA_character_,
        # str_detect(Feedback, "attention|ac\\'s") ~ 'Attention_check',
        Feedback %in% uninformative ~ 'uninformative',
        Feedback_wc <= 5 & str_detect(Feedback, c('good|great|nice|excellent|wonderful|fine|amazing|well done')) ~ 'good',
        Feedback %in% goodstudy ~ 'good',
        Feedback_wc <= 5 & str_detect(Feedback, c('thank')) ~ 'thanks',
        Feedback %in% thanks ~ 'thanks',
        Feedback_wc <= 6 & str_detect(Feedback, c('interest|intersting')) ~ 'interesting',
        Feedback %in% interestingstudy ~ 'interesting',
        TRUE ~ Feedback
      ),
      Feedback_wc = str_count(Feedback2, '\\w+'),
      Feedback3 = case_when(
        is.na(Feedback2) ~ 'None',
        Feedback2 %in% c('good', 'interesting', 'thanks', 'uninformative') ~ 'Generic',
        # Feedback2 == 'Attention_check' ~ 'Attention check',
        TRUE ~ 'Informative'
      ),
      Feedback3 = factor(Feedback3, levels = c('None', 'Generic', 'Informative'))
    ) %>%
    mutate(
      Informative = case_when(
        Feedback3 != 'Informative' ~ NA_character_,
        Feedback_raw %in% study_criticism ~ 'Study criticism',
        Feedback_raw %in% study_comment ~ 'Study comment',
        Feedback_raw %in% vignette_endorsement ~ 'Vignette endorsement',
        Feedback_raw %in% vignette_comment ~ 'Vignette comment',
        Feedback_raw %in% decision_explanation ~ 'Decision explanation',
        Feedback_raw %in% T3reaction ~ 'T3 reaction',
        TRUE ~ 'Failed match'
      )
    )
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
