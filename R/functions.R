
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
    m_pilot_mentallyill = glm(MentallyIll ~ signal, family = binomial, d),
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
    geom_point(data = dmean, aes(T1Belief, T1Action), colour = 'black', shape = 3, size = 3) +
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
}

effect_plots_India <- function(models, data){
  list(
    m7plot = effect_plot(fit=models$m7, xvar='signal', by='T1Divide', ylbl='T2 Division', data=data),
    m8plot = effect_plot(fit=models$m8, xvar='Age', by='signal', ylbl='T2 Division', data=data)
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
    labs(x = '\nBelief/Action', y = '') +
    theme_minimal(15) +
    theme(strip.text.y = element_text(angle=0, hjust=0), legend.position = 'top')
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
