library(targets)
library(tarchetypes)
library(future)
plan(multisession)

# Define custom functions and other global objects.

source("R/functions.R")

# Set target-specific options such as packages.
tar_option_set(
  packages = c("ks", "car", "margins", "hagenutils", "visreg", "effects", "glmmboot", "broom", "mediation", "frm", "gglm", "gt", "ggforce", "dplyr", "tidyr", "purrr", "ggplot2", "patchwork", "forcats", "stringr", "furrr"),
  imports = c("signalingdata2018", "signaling2020data")
  )

# List of target objects
list(

# Data preparation --------------------------------------------------------

  tar_target(
    signalingdata2018b,
    prep_pilotdata(signalingdata2018::signalingdata2018)
  ),

  tar_target(
    signaling2020,
    signaling2020data::signaling2020
  ),

  tar_target(
  d_tmp,
    signaling2020 %>%
    dplyr::filter(CompleteSurvey) %>%
    dplyr::mutate(
      id = MTurkID,
      across(.cols = c(contains('Belief'), contains('Action'), contains('Divide')), ~.x/100),
      signal = ordered(signal, levels = c('Verbal request', 'Crying', 'Mild depression', 'Depression', 'Suicide attempt')),
      vignette = factor(vignette, levels = c('Thwarted marriage', 'Basketball coach', 'Romantic partner', 'Brother-in-law')),
      T1LowMood = T1Depressed + T1Distressed + T1Sad,
      T1Manipulative = T1Devious + T1Jealous,
      T2LowMood = T2Depressed + T2Distressed + T2Sad,
      T2Manipulative = T2Devious + T2Jealous
    )
  ),

# PCA ---------------------------------------------------------------------

  tar_target(
    mpcaT1,
    d_tmp %>%
      dplyr::select(T1Angry:T1Violated) %>%
      prcomp()
  ),
  tar_target(
    mpcaT2,
    d_tmp %>%
      dplyr::select(T2Angry:T2Violated) %>%
      prcomp()
  ),

# Final data --------------------------------------------------------------

  tar_target(
    d,
    d_tmp %>%
      mutate(
        PC1emotionT1 = -mpcaT1$x[,1],
        PC2emotionT1 = -mpcaT1$x[,2],
        PC1emotionT2 = -mpcaT2$x[,1],
        PC2emotionT2 = -mpcaT2$x[,2]
      )
  ),

  tar_target(
    d_India,
    d %>%
      dplyr::filter(vignette == 'Thwarted marriage')
  ),

  tar_target(
    d_US,
    d %>%
      dplyr::filter(vignette != 'Thwarted marriage') %>%
      mutate(vignette = fct_drop(vignette))
  ),

# Power -------------------------------------------------------------------

  tar_target(
    power_curve,
    pwr_curve(signalingdata2018b)
  ),

# Pilot results -----------------------------------------------------------

  tar_target(
    pilot_results,
    pilotResults(signalingdata2018b)
  ),

# Models ------------------------------------------------------------------

  tar_target(
    model_formulas,
    c(
      m1 = "T2Belief ~ T1Belief + signal",
      m1b = "T2Belief ~ T1Belief * signal",
      m2 = "T2Belief ~ T1Belief + signal * vignette",
      m3 = "T2Action ~ T1Action + signal",
      m3b = "T2Action ~ T1Action * signal",
      m4 = "T2Action ~ T1Action + signal * vignette",
      m5 = "T3Action ~ T2Action * signal",
      m6 = "T3Action ~ T2Action * vignette + signal",
      m9 = "T2Jealous ~ T1Jealous * signal",
      m10 = "T2Devious ~ T1Devious + signal",
      m11 = "T2Angry ~ T1Angry + signal",

      # Demographic models
      m12 = "T2Belief ~ T1Belief + vignette + signal + years_education + Income + Age + Sex", # Age, sex sig
      m13 = "T2Action ~ T1Action + signal + vignette + years_education + Income + Age + Sex", # Age, sex sig

      m14 = "T2Belief ~ T1Belief + vignette + Age + years_education + Income + signal * Sex", # Interaction not sig
      m15 = "T2Action ~ T1Action + vignette + Age + years_education + Income + signal * Sex", # Interaction not sig

      m16 = "T2Belief ~ T1Belief + vignette + Sex + years_education + Income + signal * Age", # Interaction not sig
      m17 = "T2Action ~ T1Action + vignette + Sex + years_education + Income + signal * Age", # Interaction not sig

      m18 = "T2Belief ~ T1Belief + signal + Age + years_education + Income + vignette * Sex", # Interaction not sig
      m19 = "T2Action ~ T1Action + signal + Age + years_education + Income + vignette * Sex", # Interaction not sig

      m24 = "T2Belief ~ T1Belief + signal + Sex + years_education + Income + vignette * Age", # Interaction not sig
      m25 = "T2Action ~ T1Action + signal + Sex + years_education + Income + vignette * Age", # Interaction not sig

      m28 = "T2Belief ~ T1Belief + signal + Age + Income + vignette + years_education * Sex", # Interaction not sig
      m29 = "T2Action ~ T1Action + signal + Age + Income + vignette + years_education * Sex", # Interaction not sig

      m30 = "T2Belief ~ T1Belief + signal + Sex + Income + vignette + years_education * Age", # Interaction not sig
      m31 = "T2Action ~ T1Action + signal + Sex + Income + vignette + years_education * Age", # Interaction not sig

            # Misc models
      m20 = "T2MentallyIll ~ T1MentallyIll + signal",
      m21 = "T2MentallyIll ~ T1MentallyIll + signal * vignette",
      m22 = "T2Belief ~ T1Belief * signal + PC1emotionT1 + PC2emotionT1",
      m23 = "T2Action ~ T1Action * signal + PC1emotionT1 + PC2emotionT1",
      m26 = 'T3Action ~ T2Action + vignette',
      m27 = 'T1MentallyIll ~ vignette'
    )
  ),

  tar_target(
    model_formulas_India,
    c(
      m7 = "T2Divide ~ T1Divide * signal",
      m8 = "T2Divide ~ T1Divide + Age * signal"
    )
  ),

  tar_target(
    models,
    fit_models(d, model_formulas, family = 'quasibinomial')
  ),

  tar_target(
    models_US,
    fit_models(d_US, model_formulas, family = 'quasibinomial')
  ),

  tar_target(
    models_India,
    fit_models(d_India, c(model_formulas_India), family = 'quasibinomial')
  ),

  tar_target(
    models_prereg,
    fit_models(d, model_formulas[c('m1', 'm1b', 'm3', 'm3b')], family = 'gaussian')
  ),


# Bootstrapped models -----------------------------------------------------

  tar_target(
    m1_boot,
    bootstrap_model(
      base_model = glm(T2Belief ~ T1Belief + signal, family = binomial, d),
      base_data = d,
      resamples = 999,
      parallelism = 'parallel'
    )
  ),

  tar_target(
    m3_boot,
    bootstrap_model(
      base_model = glm(T2Action ~ T1Action + signal, family = binomial, d),
      base_data = d,
      resamples = 999,
      parallelism = 'parallel'
    )
  ),

  tar_target(
    m1b_boot,
    bootstrap_model(
      base_model = glm(T2Belief ~ T1Belief * signal, family = binomial, d),
      base_data = d,
      resamples = 999,
      parallelism = 'parallel'
    )
  ),

  tar_target(
    m3b_boot,
    bootstrap_model(
      base_model = glm(T2Action ~ T1Action * signal, family = binomial, d),
      base_data = d,
      resamples = 999,
      parallelism = 'parallel'
    )
  ),

# Fractional regression models --------------------------------------------

  tar_target(
    m1_frac,
    fractional_model(models$Model$m1)
  ),

  tar_target(
    m3_frac,
    fractional_model(models$Model$m3)
  ),

# Effects plots -----------------------------------------------------------

  tar_target(
    effectsPlots,
    effect_plots(models$Model, d)
  ),

  tar_target(
    effectsPlots_US,
    effect_plots(models_US$Model, d_US)
  ),

  tar_target(
    effectsPlots_India,
    effect_plots_India(models_India$Model, d_India)
  ),

# Mediation models --------------------------------------------------------

  tar_target(
    med_prereg,
    signal_mediate(
      data=d,
      treat.value ='Depression',
      med_f = 'T2Belief ~ signal + T1Belief',
      out_f = 'T2Action ~ signal + T1Belief + T1Action + T2Belief',
      mediator = 'T2Belief',
      family = 'gaussian'
    )
  ),

  tar_target(
    med1,
    signal_mediate(
      data=d,
      treat.value = 'Depression',
      med_f = 'T2Belief ~ signal + T1Belief',
      out_f = 'T2Action ~ signal + T1Belief + T1Action + T2Belief',
      mediator = 'T2Belief'
    )
  ),
  tar_target(
    med1b,
    signal_mediate(
      data=d,
      treat.value = 'Depression',
      med_f = 'T2Belief ~ signal * T1Belief',
      out_f = 'T2Action ~ signal * T1Belief + T1Action + T2Belief',
      mediator = 'T2Belief'
    )
  ),
  tar_target(
    med2,
    signal_mediate(
      data=d,
      control.value = 'Depression',
      treat.value = 'Suicide attempt',
      med_f = 'T2Belief ~ signal * T1Belief',
      out_f = 'T2Action ~ signal * T1Belief + T1Action + T2Belief',
      mediator = 'T2Belief'
    )
  ),
  tar_target(
    med3,
    signal_mediate(
      data = d_India,
      treat.value = 'Depression',
      med_f = 'T2Belief ~ signal * T1Belief',
      out_f = 'T2Action ~ signal * T1Belief + T1Action + T2Belief',
      mediator = 'T2Belief'
    )
  ),
  tar_target(
    med4,
    signal_mediate(
      data=d,
      treat.value = 'Suicide attempt',
      med_f = 'T2Belief ~ signal + T1Belief',
      out_f = 'T2Action ~ signal + T1Belief + T1Action + T2Belief',
      mediator = 'T2Belief'
    )
  ),
tar_target(
  med4b,
  signal_mediate(
    data=d,
    treat.value = 'Suicide attempt',
    med_f = 'T2Belief ~ signal * T1Belief',
    out_f = 'T2Action ~ signal * T1Belief + T1Action + T2Belief',
    mediator = 'T2Belief'
  )
),

# T1 plot -----------------------------------------------------------------

  tar_target(
    plot_T1Belief_T1Action,
    T1Belief_Action_dist(d)
  ),


# ECDF plot ---------------------------------------------------------------

  tar_target(
    ecdf_plot,
    plot_ecdf(d)
  ),

# 2D densities ------------------------------------------------------------

  tar_target(
    d_density2d,
    density2d(d)
  ),

  tar_target(
    densityplot,
    plot_densities(d_density2d),
    format = "file"
  ),

  tar_target(
    ggdensityplot,
    ggplot_densities(d_density2d),
    format = "file"
  ),

# Emotion plots -----------------------------------------------------------

  tar_target(
    plot_emotions,
    emotion_plot(d)
  ),

  tar_target(
    dfT1emotion_vignette,
    T1emotion_vignette(d)
  ),

  tar_target(
  dfT1emotion_signal,
  T1emotion_signal(d)
),

  tar_target(
    dfT2emotion_signal,
    T2emotion_signal(d)
  ),

# Raw data plots ----------------------------------------------------------

  tar_target(
    plot_raw_beliefs,
    plot_raw_data(d, 'Belief')
  ),

  tar_target(
    plot_raw_actions,
    plot_raw_data(d, 'Action')
  ),

  tar_target(
    plot_raw_belief_action2d,
    plot_raw_data2d(d)
  ),

# Effect sizes ------------------------------------------------------------

  tar_target(
    effect_sizes,
    eff_sizes(d)
  ),

  tar_target(
    effect_sizes2,
    eff_sizes2(d)
  ),

# Paper -------------------------------------------------------------------

  tar_render(
    paper, 'Paper.rmd'
  )

)
