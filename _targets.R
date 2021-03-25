library(targets)
library(tarchetypes)

# Define custom functions and other global objects.

source("R/functions.R")

# Set target-specific options such as packages.
tar_option_set(
  packages = c("signalingdata2018", "signaling2020data", "car", "hagenutils", "visreg", "effects", "glmmboot", "broom", "mediation", "gglm", "gt", "ggforce", "dplyr", "tidyr", "purrr", "ggplot2", "forcats", "stringr"),
  imports = c("signalingdata2018", "signaling2020data")
  )

# List of target objects
list(

# Data preparation --------------------------------------------------------

  tar_target(signalingdata2018, signalingdata2018::signalingdata2018),

  tar_target(signaling2020, signaling2020data::signaling2020),

  tar_target(
  d_tmp,
    signaling2020 %>%
    dplyr::filter(CompleteSurvey) %>%
    dplyr::mutate(
      id = MTurkID,
      across(.cols = c(contains('Belief'), contains('Action'), contains('Divide')), ~.x/100),
      signal = ordered(signal, levels = c('Verbal request', 'Crying', 'Mild depression', 'Depression', 'Suicide attempt')),
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
    d_thwarted,
    d %>%
      dplyr::filter(vignette == 'Thwarted marriage')
  ),


# Power -------------------------------------------------------------------

  tar_target(
    power_curve,
    pwr_curve(signalingdata2018)
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
      m7 = "T2Divide ~ T1Divide * signal",
      m8 = "T2Divide ~ T1Divide + Age * signal",
      m9 = "T2Jealous ~ T1Jealous * signal",
      m10 = "T2Devious ~ T1Devious + signal",
      m11 = "T2Angry ~ T1Angry + signal",
      m12 = "T2Belief ~ T1Belief + signal * Sex",
      m13 = "T2Belief ~ T1Belief + vignette * Sex",
      m14 = "T2Action ~ T1Action + signal * Sex",
      m15 = "T2Action ~ T1Action + signal + vignette * Sex",
      m16 = "T2Belief ~ T1Belief + signal * Age",
      m17 = "T2Action ~ T1Action + signal * Age",
      m18 = "T2Belief ~ T1Belief + signal + vignette * Age",
      m19 = "T2Action ~ T1Action + signal + vignette * Age",
      m20 = "T2MentallyIll ~ T1MentallyIll + signal",
      m21 = "T2MentallyIll ~ T1MentallyIll + signal*vignette",
      m22 = "T2Belief ~ T1Belief * signal + PC1emotionT1 + PC2emotionT1",
      m23 = "T2Action ~ T1Action * signal + PC1emotionT1 + PC2emotionT1",
      m24 = "T2Belief ~ T1Belief + Age + Sex * signal + vignette",
      m25 = "T2Action ~ T1Action + Age + Sex + signal + vignette",
      m26 = 'T3Action ~ T2Action + vignette',
      m27 = 'T1MentallyIll ~ vignette'
    )
  ),

  tar_target(
    models,
    fit_models(d, model_formulas, family = 'quasibinomial')
  ),

  tar_target(
    models_prereg,
    fit_models(d, model_formulas[c('m1', 'm1b', 'm3', 'm3b')], family = 'gaussian')
  ),


# Bootstrapped models -----------------------------------------------------

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

# Effects plots -----------------------------------------------------------

  tar_target(
    effectsPlots,
    effect_plots(models$Model, d)
  ),

# Mediation models --------------------------------------------------------

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
      data = d_thwarted,
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
