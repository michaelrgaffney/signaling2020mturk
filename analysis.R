

#' ## Vignettes

#' **Basketball coach**: Star player claims she was sexually assaulted by coach (but might just want more playing time)
#'
#' **Romantic partner**: Your young daughter claims your new romantic partner physically abused her (but might just want to split you up)
#'
#' **Brother-in-law**: Your teenage daughter claims your sister's husband sexually assaulted her (but might just be jealous of your sister's popular daughter)
#'
#' **Thwarted marriage**: Marriagable daughter refuses to marry available man (but might just want a bigger dowry)

#+ warning=F, message=F

library(signaling2020data)
library(tidyverse)
library(broom)
library(effects)
library(visreg)
library(car)
library(ggbiplot)
library(ggforce)
library(gt)
library(hagenutils)
library(mediation)

d <-
  signaling2020 %>%
  dplyr::filter(CompleteSurvey) %>%
  mutate(
    signal = ordered(signal, levels = c('Verbal request', 'Crying', 'Mild depression', 'Depression', 'Suicide attempt'))
  )

# Thwarted marriage only, for Divide var
d2 <-
  d %>%
  dplyr::filter(vignette == 'Thwarted marriage')

# Belief ------------------------------------------------------------------

# contrasts(d$signal, how.many = 2) <- contr.poly(5)

m1 <- lm(T2Belief ~ T1Belief * signal, d)
summary(m1)
Anova(m1, type = 3)

p <- visreg(m1, xvar='signal', by = 'T1Belief', partial = F, rug = F, gg = T)
p + theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Signals vs belief in each vignette

m1b <- lm(T2Belief ~ T1Belief + signal*vignette, d)
# summary(m1b)
Anova(m1b, type = 3)

p <- visreg(m1b, xvar='signal', by = 'vignette', partial = F, rug = F, gg = T)
p + theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Plot of raw data

d4 <-
  d %>%
  dplyr::select(MTurkID, signal, vignette, starts_with('T1'), starts_with('T2')) %>%
  pivot_longer(starts_with('T')) %>%
  separate(col=name, into = c('Time', 'Variable'), sep = 2) %>%
  dplyr::rename(
    id = MTurkID
  )

d4b <-
  d4 %>%
  dplyr::filter(Variable == 'Belief')

d4c <-
  d4b %>%
  group_by(signal, vignette, Time) %>%
  dplyr::summarise(value = mean(value))

ggplot() +
  geom_line(data = d4b, aes(Time, value, group = id), alpha = 0.1) +
  geom_line(data = d4c, aes(Time, value, group = 1), colour = 'red') +
  facet_grid(vignette~signal) +
  labs(x = '', y = 'Belief\n') +
  theme_minimal() + theme(strip.text.y = element_text(angle = 0, hjust = 0))

# Action ------------------------------------------------------------------

m2 <- lm(T2Action ~ T1Action * signal, d)
# summary(m2)
p <- visreg(m2, xvar='signal', by = 'T1Action', partial = F, rug = F, gg = T)
p + theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Signals vs actions in each vignette

m2b <- lm(T2Action ~ T1Action + signal*vignette, d)
# summary(m2b)
Anova(m2b, type = 3)

p <- visreg(m2b, xvar='signal', by = 'vignette', partial = F, rug = F, gg = T)
p + theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Effect of second signal indicating victim is telling the truth
m2c <- lm(T3Action ~ T2Action * signal, d)
Anova(m2c, type = 3)

p <- visreg(m2c, xvar = 'T2Action', by='signal', partial = F, rug = F, gg = T)
p + theme_bw()

m2d <- lm(T3Action ~ T2Action * vignette + signal, d)
Anova(m2d, type = 3)

p <- visreg(m2d, xvar = 'T2Action', by='vignette', partial = F, rug = F, gg = T)
p + theme_bw()

# Plot of raw data

d4d <-
  d4 %>%
  dplyr::filter(Variable == 'Action')

d4e <-
  d4d %>%
  group_by(signal, vignette, Time) %>%
  dplyr::summarise(value = mean(value))

ggplot() +
  geom_line(data = d4d, aes(Time, value, group = id), alpha = 0.1) +
  geom_line(data = d4e, aes(Time, value, group = 1), colour = 'red') +
  facet_grid(vignette~signal) +
  labs(x = '', y = 'Action\n') +
  theme_minimal() + theme(strip.text.y = element_text(angle = 0, hjust = 0))

# Divide (thwarted marriage only) -----------------------------------------

m13 <- lm(T2Divide ~ T1Divide * signal, d2)
Anova(m13)

p <- visreg(m13, xvar='signal', by = 'T1Divide', partial = F, rug = F, gg = T)
p + theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Exploratory

m13b <- lm(T2Divide ~ T1Divide + Age * signal, d2)
Anova(m13b)

p <- visreg(m13b, xvar='signal', by = 'Age', partial = F, rug = F, gg = T)
p + theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Jealous -----------------------------------------------------------------

m7 <- glm(T2Jealous ~ T1Jealous + signal, family = binomial, d)
Anova(m7, type = 3)

p <- visreg(m7, xvar='signal', partial = F, rug = F, gg = T, scale = 'response')
p + theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Devious -----------------------------------------------------------------

m6 <- glm(T2Devious ~ T1Devious + signal, family = binomial, d)
Anova(m6, type = 3)

p <- visreg(m6, xvar='signal', partial = F, rug = F, gg = T, scale = 'response')
p + theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Anger -----------------------------------------------------------------

m8 <- glm(T2Angry ~ T1Angry + signal, family = binomial, d)
Anova(m8, type = 3)

p <- visreg(m8, xvar='signal', partial = F, rug = F, gg = T, scale = 'response')
p + theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Sex ---------------------------------------------------------------------

# Belief vs sex
m4 <- lm(T2Belief ~ T1Belief + signal*Sex, d)
Anova(m4, type = 3)

p <- visreg(m4, xvar='Sex', by = 'signal', partial = F, rug = F, gg = T, scale = 'response')
p + theme_bw()

# Action vs sex
m5 <- lm(T2Action ~ T1Action + signal*Sex, d)
Anova(m5, type = 3)

p <- visreg(m5, xvar='Sex', by = 'signal', partial = F, rug = F, gg = T, scale = 'response')
p + theme_bw()


# Age ---------------------------------------------------------------------

# Belief vs Age
m9 <- lm(T2Belief ~ T1Belief + signal * Age, d)
Anova(m9, type = 3)

p <- visreg(m9, xvar='Age', by = 'signal', partial = F, rug = F, gg = T, scale = 'response')
p + theme_bw()

# Action vs Age
m10 <- lm(T2Action ~ T1Action + signal * Age, d)
Anova(m10, type = 3)

p <- visreg(m10, xvar='Age', by = 'signal', partial = F, rug = F, gg = T, scale = 'response')
p + theme_bw()

# Vignette vs Age
m11 <- lm(T2Belief ~ T1Belief + signal + vignette*Age, d)
Anova(m11, type = 3)

p <- visreg(m11, xvar='Age', by = 'vignette', partial = F, rug = F, gg = T, scale = 'response')
p + theme_bw()

m12 <- lm(T2Action ~ T1Action + signal + vignette*Age, d)
Anova(m12, type = 3)

p <- visreg(m12, xvar='Age', by = 'vignette', partial = F, rug = F, gg = T, scale = 'response')
p + theme_bw()

# Perceived mental illness ------------------------------------------------

m3 <- glm(T2MentallyIll ~ T1MentallyIll + signal, family = binomial, d)
Anova(m3, type = 3)

p <- visreg(m3, xvar='signal', partial = F, rug = F, gg = T, scale = 'response')
p + theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Mental illness by vignette

m3b <- glm(T2MentallyIll ~ T1MentallyIll + signal*vignette, family = binomial, d)
Anova(m3b, type = 3)

p <- visreg(m3b, xvar='signal', by = 'vignette', partial = F, rug = F, gg = T, scale = 'response')
p + theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Perceived emotions ------------------------------------------------------

#+ warning=F, message=F, fig.width=10

# Before signal
T1emo <-
  d %>%
  dplyr::select(T1Angry:T1Violated)

hagenheat(t(T1emo))

m_pca_t1emo <- prcomp(T1emo)
pca_loadings_plot(m_pca_t1emo)
ggbiplot(m_pca_t1emo, groups = d$vignette, ellipse = T)


# After signal
T2emo <-
  d %>%
  dplyr::select(T2Angry:T2Violated)
d$id <- d$MTurkID
x <- as.data.frame(t(T2emo))
names(x) <- d$id
rownames(x) <- names(T2emo)
hagenheat(x, ann_col = d[c('id', 'signal')])

m_pca_t2emo <- prcomp(T2emo)
pca_loadings_plot(m_pca_t2emo)
ggbiplot(m_pca_t2emo, groups = d$signal, ellipse = T)


# Emotion change ----------------------------------------------------------

d3 <-
  d %>%
  mutate(
    id = MTurkID,
    T1LowMood = T1Depressed + T1Distressed + T1Sad,
    T1Manipulative = T1Devious + T1Jealous,
    T2LowMood = T2Depressed + T2Distressed + T2Sad,
    T2Manipulative = T2Devious + T2Jealous
  ) %>%
  dplyr::select(
    id,
    vignette,
    signal,
    T1LowMood:T2Manipulative
  )

mean_emotions <- function(d){
  d %>%
  group_by(vignette, signal) %>%
  dplyr::summarise(
    T1LowMood_mean = mean(T1LowMood),
    T1Manipulative_mean = mean(T1Manipulative),
    T2LowMood_mean = mean(T2LowMood),
    T2Manipulative_mean = mean(T2Manipulative),
    # T1LowMood_sd = sd(T1LowMood),
    # T1Manipulative_sd = sd(T1Manipulative),
    # T2LowMood_sd = sd(T2LowMood),
    # T2Manipulative_sd = sd(T2Manipulative),
    .groups='drop'
    )
}

d3_full <- mean_emotions(d3)

n <- nrow(d3)
boot_emotions <- function(i){
  mean_emotions(d3[sample.int(n, replace = T),])
}

d3_boot <- map_df(1:500, boot_emotions)

ggplot() +
  geom_segment(data = d3_boot, aes(x = T1LowMood_mean, y = T1Manipulative_mean, xend = T2LowMood_mean, yend = T2Manipulative_mean, colour=vignette), alpha = 0.02, arrow = arrow(length = unit(3, "mm"))) +
  geom_segment(data = d3_full, aes(x = T1LowMood_mean, y = T1Manipulative_mean, xend = T2LowMood_mean, yend = T2Manipulative_mean, colour=vignette), alpha = 1, size = 1, arrow = arrow(length = unit(3, "mm"))) +
  facet_wrap(~signal) +
  labs(title = 'Change in mean emotion from T1 to T2', x = '\nLow mood', y = 'Manipulative\n') +
  theme_bw()


# Regression table --------------------------------------------------------

models <- list(
  'Belief' = m1,
  'Belief by vignette' = m1b,
  'Action' = m2,
  'Action by vignette' = m2b
)

model_stats <- map_df(models, ~tidy(., conf.int = T), .id = 'Model')

model_summary <-
  map_dfr(models, ~glance(.)) %>%
  signif(2) %>%
  str_glue_data('N={nobs}; Rsq={r.squared}; Adj.Rsq={adj.r.squared}; F({df},{df.residual})={statistic}; p={p.value}')

model_stats %>%
  gt(groupname_col = 'Model') %>%
  fmt_number(c(3:8)) %>%
  tab_footnote(model_summary, cells_row_groups())


# Mediation Model ---------------------------------------------------------

# Mediator model: Belief in need t2 ~ Signal + Belief in need t1
# Outcome model: Likelihood of help t2  ~ Signal + Belief in need t2 + Likelihood of help t1 + Belief in need t1

d_mediate <-
  d %>%
  dplyr::filter(
  signal %in% c(
    'Verbal request',
    'Depression')
) %>%
  mutate(
    signal = factor(signal, levels = c('Verbal request', 'Depression'))
  )

mmediator <- lm(T2Belief ~ signal + T1Belief, d_mediate)
mout <- lm(T2Action ~ signal + T2Belief + T1Action + T1Belief, d_mediate)

out <- mediate(mmediator, mout,treat = "signal", mediator = "T2Belief")

summary(out)
plot(out)

# mediation_model <- mediate(model.m, model.y, treat = 'signal', mediator = 'delta_needs_money', boot = T)

d_mediate <-
  d %>%
  dplyr::filter(
    signal %in% c(
      'Depression',
      'Suicide attempt')
  ) %>%
  mutate(
    signal = factor(signal, levels = c('Depression', 'Suicide attempt'))
  )

mmediator <- lm(T2Belief ~ signal + T1Belief + vignette + Sex, d_mediate)
mout <- lm(T2Action ~ signal + T2Belief + T1Action + T1Belief + vignette + Sex, d_mediate)

out <- mediate(mmediator, mout,treat = "signal", mediator = "T2Belief")

summary(out)
plot(out)

# to do: moderated mediation model

d_mediate <-
  d %>%
  dplyr::filter(
    signal %in% c(
      'Verbal request',
      'Depression'),
    vignette != "Thwarted marriage"
  ) %>%
  mutate(
    signal = factor(signal, levels = c('Verbal request', 'Depression'))
  )

mmediator <- lm(T2Belief ~ signal + T1Belief + vignette, d_mediate)
mout <- lm(T2Action ~ signal + T2Belief + T1Action + T1Belief + vignette, d_mediate)

out <- mediate(mmediator, mout, treat = "signal", mediator = "T2Belief")

summary(out)
plot(out)



d_mediate <-
  d %>%
  dplyr::filter(
    signal %in% c(
      'Depression',
      'Suicide attempt'),
    vignette != "Thwarted marriage",
    Sex == "Male"
  ) %>%
  mutate(
    signal = factor(signal, levels = c('Depression', 'Suicide attempt'))
  )

mmediator <- lm(T2Belief ~ signal + T1Belief + vignette, d_mediate)
mout <- lm(T2Action ~ signal + T2Belief + T1Action + T1Belief + vignette, d_mediate)

out <- mediate(mmediator, mout, treat = "signal", mediator = "T2Belief")

summary(out)
plot(out)
