

#' ## Vignettes

#' **Basketball coach**: Star player claims she was sexually assaulted by coach (but might just want more playing time)
#'
#' **Romantic partner**: Your young daughter claims your new romantic partner physically abused her (but might just want to split you up)
#'
#' **Brother-in-law**: Your teenage daughter claims your sister's husband sexually assaulted her (but might just be jealous of your sister's popular daughter)
#'
#' **Thwarted marriage**: Marriageable daughter refuses to marry available man (but might just want a bigger dowry)

#+ warning=F, message=F

library(signaling2020data)
library(car)
# library(ggbiplot)
library(tidyverse)
library(broom)
library(effects)
library(visreg)
library(ggforce)
library(gt)
library(hagenutils)
library(mediation)
library(gglm)
library(glmmboot)
# https://cran.r-project.org/web/packages/ggside/

# Functions ---------------------------------------------------------------

theme_bw2 <-
  theme_bw(15) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Prepare data ------------------------------------------------------------

d <-
  signaling2020 %>%
  dplyr::filter(CompleteSurvey) %>%
  dplyr::mutate(
    across(.cols = c(contains('Belief'), contains('Action')), ~.x/100),
    signal = ordered(signal, levels = c('Verbal request', 'Crying', 'Mild depression', 'Depression', 'Suicide attempt'))
  )

# Thwarted marriage only, for Divide var
d2 <-
  d %>%
  dplyr::filter(vignette == 'Thwarted marriage')

plot_age_distribution <-
  ggplot(d, aes(Age, colour = vignette)) +
  geom_density() +
  labs(x = '\nAge', y = 'Density\n') +
  theme_bw(15)
plot_age_distribution

# Belief ------------------------------------------------------------------

plot_T1Belief_distributions <-
  ggplot(d, aes(T1Belief, colour = vignette)) +
  geom_density() +
  labs(x = '\nT1 Belief', y = 'Density\n') +
  theme_bw(15)
plot_T1Belief_distributions

# contrasts(d$signal, how.many = 2) <- contr.poly(5)

m1 <- glm(T2Belief ~ T1Belief * signal, family = quasibinomial, d)
summary(m1)
Anova(m1, type = 3)
gglm(m1)

# glmmboot

m1boot <- bootstrap_model(
  base_model = m1,
  base_data = d,
  resamples = 999,
  parallelism = 'parallel'
)

plot_belief <-
  visreg(m1, xvar='signal', by = 'T1Belief', partial = F, rug = F, gg = T, scale = 'response') +
  ylim(c(0, 1)) +
  labs(x = '', y = 'T2 Belief\n') +
  theme_bw2
plot_belief

# Signals vs belief in each vignette

m1b <- glm(T2Belief ~ T1Belief + signal*vignette, family = quasibinomial, d)
# summary(m1b)
Anova(m1b, type = 3)
gglm(m1b)

plot_belief_vignettes <-
  visreg(m1b, xvar='signal', by = 'vignette', partial = F, rug = F, gg = T, scale = 'response') +
  ylim(c(0, 1)) +
  labs(x = '', y = 'T2 Belief\n') +
  theme_bw2
plot_belief_vignettes

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

plot_raw_beliefs <-
  ggplot() +
  geom_line(data = d4b, aes(Time, value, group = id), alpha = 0.1) +
  geom_line(data = d4c, aes(Time, value, group = 1), colour = 'red') +
  scale_x_discrete(expand = expansion(mult=0.1)) +
  facet_grid(vignette~signal) +
  labs(x = '', y = 'Belief\n') +
  theme_minimal(15) +
  theme(
    strip.text.y = element_text(angle = 0, hjust = 0),
    panel.spacing.x = unit(1, 'lines')
  )
plot_raw_beliefs

# Action ------------------------------------------------------------------

plot_T1Action_distributions <-
  ggplot(d, aes(T1Action, colour = vignette)) +
  geom_density() +
  labs(x = '\nT1 Action', y = 'Density\n') +
  theme_bw(15)
plot_T1Action_distributions

plot_T1Belief_T1Action <-
  ggplot(d, aes(T1Belief, T1Action, colour = vignette)) +
  geom_density2d() +
  geom_jitter(alpha = 0.5, width = 0.01, height = 0.01) +
  geom_smooth(se=F, method = 'lm') +
  labs(x = '\nT1 Belief', y = 'T1 Action\n') +
  guides(colour = 'none') +
  facet_wrap(~vignette) +
  theme_minimal(15) +
  theme(axis.title.y = element_text(angle=0))
plot_T1Belief_T1Action

plot_T2Belief_T2Action <-
  ggplot(d, aes(T2Belief, T2Action, colour = signal)) +
  geom_density2d() +
  geom_jitter(alpha = 0.5, width = 0.01, height = 0.01) +
  geom_smooth(se=F, method='lm') +
  scale_colour_viridis_d(begin=0, end=0.75, option = 'A') +
  labs(x = '\nT2 Belief', y = 'T2 Action\n') +
  guides(colour='none') +
  facet_grid(vignette~signal) +
  theme_minimal(15) +
  theme(
    strip.text.y = element_text(angle = 0, hjust = 0),
    axis.title.y = element_text(angle=0)
    )
plot_T2Belief_T2Action

m2 <- glm(T2Action ~ T1Action * signal, family = quasibinomial, d)
summary(m2)
Anova(m2, type = 3)
gglm(m2)

# glmmboot
m2boot <- bootstrap_model(
  base_model = m2,
  base_data = d,
  resamples = 999,
  parallelism = 'parallel'
)

plot_action <-
  visreg(m2, xvar='signal', by = 'T1Action', partial = F, rug = F, gg = T, scale = 'response') +
  labs(x = '', y = 'T2 Action\n') +
  theme_bw2
plot_action

# Signals vs actions in each vignette

m2b <- glm(T2Action ~ T1Action + signal*vignette, family = quasibinomial, d)
# summary(m2b)
Anova(m2b, type = 3)
gglm(m2b)

plot_action_vignette <-
  visreg(m2b, xvar='signal', by = 'vignette', partial = F, rug = F, gg = T, scale='response') +
  ylim(c(0, 1)) +
  labs(x = '', y = 'T2 Action\n') +
  theme_bw2
plot_action_vignette

# Effect of second signal indicating victim is telling the truth
m2c <- glm(T3Action ~ T2Action * signal, family = quasibinomial, d)
Anova(m2c, type = 3)
gglm(m2c)

plot_T3_action <-
  visreg(m2c, xvar = 'T2Action', by='signal', partial = F, rug = F, gg = T, scale = 'response') +
  ylim(c(0, 1)) +
  labs(x = '\nT2 Action', y = 'T3 Action\n') +
  theme_bw2
plot_T3_action

m2d <- glm(T3Action ~ T2Action * vignette + signal, family = quasibinomial, d)
Anova(m2d, type = 3)
gglm(m2d)

plot_T3_action_vignette <-
  visreg(m2d, xvar = 'T2Action', by='vignette', scale = 'response', partial = F, rug = F, gg = T) +
  ylim(c(0, 1)) +
  labs(x = '\nT2 Action', y = 'T3 Action\n') +
  theme_bw(15)
plot_T3_action_vignette

# Indian sample only

plot_t2_t3 <-
  ggplot(d[d$vignette == 'Thwarted marriage',], aes(T2Action, T3Action)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("Thwarted marriage only")
plot_t2_t3

# Plot of raw data

d4d <-
  d4 %>%
  dplyr::filter(Variable == 'Action')

d4e <-
  d4d %>%
  group_by(signal, vignette, Time) %>%
  dplyr::summarise(value = mean(value))

plot_belief_raw <-
  ggplot() +
  geom_line(data = d4d, aes(Time, value, group = id), alpha = 0.1) +
  geom_line(data = d4e, aes(Time, value, group = 1), colour = 'red') +
  scale_x_discrete(expand = expansion(mult=0.1)) +
  facet_grid(vignette~signal) +
  labs(x = '', y = 'Belief\n') +
  theme_minimal(15) +
  theme(
    strip.text.y = element_text(angle = 0, hjust = 0),
    panel.spacing.x = unit(1, 'lines')
  )
plot_belief_raw

# Divide (thwarted marriage only) -----------------------------------------

m13 <- lm(T2Divide ~ T1Divide * signal, d2)
Anova(m13)
gglm(m13)

plot_divide <-
  visreg(m13, xvar='signal', by = 'T1Divide', partial = F, rug = F, gg = T) +
  ylim(c(0, 100)) +
  labs(x = '', y = 'T2 Division to older daughter\n') +
  theme_bw2
plot_divide

# Exploratory

m13b <- lm(T2Divide ~ T1Divide + Age * signal, d2)
Anova(m13b)
gglm(m13b)

plot_divide_age <-
  visreg(m13b, xvar='signal', by = 'Age', partial = F, rug = F, gg = T) +
  ylim(c(0, 100)) +
  labs(x = '', y = 'T2 Division to older daugther') +
  theme_bw2
plot_divide_age

# Jealous -----------------------------------------------------------------

m7 <- glm(T2Jealous ~ T1Jealous + signal, family = quasibinomial, d)
Anova(m7, type = 3)
gglm(m7)

p <- visreg(m7, xvar='signal', partial = F, rug = F, gg = T, scale = 'response')
p + theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Devious -----------------------------------------------------------------

m6 <- glm(T2Devious ~ T1Devious + signal, family = quasibinomial, d)
Anova(m6, type = 3)
gglm(m6)

p <- visreg(m6, xvar='signal', partial = F, rug = F, gg = T, scale = 'response')
p + theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Anger -----------------------------------------------------------------

m8 <- glm(T2Angry ~ T1Angry + signal, family = quasibinomial, d)
Anova(m8, type = 3)
gglm(m8)

p <- visreg(m8, xvar='signal', partial = F, rug = F, gg = T, scale = 'response')
p + theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Sex ---------------------------------------------------------------------

# Belief vs sex
m4 <- glm(T2Belief ~ T1Belief + signal*Sex, family = quasibinomial, d)
Anova(m4, type = 3)
gglm(m4)

plot_belief_sex <-
  visreg(m4, xvar='Sex', by = 'signal', partial = F, rug = F, gg = T, scale = 'response') +
  ylim(c(0, 1)) +
  labs(x = '', y = 'T2 Belief\n') +
  theme_bw(15)
plot_belief_sex

m4b <- glm(T2Belief ~ T1Belief + vignette*Sex, family = quasibinomial, d)
Anova(m4b)
gglm(m4b)

plot_belief_sex_vignette <-
  visreg(m4b, xvar='Sex', by = 'vignette', partial = F, rug = F, gg = T, scale = 'response') +
  ylim(c(0, 1)) +
  labs(x = '', y = 'T2 Belief\n') +
  theme_bw(15)
plot_belief_sex_vignette

# Action vs sex
m5 <- glm(T2Action ~ T1Action + signal*Sex, family = quasibinomial, d)
Anova(m5, type = 3)
gglm(m5)

plot_action_sex <-
  visreg(m5, xvar='Sex', by = 'signal', partial = F, rug = F, gg = T, scale = 'response') +
  ylim(c(0, 1)) +
  labs(x = '', y = 'T2 Action\n') +
  theme_bw(15)
plot_action_sex

m5b <- glm(T2Action ~ T1Action + vignette*Sex, family = quasibinomial, d)
Anova(m5b, type = 3)
gglm(m5b)

plot_action_sex_vignette <-
  visreg(m5b, xvar='Sex', by = 'vignette', partial = F, rug = F, gg = T, scale = 'response') +
  ylim(c(0, 1)) +
  labs(x = '', y = 'T2 Action\n') +
  theme_bw(15)
plot_action_sex_vignette

# Age ---------------------------------------------------------------------

# Belief vs Age
m9 <- glm(T2Belief ~ T1Belief + signal * Age, family = quasibinomial, d)
Anova(m9, type = 3)
gglm(m9)

plot_belief_age <-
  visreg(m9, xvar='Age', by = 'signal', partial = F, rug = F, gg = T, scale = 'response') +
  ylim(c(0, 1)) +
  labs(x = '\nAge', y = 'T2 Belief\n') +
  theme_bw2
plot_belief_age

# Action vs Age
m10 <- glm(T2Action ~ T1Action + signal * Age, family = quasibinomial, d)
Anova(m10, type = 3)
gglm(m10)

plot_action_age <-
  visreg(m10, xvar='Age', by = 'signal', partial = F, rug = F, gg = T, scale = 'response') +
  ylim(c(0, 1)) +
  labs(x = '\nAge', y = 'T2 Action\n') +
  theme_bw2
plot_action_age

# Vignette vs Age
m11 <- lm(T2Belief ~ T1Belief + signal + vignette*Age, family = quasibinomial, d)
Anova(m11, type = 3)
gglm(m11)

plot_belief_age_vignette <-
  visreg(m11, xvar='Age', by = 'vignette', partial = F, rug = F, gg = T, scale = 'response') +
  ylim(c(0, 1)) +
  labs(x = '\nAge', y = 'Belief\n') +
  theme_bw2
plot_belief_age_vignette

m12 <- glm(T2Action ~ T1Action + signal + vignette*Age, family = quasibinomial, d)
Anova(m12, type = 3)
gglm(m12)

plot_action_age_vignette <-
  visreg(m12, xvar='Age', by = 'vignette', partial = F, rug = F, gg = T, scale = 'response') +
  ylim(c(0, 1)) +
  labs(x = '\nAge', y = 'Action\n') +
  theme_bw2
plot_action_age_vignette

# Perceived mental illness ------------------------------------------------

m3 <- glm(T2MentallyIll ~ T1MentallyIll + signal, family = quasibinomial, d)
Anova(m3, type = 3)
gglm(m3)

plot_mentallyill <-
  visreg(m3, xvar='signal', partial = F, rug = F, gg = T, scale = 'response') +
  ylim(c(0, 1)) +
  labs(x = '', y = 'T2 Mentally ill\n') +
  theme_bw2
plot_mentallyill

# Mental illness by vignette

m3b <- glm(T2MentallyIll ~ T1MentallyIll + signal*vignette, family = quasibinomial, d)
Anova(m3b, type = 3)
gglm(m3b)

plot_mentallyill_vignette <-
  visreg(m3b, xvar='signal', by = 'vignette', partial = F, rug = F, gg = T, scale = 'response') +
  ylim(c(0, 1)) +
  labs(x = '', y = 'T2 Mentally ill\n') +
  theme_bw2
plot_mentallyill_vignette

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

d$PC1emotion <- -m_pca_t2emo$x[,1]
d$PC2emotion <- -m_pca_t2emo$x[,2]

ggplot(d, aes(PC1emotion, PC2emotion, colour = T2Belief)) +
  geom_density_2d(alpha = 0.35) +
  geom_point() +
  coord_fixed() +
  scale_color_viridis_c(option='A') +
  theme_minimal(15)

m_emotion_belief <- glm(T2Belief ~ T1Belief * signal + PC1emotion + PC2emotion, family = quasibinomial, d)
Anova(m_emotion_belief)
x <- visreg(m_emotion_belief, scale = 'response', gg=T, rug = F)
(x[[1]] + x[[2]])/(x[[3]] + x[[4]]) & theme_bw()

m_emotion_action <- glm(T2Action ~ T1Action * signal + PC1emotion + PC2emotion, family = quasibinomial, d)

# Emotion change ----------------------------------------------------------

d3 <-
  d %>%
  dplyr::mutate(
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
    .groups='drop'
    )
}

d3_full <- mean_emotions(d3)

n <- nrow(d3)
boot_emotions <- function(i){
  mean_emotions(d3[sample.int(n, replace = T),])
}

d3_boot <- map_df(1:500, boot_emotions)

plot_emotions <-
  ggplot() +
  geom_segment(data = d3_boot, aes(x = T1LowMood_mean, y = T1Manipulative_mean, xend = T2LowMood_mean, yend = T2Manipulative_mean, colour=vignette), alpha = 0.02, arrow = arrow(length = unit(3, "mm"))) +
  geom_segment(data = d3_full, aes(x = T1LowMood_mean, y = T1Manipulative_mean, xend = T2LowMood_mean, yend = T2Manipulative_mean, colour=vignette), alpha = 1, size = 1, arrow = arrow(length = unit(3, "mm"))) +
  facet_wrap(~signal) +
  labs(title = 'Change in mean emotion from T1 to T2', x = '\nLow mood', y = 'Manipulative\n') +
  theme_bw()
plot_emotions

# Regression table --------------------------------------------------------

models <- list(
  'Belief' = m1,
  'Belief by vignette' = m1b,
  'Action' = m2,
  'Action by vignette' = m2b
)

regressiontable(models)

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
  dplyr::mutate(
    signal = factor(signal, levels = c('Verbal request', 'Depression'))
  )

mmediator <- lm(T2Belief ~ signal + T1Belief, d_mediate)
mout <- lm(T2Action ~ signal + T2Belief + T1Action + T1Belief, d_mediate)

out <- mediate(mmediator, mout, treat = "signal", mediator = "T2Belief", boot = T)
summary(out)
plot(out)

mmediator <- lm(PC1emotion ~ signal + T1Belief, d_mediate)
mout <- lm(T2Belief ~ signal + PC1emotion + T1Belief, d_mediate)
out <- mediate(mmediator, mout, treat = "signal", mediator = "PC1emotion")
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
  dplyr::mutate(
    signal = factor(signal, levels = c('Depression', 'Suicide attempt'))
  )

mmediator <- lm(T2Belief ~ signal + T1Belief + vignette + Sex, d_mediate)
mout <- lm(T2Action ~ signal + T2Belief + T1Action + T1Belief + vignette + Sex, d_mediate)

out <- mediate(mmediator, mout,treat = "signal", mediator = "T2Belief", boot = T)

summary(out)
plot(out)

# to do: moderated mediation model

# Depression vs Verbal
d_mediate <-
  d %>%
  dplyr::filter(
    signal %in% c(
      'Verbal request',
      'Depression'),
    vignette != "Thwarted marriage"
  ) %>%
  dplyr::mutate(
    signal = factor(signal, levels = c('Verbal request', 'Depression'))
  )

mmediator <- lm(T2Belief ~ signal + T1Belief + vignette, d_mediate)
mout <- lm(T2Action ~ signal + T2Belief + T1Action + T1Belief + vignette, d_mediate)

out <- mediate(mmediator, mout, treat = "signal", mediator = "T2Belief", boot = T)

summary(out)
plot(out)


# Suicide attempt vs Verbal
d_mediate <-
  d %>%
  dplyr::filter(
    signal %in% c(
      'Verbal request',
      'Suicide attempt'),
    vignette != "Thwarted marriage"
  ) %>%
  mutate(
    signal = factor(signal, levels = c('Verbal request', 'Suicide attempt'))
  )

mmediator <- lm(T2Belief ~ signal + T1Belief + vignette, d_mediate)
mout <- lm(T2Action ~ signal + T2Belief + T1Action + T1Belief + vignette, d_mediate)

out <- mediate(mmediator, mout, treat = "signal", mediator = "T2Belief")

summary(out)
plot(out)


# Suicide attempt vs Depression
d_mediate <-
  d %>%
  dplyr::filter(
    signal %in% c(
      'Depression',
      'Suicide attempt'),
    vignette != "Thwarted marriage"
  ) %>%
  mutate(
    signal = factor(signal, levels = c('Depression', 'Suicide attempt'))
  )

mmediator <- lm(T2Belief ~ signal + T1Belief + vignette, d_mediate)
mout <- lm(T2Action ~ signal + T2Belief + T1Action + T1Belief + vignette, d_mediate)

out <- mediate(mmediator, mout, treat = "signal", mediator = "T2Belief")

summary(out)
plot(out)

# Minor depression vs Depression
d_mediate <-
  d %>%
  dplyr::filter(
    signal %in% c(
      'Mild depression',
      'Depression'),
    vignette != "Thwarted marriage"
  ) %>%
  mutate(
    signal = factor(signal, levels = c('Mild depression', 'Depression'))
  )

mmediator <- lm(T2Belief ~ signal + T1Belief + vignette, d_mediate)
mout <- lm(T2Action ~ signal + T2Belief + T1Action + T1Belief + vignette, d_mediate)

out <- mediate(mmediator, mout, treat = "signal", mediator = "T2Belief")

summary(out)
plot(out)

# Crying vs Verbal
d_mediate <-
  d %>%
  dplyr::filter(
    signal %in% c(
      'Crying',
      'Verbal request'),
    vignette != "Thwarted marriage"
  ) %>%
  mutate(
    signal = factor(signal, levels = c('Verbal request', 'Crying'))
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
  dplyr::mutate(
    signal = factor(signal, levels = c('Depression', 'Suicide attempt'))
  )

mmediator <- lm(T2Belief ~ signal + T1Belief + vignette, d_mediate)
mout <- lm(T2Action ~ signal + T2Belief + T1Action + T1Belief + vignette, d_mediate)

out <- mediate(mmediator, mout, treat = "signal", mediator = "T2Belief", boot = T)

summary(out)
plot(out)

dT2signal <-
  d %>%
  # dplyr::filter(vignette != 'Thwarted marriage') %>%
  dplyr::select(signal, T2Angry:T2Violated) %>%
  pivot_longer(-signal) %>%
  mutate(name = str_remove(name, 'T2')) %>%
  # dplyr::filter(value>0) %>%
  group_by(name, signal) %>%
  summarise(n = sum(value)/n()) %>%
  pivot_wider(names_from = signal, values_from = n, values_fill = 0)

hagenheat(dT2, rotate_labels = F, seriation_method = 'PCA_angle', viridis_option = 'B')

dT1vignette <-
  d %>%
  dplyr::select(vignette, T1Angry:T1Violated) %>%
  pivot_longer(-vignette) %>%
  mutate(name = str_remove(name, 'T1')) %>%
  # dplyr::filter(value>0) %>%
  group_by(name, vignette) %>%
  summarise(n = sum(value)/n()) %>%
  pivot_wider(names_from = vignette, values_from = n, values_fill = 0)

hagenheat(dT1, rotate_labels = F, seriation_method = 'PCA_angle', viridis_option = 'B')

dT1signal <-
  d %>%
  dplyr::select(signal, T1Angry:T1Violated) %>%
  pivot_longer(-signal) %>%
  mutate(name = str_remove(name, 'T1')) %>%
  group_by(name, signal) %>%
  summarise(n = sum(value)/n()) %>%
  pivot_wider(names_from = signal, values_from = n, values_fill = 0)


d_diff <- as.matrix(dT2signal[-1]) - as.matrix(dT1signal[-1])
rownames(d_diff) <- dT2signal$name

hagenheat(d_diff, rotate_labels = F, seriation_method = 'PCA_angle') + scale_fill_gradient2()


  # ggplot() +
  #   geom_segment(data = d_boot, aes(x = T1LowMood_mean, y = T1Manipulative_mean, xend = T2LowMood_mean, yend = T2Manipulative_mean, colour=signal), alpha = 0.02, arrow = arrow(length = unit(3, "mm"))) +
  #   geom_segment(data = d_full, aes(x = T1LowMood_mean, y = T1Manipulative_mean, xend = T2LowMood_mean, yend = T2Manipulative_mean, colour=signal), alpha = 1, size = 1, arrow = arrow(length = unit(3, "mm"))) +
  #   xlim(0, NA) +
  #   facet_wrap(~vignette) +
  #   labs(title = 'Change in mean emotions from T1 to T2', x = '\nLow mood', y = 'Manipulative\n') +
  #   theme_bw(15) +
  #   theme(axis.title.y = element_text(angle = 0))

p1 <- as.ggplot(~plot(d_density2d$kdeT1[[9]], display='persp', theta=50, phi=20, main = 'T1: Pre-depression'))
p2 <- as.ggplot(~plot(d_density2d$kdeT2[[9]], display='persp', theta=50, phi=20, main = 'T2: Post-depression'))
p3 <- as.ggplot(~plot(d_density2d$kdeT1[[7]], display='persp', theta=50, phi=20, main = 'T1: Pre-crying'))
p4 <- as.ggplot(~plot(d_density2d$kdeT2[[7]], display='persp', theta=50, phi=20, main = 'T2: Post-crying'))


(p1+p2)/(p3+p4)

# Do costly signals increase actions more than beliefs?

tmp <-
  d %>%
  dplyr::select(id, signal, vignette, T1Belief, T1Action, T2Belief, T2Action) %>%
  pivot_longer(T2Belief:T2Action) %>%
  separate(name, into = c('time', 'type'), sep=2) %>%
  mutate(signal = factor(as.character(signal), levels = rev(c('Verbal request', 'Crying', 'Mild depression', 'Depression', 'Suicide attempt'))))

m <- glmer(
  value ~
    type*signal +
    type*vignette +
    T1Belief +
    T1Action +
    (1|id),
  family = binomial,
  tmp,
  nAGQ = 0
  )

em <- emmeans::emmeans(m, specs='type', by='signal')
plot(pairs(em)) + theme_minimal() + theme(strip.text.y = element_text(angle = 0, hjust=0))

em <- emmeans::emmeans(m, specs='type', by='vignette')
plot(pairs(em)) + theme_minimal() + theme(strip.text.y = element_text(angle = 0, hjust=0))



e <- d_density2d$data[[10]]
uv <- apply(e[c('T2Belief', 'T2Action')], 2, rank) / (nrow(e) + 1)
fit <- kdecop(uv)
plot(fit)


dden <-
  d %>%
  group_by(signal) %>%
  nest() %>%
  rowwise() %>%
  mutate(
    matT1 = list(matrix(c(data$T1Belief, data$T1Action), ncol = 2, dimnames = list(c(), c('T1Belief', 'T1Action')))),
    matT2 = list(matrix(c(data$T2Belief, data$T2Action), ncol = 2, dimnames = list(c(), c('T2Belief', 'T2Action'))))  ) %>%
  ungroup() %>%
  arrange(signal)

uv <- apply(dden$matT1[[1]], 2, rank) / (nrow(dden$matT1[[1]]) + 1)
fit <- kdecop(uv, method = 'TTPI')
plot(fit)
plot(fit, type='contour')

uv <- apply(dden$matT2[[1]], 2, rank) / (nrow(dden$matT2[[1]]) + 1)
fit <- kdecop(uv, method = 'TTPI')
plot(fit)
plot(fit, type='contour')


d_US2 <-
  d_US %>%
  dplyr::filter(RelStatus != 'Widowed') %>%
  mutate(
    RelStatus = ifelse(str_detect(RelStatus, 'Previously'), 'Divorced', RelStatus)
  )

m <- glm(T2Belief ~ T1Belief + vignette + signal * RelStatus + Age , family = quasibinomial(), d_US2)
Anova(m, type = 3)
visreg(m, xvar='signal', by='RelStatus', scale = 'response', rug = F, gg = T) + coord_flip() + theme_bw()

m <- glm(T2Belief ~ T1Belief + signal + vignette * RelStatus + Age , family = quasibinomial(), d_US2)
Anova(m, type = 3)

m <- glm(T2Action ~ T1Action + vignette + signal * RelStatus + Age , family = quasibinomial(), d_US2)
Anova(m, type = 3)
visreg(m, xvar='signal', by='RelStatus', scale = 'response', rug = F, gg = T) + coord_flip() + theme_bw()

m <- glm(T2Action ~ T1Action + signal + vignette * RelStatus + Age , family = quasibinomial(), d_US2)
Anova(m, type = 3)



m <- glm(T2Belief ~ T1Belief + Sons + signal + vignette*Daughters + Age , family = quasibinomial(), d_US)
Anova(m, type = 3)

m <- glm(T2Action ~ T1Action + Sons + signal + vignette * Daughters + Age , family = quasibinomial(), d_US)
Anova(m, type = 3)

