# re-analysis of Gabbiadini et al. 2016
library(psych)
library(dplyr)
library(ggplot2)
library(lavaan)
library(car)

dat = read.csv("data_set_PLOS.csv")
table(dat$played_game, dat$condition, useNA = 'always')
table(dat$played_game, dat$cond, useNA = 'always')

# Experimental assignment
dat %>% 
  select(played_game:cond) %>% 
  distinct

# Avatar identification
dat %>% 
  select(avatar_id_embodied_presence1:avatar_id_char_empathy4) %>% 
  glimpse

# Masculine beliefs
dat %>% 
  select(MRNI1_aggr:MRNI15_restremotionality) %>% 
  glimpse

# Do these really belong together -- "demonstrate physical prowess" v 
# "use any means to 'convince' a girl to have sex"
dat %>% 
  select(MRNI1_aggr:MRNI15_restremotionality) %>% 
  psych::alpha()
# I guess so.  

# Empathy for victim?
dat %>% 
  select(Empathy_comprensione:Empathy_disinteresseR) %>% 
  glimpse

# Game rating. Was there a Game_rating_involvement1?
dat %>% 
  select(game_rating_involvement2_eccitante:game_rating_involvement3_coinvolgente) %>% 
  glimpse


# Analysis ---
# Effects of game on empathy towards women -- zilch ----
# I'm guessing "emp_scal" is their outcome -- empathy towards female violence victim
dat %>% 
  select(Empathy_comprensione:Empathy_disinteresse) %>% 
  psych::alpha(check.keys = T)

dat %>% 
  select(Empathy_comprensione:Empathy_disinteresse, emp_scal) %>% 
  cor(use = "pairwise") %>% 
  round(3)

m1 = aov(emp_scal ~ as.factor(condition), data = dat)
summary(m1)
TukeyHSD(m1) 

m1.1 = lm(emp_scal ~ cond, data = dat)
summary(m1.1)

tapply(dat$emp_scal, dat$cond, mean, na.rm = T)

# Moderated by gender?
m1.2 = lm(emp_scal ~ condition * as.factor(gender), data = dat)
summary(m1.2)
Anova(m1.2, type = 3) # No

# Get means by gender
tapply(dat$emp_scal, INDEX = list(dat$condition, dat$gender), FUN = mean, na.rm = T)
aov(emp_scal ~ condition, data = dat[dat$gender == 1,]) %>% TukeyHSD()
# No differences even among men

# Effects of game on masculine beliefs ----
# ANOVA
m2 = aov(mas_beli ~ condition, data = dat)
summary(m2)
TukeyHSD(m2) # GTA differs from nonviolent, p = .03

# Linear contrast
m2.1 = lm(mas_beli ~ cond, data = dat)
summary(m2.1)

ggplot(dat, aes(x = as.factor(cond), y = mas_beli, 
                col = as.factor(cond), shape = played_game)) +
  geom_boxplot(width = .3, notch = T)

ggplot(dat, aes(x = as.factor(cond), y = mas_beli, 
                col = as.factor(cond), shape = played_game)) +
  geom_point(position = position_jitter(width = .5))

# Identification with game character ----
# Greatest identification in half-life
m3 = aov(avatarID ~ condition, data = dat)
summary(m3)
TukeyHSD(m3) # HL is greater than GTA, greater than Qube/pinball

# Our predicted 3-way interaction between game, gender, and identification ---
m4 = lm(mas_beli ~ condition*as.factor(gender)*avatarID, data = dat)
summary(m4)
hist(m4$residuals)

ggplot(dat, aes(x = avatarID, y = mas_beli, col = as.factor(cond), 
                lty = as.factor(gender), shape = as.factor(gender))) +
  geom_point() + 
  geom_smooth(method = "lm", se = F)

# Does that 3-way interaction generalize to the primary outcome? ---
m5 = lm(emp_scal ~ as.factor(cond)*as.factor(gender)*avatarID, data = dat)
summary(m5) # No it does not
hist(m5$residuals)
Anova(m5, type = 3)

# Ordinal code?
m5.1 = lm(emp_scal ~ cond*as.factor(gender)*avatarID, data = dat)
summary(m5.1) # Still the answer is no
hist(m5.1$residuals)
Anova(m5.1, type = 3)

ggplot(dat, aes(x = avatarID, y = emp_scal, col = as.factor(cond), 
                lty = as.factor(gender), shape = as.factor(gender))) +
  geom_point() + 
  geom_smooth(method = 'lm', se = F)

# Kinda? But not significantly so.

# So just how many highly-identified violent male gamers are there?
hist(dat$avatarID[dat$cond == 3 & dat$gender == 1], breaks = 7)
sum(dat$avatarID[dat$cond == 3 & dat$gender == 1] > 4) # about 17
length(dat$avatarID[dat$cond == 3 & dat$gender == 1]) # out of 22

ggplot(dat, aes(x = mas_beli, y = emp_scal, col = as.factor(gender))) +
  geom_point() +
  geom_smooth(method = "lm")

# Were these two factors correlated in the control group? Yeah
dat %>% 
  filter(cond == 1) %>% 
  select(gender, mas_beli, emp_scal) %>% 
  cor(use = 'pairwise') %>% 
  round(3)

dat %>% 
  filter(cond == 1) %>% 
  ggplot(aes(x = mas_beli, y = emp_scal, col = as.factor(gender))) +
  geom_point() + 
  geom_smooth(method = "lm")

# Not sure if I'm contrast-coding this correctly
model.check = lm(emp_scal ~ mas_beli * C(as.factor(gender), "contr.treatment"), data = dat, subset = cond == 1)
summary(model.check)

# SEM ----
modelSEM = {
  "emp_scal ~ mas_beli
  mas_beli ~ cond101 + gend_con + avatad_IDcent +
              gender_codxcond101 + avatad_IDcentxgendercod + avatad_IDcentxcond101 +
              avatad_IDcentxgendercodxcond101"
}

model.sem = sem(modelSEM, data = dat)
summary(model.sem)

# How many have mas_beli > 4.29?
dat %>% 
  filter(condition == "GTA", gender == 1) %>% 
  select(mas_beli)

dat %>% 
  filter(condition == "GTA", gender == 1) %>% 
  with(., hist(mas_beli, breaks = c(1, 1.5, 2, 2.5, 3, 3.5, 4, 4.29, 4.5, 5, 5.5, 6), freq = T))
abline(v = 4.29, col = "red", lwd = 2)

dat %>% 
  filter(condition == "GTA", gender == 1) %>% 
  with(., hist(mas_beli), breaks = c(2, 3, 4.29, 6))

dat %>% 
  filter(condition == "GTA", gender == 1) %>% 
  with(., table(mas_beli > 4.29))
  