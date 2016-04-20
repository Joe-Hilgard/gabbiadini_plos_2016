# re-analysis of Gabbiadini et al. 2016
library(psych)
library(dplyr)
library(magrittr)
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

# compare to total scale
m3.1 = aov(avatar_id ~ condition, data = dat)
summary(m3.1)
TukeyHSD(m3.1) # HL is greater than GTA, greater than Qube/pinball

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
  geom_smooth(method = 'lm', se = F) +
  ggtitle("Gender × Identification × Game (-1, 0, 1)")

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



# How many have avatar_id > 4.29?
dat %>% 
  filter(condition == "GTA", gender == 1) %>% 
  select(avatarID)

dat %>% 
  filter(condition == "GTA", gender == 1) %>% 
  with(., hist(avatarID))

dat %>% 
  filter(condition == "GTA", gender == 1) %>% 
  with(., hist(avatarID, breaks = c(2, 3, 4, 4.2583, 5, 6, 7), freq = T))
abline(v = 4.2583, col = "red", lwd = 2)

dat %>% 
  filter(condition == "GTA", gender == 1) %>% 
  with(., table(avatarID > 4.2583))
  
# Why are avatar_id and avatarID different?
with(dat, plot(avatarID, avatar_id))

# avatarID seems to be the one used in the process model
with(dat, plot(avatarID, avatad_IDcent))

# Why do avatarID and avatar_id differ?
# AvatarID is just the "embodied presence" subscale of the identification measure
# Embodied presence subscale:
dat %>% 
  select(avatar_id_embodied_presence1:avatar_id_embodied_presence6) %>% 
  apply(1, FUN = mean) %T>% 
  plot(x = ., y = dat$avatarID, 
       main = "avatarID is embodied presence subscale",
       xlab = "subscale mean") %>%
  plot(x = ., y = dat$avatar_id,
       main = "avatar_id is not embodied presence subscale",
       xlab = "subscale mean") 

# Total measure mean:
dat %>% 
  select(avatar_id_embodied_presence1:avatar_id_char_empathy4) %>% 
  apply(1, FUN = mean) %T>% 
  plot(x = ., y = dat$avatarID, 
       main = "avatarID is not total measure mean",
       xlab = "Total measure mean") %>% 
  plot(x = ., y = dat$avatar_id, 
       main = "avatar_id is the total measure mean",
       xlab = "Total measure mean")  

# Are the means in table 2 avatarID or avatar_id?
tapply(dat$avatarID, dat$condition, FUN = mean) # matches
tapply(dat$avatar_id, dat$condition, FUN = mean) # does not match



# How about in GLM?
fit.x = lm(emp_scal ~ condition * as.factor(gend_con) * avatar_id, data = dat) 
summary(fit.x)

# Looking at models & plots within critical subset -----
# The critical region? Males w/ ID > 4.258
dat.crit = dat %>% 
  filter(avatarID > 4.2583, gend_con == 1) 

# Null results in total-sample GLM
m = lm(emp_scal ~ cond101*as.factor(gend_con)*avatarID, data = dat)
summary(m); Anova(m, type = 3)
# Null main effect in critical subsample GLM
m.crit = lm(emp_scal ~ cond101, data = dat.crit)
summary(m.crit); hist(m.crit$residuals)
# Significant interaction in critical subsample GLM
m.crit2 = lm(emp_scal ~ cond101*avatarID, data = dat.crit)
summary(m.crit2); hist(m.crit2$residuals)

# All subjects
ggplot(dat, aes(x = condition, y = emp_scal)) +
  geom_violin() +
  geom_boxplot(width = .2, notch = T) +
  ggtitle("All subjects")

# All males
dat %>% 
  filter(gend_con == 1) %>% 
  ggplot(aes(x = condition, y = emp_scal)) +
  geom_violin() +
  geom_boxplot(width = .2, notch = T) +
  ggtitle("All males")

# Critical region
ggplot(dat.crit, aes(x = condition, y = emp_scal)) +
  geom_violin() +
  geom_boxplot(width = .2, notch = T) +
  #geom_point(alpha = .25) +
  ggtitle("Only the Highly-Identified Males")

# Regression lines
ggplot(dat, aes(x = avatarID, y = emp_scal, col = condition, lty = as.factor(gend_con))) +
  geom_point() +
  geom_smooth(method = 'lm', se = F) +
  ggtitle("3-Way interaction among all subjects")

dat %>% 
  filter(gend_con == 1, avatarID > 4.25) %>% 
  ggplot(aes(x = avatarID, y = emp_scal, col = condition)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  geom_point(data = dat, 
             aes(x = avatarID, y = emp_scal, col = condition), 
             alpha = .15) +
  ggtitle("Regression within only highly-identified males")

dat = dat %>% 
  mutate(crit_code = ifelse(dat$gend_con == 1 & dat$avatarID > 4.2583 & dat$condition == "GTA", "Yes", "No"))

ggplot(dat, aes(x = avatarID, y = emp_scal, col = crit_code)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Empathy as function of critical combo")

ggplot(dat, aes(x = crit_code, y = emp_scal, col = crit_code)) +
  geom_violin() +
  geom_boxplot(width = .25, notch = T)


