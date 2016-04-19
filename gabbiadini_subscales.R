# Scales from Gabbiadini et al.
library(dplyr)
library(psych)

dat = read.csv("data_set_PLOS.csv")

# Full empathy composite
dat %>% 
  select(Empathy_comprensione:Empathy_affettuosita, 
         Empathy_serenitaR:Empathy_disinteresseR) %>% 
  alpha

dat$Empathy_composite_F = dat %>% 
  select(Empathy_comprensione:Empathy_affettuosita, 
         Empathy_serenitaR:Empathy_disinteresseR) %>% 
  apply(., 1, mean)

cor(dat$Empathy_composite_F, dat$emp_scal, use = 'pairwise')
mean(abs(dat$Empathy_composite_F - dat$emp_scal), na.rm = T) # not a match
table(is.na(dat$Empathy_composite_F), is.na(dat$emp_scal))

# Authors' empathy composite - discards 'affettuosita'
dat %>% 
  select(Empathy_comprensione:Empathy_premura, 
         Empathy_serenitaR:Empathy_disinteresseR) %>% 
  alpha

dat$Empathy_composite_G = dat %>% 
  select(Empathy_comprensione:Empathy_premura, 
         Empathy_serenitaR:Empathy_disinteresseR) %>% 
  apply(., 1, mean)

cor(dat$Empathy_composite_G, dat$emp_scal, use = 'pairwise')
mean(abs(dat$Empathy_composite_G - dat$emp_scal), na.rm = T) # a match
table(is.na(dat$Empathy_composite_G), is.na(dat$emp_scal))

# Masculine beliefs: equal weights, all items
dat %>% 
  select(MRNI1_aggr:MRNI15_restremotionality) %>% 
  alpha
dat$mas_composite_F = dat %>% 
  select(MRNI1_aggr:MRNI15_restremotionality) %>% 
  apply(., 1, mean)

cor(dat$mas_composite_F, dat$mas_beli, use = 'pairwise')
mean(abs(dat$mas_composite_F - dat$mas_beli), na.rm=T) # not a match
mean(abs(dat$mas_composite_F - dat$full_MRNI), na.rm=T) # match
table(is.na(dat$mas_composite_F), is.na(dat$mas_beli))

# Masculine beliefs, authors' composite
dat$mas_composite_G = dat %>% 
  select(machismoDOMINANCE,
         machismoAGGRESSIVITY:machismoRESTRICTIVEEMOTIONALITY) %>% 
  apply(., 1, mean)

cor(dat$mas_composite_G, dat$mas_beli, use = 'pairwise')
mean(abs(dat$mas_composite_G - dat$mas_beli), na.rm=T) # a match
mean(abs(dat$mas_composite_G - dat$full_MRNI), na.rm=T) # not a match
table(is.na(dat$mas_composite_G), is.na(dat$mas_beli))


with(dat, plot(mas_beli, full_MRNI))
with(dat, plot(mas_beli, machismoMRNIwithoutSELFRELIANCE))
     