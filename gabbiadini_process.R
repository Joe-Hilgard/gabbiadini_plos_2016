# Process models from Gabbiadini

library(dplyr)
library(magrittr)
library(lavaan)

dat = read.csv("data_set_PLOS.csv")

# Make the full-composite variables
# Full scales for MRNI and empathy 
dat$emp_scal_full = dat %>% 
  select(Empathy_comprensione:Empathy_affettuosita, 
         Empathy_serenitaR:Empathy_disinteresseR) %>% 
  apply(., 1, mean)
# Full scales for avatar-identification
# Pluck out the bad obs
dat$avatar_id_wishful_id4[dat$avatar_id_wishful_id4 > 7] = NA
dat$avatar_id_wishful_id5[dat$avatar_id_wishful_id5 > 7] = NA
# Make mean
dat$avatar_id_fix = dat %>% 
  select(avatar_id_embodied_presence1:avatar_id_char_empathy4) %>% 
  apply(., 1, mean)
# Make all the interaction terms
dat = dat %>% 
  mutate(IDc = avatar_id_fix - mean(avatar_id_fix, na.rm = T),
         IDcxGender = IDc * gend_con,
         IDcxcond101 = IDc * cond101,
         IDcxcond101xGender = IDc * gend_con * cond101
  )

# Modeling indirect effect using column "avatarID" as they did, with covariates ----
modelSEM1 = {
  "emp_scal ~ b*mas_beli +
              c*cond101 +
              d1*age + 
              d2*violent
   mas_beli ~ a*cond101 + 
              w*gend_con +
              z*avatad_IDcent +
              aw*gender_codxcond101 + 
              az*avatad_IDcentxcond101 +
              wz*avatad_IDcentxgendercod +
              awz*avatad_IDcentxgendercodxcond101 +
              e1*age +
              e2*violent
  # Indirect and total effects for males (gend_cod == 1)
  indirectM := a*b + w*b + z*b + aw*b + az*b + wz*b + awz*b
  totalM := indirectM + c
  # Indirect and total effects for females (gend_cod == 0)
  indirectF := a*b + az*b # aw*b, wz*b, and awz*b are zero
  totalF := indirectF + c  # cw and cwx are zero"
}
# Is it possible to retrieve unconditional indirect and total effects?
#ab := a*b + aw*b + ax*b + awx*b # WIP: I don't know what I'm doing
#total := ab + c + cw + cx + cwx # WIP: I don't know what I'm doing

fit1 = sem(modelSEM1, data = dat, se = "boot")
summary(fit1)

# Full scales for MRNI, empathy, and avatar-id, with covariates ----
modelSEM2 = {
  "emp_scal_full ~ b*full_MRNI +
  c*cond101 +
  d1*violent +
  d2*age

  full_MRNI ~ a*cond101 + 
  aw*gender_codxcond101 + 
  az*IDcxcond101 +
  wz*IDcxGender +
  awz*IDcxcond101xGender +
  e1*violent +
  e2*age

  # Indirect and total effects for males (gend_cod == 1)
  indirectM := a*b + aw*b + az*b + wz*b + awz*b
  totalM := indirectM + c 
  # Indirect and total effects for females (gend_cod == 0)
  indirectF := a*b + az*b # aw*b, wz*b, and awz*b are zero
  totalF := indirectF + c"
}

fit2 = sem(modelSEM2, data = dat, se = "boot")
summary(fit2)

# Modeling using their variables, no covariates ----
modelSEM3 = {
  "emp_scal ~ b*mas_beli +
  c*cond101 

  mas_beli ~ a*cond101 + 
  w*gend_con +
  z*avatad_IDcent +
  aw*gender_codxcond101 + 
  az*avatad_IDcentxcond101 +
  wz*avatad_IDcentxgendercod +
  awz*avatad_IDcentxgendercodxcond101
  
  # Indirect and total effects for males (gend_cod == 1)
  indirectM := a*b + w*b + z*b + aw*b + az*b + wz*b + awz*b
  totalM := indirectM + c
  # Indirect and total effects for females (gend_cod == 0)
  indirectF := a*b + az*b # aw*b, wz*b, and awz*b are zero
  totalF := indirectF + c  # cw and cwx are zero"
}
# Is it possible to retrieve unconditional indirect and total effects?
#ab := a*b + aw*b + ax*b + awx*b # WIP: I don't know what I'm doing
#total := ab + c + cw + cx + cwx # WIP: I don't know what I'm doing

fit3 = sem(modelSEM3, data = dat, se = "boot")
summary(fit3)

# Modeling using full scales, no covariates ----
modelSEM4 = {
  "emp_scal_full ~ b*full_MRNI +
  c*cond101 
  
  full_MRNI ~ a*cond101 + 
  aw*gender_codxcond101 + 
  az*IDcxcond101 +
  wz*IDcxGender +
  awz*IDcxcond101xGender 
  
  # Indirect and total effects for males (gend_cod == 1)
  indirectM := a*b + aw*b + az*b + wz*b + awz*b
  totalM := indirectM + c 
  # Indirect and total effects for females (gend_cod == 0)
  indirectF := a*b + az*b # aw*b, wz*b, and awz*b are zero
  totalF := indirectF + c"
}

fit4 = sem(modelSEM4, data = dat, se = "boot")
summary(fit4)

# Coding 1, 0 for misogyny, adjusting for violence ----
dat = dat %>% 
  mutate(misogyny = ifelse(dat$condition == "GTA", 1, 0),
         gender_codxMiso = gend_con * misogyny,
         IDcxMiso = IDc * misogyny,
         IDcxMisoxGender = IDc * gend_con * misogyny)

modelSEM5 = {
  "emp_scal_full ~ b*full_MRNI +
  c*misogyny +
  d1*age +
  d2*violent
  
  full_MRNI ~ a*misogyny + 
  aw*gender_codxMiso + 
  az*IDcxMiso +
  wz*IDcxGender +
  awz*IDcxMisoxGender +
  e1*age +
  e2*violent
  
  # Indirect and total effects for males (gend_cod == 1)
  indirectM := a*b + aw*b + az*b + wz*b + awz*b
  totalM := indirectM + c 
  # Indirect and total effects for females (gend_cod == 0)
  indirectF := a*b + az*b # aw*b, wz*b, and awz*b are zero
  totalF := indirectF + c"
}

fit5 = sem(modelSEM5, data = dat, se = "boot")
summary(fit5)