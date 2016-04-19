# Process models from Gabbiadini

library(dplyr)
library(magrittr)
library(lavaan)

dat = read.csv("data_set_PLOS.csv")

# Modeling indirect effect using column "avatarID" as they did ----
modelSEM1 = {
  "emp_scal ~ b*mas_beli +
              c*cond101 + 
              cw*gender_codxcond101 + 
              cx*avatad_IDcentxcond101 +
              cwx*avatad_IDcentxgendercodxcond101
   mas_beli ~ a*cond101 + 
              aw*gender_codxcond101 + 
              ax*avatad_IDcentxcond101 +
              awx*avatad_IDcentxgendercodxcond101
  # Indirect and total effects for males (gend_cod == 1)
  indirectM := a*b + aw*b + ax*b + awx*b
  totalM := indirectM + c + cw + cx + cwx
  # Indirect and total effects for females (gend_cod == 0)
  indirectF := a*b + ax*b # aw*b and awx*b are zero
  totalF := indirectF + c + cx # cw and cwx are zero"
}
# Is it possible to retrieve unconditional indirect and total effects?
#ab := a*b + aw*b + ax*b + awx*b # WIP: I don't know what I'm doing
#total := ab + c + cw + cx + cwx # WIP: I don't know what I'm doing

fit1 = sem(modelSEM1, data = dat, se = "boot")
summary(fit1)


# Modeling indirect effect using full scale composite "avatar_id" ----
# What happens if the full scale is substituted in the analysis?
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

# Make the model:
modelSEM2 = {
  "emp_scal ~ b*mas_beli +
              c*cond101 + 
              cw*gender_codxcond101 + 
              cx*IDcxcond101 +
              cwx*IDcxcond101xGender
  mas_beli ~ a*cond101 + 
            aw*gender_codxcond101 + 
            ax*IDcxcond101 +
            awx*IDcxcond101xGender
  # Indirect and total effects for males (gend_cod == 1)
  indirectM := a*b + aw*b + ax*b + awx*b
  totalM := indirectM + c + cw + cx + cwx
  # Indirect and total effects for females (gend_cod == 0)
  indirectF := a*b + ax*b # aw*b and awx*b are zero
  totalF := indirectF + c + cx # cw and cwx are zero"
}

fit2 = sem(modelSEM2, data = dat, se = "boot")
summary(fit2)

# Full scales for MRNI and empathy ----
dat$emp_scal_full = dat %>% 
  select(Empathy_comprensione:Empathy_affettuosita, 
         Empathy_serenitaR:Empathy_disinteresseR) %>% 
  apply(., 1, mean)

modelSEM3 = {
  "emp_scal_full ~ b*full_MRNI +
                  c*cond101 + 
                   cw*gender_codxcond101 + 
                  cx*avatad_IDcentxcond101 +
                   cwx*avatad_IDcentxgendercodxcond101

  full_MRNI ~ a*cond101 + 
             aw*gender_codxcond101 + 
             ax*avatad_IDcentxcond101 +
              awx*avatad_IDcentxgendercodxcond101

  # Indirect and total effects for males (gend_cod == 1)
  indirectM := a*b + aw*b + ax*b + awx*b
  totalM := indirectM + c + cw + cx + cwx
  # Indirect and total effects for females (gend_cod == 0)
  indirectF := a*b + ax*b # aw*b and awx*b are zero
  totalF := indirectF + c + cx # cw and cwx are zero"
}

fit3 = sem(modelSEM3, data = dat, se = "boot")
summary(fit3)

# Full scales for MRNI, empathy, and avatar-id
modelSEM4 = {
  "emp_scal_full ~ b*full_MRNI +
  c*cond101 + 
  cw*gender_codxcond101 + 
  cx*IDcxcond101 +
  cwx*IDcxcond101xGender

  full_MRNI ~ a*cond101 + 
  aw*gender_codxcond101 + 
  ax*IDcxcond101 +
  awx*IDcxcond101xGender

  # Indirect and total effects for males (gend_cod == 1)
  indirectM := a*b + aw*b + ax*b + awx*b
  totalM := indirectM + c + cw + cx + cwx
  # Indirect and total effects for females (gend_cod == 0)
  indirectF := a*b + ax*b # aw*b and awx*b are zero
  totalF := indirectF + c + cx # cw and cwx are zero"
}

fit4 = sem(modelSEM4, data = dat, se = "boot")
summary(fit4)
