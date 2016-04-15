# Process models from Gabbiadini

library(dplyr)
library(magrittr)
library(lavaan)

dat = read.csv("data_set_PLOS.csv")

# Modeling indirect effect using column "avatarID" as they did ----
modelSEM.indirect = {
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

model.sem.indirect = sem(modelSEM.indirect, data = dat, se = "boot")
summary(model.sem.indirect)


# Modeling indirect effect using full scale composite "avatar_id" ----
# What happens if the full scale is substituted in the analysis?
# Make all the interaction terms
dat = dat %>% 
  mutate(IDc = avatar_id - mean(avatar_id),
         IDcxGender = IDc * gend_con,
         IDcxcond101 = IDc * cond101,
         IDcxcond101xGender = IDc * gend_con * cond101
  )

# Make the model:
modelSEM2.indirect = {
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

model.sem2.indirect = sem(modelSEM2.indirect, data = dat, se = "boot")
summary(model.sem2.indirect)