################################################################################
################# Propensity score weigting models #############################
################################################################################
# nedded packages #
library(tidyverse)
library(rms)
library(WeightIt)
library(cobalt)
library(gridExtra)
library(broom)

# load data set #
data_frame <- readRDS("E:/ProjektDB/ALMAU/Workdata/707842/final_data_frame_210118.rds")
data_frame <- data_frame %>% 
  arrange(Alder) %>% 
  group_by(indvandre_type) %>% 
  mutate(sengedage_grouped = if_else(Sengedage >= 8, 8, Sengedage),
         time_dk = if_else(time_dk == 0, 0.1, time_dk),
         Norm_time_dk = ((time_dk - min(time_dk))/(max(time_dk) - min(time_dk))),
         Age = Alder,
         Sex = if_else(sex == "mand", "Male", "Female")) %>% 
  rename(Charlson_Comorbidity_Index = sum_ci_age,
         Mental_Illness_Index = sum_psych,
         Length_Hospital_Stay = sengedage_grouped,
         TOKS = ud_toks,
         Grouped_Socioeconomic_Status = three_levels_socio)



### Model for minimal adjusted model:
rcs_age <- as.data.frame(rcspline.eval(data_frame$Age, nk = 4, inclx = FALSE, knots.only = FALSE))
rcs_toks <- as.data.frame(rcspline.eval(data_frame$TOKS, knots = c(5, 7, 9), inclx = FALSE, knots.only = FALSE))
rcs_ci_age <- as.data.frame(rcspline.eval(data_frame$Charlson_Comorbidity_Index, knots = c(1, 2, 4), inclx = FALSE, knots.only = FALSE))
rcs_ci <- as.data.frame(rcspline.eval(data_frame$sum_ci, knots = c(2, 4, 5), inclx = FALSE, knots.only = FALSE))
rcs_sengedag <- as.data.frame(rcspline.eval(data_frame$Length_Hospital_Stay, knots = c(2, 3, 5), inclx = FALSE, knots.only = FALSE))
rcs_time_dk <- as.data.frame(rcspline.eval(data_frame$time_dk, knots = c(2400, 4000, 17000), inclx = FALSE, knots.only = FALSE))

rcs_norm_time <- as.data.frame(rcspline.eval(data_frame$Norm_time_dk, nk = 4, inclx = FALSE, knots.only = FALSE))

# combining the dataframe with the knots from above
data_frame_rcs <- cbind(data_frame, rcs_age$V1, rcs_age$V2, rcs_toks$V1, rcs_ci_age$V1, rcs_ci$V1, rcs_sengedag$V1, rcs_time_dk$V1, rcs_norm_time$V1, rcs_norm_time$V2) 
# renaming the columns for the combined data set. so i know which columns belongs to which knots 
data_frame_rcs <- data_frame_rcs %>% 
  rename(Spline1_age = ...37,
         Spline2_age = ...38,
         Spline1_toks = ...39,
         Spline1_CCI = ...40,
         Spline1_ci = ...41,
         Spline1_Hospital_stay = ...42,
         Spline1_time = ...43,
         Spline1_norm_time = ...44,
         Spline2_norm_time = ...45) %>% 
  arrange(Age) %>% 
  mutate(indvand = if_else(indvandre_type == "immigrant", 1, 0))


#### one minimal adjusted model ####
psw_minimal <- weightit(indvandre_type ~ Age + Spline1_age + Spline2_age + Sex, 
                        data = data_frame_rcs,
                        method = "ps",
                        estimand = "ATT",
                        focal = "immigrant")


### three models for adjusted model, increasing complexity
psw_simple_age <- weightit(indvandre_type ~ Age + Spline1_age + Spline2_age + Sex + Charlson_Comorbidity_Index + Spline1_CCI + Mental_Illness_Index + Grouped_Socioeconomic_Status,
                       data = data_frame_rcs,
                       method = "ps",
                       estimand = "ATT",
                       focal = "immigrant")
# psw_simple <- weightit(indvandre_type ~ Alder + v1_age + v2_age + sex + sum_ci + v1_ci + sum_psych + three_levels_socio,
                    #  data = data_frame_rcs,
                    #   method = "ps",
                    #   estimand = "ATT",
                    #   focal = "immigrant")

# rd_simple <- glm(dead_or_not ~ indvandre_type,
                   #data = data_frame_rcs,
                   #family = binomial(link = "identity"),
                   #weights = psw_simple$weights)
#summary(rd_simple)
#rd_simple_ci <- cbind(coef(rd_simple), confint(rd_simple, level = 0.95)) 
#rd_simple_ci

 
# had to check if it mattered using the charlson comorbidity score without age or the charlson comorbidity with age  


psw_intermediate <- weightit(indvandre_type ~ Norm_time_dk + Spline1_norm_time + Spline2_norm_time + Sex + Charlson_Comorbidity_Index + Spline1_CCI + Mental_Illness_Index + Grouped_Socioeconomic_Status + Length_Hospital_Stay + Spline1_Hospital_stay + TOKS + Spline1_toks, 
                             data = data_frame_rcs,
                             method = "ps",
                             estimand = "ATT",
                             focal = "immigrant")


psw_complex <- weightit(indvandre_type ~ Norm_time_dk + Spline1_norm_time + Spline2_norm_time + Sex + Charlson_Comorbidity_Index + Spline1_CCI + Mental_Illness_Index + Grouped_Socioeconomic_Status + Length_Hospital_Stay + Spline1_Hospital_stay + TOKS + Spline1_toks + Sex * Charlson_Comorbidity_Index,                                
                                 data = data_frame_rcs,
                                 method = "ps",
                                 estimand = "ATT",
                                 focal = "immigrant",
                                 maxit = 500)


#### Function that creates love.plot showing pseudo population with no trimming, 
#### trimmed at 99 and 95 percentile:

BalancePlotMin <- function(weight, title){
  trim99 <- WeightIt::trim(weight,
                           at = 0.99,
                           lower = TRUE)
  trim95 <- WeightIt::trim(weight,
                           at = 0.95,
                           lower = TRUE)
  cobalt::love.plot(weight,
                    var.order = "w1",
                    weights = list(w1 = trim99$weights,
                                   w2 = trim95$weights),
                    abs = TRUE,
                    drop.distance = TRUE,
                    line = TRUE,
                    stars = "raw",
                    thresholds = c(m = 0.1),
                    limits = c(0, 0.15),
                    title = title,
                    alpha = 0.5,
                    shapes = c(15, 16, 17, 18),
                    colors = c("white", "purple", "orange", "red"),
                    sample.names = c("", "Not truncated", "Truncated at 0.1 and 0.99 percentile", "Truncated at 0.5 and 0.95 percentile")) +
    
    ggplot2::theme(legend.title = element_blank())
}


BalancePlot <- function(weight, title){
  trim99 <- WeightIt::trim(weight,
                           at = 0.99,
                           lower = TRUE)
  trim95 <- WeightIt::trim(weight,
                           at = 0.95,
                           lower = TRUE)
  cobalt::love.plot(weight,
                    var.order = "w1",
                    weights = list(w1 = trim99$weights,
                                   w2 = trim95$weights),
                    abs = TRUE,
                    drop.distance = TRUE,
                    line = TRUE,
                    stars = "raw",
                    thresholds = c(m = 0.1),
                    limits = c(0, 0.3),
                    title = title,
                    alpha = 0.5,
                    shapes = c(15, 16, 17, 18),
                    colors = c("white", "purple", "orange", "red"),
                    sample.names = c("", "Not truncated", "Truncated at 0.1 and 0.99 percentile", "Truncated at 0.5 and 0.95 percentile")) +
    
    ggplot2::theme(legend.title = element_blank(),
                   axis.text.y = element_text(size = 11))
}

BalancePlotMin(psw_minimal, "Love plot, minimaly adjusted model")
grid.arrange(BalancePlot(psw_simple_age, "Simple"), BalancePlot(psw_intermediate, "Intermed"), BalancePlot(psw_complex, "Complex"), top = "Love plots of propensity score weighing, three complexities")
#grid.arrange(BalancePlot(psw_simple, "simple"), BalancePlot(psw_simple_age, "simple, chalson+age"), bottom = "Absolute Standardized Mean Difference")

################# risk difference #######################

# for the minimal model 

rd_minimal <- glm(dead_or_not ~ indvandre_type,
                  data = data_frame_rcs,
                  family = binomial(link = "identity"),
                  weights = psw_minimal$weights)
summary(rd)
rd_ci_minimal <- cbind(coef(rd_minimal), confint(rd_minimal, level = 0.95)) 
rd_ci

rd_minimal_df <- as.data.frame(rd_ci_minimal) %>%   
  rename(estimate = V1,
         CI_low = "2.5 %",
         CI_up = "97.5 %")

write_sas(rd_minimal_df, "rd_minimal_psm.sas7bdat")



# for the simple mpdel

rd_simple_age <- glm(dead_or_not ~ indvandre_type,
                     data = data_frame_rcs,
                     family = binomial(link = "identity"),
                     weights = psw_simple_age$weights)
summary(rd_simple)
rd_simple_ci_age <- cbind(coef(rd_simple_age), confint(rd_simple_age, level = 0.95)) 
rd_simple_ci_age

rd_simple_ci_age_df <- as.data.frame(rd_simple_ci_age) %>% 
  rename(estimate = V1,
         CI_low = "2.5 %",
         CI_up = "97.5 %")

write_sas(rd_simple_ci_age_df, "rd_simple_psm.sas7bdat")




## sensetivity analysis, adding time in dk to the logestic regression

# find starting values!!!
coef <- coef(glm(dead_or_not ~ indvandre_type,
                 data = data_frame_rcs,
                 family = binomial(link = "identity"),
                 weights = psw_simple_age$weights))

rd_time <- glm(dead_or_not ~ indvandre_type + Norm_time_dk + Spline1_norm_time + Spline2_norm_time,
              data = data_frame_rcs,
              family = binomial(link = "identity"),
              weights = psw_simple_age$weights,
              start = c(coef, 0.5, 0.3, 0.03))
rd_time_df <- as.data.frame(coef(rd_time)) %>% 
  rename(estimate = "coef(rd_time)")

write_sas(rd_time_df, "rd_simple_time_psm.sas7bdat")



# it wants to have starting
confint(rd_)
rd_ci_time <- cbind(coef(rd_time), confint(rd_time, level = 0.95, start = c(coef, 0.5, 0.3, 0.03)))
rd_time 
rd_ci_time
coef
