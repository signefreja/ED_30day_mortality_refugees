######### -Kaplan Meier- ##########

### cheking assumption: The survival probabilities are the same for participants, recruted early and late:
# Akutmodtagelsen flyttede ud til skejby i 2018, kunne påvirke survival probilities
# creating a Kaplan Meier pplot, stratificeret på 2016, 20117, 2018 amt pooled med danskerer vs indvandrere

# Creating dataset with variables, year of visit, days to event/censored, immigration type needed for Kaplan Meier analysis

data_set <- read_rds("final_data_frame_210118.rds")

data_set_km <- data_set %>% 
  select(CPR_krypt, Forloeb_ID, dage_doed, Nuv_Statustekst, sex, DatoTid_Ind, DatoTid_Udskr, indvandre_type, dead_or_not) %>% 
  mutate(indvandre_type = if_else(indvandre_type == "dane", "Native Dane", "Refugee"))

# Creating variable for year of visit
pop_km_final <- data_set_km %>% 
  mutate(aarstal_ind = str_sub(DatoTid_Ind, 1, 4)) %>% 
  select(dage_doed, indvandre_type, dead_or_not, aarstal_ind)  
  distinct() # da der joinede sammen blev der lavet dobbelt af ca. 500 par, men er identiske...



# Kaplan Meier plot, stratified on refugees and natve Danes

km_refugee <- survfit(Surv(dage_doed, dead_or_not) ~ indvandre_type, 
                      data = pop_km_final,
                      ctype = 1) 

ggsurvplot(km_refugee, 
           data = pop_km_final,
           xlim = c(0, 30),
           break.x.by = 5,
           ylim = c(0.94, 1.00),
           censor = FALSE,
           conf.int = TRUE,
           conf.int.style = "step",
           title = "Kaplan-Meier plot stratified on native Danes and refugees",
           xlab = "Days from discharge to death",
           risk.table = FALSE,
           legend.title = "Native Dane or refugee:",
           legend.labs = c("Native Dane n = 28,626", "Refugee n = 631"),
           palette = c("purple", "orange"),
           ggtheme = theme(plot.title = element_text(hjust = 0.5),
                           plot.subtitle = element_text(hjust = 0.5)))
table_km_indvandre
  

# Getting the 30 events:
table_km_indvandre <- summary(km_refugee, times = seq(0, 30)) # de første 30 events

dk_prob <- table_km_indvandre$surv[30]
dk_ci_low <- table_km_indvandre$lower[30]
dk_ci_up <- table_km_indvandre$upper[30]

asyl_prob <- table_km_indvandre$surv[60]
asyl_ci_low <- table_km_indvandre$lower[60]
asyl_ci_up <- table_km_indvandre$upper[60]

dk_asyl_surv_ci <- rbind(dk_prob, dk_ci_low, dk_ci_up, asyl_prob, asyl_ci_low, asyl_ci_up)
dk_asyl_surv_ci <- as.data.frame(dk_asyl_surv_ci)

write_sas(dk_asyl_surv_ci, "km_surv_prob_may.sas7bdat")
                         
head(table_km_indvandre)

# KM stratified on year of visit:

km_aar <- survfit(Surv(dage_doed, dead_or_not) ~ aarstal_ind + indvandre_type,
                  data = pop_km_final,
                  ctype = 1,
                  conf.type = "log-log")

ggsurvplot(km_aar, 
           data = pop_km_final,
           xlim = c(0, 30),
           ylim = c(0.915, 1.00),
           break.x.by = 5,
           censor = FALSE,
           conf.int = TRUE,
           conf.int.style = "step",
           xlab = "Days from discharge to death",
           title = "Kaplan-Meier plot stratified on native Danes and refugees and year of visit",
           legend.title = " Refugee or native Dane and year:",
           palette = c("orange", "purple", "red", "pink", "brown", "seagreen"),
           legend.labs = c("Native Dane, 2016 n = 6602",
                           "Refugee 2016 n = 153",
                           "Native Dane, 2017 n = 9343", 
                           "Refugee 2017 n = 185",
                           "Native Dane, 2018 n = 12,681",
                           "Refugee 2018 n = 293"),
           ggtheme = theme(plot.title = element_text(hjust = 0.5)))
dk <- pop_km_final %>% 
  filter(indvandre_type == "Native Dane") 
table(dk$aarstal_ind)
ref <- pop_km_final %>% 
  filter(indvandre_type == "Refugee")
table(ref$aarstal_ind)
table_km_aar <- summary(km_aar, times = seq(0, 30))
table_km_aar

# creating table for 30 days survival of refugees and danes stratified on visit year

dk16_prob <- table_km_aar$surv[31]
dk16_ci_low <- table_km_aar$lower[31]
dk16_ci_up <- table_km_aar$upper[31]

dk17_prob <- table_km_aar$surv[93]
dk17_ci_low <- table_km_aar$lower[93]
dk17_ci_up <- table_km_aar$upper[93]

dk18_prob <- table_km_aar$surv[155]
dk18_ci_low <- table_km_aar$lower[155]
dk18_ci_up <- table_km_aar$upper[155]


asyl16_prob <- table_km_aar$surv[61]
asyl16_ci_low <- table_km_aar$lower[61]
asyl16_ci_up <- table_km_aar$upper[61]

asyl17_prob <- table_km_aar$surv[121]
asyl17_ci_low <- table_km_aar$lower[121]
asyl17_ci_up <- table_km_aar$upper[121]

asyl18_prob <- table_km_aar$surv[181]
asyl18_ci_low <- table_km_aar$lower[181]
asyl18_ci_up <- table_km_aar$upper[181]

dk_asyl_surv_prob <- rbind(dk16_prob, dk17_prob, dk18_prob, asyl16_prob, asyl17_prob, asyl18_prob)
dk_asyl_ci_low <- rbind(dk16_ci_low, dk17_ci_low, dk18_ci_low, asyl16_ci_low, asyl17_ci_low, asyl18_ci_low)
dk_asyl_ci_up <- rbind(dk16_ci_up, dk17_ci_up, dk18_ci_up, asyl16_ci_up, asyl17_ci_up, asyl18_ci_up)

table_30_aar <- cbind(dk_asyl_surv_prob, dk_asyl_ci_low, dk_asyl_ci_up)

table_30_aar <- as.data.frame(table_30_aar) %>% 
  rename(thirty_days_surv_prob = V1,
         ci_low = V2,
         ci_up = V3)
write_sas(table_30_aar, "table_km_stratified_year_may.sas7bdat")

# Kaplan Meier for indvandrer og sex
km_sex_indvandrer <- survfit(Surv(dage_doed, dead_or_not) ~ indvandre_type + sex,
                             data = pop_km_final,
                             ctype = 1,
                             conf.type = "log-log")
ggsurvplot(km_sex_indvandrer, 
           data = pop_km_final,
           ylim = c(0.75, 1.00),
           censor = FALSE,
           conf.int = TRUE,
           conf.int.style = "step",
           xlab = "Antal dage fra udskrivelse til doed",
           title = "Kaplan-Meier, stratificeret på sex og indvandrere type",
           legend.labs = c("DK, kvinde",
                           "DK, mand",
                           "Immigrant, kvinde",
                           "Immigrant, mand"))

