# missing fraction plot

pop <- read_rds("final_data_frame_210118.rds")
pop_missing <- pop %>% 
  select(CPR_krypt, Alder, sex, sum_ci, sum_psych, new_socio, indvandre_type, grouped_country_origin, Sengedage, ud_toks, opholds_grundlag, Aktionsdiagnose, time_dk, dage_doed)
  


## missing frequencies for asyl, changing names of variables so it looks better on plot
pop_missing_asyl <- pop_missing %>% 
  filter(opholds_grundlag == "asyl") %>%
  mutate_all(as.factor) %>%
  rename("TOKS at discharge" = "ud_toks",
         "Time in Denmark" = "time_dk",
         "Serius Mental Illness score" = "sum_psych",
         "Charlson Comorbidity Index" = "sum_ci",
         "Sex" = "sex",
         "Length of stay at hospital" = "Sengedage",
         "Immigration status" = "opholds_grundlag",
         "Socioeconomic status" = "new_socio",
         "Country of origin" = "grouped_country_origin",
         "Age" = "Alder", 
         "Diagnosis" = "Aktionsdiagnose",
         "Death date" = "dage_doed") %>% 
  select(-indvandre_type, -"Immigration status")

pop_missing_asyl_long <- pivot_longer(pop_missing_asyl, !CPR_krypt, names_to = "varible", values_to = "value")

# all missing values is true and not missing is false
pop_missing_asyl_T_F <- pop_missing_asyl_long %>% 
  group_by(varible) %>% 
  count(t_f = is.na(value)) %>% 
  mutate(t_f = tolower(t_f))


pop_missing_asyl_wide <- pivot_wider(pop_missing_asyl_T_F, id_cols = varible, names_from = t_f, values_from = n)
pop_missing_asyl_wide <- pop_missing_asyl_wide %>% 
  mutate(true = replace_na(true, 0)) #na values pressent that there are no missing (no true's)

# making the fractions:
pop_missing_freq_asyl <- pop_missing_asyl_wide %>% 
  group_by(varible) %>% 
  summarise(freq = true/(true+false), true = true) %>% 
  mutate(asyl_or_not = "Refugee")


##missing freq for DK

pop_missing_dk <- pop_missing %>% 
  filter(indvandre_type == "dane") %>%
  mutate_all(as.factor)%>%
  select(-indvandre_type, -opholds_grundlag) %>% 
  rename("TOKS at discharge" = "ud_toks",
         "Time in Denmark" = "time_dk",
         "Serius Mental Illness score" = "sum_psych",
         "Charlson Comorbidity Index" = "sum_ci",
         "Sex" = "sex",
         "Length of stay at hospital" = "Sengedage",
         "Socioeconomic status" = "new_socio",
         "Country of origin" = "grouped_country_origin",
         "Age" = "Alder", 
         "Diagnosis" = "Aktionsdiagnose",
         "Death date" = "dage_doed")
  

pop_missing_dk_long <- pivot_longer(pop_missing_dk, !CPR_krypt, names_to = "varible", values_to = "value")

pop_missing_dk_T_F <- pop_missing_dk_long %>% 
  group_by(varible) %>% 
  count(t_f = is.na(value)) %>% 
  mutate(t_f = tolower(t_f))

pop_missing_dk_wide <- pivot_wider(pop_missing_dk_T_F, id_cols = varible, names_from = t_f, values_from = n)

pop_missing_dk_wide <- pop_missing_dk_wide %>% 
  mutate(true = replace_na(true, 0))

pop_missing_freq_dk <- pop_missing_dk_wide %>% 
  group_by(varible) %>% 
  summarise(freq = true/(true+false), true = true) %>% 
  mutate(asyl_or_not = "Native Dane")

# joining dk and asyl: 
missing_dk_asyl <- rbind(pop_missing_freq_asyl, pop_missing_freq_dk)
write_sas(missing_dk_asyl, "table_missings.sas7bdat")

# sorting fo that it follows table 1:
sort_missing_dk_asyl <- missing_dk_asyl %>% 
  mutate(varible = fct_relevel(varible, "Death date", "Diagnosis", "Serius Mental Illness score", "Charlson Comorbidity Index",
                               "Length of stay at hospital", "TOKS at discharge",
                               "Socioeconomic status", "Time in Denmark", "Country of origin",
                               "Sex", "Age"))

ggplot(sort_missing_dk_asyl, aes(freq, varible, shape = asyl_or_not, colour = asyl_or_not)) + 
  geom_point() +
  ggtitle("Fraction plot of missing values") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlim(0, 0.5) + 
  xlab("Fraction of missing values") +
  ylab("Variables") + 
  theme(legend.title = element_blank()) +
  scale_colour_manual(values = c("orange", "purple")) +
  scale_shape_manual(values = c(1, 2))


