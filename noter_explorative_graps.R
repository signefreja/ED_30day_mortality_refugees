# explorative graphs: 
data_frame <- readRDS("E:/ProjektDB/ALMAU/Workdata/707842/final_data_frame_210118.rds")


#cont-cont plot
data_cont <- data_frame %>% 
  select(Alder, sum_ci_age, sum_psych, ud_toks, time_dk, Sengedage) %>% 
  mutate(time_dk_years = time_dk/365.25) %>% 
  mutate_all(as.numeric)
 

# Function for continouse plots:
cont_cont_plot <- function(data, x, y, xtitle, ytitle){
  ggplot(data, aes(x, y)) +
    geom_count(aes(colour = ..n..), alpha = 0.4, show.legend = FALSE) +
    scale_colour_viridis_c() +
    xlab(xtitle) +
    ylab(ytitle)
}


cont_cont_plot(data_cont, data_cont$Alder, data_cont$Alder, "Age", "Age")
cont_cont_plot(data_cont, data_cont$Alder, data_cont$sum_ci_age, "Age", "Charlson comorbidity score, incl. age")
cont_cont_plot(data_cont, data_cont$sum_ci_age, data_cont$sum_ci_age, "Charlson comorbidity score, incl. age", "Charlson comorbidity score, incl. age")
cont_cont_plot(data_cont, data_cont$Alder, data_cont$sum_psych, "Age", "Mental Ilness score")
cont_cont_plot(data_cont, data_cont$sum_psych, data_cont$sum_psych, "Mental Ilness score", "Mental Ilness score")
cont_cont_plot(data_cont, data_cont$Alder, data_cont$ud_toks, "Age", "TOKS")
cont_cont_plot(data_cont, data_cont$ud_toks, data_cont$ud_toks, "TOKS", "TOKS")
cont_cont_plot(data_cont, data_cont$Alder, data_cont$time_dk_years, "Age", "Years in Denmark")
cont_cont_plot(data_cont, data_cont$time_dk_years, data_cont$time_dk_years, "Years in Denmark", "Years in Denmark")
cont_cont_plot(data_cont, data_cont$time_dk_years, data_cont$sum_ci_age, "Years in Denmark", "Charlson comorbidity score, incl. age")
cont_cont_plot(data_cont, data_cont$time_dk_years, data_cont$sum_psych, "Years in Denmark", "Mental Ilness score")
cont_cont_plot(data_cont, data_cont$time_dk_years, data_cont$ud_toks, "Years in Denmark",  "TOKS")
cont_cont_plot(data_cont, data_cont$sum_psych, data_cont$sum_ci_age, "Mental Ilness score", "Charlson comorbidity score, incl. age")


####### categorical data ##########
cat_data <- data_frame %>% 
  select(sex, socio_tekst, three_levels_socio, country_origin, grouped_country_origin_who, indvandre_type, dead_or_not) %>% 
  mutate(sex = as.factor(sex), 
         socio_tekst = as.factor(socio_tekst), 
         three_levels_socio = as.factor(three_levels_socio), 
         country_origin = as.factor(country_origin), 
         grouped_country_origin_who = as.factor(grouped_country_origin_who), 
         indvandre_type = as.factor(indvandre_type))


# plot of country of origin, grouped country of origin and sex # 

cat_co_f <- cat_data %>% 
  filter(indvandre_type == "immigrant", sex == "kvinde") %>% 
  select(country_origin, grouped_country_origin_who) %>% 
  pivot_longer(names_to = "varible", values_to = "values", cols = c(country_origin, grouped_country_origin_who)) %>% 
  group_by(varible) %>% 
  mutate(length_varib = length(varible)) %>% 
  group_by(values) %>% 
  mutate(length_values = length(values), 
         freq = length_values / length_varib,
         female_male = "female")
cat_co_m <- cat_data %>% 
  filter(indvandre_type == "immigrant", sex == "mand") %>% 
  select(country_origin, grouped_country_origin_who) %>% 
  pivot_longer(names_to = "varible", values_to = "values", cols = c(country_origin, grouped_country_origin_who)) %>% 
  group_by(varible) %>% 
  mutate(length_varib = length(varible)) %>% 
  group_by(values) %>% 
  mutate(length_values = length(values), 
         freq = length_values / length_varib,
         female_male = "male")
cat_co_fm <- rbind(cat_co_f, cat_co_m)
cat_co_fm_sort <- cat_co_fm %>% 
  mutate(values = fct_relevel(values,    
                              "Denmark", 
                              "udlandet_uoplyst", "statsloes", "colombia", "kina", "vietnam", "other", 
                              "myanmar", "sri_lanka", "bhutan", "South_East_Asian",
                              "serbien", "kroatien", "jugoslavien", "bosnien_hercegovina", "ex-yugosalavia",
                              "aserbajdsjan", "rusland", "isreal","European", 
                              "togo", "etiopien",  "centralafrikansk_republik", "algeriet",
                              "cameroun", "angola", "rwanda", "congo_republikken", "congo_demokratisk_republik", 
                              "eritrea", "African", 
                              "mellemoesten_uoplyst", "pakistan", "saudi_arabien","jordan", 
                              "forenede_arabiske_emirater", "sudan", "egypten", "libanon", "kuwait",  
                               "afghanistan", "iran", "somalia", "syrien", "irak", "Eastern_Mediterranean"))


ggplot(cat_co_fm_sort, aes(x = freq, y = values, shape = female_male, colour = female_male)) +
  geom_point() +
  ggtitle("Fraction plot of country of origin, stratified on sex") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Fraction") +
  ylab("Country of origin") + 
  theme(legend.title = element_blank()) +
  scale_colour_manual(values = c("orange", "purple")) +
  scale_shape_manual(values = c(1, 2))



# plot of socioeconomic status, grouped socio and refugee status # 
cat_co_r <- cat_data %>% 
  filter(indvandre_type == "immigrant") %>% 
  select(socio_tekst, three_levels_socio) %>% 
  pivot_longer(names_to = "varible", values_to = "values", cols = c(socio_tekst, three_levels_socio)) %>% 
  group_by(varible) %>% 
  mutate(length_varib = length(varible)) %>% 
  group_by(values) %>% 
  mutate(length_values = length(values), 
         freq = length_values / length_varib,
         female_male = "refugee")
cat_co_d <- cat_data %>% 
  filter(indvandre_type == "dane") %>% 
  select(socio_tekst, three_levels_socio) %>% 
  pivot_longer(names_to = "varible", values_to = "values", cols = c(socio_tekst, three_levels_socio)) %>% 
  group_by(varible) %>% 
  mutate(length_varib = length(varible)) %>% 
  group_by(values) %>% 
  mutate(length_values = length(values), 
         freq = length_values / length_varib,
         female_male = "dane")
cat_co_rd_socio <- rbind(cat_co_r, cat_co_d)

cat_co_rd_socio_sort <- cat_co_rd_socio %>% 
  mutate(values = fct_relevel(values, "under_uddan_min_15aar", "student",   
                              "loen_ledelse",
                               "loen_andre",
                              "selvstaendig_10_eller_flere", "selvstaendig_5_9", "selvstaendig_1_4", "selvstaendig_ingen",
                              "loen_stilling_ikke_oplyst", "loen_faerdigheder_hoejeste", 
                              "loen_faerdigheder_mellem", 
                              "loen_faerdigheder_grund", "working",
                              "medarbejdende_aegtefaelle",  "arbejs_loes_min_halvdelen_aaret", 
                              "modtager_syge_su_orlov_mm", "efterloensmodtager", 
                              "andre", "foetidspensionist", "folkepensionist", 
                              "kontathjælp", "not_working"))



ggplot(cat_co_rd_socio_sort, aes(x = freq, y = values, shape = female_male, colour = female_male)) +
  geom_point() +
  ggtitle("Fraction plot of socioeconomic status, stratified on refugee status") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Fraction") +
  ylab("Socioeconomic status") + 
  theme(legend.title = element_blank()) +
  scale_colour_manual(values = c("orange", "purple")) +
  scale_shape_manual(values = c(1, 2))


# plot of country of origin and socioeconomic status # 

cat_co_socio_a <- cat_data %>% 
  filter(indvandre_type == "immigrant", grouped_country_origin_who == "African") %>% 
  select(grouped_country_origin_who, three_levels_socio) %>% 
  group_by(three_levels_socio) %>% 
  mutate(length_socio = length(three_levels_socio)) %>% 
  group_by(grouped_country_origin_who) %>% 
  mutate(length_co = length(grouped_country_origin_who), 
         freq = length_socio / length_co)
cat_co_socio_e <- cat_data %>% 
  filter(indvandre_type == "immigrant", grouped_country_origin_who == "European") %>% 
  select(grouped_country_origin_who, three_levels_socio) %>% 
  group_by(three_levels_socio) %>% 
  mutate(length_socio = length(three_levels_socio)) %>% 
  group_by(grouped_country_origin_who) %>% 
  mutate(length_co = length(grouped_country_origin_who), 
         freq = length_socio / length_co)

cat_co_socio_ex <- cat_data %>% 
  filter(indvandre_type == "immigrant", grouped_country_origin_who == "ex-yugosalavia") %>% 
  select(grouped_country_origin_who, three_levels_socio) %>% 
  group_by(three_levels_socio) %>% 
  mutate(length_socio = length(three_levels_socio)) %>% 
  group_by(grouped_country_origin_who) %>% 
  mutate(length_co = length(grouped_country_origin_who), 
         freq = length_socio / length_co)

cat_co_socio_se <- cat_data %>% 
  filter(indvandre_type == "immigrant", grouped_country_origin_who == "South_East_Asian") %>% 
  select(grouped_country_origin_who, three_levels_socio) %>% 
  group_by(three_levels_socio) %>% 
  mutate(length_socio = length(three_levels_socio)) %>% 
  group_by(grouped_country_origin_who) %>% 
  mutate(length_co = length(grouped_country_origin_who), 
         freq = length_socio / length_co)

cat_co_socio_o <- cat_data %>% 
  filter(indvandre_type == "immigrant", grouped_country_origin_who == "other") %>% 
  select(grouped_country_origin_who, three_levels_socio) %>% 
  group_by(three_levels_socio) %>% 
  mutate(length_socio = length(three_levels_socio)) %>% 
  group_by(grouped_country_origin_who) %>% 
  mutate(length_co = length(grouped_country_origin_who), 
         freq = length_socio / length_co)

cat_co_socio_em <- cat_data %>% 
  filter(indvandre_type == "immigrant", grouped_country_origin_who == "Eastern_Mediterranean") %>% 
  select(grouped_country_origin_who, three_levels_socio) %>% 
  group_by(three_levels_socio) %>% 
  mutate(length_socio = length(three_levels_socio)) %>% 
  group_by(grouped_country_origin_who) %>% 
  mutate(length_co = length(grouped_country_origin_who), 
         freq = length_socio / length_co)
cat_co_socio <- rbind(cat_co_socio_e, cat_co_socio_em, cat_co_socio_ex, cat_co_socio_a, cat_co_socio_se, cat_co_socio_o)


cat_co_socio_sort <- cat_co_socio %>% 
  mutate(grouped_country_origin_who = fct_relevel(grouped_country_origin_who, "other", "South_East_Asian", "ex-yugosalavia", "European","African","Eastern_Mediterranean"))


ggplot(cat_co_socio_sort, aes(x = freq, y = grouped_country_origin_who, shape = three_levels_socio, colour = three_levels_socio)) +
  geom_point() +
  ggtitle("Fraction plot of country of origin, stratified on socioeconomic status") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Fraction") +
  ylab("Region of origin") + 
  theme(legend.title = element_blank()) +
  scale_colour_manual(values = c("orange", "purple", "green")) +
  scale_shape_manual(values = c(1, 2, 3))

# plot socioeconomic status and sex # 

cat_sex_socio_w <- cat_data %>% 
  filter(three_levels_socio == "working") %>% 
  select(three_levels_socio, sex) %>% 
  group_by(three_levels_socio) %>% 
  mutate(length_socio = length(three_levels_socio)) %>% 
  group_by(sex) %>% 
  mutate(length_sex = length(sex), 
         freq = length_sex / length_socio)

cat_sex_socio_nw <- cat_data %>% 
  filter(three_levels_socio == "not_working") %>% 
  select(three_levels_socio, sex) %>% 
  group_by(three_levels_socio) %>% 
  mutate(length_socio = length(three_levels_socio)) %>% 
  group_by(sex) %>% 
  mutate(length_sex = length(sex), 
         freq = length_sex / length_socio)

cat_sex_socio_s <- cat_data %>% 
  filter(three_levels_socio == "student") %>% 
  select(three_levels_socio, sex) %>% 
  group_by(three_levels_socio) %>% 
  mutate(length_socio = length(three_levels_socio)) %>% 
  group_by(sex) %>% 
  mutate(length_sex = length(sex), 
         freq = length_sex / length_socio)

cat_sex_socio <- rbind(cat_sex_socio_w, cat_sex_socio_s, cat_sex_socio_nw)

ggplot(cat_sex_socio, aes(x = freq, y = three_levels_socio, shape = sex, colour = sex)) +
  geom_point() +
  ggtitle("Fraction plot of socioeconomic status, stratified on sex") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlim(0, 0.6) +
  xlab("Fraction") +
  ylab("Socio economic status") + 
  theme(legend.title = element_blank()) +
  scale_colour_manual(values = c("orange", "purple")) +
  scale_shape_manual(values = c(1, 2))

# death and region of origin # 
cat_co_death <- cat_data %>% 
  select(grouped_country_origin_who, dead_or_not) %>% 
  mutate(dead_or_not = as.numeric(dead_or_not)) %>% 
  group_by(grouped_country_origin_who) %>% 
  mutate(sum_dead = sum(dead_or_not, na.rm = TRUE))


cat_co_death_em <- cat_data %>% 
  filter(grouped_country_origin_who == "Eastern_Mediterranean") %>% 
  select(dead_or_not, grouped_country_origin_who) %>% 
  ungroup() %>% 
  mutate(freq = sum(dead_or_not, na.rm = TRUE) / length(grouped_country_origin_who))

cat_co_death_a <- cat_data %>% 
  filter(grouped_country_origin_who == "African") %>% 
  select(dead_or_not, grouped_country_origin_who) %>% 
  ungroup() %>% 
  mutate(freq = sum(dead_or_not, na.rm = TRUE) / length(grouped_country_origin_who))
cat_co_death_e <- cat_data %>% 
  filter(grouped_country_origin_who == "European") %>% 
  select(dead_or_not, grouped_country_origin_who) %>% 
  ungroup() %>% 
  mutate(freq = sum(dead_or_not, na.rm = TRUE) / length(grouped_country_origin_who))
cat_co_death_ex <- cat_data %>% 
  filter(grouped_country_origin_who == "Ex-Yugoslavian") %>% 
  select(dead_or_not, grouped_country_origin_who) %>% 
  ungroup() %>% 
  mutate(freq = sum(dead_or_not, na.rm = TRUE) / length(grouped_country_origin_who))
cat_co_death_se <- cat_data %>% 
  filter(grouped_country_origin_who == "South_East_Asian") %>% 
  select(dead_or_not, grouped_country_origin_who) %>% 
  ungroup() %>% 
  mutate(freq = sum(dead_or_not, na.rm = TRUE) / length(grouped_country_origin_who))
cat_co_death_o <- cat_data %>% 
  filter(grouped_country_origin_who == "Other") %>% 
  select(dead_or_not, grouped_country_origin_who) %>% 
  ungroup() %>% 
  mutate(freq = sum(dead_or_not, na.rm = TRUE) / length(grouped_country_origin_who))
cat_co_death_dk <- cat_data %>% 
  filter(grouped_country_origin_who == "Danish") %>% 
  select(dead_or_not, grouped_country_origin_who) %>% 
  ungroup() %>% 
  mutate(freq = sum(dead_or_not, na.rm = TRUE) / length(grouped_country_origin_who))


  
cat_co_death <- rbind(cat_co_death_o, cat_co_death_se, cat_co_death_e, cat_co_death_ex, cat_co_death_a, cat_co_death_em, cat_co_death_dk)  
cat_co_death_sort <- cat_co_death %>% 
  mutate(grouped_country_origin_who = case_when(grouped_country_origin_who %in% "Other" ~ "Other", 
                                                grouped_country_origin_who %in% "South_East_Asian" ~ "South East Asian", 
                                                grouped_country_origin_who %in% "European" ~ "European",
                                                grouped_country_origin_who %in% "Ex-Yugoslavian" ~ "Ex-Yugoslavian", 
                                                grouped_country_origin_who %in% "African" ~ "African", 
                                                grouped_country_origin_who %in% "Eastern_Mediterranean" ~ "Eastern Mediterranean",
                                                grouped_country_origin_who %in% "Danish" ~ "Danish"),
         grouped_country_origin_who = fct_relevel(grouped_country_origin_who,
                                                  "Other", "South East Asian", "European",
                                                  "Ex-Yugoslavian", "African", "Eastern Mediterranean",
                                                  "Danish"))

ggplot(cat_co_death_sort, aes(x = freq, y = grouped_country_origin_who)) +
  geom_point() +
  ggtitle("Fraction plot of death, by region of origin") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Fraction") +
  ylab("Region of origin") + 
  theme(legend.title = element_blank()) +
  scale_x_continuous(breaks = seq(0, 0.07, 0.01), expand = expansion(mult = c(0.07)))
  
############# plot categorical data with continouse data ################

cat_cont <- data_frame %>% 
  select(time_dk, ud_toks, Alder, sum_ci_age, sum_ci, sum_psych, 
         three_levels_socio, socio_tekst, country_origin, grouped_country_origin_who, sex, indvandre_type) %>% 
  mutate(sex = as.factor(sex), 
         socio_tekst = as.factor(socio_tekst), 
         three_levels_socio = as.factor(three_levels_socio), 
         country_origin = as.factor(country_origin), 
         grouped_country_origin_who = as.factor(grouped_country_origin_who), 
         indvandre_type = as.factor(indvandre_type),
         time_dk = time_dk/365,23)
  
# time in dk as x axis: 
cont_cont_plot(cat_cont, cat_cont$time_dk, cat_cont$three_levels_socio, "Time in Denmark, years", "Grouped socioeconomic status")
cont_cont_plot(cat_cont, cat_cont$time_dk, cat_cont$socio_tekst, "Time in Denmark, years", "Socioeconomic status")
cont_cont_plot(cat_cont, cat_cont$time_dk, cat_cont$country_origin, "Time in Denmark, years", "Country of origin")
cont_cont_plot(cat_cont, cat_cont$time_dk, cat_cont$grouped_country_origin_who, "Time in Denmark, years", "Region of origin")
cont_cont_plot(cat_cont, cat_cont$time_dk, cat_cont$sex, "Time in Denmark, years", "Sex")


# age as the x axis:
cont_cont_plot(cat_cont, cat_cont$Alder, cat_cont$three_levels_socio, "Age", "Grouped socioeconomic status")
cont_cont_plot(cat_cont, cat_cont$Alder, cat_cont$socio_tekst, "Age", "Socioeconomic status")
cont_cont_plot(cat_cont, cat_cont$Alder, cat_cont$country_origin, "Age", "Country of origin")
cont_cont_plot(cat_cont, cat_cont$Alder, cat_cont$grouped_country_origin_who, "Age", "Region of origin")
cont_cont_plot(cat_cont, cat_cont$Alder, cat_cont$sex, "Age", "Sex")
cont_cont_plot(cat_cont, cat_cont$Alder, cat_cont$indvandre_type, "Age", "Refugee status")

# toks as the x axis:
cont_cont_plot(cat_cont, cat_cont$ud_toks, cat_cont$three_levels_socio, "TOKS, early warning score", "Grouped socioeconomic status")
cont_cont_plot(cat_cont, cat_cont$ud_toks, cat_cont$socio_tekst, "TOKS, early warning score", "Socioeconomic status")
cont_cont_plot(cat_cont, cat_cont$ud_toks, cat_cont$country_origin, "TOKS, early warning score", "Country of origin")
cont_cont_plot(cat_cont, cat_cont$ud_toks, cat_cont$grouped_country_origin_who, "TOKS, early warning score", "Region of origin")
cont_cont_plot(cat_cont, cat_cont$ud_toks, cat_cont$sex, "TOKS, early warning score", "Sex")
cont_cont_plot(cat_cont, cat_cont$ud_toks, cat_cont$indvandre_type, "TOKS, early warning score", "Refugee status")

# charlson comorbidity score, age included as x-axsis
cont_cont_plot(cat_cont, cat_cont$sum_ci_age, cat_cont$three_levels_socio, "Charlson comorbidity score, incl. age", "Grouped socioeconomic status")
cont_cont_plot(cat_cont, cat_cont$sum_ci_age, cat_cont$socio_tekst, "Charlson comorbidity score, incl. age", "Socioeconomic status")
cont_cont_plot(cat_cont, cat_cont$sum_ci_age, cat_cont$country_origin, "Charlson comorbidity score, incl. age", "Country of origin")
cont_cont_plot(cat_cont, cat_cont$sum_ci_age, cat_cont$grouped_country_origin_who, "Charlson comorbidity score, incl. age", "Region of origin")
cont_cont_plot(cat_cont, cat_cont$sum_ci_age, cat_cont$sex, "Charlson comorbidity score, incl. age", "Sex")
cont_cont_plot(cat_cont, cat_cont$sum_ci_age, cat_cont$indvandre_type, "Charlson comorbidity score, incl. age", "Refugee status")

# mental illness score as x-axis
cont_cont_plot(cat_cont, cat_cont$sum_psych, cat_cont$three_levels_socio, "Mental ilness score", "Grouped socioeconomic status")
cont_cont_plot(cat_cont, cat_cont$sum_psych, cat_cont$socio_tekst, "Mental ilness score", "Socioeconomic status")
cont_cont_plot(cat_cont, cat_cont$sum_psych, cat_cont$country_origin, "Mental ilness score", "Country of origin")
cont_cont_plot(cat_cont, cat_cont$sum_psych, cat_cont$grouped_country_origin_who, "Mental ilness score", "Region of origin")
cont_cont_plot(cat_cont, cat_cont$sum_psych, cat_cont$sex, "Mental ilness score", "Sex")
cont_cont_plot(cat_cont, cat_cont$sum_psych, cat_cont$indvandre_type, "Mental ilness score", "Refugee status")






################################ old ###########################################
## continouse varibler: 
# Age: 

ggplot(data_frame, aes(Alder, Alder)) +
  geom_point()


with(data_frame, ggfreqScatter(Alder, Alder, 
                               bins = 70, 
                               g = 10,
                               xlab = "Age",
                               ylab = "Age")) # den er ok#

ggplot(data_frame, aes(Alder, Alder)) +
  geom_count(aes(colour = ..n..), alpha = 0.1) +
  scale_color_viridis_c()

ggplot(data_frame, aes(Alder)) + 
  geom_histogram(bins = 100, colour = "black", fill = "white") +
  scale_x_continuous(breaks = seq(15, 110, 5), lim = c(18, 110))



# alder og ci
with(data_frame, ggfreqScatter(Alder, sum_ci_age, bins = 70, g = 20))

ggplot(data_frame, aes(Alder, sum_ci_age)) +
  geom_count(aes(colour = ..n..)) +
  scale_color_viridis_c() +
  xlab("age") +
  ylab("charlson comorbidity score, incl. age")


# ci_alder mod sig selv
with(data_frame_variabler, ggfreqScatter(sum_ci_age, sum_ci_age, bins = 70, g = 10))

## categorical variabler: 
# sex og socioeco

sex_ses <- data_frame %>% 
  select(sex, socio_tekst) 
sex_ses_n <- sex_ses %>% 
  group_by(socio_tekst) %>% 
  summarise(n_socio = length(socio_tekst), n_male = length(sex[sex == "mand"]), n_female = length(sex[sex == "kvinde"]))
long_sex_ses <- pivot_longer(sex_ses_n,
                             cols = c("n_male", "n_female"),
                             names_to = "sex",
                             values_to = "sex_n") %>% 
  select(-n_socio)

ggplot(long_sex_ses, aes(sex_n, socio_tekst, colour = sex)) +
  geom_point()



# sex og alder
sex_age <- data_frame_variabler %>% 
  select(Alder, sex) %>% 
  mutate(age_round = round(Alder, digits = 0))
sex_age_n <- sex_age %>% 
  group_by(age_round) %>% 
  summarise(n_male = length(age_round[sex == "mand"]), n_female = length(age_round[sex == "kvinde"]))

long_sex_age <- pivot_longer(sex_age_n,
                             cols = c("n_male", "n_female"),
                             names_to = "sex",
                             values_to = "sex_n")

ggplot(long_sex_age, aes(age_round, sex_n, colour = sex)) +
  geom_point() +
  geom_line()

age_sex_violin <- ggplot(data_frame_variabler, aes(sex, Alder)) +
  geom_violin()

age_sex_violin + 
  geom_dotplot(binaxis = "y", binwidth = 5, stackdir = "center", dotsize = 0.2)

age_sex_violin <- ggplot(data_frame_variabler, aes(sex, Alder)) +
  geom_violin()

age_sex_violin +
  geom_count(
    aes(x = sex, 
        y = Alder,
        colour = ..n..),
    alpha = 0.5)

# alder og ses
#ci as categorical: 
data_frame_variabler_cat_ci <- data_frame_variabler %>% 
  mutate(sum_ci_factor = as.factor(sum_ci))

age_ses_ci <- ggplot(data_frame_variabler_cat_ci, aes(Alder, 
                                        socio_tekst, 
                                        colour = sum_ci_factor)) +
  geom_point()

age_ses_ci + scale_colour_manual(breaks = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15"),
                                 values = c("green", "blue", "purple", "yellow", "orange", "red", "red", "red", "red", "red", "red", "red", "red", "red", "red", "red"))

with(data_frame_variabler_cat_ci, ggfreqScatter(Alder, socio_tekst, bins = 70, g = 20))

ggplot(data_frame_variabler, aes(Alder, socio_tekst)) +
  geom_count(aes(colour = ..n..)) + 
  scale_color_viridis_c()



# ci og ses
ggplot(data_frame_variabler, aes(sum_ci, socio_tekst)) + 
  geom_point()

with(data_frame_variabler_cat_ci, ggfreqScatter(sum_ci, socio_tekst, bins = 70, g = 20))

ggplot(data_frame_variabler_cat_ci) +
  geom_mosaic(aes(x = product(socio_tekst, sum_ci_factor), fill = sum_ci_factor)) +
  labs(x = "socio", title = "laalaalaa")

#ci og sex
ggplot(data_frame_variabler_cat_ci) +
  geom_mosaic(aes(product(sum_ci_factor, sex), fill = sum_ci_factor))

with(data_frame_variabler, ggfreqScatter(sum_ci, sex, bins = 70, g = 20))

ggplot(data_frame_variabler, aes(sum_ci, sex)) + 
  geom_point()              



ggplot(data_frame, aes(Sengedage)) + 
  geom_histogram(bins = 100, colour = "black", fill = "white") +
  scale_x_continuous(breaks = seq(0, 210, 5), lim = c(0, 100))
