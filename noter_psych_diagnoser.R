# noter- serious mental illness
pop <- read_sas("kontakter_population.sas7bdat")

# Nu skal koden bruges på alle kontakter, med aktionsdiagnoser: 

diagnoser_pop <- pop %>% 
  group_by(Forloeb_ID) %>% 
  select(Forloeb_ID, Aktionsdiagnose, Diagnoser, DatoTid_Ind)

diagnose_sep <- diagnoser_pop  %>% 
  mutate(diagnoser_minus_tillaegskoder = str_replace_all(Diagnoser, "#\\+:[A-Z][A-Z][A-Z].[0-9]|#\\+:[A-Z][A-Z][A-Z].", "")) %>% 
  separate(col = diagnoser_minus_tillaegskoder, 
           into = c("dig_aktion", "dig_b_first", "dig_b_second", "dig_b_third", "dig_b_fourth", "dig_b_fithh", "dig_b_sixth", "dig_b_seventh"), 
           sep = "#",
           remove = FALSE) %>% 
  pivot_longer(
    cols = starts_with("dig_"),
    names_to = "diagnose_order",
    values_to = "diagnose_kode",
    values_drop_na = TRUE
  )

diagnose_psyc <- diagnose_sep %>% 
  select(Forloeb_ID, diagnose_kode) %>%
  mutate(diagnose  = str_replace(diagnose_kode, "([A|B|H]:D)(F20[0-9])", "schizophrenia"),
         diagnose = str_replace(diagnose, "(([A|B|H]:D)(F30[0-9]))", "manic"),
         diagnose = str_replace(diagnose, "([A|B|H]:D)(F31[0-9])", "bipolar_affective"),
         diagnose = str_replace(diagnose, "([A|B|H]:D)(F322)", "server_depression_no_psycotic"),
         diagnose = str_replace(diagnose, "([A|B|H]:D)(F323)", "server_depression_with_psycotic"),
         diagnose = str_replace(diagnose, "([A|B|H]:D)(F332)", "recurrent_server_depression_no_psycotic"),
         diagnose = str_replace(diagnose, "([A|B|H]:D)(F333)", "recurrent_server_depression_with_psycotic"),
         diagnose = str_replace(diagnose, "([A|B|H]:D)(F431)", "ptsd")) %>% 
  select(Forloeb_ID, diagnose_kode, diagnose) %>% 
  mutate(psych_weight = recode(diagnose, 
                               schizophrenia  = 1,
                               manic = 1,
                               bipolar_affective = 1,
                               server_depression_no_psycotic = 1,
                               server_depression_with_psycotic = 1,
                               recurrent_server_depression_no_psycotic = 1,
                               recurrent_server_depression_with_psycotic = 1,
                               ptsd =1,
                               .default = 0))


# et forløb en vægtning:
unik_forloeb_psych <- diagnose_psyc %>% 
  group_by(Forloeb_ID) %>% 
  summarise(sum_psych = sum(psych_weight))




##### Updated and improved SMI score ###### 
diagnoser_pop <- pop %>% 
  group_by(Forloeb_ID) %>% 
  select(Forloeb_ID, Aktionsdiagnose, Diagnoser, DatoTid_Ind)

diagnose_sep <- diagnoser_pop  %>% 
  mutate(diagnoser_minus_tillaegskoder = str_replace_all(Diagnoser, "#\\+:[A-Z][A-Z][A-Z].[0-9]|#\\+:[A-Z][A-Z][A-Z].", "")) %>% 
  separate(col = diagnoser_minus_tillaegskoder, 
           into = c("dig_aktion", "dig_b_first", "dig_b_second", "dig_b_third", "dig_b_fourth", "dig_b_fithh", "dig_b_sixth", "dig_b_seventh"), 
           sep = "#",
           remove = FALSE) %>% 
  pivot_longer(
    cols = starts_with("dig_"),
    names_to = "diagnose_order",
    values_to = "diagnose_kode",
    values_drop_na = TRUE
  )

diagnose_psyc <- diagnose_sep %>% 
  select(Forloeb_ID, diagnose_kode) %>%
  mutate(diagnose  = str_replace(diagnose_kode, "([A|B|H]:D)(F20[0-9])", "schizophrenia"),
         diagnose = str_replace(diagnose, "([A|B|H]:D)(F2[1-9][0-9])", "schizophrenia_like"),
         diagnose = str_replace(diagnose, "([A|B|H]:D)(F1[1-9][0-9])", "substance_abuse"),
         diagnose = str_replace(diagnose, "(([A|B|H]:D)(F3[0-9][0-9]))", "mood_affective"),
         diagnose = str_replace(diagnose, "([A|B|H]:D)(F431)", "ptsd"),
         diagnose = str_replace(diagnose, "([A|B|H]:D)(F50[0-9])", "anoretic")) %>% 
  mutate(psych_weight = recode(diagnose, 
                               schizophrenia  = 1,
                               schizophrenia_like  = 1,
                               mood_affective = 1,
                               ptsd =1,
                               anoretic = 1,
                               .default = 0))
# et forløb en vægtning:
unik_forloeb_psych <- diagnose_psyc %>% 
  group_by(Forloeb_ID) %>% 
  summarise(sum_psych = sum(psych_weight))

write_rds(unik_forloeb_psych, "psych_sum_per_forloeb_202110.rds")
