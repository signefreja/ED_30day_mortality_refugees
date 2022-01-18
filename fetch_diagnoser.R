# diagnoser 

diagnoser <- read_sas("diagnoser_kontakter.sas7bdat")
diagnoser_afd <- diagnoser %>% 
  select(Aktionsdiagnose, Forloeb_ID, ShakAfdkodeTekst, DatoTid_Ind, DatoTid_Udskr) %>% 
  mutate(afd = str_remove_all(ShakAfdkodeTekst, "(^[0-9]+)"),
         afd = str_remove_all(afd, "([-])"),
         afd = str_trim(afd, side = "both"),
         afd = str_to_lower(afd),
         med = str_extract(afd, "(medicinsk)|(sygdomme)|(geri)|(logi)|(steno)"),
         med = if_else(!is.na(med), "medicinsk", NA_character_ ),
         kir = str_extract(afd, "(kirurgi)|(kir)|(operation)|(urologisk)"),
         kir = if_else(!is.na(kir), "kirurgisk", NA_character_ ),
         akut = str_extract(afd, "(akut)"),
         ukendt = str_extract(afd, "(ukendt)"),
         psyk = str_extract(afd, "(psyk)|(angst)"),
         psyk = if_else(!is.na(psyk), "psyk", NA_character_),
         b_og_u = str_extract(afd, "(børn)|(pædiatrisk)"),
         b_og_u = if_else(!is.na(b_og_u), "børn", NA_character_),
         andet = str_extract(afd, "(diagnostisk)|(øjen)|(øre)|(fys)|(kræft)|(onkologi)|(hammel)|(hospice)|(samsø)|(hotel)|(ryg)|(halsklinik)|(ørenæsehals)|(amkvagt)|(biokemi)|(neurologi)|(intensiv)|(kvinde)|(obstetrik)|(kæbe)"),
         andet = if_else(!is.na(andet), "andet", NA_character_)) %>% 
  unite("med_kir_akut", med:andet, na.rm = TRUE) %>% 
  mutate(med_kir_akut = if_else(afd == "neurofysiologisk overafd.", "andet", med_kir_akut),
         med_kir_akut = if_else(afd == "ortopædkirurgisk fysio og ergoterapi hem", "andet", med_kir_akut),
         med_kir_akut = if_else(med_kir_akut == "medicinsk_kirurgisk", "kirurgisk", med_kir_akut),
         med_kir_akut = if_else(afd == "anæstesiologisk afdeling" | afd == "anæstesiologisk overafdeling  randers" | afd == "anæstesiogiskintensiv overafdeling  heh", "andet", med_kir_akut),
         med_kir_akut = if_else(afd == "terapiafdelingen overafdeling  heh", "andet", med_kir_akut),
         med_kir_akut = if_else(med_kir_akut == "medicinsk_andet" | med_kir_akut == "medicinsk_kirurgisk_andet", "andet", med_kir_akut),
         med_kir_akut = if_else(med_kir_akut == "kirurgisk_andet", "kirurgisk", med_kir_akut),
         DatoTid_Ind = na_if(DatoTid_Ind, "NULL"),
         med_kir_akut = na_if(med_kir_akut, "ukendt"),
         afd = na_if(afd, "ukendt"),
         DatoTid_Ind = as.Date(DatoTid_Ind)) %>%
  #select(- ShakAfdkodeTekst) %>% 
  arrange(desc(DatoTid_Ind))

write_rds(diagnoser_afd, "diagnoser_afd_20210104.rds")

# Adding a coulmn to keep track of where first admitted and where transferred to
diagnoser_afd <- diagnoser_afd %>% 
  arrange(DatoTid_Ind, med_kir_akut) %>%
  group_by(Forloeb_ID) %>% 
  mutate(admitted_order = 1:n())

# making the data frame wider, to distingiush between the order in admission for department, date in and date out:
department_order <- diagnoser_afd %>% 
  pivot_wider(id_cols = Forloeb_ID, names_from = admitted_order, values_from = med_kir_akut)

department_date_in <- diagnoser_afd %>%
  pivot_wider(id_cols = Forloeb_ID, names_from = admitted_order, values_from = DatoTid_Ind)

department_date_ud <- diagnoser_afd %>% 
  pivot_wider(id_cols = Forloeb_ID, names_from = admitted_order, values_from = DatoTid_Udskr)

department_joined <- list(department_order, department_date_in, department_date_ud) %>% 
  reduce(left_join, by = "Forloeb_ID")

#only using the department which they are admitted to and the one there are emeitly transfeered to:
department_join_1_2_admit <- department_joined %>% 
  select(Forloeb_ID, "1.x", "2.x", "1.y", "2.y", "1", "2") %>% 
  rename(c(first_admission_depart = "1.x",
         second_admission_depart = "2.x",
         first_admission_date_in = "1.y",
         second_admission_date_in = "2.y",
         first_admission_date_out = "1",
         second_admission_date_out = "2"))

write_rds(department_join_1_2_admit, "diagnoser_afd_wide_20210107.rds")

diagnoser_afd_unique <- diagnoser_afd %>% 
  group_by(Forloeb_ID) %>% 
  arrange(desc(admitted_order)) %>% 
  distinct(Forloeb_ID, .keep_all = TRUE) %>% 
  select(-DatoTid_Udskr, -DatoTid_Ind)

dep_1_2_afd <- left_join(department_join_1_2_admit, diagnoser_afd_unique)

write_rds(dep_1_2_afd, "diagnoser_afd_210119.rds")

# finder ud af hvilke afdelinger der hører med til hver af kategorierne:

med <- diagnoser_afd %>% 
  filter(med_kir_akut == "medicinsk")
table(med$afd)

kir <- diagnoser_afd %>% 
  filter(med_kir_akut == "kirurgisk")
table(kir$afd)

andet <- diagnoser_afd %>% 
  filter(med_kir_akut == "andet")
table(andet$afd)

akut <- diagnoser_afd %>% 
  filter(med_kir_akut == "akut")
table(akut$afd)


