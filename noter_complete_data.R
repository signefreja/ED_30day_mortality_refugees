######### creating data frame containing all variables needed:#######
# setting directory to read the files from
setwd("E:/ProjektDB/ALMAU/Workdata/707842")

# read in data frames that I want to join: 
pop <- read_sas("kontakter_population.sas7bdat")
ophgin_total <- readRDS("E:/ProjektDB/ALMAU/Workdata/707842/ophgin_total_201112.rds")
cpr_oversaet <- read_sas("cpr_oversaet.sas7bdat")
ie_type <- read_rds("iepe_total_grouped_210118.rds")
ci_sum <- read_rds("ci_noage_sum_pr_forloeb_210713.rds")
age_ci <- read_rds("ci_sum_pr_forloeb_202411.rds")
socio_eco <- read_rds("socio_eco_grouped_210431.rds")
toks <- read_rds("toks_210105.rds")
psych_sum <- read_rds("psych_sum_per_forloeb_202110.rds")
afd <- read_rds("diagnoser_afd_210119.rds")

# Only want unique patients, using last visit
# distinct functionen keeps only the first one, therefore sorting so last visit is first

unikke_cpr <- pop %>% 
  arrange(desc(DatoTid_Udskr)) %>% 
  distinct(CPR_krypt, .keep_all = TRUE) %>% 
  mutate(sex = as.factor(tolower(kon))) %>% 
  select(CPR_krypt, Forloeb_ID, Alder, sex, Sengedage, Aktionsdiagnose, DatoTid_Ind, DatoTid_Udskr, Diagnoser,  AntalDageTilDoed, Nuv_Statusdato, Nuv_Statustekst)
  
# adding charlson comorbidity score 
dataset_pop_ci <- left_join(unikke_cpr, ci_sum, by = "Forloeb_ID")

dataset_pop_ci_age <- left_join(dataset_pop_ci, age_ci, by = "Forloeb_ID") 

# adding TOKS at admission and discahrge
dataset_pop_ci_toks <- left_join(dataset_pop_ci_age, toks, by = "Forloeb_ID")

# adding CPR, so data can be linked
dataset_cpr_pnr <- left_join(dataset_pop_ci_toks, cpr_oversaet, by = "CPR_krypt") 

# adding socioeconomic status to dataframe: 
data_ses <- left_join(dataset_cpr_pnr, socio_eco, by = c("CPRNummer" = "pnr")) %>% 
  select(-SOCIO13)

# adding country of origin
data_co <- left_join(data_ses, ie_type, by = c("CPRNummer" = "pnr")) %>% 
  select(-ie_type, -opr_land)

#adding refugee status
data_ophgin <- left_join(data_co, ophgin_total, by = c("CPRNummer" = "PNR")) %>% 
  select(-KATEGORI:-TILLADELSESDATO)

# Adding serious mental illness
data_frame_variabler <- left_join(data_ophgin, psych_sum, by = "Forloeb_ID")

# adding department:
data_frame_afd <- left_join(data_frame_variabler, afd, by = "Forloeb_ID")

# sorting working diagnosis (aktionsdiagnose) into icd 10 chapters:
data_frame_icd10 <- data_frame_afd %>% 
  mutate(icd10 = str_replace(Aktionsdiagnose,"^D[A|B][0-9][0-9][0-9].","certain_infectious_and_parasitic"),
         icd10 = str_replace(icd10,"^(DC...)|^(DD[0-4][0-9]..)", "neoplasms"),
         icd10 = str_replace(icd10,"^DD[5-8][0-9]..", "diseases_of_the_blood"),
         icd10 = str_replace(icd10,"^DE....", "endocrine_nutritional_metabolic"),
         icd10 = str_replace(icd10,"^DF....", "mental_behavioral_neurodevelopmental"),
         icd10 = str_replace(icd10,"^DG....", "nervous_system"),
         icd10 = str_replace(icd10,"^DH[0-5][0-9]..", "eye_and_adnexa"),
         icd10 = str_replace(icd10,"^DH[6-9][0-9]..", "ear_and_mastoid_process"),
         icd10 = str_replace(icd10,"^DI....", "circulatory_system"),
         icd10 = str_replace(icd10,"^DJ....", "respiratory_system"),
         icd10 = str_replace(icd10,"^DK....", "digestive_system"),
         icd10 = str_replace(icd10,"^DL....", "skin_and_subcutaneous_tissue"),
         icd10 = str_replace(icd10,"^DM....", "musculoskeletal_and_connective_tissue"),
         icd10 = str_replace(icd10,"^DN....", "genitourinary_system"),
         icd10 = str_replace(icd10,"^DO....", "pregnancy_childbirth_puerperium"),
         icd10 = str_replace(icd10,"^DP....", "conditions_originating_perinatal"),
         icd10 = str_replace(icd10,"^DQ....", "malformations_deformations_chromosomal"),
         icd10 = str_replace(icd10,"^DR....", "symptoms_signs_abnormal_clinical_laboratory"),
         icd10 = str_replace(icd10,"^D[S|T]....", "injury_poisoning_external"),
         icd10 = str_replace(icd10,"^D[V|Y]....", "external_causes_morbidity"), #none in this dataset
         icd10 = str_replace(icd10,"^DZ....", "factors_influencing_health_and_contact_to_hc"),
         icd10 = str_remove_all(icd10, "-.*"),
         icd10 = str_remove_all(icd10, "[0-9]"),
         icd10 = str_remove_all(icd10, "[A|C|R|D|M|B$]"),
         icd10 = str_trim(icd10, side = "both")) %>% 
  select(-Diagnoser)

##### above 18 of years, not diagnosed with icd in "injury_poisoning_external" #####
data_frame_icd10 <- data_frame_icd10 %>% 
  filter(Alder > 17.999 & icd10 != "injury_poisoning_external" & icd10 != "factors_influencing_health_and_contact_to_hc")

data_frame_icd10

# duplicates have been introduced, seems like it is 
data_dup <- data_frame_icd10 %>% 
  group_by(Forloeb_ID) %>% 
  filter(n()>1) %>% 
  mutate(opholds_grundlag = as.character(opholds_grundlag),
         indvandre_type = as.character(indvandre_type))

data_dup2 <- data_dup %>% 
  group_by(Forloeb_ID) %>% 
  summarise(ophold_edit = paste(opholds_grundlag, collapse = "#")) %>% 
  mutate(ophold_aendring = recode(ophold_edit, 
                                  "asyl#asyl" = "ens",
                                  "asyl#asyl#asyl#asyl#asyl#asyl#asyl" = "ens",
                                  "asyl#eu" = "forsk",
                                  "asyl#familiesam" = "forsk",
                                  "asyl#familiesam#familiesam" ="forsk",
                                  "asyl#studie" = "forsk",
                                  "asyl#work" = "forsk",
                                  "eu#eu" = "ens",
                                  "eu#eu#eu" =  "ens",
                                  "eu#eu#eu#eu" =  "ens",
                                  "eu#oevrig_opholdsomraade" =  "forsk",
                                  "familiesam#asyl" = "forsk",
                                  "familiesam#eu" = "forsk", 
                                  "familiesam#familiesam" =   "ens",
                                  "familiesam#familiesam#familiesam" =  "ens",
                                  "familiesam#oevrig_opholdsomraade" = "forsk",
                                  "familiesam#studie" = "forsk",
                                  "familiesam#work" =  "forsk",
                                  "NA#NA" = "NA",
                                  "oevrig_opholdsomraade#asyl" = "forsk",
                                  "oevrig_opholdsomraade#familiesam" =  "forsk",
                                  "oevrig_opholdsomraade#oevrig_opholdsomraade" =  "ens",
                                  "oevrig_opholdsomraade#oevrig_opholdsomraade#oevrig_opholdsomraade#oevrig_opholdsomraade" =  "ens",
                                  "oevrig_opholdsomraade#studie#oevrig_opholdsomraade" = "forsk",
                                  "oevrig_opholdsomraade#work" = "forsk",
                                  "studie#asyl" = "forsk",
                                  "studie#eu" = "forsk",
                                  "studie#eu#eu" =  "forsk",
                                  "studie#familiesam" =  "forsk",
                                  "studie#oevrig_opholdsomraade" = "forsk",
                                  "studie#studie" =  "ens",
                                  "studie#studie#eu#eu" = "forsk",
                                  "studie#studie#familiesam" = "forsk",
                                  "studie#studie#studie#work#eu" = "forsk",
                                  "studie#studie#work" = "forsk",
                                  "studie#work" = "forsk",
                                  "studie#work#eu" =  "forsk",
                                  "studie#work#studie" = "forsk",
                                  "studie#work#work#work#work" = "forsk",
                                  "studie#work#work#work#work#work" = "forsk",
                                  "work#eu" = "forsk",
                                  "work#studie" = "forsk",
                                  "work#work" =  "ens",
                                  "work#work#eu" = "forsk",
                                  "work#work#work" = "ens",
                                  "work#work#work#work" = "ens",
                                  "work#work#work#work#eu" =  "forsk")) %>% 
  mutate(asyl = str_extract_all(ophold_edit, "(asyl)"),
         asyl = as.character(asyl),
         asyl_edit = if_else(asyl != "character(0)", "asyl", "not_asyl"))  %>% # 13 har haft asyl status
  select(-asyl)

data_dup_total <- left_join(data_frame_icd10, data_dup2, by = "Forloeb_ID")

## I want to keep fist date of immigration and the immigrationstatus asyl
# also making sure that no danes appear to have immigration status
data_frame_refu_dk <- data_dup_total %>% 
  filter(opholds_grundlag == "asyl" | indvandre_type == "dane" | asyl_edit == "asyl") %>% 
  group_by(CPR_krypt) %>% 
  mutate(opholds_grundlag = as.character(opholds_grundlag),
         indvandre_type = as.character(indvandre_type),
         opholds_grundlag = if_else(indvandre_type == "dane", NA_character_, opholds_grundlag), 
         indvandre_type = if_else(indvandre_type == "dane" & !is.na(VAN_VTIL), NA_character_, indvandre_type),
         country_origin = as.character(country_origin),
         country_origin = if_else(is.na(indvandre_type), NA_character_, country_origin),
         VAN_VTIL = if_else(is.na(indvandre_type), NA_Date_, VAN_VTIL),
         immigration_date = min(VAN_VTIL)) %>%  #so the first immigration date will apear, if more immigration dates are presented
  filter(opholds_grundlag == "asyl" | indvandre_type == "dane") %>% 
  arrange(immigration_date) %>% 
  distinct(CPR_krypt, .keep_all = TRUE) %>% 
  select(-VAN_VTIL, -asyl_edit, -ophold_edit, -ophold_aendring)


# creating variable for time in dk for refugees. from immigration date to discharge 
data_frame_refu_dk <- data_frame_refu_dk %>% 
  mutate(time_dk = as.numeric(DatoTid_Udskr - immigration_date), # in days
         time_dk = if_else(is.na(opholds_grundlag),(Alder - 18) * 356.2425, time_dk),#danes time in DK age*365.2425 days/year. From the day they are 18 (none are below)
         time_dk = if_else(time_dk < 0, 0, time_dk)) %>% 
  select(-first_admission_date_in, -second_admission_date_in, -first_admission_date_out, -second_admission_date_out, first_admission_depart)

##### creating indicator variable, if patient died or not and how many days to death: #####
# see nuv_statustekst to see how extend exploration of how days to death is coded.
# All with NA in AntalDageTilDoed should have full follow up
data_indikator <- data_frame_refu_dk %>% 
  mutate(dage_doed = if_else(Nuv_Statustekst == "Aktiv, bopæl i dansk folkeregister", ymd("2020-05-28") - DatoTid_Udskr, Nuv_Statusdato - DatoTid_Udskr), 
         dage_doed = as.numeric(dage_doed),
         dage_doed = if_else(Nuv_Statusdato == DatoTid_Ind, 0, dage_doed),
         dage_doed = if_else(dage_doed == 0, NA_real_, dage_doed), # dead during hospital day is not included in the research question
         dage_doed = if_else(dage_doed < 0, NA_real_, dage_doed),
         dead_or_not = if_else(dage_doed < 30, 1, 0)) %>%  # negative number of days to death are replaced with NA)
  select(-AntalDageTilDoed, -Nuv_Statusdato)

# because there for country of origin is below 5 missing we will give the most common country for these missing same
# same goes for ses for refugee group:

data_correct_missing <- data_indikator %>% 
  mutate(grouped_country_origin = if_else(grouped_country_origin == "udlandet_uoplyst", "irak", grouped_country_origin),
         new_socio = if_else(opholds_grundlag == "asyl" & is.na(new_socio),  "kontant_sygedp", new_socio),
         three_levels_socio = if_else(opholds_grundlag == "asyl" & is.na(three_levels_socio),  "not_working", three_levels_socio)) %>% 
  select(-ind_toks)

write_rds(data_correct_missing, "final_data_frame_210118.rds")

