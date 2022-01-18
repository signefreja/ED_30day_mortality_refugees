# noter charlson 

# har brugt ca. 100 aar p åat finde ud af hvordan quan bruger ci, here goes:
# following comrobid conditions were mutually exclusive: 
# diabetic and diabetic with chronic complications
# mild liver disaease and moderate or servere liver disease
# any malignancy and metastic solid tumor

#### Nu skal koden bruges på alle kontakter, inklusiv aktionsdiagnoser: ####
pop <- read_sas("kontakter_population.sas7bdat")

diagnoser_pop <- pop %>% 
  group_by(Forloeb_ID) %>% 
  select(Forloeb_ID, Aktionsdiagnose, Diagnoser, DatoTid_Ind, Alder)

diagnose_sep <- diagnoser_pop  %>% 
  mutate(diagnoser_minus_tillaegskoder = str_replace_all(Diagnoser, "#\\+:[A-Z][A-Z][A-Z].[0-9]|#\\+:[A-Z][A-Z][A-Z].", "")) %>%               # der findes maks 8 tillaegs koder
  separate(col = diagnoser_minus_tillaegskoder, 
           into = c("dig_aktion", "dig_b_first", "dig_b_second", "dig_b_third", "dig_b_fourth", "dig_b_fithh", "dig_b_sixth", "dig_b_seventh", "dig_b_eight"), 
           sep = "#",
           remove = FALSE) %>% 
  pivot_longer(
    cols = starts_with("dig_"),
    names_to = "diagnose_order",
    values_to = "diagnose_kode",
    values_drop_na = TRUE)


diagnose_ci <- diagnose_sep %>% 
  select(Forloeb_ID, diagnose_kode, Alder) %>%
  mutate(diagnose = str_replace(diagnose_kode, "([A|B|H|+]:D)(I2[1|2][0-9]|I252)", "myocardial_infarct"),
         diagnose = str_replace(diagnose, "(([A|B|H|+]:D)((I099)|(I110)|(I130)|(I132)|(I255)|(I420)|(I42[5-9])|(I43[0-9])|(I50[0-9])|(P290)))", "heart_failure"),
         diagnose = str_replace(diagnose, "([A|B|H|+]:D)((I7[0|1][0-9])|(I73[1|8|9])|(I771)|(I79[0|2])|(K55[1|8|9])|(Z95[8|9]))", "vascular_disease"),
         diagnose = str_replace(diagnose, "([A|B|H|+]:D)((G4[5|6][0-9])|(H340)|(I6[0-9][0-9]))", "cerebrovascular_disease"),
         diagnose = str_replace(diagnose, "([A|B|H|+]:D)((F0[0-3][0-9])|(F051)|(G30[0-9])|(G311))", "dementia"),
         diagnose = str_replace(diagnose, "([A|B|H|+]:D)((I27[8|9])|(J4[0-7][0-9])|(J6[0-7][0-9])|(J684)|(J70[1|3]))", "chronic_pulmonary_disease"),
         diagnose = str_replace(diagnose, "([A|B|H|+]:D)((M0[5|6][0-9])|(M315)|(M3[2-4][0-9])|(M35[1|3])|(M360))", "rheumatic_disease"),
         diagnose = str_replace(diagnose, "([A|B|H|+]:D)(K2[5-8][0-9])", "peptic_ulcer"),
         diagnose = str_replace(diagnose, "([A|B|H|+]:D)((B18[0-9])|(K70[0-3|9])|(K71[3-5|7])|(K7[3|4][0-9].)|(K76[0|2-4|8|9])|(K944))", "mild_liver_disease"),
         diagnose = str_replace(diagnose, "([A|B|H|+]:D)(E1[0-4][0|1|6|8|9].)",  "diabetes"),
         diagnose = str_replace(diagnose, "([A|B|H|+]:D)((G041)|(G114)|(G80[1|2])|(G8[1|2][0-9])|(G83[0-4|9]))", "hemiplegia_paraplegia"),
         diagnose = str_replace(diagnose, "([A|B|H|+]:D)((I120)|(I130)|(N03[2-7])|(N05[2-7])|(N1[8|9][0-9])|(N250)|(Z49[0-2])|(Z940)|(Z992))", "renal_disease"),
         diagnose = str_replace(diagnose, "([A|B|H|+]:D)((E10[2-5|7])|(E11[2-5|7])|(E12[2-5|7])|(E13[2-5|7])|(E14[2-5|7]))", "diabetes_organ_damage"),
         diagnose = str_replace(diagnose, 
                             "([A|B|H|+]:D)((C[0-1][0-9][0-9])|(C2[0-6][0-9])|(C3[0-4][0-9])|(C3[7-9][0-9])|(C4[0|1][0-9])|(C43[0-9])|(C4[5-9][0-9])|(C5[0-8][0-9])|(C6[0-9][0-9])|(C7[0-6][0-9])|(C97[0-9]))", "tumor"),
         diagnose = str_replace(diagnose, "([A|B|H|+]:D)(C9[0-6][0-9])", "tumor"),
         diagnose = str_replace(diagnose, "([A|B|H|+]:D)(C8[1-8][0-9])", "tumor"),
         diagnose = str_replace(diagnose, "([A|B|H|+]:D)((I85[0|9])|(I864)|(I982)|(K704)|(K7[1|2]1)|(K729)|(K76[5-7]))", "moderat_servere_liver_disease"),
         diagnose = str_replace(diagnose, "([A|B|H|+]:D)((C7[7-9][0-9])|(C80[0-9]))", "tumor_metastatic"),
         diagnose = str_replace(diagnose, "([A|B|H|+]:D)((B2[0-2][0-9])|(B24[0-9]))", "aids"),
         diagnose = str_replace_all(diagnose, "(^[A|B|H|+])", NA_character_),
         diagnose = str_remove_all(diagnose, "([A-Z]+)|([0-9]+)"))


ci_heavy_code <- diagnose_ci %>%
  mutate(charlson_weight = recode(diagnose, 
                                  myocardial_infarct = 1,
                                  heart_failure = 1,
                                  vascular_disease = 1,
                                  cerebrovascular_disease = 1,
                                  dementia = 1,
                                  chronic_pulmonary_disease = 1,
                                  rheumatic_disease = 1, 
                                  peptic_ulcer = 1, 
                                  mild_liver_disease = 1,
                                  diabetes = 1,
                                  hemiplegia_paraplegia = 2,
                                  renal_disease = 2,
                                  diabetes_organ_damage = 2,
                                  tumor = 2, 
                                  leukemia = 2,
                                  lymphoma = 2,
                                  moderat_servere_liver_disease = 3,
                                  tumor_metastatic = 6,
                                  aids = 6,
                                  .default = 0))

write_rds(ci_heavy_code, "ci_heavy_code.rds")


# kan kun have en diagnose en gang:
ci_heavy_code <- read_rds("ci_heavy_code.rds")

diagnose_ci_unique <- ci_heavy_code %>% 
 group_by(Forloeb_ID) %>%
  distinct(diagnose, .keep_all = TRUE)

# Laver et dataset, for at markere dem som jeg senere vil aendre vægtning af, hvis de:
# following comrobid conditions were mutually exclusive: 
# diabetic and diabetic with chronic complications
# mild liver disaease and moderate or servere liver disease
# any malignancy and metastic solid tumor
# man skal kun kunne faa en diagnose en gang (undersoeger kun diabetes i foerste omgang for at faa kode til at virke)


#kigger på dem med liver, tumor og diabetes, saetter dem sammen saa jeg kan lave en dummy variabel som fortaeller om 
# det samme forloeb har de "forbudte" diagnsoer sammen:
more_than_1_ci_prob <- diagnose_ci_unique %>% 
  filter(diagnose == "diabetes" | diagnose == "diabetes_organ_damage" | diagnose == "moderat_servere_liver_disease" | diagnose == "mild_liver_disease" | diagnose == "tumor" | diagnose == "tumor_metastatic") %>% 
  group_by(Forloeb_ID) %>% 
  filter(n()>1) %>% 
  group_by(Forloeb_ID) %>% 
  summarise(digs = paste(diagnose, collapse = ",")) %>% 
  mutate(need_to_drop_tumor = if_else(digs == "tumor,tumor_metastatic" | digs == "tumor_metastatic,tumor" | digs == "diabetes,tumor,tumor_metastatic" | digs == "tumor,tumor_metastatic,diabetes", 1, 0),
         need_to_drop_diabetes = if_else(digs == "diabetes,diabetes_organ_damage" | digs == "diabetes_organ_damage,diabetes", 1, 0),
         need_to_drop_liver = if_else(digs == "mild_liver_disease,moderat_servere_liver_disease" | digs == "mild_liver_disease,tumor,moderat_servere_liver_disease" | digs == "moderat_servere_liver_disease,mild_liver_disease", 1, 0))



# jeg saetter dummyvariablerne sammen med det oprindlige charlson datasaet:
diagnose_ci_need_to_drop <- left_join(diagnose_ci_unique, more_than_1_ci_prob, by = "Forloeb_ID")

# Og her aendre jeg vagtningen og navnet på de "forbudte" diagnoser:
diagnose_ci_droped <- diagnose_ci_need_to_drop %>% 
  select(-digs) %>% 
  mutate(diagnose = if_else(diagnose == "mild_liver_disease" & need_to_drop_liver == 1, "liver_dup", diagnose),
         diagnose = if_else(diagnose == "diabetes" & need_to_drop_diabetes == 1, "diabetes_dup", diagnose),
         diagnose = if_else(diagnose == "tumor" & need_to_drop_tumor == 1, "tumor_dup", diagnose),
         charlson_weight = if_else(diagnose == "liver_dup" | diagnose == "diabetes_dup" | diagnose == "tumor_dup", 0, charlson_weight))

ci_sum <- diagnose_ci_droped %>% 
  group_by(Forloeb_ID) %>% 
  mutate(sum_ci = if_else(is.na(charlson_weight), 0 , charlson_weight)) %>% 
  summarise(sum_ci = sum(sum_ci), Alder, Forloeb_ID, charlson_weight, diagnose) 

### data set for charlson cormibidity score, whithout age ###
ci_sum_save <- ci_sum %>% 
  distinct(Forloeb_ID, .keep_all = TRUE) %>% 
  select(Forloeb_ID, sum_ci)

write_rds(ci_sum_save, "ci_noage_sum_pr_forloeb_210713.rds")

# including age in charlson comorbidity score # 

age_adjusted <- ci_sum %>%
  mutate(age_adjust = case_when(
    Alder < 50 ~ 0,
    Alder >= 100 ~ 6,
    Alder >= 50 & Alder < 60 ~ 1,
    Alder >= 60 & Alder < 70 ~ 2,
    Alder >= 70 & Alder < 80 ~ 3,
    Alder >= 80 & Alder < 90 ~ 4,
    Alder >= 90 & Alder < 100 ~ 5))

### only needs one sum charlson and one age score pr. id:
age_adjust_uniq <- age_adjusted %>% 
  group_by(Forloeb_ID) %>% 
  arrange(diagnose) %>% 
  distinct(Forloeb_ID, .keep_all = TRUE)

# pivot to long format, so age and charlson score can be summarised
long <- pivot_longer(age_adjust_uniq, cols = c(age_adjust, sum_ci), 
                     names_to = "age_charl", values_to = "value_age_char")
# summarising to one score incl age and comorbidity
ci_age_sum <- long %>% 
  group_by(Forloeb_ID) %>% 
  summarise(sum_ci_age = sum(value_age_char))
    




write_rds(ci_age_sum, "ci_sum_pr_forloeb_202411.rds")
#write_rds(ci_sum, "ci_sum_pr_forloeb_210207.rds")




######## kigger lige individuelt på de tre forskellige diagnoser ##############
#liver :
liver_ci_prob <- more_than_1_ci_prob %>% 
  filter(diagnose == "moderat_servere_liver_disease" | diagnose == "mild_liver_disease") %>% 
  arrange(Forloeb_ID, diagnose) %>% 
  filter(n()>1)

#diabetes :
diabetes_ci_prob <- more_than_1_ci_prob %>% 
  filter(diagnose == "diabetes" | diagnose == "diabetes_organ_damage") %>% 
  arrange(Forloeb_ID, diagnose) %>% 
  filter(n()>1)

#tumor :
tumor_ci_prob <- more_than_1_ci_prob %>% 
  filter(diagnose == "tumor" | diagnose == "tumor_metastatic") %>% 
  arrange(Forloeb_ID, diagnose) %>% 
  filter(n()>1)
############################################################################



############## Aktionsdiagnose eller ej? ################

diagnoser <- read_sas("diagnoser_kontakter.sas7bdat")
diagnoser_100n <- diagnoser %>% 
  group_by(Forloeb_ID) %>% 
  select(Forloeb_ID, Aktionsdiagnose, Diagnoser, DatoTid_Ind, DiagnoseVedSygehusUdskrivelse) %>% 
  head(100)

#"(?<=B:)[A-Z][A-Z][0-9][0-9][0-9]"))
# str_extract(Diagnoser, "B:[A-Z][A-Z][0-9][0-9][0-9]")

diagnoser_100_n_test <- diagnoser_100n  %>% 
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

diagnose_100n_koder <- diagnoser_100_n_test %>% 
  select(Forloeb_ID, diagnose_kode) %>%
  mutate(myocardial_infarct = str_replace(diagnose_kode, "([A|B|H]:D)(I2[1|2][0-9]|I252)", "myocardial_infarct"),
         heart_failure = str_replace(diagnose_kode, "(([A|B|H]:D)((I099)|(I110)|(I130)|(I132)|(I255)|(I420)|(I42[5-9])|(I43[0-9])|(I50[0-9])|(P290)))", "heart_failure"),
         vascular_disease = str_replace(diagnose_kode, "([A|B|H]:D)((I7[0|1][0-9])|(I73[1|8|9])|(I771)|(I79[0|2])|(K55[1|8|9])|(Z95[8|9]))", "vascular_disease"),
         cerebrovascular_disease = str_replace(diagnose_kode, "([A|B|H]:D)((G4[5|6][0-9])|(H340)|(I6[0-9][0-9]))", "cerebrovascular_disease"),
         dementia = str_replace(diagnose_kode, "([A|B|H]:D)((F0[0-3][0-9])|(F051)|(G30[0-9])|(G311))", "dementia"),
         chronic_pulmonary_disease = str_replace(diagnose_kode, "([A|B|H]:D)((I27[8|9])|(J4[0-7][0-9])|(J6[0-7][0-9])|(J684)|(J70[1|3]))", "chronic_pulmonary_disease"),
         rheumatic_disease = str_replace(diagnose_kode, "([A|B|H]:D)((M0[5|6][0-9])|(M315)|(M3[2-4][0-9])|(M35[1|3])|(M360))", "rheumatic_disease"),
         peptic_ulcer_disease = str_replace(diagnose_kode, "([A|B|H]:D)(K2[5-8][0-9])", "peptic_ulcer"),
         mild_liver_disease = str_replace(diagnose_kode, "([A|B|H]:D)((B18[0-9])|(K70[0-3|9])|(K71[3-5|7])|(K7[3|4][0-9].)|(K76[0|2-4|8|9])|(K944))", "mild_liver_disease"),
         diabetes = str_replace(diagnose_kode, "([A|B|H]:D)(E1[0-4][0|1|9].)",  "diabetes"),
         hemiplegia_paraplegia = str_replace(diagnose_kode, "([A|B|H]:D)((G041)|(G114)|(G80[1|2])|(G8[1|2][0-9])|(G83[0-4|9]))", "hemiplegia_paraplegia"),
         renal_disease = str_replace(diagnose_kode, "([A|B|H]:D)((I120)|(I130)|(N03[2-7])|(N05[2-7])|(N1[8|9][0-9])|(N250)|(Z49[0-2])|(Z940)|(Z992))", "renal_disease"),
         diabets_organ_damage = str_replace(diagnose_kode, "([A|B|H]:D)((E10[2-5|7])|(E11[2-5|7])|(E12[2-5|7])|(E13[2-5|7])|(E14[2-5|7]))", "diabetes_organ_damage"),
         tumor = str_replace(diagnose_kode, 
                             "([A|B|H]:D)((C[0-1][0-9][0-9])|(C2[0-6][0-9])|(C3[0-4][0-9])|(C3[7-9][0-9])|(C4[0|1][0-9])|(C43[0-9])|(C4[5-9][0-9])|(C5[0-8][0-9])|(C6[0-9][0-9])|(C7[0-6][0-9])|(C97[0-9]))", "tumor"),
         leukemia = str_replace(diagnose_kode, "([A|B|H]:D)(C9[0-6][0-9])", "leukemia"),
         lymphoma = str_replace(diagnose_kode, "([A|B|H]:D)(C8[1-8][0-9])", "lymphoma"),
         moderat_servere_liver_disease = str_replace(diagnose_kode, "([A|B|H]:D)((I85[0|9])|(I864)|(I982)|(K704)|(K7[1|2]1)|(K729)|(K76[5-7]))", "moderat_servere_liver_disease"),
         tumor_metastatic = str_replace(diagnose_kode, "([A|B|H]:D)((C7[7-9][0-9])|(C80[0-9]))", "tumor_metastatic"),
         aids = str_replace(diagnose_kode, "([A|B|H]:D)((B2[0-2][0-9])|(B24[0-9]))", "aids")) %>% 
  mutate(myocardial_infarct_na = if_else(myocardial_infarct == "myocardial_infarct", "myocardial_infarct", NA_character_),
         heart_failure_na = if_else(myocardial_infarct == "heart_failure", "heart_failure", NA_character_),
         vascular_disease_na = if_else(vascular_disease == "vascular_disease", "vascular_disease", NA_character_),
         cerebrovascular_disease_na = if_else(cerebrovascular_disease == "cerebrovascular_disease", "cerebrovascular_disease", NA_character_),
         dementia_na = if_else(dementia == "dementia", "dementia", NA_character_),
         chronic_pulmonary_disease_na = if_else(chronic_pulmonary_disease == "chronic_pulmonary_disease", "chronic_pulmonary_disease", NA_character_),
         rheumatic_disease_na = if_else(rheumatic_disease == "rheumatic_disease", "rheumatic_disease", NA_character_),
         peptic_ulcer_disease_na = if_else(peptic_ulcer_disease == "peptic_ulcer", "peptic_ulcer", NA_character_),
         mild_liver_disease_na = if_else(mild_liver_disease == "mild_liver_disease", "mild_liver_disease", NA_character_), 
         diabetes_na = if_else(diabetes == "diabetes", "diabetes", NA_character_),
         hemiplegia_paraplegia_na = if_else(hemiplegia_paraplegia == "hemiplegia_paraplegia", "hemiplegia_paraplegia", NA_character_),
         renal_disease_na = if_else(renal_disease == "renal_disease", "renal_disease", NA_character_),
         diabets_organ_damage_na = if_else(diabets_organ_damage == "diabetes_organ_damage", "diabetes_organ_damage", NA_character_),
         tumor_na = if_else(tumor == "tumor", "tumor", NA_character_),
         leukemia_na = if_else(leukemia == "leukemia", "leukemia", NA_character_),
         lymphoma_na = if_else(lymphoma == "lymphoma", "lymphoma", NA_character_),
         moderat_servere_liver_disease_na = if_else(moderat_servere_liver_disease == "moderat_servere_liver_disease", "moderat_servere_liver_disease", NA_character_),
         tumor_metastatic_na = if_else(tumor_metastatic == "tumor_metastatic", "tumor_metastatic", NA_character_),
         aids_na = if_else(aids == "aids", "aids", NA_character_)) %>% 
  unite("charlson_diag", myocardial_infarct_na:aids_na, na.rm = TRUE, remove = TRUE) %>% 
  select(Forloeb_ID, diagnose_kode, charlson_diag) %>% 
  mutate(charlson_weight = recode(charlson_diag, 
                                  myocardial_infarct = 1,
                                  heart_failure = 1,
                                  vascular_disease = 1,
                                  cerebrovascular_disease = 1,
                                  dementia = 1,
                                  chronic_pulmonary_disease = 1,
                                  rheumatic_disease = 1, 
                                  peptic_ulcer = 1, 
                                  mild_liver_disease = 1,
                                  diabetes = 1,
                                  hemiplegia_paraplegia = 2,
                                  renal_disease = 2,
                                  diabetes_organ_damage = 2,
                                  tumor = 2, 
                                  leukemia = 2,
                                  lymphoma = 2,
                                  moderat_servere_liver_disease = 3,
                                  tumor_metastatic = 6,
                                  aids = 6,
                                  .default = 0))

