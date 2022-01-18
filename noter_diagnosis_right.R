pop_afd <- read_rds("final_data_frame_210118.rds")

pop_afd <- pop_afd %>% 
  select(afd, icd10, Aktionsdiagnose, med_kir_akut, opholds_grundlag)

# star meaning refugees in that cat is less than 5

pop_afd_ny_ny_opdel <- pop_afd %>% 
  mutate(ny_opdeling = str_replace(icd10, "(eye_and_adnexa)", "other"),
         ny_opdeling = str_replace(ny_opdeling, "(malformations_deformations_chromosomal)", "other"),
         ny_opdeling = str_replace(ny_opdeling, "(neoplasms)", "other"),
         ny_opdeling = str_replace(ny_opdeling, "(conditions_originating_perinatal)", "other"),
         ny_opdeling = str_replace(ny_opdeling, "(ear_and_mastoid_process)", "other"),
         ny_opdeling = str_replace(ny_opdeling, "(nervous_system)", "other"),
         ny_opdeling = str_replace(ny_opdeling, "(factors_influencing_health_and_contact_to_hc)", "other"),
         ny_opdeling = str_replace(ny_opdeling, "(pregnancy_childbirth_puerperium)", "other"),
         ny_opdeling = str_replace(ny_opdeling, "(mental_behavioral_neurodevelopmental)", "psych"),
         ny_opdeling = if_else(ny_opdeling == "symptoms_signs_abnormal_clinical_laboratory", med_kir_akut, ny_opdeling),
         ny_opdeling = str_replace(ny_opdeling, "(certain_infectious_and_parasitic)", "infec medical"),
         ny_opdeling = str_replace(ny_opdeling, "(diseases_of_the_blood)", "blood medical"),
         ny_opdeling = if_else(Aktionsdiagnose == "DD629 - Akut blødningsanæmi UNS", "abdominal surgical", ny_opdeling), # belongs to diseases of blood, but is more appropiate in surgical group
         ny_opdeling = if_else(ny_opdeling == "psyk_børn" | ny_opdeling == "akut_psyk", "psych", ny_opdeling),
         ny_opdeling = str_replace(ny_opdeling, "(endocrine_nutritional_metabolic)", "endo medical"),
         ny_opdeling = if_else(Aktionsdiagnose == "DE105B - Type 1-diabetes med fodsår" | Aktionsdiagnose == "DE115B - Type 2-diabetes med fodsår" |
                                 Aktionsdiagnose == "DE135B - Anden diabetes med fodsår" | Aktionsdiagnose == "DE145B - Diabetes UNS med fodsår", 
                               "endo surgical*", ny_opdeling),
         ny_opdeling = if_else(med_kir_akut == "kirurgisk" & icd10 == "circulatory_system", "circulatory surgical", ny_opdeling),
         ny_opdeling = if_else(med_kir_akut == "medicinsk" & icd10 == "circulatory_system", "circulatory medical*", ny_opdeling),
         ny_opdeling = if_else(med_kir_akut == "andet" & icd10 == "circulatory_system", "circulatory other*", ny_opdeling),
         ny_opdeling = if_else(med_kir_akut == "akut" & icd10 == "circulatory_system", "circulatory unspec", ny_opdeling),
         ny_opdeling = if_else(ny_opdeling == "circulatory_system", "circulatory unspec", ny_opdeling),
         ny_opdeling = if_else(is.na(afd) & icd10 == "circulatory_system", "circulatory unknown*", ny_opdeling),
         ny_opdeling = if_else(med_kir_akut == "kirurgisk" & icd10 == "digestive_system", "abdominal surgical", ny_opdeling),
         ny_opdeling = if_else(med_kir_akut == "medicinsk" & icd10 == "digestive_system", "abdominal medical*", ny_opdeling),
         ny_opdeling = if_else(med_kir_akut == "andet" & icd10 == "digestive_system", "abdominal other*", ny_opdeling),
         ny_opdeling = if_else(med_kir_akut == "akut" & icd10 == "digestive_system", "abdominal unspec", ny_opdeling),
         ny_opdeling = if_else(ny_opdeling == "digestive_system", "abdominal unspec", ny_opdeling),
         ny_opdeling = if_else(is.na(afd) & icd10 == "digestive_system", "abdominal unknwon", ny_opdeling),
         ny_opdeling = if_else(med_kir_akut == "medicinsk" & icd10 == "genitourinary_system", "genitourinary medical*", ny_opdeling),
         ny_opdeling = if_else(med_kir_akut == "andet" & icd10 == "genitourinary_system", "genitourinary other", ny_opdeling),
         ny_opdeling = if_else(med_kir_akut == "akut" & icd10 == "genitourinary_system", "genitourinary unspec", ny_opdeling),
         ny_opdeling = if_else(ny_opdeling == "genitourinary_system", "genitourinary unspec", ny_opdeling),
         ny_opdeling = if_else(med_kir_akut == "kirurgisk" & icd10 == "genitourinary_system", "genitourinary surgical", ny_opdeling),
         ny_opdeling = if_else(is.na(afd) & icd10 == "genitourinary_system", "genitourinary unknown", ny_opdeling),
         ny_opdeling = if_else(med_kir_akut == "medicinsk" & icd10 == "musculoskeletal_and_connective_tissue", "musculoskeletal medical*", ny_opdeling),
         ny_opdeling = if_else(med_kir_akut == "andet" & icd10 == "musculoskeletal_and_connective_tissue", "musculoskeletal other*", ny_opdeling),
         ny_opdeling = if_else(med_kir_akut == "akut" & icd10 == "musculoskeletal_and_connective_tissue", "musculoskeletal unspec", ny_opdeling),
         ny_opdeling = if_else(med_kir_akut == "kirurgisk" & icd10 == "musculoskeletal_and_connective_tissue", "musculoskeletal surgical*", ny_opdeling),
         ny_opdeling = if_else(is.na(afd) & icd10 == "musculoskeletal_and_connective_tissue", "musculoskeletal unknown", ny_opdeling),
         ny_opdeling = if_else(ny_opdeling == "musculoskeletal_and_connective_tissue", "musculoskeletal unspec", ny_opdeling),
         ny_opdeling = if_else(med_kir_akut == "medicinsk" & icd10 == "respiratory_system", "respiratory medical", ny_opdeling),
         ny_opdeling = if_else(med_kir_akut == "andet" & icd10 == "respiratory_system", "respiratory other", ny_opdeling),
         ny_opdeling = if_else(med_kir_akut == "akut" & icd10 == "respiratory_system", "respiratory unspec", ny_opdeling),
         ny_opdeling = if_else(med_kir_akut == "kirurgisk" & icd10 == "respiratory_system", "respiratory surgical", ny_opdeling),
         ny_opdeling = if_else(is.na(afd) & icd10 == "respiratory_system", "respiratory unknown*", ny_opdeling),
         ny_opdeling = if_else(ny_opdeling == "respiratory_system", "respiratory unspec", ny_opdeling), # the ones who has psyk as afd gets unspec as well
         ny_opdeling = if_else(icd10 == "symptoms_signs_abnormal_clinical_laboratory", "symptoms signs abnormal clinical", ny_opdeling),
         ny_opdeling = if_else(med_kir_akut == "medicinsk" & icd10 == "symptoms_signs_abnormal_clinical_laboratory", "symptoms abnormal medical", ny_opdeling),
         ny_opdeling = if_else(med_kir_akut == "andet" & icd10 == "symptoms_signs_abnormal_clinical_laboratory", "symptoms abnormal other*", ny_opdeling),
         ny_opdeling = if_else(med_kir_akut == "akut" & icd10 == "symptoms_signs_abnormal_clinical_laboratory", "symptoms abnormal unspec", ny_opdeling),
         ny_opdeling = if_else(med_kir_akut == "kirurgisk" & icd10 == "symptoms_signs_abnormal_clinical_laboratory", "symptoms abnormal surgical*", ny_opdeling),
         ny_opdeling = if_else(ny_opdeling == "symptoms signs abnormal clinical" & icd10 == "symptoms_signs_abnormal_clinical_laboratory", "symptoms abnormal unspec", ny_opdeling),
         ny_opdeling = if_else(is.na(afd) & icd10 == "symptoms_signs_abnormal_clinical_laboratory", "symptoms abnormal unknown", ny_opdeling),
         ny_opdeling = if_else(ny_opdeling == "skin_and_subcutaneous_tissue", "skin", ny_opdeling))




pop_smerte_dig_ny <- pop_afd_ny_ny_opdel %>% 
  mutate(smerte = str_extract(Aktionsdiagnose, "(smerte)|(pine)"),
         smerte = if_else(Aktionsdiagnose == "DK309 - Funktionel dyspepsi UNS" | Aktionsdiagnose == "DK297 - Mavekatar UNS", "smerte", smerte),
         pain = if_else(!is.na(smerte) & med_kir_akut == "andet", "other pain*", smerte),
         pain = if_else(!is.na(smerte) & med_kir_akut == "akut", "unspec pain*", pain),
         pain = if_else(icd10 == "circulatory_system" & !is.na(smerte), "circulatory pain*", pain),
         pain = if_else(!is.na(smerte) & icd10 == "musculoskeletal_and_connective_tissue", "musculoskeletal pain", pain),
         pain = if_else(!is.na(smerte) & icd10 == "genitourinary_system", "abdominal pain", pain),
         pain = if_else(Aktionsdiagnose == "DR073 - Andre brystsmerter" | Aktionsdiagnose == "DR074 - Brystsmerter UNS", "chest pain", pain),
         pain = if_else(!is.na(smerte) & icd10 == "nervous_system", "head pain", pain),
         pain = if_else(Aktionsdiagnose == "DR519 - Hovedpine UNS", "head pain", pain),
         pain = if_else(pain == smerte, "other pain*", pain),
         pain = if_else(!is.na(smerte) & is.na(smerte), "unspec pain*", pain),
         mave_smerte = str_extract(Aktionsdiagnose, "avesmerter"),
         pain = if_else(!is.na(mave_smerte), "abdominal pain", pain),
         pain = if_else(!is.na(smerte) & icd10 == "digestive_system", "abdominal pain", pain))






pop_and_pain <- left_join(pop_afd_ny_ny_opdel, pop_smerte_dig_ny) %>% 
  mutate(ny_opdeling = if_else(!is.na(smerte), "pain", ny_opdeling),
         ny_opdeling = str_to_title(ny_opdeling, locale = "en"),
         pain = str_to_title(pain, locale = "en"),
         ny_opdeling = as.factor(ny_opdeling),
         ny_opdeling = fct_relevel(ny_opdeling, "Abdominal Unknwon", "Genitourinary Other", "Musculoskeletal Unknown",
                                   "Respiratory Other", "Circulatory Surgical",
                                   "Circulatory Medical*", "Abdominal Other*", "Respiratory Unknown*",
                                   "Musculoskeletal Medical*", "Circulatory Other*", "Endo Surgical*",
                                   "Musculoskeletal Other*", "Musculoskeletal Surgical*", "Abdominal Medical*", 
                                   "Symptoms Abnormal Other*", "Circulatory Unknown*", "Symptoms Abnormal Surgical*",
                                   "Genitourinary Medical*", "Respiratory Surgical", "Genitourinary Unknown",
                                   "Genitourinary Surgical", "Circulatory Unspec", "Blood Medical", "Abdominal Surgical",
                                   "Symptoms Abnormal Medical", "Symptoms Abnormal Unknown", "Endo Medical", "Abdominal Unspec",
                                   "Genitourinary Unspec", "Musculoskeletal Unspec", "Respiratory Medical", "Respiratory Unspec",
                                   "Infec Medical", "Other", "Psych", "Skin", "Symptoms Abnormal Unspec", "Pain"))          




################# plot diagnosis ##################

dig_ref <- pop_and_pain %>% 
  filter(opholds_grundlag == "asyl") %>% 
  group_by(ny_opdeling) %>% 
  mutate(ny_leng = length(ny_opdeling)) %>% 
  ungroup() %>% 
  mutate(ny_leng = as.double(ny_leng),
         ny_leng = if_else(ny_leng < 5, 5, ny_leng),
         freq = ny_leng / length(opholds_grundlag),
         ref_dk = "Refugee") 
dig_dk <- pop_and_pain %>% 
  filter(is.na(opholds_grundlag)) %>% 
  group_by(ny_opdeling) %>% 
  mutate(ny_leng = length(ny_opdeling)) %>% 
  ungroup() %>% 
  mutate(freq = ny_leng / length(opholds_grundlag),
         ref_dk = "Dane")
  
dig_ref_dk <- rbind(dig_ref, dig_dk)

ggplot(dig_ref_dk, aes(x = freq, y = ny_opdeling, colour = ref_dk, shape = ref_dk)) +
  geom_point() +
  ggtitle("Fraction plot of distribution of diagnosis") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Fraction") +
  ylab("Diagnosis categories") + 
  theme(legend.title = element_blank()) +
  scale_colour_manual(values = c("orange", "purple")) +
  scale_shape_manual(values = c(1, 2))

check_dig_ref <- dig_ref %>% 
  group_by(ny_opdeling) %>% 
  mutate(n = length(ny_opdeling)) %>% 
  distinct(ny_opdeling, .keep_all = TRUE) %>% 
  select(ny_opdeling, n)

################# plot pain ####################
check_pain <- pop_and_pain %>% 
  group_by(pain) %>% 
  distinct(pain, .keep_all = TRUE)

pain <- pop_and_pain %>% 
  filter(!is.na(smerte)) %>% 
  select(ny_opdeling, opholds_grundlag, pain, icd10, Aktionsdiagnose, med_kir_akut) %>% 
  mutate(pain = if_else(is.na(pain), "Other Pain*", pain),
    pain = as.factor(pain),
         pain = fct_relevel(pain, "Other Pain*", "Circulatory Pain*", 
                            "Unspec Pain*", "Musculoskeletal Pain",
                            "Head Pain", "Chest Pain", "Abdominal Pain"))

pain_ref <- pain %>% 
  filter(opholds_grundlag == "asyl") %>% 
  group_by(pain) %>% 
  mutate(ny_leng = length(pain),
         ny_leng = as.double(ny_leng),
         ny_leng = if_else(ny_leng < 5, 5, ny_leng)) %>% 
  ungroup() %>% 
  mutate(freq = ny_leng / length(opholds_grundlag),
         ref_dk = "Refugee")
pain_dk <- pain %>% 
  filter(is.na(opholds_grundlag)) %>% 
  group_by(pain) %>% 
  mutate(ny_leng = length(pain)) %>% 
  ungroup() %>% 
  mutate(freq = ny_leng / length(opholds_grundlag),
         ref_dk = "Dane")

pain_dk_check <- pain_dk %>% 
  group_by(pain) %>% 
  distinct(pain, .keep_all = TRUE)

pain_ref_dk <- rbind(pain_ref, pain_dk)


ggplot(pain_ref_dk, aes(x = freq, y = pain, colour = ref_dk, shape = ref_dk)) +
  geom_point() +
  ggtitle("Fraction plot of pain categories") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Fraction") +
  ylab("Pain categories") + 
  theme(legend.title = element_blank()) +
  scale_colour_manual(values = c("orange", "purple")) +
  scale_shape_manual(values = c(1, 2))

