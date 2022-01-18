#### missing toks #### 
data_frame <- read_rds("final_data_frame_210118.rds")

missing_toks <- data_frame %>% 
  select(ud_toks, Aktionsdiagnose, opholds_grundlag, icd10)

# creating fraction of missing toks, both for when first seen and at discharge for asyl
missing_toks_asyl <- missing_toks %>% 
  filter(opholds_grundlag == "asyl") %>% 
  group_by(icd10) %>% 
  summarise(n_ud_toks = sum(is.na(ud_toks))) %>% 
  ungroup() %>% 
  summarise(icd10 = icd10, n_ud_toks = n_ud_toks) %>% 
  mutate(n_ud_toks = as.double(n_ud_toks),
         n_ud_toks = if_else(n_ud_toks < 5, 5, n_ud_toks),
         freq = n_ud_toks/sum(n_ud_toks),
         asyl = "Refugee")


#making long format, so can be used in ggplot
longer_missing_toks_asyl <- pivot_longer(missing_toks_asyl, 
                                         cols = "freq",
                                         names_to = "toks",
                                         values_to = "missing_fraction")

# doing the same as above, but for danes.
missing_toks_dk <- missing_toks %>% 
  filter(is.na(opholds_grundlag)) %>% 
  group_by(icd10) %>% 
  summarise(n_ud_toks = sum(is.na(ud_toks))) %>% 
  ungroup() %>% 
  summarise(icd10 = icd10, n_ud_toks = n_ud_toks,freq = n_ud_toks/sum(n_ud_toks)) %>% 
  mutate(asyl = "Native Dane")

#making long format, so can be used in ggplot
longer_missing_toks_dk <- pivot_longer(missing_toks_dk, 
                                       cols = c("freq"),
                                       names_to = "toks_ind_ud",
                                       values_to = "missing_fraction")
## joining data frame for dk and asyl:
missing_toks_asyl_dk <- rbind(missing_toks_asyl, missing_toks_dk) %>% 
  arrange(desc(freq)) %>% 
  mutate(icd10 = str_replace_all(icd10, "_", " "),
         icd10 = str_to_title(icd10),
         icd10 = str_replace(icd10, "(Digestive System)", "Digestive System*"), 
         icd10 = str_replace(icd10, "(Diseases Of The Blood)", "Diseases Of The Blood*"),
         icd10 = str_replace(icd10, "(Endocrine Nutritional Metabolic)", "Endocrine Nutritional Metabolic*"),
         icd10 = str_replace(icd10, "(Pregnancy Childbirth Puerperium)", "Pregnancy Childbirth Puerperium*"),
         icd10 = str_replace(icd10, "(Ear And Mastoid Process)", "Ear And Mastoid Process*"),
         icd10 = str_replace(icd10, "(Eye And Adnexa)", "Eye And Adnexa*"),
         icd10 = fct_relevel(icd10, "Symptoms Signs Abnormal Clinical Laboratory",
                             "Musculoskeletal And Connective Tissue", "Skin And Subcutaneous Tissue",
                             "Mental Behavioral Neurodevelopmental", "Respiratory System", "Circulatory System",
                             "Digestive System*", "Certain Infectious And Parasitic", "Nervous System", 
                             "Genitourinary System", "Malformations Deformations Chromosomal", "Eye And Adnexa*", "Endocrine Nutritional Metabolic*",
                             "Diseases Of The Blood*", "Ear And Mastoid Process*", "Pregnancy Childbirth Puerperium*",
                             "Neoplasms", "Conditions Originating Perinatal"))

ggplot(missing_toks_asyl_dk, aes(freq, icd10, colour = asyl, shape = asyl)) +
  geom_point() +
  ggtitle("Fraction plot of missing TOKS, according to ICD 10 chapter \n and stratified on refugees and native Danes") +
  ylab("ICD 10 diagnosis chapter") + 
  xlab("Fraction of missing TOKS values") + 
  theme(legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  scale_colour_manual(values = c("orange", "purple")) +
  scale_shape_manual(values = c(1, 2))


