#Socioeconomic status


#Loading the files:
 socio_eco_2004 <- read_sas("akm2004.sas7bdat")
 socio_eco_2005 <- read_sas("akm2005.sas7bdat")
 socio_eco_2006 <- read_sas("akm2006.sas7bdat")
 socio_eco_2007 <- read_sas("akm2007.sas7bdat")
 socio_eco_2008 <- read_sas("akm2008.sas7bdat")
 socio_eco_2009 <- read_sas("akm2009.sas7bdat")
 socio_eco_2010 <- read_sas("akm2010.sas7bdat")
 socio_eco_2011 <- read_sas("akm2011.sas7bdat")
 socio_eco_2012 <- read_sas("akm2012.sas7bdat")
 socio_eco_2013 <- read_sas("akm2013.sas7bdat")
 socio_eco_2014 <- read_sas("akm2014.sas7bdat")
 socio_eco_2015 <- read_sas("akm2015.sas7bdat")
 socio_eco_2016 <- read_sas("akm2016.sas7bdat")
 socio_eco_2017 <- read_sas("akm2017.sas7bdat")
 socio_eco_2018 <- read_sas("akm2018.sas7bdat")
 
 # renamin all to lowercase
 socio_eco_2018 <- socio_eco_2018 %>% 
   rename("pnr" = "PNR")
 
 socio_eco_2017 <- socio_eco_2017 %>% 
   rename("pnr" = "PNR")
 

# joining them to one dataset:

 total_socio_eco <- list(socio_eco_2004,
                         socio_eco_2005,
                         socio_eco_2006,
                         socio_eco_2007,
                         socio_eco_2008,
                         socio_eco_2009,
                         socio_eco_2010,
                         socio_eco_2011,
                         socio_eco_2012,
                         socio_eco_2013,
                         socio_eco_2014,
                         socio_eco_2015,
                         socio_eco_2016,
                         socio_eco_2017,
                         socio_eco_2018) %>% 
   reduce(full_join, by = c("pnr", "SOCIO13")) %>% 
   distinct(pnr, .keep_all = TRUE)

 #Tilføjet d. 10/9
total_socio_eco <- total_socio_eco %>% 
   mutate(socio_tekst = factor(SOCIO13,
                               levels = c(111, 112, 113, 
                                          114, 120, 131, 
                                          132, 133, 134, 
                                          135, 139, 210, 
                                          220, 310, 321, 
                                          322, 323, 330, 
                                          410, 420),
                               labels = c("selvstaendig_10_eller_flere", "selvstaendig_5_9", "selvstaendig_1_4", 
                                          "selvstaendig_ingen", "medarbejdende_aegtefaelle", "loen_ledelse",
                                          "loen_faerdigheder_hoejeste", "loen_faerdigheder_mellem", "loen_faerdigheder_grund",
                                          "loen_andre", "loen_stilling_ikke_oplyst", "arbejs_loes_min_halvdelen_aaret",
                                          "modtager_syge_su_orlov_mm", "under_uddan_min_15aar", "foetidspensionist",
                                          "folkepensionist", "efterloensmodtager", "kontathjælp", 
                                          "andre", "u_15aar")))

 
write_rds(total_socio_eco, path = "dta_all_socio_eco300910.rds")
 
# New grouping of socioeconimic status;
ses <- read_rds("dta_all_socio_eco300910.rds")

ses_grouped <- ses %>% 
   mutate(socio_tekst = as.character(socio_tekst),
          new_socio = case_when( # new grouping of s
             socio_tekst %in% "folkepensionist" ~ "pensionist",
             socio_tekst %in% "efterloensmodtager" ~ "pensionist",
             socio_tekst %in% "medarbejdende_aegtefaelle" ~ "andre",
             socio_tekst %in% "arbejs_loes_min_halvdelen_aaret" ~ "andre",
             socio_tekst %in% "modtager_syge_su_orlov_mm" ~ "kontant_sygedp",
             socio_tekst %in% "kontathjælp" ~ "kontant_sygedp"),
          three_levels_socio = case_when(
             socio_tekst %in% "folkepensionist" ~ "not_working",
             socio_tekst %in% "efterloensmodtager" ~ "not_working",
             socio_tekst %in% "medarbejdende_aegtefaelle" ~ "not_working",
             socio_tekst %in% "arbejs_loes_min_halvdelen_aaret" ~ "not_working",
             socio_tekst %in% "modtager_syge_su_orlov_mm" ~ "not_working",
             socio_tekst %in% "kontathjælp" ~ "not_working",
             socio_tekst %in% "andre" ~ "not_working",
             socio_tekst %in% "foetidspensionist" ~ "not_working",
             socio_tekst %in% "under_uddan_min_15aar" ~ "student",
             socio_tekst %in% "selvstaendig_10_eller_flere" ~ "working",
             socio_tekst %in% "selvstaendig_5_9" ~ "working",
             socio_tekst %in% "selvstaendig_1_4" ~ "working",
             socio_tekst %in% "selvstaendig_ingen" ~ "working",
             socio_tekst %in% "loen_faerdigheder_hoejeste" ~ "working",
             socio_tekst %in% "loen_faerdigheder_mellem" ~ "working",
             socio_tekst %in% "loen_faerdigheder_grund" ~ "working",
             socio_tekst %in% "loen_ledelse" ~ "working",
             socio_tekst %in% "loen_andre" ~ "working",
             socio_tekst %in% "loen_stilling_ikke_oplyst" ~ "working"),
          new_socio = if_else(!is.na(new_socio), new_socio, socio_tekst),
          new_socio = str_remove_all(new_socio, "(_ingen)|(_[1|5]_[4|9])|_(10_eller_flere)"), #creating one group for indepented ses
          new_socio = str_remove_all(new_socio, "(_faerdigheder_hoejeste)|(_ledelse)|(_faerdigheder_mellem)|(_faerdigheder_grund)|(_andre)|(_stilling_ikke_oplyst)"))

write_rds(ses_grouped, "socio_eco_grouped_210431.rds")

