# oprindelsesland

iepe_2017 <- read_sas("iepe2017.sas7bdat")
iepe_2018 <- read_sas("iepe2018.sas7bdat")

# rename columns, to make them identical...

iepe_2017 <- iepe_2017 %>% 
  rename_all(tolower)

iepe_2018 <- iepe_2018 %>% 
  rename_all(tolower)

iepe_total <- left_join(iepe_2018, iepe_2017, by = c("pnr", "ie_type", "opr_land"))

iepe_total <- iepe_total %>% 
  mutate(indvandre_type =  factor(ie_type,
                                  levels = c(1, 2, 3),
                                  labels = c("dane", "immigrant", "efterkommer")),
         country_origin = factor(opr_land,
                                 levels = c(5100, 5102, 5103, 5104, 5106, 5108, 5110, 5120, 5122, 5126, 5128, 5129, 5130, 5134, 5140, 5142,
                                            5150, 5151, 5152, 5153, 5154, 5156, 5158, 5160, 5162, 5164, 5170, 5172, 5174, 5180, 5199, 5202,
                                            5204, 5207, 5213, 5214, 5215, 5216, 5222, 5228, 5231, 5232, 5234, 5236, 5238, 5240, 5244, 5245,
                                            5246, 5247, 5255, 5258, 5262, 5266, 5268, 5269, 5272, 5276, 5277, 5278, 5279, 5281, 5282, 5284, 
                                            5285, 5287, 5288, 5289, 5293, 5295, 5296, 5299, 5302, 5304, 5306, 5308, 5314, 5316, 5318, 5322,
                                            5324, 5326, 5328, 5342, 5348, 5352, 5354, 5356, 5358, 5364, 5366, 5374, 5376, 5390, 5392, 5402,
                                            5403, 5404, 5406, 5408, 5410, 5414, 5418, 5422, 5424, 5432, 5434, 5436, 5438, 5442, 5444, 5446, 
                                            5448, 5452, 5454, 5456, 5458, 5459, 5464, 5466, 5472, 5474, 5478, 5482, 5484, 5486, 5487, 5488,
                                            5492, 5496, 5502, 5514, 5522, 5525, 5526, 5607, 5609, 5611, 5700, 5704, 5706, 5708, 5710, 5712,
                                            5714, 5716, 5718, 5720, 5724, 5750, 5752, 5754, 5756, 5757, 5758, 5761, 5776, 5778, 5999),
                                 labels = c("Denmark", "Foreign_country_unknown", "Stateless", "Finland", "Iceland", "Luxemburg", "Norway", "Sweeden",
                                            "Albania", "Belgium", "Bulgaria", "tjekkoslovakiet", "France", "Greece", "Holland", 
                                            "Ireland", "Italy",  "Serbia_montenegro", "Yugoslavia",  "Malta", "Poland", "Portugal", "Rumenia",
                                            "Scwitzerland", "Sovijet_Union", "Spain", "Great_Britian", "Turkey", "Hungary", "Germany", "eu_uoplyst",
                                            "Algeria", "Angola", "Botswana", "Burundi", "Ethiopia", "Comoros", "Eritrea", "Gambia", "Ghana", 
                                            "Guinea_Bissau", "Guinea", "Kenya", "Liberia", "Libya", "Mozambique", "Morocco", "Mauritius", "Nigeria",
                                            "Namibia", "Sierra_leone", "Sudan", "South_Africa", "Tanzania", "Tunesia", "Uganda", "Egypt", 
                                            "Central_African_Republic", "Cameroon", "Democratic_Republic_Congo", "Republic_Congo", "Benin", 
                                            "Cote_dIvoire", "Mauretanien", "Niger", "Rwanda", "Senegal", "Somalia", "Togo", "Zimbabwe", "Zambia",
                                            "afrika_uoplyst", "argentina", "bolivia", "brasilien", "guyana", "canada", "chile", "Colombia", 
                                            "costa_rica", "cuba", "dominikansk_republik", "ecuador", "haiti", "hounduras", "jamaica", "mexico",
                                            "nicaragua", "panama", "paraguay", "peru", "trinidad_tobago", "uruguay", "usa", "venezuela", "yemen",
                                            "United_Arab_Emirates", "Afghanistan", "Bahrain", "Bhutan", "Bangladesh", "Myanmar", "Sri_Lanka",
                                            "Cypern", "Taiwan", "India", "Indonesia", "Iraq", "Iran", "Israel", "Japan", "Jordan", "China", 
                                            "Kuwait", "Laos", "Lebanon", "Malaysia", "Mongolia", "Nepal", "nordkorea", "Pakistan", "Philippins",
                                            "Saudi_Arabia", "singapore", "sydkorea", "Syria", "Middeleast_unknown", "Vietnam", "Thailand",
                                            "qatar", "Australien", "new_zealand", "samoa", "djibouti", "belize", "estland", "letland", "litauen",
                                            "Russia", "ukraine", "hviderusland", "armenien", "Azerbaijan", "Moldova", "Uzbekistan", "kasakhstan", 
                                            "turkmenistan", "kirgisistan", "georgien", "Croatia", "Slovenia", "Bosnia_hercegovina", "Macedonia",
                                            "Serbia", "Yugoslavien_forbundsrepublikken", "Kosovo", "Czechia", "Slovakia", "land_ukendt")))

write_rds(iepe_total, path = "dta_country_origin_ie_type200827.rds")

iepe_total <- read_rds("dta_country_origin_ie_type200827.rds")

# who grouping of countries:
# New grouping of country_of_origin
iepe_total_grouped <- iepe_total %>% 
  mutate(grouped_country_origin_who = case_when(
    country_origin %in% "Afghanistan" ~ "Eastern_Mediterranean",
    country_origin %in% "Syria" ~ "Eastern_Mediterranean",
    country_origin %in% "Iraq" ~ "Eastern_Mediterranean",
    country_origin %in% "Iran" ~ "Eastern_Mediterranean",
    country_origin %in% "Kuwait" ~ "Eastern_Mediterranean",
    country_origin %in% "Somalia" ~ "Eastern_Mediterranean",
    country_origin %in% "Eritrea" ~ "African", 
    country_origin %in% "Denmark" ~ "Danish",
    country_origin %in% "Democratic_Republic_Congo" ~ "African",
    country_origin %in% "Lebanon" ~ "Eastern_Mediterranean",
    country_origin %in% "Yugoslavia" ~ "Ex-Yugoslavian",
    country_origin %in% "Serbia" ~ "Ex-Yugoslavian",
    country_origin %in% "Serbia_montenegro" ~ "Ex-Yugoslavian",
    country_origin %in% "Croatia" ~ "Ex-Yugoslavian",
    country_origin %in% "Slovenia" ~ "Ex-Yugoslavian",
    country_origin %in% "Bosnia_hercegovina" ~ "Ex-Yugoslavian",
    country_origin %in% "Macedonia" ~ "Ex-Yugoslavian",
    country_origin %in% "Yugoslavien_forbundsrepublikken" ~ "Ex-Yugoslavian",
    country_origin %in% "Kosovo" ~ "Ex-Yugoslavian",
    country_origin %in% "Algeria" ~ "African",
    country_origin %in% "Angola" ~ "African",
    country_origin %in% "Burundi" ~ "African",
    country_origin %in% "Ethiopia" ~ "African",
    country_origin %in% "Cameroon" ~ "African",
    country_origin %in% "Libya" ~ "Eastern_Mediterranean",
    country_origin %in% "Rwanda" ~ "African",
    country_origin %in% "Sudan" ~ "Eastern_Mediterranean", 
    country_origin %in% "Egypt" ~ "Eastern_Mediterranean", 
    country_origin %in% "Republic_Congo" ~ "African",
    country_origin %in% "Togo" ~ "African",
    country_origin %in% "Cote_dIvoire" ~ "African",
    country_origin %in% "Central_African_Republic" ~ "African",
    country_origin %in% "Middeleast_unknown" ~ "Eastern_Mediterranean",
    country_origin %in% "Armenia" ~ "European",
    country_origin %in% "Uzbekistan" ~ "European",
    country_origin %in% "United_Arab_Emirates" ~ "Eastern_Mediterranean",
    country_origin %in% "Israel" ~ "European",
    country_origin %in% "Azerbaijan" ~ "European",
    country_origin %in% "Saudi_arabia" ~ "Eastern_Mediterranean",
    country_origin %in% "Pakistan" ~ "Eastern_Mediterranean",
    country_origin %in% "Jordan" ~ "Eastern_Mediterranean",
    country_origin %in% "Sri_Lanka" ~ "South_East_Asian",
    country_origin %in% "Bhutan" ~ "South_East_Asian",
    country_origin %in% "Vietnam" ~ "Other",
    country_origin %in% "Myanmar" ~ "South_East_Asian",
    country_origin %in% "China" ~ "Other",
    country_origin %in% "Russia" ~ "European",
    country_origin %in% "Albania" ~ "European",
    country_origin %in% "Scwitzerland" ~ "European",
    country_origin %in% "Turkey" ~ "European",
    country_origin %in% "Moldova" ~ "European",
    country_origin %in% "Sovijet_Union" ~ "European",
    country_origin %in% "Colombia" ~ "Other",
    country_origin %in% "Stateless" ~ "Other",
    country_origin %in% "Foreign_country_unknown" ~ NA_character_),
    country_origin = as.character(country_origin),
    grouped_country_origin = if_else(is.na(grouped_country_origin_who), country_origin, grouped_country_origin_who))


### bruges ikke ###
iepe_total_grouped <- iepe_total %>% 
  mutate(grouped_country_origin = case_when(
    country_origin %in% "jugoslavien" ~ "ex-yugosalavia",
    country_origin %in% "serbien" ~ "ex-yugosalavia",
    country_origin %in% "serbien_montenegro" ~ "ex-yugosalavia",
    country_origin %in% "kroatien" ~ "ex-yugosalavia",
    country_origin %in% "slovenien" ~ "ex-yugosalavia",
    country_origin %in% "bosnien_hercegovina" ~ "ex-yugosalavia",
    country_origin %in% "makedonien" ~ "ex-yugosalavia",
    country_origin %in% "jugoslavien_forbundsrepublikken" ~ "ex-yugosalavia",
    country_origin %in% "kosovo" ~ "ex-yugosalavia",
    country_origin %in% "algeriet" ~ "other_african",
    country_origin %in% "angola" ~ "other_african",
    country_origin %in% "burundi" ~ "other_african",
    country_origin %in% "etiopien" ~ "other_african",
    country_origin %in% "cameroun" ~ "other_african",
    country_origin %in% "libyen" ~ "other_african",
    country_origin %in% "rwanda" ~ "other_african",
    country_origin %in% "sudan" ~ "other_african",
    country_origin %in% "egypten" ~ "other_african",
    country_origin %in% "congo_republikken" ~ "other_african",
    country_origin %in% "togo" ~ "other_african",
    country_origin %in% "elfenbenskysten" ~ "other_african",
    country_origin %in% "centralafrikansk_republik" ~ "other_african",
    country_origin %in% "mellemoesten_uoplyst" ~ "other_middle_east",
    country_origin %in% "armenien" ~ "other_middle_east",
    country_origin %in% "usbekistan" ~ "other_middle_east",
    country_origin %in% "forenede_arabiske_emirater" ~ "other_middle_east",
    country_origin %in% "isreal" ~ "other_middle_east",
    country_origin %in% "aserbajdsjan" ~ "other_middle_east",
    country_origin %in% "saudi_arabien" ~ "other_middle_east",
    country_origin %in% "pakistan" ~ "other_middle_east",
    country_origin %in% "jordan" ~ "other_middle_east",
    country_origin %in% "sri_lanka" ~ "other_asian",
    country_origin %in% "bhutan" ~ "other_asian",
    country_origin %in% "vietnam" ~ "other_asian",
    country_origin %in% "myanmar" ~ "other_asian",
    country_origin %in% "kina" ~ "other_asian",
    country_origin %in% "rusland" ~ "other_eu",
    country_origin %in% "albanien" ~ "other_eu",
    country_origin %in% "schweiz" ~ "other_eu",
    country_origin %in% "tyrkiet" ~ "other_eu",
    country_origin %in% "moldova" ~ "other_eu",
    country_origin %in% "sovjetunionen" ~ "other_eu",
    country_origin %in% "colombia" ~ "other",
    country_origin %in% "statsloes" ~ "other",
    country_origin %in% "udlandet_uoplyst" ~ NA_character_),
    country_origin = as.character(country_origin),
    grouped_country_origin = if_else(is.na(grouped_country_origin), country_origin, grouped_country_origin))

write_rds(iepe_total_grouped, "iepe_total_grouped_210118.rds")





