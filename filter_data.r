#script to filter the imported datasets

NHS24_Sat_Sun_AA <- filter(NHS24_Sat_Sun, board == "NHS AYRSHIRE & ARRAN")
NHS24_Sat_Sun_borders <- filter(NHS24_Sat_Sun, board == "NHS BORDERS")
NHS24_Sat_Sun_dg <- filter(NHS24_Sat_Sun, board == "NHS DUMFRIES & GALLOWAY")
NHS24_Sat_Sun_fife <- filter(NHS24_Sat_Sun, board == "NHS FIFE")
NHS24_Sat_Sun_forthV <- filter(NHS24_Sat_Sun, board == "NHS FORTH VALLEY")
NHS24_Sat_Sun_gramp <- filter(NHS24_Sat_Sun, board == "NHS GRAMPIAN")
NHS24_Sat_Sun_glow <- filter(NHS24_Sat_Sun, board == "NHS GREATER GLASGOW & CLYDE")
NHS24_Sat_Sun_high <- filter(NHS24_Sat_Sun, board == "NHS HIGHLAND")
NHS24_Sat_Sun_lan <- filter(NHS24_Sat_Sun, board == "NHS LANARKSHIRE")
NHS24_Sat_Sun_loth <- filter(NHS24_Sat_Sun, board == "NHS LOTHIAN")
NHS24_Sat_Sun_ork <- filter(NHS24_Sat_Sun, board == "NHS ORKNEY")
NHS24_Sat_Sun_shet <- filter(NHS24_Sat_Sun, board == "NHS SHETLAND")
NHS24_Sat_Sun_tay <- filter(NHS24_Sat_Sun, board == "NHS TAYSIDE")
NHS24_Sat_Sun_west <- filter(NHS24_Sat_Sun, board == "NHS WESTERN ISLES")
NHS24_Sat_Sun_scot <- filter(NHS24_Sat_Sun, board == "Scotland")


NHS24_Mon_Fri_AA <- filter(NHS24_Mon_Fri, board == "NHS AYRSHIRE & ARRAN")
NHS24_Mon_Fri_borders <- filter(NHS24_Mon_Fri, board == "NHS BORDERS")
NHS24_Mon_Fri_dg <- filter(NHS24_Mon_Fri, board == "NHS DUMFRIES & GALLOWAY")
NHS24_Mon_Fri_fife <- filter(NHS24_Mon_Fri, board == "NHS FIFE")
NHS24_Mon_Fri_forthV <- filter(NHS24_Mon_Fri, board == "NHS FORTH VALLEY")
NHS24_Mon_Fri_gramp <- filter(NHS24_Mon_Fri, board == "NHS GRAMPIAN")
NHS24_Mon_Fri_glow <- filter(NHS24_Mon_Fri, board == "NHS GREATER GLASGOW & CLYDE")
NHS24_Mon_Fri_high <- filter(NHS24_Mon_Fri, board == "NHS HIGHLAND")
NHS24_Mon_Fri_lan <- filter(NHS24_Mon_Fri, board == "NHS LANARKSHIRE")
NHS24_Mon_Fri_loth <- filter(NHS24_Mon_Fri, board == "NHS LOTHIAN")
NHS24_Mon_Fri_ork <- filter(NHS24_Mon_Fri, board == "NHS ORKNEY")
NHS24_Mon_Fri_shet <- filter(NHS24_Mon_Fri, board == "NHS SHETLAND")
NHS24_Mon_Fri_tay <- filter(NHS24_Mon_Fri, board == "NHS TAYSIDE")
NHS24_Mon_Fri_west <- filter(NHS24_Mon_Fri, board == "NHS WESTERN ISLES")
NHS24_Mon_Fri_scot <- filter(NHS24_Mon_Fri, board == "Scotland")


NHS24_Mon_Fri_IH <- filter(NHS24_Mon_Fri_IH, !is.na(value))
NHS24_Mon_Fri_IH_AA <- filter(NHS24_Mon_Fri_IH, board == "NHS AYRSHIRE & ARRAN")
NHS24_Mon_Fri_IH_borders <- filter(NHS24_Mon_Fri_IH, board == "NHS BORDERS")
NHS24_Mon_Fri_IH_dg <- filter(NHS24_Mon_Fri_IH, board == "NHS DUMFRIES & GALLOWAY")
NHS24_Mon_Fri_IH_fife <- filter(NHS24_Mon_Fri_IH, board == "NHS FIFE")
NHS24_Mon_Fri_IH_forthV <- filter(NHS24_Mon_Fri_IH, board == "NHS FORTH VALLEY")
NHS24_Mon_Fri_IH_gramp <- filter(NHS24_Mon_Fri_IH, board == "NHS GRAMPIAN")
NHS24_Mon_Fri_IH_glow <- filter(NHS24_Mon_Fri_IH, board == "NHS GREATER GLASGOW & CLYDE")
NHS24_Mon_Fri_IH_high <- filter(NHS24_Mon_Fri_IH, board == "NHS HIGHLAND")
NHS24_Mon_Fri_IH_lan <- filter(NHS24_Mon_Fri_IH, board == "NHS LANARKSHIRE")
NHS24_Mon_Fri_IH_loth <- filter(NHS24_Mon_Fri_IH, board == "NHS LOTHIAN")
NHS24_Mon_Fri_IH_ork <- filter(NHS24_Mon_Fri_IH, board == "NHS ORKNEY")
NHS24_Mon_Fri_IH_shet <- filter(NHS24_Mon_Fri_IH, board == "NHS SHETLAND")
NHS24_Mon_Fri_IH_tay <- filter(NHS24_Mon_Fri_IH, board == "NHS TAYSIDE")
NHS24_Mon_Fri_IH_west <- filter(NHS24_Mon_Fri_IH, board == "NHS WESTERN ISLES")
NHS24_Mon_Fri_IH_scot <- filter(NHS24_Mon_Fri_IH, board == "Scotland")

#if you forget to get rid of the NA values 
# NHS24_Mon_Fri_IH_AA <- filter(NHS24_Mon_Fri_IH_AA, !is.na(value))
# NHS24_Mon_Fri_IH_borders <- filter(NHS24_Mon_Fri_IH_borders, !is.na(value))
# NHS24_Mon_Fri_IH_dg <- filter(NHS24_Mon_Fri_IH_dg, !is.na(value))
# NHS24_Mon_Fri_IH_fife <- filter(NHS24_Mon_Fri_IH_fife, !is.na(value))
# NHS24_Mon_Fri_IH_forthV <- filter(NHS24_Mon_Fri_IH_forthV, !is.na(value))
# NHS24_Mon_Fri_IH_gramp <- filter(NHS24_Mon_Fri_IH_gramp, !is.na(value))
# NHS24_Mon_Fri_IH_glow <- filter(NHS24_Mon_Fri_IH_glow, !is.na(value))
# NHS24_Mon_Fri_IH_high <- filter(NHS24_Mon_Fri_IH_high, !is.na(value))
# NHS24_Mon_Fri_IH_lan <- filter(NHS24_Mon_Fri_IH_lan, !is.na(value))
# NHS24_Mon_Fri_IH_loth <- filter(NHS24_Mon_Fri_IH_loth, !is.na(value))
# NHS24_Mon_Fri_IH_ork <- filter(NHS24_Mon_Fri_IH_ork, !is.na(value))
# NHS24_Mon_Fri_IH_shet <- filter(NHS24_Mon_Fri_IH_shet, !is.na(value))
# NHS24_Mon_Fri_IH_tay <- filter(NHS24_Mon_Fri_IH_tay, !is.na(value))
# NHS24_Mon_Fri_IH_west <- filter(NHS24_Mon_Fri_IH_west, !is.na(value))
# NHS24_Mon_Fri_IH_scot <- filter(NHS24_Mon_Fri_IH_scot, !is.na(value))


SAS_attended_AA <- filter(SAS_attended, board == "Ayrshire & Arran")
SAS_attended_borders <- filter(SAS_attended, board == "Borders")
SAS_attended_dg <- filter(SAS_attended, board == "Dumfries & Galloway")
SAS_attended_fife <- filter(SAS_attended, board == "Fife")
SAS_attended_forthV <- filter(SAS_attended, board == "Forth Valley")
SAS_attended_gramp <- filter(SAS_attended, board == "Grampian")
SAS_attended_glow <- filter(SAS_attended, board == "Greater Glasgow & Clyde")
SAS_attended_high <- filter(SAS_attended, board == "Highland")
SAS_attended_lan <- filter(SAS_attended, board == "Lanarkshire")
SAS_attended_loth <- filter(SAS_attended, board == "Lothian")
SAS_attended_ork <- filter(SAS_attended, board == "Orkney")
SAS_attended_shet <- filter(SAS_attended, board == "Shetland")
SAS_attended_tay <- filter(SAS_attended, board == "Tayside")
SAS_attended_west <- filter(SAS_attended, board == "Western Isles")
SAS_attended_scot <- filter(SAS_attended, board == "Scotland")


SAS_conveyed_AA <- filter(SAS_conveyed, board == "Ayrshire & Arran")
SAS_conveyed_borders <- filter(SAS_conveyed, board == "Borders")
SAS_conveyed_dg <- filter(SAS_conveyed, board == "Dumfries & Galloway")
SAS_conveyed_fife <- filter(SAS_conveyed, board == "Fife")
SAS_conveyed_forthV <- filter(SAS_conveyed, board == "Forth Valley")
SAS_conveyed_gramp <- filter(SAS_conveyed, board == "Grampian")
SAS_conveyed_glow <- filter(SAS_conveyed, board == "Greater Glasgow & Clyde")
SAS_conveyed_high <- filter(SAS_conveyed, board == "Highland")
SAS_conveyed_lan <- filter(SAS_conveyed, board == "Lanarkshire")
SAS_conveyed_loth <- filter(SAS_conveyed, board == "Lothian")
SAS_conveyed_ork <- filter(SAS_conveyed, board == "Orkney")
SAS_conveyed_shet <- filter(SAS_conveyed, board == "Shetland")
SAS_conveyed_tay <- filter(SAS_conveyed, board == "Tayside")
SAS_conveyed_west <- filter(SAS_conveyed, board == "Western Isles")
SAS_conveyed_scot <- filter(SAS_conveyed, board == "Scotland")


GPOOH_Mon_Fri_AA <- filter(GPOOH_Mon_Fri, board == "NHS AYRSHIRE & ARRAN")
GPOOH_Mon_Fri_borders <- filter(GPOOH_Mon_Fri, board == "NHS BORDERS")
GPOOH_Mon_Fri_dg <- filter(GPOOH_Mon_Fri, board == "NHS DUMFRIES & GALLOWAY")
GPOOH_Mon_Fri_fife <- filter(GPOOH_Mon_Fri, board == "NHS FIFE")
GPOOH_Mon_Fri_forthV <- filter(GPOOH_Mon_Fri, board == "NHS FORTH VALLEY")
GPOOH_Mon_Fri_gramp <- filter(GPOOH_Mon_Fri, board == "NHS GRAMPIAN")
GPOOH_Mon_Fri_glow <- filter(GPOOH_Mon_Fri, board == "NHS GREATER GLASGOW & CLYDE")
GPOOH_Mon_Fri_high <- filter(GPOOH_Mon_Fri, board == "NHS HIGHLAND")
GPOOH_Mon_Fri_lan <- filter(GPOOH_Mon_Fri, board == "NHS LANARKSHIRE")
GPOOH_Mon_Fri_loth <- filter(GPOOH_Mon_Fri, board == "NHS LOTHIAN")
GPOOH_Mon_Fri_ork <- filter(GPOOH_Mon_Fri, board == "NHS ORKNEY")
GPOOH_Mon_Fri_shet <- filter(GPOOH_Mon_Fri, board == "NHS SHETLAND")
GPOOH_Mon_Fri_tay <- filter(GPOOH_Mon_Fri, board == "NHS TAYSIDE")
GPOOH_Mon_Fri_west <- filter(GPOOH_Mon_Fri, board == "NHS WESTERN ISLES")
GPOOH_Mon_Fri_scot <- filter(GPOOH_Mon_Fri, board == "Scotland")


GPOOH_Sat_Sun_AA <- filter(GPOOH_Sat_Sun, board == "NHS AYRSHIRE & ARRAN")
GPOOH_Sat_Sun_borders <- filter(GPOOH_Sat_Sun, board == "NHS BORDERS")
GPOOH_Sat_Sun_dg <- filter(GPOOH_Sat_Sun, board == "NHS DUMFRIES & GALLOWAY")
GPOOH_Sat_Sun_fife <- filter(GPOOH_Sat_Sun, board == "NHS FIFE")
GPOOH_Sat_Sun_forthV <- filter(GPOOH_Sat_Sun, board == "NHS FORTH VALLEY")
GPOOH_Sat_Sun_gramp <- filter(GPOOH_Sat_Sun, board == "NHS GRAMPIAN")
GPOOH_Sat_Sun_glow <- filter(GPOOH_Sat_Sun, board == "NHS GREATER GLASGOW & CLYDE")
GPOOH_Sat_Sun_high <- filter(GPOOH_Sat_Sun, board == "NHS HIGHLAND")
GPOOH_Sat_Sun_lan <- filter(GPOOH_Sat_Sun, board == "NHS LANARKSHIRE")
GPOOH_Sat_Sun_loth <- filter(GPOOH_Sat_Sun, board == "NHS LOTHIAN")
GPOOH_Sat_Sun_ork <- filter(GPOOH_Sat_Sun, board == "NHS ORKNEY")
GPOOH_Sat_Sun_shet <- filter(GPOOH_Sat_Sun, board == "NHS SHETLAND")
GPOOH_Sat_Sun_tay <- filter(GPOOH_Sat_Sun, board == "NHS TAYSIDE")
GPOOH_Sat_Sun_west <- filter(GPOOH_Sat_Sun, board == "NHS WESTERN ISLES")
GPOOH_Sat_Sun_scot <- filter(GPOOH_Sat_Sun, board == "Scotland")


assHub_AA <- filter(assHub, board == "NHS AYRSHIRE & ARRAN")
assHub_borders <- filter(assHub, board == "NHS BORDERS")
assHub_dg <- filter(assHub, board == "NHS DUMFRIES & GALLOWAY")
assHub_fife <- filter(assHub, board == "NHS FIFE")
assHub_forthV <- filter(assHub, board == "NHS FORTH VALLEY")
assHub_gramp <- filter(assHub, board == "NHS GRAMPIAN")
assHub_glow <- filter(assHub, board == "NHS GREATER GLASGOW & CLYDE")
assHub_high <- filter(assHub, board == "NHS HIGHLAND")
assHub_lan <- filter(assHub, board == "NHS LANARKSHIRE")
assHub_loth <- filter(assHub, board == "NHS LOTHIAN")
assHub_ork <- filter(assHub, board == "NHS ORKNEY")
assHub_shet <- filter(assHub, board == "NHS SHETLAND")
assHub_tay <- filter(assHub, board == "NHS TAYSIDE")
assHub_west <- filter(assHub, board == "NHS WESTERN ISLES")
assHub_scot <- filter(assHub, board == "Scotland")


selfPres_AA <- filter(selfPres, board == "NHS AYRSHIRE & ARRAN")
selfPres_borders <- filter(selfPres, board == "NHS BORDERS")
selfPres_dg <- filter(selfPres, board == "NHS DUMFRIES & GALLOWAY")
selfPres_fife <- filter(selfPres, board == "NHS FIFE")
selfPres_forthV <- filter(selfPres, board == "NHS FORTH VALLEY")
selfPres_gramp <- filter(selfPres, board == "NHS GRAMPIAN")
selfPres_glow <- filter(selfPres, board == "NHS GREATER GLASGOW & CLYDE")
selfPres_high <- filter(selfPres, board == "NHS HIGHLAND")
selfPres_lan <- filter(selfPres, board == "NHS LANARKSHIRE")
selfPres_loth <- filter(selfPres, board == "NHS LOTHIAN")
selfPres_ork <- filter(selfPres, board == "NHS ORKNEY")
selfPres_shet <- filter(selfPres, board == "NHS SHETLAND")
selfPres_tay <- filter(selfPres, board == "NHS TAYSIDE")
selfPres_west <- filter(selfPres, board == "NHS WESTERN ISLES")
selfPres_scot <- filter(selfPres, board == "Scotland")


fourHrPerf_AA <- filter(fourHrPerf, board == "NHS AYRSHIRE & ARRAN")
fourHrPerf_borders <- filter(fourHrPerf, board == "NHS BORDERS")
fourHrPerf_dg <- filter(fourHrPerf, board == "NHS DUMFRIES & GALLOWAY")
fourHrPerf_fife <- filter(fourHrPerf, board == "NHS FIFE")
fourHrPerf_forthV <- filter(fourHrPerf, board == "NHS FORTH VALLEY")
fourHrPerf_gramp <- filter(fourHrPerf, board == "NHS GRAMPIAN")
fourHrPerf_glow <- filter(fourHrPerf, board == "NHS GREATER GLASGOW & CLYDE")
fourHrPerf_high <- filter(fourHrPerf, board == "NHS HIGHLAND")
fourHrPerf_lan <- filter(fourHrPerf, board == "NHS LANARKSHIRE")
fourHrPerf_loth <- filter(fourHrPerf, board == "NHS LOTHIAN")
fourHrPerf_ork <- filter(fourHrPerf, board == "NHS ORKNEY")
fourHrPerf_shet <- filter(fourHrPerf, board == "NHS SHETLAND")
fourHrPerf_tay <- filter(fourHrPerf, board == "NHS TAYSIDE")
fourHrPerf_west <- filter(fourHrPerf, board == "NHS WESTERN ISLES")
fourHrPerf_scot <- filter(fourHrPerf, board == "Scotland")


emerg_ad_AA <- filter(emerg_ad, board == "NHS Ayrshire & Arran")
emerg_ad_borders <- filter(emerg_ad, board == "NHS Borders")
emerg_ad_dg <- filter(emerg_ad, board == "NHS Dumfries & Galloway")
emerg_ad_fife <- filter(emerg_ad, board == "NHS Fife")
emerg_ad_forthV <- filter(emerg_ad, board == "NHS Forth Valley")
emerg_ad_gramp <- filter(emerg_ad, board == "NHS Grampian")
emerg_ad_glow <- filter(emerg_ad, board == "NHS Greater Glasgow & Clyde")
emerg_ad_high <- filter(emerg_ad, board == "NHS Highland")
emerg_ad_lan <- filter(emerg_ad, board == "NHS Lanarkshire")
emerg_ad_loth <- filter(emerg_ad, board == "NHS Lothian")
emerg_ad_ork <- filter(emerg_ad, board == "NHS Orkney")
emerg_ad_shet <- filter(emerg_ad, board == "NHS Shetland")
emerg_ad_tay <- filter(emerg_ad, board == "NHS Tayside")
emerg_ad_west <- filter(emerg_ad, board == "NHS Western Isles")
emerg_ad_scot <- filter(emerg_ad, board == "Scotland")


emerg_ad <- mutate(emerg_ad, day = weekdays(date))
emerg_ad_weekday <- filter(emerg_ad, day != "Saturday" & day != "Sunday")
emerg_ad_weekend <- filter(emerg_ad, day == "Saturday" | day == "Sunday")
emerg_ad_weekday_AA <- filter(emerg_ad_weekday, board == "NHS Ayrshire & Arran")
emerg_ad_weekday_borders <- filter(emerg_ad_weekday, board == "NHS Borders")
emerg_ad_weekday_dg <- filter(emerg_ad_weekday, board == "NHS Dumfries & Galloway")
emerg_ad_weekday_fife <- filter(emerg_ad_weekday, board == "NHS Fife")
emerg_ad_weekday_forthV <- filter(emerg_ad_weekday, board == "NHS Forth Valley")
emerg_ad_weekday_gramp <- filter(emerg_ad_weekday, board == "NHS Grampian")
emerg_ad_weekday_glow <- filter(emerg_ad_weekday, board == "NHS Greater Glasgow & Clyde")
emerg_ad_weekday_high <- filter(emerg_ad_weekday, board == "NHS Highland")
emerg_ad_weekday_lan <- filter(emerg_ad_weekday, board == "NHS Lanarkshire")
emerg_ad_weekday_loth <- filter(emerg_ad_weekday, board == "NHS Lothian")
emerg_ad_weekday_ork <- filter(emerg_ad_weekday, board == "NHS Orkney")
emerg_ad_weekday_shet <- filter(emerg_ad_weekday, board == "NHS Shetland")
emerg_ad_weekday_tay <- filter(emerg_ad_weekday, board == "NHS Tayside")
emerg_ad_weekday_west <- filter(emerg_ad_weekday, board == "NHS Western Isles")
emerg_ad_weekday_scot <- filter(emerg_ad_weekday, board == "Scotland")

emerg_ad_weekend_AA <- filter(emerg_ad_weekend, board == "NHS Ayrshire & Arran")
emerg_ad_weekend_borders <- filter(emerg_ad_weekend, board == "NHS Borders")
emerg_ad_weekend_dg <- filter(emerg_ad_weekend, board == "NHS Dumfries & Galloway")
emerg_ad_weekend_fife <- filter(emerg_ad_weekend, board == "NHS Fife")
emerg_ad_weekend_forthV <- filter(emerg_ad_weekend, board == "NHS Forth Valley")
emerg_ad_weekend_gramp <- filter(emerg_ad_weekend, board == "NHS Grampian")
emerg_ad_weekend_glow <- filter(emerg_ad_weekend, board == "NHS Greater Glasgow & Clyde")
emerg_ad_weekend_high <- filter(emerg_ad_weekend, board == "NHS Highland")
emerg_ad_weekend_lan <- filter(emerg_ad_weekend, board == "NHS Lanarkshire")
emerg_ad_weekend_loth <- filter(emerg_ad_weekend, board == "NHS Lothian")
emerg_ad_weekend_ork <- filter(emerg_ad_weekend, board == "NHS Orkney")
emerg_ad_weekend_shet <- filter(emerg_ad_weekend, board == "NHS Shetland")
emerg_ad_weekend_tay <- filter(emerg_ad_weekend, board == "NHS Tayside")
emerg_ad_weekend_west <- filter(emerg_ad_weekend, board == "NHS Western Isles")
emerg_ad_weekend_scot <- filter(emerg_ad_weekend, board == "Scotland")


NHS24_paediatrics <- mutate(NHS24_paediatrics, day = weekdays(date))
NHS24_paediatrics_weekend <- filter(NHS24_paediatrics, day == "Saturday" | day == "Sunday")
NHS24_paediatrics_weekday <- filter(NHS24_paediatrics, day != "Saturday" & day != "Sunday")
NHS24_under_1_weekday <- mutate(NHS24_paediatrics_weekday, value = under_1)
NHS24_under_12_weekday <- mutate(NHS24_paediatrics_weekday, value = under_12)
NHS24_1_4_weekday <- mutate(NHS24_paediatrics_weekday, value = age_1_4)
NHS24_5_9_weekday <- mutate(NHS24_paediatrics_weekday, value = age_5_9)
NHS24_10_14_weekday <- mutate(NHS24_paediatrics_weekday, value = age_10_14)
NHS24_15_19_weekday <- mutate(NHS24_paediatrics_weekday, value = age_15_19)
NHS24_under_1_weekend <- mutate(NHS24_paediatrics_weekend, value = under_1)
NHS24_under_12_weekend <- mutate(NHS24_paediatrics_weekend, value = under_12)
NHS24_1_4_weekend <- mutate(NHS24_paediatrics_weekend, value = age_1_4)
NHS24_5_9_weekend <- mutate(NHS24_paediatrics_weekend, value = age_5_9)
NHS24_10_14_weekend <- mutate(NHS24_paediatrics_weekend, value = age_10_14)
NHS24_15_19_weekend <- mutate(NHS24_paediatrics_weekend, value = age_15_19)

self_pres_under_1 <- mutate(self_pres_paediatrics, value = under_1)
self_pres_under_18_months <- mutate(self_pres_paediatrics, value = under_18_months)
self_pres_under_12 <- mutate(self_pres_paediatrics, value = under_12)
self_pres_1_4 <- mutate(self_pres_paediatrics, value = age_1_4)
self_pres_5_9 <- mutate(self_pres_paediatrics, value = age_5_9)
self_pres_10_14 <- mutate(self_pres_paediatrics, value = age_10_14)
self_pres_15_19 <- mutate(self_pres_paediatrics, value = age_15_19)

ED_attendances_under_1 <- mutate(ED_attendances_paediatrics, value = under_1)
ED_attendances_under_18_months <- mutate(ED_attendances_paediatrics, value = under_18_months)
ED_attendances_under_12 <- mutate(ED_attendances_paediatrics, value = under_12)
ED_attendances_1_4 <- mutate(ED_attendances_paediatrics, value = age_1_4)
ED_attendances_5_9 <- mutate(ED_attendances_paediatrics, value = age_5_9)
ED_attendances_10_14 <- mutate(ED_attendances_paediatrics, value = age_10_14)
ED_attendances_15_19 <- mutate(ED_attendances_paediatrics, value = age_15_19)


SAS_paediatrics_under_1 <- mutate(SAS_paediatrics, value = under_1)
SAS_paediatrics_under_12 <- mutate(SAS_paediatrics, value = under_12)
SAS_paediatrics_1_4 <- mutate(SAS_paediatrics, value = age_1_4)
SAS_paediatrics_5_9 <- mutate(SAS_paediatrics, value = age_5_9)
SAS_paediatrics_10_14 <- mutate(SAS_paediatrics, value = age_10_14)
SAS_paediatrics_15_19 <- mutate(SAS_paediatrics, value = age_15_19)


GPOOH_MH <- mutate(GPOOH_MH, day = weekdays(date))
GPOOH_MH_weekday <- filter(GPOOH_MH, day != "Saturday" & day != "Sunday")
GPOOH_MH_weekend <- filter(GPOOH_MH, day == "Saturday" | day == "Sunday")


NHS24_MH_PWP_Symptoms <- mutate(NHS24_MH_PWP_Symptoms, day = weekdays(date))
NHS24_MH_PWP_Symptoms_weekday <- filter(NHS24_MH_PWP_Symptoms, day != "Saturday" & day != "Sunday")
NHS24_MH_PWP_Symptoms_weekend <- filter(NHS24_MH_PWP_Symptoms, day == "Saturday" | day == "Sunday")

NHS24_MH_PWP_only <- mutate(NHS24_MH_PWP_only, day = weekdays(date))
NHS24_MH_PWP_only_weekday <- filter(NHS24_MH_PWP_only, day != "Saturday" & day != "Sunday")
NHS24_MH_PWP_only_weekend <- filter(NHS24_MH_PWP_only, day == "Saturday" | day == "Sunday")