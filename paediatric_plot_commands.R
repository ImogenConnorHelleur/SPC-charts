plot_SPC(cht_title = "ED Attendances per Day",chart_typ = "C", df = ed_attendances_1_4, 
         place_title = "Age: 1-4",breakPoint = 40, breakPoint2 = 111)
plot_SPC(cht_title = "ED Attendances per Day",chart_typ = "C", df = ed_attendances_10_14, 
         place_title = "Age: 10-14",breakPoint = 35, breakPoint2 = 107)
plot_SPC(cht_title = "ED Attendances per Day",chart_typ = "C", df = ed_attendances_15_19, 
         place_title = "Age: 15-19",breakPoint = 35, breakPoint2 = 104, breakPoint3 = 125)
plot_SPC(cht_title = "ED Attendances per Day",chart_typ = "C", df = ed_attendances_5_9, 
         place_title = "Age: 5-9",breakPoint = 37, breakPoint2 = 111)
plot_SPC(cht_title = "ED Attendances per Day",chart_typ = "C", df = ed_attendances_under_1, 
         place_title = "Age: Under 1",breakPoint = 108)
plot_SPC(cht_title = "ED Attendances per Day",chart_typ = "C", df = ed_attendances_under_12, 
         place_title = "Age: Under 12",breakPoint = 36, breakPoint2 = 107, breakPoint3 = 128)


plot_SPC(cht_title = "ED Performance per Day",chart_typ = "P", df = ed_performance_1_4, 
         place_title = "Age: 1-4")
plot_SPC(cht_title = "ED Performance per Day",chart_typ = "P", df = ed_performance_10_14, 
         place_title = "Age: 10-14")
plot_SPC(cht_title = "ED Performance per Day",chart_typ = "P", df = ed_performance_15_19, 
         place_title = "Age: 15-19")
plot_SPC(cht_title = "ED Performance per Day",chart_typ = "P", df = ed_performance_5_9, 
         place_title = "Age: 5-9", breakPoint = 22, override_y_lim = 110)
plot_SPC(cht_title = "ED Performance per Day",chart_typ = "P", df = ed_performance_under_1, 
         place_title = "Age: Under 1", override_y_lim = 110, override_annotation_dist_P = 20, breakPoint = 47, breakPoint2 = 71, breakPoint3 = 106)
plot_SPC(cht_title = "ED Performance per Day",chart_typ = "P", df = ed_performance_under_12, 
         place_title = "Age: Under 12", override_y_lim = 110, breakPoint = 35)


plot_SPC(cht_title = "Self Presenters per Day",chart_typ = "C", df = self_presenters_1_4,
         place_title = "Age: 1-4", breakPoint = 38, breakPoint2 = 91, breakPoint3 = 126)
plot_SPC(cht_title = "Self Presenters per Day",chart_typ = "C", df = self_presenters_10_14,
         place_title = "Age: 10-14", breakPoint = 37, breakPoint2 = 106, breakPoint3 = 129)
plot_SPC(cht_title = "Self Presenters per Day",chart_typ = "C", df = self_presenters_15_19,
         place_title = "Age: 15-19", breakPoint = 35, breakPoint2 = 90, breakPoint3 = 111, breakPoint4 = 132)
plot_SPC(cht_title = "Self Presenters per Day",chart_typ = "C", df = self_presenters_5_9,
         place_title = "Age: 5-9")
plot_SPC(cht_title = "Self Presenters per Day",chart_typ = "C", df = self_presenters_under_1,
         place_title = "Age: Under 1", breakPoint = 124)
plot_SPC(cht_title = "Self Presenters per Day",chart_typ = "C", df = self_presenters_under_12,
         place_title = "Age: Under 12", breakPoint = 37, breakPoint2 = 104, breakPoint3 = 125)


# plot_SPC(cht_title = "NHS 24 Contacts per Day",chart_typ = "C", df = nhs24_1_4_daily, 
#          place_title = "Age: 1-4",breakPoint = 123)
# plot_SPC(cht_title = "NHS 24 Contacts per Day",chart_typ = "C", df = nhs24_10_14_daily, 
#          place_title = "Age: 10-14",breakPoint = 33, breakPoint2 = 90)
# plot_SPC(cht_title = "NHS 24 Contacts per Day",chart_typ = "C", df = nhs24_15_19_daily, 
#          place_title = "Age: 15-19")
# plot_SPC(cht_title = "NHS 24 Contacts per Day",chart_typ = "C", df = nhs24_5_9_daily, 
#          place_title = "Age: 5-9")
# plot_SPC(cht_title = "NHS 24 Contacts per Day",chart_typ = "C", df = nhs24_under_1_daily, 
#          place_title = "Age: Under 1")
# plot_SPC(cht_title = "NHS 24 Contacts per Day",chart_typ = "C", df = nhs24_under_12_daily, 
#          place_title = "Age: Under 12")

plot_SPC(cht_title = "NHS 24 Weekday Contacts per Day",chart_typ = "C", df = NHS24_under_1_weekday, 
         place_title = "Age: Under 1", breakPoint = 39, breakPoint2 = 92)
plot_SPC(cht_title = "NHS 24 Weekday Contacts per Day",chart_typ = "C", df = NHS24_under_12_weekday, 
         place_title = "Age: Under 12")
plot_SPC(cht_title = "NHS 24 Weekday Contacts per Day",chart_typ = "C", df = NHS24_1_4_weekday, 
         place_title = "Age: 1-4", breakPoint = 70, breakPoint2 = 91)
plot_SPC(cht_title = "NHS 24 Weekday Contacts per Day",chart_typ = "C", df = NHS24_5_9_weekday, 
         place_title = "Age: 5-9", breakPoint = 26, breakPoint2 = 66, breakPoint3 = 91)
plot_SPC(cht_title = "NHS 24 Weekday Contacts per Day",chart_typ = "C", df = NHS24_10_14_weekday, 
         place_title = "Age: 10-14", breakPoint = 22, breakPoint2 = 51, breakPoint3 = 91)
plot_SPC(cht_title = "NHS 24 Weekday Contacts per Day",chart_typ = "C", df = NHS24_15_19_weekday, 
         place_title = "Age: 15-19", breakPoint = 26, breakPoint2 = 66, breakPoint3 = 96)





plot_SPC(cht_title = "NHS 24 Weekend Contacts per Day",chart_typ = "C", df = NHS24_1_4_weekend, 
         place_title = "Age: 1-4", breakPoint = 35)
plot_SPC(cht_title = "NHS 24 Weekend Contacts per Day",chart_typ = "C", df = NHS24_10_14_weekend, 
         place_title = "Age: 10-14", breakPoint = 31)
plot_SPC(cht_title = "NHS 24 Weekend Contacts per Day",chart_typ = "C", df = NHS24_15_19_weekend, 
         place_title = "Age: 15-19", breakPoint = 27)
plot_SPC(cht_title = "NHS 24 Weekend Contacts per Day",chart_typ = "C", df = NHS24_5_9_weekend, 
         place_title = "Age: 5-9")
plot_SPC(cht_title = "NHS 24 Weekend Contacts per Day",chart_typ = "C", df = NHS24_under_1_weekend, 
         place_title = "Age: Under 1", breakPoint = 31)
plot_SPC(cht_title = "NHS 24 Weekend Contacts per Day",chart_typ = "C", df = NHS24_under_12_weekend, 
         place_title = "Age: Under 12", breakPoint = 35)


plot_SPC(cht_title = "NHS 24 In Hours Contacts per Day",chart_typ = "C", df = NHS24_IH_1_4,
         place_title = "Age: 1-4", breakPoint = 89, exclude = 20, exclude2 = c(1,2))
plot_SPC(cht_title = "NHS 24 In Hours Contacts per Day",chart_typ = "C", df = NHS24_IH_10_14,
         place_title = "Age: 10-14", breakPoint = 24, breakPoint2 = 53)
plot_SPC(cht_title = "NHS 24 In Hours Contacts per Day",chart_typ = "C", df = NHS24_IH_15_19,
         place_title = "Age: 15-19", breakPoint = 24, breakPoint2 = 63)
plot_SPC(cht_title = "NHS 24 In Hours Contacts per Day",chart_typ = "C", df = NHS24_IH_5_9,
         place_title = "Age: 5-9", breakPoint = 29, breakPoint2 = 63, breakPoint3 = 89, 
         exclude = c(1,2,20), exclude4 = 1)
plot_SPC(cht_title = "NHS 24 In Hours Contacts per Day",chart_typ = "C", df = NHS24_IH_under_1,
         place_title = "Age: Under 1", breakPoint = 29, breakPoint2 = 73)
plot_SPC(cht_title = "NHS 24 In Hours Contacts per Day",chart_typ = "C", df = NHS24_IH_under_12,
         place_title = "Age: Under 12",  breakPoint2  = 66,breakPoint = 26,breakPoint3 = 93,
         exclude = c(2,20), exclude3 = c(19))


plot_SPC(cht_title = "SAS Contacts per Day",chart_typ = "C", df = SAS_1_4, 
         place_title = "Age: 1-4",breakPoint = 21, breakPoint2 = 45, breakPoint3 = 80, breakPoint4 = 111)
plot_SPC(cht_title = "SAS Contacts per Day",chart_typ = "C", df = SAS_10_14, 
         place_title = "Age: 10-14",breakPoint = 43, breakPoint2 = 86, breakPoint3 = 110)
plot_SPC(cht_title = "SAS Contacts per Day",chart_typ = "C", df = SAS_15_19, 
         place_title = "Age: 15-19", breakPoint = 124)
plot_SPC(cht_title = "SAS Contacts per Day",chart_typ = "C", df = SAS_5_9, 
         place_title = "Age: 5-9", breakPoint = 117)
plot_SPC(cht_title = "SAS Contacts per Day",chart_typ = "C", df = SAS_under_1, 
         place_title = "Age: Under 1", breakPoint = 70, breakPoint2 = 107)
plot_SPC(cht_title = "SAS Contacts per Day",chart_typ = "C", df = SAS_under_12, 
         place_title = "Age: Under 12", breakPoint = 24, breakPoint2 = 45, breakPoint3 = 78, breakPoint4 = 111)


plot_SPC(cht_title = "NHS 24 Weekday Contacts per Day",chart_typ = "C", df = NHS24_mh_weekday,
         place_title = "Mental Health", breakPoint = 9, breakPoint2 = 98)
plot_SPC(cht_title = "NHS 24 Weekend Contacts per Day",chart_typ = "C", df = NHS24_mh_weekend,
         place_title = "Mental Health")
plot_SPC(cht_title = "GPOOH Weekday Contacts per Day",chart_typ = "C", df = GPOOH_mh_weekday,
         place_title = "Mental Health")
plot_SPC(cht_title = "GPOOH Weekend Contacts per Day",chart_typ = "C", df = GPOOH_mh_weekend,
         place_title = "Mental Health")
plot_SPC(cht_title = "ED Attendances per Day",chart_typ = "C", df = ed_attendances_mh,
         place_title = "Mental Health", breakPoint = 50, breakPoint2 = 106)
plot_SPC(cht_title = "SAS Contacts per Day",chart_typ = "C'", df = SAS_mh,
         place_title = "Mental Health", breakPoint = 21, breakPoint2 = 89, breakPoint3 = 153)

