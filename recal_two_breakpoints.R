library(tidyverse)
library(dplyr)
library(qicharts2)
library(ggplot2)
library(scales)
library(zoo)
library(lubridate)
library(wktmo)
library(grid)
library(gridExtra)

source("spc_rules.R")

plot_volume_recalc_two_breakpoints <- function(df, 
                                          r1_col = "orange", r2_col = "steelblue3", 
                                          cht_title = "title",
                                          place_title = "Ayrshire and Arran",
                                          breakPoint = nrow(df),
                                          breakPoint2 = nrow(df),
                                          chart_typ = "C",
                                          plot_chart = T
) { 
  
  
  df$date <- as.Date(df$date, tz = "Europe/London")
  df_all <- df 
  st.dt <- min(df$date)
  ed.dt <- max(df$date)
  
  if(chart_typ == "C"){
    pct_firstPeriod <- qicharts2::qic(date, value, data = df[1:21,], chart = 'c') 
    pct_full <- qicharts2::qic(date, value, data = df, chart = 'c')
  }else if(chart_typ == "C'"){
    pct_firstPeriod <- qicharts2::qic(date, value, n = rep(1, 21), data = df[1:21,], chart = 'up') 
    pct_full <- qicharts2::qic(date, value, n = rep(1, nrow(df)), data = df, chart = 'up')
  }else if(chart_typ == "P'"){
    pct_firstPeriod <- qicharts2::qic(date, nonBreach, n = total, data = df[1:21,], chart = 'pp', multiply = 100)
    pct_full <- qicharts2::qic(date, nonBreach, n = total, data = df, chart = 'pp', multiply = 100)
  }else if(chart_typ == "P"){
    pct_firstPeriod <- qicharts2::qic(date, nonBreach, n = total, data = df[1:21,], chart = 'p', multiply = 100)
    pct_full <- qicharts2::qic(date, nonBreach, n = total, data = df, chart = 'p', multiply = 100)
  }
  
  #calculate limits for first 21 points - solid lines
  pct_firstPeriod <- pct_firstPeriod$data %>%
    select(x,ucl,lcl, cl)
  pct_recal <- pct_full$data %>%
    left_join(pct_firstPeriod, by = "x") 
  pct_recal[0:21,"Limit"] <- "new period" 
  
  #lock limits for next points after the first 21 until there is a Rule 2 break - dotted line 
  pct_recal[22:breakPoint,"ucl.y"] <- pct_recal[21, "ucl.y"]
  pct_recal[22:breakPoint,"lcl.y"] <- pct_recal[21, "lcl.y"]
  pct_recal[22:breakPoint,"cl.y"] <- pct_recal[21, "cl.y"]
  pct_recal[21:breakPoint,"limitType2"] <- "period extension"
  
  #recalculate limits for 21 day period after rule break - solid lines 
  end_point <- min(nrow(df), (breakPoint+21))
  extension_length <- end_point - breakPoint
  
  if(chart_typ == "C"){
    pct_ruleBreak <- qicharts2::qic(date, value, data = df[breakPoint:end_point,], 
                                    chart = 'c') 
  }else if(chart_typ == "C'"){
    pct_ruleBreak <- qicharts2::qic(date, value, n = rep(1, (extension_length+1)), data = df[breakPoint:end_point,], 
                                    chart = 'up') 
  }else if(chart_typ == "P'"){
    pct_ruleBreak <- qicharts2::qic(date, nonBreach, n = total, data = df[breakPoint:end_point,], chart = 'pp', multiply = 100)
  }else if(chart_typ == "P"){
    pct_ruleBreak <- qicharts2::qic(date, nonBreach, n = total, data = df[breakPoint:end_point,], chart = 'p', multiply = 100)
  }
  
  pct_ruleBreak <- pct_ruleBreak$data %>%
    select(x,ucl,lcl, cl)
  pct_recal <- pct_recal %>%
    left_join(pct_ruleBreak, by = "x") %>%
    mutate(ucl = ifelse(is.na(ucl), ucl.y, ucl)) %>%
    mutate(lcl = ifelse(is.na(lcl), lcl.y, lcl)) %>%
    mutate(cl = ifelse(is.na(cl), cl.y, cl))
  pct_recal[(breakPoint):end_point,"limitType3"] <- "new period" 
  
  #lock limits afterwards until next rule 2 break - dotted line 
  pct_recal[end_point:breakPoint2,"ucl"] <- pct_recal[end_point, "ucl"]
  pct_recal[end_point:breakPoint2,"lcl"] <- pct_recal[end_point, "lcl"]
  pct_recal[end_point:breakPoint2,"cl"] <- pct_recal[end_point, "cl"]
  pct_recal[end_point:breakPoint2,"limitType4"] <- "period extension"
  
  #recalculate limits after rule break 2 - solid line
  end_point2 <- min(nrow(df), (breakPoint2+21))
  extension_length2 <- end_point2 - breakPoint2
  
  if(chart_typ == "C"){
    pct_ruleBreak2 <- qicharts2::qic(date, value, data = df[breakPoint2:end_point2,], 
                                    chart = 'c')
  }else if(chart_typ == "C'"){
    pct_ruleBreak2 <- qicharts2::qic(date, value, n = rep(1, (extension_length2+1)), data = df[breakPoint2:end_point2,], 
                                     chart = 'up')
  }else if(chart_typ == "P'"){
    pct_ruleBreak2 <- qicharts2::qic(date, nonBreach, n = total, data = df[breakPoint2:end_point2,], chart = 'pp', multiply = 100)
  }else if(chart_typ == "P"){
    pct_ruleBreak2 <- qicharts2::qic(date, nonBreach, n = total, data = df[breakPoint2:end_point2,], chart = 'p', multiply = 100)
  }
  
  pct_ruleBreak2 <- pct_ruleBreak2$data %>%
    select(x,ucl,lcl, cl)
  pct_recal <- pct_recal %>%
    left_join(pct_ruleBreak2, by = "x") %>%
    rename(ucl = ucl.x.x, lcl = lcl.x.x, cl = cl.x.x) %>%
    mutate(ucl = ifelse(is.na(ucl), ucl.y.y, ucl)) %>%
    mutate(lcl = ifelse(is.na(lcl), lcl.y.y, lcl)) %>%
    mutate(cl = ifelse(is.na(cl), cl.y.y, cl))
  pct_recal[(breakPoint2):end_point2,"limitType5"] <- "new period"
  
  #lock limits afterwards until end - dotted
  pct_recal[end_point2:nrow(df),"ucl"] <- pct_recal[end_point2, "ucl"]
  pct_recal[end_point2:nrow(df),"lcl"] <- pct_recal[end_point2, "lcl"]
  pct_recal[end_point2:nrow(df),"cl"] <- pct_recal[end_point2, "cl"]
  pct_recal[end_point2:nrow(df),"limitType6"] <- "period extension"


  pct_recal$x <- as.Date(pct_recal$x, tz = 'Europe/London')
  cht_data <- add_rule_breaks(pct_recal)
  #fix for limits dropping down on last point
  cht_data$cl[nrow(cht_data)] <- cht_data$cl[nrow(cht_data) - 1]
  cht_data$ucl[nrow(cht_data)] <- cht_data$ucl[nrow(cht_data) - 1]
  cht_data$lcl[nrow(cht_data)] <- cht_data$lcl[nrow(cht_data) - 1]
  pct <- ggplot(cht_data, aes(x,y))

  # chart y limit
  ylimlow <- 0

  if(chart_typ == "C" | chart_typ == "C'"){
    ylimhigh <- max(df$value) + max(df$value)/10 +10
    ytitle <- "Number"
  }else if(chart_typ == "P" | chart_typ == "P'"){
    ylimhigh <- 100
    ytitle <- "Percentage"
  }

  cl_start <- round(cht_data$cl[1])
  cl_end <- round(cht_data$cl[(nrow(df)-1)])

  if(plot_chart == T){

    format_recal_control_chart_double(pct, r1_col = r1_col, r2_col = r2_col) +
      scale_x_date(labels = date_format("%Y-%m-%d"), breaks = seq(st.dt, ed.dt, 7),
                   limits = c(st.dt, ed.dt)) +
      ggtitle(cht_title, subtitle = place_title) +
      labs(x = "Day", y = ytitle,
           caption = paste(chart_typ,"Shewhart Chart.","\n*Shewhart chart rules apply (see Understanding the Analysis tab for more detail) \nRule 1: Any month outside the control limits \nRule 2: Eight or more consecutive months all above, or all below, the centre line"),
           size = 10) +
      scale_y_continuous(limits = c(ylimlow, ylimhigh),
                         breaks = breaks_pretty(),
                         labels = number_format(accuracy = 1, big.mark = ",")) +
      annotate("text", x=st.dt, y=cl_start + cl_start/25, label = cl_start) +
      annotate("text", x=ed.dt, y=cl_end + cl_start/25, label = cl_end)

  }else{
    cht_data
  }
}


format_recal_control_chart_double <- function(cht, r1_col, r2_col, ymin, ymax) {
  point_colours <- c("Rule 1" = r1_col, "Rule 2" = r2_col, "None" = "black")
  cht + 
    geom_line(colour = "black", size = 0.5) + 
    geom_line(aes(x,cl, linetype = Limit), size = 0.75) +
    geom_line(aes(x,ucl, linetype = Limit), size = 0.5) +
    geom_line(aes(x,lcl, linetype = Limit), size = 0.5) +
    geom_line(aes(x,cl, linetype = limitType2), size = 0.75) +
    geom_line(aes(x,ucl, linetype = limitType2), size = 0.75) +
    geom_line(aes(x,lcl, linetype = limitType2), size = 0.75) +
    geom_line(aes(x,cl, linetype = limitType3), size = 0.75) +
    geom_line(aes(x,ucl, linetype = limitType3), size = 0.5) +
    geom_line(aes(x,lcl, linetype = limitType3), size = 0.5) +
    geom_line(aes(x,cl, linetype = limitType4), size = 0.75) +
    geom_line(aes(x,ucl, linetype = limitType4), size = 0.75) +
    geom_line(aes(x,lcl, linetype = limitType4), size = 0.75) +
    geom_line(aes(x,cl, linetype = limitType5), size = 0.75) +
    geom_line(aes(x,ucl, linetype = limitType5), size = 0.5) +
    geom_line(aes(x,lcl, linetype = limitType5), size = 0.5) +
    geom_line(aes(x,cl, linetype = limitType6), size = 0.75) +
    geom_line(aes(x,ucl, linetype = limitType6), size = 0.75) +
    geom_line(aes(x,lcl, linetype = limitType6), size = 0.75) +
    geom_point(aes(colour = highlight), size = 2) +
    scale_color_manual("Rule triggered*", values = point_colours) + 
    theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour = "grey80"),
          panel.grid.minor = element_blank(), panel.background = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1.0, size = 14),
          axis.text.y = element_text(size = 14), axis.title = element_text(size = 14),
          plot.title = element_text(size = 20, hjust = 0),
          plot.subtitle = element_text(size = 16, face = "italic"),
          axis.line = element_line(colour = "grey60"),
          plot.caption = element_text(size = 10, hjust = 0.5)) 
  
}

