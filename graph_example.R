rm(list = ls()) #clear list

#automatic installation of required packages
packages <- c("xlsx","calibrate","stargazer","sandwich","lmtest","getopt","CausalGAM","ggplot2","reshape2","xts",
              "lattice","gridExtra","gtable","plm","lfe","lmtest","car","tis","foreign","MASS","quantreg","ggrepel",
              "dplyr","stringr","datasets","rio","psych","systemfit","MatchIt","CRTgeeDR","eurostat","plyr","zoo","ggthemes",
              "robumeta","metafor","dplyr","clubSandwich","Hmisc","metafor","pracma","pkgs","broom","sjPlot", "here", "data.table",
              "countrycode", "GGally", "car", "lpirfs")

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

ipak(packages)

#load packages
library(xlsx) #Excel-Paket laden
library(calibrate) #Laden des Pakets, das f??r Datenbeschriftung n??tig ist
library (stargazer) #Laden des Pakets, mit dem R-Regressionsoutput in Latex-Tabellen ??bergef??hrt werden kann
library(sandwich)
library(lmtest)
library(getopt)
library(CausalGAM)
library(ggplot2)
library(reshape2)
library(xts)
library(lattice)
library(gridExtra)
library(gtable)
library(plm)
library(lfe)
library(lmtest)
library(car)
library(tis)
library(foreign)
library(MASS)
library(quantreg)
library(ggrepel)
library(dplyr)
library(stringr)
library(ggplot2)
library(datasets)
library(rio)
library(psych)
library(systemfit)
library(foreign)
library(MatchIt)
library(CRTgeeDR)
library(eurostat)
library(plyr)
library(zoo)
library(ggthemes)
library("robumeta")
library("metafor")
library("dplyr")
library(clubSandwich)
library(Hmisc)
library(metafor)
library(pracma)
library(broom)
library(sjPlot)
library(here)
library(data.table)
library(countrycode)

#read data
data <- fread(here("data_poleconfisc.csv"), check.names = FALSE, header=TRUE)
data<-subset(data, year %in% c('1980', '1981', '1982', '1983', '1984', '1985', '1986', '1987', '1988', '1989', '1990', '1991', '1992', '1993', '1994', '1995', '1996', '1997', '1998', '1999', '2000', '2001', '2002', '2003','2004','2005','2006','2007','2008','2009','2010','2011','2012','2013','2014','2015'))
data$log_RGDPB <- log(data$RGDPB)
data$log_GPINVB <- log(data$GPINVB)
data$OGAP <- as.double(data$OGAP)
data$INFL <- as.double(data$INFL)
data$log_RATIOINVCONS <- log(data$RATIOINVCONS)
data$log_gini_disp <- log(data$gini_disp)
data$log_HOUS <- log(data$HOUS)
data$log_ENVIR <- log(data$ENVIR)
data$log_ECAFF <- log(data$ECAFF)
data$log_ORDER <- log(data$ORDER)
data$log_DEF <- log(data$DEF)
data$log_RECR <- log(data$RECR)

#restrict on eu
#data <- subset(data, ccode %in% c('ESP', 'IRL', 'ITA', 'PRT'))

#gross public investment
#reg_inv_v1 <- plm(log_ginv ~  lag(SPEND)+OGAP+lag(PDEBT), index=c("ccode", "year"), model="within", effect="twoways", data=data)
#summary(reg_inv_v1)
#coeftest(reg_inv_v1, vcov.=function(x) vcovHC(x, type="sss"))

#local projections
library(lpirfs)

data_select <- dplyr::select(data, ccode, year, TOTAL, TAX, SPEND, DSPENDE, log_ginv, log_RGDPB, PDEBT, log_gini_disp, OGAP,RGDPGR, UNEM, INFL, RGROWTH, log_RGCONS, log_RATIOINVCONS, CONSRATIOINVCONS, RYIELD, log_DEFSAFE, log_EDUC, log_HEALTH, log_OTHER, log_PUBSERV, log_SOCPR, log_HOUS, log_DEF, log_ORDER, log_RECR, log_ECAFF, log_ENVIR, lrgov)
#data_select <-na.omit(data_select)
#data_sample <- subset(data_select, year %in% c('1995', '1996', '1997','1998', '1999','2000','2001','2002','2003','2004','2005','2006','2007','2008','2009','2010','2011','2012','2013','2014','2015'))

#log real government investment
results_panel_log_ginv <- lpirfs::lp_lin_panel(data_set = data_select,
                                               endog_data="log_ginv",
                                               cumul_mult=TRUE,
                                               shock="SPEND",
                                               diff_shock=FALSE,
                                               panel_model="within",
                                               panel_effect="twoways",
                                               robust_cov="vcovSCC",
                                               l_exog_data=c("PDEBT", "RYIELD", "RGROWTH", "lrgov"),
                                               lags_exog_data=1,
                                               confint=1.959964,
                                               hor=5)

plot(results_panel_log_ginv)
results_panel_log_ginv$reg_summaries
results_panel_log_ginv$reg_summaries[[1]][4]
#prepare plot results_panel_log_ginv
results_panel_log_ginv_point_h1 <- results_panel_log_ginv$reg_summaries[[1]][1]
results_panel_log_ginv_upper_h1 <- results_panel_log_ginv$reg_summaries[[1]][1] + (1.959964*results_panel_log_ginv$reg_summaries[[1]][6]) #This is equivalent to a 95% confidence interval, which is obtained by multiplying the standard error with 1.959964 in both directions.
results_panel_log_ginv_lower_h1 <- results_panel_log_ginv$reg_summaries[[1]][1] - (1.959964*results_panel_log_ginv$reg_summaries[[1]][6])

results_panel_log_ginv_point_h2 <- results_panel_log_ginv$reg_summaries[[2]][1]
results_panel_log_ginv_upper_h2 <- results_panel_log_ginv$reg_summaries[[2]][1] + (1.959964*results_panel_log_ginv$reg_summaries[[2]][6])
results_panel_log_ginv_lower_h2 <- results_panel_log_ginv$reg_summaries[[2]][1] - (1.959964*results_panel_log_ginv$reg_summaries[[2]][6])

results_panel_log_ginv_point_h3 <- results_panel_log_ginv$reg_summaries[[3]][1]
results_panel_log_ginv_upper_h3 <- results_panel_log_ginv$reg_summaries[[3]][1] + (1.959964*results_panel_log_ginv$reg_summaries[[3]][6])
results_panel_log_ginv_lower_h3 <- results_panel_log_ginv$reg_summaries[[3]][1] - (1.959964*results_panel_log_ginv$reg_summaries[[3]][6])

results_panel_log_ginv_point_h4 <- results_panel_log_ginv$reg_summaries[[4]][1]
results_panel_log_ginv_upper_h4 <- results_panel_log_ginv$reg_summaries[[4]][1] + (1.959964*results_panel_log_ginv$reg_summaries[[4]][6])
results_panel_log_ginv_lower_h4 <- results_panel_log_ginv$reg_summaries[[4]][1] - (1.959964*results_panel_log_ginv$reg_summaries[[4]][6])

results_panel_log_ginv_point_h5 <- results_panel_log_ginv$reg_summaries[[5]][1]
results_panel_log_ginv_upper_h5 <- results_panel_log_ginv$reg_summaries[[5]][1] + (1.959964*results_panel_log_ginv$reg_summaries[[5]][6])
results_panel_log_ginv_lower_h5 <- results_panel_log_ginv$reg_summaries[[5]][1] - (1.959964*results_panel_log_ginv$reg_summaries[[5]][6])

results_panel_log_ginv_point <- c(results_panel_log_ginv_point_h1, results_panel_log_ginv_point_h2, results_panel_log_ginv_point_h3, results_panel_log_ginv_point_h4, results_panel_log_ginv_point_h5)
results_panel_log_ginv_upper <- c(results_panel_log_ginv_upper_h1, results_panel_log_ginv_upper_h2, results_panel_log_ginv_upper_h3, results_panel_log_ginv_upper_h4, results_panel_log_ginv_upper_h5)
results_panel_log_ginv_lower <- c(results_panel_log_ginv_lower_h1, results_panel_log_ginv_lower_h2, results_panel_log_ginv_lower_h3, results_panel_log_ginv_lower_h4, results_panel_log_ginv_lower_h5)
results_panel_log_ginv_point <- results_panel_log_ginv_point*100
results_panel_log_ginv_upper <- results_panel_log_ginv_upper*100
results_panel_log_ginv_lower <- results_panel_log_ginv_lower*100

results_panel_log_ginv_table <- data.frame(results_panel_log_ginv_point, results_panel_log_ginv_upper, results_panel_log_ginv_lower)
names(results_panel_log_ginv_table) <- c('point', 'upper', 'lower')
results_panel_log_ginv_table$year <- rep(1:5, 1)

#plot_results_panel_log_ginv
plot_results_panel_log_ginv<-ggplot(results_panel_log_ginv_table, aes(x=year, y=point)) +
  geom_line() +
  scale_y_continuous(limits = c(-30,0)) +
  geom_abline(intercept=0, slope=0, colour='#E41A1C', linetype=2)+
  geom_ribbon(data=results_panel_log_ginv_table, aes(ymin = lower,
                                                     ymax = upper,
                                                     linetype=NA), alpha = .25) +
  theme(panel.background = element_rect(fill = "white")) +
  labs(x = "Year",
       y = "in %",
       title = "Effect of government spending cuts (1%-point of GDP)\n on real public investment") +
  theme(title=element_text(size=8, face='bold'))+
  theme(axis.text.x = element_text(size=11))+
  theme(axis.text.y = element_text(size=11))+
  theme(legend.position="none")
plot_results_panel_log_ginv

#log real government consumption
results_panel_log_RGCONS <- lpirfs::lp_lin_panel(data_set = data_select,
                                                 endog_data="log_RGCONS",
                                                 cumul_mult=TRUE,
                                                 shock="SPEND",
                                                 diff_shock=FALSE,
                                                 panel_model="within",
                                                 panel_effect="twoways",
                                                 robust_cov="vcovSCC",
                                                 l_exog_data=c("PDEBT", "RYIELD", "RGROWTH", "lrgov"),
                                                 lags_exog_data=1,
                                                 confint=1.959964,
                                                 hor=5)

#plot(results_panel_log_RGCONS)

#prepare plot results_panel_log_RGCONS
results_panel_log_RGCONS_point_h1 <- results_panel_log_RGCONS$reg_summaries[[1]][1]
results_panel_log_RGCONS_upper_h1 <- results_panel_log_RGCONS$reg_summaries[[1]][1] + (1.959964*results_panel_log_RGCONS$reg_summaries[[1]][6])
results_panel_log_RGCONS_lower_h1 <- results_panel_log_RGCONS$reg_summaries[[1]][1] - (1.959964*results_panel_log_RGCONS$reg_summaries[[1]][6])

results_panel_log_RGCONS_point_h2 <- results_panel_log_RGCONS$reg_summaries[[2]][1]
results_panel_log_RGCONS_upper_h2 <- results_panel_log_RGCONS$reg_summaries[[2]][1] + (1.959964*results_panel_log_RGCONS$reg_summaries[[2]][6])
results_panel_log_RGCONS_lower_h2 <- results_panel_log_RGCONS$reg_summaries[[2]][1] - (1.959964*results_panel_log_RGCONS$reg_summaries[[2]][6])

results_panel_log_RGCONS_point_h3 <- results_panel_log_RGCONS$reg_summaries[[3]][1]
results_panel_log_RGCONS_upper_h3 <- results_panel_log_RGCONS$reg_summaries[[3]][1] + (1.959964*results_panel_log_RGCONS$reg_summaries[[3]][6])
results_panel_log_RGCONS_lower_h3 <- results_panel_log_RGCONS$reg_summaries[[3]][1] - (1.959964*results_panel_log_RGCONS$reg_summaries[[3]][6])

results_panel_log_RGCONS_point_h4 <- results_panel_log_RGCONS$reg_summaries[[4]][1]
results_panel_log_RGCONS_upper_h4 <- results_panel_log_RGCONS$reg_summaries[[4]][1] + (1.959964*results_panel_log_RGCONS$reg_summaries[[4]][6])
results_panel_log_RGCONS_lower_h4 <- results_panel_log_RGCONS$reg_summaries[[4]][1] - (1.959964*results_panel_log_RGCONS$reg_summaries[[4]][6])

results_panel_log_RGCONS_point_h5 <- results_panel_log_RGCONS$reg_summaries[[5]][1]
results_panel_log_RGCONS_upper_h5 <- results_panel_log_RGCONS$reg_summaries[[5]][1] + (1.959964*results_panel_log_RGCONS$reg_summaries[[5]][6])
results_panel_log_RGCONS_lower_h5 <- results_panel_log_RGCONS$reg_summaries[[5]][1] - (1.959964*results_panel_log_RGCONS$reg_summaries[[5]][6])

results_panel_log_RGCONS_point <- c(results_panel_log_RGCONS_point_h1, results_panel_log_RGCONS_point_h2, results_panel_log_RGCONS_point_h3, results_panel_log_RGCONS_point_h4, results_panel_log_RGCONS_point_h5)
results_panel_log_RGCONS_upper <- c(results_panel_log_RGCONS_upper_h1, results_panel_log_RGCONS_upper_h2, results_panel_log_RGCONS_upper_h3, results_panel_log_RGCONS_upper_h4, results_panel_log_RGCONS_upper_h5)
results_panel_log_RGCONS_lower <- c(results_panel_log_RGCONS_lower_h1, results_panel_log_RGCONS_lower_h2, results_panel_log_RGCONS_lower_h3, results_panel_log_RGCONS_lower_h4, results_panel_log_RGCONS_lower_h5)
results_panel_log_RGCONS_point <- results_panel_log_RGCONS_point*100
results_panel_log_RGCONS_upper <- results_panel_log_RGCONS_upper*100
results_panel_log_RGCONS_lower <- results_panel_log_RGCONS_lower*100

results_panel_log_RGCONS_table <- data.frame(results_panel_log_RGCONS_point, results_panel_log_RGCONS_upper, results_panel_log_RGCONS_lower)
names(results_panel_log_RGCONS_table) <- c('point', 'upper', 'lower')
results_panel_log_RGCONS_table$year <- rep(1:5, 1)

#plot_results_panel_log_RGCONS
plot_results_panel_log_RGCONS<-ggplot(results_panel_log_RGCONS_table, aes(x=year, y=point)) +
  geom_line() +
  scale_y_continuous(limits = c(-30,0)) +
  geom_abline(intercept=0, slope=0, colour='#E41A1C', linetype=2)+
  geom_ribbon(data=results_panel_log_RGCONS_table, aes(ymin = lower,
                                                       ymax = upper,
                                                       linetype=NA), alpha = .25) +
  theme(panel.background = element_rect(fill = "white")) +
  labs(x = "Year",
       y = "in %",
       title = "Effect of government spending cuts (1%-point of GDP)\n on real government consumption") +
  theme(title=element_text(size=8, face='bold'))+
  theme(axis.text.x = element_text(size=11))+
  theme(axis.text.y = element_text(size=11))+
  theme(legend.position="none")
plot_results_panel_log_RGCONS

#combine plots
library(ggpubr)
combine_plot_results_panel_log_ginv_RGCONS <- ggpubr::ggarrange(
  plot_results_panel_log_ginv, plot_results_panel_log_RGCONS, ncol = 2, nrow=1, legend = "none", labels = c("A)", "B)"))
combine_plot_results_panel_log_ginv_RGCONS

filename <- "linear_IRF_ginv_gcons.jpg"
ggsave(filename, plot = combine_plot_results_panel_log_ginv_RGCONS, width = 10, height = 8)