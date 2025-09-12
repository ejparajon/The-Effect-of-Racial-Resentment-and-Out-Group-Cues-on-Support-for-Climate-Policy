#The Effect of Racial Resentment and Out-Group Cues on Support for Climate Policy
#9/12/2025

# Preamble ----------------------------------------------------------------

#Clear workspace
rm(list=ls())

#The pacman package is required to load/install the additional packages.
if (!require("pacman")) install.packages("pacman")

#Installing and loading packages for use
pacman::p_load(broom,stargazer,rio,tidyverse,interplot,patchwork,xtable,modelsummary,naniar,estimatr,jtools,sandwich,marginaleffects,cowplot,lavaan,sensemakr)


#Loading in the data file, this assumes the datafile is saved in the same working directory as the R code file.
Main_data_analysis <- import("data/CES_dat_clean.rds")


# Calling custom functions and ggplot theme
source("custom_functions.R")

# B. Analysis ----------------------------------------------------------------

#B1. Description of DVs and key IV
#Summarizing DVs
mean(Main_data_analysis$climate_COP,na.rm=T)*100 #  61.09 % approval
mean(Main_data_analysis$climate_CPP,na.rm=T)*100 # 63.81 % approval

#Correlation between DVs
cor.test(Main_data_analysis$climate_COP, Main_data_analysis$climate_CPP, method=c("pearson")) #0.52

#Racial resentment
mean(Main_data_analysis$Racial_resentment,na.rm=T) #0.52

#Distribution of IV
RR<-pctgroup(Main_data_analysis,Racial_resentment)

#Table
print(xtable(RR,"Distribution of Racial Resentment Across Respondents"), include.rownames=FALSE)

#Saving Figure
#Title= Distribution of Racial Resentment Across White Respondents 
#ggsave("Dist_RR.png",Dist_RR,width=7, height=3,units="in",dpi=300,bg = 'white')

##### Fig1 ##### 

#B2. Hypothesis testing
#H1: The effect of racial resentment at reducing support for climate action will hold when the climate action in question is U.S. involvement in an international agreement as well as domestic climate action.

#COP21 DV logit
main_model_cop_logit<-glm(climate_COP~Racial_resentment+educ+gender+region+Income+Age+political_ideo_3+PID_leaners, family=binomial(link="logit"), data = Main_data_analysis)

#using jtools::summ() to summarize regression output

summ(main_model_cop_logit)


#CPP DV logit
main_model_cpp_logit<-glm(climate_CPP~Racial_resentment+educ+gender+region+Income+Age+political_ideo_3+PID_leaners, family=binomial(link="logit"), data = Main_data_analysis)

summ(main_model_cpp_logit)

#using jtools::make_predictions() to Generate predicted data for plotting results of regression models

  #Average case approach, with regressors at their modes, and rounding numeric variable means to nearest whole number
  # and varying the level of RR across entire observed spectrum (0-1)
unique(Main_data_analysis$Racial_resentment)
mode_func(Main_data_analysis$educ)
mode_func(Main_data_analysis$gender)
mode_func(Main_data_analysis$region)
mode_func(Main_data_analysis$political_ideo_3)
mode_func(Main_data_analysis$PID_leaners)
round(mean(Main_data_analysis$Income,na.rm=T),0)
round(mean(Main_data_analysis$Age,na.rm=T),0)

hypo_data <- expand.grid(Racial_resentment=c(0,0.125,0.25,0.375,0.5,0.625,0.75,0.875,1),
                         educ="High school graduate",
                         gender="Female",
                         region="South",
                         Income=7,
                         Age=50,
                         political_ideo_3="Conservative",
                         PID_leaners="Democrat")

COP21_pred<-make_predictions(main_model_cop_logit, pred = "Racial_resentment",interval = TRUE,new_data=hypo_data)          
COP21_pred$Policy <- "COP21 (International)"
COP21_pred$Fit<-COP21_pred$climate_COP

CPP_pred<- make_predictions(main_model_cpp_logit, pred = "Racial_resentment",interval = TRUE,new_data=hypo_data)
CPP_pred$Policy <- "Clean Power Plan (Domestic)"
CPP_pred$Fit<-CPP_pred$climate_CPP

#Combining data also displaying probabilities for H2 in text.
Fig1_data <- bind_rows(COP21_pred, CPP_pred) %>% 
  dplyr::select(Fit,ymax,ymin,Racial_resentment,Policy) %>% 
  mutate(Policy = factor(Policy, levels=c("COP21 (International)","Clean Power Plan (Domestic)")))


#Figure 1: Effect of Racial Resentment on Climate Policy Approval

#Graphing
Fig1<-ggplot(Fig1_data, aes(y=Fit,x=Racial_resentment)) +
  geom_line(aes(linetype=Policy,group=Policy,color=Policy),linewidth=1) +
  geom_ribbon(aes(group=Policy,ymin=ymin,ymax=ymax ,fill=Policy),alpha=0.5) +
  scale_color_manual(values=c("#3B9AB2","#EBCC2A"))+
  scale_fill_manual(values=c("#3B9AB2","#EBCC2A"))+
  labs(x="Level of Racial Resentment",y="Predicted Probability of Approving Climate Policy")+
  plot_theme+
  scale_y_continuous(limits=c(0,1),breaks = c(0,.10,.20,.30,.40,.50,.60,.70,.80,.90,1))+
  scale_x_continuous(breaks = c(0,0.125,0.25,0.375,0.5,0.625,0.75,0.875,1))+
  #labs(caption='Predicted support for climate policy by level of racial resentment. \n Values of all other variables held at mode/mean. \n Estimates shown with 95% CI.')+
theme(plot.margin = unit(c(0,0,-3.5,0), "lines"))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.line.x=element_blank())+
  theme(legend.position = c(.87, .87))

#Adding histogram
#Graph: Distribution of RR
Dist_RR<- ggplot(RR, aes(x = Racial_resentment,y=Percentage,label=round(Percentage,digits=2))) +  
  geom_bar(stat="identity", position = position_dodge(width = 1)) +
  geom_bar(stat="identity", position = position_dodge(width = 1),fill = "lightgray") +
  plot_theme+
  scale_x_continuous(breaks = c(0,0.125,0.25,0.375,0.5,0.625,0.75,0.875,1))+
  labs(x="Racial Resentment",y=NULL,caption='Predicted support for climate policy by level of racial resentment. \n Values of all other variables held at mode/mean. \n Estimates shown with 95% CI. \n Distribution of racial resentment shown in histogram.')+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


#Combining into 1 with overlaid histogram to show dist of RR
Fig1_final<-plot_grid(Fig1, Dist_RR, ncol = 1, nrow = 2, align = "v", axis = "lr",rel_heights=c(2.5,1))

#Title= Overall Effect of Racial Resentment on Climate Policy Approval
#ggsave("Figure1.png",Fig1_final,width=11,height=7,units="in",dpi=300,bg = 'white')


#Min max values overall for in text discussion p.15.
Fig1_dataMM<-Fig1_data %>% 
  filter(Racial_resentment%in%c(0,1)) %>% 
  group_by(Policy) %>% 
  mutate(diff = (Fit - Fit[1])*100, 
         diff = replace(diff, row_number() == 1, NA))


#Table A4

#Adding in control variables for the main models
stargazer(main_model_cop_logit,main_model_cpp_logit,title="Overall Effect of Racial Resentment on Climate Policy Approval: With Controls Shown",digits=3,
          label="Full_results_controls",
          covariate.labels = c("\\textbf{Racial Resentment}",
                               "\\textbf{Education} (reference= Associate degree) &   \\\\
                      \\hspace{1cm}Advanced Degree",
                      "\\hspace{1cm}Bachelor's degree",
                      "\\hspace{1cm}Some college",
                      "\\hspace{1cm}High school graduate",
                      "\\hspace{1cm}No high school",
                      "\\textbf{Gender} (reference= Female) &   \\\\
                      \\hspace{1cm}Male",
                      "\\textbf{Region} (reference= Midwest) &   \\\\
                      \\hspace{1cm}Northeast",
                      "\\hspace{1cm}South",
                      "\\hspace{1cm}West",
                      "\\textbf{Income}",
                      "\\textbf{Age}",
                      "\\textbf{Political Ideology} (reference= Moderate) &   \\\\
                      \\hspace{1cm}Liberal",
                      "\\hspace{1cm}Conservative",
                      "\\textbf{Political Party (leaners inc.)} (reference= Independent) &   \\\\
                      \\hspace{1cm}Democrat",
                      "\\hspace{1cm}Republican"),
          notes="\\parbox[t]{12cm}{Coefficients reported from logit regression models. The dependent variables are coded 1 if the respondent indicated supporting the climate policy option and 0 if they opposed the climate policy option. Significance codes:{$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01}, two-tailed tests.}",
          notes.append=FALSE,
          style="apsr",
          dep.var.caption = c(""),
          dep.var.labels.include = FALSE,
          table.layout ="-c-!t-s-n",
          header=FALSE,
          column.labels = c("COP21", "CPP"),
          notes.align= "l",
          column.sep.width = "1pt",
          font.size = "scriptsize",
          no.space = TRUE)

#2.3 H3. The relationship between racial resentment and climate action will hold for individuals who identify with both the Republican and Democratic party.
##### Fig2: PID ##### 


#Number of min and max RR by party
RR_party<-pctgroup(Main_data_analysis,PID_leaners,Racial_resentment) %>% 
  filter(Racial_resentment%in%c(0,1)) %>% 
  mutate(Racial_resentment = recode(Racial_resentment,"1" = "Maximum",
                                    "0" = "Minimum"))  

print(xtable(RR_party,"Distribution of Racial Resentment Across Respondents (by party)"), include.rownames=FALSE)

#Creating Subsample- Democrats
Democrats<-Main_data_analysis %>% 
  filter(PID_leaners=="Democrat")

#Creating Subsample- independents
Independents<-Main_data_analysis %>% 
  filter(PID_leaners=="Independent")

#Creating Subsample- Republicans
Republicans<-Main_data_analysis %>% 
  filter(PID_leaners=="Republican")

#Dems
m1_dems<-glm(climate_COP~Racial_resentment+educ+gender+region+Income+Age+political_ideo_3, family=binomial(link="logit"), data = Democrats)

m2_dems<-glm(climate_CPP~Racial_resentment+educ+gender+region+Income+Age+political_ideo_3, family=binomial(link="logit"), data = Democrats)

summ(m1_dems,confint=TRUE)
# RR: -3.36   (95\% CI: -3.63,-3.08) p=0.00

summ(m2_dems,confint=TRUE)
# RR: -2.08(95\% CI: -2.27,-1.89) p=0.00

#Independents
m3_indeps<-glm(climate_COP~Racial_resentment+educ+gender+region+Income+Age+political_ideo_3, family=binomial(link="logit"), data = Independents)

m4_indeps<-glm(climate_CPP~Racial_resentment+educ+gender+region+Income+Age+political_ideo_3, family=binomial(link="logit"), data = Independents)

summ(m3_indeps,confint=TRUE)
# -3.40  (95\% CI: -3.69, -3.11 p=0.00

summ(m4_indeps,confint=TRUE)
# -2.42  (95\% CI: -2.68, -2.16) p=0.00

#Repubs
m5_repubs<-glm(climate_COP~Racial_resentment+educ+gender+region+Income+Age+political_ideo_3, family=binomial(link="logit"), data = Republicans)

m6_repubs<-glm(climate_CPP~Racial_resentment+educ+gender+region+Income+Age+political_ideo_3, family=binomial(link="logit"), data = Republicans)

summ(m5_repubs,confint=TRUE)
# -2.67 (95\% CI: -2.86, -2.48) 

summ(m6_repubs,confint=TRUE)
# -1.56 (95\% CI: -1.72,-1.40) 

#All p-values=0.00

#Table 3: Overall Effect of Racial Resentment on Climate Policy Approval (By Party)

stargazer(m1_dems,m2_dems,m3_indeps,m4_indeps,m5_repubs,m6_repubs,title="Overall Effect of Racial Resentment on Climate Policy Approval (By Party)",digits=3,keep.stat=c("n","rsq"),
          omit = c("Constant"),
          keep = c("Racial_resentment"), 
          label="Partyeffect_table",
          covariate.labels = c("Racial Resentment"),
          notes="\\parbox[t]{12cm}{Coefficients reported from logistical regression models. Models include control variables for education, political ideology, gender, age, region, income, and race. The dependent variables are coded 1 if the respondent indicated supporting the climate policy option and 0 if they opposed the climate policy option. Significance codes:{$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01}, two-tailed tests.}",
          notes.append=FALSE,
          style="apsr",
          dep.var.labels.include=TRUE,
          dep.var.labels = c("COP21", "CPP","COP21", "CPP","COP21", "CPP"),
          table.layout ="-cd-!t-sa-n",
          header=FALSE,
          model.numbers = TRUE,
          column.labels = c("Democrats","Independents", "Republicans"), 
          column.separate = c(2, 2, 2), 
          notes.align= "c",
          column.sep.width = "1pt",
          add.lines=list(c('Control Variables', 'Yes','Yes',"Yes","Yes","Yes","Yes")),
          no.space = TRUE)


#Displaying w/ controls

stargazer(m1_dems,m2_dems,m3_indeps,m4_indeps,m5_repubs,m6_repubs,title="Overall Effect of Racial Resentment on Climate Policy Approval (By Party): With Controls Shown",digits=3,keep.stat=c("n","rsq"),
          omit = c("Constant"),
          covariate.labels = c("\\textbf{Racial Resentment}",
                      "\\textbf{Education} (reference= Associate degree) &   \\\\
                      \\hspace{1cm}Advanced Degree",
                      "\\hspace{1cm}Bachelor's degree",
                      "\\hspace{1cm}Some college",
                      "\\hspace{1cm}High school graduate",
                      "\\hspace{1cm}No high school",
                      "\\textbf{Gender} (reference= Female) &   \\\\
                      \\hspace{1cm}Male",
                      "\\textbf{Region} (reference= Midwest) &   \\\\
                      \\hspace{1cm}Northeast",
                      "\\hspace{1cm}South",
                      "\\hspace{1cm}West",
                      "\\textbf{Income}",
                      "\\textbf{Age}",
                      "\\textbf{Political Ideology} (reference= Moderate) &   \\\\
                      \\hspace{1cm}Liberal",
                      "\\hspace{1cm}Conservative"),
          notes="\\parbox[t]{12cm}{Coefficients reported from logistical regression models. Models include control variables for education, political ideology, partisanship, gender, age, region, income, and race. The dependent variables are coded 1 if the respondent indicated supporting the climate policy option and 0 if they opposed the climate policy option. Significance codes:{$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01}, two-tailed tests.}",
          notes.append=FALSE,
          style="apsr",
          dep.var.labels.include=TRUE,
          dep.var.labels = c("COP21", "CPP","COP21", "CPP","COP21", "CPP"),
          table.layout ="-cd-!t-s-n",
          header=FALSE,
          model.numbers = TRUE,
          column.labels = c("Democrats","Independents", "Republicans"), 
          column.separate = c(2, 2, 2), 
          notes.align= "l",
          column.sep.width = "1pt",
          font.size = "scriptsize",
          no.space = TRUE)

#using jtools::make_predictions() to Generate predicted data for plotting results of regression models 
#Democrats

mode_func(Democrats$educ)
mode_func(Democrats$gender)
mode_func(Democrats$region)
mode_func(Democrats$political_ideo_3)
round(mean(Democrats$Income,na.rm=T),0)
round(mean(Democrats$Age,na.rm=T),0)

hypo_data_dems <- expand.grid(Racial_resentment=c(0,0.125,0.25,0.375,0.5,0.625,0.75,0.875,1),
                         educ="Bachelor's degree",
                         gender="Female",
                         region="South",
                         Income=7,
                         Age=49,
                         political_ideo_3="Liberal")


COP21_pred_dem <-make_predictions(m1_dems, pred = "Racial_resentment",interval = TRUE,new_data=hypo_data_dems)
COP21_pred_dem$Policy <- "COP21 (International)"
COP21_pred_dem$Fit<-COP21_pred_dem$climate_COP

CPP_pred_dem <- make_predictions(m2_dems, pred = "Racial_resentment",interval = TRUE,new_data=hypo_data_dems)
CPP_pred_dem$Policy <- "Clean Power Plan (Domestic)"
CPP_pred_dem$Fit<-CPP_pred_dem$climate_CPP

#Combining data (also for in-text)
Fig_dem <- bind_rows(COP21_pred_dem, CPP_pred_dem) %>% 
  dplyr::select(Fit,ymax,ymin,Racial_resentment,Policy)

Fig_dem$Policy <- factor(Fig_dem$Policy, levels = c("COP21 (International)", "Clean Power Plan (Domestic)"))
Fig_dem$Party<-"Democrats"


#Figure A1: Effect of Racial Resentment on Climate Policy Approval By Party Affiliation

#Graphing
Figure_dems<-ggplot(Fig_dem, aes(y=Fit,x=Racial_resentment)) +
  geom_line(aes(linetype=Policy,group=Policy,color=Policy),linewidth=1) +
  geom_ribbon(aes(group=Policy,ymin=ymin,ymax=ymax ,fill=Policy),alpha=0.5) +
  scale_color_manual(values=c("#3B9AB2","#EBCC2A"))+
  scale_fill_manual(values=c("#3B9AB2","#EBCC2A"))+
  #labs(x="Level of Racial Resentment",y="Predicted Probability of Approving Climate Policy")+
  plot_theme+
  scale_y_continuous(limits=c(0,1),breaks = c(.0,.10,.20,.30,.40,.50,.60,.70,.80,.90,1))+
  scale_x_continuous(breaks = c(0,0.125,0.25,0.375,0.5,0.625,0.75,0.875,1))+
  ggtitle("Democrats")+ 
  labs(x="Level of Racial Resentment",y="Predicted Probability of Approving Climate Policy")+
  theme(legend.position="none")+
theme(plot.margin = unit(c(0,0,-3.5,0), "lines"))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.line.x=element_blank())

#Adding histogram

#Graph: Distribution of RR for dems
RR_dems<-pctgroup(Democrats,Racial_resentment)

Dist_RR_dems<- ggplot(RR_dems, aes(x = Racial_resentment,y=Percentage,label=round(Percentage,digits=2))) +  
  geom_bar(stat="identity", position = position_dodge(width = 1)) +
  geom_bar(stat="identity", position = position_dodge(width = 1),fill = "lightgray") +
  plot_theme+
  scale_x_continuous(breaks = c(0,0.125,0.25,0.375,0.5,0.625,0.75,0.875,1))+
  labs(x="Level of Racial Resentment",y=NULL)+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

#Combining into 1 with overlaid histogram to show dist of RR
Figure_dems_final<-plot_grid(Figure_dems, Dist_RR_dems, ncol = 1, nrow = 2, align = "v", axis = "lr",rel_heights=c(2.5,1))

#indep

mode_func(Independents$educ)
mode_func(Independents$gender)
mode_func(Independents$region)
mode_func(Independents$political_ideo_3)
round(mean(Independents$Income,na.rm=T),0)
round(mean(Independents$Age,na.rm=T),0)

hypo_data_indep <- expand.grid(Racial_resentment=c(0,0.125,0.25,0.375,0.5,0.625,0.75,0.875,1),
                              educ="High school graduate",
                              gender="Female",
                              region="South",
                              Income=6,
                              Age=48,
                              political_ideo_3="Middle of the Road")

COP21_pred_indep <-make_predictions(m3_indeps, pred = "Racial_resentment",interval = TRUE,new_data=hypo_data_indep)
COP21_pred_indep$Policy <- "COP21 (International)"
COP21_pred_indep$Fit<-COP21_pred_indep$climate_COP

CPP_pred_indep <- make_predictions(m4_indeps, pred = "Racial_resentment",interval = TRUE,new_data=hypo_data_indep)
CPP_pred_indep$Policy <- "Clean Power Plan (Domestic)"
CPP_pred_indep$Fit<-CPP_pred_indep$climate_CPP

#Combining data
Fig_indeps <- bind_rows(COP21_pred_indep, CPP_pred_indep) %>% 
  dplyr::select(Fit,ymax,ymin,Racial_resentment,Policy)

Fig_indeps$Policy <- factor(Fig_indeps$Policy, levels = c("COP21 (International)", "Clean Power Plan (Domestic)"))
Fig_indeps$Party<-"Independents"

#Graphing
Figure_indeps<-ggplot(Fig_indeps, aes(y=Fit,x=Racial_resentment)) +
  geom_line(aes(linetype=Policy,group=Policy,color=Policy),linewidth=1) +
  geom_ribbon(aes(group=Policy,ymin=ymin,ymax=ymax ,fill=Policy),alpha=0.5) +
  scale_color_manual(values=c("#3B9AB2","#EBCC2A"))+
  scale_fill_manual(values=c("#3B9AB2","#EBCC2A"))+
  labs(x="Level of Racial Resentment",y="Predicted Probability of Approving Climate Policy")+
  plot_theme+
  scale_y_continuous(limits=c(0,1),breaks = c(.0,.10,.20,.30,.40,.50,.60,.70,.80,.90,1))+
  scale_x_continuous(breaks = c(0,0.125,0.25,0.375,0.5,0.625,0.75,0.875,1))+
  ggtitle("Independents")+
  theme(plot.margin = unit(c(0,0,-3.5,0), "lines"))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.line.x=element_blank())+
  theme(legend.position="")

#Adding histogram

#Graph: Distribution of RR for indeps
RR_indep<-pctgroup(Independents,Racial_resentment)

Dist_RR_indep<- ggplot(RR_indep, aes(x = Racial_resentment,y=Percentage,label=round(Percentage,digits=2))) +  
  geom_bar(stat="identity", position = position_dodge(width = 1)) +
  geom_bar(stat="identity", position = position_dodge(width = 1),fill = "lightgray") +
  plot_theme+
  scale_x_continuous(breaks = c(0,0.125,0.25,0.375,0.5,0.625,0.75,0.875,1))+
  labs(x="Level of Racial Resentment",y=NULL)+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


#Combining into 1 with overlaid histogram to show dist of RR
Figure_indeps_final<-plot_grid(Figure_indeps, Dist_RR_indep, ncol = 1, nrow = 2, align = "v", axis = "lr",rel_heights=c(2.5,1))

#repub

mode_func(Republicans$educ)
mode_func(Republicans$gender)
mode_func(Republicans$region)
mode_func(Republicans$political_ideo_3)
round(mean(Republicans$Income,na.rm=T),0)
round(mean(Republicans$Age,na.rm=T),0)

hypo_data_repubs <- expand.grid(Racial_resentment=c(0,0.125,0.25,0.375,0.5,0.625,0.75,0.875,1),
                               educ="High school graduate",
                               gender="Female",
                               region="South",
                               Income=7,
                               Age=54,
                               political_ideo_3="Conservative")


COP21_pred_repubs<-make_predictions(m5_repubs, pred = "Racial_resentment",interval = TRUE,new_data=hypo_data_repubs)
COP21_pred_repubs$Policy <- "COP21 (International)"
COP21_pred_repubs$Fit<-COP21_pred_repubs$climate_COP

CPP_pred_repubs<- make_predictions(m6_repubs, pred = "Racial_resentment",interval = TRUE,new_data=hypo_data_repubs)
CPP_pred_repubs$Policy<- "Clean Power Plan (Domestic)"
CPP_pred_repubs$Fit<-CPP_pred_repubs$climate_CPP

#Combining data
Fig_repubs<- bind_rows(COP21_pred_repubs, CPP_pred_repubs) %>% 
  dplyr::select(Fit,ymax,ymin,Racial_resentment,Policy)

Fig_repubs$Policy <- factor(Fig_repubs$Policy, levels = c("COP21 (International)", "Clean Power Plan (Domestic)"))
Fig_repubs$Party<-"Republicans"

#Graphing
Figure_repubs<-ggplot(Fig_repubs, aes(y=Fit,x=Racial_resentment)) +
  geom_line(aes(linetype=Policy,group=Policy,color=Policy),linewidth=1) +
  geom_ribbon(aes(group=Policy,ymin=ymin,ymax=ymax ,fill=Policy),alpha=0.5) +
  scale_color_manual(values=c("#3B9AB2","#EBCC2A"))+
  scale_fill_manual(values=c("#3B9AB2","#EBCC2A"))+
  labs(x="Level of Racial Resentment",y="Predicted Probability of Approving Climate Policy")+
  plot_theme+
  scale_y_continuous(limits=c(0,1),breaks = c(.0,.10,.20,.30,.40,.50,.60,.70,.80,.90,1))+
  scale_x_continuous(breaks = c(0,0.125,0.25,0.375,0.5,0.625,0.75,0.875,1))+
  ggtitle("Republicans")+ 
  theme(legend.position = c(.7, .9))+
  theme(plot.margin = unit(c(0,0,-3.5,0), "lines"))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.line.x=element_blank())

#Adding histogram

#Graph: Distribution of RR for repub
RR_repub<-pctgroup(Republicans,Racial_resentment)

Dist_RR_repub<- ggplot(RR_repub, aes(x = Racial_resentment,y=Percentage,label=round(Percentage,digits=2))) +  
  geom_bar(stat="identity", position = position_dodge(width = 1)) +
  geom_bar(stat="identity", position = position_dodge(width = 1),fill = "lightgray") +
  plot_theme+
  scale_x_continuous(breaks = c(0,0.125,0.25,0.375,0.5,0.625,0.75,0.875,1))+
  labs(x="Level of Racial Resentment",y=NULL)+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

#Combining into 1 with overlaid histogram to show dist of RR
Figure_repub_final<-plot_grid(Figure_repubs, Dist_RR_repub, ncol = 1, nrow = 2, align = "v", axis = "lr",rel_heights=c(2.5,1))


#Combining plot with patchwork
Party_plot<-Figure_dems_final+Figure_indeps_final+Figure_repub_final

#Adding caption
  Party_plot<- Party_plot+plot_annotation(
    caption = 'Predicted support for climate policy by level of racial resentment. \n Values of all other variables held at party mode/mean. \n Estimates shown with 95% CI. \n Distribution of racial resentment by party shown in histogram.'
  )
  

#Adding annotations
Party_plot<-Party_plot + plot_annotation(tag_levels = 'a')

Party_plot<-Party_plot + plot_annotation(tag_levels = list(c("(a)","(b)","(c)")))& 
  theme(plot.tag.position = c(0, 1),
        plot.tag = element_text(hjust = -.5, vjust = 1.2))

Party_plot

#Title=Effect of Racial Resentment on Climate Policy Approval  w/ 95\% CI By Party Affiliation
#ggsave("Party_plot_pres_CES.png",Party_plot,width=12,height=7,units="in",dpi=300,bg = 'white')

#Min max values by party
Party_min_max<-rbind(Fig_dem,Fig_indeps,Fig_repubs)%>% 
  filter(Racial_resentment%in%c(0,1))

#Percentage point reductions in support (in-text)
diff_values<-Party_min_max %>% 
        group_by(Policy,Party) %>% 
  mutate(diff = (Fit - Fit[1])*100, 
         diff = replace(diff, row_number() == 1, NA))



# C. Appendix Analysis ----------------------------------------------------

#Table A2: Survey Demographic Information

demographics <- dplyr::select(Main_data_analysis,Education=educ,Race=race,Gender=gender,Income=Income_cat,Region=region,'Political Ideology'=political_ideo_3,Party=PID_leaners) %>%
  pivot_longer(everything(),names_to = "Variable",values_to = "Value") %>%
  pctgroup(Variable,Value)%>% 
  mutate(Value = factor(Value, levels=c("Advanced Degree", "Bachelor's degree", "Associate degree","Some college", "High school graduate","No high school",
                                        "Female","Male",
                                        "More than $150,000","$100,000--$149,999","$60,000--$99,999","$30,0000--$59,999","Up to $29,999",
                                        "Independent","Democrat","Republican",
                                        "Conservative","Middle of the Road","Liberal",
                                        "Strongly approve","Somewhat approve","Somewhat disapprove","Strongly disapprove",
                                        "Asian","Black","Hispanic","Other","White",
                                        "Midwest","Northeast","South","West"))) %>% 
  arrange(Variable,Value)

print(xtable(demographics,"Survey Demographics"), include.rownames=FALSE)

demographics_age <-  dplyr::select(Main_data_analysis,Age) %>% 
  summarise(Mean_age=mean(Age))
#Mean_age= 50.4


#Confirmatory factor analysis: RR
RR_item<-Main_data_analysis %>%
  dplyr::select(RR_trad_no_favors,RR_trad_slavery)

scale.model_RR <- 'f1 =~a*RR_trad_no_favors + a*RR_trad_slavery'

fit.cat_RR <- cfa(scale.model_RR, data=RR_item, mimic =c("MPlus"), std.lv = TRUE,ordered=TRUE)

#standardized factor loadings and their standard errors for the items
#"they indicate how much scores on an item change with a one-unit change in the latent factor"
parameterEstimates(fit.cat_RR, standardized=TRUE) %>%
  filter(op == "=~") %>%
  dplyr::select(Item=rhs, Loading=est, ci.lower, ci.upper, SE=se, Z=z, 'p-value'=pvalue)


##### A.1.2 Study 1: Robustness checks ##### 

#Different specifications of Racial resentment IV
#1. Do you agree or disagree...White people in the U.S. have certain advantages because of the color of their skin.
#2. Racial problems in the U.S. are rare, isolated situations.

#Each FIRE question individually

#COP21 DV whites_advantage
main_model_cop_FIRE_1<-glm(climate_COP~RR_full_whites_advantage+educ+gender+region+Income+Age+political_ideo_3+PID_leaners, family=binomial(link="logit"), data = Main_data_analysis)

#CPP DV whites_advantage
main_model_cpp_FIRE_1<-glm(climate_CPP~RR_full_whites_advantage+educ+gender+region+Income+Age+political_ideo_3+PID_leaners, family=binomial(link="logit"), data = Main_data_analysis)

#COP21 DV racialprobs_rare
main_model_cop_FIRE_2<-glm(climate_COP~RR_full_racialprobs_rare+educ+gender+region+Income+Age+political_ideo_3+PID_leaners, family=binomial(link="logit"), data = Main_data_analysis)


main_model_cpp_FIRE_2<-glm(climate_CPP~RR_full_racialprobs_rare+educ+gender+region+Income+Age+political_ideo_3+PID_leaners, family=binomial(link="logit"), data = Main_data_analysis)

#Table A8: Effect of FIRE Questions on Climate Policy Approval

stargazer(main_model_cop_FIRE_1,main_model_cpp_FIRE_1,main_model_cop_FIRE_2,main_model_cpp_FIRE_2,title="Effect of FIRE Questions Individually on Climate Policy Approval: With Controls Shown",digits=3,keep.stat=c("n","rsq"),
          omit = c("Constant"),
          label="FIRE_indv",
          covariate.labels = c("\\textbf{White people in the U.S. have advantages}",
                               "\\textbf{Racial Problems are rare}",
                               "\\textbf{Education} (reference= Associate degree) &   \\\\
                      \\hspace{1cm}Advanced Degree",
                      "\\hspace{1cm}Bachelor's degree",
                      "\\hspace{1cm}Some college",
                      "\\hspace{1cm}High school graduate",
                      "\\hspace{1cm}No high school",
                      "\\textbf{Gender} (reference= Female) &   \\\\
                      \\hspace{1cm}Male",
                      "\\textbf{Region} (reference= Midwest) &   \\\\
                      \\hspace{1cm}Northeast",
                      "\\hspace{1cm}South",
                      "\\hspace{1cm}West",
                      "\\textbf{Income}",
                      "\\textbf{Age}",
                      "\\textbf{Political Ideology} (reference= Moderate) &   \\\\
                      \\hspace{1cm}Liberal",
                      "\\hspace{1cm}Conservative",
                      "\\textbf{Political Party (leaners inc.)} (reference= Independent) &   \\\\
                      \\hspace{1cm}Democrat",
                      "\\hspace{1cm}Republican"),
          notes="\\parbox[t]{12cm}{Coefficients reported from logistical regression models. The dependent variables are coded 1 if the respondent indicated supporting the climate policy option and 0 if they opposed the climate policy option. Significance codes:{$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01}, two-tailed tests.}",
          notes.append=FALSE,
          style="apsr",
          dep.var.labels.include=TRUE,
          dep.var.labels = c("COP21", "CPP","COP21", "CPP"),
          table.layout ="-cd-!t-s-n",
          header=FALSE,
          model.numbers = TRUE,
          column.labels = c("White people in the U.S. have advantages", "Racial Problems are rare"),
          column.separate = c(2, 2), 
          notes.align= "c",
          font.size = "scriptsize",
          no.space = TRUE)



#Climate proposals robustness check w/ same controls as before

#regulate_CO2
regulate_CO2<-glm(regulate_CO2~Racial_resentment+educ+gender+region+Income+Age+political_ideo_3+PID_leaners, family=binomial(link="logit"), data = Main_data_analysis)

#renewable_fuels
renewable_fuels<-glm(renewable_fuels~Racial_resentment+educ+gender+region+Income+Age+political_ideo_3+PID_leaners, family=binomial(link="logit"), data = Main_data_analysis)

#strengthen_EPA
strengthen_EPA<-glm(strengthen_EPA~Racial_resentment+educ+gender+region+Income+Age+political_ideo_3+PID_leaners, family=binomial(link="logit"), data = Main_data_analysis)

#raise_fuelefficiency
raise_fuelefficiency<-glm(raise_fuelefficiency~Racial_resentment+educ+gender+region+Income+Age+political_ideo_3+PID_leaners, family=binomial(link="logit"), data = Main_data_analysis)


#Table A10: Effect of Racial Resentment on Climate Proposal Approval
stargazer(regulate_CO2,renewable_fuels,strengthen_EPA,raise_fuelefficiency,title="Overall Effect of Racial Resentment on Climate Proposal Approval: With Controls Shown",digits=3,keep.stat=c("n","rsq"),
          omit = c("Constant"),
          label = "Proposals_results_control",
          covariate.labels = c("\\textbf{Racial Resentment}",
                               "\\textbf{Education} (reference= Associate degree) &   \\\\
                      \\hspace{1cm}Advanced Degree",
                      "\\hspace{1cm}Bachelor's degree",
                      "\\hspace{1cm}Some college",
                      "\\hspace{1cm}High school graduate",
                      "\\hspace{1cm}No high school",
                      "\\textbf{Gender} (reference= Female) &   \\\\
                      \\hspace{1cm}Male",
                      "\\textbf{Region} (reference= Midwest) &   \\\\
                      \\hspace{1cm}Northeast",
                      "\\hspace{1cm}South",
                      "\\hspace{1cm}West",
                      "\\textbf{Income}",
                      "\\textbf{Age}",
                      "\\textbf{Political Ideology} (reference= Moderate) &   \\\\
                      \\hspace{1cm}Liberal",
                      "\\hspace{1cm}Conservative",
                      "\\textbf{Political Party (leaners inc.)} (reference= Independent) &   \\\\
                      \\hspace{1cm}Democrat",
                      "\\hspace{1cm}Republican"),
          notes="\\parbox[t]{12cm}{Coefficients reported from logistical regression models. The dependent variables are coded 1 if the respondent indicated supporting the climate policy option and 0 if they opposed the climate policy option. Significance codes:{$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01}, two-tailed tests.}",
          notes.append=FALSE,
          style="apsr",
          dep.var.caption = c(""),
          dep.var.labels.include = FALSE,
          table.layout ="-c-!t-s-n",
          header=FALSE,
          column.labels = c("Regulate CO2", "Renewable Fuels", "Strengthen EPA", "Raise Fuel Efficiency"),
          notes.align= "l",
          column.sep.width = "1pt",
          font.size = "tiny",
          no.space = TRUE)




##### Location analysis ##### 
# File contains regressions incorporating location-based information
# Models based on rural/non-rural
# Regressions with Bureau of Labor Statistics data for industrial employment

pacman::p_load(zipcodeR, # crosswalk for zip codes to other units
               lme4, # multilevel models
               tigris, # US maps
               crosswalkr,
               texreg)

# Getting zip code crosswalk
zips <- zip_code_db |> dplyr::select("zipcode", "state", "major_city", "county")
data(stcrosswalk)
zips <- left_join(zips, stcrosswalk[, c("stabbr", "stname")], by = c("state" = "stabbr"))
zips$zipcode<-as.numeric(zips$zipcode)
# Merging with maindata
Main_data2 <- left_join(Main_data_analysis, zips, by = c("zip" = "zipcode"))


#Table A11: Effect of Racial Resentment: Industry-Based Analysis

# BLS County-HL Industry from 2020 ----
bls_hl <- import("Data/allhlcn20.xlsx")
bls_hl$wage_quotient <- bls_hl$`Total Wage Location Quotient Relative to U.S.`
bls_hl$employ_quotient <- bls_hl$`Employment Location Quotient Relative to U.S.`
bls_hl$state <- bls_hl$`St Name`

## Natural resources ----
# Filtering to just natural resources
bls_county_hl <- bls_hl |>
  dplyr::filter(`Area Type` == "County") |>
  dplyr::mutate(county = str_split_i(Area, pattern = ", ", 1)) |>
  dplyr::filter(NAICS == 1011) |>
  dplyr::select(county, state, wage_quotient, employ_quotient)

# Merging with the data
Main_data_bls <- left_join(Main_data2, bls_county_hl, by = c("county", "stname" = "state"))

# Linear regressions interacted with industrial indicators
# Creating function to output the formula
fun.bls.form <- function(x) {
  line <- c("climate_COP ~ ", "Racial_resentment+", paste(x), " + educ+gender+region+Income+Age+political_ideo_3+PID_leaners")
  form <- formula(paste(line, collapse = " "))
  return(form)
}


fun.bls.form_CPP <- function(x) {
  line <- c("climate_CPP ~ ", "Racial_resentment+", paste(x), " + educ+gender+region+Income+Age+political_ideo_3+PID_leaners")
  form <- formula(paste(line, collapse = " "))
  return(form)
}

# Employment quotient
#COP21
model_county_nr_employ <-glm(fun.bls.form("employ_quotient"),
                             family=binomial(link="logit"), data = Main_data_bls)

summary(model_county_nr_employ)

#CPP
model_county_nr_employ_CPP <-glm(fun.bls.form_CPP("employ_quotient"),
                                 family=binomial(link="logit"), data = Main_data_bls)

summary(model_county_nr_employ_CPP)

# Wage quotient
#CPP
model_county_nr_wage <-glm(fun.bls.form("wage_quotient"),
                           family=binomial(link="logit"), data = Main_data_bls)
summary(model_county_nr_wage)

#CPP
model_county_nr_wage_CPP <-glm(fun.bls.form_CPP("wage_quotient"),
                               family=binomial(link="logit"), data = Main_data_bls)
summary(model_county_nr_wage_CPP)

#Controlling for both employment or  wage quotient in natural resources the negative effect of RR remains persistently negative and statistically and substantively meaningful. 

#Putting it all in a table
stargazer(model_county_nr_employ,model_county_nr_employ_CPP,model_county_nr_wage,model_county_nr_wage_CPP,title="Effect of Racial Resentment: Location-Based Analysis",digits=3,keep.stat=c("n","rsq"),
          omit = c("Constant"),
          #keep = c("Racial_resentment","employ_quotient","wage_quotient"), 
          label="location_Table",
          covariate.labels = c("Racial Resentment","Employment quotient", "Wage quotient",                               "\\textbf{Education} (reference= Associate degree) &   \\\\
                      \\hspace{1cm}Advanced Degree",
                      "\\hspace{1cm}Bachelor's degree",
                      "\\hspace{1cm}Some college",
                      "\\hspace{1cm}High school graduate",
                      "\\hspace{1cm}No high school",
                      "\\textbf{Gender} (reference= Female) &   \\\\
                      \\hspace{1cm}Male",
                      "\\textbf{Region} (reference= Midwest) &   \\\\
                      \\hspace{1cm}Northeast",
                      "\\hspace{1cm}South",
                      "\\hspace{1cm}West",
                      "\\textbf{Income}",
                      "\\textbf{Age}",
                      "\\textbf{Political Ideology} (reference= Moderate) &   \\\\
                      \\hspace{1cm}Liberal",
                      "\\hspace{1cm}Conservative",
                      "\\textbf{Political Party (leaners inc.)} (reference= Independent) &   \\\\
                      \\hspace{1cm}Democrat",
                      "\\hspace{1cm}Republican"),
          notes="\\parbox[t]{8cm}{Coefficients reported from logistical regression models. Models include control variables for education, political ideology, gender, age, region, income, and race. The dependent variables are coded 1 if the respondent indicated supporting the climate policy option and 0 if they opposed the climate policy option. Significance codes:{$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01}, two-tailed tests.}",
          notes.append=FALSE,
          style="apsr",
          dep.var.labels.include=TRUE,
          dep.var.labels = c("COP21", "CPP","COP21", "CPP"),
          table.layout ="-cd-!t-sa-n",
          header=FALSE,
          model.numbers = TRUE,
          column.labels = c("Employment quotient", "Wage quotient"),
          column.separate = c(2, 2), 
          notes.align= "c",
          column.sep.width = "1pt",
          no.space = TRUE)


#Table A12: Effect of Racial Resentment on Climate Policy Approval (By Rural)

COP_rural<-glm(climate_COP~Racial_resentment+region+educ+gender+Income+Age+political_ideo_3+PID_leaners, family=binomial(link="logit"), data = Main_data_analysis %>% filter(rural=="Yes"))

COP_non_rural<-glm(climate_COP~Racial_resentment+region+educ+gender+Income+Age+political_ideo_3+PID_leaners, family=binomial(link="logit"), data = Main_data_analysis %>% filter(rural=="No"))

#CPP DV logit
CPP_rural<-glm(climate_CPP~Racial_resentment+region+educ+gender+Income+Age+political_ideo_3+PID_leaners, family=binomial(link="logit"),data = Main_data_analysis %>% filter(rural=="Yes"))

CPP_non_rural<-glm(climate_CPP~Racial_resentment+region+educ+gender+Income+Age+political_ideo_3+PID_leaners, family=binomial(link="logit"), data = Main_data_analysis %>% filter(rural=="No"))

#Putting it all in a table
stargazer(COP_non_rural,CPP_non_rural,COP_rural,CPP_rural,title="Overall Effect of Racial Resentment on Climate Policy Approval (By Rural)",digits=3,keep.stat=c("n","rsq"),
          omit = c("Constant"),
         # keep = c("Racial_resentment"), 
          label="Rural_table",
          covariate.labels = c("\\textbf{Racial Resentment}",
                               "\\textbf{Education} (reference= Associate degree) &   \\\\
                      \\hspace{1cm}Advanced Degree",
                      "\\hspace{1cm}Bachelor's degree",
                      "\\hspace{1cm}Some college",
                      "\\hspace{1cm}High school graduate",
                      "\\hspace{1cm}No high school",
                      "\\textbf{Gender} (reference= Female) &   \\\\
                      \\hspace{1cm}Male",
                      "\\textbf{Region} (reference= Midwest) &   \\\\
                      \\hspace{1cm}Northeast",
                      "\\hspace{1cm}South",
                      "\\hspace{1cm}West",
                      "\\textbf{Income}",
                      "\\textbf{Age}",
                      "\\textbf{Political Ideology} (reference= Moderate) &   \\\\
                      \\hspace{1cm}Liberal",
                      "\\hspace{1cm}Conservative",
                      "\\textbf{Political Party (leaners inc.)} (reference= Independent) &   \\\\
                      \\hspace{1cm}Democrat",
                      "\\hspace{1cm}Republican"),
          notes="\\parbox[t]{8cm}{Coefficients reported from logistical regression models. Models include control variables for education, political ideology, gender, age, region, income, and race. The dependent variables are coded 1 if the respondent indicated supporting the climate policy option and 0 if they opposed the climate policy option. Significance codes:{$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01}, two-tailed tests.}",
          notes.append=FALSE,
          style="apsr",
          dep.var.labels.include=TRUE,
          dep.var.labels = c("COP21", "CPP","COP21", "CPP"),
          table.layout ="-cd-!t-sa-n",
          header=FALSE,
          model.numbers = TRUE,
          column.labels = c("Non-Rural", "Rural"),
          column.separate = c(2, 2), 
          notes.align= "c",
          column.sep.width = "1pt",
          no.space = TRUE)

#Table A21: CES Effect of Racial Resentment on Climate Policy Approval: Wall Control

  ## Nationalism proxy ----
  
# including support for building a wall
main_model_cop_logit_wall<-glm(climate_COP~Racial_resentment+educ+gender+region+Income+Age+political_ideo_3+PID_leaners+Mexico_wall, family=binomial(link="logit"), data = Main_data_analysis)

#CPP DV logit
main_model_cpp_logit_wall<-glm(climate_CPP~Racial_resentment+educ+gender+region+Income+Age+political_ideo_3+PID_leaners+Mexico_wall, family=binomial(link="logit"), data = Main_data_analysis)



#Adding in control variables for the main models
stargazer(main_model_cop_logit_wall,main_model_cpp_logit_wall,title="Effect of Racial Resentment on Climate Policy Approval: Wall Control",digits=3,keep.stat=c("n","rsq"),
          omit = c("Constant"),
          label="Wall_results_controls",
          covariate.labels = c("\\textbf{Racial Resentment}",
                               "\\textbf{Education} (reference= Associate degree) &   \\\\
                      \\hspace{1cm}Advanced Degree",
                      "\\hspace{1cm}Bachelor's degree",
                      "\\hspace{1cm}Some college",
                      "\\hspace{1cm}High school graduate",
                      "\\hspace{1cm}No high school",
                      "\\textbf{Gender} (reference= Female) &   \\\\
                      \\hspace{1cm}Male",
                      "\\textbf{Region} (reference= Midwest) &   \\\\
                      \\hspace{1cm}Northeast",
                      "\\hspace{1cm}South",
                      "\\hspace{1cm}West",
                      "\\textbf{Income}",
                      "\\textbf{Age}",
                      "\\textbf{Political Ideology} (reference= Moderate) &   \\\\
                      \\hspace{1cm}Liberal",
                      "\\hspace{1cm}Conservative",
                      "\\textbf{Political Party (leaners inc.)} (reference= Independent) &   \\\\
                      \\hspace{1cm}Democrat",
                      "\\hspace{1cm}Republican",
                      "\\textbf{Support Wall} (reference= Oppose)"),
          notes="\\parbox[t]{12cm}{Coefficients reported from logit regression models. The dependent variables are coded 1 if the respondent indicated supporting the climate policy option and 0 if they opposed the climate policy option. Significance codes:{$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01}, two-tailed tests.}",
          notes.append=FALSE,
          style="apsr",
          dep.var.caption = c(""),
          dep.var.labels.include = FALSE,
          table.layout ="-c-!t-s-n",
          header=FALSE,
          column.labels = c("COP21", "CPP"),
          notes.align= "l",
          column.sep.width = "1pt",
          font.size = "scriptsize",
          no.space = TRUE)


## Sensitivity Analysis ----

# Linear probability model (for sensemakr compatibility)
sense_model_COP21 <- lm(climate_COP ~ Racial_resentment + educ + gender + region +
                          Income + Age + political_ideo_3 + PID_leaners,
                        data = Main_data_analysis)

# Summarize model
summary(sense_model_COP21)

# Conduct sensitivity analysis
sense_COP <- sensemakr(model = sense_model_COP21,
                       treatment = "Racial_resentment",
                       benchmark_covariates = "PID_leanersRepublican",
                       kd = 1:3)


ovb_minimal_reporting(sense_COP)
#	An unobserved confounder would need to explain 24.5% of the residual variation in both the treatment (Racial_resentment) and the outcome (climate_COP) to reduce the estimate to zero.

#It would take a confounder about 13–16x stronger than Republican partisanship to eliminate this effect


# Linear probability model (for sensemakr compatibility)
sense_model_CPP <- lm(climate_CPP ~ Racial_resentment + educ + gender + region +
                          Income + Age + political_ideo_3 + PID_leaners,
                        data = Main_data_analysis)

# Summarize model
summary(sense_model_CPP)

# Conduct sensitivity analysis
sense_CPP<- sensemakr(model = sense_model_CPP,
                       treatment = "Racial_resentment",
                       benchmark_covariates = "PID_leanersRepublican",
                       kd = 1:3)

ovb_minimal_reporting(sense_CPP)
#unobserved confounder would need to explain over 18% of the residual variation in both racial resentment and support for the CPP to render this effect statistically insignificant
#An unmeasured confounder would need to be over 10× as strong as Republican partisanship in predicting both racial resentment and CPP support to explain away the effect



## Additional CES years 2016-2018 ----
#Loading in the data file, this assumes the datafile is saved in the same working directory as the R code file.
CCES_2016_2018_analysis <- import("data/CCES_2016_2018.rds")

#Table A13: 2016: Effect of Racial Resentment on Climate Proposal Approval

#regulate_CO2
regulate_CO2_16<-glm(regulate_CO2~Racial_resentment+educ+gender+region+Income+Age+political_ideo_3+PID_leaners, family=binomial(link="logit"), data = CCES_2016_2018_analysis %>% filter(year==2016))

#renewable_fuels
renewable_fuels_16<-glm(renewable_fuels~Racial_resentment+educ+gender+region+Income+Age+political_ideo_3+PID_leaners, family=binomial(link="logit"), data = CCES_2016_2018_analysis %>% filter(year==2016))

#strengthen_EPA
strengthen_EPA_16<-glm(strengthen_EPA~Racial_resentment+educ+gender+region+Income+Age+political_ideo_3+PID_leaners, family=binomial(link="logit"), data = CCES_2016_2018_analysis %>% filter(year==2016))

#raise_fuelefficiency
raise_fuelefficiency_16<-glm(raise_fuelefficiency~Racial_resentment+educ+gender+region+Income+Age+political_ideo_3+PID_leaners, family=binomial(link="logit"), data = CCES_2016_2018_analysis %>% filter(year==2016))


stargazer(regulate_CO2_16,renewable_fuels_16,strengthen_EPA_16,raise_fuelefficiency_16,title="2016:Effect of Racial Resentment on Climate Proposal Approval: With Controls Shown",digits=3,keep.stat=c("n","rsq"),
          omit = c("Constant"),
          label = "CES:2016",
          covariate.labels = c("\\textbf{Racial Resentment}",
                               "\\textbf{Education} (reference= Associate degree) &   \\\\
                      \\hspace{1cm}Advanced Degree",
                      "\\hspace{1cm}Bachelor's degree",
                      "\\hspace{1cm}Some college",
                      "\\hspace{1cm}High school graduate",
                      "\\hspace{1cm}No high school",
                      "\\textbf{Gender} (reference= Female) &   \\\\
                      \\hspace{1cm}Male",
                      "\\textbf{Region} (reference= Midwest) &   \\\\
                      \\hspace{1cm}Northeast",
                      "\\hspace{1cm}South",
                      "\\hspace{1cm}West",
                      "\\textbf{Income}",
                      "\\textbf{Age}",
                      "\\textbf{Political Ideology} (reference= Moderate) &   \\\\
                      \\hspace{1cm}Liberal",
                      "\\hspace{1cm}Conservative",
                      "\\textbf{Political Party (leaners inc.)} (reference= Independent) &   \\\\
                      \\hspace{1cm}Democrat",
                      "\\hspace{1cm}Republican"),
          notes="\\parbox[t]{12cm}{Coefficients reported from logistical regression models. Models include control variables for education, political ideology, gender, age, region, income, and race. The dependent variables are coded 1 if the respondent indicated supporting the climate policy option and 0 if they opposed the climate policy option. Significance codes:{$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01}, two-tailed tests.}",
          notes.append=FALSE,
          style="apsr",
          dep.var.labels.include=FALSE,
          dep.var.caption = "",
          table.layout ="-c-!t-sa-n",
          header=FALSE,
          model.numbers = TRUE,
          column.labels = c("Regulate CO2", "Renewable Fuels", "Strengthen EPA", "Raise Fuel Efficiency"),
          notes.align= "c",
          column.sep.width = "1pt",
          no.space = TRUE)

#Table A14: 2018: Effect of Racial Resentment on Climate Proposal Approval
#regulate_CO2
regulate_CO2_18<-glm(regulate_CO2~Racial_resentment+educ+gender+region+Income+Age+political_ideo_3+PID_leaners, family=binomial(link="logit"), data = CCES_2016_2018_analysis %>% filter(year==2018))

#renewable_fuels
renewable_fuels_18<-glm(renewable_fuels~Racial_resentment+educ+gender+region+Income+Age+political_ideo_3+PID_leaners, family=binomial(link="logit"), data = CCES_2016_2018_analysis %>% filter(year==2018))

#strengthen_EPA
strengthen_EPA_18<-glm(strengthen_EPA~Racial_resentment+educ+gender+region+Income+Age+political_ideo_3+PID_leaners, family=binomial(link="logit"), data = CCES_2016_2018_analysis %>% filter(year==2018))

#raise_fuelefficiency
raise_fuelefficiency_18<-glm(raise_fuelefficiency~Racial_resentment+educ+gender+region+Income+Age+political_ideo_3+PID_leaners, family=binomial(link="logit"), data = CCES_2016_2018_analysis %>% filter(year==2018))



stargazer(regulate_CO2_18,renewable_fuels_18,strengthen_EPA_18,raise_fuelefficiency_18,title="2018:Effect of Racial Resentment on Climate Proposal Approval: With Controls Shown",digits=3,keep.stat=c("n","rsq"),
          omit = c("Constant"),
          label = "CES:2018",
          covariate.labels = c("\\textbf{Racial Resentment}",
                               "\\textbf{Education} (reference= Associate degree) &   \\\\
                      \\hspace{1cm}Advanced Degree",
                      "\\hspace{1cm}Bachelor's degree",
                      "\\hspace{1cm}Some college",
                      "\\hspace{1cm}High school graduate",
                      "\\hspace{1cm}No high school",
                      "\\textbf{Gender} (reference= Female) &   \\\\
                      \\hspace{1cm}Male",
                      "\\textbf{Region} (reference= Midwest) &   \\\\
                      \\hspace{1cm}Northeast",
                      "\\hspace{1cm}South",
                      "\\hspace{1cm}West",
                      "\\textbf{Income}",
                      "\\textbf{Age}",
                      "\\textbf{Political Ideology} (reference= Moderate) &   \\\\
                      \\hspace{1cm}Liberal",
                      "\\hspace{1cm}Conservative",
                      "\\textbf{Political Party (leaners inc.)} (reference= Independent) &   \\\\
                      \\hspace{1cm}Democrat",
                      "\\hspace{1cm}Republican"),
          notes="\\parbox[t]{12cm}{Coefficients reported from logistical regression models. Models include control variables for education, political ideology, gender, age, region, income, and race. The dependent variables are coded 1 if the respondent indicated supporting the climate policy option and 0 if they opposed the climate policy option. Significance codes:{$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01}, two-tailed tests.}",
          notes.append=FALSE,
          style="apsr",
          dep.var.labels.include=FALSE,
          dep.var.caption = "",
          table.layout ="-c-!t-sa-n",
          header=FALSE,
          model.numbers = TRUE,
          column.labels = c("Regulate CO2", "Renewable Fuels", "Strengthen EPA", "Raise Fuel Efficiency"),
          notes.align= "c",
          column.sep.width = "1pt",
          no.space = TRUE)

