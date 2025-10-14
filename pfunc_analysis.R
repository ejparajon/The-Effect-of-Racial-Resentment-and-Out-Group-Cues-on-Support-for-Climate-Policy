# Preamble ----------------------------------------------------------------

#Clear workspace and setting seed for any randomization
rm(list=ls())

set.seed(101)


#The pacman package is required to load/install the additional packages.

if (!require("pacman")) install.packages("pacman")

#Installing and loading packages for use
pacman::p_load(broom,stargazer,rio,tidyverse,interplot,patchwork,xtable,modelsummary,naniar,estimatr,jtools,sandwich,regrrr,marginaleffects,psych,cowplot,janitor,lavaan,sensemakr,cobalt)


#Loading in the data file, this assumes the datafile is saved in the same working directory as the R code file.
Main_data_analysis<- readRDS("data/experiment_dat_clean.rds")

# Calling custom functions and ggplot theme
source("custom_functions.R")


#B1. Description of DV and key IV

#treatment distribution (3x2)
Main_data_analysis %>% tabyl(frame)
Main_data_analysis %>% tabyl(scope)
Main_data_analysis %>% tabyl(frame,scope)


#B2. Hypothesis testing

#Mean DV across frame conditions
treat_means <- Main_data_analysis %>% dplyr::select(dv, frame) %>% 
  filter(!is.na(frame)) %>%
  filter(!is.na(dv)) %>%
  group_by(frame) %>% 
  summarize(mean=mean(dv), sd=sd(dv), n=n(), se=sd/sqrt(n)) %>% 
  mutate(upper95CI = mean+1.96*se, lower95CI =mean-1.96*se) %>% 
  mutate(frame = factor(frame, levels=c("benefit","harm","control"))) %>% 
  dplyr::select(frame,"mean(0-4)"=mean,lower95CI,upper95CI,n)

print(xtable(treat_means,"Mean DV across frame treatment conditions"), include.rownames=T)

#Ceiling effect exploration
support<-Main_data_analysis %>% dplyr::select(dv, frame)
ceiling<-pctgroup(support,frame,dv)


##### H2: Regardless of scope did the treatment have an effect? ##### 

#H2a:White respondents will be less likely to support climate action that is framed at benefiting non-whites
##H2b:  White respondents will be less likely to support domestic climate action that is framed at addressing harm to non-white people.

H2_mod<-lm(dv ~ frame, data = Main_data_analysis)

summary(H2_mod)
#Number of observations for N
nobs(H2_mod)

H2_base <- avg_slopes(H2_mod,variable="frame")

H2_base$contrast[H2_base$contrast == "benefit - control"] <- "Benefit"
H2_base$contrast[H2_base$contrast == "harm - control"] <- "Harm"

#Selecting contrasts of interest to plot
H2_base<-H2_base %>% 
  filter(contrast%in%c("Benefit","Harm")) %>% 
  mutate(lower90=estimate-1.645*std.error) %>% 
  mutate(upper90=estimate+1.645*std.error) 

#"Compared to the control condition, support for the climate action is lower in both the bene f it condition (-0.362, p <0.001) and the harm condition (-0.285, p=0.003)."

#Figure 4: Effect of Cues on Support for Climate Action

#Plotting 
H2_plot <- ggplot(data=H2_base, aes(x=contrast)) + 
  geom_hline(yintercept=0, color="red", linewidth=.5) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high, width=0), linewidth=.5, position = position_dodge(width=.2)) +
  geom_errorbar(aes(ymin=lower90, ymax=upper90, width=0), linewidth=1, position = position_dodge(width=.2)) + 
  geom_point(aes(y=estimate), size=1.75, position = position_dodge(width=.2)) +
  xlab("Treatment\nCondition") +
  ylab("Effect of Cue on Support for Climate Policy") +
  coord_flip()+
  ylim(-0.6,0)+
  geom_label(aes(y=estimate,label=round(estimate, digits=2)), size=5,show.legend = FALSE)+
  plot_theme+
  labs(title="Marginal effects")

H2_plot

#Substantive effects
#Creating predicted models

plotData_H2_pred <- predictions(H2_mod,by=c("frame")) %>% 
  mutate(frame= dplyr::recode(frame,"control" = "Control",
                              "harm" = "Harm",
                              "benefit" = "Benefit"))%>% 
  mutate(lower90=estimate-1.645*std.error) %>% 
  mutate(upper90=estimate+1.645*std.error)  

#plotting
H2_effectsPlot <- ggplot(plotData_H2_pred, aes(x=frame, y=estimate, ymin=conf.low, ymax=conf.high)) +
  geom_point(aes(y=estimate), size=1.75, position = position_dodge(width=.2)) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high, width=0), linewidth=.5, position = position_dodge(width=.2)) +
  geom_errorbar(aes(ymin=lower90, ymax=upper90, width=0), linewidth=1, position = position_dodge(width=.2)) + 
  geom_label(aes(y=estimate,label=round(estimate, digits=2)), size=3)+
  plot_theme+
  ylim(2,3)+
  labs(title="Substantive Effects",y="Support for Climate Policy (0-4)",x="Treatment Condition",caption='Support ranges from 0 (oppose a great deal) to 4 (support a great deal).\nReference Category: Control condition. \n Outer errors bars: 95% CI; inner error bars: 90% CI.\n N=1,152.')

H2_effectsPlot


#Combining into one plot (Figure 4)
H2_final_plot<-H2_plot+H2_effectsPlot
#Adding annotations
H2_final_plot<-H2_final_plot + plot_annotation(tag_levels = 'a')

H2_final_plot<-H2_final_plot + plot_annotation(tag_levels = list(c("(a)","(b)")))& 
  theme(plot.tag.position = c(0, 1),
        plot.tag = element_text(hjust = -.25, vjust = 1.2))
H2_final_plot

#ggsave("H2_final_plot.png",H2_final_plot,width=12,height=6,units="in",dpi=300,bg = 'white')
ggsave("Figure4.eps",H2_final_plot,width=6.5,height=5.25,units="in",dpi=300,bg = 'white')


#binary dv for percentage point decline in support
H2_bin_mod<-lm(dv_bin ~ frame, data = Main_data_analysis)
H2_bin_mod <- avg_slopes(H2_bin_mod,variable="frame")

#13.3 (95% CI [-20.3,-6.31]) percentage points smaller in the benefit condition and 8.9 (95% CI [-16.0, -1.77]) percentage points lower in the harm condition. 


#Table A17: Effect of Frame Treatment on Climate Policy Approval
stargazer(H2_mod,title="Effect of Frame Treatment on Climate Policy Approval",digits=3,star.cutoffs = c(0.1,0.05,0.01),
          label="H2_table",
          covariate.labels = c("\\textbf{Treatment} (reference= Control) &   \\\\
                      \\hspace{1cm}Benefit",
                      "\\hspace{1cm}Harm"),
          
          notes="\\parbox[t]{12cm}{Coefficients reported from OLS regression model. The dependent variable is coded on a five point scale, with four indicating support a great deal. Significance codes:{$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01}, two-tailed tests.}",
          notes.append=FALSE,
          style="apsr",
          dep.var.caption = c(""),
          dep.var.labels.include = FALSE,
          table.layout ="-c-!t-s-n",
          header=FALSE,
          notes.align= "l",
          column.sep.width = "1pt",
          font.size = "tiny",
          no.space = TRUE)


##### H2: Regardless of scope did frame have an effect on mechanism ##### 

#Mechanism Help/Hurt: "To what extent do you believe the proposed policy will help or harm people like you?"
H2_mod_mech<-lm(dv_help_hurt ~ frame, data = Main_data_analysis)
nobs(H2_mod_mech)
summary(H2_mod_mech)
#benefit cue (-0.306, $p< 0.001$) and the harm cue (-0.14, p=0.098)

H2_base_mech <- avg_slopes(H2_mod_mech,variable="frame")

H2_base_mech$contrast[H2_base_mech$contrast == "benefit - control"] <- "Benefit"
H2_base_mech$contrast[H2_base_mech$contrast == "harm - control"] <- "Harm"

#Selecting contrasts of interest to plot
H2_base_mech<-H2_base_mech %>% 
  filter(contrast%in%c("Benefit","Harm")) %>% 
  mutate(lower90=estimate-1.645*std.error) %>% 
  mutate(upper90=estimate+1.645*std.error) 
  

#Figure 5: Effect of Cues on Perceived Personal Benefit of Climate Action
H2_plot_mech <- ggplot(data=H2_base_mech, aes(x=contrast)) + 
  geom_hline(yintercept=0, color="red", linewidth=.5) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high, width=0), linewidth=.5, position = position_dodge(width=.2)) +
  geom_errorbar(aes(ymin=lower90, ymax=upper90, width=0), linewidth=1, position = position_dodge(width=.2)) + 
    geom_point(aes(y=estimate), size=2.75, position = position_dodge(width=.2)) +
  xlab("Treatment\ncondition") +
  labs(y='Effect of Cue on Belief the Climate Policy will Help/Harm People Like You',
       caption='Support ranges from 0 (harm a great deal) to 4 (help a great deal).\nReference Category: Control condition. \n Outer errors bars: 95% CI; inner error bars: 90% CI.\n N=1,154.')+
  coord_flip()+
  ylim(-0.5,0.15)+
  geom_label(aes(y=estimate,label=round(estimate, digits=2)), size=5)+
  plot_theme 
H2_plot_mech

#ggsave("Pfunc\\H2_plot_mech.png",H2_plot_mech,width=7.5, height=4,units="in",dpi=300,bg = 'white')
ggsave("Figure5.eps",H2_plot_mech,width=6.5,height=3,units="in",dpi=300,bg = 'white')


#Table for Appendix
stargazer(H2_mod_mech,title="Overall Effect of Frame Treatment on Help/Hurt",digits=3,star.cutoffs = c(0.1,0.05,0.01),
          label="H2_table_mech",
          covariate.labels = c("\\textbf{Treatment} (reference= Control) &   \\\\
                      \\hspace{1cm}Benefit",
                      "\\hspace{1cm}Harm"),
          notes="\\parbox[t]{12cm}{Coefficients reported from OLS regression model. The dependent variable is coded on a five point scale, with four indicating help a great deal. Significance codes:{$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01}, two-tailed tests.}",
          notes.append=FALSE,
          style="apsr",
          dep.var.caption = c(""),
          dep.var.labels.include = FALSE,
          table.layout ="-c-!t-s-n",
          header=FALSE,
          notes.align= "l",
          column.sep.width = "1pt",
          font.size = "scriptsize",
          no.space = TRUE)


#H3: The effect of the Frame conditions relative to the control, will be more negative for individuals exhibiting higher levels of racial resentment (more racially conservative attitudes) compared to those with lower levels of racial resentment.

##### 4.3 Role of racial resentment ##### 

#Racial resentment
mean(Main_data_analysis$Racial_resentment,na.rm=T) #0.54

#Psych package Cronbach's Alpha
RR_items<-Main_data_analysis%>% 
  dplyr::select(RR_no_favors,RR_slavery,RR_tryharder,RR_deserve)

#Alpha=0.73
RR_alpha<-alpha(RR_items)


#Selecting contrasts of interest to plot

frame_model_RR<-lm(dv~frame*Racial_resentment+age+gender+Income+PartyID+political_ideo_3+Poli_interest_num+Religiosity_num+educ,data = Main_data_analysis)

#result of lm
summ(frame_model_RR)

#"While the coefficient for HarmXRacialResentment is statistically significant (p = 0.03), the coefficient for Bene f itXRacialResentment is not (p = 0.30)."


#n=1,137
nobs(frame_model_RR)

#Pulling out marginal effects by level of RR
ME_RR_frame <- avg_slopes(frame_model_RR,variables="frame",by=c("Racial_resentment"))

#Renaming variables
ME_RR_frame$contrast[ME_RR_frame$contrast == "mean(benefit) - mean(control)"] <- "Benefit"
ME_RR_frame$contrast[ME_RR_frame$contrast == "mean(harm) - mean(control)"] <- "Harm"

#Adding in 83.4% CI for comparison
ME_RR_frame<-ME_RR_frame %>% 
  filter(contrast%in%c("Benefit","Harm")) %>% 
  mutate(lower83=estimate-1.386*std.error) %>% 
  mutate(upper83=estimate+1.386*std.error) 

#Just harm condition for plot
ME_RR_frame_plot<-ME_RR_frame %>% 
            filter(contrast=="Harm")

#"respondents with the highest level of racial resentment reporting a decline in support of -0.60 (p <0.00), relative to respondents with the highest level of racial resentment in the control"

#Figure 6: Effect of Harm Cue on Support for Climate Action By Level of Racial Resentment
H3_plot <- ggplot(ME_RR_frame_plot, aes(x = Racial_resentment, y = estimate))+
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high, width=0.0), linewidth=.5) +
  geom_errorbar(aes(ymin=lower83, ymax=upper83, width=0.0), linewidth=.8) + 
  geom_point(aes(y=estimate), size=1.75) +
  geom_hline(yintercept = 0) +
  geom_label(aes(y=estimate,label=round(estimate, digits=2)), size=3.5)+
  ylim(-1, 0.5) +
  xlim(0, 1) +
  plot_theme+
  labs(x="",y='Effect of Harm Cue on Support for Climate Policy')+
  theme(plot.margin = unit(c(0,0,-5.5,0), "lines"))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.line.x=element_blank())

H3_plot

#Adding histogram
#Distribution of RR in harm condition
RR_harm<-dplyr::select(Main_data_analysis,Racial_resentment,frame) %>% 
  filter(frame=="harm") %>% 
  group_by(Racial_resentment) %>% 
  summarise(n = n()) %>%
  mutate(Percentage = round(100*n/sum(n),2)) %>% 
  filter(Racial_resentment!="")


Dist_RR_harm<- ggplot(RR_harm, aes(x = Racial_resentment,y=n)) +  
  geom_bar(stat="identity", position = position_dodge(width = 1),fill = "lightgray") +
  plot_theme+
  scale_x_continuous(breaks = c(0,0.125,0.25,0.375,0.5,0.625,0.75,0.875,1))+
  labs(x="Racial Resentment",y=NULL,caption='Support ranges from 0 (oppose a great deal) to 4 (support a great deal).\nReference Category: Control condition. \nOuter errors bars: 95% CI; inner error bars: 83.4% CI.\n Model controls for demographics.\n N=1,137 \n Distribution of racial resentment shown in histogram.')+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

#Combining into 1 with overlaid histogram to show dist of RR in harm condition
H3_plot_final<-plot_grid(H3_plot, Dist_RR_harm, ncol = 1, nrow = 2, align = "v", axis = "lr",rel_heights=c(2.5,1))

#Title: Marginal Effect of Treatment on Climate Policy Approval By Level of Racial Resentment w/ 95\% CI
#ggsave("Pfunc\\H3_plot_alt.png",H3_plot_final,width=10, height=8.75,units="in",dpi=300,bg = 'white')
ggsave("Figure6.eps",H3_plot_final,width=6.5,height=8.5,units="in",dpi=300,bg = 'white')

#Tables for appendix

#Outputting table of main results for body of paper
stargazer(frame_model_RR,title="Heterogeneous Effects of Treatment on Climate Policy Approval: Racial Resentment",digits=3,keep.stat=c("n","rsq"),
          omit = c("Constant"),
          keep = c("framebenefit","frameharm","Racial_resentment"), 
          label="H3_table_main",
          order = c(27,28,1,2,3,4,5),
          covariate.labels = c("\\textbf{Frame} (reference= Control) &   \\\\
         \\hspace{1cm}Benefit * Racial Resentment",
         "\\hspace{1cm}Harm * Racial Resentment",
         "\\hspace{1cm}Benefit",
         "\\hspace{1cm}Harm",
         "\\textbf{Racial Resentment}"),
          notes="\\parbox[t]{12cm}{Coefficients reported from OLS regression model. Model includes control variables for age, gender, income, partyID, political interest, religiosity, political ideology, and education level. The dependent variable is coded on a five point scale, with four indicating support a great deal. Significance codes:{$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01}, two-tailed tests.}",
          notes.append=FALSE,
          style="apsr",
          dep.var.caption = c(""),
          dep.var.labels.include = FALSE,
          table.layout ="-c-!t-sa-n",
          header=FALSE,
          notes.align= "l",
          column.sep.width = "1pt",
          font.size = "small",
          add.lines=list(c('Control Variables', 'Yes')),
          no.space = TRUE,
         out = "Table2.html")

#Table A21: Heterogeneous Effects of Frame Treatment on Climate Policy Approval: Racial Resentment

#Control variables shown in table for appendix
stargazer(frame_model_RR,title="Heterogeneous Effects of Frame Treatment on Climate Policy Approval: Racial Resentment",digits=3,star.cutoffs = c(0.1,0.05,0.01),
          order = c(27,28,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26),
          label="H3_table",
          covariate.labels = c("\\textbf{Frame} (reference= Control) &   \\\\
                      \\hspace{1cm}Benefit * Racial Resentment",
                      "\\hspace{1cm}Harm * Racial Resentment",
                      "\\hspace{1cm}Benefit",
                      "\\hspace{1cm}Harm",
                      "\\textbf{Racial Resentment}",
                      "\\textbf{Age} (reference= 18-24) &   \\\\
                      \\hspace{1cm}25 - 34",
                      "\\hspace{1cm}35 - 44",
                      "\\hspace{1cm}45 - 54",
                      "\\hspace{1cm}55 or older",
                      "\\textbf{Gender} (reference= Female) &   \\\\
                      \\hspace{1cm}Male",
                      "\\hspace{1cm}Other",
                      "\\textbf{Income} (reference= Up to \\$29,999) &   \\\\
                      \\hspace{1cm}\\$30,0000--\\$59,999",
                      "\\hspace{1cm}\\$60,000--\\$99,999",
                      "\\hspace{1cm}\\$100,000--\\$149,999",
                      "\\hspace{1cm}More than \\$150,000",
                      "\\hspace{1cm}Prefer not to say",
                      "\\textbf{Political Party} (reference= Independent) &   \\\\
                      \\hspace{1cm}Democrat",
                      "\\hspace{1cm}Republican",
                      "\\hspace{1cm}Don't know/Other",
                      "\\textbf{Political Ideology} (reference= Moderate) &   \\\\
                      \\hspace{1cm}Liberal",
                      "\\hspace{1cm}Conservative",
                      "\\textbf{Political interest}",
                      "\\textbf{Religiosity}",
                      "\\textbf{Education} (reference= Associate's Degree) &   \\\\
                       \\hspace{1cm}Less than high school",
                      "\\hspace{1cm}High school graduate",
                      "\\hspace{1cm}Some college",
                      "\\hspace{1cm}Bachelor's Degree",
                      "\\hspace{1cm}Advanced Degree"),
          notes="\\parbox[t]{12cm}{Coefficients reported from OLS regression model. The dependent variable is coded on a five point scale, with four indicating support a great deal. Significance codes:{$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01}, two-tailed tests.}",
          notes.append=FALSE,
          style="apsr",
          dep.var.caption = c(""),
          dep.var.labels.include = FALSE,
          table.layout ="-c-!t-s-n",
          header=FALSE,
          notes.align= "l",
          column.sep.width = "1pt",
          font.size = "tiny",
          no.space = TRUE)

#Substantive effects RR 

RR_sub_effect <- avg_predictions(
  frame_model_RR,
  variables = list(
    frame = c("control", "harm", "benefit"),
    Racial_resentment = c(0, 0.125, 0.25, 0.375, 0.5, 0.625, 0.75, 0.875, 1)),
  conf_level = 0.834) %>%
  mutate(frame = recode(
    frame,
    "control" = "Control",
    "harm" = "Harm",
    "benefit" = "Benefit"))


#Min max values of RR
plotData_RR_minmax<-RR_sub_effect%>% 
  filter(Racial_resentment%in%c(0,1)) %>% 
  dplyr::select(Racial_resentment,frame,estimate,conf.low,conf.high)



##### Effects by partyID ##### 
#PID leaners included

main_model_party_leaners<-lm(dv~frame*pid_leaners_inc+age+gender+Income+political_ideo_3+Poli_interest_num+Religiosity_num+educ, data = Main_data_analysis %>% filter(pid_leaners_inc!="Neither/I don't know"))
summ(main_model_party_leaners)

nobs(main_model_party_leaners)
ME_party_leaners <- avg_slopes(main_model_party_leaners,variables="frame",by=c("pid_leaners_inc"))

#Renaming contrast for ease of comparison
ME_party_leaners$contrast[ME_party_leaners$contrast == "mean(benefit) - mean(control)"] <- "Benefit"
ME_party_leaners$contrast[ME_party_leaners$contrast == "mean(harm) - mean(control)"] <- "Harm"
ME_party_leaners$contrast[ME_party_leaners$contrast == "benefit - control"] <- "Benefit"
ME_party_leaners$contrast[ME_party_leaners$contrast == "harm - control"] <- "Harm"


#"These negative effects of the cues are statistically significant at the p <0.01 level."
ME_party_leaners_frame<-ME_party_leaners %>% 
  filter(contrast%in%c("Benefit","Harm")) %>% 
  mutate(PartyID= dplyr::recode(pid_leaners_inc,"Republican" = "Republicans",
                                "Democrat" = "Democrats")) %>% 
  mutate(lower90=estimate-1.645*std.error) %>% 
  mutate(upper90=estimate+1.645*std.error) 


#Figure 7: Effect of Cue on Support for Climate Action by Party ID
Party_plot_leaners <- ggplot(ME_party_leaners_frame, aes(x = PartyID, y = estimate, color = PartyID))+
  labs(x="PartyID",y='Effect of Cue on Support for Climate Policy',
       caption='Support ranges from 0 (oppose a great deal) to 4 (support a great deal).\nReference Category: Control condition. \\n Outer errors bars: 95% CI; inner error bars: 90% CI.\n Model controls for demographics.\n N=944. Leaners included.')+
  scale_color_manual(values=c("#3B9AB2","#F21A00"))+
  facet_grid(~contrast)+
  geom_errorbar(aes(ymin=lower90, ymax=upper90, width=0), linewidth=1 , position = position_dodge(width=.2)) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high, width=0), linewidth=.5 , position = position_dodge(width=.2)) +
  geom_point(aes(y=estimate), size=2.75, position = position_dodge(width=.2)) +
  geom_hline(yintercept = 0) +
  geom_label(aes(y=estimate,label=round(estimate, digits=2)), size=5,show.legend = FALSE)+
  ylim(-1, 0.1) +
  plot_theme+
  coord_flip()+
  guides(color="none")
Party_plot_leaners

#ggsave("Pfunc\\Party_plot_leaners.png",Party_plot_leaners,width=10, height=6,units="in",dpi=300,bg = 'white')
ggsave("Figure7.eps",Party_plot_leaners,width=6.5,height=3.9,units="in",dpi=300,bg = 'white')


#Substantive effects
party_sub_effect <- avg_predictions(
  main_model_party_leaners,
  variables = list(
    frame = c("control", "harm", "benefit"),
    pid_leaners_inc = c("Republican", "Democrat")
  ),
  newdata = model.frame(main_model_party_leaners),
  conf_level = 0.834
)%>% 
  mutate(frame= dplyr::recode(frame,"control" = "Control",
                              "harm" = "Harm",
                              "benefit" = "Benefit")) %>% 
  mutate(PartyID= dplyr::recode(pid_leaners_inc,"Republican" = "Republicans",
                                "Democrat" = "Democrats"))

#"Furthermore, support for climate policy among Democrats remained substantively higher than among Republicans, with Democrats in both cue conditions (2.79 in the benefit condition and 2.82 in harm) expressing a higher level of support than Republicans in the control group (2.33)."


#Table A22: Heterogeneous Effects of Frame Treatment on Climate Policy Approval: PartyID (leaners inc.)
stargazer(main_model_party_leaners,title="Heterogeneous Effects of Frame Treatment on Climate Policy Approval: PartyID (leaners inc.)",digits=3,star.cutoffs = c(0.1,0.05,0.01),
          order = c(24,25,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23),
          label="partyID_table",
          covariate.labels = c("\\textbf{Frame*PartyID} (reference= Control * Democrat) &   \\\\
                      \\hspace{1cm}Benefit * Republican",
                      "\\hspace{1cm}Harm * Republican",
            "\\textbf{Frame} (reference= Control) &   \\\\
                      \\hspace{1cm}Benefit",
                      "\\hspace{1cm}Harm",
            "\\textbf{Political Party (leaners inc.)} (reference= Democrat) &   \\\\
            \\hspace{1cm}Republican",
                      "\\textbf{Age} (reference= 18-24) &   \\\\
                      \\hspace{1cm}25 - 34",
                      "\\hspace{1cm}35 - 44",
                      "\\hspace{1cm}45 - 54",
                      "\\hspace{1cm}55 or older",
                      "\\textbf{Gender} (reference= Female) &   \\\\
                      \\hspace{1cm}Male",
                      "\\hspace{1cm}Other",
                      "\\textbf{Income} (reference= Up to \\$29,999) &   \\\\
                      \\hspace{1cm}\\$30,0000--\\$59,999",
                      "\\hspace{1cm}\\$60,000--\\$99,999",
                      "\\hspace{1cm}\\$100,000--\\$149,999",
                      "\\hspace{1cm}More than \\$150,000",
                      "\\hspace{1cm}Prefer not to say",
                      "\\textbf{Political Ideology} (reference= Moderate) &   \\\\
                      \\hspace{1cm}Liberal",
                      "\\hspace{1cm}Conservative",
                      "\\textbf{Political interest}",
                      "\\textbf{Religiosity}",
                      "\\textbf{Education} (reference= Associate's Degree) &   \\\\
                       \\hspace{1cm}Less than high school",
                      "\\hspace{1cm}High school graduate",
                      "\\hspace{1cm}Some college",
                      "\\hspace{1cm}Bachelor's Degree",
                      "\\hspace{1cm}Advanced Degree"),
          notes="\\parbox[t]{12cm}{Coefficients reported from OLS regression model. The dependent variable is coded on a five point scale, with four indicating support a great deal. Significance codes:{$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01}, two-tailed tests.}",
          notes.append=FALSE,
          style="apsr",
          dep.var.caption = c(""),
          dep.var.labels.include = FALSE,
          table.layout ="-c-!t-s-n",
          header=FALSE,
          notes.align= "l",
          column.sep.width = "1pt",
          font.size = "tiny",
          no.space = TRUE)


#Footnote 34: The results are similar among pure partisans.
#Partyid without leaners

main_model_party<-lm(dv~frame*PartyID+age+gender+Income+political_ideo_3+Poli_interest_num+Religiosity_num+educ, data = Main_data_analysis %>% filter(PartyID!="Don't know/other"))
summ(main_model_party)

nobs(main_model_party)
ME_party <- avg_slopes(main_model_party,variables="frame",by=c("PartyID"))

#Renaming contrast for ease of comparison
ME_party$contrast[ME_party$contrast == "mean(benefit) - mean(control)"] <- "Benefit"
ME_party$contrast[ME_party$contrast == "mean(harm) - mean(control)"] <- "Harm"
ME_party$contrast[ME_party$contrast == "benefit - control"] <- "Benefit"
ME_party$contrast[ME_party$contrast == "harm - control"] <- "Harm"

ME_party_frame<-ME_party %>% 
  filter(contrast%in%c("Benefit","Harm")) %>% 
  mutate(PartyID= dplyr::recode(PartyID,"Republican" = "Republicans",
                                "Independent" = "Independents",
                                "Democrat" = "Democrats")) %>% 
  mutate(lower90=estimate-1.645*std.error) %>% 
  mutate(upper90=estimate+1.645*std.error) 


# Appendix ----------------------------------------------------------------
##### Demographics #####

#Creating demographic balance table on the treatment variable among white respondents

demographics<-Main_data_analysis %>% 
  dplyr::select(frame,Ideology=political_ideo_3,RR_three_cat,age,gender,Income,PartyID,Poli_interest =interest ,religiosity,educ) %>% 
  #filter(gender%in%c("Female","Male")) %>% 
  mutate(Poli_interest = factor(Poli_interest, levels=c("Most of the time","Some of the time","Only now and then","Hardly at all"))) %>% 
  mutate(Religiosity = factor(religiosity, levels=c("More than once a week","Once a week","A few times a month","A few times a year","Never"))) %>% 
  #filter(Religiosity!="") %>% 
  #filter(gender!="") %>% 
  mutate(Ideology = factor(Ideology, levels=c("Liberal","Moderate", "Conservative"))) 


#Treatment Balance Table
treat_balance<-datasummary_balance(~ frame,data = dplyr::select(demographics,frame,age,gender,Income,PartyID,Poli_interest ,Religiosity,educ,RR_three_cat),output="latex",title="Demographic Balance Table: Pfunc")

#Balance testing
#Conservative threshold of 0.1 based on 
#Stuart, Elizabeth A., Brian K. Lee, and Finbarr P. Leacy. 2013. “Prognostic Score-Based Balance Measures Can Be a Useful Diagnostic for Propensity Score Methods in Comparative Effectiveness Research.” Journal of Clinical Epidemiology 66 (8): S84. https://doi.org/10.1016/j.jclinepi.2013.01.013.

bal.tab(frame ~ Racial_resentment+age+female+Income+PartyID+political_ideo_3+Poli_interest_num+Religiosity_num+educ, data = Main_data_analysis, estimand = "ATT", m.threshold = .1)

##### Checking comp checks #####

#Footnote 26: "Respondents who received the benefit treatment answered that “non-Whites” are the group that most benefit at significantly (p < 0.000) higher rates (23 percentage points higher) than those in the control. Additionally, those who received the harm treatment answered that that “non-Whites” are the group most negatively impacted by climate change at significantly (p < 0.000), 22 percentage points higher, higher rates than those in the control."

  #Benefit
Benefit_Comp_check <- Main_data_analysis %>% dplyr::select(comp_check_benefit_bin, frame) %>% 
  dplyr:: filter(!is.na(comp_check_benefit_bin)) %>%
  group_by(frame) %>% 
  summarize(mean=mean(comp_check_benefit_bin), sd=sd(comp_check_benefit_bin), n=n(), se=sd/sqrt(n)) %>% 
  mutate(upper95CI = mean+1.96*se, lower95CI =mean-1.96*se) %>% 
  dplyr::select(frame,"Comprehension Check:Benefit"=mean,lower95CI,upper95CI,n)

print(xtable(Benefit_Comp_check,"Answers to Factual Comprehension Check: Benefit"), include.rownames=T)

    #T.test: benefit
ttest_benefit_check<-t.test(Main_data_analysis$comp_check_benefit_bin[Main_data_analysis$frame=="control"],Main_data_analysis$comp_check_benefit_bin[Main_data_analysis$frame=="benefit"])

  #Harm 
Harm_Comp_check <- Main_data_analysis %>% dplyr::select(comp_check_harm_bin, frame) %>% 
  dplyr:: filter(!is.na(comp_check_harm_bin)) %>%
  group_by(frame) %>% 
  summarize(mean=mean(comp_check_harm_bin), sd=sd(comp_check_harm_bin), n=n(), se=sd/sqrt(n)) %>% 
  mutate(upper95CI = mean+1.96*se, lower95CI =mean-1.96*se) %>% 
  dplyr::select(frame,"Comprehension Check:Harm"=mean,lower95CI,upper95CI,n)

print(xtable(Harm_Comp_check,"Answers to Factual Comprehension Check: Harm"), include.rownames=T)

#T.test: harm
ttest_harm_check<-t.test(Main_data_analysis$comp_check_harm_bin[Main_data_analysis$frame=="control"],Main_data_analysis$comp_check_harm_bin[Main_data_analysis$frame=="harm"])



##### Confirmatory factor analysis: RR and Nationalism #####

#Table A19: Confirmatory Factor Analysis of Racial Resentment Index Ite
RR_item<-Main_data_analysis %>%
  dplyr::select(RR_no_favors,RR_slavery,RR_tryharder,RR_deserve)

scale.model_RR <- 'scale =~ RR_no_favors + RR_slavery + RR_tryharder+RR_deserve'

fit.cat_RR <- cfa(scale.model_RR, data=RR_item, mimic =c("MPlus"), std.lv = TRUE, ordered = TRUE)


#the Comparative Fit Index (CFI)=0.851 and the standardized root mean square residual (srmr)=0.157  — to assess model adequacy
fitMeasures(fit.cat_RR, c("cfi.scaled", "srmr"))

#standardized factor loadings and their standard errors for the items
parameterEstimates(fit.cat_RR, standardized=TRUE) %>%
  filter(op == "=~") %>%
  dplyr::select(Item=rhs, Loading=est, ci.lower, ci.upper, SE=se, Z=z, 'p-value'=pvalue)



#Table A20: Confirmatory Factor Analysis of Nationalism Index Items
nat_item<-Main_data_analysis %>%
  dplyr::select(EP.nationalism_1,EP.nationalism_2,EP.nationalism_3)

scale.model_nat <- 'scale =~ EP.nationalism_1+EP.nationalism_2+EP.nationalism_3'

fit.cat_nat <- cfa(scale.model_nat, data=nat_item, mimic =c("MPlus"), std.lv = TRUE, ordered = TRUE)


parameterEstimates(fit.cat_nat, standardized=TRUE) %>%
  filter(op == "=~") %>%
  dplyr::select(Item=rhs, Loading=est, ci.lower, ci.upper, SE=se, Z=z, 'p-value'=pvalue)




##### A.2.6 Breaking out results by scope ##### 

#Mean DV across frame conditions
treat_means_scope <- Main_data_analysis %>% dplyr::select(dv, frame,scope) %>% 
  filter(!is.na(frame)) %>%
  filter(!is.na(scope)) %>%
  filter(!is.na(dv)) %>%
  group_by(scope,frame) %>% 
  summarize(mean=mean(dv), sd=sd(dv), n=n(), se=sd/sqrt(n)) %>% 
  mutate(upper95CI = mean+1.96*se, lower95CI =mean-1.96*se) %>% 
  mutate(frame = factor(frame, levels=c("benefit","harm","control"))) %>% 
  dplyr::select(scope,frame,"mean(0-4)"=mean,lower95CI,upper95CI,n)

print(xtable(treat_means_scope,"Mean DV across frame/scope treatment conditions"), include.rownames=T)


H2_mod_scope<-lm(dv ~ frame*scope, data = Main_data_analysis)
nobs(H2_mod_scope)

H2_scope <- avg_slopes(H2_mod_scope,variable="frame",by="scope")

#Renaming contrast for ease of comparison
H2_scope$contrast[H2_scope$contrast == "mean(benefit) - mean(control)"] <- "Benefit"
H2_scope$contrast[H2_scope$contrast == "mean(harm) - mean(control)"] <- "Harm"
H2_scope$contrast[H2_scope$contrast == "mean(International) - mean(Domestic)"] <- "International - Domestic"


#The negative effect of the cues is statistically
#significant at the p < 0.05 level in all but harm for the domestic scope (p=0.26).

#Selecting contrasts of interest to plot
H2_scope<-H2_scope %>% 
  filter(contrast%in%c("Benefit","Harm")) %>% 
  mutate(Scope=scope)%>% 
  mutate(lower90=estimate-1.645*std.error) %>% 
  mutate(upper90=estimate+1.645*std.error) 

#Figure A2: Effect of Cues on Support for Climate Action by Type
H2_scope_plot <- ggplot(data=H2_scope, aes(x=contrast,color=Scope,shape=Scope)) + 
  geom_hline(yintercept=0, color="red", linewidth=.5) +
  geom_errorbar(aes(ymin=lower90, ymax=upper90, width=0), linewidth=1 , position = position_dodge(width=.2)) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high, width=0), linewidth=.5 , position = position_dodge(width=.2)) +
  geom_point(aes(y=estimate), size=2.75, position = position_dodge(width=.2)) +
  xlab("Treatment\ncondition") +
  labs(y='Effect of Cue on Support for Climate Policy',
       caption='Support ranges from 0 (oppose a great deal) to 4 (support a great deal).\nReference Category: Control condition. \n Outer errors bars: 95% CI; inner error bars: 90% CI.\n N=1,152')+
  coord_flip()+
  ylim(-0.75,0.25)+
  geom_label(aes(y=estimate,label=round(estimate, digits=2)), size=5,show.legend = FALSE)+
  plot_theme+
  scale_color_manual(values=c("#EBCC2A","#3B9AB2"))

H2_scope_plot

#Saving Figure
#Title= H2a/H2b 
#ggsave("Pfunc\\H2_scope_plot.png",H2_scope_plot,width=9, height=8,units="in",dpi=300,bg = 'white')



#Table for Appendix
stargazer(H2_mod_scope,title="Overall Effect of Frame*Scope Treatments on Climate Policy Approval",digits=3,star.cutoffs = c(0.1,0.05,0.01),
          label="H2_table",
          covariate.labels = c("\\textbf{Frame Treatment} (reference= Control) &   \\\\
                      \\hspace{1cm}Benefit",
                      "\\hspace{1cm}Harm",
                      "\\textbf{Scope Treatment} (reference= Domestic) &   \\\\
                      \\hspace{1cm}International",
                      "\\textbf{Frame*Scope Treatment} (reference= Control*Domestic) &   \\\\
                      \\hspace{1cm}Benefit*International",
                      "\\hspace{1cm}Harm*International"),
          notes="\\parbox[t]{12cm}{Coefficients reported from OLS regression model. The dependent variable is coded on a five point scale, with four indicating support a great deal. Significance codes:{$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01}, two-tailed tests.}",
          notes.append=FALSE,
          style="apsr",
          dep.var.caption = c(""),
          dep.var.labels.include = FALSE,
          table.layout ="-c-!t-s-n",
          header=FALSE,
          notes.align= "l",
          column.sep.width = "1pt",
          font.size = "scriptsize",
          no.space = TRUE)



##### Breaking out results by scope for mechanism ##### 

H2_mod_mech_scope<-lm(dv_help_hurt ~ frame*scope, data = Main_data_analysis)
nobs(H2_mod_mech_scope)

H2_mech_scope_df <- avg_slopes(H2_mod_mech_scope,variable="frame",by="scope")
H2_mech_scope_df$contrast[H2_mech_scope_df$contrast == "mean(benefit) - mean(control)"] <- "Benefit"
H2_mech_scope_df$contrast[H2_mech_scope_df$contrast == "mean(harm) - mean(control)"] <- "Harm"

#Selecting contrasts of interest to plot
H2_mech_scope_df<-H2_mech_scope_df %>% 
  filter(contrast%in%c("Benefit","Harm")) %>% 
  mutate(Scope=scope)%>% 
  mutate(lower90=estimate-1.645*std.error) %>% 
  mutate(upper90=estimate+1.645*std.error) 

#Figure A3: Effect of Cues on Perceived Personal Benefit of Climate Action by Type
H2_scope_plot_mech <- ggplot(data=H2_mech_scope_df, aes(x=contrast,color=Scope,shape=Scope)) + 
  geom_hline(yintercept=0, color="red", linewidth=.5) +
  geom_errorbar(aes(ymin=lower90, ymax=upper90, width=0), linewidth=1 , position = position_dodge(width=.2)) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high, width=0), linewidth=.5 , position = position_dodge(width=.2)) +
  geom_point(aes(y=estimate), size=2.75, position = position_dodge(width=.2)) +
  xlab("Treatment\ncondition") +
  labs(y='Effect of Cue on Belief the Climate Policy will Help/Harm People Like You',
       caption='Support ranges from 0 (harm a great deal) to 4 (help a great deal).\nReference Category: Control condition. \n Outer errors bars: 95% CI; inner error bars: 90% CI.\n N=1,154')+
  coord_flip()+
  ylim(-0.75,0.25)+
  geom_label(aes(y=estimate,label=round(estimate, digits=2)), size=5,show.legend = FALSE)+
  plot_theme+
  scale_color_manual(values=c("#EBCC2A","#3B9AB2"))
H2_scope_plot_mech

#Saving Figure
#Title= H2a/H2b 
#ggsave("Pfunc\\H2_scope_plot_mech.png",H2_scope_plot_mech,width=9, height=8,units="in",dpi=300,bg = 'white')

#Table for appendix
stargazer(H2_mod_mech_scope,title="Overall Effect of Frame*Scope Treatments on Help/Hurt",digits=3,star.cutoffs = c(0.1,0.05,0.01),
          label="H2_table_mech_scope",
          covariate.labels = c("\\textbf{Frame Treatment} (reference= Control) &   \\\\
                      \\hspace{1cm}Benefit",
                      "\\hspace{1cm}Harm",
                      "\\textbf{Scope Treatment} (reference= Domestic) &   \\\\
                      \\hspace{1cm}International",
                      "\\textbf{Frame*Scope Treatment} (reference= Control*Domestic) &   \\\\
                      \\hspace{1cm}Benefit*International",
                      "\\hspace{1cm}Harm*International"),
          notes="\\parbox[t]{12cm}{Coefficients reported from OLS regression model. The dependent variable is coded on a five point scale, with four indicating help a great deal. Significance codes:{$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01}, two-tailed tests.}",
          notes.append=FALSE,
          style="apsr",
          dep.var.caption = c(""),
          dep.var.labels.include = FALSE,
          table.layout ="-c-!t-s-n",
          header=FALSE,
          notes.align= "l",
          column.sep.width = "1pt",
          font.size = "scriptsize",
          no.space = TRUE)


##### H5: Nationalism as a moderator ##### 

#Correlation between two
# "The Pearson correlation coefficient between racial resentment and nationalism is 0.346 suggesting a weak relationship"
correlation_df<-dplyr::select(Main_data_analysis,Racial_resentment,Nationalism)

correlation<-corr.test(correlation_df)

#Displaying corr values in table
print(xtable(correlation$r,caption = "Correlations between Psychological Factors and Racial Resentment",digits=3),comment = FALSE,include.rownames=TRUE)


#Nationalism
mean(Main_data_analysis$Nationalism,na.rm=T) #0.59

#Alpha
Nat_items<-Main_data_analysis%>% 
  dplyr::select(EP.nationalism_1,EP.nationalism_2,EP.nationalism_3)

#Alpha=0.75
Nat_alpha<-alpha(Nat_items)


#Restricting to only international scope for analysis
main_model_nat<-lm(dv ~ frame * Nationalism + age + gender + 
                     Income + PartyID + political_ideo_3 + Poli_interest_num + 
                     Religiosity_num + educ, data = Main_data_analysis %>% filter(scope=="International"))

summary(main_model_nat)
#561
nobs(main_model_nat)


#Table A22: Heterogeneous Effects of Treatment on Climate Policy Approval: Nationalism

stargazer(main_model_nat,title="Heterogeneous Effects of Treatment on Climate Policy Approval: Nationalism",digits=3,star.cutoffs = c(0.1,0.05,0.01),
          order = c(27,28,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27),
          label="nat_table",
          covariate.labels = c("\\textbf{Frame} (reference= control) &   \\\\
                      \\hspace{1cm}Benefit * Nationalism",
                      "\\hspace{1cm}Harm * Nationalism",
                      "\\hspace{1cm}Benefit",
                      "\\hspace{1cm}Harm",
                      "\\textbf{Nationalism}",
                      "\\textbf{Age} (reference= 18-24) &   \\\\
                      \\hspace{1cm}25 - 34",
                      "\\hspace{1cm}35 - 44",
                      "\\hspace{1cm}45 - 54",
                      "\\hspace{1cm}55 or older",
                      "\\textbf{Gender} (reference= Female) &   \\\\
                      \\hspace{1cm}Male",
                      "\\hspace{1cm}Other",
                      "\\textbf{Income} (reference= Up to \\$29,999) &   \\\\
                      \\hspace{1cm}\\$30,0000--\\$59,999",
                      "\\hspace{1cm}\\$60,000--\\$99,999",
                      "\\hspace{1cm}\\$100,000--\\$149,999",
                      "\\hspace{1cm}More than \\$150,000",
                      "\\hspace{1cm}Prefer not to say",
                      "\\textbf{Political Party} (reference= Independent) &   \\\\
                      \\hspace{1cm}Democrat",
                      "\\hspace{1cm}Republican",
                      "\\hspace{1cm}Don't know/Other",
                      "\\textbf{Political Ideology} (reference= Moderate) &   \\\\
                      \\hspace{1cm}Liberal",
                      "\\hspace{1cm}Conservative",
                      "\\textbf{Political interest}",
                      "\\textbf{Religiosity}",
                      "\\textbf{Education} (reference= Associate's Degree) &   \\\\
                       \\hspace{1cm}Less than high school",
                      "\\hspace{1cm}High school graduate",
                      "\\hspace{1cm}Some college",
                      "\\hspace{1cm}Bachelor's Degree",
                      "\\hspace{1cm}Advanced Degree"),
          notes="\\parbox[t]{12cm}{Coefficients reported from OLS regression models. The dependent variable is coded on a five point scale, with four indicating support a great deal. Significance codes:{$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01}, two-tailed tests.}",
          notes.append=FALSE,
          style="apsr",
          dep.var.caption = c(""),
          dep.var.labels.include = FALSE,
          table.layout ="-c-!t-s-n",
          header=FALSE,
          notes.align= "l",
          column.sep.width = "1pt",
          font.size = "tiny",
          no.space = TRUE)



## Pre-registered scope difference ----

#Table A25: Effects of Scope Treatment on Climate Policy Approval

#Base Model that I'll get contrasts off of 
scope_model<-lm(dv~scope, data = Main_data_analysis %>% filter(frame=="control"))
summ(scope_model)
nobs(scope_model)



#H2a: Racial resentful respondents prefer domestic

scope_model_RR<-lm(dv~scope*Racial_resentment+age+gender+Income+PartyID+political_ideo_3+Poli_interest_num+Religiosity_num+educ, data = Main_data_analysis %>% filter(frame=="control"))
summ(scope_model_RR)

#scope b: Nationalistic respondents prefer domestic
scope_model_nat<-lm(dv~scope*Nationalism+age+gender+Income+PartyID+political_ideo_3+Poli_interest_num+Religiosity_num+educ, data = Main_data_analysis %>% filter(frame=="control"))

#combined Tables for 

stargazer(scope_model,scope_model_RR,scope_model_nat,title="Heterogeneous Effects of Scope Treatment on Climate Policy Approval",digits=3,star.cutoffs = c(0.1,0.05,0.01),
          order = c(27,28,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26),
          label="scope_RR_Nat",
          covariate.labels = c("\\textbf{Scope} (reference= Domestic) &   \\\\
                      \\hspace{1cm}International * Racial Resentment",
                      "\\hspace{1cm}International * Nationalism",
                      "\\hspace{1cm}International",
                      "\\textbf{Racial Resentment}",
                      "\\textbf{Nationalism}",
                      "\\textbf{Age} (reference= 18-24) &   \\\\
                      \\hspace{1cm}25 - 34",
                      "\\hspace{1cm}35 - 44",
                      "\\hspace{1cm}45 - 54",
                      "\\hspace{1cm}55 or older",
                      "\\textbf{Gender} (reference= Female) &   \\\\
                      \\hspace{1cm}Male",
                      "\\hspace{1cm}Other",
                      "\\textbf{Income} (reference= Up to \\$29,999) &   \\\\
                      \\hspace{1cm}\\$30,0000--\\$59,999",
                      "\\hspace{1cm}\\$60,000--\\$99,999",
                      "\\hspace{1cm}\\$100,000--\\$149,999",
                      "\\hspace{1cm}More than \\$150,000",
                      "\\hspace{1cm}Prefer not to say",
                      "\\textbf{Political Party} (reference= Independent) &   \\\\
                      \\hspace{1cm}Democrat",
                      "\\hspace{1cm}Republican",
                      "\\hspace{1cm}Don't know/Other",
                      "\\textbf{Political Ideology} (reference= Moderate) &   \\\\
                      \\hspace{1cm}Liberal",
                      "\\hspace{1cm}Conservative",
                      "\\textbf{Political interest}",
                      "\\textbf{Religiosity}",
                      "\\textbf{Education} (reference= Associate's Degree) &   \\\\
                       \\hspace{1cm}Less than high school",
                      "\\hspace{1cm}High school graduate",
                      "\\hspace{1cm}Some college",
                      "\\hspace{1cm}Bachelor's Degree",
                      "\\hspace{1cm}Advanced Degree"),
          notes="\\parbox[t]{12cm}{Coefficients reported from OLS regression models. The dependent variable is coded on a five point scale, with four indicating support a great deal. Significance codes:{$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01}, two-tailed tests.}",
          notes.append=FALSE,
          style="apsr",
          dep.var.caption = c(""),
          dep.var.labels.include = FALSE,
          column.labels = c("Main Effects","Racial Resentment", "Nationalism"),
          table.layout ="-c-!t-s-n",
          header=FALSE,
          notes.align= "l",
          column.sep.width = "1pt",
          font.size = "tiny",
          no.space = TRUE)


## Sensitivity Analysis ----
sense_model_1<-lm(dv ~ frame+age+gender+Income+PartyID+political_ideo_3+Poli_interest_num+Religiosity_num+educ, data = Main_data_analysis)

summary(sense_model_1)
sense_1 <- sensemakr(model = sense_model_1,
                     treatment = "framebenefit",
                     benchmark_covariates = "PartyIDRepublican",
                     kd = 1:3)
summary(sense_1)
plot(sense_1, sensitivity.of = "t-value")

ovb_minimal_reporting(sense_1)

#The negative effect of the frame is robust, an omitted variable would need to be many times stronger than partisanship to fully explain it away.


