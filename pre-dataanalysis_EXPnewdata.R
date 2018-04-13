# preliminar Dataanalysis Navon, AGLT, AEFT (ANOVA, LM)
#libraries
library("dplyr", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.3")
library("psych", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.3")
library("ggplot2", lib.loc="/usr/local/lib/R/site-library")
library("tidyr", lib.loc="/usr/local/lib/R/site-library")
#Struktur des Datensatzes
str(allresults)

# mixed design anova example 2(inc/con)x 2(AP/RP)
# Navon L
allresults_long <- gather(allresults, time, value, vis_SACS_Linc,vis_SACS_Lcon)
aov_local <- aov(value ~ APdef*time + Error(code/time), data=allresults_long)
summary(aov_local)
boxplot(value ~ time* APdef, data=allresults_long)
#Navon G
allresults_longG <- gather(allresults, timeG, valueG, vis_SACS_Ginc,vis_SACS_Gcon)
aov_global <- aov(valueG ~ APdef*timeG + Error(code/timeG), data=allresults_longG)
summary(aov_global)
boxplot(valueG ~ timeG* APdef, data=allresults_longG)
#AGLT G
allresults_longGaud <- gather(allresults, timeGaud, valueGaud, aud_SACS_Ginc,aud_SACS_Gcon)
boxplot(valueGaud ~ timeGaud* APdef, data=allresults_longGaud)
aov_globalaud <- aov(valueGaud ~ APdef*timeGaud + Error(code/timeGaud), data=allresults_longGaud)
summary(aov_globalaud)
#AGLT L
allresults_longLaud <- gather(allresults, timeLaud, valueLaud, aud_SACS_Linc,aud_SACS_Lcon)
boxplot(valueLaud ~ timeLaud* APdef, data=allresults_longLaud)
aov_localaud <- aov(valueLaud ~ APdef*timeLaud + Error(code/timeLaud), data=allresults_longLaud)
summary(aov_localaud)





##mixed hlm 2(inc/con)x2(L/G)x 2(AP/RP) visuell
allresults_longvis <- gather(allresults, time, value, vis_SACS_Linc, vis_SACS_Lcon,vis_SACS_Ginc,vis_SACS_Gcon)
LG<-character(144)
LG[1:72]<-"L"
LG[73:144]<- "G"
congruency<-character(144)
congruency[1:36]<-"inc"
congruency[73:108]<- "inc"
congruency[37:72]<-"con"
congruency[109:144]<-"con"
allresults_longvis <- data.frame(allresults_longvis,LG,congruency)
#lmer(LG ~timeLG+(timeLG|APdef)+timeL+(timeL|APdef)+timeG+(timeG|APdef)+APdef, data = allresults_long2)
#mixed ANOVA
aov_vis <- aov(value ~ APdef*LG*congruency+Error(code/(LG*congruency)), data=allresults_longvis)
pairwise.t.test(allresults_longvis$value,allresults_longvis$LG)
summary(aov_vis)
boxplot(value ~ LG*congruency* APdef, data=allresults_longvis, main= "Navon")
#plot error bars
means_vis<-tapply(X=allresults_longvis$value,
                  INDEX=list(allresults_longvis$congruency,allresults_longvis$LG,
                             allresults_longvis$APdef),FUN=mean)
sd_vis<-tapply(X=allresults_longvis$value,
               INDEX=list(allresults_longvis$congruency,allresults_longvis$LG,
                            allresults_longvis$APdef),FUN=sd)
length_xvis<-tapply(X=allresults_longvis$value,
                 INDEX=list(allresults_longvis$congruency,allresults_longvis$LG,
                            allresults_longvis$APdef),FUN=length)
se_vis<-sd_vis/sqrt(length_xvis)
descr_vis<-cbind(group=c(1,2,3,4,5,6,7,8),means_vis,se_vis)
rownames(descr_vis)<-c("G_con_0","G_inc_0","L_con_0","L_inc_0",
                       "G_con_1","G_inc_1","L_con_1","L_inc_1")
descr_vis<-data.frame(descr_vis)
j<-ggplot(descr_vis,aes(x=group,y=means_vis,ymin=means_vis-1.96*se_vis,ymax=means_vis+1.96*se_vis))
j+geom_point(size=3)+geom_errorbar(color=c("blue","blue","blue","blue","#00CC33","#00CC33","#00CC33","#00CC33"),size=0.7, width=0.5)+theme_classic()+ labs(x="Condition",
                      y= "Speed-accuracy-composite-Score
(SACS)", 
                      title="Navon",subtitle="local (L) vs. global (G), congruent (con) vs. incongruent (inc), by group (RP=blue vs. AP=green)") +
  coord_cartesian(ylim=c(-2,2))+
  xlim(labels=c("G_con_0","G_inc_0","L_con_0","L_inc_0",
                "G_con_1","G_inc_1","L_con_1","L_inc_1"))+
  theme(axis.text.x=element_text(color="black", size=10),
        axis.text.y=element_text(color="black", size=10),
        axis.title=element_text(face="bold",size=12),
        title=element_text(size=13),panel.grid.major.y = element_line(colour = "grey"))+
        annotate(geom="text",x=7.5,y=-1.8, label="Means & Confidence Intervals")



            


##mixed hlm 2(inc/con)x2(L/G)x 2(AP/RP) auditiv
allresults_longaud<- gather(allresults, time, value, aud_SACS_Linc, aud_SACS_Lcon,aud_SACS_Ginc,aud_SACS_Gcon)
LG<-character(144)
LG[1:72]<-"L"
LG[73:144]<- "G"
congruency<-character(144)
congruency[1:36]<-"inc"
congruency[73:108]<- "inc"
congruency[37:72]<-"con"
congruency[109:144]<-"con"
allresults_longaud <- data.frame(allresults_longaud,LG,congruency)
#lmer(LG ~timeLG+(timeLG|APdef)+timeL+(timeL|APdef)+timeG+(timeG|APdef)+APdef, data = allresults_long2)
#mixed ANOVA
aov_aud <- aov(value ~ APdef*LG*congruency+Error(code/(LG*congruency)), data=allresults_longaud)
summary(aov_aud)
boxplot(value ~ LG*congruency* APdef, data=allresults_longaud, main="AGLT")
#errorplot
means_aud<-tapply(X=allresults_longaud$value,
                  INDEX=list(allresults_longaud$congruency,allresults_longaud$LG,
                             allresults_longaud$APdef),FUN=mean)
sd_aud<-tapply(X=allresults_longaud$value,
               INDEX=list(allresults_longaud$congruency,allresults_longaud$LG,
                          allresults_longaud$APdef),FUN=sd)
length_xaud<-tapply(X=allresults_longaud$value,
               INDEX=list(allresults_longaud$congruency,allresults_longaud$LG,
                          allresults_longaud$APdef),FUN=length)
se_aud<-sd_aud/sqrt(length_xaud)
descr_aud<-cbind(group=c(1,2,3,4,5,6,7,8),means_aud,se_aud)
rownames(descr_aud)<-c("G_con_0","G_inc_0","L_con_0","L_inc_0",
                       "G_con_1","G_inc_1","L_con_1","L_inc_1")
descr_aud<-data.frame(descr_aud)
h<-ggplot(descr_aud,aes(group,means_aud,ymin=means_aud-1.96*se_aud,ymax=means_aud+1.96*se_aud))
h+geom_point(size=3)+geom_errorbar(color=c("blue","blue","blue","blue","#00CC33","#00CC33","#00CC33","#00CC33"),size=0.7, width=0.5)+theme_classic()+ labs(x="Condition",
                                            y= "Speed-accuracy-composite-Score
(SACS)", 
                                            title="AGLT", subtitle="local (L) vs. global (G), congruent (con) vs. incongruent (inc), by group (RP=blue vs. AP=green)")+
  coord_cartesian(ylim=c(-2,2))+
  xlim(labels=c("G_con_0","G_inc_0","L_con_0","L_inc_0",
                "G_con_1","G_inc_1","L_con_1","L_inc_1"))+
  theme(axis.text.x=element_text(color="black", size=10),
        axis.text.y=element_text(color="black", size=10),
        axis.title=element_text(face="bold",size=12),
        title=element_text(size=13),panel.grid.major.y = element_line(colour = "grey"))+
  annotate(geom="text",x=7.5,y=-1.8, label="Means & Confidence Intervals")








##mixed hlm 4(d0,6,12,24)x 2(AP/RP) AEFT
allresults_longAEFT <- gather(allresults, separation, value, d0,d6,d12,d24)
#lmer(LG ~timeLG+(timeLG|APdef)+timeL+(timeL|APdef)+timeG+(timeG|APdef)+APdef, data = allresults_long2)
#mixed ANOVA
aov_AEFT <- aov(value ~ APdef*separation+Error(code/separation), data=allresults_longAEFT)
boxplot(value ~ separation*APdef, data=allresults_longAEFT, main="AEFT")
summary(aov_AEFT)
#errorbars
means_AEFT<-tapply(X=allresults_longAEFT$value,
                  INDEX=list(allresults_longAEFT$separation,
                             allresults_longAEFT$APdef),FUN=mean)
sd_AEFT<-tapply(X=allresults_longAEFT$value,
               INDEX=list(allresults_longAEFT$separation,
                          allresults_longAEFT$APdef),FUN=sd)
dim(sd_AEFT) <- c(8,1)
dim(means_AEFT)<-c(8,1)
length_xAEFT<-tapply(X=allresults_longAEFT$value,
                 INDEX=list(allresults_longAEFT$separation,
                            allresults_longAEFT$APdef),FUN=length)
dim(length_xAEFT)<-c(8,1)
se_AEFT<-sd_AEFT/sqrt(length_xAEFT)
descr_AEFT<-cbind(group=c(1,2,3,4,5,6,7,8),means_AEFT,se_AEFT)#first rearrange dataframes! tbd!
rownames(descr_AEFT)<-c("d0_R","d6_R","d12_R","d24_R",
                       "d0_A","d6_A","d12_A","d24_A")
descr_AEFT<-data.frame(descr_AEFT)
k<-ggplot(descr_AEFT,aes(group,means_AEFT,ymin=means_AEFT-1.96*se_AEFT,ymax=means_AEFT+1.96*se_AEFT))
k+geom_point()+geom_errorbar(group=c(1,2,3,4,5,6,7,8),color=c("blue","blue","blue","blue","#00CC33","#00CC33","#00CC33","#00CC33"), size=0.7)+theme_classic()+ labs(x="Condition",
                                                y= "Sensitivity to detect target melody (d')", 
                                                title="AEFT", subtitle="Mean & Confidence intervals for 0,6,12 and 24 semitone separation by group (RP=blue, AP=green)")+
  coord_cartesian(ylim=c(-2,2))+
  xlim(labels=c("G_con_RP","L_con_RP","G_inc_RP","L_inc_RP",
                "G_con_AP","L_con_AP","G_inc_AP","L_inc_AP"))+
  theme(axis.text.x=element_text(color="black", size=10),
        axis.text.y=element_text(color="black", size=10),
        axis.title=element_text(face="bold",size=10),
        title=element_text(size=12),panel.grid.major.y = element_line(colour = "grey"))
