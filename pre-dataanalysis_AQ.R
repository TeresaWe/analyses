# Dataanalysis group differences and autistic traits


#libraries
library("dplyr", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.3")
library("psych", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.3")
library("ggplot2", lib.loc="/usr/local/lib/R/site-library")
library("tidyr", lib.loc="/usr/local/lib/R/site-library")
#Struktur des Datensatzes
str(allresults)
#MAD->AP_sums
lm<-lm(MAD~AP_sums, data=allresults)
#SDfoM->AP_sums
lm2<-lm(AP_sums~SDfoM, data=allresults)
summary(lm)
summary(lm2)
ggplot(data=allresults,aes(x=AP_sums, y=MAD))+
  geom_point(mapping=aes(x=AP_sums, y=MAD))+
  geom_abline(aes(intercept=290.329, slope=-8.226))+
  geom_point(aes(size=SDfoM))+theme_classic()+labs(x="Total of correct identified (labeled) sine waves (max=36)",
                                              y= "MAD (Mean absolute deviation from target tone)", 
                                              title="Validation of Pitch Adjustment Test", 
                                              subtitle="Correlation between MAD, SDfoM and Pitch Identification Score")+
  coord_cartesian(ylim=c(0,600),xlim=c(0,36))+
  theme(axis.text.x=element_text(color="black", size=10),
        axis.text.y=element_text(color="black", size=10),
        legend.title=element_text(color="black", size=10,face="bold"),
        legend.text=element_text(color="black", size=10),
        axis.title=element_text(face="bold",size=10),
        title=element_text(size=12),panel.grid.major.y = element_line(colour = "grey"))

#SDfoM->AQ_Score
lm3<-lm(AQ_Score~SDfoM, data=allresults)
summary(lm3)
ggplot(data=allresults,aes(x=SDfoM, y=AQ_Score))+
  geom_point(mapping=aes(x=SDfoM, y=AQ_Score))+
  geom_abline(aes(intercept=20.400532, slope=-0.009088), size=1)+
  geom_abline(aes(intercept=32, slope=0),color="red")+
  geom_abline(aes(intercept=26, slope=0),color="darkgreen")+
  geom_abline(aes(intercept=16.4, slope=0),color="blue")+
  geom_point(aes(size=starting_age))+theme_classic()+labs(x="SDfoM",
                                            y= "AQ_Score (Autism-Spectrum-Quotient)", 
                                            title="Pitch-label-accuracy and autistic traits", 
                                            subtitle="red: threshold for diagnosis, green: high sensitivity threshold, blue: population mean")+
  coord_cartesian(ylim=c(0,36),xlim=c(0,400))+
  theme(axis.text.x=element_text(color="black", size=10),
        axis.text.y=element_text(color="black", size=10),
        legend.title=element_text(color="black", size=10,face="bold"),
        legend.text=element_text(color="black", size=10),
        axis.title=element_text(face="bold",size=10),
        title=element_text(size=12),panel.grid.major.y = element_line(colour = "grey"))
####AP_sums
lm4<-lm(AQ_Score~AP_sums, data=allresults)
summary(lm4)
##plot
ggplot(data=allresults,aes(x=AP_sums, y=AQ_Score))+
  geom_point(mapping=aes(x=AP_sums, y=AQ_Score))+
  geom_abline(aes(intercept=15.89662, slope=0.17526), size=1)+
  geom_abline(aes(intercept=32, slope=0),color="red")+
  geom_abline(aes(intercept=26, slope=0),color="darkgreen")+
  geom_abline(aes(intercept=16.4, slope=0),color="blue")+
  geom_point(aes(color=starting_age, size=`Sex (1=w,0=m)`))+theme_classic()+labs(x="AP_sums",
                                                           y= "AQ_Score (Autism-Spectrum-Quotient)", 
                                                           title="Pitch identification and autistic traits", 
                                                           subtitle="red: threshold for diagnosis, green: high sensitivity threshold, blue: population mean")+
  coord_cartesian(ylim=c(0,36),xlim=c(0,36))+
  theme(axis.text.x=element_text(color="black", size=10),
        axis.text.y=element_text(color="black", size=10),
        legend.title=element_text(color="black", size=10,face="bold"),
        legend.text=element_text(color="black", size=10),
        axis.title=element_text(face="bold",size=10),
        title=element_text(size=12),panel.grid.major.y = element_line(colour = "grey"))
