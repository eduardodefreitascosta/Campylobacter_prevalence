

#Reading data
campy<-read_xlsx(here("Data","Processed","campylo_final.xlsx"),sheet = "final")

count_bull<-campy%>%
  group_by(farm_ID)%>%
  summarise(n_bull=n())


# Group per farm
campy2<-campy%>%
  group_by(farm_ID, meso,vaccine,exploration,bull_sale,art_in,tour_repa,CGB)%>%
  summarise(total=n(),pos=sum(result),N_bulls=mean(N_bulls))

# Within herd sensitivity of sampling
campy2$se<-phyper(0,campy2$N_bulls*0.3,campy2$N_bulls-(campy2$N_bulls*0.3),campy2$total,lower.tail=FALSE)
campy2$outcome<-ifelse(campy2$pos>0,1,0)

ggplot(campy2,aes(x=se))+
  geom_histogram()



#save file
write.csv2(campy2,here("Outputs","Campy2.csv"))

###############
# Descriptive #
###############

## General prevalence
prev_general<-glm(outcome~1,family = binomial(link="logit"),data=campy2)

#prevalence
1/(1+exp(-prev_general$coefficients))

#95% CI
1/(1+exp(-confint(prev_general)))

## Graphics for the paper

my_lab<-c("COc", "COr", "PoA", "NE", "NW", "SE", "SW")
my_lab1<-c('Cfv-Negative', 'Cfv-Positive')


# Mesoregion
a<-ggplot(campy2,aes(x =factor(meso),fill=factor(outcome))) +
  ggtitle("Region")+
  theme_minimal()+
  geom_bar(position="fill")+
  geom_text(aes(label = ..count..), stat = "count" ,position = position_fill(.5),size=3)+
  #theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5))+
  xlab(" ") + ylab(" ")+
  labs(fill =substitute(" "))+
  theme(
    axis.title.x = element_text( size=8),
    axis.title.y = element_text( size=9),
    axis.text.x=element_text(size=rel(0.95),vjust = 1,angle = 0, hjust = 0.5),
    axis.text.y=element_text(size=rel(1.2),hjust = 4),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 15),
    plot.title = element_text(size = 12),
    plot.caption = element_text(hjust = 0,size = 14))+
    scale_fill_discrete(labels=my_lab1)+
    scale_x_discrete(labels=my_lab)


# Vaccine
b<-ggplot(subset(campy2,!is.na(vaccine)),aes(x =vaccine,fill=factor(outcome))) +
   ggtitle("Vaccine")+
  theme_minimal()+
  geom_bar(position="fill")+
  geom_text(aes(label = ..count..), stat = "count" ,position = position_fill(.5),size=3)+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5))+
  xlab(" ") + ylab(" ")+
  labs(fill =substitute(" "))+
  theme(
    axis.title.x = element_text( size=8),
    axis.title.y = element_text( size=9),
    axis.text.x=element_text(size=rel(1.2),vjust = 1),
    axis.text.y=element_text(size=rel(1.2),hjust = 4),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 15),
    plot.title = element_text(size = 12),
    plot.caption = element_text(hjust = 0,size = 14))+
    scale_fill_discrete(labels=my_lab1)+
    scale_x_discrete(labels=c("No","Yes"))



# Exploration
c<-ggplot(campy2,aes(x =exploration,fill=factor(outcome))) +
  ggtitle("Type of farm")+
  theme_minimal()+
  geom_bar(position="fill")+
  geom_text(aes(label = ..count..), stat = "count" ,position = position_fill(.5),size=3)+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5))+
  xlab(" ") + ylab(" ")+
  labs(fill =substitute(" "))+
  theme(
    axis.title.x = element_text( size=8),
    axis.title.y = element_text( size=9),
    axis.text.x=element_text(size=rel(0.95),vjust = 1),
    axis.text.y=element_text(size=rel(1.2),hjust = 4),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 15),
    plot.title = element_text(size = 12),
    plot.caption = element_text(hjust = 0,size = 14))+
    scale_fill_discrete(labels=my_lab1)+
    scale_x_discrete(labels=c("Complete \ncicle","Raising & \nrestocking"))


# Bull sale
kable(table(campy2$bull_sale))

d<-ggplot(subset(campy2,!is.na(bull_sale)),aes(x =bull_sale,fill=factor(outcome))) +
  ggtitle("Bull sale")+
  theme_minimal()+
  geom_bar(position="fill")+
  geom_text(aes(label = ..count..), stat = "count" ,position = position_fill(.5),size=3)+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5))+
  xlab(" ") + ylab(" ")+
  labs(fill =substitute(" "))+
  theme(
    axis.title.x = element_text( size=8),
    axis.title.y = element_text( size=9),
    axis.text.x=element_text(size=rel(1.2),vjust = 1),
    axis.text.y=element_text(size=rel(1.2),hjust = 4),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 15),
    plot.title = element_text(size = 12),
    plot.caption = element_text(hjust = 0,size = 14))+
    scale_fill_discrete(labels=my_lab1)+
    scale_x_discrete(labels=c("No","Yes"))



#Artificial insemination
#No analysis because multicolinearity

# Bull repass
kable(table(campy2$tour_repa))

e<-ggplot(campy2,aes(x =tour_repa,fill=factor(outcome))) +
  ggtitle("Natural service")+
  theme_minimal()+
  geom_bar(position="fill")+
  geom_text(aes(label = ..count..), stat = "count" ,position = position_fill(.5),size=3)+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5))+
  xlab(" ") + ylab(" ")+
  labs(fill =substitute(" "))+
  theme(
    axis.title.x = element_text( size=9),
    axis.title.y = element_text( size=8),
    axis.text.x=element_text(size=rel(1.2),vjust = 1),
    axis.text.y=element_text(size=rel(1.2),hjust = 4),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 15),
    plot.title = element_text(size = 12),
    plot.caption = element_text(hjust = 0,size = 14))+
    scale_fill_discrete(labels=my_lab1)+
    scale_x_discrete(labels=c("No","Yes"))


# Cfv testing
kable(table(campy2$CGB))

f<-ggplot(campy2,aes(x =CGB,fill=factor(outcome))) +
  ggtitle("Cfv test")+
  theme_minimal()+
  geom_bar(position="fill")+
  geom_text(aes(label = ..count..), stat = "count" ,position = position_fill(.5),size=3)+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5))+
  xlab(" ") + ylab(" ")+
  labs(fill =substitute(" "))+
  theme(
    axis.title.x = element_text( size=9),
    axis.title.y = element_text( size=8),
    axis.text.x=element_text(size=rel(1.2),vjust = 1),
    axis.text.y=element_text(size=rel(1.2),hjust = 4),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 15),
    plot.title = element_text(size = 12),
    plot.caption = element_text(hjust = 0,size = 14))+
    scale_fill_discrete(labels=my_lab1)+
    scale_x_discrete(labels=c("No","Yes"))

#Arrange the plots
ggarrange(a,b,c,d,e,f,ncol=2, nrow=3, common.legend = TRUE, legend="bottom")

#Save figure in tiff
ggsave(file=here("Figures","Fig1.tiff"),bg="white",width=18,height = 18,units = "cm",last_plot(),dpi=600,compression = 'lzw')


###################################
# Logistic regression (inference) #
###################################

##Backwards selection
#Step1 (full)
mod1<-glm(outcome~meso+vaccine+exploration+bull_sale+tour_repa+CGB+N_bulls,data=campy2,family = binomial(link="logit"))
Anova(mod1)
summary(mod1)
vif(mod1)

#Step2 drop exploration
mod2<-glm(outcome~vaccine+meso+bull_sale+tour_repa+CGB+N_bulls,data=campy2,family = binomial(link="logit"))
Anova(mod2)
summary(mod2)

#Step3 drop region
mod3<-glm(outcome~vaccine+bull_sale+tour_repa+CGB+N_bulls,data=campy2,family = binomial(link="logit"))
Anova(mod3)
summary(mod3)

#Step4 drop vaccine
mod4<-glm(outcome~bull_sale+tour_repa+CGB+N_bulls,data=campy2,family = binomial(link="logit"))
Anova(mod4)
summary(mod4)


#Step5 drop bull_sale
mod5<-glm(outcome~tour_repa+CGB+N_bulls,data=campy2,family = binomial(link="logit"))
Anova(mod5)
summary(mod5)
vif(mod5)


emmeans(mod5,pairwise~tour_repa,type="response")
emmeans(mod5,pairwise~CGB,type="response")



