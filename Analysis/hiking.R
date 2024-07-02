library(readr)
library(ez)
library(dplyr)
library(magrittr)
library(psychReport)
hikingB <- read_delim("hikingB.csv", ";", escape_double = FALSE, trim_ws = TRUE)

hikingB$condition_numbers <-hikingB$condition
hikingB$condition_numbers<-as.factor(hikingB$condition_numbers)
levels(hikingB$condition_numbers) <- c(1, 2, 3, 4)


ezA <- ezANOVA(hikingB,Borg,ParticipantID,within=condition, return_aov = TRUE, detailed = TRUE )
ezA
model<-aov(Borg ~ condition + (1|ParticipantID), data=hikingB)
TukeyHSD(model)
boxplot(Borg ~ condition, data=hikingB)
fValueString(ezA,"condition")

hiking <- read_delim("hiking.csv",";", escape_double = FALSE, trim_ws = TRUE)
hiking$condition<-as.factor(hiking$condition)
levels(hiking$condition) <- c("Hiking", "Chairlift", "Charging", "Instant")

hiking[17:24]=hiking[17:24]+4
hiking$creepySUM=0
hiking$creepySUM
hiking$creepySUM =(hiking$CREEPY_IMMORAL_INTENTIONS+hiking$CREEPY_UNETHICAL_DESIGN)*1.5+hiking$CREEPY_PEOPLE_LAUGH_AT_ME+hiking$CREEPY_UNEASY_IN_PUBLIC+hiking$CREEPY_SYSTEM_BIZARRE+(7-hiking$CREEPY_SYSTEM_LOOKS_AS_EXPECTED)+hiking$CREEPY_DONT_KNOW_PURPOSE+(7-hiking$CREEPY_CLEAR_PURPOSE)

hiking<-data.frame(hiking)

ezANOVA(hiking,creepySUM,ParticipantID,within=condition)
model<-aov(creepySUM ~ condition + (1|ParticipantID), data=hiking)
TukeyHSD(model)
boxplot(creepySUM ~ condition, data=hiking)

hiking$SMSSUM = rowSums(hiking[,grep("SMS_", names(hiking))])
ezANOVA(hiking,SMSSUM,ParticipantID,within=condition)
model<-aov(SMSSUM ~ condition + (1|ParticipantID), data=hiking)
TukeyHSD(model)
boxplot(SMSSUM ~ condition, data=hiking)

attach(hiking)
hiking$PANAS_P = EMOTION_INTERESTED + EMOTION_EXCITED + EMOTION_STRONG + EMOTION_ENTHUSIASTIC + EMOTION_PROUD + EMOTION_ALERT + EMOTION_INSPIRED + EMOTION_DETERMINED + EMOTION_ATTENTIVE + EMOTION_ACTIVE
hiking$PANAS_N = EMOTION_DISTRESSED + EMOTION_UPSET+ EMOTION_GUILTY+ EMOTION_HOSTILE+ EMOTION_SCARED+ EMOTION_IRRITABLE+ EMOTION_ASHAMED+ EMOTION_NERVOUS+ EMOTION_JITTERY+ EMOTION_AFRAID
detach(hiking)

ezANOVA(hiking,PANAS_P,ParticipantID,within=condition)
model<-aov(PANAS_P ~ condition + (1|ParticipantID), data=hiking)
TukeyHSD(model)
boxplot(PANAS_P ~ condition, data=hiking)

ezANOVA(hiking,PANAS_N,ParticipantID,within=condition)
model<-aov(PANAS_N ~ condition + (1|ParticipantID), data=hiking)
TukeyHSD(model)
boxplot(PANAS_N ~ condition, data=hiking)


hiking$TotalTLX = rowSums(hiking[,grep("TLX_", names(hiking))])
ezANOVA(hiking,TotalTLX,ParticipantID,within=condition)
model<-aov(TotalTLX ~ condition + (1|ParticipantID), data=hiking)
TukeyHSD(model)
boxplot(TotalTLX ~ condition, data=hiking)

ezANOVA(hiking,TLX_PERFORMANCE,ParticipantID,within=condition)
model<-aov(TLX_PERFORMANCE ~ condition + (1|ParticipantID), data=hiking)
TukeyHSD(model)
boxplot(TLX_PERFORMANCE ~ condition, data=hiking)

attach(hiking)
hiking$IPQTotal = (IPQ_AwareRealWorld + IPQ_SeemingReal + IPQ_ActingVirtualSpace+ IPQ_Consistency+ IPQ_RealnessCompared -IPQ_NotPresent- IPQ_NotAwareRealWorld+ IPQ_BeingThere+ IPQ_VirtualWorldSurrounded+ IPQ_Present+ IPQ_Attention+ IPQ_Realistic+ IPQ_PerceivingPictures+ IPQ_Captivation)/14
detach(hiking)

ezANOVA(hiking,IPQTotal,ParticipantID,within=condition)
model<-aov(IPQTotal ~ condition + (1|ParticipantID), data=hiking)
TukeyHSD(model)
boxplot(IPQTotal ~ condition, data=hiking)



library(ggplot2)
library(ggsignif)

# Create a lookup table using a named vector
lookup_table <- c("Hiking" = 1, "Chairlift" = 2, "Charging" = 3, "Instant" = 4)

# Map the values to labels using the lookup table
hikingB$condition <- names(lookup_table)[match(hikingB$condition, lookup_table)]

hikingB$condition<-factor(hikingB$condition, levels=c("Hiking", "Chairlift", "Charging", "Instant"))
hikingB$condition

ggplot(hikingB) +
  aes(x = condition, y = Borg, fill = condition) +
  geom_boxplot(shape = "circle") +
  scale_fill_hue(direction = 1) +
  labs(x = "Condition", y = "Borg RPE") +
  theme_classic()+
  geom_signif(comparisons = list(c("Hiking","Chairlift")),
              map_signif_level=TRUE, y_position = 17.5, tip_length = 0.01, vjust = 0.2, textsize=8) +
  geom_signif(comparisons = list(c("Hiking","Charging")),
              map_signif_level=TRUE, y_position = 18.5, tip_length = 0.01, vjust = 0.2, textsize=8) +
  geom_signif(comparisons = list(c("Hiking","Instant")),
              map_signif_level=TRUE, y_position = 19.5, tip_length = 0.01, vjust = 0.2, textsize=8)+
  theme(legend.position = "none", text = element_text(size=25)) 
ggsave("borg_new.pdf",device="pdf",width=9,height=7.5,units="in")


ggplot(hiking) +
  aes(x = condition, y = SMSSUM, fill = condition) +
  geom_boxplot(shape = "circle") +
  scale_fill_hue(direction = 1) +
  labs(x = "Condition", y = "State Mindfluness Score") +
  ggthemes::theme_pander(base_size = 20) +
  theme_classic()+
  geom_signif(comparisons = list(c("Hiking","Chairlift")),
              map_signif_level=TRUE, y_position = 80, tip_length = 0.01, vjust = 0.2, textsize=8) +
  geom_signif(comparisons = list(c("Hiking","Charging")),
              map_signif_level=TRUE, y_position = 85, tip_length = 0.01, vjust = 0.2, textsize=8) +
  geom_signif(comparisons = list(c("Hiking","Instant")),
              map_signif_level=TRUE, y_position = 90, tip_length = 0.01, vjust = 0.2, textsize=8)+
  theme(legend.position = "none", text = element_text(size=25)) +
  scale_y_continuous(limits=c(0,100))
ggsave("sms_new.pdf",device="pdf",width=9,height=7.5,units="in")

ggplot(hiking) +
  aes(x = condition, y = PANAS_P, fill = condition) +
  geom_boxplot(shape = "circle") +
  scale_fill_hue(direction = 1) +
  labs(x = "Condition", y = "Positive Emotion Score") +
  ggthemes::theme_pander(base_size = 20) +
  theme_classic()+
  geom_signif(comparisons = list(c("Hiking","Chairlift")),
              map_signif_level=TRUE, y_position = 44, tip_length = 0.01, vjust = 0.2, textsize=8) +
  geom_signif(comparisons = list(c("Hiking","Charging")),
              map_signif_level=TRUE, y_position = 47, tip_length = 0.01, vjust = 0.2, textsize=8) +
  geom_signif(comparisons = list(c("Hiking","Instant")),
              map_signif_level=TRUE, y_position = 50, tip_length = 0.01, vjust = 0.2, textsize=8)+
  theme(legend.position = "none", text = element_text(size=25)) 
ggsave("panas_p_new.pdf",device="pdf",width=9,height=7.5,units="in")

ggplot(hiking) +
  aes(x = condition, y = TotalTLX, fill = condition) +
  geom_boxplot(shape = "circle") +
  scale_fill_hue(direction = 1) +
  labs(x = "Condition", y = "TLX Score") +
  ggthemes::theme_pander(base_size = 20) +
  theme_classic()+
  geom_signif(comparisons = list(c("Hiking","Chairlift")),
              map_signif_level=TRUE, y_position = 80, tip_length = 0.01, vjust = 0.2, textsize=8) +
  geom_signif(comparisons = list(c("Hiking","Charging")),
              map_signif_level=TRUE, y_position = 85, tip_length = 0.01, vjust = 0.2, textsize=8) +
  geom_signif(comparisons = list(c("Hiking","Instant")),
              map_signif_level=TRUE, y_position = 90, tip_length = 0.01, vjust = 0.2, textsize=8) +
theme(legend.position = "none", text = element_text(size=25)) +
  scale_y_continuous(limits=c(0,100))
ggsave("tlx_new.pdf",device="pdf",width=9,height=7.5,units="in")

ggplot(hiking) +
  aes(x = condition, y = IPQTotal, fill = condition) +
  geom_boxplot(shape = "circle") +
  scale_fill_hue(direction = 1) +
  labs(x = "Condition", y = "Presence Score") +
  ggthemes::theme_pander(base_size = 20) +
  theme_classic()+
    geom_signif(comparisons = list(c("Hiking","Charging")),
              map_signif_level=TRUE, y_position = 1.5, tip_length = 0.01, vjust = 0.2,  textsize=8)+
  theme(legend.position = "none", text = element_text(size=25)) 
ggsave("ipq_new.pdf",device="pdf",width=18,height=7.5,units="in")

