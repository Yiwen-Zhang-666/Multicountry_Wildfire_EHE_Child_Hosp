library(tidyverse)
library(ggplot2)
library(dplyr)


# Figure 2
Fig2 <- read_excel("Figure2_3.xlsx")
Fig2$exposure <- factor(Fig2$exposure, levels = c("HFPP_alone","EHE_alone","compound"))
Fig2$outcome <- factor(Fig2$outcome, levels = c("allcause_all_total","resp_all_total","cvd_all_total","infect_all_total","cancer_all_total",
                                                    "digest_all_total","renal_all_total","diabetes_all_total","mental_all_total","nervous_all_total"))

## panel 1
Fig2_A <- Fig2 %>%
  na.omit() %>%
  filter(group  == "country")
Fig2_A$subgroup <- factor(Fig2_A$subgroup, levels = c("AUS","BRA","CAN","CHL","THA"))
Fig2_A$exposure <- factor(Fig2_A$exposure, levels = c("HFPP_along","EHE_alone","compound"))
group.colors <- c(`AUS` = "#FF9149", `BRA` ="#3D90D7", `CAN` ="#FF0B55", `CHL` ="#8C00FF", `THA` ="#08CB00")


ggplot(Fig2_A,aes(x=subgroup, y = RR,color = subgroup)) +
  facet_grid(cols = vars(outcome))+
  geom_errorbar(aes(x=subgroup, ymin=RRlow,ymax=RRhigh,group = exposure),position = position_dodge(width = 0.7),width = 0.45)+
  geom_point(aes(x = subgroup, y = RR,group = exposure,shape = exposure),position = position_dodge(width = 0.7),size = 2)+
  geom_hline(aes(yintercept=1),linetype="dashed",color = "#A6A9B6") +
  ggtitle('')+
  xlab("")+
  ylab("Relative Risk (95% CI)")+
  theme_bw()+
  scale_color_manual(values =group.colors)+
  theme(text = element_text(size=14),
        plot.title=element_text(size=15,face = "bold",family = 'serif'),
        axis.text = element_text(size = 14,colour = "black",family = 'serif'),
        axis.text.x = element_blank(),
        axis.title.x  = element_text(size = 14,colour = "black",family = 'serif'),
        axis.title.y  = element_text(size = 14,colour = "black",family = 'serif'),
        axis.title.y.right = element_blank(), 
        axis.text.y.right = element_blank(),
        axis.text.y = element_text(margin = margin(r = 0)),
        panel.spacing = unit(0, "mm"),
        strip.background = element_blank(), 
        legend.text = element_text(size = 12,colour = "black",family = 'serif'),
        legend.title = element_text(size = 12,colour = "black",family = 'serif'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())

## panel 2
RERI <- read_excel("Figure2_4.xlsx") ## load RERI data
RERI$outcome <- factor(RERI$outcome, levels = c("allcause_all_total","resp_all_total","cvd_all_total","infect_all_total","cancer_all_total",
                                                "digest_all_total","renal_all_total","diabetes_all_total","mental_all_total","nervous_all_total"))

Fig2_B <- RERI %>%
  na.omit() %>%
  filter(group  == "country")
Fig2_B$subgroup <- factor(Fig2_B$subgroup, levels = c("AUS","BRA","CAN","CHL","THA"))
group.colors <- c(`AUS` = "#FF9149", `BRA` ="#3D90D7", `CAN` ="#FF0B55", `CHL` ="#8C00FF", `THA` ="#08CB00")

ggplot(Fig2_B,aes(x=subgroup, y = RERI,color = subgroup,group = subgroup, fill = subgroup)) +
  facet_grid(cols = vars(outcome))+
  geom_errorbar(aes(x=subgroup, ymin=RERIlow,ymax=RERIhigh,group = subgroup),position = position_dodge(width = 0.7),width = 0.45)+
  geom_bar(aes(x = subgroup, y = RERI),width = 0.5,alpha = 0.88,stat="identity") + 
  geom_hline(aes(yintercept=0),linetype="dashed",color = "#A6A9B6") +
  ggtitle('')+
  xlab("")+
  ylab("Relative Risk (95% CI)")+
  theme_bw()+
  scale_color_manual(values =group.colors,
                     aesthetics = c("colour", "fill"))+
  theme(text = element_text(size=14),
        plot.title=element_text(size=15,face = "bold",family = 'serif'),
        axis.text = element_text(size = 14,colour = "black",family = 'serif'),
        axis.text.x = element_blank(),
        axis.title.x  = element_text(size = 14,colour = "black",family = 'serif'),
        axis.title.y  = element_text(size = 14,colour = "black",family = 'serif'),
        axis.title.y.right = element_blank(), 
        axis.text.y.right = element_blank(),
        axis.text.y = element_text(margin = margin(r = 0)),
        panel.spacing = unit(0, "mm"),
        strip.background = element_blank(), 
        legend.text = element_text(size = 12,colour = "black",family = 'serif'),
        legend.title = element_text(size = 12,colour = "black",family = 'serif'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) 


# Figure 3
Fig3 <- read_excel("Figure2_3.xlsx")
Fig3$exposure <- factor(Fig3$exposure, levels = c("HFPP_alone","EHE_alone","compound"))
Fig3$outcome <- factor(Fig3$outcome, levels = c("allcause_all_total","resp_all_total","cvd_all_total","infect_all_total","cancer_all_total",
                                                "digest_all_total","renal_all_total","diabetes_all_total","mental_all_total","nervous_all_total"))
## panel 1
Fig3_A <- Fig3 %>%
  na.omit() %>%
  filter(group  == "sex" & exposure == "compound")
Fig3_A$subgroup <- factor(Fig3_A$subgroup, levels = c("female","male"))
group.colors <- c(`female` = "#FF9149", `male` ="#3D90D7")

ggplot(Fig2_A,aes(x=subgroup, y = RR,color = subgroup)) +
  facet_grid(cols = vars(outcome))+
  geom_errorbar(aes(x=subgroup, ymin=RRlow,ymax=RRhigh),width = 0.3)+
  geom_point(aes(x = subgroup, y = RR),size = 2.2)+
  scale_y_continuous(limits = c(0.80,1.85),breaks = c(0.80,0.90,1.00,1.10,1.20,1.30,1.40,1.50,1.60,1.70,1.80))+
  geom_hline(aes(yintercept=1),linetype="dashed",color = "#A6A9B6") +
  ggtitle('')+
  xlab("")+
  ylab("Relative Risk (95% CI)")+
  theme_bw()+
  scale_color_manual(values =group.colors)+
  theme(text = element_text(size=14),
        plot.title=element_text(size=15,face = "bold",family = 'serif'),
        axis.text = element_text(size = 14,colour = "black",family = 'serif'),
        axis.text.x = element_blank(),
        axis.title.x  = element_text(size = 14,colour = "black",family = 'serif'),
        axis.title.y  = element_text(size = 14,colour = "black",family = 'serif'),
        axis.title.y.right = element_blank(), 
        axis.text.y.right = element_blank(),
        axis.text.y = element_text(margin = margin(r = 0)),
        panel.spacing = unit(0, "mm"),
        strip.background = element_blank(), 
        legend.text = element_text(size = 12,colour = "black",family = 'serif'),
        legend.title = element_text(size = 12,colour = "black",family = 'serif'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) 


## panel 2
Fig3_B <- Fig3 %>%
  na.omit() %>%
  filter(group  == "age" & exposure == "compound")
Fig3_B$subgroup <- factor(Fig3_B$subgroup, levels = c("infant","chi","ado"))
group.colors <- c(`infant` = "#FF9149", `chi` ="#3D90D7", `ado` ="#FF0B55")

ggplot(Fig3_B,aes(x=subgroup, y = RR,color = subgroup)) +
  facet_grid(cols = vars(outcome))+
  geom_errorbar(aes(x=subgroup, ymin=RRlow,ymax=RRhigh),width = 0.3)+
  geom_point(aes(x = subgroup, y = RR),size = 2.2)+
  scale_y_continuous(limits = c(0.80,1.85),breaks = c(0.80,0.90,1.00,1.10,1.20,1.30,1.40,1.50,1.60,1.70,1.80))+
  geom_hline(aes(yintercept=1),linetype="dashed",color = "#A6A9B6") +
  ggtitle('')+
  xlab("")+
  ylab("Relative Risk (95% CI)")+
  theme_bw()+
  scale_color_manual(values =group.colors)+
  theme(text = element_text(size=14),
        plot.title=element_text(size=15,face = "bold",family = 'serif'),
        axis.text = element_text(size = 14,colour = "black",family = 'serif'),
        axis.text.x = element_blank(),
        axis.title.x  = element_text(size = 14,colour = "black",family = 'serif'),
        axis.title.y  = element_text(size = 14,colour = "black",family = 'serif'),
        axis.title.y.right = element_blank(), 
        axis.text.y.right = element_blank(),
        axis.text.y = element_text(margin = margin(r = 0)),
        panel.spacing = unit(0, "mm"),
        strip.background = element_blank(), 
        legend.text = element_text(size = 12,colour = "black",family = 'serif'),
        legend.title = element_text(size = 12,colour = "black",family = 'serif'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) 


## panel 3
Fig3_C <- Fig3 %>%
  na.omit() %>%
  filter(group  == "GDP" & exposure == "compound")
Fig3_C$subgroup <- factor(Fig3_C$subgroup, levels = c("Low","Medium","High"))
group.colors <- c(`Low` = "#FF9149", `Medium` ="#3D90D7", `High` ="#FF0B55")

ggplot(Fig3_C,aes(x=subgroup, y = RR,color = subgroup)) +
  facet_grid(cols = vars(outcome))+
  geom_errorbar(aes(x=subgroup, ymin=RRlow,ymax=RRhigh),width = 0.3)+
  geom_point(aes(x = subgroup, y = RR),size = 2.2)+
  scale_y_continuous(limits = c(0.80,1.85),breaks = c(0.80,0.90,1.00,1.10,1.20,1.30,1.40,1.50,1.60,1.70,1.80))+
  geom_hline(aes(yintercept=1),linetype="dashed",color = "#A6A9B6") +
  ggtitle('')+
  xlab("")+
  ylab("Relative Risk (95% CI)")+
  theme_bw()+
  scale_color_manual(values =group.colors)+
  theme(text = element_text(size=14),
        plot.title=element_text(size=15,face = "bold",family = 'serif'),
        axis.text = element_text(size = 14,colour = "black",family = 'serif'),
        axis.text.x = element_blank(),
        axis.title.x  = element_text(size = 14,colour = "black",family = 'serif'),
        axis.title.y  = element_text(size = 14,colour = "black",family = 'serif'),
        axis.title.y.right = element_blank(),
        axis.text.y.right = element_blank(),
        axis.text.y = element_text(margin = margin(r = 0)),
        panel.spacing = unit(0, "mm"),
        strip.background = element_blank(), 
        legend.text = element_text(size = 12,colour = "black",family = 'serif'),
        legend.title = element_text(size = 12,colour = "black",family = 'serif'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) 
  

## panel 4
Fig3_D <- Fig3 %>%
  na.omit() %>%
  filter(group  == "BUILT" & exposure == "compound")
Fig3_D$subgroup <- factor(Fig3_D$subgroup, levels = c("Low","Medium","High"))
group.colors <- c(`Low` = "#FF9149", `Medium` ="#3D90D7", `High` ="#FF0B55")

ggplot(Fig3_D,aes(x=subgroup, y = RR,color = subgroup)) +
  facet_grid(cols = vars(outcome))+
  geom_errorbar(aes(x=subgroup, ymin=RRlow,ymax=RRhigh),width = 0.3)+
  geom_point(aes(x = subgroup, y = RR),size = 2.2)+
  scale_y_continuous(limits = c(0.80,1.85),breaks = c(0.80,0.90,1.00,1.10,1.20,1.30,1.40,1.50,1.60,1.70,1.80))+
  geom_hline(aes(yintercept=1),linetype="dashed",color = "#A6A9B6") +
  ggtitle('')+
  xlab("")+
  ylab("Relative Risk (95% CI)")+
  theme_bw()+
  scale_color_manual(values =group.colors)+
  theme(text = element_text(size=14),
        plot.title=element_text(size=15,face = "bold",family = 'serif'),
        axis.text = element_text(size = 14,colour = "black",family = 'serif'),
        axis.text.x = element_blank(),
        axis.title.x  = element_text(size = 14,colour = "black",family = 'serif'),
        axis.title.y  = element_text(size = 14,colour = "black",family = 'serif'),
        axis.title.y.right = element_blank(), 
        axis.text.y.right = element_blank(),
        axis.text.y = element_text(margin = margin(r = 0)),
        panel.spacing = unit(0, "mm"),
        strip.background = element_blank(), 
        legend.text = element_text(size = 12,colour = "black",family = 'serif'),
        legend.title = element_text(size = 12,colour = "black",family = 'serif'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())


# Figure 4
Fig4 <- read_excel("Figure2_4.xlsx")
Fig4$outcome <- factor(Fig4$outcome, levels = c("allcause_all_total","resp_all_total","cvd_all_total","infect_all_total","cancer_all_total",
                                                    "digest_all_total","renal_all_total","diabetes_all_total","mental_all_total","nervous_all_total"))

## panel 1
Fig4_A <- Fig4 %>%
  na.omit() %>%
  filter(group  == "sex")
group.colors <- c(`female` = "#FF9149", `male` ="#3D90D7")

ggplot(Fig4_A,aes(x=subgroup, y = RERI,color = subgroup,group = subgroup, fill = subgroup)) +
  facet_grid(cols = vars(outcome))+
  geom_errorbar(aes(x=subgroup, ymin=RERIlow,ymax=RERIhigh,group = subgroup),position = position_dodge(width = 0.7),width = 0.45)+
  geom_bar(aes(x = subgroup, y = RERI),width = 0.5,alpha = 0.88,stat="identity") + 
  geom_hline(aes(yintercept=0),linetype="dashed",color = "#A6A9B6") +
  ggtitle('')+
  xlab("")+
  ylab("Relative Risk (95% CI)")+
  theme_bw()+
  scale_color_manual(values =group.colors,
                     aesthetics = c("colour", "fill"))+
  theme(text = element_text(size=14),
        plot.title=element_text(size=15,face = "bold",family = 'serif'),
        axis.text = element_text(size = 14,colour = "black",family = 'serif'),
        axis.text.x = element_blank(),
        axis.title.x  = element_text(size = 14,colour = "black",family = 'serif'),
        axis.title.y  = element_text(size = 14,colour = "black",family = 'serif'),
        axis.title.y.right = element_blank(), 
        axis.text.y.right = element_blank(),
        axis.text.y = element_text(margin = margin(r = 0)),
        panel.spacing = unit(0, "mm"),
        strip.background = element_blank(), 
        legend.text = element_text(size = 12,colour = "black",family = 'serif'),
        legend.title = element_text(size = 12,colour = "black",family = 'serif'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) 


## panel 2
Fig4_B <- Fig4 %>%
  na.omit() %>%
  filter(group  == "age")
Fig4_B$subgroup <- factor(Fig4_B$subgroup, levels = c("infan","chi","ado"))
group.colors <- c(`infan` = "#FF9149", `chi` ="#3D90D7", `ado` ="#FF0B55")

ggplot(Fig4_B,aes(x=subgroup, y = RERI,color = subgroup,group = subgroup, fill = subgroup)) +
  facet_grid(cols = vars(outcome))+
  geom_errorbar(aes(x=subgroup, ymin=RERIlow,ymax=RERIhigh,group = subgroup),position = position_dodge(width = 0.7),width = 0.45)+
  geom_bar(aes(x = subgroup, y = RERI),width = 0.5,alpha = 0.88,stat="identity") + 
  geom_hline(aes(yintercept=0),linetype="dashed",color = "#A6A9B6") +
  ggtitle('')+
  xlab("")+
  ylab("Relative Risk (95% CI)")+
  theme_bw()+
  scale_color_manual(values =group.colors,
                     aesthetics = c("colour", "fill"))+
  theme(text = element_text(size=14),
        plot.title=element_text(size=15,face = "bold",family = 'serif'),
        axis.text = element_text(size = 14,colour = "black",family = 'serif'),
        axis.text.x = element_blank(),
        axis.title.x  = element_text(size = 14,colour = "black",family = 'serif'),
        axis.title.y  = element_text(size = 14,colour = "black",family = 'serif'),
        axis.title.y.right = element_blank(), 
        axis.text.y.right = element_blank(),
        axis.text.y = element_text(margin = margin(r = 0)),
        panel.spacing = unit(0, "mm"),
        strip.background = element_blank(),
        legend.text = element_text(size = 12,colour = "black",family = 'serif'),
        legend.title = element_text(size = 12,colour = "black",family = 'serif'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())  
 

## panel 3
Fig4_C <- Fig4 %>%
  na.omit() %>%
  filter(group  == "GDP")
Fig2_C$subgroup <- factor(Fig2_C$subgroup, levels = c("Low","Medium","High"))
group.colors <- c(`Low` = "#FF9149", `Medium` ="#3D90D7", `High` ="#FF0B55")

ggplot(Fig4_C,aes(x=subgroup, y = RERI,color = subgroup,group = subgroup, fill = subgroup)) +
  facet_grid(cols = vars(outcome))+
  geom_errorbar(aes(x=subgroup, ymin=RERIlow,ymax=RERIhigh,group = subgroup),position = position_dodge(width = 0.7),width = 0.45)+
  geom_bar(aes(x = subgroup, y = RERI),width = 0.5,alpha = 0.88,stat="identity") + 
  geom_hline(aes(yintercept=0),linetype="dashed",color = "#A6A9B6") +
  ggtitle('')+
  xlab("")+
  ylab("Relative Risk (95% CI)")+
  theme_bw()+
  scale_color_manual(values =group.colors,
                     aesthetics = c("colour", "fill"))+
  theme(text = element_text(size=14),
        plot.title=element_text(size=15,face = "bold",family = 'serif'),
        axis.text = element_text(size = 14,colour = "black",family = 'serif'),
        axis.text.x = element_blank(),
        axis.title.x  = element_text(size = 14,colour = "black",family = 'serif'),
        axis.title.y  = element_text(size = 14,colour = "black",family = 'serif'),
        axis.title.y.right = element_blank(), 
        axis.text.y.right = element_blank(),
        axis.text.y = element_text(margin = margin(r = 0)),
        panel.spacing = unit(0, "mm"),
        strip.background = element_blank(), 
        legend.text = element_text(size = 12,colour = "black",family = 'serif'),
        legend.title = element_text(size = 12,colour = "black",family = 'serif'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())


## panel 4
Fig4_D <- Fig4 %>%
  na.omit() %>%
  filter(group  == "BUILT")
Fig4_D$subgroup <- factor(Fig4_D$subgroup, levels = c("Low","Medium","High"))
group.colors <- c(`Low` = "#FF9149", `Medium` ="#3D90D7", `High` ="#FF0B55")

ggplot(Fig4_D,aes(x=subgroup, y = RERI,color = subgroup,group = subgroup, fill = subgroup)) +
  facet_grid(cols = vars(outcome))+
  geom_errorbar(aes(x=subgroup, ymin=RERIlow,ymax=RERIhigh,group = subgroup),position = position_dodge(width = 0.7),width = 0.45)+
  geom_bar(aes(x = subgroup, y = RERI),width = 0.5,alpha = 0.88,stat="identity") + 
  geom_hline(aes(yintercept=0),linetype="dashed",color = "#A6A9B6") +
  ggtitle('')+
  xlab("")+
  ylab("Relative Risk (95% CI)")+
  theme_bw()+
  scale_color_manual(values =group.colors,
                     aesthetics = c("colour", "fill"))+
  theme(text = element_text(size=14),
        plot.title=element_text(size=15,face = "bold",family = 'serif'),
        axis.text = element_text(size = 14,colour = "black",family = 'serif'),
        axis.text.x = element_blank(),
        axis.title.x  = element_text(size = 14,colour = "black",family = 'serif'),
        axis.title.y  = element_text(size = 14,colour = "black",family = 'serif'),
        axis.title.y.right = element_blank(),
        axis.text.y.right = element_blank(),
        axis.text.y = element_text(margin = margin(r = 0)),
        panel.spacing = unit(0, "mm"),
        strip.background = element_blank(), 
        legend.text = element_text(size = 12,colour = "black",family = 'serif'),
        legend.title = element_text(size = 12,colour = "black",family = 'serif'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) 


# Figure 5
Fig5 <- read.csv("Figure5.csv")
Fig5$outcome <- factor(Fig5$outcome, levels = c("allcause_all_total","resp_all_total","cvd_all_total","infect_all_total","cancer_all_total",
                                                    "digest_all_total","renal_all_total","diabetes_all_total","mental_all_total","nervous_all_total"))

## panel 1
Fig5_A <- Fig5 %>%
  filter(group  == "RR")
group.colors <- c(`EHE_fire` = "#FF9F00", `EHE_nonfire` ="#309898") ##EHE_fire denotes compound exposure to high EHE & high HFPP; EHE_nonfire denotes compound exposure to high EHE & high HNPP

ggplot(Fig5_A,aes(x=exposure, y = RR,color = exposure,group = exposure)) +
  facet_grid(cols = vars(outcome))+
  geom_errorbar(aes(x=exposure, ymin=RRlow,ymax=RRhigh,group = exposure),position = position_dodge(width = 0.7),width = 0.45)+
  geom_point(aes(x = exposure, y = RR,group = exposure),position = position_dodge(width = 0.7),size = 2)+
  geom_hline(aes(yintercept=1),linetype="dashed",color = "#A6A9B6") +
  ggtitle('')+
  xlab("")+
  ylab("Relative Risk (95% CI)")+
  theme_bw()+
  scale_color_manual(values =group.colors)+
  theme(text = element_text(size=14),
        plot.title=element_text(size=15,face = "bold",family = 'serif'),
        axis.text = element_text(size = 14,colour = "black",family = 'serif'),
        axis.text.x = element_blank(),
        axis.title.x  = element_text(size = 14,colour = "black",family = 'serif'),
        axis.title.y  = element_text(size = 14,colour = "black",family = 'serif'),
        axis.title.y.right = element_blank(), 
        axis.text.y.right = element_blank(),
        axis.text.y = element_text(margin = margin(r = 0)),
        panel.spacing = unit(0, "mm"),
        strip.background = element_blank(), 
        legend.text = element_text(size = 12,colour = "black",family = 'serif'),
        legend.title = element_text(size = 12,colour = "black",family = 'serif'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) 

## panel 2
Fig5_B <- Fig5 %>%
  na.omit() %>%
  filter(group  == "RERI")

ggplot(Fig5_B,aes(x=exposure, y = RR,color = exposure,group = exposure)) +
  facet_grid(cols = vars(outcome))+
  geom_errorbar(aes(x=exposure, ymin=RRlow,ymax=RRhigh,group = exposure),position = position_dodge(width = 0.7),width = 0.45)+
  geom_bar(aes(x = exposure, y = RR, fill = exposure),width = 0.5,alpha = 0.88,stat="identity") + 
  geom_hline(aes(yintercept=0),linetype="dashed",color = "#A6A9B6") +
  ggtitle('')+
  xlab("")+
  ylab("Relative Risk (95% CI)")+
  theme_bw()+
  scale_color_manual(values =group.colors,
                     aesthetics = c("colour", "fill"))+
  theme(text = element_text(size=14),
        plot.title=element_text(size=15,face = "bold",family = 'serif'),
        axis.text = element_text(size = 14,colour = "black",family = 'serif'),
        axis.text.x = element_blank(),
        axis.title.x  = element_text(size = 14,colour = "black",family = 'serif'),
        axis.title.y  = element_text(size = 14,colour = "black",family = 'serif'),
        axis.title.y.right = element_blank(), 
        axis.text.y.right = element_blank(),
        axis.text.y = element_text(margin = margin(r = 0)),
        panel.spacing = unit(0, "mm"),
        strip.background = element_blank(), 
        legend.text = element_text(size = 12,colour = "black",family = 'serif'),
        legend.title = element_text(size = 12,colour = "black",family = 'serif'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) 