# MFWS Tree Health 2022
# Adaptive Management Support request from Simon Stratford PCS


# Responded to by Luke O'Loughlin
# code written: 2022-09-26 

# 1. PREPARATION_________________________________####

## total clean up 
rm(list = ls()) 
invisible(gc())

## Load Libraries 
library(tidyverse)
library(ggpubr)

## Load Data
# CR Dieback Monitoring 2017
CR_Trees_MFWS_2017 <- read_csv("CR_Trees_MFWS_2017.csv")
View(CR_Trees_MFWS_2017)
# CR Dieback Monitoring 2017
CR_Trees_MFWS_2021 <- read_csv("CR_Trees_MFWS_2021.csv")
View(CR_Trees_MFWS_2021)
# Offsets Condition Monitoring 2020
Offsets_Trees_MFWS_2020 <- read_csv("Offsets_Trees_MFWS_2020.csv")
View(Offsets_Trees_MFWS_2020)


###Create ONE DATAFRAME that has TreeID, the 3 measures of health for each year, DBH in each year.

# 2017 data 
# 1 = give trees without "SITE_ID" their "OBJECTID"
# 2 = rename columns we want
# 3 = calculate DBH

dat17 <- CR_Trees_MFWS_2017%>%
  mutate(SITE_ID = ifelse(is.na(SITE_ID), OBJECTID, SITE_ID))%>%
  rename(TreeID = SITE_ID,
         Crown_17 = CROWN_DENSITY,
         DeadBranch_17 = DEAD_BRANCHES,
         Epicormic_17 = EPICORMIC_GROWTH,
         Species = SPECIES)%>%
  rowwise()%>%
  mutate(DBHcm_17 = sum(c_across(DBHcm_1:DBHcm_6), na.rm=TRUE))%>%
  select(Long, Lat, TreeID, Species, Crown_17, DeadBranch_17, Epicormic_17, DBHcm_17)

# 2021 data 
# 1 = just LONG + TreeID from 17, to give the right TreeID to the unidentified trees
# 2 = give trees without "PlotID" their "OBJECTID"
# 3 = rename columns we want
# 4 = calculate DBH
# 5 = select only columns we want

ID17 <- dat17 %>% select(Long, TreeID)%>%rename(TreeID2 = TreeID)%>%filter(TreeID2 != 58, TreeID2 <100)%>%mutate(Long = round(Long, 4))
CR_Trees_MFWS_2021 <- CR_Trees_MFWS_2021%>%mutate(Long = round(Long, 4))
dat21 <- left_join(CR_Trees_MFWS_2021, ID17)%>%
  mutate(PlotID = ifelse(is.na(PlotID), TreeID2, PlotID))%>%
  subset(select = -c(TreeID))%>%
  rename(TreeID = PlotID,
         Crown_21 = CrownDensity,
         DeadBranch_21 = DeadBranches,
         Epicormic_21 = EpicormicGrowth)%>%
  rowwise()%>%
  mutate(DBHcm_21 = sum(c_across(TreeDBH1:TreeDBH7), na.rm=TRUE))%>%
  select(TreeID, Crown_21, DeadBranch_21, Epicormic_21, DBHcm_21)
  
# Join the data into the master DATAFRAME

df <- left_join(dat17, dat21)
#convert 2017 circumference measures to diameter, and use other years measures if DBH not recorded
df <- df%>%mutate(DBHcm_17 = DBHcm_17/3.14)
df <- df%>%mutate(DBHcm_17 = ifelse(DBHcm_17 == 0, DBHcm_21, DBHcm_17))
df <- df%>%mutate(DBHcm_21 = ifelse(DBHcm_21 == 0, DBHcm_17, DBHcm_21))


#Calculate "OverallHealth"
df <- df%>%
  mutate(OverallHealth17 = (sum(Crown_17,DeadBranch_17,Epicormic_17))/3,
         OverallHealth21 = (sum(Crown_21,DeadBranch_21, Epicormic_21))/3)


#Calculate change between years
df <- df%>%
  mutate(Crown_change = Crown_21 - Crown_17,
         DeadBranch_change = DeadBranch_21 - DeadBranch_17,
         Epicormic_change = Epicormic_21 - Epicormic_17,
         OverallHealth_change = OverallHealth21 - OverallHealth17)


#Make a longer dataframe to make ploting easier
dat17l <- dat17%>%
  rename(Crown = Crown_17,
         DeadBranch = DeadBranch_17,
         Epicormic = Epicormic_17,
         DBHcm = DBHcm_17)%>%
  mutate(DBHcm = DBHcm/3.14)
dat17l<- dat17l%>%
  mutate(OverallHealth = (sum(Crown,DeadBranch,Epicormic))/3,
         Year = 2017)

fx <- dat17 %>% select(TreeID, Long, Lat, Species)
dat21l<-left_join(dat21, fx)
dat21l <- dat21l%>%
  rename(Crown = Crown_21,
         DeadBranch = DeadBranch_21,
         Epicormic = Epicormic_21,
         DBHcm = DBHcm_21)
dat21l<- dat21l%>%
  mutate(OverallHealth = (sum(Crown,DeadBranch,Epicormic))/3,
         Year = 2021)

dfl <- rbind(dat17l, dat21l)
dfl <- dfl%>%pivot_longer(!Long & !Lat & !TreeID & !TreeID & !Species & !Year, names_to = "Metric", values_to = "Response")

# 2. GRAPHS_________________________________####


#Figure 1 == Change in all condition metrics YEAR to YEAR

labels <- c("Crown" = "Crown Density",
            "DeadBranch" = "Dead Branches",
            "Epicormic" = "Epicormic Growth",
            "OverallHealth" = "Overall Health")

Figure1 <- dfl %>%
  mutate(Year = as.factor(Year))%>%
  filter(Metric != "DBHcm")%>%
  ggplot(aes(x=Year))+
  geom_boxplot(aes(y=Response, fill=Year, alpha = 0.5))+
  geom_jitter(aes(y=Response, alpha = 0.5))+
  facet_wrap(~Metric, scales = "free_y", labeller = labeller(Metric = labels))+
  theme_bw()+
  xlab("Year")+ylab("Condition Score")+ggtitle("MFWS Tree Condition Monitoring (n=62)")+
  theme(legend.position = "none")
Figure1

ggsave("MFWS_Trees_Fig1.tiff",
       plot=Figure1,
       width = 6.5, height = 5.5, units = "in",
       dpi = 300)


#Figure 2 === How did starting condition influence change

Figure2a <- df %>%
  mutate(Crown_17 = as.factor(Crown_17)) %>%
  mutate(Crown_17 = fct_relevel(Crown_17, "1", "2", "3", "4", "5")) %>% 
  ggplot(aes(y=Crown_17))+
  geom_boxplot(aes(x=Crown_change), fill = "#414487ff", alpha = 0.5)+
  geom_jitter(aes(x=Crown_change, alpha = 0.5))+
  theme_bw()+
  xlab("Change in Crown Density Score in 2021")+ylab("Crown Density Score in 2017")+
  theme(legend.position = "none")+
  scale_x_continuous(limits = c(-4, 4), breaks = c(-4, -2, 0, 2, 4))
Figure2a

Figure2b <- df %>%
  mutate(DeadBranch_17 = as.factor(DeadBranch_17)) %>%
  mutate(DeadBranch_17 = fct_relevel(DeadBranch_17, "1", "2", "3", "4", "5")) %>% 
  ggplot(aes(y=DeadBranch_17))+
  geom_boxplot(aes(x=DeadBranch_change), fill = "#2A788EFF", alpha = 0.5)+
  geom_jitter(aes(x=DeadBranch_change, alpha = 0.5))+
  theme_bw()+
  xlab("Change in Dead Branches Score in 2021")+ylab("Dead Branches Score in 2017")+
  theme(legend.position = "none")+
  scale_x_continuous(limits = c(-4, 4), breaks = c(-4, -2, 0, 2, 4))
Figure2b

Figure2c <- df %>%
  mutate(Epicormic_17 = as.factor(Epicormic_17)) %>%
  mutate(Epicormic_17 = fct_relevel(Epicormic_17, "1", "2", "3", "4", "5")) %>% 
  ggplot(aes(y=Epicormic_17))+
  geom_boxplot(aes(x=Epicormic_change), fill = "#22A884FF", alpha = 0.5)+
  geom_jitter(aes(x=Epicormic_change, alpha = 0.5))+
  theme_bw()+
  xlab("Change in Epicormic Growth Score in 2021")+ylab("Epicormic Growth Score in 2017")+
  theme(legend.position = "none")+
  scale_x_continuous(limits = c(-4, 4), breaks = c(-4, -2, 0, 2, 4))
Figure2c

Figure2d <- df %>%
  mutate(OverallHealth17 = round(OverallHealth17, 0)) %>%
  mutate(OverallHealth17 = as.factor(OverallHealth17)) %>%
  mutate(OverallHealth17 = fct_relevel(OverallHealth17, "1", "2", "3", "4", "5")) %>% 
  ggplot(aes(y=OverallHealth17))+
  geom_boxplot(aes(x=OverallHealth_change), fill = "#7AD151FF", alpha = 0.5)+
  geom_jitter(aes(x=OverallHealth_change, alpha = 0.5))+
  theme_bw()+
  xlab("Change in Overall Health Score in 2021")+ylab("Overall Health Score in 2017")+
  theme(legend.position = "none")+
  scale_x_continuous(limits = c(-4, 4), breaks = c(-4, -2, 0, 2, 4))
Figure2d

Figure2 <- ggarrange(Figure2a, Figure2b, Figure2c, Figure2d, 
                     ncol = 2, nrow = 2)
Figure2

ggsave("MFWS_Trees_Fig2.tiff",
       plot=Figure2,
       width = 7.8, height = 6.1, units = "in",
       dpi = 300)


# Figure 3, YEAR-YEAR Change in Scores arranged by species

Figure3 <- dfl %>%
  mutate(Year = as.factor(Year))%>%
  mutate(Species = ifelse(Species == "Eucalyptus blakelyi", "E. blakelyi (n=25)",
                          ifelse(Species == "Eucalyptus melliodora", "E. melliodora (n=23)", "Other species (n=16)")))%>%
  filter(Metric != "DBHcm")%>%
  ggplot(aes(x=Year))+
  geom_boxplot(aes(y=Response, fill=Year, alpha = 0.5))+
  geom_jitter(aes(y=Response, alpha = 0.5))+
  facet_grid(Species~Metric, scales = "free_y", labeller = labeller(Metric = labels))+
  theme_bw()+
  xlab("Year")+ylab("Condition Score")+ggtitle("MFWS Tree Condition Monitoring")+
  theme(legend.position = "none")
Figure3

ggsave("MFWS_Trees_Fig3.tiff",
       plot=Figure3,
       width = 6.1, height = 5.6, units = "in",
       dpi = 300)


#Figure 4, relationship between DBH and condition score

#Figure 4, relationship between DBH and condition score

dfl2 <- rbind(dat17l, dat21l)
dfl2 <- dfl2%>%pivot_longer(!Long & !Lat & !TreeID & !TreeID & !Species & !Year &!DBHcm, names_to = "Metric", values_to = "Response")


Figure4 <- dfl2%>%
  mutate(Species = ifelse(Species == "Eucalyptus blakelyi", "E. blakelyi (n=25)",
                          ifelse(Species == "Eucalyptus melliodora", "E. melliodora (n=23)", "Other species (n=16)")))%>%
  filter(Species != "Other species (n=16)")%>%
  ggplot(aes(x=DBHcm, y=Response))+
  geom_point(aes(colour = Species), size = 2)+
  scale_colour_viridis_d(end = 0.5)+
  geom_smooth(aes(group=Species, colour = Species), method = "lm", alpha=0.5)+
  facet_grid(Year~Metric, scales = "free_y", labeller = labeller(Metric = labels))+
  theme_bw()+
  xlab("DBH (cm)")+ylab("Condition Score")+ggtitle("MFWS Tree Condition Monitoring")+
  theme(legend.position = "bottom")
Figure4

ggsave("MFWS_Trees_Fig4.tiff",
       plot=Figure4,
       width = 6.3, height = 7.2, units = "in",
       dpi = 300)
