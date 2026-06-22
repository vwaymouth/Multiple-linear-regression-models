#make sure to set working directory/filepath
library("ggplot2")
library("ggpubr")
library("MuMIn")
library("ggforce")
library("tidyr")
#

all.df<-read.csv("Data for models all.csv")
nutdata.df<-read.csv("Data for models all2 scatterplots.csv")

EV<-subset(all.df, Species=="EV")
KL<-subset(all.df, Species=="KL")
ME<-subset(all.df, Species=="ME")

EV1<-subset(nutdata.df, Species=="EV")
KL1<-subset(nutdata.df, Species=="KL")
ME1<-subset(nutdata.df, Species=="ME")

summary(all.df)
names(EV)
EVLeaves<-ggplot(EV, aes (x = Ppath, y = nleaves, color = Harvest)) +
  geom_point() + # Add points
  geom_smooth (method = "lm", se = FALSE, linetype = "solid", size = 2) + # Add regression lines without confidence
  labs (x = "Plant Pathogen Abundance", y = "Number of leaves", color = "Harvest") + # Set axis labels
  theme_classic()+# Apply a minimal theme
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, by = 1000)) +
  scale_y_continuous(limits = c(0, 3), breaks = seq(0, 3, by = 0.5)) 
plot(EVLeaves)

KLLeaves<-ggplot(KL,aes (x = Ppath, y = nleaves, color = Harvest)) +
  geom_point() + # Add points
  geom_smooth (method = "lm", se = FALSE, linetype = "dashed", size = 2) + # Add regression lines without confidence
  labs (x = "Plant Pathogen Abundance", y = "Number of leaves", color = "Harvest") + # Set axis labels
  theme_classic()+# Apply a minimal theme
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, by = 250)) +
  scale_y_continuous(limits = c(0, 3), breaks = seq(0, 3, by = 0.5)) 
plot(KLLeaves)

MELeaves<-ggplot(ME,aes (x = Ppath, y = nleaves, color = Harvest)) +
  geom_point() + # Add points
  geom_smooth (method = "lm", se = FALSE, linetype = "dashed", size = 2) + # Add regression lines without confidence
  labs (x = "Plant Pathogen Abundance", y = "Number of leaves", color = "Harvest") + # Set axis labels
  theme_classic()+# Apply a minimal theme
  scale_x_continuous(limits = c(0, 300), breaks = seq(0, 300, by = 75)) +
  scale_y_continuous(limits = c(0, 3), breaks = seq(0, 3, by = 0.5)) 
plot(MELeaves)
names(EV1)
 names(all.df)
 
 #EV1_long <- EV1 %>%
  # pivot_longer(cols = c(ECM, Endo), names_to = "Type", values_to = "Abundance")
 
 # Create the plot with the reshaped data
 EVcarb <- ggplot(EV1, aes(x = Symbio, y = C)) +
   geom_point() + # Add points
   geom_smooth(method = "lm", se = FALSE, color = "#86b380", linetype = "dashed", size = 2) + # Add regression lines without confidence
   labs(x = "Symbiotrophs (Abundance)", y = "Carbon (%)") + # Set axis labels
   theme_classic() + # Apply a minimal theme
   scale_x_continuous(limits = c(0, 100000), breaks = seq(0, 100000, by = 25000)) +
   scale_y_continuous(limits = c(43, 47), breaks = seq(43, 47, by = 0.5))
plot(EVcarb)

Kl1_long <- KL1 %>%
  pivot_longer(cols = c(ECM, Endo), names_to = "Type", values_to = "Abundance")

KLCarb<-ggplot(KL1, aes(x = Symbio, y = C)) +
  geom_point() + # Add points
  geom_smooth(method = "lm", se = FALSE, color = "#86b380", linetype = "solid", size = 2) +  # Add regression lines without confidence
  labs(x = "Symbiotrophs (Abundance)", y = "Carbon (%)") + # Set axis labels
  theme_classic() + # Apply a minimal theme
  scale_x_continuous(limits = c(0, 100000), breaks = seq(0, 100000, by = 25000)) +
  scale_y_continuous(limits = c(43, 47), breaks = seq(43, 47, by = 0.5)) 
plot(KLCarb)

ME1_long <- ME1 %>%
  pivot_longer(cols = c(ECM, Endo), names_to = "Type", values_to = "Abundance")

MEcarb<-ggplot(ME1, aes(x = Symbio, y = C)) +
  geom_point() + # Add points
  geom_smooth(method = "lm", se = FALSE, color = "#86b380", linetype = "dashed", size = 2) + # Add regression lines without confidence
  labs(x = "Symbiotrophs (Abundance)", y = "Carbon (%)") + # Set axis labels
  theme_classic() + # Apply a minimal theme
  scale_x_continuous(limits = c(30000, 130000), breaks = seq(30000, 130000, by = 25000)) +
  scale_y_continuous(limits = c(43, 47), breaks = seq(43, 47, by = 0.5)) 
plot(MEcarb)

EVphos<-ggplot(EV1, aes (x = Leot, y = P)) +
  geom_point() + # Add points
  geom_smooth (method = "lm", se = FALSE, linetype = "solid", size = 2) + # Add regression lines without confidence
  labs (x = "Leotiomycetes (Abundance)", y = "Phosphorus (% w/w)") + # Set axis labels
  theme_classic()+# Apply a minimal theme
  scale_x_continuous(limits = c(0, 25000), breaks = seq(0, 25000, by = 5000)) +
  scale_y_continuous(limits = c(0, 0.41), breaks = seq(0, 0.41, by = 0.05)) 
plot(EVphos)

KLphos<-ggplot(KL1, aes (x = Leot, y = P)) +
  geom_point() + # Add points
  geom_smooth (method = "lm", se = FALSE,linetype = "dashed", size = 2) + # Add regression lines without confidence
  labs (x = "Leotiomycetes (Abundance)", y = "Phosphorus (% w/w)") + # Set axis labels
  theme_classic()+# Apply a minimal theme
  scale_x_continuous(limits = c(0, 50000), breaks = seq(0, 50000, by = 10000)) +
  scale_y_continuous(limits = c(0, 0.41), breaks = seq(0, 0.41, by = 0.05)) 
plot(KLphos)
names(ME1)
MEphos<-ggplot(ME1, aes (x = Euro, y = P)) +
  geom_point() + # Add points
  geom_smooth (method = "lm", se = FALSE,linetype = "solid", size = 2) + # Add regression lines without confidence
  labs (x = "Eurotiomycetes (Abundance)", y = "Phosphorus (% w/w)") + # Set axis labels
  theme_classic()+# Apply a minimal theme
  scale_x_continuous(limits = c(0, 155), breaks = seq(0, 155, by = 25)) +
  scale_y_continuous(limits = c(0, 0.41), breaks = seq(0, 0.41, by = 0.05)) 
plot(MEphos)

allplots<-ggarrange(EVcarb,KLCarb,MEcarb,
                    EVphos,KLphos,MEphos,
                    nrow=2, ncol=3, common.legend = FALSE, legend = "bottom")
plot(allplots)
allplots<-ggsave("scatterplots.tiff", width = 10,
              height = 6.7, dpi=900)#can specify requirements further
#creates scatterplots of relatiohsips that were important based on analysis
