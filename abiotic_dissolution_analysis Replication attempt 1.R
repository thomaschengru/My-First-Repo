######## Trossulus dissolution analysis ########

### Load required packages
library(tidyverse)
library(sf)
library(purrr)
library(ggplot2)
library(car)
library(rcompanion)

### Load dataset
tross <- read.csv("C:/Users/evely/OneDrive/Desktop/Data_for_Decision_making/abiotic_dissolution_Mtrossulus_Mcalifornianus.csv")

### Plot all data
ggplot(tross, aes(OmegaAragonite, G, color = as.factor(treatment))) + geom_point() + 
  geom_smooth(data = subset(tross, treatment != "shell.californianus.painted" & treatment != "shell.trossulus.painted"), aes(OmegaAragonite, G, color = as.factor(treatment)), formula = y ~ log(x), se = FALSE) +
  scale_color_manual(
    values = c("#98BAD9", "#59638F", "#E8101f", "#de9a1b"),
    name = "Treatment group",
    labels = c(
      "M. californianus sealed",
      "M. californianus unsealed",
      "M. trossulus sealed",
      "M. trossulus unsealed"
    )
  ) +
  xlab("Ωaragonite")+
  ylab("Dissolution rate (µmol hr−1 g−1)") +
  scale_x_continuous(breaks = seq(0, 8, len = 5)) +
  scale_y_continuous(breaks = seq(-0.375, 0, len = 6)) +
  theme_classic()


#trying to flip things around
ggplot(tross, aes(OmegaAragonite, G, color = as.factor(treatment))) + geom_point() + 
  geom_smooth(data = subset(tross, treatment != "shell.californianus.painted" & treatment != "shell.trossulus.painted"), aes(OmegaAragonite, G, color = as.factor(treatment)), formula = y ~ log(x), se = FALSE) +
  scale_color_manual(
    values = c("#98BAD9", "#59638F", "#E8101f", "#de9a1b"),
    name = "Treatment group",
    labels = c(
      "M. californianus sealed",
      "M. californianus unsealed",
      "M. trossulus sealed",
      "M. trossulus unsealed"
    )
  ) +
  xlab("Ωaragonite")+
  ylab("Dissolution rate (µmol hr−1 g−1)") +
  scale_x_continuous(breaks = seq(0, 8, len = 5), trans = "reverse") +
  scale_y_continuous(breaks = seq(-0.375, 0, len = 6), trans = "reverse") +
  theme_classic()

#successful #attempt 3 with different formula 

ggplot(tross, aes(OmegaAragonite, G, color = as.factor(treatment))) + geom_point() + 
geom_smooth(
  data = subset(tross, treatment != "shell.californianus.painted" & treatment != "shell.trossulus.painted"),
  aes(OmegaAragonite, G, color = as.factor(treatment)),
  formula = y ~ x,  # remove log(x)
  se = FALSE
) +
  scale_x_log10() +  # log transform axis instead
    scale_color_manual(
    values = c("#98BAD9", "#59638F", "#E8101f", "#de9a1b"),
    name = "Treatment group",
    labels = c(
      "M. californianus sealed",
      "M. californianus unsealed",
      "M. trossulus sealed",
      "M. trossulus unsealed"
    )
  ) +
  xlab("Ωaragonite")+
  ylab("Dissolution rate µmol hr−1 g−1") +
  scale_x_continuous(breaks = seq(0, 8, len = 5), trans = "reverse") +
  scale_y_continuous(breaks = seq(-0.375, 0, len = 6), trans = "reverse") +
  theme_classic()

#unsuccessful #Attempt #4 - another formula since #3 is squiggly 
ggplot(tross, aes(OmegaAragonite, G, color = as.factor(treatment))) + geom_point() + 
  geom_smooth(
    data = subset(tross, treatment != "shell.californianus.painted" &
                    treatment != "shell.trossulus.painted" &
                    OmegaAragonite > 0.01),  # filter small values
    aes(OmegaAragonite, G, color = as.factor(treatment)),
    formula = y ~ log(x),
    se = FALSE
  )+
  scale_color_manual(
    values = c("#98BAD9", "#59638F", "#E8101f", "#de9a1b"),
    name = "Treatment group",
    labels = c(
      "M. californianus sealed",
      "M. californianus unsealed",
      "M. trossulus sealed",
      "M. trossulus unsealed"
    )
  ) +
  xlab("Ωaragonite")+
  ylab("Dissolution rate (µmol hr−1 g−1)") +
  scale_x_continuous(breaks = seq(0, 8, len = 5), trans = "reverse") +
  scale_y_continuous(breaks = seq(-0.375, 0, len = 6), trans = "reverse") +
  theme_classic()

#unsuccessful

#Attempt 5 - another formula

ggplot(tross, aes(OmegaAragonite, G, color = as.factor(treatment))) + geom_point() + 
  geom_smooth(
    data = subset(tross, treatment != "shell.californianus.painted" &
                    treatment != "shell.trossulus.painted" &
                    OmegaAragonite > 0),
    aes(OmegaAragonite, G, color = as.factor(treatment)),
    method = "lm",
    formula = y ~ log(x),
    se = FALSE
  )+
  scale_color_manual(
    values = c("#98BAD9", "#59638F", "#E8101f", "#de9a1b"),
    name = "Treatment group",
    labels = c(
      "M. californianus sealed",
      "M. californianus unsealed",
      "M. trossulus sealed",
      "M. trossulus unsealed"
    )
  ) +
  xlab("Ωaragonite")+
  ylab("Dissolution rate (µmol hr−1 g−1)") +
  scale_x_continuous(breaks = seq(0, 8, len = 5), trans = "reverse") +
  scale_y_continuous(breaks = seq(-0.375, 0, len = 6), trans = "reverse") +
  theme_classic()

#unsuccessful #Attempt 6 - adding more labels, a title, and changing the wording to be more suited to a standalone graphic

ggplot(tross, aes(OmegaAragonite, G, color = as.factor(treatment))) + geom_point() + 
  geom_smooth(
    data = subset(tross, treatment != "shell.californianus.painted" & treatment != "shell.trossulus.painted"),
    aes(OmegaAragonite, G, color = as.factor(treatment)),
    formula = y ~ x,  # remove log(x)
    se = FALSE
  ) +
  scale_x_log10() +  # log transform axis instead
  scale_color_manual(
    values = c("#98BAD9", "#59638F", "#E8101f", "#de9a1b"),
    name = "Treatment group",
    labels = c(
      "M. californianus sealed",
      "M. californianus unsealed",
      "M. trossulus sealed",
      "M. trossulus unsealed"
    )
  ) +
   labs(title = "Shells, Acid, Heat: Mussels at Risk in a Changing Ocean",
         subtitle = "Ocean acidification poses a threat to mussels in the California Current System",
         caption = "Source: (Carlson et al., 2025)
")
+
  xlab("Ωaragonite")+
  ylab("Dissolution rate (µmol/hr/g)")  +
  scale_x_continuous(breaks = seq(0, 8, len = 5), trans = "reverse") +
  scale_y_continuous(breaks = seq(-0.375, 0, len = 6), trans = "reverse") +
  theme_classic()

#Unsuccessful #Attempt 7 - Messed with too many things. I think it is the labels. I will try using ggtitle

ggplot(tross, aes(OmegaAragonite, G, color = as.factor(treatment))) + geom_point() + 
  geom_smooth(
    data = subset(tross, treatment != "shell.californianus.painted" & treatment != "shell.trossulus.painted"),
    aes(OmegaAragonite, G, color = as.factor(treatment)),
    formula = y ~ x,  # remove log(x)
    se = FALSE
  ) +
  scale_x_log10() +  # log transform axis instead
  scale_color_manual(
    values = c("#98BAD9", "#59638F", "#E8101f", "#de9a1b"),
    name = "Treatment group",
    labels = c(
      "M. californianus sealed",
      "M. californianus unsealed",
      "M. trossulus sealed",
      "M. trossulus unsealed"
    )
  ) +
ggtitle("Shells, Acid, Heat: Mussels at Risk in a Changing Ocean", subtitle = "Ocean acidification poses a threat to mussels in the California Current System")
+
  xlab("Ωaragonite")+
  ylab("Dissolution rate (µmol/hr/g)")  +
  scale_x_continuous(breaks = seq(0, 8, len = 5), trans = "reverse") +
  scale_y_continuous(breaks = seq(-0.375, 0, len = 6), trans = "reverse") +
  theme_classic()

#Successful #Attempt 8 - Title, subtitle, and labels improved.

ggplot(tross, aes(OmegaAragonite, G, color = as.factor(treatment))) + geom_point() + 
  geom_smooth(
    data = subset(tross, treatment != "shell.californianus.painted" & treatment != "shell.trossulus.painted"),
    aes(OmegaAragonite, G, color = as.factor(treatment)),
    formula = y ~ x,  # remove log(x)
    se = FALSE
  ) +
  scale_x_log10() +  # log transform axis instead
  scale_color_manual(
    values = c("#98BAD9", "#59638F", "#E8101f", "#de9a1b"),
    name = "Treatment group",
    labels = c(
      "M. californianus sealed",
      "M. californianus unsealed",
      "M. trossulus sealed",
      "M. trossulus unsealed"
    )
  ) +
  ggtitle("Shells, Acid, Heat: Mussels at Risk in a Changing Ocean",subtitle="Ocean acidification endangers California mussels")+
  xlab("Ωaragonite")+
  ylab("Dissolution rate (µmol/hr/g)") +
  scale_x_continuous(breaks = seq(0, 8, len = 5), trans = "reverse") +
  scale_y_continuous(breaks = seq(-0.375, 0, len = 6), trans = "reverse") +
  theme_classic()

#Successful #Attempt 9 - Trying to add caption using labs feature

ggplot(tross, aes(OmegaAragonite, G, color = as.factor(treatment))) + geom_point() + 
  geom_smooth(
    data = subset(tross, treatment != "shell.californianus.painted" & treatment != "shell.trossulus.painted"),
    aes(OmegaAragonite, G, color = as.factor(treatment)),
    formula = y ~ x,  # remove log(x)
    se = FALSE
  ) +
  scale_x_log10() +  # log transform axis instead
  scale_color_manual(
    values = c("#98BAD9", "#59638F", "#E8101f", "#de9a1b"),
    name = "Treatment group",
    labels = c(
      "M. californianus sealed",
      "M. californianus unsealed",
      "M. trossulus sealed",
      "M. trossulus unsealed"
    )
  ) +
  ggtitle("Shells, Acid, Heat: Mussels at Risk in a Changing Ocean",subtitle="Ocean acidification endangers California mussels")+
  labs(caption="Source:  (Carlson et al., 2025)")+
  xlab("Ωaragonite")+
  ylab("Dissolution rate (µmol/hr/g)") +
  scale_x_continuous(breaks = seq(0, 8, len = 5), trans = "reverse") +
  scale_y_continuous(breaks = seq(-0.375, 0, len = 6), trans = "reverse") +
  theme_classic()

#Successful #Attempt 10 - Trying to center caption

ggplot(tross, aes(OmegaAragonite, G, color = as.factor(treatment))) + geom_point() + 
  geom_smooth(
    data = subset(tross, treatment != "shell.californianus.painted" & treatment != "shell.trossulus.painted"),
    aes(OmegaAragonite, G, color = as.factor(treatment)),
    formula = y ~ x,  # remove log(x)
    se = FALSE
  ) +
  scale_x_log10() +  # log transform axis instead
  scale_color_manual(
    values = c("#98BAD9", "#59638F", "#E8101f", "#de9a1b"),
    name = "Treatment group",
    labels = c(
      "M. californianus sealed",
      "M. californianus unsealed",
      "M. trossulus sealed",
      "M. trossulus unsealed"
    )
  ) +
  ggtitle("Shells, Acid, Heat: Mussels at Risk in a Changing Ocean",subtitle="Ocean acidification endangers California mussels")+
  labs(caption="Source: Carlson et al., 2025")+
  theme(caption = element_text(hjust=10))+
  xlab("Ωaragonite")+
  ylab("Dissolution rate (µmol/hr/g)") +
  scale_x_continuous(breaks = seq(0, 8, len = 5), trans = "reverse") +
  scale_y_continuous(breaks = seq(-0.375, 0, len = 6), trans = "reverse") +
  theme_classic()
