
### Water stress experiment only on PO and RS

# Install packages

library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)

#Import data

d0 <- readxl::read_excel("data_dec/water_stress_Group_5_only.xlsx")
d0

# Means, standard deviations

# Regression
dcor.lm <- lm(Chlorophyll_content ~ Soil_humidity, data = d0)
dcor.lm
### b0 = 29.43 (intercept), b1 = -0.084
### For each chlorophyll content ____(unit), the soil humidity decreases 0.084 ____(unit). 
### y = 29.43 - 0.084 x
summary(dcor.lm)
# p-value = 7 * 10-9 (v low = not due to random)

# Homoscedasticity 
#car::leveneTest(dcor.lm)
## Groups are not homoscedastic. Variances are different between groups. 

# Correlations
## Relationship between chlorophyll content and soil humidity
dcor <- d0[c("Chlorophyll_content", "Soil_humidity")]
dcor <- na.exclude(dcor)
cor <- cor(dcor$Chlorophyll_content, dcor$Soil_humidity)
cor
### r = -0,385

## Relationship between chlorophyll content and plant height
dcor1 <- d0[c("Chlorophyll_content", "Plant_height")]
dcor1 <- na.exclude(dcor1)
cor1 <- cor(dcor1$Chlorophyll_content, dcor1$Plant_height)
cor1
### r = 0.152 (weak relationship)
plot(dcor1)

## Relationship between chl and leaf area
dcor2 <- d0[c("Chlorophyll_content", "Leaf_area")]
dcor2 <- na.exclude(dcor2)
cor2 <- cor(dcor2$Chlorophyll_content, dcor2$Leaf_area)
cor2
### r = 0.127 (weak relationship)

## Relationship between soil hum and plant height
dcor3 <- d0[c("Soil_humidity", "Plant_height")]
dcor3 <- na.exclude(dcor3)
cor3 <- cor(dcor3$Soil_humidity, dcor3$Plant_height)
cor3
### r = -0.484 x

## Relationship between soil hum and leaf area
dcor4 <- d0[c("Soil_humidity", "Leaf_area")]
dcor4 <- na.exclude(dcor4)
cor4 <- cor(dcor4$Soil_humidity, dcor4$Leaf_area)
cor4
### r = -0.470

## Relationship between soil hum and leaf length
dcor5 <- d0[c("Soil_humidity", "Leaf_length")]
dcor5 <- na.exclude(dcor5)
cor5 <- cor(dcor5$Soil_humidity, dcor5$Leaf_length)
cor5
### r = 0.363

## Relationship between soil hum and aerial fresh weight
dcor6 <- d0[c("Soil_humidity", "Aerial_fresh_weight")]
dcor6 <- na.exclude(dcor6)
cor6 <- cor(dcor6$Soil_humidity, dcor6$Aerial_fresh_weight)
cor6
### r = -0.087

## Relationship between soil hum and root length
dcor7 <- d0[c("Soil_humidity", "Root_length")]
dcor7 <- na.exclude(dcor7)
cor7 <- cor(dcor7$Soil_humidity, dcor7$Root_length)
cor7
### r = -0.632 x

## Relationship between soil hum and root fresh weight
dcor8 <- d0[c("Soil_humidity", "Roots_fresh_weight")]
dcor8 <- na.exclude(dcor8)
cor8 <- cor(dcor8$Soil_humidity, dcor8$Roots_fresh_weight)
cor8
### r = -0.213

## Relationship between soil hum and root dry weight
dcor9 <- d0[c("Soil_humidity", "Roots_dry_weight")]
dcor9 <- na.exclude(dcor9)
cor9 <- cor(dcor9$Soil_humidity, dcor9$Roots_dry_weight)
cor9
### r = -0.493

## Relationship between soil hum and leaf number
dcor10 <- d0[c("Soil_humidity", "Leaf_number")]
dcor10 <- na.exclude(dcor10)
cor10 <- cor(dcor10$Soil_humidity, dcor10$Leaf_number)
cor10
### r = 0.070


# For loop geomline
v1 <- c("Plant_height", "Leaf_width", "Leaf_length", "Leaf_area", "Leaf_number", "Root_length", "Chlorophyll_content", "Soil_humidity", "Electrical_conductivity")
sp <- "Portulaca oleracea"
variable <- "Plant_height"

for(sp in levels(as.factor(d0$Species))) {
  for(variable in v1) {
    s1 <- d0[d0$Species==sp, c(variable, "Week", "PlantId", "Treatment")]
    s1 <- na.exclude(s1)
    p <- ggplot(s1, aes(x= Week, y= .data[[variable]], group= PlantId, color= Treatment)) + 
      geom_line() + 
      labs(title = sp)
    print(p)
  }
}

# Soil moisture affecting plant height
v0 <- c("Plant_height")
sp0 <- "Portulaca oleracea"
variable <- "Plant_height"

for(sp0 in levels(as.factor(d0$Species))) {
  for(variable in v0) {
s0 <- d0[d0$Species==sp0, c(variable, "Soil_humidity", "Treatment")]
s0 <- na.exclude(s0)
p0 <- ggplot(s0, aes(x= Soil_humidity, y= .data[[variable]], color= Treatment)) + 
  geom_line() + 
  labs(title = sp0)
print(p0)
  }
}

# ANOVA just Raphanus, pb to select just week6

dd <- d0[d0$Species  == "Raphanus sativus", c("Treatment", "Plant_height")]
dd <- na.exclude(dd)                                                      #remove the ones with empty cells 
lmm <- lm(Plant_height ~ Treatment, data = dd)
anova(lmm)
summary(lmm)
gg <- ggplot(dd, aes(x = Treatment, y = Plant_height))

gg + geom_boxplot() +
  labs(title = 'Fig: Boxplot RS, ANOVA', caption = 'c = control, i = intermediate, s = stress')

# ANOVA (two factors: treatments, species)

## Plant_height
### week 1
d1 <- d0[d0$Week == "W1", c("Treatment", "Species", "Plant_height")]
d1 <- na.exclude(d1)                                                      #remove the ones with empty cells 
lm1 <- lm(Plant_height ~ Treatment + Species, data = d1)
anova(lm1)
summary(lm1)
g1 <- ggplot(d1, aes(x = Treatment, y = Plant_height,
                    fill = Species))
g1 + geom_boxplot() +
  labs(title = 'Fig: Boxplot comparing two species by plant height by treatment, ANOVA week 1', caption = 'c = control, i = intermediate, s = stress')

### Week 6
d6 <- d0[d0$Week == "W6", c("Treatment", "Species", "Plant_height")]
d6 <- na.exclude(d6)                                                      #remove the ones with empty cells 
lm6 <- lm(Plant_height ~ Treatment + Species, data = d6)
anova(lm6)
summary(lm6)
g6 <- ggplot(d6, aes(x = Treatment, y = Plant_height,
                    fill = Species))
g6 + geom_boxplot() +
  labs(title = 'Fig1: Boxplot comparing two species by plant height by treatment, ANOVA week 6', caption = 'c = control, i = intermediate, s = stress')

## Leaf_area
### week1
d1.area <- d0[d0$Week == "W1", c("Treatment", "Species", "Leaf_area")]
d1.area <- na.exclude(d1.area)                                                      #remove the ones with empty cells 
lm1.area <- lm(Leaf_area ~ Treatment + Species, data = d1.area)
anova(lm1.area)
summary(lm1.area)
g1.area <- ggplot(d1.area, aes(x = Treatment, y = Leaf_area,
                     fill = Species))
g1.area + geom_boxplot() +
  labs(title = 'Fig: Boxplot comparing two species by leaf area by treatment, ANOVA week 1', 
       caption = 'c = control, i = intermediate, s = stress')

### week6
d6.area <- d0[d0$Week == "W6", c("Treatment", "Species", "Leaf_area")]
d6.area <- na.exclude(d6.area)                                                      #remove the ones with empty cells 
lm6.area <- lm(Leaf_area ~ Treatment + Species, data = d6.area)
anova(lm6.area)
summary(lm6.area)
g6.area <- ggplot(d6.area, aes(x = Treatment, y = Leaf_area,
                     fill = Species))
g6.area + geom_boxplot() +
  labs(title = 'Fig2: Boxplot comparing two species by leaf area by treatment, ANOVA week 6', 
       caption = 'c = control, i = intermediate, s = stress')

## Leaf number
### week1
d1.nb <- d0[d0$Week == "W1", c("Treatment", "Species", "Leaf_number")]
d1.nb <- na.exclude(d1.nb)                                                      #remove the ones with empty cells 
lm1.nb <- lm(Leaf_number ~ Treatment + Species, data = d1.nb)
anova(lm1.nb)
summary(lm1.nb)
g1.nb <- ggplot(d1.nb, aes(x = Treatment, y = Leaf_number,
                               fill = Species))
g1.nb + geom_boxplot() +
  labs(title = 'Fig: Boxplot comparing two species by leaf number by treatment, ANOVA week 1', 
       caption = 'c = control, i = intermediate, s = stress')

### week6
d6.nb <- d0[d0$Week == "W6", c("Treatment", "Species", "Leaf_number")]
d6.nb <- na.exclude(d6.nb)                                                      #remove the ones with empty cells 
lm6.nb <- lm(Leaf_number ~ Treatment + Species, data = d6.nb)
anova(lm6.nb)
summary(lm6.nb)
g6.nb <- ggplot(d6.nb, aes(x = Treatment, y = Leaf_number,
                               fill = Species))
g6.nb + geom_boxplot() +
  labs(title = 'Fig2: Boxplot comparing two species by leaf number by treatment, ANOVA week 6', 
       caption = 'c = control, i = intermediate, s = stress')

## Leaf length
### week1
d1.l <- d0[d0$Week == "W1", c("Treatment", "Species", "Leaf_length")]
d1.l <- na.exclude(d1.l)                                                      #remove the ones with empty cells 
lm1.l <- lm(Leaf_length ~ Treatment + Species, data = d1.l)
anova(lm1.l)
summary(lm1.l)
g1.l <- ggplot(d1.l, aes(x = Treatment, y = Leaf_length,
                           fill = Species))
g1.l + geom_boxplot() +
  labs(title = 'Fig: Boxplot comparing two species by leaf length by treatment, ANOVA week 1', 
       caption = 'c = control, i = intermediate, s = stress')

### week6
d6.l <- d0[d0$Week == "W6", c("Treatment", "Species", "Leaf_length")]
d6.l <- na.exclude(d6.l)                                                      #remove the ones with empty cells 
lm6.l <- lm(Leaf_length ~ Treatment + Species, data = d6.l)
anova(lm6.l)
summary(lm6.l)
g6.l <- ggplot(d6.l, aes(x = Treatment, y = Leaf_length,
                           fill = Species))
g6.l + geom_boxplot() +
  labs(title = 'Fig3a: Boxplot comparing two species by leaf length by treatment, ANOVA week 6', 
       caption = 'c = control, i = intermediate, s = stress')

## Leaf width
### week1
d1.w <- d0[d0$Week == "W1", c("Treatment", "Species", "Leaf_width")]
d1.w <- na.exclude(d1.w)                                                      #remove the ones with empty cells 
lm1.w <- lm(Leaf_width ~ Treatment + Species, data = d1.w)
anova(lm1.w)
summary(lm1.w)
g1.w <- ggplot(d1.w, aes(x = Treatment, y = Leaf_width,
                         fill = Species))
g1.w + geom_boxplot() +
  labs(title = 'Fig: Boxplot comparing two species by leaf width by treatment, ANOVA week 1', 
       caption = 'c = control, i = intermediate, s = stress')

### week6
d6.w <- d0[d0$Week == "W6", c("Treatment", "Species", "Leaf_width")]
d16.w <- na.exclude(d6.w)                                                      #remove the ones with empty cells 
lm6.w <- lm(Leaf_width ~ Treatment + Species, data = d6.w)
anova(lm6.w)
summary(lm6.w)
g6.w <- ggplot(d6.w, aes(x = Treatment, y = Leaf_width,
                         fill = Species))
g6.w + geom_boxplot() +
  labs(title = 'Fig3b: Boxplot comparing two species by leaf width by treatment, ANOVA week 6', 
       caption = 'c = control, i = intermediate, s = stress')

## Aerial fresh 
### week6
d6.af <- d0[d0$Week == "W6", c("Treatment", "Species", "Aerial_fresh_weight")]
d6.af <- na.exclude(d6.af)                                                      #remove the ones with empty cells 
lm6.af <- lm(Aerial_fresh_weight ~ Treatment + Species, data = d6.af)
anova(lm6.af)
summary(lm6.af)
g6.af <- ggplot(d6.af, aes(x = Treatment, y = Aerial_fresh_weight,
                         fill = Species))
g6.af + geom_boxplot() +
  labs(title = 'Fig: Boxplot comparing two species by aerial fresh weight width by treatment, ANOVA week 6', 
       caption = 'c = control, i = intermediate, s = stress')

## Aerial dry weight
### week6
d6.ad <- d0[d0$Week == "W6", c("Treatment", "Species", "Aerial_dry_weight")]
d6.ad <- na.exclude(d6.ad)                                                      #remove the ones with empty cells 
lm6.ad <- lm(Aerial_dry_weight ~ Treatment + Species, data = d6.ad)
anova(lm6.ad)
summary(lm6.ad)
g6.ad <- ggplot(d6.ad, aes(x = Treatment, y = Aerial_dry_weight,
                           fill = Species))
g6.ad + geom_boxplot() +
  labs(title = 'Fig4: Boxplot comparing two species by aerial dry weight by treatment, ANOVA week 6', 
       caption = 'c = control, i = intermediate, s = stress')

## Root length
### week6
d6.rl <- d0[d0$Week == "W6", c("Treatment", "Species", "Root_length")]
d6.rl <- na.exclude(d6.rl)                                                      #remove the ones with empty cells 
lm6.rl <- lm(Root_length ~ Treatment + Species, data = d6.rl)
anova(lm6.rl)
summary(lm6.rl)
g6.rl <- ggplot(d6.rl, aes(x = Treatment, y = Root_length,
                           fill = Species))
g6.rl + geom_boxplot() +
  labs(title = 'Fig: Boxplot comparing two species by root length by treatment, ANOVA week 6', 
       caption = 'c = control, i = intermediate, s = stress')

## Root fresh weight
### week6
d6.rf <- d0[d0$Week == "W6", c("Treatment", "Species", "Roots_fresh_weight")]
d6.rf <- na.exclude(d6.rf)                                                      #remove the ones with empty cells 
lm6.rf <- lm(Roots_fresh_weight ~ Treatment + Species, data = d6.rf)
anova(lm6.rf)
summary(lm6.rf)
g6.rf <- ggplot(d6.rf, aes(x = Treatment, y = Roots_fresh_weight,
                           fill = Species))
g6.rf + geom_boxplot() +
  labs(title = 'Fig: Boxplot comparing two species by roots fresh weight by treatment, ANOVA week 6', 
       caption = 'c = control, i = intermediate, s = stress')

## Root dry weight
### week6
d6.rd <- d0[d0$Week == "W6", c("Treatment", "Species", "Roots_dry_weight")]
d6.rd <- na.exclude(d6.rd)                                                      #remove the ones with empty cells 
lm6.rd <- lm(Roots_dry_weight ~ Treatment + Species, data = d6.rd)
anova(lm6.rd)
summary(lm6.rd)
g6.rd <- ggplot(d6.rd, aes(x = Treatment, y = Roots_dry_weight,
                           fill = Species))
g6.rd + geom_boxplot() +
  labs(title = 'Fig5: Boxplot comparing two species by roots dry weight by treatment, ANOVA week 6', 
       caption = 'c = control, i = intermediate, s = stress')


# Histogram
h <- d0[d0$Week == "W1", c()]
hist(d1, breaks=30, xlim=c(0,300), xlab="Plant_height", 
     ylab="height", main="main")


# PCA Principal Component Analysis
PCA_data <- d0[c(8:9, 11:21)]
PCA_data_scaled <- as.data.frame(scale(PCA_data))
view(PCA_data_scaled)
PCA_data01 <- na.exclude(PCA_data_scaled)

PCA <- prcomp(PCA_data01, scale = FALSE)
print(PCA)

library(factoextra)
library(ggplot2)
library(devtools)
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/factoextra")
                         
fviz_eig(PCA)

### graph for individuals
fviz_pca_ind(PCA,
             col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)     # Avoid text overlapping


### graph of variable
fviz_pca_var(PCA,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)     # Avoid text overlapping

