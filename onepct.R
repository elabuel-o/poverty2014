library(stringr)
library(ggplot2)
library(ineq)
library(foreign)
library(survey)


## =================
## The one percent 
## =================

mydata <- read.dbf("./data/Concen.dbf")
names <- read.csv("./data/names.csv", header = TRUE)
colnames(names) <- c("CVE_ENT", "NOM_ENT", "NOM_ABR", "NOM_CAP")


## new variables
mydata$CVE_ENT <- as.numeric(str_sub(mydata$ubica_geo, 1, 2))
mydata$ing_men <- mydata$ing_cor/3

## Survey object
svy_data <- svydesign(id = ~upm + folioviv, strata = ~est_dis, 
                      weights = ~factor_hog, data = mydata)

## Nationwide
svymean(~ing_men, svy_data)
svyquantile(~ing_men, svy_data, seq(0.1, 0.9, by = 0.1))
svyquantile(~ing_men, svy_data, 0.99)

## Income by state
income <- svyby(~ing_men, ~CVE_ENT, svy_data, svymean)
dec <- seq(0, 0.9, by = 0.1)
deciles <- svyby(~ing_men, ~CVE_ENT, svy_data, svyquantile,
                 quantiles = dec, keep.var = FALSE)

###########################################
## The one percent for each state
###########################################
onepct <- svyby(~ing_men, ~CVE_ENT, svy_data, svyquantile,
                quantiles = 0.99, ci = TRUE, vartype = "ci")

## Merging both data frames
onepct <- merge(onepct, names, by = "CVE_ENT") 

ggplot(onepct, aes(x = ing_men, y = reorder(NOM_ENT, ing_men))) + 
        geom_point(size = 3) + xlab("Ingresos mensuales del hogar") + ylab("") + 
        geom_segment(aes(yend = NOM_ENT), xend = 0, colour = "blue") + 
        theme_bw() + 
        theme(panel.grid.major.x = element_line(), 
              panel.grid.minor.x = element_blank(), 
              panel.grid.major.y = element_line(colour = "gray60", linetype = "dotted"))

ggplot(onepct, aes(x = reorder(NOM_ENT, ing_men), y = ing_men)) +
        geom_bar(stat = "identity", fill = "#F8766D") + coord_flip() # +
        # geom_errorbar(aes(ymin = ing_men - ci_l, ymax = ing_men + ci_u))

###########################################
## The ten percent of each state
###########################################

tenpct <- svyby(~ing_men, ~CVE_ENT, svy_data, svyquantile, 
                quantiles = 0.9, ci = TRUE, vartype = "ci")

tenpct <- merge(tenpct, names, by = "CVE_ENT")

ggplot(tenpct, aes(x = ing_men, y = reorder(NOM_ENT, ing_men))) + 
        geom_point(size = 3) + xlab("Ingresos mensuales del hogar") + ylab("") + 
        geom_segment(aes(yend = NOM_ENT), xend = 0, colour = "blue") + 
        theme_bw() + 
        theme(panel.grid.major.x = element_line(), 
              panel.grid.minor.x = element_blank(), 
              panel.grid.major.y = element_line(colour = "gray60", linetype = "dotted"))





