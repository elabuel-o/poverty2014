
library(XLConnect)
library(ggplot2)
library(foreign)
library(ineq)
library(stringr)
library(scales)
library(gtools)
library(survey)


## ============================================================================
## Pobreza
## ============================================================================

## =======================================
## function for reading xls rows and cols
## =======================================
read.conevalPH <- function(worksheet, cols, rows){
        ## worksheet = character name of the worksheet of Coneval stats append.
        ## cols = vector with start and end columns.
        ## rows = vector with start and end rows.
        
        ## loading the xlsx file into a XLConnect Workbook.
        ## XLConnect package required.
        wb <- loadWorkbook("./data/Anexo estadístico pruebas hipótesis 2010-2014.xlsx")
        data <- readWorksheet(wb,
                                sheet = worksheet, 
                                startRow = rows[1], 
                                endRow   = rows[2], 
                                startCol = cols[1], 
                                endCol   = cols[2])
        
        ## setting columns names.
        ## there are 7 columns corresponding to:
        ## state name, %poverty in 2010, 2012, 2014 and corresponding
        ## standard errors.
        colnames(data) <- c("name", "2010", "ee10", "2012", "ee12", "2014", "ee14")
        
        ## changing state names containing accents.
        data[15, 1] <- "Mexico"
        data[16, 1] <- "Michoacan"
        data[19, 1] <- "Nuevo Leon"
        data[22, 1] <- "Queretaro"
        data[24, 1] <- "San Luis Potosi"
        data[31, 1] <- "Yucatan"
        
        data
}

## =============================
## function for plotting changes
## =============================
plot.changes <- function(df, year, file, type = NULL){
        ## df = the dataframe previously read 
        ## year = year of comparison with respect to '14 (it can be '10 or '12)
        ## file = character for file extension: pdf, png, jpeg
        ## type = extrema or normal (default)
        
        ## computing changes and its standard errors
        if(year == 2010){
                df$chg <- df$'2014' - df$'2010'
        } else{
                df$chg <- df$'2014' - df$'2012'
        }
        if(year == 2010){
                df$eechg <- sqrt(df$ee14^2 + df$ee10^2)
        } else{
                df$eechg <- sqrt(df$ee14^2 + df$ee12^2)
        }
        
        ## computing confidence intervals
        df$ci <- df$eechg*1.96
        
        ## logical for negative or positive change
        df$pos <- df$chg >= 0
        
        ## plotting the changes
        ## title
        plottitle <- paste("Cambios en porcentaje de pobreza", type, ",", year, "-2014")
        
        ## the plot
        p <- ggplot(df, aes(x = reorder(name, chg), y = chg, fill = pos)) + 
                geom_bar(stat = "identity", position = "identity") + coord_flip() + 
                geom_errorbar(aes(ymin = chg - ci, ymax = chg + ci, width = 0.5)) + 
                scale_fill_manual(values = c("#00BFC4", "#F8766D"), guide = FALSE) + 
                labs(title = plottitle) +
                ylab("Puntos porcentuales (intervalos de confianza al 95%)") + xlab("") + 
                theme_bw() + 
                theme(panel.grid.major.x = element_line(linetype = "dashed", 
                                                        size = 0.1, colour = "gray85"), 
                      panel.grid.major.y = element_blank())
        
        ## displaying the plot
        p
        
        ## saving the plot
        filename = paste("changes", year, type, ".", file, sep = "")
        ggsave(file = filename)
}

## ============================================
## reading the xls file and saving change plots
## ============================================

## ================================
## POBREZA
## read data
mydata <- read.conevalPH("Cuadro 4A", cols = c(2, 8), rows = c(11, 43))

## plot for 2010-2014 
plot.changes(df = mydata, year = 2010, file = "png")

## plot for 2012-2014 
plot.changes(df = mydata, year = 2012, file = "png")

## ================================
## POBREZA EXTREMA
## read data
extrema <- read.conevalPH("Cuadro 4A", cols = c(16, 22), rows = c(11, 43))

## plot for 2010-2014 
plot.changes(df = extrema, year = 2010, file = "png", type = "extrema")

## plot for 2012-2014
plot.changes(df = extrema, year = 2012, file = "png", type = "extrema")


## ============================================================================
## Gini indexes by State
## ============================================================================

## Loading data frames
mydata <- read.dbf("./data/Concen.dbf")
mydata$CVE_ENT <- as.numeric(str_sub(mydata$ubica_geo, 1, 2))
names <- read.csv("./data/names.csv")

colnames(names) <- c("CVE_ENT", "NOM_ENT", "NOM_ABR", "NOM_CAP")

## creating the survey object
## household factors, strata and sampling units
svy_data <- svydesign(i = ~upm + folioviv, strata = ~est_dis, 
                      weights = ~factor_hog, data = mydata)

## nationwide gini coefficient
gini <- svyquantile(~ing_cor, svy_data, seq(0, 1, by = 0.001))

## quantiles for each state
quant <- seq(0, 1, by = 0.001)
quant_state <- svyby(~ing_cor, ~CVE_ENT, svy_data, svyquantile, 
                     quantiles = quant, keep.var = FALSE)

## applying the Gini function to each state
quant_state_sub <- quant_state[ , 2:1002]
quant_state_sub$gini <- apply(quant_state, 1, Gini)
gini_states <- subset(quant_state_sub, select = gini)

## setting the new data frame
## two vectors
CVE_ENT <- 1:32
gini_vec <- gini_states$gini

## insterting vectors as columns 
gini_states$CVE_ENT <- CVE_ENT
gini_states$gini_bis <- gini_vec

gini_states <- subset(gini_states, select = c(CVE_ENT, gini_bis))

gini_states <- merge(names, gini_states, by = "CVE_ENT")
