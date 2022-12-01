## requires
library(tidyverse)
library(readxl)
library(dplyr)
library(writexl)
library(haven)
library(hhi)
library(pvclust)
library(factoextra)
library(plotly)
library(BSDA)
library(ggpubr)
library(moments)

## data
edit <- as.data.frame(read_dta('./src/data/EDIT.dta')) 
eam <- as.data.frame(read_dta('./src/data/EAM.dta')) 

## first filters
edit <- edit %>%
  select(NORDEMP,
         CIIU4,
         I1R1C1N,
         I1R1C2N,
         I1R2C1N,
         I1R2C2N,
         I1R3C1N,
         I1R3C2N,
         I1R4C2N,
         I1R1C1M,
         I1R1C2M,
         I1R2C1M,
         I1R2C2M,
         I1R3C1M,
         I1R3C2M,
         I1R4C2M,
         I2R3C1,
         I3R1C1,
         I3R2C1,
         II1R10C1,
         II1R10C2,
         IV1R11C1,
         IV1R11C2,
         IV1R11C3,
         IV1R11C4,
         VI1R1C1,
         VI1R1C2,
         VI1R2C1,
         VI1R2C2,
         VI1R3C1,
         VI1R3C2,
         VI1R4C1,
         VI1R4C2,
         VI1R5C1,
         VI1R5C2,
         VI1R6C1,
         VI1R6C2,
         VI1R8C2,
         VI2R1C1,
         VI2R1C2,
         VI2R2C1,
         VI2R2C2,
         VI2R3C1,
         VI2R3C2,
         VI2R4C1,
         VI2R4C2,
         VI2R5C1,
         VI2R5C2,
         VI2R6C1,
         VI2R6C2,
         VI2R8C2,
         VI3R1C1,
         VI3R1C2,
         VI3R2C1,
         VI3R2C2,
         VI3R3C1,
         VI3R3C2,
         VI3R4C1,
         VI3R4C2,
         VI3R5C2)

eam <- eam %>% select(nordemp,
                      nordest,
                      dpto,
                      ciiu4,
                      c6r5c1,
                      c6r5c3,
                      c7r10c1,
                      c7c7r2,
                      c7c7r3,
                      PRODBR2,
                      PRODBIND,
                      ACTIVFI,
                      VALORVEN,
                      PERTOTAL,
                      VALAGRI
                      )
names(eam)[1] <- "NORDEMP"
names(eam)[4] <- "CIIU4"

## inner join by ISIC and NORDEMP

merge.criteria <- c("NORDEMP", "CIIU4")
main <- inner_join(eam, edit, by = merge.criteria)

## find and map NA values
isNaDf <- function(x){
    as.data.frame(do.call(cbind, lapply(x, is.nan)))
}
NA.suche <- names(
  which(
    colSums(
      is.na(main))>0
       )
) 
mapNA <- isNaDf(main)
col_NA <- as.data.frame(NA.suche)

## NA replacement by zero

branch <- main
branch[is.na(branch)] <- 0
main <- branch 

## check
mapNA <- isNaDf(main)

## second filter, based on feedback by supervisor/teacher/own elaborations

main <- main %>%
    filter(between(CIIU4,
                  1011,
                  3530)
)
main <- main %>% select(NORDEMP,
                        dpto,
                        CIIU4,
                        I1R1C2N,
                        I1R2C2N,
                        I1R3C2N,
                        I1R4C2N,
                        I1R1C2M,
                        I1R2C2M,
                        I1R3C2M,
                        I1R4C2M,
                        I2R3C1, 
                        I3R1C1,
                        I3R2C1,
                        II1R10C1,
                        II1R10C2, 
                        IV1R11C3,
                        IV1R11C4, 
                        PRODBIND, 
                        PERTOTAL,
                        VALAGRI,
                        VI1R8C2,
                        VI2R8C2,
                        VI3R5C2
)

## Further changes for potential georeferentiation using library(colmaps)

names(main)[2] <- "id_depto"
main$id_depto <- as.character(main$id_depto)
main$id_depto[main$id_depto==5]<- "05"
main$id_depto[main$id_depto==8]<- "08"
class(main$id_depto)



## Extra: find ISIC sectors that are not in both surveys
# editISIC <- as.data.frame(unique(edit$CIIU4))
# eamISIC<- as.data.frame(unique(eam$CIIU4))
# dif <- as.data.frame(edit$CIIU4[!(edit$CIIU4 %in% eam$CIIU4)])
# names(dif)[1] <- "non"
# distintos <- as.data.frame(unique(dif$non))

