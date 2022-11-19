library(tidyverse)
library(readxl)
library(writexl)
edit <- read_xlsx("C:/Users/Juan Jose/Documents/JJ/UN/6to Semestre/Joint Seminar/data/EDITXLSX.xlsx")
edit <- data.frame(edit)
edit <- edit %>% 
  select(CIIU.4, 
         TIPOLO, 
         II1R1C2,
         II1R2C2,
         II1R5C2,
         II1R6C2,
         II1R8C2,
         II1R4C2,
         II1R3C2,
         II1R7C2,
         II1R9C2,
         II1R10C2) %>% 
  drop_na()
colnames(edit) <- c("CIIU", 
                    "TYPE", 
                    "INTERNAL.R&D", 
                    "EXTERNAL.R&D", 
                    "MARKETING", 
                    "TECH.TRANSFER", 
                    "ENG.INDUSTRIAL.DESIGN",
                    "ICT",
                    "EQUIPMENT",
                    "CONSULTING",
                    "HUMAN.CAPITAL",
                    "TOTAL.EXPENDITURE"
                    )

edit <- data.frame(edit)
edit <- edit %>%
  filter(between(CIIU,
                 1011,
                 3530))

#SECTORIAL

edit.food <- edit %>% 
  filter(between(CIIU,
                 1000,
                 1200))
edit.food <- data.frame(edit.food)
edit.food.sum <- sum(edit.food$TOTAL.EXPENDITURE)


edit.textiles <- edit %>% 
  filter(between(CIIU,
                 1300,
                 1523))
edit.textiles <- data.frame(edit.textiles)
edit.textiles.sum <- sum(edit.textiles$TOTAL.EXPENDITURE)

edit.wood.paper <- edit %>% 
  filter(between(CIIU,
                 1600,
                 1820))
edit.wood.paper <- data.frame(edit.wood.paper)
edit.wood.paper.sum <- sum(edit.wood.paper$TOTAL.EXPENDITURE)

edit.chems.oil <- edit %>% 
  filter(between(CIIU,
                 1910,
                 2230))
edit.chems.oil <- data.frame(edit.chems.oil)
edit.chems.oil.sum <- sum(edit.chems.oil$TOTAL.EXPENDITURE)

edit.metals <- edit %>% 
  filter(between(CIIU,
                 2310,
                 2599))
edit.metals <- data.frame(edit.metals)
edit.metals.sum <- sum(edit.metals$TOTAL.EXPENDITURE)

edit.tech.transport <- edit %>% 
  filter(between(CIIU,
                 2610,
                 3099))
edit.tech.transport <- data.frame(edit.tech.transport)
edit.tech.transport.sum <- sum(edit.tech.transport$TOTAL.EXPENDITURE)

edit.furniture <- edit %>% 
  filter(between(CIIU,
                 3110,
                 3320))
edit.furniture <- data.frame(edit.furniture)
edit.furniture.sum <- sum(edit.furniture$TOTAL.EXPENDITURE)


COL.manufacture.expenditure <- sum(c(edit.food.sum, 
                                     edit.textiles.sum, 
                                     edit.wood.paper.sum, 
                                     edit.chems.oil.sum, 
                                     edit.metals.sum, 
                                     edit.tech.transport.sum, 
                                     edit.furniture.sum))
grouped.edit <- c(COL.manufacture.expenditure,
                  edit.food.sum, 
                  edit.textiles.sum, 
                  edit.wood.paper.sum, 
                  edit.chems.oil.sum, 
                  edit.metals.sum, 
                  edit.tech.transport.sum, 
                  edit.furniture.sum)
grouped.edit <- as.numeric(grouped.edit)
grouped.edit <- data.frame(grouped.edit)





# Recuerda, todo en miles de pesos.
# 23-26 lines just for checking that each row belongs to a CIIU manufacturing code.
# CIIU found at https://www.miplanilla.com/private/publico/ActividadesEconomicas.pdf