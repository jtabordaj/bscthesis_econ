# ISIC (spanish) at https://www.dane.gov.co/files/sen/nomenclatura/ciiu/CIIU_Rev_4_AC2020.pdf  and 
# https://www.miplanilla.com/private/publico/ActividadesEconomicas.pdf 

## dflist (an object full of dfs)
dflist <- split(main, f = main$CIIU4)

## find number of observations per industry. 20 will be the minimun number of observations for this study
ndistr <- data.frame(sapply(dflist,function(x){nrow(x)}))
colnames(ndistr) <- "n"
summary(ndistr$n)

signifn <- 20
significance <- data.frame(sapply(dflist,function(x){ifelse(nrow(x) > signifn, 1, 0)}))
colnames(significance) <- "has20"

dflist <- Filter(function(x){nrow(x) > signifn}, dflist)

## first set of new measures will be for the concentration measure, as it is a measure of measures

dflist <- dflist %>%
   map(~mutate(., MS = (I3R2C1 / sum(I3R2C1))*100)) %>%
   map(~mutate(., MSA = (II1R10C2 / sum(II1R10C2))*100)) %>%
   map(~mutate(., LDS = (PERTOTAL / sum(PERTOTAL))*100)) %>%
   map(~mutate(., SS = (PRODBIND / sum(PRODBIND))*100)
)

## I rename the columns used for the technological opportunities measure. Just because of personal liking

dflist <- dflist %>% 
    map(~rename(., PM = VI1R8C2)) %>%
    map(~rename(., PM1718 = VI2R8C2)) %>%
    map(~rename(., NCPM1718 = VI3R5C2)
)

## loop for destructuring the dflist. Codename "Flipper"

main_l <- do.call(rbind.data.frame, dflist)
grupo <- function(x) { 
    as.data.frame(filter(
            main_l, 
            CIIU4 == x
        )
    )
}
industries <- as.vector(unique(main_l$CIIU4))
for(i in industries){
    assign(paste("sector",i, sep=""), 
        grupo(i), 
        envir = .GlobalEnv, 
        )
}

## I build something called a macro file where all measures will be included and further analysis will be conducted

main_macro <- as.data.frame(unique(main_l$CIIU4))
names(main_macro)[1] <- "CIIU4"
main_macro$CIIU4 <- as.character(main_macro$CIIU4)

## Stability Index (STA)

sr <- data.frame(sapply(dflist, function(x) {sum(x$I1R4C2N)/(sum(x$I1R4C2N)+sum(x$I1R4C2M))})) 
colnames(sr) <- "sr"
sr <- tibble::rownames_to_column(sr, "CIIU4")
si <- data.frame(sapply(dflist, function(x) {sum(x$I1R4C2M)/(sum(x$I1R4C2N)+sum(x$I1R4C2M))})) 
colnames(si) <- "si"
si <- tibble::rownames_to_column(si, "CIIU4")

## Concentration index (CON)

HHms <- data.frame(sapply(dflist, function(x) hhi(x, "MS")))
colnames(HHms) <- "HHms"
HHms <- tibble::rownames_to_column(HHms, "CIIU4")

HHmsa <- data.frame(sapply(dflist, function(x) hhi(x, "MSA")))
colnames(HHmsa) <- "HHmsa"
HHmsa <- tibble::rownames_to_column(HHmsa, "CIIU4")

HHlds <- data.frame(sapply(dflist, function(x) hhi(x, "LDS")))
colnames(HHlds) <- "HHlds"
HHlds <- tibble::rownames_to_column(HHlds, "CIIU4")

HHss <- data.frame(sapply(dflist, function(x) hhi(x, "SS")))
colnames(HHss) <- "HHss"
HHss <- tibble::rownames_to_column(HHss, "CIIU4")

## Technological Opportunities Index (TO)

TO <- data.frame(sapply(dflist, function(x) {(sum(x$PM1718) + sum(x$NCPM1718))/(sum(x$PM))}))
colnames(TO) <- "TO"
TO <- tibble::rownames_to_column(TO, "CIIU4")

## several joins here, mostly to include measures for dimensions on the main file

merge_criteria_macro <- c("CIIU4")
main_macro <- inner_join(main_macro, sr, by = merge_criteria_macro) %>%
    inner_join(., si, by = merge_criteria_macro) %>%
    inner_join(., HHms, by = merge_criteria_macro) %>%
    inner_join(., HHmsa, by = merge_criteria_macro) %>%
    inner_join(., HHlds, by = merge_criteria_macro) %>%
    inner_join(., HHss, by = merge_criteria_macro) %>%
    inner_join(., TO, by = merge_criteria_macro
)
main_macro <- main_macro %>% mutate(CON = (HHms*HHmsa*HHlds*HHss)^(1/4)) %>%
    mutate(STA = sr-si
)

## no method for is.nan for a df. So I create a method

isNaN <- function(x){
    do.call(cbind, lapply(x, is.nan))
}
main_macro[isNaN(main_macro)] <- 0

## Then some tweaks to the TO
main_macro <- main_macro %>% mutate(TO = ifelse(is.finite(TO), TO, NaN))
main_macro[isNaN(main_macro)] <- 0

