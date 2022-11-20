
## I do a double check on TO just in case

main_macro$TO[!is.finite(main_macro$TO)] <- 0
rownames(main_macro) <- main_macro$CIIU4

## Traspose to put all dimensions on rows and ISICs on columns
macro_t <- data.frame(t(main_macro))
colnames(macro_t) <- macro_t[1,]
macro_t <- macro_t[8:10,]
macro_t[] <- lapply(macro_t, function(x) as.numeric(x))
sapply(macro_t, class)

## What is this? idk i forgor but it is important trust me

numbers_only <- main_macro[,-1]
rownames(numbers_only) <- main_macro[,1]

## Cluster Method: K-Means Clustering
set.seed(55)
k_means <- kmeans(numbers_only[, 7:9 ], 
    2, 
    nstart = 10,
    algorithm = "Lloyd")
k_means
options(ggrepel.max.overlaps = Inf)
clusterkm <- fviz_cluster(k_means, data = numbers_only,
             palette = c("#2E9FDF", "#ec1010"), 
             geom = c("point","text"),
             ellipse.type = "convex", 
             main = "K-means clustering plot",
             ggtheme = theme_bw(),
             repel = TRUE,
             stand = TRUE
             )
clusterkm

## create an objetc suitable for both export and further operations
export <- main_macro[, c(1,8,9,10)]
# write_xlsx(export, path ="./") # optional

## Resulting kmeans object, used based on its first component to create a matrix with ISIC and cluster,
cluster_matrix <- data.frame(k_means[1])
cluster_matrix$CIIU4 <- rownames(cluster_matrix)

# inner join and creating both Mark 1 and Mark 2 data frames, with some descriptive statistics
results <- inner_join(export, cluster_matrix, by = "CIIU4")

mark1 <- data.frame(subset(results, cluster == '2'))
mark2 <- data.frame(subset(results, cluster == '1'))

summary(mark1)
lapply(mark1[,2:4], sd)
lapply(mark1[,2:4], var)
# write_xlsx(mark1, path ="./") # optional

summary(mark2)
lapply(mark2[,2:4], sd)
lapply(mark2[,2:4], var)
# write_xlsx(mark2, path ="./") # optional

## From now on, everything is either plots or descriptive/inferential statistics
## This can be shortened a lot by applying functions, but it would have a lot of variables. Not practical

n_distr$CIIU4 <- rownames(n_distr)

t_m1 <- inner_join(mark1, n_distr, by = "CIIU4")
t_m2 <- inner_join(mark2, n_distr, by = "CIIU4")

n_m1 <- sum(t_m1$n)
n_m2 <- sum(t_m2$n)

## Data vis for analysis

blue <- c("#0a5ffd")
red <- c("#fd0a0a")

#STA

STA1 <- ggplot(results, aes(STA)) + 
    geom_density(data = subset(results, cluster == 1), 
    fill = "#0a5ffd", 
    alpha = .3) + 
    geom_vline(aes(xintercept = mean(mark1$STA)),
    linetype = "dotted") + 
    labs( x = "Stability Measure (STA)", 
    y = "Density",
    title ="Density of the Stability Measure",
    subtitle = "Cluster Group 1") +
    scale_x_continuous(limits = c(-1, 1)) +
    annotate(geom = "text", 
    x = .8, 
    y = .10,
    label = "Radical 
    Innovations") +
    annotate(geom = "text", 
    x = -.8, 
    y = .10,
    label = "Incremental 
    Innovations") +
    annotate(geom = "text",
    x = .8,
    y = .55,
    label = "Skewness = 0.780
    Kurtosis = 3.211",
    fontface = "italic")+
    theme_bw()
STA1
kurtosis(mark2$STA)
skewness(mark2$STA)

STA2 <- ggplot(results, aes(STA)) + 
    geom_density(data = subset(results, cluster == 2), 
    fill = "#fd0a0a", 
    alpha = .3)+
    geom_vline(aes(xintercept = mean(mark2$STA)),
    linetype = "dotted") +
    scale_x_continuous(limits = c(-1, 1)) +
    labs( x = "Stability Measure (STA)", 
    y = "Density",
    title ="Density of the Stability Measure",
    subtitle = "Cluster Group 2") +
    annotate(geom = "text", 
    x = .9, 
    y = .15,
    label = "Radical 
    Innovations") +
    annotate(geom = "text", 
    x = -.9, 
    y = .15,
    label = "Incremental 
    Innovations") +
    annotate(geom = "text",
    x = .75,
    y = .85,
    label = "Skewness = 0.122
    Kurtosis = 2.662",
    fontface = "italic") +
    theme_bw()
STA2
kurtosis(mark1$STA)
skewness(mark1$STA)

BPSTA1 <- ggplot(results, aes(STA)) + 
    geom_boxplot(data = subset(results, cluster == 1),
    fill = "#0a5ffd",
    outlier.color = "blue", 
    alpha = .3,
    notch = FALSE) +
    labs( x = "Stability Measure (STA)", 
    y = "Density",
    title ="Boxplot of the Stability Measure",
    subtitle = "Cluster Group 1") +
    scale_x_continuous(limits = c(-1,1)) +
    theme_bw()
BPSTA1

BPSTA2 <- ggplot(results, aes(STA)) + 
    geom_boxplot(data = subset(results, cluster == 2),
    fill = "#fd0a0a",
    outlier.color = "red", 
    alpha = .3,
    notch = FALSE) +
    labs( x = "Stability Measure (STA)", 
    y = "Density",
    title ="Boxplot of the Stability Measure",
    subtitle = "Cluster Group 2") +
    scale_x_continuous(limits = c(-1, 1)) +
    theme_bw()
BPSTA2



# CON
CON1 <- ggplot(results, aes(CON)) + 
    geom_density(data = subset(results, cluster == 1), 
    fill = "#0a5ffd", 
    alpha = .3) + 
    geom_vline(aes(xintercept = mean(mark2$CON)),
    linetype = "dotted") + 
    labs( x = "Concentration Measure (CON)", 
    y = "Density",
    title ="Density of the Concentration Measure",
    subtitle = "Cluster Group 1") +
    scale_x_continuous(limits = c(0, 3000)) +
    theme_bw()
CON1
kurtosis(mark2$CON)
skewness(mark2$CON)

CON2 <- ggplot(results, aes(CON)) + 
    geom_density(data = subset(results, cluster == 2), 
    fill = "#fd0a0a", 
    alpha = .3)+
    geom_vline(aes(xintercept = mean(mark1$CON)),
    linetype = "dotted") +
    labs( x = "Concentration Measure (CON)", 
    y = "Density",
    title ="Density of the Concentration Measure",
    subtitle = "Cluster Group 2") +
    scale_x_continuous(limits = c(0, 3000)) +
    theme_bw()
CON2
kurtosis(mark1$STA)
skewness(mark1$STA)

BPCON1 <- ggplot(results, aes(CON)) + 
    geom_boxplot(data = subset(results, cluster == 1),
    fill = "#0a5ffd",
    outlier.color = "blue", 
    alpha = .3,
    notch = FALSE) +
    labs( x = "Concentration Measure (CON)", 
    y = "Density",
    title ="Boxplot of the Concentration Measure",
    subtitle = "Cluster Group 1") +
    scale_x_continuous(limits = c(0, 3000)) +
    theme_bw()
BPCON1

BPCON2 <- ggplot(results, aes(CON)) + 
    geom_boxplot(data = subset(results, cluster == 2),
    fill = "#fd0a0a",
    outlier.color = "red", 
    alpha = .3,
    notch = FALSE) +
    labs( x = "Concentration Measure (CON)", 
    y = "Density",
    title ="Boxplot of the Concentration Measure",
    subtitle = "Cluster Group 2") +
    scale_x_continuous(limits = c(0, 3000)) +
    theme_bw()
BPCON2


## TO
TO1 <- ggplot(results, aes(TO)) + 
    geom_density(data = subset(results, cluster == 1), 
    fill = "#0a5ffd", 
    alpha = .3) + 
    geom_vline(aes(xintercept = mean(mark2$TO)),
    linetype = "dotted") + 
    labs( x = "Technological Opportunities (TO)", 
    y = "Density",
    title ="Density of the Technological Opportunities measure",
    subtitle = "Cluster Group 1") +
    scale_x_continuous(limits = c(-0.05, 6.3)) +  #### Aqui cambie limites
    annotate(geom = "text",
    x = 5.5,
    y = .6,
    label = "Skewness = 2.255
    Kurtosis = 7.098",
    fontface = "italic") +
    theme_bw()
TO1
kurtosis(mark2$TO)
skewness(mark2$TO)

TO2 <- ggplot(results, aes(TO)) + 
    geom_density(data = subset(results, cluster == 2), 
    fill = "#fd0a0a", 
    alpha = .3)+
    geom_vline(aes(xintercept = mean(mark1$TO)),
    linetype = "dotted") +
    labs( x = "Technological Opportunities (TO)", 
    y = "Density",
    title ="Density of the Technological Opportunities Measure",
    subtitle = "Cluster Group 2") +
    scale_x_continuous(limits = c(0.08, 6)) + 
    annotate(geom = "text",
    x = 5.25,
    y = 1,
    label = "Skewness = 2.755
    Kurtosis = 11.842",
    fontface = "italic") +
    theme_bw()
TO2
kurtosis(mark1$TO)
skewness(mark1$TO)

BPTO1 <- ggplot(results, aes(TO)) + 
    geom_boxplot(data = subset(results, cluster == 1),
    fill = "#0a5ffd",
    outlier.color = "blue", 
    alpha = .3,
    notch = FALSE) +
    labs( x = "Technological Opportunities (TO)", 
    y = "Density",
    title ="Boxplot of the Technological Opportunities Measure",
    subtitle = "Cluster Group 1") +
    scale_x_continuous(limits = c(0, 6)) +
    theme_bw()
BPTO1

BPTO2 <- ggplot(results, aes(TO)) + 
    geom_boxplot(data = subset(results, cluster == 2),
    fill = "#fd0a0a",
    outlier.color = "red", 
    alpha = .3,
    notch = FALSE) +
    labs( x = "Technological Opportunities (TO)", 
    y = "Density",
    title ="Boxplot of the Technological Opportunities Measure",
    subtitle = "Cluster Group 2") +
    scale_x_continuous(limits = c(0, 6)) +
    theme_bw()
BPTO2


##

summary(results)
limitsd <- function(d,mn,mx){
    data.frame(quantile(d, probs = c(mn,mx)));
}

#TO
limitsTO2 <- limitsd(mark1$TO, .10, .9)
limitsTO1 <- limitsd(mark2$TO, .10, .9)

limitsTO1
TO1ne <- ggplot(results, aes(TO)) + 
    geom_density(data = subset(results, cluster == 1), 
    fill = "#0a5ffd", 
    alpha = .3) + 
    geom_vline(aes(xintercept = mean(mark2$TO)),
    linetype = "dotted") + 
    labs( x = "Technological Opportunities (TO)", 
    y = "Density",
    title ="Density of the Technological Opportunities measure",
    subtitle = "Cluster Group 1 - Excluding extreme values") +
    scale_x_continuous(limits = c(0.065, 2.76)) +  
    theme_bw()
TO1ne

limitsTO2
TO2ne <- ggplot(results, aes(TO)) + 
    geom_density(data = subset(results, cluster == 2), 
    fill = "#fd0a0a", 
    alpha = .3)+
    geom_vline(aes(xintercept = mean(mark1$TO)),
    linetype = "dotted") +
    labs( x = "Technological Opportunities (TO)", 
    y = "Density",
    title ="Density of the Technological Opportunities Measure",
    subtitle = "Cluster Group 2 - Excluding extreme values") +
    scale_x_continuous(limits = c(0.08, 1.20)) +
    theme_bw()
TO2ne

#CON
limitsCON2 <- limitsd(mark1$CON, .10, .9)
limitsCON1 <- limitsd(mark2$CON, .10, .9)

limitsCON1
CON1ne <- ggplot(results, aes(CON)) + 
    geom_density(data = subset(results, cluster == 1), 
    fill = "#0a5ffd", 
    alpha = .3) + 
    geom_vline(aes(xintercept = mean(mark2$CON)),
    linetype = "dotted") + 
    labs( x = "Concentration Measure (CON)", 
    y = "Density",
    title ="Density of the Concentration Measure",
    subtitle = "Cluster Group 1 - Excluding extreme values") +
    scale_x_continuous(limits = c(1096, 1911)) +
    theme_bw()
CON1ne

limitsCON2
CON2ne <- ggplot(results, aes(CON)) + 
    geom_density(data = subset(results, cluster == 2), 
    fill = "#fd0a0a", 
    alpha = .3)+
    geom_vline(aes(xintercept = mean(mark1$CON)),
    linetype = "dotted") +
    labs( x = "Concentration Measure (CON)", 
    y = "Density",
    title ="Density of the Concentration Measure",
    subtitle = "Cluster Group 2 - Excluding extreme values") +
    scale_x_continuous(limits = c(218, 921)) +
    theme_bw()
CON2ne

#STA
limitsSTA2 <- limitsd(mark1$STA, .10, .9)
limitsSTA1 <- limitsd(mark2$STA, .10, .9)

limitsSTA1
STA1ne <- ggplot(results, aes(STA)) + 
    geom_density(data = subset(results, cluster == 1), 
    fill = "#0a5ffd", 
    alpha = .3) + 
    geom_vline(aes(xintercept = mean(mark1$STA)),
    linetype = "dotted") + 
    labs( x = "Stability Measure (STA)", 
    y = "Density",
    title ="Density of the Stability Measure",
    subtitle = "Cluster Group 1 - Excluding extreme values") +
    scale_x_continuous(limits = c(-0.933, 0.234)) +
    annotate(geom = "text", 
    x = .18, 
    y = .15,
    label = "Radical 
    Innovations") +
    annotate(geom = "text", 
    x = -.8, 
    y = .15,
    label = "Incremental 
    Innovations") +
    theme_bw()
STA1ne

limitsSTA2
STA2ne <- ggplot(results, aes(STA)) + 
    geom_density(data = subset(results, cluster == 2), 
    fill = "#fd0a0a", 
    alpha = .3)+
    geom_vline(aes(xintercept = mean(mark2$STA)),
    linetype = "dotted") +
    scale_x_continuous(limits = c(-1, 0.07)) +
    labs( x = "Stability Measure (STA)", 
    y = "Density",
    title ="Density of the Stability Measure",
    subtitle = "Cluster Group 2 - Excluding extreme values") +
    annotate(geom = "text", 
    x = 0, 
    y = .15,
    label = "Radical 
    Innovations") +
    annotate(geom = "text", 
    x = -.9, 
    y = .15,
    label = "Incremental 
    Innovations") +
    theme_bw()
STA2ne


ggarrange(ncol = 2, nrow = 3, STA1, STA1ne, STA2, STA2ne, BPSTA1, BPSTA2)

ggarrange(ncol = 2, nrow = 3, CON1, CON1ne, CON2, CON2ne, BPCON1, BPCON2)

ggarrange(ncol = 2, nrow = 3, TO1, TO1ne, TO2, TO2ne, BPTO1, BPTO2)
