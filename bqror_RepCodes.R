## Installing and setting up the CRAN's bqror library
cat("\014")
install.packages('bqror')
library('bqror')
library('bayesplot')
library('ggplot2')


################################################################################
##                      Simulation Study I
################################################################################

## << quantregOR1>>
data("data25j4")
y <- data25j4$y
xMat <- data25j4$x
k <- dim(xMat)[2]
J <- dim(as.array(unique(y)))[1]
b0 <- array(rep(0, k), dim = c(k, 1))
B0 <- 10*diag(k)
d0 <- array(0, dim = c(J-2, 1))
D0 <- 0.25*diag(J - 2)
modelORI <- quantregOR1(y = y, x = xMat, b0, B0, d0, D0,
                        burn = 1125, mcmc = 4500, p = 0.25, tune = 1, accutoff = 0.1,
                        maxlags = 400, verbose = TRUE)

## <<logMargLikeOR1>>
modelORI$logMargLike

## <<dicOR1>>
modelORI$dicQuant

## <<infactorOR1>>
modelORI$ineffactor

################################################################################
##                      Simulation Study II
################################################################################
cat("\014")
library('bqror')
library('bayesplot')
library('ggplot2')

## <<quantregOR2>>
data("data25j3")
y <- data25j3$y
xMat <- data25j3$x
k <- dim(xMat)[2]
b0 <- array(rep(0, k), dim = c(k, 1))
B0 <- 10*diag(k)
n0 <- 5
d0 <- 8
modelORII <- quantregOR2(y = y, x = xMat, b0, B0, n0,
                         d0, gammacp2 = 3, burn = 1125, mcmc = 4500,
                         p = 0.25, accutoff = 0.1, maxlags = 400, verbose = TRUE)

## <<logMargLikeOR2>>
modelORII$logMargLike

## <<dicOR2>>
modelORII$dicQuant

## <<infactorOR2>>
modelORII$ineffactor

################################################################################
##                      Application: Educational Attainment
################################################################################
cat("\014")
library('bqror')
library('tidyverse')
library('ggrepel')
library('bayesplot')
library('ggplot2')

## <<Bar Chart Plot (Educational Attainment)>>
data("Educational_Attainment")
data <- na.omit(Educational_Attainment)

## <<Count of each Y class >>
percentages <- c("22.86", "35.48", "22.32", "19.32")
df <- data %>% group_by(dep_edu_level) %>% summarise(counts = n())

ggplot(df, aes(x = dep_edu_level, y = counts)) +
    geom_bar(stat = 'identity', width = 0.7, alpha = 0.0, colour = 'black') +
    theme(axis.line = element_line(colour = "black"),
          axis.text = element_text(size = 10, colour = "black"),
          axis.title = element_text(size = 10, colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
    geom_text(aes(label = percentages), size = 4, vjust = -0.3) +
    geom_text(aes(label = counts), size = 4, vjust = 5, hjust = 0.5) +
    scale_x_discrete(limits =c("Less than high \n school", "High school \n degree",
                                        "Some college or \n associate's degree", "College or \n graduate degree")) +
    xlab("") + ylab("No. of Observations")

## <<Educational_Attainment>>
data("Educational_Attainment")                             ## Load data from the package itself for application
data <- na.omit(Educational_Attainment)
data$fam_income_sqrt <- sqrt(data$fam_income)
cols <- c("mother_work","urban","south",
          "father_educ","mother_educ","fam_income_sqrt","female",
          "black","age_cohort_2","age_cohort_3","age_cohort_4")
x <- data[cols]
x$intercept <- 1
xMat <- x[,c(12,6,5,4,1,7,8,2,3,9,10,11)]
yOrd <- data$dep_edu_level
k <- dim(xMat)[2]
J <- dim(as.array(unique(yOrd)))[1]
b0 <- array(rep(0, k), dim = c(k, 1))
B0 <- 1*diag(k)
d0 <- array(0, dim = c(J-2, 1))
D0 <- 0.25*diag(J - 2)
p <- 0.5
EducAtt <- quantregOR1(y = yOrd, x = xMat, b0, B0,
                       d0, D0, burn = 1125, mcmc = 4500, p, tune=1, accutoff = 0.1,
                       maxlags = 400, TRUE)

## <<Trace_plots>>
mcmc <- 4500
burn <- round(0.25*mcmc)
nsim <- mcmc + burn
mcmcDraws <- cbind(t(EducAtt$betadraws), t(EducAtt$deltadraws))
color_scheme_set('darkgray')
bayesplot_theme_set(theme_minimal())
mcmc_trace(mcmcDraws[(burn+1):nsim, ], facet_args = list(ncol = 3))

## <<Covariate_Effect>>
xMat1 <- xMat
xMat2 <- xMat
xMat2$fam_income_sqrt <- sqrt((xMat1$fam_income_sqrt)^2 + 10)
EducAttCE <- covEffectOR1(EducAtt, yOrd, xMat1, xMat2, p = 0.5, verbose = TRUE)

################################################################################
##                      Application: Tax Policy
################################################################################
rm(list=ls()) 
cat("\014")
library('bqror')
library('tidyverse')
library('ggrepel')
library('bayesplot')
library('ggplot2')

## <<Bar Chart Plot (Policy Opinion)>> ##
data("Policy_Opinion")
data <- na.omit(Policy_Opinion)

## <<Count of each Y class >>
percentages <- c("22.59", "22.42", "54.98")
df <- data %>% group_by(y) %>% summarise(counts = n())

ggplot(df, aes(x = y, y = counts)) +
    geom_bar(stat = 'identity', width = 0.7, alpha = 0.0, colour = 'black') +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_text(size = 10, colour = "black"),
          axis.title = element_text(size = 10, colour = "black"),
          panel.border = element_blank(),
          panel.background = element_blank()) +
    geom_text(aes(label = percentages), size = 4, vjust = -0.3) +
    geom_text(aes(label = counts), size = 4, vjust = 5, hjust = 0.5) +
    scale_x_discrete(limits = c("Oppose", "Neither favor nor \n oppose",
                                "Favor")) +
    xlab("") + ylab("No. of Observations")

## <<Policy_Opinion>>
data("Policy_Opinion")
data <- na.omit(Policy_Opinion)
cols <- c("Intercept","EmpCat","IncomeCat",
          "Bachelors","Post.Bachelors","Computers","CellPhone","White")
x <- data[cols]
xMat <- x[,c(1,2,3,4,5,6,7,8)]
yOrd <- data$y
k <- dim(x)[2]
b0 <- array(rep(0, k), dim = c(k, 1))
B0 = 1*diag(k)
n0 <- 5
d0 <- 8
FedTax <- quantregOR2(y = yOrd, x = xMat, b0, B0,
                      n0, d0, gammacp2 = 3, burn = 1125, mcmc = 4500, accutoff = 0.1,
                      p = 0.5, maxlags = 400, TRUE)

## << Trace_plots>>
mcmc <- 4500
burn <- round(0.25*mcmc)
nsim <- mcmc + burn
mcmcDraws <- cbind(t(FedTax$betadraws), t(FedTax$sigmadraws))
color_scheme_set('darkgray')
bayesplot_theme_set(theme_minimal())
mcmc_trace(mcmcDraws[(burn+1):nsim, ], facet_args = list(ncol = 3))

## <<Covariate_Effect>>
xMat1 <- xMat
xMat1$Computers <- 0
xMat2 <- xMat
xMat2$Computers <- 1
FedTaxCE <- covEffectOR2(FedTax, yOrd, xMat1, xMat2, gammacp2 = 3, p = 0.5,
                         verbose = TRUE)
