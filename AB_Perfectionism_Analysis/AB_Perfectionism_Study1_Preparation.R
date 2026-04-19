# AB Perfectionism - Study 1 (Merged Sample)

#-*-*-*-*-*-*-*-*-*#
# Data Preparation #
#-*-*-*-*-*-*-*-*-*#

# Load libraries
library(rmarkdown)
library(shiny)

library(readxl) # For reading excel files
library(dplyr) # For reshaping and summarizing data

library(ggplot2) # For plots
library(gridExtra) # For arranging ggplots in a grid
library(grid) # For grob functions
library(sjPlot) # For plotting interactions
library(ggeffects) # For plotting gamlss

library(psych) # For Cronbach's alpha and McDonald's omega
library(car) # For inverse logit function
library(pastecs) # For testing normality assumptions

library(gamlss) # For gamlss fitting
library(lmtest) # For likelihood ratio tests
library(boot) # For inverse logit function

# Questionnaires
#---------------

# Read data sample 1
qu1.1 <- read_excel("AB_Perfectionism_Data/questionnaires_study1.1.xlsx")
qu1.2 <- read_excel("AB_Perfectionism_Data/questionnaires_study1.2.xlsx")

qu1.1$ID <- as.factor(qu1.1$ID)
qu1.2$ID <- as.factor(qu1.2$ID)
qu1 <- qu1.1 %>% inner_join(qu1.2, by = "ID")
qu1 <- qu1 %>% rename(subject_nr = ID_EyeTracker)

# Read data sample 2
qu2.1 <- read_excel("AB_Perfectionism_Data/questionnaires_study2.1.xlsx")
qu2.2 <- read_excel("AB_Perfectionism_Data/questionnaires_study2.2.xlsx")
qu2.3_1 <- read_excel("AB_Perfectionism_Data/questionnaires_study2.3_1.xlsx")
qu2.3_2 <- read_excel("AB_Perfectionism_Data/questionnaires_study2.3_2.xlsx")
qu2.3_3 <- read_excel("AB_Perfectionism_Data/questionnaires_study2.3_3.xlsx")

qu2a <- qu2.1 %>% inner_join(qu2.2, by = "ID")
qu2b <- qu2.3_1 %>% inner_join(qu2.3_2, by = "ID") %>% inner_join(qu2.3_3, by = "ID")
qu2 <- qu2a %>% inner_join(qu2b, by = "ID")

qu2 <- qu2 %>% rename(subject_nr = ID_EyeTracker_1)
qu2$subject_nr <- as.numeric(qu2$subject_nr)
qu2$Age <- as.numeric(qu2$Age)
qu2$GAD7_1 <- as.numeric(qu2$GAD7_1)
qu2$GAD7_2 <- as.numeric(qu2$GAD7_2)
qu2$GAD7_3 <- as.numeric(qu2$GAD7_3)
qu2$GAD7_4 <- as.numeric(qu2$GAD7_4)
qu2$GAD7_5 <- as.numeric(qu2$GAD7_5)
qu2$GAD7_6 <- as.numeric(qu2$GAD7_6)
qu2$GAD7_7 <- as.numeric(qu2$GAD7_7)

# Merge questionnaire samples
qu <- qu1 %>% full_join(qu2)

# HMPS reverse coding
qu[, c("HMPS_2", "HMPS_3", "HMPS_4", "HMPS_8", "HMPS_9", "HMPS_10", "HMPS_12",
       "HMPS_19", "HMPS_21", "HMPS_24", "HMPS_30", "HMPS_34", "HMPS_36", "HMPS_37",
       "HMPS_38", "HMPS_43", "HMPS_44", "HMPS_45")] <-
  8 - qu[, c("HMPS_2", "HMPS_3", "HMPS_4", "HMPS_8", "HMPS_9", "HMPS_10", "HMPS_12",
             "HMPS_19", "HMPS_21", "HMPS_24", "HMPS_30", "HMPS_34", "HMPS_36", "HMPS_37",
             "HMPS_38", "HMPS_43", "HMPS_44", "HMPS_45")]

# HMPS subscales
SOP <- qu[, c("HMPS_1", "HMPS_6", "HMPS_8", "HMPS_12", "HMPS_14", "HMPS_15", "HMPS_17",
              "HMPS_20", "HMPS_23", "HMPS_28", "HMPS_32", "HMPS_34", "HMPS_36", "HMPS_40", "HMPS_42")]
OOP <- qu[, c("HMPS_2", "HMPS_3", "HMPS_4", "HMPS_7", "HMPS_10", "HMPS_16", "HMPS_19",
              "HMPS_22", "HMPS_24", "HMPS_26", "HMPS_27", "HMPS_29", "HMPS_38", "HMPS_43", "HMPS_45")]
SPP <- qu[, c("HMPS_5", "HMPS_9", "HMPS_11", "HMPS_13", "HMPS_18", "HMPS_21", "HMPS_25",
              "HMPS_30", "HMPS_31", "HMPS_33", "HMPS_35", "HMPS_37", "HMPS_39", "HMPS_41", "HMPS_44")]

qu$SOP <- rowSums(SOP)
qu$OOP <- rowSums(OOP)
qu$SPP <- rowSums(SPP)

# FMPS subscales
PS <- qu[, c("FMPS_4", "FMPS_6", "FMPS_12", "FMPS_16", "FMPS_19", "FMPS_24", "FMPS_30")]
PE <- qu[, c("FMPS_1", "FMPS_11", "FMPS_15", "FMPS_20", "FMPS_26")]
PC <- qu[, c("FMPS_3", "FMPS_5", "FMPS_22", "FMPS_35")]
CM <- qu[, c("FMPS_9", "FMPS_10", "FMPS_13", "FMPS_14", "FMPS_18", "FMPS_21", "FMPS_23", "FMPS_25", "FMPS_34")]
DA <- qu[, c("FMPS_17", "FMPS_28", "FMPS_32", "FMPS_33")]
O  <- qu[, c("FMPS_2", "FMPS_7", "FMPS_8", "FMPS_27", "FMPS_29", "FMPS_31")]

qu$PS <- rowSums(PS)
qu$PE <- rowSums(PE)
qu$PC <- rowSums(PC)
qu$CM <- rowSums(CM)
qu$DA <- rowSums(DA)
qu$O  <- rowSums(O)

# CES-D reverse coding and totals
qu[, c("CESD_4", "CESD_8", "CESD_12", "CESD_16")] <-
  3 - qu[, c("CESD_4", "CESD_8", "CESD_12", "CESD_16")]

CESD <- qu[, c("CESD_1", "CESD_2", "CESD_3", "CESD_4", "CESD_5", "CESD_6", "CESD_7", "CESD_8",
               "CESD_9", "CESD_10", "CESD_11", "CESD_12", "CESD_13", "CESD_14", "CESD_15",
               "CESD_16", "CESD_17", "CESD_18", "CESD_19", "CESD_20")]
qu$CESD <- rowSums(CESD)

# GAD-7 totals
GAD7 <- qu[, c("GAD7_1", "GAD7_2", "GAD7_3", "GAD7_4", "GAD7_5", "GAD7_6", "GAD7_7")]
qu$GAD7 <- rowSums(GAD7)

# Standardize questionnaire variables
qu[, c("ZSOP", "ZOOP", "ZSPP", "ZPS", "ZPC", "ZPE", "ZCM", "ZDA", "ZO", "ZGAD7", "ZCESD")] <-
  scale(qu[, c("SOP", "OOP", "SPP", "PS", "PC", "PE", "CM", "DA", "O", "GAD7", "CESD")])

# Identify univariate outliers
univar_out <- c(
  which(qu$ZSOP > 3 | qu$ZSOP < -3),
  which(qu$ZOOP > 3 | qu$ZOOP < -3),
  which(qu$ZSPP > 3 | qu$ZSPP < -3),
  which(qu$ZPS > 3 | qu$ZPS < -3),
  which(qu$ZPE > 3 | qu$ZPE < -3),
  which(qu$ZPC > 3 | qu$ZPC < -3),
  which(qu$ZCM > 3 | qu$ZCM < -3),
  which(qu$ZDA > 3 | qu$ZDA < -3),
  which(qu$ZO > 3 | qu$ZO < -3),
  which(qu$ZGAD7 > 3 | qu$ZGAD7 < -3),
  which(qu$ZCESD > 3 | qu$ZCESD < -3)
)

univar_out <- unique(univar_out)
length(univar_out)

# Remove univariate outliers
qu <- qu[-univar_out, ]

# Identify multivariate outliers
vars <- qu[, c("SOP", "OOP", "SPP", "PS", "PC", "PE", "CM", "DA", "O", "GAD7", "CESD")]
md <- mahalanobis(vars, center = colMeans(vars), cov = cov(vars))
cutoff <- qchisq(p = 1 - .001, df = ncol(vars))
multivar_out_MH <- which(md > cutoff)

length(multivar_out_MH)

# Perfectionism dimensions
qu$Perf_Strivings <- rowSums(qu[, c("ZPS", "ZSOP")])
qu$Perf_Concerns  <- rowSums(qu[, c("ZCM", "ZDA", "ZSPP")])

# Reliability estimates
alphaSOP <- psych::alpha(SOP)
omegaSOP <- psych::omega(SOP, plot = FALSE)
alphaOOP <- psych::alpha(OOP)
omegaOOP <- psych::omega(OOP, plot = FALSE)
alphaSPP <- psych::alpha(SPP)
omegaSPP <- psych::omega(SPP, plot = FALSE)

alphaPS <- psych::alpha(PS)
omegaPS <- psych::omega(PS, plot = FALSE)
alphaPE <- psych::alpha(PE)
omegaPE <- psych::omega(PE, plot = FALSE)
alphaPC <- psych::alpha(PC)
omegaPC <- psych::omega(PC, plot = FALSE)
alphaCM <- psych::alpha(CM)
omegaCM <- psych::omega(CM, plot = FALSE)
alphaDA <- psych::alpha(DA)
omegaDA <- psych::omega(DA, plot = FALSE)
alphaO  <- psych::alpha(O)
omegaO  <- psych::omega(O, plot = FALSE)

alphaPerf_Strivings <- psych::alpha(qu[, c("ZPS", "ZSOP")])
alphaPerf_Concerns  <- psych::alpha(qu[, c("ZCM", "ZDA", "ZSPP")])

alphaGAD7 <- psych::alpha(GAD7)
omegaGAD7 <- psych::omega(GAD7, plot = FALSE)
alphaCESD <- psych::alpha(CESD)
omegaCESD <- psych::omega(CESD, plot = FALSE)

# Descriptive statistics
round(describe(qu[c("SOP", "OOP", "SPP", "PS", "PE", "PC", "CM", "DA", "O",
                    "Perf_Strivings", "Perf_Concerns", "GAD7", "CESD")]), 2)

# Correlations
corr_table <- corr.test(qu[c("SOP", "OOP", "SPP", "PS", "PE", "PC", "CM", "DA", "O",
                             "Perf_Strivings", "Perf_Concerns", "GAD7", "CESD")])$stars
print(corr_table)

# Response Times
#---------------

# Read data
rt1 <- read_excel("AB_Perfectionism_Data/rt_study1.xlsx")
rt2 <- read_excel("AB_Perfectionism_Data/rt_study2.xlsx")

# Separate T1 and T2
t1 <- which(rt2$Temps_de_mesure == "T1")
rt2_1 <- rt2[t1, ]

# Merge Study 1 and baseline Study 2 data
rt2_1 <- subset(rt2_1, select = -ID) %>% rename(ID = Code)
rt <- rt1 %>% full_join(rt2_1)

# Remove incorrect and RT < 200 ms
rt$correct_response <- as.factor(rt$correct_response) %>%
  dplyr::recode("Vrai" = 1, "Faux" = 0)

incorrect <- which(rt$correct_response == 0)
rt200 <- which(rt$response_time < 200)

length(incorrect)
length(rt200)

rt <- rt[-c(incorrect, rt200), ]

# Code rule awareness
rt <- rt %>% rename(guessed_block_rule = response_keyboard_response)
rt$guessed_block_rule <- as.factor(rt$guessed_block_rule) %>%
  dplyr::recode("o" = 1, "O" = 1, "n" = 0, "N" = 0)

# Merge RT data with questionnaire data
rt_qu <- rt[c("subject_nr", "Change_category", "response_time", "block_loop",
              "guessed_block_rule")]  %>% 
  inner_join(qu[c("subject_nr", "Perf_Strivings", "Perf_Concerns", "CESD", "GAD7",
                  "Sexe", "Age", "Statut","Actual_Studies","University_HE",
                  "Actual_Program","DomaineTravail", "Mother_Tongue")], 
             relationship = "many-to-many")

# demographics
rt_qu$Sexe <- as.factor(rt_qu$Sexe) %>% dplyr::recode("Femme\r\n" = 0, "Homme\r\n" = 1)
sex <- rt_qu[c("subject_nr","Sexe")] %>% group_by(subject_nr) %>% summarise(Sexe = mean(Sexe))
sex$Sexe <- as.factor(sex$Sexe) %>% dplyr::recode("0" = "Female", "1" = "Male")
table(sex$Sexe)

round(describe(rt_qu$Age),2)

# response time
round(describe(rt_qu$response_time),2)
rt_qu[c("response_time", "Change_category")] %>% group_by(Change_category) %>% 
  summarise(M = round(mean(response_time),2), SD = round(sd(response_time),2))

# Eye Tracking
#-------------
# Read data
et1 <- read_excel("AB_Perfectionism_Data/et_study1_500to5500.xlsx")
et2 <- read_excel("AB_Perfectionism_Data/et_study2_500to5500.xlsx")

et1 <- et1 %>% rename(subject_nr = RECORDING_SESSION_LABEL)
et2 <- et2 %>% rename(subject_nr = RECORDING_SESSION_LABEL)

t1 <- which(et2$MEASUREMENT_TIME == "T1")
et2_1 <- et2[t1, ]

et <- et1 %>% full_join(et2_1)

# Recode labels and variables used for index computation
et$IA_LABEL <- as.factor(et$IA_LABEL)
et$IA_LABEL <- et$IA_LABEL %>%
  dplyr::recode("Neutre" = "Neutral",
                "Pos" = "Positive",
                "Neg" = "Negative",
                "Pos_Perf" = "Success",
                "Neg_Perf" = "Failure")

et$IA_FIRST_FIXATION_VISITED_IA_COUNT <- as.factor(et$IA_FIRST_FIXATION_VISITED_IA_COUNT)
et$IA_FIRST_FIXATION_VISITED_IA_COUNT <- et$IA_FIRST_FIXATION_VISITED_IA_COUNT %>%
  dplyr::recode("0" = "1", "1" = "1", "2" = "0", "3" = "0", "4" = "0")
et$IA_FIRST_FIXATION_VISITED_IA_COUNT <- as.numeric(as.character(et$IA_FIRST_FIXATION_VISITED_IA_COUNT))

et$IA_FIRST_FIXATION_TIME <- as.numeric(et$IA_FIRST_FIXATION_TIME)

# Remove out of bounds values
out_first_fix_latency <- which(et$IA_FIRST_FIXATION_TIME > 5000)
length(out_first_fix_latency)

et$IA_FIRST_FIXATION_TIME[out_first_fix_latency] <- NA

#check missing values
length(which(is.na(et$IA_FIRST_FIXATION_VISITED_IA_COUNT))) #3090
length(which(is.na(et$IA_FIRST_FIXATION_TIME))) #3571
length(which(is.na(et$IA_DWELL_TIME))) #0
length(which(is.na(et$IA_DWELL_TIME_500to1500))) #0
length(which(is.na(et$IA_DWELL_TIME_1500to3500))) #0
length(which(is.na(et$IA_DWELL_TIME_3500to5500))) #0

# Calculate ET indices
# Merge ET with RT to recover block-level rule variables
etrt <- et %>% inner_join(rt, relationship = "many-to-many")

et_a <- etrt[c("subject_nr", "block_loop", "IA_LABEL", "Change_category", "guessed_block_rule",
               "IA_DWELL_TIME", "IA_DWELL_TIME_500to1500", "IA_DWELL_TIME_1500to3500", "IA_DWELL_TIME_3500to5500",
               "IA_FIRST_FIXATION_VISITED_IA_COUNT", "IA_FIRST_FIXATION_TIME")] %>%
  group_by(subject_nr, block_loop, IA_LABEL, Change_category, guessed_block_rule) %>%
  reframe(
    IA_FIRST_FIXATION_PROBABILITY = mean(na.omit(IA_FIRST_FIXATION_VISITED_IA_COUNT)),
    IA_FIRST_FIXATION_TIME_BLOCKMEAN = mean(na.omit(IA_FIRST_FIXATION_TIME)),
    IA_DWELL_TIME_BLOCKMEAN = mean(IA_DWELL_TIME),
    IA_DWELL_TIME_500to1500_BLOCKMEAN = mean(IA_DWELL_TIME_500to1500),
    IA_DWELL_TIME_1500to3500_BLOCKMEAN = mean(IA_DWELL_TIME_1500to3500),
    IA_DWELL_TIME_3500to5500_BLOCKMEAN = mean(IA_DWELL_TIME_3500to5500)
  )

et_b <- et_a %>%
  group_by(subject_nr, block_loop) %>%
  summarise(
    AVG_FIRST_FIXATION_TIME = sum(na.omit(IA_FIRST_FIXATION_TIME_BLOCKMEAN)),
    AVG_DWELL_TIME = sum(na.omit(IA_DWELL_TIME_BLOCKMEAN)),
    AVG_DWELL_TIME_500to1500 = sum(IA_DWELL_TIME_500to1500_BLOCKMEAN),
    AVG_DWELL_TIME_1500to3500 = sum(IA_DWELL_TIME_1500to3500_BLOCKMEAN),
    AVG_DWELL_TIME_3500to5500 = sum(IA_DWELL_TIME_3500to5500_BLOCKMEAN)
  )

et <- et_a %>% inner_join(et_b, relationship = "many-to-many")

et$first_fix_prob <- et$IA_FIRST_FIXATION_PROBABILITY
et$first_fix_latency <- et$IA_FIRST_FIXATION_TIME_BLOCKMEAN / et$AVG_FIRST_FIXATION_TIME
et$dwell_time <- et$IA_DWELL_TIME_BLOCKMEAN / et$AVG_DWELL_TIME
et$dwell_time_early <- et$IA_DWELL_TIME_500to1500_BLOCKMEAN / et$AVG_DWELL_TIME_500to1500
et$dwell_time_intermediate <- et$IA_DWELL_TIME_1500to3500_BLOCKMEAN / et$AVG_DWELL_TIME_1500to3500
et$dwell_time_late <- et$IA_DWELL_TIME_3500to5500_BLOCKMEAN / et$AVG_DWELL_TIME_3500to5500

# Remove remaining missing values
missing_ffp <- which(is.na(et$first_fix_prob))
missing_ffl <- which(is.na(et$first_fix_latency))
et <- et[-unique(c(missing_ffp, missing_ffl)), ]

# Merge ET data with questionnaire data
et_qu <- et %>%
  inner_join(
    qu[c("subject_nr", "Perf_Strivings", "Perf_Concerns", "CESD", "GAD7",
         "Sexe", "Age", "Statut", "Actual_Studies", "University_HE",
         "Actual_Program", "DomaineTravail", "Mother_Tongue")],
    relationship = "many-to-many"
  )

round(describe(et_qu[c("first_fix_prob", "first_fix_latency", "dwell_time",
                       "dwell_time_early", "dwell_time_intermediate", "dwell_time_late")]), 2)

et_qu[c("IA_LABEL", "first_fix_prob", "first_fix_latency", "dwell_time",
        "dwell_time_early", "dwell_time_intermediate", "dwell_time_late")] %>%
  group_by(IA_LABEL) %>%
  summarise(
    M_first_fix_prob = round(mean(first_fix_prob), 2),
    SD_first_fix_prob = round(sd(first_fix_prob), 2),
    M_first_fix_latency = round(mean(first_fix_latency), 2),
    SD_first_fix_latency = round(sd(first_fix_latency), 2),
    M_dwell_time = round(mean(dwell_time), 2),
    SD_dwell_time = round(sd(dwell_time), 2),
    M_dwell_time_early = round(mean(dwell_time_early), 2),
    SD_dwell_time_early = round(sd(dwell_time_early), 2),
    M_dwell_time_intermediate = round(mean(dwell_time_intermediate), 2),
    SD_dwell_time_intermediate = round(sd(dwell_time_intermediate), 2),
    M_dwell_time_late = round(mean(dwell_time_late), 2),
    SD_dwell_time_late = round(sd(dwell_time_late), 2)
  )

# Model inputs and predictor coding
#----------------------------------

# Code semantic class 

  # RT
rt_qu$Change_category_EV <- 0
rt_qu$Change_category_EV[rt_qu$Change_category == "Failure" | rt_qu$Change_category == "Negative"] <- 1
rt_qu$Change_category_EV[rt_qu$Change_category == "Success" | rt_qu$Change_category == "Positive"] <- -1

rt_qu$Change_category_PR_SvsF <- 0
rt_qu$Change_category_PR_SvsF[rt_qu$Change_category == "Success"] <- -1
rt_qu$Change_category_PR_SvsF[rt_qu$Change_category == "Failure"] <- 1

rt_qu$Change_category_PR_LvsH <- -2
rt_qu$Change_category_PR_LvsH[rt_qu$Change_category == "Success"] <- 1
rt_qu$Change_category_PR_LvsH[rt_qu$Change_category == "Failure"] <- 1

  # ET
et_qu$IA_LABEL_EV <- 0
et_qu$IA_LABEL_EV[et_qu$IA_LABEL == "Failure" | et_qu$IA_LABEL == "Negative"] <- 1
et_qu$IA_LABEL_EV[et_qu$IA_LABEL == "Success" | et_qu$IA_LABEL == "Positive"] <- -1

et_qu$IA_LABEL_PR_SvsF <- 0
et_qu$IA_LABEL_PR_SvsF[et_qu$IA_LABEL == "Success"] <- -1
et_qu$IA_LABEL_PR_SvsF[et_qu$IA_LABEL == "Failure"] <- 1

et_qu$IA_LABEL_PR_LvsH <- -2
et_qu$IA_LABEL_PR_LvsH[et_qu$IA_LABEL == "Success"] <- 1
et_qu$IA_LABEL_PR_LvsH[et_qu$IA_LABEL == "Failure"] <- 1

# Center continuous predictors
qu$Perf_Strivings_c <- qu$Perf_Strivings - mean(qu$Perf_Strivings)
qu$Perf_Concerns_c <- qu$Perf_Concerns - mean(qu$Perf_Concerns)

rt_qu$Perf_Strivings_c <- rt_qu$Perf_Strivings - mean(rt_qu$Perf_Strivings)
rt_qu$Perf_Concerns_c <- rt_qu$Perf_Concerns - mean(rt_qu$Perf_Concerns)

et_qu$Perf_Strivings_c <- et_qu$Perf_Strivings - mean(et_qu$Perf_Strivings)
et_qu$Perf_Concerns_c <- et_qu$Perf_Concerns - mean(et_qu$Perf_Concerns)

# Convert smooth terms to numeric
rt_qu$subject_nr <- as.numeric(rt_qu$subject_nr)
rt_qu$block_loop <- as.numeric(rt_qu$block_loop)

et_qu$subject_nr <- as.numeric(et_qu$subject_nr)
et_qu$block_loop <- as.numeric(et_qu$block_loop)

# Code change category
et_qu$Change_category <- as.factor(et_qu$Change_category)
et_qu$Change_category01 <- 0
inds <- which(et_qu$IA_LABEL == et_qu$Change_category)
et_qu$Change_category01[inds] <- +1

# Model Diagnostics functions
#----------------------------

# Quick residuals vs fitted plot
diag_resid_fitted <- function(model) {
  
  ggplot(
    data.frame(
      fitted = predict(model),
      residuals = resid(model)
    ),
    aes(x = fitted, y = residuals)
  ) +
    geom_point(alpha = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    ggtitle(paste("Residuals vs. Fitted Values:", deparse(substitute(model)))) +
    theme_minimal()
}


# Quick posterior predictive check for GAMLSS
diag_ppc_beta <- function(model) {
  mu_hat <- fitted(model, "mu")
  sigma_hat <- fitted(model, "sigma")
  
  phi_hat <- (1 / sigma_hat^2) - 1
  
  set.seed(123)
  predicted_values <- rbeta(
    length(mu_hat),
    shape1 = mu_hat * phi_hat,
    shape2 = (1 - mu_hat) * phi_hat
  )
  
  observed_values <- model$y
  
  ggplot() +
    geom_density(aes(x = observed_values, color = "Observed"), linetype = "dashed", linewidth = 1) +
    geom_density(aes(x = predicted_values, color = "Predicted"), linewidth = 1) +
    labs(
      title = paste("Observed vs. Predicted Values:", deparse(substitute(model))),
      x = "Value",
      y = "Probability Density"
    ) +
    scale_color_manual(values = c("Observed" = "blue", "Predicted" = "red")) +
    theme_minimal()
}

diag_ppc_gamma <- function(model) {
  mu_hat <- fitted(model, "mu")
  sigma_hat <- fitted(model, "sigma")
  
  alpha_hat <- 1 / (sigma_hat^2)
  beta_hat  <- mu_hat / alpha_hat
  
  set.seed(123)
  predicted_values <- rgamma(length(mu_hat), shape = alpha_hat, scale = beta_hat)
  
  observed_values <- model$y
  
  ggplot() +
    geom_density(aes(x = observed_values, color = "Observed"), linetype = "dashed", linewidth = 1) +
    geom_density(aes(x = predicted_values, color = "Predicted"), linewidth = 1) +
    labs(
      title = paste("Observed vs. Predicted Values:", deparse(substitute(model))),
      x = "Value",
      y = "Probability Density"
    ) +
    scale_color_manual(values = c("Observed" = "blue", "Predicted" = "red")) +
    theme_minimal()
}
