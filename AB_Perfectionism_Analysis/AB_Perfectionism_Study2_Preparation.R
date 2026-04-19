# AB Perfectionism - Study 2

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
library(lme4) # For lmm fitting
library(lmtest) # For likelihood ratio tests
library(boot) # For inverse logit function

# Questionnaires
#---------------

# Read data
qu2.1 <- read_excel("AB_Perfectionism_Data/questionnaires_study2.1.xlsx")
qu2.2 <- read_excel("AB_Perfectionism_Data/questionnaires_study2.2.xlsx")
qu2.3_1 <- read_excel("AB_Perfectionism_Data/questionnaires_study2.3_1.xlsx")
qu2.3_2 <- read_excel("AB_Perfectionism_Data/questionnaires_study2.3_2.xlsx")
qu2.3_3 <- read_excel("AB_Perfectionism_Data/questionnaires_study2.3_3.xlsx")

# Merge questionnaire files
qu2a <- qu2.1 %>% inner_join(qu2.2, by = "ID")
qu2b <- qu2.3_1 %>% inner_join(qu2.3_2, by = "ID") %>% inner_join(qu2.3_3, by = "ID")
qu2 <- qu2a %>% inner_join(qu2b, by = "ID")

# HMPS reverse coding
qu2[, c("HMPS_2", "HMPS_3", "HMPS_4", "HMPS_8", "HMPS_9", "HMPS_10", "HMPS_12",
        "HMPS_19", "HMPS_21", "HMPS_24", "HMPS_30", "HMPS_34", "HMPS_36", "HMPS_37",
        "HMPS_38", "HMPS_43", "HMPS_44", "HMPS_45")] <-
  8 - qu2[, c("HMPS_2", "HMPS_3", "HMPS_4", "HMPS_8", "HMPS_9", "HMPS_10", "HMPS_12",
              "HMPS_19", "HMPS_21", "HMPS_24", "HMPS_30", "HMPS_34", "HMPS_36", "HMPS_37",
              "HMPS_38", "HMPS_43", "HMPS_44", "HMPS_45")]

# HMPS subscales
SOP <- qu2[, c("HMPS_1", "HMPS_6", "HMPS_8", "HMPS_12", "HMPS_14", "HMPS_15", "HMPS_17",
               "HMPS_20", "HMPS_23", "HMPS_28", "HMPS_32", "HMPS_34", "HMPS_36", "HMPS_40", "HMPS_42")]
OOP <- qu2[, c("HMPS_2", "HMPS_3", "HMPS_4", "HMPS_7", "HMPS_10", "HMPS_16", "HMPS_19",
               "HMPS_22", "HMPS_24", "HMPS_26", "HMPS_27", "HMPS_29", "HMPS_38", "HMPS_43", "HMPS_45")]
SPP <- qu2[, c("HMPS_5", "HMPS_9", "HMPS_11", "HMPS_13", "HMPS_18", "HMPS_21", "HMPS_25",
               "HMPS_30", "HMPS_31", "HMPS_33", "HMPS_35", "HMPS_37", "HMPS_39", "HMPS_41", "HMPS_44")]

qu2$SOP <- rowSums(SOP)
qu2$OOP <- rowSums(OOP)
qu2$SPP <- rowSums(SPP)

# FMPS subscales
PS <- qu2[, c("FMPS_4", "FMPS_6", "FMPS_12", "FMPS_16", "FMPS_19", "FMPS_24", "FMPS_30")]
PE <- qu2[, c("FMPS_1", "FMPS_11", "FMPS_15", "FMPS_20", "FMPS_26")]
PC <- qu2[, c("FMPS_3", "FMPS_5", "FMPS_22", "FMPS_35")]
CM <- qu2[, c("FMPS_9", "FMPS_10", "FMPS_13", "FMPS_14", "FMPS_18", "FMPS_21", "FMPS_23", "FMPS_25", "FMPS_34")]
DA <- qu2[, c("FMPS_17", "FMPS_28", "FMPS_32", "FMPS_33")]
O  <- qu2[, c("FMPS_2", "FMPS_7", "FMPS_8", "FMPS_27", "FMPS_29", "FMPS_31")]

qu2$PS <- rowSums(PS)
qu2$PE <- rowSums(PE)
qu2$PC <- rowSums(PC)
qu2$CM <- rowSums(CM)
qu2$DA <- rowSums(DA)
qu2$O  <- rowSums(O)

# VAMS composites
POS_1 <- qu2[, c("VAMS1_1", "VAMS1_6", "VAMS1_7")]
NEG_1 <- qu2[, c("VAMS1_2", "VAMS1_3", "VAMS1_4", "VAMS1_5")]
POS_2 <- qu2[, c("VAMS2_1", "VAMS2_6", "VAMS2_7")]
NEG_2 <- qu2[, c("VAMS2_2", "VAMS2_3", "VAMS2_4", "VAMS2_5")]
POS_3 <- qu2[, c("VAMS3_1", "VAMS3_6", "VAMS3_7")]
NEG_3 <- qu2[, c("VAMS3_2", "VAMS3_3", "VAMS3_4", "VAMS3_5")]
POS_4 <- qu2[, c("VAMS4_1", "VAMS4_6", "VAMS4_7")]
NEG_4 <- qu2[, c("VAMS4_2", "VAMS4_3", "VAMS4_4", "VAMS4_5")]

qu2$POS_1 <- rowSums(POS_1)
qu2$NEG_1 <- rowSums(NEG_1)
qu2$POS_2 <- rowSums(POS_2)
qu2$NEG_2 <- rowSums(NEG_2)
qu2$POS_3 <- rowSums(POS_3)
qu2$NEG_3 <- rowSums(NEG_3)
qu2$POS_4 <- rowSums(POS_4)
qu2$NEG_4 <- rowSums(NEG_4)

# CES-D reverse coding and totals
qu2[, c("CESD_4", "CESD_8", "CESD_12", "CESD_16")] <-
  3 - qu2[, c("CESD_4", "CESD_8", "CESD_12", "CESD_16")]

CESD <- qu2[, c("CESD_1", "CESD_2", "CESD_3", "CESD_4", "CESD_5", "CESD_6", "CESD_7", "CESD_8",
                "CESD_9", "CESD_10", "CESD_11", "CESD_12", "CESD_13", "CESD_14", "CESD_15",
                "CESD_16", "CESD_17", "CESD_18", "CESD_19", "CESD_20")]
qu2$CESD <- rowSums(CESD)

# GAD-7 totals
qu2$GAD7_1 <- as.numeric(qu2$GAD7_1)
qu2$GAD7_2 <- as.numeric(qu2$GAD7_2)
qu2$GAD7_3 <- as.numeric(qu2$GAD7_3)
qu2$GAD7_4 <- as.numeric(qu2$GAD7_4)
qu2$GAD7_5 <- as.numeric(qu2$GAD7_5)
qu2$GAD7_6 <- as.numeric(qu2$GAD7_6)
qu2$GAD7_7 <- as.numeric(qu2$GAD7_7)

GAD7 <- qu2[, c("GAD7_1", "GAD7_2", "GAD7_3", "GAD7_4", "GAD7_5", "GAD7_6", "GAD7_7")]
qu2$GAD7 <- rowSums(GAD7)

# Standardize questionnaire variables
qu2[, c("ZSOP", "ZOOP", "ZSPP", "ZPS", "ZPC", "ZPE", "ZCM", "ZDA", "ZO", "ZGAD7", "ZCESD",
        "ZPOS_1", "ZNEG_1", "ZPOS_2", "ZNEG_2", "ZPOS_3", "ZNEG_3", "ZPOS_4", "ZNEG_4")] <-
  scale(qu2[, c("SOP", "OOP", "SPP", "PS", "PC", "PE", "CM", "DA", "O", "GAD7", "CESD",
                "POS_1", "NEG_1", "POS_2", "NEG_2", "POS_3", "NEG_3", "POS_4", "NEG_4")])

# Identify univariate outliers
univar_out <- c(
  which(qu2$ZSOP > 3 | qu2$ZSOP < -3),
  which(qu2$ZOOP > 3 | qu2$ZOOP < -3),
  which(qu2$ZSPP > 3 | qu2$ZSPP < -3),
  which(qu2$ZPS > 3 | qu2$ZPS < -3),
  which(qu2$ZPE > 3 | qu2$ZPE < -3),
  which(qu2$ZPC > 3 | qu2$ZPC < -3),
  which(qu2$ZCM > 3 | qu2$ZCM < -3),
  which(qu2$ZDA > 3 | qu2$ZDA < -3),
  which(qu2$ZO > 3 | qu2$ZO < -3),
  which(qu2$ZGAD7 > 3 | qu2$ZGAD7 < -3),
  which(qu2$ZCESD > 3 | qu2$ZCESD < -3),
  which(qu2$ZPOS_1 > 3 | qu2$ZPOS_1 < -3),
  which(qu2$ZNEG_1 > 3 | qu2$ZNEG_1 < -3),
  which(qu2$ZPOS_2 > 3 | qu2$ZPOS_2 < -3),
  which(qu2$ZNEG_2 > 3 | qu2$ZNEG_2 < -3),
  which(qu2$ZPOS_3 > 3 | qu2$ZPOS_3 < -3),
  which(qu2$ZNEG_3 > 3 | qu2$ZNEG_3 < -3),
  which(qu2$ZPOS_4 > 3 | qu2$ZPOS_4 < -3),
  which(qu2$ZNEG_4 > 3 | qu2$ZNEG_4 < -3)
)

univar_out <- unique(univar_out)

# Remove univariate outliers
qu2 <- qu2[-univar_out, ]

# Identify multivariate outliers
vars <- qu2[, c("SOP", "OOP", "SPP", "PS", "PC", "PE", "CM", "DA", "O",
                "POS_1", "NEG_1", "POS_2", "NEG_2", "POS_3", "NEG_3", "POS_4", "NEG_4")]
md <- mahalanobis(vars, center = colMeans(vars), cov = cov(vars))
cutoff <- qchisq(p = 1 - .001, df = ncol(vars))
multivar_out_MH <- which(md > cutoff)

length(multivar_out_MH)

# Remove one participant flagged administratively as outlier in the condition variable
which(qu2$Condition == "Outlier")
qu2 <- qu2[-which(qu2$Condition == "Outlier"), ]

# Perfectionism dimensions
qu2$Perf_Strivings <- rowSums(qu2[, c("ZPS", "ZSOP")])
qu2$Perf_Concerns  <- rowSums(qu2[, c("ZCM", "ZDA", "ZSPP")])

# Standardize repeated mood scores before computing change indices
qu2[, c("ZPOS_2", "ZPOS_3", "ZPOS_4", "ZNEG_2", "ZNEG_3", "ZNEG_4")] <-
  scale(qu2[, c("POS_2", "POS_3", "POS_4", "NEG_2", "NEG_3", "NEG_4")])

# ABM-related change
qu2$POS_23 <- qu2$ZPOS_3 - qu2$ZPOS_2
qu2$NEG_23 <- qu2$ZNEG_3 - qu2$ZNEG_2

# Tangram-related change
qu2$POS_34 <- qu2$ZPOS_4 - qu2$ZPOS_3
qu2$NEG_34 <- qu2$ZNEG_4 - qu2$ZNEG_3

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

alphaPerf_Strivings <- psych::alpha(qu2[, c("ZPS", "ZSOP")])
alphaPerf_Concerns  <- psych::alpha(qu2[, c("ZCM", "ZDA", "ZSPP")])

alphaPOS_1 <- psych::alpha(POS_1)
omegaPOS_1 <- psych::omega(POS_1, plot = FALSE)
alphaNEG_1 <- psych::alpha(NEG_1)
omegaNEG_1 <- psych::omega(NEG_1, plot = FALSE)

alphaPOS_2 <- psych::alpha(POS_2)
omegaPOS_2 <- psych::omega(POS_2, plot = FALSE)
alphaNEG_2 <- psych::alpha(NEG_2)
omegaNEG_2 <- psych::omega(NEG_2, plot = FALSE)

alphaPOS_3 <- psych::alpha(POS_3)
omegaPOS_3 <- psych::omega(POS_3, plot = FALSE)
alphaNEG_3 <- psych::alpha(NEG_3)
omegaNEG_3 <- psych::omega(NEG_3, plot = FALSE)

alphaPOS_4 <- psych::alpha(POS_4)
omegaPOS_4 <- psych::omega(POS_4, plot = FALSE)
alphaNEG_4 <- psych::alpha(NEG_4)
omegaNEG_4 <- psych::omega(NEG_4, plot = FALSE)

alphaGAD7 <- psych::alpha(GAD7)
omegaGAD7 <- psych::omega(GAD7, plot = FALSE)
alphaCESD <- psych::alpha(CESD)
omegaCESD <- psych::omega(CESD, plot = FALSE)

# Descriptive statistics
round(describe(qu2[c("SOP", "OOP", "SPP", "PS", "PE", "PC", "CM", "DA", "O",
                     "Perf_Strivings", "Perf_Concerns",
                     "POS_23", "NEG_23", "POS_34", "NEG_34", "CESD", "GAD7")]), 2)

# Correlations
corr_table <- corr.test(qu2[c("Perf_Strivings", "Perf_Concerns",
                              "POS_23", "NEG_23", "POS_34", "NEG_34", "CESD", "GAD7")])$stars
print(corr_table)

# Response Times
#---------------

# Read data
rt2 <- read_excel("AB_Perfectionism_Data/rt_study2.xlsx")

# Remove incorrect and RT < 200 ms
rt2$correct_response <- as.factor(rt2$correct_response) %>%
  dplyr::recode("Vrai" = 1, "Faux" = 0)

correct_responses <- rt2[,c("subject_nr", "correct_response", "Temps_de_mesure")] %>% group_by(subject_nr, Temps_de_mesure) %>% summarize(correct_response = sum(correct_response))

incorrect <- which(rt2$correct_response == 0)
rt200 <- which(rt2$response_time < 200)

length(incorrect)
length(rt200)

rt2$response_time[c(incorrect, rt200)] <- NA

# Code rule awareness
rt2 <- rt2 %>% rename(guessed_block_rule = response_keyboard_response)
rt2$guessed_block_rule <- as.factor(rt2$guessed_block_rule) %>%
  dplyr::recode("o" = 1, "O" = 1, "n" = 0, "N" = 0)

# Separate T1 and T2
t1 <- which(rt2$Temps_de_mesure == "T1")
t2 <- which(rt2$Temps_de_mesure == "T2")

rt2_1 <- rt2[t1, ]
rt2_2 <- rt2[t2, ]

drop <- c("Temps_de_mesure", "ID", "datetime", "response_position", "participant_response", "correct_response")

rt2_1 <- rt2_1[, !(names(rt2_1) %in% drop)] %>%
  rename(subject_nr_1 = subject_nr,
         response_time_1 = response_time,
         guessed_block_rule_1 = guessed_block_rule)

rt2_2 <- rt2_2[, !(names(rt2_2) %in% drop)] %>%
  rename(subject_nr_2 = subject_nr,
         response_time_2 = response_time,
         guessed_block_rule_2 = guessed_block_rule)

rt <- rt2_1 %>%
  inner_join(
    rt2_2,
    by = c("Code", "Change_category", "Change_location", "Change_word", "Flicker_word",
           "A", "B", "C", "D", "E",
           "OS_A_word", "OS_B_word", "OS_C_word", "OS_D_word", "OS_E_word",
           "CS_A_word", "CS_B_word", "CS_C_word", "CS_D_word", "CS_E_word", "block_loop")
  )

rt$response_time_diff <- rt$response_time_2 - rt$response_time_1

# Merge rt data with questionnaire data
rt_qu2 <- rt[c("Code",  "block_loop", "Change_category",
               "subject_nr_1","response_time_1", "guessed_block_rule_1", 
               "subject_nr_2","response_time_2", "guessed_block_rule_2", 
               "response_time_diff")]  %>% 
  rename(ID = Code) %>%
  inner_join(qu2[c("ID", "Condition", "Perf_Strivings", "Perf_Concerns",
                   "POS_23", "NEG_23", "POS_34", "NEG_34", "CESD", "GAD7",
                   "Age", "Sexe", "Statut","Actual_Studies","University_HE",
                   "Actual_Program","DomaineTravail", "Mother_Tongue")], 
             relationship = "many-to-many")

# demographics
rt_qu2$Sexe <- as.factor(rt_qu2$Sexe) %>% dplyr::recode("Femme\r\n" = 0, "Homme\r\n" = 1)
sex <- rt_qu2[c("subject_nr_1", "Sexe")] %>% group_by(subject_nr_1) %>% summarise(Sexe = mean(Sexe))
sex$Sexe <- as.factor(sex$Sexe) %>% dplyr::recode("0" = "Female", "1" = "Male")
table(sex$Sexe)

round(describe(as.numeric(rt_qu2$Age)), 2)

# response time
round(describe(rt_qu2[c("response_time_1", "response_time_2", "response_time_diff")]), 2)

rt_qu2[c("Change_category", "response_time_1", "response_time_2", "response_time_diff")] %>%
  group_by(Change_category) %>%
  summarise(
    M_response_time_1 = round(mean(response_time_1, na.rm = TRUE), 2),
    SD_response_time_1 = round(sd(response_time_1, na.rm = TRUE), 2),
    M_response_time_2 = round(mean(response_time_2, na.rm = TRUE), 2),
    SD_response_time_2 = round(sd(response_time_2, na.rm = TRUE), 2),
    M_response_time_diff = round(mean(response_time_diff, na.rm = TRUE), 2),
    SD_response_time_diff = round(sd(response_time_diff, na.rm = TRUE), 2)
  )

# Eye Tracking
#-------------

# Read data
et2 <- read_excel("AB_Perfectionism_Data/et_study2_500to5500.xlsx")
#for merging later
et2 <- et2 %>% rename(subject_nr = RECORDING_SESSION_LABEL)

et2$IA_LABEL <- as.factor(et2$IA_LABEL)
et2$IA_LABEL <- et2$IA_LABEL %>%
  dplyr::recode("Neutre" = "Neutral",
                "Pos" = "Positive",
                "Neg" = "Negative",
                "Pos_Perf" = "Success",
                "Neg_Perf" = "Failure")

et2$IA_FIRST_FIXATION_VISITED_IA_COUNT <- as.factor(et2$IA_FIRST_FIXATION_VISITED_IA_COUNT)
et2$IA_FIRST_FIXATION_VISITED_IA_COUNT <- et2$IA_FIRST_FIXATION_VISITED_IA_COUNT %>%
  dplyr::recode("0" = "1", "1" = "1", "2" = "0", "3" = "0", "4" = "0")
et2$IA_FIRST_FIXATION_VISITED_IA_COUNT <- as.numeric(as.character(et2$IA_FIRST_FIXATION_VISITED_IA_COUNT))

et2$IA_FIRST_FIXATION_TIME <- as.numeric(et2$IA_FIRST_FIXATION_TIME)

# Calculate ET indices
# Merge ET with RT to recover block-level rule variables
et2$response_time <- as.numeric(et2$response_time)
etrt2 <- et2 %>% inner_join(rt2, relationship = "many-to-many")

et2a <- etrt2[c("Temps_de_mesure", "Code", "subject_nr", "block_loop", "IA_LABEL", "Change_category", "guessed_block_rule",
                "IA_DWELL_TIME", "IA_DWELL_TIME_500to1500", "IA_DWELL_TIME_1500to3500", "IA_DWELL_TIME_3500to5500",
                "IA_FIRST_FIXATION_VISITED_IA_COUNT", "IA_FIRST_FIXATION_TIME")] %>%
  group_by(Temps_de_mesure, Code, subject_nr, block_loop, IA_LABEL, Change_category, guessed_block_rule) %>%
  reframe(
    IA_FIRST_FIXATION_PROBABILITY = mean(na.omit(IA_FIRST_FIXATION_VISITED_IA_COUNT)),
    IA_FIRST_FIXATION_TIME_BLOCKMEAN = mean(na.omit(IA_FIRST_FIXATION_TIME)),
    IA_DWELL_TIME_BLOCKMEAN = mean(IA_DWELL_TIME),
    IA_DWELL_TIME_500to1500_BLOCKMEAN = mean(IA_DWELL_TIME_500to1500),
    IA_DWELL_TIME_1500to3500_BLOCKMEAN = mean(IA_DWELL_TIME_1500to3500),
    IA_DWELL_TIME_3500to5500_BLOCKMEAN = mean(IA_DWELL_TIME_3500to5500)
  )

et2b <- et2a %>%
  group_by(subject_nr, block_loop) %>%
  summarise(
    AVG_FIRST_FIXATION_TIME = sum(na.omit(IA_FIRST_FIXATION_TIME_BLOCKMEAN)),
    AVG_DWELL_TIME = sum(IA_DWELL_TIME_BLOCKMEAN),
    AVG_DWELL_TIME_500to1500 = sum(IA_DWELL_TIME_500to1500_BLOCKMEAN),
    AVG_DWELL_TIME_1500to3500 = sum(IA_DWELL_TIME_1500to3500_BLOCKMEAN),
    AVG_DWELL_TIME_3500to5500 = sum(IA_DWELL_TIME_3500to5500_BLOCKMEAN)
  )

et2 <- et2a %>% inner_join(et2b, relationship = "many-to-many")

et2$first_fix_prob <- et2$IA_FIRST_FIXATION_PROBABILITY
et2$first_fix_latency <- et2$IA_FIRST_FIXATION_TIME_BLOCKMEAN / et2$AVG_FIRST_FIXATION_TIME
et2$dwell_time <- et2$IA_DWELL_TIME_BLOCKMEAN / et2$AVG_DWELL_TIME
et2$dwell_time_early <- et2$IA_DWELL_TIME_500to1500_BLOCKMEAN / et2$AVG_DWELL_TIME_500to1500
et2$dwell_time_intermediate <- et2$IA_DWELL_TIME_1500to3500_BLOCKMEAN / et2$AVG_DWELL_TIME_1500to3500
et2$dwell_time_late <- et2$IA_DWELL_TIME_3500to5500_BLOCKMEAN / et2$AVG_DWELL_TIME_3500to5500

# Remove remaining missing values
missing_ffp2 <- which(is.na(et2$first_fix_prob))
missing_ffl2 <- which(is.na(et2$first_fix_latency))
missing_dte2 <- which(is.na(et2$dwell_time_early))
missing_dtl2 <- which(is.na(et2$dwell_time_late))

et2 <- et2[-unique(c(missing_ffp2, missing_ffl2, missing_dte2, missing_dtl2)), ]

# Separate T1 and T2
t1 <- which(et2$Temps_de_mesure == "T1")
t2 <- which(et2$Temps_de_mesure == "T2")

et2_1 <- et2[t1, ]
et2_2 <- et2[t2, ]

et2_1 <- et2_1[c("Code", "subject_nr", "block_loop", "IA_LABEL", "Change_category",
                 "guessed_block_rule", "first_fix_prob", "first_fix_latency", "dwell_time",
                 "dwell_time_early", "dwell_time_intermediate", "dwell_time_late")] %>%
  rename(subject_nr_1 = subject_nr,
         first_fix_prob_1 = first_fix_prob,
         first_fix_latency_1 = first_fix_latency,
         dwell_time_1 = dwell_time,
         dwell_time_early_1 = dwell_time_early,
         dwell_time_intermediate_1 = dwell_time_intermediate,
         dwell_time_late_1 = dwell_time_late,
         guessed_block_rule_1 = guessed_block_rule)

et2_2 <- et2_2[c("Code", "subject_nr", "block_loop", "IA_LABEL", "Change_category",
                 "guessed_block_rule", "first_fix_prob", "first_fix_latency", "dwell_time",
                 "dwell_time_early", "dwell_time_intermediate", "dwell_time_late")] %>%
  rename(subject_nr_2 = subject_nr,
         first_fix_prob_2 = first_fix_prob,
         first_fix_latency_2 = first_fix_latency,
         dwell_time_2 = dwell_time,
         dwell_time_early_2 = dwell_time_early,
         dwell_time_intermediate_2 = dwell_time_intermediate,
         dwell_time_late_2 = dwell_time_late,
         guessed_block_rule_2 = guessed_block_rule)

et <- et2_1 %>% inner_join(et2_2, by = c("Code", "block_loop", "IA_LABEL", "Change_category"))

et$first_fix_prob_diff <- et$first_fix_prob_2 - et$first_fix_prob_1
et$first_fix_latency_diff <- et$first_fix_latency_2 - et$first_fix_latency_1
et$dwell_time_diff <- et$dwell_time_2 - et$dwell_time_1
et$dwell_time_early_diff <- et$dwell_time_early_2 - et$dwell_time_early_1
et$dwell_time_intermediate_diff <- et$dwell_time_intermediate_2 - et$dwell_time_intermediate_1
et$dwell_time_late_diff <- et$dwell_time_late_2 - et$dwell_time_late_1

# Merge ET with questionnaire data
et_qu2 <- et %>%
  rename(ID = Code) %>%
  inner_join(
    qu2[c("ID", "Condition", "Perf_Strivings", "Perf_Concerns",
          "POS_23", "NEG_23", "POS_34", "NEG_34", "CESD", "GAD7",
          "Age", "Sexe", "Statut", "Actual_Studies", "University_HE",
          "Actual_Program", "DomaineTravail", "Mother_Tongue")],
    relationship = "many-to-many"
  )

round(describe(et_qu2[c("first_fix_prob_1", "first_fix_latency_1", "dwell_time_1",
                        "dwell_time_early_1", "dwell_time_intermediate_1", "dwell_time_late_1")]), 2)

round(describe(et_qu2[c("first_fix_prob_2", "first_fix_latency_2", "dwell_time_2",
                        "dwell_time_early_2", "dwell_time_intermediate_2", "dwell_time_late_2")]), 2)

round(describe(et_qu2[c("first_fix_prob_diff", "first_fix_latency_diff", "dwell_time_diff",
                        "dwell_time_early_diff", "dwell_time_intermediate_diff", "dwell_time_late_diff")]), 2)

# Descriptives response accuracy
accuracy <- correct_responses %>% group_by(subject_nr, Temps_de_mesure) %>% summarize(correct_response = sum(correct_response))

accuracy_1 <- accuracy$correct_response[correct_responses$Temps_de_mesure=='T1']
accuracy_2 <- accuracy$correct_response[correct_responses$Temps_de_mesure=='T2']

describe(accuracy_1)
describe(accuracy_2)

# Descriptives rule awareness
rule_awareness <- rt2 %>%
  group_by(subject_nr, block_loop, Temps_de_mesure) %>%
  summarise(guessed_block_rule = mean(as.numeric(as.character(guessed_block_rule))), .groups = "drop") %>%
  group_by(subject_nr, Temps_de_mesure) %>%
  summarise(guessed_block_rule = sum(guessed_block_rule), .groups = "drop")

rule_awareness_1 <- rule_awareness$guessed_block_rule[rule_awareness$Temps_de_mesure == "T1"]
rule_awareness_2 <- rule_awareness$guessed_block_rule[rule_awareness$Temps_de_mesure == "T2"]

round(describe(rule_awareness_1), 2)
round(describe(rule_awareness_2), 2)

# Model Inputs and Predictor Coding
#----------------------------------

# Code semantic class
  #RT
rt_qu2$Change_category_EV <- 0
rt_qu2$Change_category_EV[rt_qu2$Change_category == "Failure" | rt_qu2$Change_category == "Negative"] <- 1
rt_qu2$Change_category_EV[rt_qu2$Change_category == "Success" | rt_qu2$Change_category == "Positive"] <- -1

rt_qu2$Change_category_PR_SvsF <- 0
rt_qu2$Change_category_PR_SvsF[rt_qu2$Change_category == "Success"] <- -1
rt_qu2$Change_category_PR_SvsF[rt_qu2$Change_category == "Failure"] <- 1

rt_qu2$Change_category_PR_LvsH <- -2
rt_qu2$Change_category_PR_LvsH[rt_qu2$Change_category == "Success"] <- 1
rt_qu2$Change_category_PR_LvsH[rt_qu2$Change_category == "Failure"] <- 1

  #ET
et_qu2$IA_LABEL_EV <- 0
et_qu2$IA_LABEL_EV[et_qu2$IA_LABEL == "Failure" | et_qu2$IA_LABEL == "Negative"] <- 1
et_qu2$IA_LABEL_EV[et_qu2$IA_LABEL == "Success" | et_qu2$IA_LABEL == "Positive"] <- -1

et_qu2$IA_LABEL_PR_SvsF <- 0
et_qu2$IA_LABEL_PR_SvsF[et_qu2$IA_LABEL == "Success"] <- -1
et_qu2$IA_LABEL_PR_SvsF[et_qu2$IA_LABEL == "Failure"] <- 1

et_qu2$IA_LABEL_PR_LvsH <- -2
et_qu2$IA_LABEL_PR_LvsH[et_qu2$IA_LABEL == "Success"] <- 1
et_qu2$IA_LABEL_PR_LvsH[et_qu2$IA_LABEL == "Failure"] <- 1

# ABM Condition
qu2$Condition <- relevel(as.factor(qu2$Condition), ref = "Control")
rt_qu2$Condition <- relevel(as.factor(rt_qu2$Condition), ref = "Control")
et_qu2$Condition <- relevel(as.factor(et_qu2$Condition), ref = "Control")

# Center predictors
qu2$Perf_Strivings_c <- qu2$Perf_Strivings - mean(qu2$Perf_Strivings)
qu2$Perf_Concerns_c  <- qu2$Perf_Concerns  - mean(qu2$Perf_Concerns)

rt_qu2$Perf_Strivings_c <- rt_qu2$Perf_Strivings - mean(rt_qu2$Perf_Strivings)
rt_qu2$Perf_Concerns_c  <- rt_qu2$Perf_Concerns  - mean(rt_qu2$Perf_Concerns)

et_qu2$Perf_Strivings_c <- et_qu2$Perf_Strivings - mean(et_qu2$Perf_Strivings)
et_qu2$Perf_Concerns_c  <- et_qu2$Perf_Concerns  - mean(et_qu2$Perf_Concerns)

# Convert smooth terms to numeric
rt_qu2$subject_nr_2 <- as.numeric(rt_qu2$subject_nr_2)
rt_qu2$block_loop <- as.numeric(rt_qu2$block_loop)

et_qu2$subject_nr_2 <- as.numeric(et_qu2$subject_nr_2)
et_qu2$block_loop <- as.numeric(et_qu2$block_loop)

# Code change rule
et_qu2$Change_category <- as.factor(et_qu2$Change_category)

et_qu2$Change_category01 <- 0
inds <- which(et_qu2$IA_LABEL == et_qu2$Change_category)
et_qu2$Change_category01[inds] <- 1

# Model Diagnostics functions

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
