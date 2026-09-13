# AB Perfectionism - Study 1 preparation

# Packages required by the preparation and supplementary-analysis scripts
library(readxl)
library(dplyr)
library(emmeans)
library(glmmTMB)
library(lmtest)

# Read data sample 1
qu1.1 <- read_excel("AB_Perfectionism_Data/questionnaires_study1.1.xlsx")
qu1.2 <- read_excel("AB_Perfectionism_Data/questionnaires_study1.2.xlsx")

qu1.1$ID <- as.factor(qu1.1$ID)
qu1.2$ID <- as.factor(qu1.2$ID)
qu1 <- qu1.1 %>% inner_join(qu1.2, by = "ID")
qu1 <- qu1 %>% rename(subject_id = ID_EyeTracker)

# Read data sample 2
qu2.1 <- read_excel("AB_Perfectionism_Data/questionnaires_study2.1.xlsx")
qu2.2 <- read_excel("AB_Perfectionism_Data/questionnaires_study2.2.xlsx")
qu2.3_1 <- read_excel("AB_Perfectionism_Data/questionnaires_study2.3_1.xlsx")
qu2.3_2 <- read_excel("AB_Perfectionism_Data/questionnaires_study2.3_2.xlsx")
qu2.3_3 <- read_excel("AB_Perfectionism_Data/questionnaires_study2.3_3.xlsx")

qu2a <- qu2.1 %>% inner_join(qu2.2, by = "ID")
qu2b <- qu2.3_1 %>% inner_join(qu2.3_2, by = "ID") %>% inner_join(qu2.3_3, by = "ID")
qu2 <- qu2a %>% inner_join(qu2b, by = "ID")

qu2 <- qu2 %>% rename(subject_id = ID_EyeTracker_1)
qu2$subject_id <- as.numeric(qu2$subject_id)
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
# Remove univariate outliers
qu <- qu[-univar_out, ]

# Identify multivariate outliers
vars <- qu[, c("SOP", "OOP", "SPP", "PS", "PC", "PE", "CM", "DA", "O", "GAD7", "CESD")]
md <- mahalanobis(vars, center = colMeans(vars), cov = cov(vars))
cutoff <- qchisq(p = 1 - .001, df = ncol(vars))
multivar_out_MH <- which(md > cutoff)

qu$Perf_Strivings <- rowSums(qu[, c("ZPS", "ZSOP")])
qu$Perf_Concerns  <- rowSums(qu[, c("ZCM", "ZDA", "ZSPP")])
# Read response-time data
rt1 <- read_excel("AB_Perfectionism_Data/rt_study1.xlsx") %>%
  rename(subject_id = subject_nr, block_id = block_loop)
rt2 <- read_excel("AB_Perfectionism_Data/rt_study2.xlsx") %>%
  rename(subject_id = subject_nr, block_id = block_loop)

# Read the raw eye-tracking data before block-level aggregation. TRIAL_INDEX is
# available in these files, rather than in the response-time files.
et1 <- read_excel("AB_Perfectionism_Data/et_study1_500to5500.xlsx") %>%
  rename(subject_id = RECORDING_SESSION_LABEL)
et2 <- read_excel("AB_Perfectionism_Data/et_study2_500to5500.xlsx") %>%
  rename(subject_id = RECORDING_SESSION_LABEL)

# Number blocks by their order of presentation within each participant/session.
# block_id uniquely identifies each block, including paired blocks with
# the same change category.
rt1 <- rt1 %>%
  group_by(subject_id) %>%
  mutate(
    block_nr = match(block_id, unique(block_id)),
    # RT rows are stored in presentation order; this ordering was verified
    # against EyeLink TRIAL_INDEX for every unambiguously matched trial.
    trial_nr = row_number()
  ) %>%
  ungroup()

rt2 <- rt2 %>%
  group_by(subject_id, Temps_de_mesure) %>%
  mutate(
    block_nr = match(block_id, unique(block_id)),
    trial_nr = row_number()
  ) %>%
  ungroup()

# Separate T1 and T2
t1 <- which(rt2$Temps_de_mesure == "T1")
rt2_1 <- rt2[t1, ]

t1_et <- which(et2$MEASUREMENT_TIME == "T1")
et2_1 <- et2[t1_et, ]

# Merge Study 1 and baseline Study 2 data
rt2_1 <- subset(rt2_1, select = -ID) %>% rename(ID = Code)
rt <- rt1 %>% full_join(rt2_1)
rt$correct_response <- as.factor(rt$correct_response) %>%
  dplyr::recode("Vrai" = 1, "Faux" = 0)

incorrect <- which(rt$correct_response == 0)
rt200 <- which(rt$response_time < 200)

rt <- rt[-c(incorrect, rt200), ]
rt <- rt %>% rename(guessed_block_rule = response_keyboard_response)
rt$guessed_block_rule <- as.factor(rt$guessed_block_rule) %>%
  dplyr::recode("o" = 1, "O" = 1, "n" = 0, "N" = 0)
# The raw eye-tracking files and the Study 2 baseline subset were read above so
# that TRIAL_INDEX could be transferred before any block-level aggregation.
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
out_first_fix_latency <- which(et$IA_FIRST_FIXATION_TIME > 5000)
et$IA_FIRST_FIXATION_TIME[out_first_fix_latency] <- NA

#identify participants missing > 25% of trials => NONE
et$missing_IA_FIRST_FIXATION_VISITED_IA_COUNT <- NA
et$missing_IA_FIRST_FIXATION_TIME <- NA
for (participant in levels(as.factor(et$subject_id))){
  et$missing_IA_FIRST_FIXATION_VISITED_IA_COUNT[et$subject_id == participant] <- (length(which(is.na(et$IA_FIRST_FIXATION_VISITED_IA_COUNT[et$subject_id == participant])))/375)*100
  et$missing_IA_FIRST_FIXATION_TIME[et$subject_id == participant] <- (length(which(is.na(et$IA_FIRST_FIXATION_TIME[et$subject_id == participant])))/375)*100
}
# Merge ET with RT to recover block-level rule variables. Trial number comes
# from the eye-tracking export and provides the within-participant trial key.
rt_block_lookup <- rt %>%
  select(subject_id, trial_nr, Change_category, block_id, block_nr,
         guessed_block_rule) %>%
  distinct()

etrt <- et %>%
  rename(trial_nr = TRIAL_INDEX) %>%
  left_join(rt_block_lookup,
            by = c("subject_id", "trial_nr", "Change_category"),
            relationship = "many-to-one")

et_a <- etrt[c("subject_id", "block_id", "block_nr", "IA_LABEL", "Change_category", "guessed_block_rule",
               "IA_DWELL_TIME", "IA_DWELL_TIME_500to1500", "IA_DWELL_TIME_1500to3500", "IA_DWELL_TIME_3500to5500",
               "IA_FIRST_FIXATION_VISITED_IA_COUNT", "IA_FIRST_FIXATION_TIME")] %>%
  group_by(subject_id, block_id, block_nr, IA_LABEL, Change_category, guessed_block_rule) %>%
  reframe(
    IA_FIRST_FIXATION_PROBABILITY = mean(na.omit(IA_FIRST_FIXATION_VISITED_IA_COUNT)),
    IA_FIRST_FIXATION_TIME_BLOCKMEAN = mean(na.omit(IA_FIRST_FIXATION_TIME)),
    IA_DWELL_TIME_BLOCKMEAN = mean(IA_DWELL_TIME),
    IA_DWELL_TIME_500to1500_BLOCKMEAN = mean(IA_DWELL_TIME_500to1500),
    IA_DWELL_TIME_1500to3500_BLOCKMEAN = mean(IA_DWELL_TIME_1500to3500),
    IA_DWELL_TIME_3500to5500_BLOCKMEAN = mean(IA_DWELL_TIME_3500to5500)
  )

et_b <- et_a %>%
  group_by(subject_id, block_id, block_nr) %>%
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

# Restrict questionnaire data to participants with usable attention-task data.
task_completer_ids <- et %>%
  filter(!is.na(subject_id)) %>%
  distinct(subject_id)

qu_task <- qu %>%
  semi_join(task_completer_ids, by = "subject_id") %>%
  distinct(subject_id, .keep_all = TRUE)

stopifnot(nrow(qu_task) == n_distinct(qu_task$subject_id))
rt_qu <- rt[c("subject_id", "Change_category", "response_time", "block_id", "block_nr", "trial_nr",
                "guessed_block_rule")]  %>% 
  inner_join(qu_task[c("subject_id", "SOP", "OOP", "SPP", "PS", "PE", "PC", "CM", "DA", "O",
                       "Perf_Strivings", "Perf_Concerns", "CESD", "GAD7")], 
             relationship = "many-to-many")
et_qu <- et %>%
  inner_join(
    qu_task[c("subject_id", "SOP", "OOP", "SPP", "PS", "PE", "PC", "CM", "DA", "O",
              "Perf_Strivings", "Perf_Concerns", "CESD", "GAD7")],
    relationship = "many-to-many"
  )
semantic_levels <- c("Neutral", "Positive", "Negative", "Success", "Failure")

# Numeric semantic codes retained for supplementary scripts that use the
# original planned-contrast parameterization.
semantic_codes <- tibble::tribble(
  ~semantic_category, ~EV, ~PR_SvsF, ~PR_LvsH, ~NonNeutral,
  "Positive", -1,  0, -2,  1,
  "Negative",  1,  0, -2,  1,
  "Neutral",   0,  0, -2, -4,
  "Success",  -1, -1,  3,  1,
  "Failure",   1,  1,  3,  1
)

rt_qu <- rt_qu %>%
  left_join(
    semantic_codes %>%
      rename(
        Change_category = semantic_category,
        Change_category_EV = EV,
        Change_category_PR_SvsF = PR_SvsF,
        Change_category_PR_LvsH = PR_LvsH,
        Change_category_NonNeutral = NonNeutral
      ),
    by = "Change_category"
  )

et_qu <- et_qu %>%
  left_join(
    semantic_codes %>%
      rename(
        IA_LABEL = semantic_category,
        IA_LABEL_EV = EV,
        IA_LABEL_PR_SvsF = PR_SvsF,
        IA_LABEL_PR_LvsH = PR_LvsH,
        IA_LABEL_NonNeutral = NonNeutral
      ),
    by = "IA_LABEL"
  )

stopifnot(
  !anyNA(rt_qu$Change_category_NonNeutral),
  !anyNA(et_qu$IA_LABEL_NonNeutral)
)

semantic_planned <- list(
  # Negative minus positive, irrespective of performance relevance
  positive_vs_negative = c(
    Neutral = 0, Positive = -1, Negative = 1, Success = -1, Failure = 1
  ),
  # Failure minus success
  Success_Failure = c(
    Neutral = 0, Positive = 0, Negative = 0, Success = -1, Failure = 1
  ),
  # Performance-relevant minus performance-irrelevant
  performance_irrelevant_vs_relevant = c(
    Neutral = -2, Positive = -2, Negative = -2, Success = 3, Failure = 3
  ),
  # All non-neutral categories minus Neutral
  NonNeutral_vs_Neutral = c(
    Neutral = -4, Positive = 1, Negative = 1, Success = 1, Failure = 1
  )
)
stopifnot(
  all(vapply(semantic_planned, sum, numeric(1)) == 0),
  all(vapply(semantic_planned, function(x) identical(names(x), semantic_levels),
             logical(1)))
)

# Colorblind-accessible Okabe-Ito palette.
semantic_colors <- c(
  Neutral = "#000000", Positive = "#56B4E9", Negative = "#D55E00",
  Success = "#009E73", Failure = "#CC79A7"
)
contrast_colors <- c(
  positive_vs_negative = "#0072B2", Success_Failure = "#D55E00",
  performance_irrelevant_vs_relevant = "#009E73",
  NonNeutral_vs_Neutral = "#CC79A7"
)
rt_qu$Change_category_f <- factor(rt_qu$Change_category, levels = semantic_levels)
et_qu$IA_LABEL_f <- factor(et_qu$IA_LABEL, levels = semantic_levels)
stopifnot(
  levels(rt_qu$Change_category_f)[1] == "Neutral",
  levels(et_qu$IA_LABEL_f)[1] == "Neutral"
)
qu_task$Perf_Strivings_c <- qu_task$Perf_Strivings - mean(qu_task$Perf_Strivings, na.rm = TRUE)
qu_task$Perf_Concerns_c <- qu_task$Perf_Concerns - mean(qu_task$Perf_Concerns, na.rm = TRUE)

rt_qu$Perf_Strivings_c <- rt_qu$Perf_Strivings - mean(qu_task$Perf_Strivings, na.rm = TRUE)
rt_qu$Perf_Concerns_c <- rt_qu$Perf_Concerns - mean(qu_task$Perf_Concerns, na.rm = TRUE)

et_qu$Perf_Strivings_c <- et_qu$Perf_Strivings - mean(qu_task$Perf_Strivings, na.rm = TRUE)
et_qu$Perf_Concerns_c <- et_qu$Perf_Concerns - mean(qu_task$Perf_Concerns, na.rm = TRUE)
rt_qu$subject_id <- as.factor(rt_qu$subject_id)
rt_qu$block_id <- as.factor(rt_qu$block_id)
rt_qu$block_nr <- as.integer(rt_qu$block_nr)
rt_qu$trial_nr <- as.integer(rt_qu$trial_nr)
rt_qu$trial_nr_c <- rt_qu$trial_nr - mean(rt_qu$trial_nr, na.rm = TRUE)

et_qu$subject_id <- as.factor(et_qu$subject_id)
et_qu$block_id <- as.factor(et_qu$block_id)
et_qu$block_nr <- as.integer(et_qu$block_nr)
et_qu$block_nr_c <- et_qu$block_nr - mean(et_qu$block_nr, na.rm = TRUE)
et_qu$Change_category <- as.factor(et_qu$Change_category)
et_qu$Change_category01 <- 0
et_qu$Change_category01 <- as.integer(
  as.character(et_qu$IA_LABEL) == as.character(et_qu$Change_category)
)



