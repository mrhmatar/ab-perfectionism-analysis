# AB Perfectionism - Study 2 preparation
# Packages required by the preparation and supplementary-analysis scripts
library(readxl)
library(dplyr)
library(emmeans)
library(glmmTMB)
library(lmtest)

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

# Remove one participant flagged administratively as outlier in the condition variable
qu2 <- qu2 %>% filter(Condition != "Outlier")

qu2$Perf_Strivings <- rowSums(qu2[, c("ZPS", "ZSOP")])
qu2$Perf_Concerns  <- rowSums(qu2[, c("ZCM", "ZDA", "ZSPP")])

rt2 <- read_excel("AB_Perfectionism_Data/rt_study2.xlsx") %>%
  rename(subject_id = subject_nr, block_id = block_loop)

# Number blocks by their order of presentation within each participant and
# measurement occasion. block_id uniquely identifies each block,
# including paired blocks with the same change category.
rt2 <- rt2 %>%
  group_by(subject_id, Temps_de_mesure) %>%
  mutate(block_nr = match(block_id, unique(block_id)),
         trial_nr = row_number()) %>%
  ungroup()

rt2$correct_response <- as.factor(rt2$correct_response) %>%
  dplyr::recode("Vrai" = 1, "Faux" = 0)

# Retain trial accuracy before excluding incorrect response-time observations.
correct_responses <- rt2[, c("subject_id", "correct_response", "Temps_de_mesure")] %>%
  group_by(subject_id, Temps_de_mesure) %>%
  summarise(correct_response = sum(correct_response), .groups = "drop")

incorrect <- which(rt2$correct_response == 0)
rt200 <- which(rt2$response_time < 200)

rt2$response_time[c(incorrect, rt200)] <- NA

 rt2 <- rt2 %>% rename(guessed_block_rule = response_keyboard_response)
rt2$guessed_block_rule <- as.factor(rt2$guessed_block_rule) %>%
  dplyr::recode("o" = 1, "O" = 1, "n" = 0, "N" = 0)

 t1 <- which(rt2$Temps_de_mesure == "T1")
t2 <- which(rt2$Temps_de_mesure == "T2")

rt2_1 <- rt2[t1, ]
rt2_2 <- rt2[t2, ]

drop <- c("Temps_de_mesure", "ID", "datetime", "response_position", "participant_response", "correct_response")

rt2_1 <- rt2_1[, !(names(rt2_1) %in% drop)] %>%
  rename(subject_id_1 = subject_id,
         block_nr_1 = block_nr,
         trial_nr_1 = trial_nr,
         response_time_1 = response_time,
         guessed_block_rule_1 = guessed_block_rule)

rt2_2 <- rt2_2[, !(names(rt2_2) %in% drop)] %>%
  rename(subject_id_2 = subject_id,
         block_nr_2 = block_nr,
         trial_nr_2 = trial_nr,
         response_time_2 = response_time,
         guessed_block_rule_2 = guessed_block_rule)

rt <- rt2_1 %>%
  inner_join(
    rt2_2,
    by = c("Code", "Change_category", "Change_location", "Change_word", "Flicker_word",
           "A", "B", "C", "D", "E",
           "OS_A_word", "OS_B_word", "OS_C_word", "OS_D_word", "OS_E_word",
           "CS_A_word", "CS_B_word", "CS_C_word", "CS_D_word", "CS_E_word", "block_id"),
    relationship = "one-to-one"
  )

# Merge rt data with questionnaire data
rt_qu2 <- rt[c("Code", "block_id", "block_nr_1", "block_nr_2", "Change_category",
               "subject_id_1", "trial_nr_1", "response_time_1", "guessed_block_rule_1", 
               "subject_id_2", "trial_nr_2", "response_time_2", "guessed_block_rule_2")]  %>% 
  rename(ID = Code) %>%
  inner_join(qu2[c("ID", "Condition", "Perf_Strivings", "Perf_Concerns",
                   "CESD", "GAD7")], 
             relationship = "many-to-many")

et2 <- read_excel("AB_Perfectionism_Data/et_study2_500to5500.xlsx")
#for merging later
et2 <- et2 %>% rename(subject_id = RECORDING_SESSION_LABEL)

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

# Merge ET with RT to recover block-level rule variables. EyeLink TRIAL_INDEX
# corresponds to trial_nr within each participant and measurement occasion.
rt_block_lookup2 <- rt2 %>%
  select(Temps_de_mesure, subject_id, trial_nr, Change_category, Code,
         block_id, block_nr, guessed_block_rule) %>%
  distinct()

etrt2 <- et2 %>%
  rename(trial_nr = TRIAL_INDEX) %>%
  left_join(rt_block_lookup2,
            by = c("MEASUREMENT_TIME" = "Temps_de_mesure", "subject_id",
                   "trial_nr", "Change_category"),
            relationship = "many-to-one") %>%
  rename(Temps_de_mesure = MEASUREMENT_TIME)

et2a <- etrt2[c("Temps_de_mesure", "Code", "subject_id", "block_id", "block_nr", "IA_LABEL", "Change_category", "guessed_block_rule",
                "IA_DWELL_TIME", "IA_DWELL_TIME_500to1500", "IA_DWELL_TIME_1500to3500", "IA_DWELL_TIME_3500to5500",
                "IA_FIRST_FIXATION_VISITED_IA_COUNT", "IA_FIRST_FIXATION_TIME")] %>%
  group_by(Temps_de_mesure, Code, subject_id, block_id, block_nr, IA_LABEL, Change_category, guessed_block_rule) %>%
  reframe(
    IA_FIRST_FIXATION_PROBABILITY = mean(na.omit(IA_FIRST_FIXATION_VISITED_IA_COUNT)),
    IA_FIRST_FIXATION_TIME_BLOCKMEAN = mean(na.omit(IA_FIRST_FIXATION_TIME)),
    IA_DWELL_TIME_BLOCKMEAN = mean(IA_DWELL_TIME),
    IA_DWELL_TIME_500to1500_BLOCKMEAN = mean(IA_DWELL_TIME_500to1500),
    IA_DWELL_TIME_1500to3500_BLOCKMEAN = mean(IA_DWELL_TIME_1500to3500),
    IA_DWELL_TIME_3500to5500_BLOCKMEAN = mean(IA_DWELL_TIME_3500to5500)
  )

et2b <- et2a %>%
  group_by(Temps_de_mesure, subject_id, block_id, block_nr) %>%
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

t1 <- which(et2$Temps_de_mesure == "T1")
t2 <- which(et2$Temps_de_mesure == "T2")

et2_1 <- et2[t1, ]
et2_2 <- et2[t2, ]

et2_1 <- et2_1[c("Code", "subject_id", "block_id", "block_nr", "IA_LABEL", "Change_category",
                 "guessed_block_rule", "first_fix_prob", "first_fix_latency", "dwell_time",
                 "dwell_time_early", "dwell_time_intermediate", "dwell_time_late")] %>%
  rename(subject_id_1 = subject_id,
         block_nr_1 = block_nr,
         first_fix_prob_1 = first_fix_prob,
         first_fix_latency_1 = first_fix_latency,
         dwell_time_1 = dwell_time,
         dwell_time_early_1 = dwell_time_early,
         dwell_time_intermediate_1 = dwell_time_intermediate,
         dwell_time_late_1 = dwell_time_late,
         guessed_block_rule_1 = guessed_block_rule)

et2_2 <- et2_2[c("Code", "subject_id", "block_id", "block_nr", "IA_LABEL", "Change_category",
                 "guessed_block_rule", "first_fix_prob", "first_fix_latency", "dwell_time",
                 "dwell_time_early", "dwell_time_intermediate", "dwell_time_late")] %>%
  rename(subject_id_2 = subject_id,
         block_nr_2 = block_nr,
         first_fix_prob_2 = first_fix_prob,
         first_fix_latency_2 = first_fix_latency,
         dwell_time_2 = dwell_time,
         dwell_time_early_2 = dwell_time_early,
         dwell_time_intermediate_2 = dwell_time_intermediate,
         dwell_time_late_2 = dwell_time_late,
         guessed_block_rule_2 = guessed_block_rule)

et <- et2_1 %>%
  inner_join(
    et2_2,
    by = c("Code", "block_id", "IA_LABEL", "Change_category"),
    relationship = "one-to-one"
  )

# Restrict all data to participants with both response-time and eye-tracking data.
task_completer_ids <- inner_join(
  rt %>% distinct(ID = Code),
  et %>% distinct(ID = Code),
  by = "ID",
  relationship = "one-to-one"
)

qu2 <- qu2 %>%
  semi_join(task_completer_ids, by = "ID") %>%
  distinct(ID, .keep_all = TRUE)

stopifnot(nrow(qu2) == n_distinct(qu2$ID))

rt2 <- rt2 %>% filter(Code %in% qu2$ID)
rt_qu2 <- rt_qu2 %>% semi_join(qu2 %>% distinct(ID), by = "ID")
correct_responses <- correct_responses %>%
  semi_join(rt2 %>% distinct(subject_id), by = "subject_id")

# Participant-level task-performance variables used in the main control analyses.
accuracy <- correct_responses
accuracy_1 <- accuracy$correct_response[accuracy$Temps_de_mesure == "T1"]
accuracy_2 <- accuracy$correct_response[accuracy$Temps_de_mesure == "T2"]

rule_awareness <- rt2 %>%
  group_by(subject_id, block_id, block_nr, Temps_de_mesure) %>%
  summarise(
    guessed_block_rule = mean(as.numeric(as.character(guessed_block_rule))),
    .groups = "drop"
  ) %>%
  group_by(subject_id, Temps_de_mesure) %>%
  summarise(guessed_block_rule = sum(guessed_block_rule), .groups = "drop")

rule_awareness_1 <- rule_awareness$guessed_block_rule[
  rule_awareness$Temps_de_mesure == "T1"
]
rule_awareness_2 <- rule_awareness$guessed_block_rule[
  rule_awareness$Temps_de_mesure == "T2"
]

# Merge the paired eye-tracking outcomes with questionnaire predictors.
et_qu2 <- et %>%
  rename(ID = Code) %>%
  inner_join(
    qu2[c("ID", "Condition", "Perf_Strivings", "Perf_Concerns",
          "CESD", "GAD7")],
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

rt_qu2 <- rt_qu2 %>%
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

et_qu2 <- et_qu2 %>%
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
  !anyNA(rt_qu2$Change_category_NonNeutral),
  !anyNA(et_qu2$IA_LABEL_NonNeutral)
)

# Planned comparisons estimated from the saturated five-level factor model.
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

rt_qu2$Change_category_f <- factor(rt_qu2$Change_category, levels = semantic_levels)
et_qu2$IA_LABEL_f <- factor(et_qu2$IA_LABEL, levels = semantic_levels)
stopifnot(
  levels(rt_qu2$Change_category_f)[1] == "Neutral",
  levels(et_qu2$IA_LABEL_f)[1] == "Neutral"
)

qu2$Condition <- relevel(as.factor(qu2$Condition), ref = "Control")
rt_qu2$Condition <- relevel(as.factor(rt_qu2$Condition), ref = "Control")
et_qu2$Condition <- relevel(as.factor(et_qu2$Condition), ref = "Control")
stopifnot(
  levels(qu2$Condition)[1] == "Control",
  levels(rt_qu2$Condition)[1] == "Control",
  levels(et_qu2$Condition)[1] == "Control"
)

qu2$Perf_Strivings_c <- qu2$Perf_Strivings - mean(qu2$Perf_Strivings, na.rm = TRUE)
qu2$Perf_Concerns_c  <- qu2$Perf_Concerns  - mean(qu2$Perf_Concerns, na.rm = TRUE)

rt_qu2$Perf_Strivings_c <- rt_qu2$Perf_Strivings - mean(rt_qu2$Perf_Strivings, na.rm = TRUE)
rt_qu2$Perf_Concerns_c  <- rt_qu2$Perf_Concerns  - mean(rt_qu2$Perf_Concerns, na.rm = TRUE)

et_qu2$Perf_Strivings_c <- et_qu2$Perf_Strivings - mean(et_qu2$Perf_Strivings, na.rm = TRUE)
et_qu2$Perf_Concerns_c  <- et_qu2$Perf_Concerns  - mean(et_qu2$Perf_Concerns, na.rm = TRUE)

rt_qu2$subject_id_2 <- as.factor(rt_qu2$subject_id_2)
rt_qu2$subject_id_1 <- as.factor(rt_qu2$subject_id_1)
rt_qu2$block_id <- as.factor(rt_qu2$block_id)
rt_qu2$block_nr_1 <- as.integer(rt_qu2$block_nr_1)
rt_qu2$block_nr_2 <- as.integer(rt_qu2$block_nr_2)
rt_qu2$trial_nr_1 <- as.integer(rt_qu2$trial_nr_1)
rt_qu2$trial_nr_2 <- as.integer(rt_qu2$trial_nr_2)
rt_qu2$trial_nr_1_c <- rt_qu2$trial_nr_1 - mean(rt_qu2$trial_nr_1, na.rm = TRUE)
rt_qu2$trial_nr_2_c <- rt_qu2$trial_nr_2 - mean(rt_qu2$trial_nr_2, na.rm = TRUE)

et_qu2$subject_id_2 <- as.factor(et_qu2$subject_id_2)
et_qu2$subject_id_1 <- as.factor(et_qu2$subject_id_1)
et_qu2$block_id <- as.factor(et_qu2$block_id)
et_qu2$block_nr_1 <- as.integer(et_qu2$block_nr_1)
et_qu2$block_nr_2 <- as.integer(et_qu2$block_nr_2)
et_qu2$block_nr_c <- et_qu2$block_nr_2 - mean(et_qu2$block_nr_2, na.rm = TRUE)

et_qu2$Change_category <- as.factor(et_qu2$Change_category)

et_qu2$Change_category01 <- 0
et_qu2$Change_category01 <- as.integer(
  as.character(et_qu2$IA_LABEL) == as.character(et_qu2$Change_category)
)


