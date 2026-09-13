# AB Perfectionism - Study 2 T1 supplementary analyses

source("AB_Perfectionism_Study2_Preparation.R")

# Data subset for beta GLMMs
sub_et_qu2 <- et_qu2[c("first_fix_prob_1", "first_fix_latency_1", "dwell_time_1",
                     "dwell_time_early_1", "dwell_time_intermediate_1", "dwell_time_late_1",
                     "subject_id_1", "block_id", "block_nr_1", "block_nr_c",
                     "guessed_block_rule_1", "Change_category01", "IA_LABEL_f",
                     "Perf_Strivings_c" , "Perf_Concerns_c")]

transform01 <- function(x) {
  n <- sum(!is.na(x))
  ifelse(is.na(x), NA_real_, (x * (n - 1) + 0.5) / n)
}

sub_et_qu2$first_fix_prob_1_t01 <- transform01(sub_et_qu2$first_fix_prob_1)
sub_et_qu2$first_fix_latency_1_t01 <- transform01(sub_et_qu2$first_fix_latency_1)
sub_et_qu2$dwell_time_early_1_t01 <- transform01(sub_et_qu2$dwell_time_early_1)
sub_et_qu2$dwell_time_intermediate_1_t01 <- transform01(sub_et_qu2$dwell_time_intermediate_1)
sub_et_qu2$dwell_time_late_1_t01 <- transform01(sub_et_qu2$dwell_time_late_1)
sub_et_qu2$dwell_time_1_t01 <- transform01(sub_et_qu2$dwell_time_1)

# Probability of first or second fixation
#----------------------------------------
bm0_first_fix_prob_1 <- glmmTMB(
  first_fix_prob_1_t01 ~ 
    block_nr_c +
    Change_category01 +
    #(1 | block_id) +  
    (1 + Change_category01 || subject_id_1),
  family = beta_family(link = "logit"),
  dispformula = ~ guessed_block_rule_1,
  data = na.omit(sub_et_qu2),
  
)
summary(bm0_first_fix_prob_1)
# Family: beta  ( logit )
# Formula:          first_fix_prob_1_t01 ~ block_nr_c + Change_category01 + (1 +  
#     Change_category01 || subject_id_1)
# Dispersion:                            ~guessed_block_rule_1
# Data: na.omit(sub_et_qu2)
# 
#      AIC      BIC   logLik deviance df.resid 
#  -2510.5  -2465.5   1262.2  -2524.5     4577 
# 
# Random effects:
# 
# Conditional model:
#  Groups       Name              Variance Std.Dev. Corr 
#  subject_id_1 (Intercept)       0.005915 0.07691       
#               Change_category01 0.032582 0.18050  0.00 
# Number of obs: 4584, groups:  subject_id_1, 92
# 
# Conditional model:
#                    Estimate Std. Error z value Pr(>|z|)    
# (Intercept)       -0.317120   0.015033 -21.094  < 2e-16 ***
# block_nr_c         0.001953   0.003955   0.494    0.622    
# Change_category01 -0.172723   0.034265  -5.041 4.64e-07 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Dispersion model:
#                      Estimate Std. Error z value Pr(>|z|)    
# (Intercept)          1.752906   0.039987   43.84   <2e-16 ***
# guessed_block_rule_1 0.008603   0.045852    0.19    0.851    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

bm1_first_fix_prob_1 <- glmmTMB(
  first_fix_prob_1_t01 ~ 
    IA_LABEL_f + 
    block_nr_c +
    Change_category01 +
    #(1 | block_id) +  
    (1 + Change_category01 || subject_id_1),
  dispformula = ~ guessed_block_rule_1,
  family = beta_family(link = "logit"),
  data = na.omit(sub_et_qu2),
  
)
summary(bm1_first_fix_prob_1)
# Family: beta  ( logit )
# Formula:          first_fix_prob_1_t01 ~ IA_LABEL_f + block_nr_c + Change_category01 +  
#     (1 + Change_category01 || subject_id_1)
# Dispersion:                            ~guessed_block_rule_1
# Data: na.omit(sub_et_qu2)
# 
#      AIC      BIC   logLik deviance df.resid 
#  -2531.0  -2460.3   1276.5  -2553.0     4573 
# 
# Random effects:
# 
# Conditional model:
#  Groups       Name              Variance Std.Dev. Corr 
#  subject_id_1 (Intercept)       0.005808 0.07621       
#               Change_category01 0.033219 0.18226  0.00 
# Number of obs: 4584, groups:  subject_id_1, 92
# 
# Conditional model:
#                     Estimate Std. Error z value Pr(>|z|)    
# (Intercept)        -0.333348   0.027139 -12.283  < 2e-16 ***
# IA_LABEL_fPositive  0.087278   0.035794   2.438   0.0148 *  
# IA_LABEL_fNegative -0.087656   0.035946  -2.439   0.0147 *  
# IA_LABEL_fSuccess   0.063814   0.035804   1.782   0.0747 .  
# IA_LABEL_fFailure   0.017679   0.035812   0.494   0.6215    
# block_nr_c          0.002041   0.003947   0.517   0.6050    
# Change_category01  -0.174046   0.034331  -5.070 3.99e-07 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Dispersion model:
#                      Estimate Std. Error z value Pr(>|z|)    
# (Intercept)           1.76213    0.04007   43.98   <2e-16 ***
# guessed_block_rule_1  0.00483    0.04594    0.11    0.916    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

lrtest(bm0_first_fix_prob_1, bm1_first_fix_prob_1)
# Model 1: first_fix_prob_1_t01 ~ block_nr_c + Change_category01 + (1 + 
#     Change_category01 || subject_id_1)
# Model 2: first_fix_prob_1_t01 ~ IA_LABEL_f + block_nr_c + Change_category01 + 
#     (1 + Change_category01 || subject_id_1)
#   #Df LogLik Df  Chisq Pr(>Chisq)    
# 1   7 1262.2                         
# 2  11 1276.5  4 28.493  9.908e-06 ***
  
AIC(bm1_first_fix_prob_1, bm0_first_fix_prob_1)
#                      df       AIC
# bm1_first_fix_prob_1 11 -2530.990
# bm0_first_fix_prob_1  7 -2510.497

bm2.1_first_fix_prob_1 <- glmmTMB(
  first_fix_prob_1_t01 ~ 
    IA_LABEL_f * Perf_Strivings_c +
    block_nr_c +
    Change_category01 +
    #(1 | block_id)  
    (1 + Change_category01 || subject_id_1),
  dispformula = ~ guessed_block_rule_1,
  family = beta_family(link = "logit"),
  data = na.omit(sub_et_qu2),
  
)
summary(bm2.1_first_fix_prob_1)
# Family: beta  ( logit )
# Formula:          first_fix_prob_1_t01 ~ IA_LABEL_f * Perf_Strivings_c + block_nr_c +  
#     Change_category01 + (1 + Change_category01 || subject_id_1)
# Dispersion:                            ~guessed_block_rule_1
# Data: na.omit(sub_et_qu2)
# 
#      AIC      BIC   logLik deviance df.resid 
#  -2523.2  -2420.3   1277.6  -2555.2     4568 
# 
# Random effects:
# 
# Conditional model:
#  Groups       Name              Variance Std.Dev. Corr 
#  subject_id_1 (Intercept)       0.005542 0.07445       
#               Change_category01 0.032977 0.18160  0.00 
# Number of obs: 4584, groups:  subject_id_1, 92
# 
# Conditional model:
#                                       Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                         -0.3334083  0.0270845 -12.310  < 2e-16 ***
# IA_LABEL_fPositive                   0.0873434  0.0357940   2.440   0.0147 *  
# IA_LABEL_fNegative                  -0.0876557  0.0359445  -2.439   0.0147 *  
# IA_LABEL_fSuccess                    0.0638275  0.0358018   1.783   0.0746 .  
# IA_LABEL_fFailure                    0.0176931  0.0358092   0.494   0.6212    
# Perf_Strivings_c                    -0.0044447  0.0144151  -0.308   0.7578    
# block_nr_c                           0.0020448  0.0039470   0.518   0.6044    
# Change_category01                   -0.1738644  0.0342919  -5.070 3.98e-07 ***
# IA_LABEL_fPositive:Perf_Strivings_c -0.0150513  0.0192652  -0.781   0.4346    
# IA_LABEL_fNegative:Perf_Strivings_c -0.0025692  0.0194036  -0.132   0.8947    
# IA_LABEL_fSuccess:Perf_Strivings_c  -0.0049733  0.0193363  -0.257   0.7970    
# IA_LABEL_fFailure:Perf_Strivings_c  -0.0001934  0.0193458  -0.010   0.9920    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Dispersion model:
#                      Estimate Std. Error z value Pr(>|z|)    
# (Intercept)          1.762607   0.040136   43.92   <2e-16 ***
# guessed_block_rule_1 0.004396   0.046033    0.10    0.924    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

lrtest(bm1_first_fix_prob_1, bm2.1_first_fix_prob_1)
# Model 1: first_fix_prob_1_t01 ~ IA_LABEL_f + block_nr_c + Change_category01 + 
#     (1 + Change_category01 || subject_id_1)
# Model 2: first_fix_prob_1_t01 ~ IA_LABEL_f * Perf_Strivings_c + block_nr_c + 
#     Change_category01 + (1 + Change_category01 || subject_id_1)
#   #Df LogLik Df Chisq Pr(>Chisq)
# 1  11 1276.5                    
# 2  16 1277.6  5 2.239     0.8152

AIC(bm2.1_first_fix_prob_1, bm1_first_fix_prob_1)
#                        df       AIC
# bm2.1_first_fix_prob_1 16 -2523.229
# bm1_first_fix_prob_1   11 -2530.990

bm2.2_first_fix_prob_1 <- glmmTMB(
  first_fix_prob_1_t01 ~ 
    IA_LABEL_f * Perf_Concerns_c +
    block_nr_c +
    Change_category01 +
    #(1 | block_id) +  
    (1 + Change_category01 || subject_id_1),
  dispformula = ~ guessed_block_rule_1,
  family = beta_family(link = "logit"),
  data = na.omit(sub_et_qu2),
  
)
summary(bm2.2_first_fix_prob_1)
# Family: beta  ( logit )
# Formula:          first_fix_prob_1_t01 ~ IA_LABEL_f * Perf_Concerns_c + block_nr_c +  
#     Change_category01 + (1 + Change_category01 || subject_id_1)
# Dispersion:                            ~guessed_block_rule_1
# Data: na.omit(sub_et_qu2)
# 
#      AIC      BIC   logLik deviance df.resid 
#  -2523.5  -2420.6   1277.8  -2555.5     4568 
# 
# Random effects:
# 
# Conditional model:
#  Groups       Name              Variance Std.Dev. Corr 
#  subject_id_1 (Intercept)       0.005785 0.07606       
#               Change_category01 0.033256 0.18236  0.00 
# Number of obs: 4584, groups:  subject_id_1, 92
# 
# Conditional model:
#                                     Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                        -0.333312   0.027129 -12.286  < 2e-16 ***
# IA_LABEL_fPositive                  0.087212   0.035787   2.437   0.0148 *  
# IA_LABEL_fNegative                 -0.087719   0.035939  -2.441   0.0147 *  
# IA_LABEL_fSuccess                   0.063731   0.035797   1.780   0.0750 .  
# IA_LABEL_fFailure                   0.017617   0.035805   0.492   0.6227    
# Perf_Concerns_c                     0.008134   0.011178   0.728   0.4668    
# block_nr_c                          0.002061   0.003946   0.522   0.6015    
# Change_category01                  -0.174087   0.034336  -5.070 3.98e-07 ***
# IA_LABEL_fPositive:Perf_Concerns_c -0.021315   0.014974  -1.423   0.1546    
# IA_LABEL_fNegative:Perf_Concerns_c -0.010073   0.015024  -0.670   0.5026    
# IA_LABEL_fSuccess:Perf_Concerns_c  -0.002949   0.015027  -0.196   0.8444    
# IA_LABEL_fFailure:Perf_Concerns_c  -0.012242   0.014948  -0.819   0.4128    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Dispersion model:
#                      Estimate Std. Error z value Pr(>|z|)    
# (Intercept)          1.762564   0.040100   43.95   <2e-16 ***
# guessed_block_rule_1 0.004984   0.045990    0.11    0.914    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

lrtest(bm1_first_fix_prob_1, bm2.2_first_fix_prob_1)
# Model 1: first_fix_prob_1_t01 ~ IA_LABEL_f + block_nr_c + Change_category01 + 
#     (1 + Change_category01 || subject_id_1)
# Model 2: first_fix_prob_1_t01 ~ IA_LABEL_f * Perf_Concerns_c + block_nr_c + 
#     Change_category01 + (1 + Change_category01 || subject_id_1)
#   #Df LogLik Df  Chisq Pr(>Chisq)
# 1  11 1276.5                     
# 2  16 1277.8  5 2.5403     0.7704

AIC(bm2.2_first_fix_prob_1, bm1_first_fix_prob_1)
#                       df       AIC
# bm2.2_first_fix_prob_1 16 -2523.531
# bm1_first_fix_prob_1   11 -2530.990

# Latency to first fixation
#--------------------------
bm0_first_fix_latency_1 <- glmmTMB(
  first_fix_latency_1_t01 ~ 
    block_nr_c +
    Change_category01 +
    #(1 | block_id) +  
    (1 + Change_category01 || subject_id_1), 
  dispformula = ~ guessed_block_rule_1,
  family = beta_family(link = "logit"),
  data = na.omit(sub_et_qu2),
  
)
summary(bm0_first_fix_latency_1)
#  Family: beta  ( logit )
# Formula:          first_fix_latency_1_t01 ~ block_nr_c + Change_category01 + (1 +  
#     Change_category01 || subject_id_1)
# Dispersion:                               ~guessed_block_rule_1
# Data: na.omit(sub_et_qu2)
# 
#      AIC      BIC   logLik deviance df.resid 
# -21166.3 -21121.3  10590.2 -21180.3     4577 
# 
# Random effects:
# 
# Conditional model:
#  Groups       Name              Variance  Std.Dev.  Corr 
#  subject_id_1 (Intercept)       1.613e-12 1.270e-06      
#               Change_category01 2.087e-10 1.445e-05 0.00 
# Number of obs: 4584, groups:  subject_id_1, 92
# 
# Conditional model:
#                     Estimate Std. Error z value Pr(>|z|)    
# (Intercept)       -1.390e+00  2.487e-03  -559.0  < 2e-16 ***
# block_nr_c         3.142e-07  7.729e-04     0.0 0.999676    
# Change_category01  2.081e-02  5.529e-03     3.8 0.000168 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Dispersion model:
#                      Estimate Std. Error z value Pr(>|z|)    
# (Intercept)           5.55798    0.04266  130.30   <2e-16 ***
# guessed_block_rule_1  0.07733    0.04889    1.58    0.114    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

bm1_first_fix_latency_1 <- glmmTMB(
  first_fix_latency_1_t01 ~ 
    IA_LABEL_f +
    block_nr_c +
    Change_category01 +
    #(1 | block_id) +  
    (1 + Change_category01 || subject_id_1), 
  dispformula = ~ guessed_block_rule_1,
  family = beta_family(link = "logit"),
  data = na.omit(sub_et_qu2)
)
summary(bm1_first_fix_latency_1)
#  Family: beta  ( logit )
# Formula:          first_fix_latency_1_t01 ~ IA_LABEL_f + block_nr_c + Change_category01 +  
#     (1 + Change_category01 || subject_id_1)
# Dispersion:                               ~guessed_block_rule_1
# Data: na.omit(sub_et_qu2)
# 
#      AIC      BIC   logLik deviance df.resid 
# -21168.7 -21098.0  10595.4 -21190.7     4573 
# 
# Random effects:
# 
# Conditional model:
#  Groups       Name              Variance  Std.Dev.  Corr 
#  subject_id_1 (Intercept)       4.201e-15 6.481e-08      
#               Change_category01 1.869e-10 1.367e-05 0.00 
# Number of obs: 4584, groups:  subject_id_1, 92
# 
# Conditional model:
#                      Estimate Std. Error z value Pr(>|z|)    
# (Intercept)        -1.386e+00  5.073e-03 -273.22  < 2e-16 ***
# IA_LABEL_fPositive -1.640e-02  7.021e-03   -2.34 0.019504 *  
# IA_LABEL_fNegative  2.705e-03  6.992e-03    0.39 0.698817    
# IA_LABEL_fSuccess  -7.701e-03  7.003e-03   -1.10 0.271447    
# IA_LABEL_fFailure   1.328e-03  6.993e-03    0.19 0.849363    
# block_nr_c         -5.561e-06  7.721e-04   -0.01 0.994253    
# Change_category01   2.076e-02  5.524e-03    3.76 0.000171 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Dispersion model:
#                      Estimate Std. Error z value Pr(>|z|)    
# (Intercept)           5.56477    0.04272  130.26   <2e-16 ***
# guessed_block_rule_1  0.07141    0.04899    1.46    0.145    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

lrtest(bm0_first_fix_latency_1, bm1_first_fix_latency_1)
# Model 1: first_fix_latency_1_t01 ~ block_nr_c + Change_category01 + (1 + 
#     Change_category01 || subject_id_1)
# Model 2: first_fix_latency_1_t01 ~ IA_LABEL_f + block_nr_c + Change_category01 + 
#     (1 + Change_category01 || subject_id_1)
#   #Df LogLik Df  Chisq Pr(>Chisq)  
# 1   7  10590                       
# 2  11  10595  4 10.428     0.0338 *
  
AIC(bm1_first_fix_latency_1, bm0_first_fix_latency_1)
#                         df       AIC
# bm1_first_fix_latency_1 11 -21168.74
# bm0_first_fix_latency_1  7 -21166.31

bm2.1_first_fix_latency_1 <- glmmTMB(
  first_fix_latency_1_t01 ~ 
    IA_LABEL_f * Perf_Strivings_c +
    block_nr_c +
    Change_category01 +
    #(1 | block_id) +  
    (1 + Change_category01 || subject_id_1), 
  dispformula = ~ guessed_block_rule_1,
  family = beta_family(link = "logit"),
  data = na.omit(sub_et_qu2),
  
)
summary(bm2.1_first_fix_latency_1)
# Family: beta  ( logit )
# Formula:          first_fix_latency_1_t01 ~ IA_LABEL_f * Perf_Strivings_c + block_nr_c +  
#     Change_category01 + (1 + Change_category01 || subject_id_1)
# Dispersion:                               ~guessed_block_rule_1
# Data: na.omit(sub_et_qu2)
# 
#      AIC      BIC   logLik deviance df.resid 
# -21161.4 -21058.5  10596.7 -21193.4     4568 
# 
# Random effects:
# 
# Conditional model:
#  Groups       Name              Variance  Std.Dev.  Corr 
#  subject_id_1 (Intercept)       1.134e-11 3.368e-06      
#               Change_category01 5.677e-11 7.535e-06 0.00 
# Number of obs: 4584, groups:  subject_id_1, 92
# 
# Conditional model:
#                                       Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                         -1.386e+00  5.072e-03 -273.31  < 2e-16 ***
# IA_LABEL_fPositive                  -1.638e-02  7.019e-03   -2.33  0.01962 *  
# IA_LABEL_fNegative                   2.713e-03  6.990e-03    0.39  0.69797    
# IA_LABEL_fSuccess                   -7.708e-03  7.001e-03   -1.10  0.27088    
# IA_LABEL_fFailure                    1.328e-03  6.991e-03    0.19  0.84931    
# Perf_Strivings_c                    -6.998e-04  2.672e-03   -0.26  0.79339    
# block_nr_c                          -6.943e-06  7.718e-04   -0.01  0.99282    
# Change_category01                    2.076e-02  5.522e-03    3.76  0.00017 ***
# IA_LABEL_fPositive:Perf_Strivings_c  1.961e-03  3.789e-03    0.52  0.60469    
# IA_LABEL_fNegative:Perf_Strivings_c -2.792e-03  3.774e-03   -0.74  0.45946    
# IA_LABEL_fSuccess:Perf_Strivings_c   1.647e-03  3.783e-03    0.44  0.66320    
# IA_LABEL_fFailure:Perf_Strivings_c   2.600e-03  3.780e-03    0.69  0.49154    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Dispersion model:
#                      Estimate Std. Error z value Pr(>|z|)    
# (Intercept)           5.56277    0.04283  129.88   <2e-16 ***
# guessed_block_rule_1  0.07480    0.04915    1.52    0.128    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

lrtest(bm1_first_fix_latency_1, bm2.1_first_fix_latency_1)
# Model 1: first_fix_latency_1_t01 ~ IA_LABEL_f + block_nr_c + Change_category01 + 
#     (1 + Change_category01 || subject_id_1)
# Model 2: first_fix_latency_1_t01 ~ IA_LABEL_f * Perf_Strivings_c + block_nr_c + 
#     Change_category01 + (1 + Change_category01 || subject_id_1)
#   #Df LogLik Df  Chisq Pr(>Chisq)
# 1  11  10595                     
# 2  16  10597  5 2.6323     0.7564

AIC(bm2.1_first_fix_latency_1, bm1_first_fix_latency_1)
#                           df       AIC
# bm2.1_first_fix_latency_1 16 -21161.37
# bm1_first_fix_latency_1   11 -21168.74

bm2.2_first_fix_latency_1 <- glmmTMB(
  first_fix_latency_1_t01 ~ 
    IA_LABEL_f * Perf_Concerns_c +
    block_nr_c +
    Change_category01 +
    #(1 | block_id) +  
    (1 + Change_category01 || subject_id_1), 
  dispformula = ~ guessed_block_rule_1,
  family = beta_family(link = "logit"),
  data = na.omit(sub_et_qu2),
  
)
summary(bm2.2_first_fix_latency_1)
# Family: beta  ( logit )
# Formula:          first_fix_latency_1_t01 ~ IA_LABEL_f * Perf_Concerns_c + block_nr_c +  
#     Change_category01 + (1 + Change_category01 || subject_id_1)
# Dispersion:                               ~guessed_block_rule_1
# Data: na.omit(sub_et_qu2)
# 
#      AIC      BIC   logLik deviance df.resid 
# -21159.9 -21057.0  10596.0 -21191.9     4568 
# 
# Random effects:
# 
# Conditional model:
#  Groups       Name              Variance  Std.Dev.  Corr 
#  subject_id_1 (Intercept)       4.458e-12 2.111e-06      
#               Change_category01 1.396e-10 1.181e-05 0.00 
# Number of obs: 4584, groups:  subject_id_1, 92
# 
# Conditional model:
#                                      Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                        -1.386e+00  5.072e-03 -273.26  < 2e-16 ***
# IA_LABEL_fPositive                 -1.639e-02  7.020e-03   -2.33  0.01956 *  
# IA_LABEL_fNegative                  2.713e-03  6.991e-03    0.39  0.69802    
# IA_LABEL_fSuccess                  -7.707e-03  7.002e-03   -1.10  0.27101    
# IA_LABEL_fFailure                   1.322e-03  6.992e-03    0.19  0.85004    
# Perf_Concerns_c                    -3.571e-04  2.070e-03   -0.17  0.86305    
# block_nr_c                         -5.173e-06  7.720e-04   -0.01  0.99465    
# Change_category01                   2.077e-02  5.523e-03    3.76  0.00017 ***
# IA_LABEL_fPositive:Perf_Concerns_c  1.701e-04  2.934e-03    0.06  0.95378    
# IA_LABEL_fNegative:Perf_Concerns_c -1.212e-03  2.926e-03   -0.41  0.67860    
# IA_LABEL_fSuccess:Perf_Concerns_c   8.558e-04  2.930e-03    0.29  0.77019    
# IA_LABEL_fFailure:Perf_Concerns_c   1.822e-03  2.924e-03    0.62  0.53330    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Dispersion model:
#                      Estimate Std. Error z value Pr(>|z|)    
# (Intercept)           5.56381    0.04274  130.18   <2e-16 ***
# guessed_block_rule_1  0.07301    0.04901    1.49    0.136    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

lrtest(bm1_first_fix_latency_1, bm2.2_first_fix_latency_1)
# Model 1: first_fix_latency_1_t01 ~ IA_LABEL_f + block_nr_c + Change_category01 + 
#     (1 + Change_category01 || subject_id_1)
# Model 2: first_fix_latency_1_t01 ~ IA_LABEL_f * Perf_Concerns_c + block_nr_c + 
#     Change_category01 + (1 + Change_category01 || subject_id_1)
#   #Df LogLik Df  Chisq Pr(>Chisq)
# 1  11  10595                     
# 2  16  10596  5 1.1741     0.9473

AIC(bm2.2_first_fix_latency_1, bm1_first_fix_latency_1)
#                           df       AIC
# bm2.2_first_fix_latency_1 16 -21159.92
# bm1_first_fix_latency_1   11 -21168.74

# Early dwell time
#-----------------
bm0_dwell_time_early_1 <- glmmTMB(
  dwell_time_early_1_t01 ~ 
    block_nr_c +
    Change_category01 +
    #(1 | block_id) +  
    (1 + Change_category01 || subject_id_1),
  dispformula = ~ guessed_block_rule_1,
  family = beta_family(link = "logit"),
  data = na.omit(sub_et_qu2),
  
)
summary(bm0_dwell_time_early_1)
# Family: beta  ( logit )
# Formula:          dwell_time_early_1_t01 ~ block_nr_c + Change_category01 + (1 +  
#     Change_category01 || subject_id_1)
# Dispersion:                              ~guessed_block_rule_1
# Data: na.omit(sub_et_qu2)
# 
#      AIC      BIC   logLik deviance df.resid 
#  -8269.7  -8224.6   4141.8  -8283.7     4577 
# 
# Random effects:
# 
# Conditional model:
#  Groups       Name              Variance Std.Dev. Corr 
#  subject_id_1 (Intercept)       0.01092  0.1045        
#               Change_category01 0.01596  0.1263   0.00 
# Number of obs: 4584, groups:  subject_id_1, 92
# 
# Conditional model:
#                     Estimate Std. Error z value Pr(>|z|)    
# (Intercept)       -1.4045108  0.0153477  -91.51   <2e-16 ***
# block_nr_c        -0.0001389  0.0032547   -0.04    0.966    
# Change_category01 -0.0168450  0.0268430   -0.63    0.530    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Dispersion model:
#                      Estimate Std. Error z value Pr(>|z|)    
# (Intercept)           2.56141    0.04151   61.70   <2e-16 ***
# guessed_block_rule_1  0.05504    0.04715    1.17    0.243    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

bm1_dwell_time_early_1 <- glmmTMB(
  dwell_time_early_1_t01 ~ 
    IA_LABEL_f +
    block_nr_c +
    Change_category01 +
    #(1 | block_id) +  
    (1 + Change_category01 || subject_id_1),
  dispformula = ~ guessed_block_rule_1,
  family = beta_family(link = "logit"),
  data = na.omit(sub_et_qu2),
  
)
summary(bm1_dwell_time_early_1)
# Family: beta  ( logit )
# Formula:          dwell_time_early_1_t01 ~ IA_LABEL_f + block_nr_c + Change_category01 +  
#     (1 + Change_category01 || subject_id_1)
# Dispersion:                              ~guessed_block_rule_1
# Data: na.omit(sub_et_qu2)
# 
#      AIC      BIC   logLik deviance df.resid 
#  -8289.5  -8218.8   4155.8  -8311.5     4573 
# 
# Random effects:
# 
# Conditional model:
#  Groups       Name              Variance Std.Dev. Corr 
#  subject_id_1 (Intercept)       0.01057  0.1028        
#               Change_category01 0.01812  0.1346   0.00 
# Number of obs: 4584, groups:  subject_id_1, 92
# 
# Conditional model:
#                      Estimate Std. Error z value Pr(>|z|)    
# (Intercept)        -1.464e+00  2.426e-02  -60.35  < 2e-16 ***
# IA_LABEL_fPositive  1.435e-01  2.950e-02    4.86 1.15e-06 ***
# IA_LABEL_fNegative  4.191e-02  2.970e-02    1.41  0.15826    
# IA_LABEL_fSuccess   3.163e-02  2.975e-02    1.06  0.28766    
# IA_LABEL_fFailure   8.207e-02  2.959e-02    2.77  0.00555 ** 
# block_nr_c         -8.543e-05  3.247e-03   -0.03  0.97901    
# Change_category01  -2.069e-02  2.726e-02   -0.76  0.44796    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Dispersion model:
#                      Estimate Std. Error z value Pr(>|z|)    
# (Intercept)           2.58226    0.04171   61.92   <2e-16 ***
# guessed_block_rule_1  0.03643    0.04739    0.77    0.442    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

lrtest(bm1_dwell_time_early_1, bm0_dwell_time_early_1)
# Model 1: dwell_time_early_1_t01 ~ IA_LABEL_f + block_nr_c + Change_category01 + 
#     (1 + Change_category01 || subject_id_1)
# Model 2: dwell_time_early_1_t01 ~ block_nr_c + Change_category01 + (1 + 
#     Change_category01 || subject_id_1)
#   #Df LogLik Df  Chisq Pr(>Chisq)    
# 1  11 4155.8                         
# 2   7 4141.8 -4 27.882  1.318e-05 ***
  
AIC(bm1_dwell_time_early_1, bm0_dwell_time_early_1)
#                       df       AIC
# bm1_dwell_time_early_1 11 -8289.536
# bm0_dwell_time_early_1  7 -8269.654

bm2.1_dwell_time_early_1 <- glmmTMB(
  dwell_time_early_1_t01 ~ 
    IA_LABEL_f * Perf_Strivings_c +
    block_nr_c +
    Change_category01 +
    #(1 | block_id) +  
    (1 + Change_category01 || subject_id_1),
  dispformula = ~ guessed_block_rule_1,
  family = beta_family(link = "logit"),
  data = na.omit(sub_et_qu2),
  
)
summary(bm2.1_dwell_time_early_1)
#  Family: beta  ( logit )
# Formula:          dwell_time_early_1_t01 ~ IA_LABEL_f * Perf_Strivings_c + block_nr_c +  
#     Change_category01 + (1 + Change_category01 || subject_id_1)
# Dispersion:                              ~guessed_block_rule_1
# Data: na.omit(sub_et_qu2)
# 
#      AIC      BIC   logLik deviance df.resid 
#  -8288.8  -8185.9   4160.4  -8320.8     4568 
# 
# Random effects:
# 
# Conditional model:
#  Groups       Name              Variance Std.Dev. Corr 
#  subject_id_1 (Intercept)       0.01051  0.1025        
#               Change_category01 0.01801  0.1342   0.00 
# Number of obs: 4584, groups:  subject_id_1, 92
# 
# Conditional model:
#                                       Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                         -1.465e+00  2.424e-02  -60.42  < 2e-16 ***
# IA_LABEL_fPositive                   1.439e-01  2.948e-02    4.88 1.06e-06 ***
# IA_LABEL_fNegative                   4.200e-02  2.968e-02    1.42  0.15702    
# IA_LABEL_fSuccess                    3.181e-02  2.973e-02    1.07  0.28466    
# IA_LABEL_fFailure                    8.214e-02  2.957e-02    2.78  0.00548 ** 
# Perf_Strivings_c                     1.964e-02  1.281e-02    1.53  0.12526    
# block_nr_c                           1.841e-05  3.245e-03    0.01  0.99547    
# Change_category01                   -2.047e-02  2.722e-02   -0.75  0.45210    
# IA_LABEL_fPositive:Perf_Strivings_c -3.318e-02  1.589e-02   -2.09  0.03675 *  
# IA_LABEL_fNegative:Perf_Strivings_c -1.141e-02  1.593e-02   -0.72  0.47397    
# IA_LABEL_fSuccess:Perf_Strivings_c  -3.647e-02  1.601e-02   -2.28  0.02269 *  
# IA_LABEL_fFailure:Perf_Strivings_c  -2.900e-03  1.603e-02   -0.18  0.85647    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Dispersion model:
#                      Estimate Std. Error z value Pr(>|z|)    
# (Intercept)           2.58740    0.04181   61.89   <2e-16 ***
# guessed_block_rule_1  0.03231    0.04754    0.68    0.497    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

lrtest(bm2.1_dwell_time_early_1, bm1_dwell_time_early_1)
# Model 1: dwell_time_early_1_t01 ~ IA_LABEL_f * Perf_Strivings_c + block_nr_c + 
#     Change_category01 + (1 + Change_category01 || subject_id_1)
# Model 2: dwell_time_early_1_t01 ~ IA_LABEL_f + block_nr_c + Change_category01 + 
#     (1 + Change_category01 || subject_id_1)
#   #Df LogLik Df  Chisq Pr(>Chisq)  
# 1  16 4160.4                       
# 2  11 4155.8 -5 9.2949    0.09787 .

AIC(bm2.1_dwell_time_early_1, bm1_dwell_time_early_1)
#                          df       AIC
# bm2.1_dwell_time_early_1 16 -8288.831
# bm1_dwell_time_early_1   11 -8289.536

bm2.2_dwell_time_early_1 <- glmmTMB(
  dwell_time_early_1_t01 ~ 
    IA_LABEL_f * Perf_Concerns_c +
    block_nr_c +
    Change_category01 +
    #(1 | block_id) +  
    (1 + Change_category01 || subject_id_1),
  dispformula = ~ guessed_block_rule_1,
  family = beta_family(link = "logit"),
  data = na.omit(sub_et_qu2),
  
)
summary(bm2.2_dwell_time_early_1)
#  Family: beta  ( logit )
# Formula:          dwell_time_early_1_t01 ~ IA_LABEL_f * Perf_Concerns_c + block_nr_c +  
#     Change_category01 + (1 + Change_category01 || subject_id_1)
# Dispersion:                              ~guessed_block_rule_1
# Data: na.omit(sub_et_qu2)
# 
#      AIC      BIC   logLik deviance df.resid 
#  -8282.0  -8179.1   4157.0  -8314.0     4568 
# 
# Random effects:
# 
# Conditional model:
#  Groups       Name              Variance Std.Dev. Corr 
#  subject_id_1 (Intercept)       0.01042  0.1021        
#               Change_category01 0.01828  0.1352   0.00 
# Number of obs: 4584, groups:  subject_id_1, 92
# 
# Conditional model:
#                                      Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                        -1.464e+00  2.423e-02  -60.44  < 2e-16 ***
# IA_LABEL_fPositive                  1.435e-01  2.950e-02    4.86 1.15e-06 ***
# IA_LABEL_fNegative                  4.182e-02  2.970e-02    1.41  0.15906    
# IA_LABEL_fSuccess                   3.168e-02  2.974e-02    1.06  0.28688    
# IA_LABEL_fFailure                   8.204e-02  2.959e-02    2.77  0.00556 ** 
# Perf_Concerns_c                     1.195e-02  9.977e-03    1.20  0.23091    
# block_nr_c                         -5.393e-06  3.247e-03    0.00  0.99867    
# Change_category01                  -2.074e-02  2.729e-02   -0.76  0.44726    
# IA_LABEL_fPositive:Perf_Concerns_c -1.298e-02  1.236e-02   -1.05  0.29355    
# IA_LABEL_fNegative:Perf_Concerns_c -1.151e-02  1.244e-02   -0.93  0.35486    
# IA_LABEL_fSuccess:Perf_Concerns_c  -1.886e-02  1.247e-02   -1.51  0.13039    
# IA_LABEL_fFailure:Perf_Concerns_c  -1.385e-02  1.241e-02   -1.12  0.26412    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Dispersion model:
#                      Estimate Std. Error z value Pr(>|z|)    
# (Intercept)           2.58180    0.04171   61.90   <2e-16 ***
# guessed_block_rule_1  0.03758    0.04740    0.79    0.428    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

lrtest(bm2.2_dwell_time_early_1, bm1_dwell_time_early_1)
# Model 1: dwell_time_early_1_t01 ~ IA_LABEL_f * Perf_Concerns_c + block_nr_c + 
#     Change_category01 + (1 + Change_category01 || subject_id_1)
# Model 2: dwell_time_early_1_t01 ~ IA_LABEL_f + block_nr_c + Change_category01 + 
#     (1 + Change_category01 || subject_id_1)
#   #Df LogLik Df  Chisq Pr(>Chisq)
# 1  16 4157.0                     
# 2  11 4155.8 -5 2.4916     0.7778

AIC(bm2.2_dwell_time_early_1, bm1_dwell_time_early_1)
#                          df       AIC
# bm2.2_dwell_time_early_1 16 -8282.028
# bm1_dwell_time_early_1   11 -8289.536

# Intermediate dwell time
#-------------------------
bm0_dwell_time_intermediate_1 <- glmmTMB(
  dwell_time_intermediate_1_t01 ~ 
    block_nr_c +
    Change_category01 +
    #(1 | block_id) +  
    (1 + Change_category01 || subject_id_1),
  dispformula = ~ guessed_block_rule_1,
  family = beta_family(link = "logit"),
  data = na.omit(sub_et_qu2),
  
)
summary(bm0_dwell_time_intermediate_1)
# Family: beta  ( logit )
# Formula:          dwell_time_intermediate_1_t01 ~ block_nr_c + Change_category01 +  
#     (1 + Change_category01 || subject_id_1)
# Dispersion:                                     ~guessed_block_rule_1
# Data: na.omit(sub_et_qu2)
# 
#      AIC      BIC   logLik deviance df.resid 
# -11861.4 -11816.4   5937.7 -11875.4     4577 
# 
# Random effects:
# 
# Conditional model:
#  Groups       Name              Variance Std.Dev. Corr 
#  subject_id_1 (Intercept)       0.01771  0.1331        
#               Change_category01 0.24904  0.4990   0.00 
# Number of obs: 4584, groups:  subject_id_1, 92
# 
# Conditional model:
#                    Estimate Std. Error z value Pr(>|z|)    
# (Intercept)       -1.517136   0.015585  -97.35   <2e-16 ***
# block_nr_c        -0.000769   0.002105   -0.37    0.715    
# Change_category01  0.541324   0.053947   10.03   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Dispersion model:
#                      Estimate Std. Error z value Pr(>|z|)    
# (Intercept)           3.68135    0.04327   85.08  < 2e-16 ***
# guessed_block_rule_1 -0.13541    0.05000   -2.71  0.00676 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

bm1_dwell_time_intermediate_1 <- glmmTMB(
  dwell_time_intermediate_1_t01 ~ 
    IA_LABEL_f +
    block_nr_c +
    Change_category01 +
    #(1 | block_id) +  
    (1 + Change_category01 || subject_id_1),
  dispformula = ~ guessed_block_rule_1,
  family = beta_family(link = "logit"),
  data = na.omit(sub_et_qu2),
)
summary(bm1_dwell_time_intermediate_1)
# Family: beta  ( logit )
# Formula:          dwell_time_intermediate_1_t01 ~ IA_LABEL_f + block_nr_c + Change_category01 +  
#     (1 + Change_category01 || subject_id_1)
# Dispersion:                                     ~guessed_block_rule_1
# Data: na.omit(sub_et_qu2)
# 
#      AIC      BIC   logLik deviance df.resid 
# -11882.1 -11811.3   5952.0 -11904.1     4573 
# 
# Random effects:
# 
# Conditional model:
#  Groups       Name              Variance Std.Dev. Corr 
#  subject_id_1 (Intercept)       0.01779  0.1334        
#               Change_category01 0.24939  0.4994   0.00 
# Number of obs: 4584, groups:  subject_id_1, 92
# 
# Conditional model:
#                      Estimate Std. Error z value Pr(>|z|)    
# (Intercept)        -1.5556502  0.0198232  -78.48  < 2e-16 ***
# IA_LABEL_fPositive  0.0744160  0.0190965    3.90 9.75e-05 ***
# IA_LABEL_fNegative  0.0514463  0.0190984    2.69 0.007065 ** 
# IA_LABEL_fSuccess   0.0661192  0.0190651    3.47 0.000524 ***
# IA_LABEL_fFailure  -0.0006053  0.0192440   -0.03 0.974907    
# block_nr_c         -0.0007097  0.0020984   -0.34 0.735211    
# Change_category01   0.5409400  0.0539706   10.02  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Dispersion model:
#                      Estimate Std. Error z value Pr(>|z|)    
# (Intercept)           3.69499    0.04329   85.35  < 2e-16 ***
# guessed_block_rule_1 -0.14461    0.05003   -2.89  0.00384 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

lrtest(bm1_dwell_time_intermediate_1, bm0_dwell_time_intermediate_1)
# Model 1: dwell_time_intermediate_1_t01 ~ IA_LABEL_f + block_nr_c + Change_category01 + 
#     (1 + Change_category01 || subject_id_1)
# Model 2: dwell_time_intermediate_1_t01 ~ block_nr_c + Change_category01 + 
#     (1 + Change_category01 || subject_id_1)
#   #Df LogLik Df  Chisq Pr(>Chisq)    
# 1  11 5952.0                         
# 2   7 5937.7 -4 28.656   9.18e-06 ***
  
AIC(bm1_dwell_time_intermediate_1, bm0_dwell_time_intermediate_1)
#                               df       AIC
# bm1_dwell_time_intermediate_1 11 -11882.06
# bm0_dwell_time_intermediate_1  7 -11861.40

bm2.1_dwell_time_intermediate_1 <- glmmTMB(
  dwell_time_intermediate_1_t01 ~ 
    IA_LABEL_f * Perf_Strivings_c +
    block_nr_c +
    Change_category01 +
    #(1 | block_id) +  
    (1 + Change_category01 || subject_id_1),
  dispformula = ~ guessed_block_rule_1,
  family = beta_family(link = "logit"),
  data = na.omit(sub_et_qu2),
  
)
summary(bm2.1_dwell_time_intermediate_1)
# Family: beta  ( logit )
# Formula:          dwell_time_intermediate_1_t01 ~ IA_LABEL_f * Perf_Strivings_c +  
#     block_nr_c + Change_category01 + (1 + Change_category01 ||      subject_id_1)
# Dispersion:                                     ~guessed_block_rule_1
# Data: na.omit(sub_et_qu2)
# 
#      AIC      BIC   logLik deviance df.resid 
# -11877.0 -11774.1   5954.5 -11909.0     4568 
# 
# Random effects:
# 
# Conditional model:
#  Groups       Name              Variance Std.Dev. Corr 
#  subject_id_1 (Intercept)       0.01757  0.1325        
#               Change_category01 0.24929  0.4993   0.00 
# Number of obs: 4584, groups:  subject_id_1, 92
# 
# Conditional model:
#                                       Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                         -1.5557020  0.0197593  -78.73  < 2e-16 ***
# IA_LABEL_fPositive                   0.0744695  0.0190895    3.90 9.58e-05 ***
# IA_LABEL_fNegative                   0.0514057  0.0190921    2.69 0.007092 ** 
# IA_LABEL_fSuccess                    0.0661893  0.0190579    3.47 0.000515 ***
# IA_LABEL_fFailure                   -0.0005999  0.0192370   -0.03 0.975121    
# Perf_Strivings_c                    -0.0089083  0.0106260   -0.84 0.401833    
# block_nr_c                          -0.0007171  0.0020977   -0.34 0.732455    
# Change_category01                    0.5409015  0.0539592   10.02  < 2e-16 ***
# IA_LABEL_fPositive:Perf_Strivings_c  0.0076047  0.0102555    0.74 0.458378    
# IA_LABEL_fNegative:Perf_Strivings_c -0.0103379  0.0102495   -1.01 0.313155    
# IA_LABEL_fSuccess:Perf_Strivings_c   0.0058209  0.0102791    0.57 0.571204    
# IA_LABEL_fFailure:Perf_Strivings_c   0.0054422  0.0103656    0.53 0.599564    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Dispersion model:
#                      Estimate Std. Error z value Pr(>|z|)    
# (Intercept)           3.69501    0.04331   85.31  < 2e-16 ***
# guessed_block_rule_1 -0.14345    0.05005   -2.87  0.00415 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

lrtest(bm2.1_dwell_time_intermediate_1, bm1_dwell_time_intermediate_1)
# Model 1: dwell_time_intermediate_1_t01 ~ IA_LABEL_f * Perf_Strivings_c + 
#   block_nr_c + Change_category01 + (1 + Change_category01 || 
#                                       subject_id_1)
# Model 2: dwell_time_intermediate_1_t01 ~ IA_LABEL_f + block_nr_c + Change_category01 + 
#   (1 + Change_category01 || subject_id_1)
# #Df LogLik Df  Chisq Pr(>Chisq)
# 1  16 5954.5                     
# # 2  11 5952.0 -5 4.8985     0.4284

AIC(bm2.1_dwell_time_intermediate_1, bm1_dwell_time_intermediate_1)
#                                 df       AIC
# bm2.1_dwell_time_intermediate_1 16 -11876.96
# bm1_dwell_time_intermediate_1   11 -11882.06

bm2.2_dwell_time_intermediate_1 <- glmmTMB(
  dwell_time_intermediate_1_t01 ~ 
    IA_LABEL_f * Perf_Concerns_c +
    block_nr_c +
    Change_category01 +
    #(1 | block_id) +  
    (1 + Change_category01 || subject_id_1),
  dispformula = ~ guessed_block_rule_1,
  family = beta_family(link = "logit"),
  data = na.omit(sub_et_qu2),
)
summary(bm2.2_dwell_time_intermediate_1)
# Family: beta  ( logit )
# Formula:          dwell_time_intermediate_1_t01 ~ IA_LABEL_f * Perf_Concerns_c +  
#     block_nr_c + Change_category01 + (1 + Change_category01 ||      subject_id_1)
# Dispersion:                                     ~guessed_block_rule_1
# Data: na.omit(sub_et_qu2)
# 
#      AIC      BIC   logLik deviance df.resid 
# -11889.4 -11786.5   5960.7 -11921.4     4568 
# 
# Random effects:
# 
# Conditional model:
#  Groups       Name              Variance Std.Dev. Corr 
#  subject_id_1 (Intercept)       0.01748  0.1322        
#               Change_category01 0.24970  0.4997   0.00 
# Number of obs: 4584, groups:  subject_id_1, 92
# 
# Conditional model:
#                                      Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                        -1.5556965  0.0197209  -78.89  < 2e-16 ***
# IA_LABEL_fPositive                  0.0741322  0.0190670    3.89 0.000101 ***
# IA_LABEL_fNegative                  0.0514203  0.0190680    2.70 0.007004 ** 
# IA_LABEL_fSuccess                   0.0661028  0.0190351    3.47 0.000515 ***
# IA_LABEL_fFailure                  -0.0007114  0.0192153   -0.04 0.970467    
# Perf_Concerns_c                    -0.0022903  0.0082363   -0.28 0.780960    
# block_nr_c                         -0.0007225  0.0020951   -0.34 0.730217    
# Change_category01                   0.5409249  0.0539965   10.02  < 2e-16 ***
# IA_LABEL_fPositive:Perf_Concerns_c -0.0094162  0.0079859   -1.18 0.238356    
# IA_LABEL_fNegative:Perf_Concerns_c -0.0209204  0.0080042   -2.61 0.008957 ** 
# IA_LABEL_fSuccess:Perf_Concerns_c  -0.0022654  0.0080114   -0.28 0.777348    
# IA_LABEL_fFailure:Perf_Concerns_c   0.0095334  0.0080588    1.18 0.236818    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Dispersion model:
#                      Estimate Std. Error z value Pr(>|z|)    
# (Intercept)           3.69497    0.04332   85.29  < 2e-16 ***
# guessed_block_rule_1 -0.13976    0.05008   -2.79  0.00526 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

lrtest(bm2.2_dwell_time_intermediate_1, bm1_dwell_time_intermediate_1)
# Model 1: dwell_time_intermediate_1_t01 ~ IA_LABEL_f * Perf_Concerns_c + 
#     block_nr_c + Change_category01 + (1 + Change_category01 || 
#     subject_id_1)
# Model 2: dwell_time_intermediate_1_t01 ~ IA_LABEL_f + block_nr_c + Change_category01 + 
#     (1 + Change_category01 || subject_id_1)
#   #Df LogLik Df  Chisq Pr(>Chisq)   
# 1  16 5960.7                        
# 2  11 5952.0 -5 17.297   0.003969 **

AIC(bm2.2_dwell_time_intermediate_1, bm1_dwell_time_intermediate_1)
#                                 df       AIC
# bm2.2_dwell_time_intermediate_1 16 -11889.36
# bm1_dwell_time_intermediate_1   11 -11882.06

dwell_time_intermediate_concerns_slopes <- emtrends(
  bm2.2_dwell_time_intermediate_1, ~ IA_LABEL_f, var = "Perf_Concerns_c"
)
summary(dwell_time_intermediate_concerns_slopes, infer = c(TRUE, TRUE))
# IA_LABEL_f Perf_Concerns_c.trend      SE  df asymp.LCL asymp.UCL z.ratio p.value
#  Neutral                 -0.00229 0.00824 Inf  -0.01843   0.01385  -0.278  0.7810
#  Positive                -0.01171 0.00817 Inf  -0.02772   0.00431  -1.432  0.1520
#  Negative                -0.02321 0.00817 Inf  -0.03922  -0.00720  -2.841  0.0045
#  Success                 -0.00456 0.00819 Inf  -0.02060   0.01149  -0.556  0.5779
#  Failure                  0.00724 0.00823 Inf  -0.00889   0.02337   0.880  0.3788

dwell_time_intermediate_planned_slopes <- contrast(
  dwell_time_intermediate_concerns_slopes, method = semantic_planned, adjust = "none"
)
dwell_time_intermediate_planned_slopes
# contrast                            estimate     SE  df z.ratio p.value
#  positive_vs_negative                0.000295 0.0113 Inf   0.026  0.9791
#  Success_Failure                     0.011799 0.0080 Inf   1.474  0.1405
#  performance_irrelevant_vs_relevant  0.082477 0.0310 Inf   2.662  0.0078
#  NonNeutral_vs_Neutral              -0.023069 0.0254 Inf  -0.907  0.3643

# Late dwell time
#----------------
bm0_dwell_time_late_1 <- glmmTMB(
  dwell_time_late_1_t01 ~ 
    block_nr_c +
    Change_category01 +
    #(1 | block_id) +  
    (1 + Change_category01 || subject_id_1),
  dispformula = ~ guessed_block_rule_1,
  family = beta_family(link = "logit"),
  data = na.omit(sub_et_qu2),
  
)
summary(bm0_dwell_time_late_1)
# Family: beta  ( logit )
# Formula:          dwell_time_late_1_t01 ~ block_nr_c + Change_category01 + (1 +  
#     Change_category01 || subject_id_1)
# Dispersion:                             ~guessed_block_rule_1
# Data: na.omit(sub_et_qu2)
# 
#      AIC      BIC   logLik deviance df.resid 
#  -9199.1  -9154.1   4606.6  -9213.1     4577 
# 
# Random effects:
# 
# Conditional model:
#  Groups       Name              Variance Std.Dev. Corr 
#  subject_id_1 (Intercept)       0.2111   0.4595        
#               Change_category01 1.5063   1.2273   0.00 
# Number of obs: 4584, groups:  subject_id_1, 92
# 
# Conditional model:
#                    Estimate Std. Error z value Pr(>|z|)    
# (Intercept)       -1.840787   0.049677  -37.06   <2e-16 ***
# block_nr_c        -0.002849   0.003481   -0.82    0.413    
# Change_category01  1.528860   0.130110   11.75   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Dispersion model:
#                      Estimate Std. Error z value Pr(>|z|)    
# (Intercept)           2.81128    0.04372   64.30  < 2e-16 ***
# guessed_block_rule_1 -0.37534    0.05100   -7.36 1.85e-13 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

bm1_dwell_time_late_1 <- glmmTMB(
  dwell_time_late_1_t01 ~ 
    IA_LABEL_f +
    block_nr_c +
    Change_category01 +
    #(1 | block_id) +  
    (1 + Change_category01 || subject_id_1),
  dispformula = ~ guessed_block_rule_1,
  family = beta_family(link = "logit"),
  data = na.omit(sub_et_qu2),
  
)
summary(bm1_dwell_time_late_1)
# Family: beta  ( logit )
# Formula:          dwell_time_late_1_t01 ~ IA_LABEL_f + block_nr_c + Change_category01 +  
#     (1 + Change_category01 || subject_id_1)
# Dispersion:                             ~guessed_block_rule_1
# Data: na.omit(sub_et_qu2)
# 
#      AIC      BIC   logLik deviance df.resid 
#  -9206.2  -9135.5   4614.1  -9228.2     4573 
# 
# Random effects:
# 
# Conditional model:
#  Groups       Name              Variance Std.Dev. Corr 
#  subject_id_1 (Intercept)       0.2114   0.4597        
#               Change_category01 1.5076   1.2278   0.00 
# Number of obs: 4584, groups:  subject_id_1, 92
# 
# Conditional model:
#                     Estimate Std. Error z value Pr(>|z|)    
# (Intercept)        -1.914596   0.053764  -35.61  < 2e-16 ***
# IA_LABEL_fPositive  0.074071   0.032028    2.31 0.020741 *  
# IA_LABEL_fNegative  0.115929   0.031543    3.68 0.000238 ***
# IA_LABEL_fSuccess   0.088719   0.031722    2.80 0.005161 ** 
# IA_LABEL_fFailure   0.083627   0.031690    2.64 0.008317 ** 
# block_nr_c         -0.002683   0.003478   -0.77 0.440455    
# Change_category01   1.530232   0.130156   11.76  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Dispersion model:
#                      Estimate Std. Error z value Pr(>|z|)    
# (Intercept)           2.81076    0.04377   64.21  < 2e-16 ***
# guessed_block_rule_1 -0.36951    0.05111   -7.23 4.82e-13 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

lrtest(bm1_dwell_time_late_1, bm0_dwell_time_late_1)
# Model 1: dwell_time_late_1_t01 ~ IA_LABEL_f + block_nr_c + Change_category01 + 
#     (1 + Change_category01 || subject_id_1)
# Model 2: dwell_time_late_1_t01 ~ block_nr_c + Change_category01 + (1 + 
#     Change_category01 || subject_id_1)
#   #Df LogLik Df  Chisq Pr(>Chisq)   
# 1  11 4614.1                        
# 2   7 4606.6 -4 15.063   0.004572 **
  
AIC(bm1_dwell_time_late_1, bm0_dwell_time_late_1)
#                       df       AIC
# bm1_dwell_time_late_1 11 -9206.194
# bm0_dwell_time_late_1  7 -9199.131

bm2.1_dwell_time_late_1 <- glmmTMB(
  dwell_time_late_1_t01 ~ 
    IA_LABEL_f * Perf_Strivings_c +
    block_nr_c +
    Change_category01 +
    #(1 | block_id) +  
    (1 + Change_category01 || subject_id_1),
  dispformula = ~ guessed_block_rule_1,
  family = beta_family(link = "logit"),
  data = na.omit(sub_et_qu2),
  
)
summary(bm2.1_dwell_time_late_1)
# Family: beta  ( logit )
# Formula:          dwell_time_late_1_t01 ~ IA_LABEL_f * Perf_Strivings_c + block_nr_c +  
#     Change_category01 + (1 + Change_category01 || subject_id_1)
# Dispersion:                             ~guessed_block_rule_1
# Data: na.omit(sub_et_qu2)
# 
#      AIC      BIC   logLik deviance df.resid 
#  -9203.3  -9100.4   4617.6  -9235.3     4568 
# 
# Random effects:
# 
# Conditional model:
#  Groups       Name              Variance Std.Dev. Corr 
#  subject_id_1 (Intercept)       0.2115   0.4599        
#               Change_category01 1.5117   1.2295   0.00 
# Number of obs: 4584, groups:  subject_id_1, 92
# 
# Conditional model:
#                                      Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                         -1.914821   0.053771  -35.61  < 2e-16 ***
# IA_LABEL_fPositive                   0.073783   0.031999    2.31 0.021123 *  
# IA_LABEL_fNegative                   0.115654   0.031512    3.67 0.000242 ***
# IA_LABEL_fSuccess                    0.088562   0.031690    2.79 0.005195 ** 
# IA_LABEL_fFailure                    0.083597   0.031654    2.64 0.008267 ** 
# Perf_Strivings_c                     0.001681   0.028832    0.06 0.953502    
# block_nr_c                          -0.002618   0.003474   -0.75 0.451164    
# Change_category01                    1.530608   0.130321   11.74  < 2e-16 ***
# IA_LABEL_fPositive:Perf_Strivings_c -0.024743   0.017353   -1.43 0.153911    
# IA_LABEL_fNegative:Perf_Strivings_c -0.014647   0.016962   -0.86 0.387850    
# IA_LABEL_fSuccess:Perf_Strivings_c   0.014471   0.017059    0.85 0.396279    
# IA_LABEL_fFailure:Perf_Strivings_c  -0.020027   0.017173   -1.17 0.243540    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

lrtest(bm1_dwell_time_late_1, bm2.1_dwell_time_late_1)
# Model 1: dwell_time_late_1_t01 ~ IA_LABEL_f + block_nr_c + Change_category01 + 
#     (1 + Change_category01 || subject_id_1)
# Model 2: dwell_time_late_1_t01 ~ IA_LABEL_f * Perf_Strivings_c + block_nr_c + 
#     Change_category01 + (1 + Change_category01 || subject_id_1)
#   #Df LogLik Df Chisq Pr(>Chisq)
# 1  11 4614.1                    
# 2  16 4617.6  5 7.092     0.2139

AIC(bm1_dwell_time_late_1, bm2.1_dwell_time_late_1)
#                         df       AIC
# bm1_dwell_time_late_1   11 -9206.194
# bm2.1_dwell_time_late_1 16 -9203.286

bm2.2_dwell_time_late_1 <- glmmTMB(
  dwell_time_late_1_t01 ~ 
    IA_LABEL_f * Perf_Concerns_c +
    block_nr_c +
    Change_category01 +
    #(1 | block_id) +  
    (1 + Change_category01 || subject_id_1),
  dispformula = ~ guessed_block_rule_1,
  family = beta_family(link = "logit"),
  data = na.omit(sub_et_qu2),
  
)
summary(bm2.2_dwell_time_late_1)
# Family: beta  ( logit )
# Formula:          dwell_time_late_1_t01 ~ IA_LABEL_f * Perf_Concerns_c + block_nr_c +  
#     Change_category01 + (1 + Change_category01 || subject_id_1)
# Dispersion:                             ~guessed_block_rule_1
# Data: na.omit(sub_et_qu2)
# 
#      AIC      BIC   logLik deviance df.resid 
#  -9203.3  -9100.4   4617.6  -9235.3     4568 
# 
# Random effects:
# 
# Conditional model:
#  Groups       Name              Variance Std.Dev. Corr 
#  subject_id_1 (Intercept)       0.2082   0.4563        
#               Change_category01 1.5077   1.2279   0.00 
# Number of obs: 4584, groups:  subject_id_1, 92
# 
# Conditional model:
#                                     Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                        -1.913911   0.053437  -35.82  < 2e-16 ***
# IA_LABEL_fPositive                  0.072038   0.032033    2.25 0.024523 *  
# IA_LABEL_fNegative                  0.115055   0.031536    3.65 0.000264 ***
# IA_LABEL_fSuccess                   0.088435   0.031723    2.79 0.005308 ** 
# IA_LABEL_fFailure                   0.082804   0.031685    2.61 0.008967 ** 
# Perf_Concerns_c                    -0.012801   0.022339   -0.57 0.566625    
# block_nr_c                         -0.002704   0.003477   -0.78 0.436763    
# Change_category01                   1.530844   0.130158   11.76  < 2e-16 ***
# IA_LABEL_fPositive:Perf_Concerns_c -0.028770   0.013822   -2.08 0.037396 *  
# IA_LABEL_fNegative:Perf_Concerns_c -0.015836   0.013556   -1.17 0.242738    
# IA_LABEL_fSuccess:Perf_Concerns_c  -0.002930   0.013545   -0.22 0.828739    
# IA_LABEL_fFailure:Perf_Concerns_c  -0.006662   0.013604   -0.49 0.624354    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Dispersion model:
#                      Estimate Std. Error z value Pr(>|z|)    
# (Intercept)           2.81022    0.04381   64.15  < 2e-16 ***
# guessed_block_rule_1 -0.36700    0.05116   -7.17 7.27e-13 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

lrtest(bm1_dwell_time_late_1, bm2.2_dwell_time_late_1)
# Model 1: dwell_time_late_1_t01 ~ IA_LABEL_f + block_nr_c + Change_category01 + 
#     (1 + Change_category01 || subject_id_1)
# Model 2: dwell_time_late_1_t01 ~ IA_LABEL_f * Perf_Concerns_c + block_nr_c + 
#     Change_category01 + (1 + Change_category01 || subject_id_1)
#   #Df LogLik Df  Chisq Pr(>Chisq)
# 1  11 4614.1                     
# 2  16 4617.6  5 7.0745     0.2152

AIC(bm1_dwell_time_late_1, bm2.2_dwell_time_late_1)
#                         df       AIC
# bm1_dwell_time_late_1   11 -9206.194
# bm2.2_dwell_time_late_1 16 -9203.269

# Whole-trial dwell time
#-----------------------
bm0_dwell_time_1 <- glmmTMB(
  dwell_time_1_t01 ~ 
    block_nr_c +
    Change_category01 +
    #(1 | block_id) +  
    (1 + Change_category01 || subject_id_1),
  dispformula = ~ guessed_block_rule_1,
  family = beta_family(link = "logit"),
  data = na.omit(sub_et_qu2),
  
)
summary(bm0_dwell_time_1)
# Family: beta  ( logit )
# Formula:          dwell_time_1_t01 ~ block_nr_c + Change_category01 + (1 + Change_category01 ||  
#     subject_id_1)
# Dispersion:                        ~guessed_block_rule_1
# Data: na.omit(sub_et_qu2)
# 
#      AIC      BIC   logLik deviance df.resid 
# -13312.1 -13267.1   6663.1 -13326.1     4577 
# 
# Random effects:
# 
# Conditional model:
#  Groups       Name              Variance Std.Dev. Corr 
#  subject_id_1 (Intercept)       0.04098  0.2024        
#               Change_category01 0.47615  0.6900   0.00 
# Number of obs: 4584, groups:  subject_id_1, 92
# 
# Conditional model:
#                    Estimate Std. Error z value Pr(>|z|)    
# (Intercept)       -1.611805   0.022014  -73.22   <2e-16 ***
# block_nr_c        -0.000212   0.001815   -0.12    0.907    
# Change_category01  0.879832   0.072934   12.06   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Dispersion model:
#                      Estimate Std. Error z value Pr(>|z|)    
# (Intercept)           4.01640    0.04488   89.50   <2e-16 ***
# guessed_block_rule_1 -0.13332    0.05235   -2.55   0.0109 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

bm1_dwell_time_1 <- glmmTMB(
  dwell_time_1_t01 ~ 
    IA_LABEL_f +
    block_nr_c +
    Change_category01 +
    #(1 | block_id) +  
    (1 + Change_category01 || subject_id_1),
  dispformula = ~ guessed_block_rule_1,
  family = beta_family(link = "logit"),
  data = na.omit(sub_et_qu2),
)
summary(bm1_dwell_time_1)
# Family: beta  ( logit )
# Formula:          dwell_time_1_t01 ~ IA_LABEL_f + block_nr_c + Change_category01 +  
#     (1 + Change_category01 || subject_id_1)
# Dispersion:                        ~guessed_block_rule_1
# Data: na.omit(sub_et_qu2)
# 
#      AIC      BIC   logLik deviance df.resid 
# -13329.0 -13258.3   6675.5 -13351.0     4573 
# 
# Random effects:
# 
# Conditional model:
#  Groups       Name              Variance Std.Dev. Corr 
#  subject_id_1 (Intercept)       0.04097  0.2024        
#               Change_category01 0.47616  0.6900   0.00 
# Number of obs: 4584, groups:  subject_id_1, 92
# 
# Conditional model:
#                      Estimate Std. Error z value Pr(>|z|)    
# (Intercept)        -1.6590027  0.0244314  -67.90  < 2e-16 ***
# IA_LABEL_fPositive  0.0664057  0.0165748    4.01 6.16e-05 ***
# IA_LABEL_fNegative  0.0638441  0.0164722    3.88 0.000106 ***
# IA_LABEL_fSuccess   0.0675210  0.0164913    4.09 4.23e-05 ***
# IA_LABEL_fFailure   0.0368823  0.0165572    2.23 0.025910 *  
# block_nr_c         -0.0001367  0.0018099   -0.08 0.939786    
# Change_category01   0.8798619  0.0729296   12.06  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Dispersion model:
#                      Estimate Std. Error z value Pr(>|z|)    
# (Intercept)           4.02552    0.04501   89.45  < 2e-16 ***
# guessed_block_rule_1 -0.13781    0.05254   -2.62  0.00872 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

lrtest(bm1_dwell_time_1, bm0_dwell_time_1)
# Model 1: dwell_time_1_t01 ~ IA_LABEL_f + block_nr_c + Change_category01 + 
#     (1 + Change_category01 || subject_id_1)
# Model 2: dwell_time_1_t01 ~ block_nr_c + Change_category01 + (1 + Change_category01 || 
#     subject_id_1)
#   #Df LogLik Df  Chisq Pr(>Chisq)    
# 1  11 6675.5                         
# 2   7 6663.1 -4 24.899  5.271e-05 ***

AIC(bm1_dwell_time_1, bm0_dwell_time_1)
#                  df      AIC
# bm1_dwell_time_1 11 -13329.0
# bm0_dwell_time_1  7 -13312.1

bm2.1_dwell_time_1 <- glmmTMB(
  dwell_time_1_t01 ~ 
    IA_LABEL_f * Perf_Strivings_c +
    block_nr_c +
    Change_category01 +
    #(1 | block_id) +  
    (1 + Change_category01 || subject_id_1),
  dispformula = ~ guessed_block_rule_1,
  family = beta_family(link = "logit"),
  data = na.omit(sub_et_qu2),
  
)
summary(bm2.1_dwell_time_1)
# Family: beta  ( logit )
# Formula:          dwell_time_1_t01 ~ IA_LABEL_f * Perf_Strivings_c + block_nr_c +  
#     Change_category01 + (1 + Change_category01 || subject_id_1)
# Dispersion:                        ~guessed_block_rule_1
# Data: na.omit(sub_et_qu2)
# 
#      AIC      BIC   logLik deviance df.resid 
# -13322.9 -13220.0   6677.4 -13354.9     4568 
# 
# Random effects:
# 
# Conditional model:
#  Groups       Name              Variance Std.Dev. Corr 
#  subject_id_1 (Intercept)       0.04078  0.2019        
#               Change_category01 0.47613  0.6900   0.00 
# Number of obs: 4584, groups:  subject_id_1, 92
# 
# Conditional model:
#                                       Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                         -1.6590100  0.0243865  -68.03  < 2e-16 ***
# IA_LABEL_fPositive                   0.0663208  0.0165689    4.00 6.26e-05 ***
# IA_LABEL_fNegative                   0.0637862  0.0164661    3.87 0.000107 ***
# IA_LABEL_fSuccess                    0.0675304  0.0164851    4.10 4.20e-05 ***
# IA_LABEL_fFailure                    0.0368393  0.0165507    2.23 0.026024 *  
# Perf_Strivings_c                    -0.0035649  0.0131551   -0.27 0.786395    
# block_nr_c                          -0.0001397  0.0018091   -0.08 0.938468    
# Change_category01                    0.8799394  0.0729269   12.07  < 2e-16 ***
# IA_LABEL_fPositive:Perf_Strivings_c -0.0101993  0.0089476   -1.14 0.254332    
# IA_LABEL_fNegative:Perf_Strivings_c -0.0060617  0.0088739   -0.68 0.494553    
# IA_LABEL_fSuccess:Perf_Strivings_c   0.0045788  0.0089068    0.51 0.607194    
# IA_LABEL_fFailure:Perf_Strivings_c  -0.0063187  0.0089607   -0.71 0.480709    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Dispersion model:
#                      Estimate Std. Error z value Pr(>|z|)    
# (Intercept)           4.02758    0.04504   89.42   <2e-16 ***
# guessed_block_rule_1 -0.13946    0.05259   -2.65    0.008 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

lrtest(bm2.1_dwell_time_1, bm1_dwell_time_1)
# Model 1: dwell_time_1_t01 ~ IA_LABEL_f * Perf_Strivings_c + block_nr_c + 
#     Change_category01 + (1 + Change_category01 || subject_id_1)
# Model 2: dwell_time_1_t01 ~ IA_LABEL_f + block_nr_c + Change_category01 + 
#     (1 + Change_category01 || subject_id_1)
#   #Df LogLik Df  Chisq Pr(>Chisq)
# 1  16 6677.4                     
# 2  11 6675.5 -5 3.8572     0.5701

AIC(bm2.1_dwell_time_1, bm1_dwell_time_1)
#                    df       AIC
# bm2.1_dwell_time_1 16 -13322.86
# bm1_dwell_time_1   11 -13329.00

bm2.2_dwell_time_1 <- glmmTMB(
  dwell_time_1_t01 ~ 
    IA_LABEL_f * Perf_Concerns_c +
    block_nr_c +
    Change_category01 +
    #(1 | block_id) +  
    (1 + Change_category01 || subject_id_1),
  dispformula = ~ guessed_block_rule_1,
  family = beta_family(link = "logit"),
  data = na.omit(sub_et_qu2),
  
)
summary(bm2.2_dwell_time_1)
# Family: beta  ( logit )
# Formula:          dwell_time_1_t01 ~ IA_LABEL_f * Perf_Concerns_c + block_nr_c +  
#     Change_category01 + (1 + Change_category01 || subject_id_1)
# Dispersion:                        ~guessed_block_rule_1
# Data: na.omit(sub_et_qu2)
# 
#      AIC      BIC   logLik deviance df.resid 
# -13330.4 -13227.5   6681.2 -13362.4     4568 
# 
# Random effects:
# 
# Conditional model:
#  Groups       Name              Variance Std.Dev. Corr 
#  subject_id_1 (Intercept)       0.04025  0.2006        
#               Change_category01 0.47647  0.6903   0.00 
# Number of obs: 4584, groups:  subject_id_1, 92
# 
# Conditional model:
#                                      Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                        -1.6589387  0.0242635  -68.37  < 2e-16 ***
# IA_LABEL_fPositive                  0.0657051  0.0165606    3.97 7.26e-05 ***
# IA_LABEL_fNegative                  0.0637031  0.0164565    3.87 0.000108 ***
# IA_LABEL_fSuccess                   0.0674240  0.0164766    4.09 4.27e-05 ***
# IA_LABEL_fFailure                   0.0367639  0.0165424    2.22 0.026256 *  
# Perf_Concerns_c                    -0.0055556  0.0101675   -0.55 0.584788    
# block_nr_c                         -0.0001556  0.0018081   -0.09 0.931414    
# Change_category01                   0.8803036  0.0729504   12.07  < 2e-16 ***
# IA_LABEL_fPositive:Perf_Concerns_c -0.0175119  0.0070140   -2.50 0.012536 *  
# IA_LABEL_fNegative:Perf_Concerns_c -0.0092928  0.0069641   -1.33 0.182080    
# IA_LABEL_fSuccess:Perf_Concerns_c  -0.0041281  0.0069719   -0.59 0.553780    
# IA_LABEL_fFailure:Perf_Concerns_c   0.0015237  0.0070008    0.22 0.827700    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Dispersion model:
#                      Estimate Std. Error z value Pr(>|z|)    
# (Intercept)           4.02292    0.04500   89.39   <2e-16 ***
# guessed_block_rule_1 -0.13132    0.05254   -2.50   0.0124 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

lrtest(bm2.2_dwell_time_1, bm1_dwell_time_1)
# Model 1: dwell_time_1_t01 ~ IA_LABEL_f * Perf_Concerns_c + block_nr_c + 
#     Change_category01 + (1 + Change_category01 || subject_id_1)
# Model 2: dwell_time_1_t01 ~ IA_LABEL_f + block_nr_c + Change_category01 + 
#     (1 + Change_category01 || subject_id_1)
#   #Df LogLik Df  Chisq Pr(>Chisq)  
# 1  16 6681.2                       
# 2  11 6675.5 -5 11.383    0.04429 *

AIC(bm2.2_dwell_time_1, bm1_dwell_time_1)
#                    df       AIC
# bm2.2_dwell_time_1 16 -13330.38
# bm1_dwell_time_1   11 -13329.00

dwell_time_concerns_slopes <- emtrends(
  bm2.2_dwell_time_1, ~ IA_LABEL_f, var = "Perf_Concerns_c"
)
summary(dwell_time_concerns_slopes, infer = c(TRUE, TRUE))
# IA_LABEL_f Perf_Concerns_c.trend     SE  df asymp.LCL asymp.UCL z.ratio p.value
#  Neutral                 -0.00556 0.0102 Inf   -0.0255   0.01437  -0.546  0.5848
#  Positive                -0.02307 0.0102 Inf   -0.0430  -0.00317  -2.272  0.0231
#  Negative                -0.01485 0.0101 Inf   -0.0346   0.00494  -1.470  0.1414
#  Success                 -0.00968 0.0101 Inf   -0.0295   0.01015  -0.957  0.3386
#  Failure                 -0.00403 0.0101 Inf   -0.0239   0.01583  -0.398  0.6908

dwell_time_planned_slopes <- contrast(
  dwell_time_concerns_slopes, method = semantic_planned, adjust = "none"
)
dwell_time_planned_slopes
# contrast                           estimate      SE  df z.ratio p.value
#  positive_vs_negative                0.01387 0.00980 Inf   1.416  0.1569
#  Success_Failure                     0.00565 0.00692 Inf   0.816  0.4144
#  performance_irrelevant_vs_relevant  0.04580 0.02690 Inf   1.703  0.0885
#  NonNeutral_vs_Neutral              -0.02941 0.02220 Inf  -1.324  0.1855

# Response time
#--------------

# Data subset for Gamma GLMMs
sub_rt_qu2 <- rt_qu2[c("response_time_1",
                     "subject_id_1", "block_id", "trial_nr_1_c", "guessed_block_rule_1",
                     "Change_category_f", "Perf_Strivings_c",
                     "Perf_Concerns_c")]

gm0_response_time_1 <- glmmTMB(
  response_time_1 ~ 
    trial_nr_1_c +
    #(1 | block_id) +
    (1 | subject_id_1),
  family = Gamma(link = "log"),
  dispformula = ~ guessed_block_rule_1,
  data = na.omit(sub_rt_qu2),
  
)
summary(gm0_response_time_1)
# Family: Gamma  ( log )
# Formula:          response_time_1 ~ trial_nr_1_c + (1 | subject_id_1)
# Dispersion:                       ~guessed_block_rule_1
# Data: na.omit(sub_rt_qu2)
# 
#      AIC      BIC   logLik deviance df.resid 
# 108355.0 108389.1 -54172.5 108345.0     6777 
# 
# Random effects:
# 
# Conditional model:
#  Groups       Name        Variance Std.Dev.
#  subject_id_1 (Intercept) 0.07423  0.2725  
# Number of obs: 6782, groups:  subject_id_1, 92
# 
# Conditional model:
#                Estimate Std. Error z value Pr(>|z|)    
# (Intercept)   7.3718236  0.0289864  254.32   <2e-16 ***
# trial_nr_1_c -0.0051501  0.0002662  -19.35   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Dispersion model:
#                      Estimate Std. Error z value Pr(>|z|)    
# (Intercept)           1.58145    0.03490   45.31  < 2e-16 ***
# guessed_block_rule_1 -0.12085    0.04007   -3.02  0.00256 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

gm1_response_time_1 <- glmmTMB(
  response_time_1 ~ 
    Change_category_f + 
    trial_nr_1_c +
    #(1 | block_id) +
    (1 | subject_id_1),
  family = Gamma(link = "log"),
  dispformula = ~ guessed_block_rule_1,
  data = na.omit(sub_rt_qu2),
  
)
summary(gm1_response_time_1)
# Family: Gamma  ( log )
# Formula:          response_time_1 ~ Change_category_f + trial_nr_1_c + (1 | subject_id_1)
# Dispersion:                       ~guessed_block_rule_1
# Data: na.omit(sub_rt_qu2)
# 
#      AIC      BIC   logLik deviance df.resid 
# 108318.4 108379.8 -54150.2 108300.4     6773 
# 
# Random effects:
# 
# Conditional model:
#  Groups       Name        Variance Std.Dev.
#  subject_id_1 (Intercept) 0.07444  0.2728  
# Number of obs: 6782, groups:  subject_id_1, 92
# 
# Conditional model:
#                             Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                7.4306169  0.0312338  237.90  < 2e-16 ***
# Change_category_fPositive -0.0537359  0.0182645   -2.94  0.00326 ** 
# Change_category_fNegative -0.0817559  0.0183232   -4.46 8.12e-06 ***
# Change_category_fSuccess  -0.0462442  0.0182129   -2.54  0.01111 *  
# Change_category_fFailure  -0.1158550  0.0182021   -6.36 1.95e-10 ***
# trial_nr_1_c              -0.0050723  0.0002669  -19.01  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Dispersion model:
#                      Estimate Std. Error z value Pr(>|z|)    
# (Intercept)           1.58593    0.03489   45.45  < 2e-16 ***
# guessed_block_rule_1 -0.11857    0.04005   -2.96  0.00307 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

lrtest(gm1_response_time_1, gm0_response_time_1)
# Model 1: response_time_1 ~ Change_category_f + trial_nr_1_c + (1 | subject_id_1)
# Model 2: response_time_1 ~ trial_nr_1_c + (1 | subject_id_1)
#   #Df LogLik Df  Chisq Pr(>Chisq)    
# 1   9 -54150                         
# 2   5 -54172 -4 44.585   4.85e-09 ***

AIC(gm1_response_time_1, gm0_response_time_1)
#                     df      AIC
# gm1_response_time_1  9 108318.4
# gm0_response_time_1  5 108355.0

gm2.1_response_time_1 <- glmmTMB(
  response_time_1 ~ 
    Change_category_f * Perf_Strivings_c + 
    trial_nr_1_c +
    #(1 | block_id) +
    (1 | subject_id_1),
  family = Gamma(link = "log"),
  dispformula = ~ guessed_block_rule_1,
  data = na.omit(sub_rt_qu2),
  
)
summary(gm2.1_response_time_1)
# Family: Gamma  ( log )
# Formula:          response_time_1 ~ Change_category_f * Perf_Strivings_c + trial_nr_1_c +  
#     (1 | subject_id_1)
# Dispersion:                       ~guessed_block_rule_1
# Data: na.omit(sub_rt_qu2)
# 
#      AIC      BIC   logLik deviance df.resid 
# 108323.4 108418.9 -54147.7 108295.4     6768 
# 
# Random effects:
# 
# Conditional model:
#  Groups       Name        Variance Std.Dev.
#  subject_id_1 (Intercept) 0.07445  0.2729  
# Number of obs: 6782, groups:  subject_id_1, 92
# 
# Conditional model:
#                                              Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                                 7.4307341  0.0312329  237.91  < 2e-16 ***
# Change_category_fPositive                  -0.0538430  0.0182590   -2.95  0.00319 ** 
# Change_category_fNegative                  -0.0820505  0.0183170   -4.48 7.48e-06 ***
# Change_category_fSuccess                   -0.0464963  0.0182068   -2.55  0.01066 *  
# Change_category_fFailure                   -0.1161353  0.0181945   -6.38 1.74e-10 ***
# Perf_Strivings_c                           -0.0001243  0.0168861   -0.01  0.99413    
# trial_nr_1_c                               -0.0050402  0.0002674  -18.85  < 2e-16 ***
# Change_category_fPositive:Perf_Strivings_c  0.0050151  0.0098425    0.51  0.61038    
# Change_category_fNegative:Perf_Strivings_c -0.0043611  0.0098941   -0.44  0.65937    
# Change_category_fSuccess:Perf_Strivings_c  -0.0113082  0.0098541   -1.15  0.25115    
# Change_category_fFailure:Perf_Strivings_c   0.0081626  0.0098202    0.83  0.40586    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Dispersion model:
#                      Estimate Std. Error z value Pr(>|z|)    
# (Intercept)           1.58825    0.03491   45.50   <2e-16 ***
# guessed_block_rule_1 -0.12069    0.04007   -3.01   0.0026 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

lrtest(gm2.1_response_time_1, gm1_response_time_1)
# Model 1: response_time_1 ~ Change_category_f * Perf_Strivings_c + trial_nr_1_c + 
#     (1 | subject_id_1)
# Model 2: response_time_1 ~ Change_category_f + trial_nr_1_c + (1 | subject_id_1)
#   #Df LogLik Df Chisq Pr(>Chisq)
# 1  14 -54148                    
# 2   9 -54150 -5  4.95      0.422

AIC(gm2.1_response_time_1, gm1_response_time_1)
#                      df      AIC
# gm2.1_response_time_1 14 108323.4
# gm1_response_time_1    9 108318.4

gm2.2_response_time_1 <- glmmTMB(
  response_time_1 ~ 
    Change_category_f * Perf_Concerns_c + 
    trial_nr_1_c +
    #(1 | block_id) +
    (1 | subject_id_1),
  family = Gamma(link = "log"),
  dispformula = ~ guessed_block_rule_1,
  data = na.omit(sub_rt_qu2),
  
)
summary(gm2.2_response_time_1)
# Family: Gamma  ( log )
# Formula:          response_time_1 ~ Change_category_f * Perf_Concerns_c + trial_nr_1_c +  
#     (1 | subject_id_1)
# Dispersion:                       ~guessed_block_rule_1
# Data: na.omit(sub_rt_qu2)
# 
#      AIC      BIC   logLik deviance df.resid 
# 108326.3 108421.8 -54149.1 108298.3     6768 
# 
# Random effects:
# 
# Conditional model:
#  Groups       Name        Variance Std.Dev.
#  subject_id_1 (Intercept) 0.07442  0.2728  
# Number of obs: 6782, groups:  subject_id_1, 92
# 
# Conditional model:
#                                            Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                                7.430646   0.031229  237.94  < 2e-16 ***
# Change_category_fPositive                 -0.053781   0.018262   -2.94  0.00323 ** 
# Change_category_fNegative                 -0.081946   0.018320   -4.47 7.71e-06 ***
# Change_category_fSuccess                  -0.046281   0.018210   -2.54  0.01104 *  
# Change_category_fFailure                  -0.115890   0.018199   -6.37 1.92e-10 ***
# Perf_Concerns_c                            0.005688   0.013075    0.44  0.66352    
# trial_nr_1_c                              -0.005072   0.000267  -19.00  < 2e-16 ***
# Change_category_fPositive:Perf_Concerns_c  0.001066   0.007672    0.14  0.88949    
# Change_category_fNegative:Perf_Concerns_c -0.008628   0.007740   -1.11  0.26498    
# Change_category_fSuccess:Perf_Concerns_c  -0.002946   0.007655   -0.38  0.70037    
# Change_category_fFailure:Perf_Concerns_c  -0.005041   0.007592   -0.66  0.50673    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Dispersion model:
#                      Estimate Std. Error z value Pr(>|z|)    
# (Intercept)           1.58675    0.03491   45.45  < 2e-16 ***
# guessed_block_rule_1 -0.11926    0.04008   -2.98  0.00292 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

lrtest(gm2.2_response_time_1, gm1_response_time_1)
# Model 1: response_time_1 ~ Change_category_f * Perf_Concerns_c + trial_nr_1_c + 
#     (1 | subject_id_1)
# Model 2: response_time_1 ~ Change_category_f + trial_nr_1_c + (1 | subject_id_1)
#   #Df LogLik Df  Chisq Pr(>Chisq)
# 1  14 -54149                     
# 2   9 -54150 -5 2.1064     0.8342

AIC(gm2.2_response_time_1, gm1_response_time_1)
#                       df      AIC
# gm2.2_response_time_1 14 108326.3
# gm1_response_time_1    9 108318.4
