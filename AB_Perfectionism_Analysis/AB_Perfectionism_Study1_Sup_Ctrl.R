# AB Perfectionism - Study 1 supplementary analyses controlling for depressive and anxiety symptoms

source("AB_Perfectionism_Study1_Preparation.R")

# These sensitivity analyses repeat the primary models with depressive and anxiety symptoms included as covariates. Repeated interaction figures and diagnostic plots are omitted because the adjusted models yielded the same substantive conclusions as the primary analyses.

# Data subset for beta GLMMs
sub_et_qu <- et_qu[c("first_fix_prob", "first_fix_latency", "dwell_time",
                     "dwell_time_early", "dwell_time_intermediate", "dwell_time_late",
                     "subject_id", "block_id", "block_nr", "block_nr_c",
                     "guessed_block_rule", "Change_category01", "IA_LABEL_f",
                     "Perf_Strivings_c", "Perf_Concerns_c", "CESD", "GAD7")]

transform01 <- function(x) {
  n <- sum(!is.na(x))
  ifelse(is.na(x), NA_real_, (x * (n - 1) + 0.5) / n)
}

sub_et_qu$first_fix_prob_t01 <- transform01(sub_et_qu$first_fix_prob)
sub_et_qu$first_fix_latency_t01 <- transform01(sub_et_qu$first_fix_latency)
sub_et_qu$dwell_time_early_t01 <- transform01(sub_et_qu$dwell_time_early)
sub_et_qu$dwell_time_intermediate_t01 <- transform01(sub_et_qu$dwell_time_intermediate)
sub_et_qu$dwell_time_late_t01 <- transform01(sub_et_qu$dwell_time_late)
sub_et_qu$dwell_time_t01 <- transform01(sub_et_qu$dwell_time)
sub_et_qu$CESD_c <- sub_et_qu$CESD - mean(sub_et_qu$CESD, na.rm = TRUE)
sub_et_qu$GAD7_c <- sub_et_qu$GAD7 - mean(sub_et_qu$GAD7, na.rm = TRUE)

# Probability of first or second fixation
#----------------------------------------
bm0_first_fix_prob <- glmmTMB(
  first_fix_prob_t01 ~ 
    block_nr_c +
    CESD_c + GAD7_c +
    Change_category01 +
    #(1 | block_id) +  
    (1 + Change_category01 || subject_id),
  family = beta_family(link = "logit"),
  dispformula = ~ guessed_block_rule,
  data = na.omit(sub_et_qu),
  
)
summary(bm0_first_fix_prob)
#  Family: beta  ( logit )
# Formula:          first_fix_prob_t01 ~ block_nr_c + CESD_c + GAD7_c + Change_category01 +  
#     (1 + Change_category01 || subject_id)
# Dispersion:                          ~guessed_block_rule
# Data: na.omit(sub_et_qu)
# 
#      AIC      BIC   logLik deviance df.resid 
#  -3678.3  -3613.4   1848.2  -3696.3    10087 
# 
# Random effects:
# 
# Conditional model:
#  Groups     Name              Variance Std.Dev. Corr 
#  subject_id (Intercept)       0.005933 0.07703       
#             Change_category01 0.040861 0.20214  0.00 
# Number of obs: 10096, groups:  subject_id, 202
# 
# Conditional model:
#                    Estimate Std. Error z value Pr(>|z|)    
# (Intercept)       -0.316607   0.010978 -28.840  < 2e-16 ***
# block_nr_c         0.007360   0.002968   2.480 0.013142 *  
# CESD_c             0.002344   0.001423   1.648 0.099367 .  
# GAD7_c            -0.008856   0.002376  -3.727 0.000193 ***
# Change_category01 -0.177148   0.025740  -6.882 5.89e-12 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Dispersion model:
#                    Estimate Std. Error z value Pr(>|z|)    
# (Intercept)         1.49698    0.03022   49.53   <2e-16 ***
# guessed_block_rule -0.01309    0.03331   -0.39    0.694    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

bm1_first_fix_prob <- glmmTMB(
  first_fix_prob_t01 ~ 
    IA_LABEL_f + 
    block_nr_c +
    CESD_c + GAD7_c +
    Change_category01 +
    #(1 | block_id) +  
    (1 + Change_category01 || subject_id),
  dispformula = ~ guessed_block_rule,
  family = beta_family(link = "logit"),
  data = na.omit(sub_et_qu),
  
)
summary(bm1_first_fix_prob)
# Family: beta  ( logit )
# Formula:          first_fix_prob_t01 ~ IA_LABEL_f + block_nr_c + CESD_c + GAD7_c +  
#     Change_category01 + (1 + Change_category01 || subject_id)
# Dispersion:                          ~guessed_block_rule
# Data: na.omit(sub_et_qu)
# 
#      AIC      BIC   logLik deviance df.resid 
#  -3707.4  -3613.5   1866.7  -3733.4    10083 
# 
# Random effects:
# 
# Conditional model:
#  Groups     Name              Variance Std.Dev. Corr 
#  subject_id (Intercept)       0.005839 0.07642       
#             Change_category01 0.040809 0.20201  0.00 
# Number of obs: 10096, groups:  subject_id, 202
# 
# Conditional model:
#                     Estimate Std. Error z value Pr(>|z|)    
# (Intercept)        -0.307787   0.020238 -15.209  < 2e-16 ***
# IA_LABEL_fPositive  0.071866   0.026891   2.672 0.007530 ** 
# IA_LABEL_fNegative -0.090343   0.026944  -3.353 0.000800 ***
# IA_LABEL_fSuccess  -0.004131   0.026922  -0.153 0.878053    
# IA_LABEL_fFailure  -0.021824   0.026919  -0.811 0.417528    
# block_nr_c          0.007301   0.002964   2.463 0.013776 *  
# CESD_c              0.002339   0.001418   1.649 0.099183 .  
# GAD7_c             -0.008833   0.002369  -3.729 0.000192 ***
# Change_category01  -0.177581   0.025720  -6.904 5.04e-12 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Dispersion model:
#                    Estimate Std. Error z value Pr(>|z|)    
# (Intercept)         1.50483    0.03028   49.70   <2e-16 ***
# guessed_block_rule -0.01824    0.03337   -0.55    0.585    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

lrtest(bm0_first_fix_prob, bm1_first_fix_prob)
# Model 1: first_fix_prob_t01 ~ block_nr_c + CESD_c + GAD7_c + Change_category01 + 
#     (1 + Change_category01 || subject_id)
# Model 2: first_fix_prob_t01 ~ IA_LABEL_f + block_nr_c + CESD_c + GAD7_c + 
#     Change_category01 + (1 + Change_category01 || subject_id)
#   #Df LogLik Df  Chisq Pr(>Chisq)    
# 1   9 1848.2                         
# 2  13 1866.7  4 37.062  1.749e-07 ***

AIC(bm1_first_fix_prob, bm0_first_fix_prob)
#                    df       AIC
# bm1_first_fix_prob 13 -3707.397
# bm0_first_fix_prob  9 -3678.335

bm2.1_first_fix_prob <- glmmTMB(
  first_fix_prob_t01 ~ 
    IA_LABEL_f * Perf_Strivings_c +
    block_nr_c +
    CESD_c + GAD7_c +
    Change_category01 +
    #(1 | block_id)  
    (1 + Change_category01 || subject_id),
  dispformula = ~ guessed_block_rule,
  family = beta_family(link = "logit"),
  data = na.omit(sub_et_qu),
  
)
summary(bm2.1_first_fix_prob)
# Family: beta  ( logit )
# Formula:          first_fix_prob_t01 ~ IA_LABEL_f * Perf_Strivings_c + block_nr_c +  
#     CESD_c + GAD7_c + Change_category01 + (1 + Change_category01 ||      subject_id)
# Dispersion:                          ~guessed_block_rule
# Data: na.omit(sub_et_qu)
# 
#      AIC      BIC   logLik deviance df.resid 
#  -3707.6  -3577.6   1871.8  -3743.6    10078 
# 
# Random effects:
# 
# Conditional model:
#  Groups     Name              Variance Std.Dev. Corr 
#  subject_id (Intercept)       0.005494 0.07412       
#             Change_category01 0.040006 0.20002  0.00 
# Number of obs: 10096, groups:  subject_id, 202
# 
# Conditional model:
#                                      Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                         -0.307849   0.020192 -15.246  < 2e-16 ***
# IA_LABEL_fPositive                   0.071879   0.026885   2.674 0.007504 ** 
# IA_LABEL_fNegative                  -0.090235   0.026944  -3.349 0.000811 ***
# IA_LABEL_fSuccess                   -0.004070   0.026915  -0.151 0.879793    
# IA_LABEL_fFailure                   -0.021810   0.026915  -0.810 0.417746    
# Perf_Strivings_c                    -0.019809   0.011162  -1.775 0.075942 .  
# block_nr_c                           0.007311   0.002964   2.466 0.013649 *  
# CESD_c                               0.002712   0.001425   1.903 0.057051 .  
# GAD7_c                              -0.008553   0.002353  -3.635 0.000277 ***
# Change_category01                   -0.177422   0.025639  -6.920 4.51e-12 ***
# IA_LABEL_fPositive:Perf_Strivings_c  0.020477   0.014911   1.373 0.169656    
# IA_LABEL_fNegative:Perf_Strivings_c -0.004838   0.014937  -0.324 0.746045    
# IA_LABEL_fSuccess:Perf_Strivings_c   0.029507   0.014906   1.980 0.047750 *  
# IA_LABEL_fFailure:Perf_Strivings_c   0.005032   0.014908   0.338 0.735714    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Dispersion model:
#                    Estimate Std. Error z value Pr(>|z|)    
# (Intercept)         1.50539    0.03027   49.73   <2e-16 ***
# guessed_block_rule -0.01828    0.03336   -0.55    0.584    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

lrtest(bm1_first_fix_prob, bm2.1_first_fix_prob)
# Model 1: first_fix_prob_t01 ~ IA_LABEL_f + block_nr_c + CESD_c + GAD7_c + 
#     Change_category01 + (1 + Change_category01 || subject_id)
# Model 2: first_fix_prob_t01 ~ IA_LABEL_f * Perf_Strivings_c + block_nr_c + 
#     CESD_c + GAD7_c + Change_category01 + (1 + Change_category01 || 
#     subject_id)
#   #Df LogLik Df  Chisq Pr(>Chisq)  
# 1  13 1866.7                       
# 2  18 1871.8  5 10.177    0.07036 .

AIC(bm2.1_first_fix_prob, bm1_first_fix_prob)
#                      df       AIC
# bm2.1_first_fix_prob 18 -3707.575
# bm1_first_fix_prob   13 -3707.397

bm2.2_first_fix_prob <- glmmTMB(
  first_fix_prob_t01 ~ 
    IA_LABEL_f * Perf_Concerns_c +
    block_nr_c +
    CESD_c + GAD7_c +
    Change_category01 +
    #(1 | block_id) +  
    (1 + Change_category01 || subject_id),
  dispformula = ~ guessed_block_rule,
  family = beta_family(link = "logit"),
  data = na.omit(sub_et_qu),
  
)
summary(bm2.2_first_fix_prob)
# Family: beta  ( logit )
# Formula:          first_fix_prob_t01 ~ IA_LABEL_f * Perf_Concerns_c + block_nr_c +  
#     CESD_c + GAD7_c + Change_category01 + (1 + Change_category01 ||      subject_id)
# Dispersion:                          ~guessed_block_rule
# Data: na.omit(sub_et_qu)
# 
#      AIC      BIC   logLik deviance df.resid 
#  -3705.7  -3575.8   1870.9  -3741.7    10078 
# 
# Random effects:
# 
# Conditional model:
#  Groups     Name              Variance Std.Dev. Corr 
#  subject_id (Intercept)       0.005854 0.07651       
#             Change_category01 0.040987 0.20245  0.00 
# Number of obs: 10096, groups:  subject_id, 202
# 
# Conditional model:
#                                     Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                        -0.307932   0.020235 -15.217  < 2e-16 ***
# IA_LABEL_fPositive                  0.072025   0.026885   2.679 0.007383 ** 
# IA_LABEL_fNegative                 -0.090270   0.026937  -3.351 0.000805 ***
# IA_LABEL_fSuccess                  -0.003983   0.026913  -0.148 0.882358    
# IA_LABEL_fFailure                  -0.021783   0.026914  -0.809 0.418309    
# Perf_Concerns_c                    -0.008685   0.008913  -0.974 0.329884    
# block_nr_c                          0.007321   0.002964   2.470 0.013496 *  
# CESD_c                              0.002438   0.001511   1.614 0.106576    
# GAD7_c                             -0.008794   0.002381  -3.694 0.000221 ***
# Change_category01                  -0.177736   0.025734  -6.907 4.96e-12 ***
# IA_LABEL_fPositive:Perf_Concerns_c  0.004958   0.011418   0.434 0.664111    
# IA_LABEL_fNegative:Perf_Concerns_c  0.008244   0.011436   0.721 0.470984    
# IA_LABEL_fSuccess:Perf_Concerns_c   0.027511   0.011471   2.398 0.016469 *  
# IA_LABEL_fFailure:Perf_Concerns_c  -0.001643   0.011436  -0.144 0.885755    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Dispersion model:
#                    Estimate Std. Error z value Pr(>|z|)    
# (Intercept)         1.50829    0.03031   49.76   <2e-16 ***
# guessed_block_rule -0.02137    0.03341   -0.64    0.522    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

lrtest(bm1_first_fix_prob, bm2.2_first_fix_prob)
# Model 1: first_fix_prob_t01 ~ IA_LABEL_f + block_nr_c + CESD_c + GAD7_c + 
#     Change_category01 + (1 + Change_category01 || subject_id)
# Model 2: first_fix_prob_t01 ~ IA_LABEL_f * Perf_Concerns_c + block_nr_c + 
#     CESD_c + GAD7_c + Change_category01 + (1 + Change_category01 || 
#     subject_id)
#   #Df LogLik Df  Chisq Pr(>Chisq)
# 1  13 1866.7                     
# 2  18 1870.9  5 8.3204     0.1394

AIC(bm2.2_first_fix_prob, bm1_first_fix_prob)
#                      df       AIC
# bm2.2_first_fix_prob 18 -3705.718
# bm1_first_fix_prob   13 -3707.397

first_fix_prob_strivings_slopes <- emtrends(
  bm2.1_first_fix_prob, ~ IA_LABEL_f, var = "Perf_Strivings_c"
)
summary(first_fix_prob_strivings_slopes, infer = c(TRUE, TRUE))
# IA_LABEL_f Perf_Strivings_c.trend     SE  df asymp.LCL asymp.UCL z.ratio p.value
#  Neutral                 -0.019809 0.0112 Inf   -0.0417   0.00207  -1.775  0.0759
#  Positive                 0.000668 0.0111 Inf   -0.0211   0.02248   0.060  0.9521
#  Negative                -0.024647 0.0112 Inf   -0.0465  -0.00277  -2.208  0.0272
#  Success                  0.009698 0.0111 Inf   -0.0121   0.03147   0.873  0.3827
#  Failure                 -0.014777 0.0111 Inf   -0.0366   0.00703  -1.328  0.1842

first_fix_prob_planned_slopes <- contrast(
  first_fix_prob_strivings_slopes, method = semantic_planned, adjust = "none"
)
first_fix_prob_planned_slopes
# contrast                           estimate     SE  df z.ratio p.value
#  positive_vs_negative                -0.0498 0.0211 Inf  -2.364  0.0181
#  Success_Failure                     -0.0245 0.0149 Inf  -1.645  0.0999
#  performance_irrelevant_vs_relevant   0.0723 0.0577 Inf   1.254  0.2097
#  NonNeutral_vs_Neutral                0.0502 0.0472 Inf   1.063  0.2879
 
bm0_first_fix_latency <- glmmTMB(
  first_fix_latency_t01 ~ 
    block_nr_c +
    CESD_c + GAD7_c +
    Change_category01 +
    #(1 | block_id) +  
    (1 + Change_category01 || subject_id), 
  dispformula = ~ guessed_block_rule,
  family = beta_family(link = "logit"),
  data = na.omit(sub_et_qu),
  
)
summary(bm0_first_fix_latency)
# Family: beta  ( logit )
# Formula:          first_fix_latency_t01 ~ block_nr_c + CESD_c + GAD7_c + Change_category01 +  
#     (1 + Change_category01 || subject_id)
# Dispersion:                             ~guessed_block_rule
# Data: na.omit(sub_et_qu)
# 
#      AIC      BIC   logLik deviance df.resid 
# -46601.8 -46536.8  23309.9 -46619.8    10087 
# 
# Random effects:
# 
# Conditional model:
#  Groups     Name              Variance  Std.Dev.  Corr 
#  subject_id (Intercept)       2.532e-13 5.032e-07      
#             Change_category01 2.532e-10 1.591e-05 0.00 
# Number of obs: 10096, groups:  subject_id, 202
# 
# Conditional model:
#                     Estimate Std. Error z value Pr(>|z|)    
# (Intercept)       -1.391e+00  1.678e-03  -828.8  < 2e-16 ***
# block_nr_c        -2.908e-05  5.205e-04    -0.1    0.955    
# CESD_c            -2.482e-05  2.051e-04    -0.1    0.904    
# GAD7_c             9.698e-05  3.429e-04     0.3    0.777    
# Change_category01  2.601e-02  3.723e-03     7.0 2.84e-12 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Dispersion model:
#                    Estimate Std. Error z value Pr(>|z|)    
# (Intercept)         5.57291    0.03279  169.97   <2e-16 ***
# guessed_block_rule  0.05049    0.03627    1.39    0.164    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

bm1_first_fix_latency <- glmmTMB(
  first_fix_latency_t01 ~ 
    IA_LABEL_f +
    block_nr_c +
    CESD_c + GAD7_c +
    Change_category01 +
    #(1 | block_id) +  
    (1 + Change_category01 || subject_id), 
  dispformula = ~ guessed_block_rule,
  family = beta_family(link = "logit"),
  data = na.omit(sub_et_qu)
)
summary(bm1_first_fix_latency)
# Family: beta  ( logit )
# Formula:          first_fix_latency_t01 ~ IA_LABEL_f + block_nr_c + CESD_c + GAD7_c +  
#     Change_category01 + (1 + Change_category01 || subject_id)
# Dispersion:                             ~guessed_block_rule
# Data: na.omit(sub_et_qu)
# 
#      AIC      BIC   logLik deviance df.resid 
# -46619.7 -46525.9  23322.9 -46645.7    10083 
# 
# Random effects:
# 
# Conditional model:
#  Groups     Name              Variance  Std.Dev.  Corr 
#  subject_id (Intercept)       2.733e-12 1.653e-06      
#             Change_category01 2.963e-10 1.721e-05 0.00 
# Number of obs: 10096, groups:  subject_id, 202
# 
# Conditional model:
#                      Estimate Std. Error z value Pr(>|z|)    
# (Intercept)        -1.393e+00  3.429e-03  -406.2  < 2e-16 ***
# IA_LABEL_fPositive -9.339e-03  4.735e-03    -2.0   0.0486 *  
# IA_LABEL_fNegative  1.153e-02  4.720e-03     2.4   0.0146 *  
# IA_LABEL_fSuccess  -1.495e-03  4.729e-03    -0.3   0.7519    
# IA_LABEL_fFailure   9.367e-03  4.722e-03     2.0   0.0473 *  
# block_nr_c         -2.783e-05  5.199e-04    -0.1   0.9573    
# CESD_c             -2.439e-05  2.048e-04    -0.1   0.9052    
# GAD7_c              9.686e-05  3.425e-04     0.3   0.7773    
# Change_category01   2.599e-02  3.718e-03     7.0 2.74e-12 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Dispersion model:
#                    Estimate Std. Error z value Pr(>|z|)    
# (Intercept)         5.57891    0.03280  170.10   <2e-16 ***
# guessed_block_rule  0.04629    0.03628    1.28    0.202    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

lrtest(bm0_first_fix_latency, bm1_first_fix_latency)
# Model 1: first_fix_latency_t01 ~ block_nr_c + CESD_c + GAD7_c + Change_category01 + 
#     (1 + Change_category01 || subject_id)
# Model 2: first_fix_latency_t01 ~ IA_LABEL_f + block_nr_c + CESD_c + GAD7_c + 
#     Change_category01 + (1 + Change_category01 || subject_id)
#   #Df LogLik Df  Chisq Pr(>Chisq)    
# 1   9  23310                         
# 2  13  23323  4 25.921  3.282e-05 ***

AIC(bm1_first_fix_latency, bm0_first_fix_latency)
#                       df       AIC
# bm1_first_fix_latency 13 -46619.74
# bm0_first_fix_latency  9 -46601.81


bm2.1_first_fix_latency <- glmmTMB(
  first_fix_latency_t01 ~ 
    IA_LABEL_f * Perf_Strivings_c +
    block_nr_c +
    CESD_c + GAD7_c +
    Change_category01 +
    #(1 | block_id) +  
    (1 + Change_category01 || subject_id), 
  dispformula = ~ guessed_block_rule,
  family = beta_family(link = "logit"),
  data = na.omit(sub_et_qu),
  
)
summary(bm2.1_first_fix_latency)
# Family: beta  ( logit )
# Formula:          first_fix_latency_t01 ~ IA_LABEL_f * Perf_Strivings_c + block_nr_c +  
#     CESD_c + GAD7_c + Change_category01 + (1 + Change_category01 ||      subject_id)
# Dispersion:                             ~guessed_block_rule
# Data: na.omit(sub_et_qu)
# 
#      AIC      BIC   logLik deviance df.resid 
# -46611.9 -46481.9  23323.9 -46647.9    10078 
# 
# Random effects:
# 
# Conditional model:
#  Groups     Name              Variance  Std.Dev.  Corr 
#  subject_id (Intercept)       2.584e-12 1.607e-06      
#             Change_category01 1.556e-10 1.247e-05 0.00 
# Number of obs: 10096, groups:  subject_id, 202
# 
# Conditional model:
#                                       Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                         -1.393e+00  3.429e-03  -406.2  < 2e-16 ***
# IA_LABEL_fPositive                  -9.336e-03  4.735e-03    -2.0   0.0486 *  
# IA_LABEL_fNegative                   1.153e-02  4.719e-03     2.4   0.0145 *  
# IA_LABEL_fSuccess                   -1.493e-03  4.729e-03    -0.3   0.7522    
# IA_LABEL_fFailure                    9.371e-03  4.722e-03     2.0   0.0472 *  
# Perf_Strivings_c                    -1.359e-03  1.868e-03    -0.7   0.4670    
# block_nr_c                          -2.627e-05  5.199e-04    -0.1   0.9597    
# CESD_c                              -2.904e-05  2.076e-04    -0.1   0.8887    
# GAD7_c                               9.289e-05  3.432e-04     0.3   0.7867    
# Change_category01                    2.599e-02  3.718e-03     7.0 2.73e-12 ***
# IA_LABEL_fPositive:Perf_Strivings_c  3.163e-03  2.623e-03     1.2   0.2278    
# IA_LABEL_fNegative:Perf_Strivings_c  2.559e-04  2.615e-03     0.1   0.9220    
# IA_LABEL_fSuccess:Perf_Strivings_c   1.703e-03  2.619e-03     0.7   0.5156    
# IA_LABEL_fFailure:Perf_Strivings_c   2.322e-03  2.615e-03     0.9   0.3745    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

lrtest(bm1_first_fix_latency, bm2.1_first_fix_latency)
# Model 1: first_fix_latency_t01 ~ IA_LABEL_f + block_nr_c + CESD_c + GAD7_c + 
#     Change_category01 + (1 + Change_category01 || subject_id)
# Model 2: first_fix_latency_t01 ~ IA_LABEL_f * Perf_Strivings_c + block_nr_c + 
#     CESD_c + GAD7_c + Change_category01 + (1 + Change_category01 || 
#     subject_id)
#   #Df LogLik Df  Chisq Pr(>Chisq)
# 1  13  23323                     
# 2  18  23324  5 2.1425     0.8291

AIC(bm2.1_first_fix_latency, bm1_first_fix_latency)
#                         df       AIC
# bm2.1_first_fix_latency 18 -46611.88
# bm1_first_fix_latency   13 -46619.74

bm2.2_first_fix_latency <- glmmTMB(
  first_fix_latency_t01 ~ 
    IA_LABEL_f * Perf_Concerns_c +
    block_nr_c +
    CESD_c + GAD7_c +
    Change_category01 +
    #(1 | block_id) +  
    (1 + Change_category01 || subject_id), 
  dispformula = ~ guessed_block_rule,
  family = beta_family(link = "logit"),
  data = na.omit(sub_et_qu),
  
)
summary(bm2.2_first_fix_latency)
# Family: beta  ( logit )
# Formula:          first_fix_latency_t01 ~ IA_LABEL_f * Perf_Concerns_c + block_nr_c +  
#     CESD_c + GAD7_c + Change_category01 + (1 + Change_category01 ||      subject_id)
# Dispersion:                             ~guessed_block_rule
# Data: na.omit(sub_et_qu)
# 
#      AIC      BIC   logLik deviance df.resid 
# -46611.5 -46481.5  23323.8 -46647.5    10078 
# 
# Random effects:
# 
# Conditional model:
#  Groups     Name              Variance  Std.Dev.  Corr 
#  subject_id (Intercept)       6.653e-12 2.579e-06      
#             Change_category01 1.453e-10 1.205e-05 0.00 
# Number of obs: 10096, groups:  subject_id, 202
# 
# Conditional model:
#                                      Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                        -1.393e+00  3.429e-03  -406.2  < 2e-16 ***
# IA_LABEL_fPositive                 -9.338e-03  4.735e-03    -2.0   0.0486 *  
# IA_LABEL_fNegative                  1.153e-02  4.719e-03     2.4   0.0145 *  
# IA_LABEL_fSuccess                  -1.494e-03  4.729e-03    -0.3   0.7520    
# IA_LABEL_fFailure                   9.368e-03  4.722e-03     2.0   0.0473 *  
# Perf_Concerns_c                     6.510e-05  1.476e-03     0.0   0.9648    
# block_nr_c                         -2.694e-05  5.199e-04    -0.1   0.9587    
# CESD_c                             -1.911e-05  2.182e-04    -0.1   0.9302    
# GAD7_c                              9.902e-05  3.442e-04     0.3   0.7736    
# Change_category01                   2.599e-02  3.718e-03     7.0 2.75e-12 ***
# IA_LABEL_fPositive:Perf_Concerns_c  1.171e-03  2.012e-03     0.6   0.5606    
# IA_LABEL_fNegative:Perf_Concerns_c -1.202e-03  2.007e-03    -0.6   0.5493    
# IA_LABEL_fSuccess:Perf_Concerns_c  -8.474e-04  2.010e-03    -0.4   0.6733    
# IA_LABEL_fFailure:Perf_Concerns_c   3.051e-04  2.006e-03     0.2   0.8791    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Dispersion model:
#                    Estimate Std. Error z value Pr(>|z|)    
# (Intercept)         5.57896    0.03281  170.06   <2e-16 ***
# guessed_block_rule  0.04645    0.03629    1.28    0.201    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

lrtest(bm1_first_fix_latency, bm2.2_first_fix_latency)
# Model 1: first_fix_latency_t01 ~ IA_LABEL_f + block_nr_c + CESD_c + GAD7_c + 
#     Change_category01 + (1 + Change_category01 || subject_id)
# Model 2: first_fix_latency_t01 ~ IA_LABEL_f * Perf_Concerns_c + block_nr_c + 
#     CESD_c + GAD7_c + Change_category01 + (1 + Change_category01 || 
#     subject_id)
#   #Df LogLik Df  Chisq Pr(>Chisq)
# 1  13  23323                     
# 2  18  23324  5 1.7668     0.8804

AIC(bm2.2_first_fix_latency, bm1_first_fix_latency)
#                         df       AIC
# bm2.2_first_fix_latency 18 -46611.50
# bm1_first_fix_latency   13 -46619.74

# Early dwell time 
#-----------------
bm0_dwell_time_early <- glmmTMB(
  dwell_time_early_t01 ~ 
    block_nr_c +
    CESD_c + GAD7_c +
    Change_category01 +
    #(1 | block_id) +  
    (1 + Change_category01 || subject_id),
  dispformula = ~ guessed_block_rule,
  family = beta_family(link = "logit"),
  data = na.omit(sub_et_qu),
  
)
summary(bm0_dwell_time_early)
# Family: beta  ( logit )
# Formula:          dwell_time_early_t01 ~ block_nr_c + CESD_c + GAD7_c + Change_category01 +  
#     (1 + Change_category01 || subject_id)
# Dispersion:                            ~guessed_block_rule
# Data: na.omit(sub_et_qu)
# 
#      AIC      BIC   logLik deviance df.resid 
# -17322.1 -17257.1   8670.0 -17340.1    10087 
# 
# Random effects:
# 
# Conditional model:
#  Groups     Name              Variance Std.Dev. Corr 
#  subject_id (Intercept)       0.01616  0.1271        
#             Change_category01 0.02887  0.1699   0.00 
# Number of obs: 10096, groups:  subject_id, 202
# 
# Conditional model:
#                    Estimate Std. Error z value Pr(>|z|)    
# (Intercept)       -1.414110   0.011810 -119.74  < 2e-16 ***
# block_nr_c        -0.002355   0.002302   -1.02  0.30621    
# CESD_c             0.002675   0.001551    1.72  0.08455 .  
# GAD7_c            -0.007436   0.002595   -2.87  0.00417 ** 
# Change_category01  0.015246   0.020383    0.75  0.45446    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Dispersion model:
#                    Estimate Std. Error z value Pr(>|z|)    
# (Intercept)         2.50830    0.03204   78.28   <2e-16 ***
# guessed_block_rule -0.03056    0.03511   -0.87    0.384    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

bm1_dwell_time_early <- glmmTMB(
  dwell_time_early_t01 ~ 
    IA_LABEL_f +
    block_nr_c +
    CESD_c + GAD7_c +
    Change_category01 +
    #(1 | block_id) +  
    (1 + Change_category01 || subject_id),
  dispformula = ~ guessed_block_rule,
  family = beta_family(link = "logit"),
  data = na.omit(sub_et_qu),
  
)
summary(bm1_dwell_time_early)
# Family: beta  ( logit )
# Formula:          dwell_time_early_t01 ~ IA_LABEL_f + block_nr_c + CESD_c + GAD7_c +  
#     Change_category01 + (1 + Change_category01 || subject_id)
# Dispersion:                            ~guessed_block_rule
# Data: na.omit(sub_et_qu)
# 
#      AIC      BIC   logLik deviance df.resid 
# -17325.3 -17231.4   8675.6 -17351.3    10083 
# 
# Random effects:
# 
# Conditional model:
#  Groups     Name              Variance Std.Dev. Corr 
#  subject_id (Intercept)       0.01605  0.1267        
#             Change_category01 0.02903  0.1704   0.00 
# Number of obs: 10096, groups:  subject_id, 202
# 
# Conditional model:
#                     Estimate Std. Error z value Pr(>|z|)    
# (Intercept)        -1.403348   0.017674  -79.40   <2e-16 ***
# IA_LABEL_fPositive  0.006866   0.020903    0.33   0.7425    
# IA_LABEL_fNegative -0.047446   0.020962   -2.26   0.0236 *  
# IA_LABEL_fSuccess  -0.024501   0.020920   -1.17   0.2415    
# IA_LABEL_fFailure   0.011443   0.020847    0.55   0.5831    
# block_nr_c         -0.002405   0.002301   -1.05   0.2960    
# CESD_c              0.002688   0.001547    1.74   0.0823 .  
# GAD7_c             -0.007454   0.002589   -2.88   0.0040 ** 
# Change_category01   0.014568   0.020402    0.71   0.4752    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Dispersion model:
#                    Estimate Std. Error z value Pr(>|z|)    
# (Intercept)         2.51242    0.03210   78.26   <2e-16 ***
# guessed_block_rule -0.03427    0.03519   -0.97     0.33    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

lrtest(bm1_dwell_time_early, bm0_dwell_time_early)
# Model 1: dwell_time_early_t01 ~ IA_LABEL_f + block_nr_c + CESD_c + GAD7_c + 
#     Change_category01 + (1 + Change_category01 || subject_id)
# Model 2: dwell_time_early_t01 ~ block_nr_c + CESD_c + GAD7_c + Change_category01 + 
#     (1 + Change_category01 || subject_id)
#   #Df LogLik Df  Chisq Pr(>Chisq)  
# 1  13 8675.6                       
# 2   9 8670.0 -4 11.203    0.02437 *

AIC(bm1_dwell_time_early, bm0_dwell_time_early)
#                      df       AIC
# bm1_dwell_time_early 13 -17325.29
# bm0_dwell_time_early  9 -17322.09

bm2.1_dwell_time_early <- glmmTMB(
  dwell_time_early_t01 ~ 
    IA_LABEL_f * Perf_Strivings_c +
    block_nr_c +
    CESD_c + GAD7_c +
    Change_category01 +
    #(1 | block_id) +  
    (1 + Change_category01 || subject_id),
  dispformula = ~ guessed_block_rule,
  family = beta_family(link = "logit"),
  data = na.omit(sub_et_qu),
  
)
summary(bm2.1_dwell_time_early)
# Family: beta  ( logit )
# Formula:          dwell_time_early_t01 ~ IA_LABEL_f * Perf_Strivings_c + block_nr_c +  
#     CESD_c + GAD7_c + Change_category01 + (1 + Change_category01 ||      subject_id)
# Dispersion:                            ~guessed_block_rule
# Data: na.omit(sub_et_qu)
# 
#      AIC      BIC   logLik deviance df.resid 
# -17329.6 -17199.6   8682.8 -17365.6    10078 
# 
# Random effects:
# 
# Conditional model:
#  Groups     Name              Variance Std.Dev. Corr 
#  subject_id (Intercept)       0.01607  0.1268        
#             Change_category01 0.02902  0.1703   0.00 
# Number of obs: 10096, groups:  subject_id, 202
# 
# Conditional model:
#                                      Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                         -1.403314   0.017669  -79.42  < 2e-16 ***
# IA_LABEL_fPositive                   0.006680   0.020892    0.32  0.74915    
# IA_LABEL_fNegative                  -0.047511   0.020951   -2.27  0.02334 *  
# IA_LABEL_fSuccess                   -0.024608   0.020909   -1.18  0.23924    
# IA_LABEL_fFailure                    0.011297   0.020836    0.54  0.58770    
# Perf_Strivings_c                     0.015094   0.009805    1.54  0.12370    
# block_nr_c                          -0.002276   0.002300   -0.99  0.32252    
# CESD_c                               0.002748   0.001568    1.75  0.07971 .  
# GAD7_c                              -0.007330   0.002596   -2.82  0.00475 ** 
# Change_category01                    0.014061   0.020395    0.69  0.49054    
# IA_LABEL_fPositive:Perf_Strivings_c -0.029870   0.011572   -2.58  0.00985 ** 
# IA_LABEL_fNegative:Perf_Strivings_c -0.006312   0.011584   -0.54  0.58581    
# IA_LABEL_fSuccess:Perf_Strivings_c  -0.036147   0.011550   -3.13  0.00175 ** 
# IA_LABEL_fFailure:Perf_Strivings_c  -0.013379   0.011573   -1.16  0.24767    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Dispersion model:
#                    Estimate Std. Error z value Pr(>|z|)    
# (Intercept)         2.51402    0.03214   78.23   <2e-16 ***
# guessed_block_rule -0.03441    0.03523   -0.98    0.329    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

lrtest(bm2.1_dwell_time_early, bm1_dwell_time_early)
# Model 1: dwell_time_early_t01 ~ IA_LABEL_f * Perf_Strivings_c + block_nr_c + 
#     CESD_c + GAD7_c + Change_category01 + (1 + Change_category01 || 
#     subject_id)
# Model 2: dwell_time_early_t01 ~ IA_LABEL_f + block_nr_c + CESD_c + GAD7_c + 
#     Change_category01 + (1 + Change_category01 || subject_id)
#   #Df LogLik Df  Chisq Pr(>Chisq)  
# 1  18 8682.8                       
# 2  13 8675.6 -5 14.284     0.0139 *

AIC(bm2.1_dwell_time_early, bm1_dwell_time_early)
#                        df       AIC
# bm2.1_dwell_time_early 18 -17329.57
# bm1_dwell_time_early   13 -17325.29


early_dwell_strivings_slopes <- emtrends(
  bm2.1_dwell_time_early, ~ IA_LABEL_f, var = "Perf_Strivings_c"
)
summary(early_dwell_strivings_slopes, infer = c(TRUE, TRUE))
# IA_LABEL_f Perf_Strivings_c.trend      SE  df asymp.LCL asymp.UCL z.ratio p.value
#  Neutral                   0.01509 0.00980 Inf  -0.00412   0.03431   1.539  0.1237
#  Positive                 -0.01478 0.00981 Inf  -0.03400   0.00445  -1.506  0.1319
#  Negative                  0.00878 0.00981 Inf  -0.01045   0.02801   0.895  0.3707
#  Success                  -0.02105 0.00979 Inf  -0.04023  -0.00187  -2.151  0.0314
#  Failure                   0.00172 0.00981 Inf  -0.01751   0.02094   0.175  0.8612

early_dwell_planned_slopes <- contrast(
  early_dwell_strivings_slopes, method = semantic_planned, adjust = "none"
)
early_dwell_planned_slopes
# contrast                           estimate     SE  df z.ratio p.value
#  positive_vs_negative                 0.0463 0.0164 Inf   2.833  0.0046
#  Success_Failure                      0.0228 0.0116 Inf   1.971  0.0487
#  performance_irrelevant_vs_relevant  -0.0762 0.0448 Inf  -1.703  0.0886
#  NonNeutral_vs_Neutral               -0.0857 0.0366 Inf  -2.342  0.0192

bm2.2_dwell_time_early <- glmmTMB(
  dwell_time_early_t01 ~ 
    IA_LABEL_f * Perf_Concerns_c +
    block_nr_c +
    CESD_c + GAD7_c +
    Change_category01 +
    #(1 | block_id) +  
    (1 + Change_category01 || subject_id),
  dispformula = ~ guessed_block_rule,
  family = beta_family(link = "logit"),
  data = na.omit(sub_et_qu),
  
)
summary(bm2.2_dwell_time_early)
# Family: beta  ( logit )
# Formula:          dwell_time_early_t01 ~ IA_LABEL_f * Perf_Concerns_c + block_nr_c +  
#     CESD_c + GAD7_c + Change_category01 + (1 + Change_category01 ||      subject_id)
# Dispersion:                            ~guessed_block_rule
# Data: na.omit(sub_et_qu)
# 
#      AIC      BIC   logLik deviance df.resid 
# -17319.8 -17189.8   8677.9 -17355.8    10078 
# 
# Random effects:
# 
# Conditional model:
#  Groups     Name              Variance Std.Dev. Corr 
#  subject_id (Intercept)       0.01596  0.1263        
#             Change_category01 0.02901  0.1703   0.00 
# Number of obs: 10096, groups:  subject_id, 202
# 
# Conditional model:
#                                     Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                        -1.403255   0.017659  -79.47  < 2e-16 ***
# IA_LABEL_fPositive                  0.006752   0.020900    0.32  0.74665    
# IA_LABEL_fNegative                 -0.047595   0.020960   -2.27  0.02316 *  
# IA_LABEL_fSuccess                  -0.024575   0.020918   -1.17  0.24006    
# IA_LABEL_fFailure                   0.011277   0.020845    0.54  0.58849    
# Perf_Concerns_c                     0.007518   0.007955    0.94  0.34466    
# block_nr_c                         -0.002374   0.002301   -1.03  0.30214    
# CESD_c                              0.002776   0.001645    1.69  0.09138 .  
# GAD7_c                             -0.007388   0.002598   -2.84  0.00446 ** 
# Change_category01                   0.014528   0.020398    0.71  0.47633    
# IA_LABEL_fPositive:Perf_Concerns_c -0.005158   0.008880   -0.58  0.56132    
# IA_LABEL_fNegative:Perf_Concerns_c -0.008678   0.008906   -0.97  0.32986    
# IA_LABEL_fSuccess:Perf_Concerns_c  -0.017951   0.008909   -2.01  0.04392 *  
# IA_LABEL_fFailure:Perf_Concerns_c  -0.010575   0.008852   -1.19  0.23223    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Dispersion model:
#                    Estimate Std. Error z value Pr(>|z|)    
# (Intercept)         2.51153    0.03211   78.21   <2e-16 ***
# guessed_block_rule -0.03276    0.03520   -0.93    0.352    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

lrtest(bm2.2_dwell_time_early, bm1_dwell_time_early)
# Model 1: dwell_time_early_t01 ~ IA_LABEL_f * Perf_Concerns_c + block_nr_c + 
#     CESD_c + GAD7_c + Change_category01 + (1 + Change_category01 || 
#     subject_id)
# Model 2: dwell_time_early_t01 ~ IA_LABEL_f + block_nr_c + CESD_c + GAD7_c + 
#     Change_category01 + (1 + Change_category01 || subject_id)
#   #Df LogLik Df  Chisq Pr(>Chisq)
# 1  18 8677.9                     
# 2  13 8675.6 -5 4.4882     0.4815

AIC(bm2.2_dwell_time_early, bm1_dwell_time_early)
#                        df       AIC
# bm2.2_dwell_time_early 18 -17319.78
# bm1_dwell_time_early   13 -17325.29

bm0_dwell_time_intermediate <- glmmTMB(
  dwell_time_intermediate_t01 ~ 
    block_nr_c +
    CESD_c + GAD7_c +
    Change_category01 +
    #(1 | block_id) +  
    (1 + Change_category01 || subject_id),
  dispformula = ~ guessed_block_rule,
  family = beta_family(link = "logit"),
  data = na.omit(sub_et_qu),
  
)
summary(bm0_dwell_time_intermediate)
# Family: beta  ( logit )
# Formula:          dwell_time_intermediate_t01 ~ block_nr_c + CESD_c + GAD7_c +  
#     Change_category01 + (1 + Change_category01 || subject_id)
# Dispersion:                                   ~guessed_block_rule
# Data: na.omit(sub_et_qu)
# 
#      AIC      BIC   logLik deviance df.resid 
# -25603.1 -25538.1  12810.5 -25621.1    10087 
# 
# Random effects:
# 
# Conditional model:
#  Groups     Name              Variance Std.Dev. Corr 
#  subject_id (Intercept)       0.009937 0.09969       
#             Change_category01 0.174978 0.41830  0.00 
# Number of obs: 10096, groups:  subject_id, 202
# 
# Conditional model:
#                    Estimate Std. Error z value Pr(>|z|)    
# (Intercept)       -1.448537   0.008496 -170.49  < 2e-16 ***
# block_nr_c        -0.002054   0.001452   -1.41   0.1573    
# CESD_c            -0.001304   0.001156   -1.13   0.2595    
# GAD7_c             0.003767   0.001933    1.95   0.0513 .  
# Change_category01  0.249445   0.031131    8.01 1.12e-15 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Dispersion model:
#                    Estimate Std. Error z value Pr(>|z|)    
# (Intercept)        3.515125   0.033000  106.52   <2e-16 ***
# guessed_block_rule 0.007664   0.036687    0.21    0.835    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

bm1_dwell_time_intermediate <- glmmTMB(
  dwell_time_intermediate_t01 ~ 
    IA_LABEL_f +
    block_nr_c +
    CESD_c + GAD7_c +
    Change_category01 +
    #(1 | block_id) +  
    (1 + Change_category01 || subject_id),
  dispformula = ~ guessed_block_rule,
  family = beta_family(link = "logit"),
  data = na.omit(sub_et_qu),
  
)
summary(bm1_dwell_time_intermediate)
# Family: beta  ( logit )
# Formula:          dwell_time_intermediate_t01 ~ IA_LABEL_f + block_nr_c + CESD_c +  
#     GAD7_c + Change_category01 + (1 + Change_category01 || subject_id)
# Dispersion:                                   ~guessed_block_rule
# Data: na.omit(sub_et_qu)
# 
#      AIC      BIC   logLik deviance df.resid 
# -25620.3 -25526.5  12823.2 -25646.3    10083 
# 
# Random effects:
# 
# Conditional model:
#  Groups     Name              Variance Std.Dev. Corr 
#  subject_id (Intercept)       0.009946 0.09973       
#             Change_category01 0.175072 0.41842  0.00 
# Number of obs: 10096, groups:  subject_id, 202
# 
# Conditional model:
#                     Estimate Std. Error z value Pr(>|z|)    
# (Intercept)        -1.476211   0.011957 -123.46  < 2e-16 ***
# IA_LABEL_fPositive  0.047589   0.013185    3.61 0.000307 ***
# IA_LABEL_fNegative  0.045675   0.013171    3.47 0.000525 ***
# IA_LABEL_fSuccess   0.040068   0.013189    3.04 0.002382 ** 
# IA_LABEL_fFailure   0.004116   0.013256    0.31 0.756196    
# block_nr_c         -0.002049   0.001450   -1.41 0.157673    
# CESD_c             -0.001306   0.001156   -1.13 0.258905    
# GAD7_c              0.003779   0.001932    1.96 0.050521 .  
# Change_category01   0.249609   0.031134    8.02 1.08e-15 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Dispersion model:
#                    Estimate Std. Error z value Pr(>|z|)    
# (Intercept)        3.518351   0.032998  106.62   <2e-16 ***
# guessed_block_rule 0.006946   0.036685    0.19     0.85    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

lrtest(bm1_dwell_time_intermediate, bm0_dwell_time_intermediate)
# Model 1: dwell_time_intermediate_t01 ~ IA_LABEL_f + block_nr_c + CESD_c + 
#     GAD7_c + Change_category01 + (1 + Change_category01 || subject_id)
# Model 2: dwell_time_intermediate_t01 ~ block_nr_c + CESD_c + GAD7_c + 
#     Change_category01 + (1 + Change_category01 || subject_id)
#   #Df LogLik Df  Chisq Pr(>Chisq)    
# 1  13  12823                         
# 2   9  12810 -4 25.265  4.451e-05 ***

AIC(bm1_dwell_time_intermediate, bm0_dwell_time_intermediate)
#                             df       AIC
# bm1_dwell_time_intermediate 13 -25620.32
# bm0_dwell_time_intermediate  9 -25603.06

bm2.1_dwell_time_intermediate <- glmmTMB(
  dwell_time_intermediate_t01 ~ 
    IA_LABEL_f * Perf_Strivings_c +
    block_nr_c +
    CESD_c + GAD7_c +
    Change_category01 +
    #(1 | block_id) +  
    (1 + Change_category01 || subject_id),
  dispformula = ~ guessed_block_rule,
  family = beta_family(link = "logit"),
  data = na.omit(sub_et_qu),
  
)
summary(bm2.1_dwell_time_intermediate)
# Family: beta  ( logit )
# Formula:          dwell_time_intermediate_t01 ~ IA_LABEL_f * Perf_Strivings_c +  
#     block_nr_c + CESD_c + GAD7_c + Change_category01 + (1 + Change_category01 ||      subject_id)
# Dispersion:                                   ~guessed_block_rule
# Data: na.omit(sub_et_qu)
# 
#      AIC      BIC   logLik deviance df.resid 
# -25618.3 -25488.4  12827.2 -25654.3    10078 
# 
# Random effects:
# 
# Conditional model:
#  Groups     Name              Variance Std.Dev. Corr 
#  subject_id (Intercept)       0.009927 0.09963       
#             Change_category01 0.174924 0.41824  0.00 
# Number of obs: 10096, groups:  subject_id, 202
# 
# Conditional model:
#                                      Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                         -1.476266   0.011950 -123.53  < 2e-16 ***
# IA_LABEL_fPositive                   0.047633   0.013181    3.61 0.000302 ***
# IA_LABEL_fNegative                   0.045718   0.013167    3.47 0.000516 ***
# IA_LABEL_fSuccess                    0.040091   0.013185    3.04 0.002360 ** 
# IA_LABEL_fFailure                    0.004130   0.013251    0.31 0.755281    
# Perf_Strivings_c                    -0.004533   0.006755   -0.67 0.502202    
# block_nr_c                          -0.002056   0.001450   -1.42 0.156268    
# CESD_c                              -0.001234   0.001171   -1.05 0.292011    
# GAD7_c                               0.003828   0.001935    1.98 0.047945 *  
# Change_category01                    0.249657   0.031121    8.02 1.04e-15 ***
# IA_LABEL_fPositive:Perf_Strivings_c  0.009479   0.007289    1.30 0.193478    
# IA_LABEL_fNegative:Perf_Strivings_c -0.008489   0.007294   -1.16 0.244457    
# IA_LABEL_fSuccess:Perf_Strivings_c   0.006924   0.007312    0.95 0.343642    
# IA_LABEL_fFailure:Perf_Strivings_c   0.005885   0.007337    0.80 0.422508    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Dispersion model:
#                    Estimate Std. Error z value Pr(>|z|)    
# (Intercept)        3.519088   0.033007  106.62   <2e-16 ***
# guessed_block_rule 0.006981   0.036696    0.19    0.849    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

lrtest(bm2.1_dwell_time_intermediate, bm1_dwell_time_intermediate)
# Model 1: dwell_time_intermediate_t01 ~ IA_LABEL_f * Perf_Strivings_c + 
#     block_nr_c + CESD_c + GAD7_c + Change_category01 + (1 + Change_category01 || 
#     subject_id)
# Model 2: dwell_time_intermediate_t01 ~ IA_LABEL_f + block_nr_c + CESD_c + 
#     GAD7_c + Change_category01 + (1 + Change_category01 || subject_id)
#   #Df LogLik Df  Chisq Pr(>Chisq)
# 1  18  12827                     
# 2  13  12823 -5 8.0095     0.1557

AIC(bm2.1_dwell_time_intermediate, bm1_dwell_time_intermediate)
#                               df       AIC
# bm2.1_dwell_time_intermediate 18 -25618.33
# bm1_dwell_time_intermediate   13 -25620.32

bm2.2_dwell_time_intermediate <- glmmTMB(
  dwell_time_intermediate_t01 ~ 
    IA_LABEL_f * Perf_Concerns_c +
    block_nr_c +
    CESD_c + GAD7_c +
    Change_category01 +
    #(1 | block_id) +  
    (1 + Change_category01 || subject_id),
  dispformula = ~ guessed_block_rule,
  family = beta_family(link = "logit"),
  data = na.omit(sub_et_qu),
  
)
summary(bm2.2_dwell_time_intermediate)
# Family: beta  ( logit )
# Formula:          dwell_time_intermediate_t01 ~ IA_LABEL_f * Perf_Concerns_c +  
#     block_nr_c + CESD_c + GAD7_c + Change_category01 + (1 + Change_category01 ||      subject_id)
# Dispersion:                                   ~guessed_block_rule
# Data: na.omit(sub_et_qu)
# 
#      AIC      BIC   logLik deviance df.resid 
# -25629.7 -25499.7  12832.8 -25665.7    10078 
# 
# Random effects:
# 
# Conditional model:
#  Groups     Name              Variance Std.Dev. Corr 
#  subject_id (Intercept)       0.009741 0.0987        
#             Change_category01 0.175172 0.4185   0.00 
# Number of obs: 10096, groups:  subject_id, 202
# 
# Conditional model:
#                                      Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                        -1.4763089  0.0119084 -123.97  < 2e-16 ***
# IA_LABEL_fPositive                  0.0475577  0.0131754    3.61 0.000307 ***
# IA_LABEL_fNegative                  0.0458459  0.0131607    3.48 0.000495 ***
# IA_LABEL_fSuccess                   0.0401331  0.0131794    3.05 0.002326 ** 
# IA_LABEL_fFailure                   0.0041135  0.0132463    0.31 0.756149    
# Perf_Concerns_c                    -0.0039345  0.0055057   -0.71 0.474841    
# block_nr_c                         -0.0020321  0.0014493   -1.40 0.160870    
# CESD_c                             -0.0005966  0.0012222   -0.49 0.625493    
# GAD7_c                              0.0040925  0.0019280    2.12 0.033781 *  
# Change_category01                   0.2495792  0.0311393    8.01  1.1e-15 ***
# IA_LABEL_fPositive:Perf_Concerns_c -0.0084813  0.0055777   -1.52 0.128369    
# IA_LABEL_fNegative:Perf_Concerns_c -0.0141987  0.0055761   -2.55 0.010886 *  
# IA_LABEL_fSuccess:Perf_Concerns_c   0.0029844  0.0055912    0.53 0.593499    
# IA_LABEL_fFailure:Perf_Concerns_c   0.0045310  0.0055979    0.81 0.418280    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Dispersion model:
#                    Estimate Std. Error z value Pr(>|z|)    
# (Intercept)        3.518591   0.033001  106.62   <2e-16 ***
# guessed_block_rule 0.008733   0.036697    0.24    0.812    
# ---

lrtest(bm2.2_dwell_time_intermediate, bm1_dwell_time_intermediate)
# Model 1: dwell_time_intermediate_t01 ~ IA_LABEL_f * Perf_Concerns_c + 
#     block_nr_c + CESD_c + GAD7_c + Change_category01 + (1 + Change_category01 || 
#     subject_id)
# Model 2: dwell_time_intermediate_t01 ~ IA_LABEL_f + block_nr_c + CESD_c + 
#     GAD7_c + Change_category01 + (1 + Change_category01 || subject_id)
#   #Df LogLik Df  Chisq Pr(>Chisq)   
# 1  18  12833                        
# 2  13  12823 -5 19.375   0.001636 **

AIC(bm2.2_dwell_time_intermediate, bm1_dwell_time_intermediate)
#                               df       AIC
# bm2.2_dwell_time_intermediate 18 -25629.69
# bm1_dwell_time_intermediate   13 -25620.32



intermediate_dwell_concerns_slopes <- emtrends(
  bm2.2_dwell_time_intermediate, ~ IA_LABEL_f, var = "Perf_Concerns_c"
)
summary(intermediate_dwell_concerns_slopes, infer = c(TRUE, TRUE))
# IA_LABEL_f Perf_Concerns_c.trend      SE  df asymp.LCL asymp.UCL z.ratio p.value
#  Neutral                -0.003935 0.00551 Inf   -0.0147   0.00686  -0.715  0.4748
#  Positive               -0.012416 0.00548 Inf   -0.0232  -0.00168  -2.266  0.0234
#  Negative               -0.018133 0.00547 Inf   -0.0289  -0.00741  -3.315  0.0009
#  Success                -0.000950 0.00549 Inf   -0.0117   0.00980  -0.173  0.8625
#  Failure                 0.000596 0.00549 Inf   -0.0102   0.01136   0.109  0.9135

intermediate_dwell_planned_slopes <- contrast(
  intermediate_dwell_concerns_slopes, method = semantic_planned, adjust = "none"
)
intermediate_dwell_planned_slopes
# contrast                           estimate      SE  df z.ratio p.value
#  positive_vs_negative               -0.00417 0.00787 Inf  -0.530  0.5962
#  Success_Failure                     0.00155 0.00558 Inf   0.277  0.7817
#  performance_irrelevant_vs_relevant  0.06791 0.02160 Inf   3.143  0.0017
#  NonNeutral_vs_Neutral              -0.01516 0.01770 Inf  -0.857  0.3916

bm0_dwell_time_late <- glmmTMB(
  dwell_time_late_t01 ~ 
    block_nr_c +
    CESD_c + GAD7_c +
    Change_category01 +
    #(1 | block_id) +  
    (1 + Change_category01 || subject_id),
  dispformula = ~ guessed_block_rule,
  family = beta_family(link = "logit"),
  data = na.omit(sub_et_qu),
  
)
summary(bm0_dwell_time_late)
# Family: beta  ( logit )
# Formula:          dwell_time_late_t01 ~ block_nr_c + CESD_c + GAD7_c + Change_category01 +  
#     (1 + Change_category01 || subject_id)
# Dispersion:                           ~guessed_block_rule
# Data: na.omit(sub_et_qu)
# 
#      AIC      BIC   logLik deviance df.resid 
# -18195.3 -18130.4   9106.7 -18213.3    10087 
# 
# Random effects:
# 
# Conditional model:
#  Groups     Name              Variance Std.Dev. Corr 
#  subject_id (Intercept)       0.152    0.3899        
#             Change_category01 1.377    1.1735   0.00 
# Number of obs: 10096, groups:  subject_id, 202
# 
# Conditional model:
#                    Estimate Std. Error z value Pr(>|z|)    
# (Intercept)       -1.610086   0.028652  -56.19  < 2e-16 ***
# block_nr_c        -0.008377   0.002378   -3.52 0.000426 ***
# CESD_c            -0.002396   0.003914   -0.61 0.540384    
# GAD7_c             0.012477   0.006540    1.91 0.056425 .  
# Change_category01  0.694363   0.084166    8.25  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Dispersion model:
#                    Estimate Std. Error z value Pr(>|z|)    
# (Intercept)         2.69635    0.03315   81.33  < 2e-16 ***
# guessed_block_rule -0.25996    0.03673   -7.08 1.46e-12 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

bm1_dwell_time_late <- glmmTMB(
  dwell_time_late_t01 ~ 
    IA_LABEL_f +
    block_nr_c +
    CESD_c + GAD7_c +
    Change_category01 +
    #(1 | block_id) +  
    (1 + Change_category01 || subject_id),
  dispformula = ~ guessed_block_rule,
  family = beta_family(link = "logit"),
  data = na.omit(sub_et_qu),
  
)
summary(bm1_dwell_time_late)
# Family: beta  ( logit )
# Formula:          dwell_time_late_t01 ~ IA_LABEL_f + block_nr_c + CESD_c + GAD7_c +  
#     Change_category01 + (1 + Change_category01 || subject_id)
# Dispersion:                           ~guessed_block_rule
# Data: na.omit(sub_et_qu)
# 
#      AIC      BIC   logLik deviance df.resid 
# -18199.0 -18105.1   9112.5 -18225.0    10083 
# 
# Random effects:
# 
# Conditional model:
#  Groups     Name              Variance Std.Dev. Corr 
#  subject_id (Intercept)       0.1519   0.3897        
#             Change_category01 1.3767   1.1733   0.00 
# Number of obs: 10096, groups:  subject_id, 202
# 
# Conditional model:
#                     Estimate Std. Error z value Pr(>|z|)    
# (Intercept)        -1.654369   0.031748  -52.11  < 2e-16 ***
# IA_LABEL_fPositive  0.044101   0.021487    2.05 0.040128 *  
# IA_LABEL_fNegative  0.064846   0.021332    3.04 0.002367 ** 
# IA_LABEL_fSuccess   0.058439   0.021367    2.74 0.006237 ** 
# IA_LABEL_fFailure   0.052596   0.021383    2.46 0.013906 *  
# block_nr_c         -0.008338   0.002376   -3.51 0.000451 ***
# CESD_c             -0.002420   0.003912   -0.62 0.536188    
# GAD7_c              0.012506   0.006537    1.91 0.055733 .  
# Change_category01   0.694298   0.084154    8.25  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Dispersion model:
#                    Estimate Std. Error z value Pr(>|z|)    
# (Intercept)         2.69779    0.03316   81.37  < 2e-16 ***
# guessed_block_rule -0.26017    0.03673   -7.08 1.41e-12 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

lrtest(bm1_dwell_time_late, bm0_dwell_time_late)
# Model 1: dwell_time_late_t01 ~ IA_LABEL_f + block_nr_c + CESD_c + GAD7_c + 
#     Change_category01 + (1 + Change_category01 || subject_id)
# Model 2: dwell_time_late_t01 ~ block_nr_c + CESD_c + GAD7_c + Change_category01 + 
#     (1 + Change_category01 || subject_id)
#   #Df LogLik Df Chisq Pr(>Chisq)  
# 1  13 9112.5                      
# 2   9 9106.7 -4 11.63    0.02032 *

AIC(bm1_dwell_time_late, bm0_dwell_time_late)
#                    df       AIC
# bm1_dwell_time_late 13 -18198.97
# bm0_dwell_time_late  9 -18195.34

bm2.1_dwell_time_late <- glmmTMB(
  dwell_time_late_t01 ~ 
    IA_LABEL_f * Perf_Strivings_c +
    block_nr_c +
    CESD_c + GAD7_c +
    Change_category01 +
    #(1 | block_id) +  
    (1 + Change_category01 || subject_id),
  dispformula = ~ guessed_block_rule,
  family = beta_family(link = "logit"),
  data = na.omit(sub_et_qu),
  
)
summary(bm2.1_dwell_time_late)
# Family: beta  ( logit )
# Formula:          dwell_time_late_t01 ~ IA_LABEL_f * Perf_Strivings_c + block_nr_c +  
#     CESD_c + GAD7_c + Change_category01 + (1 + Change_category01 ||      subject_id)
# Dispersion:                           ~guessed_block_rule
# Data: na.omit(sub_et_qu)
# 
#      AIC      BIC   logLik deviance df.resid 
# -18198.2 -18068.3   9117.1 -18234.2    10078 
# 
# Random effects:
# 
# Conditional model:
#  Groups     Name              Variance Std.Dev. Corr 
#  subject_id (Intercept)       0.1516   0.3894        
#             Change_category01 1.3766   1.1733   0.00 
# Number of obs: 10096, groups:  subject_id, 202
# 
# Conditional model:
#                                      Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                         -1.654446   0.031724  -52.15  < 2e-16 ***
# IA_LABEL_fPositive                   0.044088   0.021480    2.05 0.040126 *  
# IA_LABEL_fNegative                   0.065097   0.021324    3.05 0.002268 ** 
# IA_LABEL_fSuccess                    0.058304   0.021362    2.73 0.006346 ** 
# IA_LABEL_fFailure                    0.052578   0.021378    2.46 0.013914 *  
# Perf_Strivings_c                     0.006046   0.018141    0.33 0.738925    
# block_nr_c                          -0.008310   0.002376   -3.50 0.000469 ***
# CESD_c                              -0.002175   0.003961   -0.55 0.582985    
# GAD7_c                               0.012723   0.006546    1.94 0.051946 .  
# Change_category01                    0.694274   0.084151    8.25  < 2e-16 ***
# IA_LABEL_fPositive:Perf_Strivings_c -0.025496   0.011874   -2.15 0.031782 *  
# IA_LABEL_fNegative:Perf_Strivings_c -0.026093   0.011797   -2.21 0.026973 *  
# IA_LABEL_fSuccess:Perf_Strivings_c  -0.001540   0.011855   -0.13 0.896622    
# IA_LABEL_fFailure:Perf_Strivings_c  -0.010509   0.011751   -0.89 0.371172    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Dispersion model:
#                    Estimate Std. Error z value Pr(>|z|)    
# (Intercept)         2.69864    0.03318   81.33  < 2e-16 ***
# guessed_block_rule -0.26014    0.03677   -7.08 1.49e-12 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

lrtest(bm1_dwell_time_late, bm2.1_dwell_time_late)
# Model 1: dwell_time_late_t01 ~ IA_LABEL_f + block_nr_c + CESD_c + GAD7_c + 
#     Change_category01 + (1 + Change_category01 || subject_id)
# Model 2: dwell_time_late_t01 ~ IA_LABEL_f * Perf_Strivings_c + block_nr_c + 
#     CESD_c + GAD7_c + Change_category01 + (1 + Change_category01 || 
#     subject_id)
#   #Df LogLik Df  Chisq Pr(>Chisq)  
# 1  13 9112.5                       
# 2  18 9117.1  5 9.2654    0.09893 .

AIC(bm1_dwell_time_late, bm2.1_dwell_time_late)
#                       df       AIC
# bm1_dwell_time_late   13 -18198.97
# bm2.1_dwell_time_late 18 -18198.23

bm2.2_dwell_time_late <- glmmTMB(
  dwell_time_late_t01 ~ 
    IA_LABEL_f * Perf_Concerns_c +
    block_nr_c +
    CESD_c + GAD7_c +
    Change_category01 +
    #(1 | block_id) +  
    (1 + Change_category01 || subject_id),
  dispformula = ~ guessed_block_rule,
  family = beta_family(link = "logit"),
  data = na.omit(sub_et_qu),
  
)
summary(bm2.2_dwell_time_late)
# Family: beta  ( logit )
# Formula:          dwell_time_late_t01 ~ IA_LABEL_f * Perf_Concerns_c + block_nr_c +  
#     CESD_c + GAD7_c + Change_category01 + (1 + Change_category01 ||      subject_id)
# Dispersion:                           ~guessed_block_rule
# Data: na.omit(sub_et_qu)
# 
#      AIC      BIC   logLik deviance df.resid 
# -18200.1 -18070.1   9118.1 -18236.1    10078 
# 
# Random effects:
# 
# Conditional model:
#  Groups     Name              Variance Std.Dev. Corr 
#  subject_id (Intercept)       0.1458   0.3819        
#             Change_category01 1.3764   1.1732   0.00 
# Number of obs: 10096, groups:  subject_id, 202
# 
# Conditional model:
#                                     Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                        -1.654036   0.031269  -52.90  < 2e-16 ***
# IA_LABEL_fPositive                  0.043481   0.021490    2.02 0.043040 *  
# IA_LABEL_fNegative                  0.064542   0.021331    3.03 0.002480 ** 
# IA_LABEL_fSuccess                   0.058073   0.021368    2.72 0.006572 ** 
# IA_LABEL_fFailure                   0.052311   0.021383    2.45 0.014431 *  
# Perf_Concerns_c                    -0.030711   0.015222   -2.02 0.043629 *  
# block_nr_c                         -0.008335   0.002376   -3.51 0.000452 ***
# CESD_c                              0.001457   0.004088    0.36 0.721502    
# GAD7_c                              0.014300   0.006448    2.22 0.026572 *  
# Change_category01                   0.694321   0.084143    8.25  < 2e-16 ***
# IA_LABEL_fPositive:Perf_Concerns_c -0.016113   0.009257   -1.74 0.081767 .  
# IA_LABEL_fNegative:Perf_Concerns_c -0.012098   0.009190   -1.32 0.188040    
# IA_LABEL_fSuccess:Perf_Concerns_c  -0.007899   0.009152   -0.86 0.388081    
# IA_LABEL_fFailure:Perf_Concerns_c  -0.005069   0.009105   -0.56 0.577722    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Dispersion model:
#                    Estimate Std. Error z value Pr(>|z|)    
# (Intercept)         2.69742    0.03315   81.38  < 2e-16 ***
# guessed_block_rule -0.25934    0.03673   -7.06 1.66e-12 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

lrtest(bm1_dwell_time_late, bm2.2_dwell_time_late)
# Model 1: dwell_time_late_t01 ~ IA_LABEL_f + block_nr_c + CESD_c + GAD7_c + 
#     Change_category01 + (1 + Change_category01 || subject_id)
# Model 2: dwell_time_late_t01 ~ IA_LABEL_f * Perf_Concerns_c + block_nr_c + 
#     CESD_c + GAD7_c + Change_category01 + (1 + Change_category01 || 
#     subject_id)
#   #Df LogLik Df  Chisq Pr(>Chisq)  
# 1  13 9112.5                       
# 2  18 9118.1  5 11.135    0.04876 *

AIC(bm1_dwell_time_late, bm2.2_dwell_time_late)
#                       df       AIC
# bm1_dwell_time_late   13 -18198.97
# bm2.2_dwell_time_late 18 -18200.10

# Follow up the supported omnibus Concerns interaction. These estimates are
# intentionally not plotted in this sensitivity analysis.
late_dwell_concerns_slopes <- emtrends(
  bm2.2_dwell_time_late, ~ IA_LABEL_f, var = "Perf_Concerns_c"
)
summary(late_dwell_concerns_slopes, infer = c(TRUE, TRUE))
# IA_LABEL_f Perf_Concerns_c.trend     SE  df asymp.LCL asymp.UCL z.ratio p.value
# Neutral                  -0.0307 0.0152 Inf   -0.0605 -0.000878  -2.018  0.0436
# Positive                 -0.0468 0.0153 Inf   -0.0767 -0.016923  -3.069  0.0021
# Negative                 -0.0428 0.0152 Inf   -0.0726 -0.012991  -2.814  0.0049
# Success                  -0.0386 0.0152 Inf   -0.0684 -0.008797  -2.538  0.0111
# Failure                  -0.0358 0.0152 Inf   -0.0655 -0.006040  -2.358  0.0184

late_dwell_planned_slopes <- contrast(
late_dwell_concerns_slopes, method = semantic_planned, adjust = "none")
late_dwell_planned_slopes
# contrast                           estimate      SE  df z.ratio p.value
# positive_vs_negative                0.00685 0.01290 Inf   0.529  0.5965
# success_vs_failure                  0.00283 0.00906 Inf   0.312  0.7548
# performance_irrelevant_vs_relevant  0.01752 0.03530 Inf   0.496  0.6197
# neutral_vs_nonneutral              -0.04118 0.02910 Inf  -1.416  0.1568

# Whole-trial dwell time
#-----------------------
bm0_dwell_time <- glmmTMB(
  dwell_time_t01 ~ 
    block_nr_c +
    CESD_c + GAD7_c +
    Change_category01 +
    #(1 | block_id) +  
    (1 + Change_category01 || subject_id),
  dispformula = ~ guessed_block_rule,
  family = beta_family(link = "logit"),
  data = na.omit(sub_et_qu),
  
)
summary(bm0_dwell_time)
#  Family: beta  ( logit )
# Formula:          dwell_time_t01 ~ block_nr_c + CESD_c + GAD7_c + Change_category01 +  
#     (1 + Change_category01 || subject_id)
# Dispersion:                      ~guessed_block_rule
# Data: na.omit(sub_et_qu)
# 
#      AIC      BIC   logLik deviance df.resid 
# -29121.5 -29056.5  14569.8 -29139.5    10087 
# 
# Random effects:
# 
# Conditional model:
#  Groups     Name              Variance Std.Dev. Corr 
#  subject_id (Intercept)       0.03982  0.1996        
#             Change_category01 0.49632  0.7045   0.00 
# Number of obs: 10096, groups:  subject_id, 202
# 
# Conditional model:
#                     Estimate Std. Error z value Pr(>|z|)    
# (Intercept)       -1.5943388  0.0146585 -108.77   <2e-16 ***
# block_nr_c        -0.0009619  0.0012362   -0.78    0.437    
# CESD_c             0.0017970  0.0020100    0.89    0.371    
# GAD7_c             0.0009752  0.0033592    0.29    0.772    
# Change_category01  0.8149298  0.0502319   16.22   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Dispersion model:
#                    Estimate Std. Error z value Pr(>|z|)    
# (Intercept)         4.00637    0.03435  116.63  < 2e-16 ***
# guessed_block_rule -0.13171    0.03834   -3.44 0.000591 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

bm1_dwell_time <- glmmTMB(
  dwell_time_t01 ~ 
    IA_LABEL_f +
    block_nr_c +
    CESD_c + GAD7_c +
    Change_category01 +
    #(1 | block_id) +  
    (1 + Change_category01 || subject_id),
  dispformula = ~ guessed_block_rule,
  family = beta_family(link = "logit"),
  data = na.omit(sub_et_qu),
  
)
summary(bm1_dwell_time)
# Family: beta  ( logit )
# Formula:          dwell_time_t01 ~ IA_LABEL_f + block_nr_c + CESD_c + GAD7_c +  
#     Change_category01 + (1 + Change_category01 || subject_id)
# Dispersion:                      ~guessed_block_rule
# Data: na.omit(sub_et_qu)
# 
#      AIC      BIC   logLik deviance df.resid 
# -29147.6 -29053.7  14586.8 -29173.6    10083 
# 
# Random effects:
# 
# Conditional model:
#  Groups     Name              Variance Std.Dev. Corr 
#  subject_id (Intercept)       0.03984  0.1996        
#             Change_category01 0.49636  0.7045   0.00 
# Number of obs: 10096, groups:  subject_id, 202
# 
# Conditional model:
#                      Estimate Std. Error z value Pr(>|z|)    
# (Intercept)        -1.6301518  0.0163080  -99.96  < 2e-16 ***
# IA_LABEL_fPositive  0.0515259  0.0112159    4.59 4.35e-06 ***
# IA_LABEL_fNegative  0.0471195  0.0111627    4.22 2.43e-05 ***
# IA_LABEL_fSuccess   0.0546080  0.0111632    4.89 9.99e-07 ***
# IA_LABEL_fFailure   0.0249844  0.0112117    2.23   0.0259 *  
# block_nr_c         -0.0009734  0.0012342   -0.79   0.4303    
# CESD_c              0.0017932  0.0020101    0.89   0.3723    
# GAD7_c              0.0009711  0.0033593    0.29   0.7725    
# Change_category01   0.8150547  0.0502317   16.23  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Dispersion model:
#                    Estimate Std. Error z value Pr(>|z|)    
# (Intercept)         4.01363    0.03438  116.74  < 2e-16 ***
# guessed_block_rule -0.13626    0.03838   -3.55 0.000384 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

lrtest(bm1_dwell_time, bm0_dwell_time)
# Model 1: dwell_time_t01 ~ IA_LABEL_f + block_nr_c + CESD_c + GAD7_c + 
#     Change_category01 + (1 + Change_category01 || subject_id)
# Model 2: dwell_time_t01 ~ block_nr_c + CESD_c + GAD7_c + Change_category01 + 
#     (1 + Change_category01 || subject_id)
#   #Df LogLik Df  Chisq Pr(>Chisq)    
# 1  13  14587                         
# 2   9  14570 -4 34.071  7.206e-07 ***

AIC(bm1_dwell_time, bm0_dwell_time)
#                df       AIC
# bm1_dwell_time 13 -29147.60
# bm0_dwell_time  9 -29121.52


bm2.1_dwell_time <- glmmTMB(
  dwell_time_t01 ~ 
    IA_LABEL_f * Perf_Strivings_c +
    block_nr_c +
    CESD_c + GAD7_c +
    Change_category01 +
    #(1 | block_id) +  
    (1 + Change_category01 || subject_id),
  dispformula = ~ guessed_block_rule,
  family = beta_family(link = "logit"),
  data = na.omit(sub_et_qu),
  
)
summary(bm2.1_dwell_time)
# Family: beta  ( logit )
# Formula:          dwell_time_t01 ~ IA_LABEL_f * Perf_Strivings_c + block_nr_c +  
#     CESD_c + GAD7_c + Change_category01 + (1 + Change_category01 ||      subject_id)
# Dispersion:                      ~guessed_block_rule
# Data: na.omit(sub_et_qu)
# 
#      AIC      BIC   logLik deviance df.resid 
# -29139.6 -29009.7  14587.8 -29175.6    10078 
# 
# Random effects:
# 
# Conditional model:
#  Groups     Name              Variance Std.Dev. Corr 
#  subject_id (Intercept)       0.03957  0.1989        
#             Change_category01 0.49634  0.7045   0.00 
# Number of obs: 10096, groups:  subject_id, 202
# 
# Conditional model:
#                                       Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                         -1.6301506  0.0162672 -100.21  < 2e-16 ***
# IA_LABEL_fPositive                   0.0514830  0.0112158    4.59 4.43e-06 ***
# IA_LABEL_fNegative                   0.0471197  0.0111624    4.22 2.43e-05 ***
# IA_LABEL_fSuccess                    0.0546005  0.0111629    4.89 1.00e-06 ***
# IA_LABEL_fFailure                    0.0249780  0.0112113    2.23   0.0259 *  
# Perf_Strivings_c                    -0.0082361  0.0093369   -0.88   0.3777    
# block_nr_c                          -0.0009751  0.0012342   -0.79   0.4295    
# CESD_c                               0.0021609  0.0020310    1.06   0.2874    
# GAD7_c                               0.0012174  0.0033562    0.36   0.7168    
# Change_category01                    0.8150604  0.0502307   16.23  < 2e-16 ***
# IA_LABEL_fPositive:Perf_Strivings_c -0.0040556  0.0062029   -0.65   0.5132    
# IA_LABEL_fNegative:Perf_Strivings_c  0.0010966  0.0061663    0.18   0.8589    
# IA_LABEL_fSuccess:Perf_Strivings_c  -0.0010848  0.0061912   -0.18   0.8609    
# IA_LABEL_fFailure:Perf_Strivings_c  -0.0018976  0.0062090   -0.31   0.7599    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Dispersion model:
#                    Estimate Std. Error z value Pr(>|z|)    
# (Intercept)         4.01316    0.03439  116.69  < 2e-16 ***
# guessed_block_rule -0.13558    0.03839   -3.53 0.000413 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

lrtest(bm2.1_dwell_time, bm1_dwell_time)
# Model 1: dwell_time_t01 ~ IA_LABEL_f * Perf_Strivings_c + block_nr_c + 
#     CESD_c + GAD7_c + Change_category01 + (1 + Change_category01 || 
#     subject_id)
# Model 2: dwell_time_t01 ~ IA_LABEL_f + block_nr_c + CESD_c + GAD7_c + 
#     Change_category01 + (1 + Change_category01 || subject_id)
#   #Df LogLik Df  Chisq Pr(>Chisq)
# 1  18  14588                     
# 2  13  14587 -5 2.0484     0.8424

AIC(bm2.1_dwell_time, bm1_dwell_time)
#                 df       AIC
# bm2.1_dwell_time 18 -29139.64
# bm1_dwell_time   13 -29147.60

bm2.2_dwell_time <- glmmTMB(
  dwell_time_t01 ~ 
    IA_LABEL_f * Perf_Concerns_c +
    block_nr_c +
    CESD_c + GAD7_c +
    Change_category01 +
    #(1 | block_id) +  
    (1 + Change_category01 || subject_id),
  dispformula = ~ guessed_block_rule,
  family = beta_family(link = "logit"),
  data = na.omit(sub_et_qu),
  
)
summary(bm2.2_dwell_time)
#  Family: beta  ( logit )
# Formula:          dwell_time_t01 ~ IA_LABEL_f * Perf_Concerns_c + block_nr_c +  
#     CESD_c + GAD7_c + Change_category01 + (1 + Change_category01 ||      subject_id)
# Dispersion:                      ~guessed_block_rule
# Data: na.omit(sub_et_qu)
# 
#      AIC      BIC   logLik deviance df.resid 
# -29144.3 -29014.4  14590.2 -29180.3    10078 
# 
# Random effects:
# 
# Conditional model:
#  Groups     Name              Variance Std.Dev. Corr 
#  subject_id (Intercept)       0.03917  0.1979        
#             Change_category01 0.49638  0.7045   0.00 
# Number of obs: 10096, groups:  subject_id, 202
# 
# Conditional model:
#                                      Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                        -1.6301587  0.0162050 -100.60  < 2e-16 ***
# IA_LABEL_fPositive                  0.0514355  0.0112145    4.59 4.51e-06 ***
# IA_LABEL_fNegative                  0.0470840  0.0111611    4.22 2.46e-05 ***
# IA_LABEL_fSuccess                   0.0545651  0.0111616    4.89 1.02e-06 ***
# IA_LABEL_fFailure                   0.0249587  0.0112099    2.23   0.0260 *  
# Perf_Concerns_c                    -0.0132025  0.0079183   -1.67   0.0954 .  
# block_nr_c                         -0.0009783  0.0012340   -0.79   0.4279    
# CESD_c                              0.0031043  0.0021237    1.46   0.1438    
# GAD7_c                              0.0015773  0.0033501    0.47   0.6378    
# Change_category01                   0.8151259  0.0502323   16.23  < 2e-16 ***
# IA_LABEL_fPositive:Perf_Concerns_c -0.0037973  0.0047978   -0.79   0.4287    
# IA_LABEL_fNegative:Perf_Concerns_c  0.0039193  0.0047675    0.82   0.4110    
# IA_LABEL_fSuccess:Perf_Concerns_c  -0.0021994  0.0047780   -0.46   0.6453    
# IA_LABEL_fFailure:Perf_Concerns_c   0.0023073  0.0047991    0.48   0.6307    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Dispersion model:
#                    Estimate Std. Error z value Pr(>|z|)    
# (Intercept)         4.01319    0.03438  116.72  < 2e-16 ***
# guessed_block_rule -0.13524    0.03838   -3.52 0.000425 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

lrtest(bm2.2_dwell_time, bm1_dwell_time)
# Model 1: dwell_time_t01 ~ IA_LABEL_f * Perf_Concerns_c + block_nr_c + 
#     CESD_c + GAD7_c + Change_category01 + (1 + Change_category01 || 
#     subject_id)
# Model 2: dwell_time_t01 ~ IA_LABEL_f + block_nr_c + CESD_c + GAD7_c + 
#     Change_category01 + (1 + Change_category01 || subject_id)
#   #Df LogLik Df  Chisq Pr(>Chisq)
# 1  18  14590                     
# 2  13  14587 -5 6.7499     0.2399

AIC(bm2.2_dwell_time, bm1_dwell_time)
#                  df       AIC
# bm2.2_dwell_time 18 -29144.35
# bm1_dwell_time   13 -29147.60

# Data subset for Gamma GLMMs
sub_rt_qu <- rt_qu[c("response_time",
                     "subject_id", "block_id", "trial_nr_c", "guessed_block_rule",
                     "Change_category_f", "Perf_Strivings_c",
                     "Perf_Concerns_c", "CESD", "GAD7")]
sub_rt_qu$CESD_c <- sub_rt_qu$CESD - mean(sub_rt_qu$CESD, na.rm = TRUE)
sub_rt_qu$GAD7_c <- sub_rt_qu$GAD7 - mean(sub_rt_qu$GAD7, na.rm = TRUE)

# Response time
#--------------
gm0_response_time <- glmmTMB(
  response_time ~ 
    trial_nr_c +
    CESD_c + GAD7_c +
    #(1 | block_id) +
    (1 | subject_id),
  family = Gamma(link = "log"),
  dispformula = ~ guessed_block_rule,
  data = na.omit(sub_rt_qu),
  
)
summary(gm0_response_time)
#  Family: Gamma  ( log )
# Formula:          response_time ~ trial_nr_c + CESD_c + GAD7_c + (1 | subject_id)
# Dispersion:                     ~guessed_block_rule
# Data: na.omit(sub_rt_qu)
# 
#       AIC       BIC    logLik  deviance  df.resid 
#  238363.1  238416.4 -119174.6  238349.1     14898 
# 
# Random effects:
# 
# Conditional model:
#  Groups     Name        Variance Std.Dev.
#  subject_id (Intercept) 0.08097  0.2845  
# Number of obs: 14905, groups:  subject_id, 202
# 
# Conditional model:
#               Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  7.3958350  0.0203809   362.9   <2e-16 ***
# trial_nr_c  -0.0050426  0.0001759   -28.7   <2e-16 ***
# CESD_c       0.0026119  0.0027976     0.9    0.350    
# GAD7_c      -0.0009136  0.0046760    -0.2    0.845    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Dispersion model:
#                    Estimate Std. Error z value Pr(>|z|)    
# (Intercept)         1.56021    0.02678   58.25   <2e-16 ***
# guessed_block_rule -0.03694    0.02970   -1.24    0.213    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

gm1_response_time <- glmmTMB(
  response_time ~ 
    Change_category_f + 
    trial_nr_c +
    CESD_c + GAD7_c +
    #(1 | block_id) +
    (1 | subject_id),
  family = Gamma(link = "log"),
  dispformula = ~ guessed_block_rule,
  data = na.omit(sub_rt_qu),
  
)
summary(gm1_response_time)
# Family: Gamma  ( log )
# Formula:          response_time ~ Change_category_f + trial_nr_c + CESD_c + GAD7_c +  
#     (1 | subject_id)
# Dispersion:                     ~guessed_block_rule
# Data: na.omit(sub_rt_qu)
# 
#       AIC       BIC    logLik  deviance  df.resid 
#  238321.8  238405.5 -119149.9  238299.8     14894 
# 
# Random effects:
# 
# Conditional model:
#  Groups     Name        Variance Std.Dev.
#  subject_id (Intercept) 0.08118  0.2849  
# Number of obs: 14905, groups:  subject_id, 202
# 
# Conditional model:
#                             Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                7.4383927  0.0217924   341.3  < 2e-16 ***
# Change_category_fPositive -0.0387200  0.0120853    -3.2 0.001356 ** 
# Change_category_fNegative -0.0443696  0.0120892    -3.7 0.000242 ***
# Change_category_fSuccess  -0.0467886  0.0120762    -3.9 0.000107 ***
# Change_category_fFailure  -0.0844404  0.0120718    -7.0 2.66e-12 ***
# trial_nr_c                -0.0050678  0.0001761   -28.8  < 2e-16 ***
# CESD_c                     0.0026395  0.0028010     0.9 0.346022    
# GAD7_c                    -0.0009571  0.0046818    -0.2 0.838014    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Dispersion model:
#                    Estimate Std. Error z value Pr(>|z|)    
# (Intercept)         1.56617    0.02680   58.44   <2e-16 ***
# guessed_block_rule -0.04034    0.02971   -1.36    0.175    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

lrtest(gm1_response_time, gm0_response_time)
# Model 1: response_time ~ Change_category_f + trial_nr_c + CESD_c + GAD7_c + 
#     (1 | subject_id)
# Model 2: response_time ~ trial_nr_c + CESD_c + GAD7_c + (1 | subject_id)
#   #Df  LogLik Df  Chisq Pr(>Chisq)    
# 1  11 -119150                         
# 2   7 -119175 -4 49.364  4.901e-10 ***

AIC(gm1_response_time, gm0_response_time)
#                   df      AIC
# gm1_response_time 11 238321.8
# gm0_response_time  7 238363.1


gm2.1_response_time <- glmmTMB(
  response_time ~ 
    Change_category_f * Perf_Strivings_c + 
    trial_nr_c +
    CESD_c + GAD7_c +
    #(1 | block_id) +
    (1 | subject_id),
  family = Gamma(link = "log"),
  dispformula = ~ guessed_block_rule,
  data = na.omit(sub_rt_qu),
  
)
summary(gm2.1_response_time)
# Family: Gamma  ( log )
# Formula:          response_time ~ Change_category_f * Perf_Strivings_c + trial_nr_c +  
#     CESD_c + GAD7_c + (1 | subject_id)
# Dispersion:                     ~guessed_block_rule
# Data: na.omit(sub_rt_qu)
# 
#       AIC       BIC    logLik  deviance  df.resid 
#  238328.6  238450.3 -119148.3  238296.6     14889 
# 
# Random effects:
# 
# Conditional model:
#  Groups     Name        Variance Std.Dev.
#  subject_id (Intercept) 0.0811   0.2848  
# Number of obs: 14905, groups:  subject_id, 202
# 
# Conditional model:
#                                              Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                                 7.4383939  0.0217827   341.5  < 2e-16 ***
# Change_category_fPositive                  -0.0387163  0.0120843    -3.2 0.001356 ** 
# Change_category_fNegative                  -0.0443811  0.0120881    -3.7 0.000241 ***
# Change_category_fSuccess                   -0.0467925  0.0120749    -3.9 0.000107 ***
# Change_category_fFailure                   -0.0845181  0.0120706    -7.0 2.52e-12 ***
# Perf_Strivings_c                           -0.0046182  0.0125539    -0.4 0.712968    
# trial_nr_c                                 -0.0050727  0.0001763   -28.8  < 2e-16 ***
# CESD_c                                      0.0028051  0.0028374     1.0 0.322844    
# GAD7_c                                     -0.0008310  0.0046902    -0.2 0.859362    
# Change_category_fPositive:Perf_Strivings_c  0.0009149  0.0066979     0.1 0.891355    
# Change_category_fNegative:Perf_Strivings_c -0.0023085  0.0066669    -0.3 0.729144    
# Change_category_fSuccess:Perf_Strivings_c  -0.0043271  0.0066920    -0.6 0.517887    
# Change_category_fFailure:Perf_Strivings_c   0.0065915  0.0066933     1.0 0.324727    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Dispersion model:
#                    Estimate Std. Error z value Pr(>|z|)    
# (Intercept)         1.56730    0.02681   58.47   <2e-16 ***
# guessed_block_rule -0.04149    0.02972   -1.40    0.163    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

lrtest(gm2.1_response_time, gm1_response_time)
# Model 1: response_time ~ Change_category_f * Perf_Strivings_c + trial_nr_c + 
#     CESD_c + GAD7_c + (1 | subject_id)
# Model 2: response_time ~ Change_category_f + trial_nr_c + CESD_c + GAD7_c + 
#     (1 | subject_id)
#   #Df  LogLik Df  Chisq Pr(>Chisq)
# 1  16 -119148                     
# 2  11 -119150 -5 3.1956     0.6699

AIC(gm2.1_response_time, gm1_response_time)
#                     df      AIC
# gm2.1_response_time 16 238328.6
# gm1_response_time   11 238321.8

gm2.2_response_time <- glmmTMB(
  response_time ~ 
    Change_category_f * Perf_Concerns_c + 
    trial_nr_c +
    CESD_c + GAD7_c +
    #(1 | block_id) +
    (1 | subject_id),
  family = Gamma(link = "log"),
  dispformula = ~ guessed_block_rule,
  data = na.omit(sub_rt_qu),
  
)
summary(gm2.2_response_time)
# Family: Gamma  ( log )
# Formula:          response_time ~ Change_category_f * Perf_Concerns_c + trial_nr_c +  
#     CESD_c + GAD7_c + (1 | subject_id)
# Dispersion:                     ~guessed_block_rule
# Data: na.omit(sub_rt_qu)
# 
#       AIC       BIC    logLik  deviance  df.resid 
#  238324.4  238446.1 -119146.2  238292.4     14889 
# 
# Random effects:
# 
# Conditional model:
#  Groups     Name        Variance Std.Dev.
#  subject_id (Intercept) 0.0812   0.285   
# Number of obs: 14905, groups:  subject_id, 202
# 
# Conditional model:
#                                             Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                                7.4382233  0.0217936   341.3  < 2e-16 ***
# Change_category_fPositive                 -0.0385452  0.0120828    -3.2 0.001422 ** 
# Change_category_fNegative                 -0.0441974  0.0120862    -3.7 0.000255 ***
# Change_category_fSuccess                  -0.0466640  0.0120730    -3.9 0.000111 ***
# Change_category_fFailure                  -0.0843471  0.0120686    -7.0 2.77e-12 ***
# Perf_Concerns_c                            0.0112902  0.0107735     1.0 0.294656    
# trial_nr_c                                -0.0050782  0.0001762   -28.8  < 2e-16 ***
# CESD_c                                     0.0023312  0.0029833     0.8 0.434557    
# GAD7_c                                    -0.0011169  0.0047065    -0.2 0.812418    
# Change_category_fPositive:Perf_Concerns_c -0.0066345  0.0051720    -1.3 0.199576    
# Change_category_fNegative:Perf_Concerns_c -0.0110905  0.0051788    -2.1 0.032234 *  
# Change_category_fSuccess:Perf_Concerns_c  -0.0112691  0.0051532    -2.2 0.028756 *  
# Change_category_fFailure:Perf_Concerns_c  -0.0113527  0.0051470    -2.2 0.027405 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Dispersion model:
#                    Estimate Std. Error z value Pr(>|z|)    
# (Intercept)         1.56592    0.02681   58.41   <2e-16 ***
# guessed_block_rule -0.03946    0.02973   -1.33    0.184    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

lrtest(gm2.2_response_time, gm1_response_time)
# Model 1: response_time ~ Change_category_f * Perf_Concerns_c + trial_nr_c + 
#     CESD_c + GAD7_c + (1 | subject_id)
# Model 2: response_time ~ Change_category_f + trial_nr_c + CESD_c + GAD7_c + 
#     (1 | subject_id)
#   #Df  LogLik Df Chisq Pr(>Chisq)
# 1  16 -119146                    
# 2  11 -119150 -5  7.37     0.1945

AIC(gm2.2_response_time, gm1_response_time)
#                     df      AIC
# gm2.2_response_time 16 238324.4
# gm1_response_time   11 238321.8

