# AB Perfectionism - Study2

#-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*#
# Supplementary Analysis: Pre-ABM Attentional Patterns #
#-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*#

source("AB_Perfectionism_Study2_Preparation.R")

# Path a': Moderating effect of ABM on Perfectionism → Attention Bias (T1)
#-------------------------------------------------------------------------

# Create a subset of the data for model fitting
sub_et_qu2 <- et_qu2[c("subject_nr_1", "first_fix_prob_1", "first_fix_latency_1", "dwell_time_1",
                       "dwell_time_early_1", "dwell_time_intermediate_1", "dwell_time_late_1",
                       "guessed_block_rule_1",
                       "block_loop", "IA_LABEL_PR_SvsF", "IA_LABEL_PR_LvsH","IA_LABEL_EV", 
                       "Change_category01", "Condition", "Perf_Strivings_c" , "Perf_Concerns_c")]

# Transform 0 and 1 observations
transform01 <- function(x) {
  (x * (length(x) - 1) + 0.5) / (length(x))
}

# Define a function to back-transform from cauchit
inv.cauchit <- function(eta) {
  (1 / pi) * atan(eta) + 0.5
}

sub_et_qu2$first_fix_prob_1_t01 <- transform01(sub_et_qu2$first_fix_prob_1)
sub_et_qu2$first_fix_prob_1_t01 <- transform01(sub_et_qu2$first_fix_prob_1)

sub_et_qu2$dwell_time_early_1_t01 <- transform01(sub_et_qu2$dwell_time_early_1)
sub_et_qu2$dwell_time_early_1_t01 <- transform01(sub_et_qu2$dwell_time_early_1)

sub_et_qu2$dwell_time_intermediate_1_t01 <- transform01(sub_et_qu2$dwell_time_intermediate_1)
sub_et_qu2$dwell_time_intermediate_1_t01 <- transform01(sub_et_qu2$dwell_time_intermediate_1)

sub_et_qu2$dwell_time_late_1_t01 <- transform01(sub_et_qu2$dwell_time_late_1)
sub_et_qu2$dwell_time_late_1_t01 <- transform01(sub_et_qu2$dwell_time_late_1)

# Fit a Beta glmm for first fixation probability 
#-----------------------------------------------

# Null model
bm0_first_fix_prob_1 <- gamlss(
  first_fix_prob_1_t01 ~ 
    pb(subject_nr_1) +   
    pb(block_loop) +  
    pb(Change_category01, by = block_loop), 
  family = BE(mu.link = "logit"),   
  sigma.formula = ~ guessed_block_rule_1,  
  data = na.omit(sub_et_qu2)
) 
summary(bm0_first_fix_prob_1)
# Family:  c("BE", "Beta") 
# 
# Call:  gamlss(formula = first_fix_prob_1_t01 ~ pb(subject_nr_1) +  
#     pb(block_loop) + pb(Change_category01, by = block_loop),  
#     sigma.formula = ~guessed_block_rule_1, family = BE(mu.link = "logit"),      data = na.omit(sub_et_qu2)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  logit
# Mu Coefficients:
#                                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                            -4.533e-01  2.672e-02 -16.968  < 2e-16 ***
# pb(subject_nr_1)                        1.219e-05  6.339e-06   1.923   0.0546 .  
# pb(block_loop)                          7.616e-03  4.155e-03   1.833   0.0669 .  
# pb(Change_category01, by = block_loop) -1.660e-01  3.005e-02  -5.524  3.5e-08 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                      Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          -0.42127    0.02873 -14.662   <2e-16 ***
# guessed_block_rule_1  0.04710    0.03147   1.497    0.135    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms maybe are not accurate. 
# ------------------------------------------------------------------
# No. of observations in the fit:  4584 
# Degrees of Freedom for the fit:  7.40676
#       Residual Deg. of Freedom:  4576.593 
#                       at cycle:  4 
#  
# Global Deviance:     -2159.128 
#             AIC:     -2144.314 
#             SBC:     -2096.687 

# Back-transformed coefficients
print(round(inv.logit(bm0_first_fix_prob_1$mu.coefficients),4))
# (Intercept)                       pb(subject_nr_1) 
# 0.3886                                 0.5000 
# pb(block_loop) pb(Change_category01, by = block_loop) 
# 0.5019                                 0.4586  

# Partial Model
bm1_first_fix_prob_1 <- gamlss(
  first_fix_prob_1_t01 ~ 
    IA_LABEL_EV + 
    IA_LABEL_PR_LvsH + 
    IA_LABEL_PR_SvsF + 
    pb(subject_nr_1) +   
    pb(block_loop) +  
    pb(Change_category01, by = block_loop), 
  sigma.formula = ~ guessed_block_rule_1,  
  family = BE(mu.link = "logit"),   
  data = na.omit(sub_et_qu2)
) 
summary(bm1_first_fix_prob_1)
# Family:  c("BE", "Beta") 
# 
# Call:  gamlss(formula = first_fix_prob_1_t01 ~ IA_LABEL_EV +  
#     IA_LABEL_PR_LvsH + IA_LABEL_PR_SvsF + pb(subject_nr_1) +  
#     pb(block_loop) + pb(Change_category01, by = block_loop),  
#     sigma.formula = ~guessed_block_rule_1, family = BE(mu.link = "logit"),      data = na.omit(sub_et_qu2)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  logit
# Mu Coefficients:
#                                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                            -4.419e-01  2.752e-02 -16.059  < 2e-16 ***
# IA_LABEL_EV                            -1.002e-01  1.887e-02  -5.308 1.16e-07 ***
# IA_LABEL_PR_LvsH                        1.163e-02  8.110e-03   1.434  0.15168    
# IA_LABEL_PR_SvsF                        8.566e-02  2.665e-02   3.215  0.00131 ** 
# pb(subject_nr_1)                        1.195e-05  6.347e-06   1.882  0.05987 .  
# pb(block_loop)                          7.550e-03  4.148e-03   1.820  0.06879 .  
# pb(Change_category01, by = block_loop) -1.674e-01  3.000e-02  -5.579 2.56e-08 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                      Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          -0.42788    0.02877 -14.870   <2e-16 ***
# guessed_block_rule_1  0.04954    0.03149   1.573    0.116    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms maybe are not accurate. 
# ------------------------------------------------------------------
# No. of observations in the fit:  4584 
# Degrees of Freedom for the fit:  10.37232
#       Residual Deg. of Freedom:  4573.628 
#                       at cycle:  4 
#  
# Global Deviance:     -2189.707 
#             AIC:     -2168.962 
#             SBC:     -2102.265  

# Back-transformed coefficients
print(round(inv.logit(bm1_first_fix_prob_1$mu.coefficients),4))
# (Intercept)                            IA_LABEL_EV 
# 0.3913                                 0.4750 
# IA_LABEL_PR_LvsH                       IA_LABEL_PR_SvsF 
# 0.5029                                 0.5214 
# pb(subject_nr_1)                         pb(block_loop) 
# 0.5000                                 0.5019 
# pb(Change_category01, by = block_loop) 
# 0.4583 

# Model comparison
lrtest(bm0_first_fix_prob_1, bm1_first_fix_prob_1)
#       #Df LogLik     Df  Chisq Pr(>Chisq)    
# 1  7.4068 1079.6                             
# 2 10.3723 1094.8 2.9656 30.579  1.043e-06 ***

AIC(bm1_first_fix_prob_1, bm0_first_fix_prob_1)
#                            df       AIC
# bm1_first_fix_prob_1 10.37232 -2168.962
# bm0_first_fix_prob_1  7.40676 -2144.314

# Full Model: Perf_Strivings 
bm2.1_first_fix_prob_1 <- gamlss(
  first_fix_prob_1_t01 ~ 
    IA_LABEL_EV * Perf_Strivings_c + 
    IA_LABEL_PR_LvsH * Perf_Strivings_c + 
    IA_LABEL_PR_SvsF * Perf_Strivings_c + 
    pb(subject_nr_1) +   
    pb(block_loop) +  
    pb(Change_category01, by = block_loop), 
  sigma.formula = ~ guessed_block_rule_1,  
  family = BE(mu.link = "logit"),   
  data = na.omit(sub_et_qu2)
) 
summary(bm2.1_first_fix_prob_1)
# Family:  c("BE", "Beta") 
# 
# Call:  gamlss(formula = first_fix_prob_1_t01 ~ IA_LABEL_EV *  
#     Perf_Strivings_c + IA_LABEL_PR_LvsH * Perf_Strivings_c +  
#     IA_LABEL_PR_SvsF * Perf_Strivings_c + pb(subject_nr_1) +  
#     pb(block_loop) + pb(Change_category01, by = block_loop),  
#     sigma.formula = ~guessed_block_rule_1, family = BE(mu.link = "logit"),      data = na.omit(sub_et_qu2)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  logit
# Mu Coefficients:
#                                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                            -4.498e-01  2.752e-02 -16.348  < 2e-16 ***
# IA_LABEL_EV                            -1.001e-01  1.886e-02  -5.308 1.16e-07 ***
# Perf_Strivings_c                       -7.013e-03  7.289e-03  -0.962  0.33605    
# IA_LABEL_PR_LvsH                        1.161e-02  8.108e-03   1.431  0.15241    
# IA_LABEL_PR_SvsF                        8.563e-02  2.664e-02   3.214  0.00132 ** 
# pb(subject_nr_1)                        1.302e-05  6.347e-06   2.051  0.04028 *  
# pb(block_loop)                          7.554e-03  4.148e-03   1.821  0.06862 .  
# pb(Change_category01, by = block_loop) -1.672e-01  2.999e-02  -5.576 2.60e-08 ***
# IA_LABEL_EV:Perf_Strivings_c           -2.495e-04  1.012e-02  -0.025  0.98034    
# Perf_Strivings_c:IA_LABEL_PR_LvsH       3.165e-03  4.360e-03   0.726  0.46794    
# Perf_Strivings_c:IA_LABEL_PR_SvsF       8.913e-03  1.431e-02   0.623  0.53331    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                      Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          -0.42888    0.02881 -14.884   <2e-16 ***
# guessed_block_rule_1  0.05017    0.03154   1.591    0.112    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms maybe are not accurate. 
# ------------------------------------------------------------------
# No. of observations in the fit:  4584 
# Degrees of Freedom for the fit:  14.3663
#       Residual Deg. of Freedom:  4569.634 
#                       at cycle:  4 
#  
# Global Deviance:     -2193.127 
#             AIC:     -2164.394 
#             SBC:     -2072.014 

# Back-transformed coefficients
print(round(inv.logit(bm2.1_first_fix_prob_1$mu.coefficients),4))
# (Intercept)                            IA_LABEL_EV 
# 0.3894                                 0.4750 
# Perf_Strivings_c                       IA_LABEL_PR_LvsH 
# 0.4982                                 0.5029 
# IA_LABEL_PR_SvsF                       pb(subject_nr_1) 
# 0.5214                                 0.5000 
# pb(block_loop) pb(Change_category01, by = block_loop) 
# 0.5019                                 0.4583 
# IA_LABEL_EV:Perf_Strivings_c      Perf_Strivings_c:IA_LABEL_PR_LvsH 
# 0.4999                                 0.5008 
# Perf_Strivings_c:IA_LABEL_PR_SvsF 
# 0.5022 

# Model comparison
lrtest(bm1_first_fix_prob_1, bm2.1_first_fix_prob_1)
#      #Df LogLik    Df  Chisq Pr(>Chisq)
# 1 10.372 1094.8                        
# 2 14.366 1096.6 3.994 3.4199     0.4902

AIC(bm1_first_fix_prob_1, bm2.1_first_fix_prob_1)
#                              df       AIC
# bm1_first_fix_prob_1   10.37232 -2168.962
# bm2.1_first_fix_prob_1 14.36630 -2164.394

# Full Model: Perf_Concerns
bm2.2_first_fix_prob_1 <- gamlss(
  first_fix_prob_1_t01 ~ 
    IA_LABEL_EV * Perf_Concerns_c + 
    IA_LABEL_PR_LvsH * Perf_Concerns_c + 
    IA_LABEL_PR_SvsF * Perf_Concerns_c + 
    pb(subject_nr_1) +   
    pb(block_loop) +  
    pb(Change_category01, by = block_loop), 
  sigma.formula = ~ guessed_block_rule_1,  
  family = BE(mu.link = "logit"),   
  data = na.omit(sub_et_qu2)
) 
summary(bm2.2_first_fix_prob_1)
# Family:  c("BE", "Beta") 
# 
# Call:  gamlss(formula = first_fix_prob_1_t01 ~ IA_LABEL_EV *  
#     Perf_Concerns_c + IA_LABEL_PR_LvsH * Perf_Concerns_c +  
#     IA_LABEL_PR_SvsF * Perf_Concerns_c + pb(subject_nr_1) +  
#     pb(block_loop) + pb(Change_category01, by = block_loop),  
#     sigma.formula = ~guessed_block_rule_1, family = BE(mu.link = "logit"),      data = na.omit(sub_et_qu2)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  logit
# Mu Coefficients:
#                                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                            -4.428e-01  2.752e-02 -16.095  < 2e-16 ***
# IA_LABEL_EV                            -1.002e-01  1.886e-02  -5.311 1.14e-07 ***
# Perf_Concerns_c                         1.136e-04  5.687e-03   0.020  0.98407    
# IA_LABEL_PR_LvsH                        1.163e-02  8.109e-03   1.435  0.15143    
# IA_LABEL_PR_SvsF                        8.564e-02  2.664e-02   3.215  0.00132 ** 
# pb(subject_nr_1)                        1.206e-05  6.348e-06   1.900  0.05747 .  
# pb(block_loop)                          7.552e-03  4.147e-03   1.821  0.06867 .  
# pb(Change_category01, by = block_loop) -1.673e-01  2.999e-02  -5.577 2.59e-08 ***
# IA_LABEL_EV:Perf_Concerns_c             7.645e-03  7.906e-03   0.967  0.33357    
# Perf_Concerns_c:IA_LABEL_PR_LvsH        2.767e-03  3.399e-03   0.814  0.41560    
# Perf_Concerns_c:IA_LABEL_PR_SvsF       -1.411e-02  1.117e-02  -1.262  0.20685    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                      Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          -0.42884    0.02880 -14.892   <2e-16 ***
# guessed_block_rule_1  0.05031    0.03152   1.596     0.11    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms maybe are not accurate. 
# ------------------------------------------------------------------
# No. of observations in the fit:  4584 
# Degrees of Freedom for the fit:  14.36986
#       Residual Deg. of Freedom:  4569.63 
#                       at cycle:  4 
#  
# Global Deviance:     -2192.143 
#             AIC:     -2163.403 
#             SBC:     -2071 

# Back-transformed coefficients
print(round(inv.logit(bm2.2_first_fix_prob_1$mu.coefficients),4))
# (Intercept)                            IA_LABEL_EV 
# 0.3911                                 0.4750 
# Perf_Concerns_c                       IA_LABEL_PR_LvsH 
# 0.5000                                 0.5029 
# IA_LABEL_PR_SvsF                       pb(subject_nr_1) 
# 0.5214                                 0.5000 
# pb(block_loop) pb(Change_category01, by = block_loop) 
# 0.5019                                 0.4583 
# IA_LABEL_EV:Perf_Concerns_c       Perf_Concerns_c:IA_LABEL_PR_LvsH 
# 0.5019                                 0.5007 
# Perf_Concerns_c:IA_LABEL_PR_SvsF 
# 0.4965

# Model comparison
lrtest(bm1_first_fix_prob_1, bm2.2_first_fix_prob_1)
#      #Df LogLik     Df  Chisq Pr(>Chisq)
# 1 10.372 1094.8                         
# 2 14.370 1096.1 3.9975 2.4362     0.6561

AIC(bm1_first_fix_prob_1, bm2.2_first_fix_prob_1)
#                              df       AIC
# bm1_first_fix_prob_1   10.37232 -2168.962
# bm2.2_first_fix_prob_1 14.36986 -2163.403

# Fit a Beta glmm for first fixation latency
#-------------------------------------------

# Null model
bm0_first_fix_latency_1 <- gamlss(
  first_fix_latency_1 ~ 
    pb(subject_nr_1) +   
    pb(block_loop) +   
    pb(Change_category01, by = block_loop),  
  sigma.formula = ~ guessed_block_rule_1,  
  family = BE(mu.link = "logit"),   
  data = na.omit(sub_et_qu2)
)
summary(bm0_first_fix_latency_1)
# Family:  c("BE", "Beta") 
# 
# Call:  gamlss(formula = first_fix_latency_1 ~ pb(subject_nr_1) +  
#     pb(block_loop) + pb(Change_category01, by = block_loop),  
#     sigma.formula = ~guessed_block_rule_1, family = BE(mu.link = "logit"),      data = na.omit(sub_et_qu2)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  logit
# Mu Coefficients:
#                                          Estimate Std. Error  t value Pr(>|t|)    
# (Intercept)                            -1.392e+00  1.118e-02 -124.532  < 2e-16 ***
# pb(subject_nr_1)                        1.373e-07  9.705e-07    0.141 0.887520    
# pb(block_loop)                          7.011e-05  8.019e-04    0.087 0.930335    
# pb(Change_category01, by = block_loop)  2.015e-02  5.617e-03    3.587 0.000338 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                      Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          -2.68755    0.02277  -118.0   <2e-16 ***
# guessed_block_rule_1 -0.05744    0.02611    -2.2   0.0279 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms maybe are not accurate. 
# ------------------------------------------------------------------
# No. of observations in the fit:  4584 
# Degrees of Freedom for the fit:  6.280152
#       Residual Deg. of Freedom:  4577.72 
#                       at cycle:  5 
#  
# Global Deviance:     -21036.97 
#             AIC:     -21024.41 
#             SBC:     -20984.03

# Back-transformed coefficients
print(round(inv.logit(bm0_first_fix_latency_1$mu.coefficients),4))
# (Intercept)                       pb(subject_nr_1) 
# 0.1991                                 0.5000 
# pb(block_loop) pb(Change_category01, by = block_loop) 
# 0.5000                                 0.5050 

# Partial Model
bm1_first_fix_latency_1 <- gamlss(
  first_fix_latency_1 ~ 
    IA_LABEL_EV + 
    IA_LABEL_PR_SvsF + 
    IA_LABEL_PR_LvsH + 
    pb(subject_nr_1) +   
    pb(block_loop) +   
    pb(Change_category01, by = block_loop),  
  sigma.formula = ~ guessed_block_rule_1,  
  family = BE(mu.link = "logit"),   
  data = na.omit(sub_et_qu2)
) 
summary(bm1_first_fix_latency_1)
# Family:  c("BE", "Beta") 
# 
# Call:  gamlss(formula = first_fix_latency_1 ~ IA_LABEL_EV + IA_LABEL_PR_SvsF +  
#     IA_LABEL_PR_LvsH + pb(subject_nr_1) + pb(block_loop) + pb(Change_category01,  
#     by = block_loop), sigma.formula = ~guessed_block_rule_1,  
#     family = BE(mu.link = "logit"), data = na.omit(sub_et_qu2)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  logit
# Mu Coefficients:
#                                          Estimate Std. Error  t value Pr(>|t|)    
# (Intercept)                            -1.392e+00  1.151e-02 -120.952  < 2e-16 ***
# IA_LABEL_EV                             1.065e-02  3.558e-03    2.993 0.002775 ** 
# IA_LABEL_PR_SvsF                       -6.681e-03  5.031e-03   -1.328 0.184248    
# IA_LABEL_PR_LvsH                        1.445e-04  1.531e-03    0.094 0.924779    
# pb(subject_nr_1)                        1.299e-07  1.422e-06    0.091 0.927204    
# pb(block_loop)                          7.267e-05  7.832e-04    0.093 0.926073    
# pb(Change_category01, by = block_loop)  2.011e-02  5.604e-03    3.589 0.000335 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                      Estimate Std. Error  t value Pr(>|t|)    
# (Intercept)          -2.69043    0.02265 -118.802   <2e-16 ***
# guessed_block_rule_1 -0.05521    0.02594   -2.128   0.0334 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms may not be reliable. 
# ------------------------------------------------------------------
# No. of observations in the fit:  4584 
# Degrees of Freedom for the fit:  9.280616
#       Residual Deg. of Freedom:  4574.719 
#                       at cycle:  5 
#  
# Global Deviance:     -21047.18 
#             AIC:     -21028.62 
#             SBC:     -20968.94

# Back-transformed coefficients
print(round(inv.logit(bm1_first_fix_latency_1$mu.coefficients),4))
# (Intercept)                            IA_LABEL_EV 
# 0.1992                                 0.5027 
# IA_LABEL_PR_SvsF                       IA_LABEL_PR_LvsH 
# 0.4983                                 0.5000 
# pb(subject_nr_1)                         pb(block_loop) 
# 0.5000                                 0.5000 
# pb(Change_category01, by = block_loop) 
# 0.5050                

# Model comparison
lrtest(bm0_first_fix_latency_1, bm1_first_fix_latency_1)
#      #Df LogLik     Df  Chisq Pr(>Chisq)  
# 1 6.2802  10518                           
# 2 9.2806  10524 3.0005 10.207    0.01689 *

AIC(bm1_first_fix_latency_1, bm0_first_fix_latency_1)
#                               df       AIC
# bm1_first_fix_latency_1 9.280616 -21028.62
# bm0_first_fix_latency_1 6.280152 -21024.41

# Full Model: Perf_Strivings
bm2.1_first_fix_latency_1 <- gamlss(
  first_fix_latency_1 ~ 
    IA_LABEL_EV * Perf_Strivings_c + 
    IA_LABEL_PR_SvsF * Perf_Strivings_c + 
    IA_LABEL_PR_LvsH * Perf_Strivings_c +
    pb(subject_nr_1) +   
    pb(block_loop) +   
    pb(Change_category01, by = block_loop),  
  sigma.formula = ~ guessed_block_rule_1,  
  family = BE(mu.link = "logit"),   
  data = na.omit(sub_et_qu2)
)
summary(bm2.1_first_fix_latency_1)
# Family:  c("BE", "Beta") 
# 
# Call:  gamlss(formula = first_fix_latency_1 ~ IA_LABEL_EV *  
#     Perf_Strivings_c + IA_LABEL_PR_SvsF * Perf_Strivings_c +  
#     IA_LABEL_PR_LvsH * Perf_Strivings_c + pb(subject_nr_1) +  
#     pb(block_loop) + pb(Change_category01, by = block_loop),  
#     sigma.formula = ~guessed_block_rule_1, family = BE(mu.link = "logit"),      data = na.omit(sub_et_qu2)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  logit
# Mu Coefficients:
#                                          Estimate Std. Error  t value Pr(>|t|)    
# (Intercept)                            -1.392e+00  1.161e-02 -119.858  < 2e-16 ***
# IA_LABEL_EV                             1.065e-02  3.580e-03    2.974 0.002951 ** 
# Perf_Strivings_c                        7.293e-04  1.599e-03    0.456 0.648385    
# IA_LABEL_PR_SvsF                       -6.678e-03  5.146e-03   -1.298 0.194445    
# IA_LABEL_PR_LvsH                        1.414e-04  7.561e-04    0.187 0.851651    
# pb(subject_nr_1)                        1.360e-07  7.130e-07    0.191 0.848775    
# pb(block_loop)                          7.080e-05  9.488e-04    0.075 0.940514    
# pb(Change_category01, by = block_loop)  2.011e-02  5.630e-03    3.572 0.000358 ***
# IA_LABEL_EV:Perf_Strivings_c           -1.823e-03  1.579e-03   -1.154 0.248439    
# Perf_Strivings_c:IA_LABEL_PR_SvsF       1.352e-03  2.400e-03    0.563 0.573144    
# Perf_Strivings_c:IA_LABEL_PR_LvsH       9.459e-04  9.278e-04    1.020 0.308008    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                      Estimate Std. Error  t value Pr(>|t|)    
# (Intercept)          -2.68988    0.02267 -118.678   <2e-16 ***
# guessed_block_rule_1 -0.05627    0.02595   -2.168   0.0302 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms maybe are not accurate. 
# ------------------------------------------------------------------
# No. of observations in the fit:  4584 
# Degrees of Freedom for the fit:  13.28075
#       Residual Deg. of Freedom:  4570.719 
#                       at cycle:  5 
#  
# Global Deviance:     -21049.45 
#             AIC:     -21022.89 
#             SBC:     -20937.49  

# Back-transformed coefficients
print(round(inv.logit(bm2.1_first_fix_latency_1$mu.coefficients),4))
# (Intercept)                            IA_LABEL_EV 
# 0.1991                                 0.5027 
# Perf_Strivings_c                       IA_LABEL_PR_SvsF 
# 0.5002                                 0.4983 
# IA_LABEL_PR_LvsH                       pb(subject_nr_1) 
# 0.5000                                 0.5000 
# pb(block_loop) pb(Change_category01, by = block_loop) 
# 0.5000                                 0.5050 
# IA_LABEL_EV:Perf_Strivings_c      Perf_Strivings_c:IA_LABEL_PR_SvsF 
# 0.4995                                 0.5003 
# Perf_Strivings_c:IA_LABEL_PR_LvsH 
# 0.5002 

# Model comparison
lrtest(bm1_first_fix_latency_1, bm2.1_first_fix_latency_1)
#       #Df LogLik     Df  Chisq Pr(>Chisq)
# 1  9.2806  10524                         
# 2 13.2807  10525 4.0001 2.2759     0.6852

AIC(bm2.1_first_fix_latency_1, bm1_first_fix_latency_1)
#                                  df       AIC
# bm1_first_fix_latency_1    9.280616 -21028.62
# bm2.1_first_fix_latency_1 13.280746 -21022.89

# Full Model: Perf_Concerns
bm2.2_first_fix_latency_1 <- gamlss(
  first_fix_latency_1 ~ 
    IA_LABEL_EV * Perf_Concerns_c + 
    IA_LABEL_PR_SvsF * Perf_Concerns_c +
    IA_LABEL_PR_LvsH * Perf_Concerns_c +
    pb(subject_nr_1) +   
    pb(block_loop) +   
    pb(Change_category01, by = block_loop),  
  sigma.formula = ~ guessed_block_rule_1,  
  family = BE(mu.link = "logit"),   
  data = na.omit(sub_et_qu2)
)
summary(bm2.2_first_fix_latency_1)
# Family:  c("BE", "Beta") 
# 
# Call:  gamlss(formula = first_fix_latency_1 ~ IA_LABEL_EV *  
#     Perf_Concerns_c + IA_LABEL_PR_SvsF * Perf_Concerns_c +  
#     IA_LABEL_PR_LvsH * Perf_Concerns_c + pb(subject_nr_1) +  
#     pb(block_loop) + pb(Change_category01, by = block_loop),  
#     sigma.formula = ~guessed_block_rule_1, family = BE(mu.link = "logit"),      data = na.omit(sub_et_qu2)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  logit
# Mu Coefficients:
#                                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                            -1.392e+00  7.762e-02 -17.928  < 2e-16 ***
# IA_LABEL_EV                             1.065e-02  3.851e-03   2.766 0.005702 ** 
# Perf_Concerns_c                         3.111e-04  3.005e-03   0.104 0.917545    
# IA_LABEL_PR_SvsF                       -6.684e-03  5.493e-03  -1.217 0.223736    
# IA_LABEL_PR_LvsH                        1.404e-04  1.059e-02   0.013 0.989422    
# pb(subject_nr_1)                        1.334e-07  4.350e-06   0.031 0.975543    
# pb(block_loop)                          7.189e-05  6.892e-03   0.010 0.991678    
# pb(Change_category01, by = block_loop)  2.011e-02  5.750e-03   3.498 0.000473 ***
# IA_LABEL_EV:Perf_Concerns_c            -8.994e-04  2.911e-03  -0.309 0.757353    
# Perf_Concerns_c:IA_LABEL_PR_SvsF        1.446e-03  5.239e-03   0.276 0.782527    
# Perf_Concerns_c:IA_LABEL_PR_LvsH        4.560e-04  2.646e-03   0.172 0.863216    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                      Estimate Std. Error  t value Pr(>|t|)    
# (Intercept)          -2.68993    0.02313 -116.289   <2e-16 ***
# guessed_block_rule_1 -0.05602    0.02664   -2.103   0.0355 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms maybe are not accurate. 
# ------------------------------------------------------------------
# No. of observations in the fit:  4584 
# Degrees of Freedom for the fit:  13.28068
#       Residual Deg. of Freedom:  4570.719 
#                       at cycle:  5 
#  
# Global Deviance:     -21048.2 
#             AIC:     -21021.63 
#             SBC:     -20936.23 

# Back-transformed coefficients
print(round(inv.logit(bm2.2_first_fix_latency_1$mu.coefficients),4))
# (Intercept)                            IA_LABEL_EV 
# 0.1991                                 0.5027 
# Perf_Concerns_c                       IA_LABEL_PR_SvsF 
# 0.5001                                 0.4983 
# IA_LABEL_PR_LvsH                       pb(subject_nr_1) 
# 0.5000                                 0.5000 
# pb(block_loop) pb(Change_category01, by = block_loop) 
# 0.5000                                 0.5050 
# IA_LABEL_EV:Perf_Concerns_c       Perf_Concerns_c:IA_LABEL_PR_SvsF 
# 0.4998                                 0.5004 
# Perf_Concerns_c:IA_LABEL_PR_LvsH 
# 0.5001 

# Model comparison
lrtest(bm1_first_fix_latency_1, bm2.2_first_fix_latency_1)
#       #Df LogLik     Df  Chisq Pr(>Chisq)
# 1  9.2806  10524                         
# 2 13.2807  10524 4.0001 1.0168     0.9072

AIC(bm2.2_first_fix_latency_1, bm1_first_fix_latency_1)
#                                  df       AIC
# bm1_first_fix_latency_1    9.280616 -21028.62
# bm2.2_first_fix_latency_1 13.280679 -21021.63

# Fit a beta glmm for Dwell  Time
#--------------------------------

# Early
#-----------

# Null Model
bm0_dwell_time_early_1 <- gamlss(
  dwell_time_early_1_t01 ~ 
    pb(subject_nr_1) +   
    pb(block_loop)+  
    pb(Change_category01, by = block_loop), 
  sigma.formula = ~ guessed_block_rule_1,  
  family = BE(mu.link = "logit"),   
  data = na.omit(sub_et_qu2)
) 
summary(bm0_dwell_time_early_1)
# Family:  c("BE", "Beta") 
# 
# Call:  gamlss(formula = dwell_time_early_1_t01 ~ pb(subject_nr_1) +  
#     pb(block_loop) + pb(Change_category01, by = block_loop),  
#     sigma.formula = ~guessed_block_rule_1, family = BE(mu.link = "logit"),      data = na.omit(sub_et_qu2)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  logit
# Mu Coefficients:
#                                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                            -1.564e+00  5.044e-02 -31.014   <2e-16 ***
# pb(subject_nr_1)                        1.719e-05  6.242e-06   2.755   0.0059 ** 
# pb(block_loop)                          7.429e-03  3.417e-03   2.174   0.0297 *  
# pb(Change_category01, by = block_loop) -2.041e-02  2.489e-02  -0.820   0.4122    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                       Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          -0.942746   0.029591 -31.859   <2e-16 ***
# guessed_block_rule_1  0.005472   0.034679   0.158    0.875    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms maybe are not accurate. 
# ------------------------------------------------------------------
# No. of observations in the fit:  4584 
# Degrees of Freedom for the fit:  10.14364
#       Residual Deg. of Freedom:  4573.856 
#                       at cycle:  6 
#  
# Global Deviance:     -7854.457 
#             AIC:     -7834.169 
#             SBC:     -7768.942 

# Back-transformed coefficients
print(round(inv.logit(bm0_dwell_time_early_1$mu.coefficients),4))
# (Intercept)                       pb(subject_nr_1) 
# 0.1730                                 0.5000 
# pb(block_loop) pb(Change_category01, by = block_loop) 
# 0.5019                                 0.4949 

# Partial Model
bm1_dwell_time_early_1 <- gamlss(
  dwell_time_early_1_t01 ~ 
    IA_LABEL_EV + 
    IA_LABEL_PR_LvsH + 
    IA_LABEL_PR_SvsF + 
    pb(subject_nr_1) +   
    pb(block_loop) +  
    pb(Change_category01, by = block_loop), 
  sigma.formula = ~ guessed_block_rule_1,  
  family = BE(mu.link = "logit"),   
  data = na.omit(sub_et_qu2)
) 
summary(bm1_dwell_time_early_1)
# Family:  c("BE", "Beta") 
# 
# Call:  gamlss(formula = dwell_time_early_1_t01 ~ IA_LABEL_EV +  
#     IA_LABEL_PR_LvsH + IA_LABEL_PR_SvsF + pb(subject_nr_1) +  
#     pb(block_loop) + pb(Change_category01, by = block_loop),  
#     sigma.formula = ~guessed_block_rule_1, family = BE(mu.link = "logit"),      data = na.omit(sub_et_qu2)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  logit
# Mu Coefficients:
#                                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                            -1.567e+00  5.057e-02 -30.985  < 2e-16 ***
# IA_LABEL_EV                            -5.585e-02  1.571e-02  -3.554 0.000383 ***
# IA_LABEL_PR_LvsH                       -4.177e-03  6.673e-03  -0.626 0.531378    
# IA_LABEL_PR_SvsF                        9.041e-02  2.216e-02   4.080 4.58e-05 ***
# pb(subject_nr_1)                        1.704e-05  6.197e-06   2.749 0.005993 ** 
# pb(block_loop)                          7.444e-03  3.408e-03   2.184 0.028990 *  
# pb(Change_category01, by = block_loop) -2.106e-02  2.460e-02  -0.856 0.392045    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                       Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          -0.948293   0.025020 -37.901   <2e-16 ***
# guessed_block_rule_1  0.009434   0.027852   0.339    0.735    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms maybe are not accurate. 
# ------------------------------------------------------------------
# No. of observations in the fit:  4584 
# Degrees of Freedom for the fit:  13.12947
#       Residual Deg. of Freedom:  4570.871 
#                       at cycle:  6 
#  
# Global Deviance:     -7872.093 
#             AIC:     -7845.834 
#             SBC:     -7761.407  

# Back-transformed coefficients
print(round(inv.logit(bm1_dwell_time_early_1$mu.coefficients),4))
# (Intercept)                            IA_LABEL_EV 
# 0.1727                                 0.4860 
# IA_LABEL_PR_LvsH                       IA_LABEL_PR_SvsF 
# 0.4990                                 0.5226 
# pb(subject_nr_1)                         pb(block_loop) 
# 0.5000                                 0.5019 
# pb(Change_category01, by = block_loop) 
# 0.4947

# Model comparison
lrtest(bm0_dwell_time_early_1, bm1_dwell_time_early_1)
#      #Df LogLik     Df  Chisq Pr(>Chisq)    
# 1 10.144 3927.2                             
# 2 13.129 3936.0 2.9858 17.637  0.0005227 ***

AIC(bm0_dwell_time_early_1, bm1_dwell_time_early_1)
#                              df       AIC
# bm1_dwell_time_early_1 13.12947 -7845.834
# bm0_dwell_time_early_1 10.14364 -7834.169

# Full Model: Perf_Strivings
bm2.1_dwell_time_early_1 <- gamlss(
  dwell_time_early_1_t01 ~ 
    IA_LABEL_EV * Perf_Strivings_c + 
    IA_LABEL_PR_LvsH * Perf_Strivings_c + 
    IA_LABEL_PR_SvsF * Perf_Strivings_c + 
    pb(subject_nr_1) +   
    pb(block_loop) +  
    pb(Change_category01, by = block_loop), 
  sigma.formula = ~ guessed_block_rule_1,  
  family = BE(mu.link = "logit"),
  data = na.omit(sub_et_qu2)
) 
summary(bm2.1_dwell_time_early_1)
# Family:  c("BE", "Beta") 
# 
# Call:  gamlss(formula = dwell_time_early_1_t01 ~ IA_LABEL_EV *  
#     Perf_Strivings_c + IA_LABEL_PR_LvsH * Perf_Strivings_c +  
#     IA_LABEL_PR_SvsF * Perf_Strivings_c + pb(subject_nr_1) +  
#     pb(block_loop) + pb(Change_category01, by = block_loop),  
#     sigma.formula = ~guessed_block_rule_1, family = BE(mu.link = "logit"),      data = na.omit(sub_et_qu2)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  logit
# Mu Coefficients:
#                                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                            -1.571e+00  5.092e-02 -30.859  < 2e-16 ***
# IA_LABEL_EV                            -5.611e-02  1.575e-02  -3.562 0.000372 ***
# Perf_Strivings_c                       -5.117e-03  6.688e-03  -0.765 0.444234    
# IA_LABEL_PR_LvsH                       -4.131e-03  6.803e-03  -0.607 0.543699    
# IA_LABEL_PR_SvsF                        9.040e-02  2.219e-02   4.074  4.7e-05 ***
# pb(subject_nr_1)                        1.760e-05  6.244e-06   2.818 0.004854 ** 
# pb(block_loop)                          7.487e-03  3.420e-03   2.189 0.028629 *  
# pb(Change_category01, by = block_loop) -2.124e-02  2.513e-02  -0.846 0.397871    
# IA_LABEL_EV:Perf_Strivings_c            6.166e-03  8.694e-03   0.709 0.478256    
# Perf_Strivings_c:IA_LABEL_PR_LvsH      -7.341e-04  4.803e-03  -0.153 0.878516    
# Perf_Strivings_c:IA_LABEL_PR_SvsF       1.620e-02  1.215e-02   1.333 0.182676    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                      Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          -0.95214    0.02596 -36.676   <2e-16 ***
# guessed_block_rule_1  0.01293    0.02925   0.442    0.659    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms maybe are not accurate. 
# ------------------------------------------------------------------
# No. of observations in the fit:  4584 
# Degrees of Freedom for the fit:  17.1176
#       Residual Deg. of Freedom:  4566.882 
#                       at cycle:  6 
#  
# Global Deviance:     -7880.313 
#             AIC:     -7846.078 
#             SBC:     -7736.006

# Back-transformed coefficients
print(round(inv.logit(bm2.1_dwell_time_early_1$mu.coefficients),4))
# (Intercept)                            IA_LABEL_EV 
# 0.1720                                 0.4860 
# Perf_Strivings_c                       IA_LABEL_PR_LvsH 
# 0.4987                                 0.4990 
# IA_LABEL_PR_SvsF                       pb(subject_nr_1) 
# 0.5226                                 0.5000 
# pb(block_loop) pb(Change_category01, by = block_loop) 
# 0.5019                                 0.4947 
# IA_LABEL_EV:Perf_Strivings_c      Perf_Strivings_c:IA_LABEL_PR_LvsH 
# 0.5015                                 0.4998 
# Perf_Strivings_c:IA_LABEL_PR_SvsF 
# 0.5040 

# Model comparison
lrtest(bm2.1_dwell_time_early_1, bm1_dwell_time_early_1)
#      #Df LogLik      Df  Chisq Pr(>Chisq)  
# 1 17.118 3940.2                            
# 2 13.129 3936.0 -3.9881 8.2197    0.08385 .

AIC(bm2.1_dwell_time_early_1, bm1_dwell_time_early_1)
#                                df       AIC
# bm2.1_dwell_time_early_1 17.11760 -7846.078
# bm1_dwell_time_early_1   13.12947 -7845.834

# Full Model: Perf_Concerns 
bm2.2_dwell_time_early_1 <- gamlss(
  dwell_time_early_1_t01 ~ 
    IA_LABEL_EV * Perf_Concerns_c + 
    IA_LABEL_PR_LvsH * Perf_Concerns_c + 
    IA_LABEL_PR_SvsF * Perf_Concerns_c + 
    pb(subject_nr_1) +   
    pb(block_loop) +  
    pb(Change_category01, by = block_loop), 
  sigma.formula = ~ guessed_block_rule_1,  
  family = BE(mu.link = "logit"),
  data = na.omit(sub_et_qu2)
) 
summary(bm2.2_dwell_time_early_1)
# Family:  c("BE", "Beta") 
# 
# Call:  gamlss(formula = dwell_time_early_1_t01 ~ IA_LABEL_EV * Perf_Concerns_c +  
#     IA_LABEL_PR_LvsH * Perf_Concerns_c + IA_LABEL_PR_SvsF * Perf_Concerns_c +  
#     pb(subject_nr_1) + pb(block_loop) + pb(Change_category01,  
#     by = block_loop), sigma.formula = ~guessed_block_rule_1,  
#     family = BE(mu.link = "logit"), data = na.omit(sub_et_qu2)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  logit
# Mu Coefficients:
#                                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                            -1.567e+00  5.025e-02 -31.184  < 2e-16 ***
# IA_LABEL_EV                            -5.584e-02  1.559e-02  -3.581 0.000346 ***
# Perf_Concerns_c                        -2.429e-03  4.725e-03  -0.514 0.607201    
# IA_LABEL_PR_LvsH                       -4.159e-03  6.720e-03  -0.619 0.535999    
# IA_LABEL_PR_SvsF                        9.041e-02  2.208e-02   4.095  4.3e-05 ***
# pb(subject_nr_1)                        1.705e-05  6.176e-06   2.761 0.005794 ** 
# pb(block_loop)                          7.431e-03  3.408e-03   2.181 0.029267 *  
# pb(Change_category01, by = block_loop) -2.117e-02  2.474e-02  -0.856 0.392135    
# IA_LABEL_EV:Perf_Concerns_c            -8.850e-04  6.548e-03  -0.135 0.892497    
# Perf_Concerns_c:IA_LABEL_PR_LvsH       -2.118e-03  2.822e-03  -0.751 0.452955    
# Perf_Concerns_c:IA_LABEL_PR_SvsF        7.669e-04  9.271e-03   0.083 0.934081    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                       Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          -0.948049   0.025561 -37.090   <2e-16 ***
# guessed_block_rule_1  0.009013   0.029296   0.308    0.758    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms may not be reliable. 
# ------------------------------------------------------------------
# No. of observations in the fit:  4584 
# Degrees of Freedom for the fit:  17.12318
#       Residual Deg. of Freedom:  4566.877 
#                       at cycle:  6 
#  
# Global Deviance:     -7872.651 
#             AIC:     -7838.404 
#             SBC:     -7728.297

# Back-transformed coefficients
print(round(inv.logit(bm2.2_dwell_time_early_1$mu.coefficients),4))
# (Intercept)                            IA_LABEL_EV 
# 0.1727                                 0.4860 
# Perf_Concerns_c                       IA_LABEL_PR_LvsH 
# 0.4994                                 0.4990 
# IA_LABEL_PR_SvsF                       pb(subject_nr_1) 
# 0.5226                                 0.5000 
# pb(block_loop) pb(Change_category01, by = block_loop) 
# 0.5019                                 0.4947 
# IA_LABEL_EV:Perf_Concerns_c       Perf_Concerns_c:IA_LABEL_PR_LvsH 
# 0.4998                                 0.4995 
# Perf_Concerns_c:IA_LABEL_PR_SvsF 
# 0.5002

# Model comparison
lrtest(bm2.2_dwell_time_early_1, bm1_dwell_time_early_1)
#      #Df LogLik      Df  Chisq Pr(>Chisq)
# 1 17.123 3936.3                          
# 2 13.129 3936.0 -3.9937 0.5576     0.9677

AIC(bm2.2_dwell_time_early_1, bm1_dwell_time_early_1)
#                                df       AIC
# bm1_dwell_time_early_1   13.12947 -7845.834
# bm2.2_dwell_time_early_1 17.12318 -7838.404

# Intermediate
#-------------

# Null model
bm0_dwell_time_intermediate_1 <- gamlss(
  dwell_time_intermediate_1_t01 ~ 
    pb(subject_nr_1) +   
    pb(block_loop)+  
    pb(Change_category01, by = block_loop), 
  sigma.formula = ~ guessed_block_rule_1,  
  family = BE(mu.link = "logit"),   
  data = na.omit(sub_et_qu2)
) 
summary(bm0_dwell_time_intermediate_1)
# Family:  c("BE", "Beta") 
# 
# Call:  gamlss(formula = dwell_time_intermediate_1_t01 ~ pb(subject_nr_1) +  
#     pb(block_loop) + pb(Change_category01, by = block_loop),  
#     sigma.formula = ~guessed_block_rule_1, family = BE(mu.link = "logit"),      data = na.omit(sub_et_qu2)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  logit
# Mu Coefficients:
#                                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                            -1.526e+00  3.521e-02 -43.324   <2e-16 ***
# pb(subject_nr_1)                        3.165e-06  4.297e-06   0.737    0.461    
# pb(block_loop)                          9.764e-04  2.350e-03   0.416    0.678    
# pb(Change_category01, by = block_loop)  5.201e-01  1.627e-02  31.968   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                      Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          -1.59996    0.02486 -64.362  < 2e-16 ***
# guessed_block_rule_1  0.18991    0.02872   6.614 4.18e-11 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms maybe are not accurate. 
# ------------------------------------------------------------------
# No. of observations in the fit:  4584 
# Degrees of Freedom for the fit:  6.036066
#       Residual Deg. of Freedom:  4577.964 
#                       at cycle:  5 
#  
# Global Deviance:     -11026.95 
#             AIC:     -11014.87 
#             SBC:     -10976.06

# Back-transformed coefficients
print(round(inv.logit(bm0_dwell_time_intermediate_1$mu.coefficients),4))
# (Intercept)                       pb(subject_nr_1) 
# 0.1786                                 0.5000 
# pb(block_loop) pb(Change_category01, by = block_loop) 
# 0.5002                                 0.6272  

# Partial Model
bm1_dwell_time_intermediate_1 <- gamlss(
  dwell_time_intermediate_1_t01 ~ 
    IA_LABEL_EV * Perf_Strivings_c + 
    IA_LABEL_PR_LvsH * Perf_Strivings_c + 
    IA_LABEL_PR_SvsF * Perf_Strivings_c + 
    pb(subject_nr_1) +   
    pb(block_loop) +  
    pb(Change_category01, by = block_loop), 
  sigma.formula = ~ guessed_block_rule_1,  
  family = BE(mu.link = "logit"),   
  data = na.omit(sub_et_qu2)
) 
summary(bm1_dwell_time_intermediate_1)
# Family:  c("BE", "Beta") 
# 
# Call:  gamlss(formula = dwell_time_intermediate_1_t01 ~ dwell_time_intermediate_1 +  
#     Condition + IA_LABEL_EV * Perf_Strivings_c + pb(subject_nr_1) +  
#     pb(block_loop) + pb(Change_category01, by = block_loop),  
#     sigma.formula = ~guessed_block_rule_1 + Condition, family = BE(mu.link = "logit"),  
#     data = na.omit(sub_et_qu2)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  logit
# Mu Coefficients:
#                                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                            -1.964e+00  4.745e-02 -41.397   <2e-16 ***
# dwell_time_intermediate_1               1.669e+00  1.022e-01  16.335   <2e-16 ***
# ConditionFailure                        2.327e-02  2.184e-02   1.066    0.287    
# ConditionSuccess                        1.394e-02  1.993e-02   0.700    0.484    
# IA_LABEL_EV                            -9.637e-04  9.382e-03  -0.103    0.918    
# Perf_Strivings_c                       -5.233e-03  4.624e-03  -1.132    0.258    
# pb(subject_nr_1)                        2.425e-06  5.299e-06   0.458    0.647    
# pb(block_loop)                          9.859e-04  2.921e-03   0.338    0.736    
# pb(Change_category01, by = block_loop)  8.969e-01  2.134e-02  42.029   <2e-16 ***
# IA_LABEL_EV:Perf_Strivings_c           -1.787e-03  5.081e-03  -0.352    0.725    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                      Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          -1.39006    0.04453 -31.215  < 2e-16 ***
# guessed_block_rule_1  0.14331    0.04234   3.385 0.000719 ***
# ConditionFailure      0.22281    0.03067   7.263 4.41e-13 ***
# ConditionSuccess      0.01168    0.02970   0.393 0.694120    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms may not be reliable. 
# ------------------------------------------------------------------
# No. of observations in the fit:  4584 
# Degrees of Freedom for the fit:  14.02507
#       Residual Deg. of Freedom:  4569.975 
#                       at cycle:  6 
#  
# Global Deviance:     -9551.657 
#             AIC:     -9523.607 
#             SBC:     -9433.421

# Back-transformed coefficients
print(round(inv.logit(bm1_dwell_time_intermediate_1$mu.coefficients),4))
# (Intercept)              dwell_time_intermediate_1 
# 0.1230                                 0.8414 
# ConditionFailure                       ConditionSuccess 
# 0.5058                                 0.5035 
# IA_LABEL_EV                       Perf_Strivings_c 
# 0.4998                                 0.4987 
# pb(subject_nr_1)                         pb(block_loop) 
# 0.5000                                 0.5002 
# pb(Change_category01, by = block_loop)           IA_LABEL_EV:Perf_Strivings_c 
# 0.7103                                 0.4996

# Model comparison
lrtest(bm0_dwell_time_intermediate_1, bm1_dwell_time_intermediate_1)
#      #Df LogLik     Df  Chisq Pr(>Chisq)    
# 1  7.0592 4741.6                             
# 2 14.0251 4775.8 6.9659 68.458  3.026e-12 ***

AIC(bm0_dwell_time_intermediate_1, bm1_dwell_time_intermediate_1)
#                                        df       AIC
# bm1_dwell_time_intermediate_1 14.025074 -9523.607
# bm0_dwell_time_intermediate_1    7.059159 -9469.081

# Partial model 
bm1_dwell_time_intermediate_1 <- gamlss(
  dwell_time_intermediate_1_t01 ~ 
    IA_LABEL_EV + 
    IA_LABEL_PR_LvsH + 
    IA_LABEL_PR_SvsF + 
    pb(subject_nr_1) +   
    pb(block_loop) +  
    pb(Change_category01, by = block_loop), 
  sigma.formula = ~ guessed_block_rule_1,  
  family = BE(mu.link = "logit"),   
  data = na.omit(sub_et_qu2)
) 
summary(bm1_dwell_time_intermediate_1)
# Family:  c("BE", "Beta") 
# 
# Call:  gamlss(formula = dwell_time_intermediate_1_t01 ~ IA_LABEL_EV +  
#     IA_LABEL_PR_LvsH + IA_LABEL_PR_SvsF + pb(subject_nr_1) +  
#     pb(block_loop) + pb(Change_category01, by = block_loop),  
#     sigma.formula = ~guessed_block_rule_1, family = BE(mu.link = "logit"),      data = na.omit(sub_et_qu2)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  logit
# Mu Coefficients:
#                                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                            -1.528e+00  3.563e-02 -42.877   <2e-16 ***
# IA_LABEL_EV                            -1.248e-02  1.083e-02  -1.152    0.250    
# IA_LABEL_PR_LvsH                       -2.524e-03  4.552e-03  -0.555    0.579    
# IA_LABEL_PR_SvsF                       -2.094e-02  1.535e-02  -1.364    0.172    
# pb(subject_nr_1)                        3.167e-06  4.397e-06   0.720    0.471    
# pb(block_loop)                          1.021e-03  2.368e-03   0.431    0.666    
# pb(Change_category01, by = block_loop)  5.196e-01  1.625e-02  31.981   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                      Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          -1.60356    0.02484 -64.563  < 2e-16 ***
# guessed_block_rule_1  0.19273    0.02868   6.719 2.05e-11 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms maybe are not accurate. 
# ------------------------------------------------------------------
# No. of observations in the fit:  4584 
# Degrees of Freedom for the fit:  9.036148
#       Residual Deg. of Freedom:  4574.964 
#                       at cycle:  5 
#  
# Global Deviance:     -11038.18 
#             AIC:     -11020.11 
#             SBC:     -10962   

# Back-transformed coefficients
print(round(inv.logit(bm1_dwell_time_intermediate_1$mu.coefficients),4))
# (Intercept)                            IA_LABEL_EV 
# 0.1783                                 0.4969 
# IA_LABEL_PR_LvsH                       IA_LABEL_PR_SvsF 
# 0.4994                                 0.4948 
# pb(subject_nr_1)                         pb(block_loop) 
# 0.5000                                 0.5003 
# pb(Change_category01, by = block_loop) 
# 0.6271 

# Model comparison
lrtest(bm0_dwell_time_intermediate_1, bm1_dwell_time_intermediate_1)
#      #Df LogLik     Df  Chisq Pr(>Chisq)  
# 1 6.0361 5513.5                           
# 2 9.0361 5519.1 3.0001 11.235    0.01052 *

AIC(bm0_dwell_time_intermediate_1, bm1_dwell_time_intermediate_1)
#                                     df       AIC
# bm1_dwell_time_intermediate_1 9.036148 -11020.11
# bm0_dwell_time_intermediate_1 6.036066 -11014.87

# Full Model: Perf_Strivings
bm2.1_dwell_time_intermediate_1 <- gamlss(
  dwell_time_intermediate_1_t01 ~ 
    IA_LABEL_EV * Perf_Strivings_c + 
    IA_LABEL_PR_LvsH * Perf_Strivings_c + 
    IA_LABEL_PR_SvsF * Perf_Strivings_c + 
    pb(subject_nr_1) +   
    pb(block_loop) +  
    pb(Change_category01, by = block_loop), 
  sigma.formula = ~ guessed_block_rule_1,  
  family = BE(mu.link = "logit"),   
  data = na.omit(sub_et_qu2)
) 
summary(bm2.1_dwell_time_intermediate_1)
# Family:  c("BE", "Beta") 
# 
# Call:  gamlss(formula = dwell_time_intermediate_1_t01 ~ IA_LABEL_EV *  
#     Perf_Strivings_c + IA_LABEL_PR_LvsH * Perf_Strivings_c +  
#     IA_LABEL_PR_SvsF * Perf_Strivings_c + pb(subject_nr_1) +  
#     pb(block_loop) + pb(Change_category01, by = block_loop),  
#     sigma.formula = ~guessed_block_rule_1, family = BE(mu.link = "logit"),      data = na.omit(sub_et_qu2)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  logit
# Mu Coefficients:
#                                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                            -1.529e+00  3.564e-02 -42.909   <2e-16 ***
# IA_LABEL_EV                            -1.246e-02  1.070e-02  -1.164   0.2446    
# Perf_Strivings_c                       -2.357e-04  1.741e-03  -0.135   0.8923    
# IA_LABEL_PR_LvsH                       -2.531e-03  4.576e-03  -0.553   0.5802    
# IA_LABEL_PR_SvsF                       -2.096e-02  1.517e-02  -1.382   0.1672    
# pb(subject_nr_1)                        3.370e-06  4.279e-06   0.788   0.4309    
# pb(block_loop)                          9.754e-04  2.399e-03   0.407   0.6844    
# pb(Change_category01, by = block_loop)  5.196e-01  1.624e-02  31.985   <2e-16 ***
# IA_LABEL_EV:Perf_Strivings_c           -9.809e-03  5.679e-03  -1.727   0.0842 .  
# Perf_Strivings_c:IA_LABEL_PR_LvsH       2.130e-03  2.290e-03   0.930   0.3523    
# Perf_Strivings_c:IA_LABEL_PR_SvsF       8.475e-03  8.049e-03   1.053   0.2924    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                      Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          -1.60355    0.02485 -64.523  < 2e-16 ***
# guessed_block_rule_1  0.19203    0.02871   6.689 2.51e-11 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms maybe are not accurate. 
# ------------------------------------------------------------------
# No. of observations in the fit:  4584 
# Degrees of Freedom for the fit:  13.03615
#       Residual Deg. of Freedom:  4570.964 
#                       at cycle:  5 
#  
# Global Deviance:     -11042.15 
#             AIC:     -11016.08 
#             SBC:     -10932.25

# Back-transformed coefficients
print(round(inv.logit(bm2.1_dwell_time_intermediate_1$mu.coefficients),4))
# (Intercept)                            IA_LABEL_EV 
# 0.1781                                 0.4969 
# Perf_Strivings_c                       IA_LABEL_PR_LvsH 
# 0.4999                                 0.4994 
# IA_LABEL_PR_SvsF                       pb(subject_nr_1) 
# 0.4948                                 0.5000 
# pb(block_loop) pb(Change_category01, by = block_loop) 
# 0.5002                                 0.6270 
# IA_LABEL_EV:Perf_Strivings_c      Perf_Strivings_c:IA_LABEL_PR_LvsH 
# 0.4975                                 0.5005 
# Perf_Strivings_c:IA_LABEL_PR_SvsF 
# 0.5021 

# Model comparison
lrtest(bm1_dwell_time_intermediate_1, bm2.1_dwell_time_intermediate_1)
#       #Df LogLik Df  Chisq Pr(>Chisq)
# 1  9.0361 5519.1                     
# 2 13.0361 5521.1  4 3.9724     0.4098

AIC(bm1_dwell_time_intermediate_1, bm2.1_dwell_time_intermediate_1)
#                                        df       AIC
# bm1_dwell_time_intermediate_1    9.036148 -11020.11
# bm2.1_dwell_time_intermediate_1 13.036146 -11016.08

# Full Model: Perf_Concerns
bm2.2_dwell_time_intermediate_1 <- gamlss(
  dwell_time_intermediate_1_t01 ~ 
    IA_LABEL_EV * Perf_Concerns_c + 
    IA_LABEL_PR_LvsH * Perf_Concerns_c + 
    IA_LABEL_PR_SvsF * Perf_Concerns_c + 
    pb(subject_nr_1) +   
    pb(block_loop) +  
    pb(Change_category01, by = block_loop), 
  sigma.formula = ~ guessed_block_rule_1,  
  family = BE(mu.link = "logit"),   
  data = na.omit(sub_et_qu2)
)  
summary(bm2.2_dwell_time_intermediate_1)
# Family:  c("BE", "Beta") 
# 
# Call:  gamlss(formula = dwell_time_intermediate_1_t01 ~ IA_LABEL_EV *  
#     Perf_Concerns_c + IA_LABEL_PR_LvsH * Perf_Concerns_c +  
#     IA_LABEL_PR_SvsF * Perf_Concerns_c + pb(subject_nr_1) +  
#     pb(block_loop) + pb(Change_category01, by = block_loop),  
#     sigma.formula = ~guessed_block_rule_1, family = BE(mu.link = "logit"),      data = na.omit(sub_et_qu2)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  logit
# Mu Coefficients:
#                                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                            -1.528e+00  3.465e-02 -44.091   <2e-16 ***
# IA_LABEL_EV                            -1.241e-02  1.072e-02  -1.157   0.2473    
# Perf_Concerns_c                         2.534e-03  3.199e-03   0.792   0.4284    
# IA_LABEL_PR_LvsH                       -2.492e-03  4.692e-03  -0.531   0.5954    
# IA_LABEL_PR_SvsF                       -2.096e-02  1.515e-02  -1.384   0.1663    
# pb(subject_nr_1)                        3.192e-06  4.317e-06   0.739   0.4597    
# pb(block_loop)                          9.493e-04  2.255e-03   0.421   0.6738    
# pb(Change_category01, by = block_loop)  5.195e-01  1.625e-02  31.979   <2e-16 ***
# IA_LABEL_EV:Perf_Concerns_c            -6.174e-03  4.473e-03  -1.380   0.1675    
# Perf_Concerns_c:IA_LABEL_PR_LvsH        3.979e-03  1.934e-03   2.058   0.0397 *  
# Perf_Concerns_c:IA_LABEL_PR_SvsF        1.054e-02  6.310e-03   1.670   0.0950 .  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                      Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          -1.60341    0.02488 -64.442  < 2e-16 ***
# guessed_block_rule_1  0.19133    0.02875   6.654 3.18e-11 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms maybe are not accurate. 
# ------------------------------------------------------------------
# No. of observations in the fit:  4584 
# Degrees of Freedom for the fit:  13.03619
#       Residual Deg. of Freedom:  4570.964 
#                       at cycle:  6 
#  
# Global Deviance:     -11045.2 
#             AIC:     -11019.13 
#             SBC:     -10935.3 

# Back-transformed coefficients
print(round(inv.logit(bm2.2_dwell_time_intermediate_1$mu.coefficients),4))
# (Intercept)                            IA_LABEL_EV 
# 0.1783                                 0.4969 
# Perf_Concerns_c                       IA_LABEL_PR_LvsH 
# 0.5006                                 0.4994 
# IA_LABEL_PR_SvsF                       pb(subject_nr_1) 
# 0.4948                                 0.5000 
# pb(block_loop) pb(Change_category01, by = block_loop) 
# 0.5002                                 0.6270 
# IA_LABEL_EV:Perf_Concerns_c       Perf_Concerns_c:IA_LABEL_PR_LvsH 
# 0.4985                                 0.5010 
# Perf_Concerns_c:IA_LABEL_PR_SvsF 
# 0.5026

# Model comparison
lrtest(bm1_dwell_time_intermediate_1, bm2.2_dwell_time_intermediate_1)
#       #Df LogLik Df  Chisq Pr(>Chisq)
# 1  9.0361 5519.1                     
# 2 13.0362 5522.6  4 7.0163      0.135

AIC(bm1_dwell_time_intermediate_1, bm2.2_dwell_time_intermediate_1)
#                                        df       AIC
# bm1_dwell_time_intermediate_1    9.036148 -11020.11
# bm2.2_dwell_time_intermediate_1 13.036191 -11019.13

# Simulate predicted values
preds_dwell_time_intermediate_PS <- ggpredict(bm2.1_dwell_time_intermediate_1, terms = c("Perf_Strivings_c", "IA_LABEL_EV"))
preds_dwell_time_intermediate_PC <- ggpredict(bm2.2_dwell_time_intermediate_1, terms = c("Perf_Concerns_c", "IA_LABEL_PR_LvsH"))

# Plot the retained interactions 
dti_PS <- ggplot(preds_dwell_time_intermediate_PS, aes(x = x, y = predicted, color = group)) +
  geom_line(size = 1) +                                # Line only, no CI ribbon
  scale_color_manual(
    values = c("-1" = "purple", "0" = "lightgreen", "1" = "skyblue"),         # Customize colors by group
    labels = c("Positive", "Neutral", "Negative")         # Custom legend labels
  ) +
  labs(
    x = "Perfectionistic Strivings",
    y = "",
    color = "Emotional Valence"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )

dti_PC <- ggplot(preds_dwell_time_intermediate_PC, aes(x = x, y = predicted, color = group)) +
  geom_line(size = 1) +                                # Line only, no CI ribbon
  scale_color_manual(
    values = c("-2" = "grey", "1" = "gold"),         # Customize colors by group
    labels = c("Irrelevant", "Relevant")         # Custom legend labels
  ) +
  labs(
    x = "Perfectionistic Concerns",
    y = "",
    color = "Performance Relevance"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )

x11(width = 12, height = 4)
grid.arrange(dti_PS, dti_PC,
             ncol = 2, nrow = 1,
             top = textGrob("Predicted Dwell Time (Intermediate, t1)",
                            gp = gpar(fontsize = 16, fontface = "bold")
                            )
             )

# Late
#-----

# Null model
bm0_dwell_time_late_1 <- gamlss(
  dwell_time_late_1_t01 ~ 
    pb(subject_nr_1) +   
    pb(block_loop)+  
    pb(Change_category01, by = block_loop), 
  sigma.formula = ~ guessed_block_rule_1 + Condition,  
  family = BE(mu.link = "logit"),    
  data = na.omit(sub_et_qu2) 
) 
summary(bm0_dwell_time_late_1) 
# Family:  c("BE", "Beta") 
# 
# Call:  gamlss(formula = dwell_time_late_1_t01 ~ pb(subject_nr_1) +  
#     pb(block_loop) + pb(Change_category01, by = block_loop),  
#     sigma.formula = ~guessed_block_rule_1 + Condition,  
#     family = BE(mu.link = "logit"), data = na.omit(sub_et_qu2)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  logit
# Mu Coefficients:
#                                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                            -1.856e+00  6.011e-02 -30.874   <2e-16 ***
# pb(subject_nr_1)                        1.903e-05  7.420e-06   2.565   0.0104 *  
# pb(block_loop)                          6.281e-03  4.143e-03   1.516   0.1296    
# pb(Change_category01, by = block_loop)  1.321e+00  2.983e-02  44.290   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                      Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          -0.88430    0.03459 -25.562  < 2e-16 ***
# guessed_block_rule_1  0.32713    0.03188  10.261  < 2e-16 ***
# ConditionFailure      0.21432    0.03186   6.727 1.94e-11 ***
# ConditionSuccess     -0.03423    0.03171  -1.079     0.28    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms maybe are not accurate. 
# ------------------------------------------------------------------
# No. of observations in the fit:  4584 
# Degrees of Freedom for the fit:  8.011947
#       Residual Deg. of Freedom:  4575.988 
#                       at cycle:  7 
#  
# Global Deviance:     -7396.445 
#             AIC:     -7380.422 
#             SBC:     -7328.902

# Back-transformed coefficients
print(round(inv.logit(bm0_dwell_time_late_1$mu.coefficients),4))
# (Intercept)                       pb(subject_nr_1) 
# 0.1352                                 0.5000 
# pb(block_loop) pb(Change_category01, by = block_loop) 
# 0.5016                                 0.7894 

# Partial Model
bm1_dwell_time_late_1 <- gamlss(
  dwell_time_late_1_t01 ~ 
    IA_LABEL_EV + 
    IA_LABEL_PR_LvsH + 
    IA_LABEL_PR_SvsF + 
    pb(subject_nr_1) +   
    pb(block_loop) +  
    pb(Change_category01, by = block_loop), 
  sigma.formula = ~ guessed_block_rule_1,  
  family = BE(mu.link = "logit"),   
  data = na.omit(sub_et_qu2) 
) 
summary(bm1_dwell_time_late_1)
# Family:  c("BE", "Beta") 
# 
# Call:  gamlss(formula = dwell_time_late_1_t01 ~ IA_LABEL_EV +  
#     IA_LABEL_PR_LvsH + IA_LABEL_PR_SvsF + pb(subject_nr_1) +  
#     pb(block_loop) + pb(Change_category01, by = block_loop),  
#     sigma.formula = ~guessed_block_rule_1, family = BE(mu.link = "logit"),      data = na.omit(sub_et_qu2)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  logit
# Mu Coefficients:
#                                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                            -1.839e+00  6.129e-02 -30.011   <2e-16 ***
# IA_LABEL_EV                             1.809e-02  1.934e-02   0.935   0.3497    
# IA_LABEL_PR_LvsH                        8.912e-03  8.209e-03   1.086   0.2777    
# IA_LABEL_PR_SvsF                       -3.390e-02  2.717e-02  -1.248   0.2122    
# pb(subject_nr_1)                        1.735e-05  7.566e-06   2.293   0.0219 *  
# pb(block_loop)                          5.419e-03  4.187e-03   1.294   0.1957    
# pb(Change_category01, by = block_loop)  1.352e+00  2.996e-02  45.138   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                      Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          -0.84354    0.02678  -31.50   <2e-16 ***
# guessed_block_rule_1  0.35685    0.03147   11.34   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms maybe are not accurate. 
# ------------------------------------------------------------------
# No. of observations in the fit:  4584 
# Degrees of Freedom for the fit:  9.011524
#       Residual Deg. of Freedom:  4574.988 
#                       at cycle:  7 
#  
# Global Deviance:     -7324.597 
#             AIC:     -7306.574 
#             SBC:     -7248.627

# Back-transformed coefficients
print(round(inv.logit(bm1_dwell_time_late_1 $mu.coefficients),4))
# (Intercept)                            IA_LABEL_EV 
# 0.1371                                 0.5045 
# IA_LABEL_PR_LvsH                       IA_LABEL_PR_SvsF 
# 0.5022                                 0.4915 
# pb(subject_nr_1)                         pb(block_loop) 
# 0.5000                                 0.5014 
# pb(Change_category01, by = block_loop) 
# 0.7945

# Model comparison
lrtest(bm0_dwell_time_late_1, bm1_dwell_time_late_1 )
#      #Df LogLik      Df  Chisq Pr(>Chisq)    
# 1 8.0119 3698.2                              
# 2 9.0115 3662.3 0.99958 71.848  < 2.2e-16 ***

AIC(bm0_dwell_time_late_1, bm1_dwell_time_late_1 )
#                             df       AIC
# bm0_dwell_time_late_1 8.011947 -7380.422
# bm1_dwell_time_late_1 9.011524 -7306.574

# Full model: Perf_Strivings
bm2.1_dwell_time_late_1 <- gamlss(
  dwell_time_late_1_t01 ~ 
    IA_LABEL_EV * Perf_Strivings_c + 
    IA_LABEL_PR_LvsH * Perf_Strivings_c  + 
    IA_LABEL_PR_SvsF * Perf_Strivings_c  + 
    pb(subject_nr_1) +   
    pb(block_loop) +  
    pb(Change_category01, by = block_loop), 
  sigma.formula = ~ guessed_block_rule_1,  
  family = BE(mu.link = "logit"),   
  data = na.omit(sub_et_qu2)
) 
summary(bm2.1_dwell_time_late_1)
# Family:  c("BE", "Beta") 
# 
# Call:  gamlss(formula = dwell_time_late_1_t01 ~ IA_LABEL_EV * Perf_Strivings_c +  
#     IA_LABEL_PR_LvsH * Perf_Strivings_c + IA_LABEL_PR_SvsF *  
#     Perf_Strivings_c + pb(subject_nr_1) + pb(block_loop) + pb(Change_category01,  
#     by = block_loop), sigma.formula = ~guessed_block_rule_1,  
#     family = BE(mu.link = "logit"), data = na.omit(sub_et_qu2)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  logit
# Mu Coefficients:
#                                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                            -1.838e+00  6.255e-02 -29.379   <2e-16 ***
# IA_LABEL_EV                             1.816e-02  1.915e-02   0.948   0.3430    
# Perf_Strivings_c                        7.964e-03  7.379e-03   1.079   0.2805    
# IA_LABEL_PR_LvsH                        8.821e-03  8.231e-03   1.072   0.2839    
# IA_LABEL_PR_SvsF                       -3.406e-02  2.701e-02  -1.261   0.2074    
# pb(subject_nr_1)                        1.712e-05  7.665e-06   2.234   0.0256 *  
# pb(block_loop)                          5.342e-03  4.211e-03   1.269   0.2046    
# pb(Change_category01, by = block_loop)  1.352e+00  2.746e-02  49.247   <2e-16 ***
# IA_LABEL_EV:Perf_Strivings_c           -5.114e-04  1.025e-02  -0.050   0.9602    
# Perf_Strivings_c:IA_LABEL_PR_LvsH       9.175e-03  4.398e-03   2.086   0.0370 *  
# Perf_Strivings_c:IA_LABEL_PR_SvsF       6.694e-04  1.446e-02   0.046   0.9631    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                      Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          -0.84471    0.02555  -33.06   <2e-16 ***
# guessed_block_rule_1  0.35740    0.02940   12.16   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms may not be reliable. 
# ------------------------------------------------------------------
# No. of observations in the fit:  4584 
# Degrees of Freedom for the fit:  13.01153
#       Residual Deg. of Freedom:  4570.988 
#                       at cycle:  7 
#  
# Global Deviance:     -7329.181 
#             AIC:     -7303.158 
#             SBC:     -7219.489 

# Back-transformed coefficients
print(round(inv.logit(bm2.1_dwell_time_late_1$mu.coefficients),4))
# (Intercept)                            IA_LABEL_EV 
# 0.1373                                 0.5045 
# Perf_Strivings_c                       IA_LABEL_PR_LvsH 
# 0.5020                                 0.5022 
# IA_LABEL_PR_SvsF                       pb(subject_nr_1) 
# 0.4915                                 0.5000 
# pb(block_loop) pb(Change_category01, by = block_loop) 
# 0.5013                                 0.7945 
# IA_LABEL_EV:Perf_Strivings_c      Perf_Strivings_c:IA_LABEL_PR_LvsH 
# 0.4999                                 0.5023 
# Perf_Strivings_c:IA_LABEL_PR_SvsF 
# 0.5002 

# Model comparison
lrtest(bm1_dwell_time_late_1 , bm2.1_dwell_time_late_1)
#       #Df LogLik Df  Chisq Pr(>Chisq)
# 1  9.0115 3662.3                     
# 2 13.0115 3664.6  4 4.5835     0.3328

AIC(bm1_dwell_time_late_1 , bm2.1_dwell_time_late_1)
#                                df       AIC
# bm1_dwell_time_late_1    9.011524 -7306.574
# bm2.1_dwell_time_late_1 13.011534 -7303.158

# Full Model: Perf_Concerns
bm2.2_dwell_time_late_1  <- gamlss(
  dwell_time_late_1_t01 ~ 
    IA_LABEL_EV * Perf_Concerns_c + 
    IA_LABEL_PR_LvsH * Perf_Concerns_c  + 
    IA_LABEL_PR_SvsF * Perf_Concerns_c  + 
    pb(subject_nr_1) +   
    pb(block_loop) +  
    pb(Change_category01, by = block_loop), 
  sigma.formula = ~ guessed_block_rule_1,  
  family = BE(mu.link = "logit"),   
  data = na.omit(sub_et_qu2)
) 
summary(bm2.2_dwell_time_late_1 )
# Family:  c("BE", "Beta") 
# 
# Call:  gamlss(formula = dwell_time_late_1_t01 ~ IA_LABEL_EV * Perf_Concerns_c +  
#     IA_LABEL_PR_LvsH * Perf_Concerns_c + IA_LABEL_PR_SvsF * Perf_Concerns_c +  
#     pb(subject_nr_1) + pb(block_loop) + pb(Change_category01,  
#     by = block_loop), sigma.formula = ~guessed_block_rule_1,  
#     family = BE(mu.link = "logit"), data = na.omit(sub_et_qu2)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  logit
# Mu Coefficients:
#                                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                            -1.840e+00  6.236e-02 -29.513   <2e-16 ***
# IA_LABEL_EV                             1.812e-02  1.916e-02   0.946   0.3444    
# Perf_Concerns_c                         1.290e-03  5.793e-03   0.223   0.8238    
# IA_LABEL_PR_LvsH                        8.826e-03  8.232e-03   1.072   0.2837    
# IA_LABEL_PR_SvsF                       -3.403e-02  2.701e-02  -1.260   0.2079    
# pb(subject_nr_1)                        1.749e-05  7.633e-06   2.291   0.0220 *  
# pb(block_loop)                          5.347e-03  4.211e-03   1.270   0.2042    
# pb(Change_category01, by = block_loop)  1.352e+00  2.746e-02  49.242   <2e-16 ***
# IA_LABEL_EV:Perf_Concerns_c             1.886e-03  8.087e-03   0.233   0.8156    
# Perf_Concerns_c:IA_LABEL_PR_LvsH        6.210e-03  3.467e-03   1.791   0.0733 .  
# Perf_Concerns_c:IA_LABEL_PR_SvsF       -4.251e-04  1.140e-02  -0.037   0.9703    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                      Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          -0.84290    0.02555  -32.99   <2e-16 ***
# guessed_block_rule_1  0.35518    0.02940   12.08   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms may not be reliable. 
# ------------------------------------------------------------------
# No. of observations in the fit:  4584 
# Degrees of Freedom for the fit:  13.01152
#       Residual Deg. of Freedom:  4570.988 
#                       at cycle:  7 
#  
# Global Deviance:     -7328.65 
#             AIC:     -7302.627 
#             SBC:     -7218.959

# Back-transformed coefficients
print(round(inv.logit(bm2.2_dwell_time_late_1 $mu.coefficients),4))
# (Intercept)                            IA_LABEL_EV 
# 0.1370                                 0.5045 
# Perf_Concerns_c                       IA_LABEL_PR_LvsH 
# 0.5003                                 0.5022 
# IA_LABEL_PR_SvsF                       pb(subject_nr_1) 
# 0.4915                                 0.5000 
# pb(block_loop) pb(Change_category01, by = block_loop) 
# 0.5013                                 0.7945 
# IA_LABEL_EV:Perf_Concerns_c       Perf_Concerns_c:IA_LABEL_PR_LvsH 
# 0.5005                                 0.5016 
# Perf_Concerns_c:IA_LABEL_PR_SvsF 
# 0.4999 

# Model comparison
lrtest(bm1_dwell_time_late_1 , bm2.2_dwell_time_late_1 )
#      #Df LogLik Df  Chisq Pr(>Chisq)
# 1  9.0115 3662.3                     
# 2 13.0115 3664.3  4 4.0529     0.3989

AIC(bm1_dwell_time_late_1 , bm2.2_dwell_time_late_1 )
#                                df       AIC
# bm1_dwell_time_late_1    9.011524 -7306.574
# bm2.2_dwell_time_late_1 13.011524 -7302.627


# Simulate predicted values
preds_dwell_time_late_PS <- ggpredict(bm2.1_dwell_time_late_1, terms = c("Perf_Strivings_c", "IA_LABEL_PR_LvsH"))

# Plot the retained interaction 
dtl_PS <- ggplot(preds_dwell_time_late_PS, aes(x = x, y = predicted, color = group)) +
  geom_line(size = 1) +                                # Line only, no CI ribbon
  scale_color_manual(
    values = c("-2" = "grey", "1" = "gold"),         # Customize colors by group
    labels = c("Irrelevant", "Relevant")         # Custom legend labels
  ) +
  labs(
    x = "Perfectionistic Strivings",
    y = "",
    title = "",
    color = "Performance Relevance"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )

x11(width = 6, height = 4)
grid.arrange(dtl_PS,
             ncol = 1, nrow = 1,
             top = textGrob("Predicted Dwell Time (Late, t1)",
                            gp = gpar(fontsize = 16, fontface = "bold"))
            )

# Whole Trial
#------------

#Null model
bm0_dwell_time_1 <- gamlss(
  dwell_time_1 ~ 
    pb(subject_nr_1) +   
    pb(block_loop) +  
    pb(Change_category01, by = block_loop), 
  sigma.formula = ~ guessed_block_rule_1 + Condition,  
  family = BE( mu.link = "cauchit"), 
  data = na.omit(sub_et_qu2)
) 
summary(bm0_dwell_time_1)
# Family:  c("BE", "Beta") 
# 
# Call:  gamlss(formula = dwell_time_1 ~ pb(subject_nr_1) +  
#     pb(block_loop) + pb(Change_category01, by = block_loop),  
#     sigma.formula = ~guessed_block_rule_1 + Condition,  
#     family = BE(mu.link = "cauchit"), data = na.omit(sub_et_qu2)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  cauchit
# Mu Coefficients:
#                                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                            -1.507e+00  4.080e-02 -36.941  < 2e-16 ***
# pb(subject_nr_1)                       -2.118e-05  5.478e-06  -3.866 0.000112 ***
# pb(block_loop)                         -6.853e-05  4.533e-04  -0.151 0.879849    
# pb(Change_category01, by = block_loop)  1.032e+00  1.964e-02  52.545  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                      Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          -1.65275    0.03294 -50.176  < 2e-16 ***
# guessed_block_rule_1  0.18894    0.03021   6.254 4.37e-10 ***
# ConditionFailure      0.14478    0.03030   4.778 1.82e-06 ***
# ConditionSuccess     -0.03905    0.02996  -1.303    0.193    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms maybe are not accurate. 
# ------------------------------------------------------------------
# No. of observations in the fit:  4584 
# Degrees of Freedom for the fit:  8.230227
#       Residual Deg. of Freedom:  4575.77 
#                       at cycle:  6 
#  
# Global Deviance:     -11304.18 
#             AIC:     -11287.72 
#             SBC:     -11234.8 

# Back-transformed coefficients
print(round(inv.cauchit(bm0_dwell_time_1$mu.coefficients),4))
# (Intercept)                       pb(subject_nr_1) 
# 0.1865                                 0.5000 
# pb(block_loop) pb(Change_category01, by = block_loop) 
# 0.5000                                 0.7550 

# Partial Model
bm1_dwell_time_1 <- gamlss(
  dwell_time_1 ~ 
    IA_LABEL_EV + 
    IA_LABEL_PR_LvsH + 
    IA_LABEL_PR_SvsF + 
    pb(subject_nr_1) +   
    pb(block_loop) +  
    pb(Change_category01, by = block_loop), 
  sigma.formula = ~ guessed_block_rule_1,  
  family = BE(mu.link = "cauchit"), 
  data = na.omit(sub_et_qu2)
)
summary(bm1_dwell_time_1)
# Family:  c("BE", "Beta") 
# 
# Call:  gamlss(formula = dwell_time_1 ~ IA_LABEL_EV + IA_LABEL_PR_LvsH +  
#     IA_LABEL_PR_SvsF + pb(subject_nr_1) + pb(block_loop) +  
#     pb(Change_category01, by = block_loop), sigma.formula = ~guessed_block_rule_1,  
#     family = BE(mu.link = "cauchit"), data = na.omit(sub_et_qu2)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  cauchit
# Mu Coefficients:
#                                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                            -1.507e+00  4.493e-02 -33.546  < 2e-16 ***
# IA_LABEL_EV                             1.712e-02  1.484e-02   1.154   0.2486    
# IA_LABEL_PR_LvsH                        5.773e-03  6.612e-03   0.873   0.3827    
# IA_LABEL_PR_SvsF                       -4.181e-02  2.057e-02  -2.033   0.0421 *  
# pb(subject_nr_1)                       -2.234e-05  5.549e-06  -4.026 5.77e-05 ***
# pb(block_loop)                          9.156e-04  3.293e-03   0.278   0.7810    
# pb(Change_category01, by = block_loop)  1.052e+00  1.963e-02  53.572  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                      Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          -1.62973    0.02544 -64.055  < 2e-16 ***
# guessed_block_rule_1  0.20657    0.02993   6.901 5.88e-12 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms maybe are not accurate. 
# ------------------------------------------------------------------
# No. of observations in the fit:  4584 
# Degrees of Freedom for the fit:  9.243016
#       Residual Deg. of Freedom:  4574.757 
#                       at cycle:  6 
#  
# Global Deviance:     -11267.02 
#             AIC:     -11248.54 
#             SBC:     -11189.1 

# Back-transformed coefficients
print(round(inv.cauchit(bm1_dwell_time_1$mu.coefficients),4))
# (Intercept)                            IA_LABEL_EV 
# 0.1865                                 0.5054 
# IA_LABEL_PR_LvsH                       IA_LABEL_PR_SvsF 
# 0.5018                                 0.4867 
# pb(subject_nr_1)                         pb(block_loop) 
# 0.5000                                 0.5003 
# pb(Change_category01, by = block_loop) 
# 0.7580

# Model comparison
lrtest(bm0_dwell_time_1, bm1_dwell_time_1)
#      #Df LogLik     Df  Chisq Pr(>Chisq)    
# 1 8.2302 5652.1                             
# 2 9.2430 5633.5 1.0128 37.162  1.087e-09 ***

AIC(bm0_dwell_time_1, bm1_dwell_time_1)
#                        df       AIC
# bm0_dwell_time_1 8.230227 -11287.72
# bm1_dwell_time_1 9.243016 -11248.54

# Full model: Perf_Strivings
bm2.1_dwell_time_1 <- gamlss(
  dwell_time_1 ~ 
    IA_LABEL_EV * Perf_Strivings_c + 
    IA_LABEL_PR_LvsH * Perf_Strivings_c  + 
    IA_LABEL_PR_SvsF * Perf_Strivings_c  + 
    pb(subject_nr_1) +   
    pb(block_loop) +  
    pb(Change_category01, by = block_loop), 
  sigma.formula = ~ guessed_block_rule_1,  
  family = BE(mu.link = "cauchit"), 
  data = na.omit(sub_et_qu2)
) 
summary(bm2.1_dwell_time_1) 
# Family:  c("BE", "Beta") 
# 
# Call:  gamlss(formula = dwell_time_1 ~ IA_LABEL_EV * Perf_Strivings_c +  
#     IA_LABEL_PR_LvsH * Perf_Strivings_c + IA_LABEL_PR_SvsF *  
#     Perf_Strivings_c + pb(subject_nr_1) + pb(block_loop) +  
#     pb(Change_category01, by = block_loop), sigma.formula = ~guessed_block_rule_1,  
#     family = BE(mu.link = "cauchit"), data = na.omit(sub_et_qu2)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  cauchit
# Mu Coefficients:
#                                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                            -1.503e+00  4.464e-02 -33.682  < 2e-16 ***
# IA_LABEL_EV                             1.723e-02  1.475e-02   1.169   0.2426    
# Perf_Strivings_c                        6.731e-03  5.554e-03   1.212   0.2256    
# IA_LABEL_PR_LvsH                        5.717e-03  6.562e-03   0.871   0.3836    
# IA_LABEL_PR_SvsF                       -4.184e-02  2.044e-02  -2.047   0.0407 *  
# pb(subject_nr_1)                       -2.290e-05  5.574e-06  -4.108 4.07e-05 ***
# pb(block_loop)                          8.556e-04  3.305e-03   0.259   0.7958    
# pb(Change_category01, by = block_loop)  1.053e+00  1.963e-02  53.625  < 2e-16 ***
# IA_LABEL_EV:Perf_Strivings_c            2.185e-03  8.568e-03   0.255   0.7987    
# Perf_Strivings_c:IA_LABEL_PR_LvsH       3.007e-03  3.304e-03   0.910   0.3627    
# Perf_Strivings_c:IA_LABEL_PR_SvsF      -1.056e-02  1.156e-02  -0.913   0.3611    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                      Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          -1.63016    0.02545 -64.065  < 2e-16 ***
# guessed_block_rule_1  0.20647    0.02995   6.895 6.13e-12 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms maybe are not accurate. 
# ------------------------------------------------------------------
# No. of observations in the fit:  4584 
# Degrees of Freedom for the fit:  13.42513
#       Residual Deg. of Freedom:  4570.575 
#                       at cycle:  6 
#  
# Global Deviance:     -11270.46 
#             AIC:     -11243.61 
#             SBC:     -11157.28 

# Back-transformed coefficients
print(round(inv.cauchit(bm2.1_dwell_time_1$mu.coefficients),4))
# (Intercept)                            IA_LABEL_EV 
# 0.1868                                 0.5055 
# Perf_Strivings_c                       IA_LABEL_PR_LvsH 
# 0.5021                                 0.5018 
# IA_LABEL_PR_SvsF                       pb(subject_nr_1) 
# 0.4867                                 0.5000 
# pb(block_loop) pb(Change_category01, by = block_loop) 
# 0.5003                                 0.7582 
# IA_LABEL_EV:Perf_Strivings_c      Perf_Strivings_c:IA_LABEL_PR_LvsH 
# 0.5007                                 0.5010 
# Perf_Strivings_c:IA_LABEL_PR_SvsF 
# 0.4966

# Model comparison
lrtest(bm1_dwell_time_1, bm2.1_dwell_time_1)
#      #Df LogLik     Df  Chisq Pr(>Chisq)
# 1  9.243 5633.5                         
# 2 13.425 5635.2 4.1821 3.4383     0.4873

AIC(bm1_dwell_time_1, bm2.1_dwell_time_1)
#                           df       AIC
# bm1_dwell_time_1    9.243016 -11248.54
# bm2.1_dwell_time_1 13.425130 -11243.61

# Full Model: Perf_Concerns
bm2.2_dwell_time_1 <- gamlss(
  dwell_time_1 ~ 
    IA_LABEL_EV * Perf_Concerns_c + 
    IA_LABEL_PR_LvsH * Perf_Concerns_c  + 
    IA_LABEL_PR_SvsF * Perf_Concerns_c  + 
    pb(subject_nr_1) +   
    pb(block_loop) +  
    pb(Change_category01, by = block_loop), 
  sigma.formula = ~ guessed_block_rule_1,  
  family = BE(mu.link = "cauchit"), 
  data = na.omit(sub_et_qu2)
) 
summary(bm2.2_dwell_time_1) 
# Family:  c("BE", "Beta") 
# 
# Call:  gamlss(formula = dwell_time_1 ~ IA_LABEL_EV * Perf_Concerns_c +  
#     IA_LABEL_PR_LvsH * Perf_Concerns_c + IA_LABEL_PR_SvsF *  
#     Perf_Concerns_c + pb(subject_nr_1) + pb(block_loop) +  
#     pb(Change_category01, by = block_loop), sigma.formula = ~guessed_block_rule_1,  
#     family = BE(mu.link = "cauchit"), data = na.omit(sub_et_qu2)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  cauchit
# Mu Coefficients:
#                                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                            -1.508e+00  4.415e-02 -34.148  < 2e-16 ***
# IA_LABEL_EV                             1.721e-02  1.456e-02   1.182   0.2373    
# Perf_Concerns_c                         7.553e-03  4.300e-03   1.757   0.0790 .  
# IA_LABEL_PR_LvsH                        5.732e-03  6.569e-03   0.873   0.3829    
# IA_LABEL_PR_SvsF                       -4.193e-02  2.026e-02  -2.069   0.0386 *  
# pb(subject_nr_1)                       -2.242e-05  5.539e-06  -4.047 5.28e-05 ***
# pb(block_loop)                          8.646e-04  3.022e-03   0.286   0.7748    
# pb(Change_category01, by = block_loop)  1.054e+00  1.964e-02  53.656  < 2e-16 ***
# IA_LABEL_EV:Perf_Concerns_c             7.534e-03  6.227e-03   1.210   0.2264    
# Perf_Concerns_c:IA_LABEL_PR_LvsH        3.513e-03  2.588e-03   1.358   0.1746    
# Perf_Concerns_c:IA_LABEL_PR_SvsF       -4.184e-03  8.606e-03  -0.486   0.6269    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                      Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          -1.62907    0.02544 -64.037  < 2e-16 ***
# guessed_block_rule_1  0.20460    0.02994   6.834 9.35e-12 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms maybe are not accurate. 
# ------------------------------------------------------------------
# No. of observations in the fit:  4584 
# Degrees of Freedom for the fit:  13.33616
#       Residual Deg. of Freedom:  4570.664 
#                       at cycle:  6 
#  
# Global Deviance:     -11272.41 
#             AIC:     -11245.74 
#             SBC:     -11159.99  

# Back-transformed coefficients
print(round(inv.cauchit(bm2.2_dwell_time_1$mu.coefficients),4))
# (Intercept)                            IA_LABEL_EV 
# 0.1864                                 0.5055 
# Perf_Concerns_c                       IA_LABEL_PR_LvsH 
# 0.5024                                 0.5018 
# IA_LABEL_PR_SvsF                       pb(subject_nr_1) 
# 0.4867                                 0.5000 
# pb(block_loop) pb(Change_category01, by = block_loop) 
# 0.5003                                 0.7583 
# IA_LABEL_EV:Perf_Concerns_c       Perf_Concerns_c:IA_LABEL_PR_LvsH 
# 0.5024                                 0.5011 
# Perf_Concerns_c:IA_LABEL_PR_SvsF 
# 0.4987

# Model comparison
lrtest(bm1_dwell_time_1, bm2.2_dwell_time_1)
#      #Df LogLik     Df  Chisq Pr(>Chisq)
# 1  9.243 5633.5                         
# 2 13.336 5636.2 4.0931 5.3923     0.2494

AIC(bm1_dwell_time_1, bm2.2_dwell_time_1)
#                           df       AIC
# bm1_dwell_time_1    9.243016 -11248.54
# bm2.2_dwell_time_1 13.336156 -11245.74

# Fit a gamma  glmm for Response Time
#------------------------------------

# Create a subset of the data for model fitting
sub_rt_qu2 <- rt_qu2[c("subject_nr_1", "response_time_1", "guessed_block_rule_1", 
                       "block_loop", "Change_category_PR_SvsF", 
                       "Change_category_PR_LvsH", "Change_category_EV", 
                       "Condition","Perf_Strivings_c" , "Perf_Concerns_c")]

# Null Model
gm0_response_time_1 <- gamlss(
  response_time_1 ~ 
    pb(subject_nr_1) +   
    pb(block_loop),  
  family = GA(mu.link = "log"),   
  sigma.formula = ~ guessed_block_rule_1 + Condition,  
  data = na.omit(sub_rt_qu2)
) 
summary(gm0_response_time_1)
# Family:  c("GA", "Gamma") 
# 
# Call:  gamlss(formula = response_time_1 ~ pb(subject_nr_1) +  
#     pb(block_loop), sigma.formula = ~guessed_block_rule_1 +  
#     Condition, family = GA(mu.link = "log"), data = na.omit(sub_rt_qu2)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  log
# Mu Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       7.015e+00  3.293e-02 213.021   <2e-16 ***
# pb(subject_nr_1)  5.644e-05  4.110e-06  13.730   <2e-16 ***
# pb(block_loop)   -5.395e-03  2.238e-03  -2.411   0.0159 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  log
# Sigma Coefficients:
#                      Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          -0.70691    0.02136 -33.098  < 2e-16 ***
# guessed_block_rule_1  0.08430    0.01980   4.257  2.1e-05 ***
# ConditionFailure      0.05799    0.02006   2.891  0.00385 ** 
# ConditionSuccess      0.02130    0.01889   1.127  0.25960    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms maybe are not accurate. 
# ------------------------------------------------------------------
# No. of observations in the fit:  6782 
# Degrees of Freedom for the fit:  17.35986
#       Residual Deg. of Freedom:  6764.64 
#                       at cycle:  9 
#  
# Global Deviance:     109941.2 
#             AIC:     109975.9 
#             SBC:     110094.3 

# Exponentiated coefficients
print(round(exp(gm0_response_time_1$mu.coefficients),4))
# (Intercept) pb(subject_nr_1)   pb(block_loop) 
# 1113.2599           1.0001           0.9946 

# Partial Model
gm1_response_time_1 <- gamlss(
  response_time_1 ~ 
    Change_category_EV + 
    Change_category_PR_LvsH + 
    Change_category_PR_SvsF + 
    pb(subject_nr_1) +   
    pb(block_loop),  
  family = GA(mu.link = "log"),   
  sigma.formula = ~ guessed_block_rule_1,  
  data = na.omit(sub_rt_qu2)
) 
summary(gm1_response_time_1)
# Family:  c("GA", "Gamma") 
# 
# Call:  gamlss(formula = response_time_1 ~ Change_category_EV +  
#     Change_category_PR_LvsH + Change_category_PR_SvsF +  
#     pb(subject_nr_1) + pb(block_loop), sigma.formula = ~guessed_block_rule_1,  
#     family = GA(mu.link = "log"), data = na.omit(sub_rt_qu2)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  log
# Mu Coefficients:
#                           Estimate Std. Error t value Pr(>|t|)    
# (Intercept)              6.899e+00  3.864e-02 178.535  < 2e-16 ***
# Change_category_EV      -4.458e-02  1.259e-02  -3.542 0.000400 ***
# Change_category_PR_LvsH  3.391e-02  1.020e-02   3.325 0.000889 ***
# Change_category_PR_SvsF -1.090e-02  1.674e-02  -0.651 0.514853    
# pb(subject_nr_1)         5.657e-05  4.157e-06  13.608  < 2e-16 ***
# pb(block_loop)           2.349e-02  5.475e-03   4.290 1.81e-05 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  log
# Sigma Coefficients:
#                      Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          -0.67664    0.01727 -39.190  < 2e-16 ***
# guessed_block_rule_1  0.08100    0.01984   4.083  4.5e-05 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms maybe are not accurate. 
# ------------------------------------------------------------------
# No. of observations in the fit:  6782 
# Degrees of Freedom for the fit:  13.72919
#       Residual Deg. of Freedom:  6768.271 
#                       at cycle:  5 
#  
# Global Deviance:     109958.3 
#             AIC:     109985.8 
#             SBC:     110079.4 

# Exponentiated coefficients
print(round(exp(gm1_response_time_1$mu.coefficients),4))
# (Intercept)      Change_category_EV Change_category_PR_LvsH Change_category_PR_SvsF 
# 991.4403                  0.9564                  1.0345                  0.9892 
# pb(subject_nr_1)          pb(block_loop) 
# 1.0001                  1.0238 

# Model comparison
lrtest(gm0_response_time_1, gm1_response_time_1)
#      #Df LogLik      Df  Chisq Pr(>Chisq)   
# 1 17.360 -54971                             
# 2 13.729 -54979 -3.6307 17.119   0.001832 **

AIC(gm0_response_time_1, gm1_response_time_1)
#                           df      AIC
# gm0_response_time_1 17.35986 109975.9
# gm1_response_time_1 13.72919 109985.8

# Full Model: Perf_Strivings
gm2.1_response_time_1 <- gamlss(
  response_time_1 ~ 
    Change_category_EV * Perf_Strivings_c + 
    Change_category_PR_LvsH * Perf_Strivings_c + 
    Change_category_PR_SvsF * Perf_Strivings_c + 
    pb(subject_nr_1) +   
    pb(block_loop),  
  family = GA(mu.link = "log"),   
  sigma.formula = ~ guessed_block_rule_1,  
  data = na.omit(sub_rt_qu2)
) 
summary(gm2.1_response_time_1)
# Family:  c("GA", "Gamma") 
# 
# Call:  gamlss(formula = response_time_1 ~ Change_category_EV * Perf_Strivings_c +  
#     Change_category_PR_LvsH * Perf_Strivings_c + Change_category_PR_SvsF *  
#     Perf_Strivings_c + pb(subject_nr_1) + pb(block_loop), sigma.formula = ~guessed_block_rule_1,  
#     family = GA(mu.link = "log"), data = na.omit(sub_rt_qu2)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  log
# Mu Coefficients:
#                                            Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                               6.895e+00  3.852e-02 178.997  < 2e-16 ***
# Change_category_EV                       -4.453e-02  1.163e-02  -3.828 0.000130 ***
# Perf_Strivings_c                         -5.575e-03  4.013e-03  -1.389 0.164826    
# Change_category_PR_LvsH                   3.351e-02  1.007e-02   3.328 0.000878 ***
# Change_category_PR_SvsF                  -1.054e-02  1.466e-02  -0.719 0.471887    
# pb(subject_nr_1)                          5.720e-05  4.163e-06  13.740  < 2e-16 ***
# pb(block_loop)                            2.335e-02  5.402e-03   4.322 1.57e-05 ***
# Change_category_EV:Perf_Strivings_c      -7.361e-03  5.605e-03  -1.313 0.189154    
# Perf_Strivings_c:Change_category_PR_LvsH -5.607e-04  2.395e-03  -0.234 0.814861    
# Perf_Strivings_c:Change_category_PR_SvsF  2.445e-02  7.882e-03   3.102 0.001928 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  log
# Sigma Coefficients:
#                      Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          -0.67828    0.01696 -40.000  < 2e-16 ***
# guessed_block_rule_1  0.08204    0.01937   4.235 2.32e-05 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms may not be reliable. 
# ------------------------------------------------------------------
# No. of observations in the fit:  6782 
# Degrees of Freedom for the fit:  17.6931
#       Residual Deg. of Freedom:  6764.307 
#                       at cycle:  5 
#  
# Global Deviance:     109945.8 
#             AIC:     109981.2 
#             SBC:     110101.9

# Exponentiated coefficients
print(round(exp(gm2.1_response_time_1$mu.coefficients),4))
# (Intercept)                       Change_category_EV 
# 987.0511                                   0.9565 
# Perf_Strivings_c                  Change_category_PR_LvsH 
# 0.9944                                   1.0341 
# Change_category_PR_SvsF                         pb(subject_nr_1) 
# 0.9895                                   1.0001 
# pb(block_loop)      Change_category_EV:Perf_Strivings_c 
# 1.0236                                   0.9927 
# Perf_Strivings_c:Change_category_PR_LvsH Perf_Strivings_c:Change_category_PR_SvsF 
# 0.9994                                   1.0248

# Model comparison
lrtest(gm1_response_time_1, gm2.1_response_time_1)
#      #Df LogLik     Df Chisq Pr(>Chisq)  
# 1 13.729 -54979                          
# 2 17.693 -54973 3.9639 12.45     0.0143 *

AIC(gm1_response_time_1, gm2.1_response_time_1)
#                             df      AIC
# gm2.1_response_time_1 17.69310 109981.2
# gm1_response_time_1   13.72919 109985.8

# Full Model: Perf_Concerns
gm2.2_response_time_1 <- gamlss(
  response_time_1 ~ 
    Change_category_EV * Perf_Concerns_c + 
    Change_category_PR_LvsH * Perf_Concerns_c + 
    Change_category_PR_SvsF * Perf_Concerns_c + 
    pb(subject_nr_1) +   
    pb(block_loop),  
  family = GA(mu.link = "log"),   
  sigma.formula = ~ guessed_block_rule_1,  
  data = na.omit(sub_rt_qu2)
) 
summary(gm2.2_response_time_1)
# Family:  c("GA", "Gamma") 
# 
# Call:  gamlss(formula = response_time_1 ~ Change_category_EV *  
#     Perf_Concerns_c + Change_category_PR_LvsH * Perf_Concerns_c +  
#     Change_category_PR_SvsF * Perf_Concerns_c + pb(subject_nr_1) +  
#     pb(block_loop), sigma.formula = ~guessed_block_rule_1,  
#     family = GA(mu.link = "log"), data = na.omit(sub_rt_qu2)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  log
# Mu Coefficients:
#                                           Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                              6.898e+00  3.833e-02 179.964  < 2e-16 ***
# Change_category_EV                      -4.447e-02  1.208e-02  -3.682 0.000233 ***
# Perf_Concerns_c                          5.798e-03  3.477e-03   1.668 0.095457 .  
# Change_category_PR_LvsH                  3.404e-02  1.009e-02   3.373 0.000748 ***
# Change_category_PR_SvsF                 -1.119e-02  1.773e-02  -0.631 0.527770    
# pb(subject_nr_1)                         5.670e-05  4.160e-06  13.631  < 2e-16 ***
# pb(block_loop)                           2.356e-02  5.403e-03   4.361 1.31e-05 ***
# Change_category_EV:Perf_Concerns_c      -6.268e-03  5.212e-03  -1.203 0.229181    
# Perf_Concerns_c:Change_category_PR_LvsH  5.478e-04  1.884e-03   0.291 0.771300    
# Perf_Concerns_c:Change_category_PR_SvsF  8.824e-03  6.854e-03   1.288 0.197940    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  log
# Sigma Coefficients:
#                      Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          -0.67848    0.01748 -38.811  < 2e-16 ***
# guessed_block_rule_1  0.08294    0.02015   4.116  3.9e-05 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms maybe are not accurate. 
# ------------------------------------------------------------------
# No. of observations in the fit:  6782 
# Degrees of Freedom for the fit:  17.68424
#       Residual Deg. of Freedom:  6764.316 
#                       at cycle:  5 
#  
# Global Deviance:     109953.1 
#             AIC:     109988.5 
#             SBC:     110109.1

# Exponentiated coefficients
print(round(exp(gm2.2_response_time_1$mu.coefficients),4))
# (Intercept)                      Change_category_EV 
# 990.2304                                  0.9565 
# Perf_Concerns_c                 Change_category_PR_LvsH 
# 1.0058                                  1.0346 
# Change_category_PR_SvsF                        pb(subject_nr_1) 
# 0.9889                                  1.0001 
# pb(block_loop)      Change_category_EV:Perf_Concerns_c 
# 1.0238                                  0.9938 
# Perf_Concerns_c:Change_category_PR_LvsH Perf_Concerns_c:Change_category_PR_SvsF 
# 1.0005                                  1.0089 

# Model comparison
lrtest(gm1_response_time_1, gm2.2_response_time_1)
#      #Df LogLik    Df Chisq Pr(>Chisq)
# 1 13.729 -54979                       
# 2 17.684 -54977 3.955 5.184     0.2689

AIC(gm1_response_time_1, gm2.2_response_time_1)
#                             df      AIC
# gm1_response_time_1   13.72919 109985.8
# gm2.2_response_time_1 17.68424 109988.5

# Simulate predicted values
preds_response_time_PS <- ggpredict(gm2.1_response_time_1, terms = c("Perf_Strivings_c", "Change_category_PR_SvsF"))

# Plot the retained interaction 
rt_PS <- ggplot(preds_response_time_PS, aes(x = x, y = predicted, color = group)) +
  geom_line(size = 1) +                                # Line only, no CI ribbon
  scale_color_manual(
    values = c("-1" = "orange", "0" = "grey", "1" = "orangered"),         # Customize colors by group
    labels = c("Success", "Non-PR", "Failure")         # Custom legend labels
  ) +
  labs(
    x = "Perfectionistic Strivings",
    y = "",
    title = "",
    color = "Performance Relevance"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )

x11(width = 6, height = 4)
grid.arrange(rt_PS,
             ncol = 1, nrow = 1,
             top = textGrob("Predicted Response Time (t1)",
                            gp = gpar(fontsize = 16, fontface = "bold"))
)