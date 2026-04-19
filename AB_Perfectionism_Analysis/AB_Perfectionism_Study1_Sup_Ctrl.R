# AB Perfectionism - Study 1 

#-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-#
# Supplementary Analysis: Controlling for CESD and GAD7 #
#-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-#

source("AB_Perfectionism_Study1_Preparation.R")

# Path a: Perfectionism → Attention Bias
#---------------------------------------

# Create a subset of the data for model fitting
sub_et_qu <- et_qu[c("first_fix_prob", "first_fix_latency", "dwell_time",
                     "dwell_time_early", "dwell_time_intermediate", "dwell_time_late",
                     "subject_nr", "block_loop", "guessed_block_rule","Change_category01", 
                     "IA_LABEL","IA_LABEL_PR_SvsF", "IA_LABEL_PR_LvsH","IA_LABEL_EV", 
                     "Perf_Strivings_c" , "Perf_Concerns_c", "CESD", "GAD7")]

# Transform 0 and 1 observations
transform01 <- function(x) {
  (x * (length(x) - 1) + 0.5) / (length(x))
}

sub_et_qu$first_fix_prob_t01 <- transform01(sub_et_qu$first_fix_prob)
sub_et_qu$dwell_time_early_t01 <- transform01(sub_et_qu$dwell_time_early)
sub_et_qu$dwell_time_intermediate_t01 <- transform01(sub_et_qu$dwell_time_intermediate)
sub_et_qu$dwell_time_late_t01 <- transform01(sub_et_qu$dwell_time_late)
sub_et_qu$CESD_c <- sub_et_qu$CESD - mean(sub_et_qu$CESD)
sub_et_qu$GAD7_c <- sub_et_qu$GAD7 - mean(sub_et_qu$GAD7)

# Define a function to back-transform from cauchit
inv.cauchit <- function(eta) {
  (1 / pi) * atan(eta) + 0.5
}

# Fit a Beta glmm for first fixation probability 
#-----------------------------------------------

# Null model
bm0_first_fix_prob <- gamlss(
  first_fix_prob_t01 ~ 
    CESD_c + GAD7_c + 
    pb(subject_nr) +  
    pb(block_loop) +  
    pb(Change_category01, by = block_loop),  
  family = BE(mu.link = "logit"),  
  sigma.formula = ~ guessed_block_rule, 
  data = na.omit(sub_et_qu)
)
summary(bm0_first_fix_prob)
# Family:  c("BE", "Beta") 
# 
# Call:  gamlss(formula = first_fix_prob_t01 ~ CESD_c + GAD7_c +  
#     pb(subject_nr) + pb(block_loop) + pb(Change_category01,  
#     by = block_loop), sigma.formula = ~guessed_block_rule,  
#     family = BE(mu.link = "logit"), data = na.omit(sub_et_qu)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  logit
# Mu Coefficients:
#                                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                            -3.343e-01  1.937e-02 -17.259  < 2e-16 ***
# CESD_c                                  2.342e-03  1.198e-03   1.955   0.0506 .  
# GAD7_c                                 -8.593e-03  1.999e-03  -4.299 1.73e-05 ***
# pb(subject_nr)                         -1.921e-07  5.741e-06  -0.033   0.9733    
# pb(block_loop)                          4.014e-03  3.025e-03   1.327   0.1845    
# pb(Change_category01, by = block_loop) -1.724e-01  2.184e-02  -7.897 3.14e-15 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                     Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        -0.239994   0.021535 -11.144   <2e-16 ***
# guessed_block_rule  0.001664   0.023517   0.071    0.944    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms maybe are not accurate. 
# ------------------------------------------------------------------
# No. of observations in the fit:  10145 
# Degrees of Freedom for the fit:  9.164453
#       Residual Deg. of Freedom:  10135.84 
#                       at cycle:  4 
#  
# Global Deviance:     -3389.183 
#             AIC:     -3370.854 
#             SBC:     -3304.644 

# Back-transformed coefficients
print(round(inv.logit(bm0_first_fix_prob$mu.coefficients),4))
# (Intercept)                                 CESD_c 
# 0.4172                                 0.5006 
# GAD7_c                         pb(subject_nr) 
# 0.4979                                 0.5000 
# pb(block_loop) pb(Change_category01, by = block_loop) 
# 0.5010                                 0.4570 

# Partial Model
bm1_first_fix_prob <- gamlss(
  first_fix_prob_t01 ~ 
    IA_LABEL_EV + 
    IA_LABEL_PR_SvsF + 
    IA_LABEL_PR_LvsH + 
    CESD_c + GAD7_c +   
    pb(subject_nr) +    
    pb(block_loop) +    
    pb(Change_category01, by = block_loop),    
  sigma.formula = ~ guessed_block_rule,   
  family = BE(mu.link = "logit"),    
  data = na.omit(sub_et_qu)
)
summary(bm1_first_fix_prob)
# Family:  c("BE", "Beta") 
# 
# Call:  gamlss(formula = first_fix_prob_t01 ~ IA_LABEL_EV +  
#     IA_LABEL_PR_SvsF + IA_LABEL_PR_LvsH + CESD_c +  
#     GAD7_c + pb(subject_nr) + pb(block_loop) + pb(Change_category01,  
#     by = block_loop), sigma.formula = ~guessed_block_rule,  
#     family = BE(mu.link = "logit"), data = na.omit(sub_et_qu)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  logit
# Mu Coefficients:
#                                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                            -3.383e-01  1.996e-02 -16.945  < 2e-16 ***
# IA_LABEL_EV                            -8.770e-02  1.372e-02  -6.394 1.69e-10 ***
# IA_LABEL_PR_SvsF                        7.448e-02  1.940e-02   3.840 0.000124 ***
# IA_LABEL_PR_LvsH                       -4.396e-03  5.904e-03  -0.745 0.456491    
# CESD_c                                  2.335e-03  1.196e-03   1.952 0.050935 .  
# GAD7_c                                 -8.584e-03  1.996e-03  -4.301 1.72e-05 ***
# pb(subject_nr)                         -1.969e-07  5.740e-06  -0.034 0.972641    
# pb(block_loop)                          4.065e-03  3.022e-03   1.345 0.178597    
# pb(Change_category01, by = block_loop) -1.729e-01  2.181e-02  -7.928 2.45e-15 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                     Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        -0.247260   0.021553  -11.47   <2e-16 ***
# guessed_block_rule  0.006822   0.023528    0.29    0.772    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms maybe are not accurate. 
# ------------------------------------------------------------------
# No. of observations in the fit:  10145 
# Degrees of Freedom for the fit:  12.1561
#       Residual Deg. of Freedom:  10132.84 
#                       at cycle:  3 
#  
# Global Deviance:     -3431.42 
#             AIC:     -3407.108 
#             SBC:     -3319.283 

# Back-transformed coefficients
print(round(inv.logit(bm1_first_fix_prob$mu.coefficients),4))
# (Intercept)                            IA_LABEL_EV 
# 0.4162                                 0.4781 
# IA_LABEL_PR_SvsF                       IA_LABEL_PR_LvsH 
# 0.5186                                 0.4989 
# CESD_c                                 GAD7_c 
# 0.5006                                 0.4979 
# pb(subject_nr)                         pb(block_loop) 
# 0.5000                                 0.5010 
# pb(Change_category01, by = block_loop) 
# 0.4569  

# Model comparison
lrtest(bm0_first_fix_prob, bm1_first_fix_prob)
#       #Df LogLik     Df  Chisq Pr(>Chisq)    
# 1  9.1645 1694.6                             
# 2 12.1561 1715.7 2.9916 42.237  3.574e-09 ***

AIC(bm1_first_fix_prob, bm0_first_fix_prob)
#                           df       AIC
# bm1_first_fix_prob 12.156097 -3407.108
# bm0_first_fix_prob  9.164453 -3370.854

# Full Model: Perf_Strivings
bm2.1_first_fix_prob <- gamlss(
  first_fix_prob_t01 ~ 
    IA_LABEL_EV * Perf_Strivings_c + 
    IA_LABEL_PR_SvsF * Perf_Strivings_c + 
    IA_LABEL_PR_LvsH * Perf_Strivings_c + 
    CESD_c + GAD7_c +   
    pb(subject_nr) +    
    pb(block_loop) +    
    pb(Change_category01, by = block_loop),    
  sigma.formula = ~ guessed_block_rule,   
  family = BE(mu.link = "logit"),    
  data = na.omit(sub_et_qu)
) 
summary(bm2.1_first_fix_prob)
# Family:  c("BE", "Beta") 
# 
# Call:  gamlss(formula = first_fix_prob_t01 ~ IA_LABEL_EV *  
#     Perf_Strivings_c + IA_LABEL_PR_SvsF * Perf_Strivings_c +  
#     IA_LABEL_PR_LvsH * Perf_Strivings_c + CESD_c +  
#     GAD7_c + pb(subject_nr) + pb(block_loop) + pb(Change_category01,  
#     by = block_loop), sigma.formula = ~guessed_block_rule,  
#     family = BE(mu.link = "logit"), data = na.omit(sub_et_qu)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  logit
# Mu Coefficients:
#                                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                            -3.405e-01  1.995e-02 -17.062  < 2e-16 ***
# IA_LABEL_EV                            -8.763e-02  1.371e-02  -6.391 1.73e-10 ***
# Perf_Strivings_c                       -7.643e-03  5.669e-03  -1.348 0.177648    
# IA_LABEL_PR_SvsF                        7.440e-02  1.939e-02   3.837 0.000125 ***
# IA_LABEL_PR_LvsH                       -4.419e-03  5.901e-03  -0.749 0.453981    
# CESD_c                                  2.687e-03  1.211e-03   2.219 0.026516 *  
# GAD7_c                                 -8.125e-03  1.998e-03  -4.066 4.82e-05 ***
# pb(subject_nr)                          4.261e-07  5.742e-06   0.074 0.940839    
# pb(block_loop)                          4.064e-03  3.020e-03   1.345 0.178510    
# pb(Change_category01, by = block_loop) -1.727e-01  2.180e-02  -7.922 2.58e-15 ***
# IA_LABEL_EV:Perf_Strivings_c           -1.729e-02  7.606e-03  -2.274 0.023012 *  
# Perf_Strivings_c:IA_LABEL_PR_SvsF       4.016e-03  1.075e-02   0.374 0.708728    
# Perf_Strivings_c:IA_LABEL_PR_LvsH       3.761e-03  3.272e-03   1.149 0.250437    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                     Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        -0.248682   0.021553 -11.538   <2e-16 ***
# guessed_block_rule  0.007347   0.023528   0.312    0.755    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms maybe are not accurate. 
# ------------------------------------------------------------------
# No. of observations in the fit:  10145 
# Degrees of Freedom for the fit:  16.15452
#       Residual Deg. of Freedom:  10128.85 
#                       at cycle:  4 
#  
# Global Deviance:     -3445.302 
#             AIC:     -3412.993 
#             SBC:     -3296.28 

# Back-transformed coefficients
print(round(inv.logit(bm2.1_first_fix_prob$mu.coefficients),4))
# (Intercept)                            IA_LABEL_EV 
# 0.4157                                 0.4781 
# Perf_Strivings_c                       IA_LABEL_PR_SvsF 
# 0.4981                                 0.5186 
# IA_LABEL_PR_LvsH                                 CESD_c 
# 0.4989                                 0.5007 
# GAD7_c                         pb(subject_nr) 
# 0.4980                                 0.5000 
# pb(block_loop) pb(Change_category01, by = block_loop) 
# 0.5010                                 0.4570 
# IA_LABEL_EV:Perf_Strivings_c      Perf_Strivings_c:IA_LABEL_PR_SvsF 
# 0.4957                                 0.5010 
# Perf_Strivings_c:IA_LABEL_PR_LvsH 
# 0.5009

# Model comparison
lrtest(bm1_first_fix_prob, bm2.1_first_fix_prob)
#      #Df LogLik     Df  Chisq Pr(>Chisq)   
# 1 12.156 1715.7                            
# 2 16.155 1722.7 3.9984 13.882   0.007682 **

AIC(bm2.1_first_fix_prob, bm1_first_fix_prob)
#                            df       AIC
# bm2.1_first_fix_prob 16.15452 -3412.993
# bm1_first_fix_prob   12.15610 -3407.108

# Full Model: Perf_Concerns
bm2.2_first_fix_prob <- gamlss(
  first_fix_prob_t01 ~ 
    IA_LABEL_EV * Perf_Concerns_c +
    IA_LABEL_PR_SvsF * Perf_Concerns_c +
    IA_LABEL_PR_LvsH * Perf_Concerns_c +
    CESD_c + GAD7_c +   
    pb(subject_nr) +    
    pb(block_loop) +    
    pb(Change_category01, by = block_loop),    
  sigma.formula = ~ guessed_block_rule,   
  family = BE(mu.link = "logit"),    
  data = na.omit(sub_et_qu)
) 
summary(bm2.2_first_fix_prob)
# Family:  c("BE", "Beta") 
# 
# Call:  gamlss(formula = first_fix_prob_t01 ~ IA_LABEL_EV *  
#     Perf_Concerns_c + IA_LABEL_PR_SvsF * Perf_Concerns_c +  
#     IA_LABEL_PR_LvsH * Perf_Concerns_c + CESD_c + GAD7_c +  
#     pb(subject_nr) + pb(block_loop) + pb(Change_category01,  
#     by = block_loop), sigma.formula = ~guessed_block_rule,  
#     family = BE(mu.link = "logit"), data = na.omit(sub_et_qu)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  logit
# Mu Coefficients:
#                                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                            -3.394e-01  1.996e-02 -17.002  < 2e-16 ***
# IA_LABEL_EV                            -8.777e-02  1.371e-02  -6.401 1.62e-10 ***
# Perf_Concerns_c                        -3.509e-04  4.809e-03  -0.073 0.941841    
# IA_LABEL_PR_SvsF                        7.449e-02  1.939e-02   3.841 0.000123 ***
# IA_LABEL_PR_LvsH                       -4.390e-03  5.902e-03  -0.744 0.456984    
# CESD_c                                  2.586e-03  1.273e-03   2.031 0.042233 *  
# GAD7_c                                 -8.411e-03  2.005e-03  -4.194 2.76e-05 ***
# pb(subject_nr)                          7.271e-08  5.745e-06   0.013 0.989904    
# pb(block_loop)                          4.086e-03  3.021e-03   1.353 0.176246    
# pb(Change_category01, by = block_loop) -1.729e-01  2.181e-02  -7.931 2.40e-15 ***
# IA_LABEL_EV:Perf_Concerns_c             3.306e-03  5.817e-03   0.568 0.569829    
# Perf_Concerns_c:IA_LABEL_PR_SvsF       -1.858e-02  8.244e-03  -2.254 0.024203 *  
# Perf_Concerns_c:IA_LABEL_PR_LvsH        2.772e-03  2.511e-03   1.104 0.269551    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                     Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        -0.249854   0.021574 -11.581   <2e-16 ***
# guessed_block_rule  0.009223   0.023557   0.392    0.695    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms maybe are not accurate. 
# ------------------------------------------------------------------
# No. of observations in the fit:  10145 
# Degrees of Freedom for the fit:  16.15548
#       Residual Deg. of Freedom:  10128.84 
#                       at cycle:  4 
#  
# Global Deviance:     -3440.121 
#             AIC:     -3407.81 
#             SBC:     -3291.091 

# Back-transformed coefficients
print(round(inv.logit(bm2.2_first_fix_prob$mu.coefficients),4))
# (Intercept)                            IA_LABEL_EV 
# 0.4160                                 0.4781 
# Perf_Concerns_c                       IA_LABEL_PR_SvsF 
# 0.4999                                 0.5186 
# IA_LABEL_PR_LvsH                                 CESD_c 
# 0.4989                                 0.5006 
# GAD7_c                         pb(subject_nr) 
# 0.4979                                 0.5000 
# pb(block_loop) pb(Change_category01, by = block_loop) 
# 0.5010                                 0.4569 
# IA_LABEL_EV:Perf_Concerns_c       Perf_Concerns_c:IA_LABEL_PR_SvsF 
# 0.5008                                 0.4954 
# Perf_Concerns_c:IA_LABEL_PR_LvsH 
# 0.5007 

# Model comparison
lrtest(bm1_first_fix_prob, bm2.2_first_fix_prob)
#      #Df LogLik     Df  Chisq Pr(>Chisq)  
# 1 12.156 1715.7                           
# 2 16.155 1720.1 3.9994 8.7016    0.06901 .

AIC(bm2.2_first_fix_prob, bm1_first_fix_prob)
#                            df       AIC
# bm2.2_first_fix_prob 16.15548 -3407.810
# bm1_first_fix_prob   12.15610 -3407.108

# Fit a Beta glmm for first fixation latency
#-------------------------------------------

# Null model
bm0_first_fix_latency <- gamlss(
  first_fix_latency ~ 
    CESD_c + GAD7_c +   
    pb(subject_nr) +    
    pb(block_loop) +    
    pb(Change_category01, by = block_loop),    
  sigma.formula = ~ guessed_block_rule,   
  family = BE(mu.link = "logit"),    
  data = na.omit(sub_et_qu)
)
summary(bm0_first_fix_latency)
# Family:  c("BE", "Beta") 
# 
# Call:  gamlss(formula = first_fix_latency ~ CESD_c + GAD7_c + pb(subject_nr) +  
#     pb(block_loop) + pb(Change_category01, by = block_loop),  
#     sigma.formula = ~guessed_block_rule, family = BE(mu.link = "logit"),  
#     data = na.omit(sub_et_qu)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  logit
# Mu Coefficients:
#                                          Estimate Std. Error  t value Pr(>|t|)    
# (Intercept)                            -1.392e+00  3.611e-03 -385.387  < 2e-16 ***
# CESD_c                                 -2.035e-05  2.088e-04   -0.097    0.922    
# GAD7_c                                  1.040e-04  3.593e-04    0.290    0.772    
# pb(subject_nr)                         -5.394e-08  4.212e-07   -0.128    0.898    
# pb(block_loop)                          2.104e-04  5.230e-04    0.402    0.688    
# pb(Change_category01, by = block_loop)  2.556e-02  3.739e-03    6.835 8.67e-12 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                    Estimate Std. Error  t value Pr(>|t|)    
# (Intercept)        -2.72028    0.01733 -156.994   <2e-16 ***
# guessed_block_rule -0.02385    0.01918   -1.244    0.214    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms may not be reliable. 
# ------------------------------------------------------------------
# No. of observations in the fit:  10145 
# Degrees of Freedom for the fit:  8.503363
#       Residual Deg. of Freedom:  10136.5 
#                       at cycle:  5 
#  
# Global Deviance:     -46714.03 
#             AIC:     -46697.02 
#             SBC:     -46635.59 

# Back-transformed coefficients
print(round(inv.logit(bm0_first_fix_latency$mu.coefficients),4))
# (Intercept)                                 CESD_c 
# 0.1991                                 0.5000 
# GAD7_c                         pb(subject_nr) 
# 0.5000                                 0.5000 
# pb(block_loop) pb(Change_category01, by = block_loop) 
# 0.5001                                 0.5064 

# Partial Model
bm1_first_fix_latency <- gamlss(
  first_fix_latency ~ 
    IA_LABEL_EV + 
    IA_LABEL_PR_SvsF + 
    IA_LABEL_PR_LvsH + 
    CESD_c + GAD7_c +   
    pb(subject_nr) +    
    pb(block_loop) +    
    pb(Change_category01, by = block_loop),    
  sigma.formula = ~ guessed_block_rule,   
  family = BE(mu.link = "logit"),    
  data = na.omit(sub_et_qu)
)
summary(bm1_first_fix_latency)
# Family:  c("BE", "Beta") 
# 
# Call:  gamlss(formula = first_fix_latency ~ IA_LABEL_EV + IA_LABEL_PR_SvsF +  
#     IA_LABEL_PR_LvsH + CESD_c + GAD7_c + pb(subject_nr) + pb(block_loop) +  
#     pb(Change_category01, by = block_loop), sigma.formula = ~guessed_block_rule,  
#     family = BE(mu.link = "logit"), data = na.omit(sub_et_qu)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  logit
# Mu Coefficients:
#                                          Estimate Std. Error  t value Pr(>|t|)    
# (Intercept)                            -1.391e+00  3.696e-03 -376.332  < 2e-16 ***
# IA_LABEL_EV                             1.089e-02  2.373e-03    4.591 4.47e-06 ***
# IA_LABEL_PR_SvsF                       -5.429e-03  3.354e-03   -1.619    0.106    
# IA_LABEL_PR_LvsH                        1.164e-03  1.021e-03    1.141    0.254    
# CESD_c                                 -2.000e-05  2.086e-04   -0.096    0.924    
# GAD7_c                                  1.032e-04  3.589e-04    0.288    0.774    
# pb(subject_nr)                         -5.519e-08  4.206e-07   -0.131    0.896    
# pb(block_loop)                          2.175e-04  5.223e-04    0.416    0.677    
# pb(Change_category01, by = block_loop)  2.554e-02  3.734e-03    6.839 8.41e-12 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                    Estimate Std. Error  t value Pr(>|t|)    
# (Intercept)        -2.72406    0.01732 -157.240   <2e-16 ***
# guessed_block_rule -0.02099    0.01917   -1.095    0.274    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms may not be reliable. 
# ------------------------------------------------------------------
# No. of observations in the fit:  10145 
# Degrees of Freedom for the fit:  11.50444
#       Residual Deg. of Freedom:  10133.5 
#                       at cycle:  5 
#  
# Global Deviance:     -46741.65 
#             AIC:     -46718.64 
#             SBC:     -46635.52 

# Back-transformed coefficients
print(round(inv.logit(bm1_first_fix_latency$mu.coefficients),4))
# (Intercept)                            IA_LABEL_EV 
# 0.1993                                 0.5027 
# IA_LABEL_PR_SvsF                       IA_LABEL_PR_LvsH 
# 0.4986                                 0.5003 
# CESD_c                                 GAD7_c 
# 0.5000                                 0.5000 
# pb(subject_nr)                         pb(block_loop) 
# 0.5000                                 0.5001 
# pb(Change_category01, by = block_loop) 
# 0.5064            

# Model comparison
lrtest(bm0_first_fix_latency, bm1_first_fix_latency)
#       #Df LogLik     Df Chisq Pr(>Chisq)    
# 1  8.5034  23357                            
# 2 11.5044  23371 3.0011 27.62  4.365e-06 ***

AIC(bm1_first_fix_latency, bm0_first_fix_latency)
#                              df       AIC
# bm1_first_fix_latency 11.504439 -46718.64
# bm0_first_fix_latency  8.503363 -46697.02

# Full Model: Perf_Strivings
bm2.1_first_fix_latency <- gamlss(
  first_fix_latency ~ 
    IA_LABEL_EV * Perf_Strivings_c + 
    IA_LABEL_PR_SvsF * Perf_Strivings_c + 
    IA_LABEL_PR_LvsH * Perf_Strivings_c +
    CESD_c + GAD7_c +   
    pb(subject_nr) +    
    pb(block_loop) +    
    pb(Change_category01, by = block_loop),    
  sigma.formula = ~ guessed_block_rule,   
  family = BE(mu.link = "logit"),    
  data = na.omit(sub_et_qu)
) 
summary(bm2.1_first_fix_latency)
# Family:  c("BE", "Beta") 
# 
# Call:  gamlss(formula = first_fix_latency ~ IA_LABEL_EV * Perf_Strivings_c +  
#     IA_LABEL_PR_SvsF * Perf_Strivings_c + IA_LABEL_PR_LvsH *  
#     Perf_Strivings_c + CESD_c + GAD7_c + pb(subject_nr) + pb(block_loop) +  
#     pb(Change_category01, by = block_loop), sigma.formula = ~guessed_block_rule,  
#     family = BE(mu.link = "logit"), data = na.omit(sub_et_qu)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  logit
# Mu Coefficients:
#                                          Estimate Std. Error  t value Pr(>|t|)    
# (Intercept)                            -1.391e+00  3.700e-03 -375.904  < 2e-16 ***
# IA_LABEL_EV                             1.089e-02  2.372e-03    4.590 4.48e-06 ***
# Perf_Strivings_c                        3.755e-04  9.878e-04    0.380    0.704    
# IA_LABEL_PR_SvsF                       -5.427e-03  3.354e-03   -1.618    0.106    
# IA_LABEL_PR_LvsH                        1.164e-03  1.021e-03    1.141    0.254    
# CESD_c                                 -2.376e-05  2.107e-04   -0.113    0.910    
# GAD7_c                                  9.816e-05  3.607e-04    0.272    0.786    
# pb(subject_nr)                         -6.200e-08  4.240e-07   -0.146    0.884    
# pb(block_loop)                          2.156e-04  5.223e-04    0.413    0.680    
# pb(Change_category01, by = block_loop)  2.553e-02  3.734e-03    6.839 8.43e-12 ***
# IA_LABEL_EV:Perf_Strivings_c           -1.248e-03  1.316e-03   -0.948    0.343    
# Perf_Strivings_c:IA_LABEL_PR_SvsF       1.570e-03  1.861e-03    0.844    0.399    
# Perf_Strivings_c:IA_LABEL_PR_LvsH       3.223e-04  5.663e-04    0.569    0.569    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                    Estimate Std. Error  t value Pr(>|t|)    
# (Intercept)        -2.72407    0.01732 -157.241   <2e-16 ***
# guessed_block_rule -0.02106    0.01917   -1.099    0.272    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms may not be reliable. 
# ------------------------------------------------------------------
# No. of observations in the fit:  10145 
# Degrees of Freedom for the fit:  15.50448
#       Residual Deg. of Freedom:  10129.5 
#                       at cycle:  5 
#  
# Global Deviance:     -46742.95 
#             AIC:     -46711.94 
#             SBC:     -46599.92 

# Back-transformed coefficients
print(round(inv.logit(bm2.1_first_fix_latency$mu.coefficients),4))
# (Intercept)                            IA_LABEL_EV 
# 0.1993                                 0.5027 
# Perf_Strivings_c                       IA_LABEL_PR_SvsF 
# 0.5001                                 0.4986 
# IA_LABEL_PR_LvsH                                 CESD_c 
# 0.5003                                 0.5000 
# GAD7_c                         pb(subject_nr) 
# 0.5000                                 0.5000 
# pb(block_loop) pb(Change_category01, by = block_loop) 
# 0.5001                                 0.5064 
# IA_LABEL_EV:Perf_Strivings_c      Perf_Strivings_c:IA_LABEL_PR_SvsF 
# 0.4997                                 0.5004 
# Perf_Strivings_c:IA_LABEL_PR_LvsH 
# 0.5001 

# Model comparison
lrtest(bm1_first_fix_latency, bm2.1_first_fix_latency)
#      #Df LogLik Df  Chisq Pr(>Chisq)
# 1 11.504  23371                     
# 2 15.504  23372  4 1.3015     0.8611

AIC(bm2.1_first_fix_latency, bm1_first_fix_latency)
#                               df       AIC
# bm1_first_fix_latency   11.50444 -46718.64
# bm2.1_first_fix_latency 15.50448 -46711.94

# Full Model: Perf_Concerns
bm2.2_first_fix_latency <- gamlss(
  first_fix_latency ~ 
    IA_LABEL_EV * Perf_Concerns_c + 
    IA_LABEL_PR_SvsF * Perf_Concerns_c +
    IA_LABEL_PR_LvsH * Perf_Concerns_c +
    CESD_c + GAD7_c +   
    pb(subject_nr) +    
    pb(block_loop) +    
    pb(Change_category01, by = block_loop),    
  sigma.formula = ~ guessed_block_rule,   
  family = BE(mu.link = "logit"),    
  data = na.omit(sub_et_qu)
)
summary(bm2.2_first_fix_latency)
# Family:  c("BE", "Beta") 
# 
# Call:  gamlss(formula = first_fix_latency ~ IA_LABEL_EV * Perf_Concerns_c +  
#     IA_LABEL_PR_SvsF * Perf_Concerns_c + IA_LABEL_PR_LvsH * Perf_Concerns_c +  
#     CESD_c + GAD7_c + pb(subject_nr) + pb(block_loop) + pb(Change_category01,  
#     by = block_loop), sigma.formula = ~guessed_block_rule, family = BE(mu.link = "logit"),  
#     data = na.omit(sub_et_qu)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  logit
# Mu Coefficients:
#                                          Estimate Std. Error  t value Pr(>|t|)    
# (Intercept)                            -1.391e+00  3.707e-03 -375.254  < 2e-16 ***
# IA_LABEL_EV                             1.089e-02  2.372e-03    4.591 4.46e-06 ***
# Perf_Concerns_c                        -1.435e-04  8.435e-04   -0.170    0.865    
# IA_LABEL_PR_SvsF                       -5.430e-03  3.354e-03   -1.619    0.105    
# IA_LABEL_PR_LvsH                        1.164e-03  1.021e-03    1.141    0.254    
# CESD_c                                 -1.731e-05  2.203e-04   -0.079    0.937    
# GAD7_c                                  1.049e-04  3.632e-04    0.289    0.773    
# pb(subject_nr)                         -5.242e-08  4.292e-07   -0.122    0.903    
# pb(block_loop)                          2.165e-04  5.223e-04    0.415    0.678    
# pb(Change_category01, by = block_loop)  2.553e-02  3.734e-03    6.839 8.44e-12 ***
# IA_LABEL_EV:Perf_Concerns_c            -1.305e-03  1.008e-03   -1.295    0.195    
# Perf_Concerns_c:IA_LABEL_PR_SvsF        1.751e-03  1.425e-03    1.229    0.219    
# Perf_Concerns_c:IA_LABEL_PR_LvsH       -1.486e-04  4.336e-04   -0.343    0.732    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        -2.72409    0.01732  -157.2   <2e-16 ***
# guessed_block_rule -0.02109    0.01917    -1.1    0.271    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms may not be reliable. 
# ------------------------------------------------------------------
# No. of observations in the fit:  10145 
# Degrees of Freedom for the fit:  15.50451
#       Residual Deg. of Freedom:  10129.5 
#                       at cycle:  5 
#  
# Global Deviance:     -46743.63 
#             AIC:     -46712.62 
#             SBC:     -46600.61 

# Back-transformed coefficients
print(round(inv.logit(bm2.2_first_fix_latency$mu.coefficients),4))
# (Intercept)                            IA_LABEL_EV 
# 0.1993                                 0.5027 
# Perf_Concerns_c                       IA_LABEL_PR_SvsF 
# 0.5000                                 0.4986 
# IA_LABEL_PR_LvsH                                 CESD_c 
# 0.5003                                 0.5000 
# GAD7_c                         pb(subject_nr) 
# 0.5000                                 0.5000 
# pb(block_loop) pb(Change_category01, by = block_loop) 
# 0.5001                                 0.5064 
# IA_LABEL_EV:Perf_Concerns_c       Perf_Concerns_c:IA_LABEL_PR_SvsF 
# 0.4997                                 0.5004 
# Perf_Concerns_c:IA_LABEL_PR_LvsH 
# 0.5000 

# Model comparison
lrtest(bm1_first_fix_latency, bm2.2_first_fix_latency)
#      #Df LogLik     Df  Chisq Pr(>Chisq)
# 1 11.504  23371                         
# 2 15.505  23372 4.0001 1.9848     0.7386

AIC(bm2.2_first_fix_latency, bm1_first_fix_latency)
#                               df       AIC
# bm1_first_fix_latency   11.50444 -46718.64
# bm2.2_first_fix_latency 15.50451 -46712.62

# Fit a Beta glmm for Dwell Time
#-------------------------------

# Early Phase
#-------------

# Null model
bm0_dwell_time_early <- gamlss(
  dwell_time_early_t01 ~ 
    CESD_c + GAD7_c +   
    pb(subject_nr) +    
    pb(block_loop) +    
    pb(Change_category01, by = block_loop),    
  sigma.formula = ~ guessed_block_rule,   
  family = BE(mu.link = "logit"),    
  data = na.omit(sub_et_qu)
)
summary(bm0_dwell_time_early)
# Family:  c("BE", "Beta") 
# 
# Call:  gamlss(formula = dwell_time_early_t01 ~ CESD_c + GAD7_c +  
#     pb(subject_nr) + pb(block_loop) + pb(Change_category01,  
#     by = block_loop), sigma.formula = ~guessed_block_rule,  
#     family = BE(mu.link = "logit"), data = na.omit(sub_et_qu)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  logit
# Mu Coefficients:
#                                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                            -1.421e+00  1.532e-02 -92.754  < 2e-16 ***
# CESD_c                                  3.278e-03  9.331e-04   3.513 0.000446 ***
# GAD7_c                                 -7.696e-03  1.564e-03  -4.920 8.77e-07 ***
# pb(subject_nr)                         -1.996e-07  6.724e-07  -0.297 0.766554    
# pb(block_loop)                          2.758e-03  2.324e-03   1.186 0.235483    
# pb(Change_category01, by = block_loop)  1.559e-02  1.665e-02   0.936 0.349177    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        -0.93221    0.01976 -47.173   <2e-16 ***
# guessed_block_rule  0.02886    0.02160   1.336    0.182    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms maybe are not accurate. 
# ------------------------------------------------------------------
# No. of observations in the fit:  10145 
# Degrees of Freedom for the fit:  8.992601
#       Residual Deg. of Freedom:  10136.01 
#                       at cycle:  6 
#  
# Global Deviance:     -17024.24 
#             AIC:     -17006.26 
#             SBC:     -16941.29 

# Back-transformed coefficients
print(round(inv.logit(bm0_dwell_time_early$mu.coefficients),4))
# (Intercept)                                 CESD_c 
# 0.1946                                 0.5008 
# GAD7_c                         pb(subject_nr) 
# 0.4981                                 0.5000 
# pb(block_loop) pb(Change_category01, by = block_loop) 
# 0.5007                                 0.5039 

# Partial Model
bm1_dwell_time_early <- gamlss(
  dwell_time_early_t01 ~ 
    IA_LABEL_EV + 
    IA_LABEL_PR_SvsF + 
    IA_LABEL_PR_LvsH + 
    CESD_c + GAD7_c +   
    pb(subject_nr) +    
    pb(block_loop) +    
    pb(Change_category01, by = block_loop),    
  sigma.formula = ~ guessed_block_rule,   
  family = BE(mu.link = "logit"),    
  data = na.omit(sub_et_qu)
) 
summary(bm1_dwell_time_early)
# Family:  c("BE", "Beta") 
# 
# Call:  gamlss(formula = dwell_time_early_t01 ~ IA_LABEL_EV + IA_LABEL_PR_SvsF +  
#     IA_LABEL_PR_LvsH + CESD_c + GAD7_c + pb(subject_nr) + pb(block_loop) +  
#     pb(Change_category01, by = block_loop), sigma.formula = ~guessed_block_rule,  
#     family = BE(mu.link = "logit"), data = na.omit(sub_et_qu)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  logit
# Mu Coefficients:
#                                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                            -1.418e+00  1.669e-02 -84.963  < 2e-16 ***
# IA_LABEL_EV                            -2.704e-02  1.071e-02  -2.526  0.01157 *  
# IA_LABEL_PR_SvsF                        4.459e-02  1.513e-02   2.947  0.00321 ** 
# IA_LABEL_PR_LvsH                        2.709e-03  4.603e-03   0.589  0.55620    
# CESD_c                                  3.289e-03  9.417e-04   3.493  0.00048 ***
# GAD7_c                                 -7.704e-03  1.626e-03  -4.738 2.19e-06 ***
# pb(subject_nr)                         -1.613e-07  1.886e-06  -0.086  0.93185    
# pb(block_loop)                          2.646e-03  2.356e-03   1.123  0.26142    
# pb(Change_category01, by = block_loop)  1.475e-02  1.689e-02   0.874  0.38239    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        -0.93609    0.01960 -47.767   <2e-16 ***
# guessed_block_rule  0.03286    0.02170   1.514     0.13    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms may not be reliable. 
# ------------------------------------------------------------------
# No. of observations in the fit:  10145 
# Degrees of Freedom for the fit:  11.9903
#       Residual Deg. of Freedom:  10133.01 
#                       at cycle:  6 
#  
# Global Deviance:     -17033.62 
#             AIC:     -17009.64 
#             SBC:     -16923.02

# Back-transformed coefficients
print(round(inv.logit(bm1_dwell_time_early$mu.coefficients),4))
# (Intercept)                            IA_LABEL_EV 
# 0.1950                                 0.4932 
# IA_LABEL_PR_SvsF                       IA_LABEL_PR_LvsH 
# 0.5111                                 0.5007 
# CESD_c                                 GAD7_c 
# 0.5008                                 0.4981 
# pb(subject_nr)                         pb(block_loop) 
# 0.5000                                 0.5007 
# pb(Change_category01, by = block_loop) 
# 0.5037 

# Model comparison
lrtest(bm1_dwell_time_early, bm0_dwell_time_early)
#       #Df LogLik      Df  Chisq Pr(>Chisq)  
# 1 11.9903 8516.8                            
# 2  8.9926 8512.1 -2.9977 9.3802    0.02464 *

AIC(bm1_dwell_time_early, bm0_dwell_time_early)
#                             df       AIC
# bm1_dwell_time_early 11.990301 -17009.64
# bm0_dwell_time_early  8.992601 -17006.26

# Full Model: Perf_Strivings
bm2.1_dwell_time_early <- gamlss(
  dwell_time_early_t01 ~ 
    IA_LABEL_EV * Perf_Strivings_c + 
    IA_LABEL_PR_SvsF * Perf_Strivings_c + 
    IA_LABEL_PR_LvsH * Perf_Strivings_c + 
    CESD_c + GAD7_c +   
    pb(subject_nr) +    
    pb(block_loop) +    
    pb(Change_category01, by = block_loop),    
  sigma.formula = ~ guessed_block_rule,   
  family = BE(mu.link = "logit"),    
  data = na.omit(sub_et_qu)
) 
summary(bm2.1_dwell_time_early)
# Family:  c("BE", "Beta") 
# 
# Call:  gamlss(formula = dwell_time_early_t01 ~ IA_LABEL_EV * Perf_Strivings_c +  
#     IA_LABEL_PR_SvsF * Perf_Strivings_c + IA_LABEL_PR_LvsH *  
#     Perf_Strivings_c + CESD_c + GAD7_c + pb(subject_nr) + pb(block_loop) +  
#     pb(Change_category01, by = block_loop), sigma.formula = ~guessed_block_rule,  
#     family = BE(mu.link = "logit"), data = na.omit(sub_et_qu)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  logit
# Mu Coefficients:
#                                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                            -1.420e+00  1.670e-02 -85.018  < 2e-16 ***
# IA_LABEL_EV                            -2.731e-02  1.070e-02  -2.553  0.01070 *  
# Perf_Strivings_c                       -9.228e-03  4.449e-03  -2.074  0.03812 *  
# IA_LABEL_PR_SvsF                        4.478e-02  1.512e-02   2.961  0.00307 ** 
# IA_LABEL_PR_LvsH                        2.738e-03  4.601e-03   0.595  0.55186    
# CESD_c                                  3.503e-03  9.507e-04   3.685  0.00023 ***
# GAD7_c                                 -7.431e-03  1.633e-03  -4.550 5.44e-06 ***
# pb(subject_nr)                          2.392e-07  1.901e-06   0.126  0.89987    
# pb(block_loop)                          2.765e-03  2.355e-03   1.174  0.24035    
# pb(Change_category01, by = block_loop)  1.441e-02  1.688e-02   0.854  0.39312    
# IA_LABEL_EV:Perf_Strivings_c            1.194e-02  5.935e-03   2.012  0.04422 *  
# Perf_Strivings_c:IA_LABEL_PR_SvsF       3.132e-04  8.384e-03   0.037  0.97020    
# Perf_Strivings_c:IA_LABEL_PR_LvsH      -3.761e-03  2.551e-03  -1.474  0.14039    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        -0.93874    0.01959 -47.912   <2e-16 ***
# guessed_block_rule  0.03505    0.02169   1.615    0.106    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms may not be reliable. 
# ------------------------------------------------------------------
# No. of observations in the fit:  10145 
# Degrees of Freedom for the fit:  16.00245
#       Residual Deg. of Freedom:  10129 
#                       at cycle:  6 
#  
# Global Deviance:     -17046.65 
#             AIC:     -17014.64 
#             SBC:     -16899.03 
            
# Back-transformed coefficients
print(round(inv.logit(bm2.1_dwell_time_early$mu.coefficients),4))
# (Intercept)                            IA_LABEL_EV 
# 0.1947                                 0.4932 
# Perf_Strivings_c                       IA_LABEL_PR_SvsF 
# 0.4977                                 0.5112 
# IA_LABEL_PR_LvsH                                 CESD_c 
# 0.5007                                 0.5009 
# GAD7_c                         pb(subject_nr) 
# 0.4981                                 0.5000 
# pb(block_loop) pb(Change_category01, by = block_loop) 
# 0.5007                                 0.5036 
# IA_LABEL_EV:Perf_Strivings_c      Perf_Strivings_c:IA_LABEL_PR_SvsF 
# 0.5030                                 0.5001 
# Perf_Strivings_c:IA_LABEL_PR_LvsH 
# 0.4991

# Model comparison
lrtest(bm2.1_dwell_time_early, bm1_dwell_time_early)
#      #Df LogLik      Df  Chisq Pr(>Chisq)  
# 1 16.002 8523.3                            
# 2 11.990 8516.8 -4.0122 13.023    0.01117 *

AIC(bm2.1_dwell_time_early, bm1_dwell_time_early)
#                              df       AIC
# bm2.1_dwell_time_early 16.00245 -17014.64
# bm1_dwell_time_early   11.99030 -17009.64

# Full Model: Perf_Concerns
bm2.2_dwell_time_early <- gamlss(
  dwell_time_early_t01 ~ 
    IA_LABEL_EV * Perf_Concerns_c +
    IA_LABEL_PR_SvsF * Perf_Concerns_c + 
    IA_LABEL_PR_LvsH * Perf_Concerns_c + 
    CESD_c + GAD7_c +   
    pb(subject_nr) +    
    pb(block_loop) +    
    pb(Change_category01, by = block_loop),    
  sigma.formula = ~ guessed_block_rule,   
  family = BE(mu.link = "logit"),    
  data = na.omit(sub_et_qu)
) 
summary(bm2.2_dwell_time_early)
# Family:  c("BE", "Beta") 
# 
# Call:  gamlss(formula = dwell_time_early_t01 ~ IA_LABEL_EV * Perf_Concerns_c +  
#     IA_LABEL_PR_SvsF * Perf_Concerns_c + IA_LABEL_PR_LvsH * Perf_Concerns_c +  
#     CESD_c + GAD7_c + pb(subject_nr) + pb(block_loop) + pb(Change_category01,  
#     by = block_loop), sigma.formula = ~guessed_block_rule, family = BE(mu.link = "logit"),  
#     data = na.omit(sub_et_qu)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  logit
# Mu Coefficients:
#                                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                            -1.418e+00  1.674e-02 -84.737  < 2e-16 ***
# IA_LABEL_EV                            -2.705e-02  1.070e-02  -2.527 0.011526 *  
# Perf_Concerns_c                        -4.455e-03  3.815e-03  -1.168 0.242931    
# IA_LABEL_PR_SvsF                        4.456e-02  1.513e-02   2.946 0.003227 ** 
# IA_LABEL_PR_LvsH                        2.720e-03  4.603e-03   0.591 0.554539    
# CESD_c                                  3.452e-03  9.947e-04   3.471 0.000521 ***
# GAD7_c                                 -7.583e-03  1.646e-03  -4.608 4.12e-06 ***
# pb(subject_nr)                          2.027e-08  1.925e-06   0.011 0.991601    
# pb(block_loop)                          2.580e-03  2.356e-03   1.095 0.273442    
# pb(Change_category01, by = block_loop)  1.472e-02  1.688e-02   0.872 0.383461    
# IA_LABEL_EV:Perf_Concerns_c            -1.014e-03  4.553e-03  -0.223 0.823712    
# Perf_Concerns_c:IA_LABEL_PR_SvsF        3.192e-03  6.437e-03   0.496 0.619955    
# Perf_Concerns_c:IA_LABEL_PR_LvsH       -3.434e-03  1.959e-03  -1.753 0.079593 .  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        -0.93620    0.01960 -47.774   <2e-16 ***
# guessed_block_rule  0.03271    0.02170   1.507    0.132    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms may not be reliable. 
# ------------------------------------------------------------------
# No. of observations in the fit:  10145 
# Degrees of Freedom for the fit:  15.99017
#       Residual Deg. of Freedom:  10129.01 
#                       at cycle:  6 
#  
# Global Deviance:     -17037.18 
#             AIC:     -17005.2 
#             SBC:     -16889.67 

# Back-transformed coefficients
print(round(inv.logit(bm2.2_dwell_time_early$mu.coefficients),4))
# (Intercept)                            IA_LABEL_EV 
# 0.1949                                 0.4932 
# Perf_Concerns_c                       IA_LABEL_PR_SvsF 
# 0.4989                                 0.5111 
# IA_LABEL_PR_LvsH                                 CESD_c 
# 0.5007                                 0.5009 
# GAD7_c                         pb(subject_nr) 
# 0.4981                                 0.5000 
# pb(block_loop) pb(Change_category01, by = block_loop) 
# 0.5006                                 0.5037 
# IA_LABEL_EV:Perf_Concerns_c       Perf_Concerns_c:IA_LABEL_PR_SvsF 
# 0.4997                                 0.5008 
# Perf_Concerns_c:IA_LABEL_PR_LvsH 
# 0.4991 

# Model comparison
lrtest(bm2.2_dwell_time_early, bm1_dwell_time_early)
#     #Df LogLik      Df  Chisq Pr(>Chisq)
# 1 15.99 8518.6                          
# 2 11.99 8516.8 -3.9999 3.5528     0.4699

AIC(bm2.2_dwell_time_early, bm1_dwell_time_early)
#                              df       AIC
# bm1_dwell_time_early   11.99030 -17009.64
# bm2.2_dwell_time_early 15.99017 -17005.20

# Intermediate Phase
#-------------------

# Null model
bm0_dwell_time_intermediate <- gamlss(
  dwell_time_intermediate_t01 ~ 
    CESD_c + GAD7_c +   
    pb(subject_nr) +    
    pb(block_loop) +    
    pb(Change_category01, by = block_loop),    
  sigma.formula = ~ guessed_block_rule,   
  family = BE(mu.link = "logit"),    
  data = na.omit(sub_et_qu)
)
summary(bm0_dwell_time_intermediate)
# Family:  c("BE", "Beta") 
# 
# Call:  gamlss(formula = dwell_time_intermediate_t01 ~ CESD_c +  
#     GAD7_c + pb(subject_nr) + pb(block_loop) + pb(Change_category01,  
#     by = block_loop), sigma.formula = ~guessed_block_rule,  
#     family = BE(mu.link = "logit"), data = na.omit(sub_et_qu)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  logit
# Mu Coefficients:
#                                          Estimate Std. Error  t value Pr(>|t|)    
# (Intercept)                            -1.442e+00  1.366e-02 -105.505   <2e-16 ***
# CESD_c                                  1.581e-04  5.825e-04    0.271    0.786    
# GAD7_c                                 -2.810e-04  1.032e-03   -0.272    0.785    
# pb(subject_nr)                          2.605e-07  1.568e-06    0.166    0.868    
# pb(block_loop)                          3.789e-04  1.570e-03    0.241    0.809    
# pb(Change_category01, by = block_loop)  2.523e-01  1.106e-02   22.814   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        -1.47710    0.01925 -76.738   <2e-16 ***
# guessed_block_rule  0.02016    0.02130   0.946    0.344    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms maybe are not accurate. 
# ------------------------------------------------------------------
# No. of observations in the fit:  10145 
# Degrees of Freedom for the fit:  8.798012
#       Residual Deg. of Freedom:  10136.2 
#                       at cycle:  6 
#  
# Global Deviance:     -24333.73 
#             AIC:     -24316.13 
#             SBC:     -24252.57 

# Back-transformed coefficients
print(round(inv.logit(bm0_dwell_time_intermediate$mu.coefficients),4))
# (Intercept)                                 CESD_c 
# 0.1913                                 0.5000 
# GAD7_c                         pb(subject_nr) 
# 0.4999                                 0.5000 
# pb(block_loop) pb(Change_category01, by = block_loop) 
# 0.5001                                 0.5627

# Partial Model
bm1_dwell_time_intermediate <- gamlss(
  dwell_time_intermediate_t01 ~ 
    IA_LABEL_EV + 
    IA_LABEL_PR_SvsF + 
    IA_LABEL_PR_LvsH + 
    CESD_c + GAD7_c +   
    pb(subject_nr) +    
    pb(block_loop) +    
    pb(Change_category01, by = block_loop),    
  sigma.formula = ~ guessed_block_rule,   
  family = BE(mu.link = "logit"),    
  data = na.omit(sub_et_qu)
)
summary(bm1_dwell_time_intermediate)
# Family:  c("BE", "Beta") 
# 
# Call:  gamlss(formula = dwell_time_intermediate_t01 ~ IA_LABEL_EV +  
#     IA_LABEL_PR_SvsF + IA_LABEL_PR_LvsH + CESD_c +  
#     GAD7_c + pb(subject_nr) + pb(block_loop) + pb(Change_category01,  
#     by = block_loop), sigma.formula = ~guessed_block_rule,  
#     family = BE(mu.link = "logit"), data = na.omit(sub_et_qu)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  logit
# Mu Coefficients:
#                                          Estimate Std. Error  t value Pr(>|t|)    
# (Intercept)                            -1.443e+00  1.213e-02 -118.971   <2e-16 ***
# IA_LABEL_EV                            -6.137e-03  9.322e-03   -0.658    0.510    
# IA_LABEL_PR_SvsF                       -1.089e-02  1.540e-02   -0.707    0.479    
# IA_LABEL_PR_LvsH                       -5.182e-04  4.275e-03   -0.121    0.904    
# CESD_c                                  1.572e-04  6.271e-04    0.251    0.802    
# GAD7_c                                 -2.729e-04  1.244e-03   -0.219    0.826    
# pb(subject_nr)                          2.799e-07  4.917e-06    0.057    0.955    
# pb(block_loop)                          4.641e-04  2.762e-03    0.168    0.867    
# pb(Change_category01, by = block_loop)  2.523e-01  1.138e-02   22.162   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        -1.47802    0.02150 -68.749   <2e-16 ***
# guessed_block_rule  0.02083    0.02439   0.854    0.393    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms maybe are not accurate. 
# ------------------------------------------------------------------
# No. of observations in the fit:  10145 
# Degrees of Freedom for the fit:  11.79813
#       Residual Deg. of Freedom:  10133.2 
#                       at cycle:  6 
#  
# Global Deviance:     -24340 
#             AIC:     -24316.4 
#             SBC:     -24231.16 

# Back-transformed coefficients
print(round(inv.logit(bm1_dwell_time_intermediate$mu.coefficients),4))
# (Intercept)                            IA_LABEL_EV 
# 0.1912                                 0.4985 
# IA_LABEL_PR_SvsF                       IA_LABEL_PR_LvsH 
# 0.4973                                 0.4999 
# CESD_c                                 GAD7_c 
# 0.5000                                 0.4999 
# pb(subject_nr)                         pb(block_loop) 
# 0.5000                                 0.5001 
# pb(Change_category01, by = block_loop) 
# 0.5627 

# Model comparison
lrtest(bm1_dwell_time_intermediate, bm0_dwell_time_intermediate)
#      #Df LogLik      Df  Chisq Pr(>Chisq)  
# 1 11.798  12170                            
# 2  8.798  12167 -3.0001 6.2744      0.099 .

AIC(bm1_dwell_time_intermediate, bm0_dwell_time_intermediate)
#                                    df       AIC
# bm1_dwell_time_intermediate 11.798132 -24316.40
# bm0_dwell_time_intermediate  8.798012 -24316.13

# Full Model: Perf_Strivings
bm2.1_dwell_time_intermediate <- gamlss(
  dwell_time_intermediate_t01 ~ 
    IA_LABEL_EV * Perf_Strivings_c + 
    IA_LABEL_PR_SvsF * Perf_Strivings_c + 
    IA_LABEL_PR_LvsH * Perf_Strivings_c + 
    CESD_c + GAD7_c +   
    pb(subject_nr) +    
    pb(block_loop) +    
    pb(Change_category01, by = block_loop),    
  sigma.formula = ~ guessed_block_rule,   
  family = BE(mu.link = "logit"),    
  data = na.omit(sub_et_qu)
) 
summary(bm2.1_dwell_time_intermediate)
# Family:  c("BE", "Beta") 
# 
# Call:  gamlss(formula = dwell_time_intermediate_t01 ~ IA_LABEL_EV *  
#     Perf_Strivings_c + IA_LABEL_PR_SvsF * Perf_Strivings_c +  
#     IA_LABEL_PR_LvsH * Perf_Strivings_c + CESD_c + GAD7_c + pb(subject_nr) +  
#     pb(block_loop) + pb(Change_category01, by = block_loop),  
#     sigma.formula = ~guessed_block_rule, family = BE(mu.link = "logit"),  
#     data = na.omit(sub_et_qu)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  logit
# Mu Coefficients:
#                                          Estimate Std. Error  t value Pr(>|t|)    
# (Intercept)                            -1.443e+00  1.128e-02 -127.874  < 2e-16 ***
# IA_LABEL_EV                            -6.106e-03  7.229e-03   -0.845  0.39829    
# Perf_Strivings_c                        2.170e-04  3.011e-03    0.072  0.94254    
# IA_LABEL_PR_SvsF                       -1.093e-02  1.023e-02   -1.069  0.28522    
# IA_LABEL_PR_LvsH                       -5.222e-04  3.113e-03   -0.168  0.86679    
# CESD_c                                  2.291e-04  6.432e-04    0.356  0.72173    
# GAD7_c                                 -1.944e-04  1.102e-03   -0.176  0.85999    
# pb(subject_nr)                          3.848e-07  1.289e-06    0.299  0.76531    
# pb(block_loop)                          3.851e-04  1.593e-03    0.242  0.80893    
# pb(Change_category01, by = block_loop)  2.522e-01  1.102e-02   22.899  < 2e-16 ***
# IA_LABEL_EV:Perf_Strivings_c           -1.151e-02  4.005e-03   -2.873  0.00407 ** 
# Perf_Strivings_c:IA_LABEL_PR_SvsF       1.061e-02  5.668e-03    1.872  0.06130 .  
# Perf_Strivings_c:IA_LABEL_PR_LvsH       2.833e-03  1.725e-03    1.642  0.10059    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        -1.47927    0.01883 -78.542   <2e-16 ***
# guessed_block_rule  0.02153    0.02085   1.032    0.302    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms may not be reliable. 
# ------------------------------------------------------------------
# No. of observations in the fit:  10145 
# Degrees of Freedom for the fit:  15.81058
#       Residual Deg. of Freedom:  10129.19 
#                       at cycle:  6 
#  
# Global Deviance:     -24351.52 
#             AIC:     -24319.9 
#             SBC:     -24205.68

# Back-transformed coefficients
print(round(inv.logit(bm2.1_dwell_time_intermediate$mu.coefficients),4))
# (Intercept)                            IA_LABEL_EV 
# 0.1912                                 0.4985 
# Perf_Strivings_c                       IA_LABEL_PR_SvsF 
# 0.5001                                 0.4973 
# IA_LABEL_PR_LvsH                                 CESD_c 
# 0.4999                                 0.5001 
# GAD7_c                         pb(subject_nr) 
# 0.5000                                 0.5000 
# pb(block_loop) pb(Change_category01, by = block_loop) 
# 0.5001                                 0.5627 
# IA_LABEL_EV:Perf_Strivings_c      Perf_Strivings_c:IA_LABEL_PR_SvsF 
# 0.4971                                 0.5027 
# Perf_Strivings_c:IA_LABEL_PR_LvsH 
# 0.5007 

# Model comparison
lrtest(bm2.1_dwell_time_intermediate, bm1_dwell_time_intermediate)
#      #Df LogLik      Df  Chisq Pr(>Chisq)  
# 1 15.811  12176                            
# 2 11.798  12170 -4.0125 11.525    0.02126 *

AIC(bm2.1_dwell_time_intermediate, bm1_dwell_time_intermediate)
#                                     df      AIC
# bm2.1_dwell_time_intermediate 15.81058 -24319.9
# bm1_dwell_time_intermediate   11.79813 -24316.4

  # Full Model: Perf_Concerns
bm2.2_dwell_time_intermediate <- gamlss(
  dwell_time_intermediate_t01 ~ 
    IA_LABEL_EV * Perf_Concerns_c +
    IA_LABEL_PR_SvsF * Perf_Concerns_c + 
    IA_LABEL_PR_LvsH * Perf_Concerns_c + 
    CESD_c + GAD7_c +   
    pb(subject_nr) +    
    pb(block_loop) +    
    pb(Change_category01, by = block_loop),    
  sigma.formula = ~ guessed_block_rule,   
  family = BE(mu.link = "logit"),    
  data = na.omit(sub_et_qu)
) 
summary(bm2.2_dwell_time_intermediate)
# Family:  c("BE", "Beta") 
# 
# Call:  gamlss(formula = dwell_time_intermediate_t01 ~ IA_LABEL_EV *  
#     Perf_Concerns_c + IA_LABEL_PR_SvsF * Perf_Concerns_c + IA_LABEL_PR_LvsH *  
#     Perf_Concerns_c + CESD_c + GAD7_c + pb(subject_nr) + pb(block_loop) +  
#     pb(Change_category01, by = block_loop), sigma.formula = ~guessed_block_rule,  
#     family = BE(mu.link = "logit"), data = na.omit(sub_et_qu)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  logit
# Mu Coefficients:
#                                          Estimate Std. Error  t value Pr(>|t|)    
# (Intercept)                            -1.443e+00  1.130e-02 -127.675  < 2e-16 ***
# IA_LABEL_EV                            -6.025e-03  7.229e-03   -0.833  0.40461    
# Perf_Concerns_c                         9.953e-04  2.574e-03    0.387  0.69902    
# IA_LABEL_PR_SvsF                       -1.101e-02  1.023e-02   -1.077  0.28163    
# IA_LABEL_PR_LvsH                       -5.305e-04  3.113e-03   -0.170  0.86469    
# CESD_c                                  3.433e-04  6.723e-04    0.511  0.60959    
# GAD7_c                                 -1.422e-04  1.110e-03   -0.128  0.89800    
# pb(subject_nr)                          4.867e-07  1.305e-06    0.373  0.70918    
# pb(block_loop)                          3.866e-04  1.593e-03    0.243  0.80821    
# pb(Change_category01, by = block_loop)  2.521e-01  1.102e-02   22.888  < 2e-16 ***
# IA_LABEL_EV:Perf_Concerns_c            -5.536e-03  3.076e-03   -1.800  0.07191 .  
# Perf_Concerns_c:IA_LABEL_PR_SvsF        5.542e-03  4.349e-03    1.274  0.20255    
# Perf_Concerns_c:IA_LABEL_PR_LvsH        3.688e-03  1.323e-03    2.787  0.00533 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        -1.47882    0.01883 -78.514   <2e-16 ***
# guessed_block_rule  0.02096    0.02085   1.005    0.315    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms may not be reliable. 
# ------------------------------------------------------------------
# No. of observations in the fit:  10145 
# Degrees of Freedom for the fit:  15.79884
#       Residual Deg. of Freedom:  10129.2 
#                       at cycle:  6 
#  
# Global Deviance:     -24351.69 
#             AIC:     -24320.09 
#             SBC:     -24205.95

# Back-transformed coefficients
print(round(inv.logit(bm2.2_dwell_time_intermediate$mu.coefficients),4))
# (Intercept)                            IA_LABEL_EV 
# 0.1911                                 0.4985 
# Perf_Concerns_c                       IA_LABEL_PR_SvsF 
# 0.5002                                 0.4972 
# IA_LABEL_PR_LvsH                                 CESD_c 
# 0.4999                                 0.5001 
# GAD7_c                         pb(subject_nr) 
# 0.5000                                 0.5000 
# pb(block_loop) pb(Change_category01, by = block_loop) 
# 0.5001                                 0.5627 
# IA_LABEL_EV:Perf_Concerns_c       Perf_Concerns_c:IA_LABEL_PR_SvsF 
# 0.4986                                 0.5014 
# Perf_Concerns_c:IA_LABEL_PR_LvsH 
# 0.5009

# Model comparison
lrtest(bm2.2_dwell_time_intermediate, bm1_dwell_time_intermediate)
#      #Df LogLik      Df  Chisq Pr(>Chisq)  
# 1 15.799  12176                            
# 2 11.798  12170 -4.0007 11.687    0.01984 *

AIC(bm2.2_dwell_time_intermediate, bm1_dwell_time_intermediate)
#                                     df       AIC
# bm2.2_dwell_time_intermediate 15.79884 -24320.09
# bm1_dwell_time_intermediate   11.79813 -24316.40

#Late Phase
#----------

# Null model
bm0_dwell_time_late <- gamlss(
  dwell_time_late_t01 ~ 
    CESD_c + GAD7_c +   
    pb(subject_nr) +    
    pb(block_loop) +    
    pb(Change_category01, by = block_loop),    
  sigma.formula = ~ guessed_block_rule,   
  family = BE(mu.link = "logit"),    
  data = na.omit(sub_et_qu)
)
summary(bm0_dwell_time_late)
# Family:  c("BE", "Beta") 
# 
# Call:  gamlss(formula = dwell_time_late_t01 ~ CESD_c + GAD7_c +  
#     pb(subject_nr) + pb(block_loop) + pb(Change_category01,  
#     by = block_loop), sigma.formula = ~guessed_block_rule,  
#     family = BE(mu.link = "logit"), data = na.omit(sub_et_qu)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  logit
# Mu Coefficients:
#                                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                            -1.480e+00  2.003e-02 -73.892  < 2e-16 ***
# CESD_c                                  2.146e-03  1.109e-03   1.936   0.0529 .  
# GAD7_c                                 -1.335e-03  1.926e-03  -0.693   0.4882    
# pb(subject_nr)                         -9.636e-06  2.245e-06  -4.292 1.79e-05 ***
# pb(block_loop)                          1.930e-03  2.785e-03   0.693   0.4883    
# pb(Change_category01, by = block_loop)  6.100e-01  1.939e-02  31.451  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        -0.80470    0.02010  -40.04   <2e-16 ***
# guessed_block_rule  0.25781    0.02198   11.73   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms maybe are not accurate. 
# ------------------------------------------------------------------
# No. of observations in the fit:  10145 
# Degrees of Freedom for the fit:  11.48459
#       Residual Deg. of Freedom:  10133.52 
#                       at cycle:  8 
#  
# Global Deviance:     -14393.45 
#             AIC:     -14370.48 
#             SBC:     -14287.51 

# Back-transformed coefficients
print(round(inv.logit(bm0_dwell_time_late$mu.coefficients),4))
# (Intercept)                                 CESD_c 
# 0.1855                                 0.5005 
# GAD7_c                         pb(subject_nr) 
# 0.4997                                 0.5000 
# pb(block_loop) pb(Change_category01, by = block_loop) 
# 0.5005                                 0.6479 

# Partial Model
bm1_dwell_time_late <- gamlss(
  dwell_time_late_t01 ~ 
    IA_LABEL_EV + 
    IA_LABEL_PR_SvsF + 
    IA_LABEL_PR_LvsH + 
    CESD_c + GAD7_c +   
    pb(subject_nr) +    
    pb(block_loop) +    
    pb(Change_category01, by = block_loop),    
  sigma.formula = ~ guessed_block_rule,   
  family = BE(mu.link = "logit"),    
  data = na.omit(sub_et_qu)
)
summary(bm1_dwell_time_late)
# Family:  c("BE", "Beta") 
# 
# Call:  gamlss(formula = dwell_time_late_t01 ~ IA_LABEL_EV +  
#     IA_LABEL_PR_SvsF + IA_LABEL_PR_LvsH + CESD_c +  
#     GAD7_c + pb(subject_nr) + pb(block_loop) + pb(Change_category01,  
#     by = block_loop), sigma.formula = ~guessed_block_rule,  
#     family = BE(mu.link = "logit"), data = na.omit(sub_et_qu)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  logit
# Mu Coefficients:
#                                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                            -1.473e+00  2.047e-02 -71.972  < 2e-16 ***
# IA_LABEL_EV                             7.766e-03  1.299e-02   0.598   0.5498    
# IA_LABEL_PR_SvsF                       -1.665e-02  1.823e-02  -0.913   0.3611    
# IA_LABEL_PR_LvsH                        9.011e-03  5.460e-03   1.650   0.0989 .  
# CESD_c                                  2.138e-03  1.084e-03   1.973   0.0486 *  
# GAD7_c                                 -1.328e-03  1.874e-03  -0.709   0.4785    
# pb(subject_nr)                         -9.630e-06  2.243e-06  -4.293 1.78e-05 ***
# pb(block_loop)                          2.055e-03  2.796e-03   0.735   0.4623    
# pb(Change_category01, by = block_loop)  6.103e-01  1.939e-02  31.475  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        -0.80520    0.02010  -40.06   <2e-16 ***
# guessed_block_rule  0.25809    0.02198   11.74   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms maybe are not accurate. 
# ------------------------------------------------------------------
# No. of observations in the fit:  10145 
# Degrees of Freedom for the fit:  14.5034
#       Residual Deg. of Freedom:  10130.5 
#                       at cycle:  8 
#  
# Global Deviance:     -14397.06 
#             AIC:     -14368.05 
#             SBC:     -14263.27 

# Back-transformed coefficients
print(round(inv.logit(bm1_dwell_time_late$mu.coefficients),4))
# (Intercept)                            IA_LABEL_EV 
# 0.1864                                 0.5019 
# IA_LABEL_PR_SvsF                       IA_LABEL_PR_LvsH 
# 0.4958                                 0.5023 
# CESD_c                                 GAD7_c 
# 0.5005                                 0.4997 
# pb(subject_nr)                         pb(block_loop) 
# 0.5000                                 0.5005 
# pb(Change_category01, by = block_loop) 
# 0.6480 

# Model comparison
lrtest(bm1_dwell_time_late, bm0_dwell_time_late)
#      #Df LogLik      Df  Chisq Pr(>Chisq)
# 1 14.503 7198.5                          
# 2 11.485 7196.7 -3.0188 3.6079      0.307

AIC(bm1_dwell_time_late, bm0_dwell_time_late)
#                           df       AIC
# bm0_dwell_time_late 11.48459 -14370.48
# bm1_dwell_time_late 14.50340 -14368.05

# Full Model: Perf_Strivings
bm2.1_dwell_time_late <- gamlss(
  dwell_time_late_t01 ~ 
    IA_LABEL_EV * Perf_Strivings_c + 
    IA_LABEL_PR_LvsH * Perf_Strivings_c + 
    IA_LABEL_PR_LvsH * Perf_Strivings_c +
    CESD_c + GAD7_c +   
    pb(subject_nr) +    
    pb(block_loop) +    
    pb(Change_category01, by = block_loop),    
  sigma.formula = ~ guessed_block_rule,   
  family = BE(mu.link = "logit"),    
  data = na.omit(sub_et_qu)
) 
summary(bm2.1_dwell_time_late)
# Family:  c("BE", "Beta") 
# 
# Call:  gamlss(formula = dwell_time_late_t01 ~ IA_LABEL_EV * Perf_Strivings_c +  
#     IA_LABEL_PR_LvsH * Perf_Strivings_c + IA_LABEL_PR_LvsH *  
#     Perf_Strivings_c + CESD_c + GAD7_c + pb(subject_nr) + pb(block_loop) +  
#     pb(Change_category01, by = block_loop), sigma.formula = ~guessed_block_rule,  
#     family = BE(mu.link = "logit"), data = na.omit(sub_et_qu)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  logit
# Mu Coefficients:
#                                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                            -1.473e+00  1.975e-02 -74.584  < 2e-16 ***
# IA_LABEL_EV                            -6.531e-04  8.948e-03  -0.073   0.9418    
# Perf_Strivings_c                        7.445e-03  5.240e-03   1.421   0.1554    
# IA_LABEL_PR_LvsH                        8.994e-03  5.446e-03   1.651   0.0987 .  
# CESD_c                                  2.066e-03  1.131e-03   1.827   0.0677 .  
# GAD7_c                                 -1.495e-03  1.942e-03  -0.770   0.4416    
# pb(subject_nr)                         -9.834e-06  2.241e-06  -4.389 1.15e-05 ***
# pb(block_loop)                          2.077e-03  2.790e-03   0.744   0.4567    
# pb(Change_category01, by = block_loop)  6.101e-01  1.898e-02  32.151  < 2e-16 ***
# IA_LABEL_EV:Perf_Strivings_c           -5.966e-06  4.938e-03  -0.001   0.9990    
# Perf_Strivings_c:IA_LABEL_PR_LvsH       6.037e-03  3.004e-03   2.010   0.0445 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        -0.80585    0.01974  -40.82   <2e-16 ***
# guessed_block_rule  0.25861    0.02190   11.81   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms may not be reliable. 
# ------------------------------------------------------------------
# No. of observations in the fit:  10145 
# Degrees of Freedom for the fit:  16.45963
#       Residual Deg. of Freedom:  10128.54 
#                       at cycle:  8 
#  
# Global Deviance:     -14400.37 
#             AIC:     -14367.45 
#             SBC:     -14248.53 

# Back-transformed coefficients
print(round(inv.logit(bm2.1_dwell_time_late$mu.coefficients),4))
# (Intercept)                            IA_LABEL_EV 
# 0.1865                                 0.4998 
# Perf_Strivings_c                       IA_LABEL_PR_LvsH 
# 0.5019                                 0.5022 
# CESD_c                                 GAD7_c 
# 0.5005                                 0.4996 
# pb(subject_nr)                         pb(block_loop) 
# 0.5000                                 0.5005 
# pb(Change_category01, by = block_loop)           IA_LABEL_EV:Perf_Strivings_c 
# 0.6480                                 0.5000 
# Perf_Strivings_c:IA_LABEL_PR_LvsH 
# 0.5015 

# Model comparison
lrtest(bm1_dwell_time_late, bm2.1_dwell_time_late)
#      #Df LogLik     Df  Chisq Pr(>Chisq)
# 1 14.503 7198.5                         
# 2 16.460 7200.2 1.9562 3.3099     0.1911

AIC(bm1_dwell_time_late, bm2.1_dwell_time_late)
#                             df       AIC
# bm1_dwell_time_late   14.50340 -14368.05
# bm2.1_dwell_time_late 16.45963 -14367.45

# Full Model: Perf_Concerns
bm2.2_dwell_time_late <- gamlss(
  dwell_time_late_t01 ~ 
    IA_LABEL_EV * Perf_Concerns_c +
    IA_LABEL_PR_SvsF * Perf_Concerns_c +
    IA_LABEL_PR_LvsH * Perf_Concerns_c +
    CESD_c + GAD7_c +   
    pb(subject_nr) +    
    pb(block_loop) +    
    pb(Change_category01, by = block_loop),    
  sigma.formula = ~ guessed_block_rule,   
  family = BE(mu.link = "logit"),    
  data = na.omit(sub_et_qu)
) 
summary(bm2.2_dwell_time_late)
# Family:  c("BE", "Beta") 
# 
# Call:  gamlss(formula = dwell_time_late_t01 ~ IA_LABEL_EV *  
#     Perf_Concerns_c + IA_LABEL_PR_SvsF * Perf_Concerns_c +  
#     IA_LABEL_PR_LvsH * Perf_Concerns_c + CESD_c + GAD7_c +  
#     pb(subject_nr) + pb(block_loop) + pb(Change_category01,  
#     by = block_loop), sigma.formula = ~guessed_block_rule,  
#     family = BE(mu.link = "logit"), data = na.omit(sub_et_qu)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  logit
# Mu Coefficients:
#                                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                            -1.477e+00  2.075e-02 -71.198  < 2e-16 ***
# IA_LABEL_EV                             7.563e-03  1.246e-02   0.607 0.543934    
# Perf_Concerns_c                        -1.104e-02  4.525e-03  -2.441 0.014673 *  
# IA_LABEL_PR_SvsF                       -1.635e-02  1.759e-02  -0.929 0.352663    
# IA_LABEL_PR_LvsH                        8.997e-03  5.457e-03   1.649 0.099216 .  
# CESD_c                                  3.355e-03  1.203e-03   2.788 0.005314 ** 
# GAD7_c                                 -5.618e-04  2.133e-03  -0.263 0.792293    
# pb(subject_nr)                         -8.268e-06  2.313e-06  -3.575 0.000352 ***
# pb(block_loop)                          1.971e-03  2.885e-03   0.683 0.494446    
# pb(Change_category01, by = block_loop)  6.095e-01  1.939e-02  31.431  < 2e-16 ***
# IA_LABEL_EV:Perf_Concerns_c             1.152e-03  4.259e-03   0.270 0.786818    
# Perf_Concerns_c:IA_LABEL_PR_SvsF        8.879e-04  6.644e-03   0.134 0.893687    
# Perf_Concerns_c:IA_LABEL_PR_LvsH        1.694e-03  2.364e-03   0.717 0.473453    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        -0.80647    0.02010  -40.11   <2e-16 ***
# guessed_block_rule  0.25894    0.02199   11.77   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms maybe are not accurate. 
# ------------------------------------------------------------------
# No. of observations in the fit:  10145 
# Degrees of Freedom for the fit:  18.44297
#       Residual Deg. of Freedom:  10126.56 
#                       at cycle:  8 
#  
# Global Deviance:     -14406.14 
#             AIC:     -14369.26 
#             SBC:     -14236.01 

# Back-transformed coefficients
print(round(inv.logit(bm2.2_dwell_time_late$mu.coefficients),4))
# (Intercept)                            IA_LABEL_EV 
# 0.1858                                 0.5019 
# Perf_Concerns_c                       IA_LABEL_PR_SvsF 
# 0.4972                                 0.4959 
# IA_LABEL_PR_LvsH                                 CESD_c 
# 0.5022                                 0.5008 
# GAD7_c                         pb(subject_nr) 
# 0.4999                                 0.5000 
# pb(block_loop) pb(Change_category01, by = block_loop) 
# 0.5005                                 0.6478 
# IA_LABEL_EV:Perf_Concerns_c       Perf_Concerns_c:IA_LABEL_PR_SvsF 
# 0.5003                                 0.5002 
# Perf_Concerns_c:IA_LABEL_PR_LvsH 
# 0.5004 

# Model comparison
lrtest(bm1_dwell_time_late, bm2.2_dwell_time_late)
#      #Df LogLik     Df Chisq Pr(>Chisq)  
# 1 14.503 7198.5                          
# 2 18.443 7203.1 3.9396 9.083    0.05906 .

AIC(bm1_dwell_time_late, bm2.2_dwell_time_late)
#                             df       AIC
# bm2.2_dwell_time_late 18.44297 -14369.26
# bm1_dwell_time_late   14.50340 -14368.05

# Whole Trial
#------------

# Null model
bm0_dwell_time <- gamlss(
  dwell_time ~ 
    CESD_c + GAD7_c +   
    pb(subject_nr) +    
    pb(block_loop) +    
    pb(Change_category01, by = block_loop),    
  sigma.formula = ~ guessed_block_rule,   
  family = BE(mu.link = "cauchit"),   
  data = na.omit(sub_et_qu)
)
summary(bm0_dwell_time)
# Family:  c("BE", "Beta") 
# 
# Call:  gamlss(formula = dwell_time ~ CESD_c + GAD7_c + pb(subject_nr) +  
#     pb(block_loop) + pb(Change_category01, by = block_loop),  
#     sigma.formula = ~guessed_block_rule, family = BE(mu.link = "cauchit"),  
#     data = na.omit(sub_et_qu)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  cauchit
# Mu Coefficients:
#                                          Estimate Std. Error  t value Pr(>|t|)    
# (Intercept)                            -1.645e+00  1.584e-02 -103.872   <2e-16 ***
# CESD_c                                 -6.536e-04  6.163e-04   -1.061    0.289    
# GAD7_c                                  1.105e-04  5.073e-04    0.218    0.828    
# pb(subject_nr)                          7.884e-07  1.910e-06    0.413    0.680    
# pb(block_loop)                         -3.208e-04  2.609e-03   -0.123    0.902    
# pb(Change_category01, by = block_loop)  9.946e-01  1.265e-02   78.613   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        -1.59172    0.01888 -84.311  < 2e-16 ***
# guessed_block_rule  0.16049    0.02100   7.642 2.34e-14 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms maybe are not accurate. 
# ------------------------------------------------------------------
# No. of observations in the fit:  10145 
# Degrees of Freedom for the fit:  9.127097
#       Residual Deg. of Freedom:  10135.87 
#                       at cycle:  6 
#  
# Global Deviance:     -24643.1 
#             AIC:     -24624.84 
#             SBC:     -24558.9 
            
# Back-transformed coefficients
print(round(inv.cauchit(bm0_dwell_time$mu.coefficients),4))
# (Intercept)                                 CESD_c 
# 0.1738                                 0.4998 
# GAD7_c                         pb(subject_nr) 
# 0.5000                                 0.5000 
# pb(block_loop) pb(Change_category01, by = block_loop) 
# 0.4999                                 0.7491  

# Partial Model
bm1_dwell_time <- gamlss(
  dwell_time ~ 
    IA_LABEL_EV + 
    IA_LABEL_PR_SvsF + 
    IA_LABEL_PR_LvsH + 
    CESD_c + GAD7_c +   
    pb(subject_nr) +    
    pb(block_loop) +    
    pb(Change_category01, by = block_loop),    
  sigma.formula = ~ guessed_block_rule,   
  family = BE(mu.link = "cauchit"),   
  data = na.omit(sub_et_qu)
)
summary(bm1_dwell_time)
# Family:  c("BE", "Beta") 
# 
# Call:  gamlss(formula = dwell_time ~ IA_LABEL_EV + IA_LABEL_PR_SvsF +  
#     IA_LABEL_PR_LvsH + CESD_c + GAD7_c + pb(subject_nr) + pb(block_loop) +  
#     pb(Change_category01, by = block_loop), sigma.formula = ~guessed_block_rule,  
#     family = BE(mu.link = "cauchit"), data = na.omit(sub_et_qu)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  cauchit
# Mu Coefficients:
#                                          Estimate Std. Error  t value Pr(>|t|)    
# (Intercept)                            -1.648e+00  1.608e-02 -102.453   <2e-16 ***
# IA_LABEL_EV                             1.233e-02  9.883e-03    1.248   0.2121    
# IA_LABEL_PR_SvsF                       -3.451e-02  1.385e-02   -2.491   0.0128 *  
# IA_LABEL_PR_LvsH                        4.495e-03  4.482e-03    1.003   0.3158    
# CESD_c                                 -6.658e-04  8.701e-04   -0.765   0.4442    
# GAD7_c                                  1.157e-04  1.501e-03    0.077   0.9386    
# pb(subject_nr)                          8.037e-07  1.738e-06    0.462   0.6439    
# pb(block_loop)                          6.954e-04  2.310e-03    0.301   0.7634    
# pb(Change_category01, by = block_loop)  9.954e-01  1.240e-02   80.248   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        -1.59200    0.01865 -85.372  < 2e-16 ***
# guessed_block_rule  0.16020    0.02069   7.743 1.06e-14 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms may not be reliable. 
# ------------------------------------------------------------------
# No. of observations in the fit:  10145 
# Degrees of Freedom for the fit:  12.12972
#       Residual Deg. of Freedom:  10132.87 
#                       at cycle:  6 
#  
# Global Deviance:     -24650.78 
#             AIC:     -24626.53 
#             SBC:     -24538.89 

# Back-transformed coefficients
print(round(inv.cauchit(bm1_dwell_time$mu.coefficients),4))
# (Intercept)                            IA_LABEL_EV 
# 0.1736                                 0.5039 
# IA_LABEL_PR_SvsF                       IA_LABEL_PR_LvsH 
# 0.4890                                 0.5014 
# CESD_c                                 GAD7_c 
# 0.4998                                 0.5000 
# pb(subject_nr)                         pb(block_loop) 
# 0.5000                                 0.5002 
# pb(Change_category01, by = block_loop) 
# 0.7493 

# Model comparison
lrtest(bm1_dwell_time, bm0_dwell_time)
#       #Df LogLik      Df  Chisq Pr(>Chisq)  
# 1 12.1297  12325                            
# 2  9.1271  12322 -3.0026 7.6856    0.05298 .

AIC(bm1_dwell_time, bm0_dwell_time)
#                       df       AIC
# bm1_dwell_time 12.129721 -24626.53
# bm0_dwell_time  9.127097 -24624.84

# Full Model: Perf_Strivings
bm2.1_dwell_time <- gamlss(
  dwell_time ~ 
    IA_LABEL_EV * Perf_Strivings_c + 
    IA_LABEL_PR_SvsF * Perf_Strivings_c + 
    IA_LABEL_PR_LvsH * Perf_Strivings_c +
    CESD_c + GAD7_c +   
    pb(subject_nr) +    
    pb(block_loop) +    
    pb(Change_category01, by = block_loop),    
  sigma.formula = ~ guessed_block_rule,   
  family = BE(mu.link = "cauchit"),   
  data = na.omit(sub_et_qu)
) 
summary(bm2.1_dwell_time)
# Family:  c("BE", "Beta") 
# 
# Call:  gamlss(formula = dwell_time ~ IA_LABEL_EV * Perf_Strivings_c +  
#     IA_LABEL_PR_SvsF * Perf_Strivings_c + IA_LABEL_PR_LvsH *  
#     Perf_Strivings_c + CESD_c + GAD7_c + pb(subject_nr) + pb(block_loop) +  
#     pb(Change_category01, by = block_loop), sigma.formula = ~guessed_block_rule,  
#     family = BE(mu.link = "cauchit"), data = na.omit(sub_et_qu)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  cauchit
# Mu Coefficients:
#                                          Estimate Std. Error  t value Pr(>|t|)    
# (Intercept)                            -1.647e+00  1.610e-02 -102.343   <2e-16 ***
# IA_LABEL_EV                             1.232e-02  9.882e-03    1.247   0.2124    
# Perf_Strivings_c                        3.097e-03  4.047e-03    0.765   0.4442    
# IA_LABEL_PR_SvsF                       -3.452e-02  1.385e-02   -2.492   0.0127 *  
# IA_LABEL_PR_LvsH                        4.508e-03  4.481e-03    1.006   0.3145    
# CESD_c                                 -8.003e-04  8.791e-04   -0.910   0.3626    
# GAD7_c                                  1.106e-05  1.510e-03    0.007   0.9942    
# pb(subject_nr)                          6.182e-07  1.752e-06    0.353   0.7242    
# pb(block_loop)                          6.572e-04  2.310e-03    0.285   0.7760    
# pb(Change_category01, by = block_loop)  9.960e-01  1.240e-02   80.306   <2e-16 ***
# IA_LABEL_EV:Perf_Strivings_c            4.944e-03  5.435e-03    0.910   0.3630    
# Perf_Strivings_c:IA_LABEL_PR_SvsF      -6.943e-03  7.646e-03   -0.908   0.3639    
# Perf_Strivings_c:IA_LABEL_PR_LvsH      -3.018e-04  2.324e-03   -0.130   0.8967    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        -1.59187    0.01865 -85.364  < 2e-16 ***
# guessed_block_rule  0.15985    0.02069   7.727 1.21e-14 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms may not be reliable. 
# ------------------------------------------------------------------
# No. of observations in the fit:  10145 
# Degrees of Freedom for the fit:  16.13948
#       Residual Deg. of Freedom:  10128.86 
#                       at cycle:  6 
#  
# Global Deviance:     -24652.69 
#             AIC:     -24620.41 
#             SBC:     -24503.81

# Back-transformed coefficients
print(round(inv.cauchit(bm2.1_dwell_time$mu.coefficients),4))
# (Intercept)                            IA_LABEL_EV 
# 0.1737                                 0.5039 
# Perf_Strivings_c                       IA_LABEL_PR_SvsF 
# 0.5010                                 0.4890 
# IA_LABEL_PR_LvsH                                 CESD_c 
# 0.5014                                 0.4997 
# GAD7_c                         pb(subject_nr) 
# 0.5000                                 0.5000 
# pb(block_loop) pb(Change_category01, by = block_loop) 
# 0.5002                                 0.7494 
# IA_LABEL_EV:Perf_Strivings_c      Perf_Strivings_c:IA_LABEL_PR_SvsF 
# 0.5016                                 0.4978 
# Perf_Strivings_c:IA_LABEL_PR_LvsH 
# 0.4999 

# Model comparison
lrtest(bm2.1_dwell_time, bm1_dwell_time)
#      #Df LogLik      Df  Chisq Pr(>Chisq)
# 1 16.139  12326                          
# 2 12.130  12325 -4.0098 1.9076     0.7527

AIC(bm2.1_dwell_time, bm1_dwell_time)
#                        df       AIC
# bm1_dwell_time   12.12972 -24626.53
# bm2.1_dwell_time 16.13948 -24620.41

# Full Model: Perf_Concerns
bm2.2_dwell_time <- gamlss(
  dwell_time ~ 
    IA_LABEL_EV * Perf_Concerns_c +
    IA_LABEL_PR_SvsF * Perf_Concerns_c + 
    IA_LABEL_PR_LvsH * Perf_Concerns_c + 
    CESD_c + GAD7_c +   
    pb(subject_nr) +    
    pb(block_loop) +    
    pb(Change_category01, by = block_loop),    
  sigma.formula = ~ guessed_block_rule,   
  family = BE(mu.link = "cauchit"),   
  data = na.omit(sub_et_qu)
) 
summary(bm2.2_dwell_time)
# Family:  c("BE", "Beta") 
# 
# Call:  gamlss(formula = dwell_time ~ IA_LABEL_EV * Perf_Concerns_c +  
#     IA_LABEL_PR_SvsF * Perf_Concerns_c + IA_LABEL_PR_LvsH * Perf_Concerns_c +  
#     CESD_c + GAD7_c + pb(subject_nr) + pb(block_loop) + pb(Change_category01,  
#     by = block_loop), sigma.formula = ~guessed_block_rule, family = BE(mu.link = "cauchit"),  
#     data = na.omit(sub_et_qu)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  cauchit
# Mu Coefficients:
#                                          Estimate Std. Error  t value Pr(>|t|)    
# (Intercept)                            -1.648e+00  1.612e-02 -102.247   <2e-16 ***
# IA_LABEL_EV                             1.238e-02  9.881e-03    1.253   0.2103    
# Perf_Concerns_c                         2.305e-03  3.458e-03    0.667   0.5051    
# IA_LABEL_PR_SvsF                       -3.456e-02  1.385e-02   -2.495   0.0126 *  
# IA_LABEL_PR_LvsH                        4.506e-03  4.481e-03    1.006   0.3146    
# CESD_c                                 -9.245e-04  9.161e-04   -1.009   0.3129    
# GAD7_c                                 -4.028e-05  1.520e-03   -0.027   0.9789    
# pb(subject_nr)                          5.595e-07  1.773e-06    0.316   0.7524    
# pb(block_loop)                          7.705e-04  2.309e-03    0.334   0.7387    
# pb(Change_category01, by = block_loop)  9.965e-01  1.240e-02   80.356   <2e-16 ***
# IA_LABEL_EV:Perf_Concerns_c             8.047e-03  4.164e-03    1.932   0.0533 .  
# Perf_Concerns_c:IA_LABEL_PR_SvsF       -3.785e-03  5.860e-03   -0.646   0.5183    
# Perf_Concerns_c:IA_LABEL_PR_LvsH       -3.799e-04  1.781e-03   -0.213   0.8311    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        -1.59170    0.01865 -85.354  < 2e-16 ***
# guessed_block_rule  0.15935    0.02069   7.702 1.46e-14 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms may not be reliable. 
# ------------------------------------------------------------------
# No. of observations in the fit:  10145 
# Degrees of Freedom for the fit:  16.12628
#       Residual Deg. of Freedom:  10128.87 
#                       at cycle:  6 
#  
# Global Deviance:     -24656.23 
#             AIC:     -24623.98 
#             SBC:     -24507.47 

# Back-transformed coefficients
print(round(inv.cauchit(bm2.2_dwell_time$mu.coefficients),4))
# (Intercept)                            IA_LABEL_EV 
# 0.1736                                 0.5039 
# Perf_Concerns_c                       IA_LABEL_PR_SvsF 
# 0.5007                                 0.4890 
# IA_LABEL_PR_LvsH                                 CESD_c 
# 0.5014                                 0.4997 
# GAD7_c                         pb(subject_nr) 
# 0.5000                                 0.5000 
# pb(block_loop) pb(Change_category01, by = block_loop) 
# 0.5002                                 0.7494 
# IA_LABEL_EV:Perf_Concerns_c       Perf_Concerns_c:IA_LABEL_PR_SvsF 
# 0.5026                                 0.4988 
# Perf_Concerns_c:IA_LABEL_PR_LvsH 
# 0.4999 

# Model comparison
lrtest(bm2.2_dwell_time, bm1_dwell_time)
#      #Df LogLik      Df  Chisq Pr(>Chisq)
# 1 16.126  12328                          
# 2 12.130  12325 -3.9966 5.4465     0.2445

AIC(bm2.2_dwell_time, bm1_dwell_time)
#                        df       AIC
# bm1_dwell_time   12.12972 -24626.53
# bm2.2_dwell_time 16.12628 -24623.98

# Fit a Gamma glmm for response time
#----------------------------------------------

# Create a subset of the data for model fitting
sub_rt_qu <- rt_qu[c("response_time",
                     "subject_nr", "block_loop", "guessed_block_rule",
                     "Change_category_PR_SvsF", "Change_category_PR_SvsF", 
                     "Change_category_PR_LvsH", "Change_category_EV", 
                     "Perf_Strivings_c" , "Perf_Concerns_c", 
                     "CESD", "GAD7")]

sub_rt_qu$CESD_c <- sub_rt_qu$CESD - mean(sub_rt_qu$CESD)
sub_rt_qu$GAD7_c <- sub_rt_qu$GAD7 - mean(sub_rt_qu$GAD7)

# Null model
gm0_response_time <- gamlss(
  response_time ~ 
    CESD_c + GAD7_c +   
    pb(subject_nr) +    
    pb(block_loop) ,    
  family = GA(mu.link = "log"),   
  sigma.formula = ~guessed_block_rule,   
  data = na.omit(sub_rt_qu)
)
summary(gm0_response_time)
# Family:  c("GA", "Gamma") 
# 
# Call:  gamlss(formula = response_time ~ CESD_c + GAD7_c +  
#     pb(subject_nr) + pb(block_loop), sigma.formula = ~guessed_block_rule,  
#     family = GA(mu.link = "log"), data = na.omit(sub_rt_qu)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  log
# Mu Coefficients:
#                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     7.423e+00  1.047e-02 708.696  < 2e-16 ***
# CESD_c          1.671e-03  5.770e-04   2.897  0.00378 ** 
# GAD7_c          2.504e-03  1.014e-03   2.469  0.01357 *  
# pb(subject_nr) -2.316e-06  1.249e-06  -1.854  0.06380 .  
# pb(block_loop)  3.590e-03  1.480e-03   2.425  0.01531 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  log
# Sigma Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        -0.64263    0.01292 -49.735   <2e-16 ***
# guessed_block_rule  0.03083    0.01430   2.155   0.0312 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms maybe are not accurate. 
# ------------------------------------------------------------------
# No. of observations in the fit:  14977 
# Degrees of Freedom for the fit:  18.96904
#       Residual Deg. of Freedom:  14958.03 
#                       at cycle:  3 
#  
# Global Deviance:     243575.6 
#             AIC:     243613.5 
#             SBC:     243758 

# Exponantiated coefficients
print(round(exp(gm0_response_time$mu.coefficients),4))
# (Intercept)         CESD_c         GAD7_c pb(subject_nr) pb(block_loop) 
# 1674.5224         1.0017         1.0025         1.0000         1.0036 

# Partial Model
gm1_response_time <- gamlss(
  response_time ~ 
    Change_category_EV + 
    Change_category_PR_SvsF + 
    Change_category_PR_LvsH + 
    CESD_c + GAD7_c +   
    pb(subject_nr) +    
    pb(block_loop) ,    
  family = GA(mu.link = "log"),   
  sigma.formula = ~guessed_block_rule,   
  data = na.omit(sub_rt_qu)
)
summary(gm1_response_time)
# Family:  c("GA", "Gamma") 
# 
# Call:  gamlss(formula = response_time ~ Change_category_EV +  
#     Change_category_PR_SvsF + Change_category_PR_LvsH +      CESD_c + GAD7_c + pb(subject_nr) + pb(block_loop),  
#     sigma.formula = ~guessed_block_rule, family = GA(mu.link = "log"),      data = na.omit(sub_rt_qu)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  log
# Mu Coefficients:
#                           Estimate Std. Error t value Pr(>|t|)    
# (Intercept)              7.389e+00  1.622e-02 455.541  < 2e-16 ***
# Change_category_EV      -1.638e-02  6.787e-03  -2.413 0.015836 *  
# Change_category_PR_SvsF -1.509e-02  9.124e-03  -1.654 0.098077 .  
# Change_category_PR_LvsH  1.362e-02  6.547e-03   2.081 0.037443 *  
# CESD_c                   1.689e-03  6.094e-04   2.772 0.005585 ** 
# GAD7_c                   2.480e-03  1.052e-03   2.357 0.018431 *  
# pb(subject_nr)          -2.340e-06  1.182e-06  -1.979 0.047782 *  
# pb(block_loop)           1.182e-02  3.502e-03   3.375 0.000741 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  log
# Sigma Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        -0.64088    0.01307 -49.036   <2e-16 ***
# guessed_block_rule  0.02929    0.01449   2.021   0.0433 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms maybe are not accurate. 
# ------------------------------------------------------------------
# No. of observations in the fit:  14977 
# Degrees of Freedom for the fit:  17.45607
#       Residual Deg. of Freedom:  14959.54 
#                       at cycle:  3 
#  
# Global Deviance:     243592 
#             AIC:     243626.9 
#             SBC:     243759.8 

# Exponantiated coefficientss
print(round(exp(gm1_response_time$mu.coefficients),4))
# (Intercept)      Change_category_EV Change_category_PR_SvsF Change_category_PR_LvsH 
# 1618.2298                  0.9838                  0.9850                  1.0137 
# CESD_c                  GAD7_c          pb(subject_nr)          pb(block_loop) 
# 1.0017                  1.0025                  1.0000                  1.0119 

# Model comparison
lrtest(gm1_response_time, gm0_response_time)
#      #Df  LogLik    Df  Chisq Pr(>Chisq)    
# 1 17.456 -121796                            
# 2 18.969 -121788 1.513 16.391  0.0002759 ***

AIC(gm1_response_time, gm0_response_time)
#                         df      AIC
# gm0_response_time 18.96904 243613.5
# gm1_response_time 17.45607 243626.9

# Full Model: Perf_Strivings
gm2.1_response_time <- gamlss(
  response_time ~ 
    Change_category_EV*Perf_Strivings_c + 
    Change_category_PR_SvsF*Perf_Strivings_c + 
    Change_category_PR_LvsH*Perf_Strivings_c + 
    CESD_c + GAD7_c +   
    pb(subject_nr) +    
    pb(block_loop) ,    
  family = GA(mu.link = "log"),   
  sigma.formula = ~guessed_block_rule,   
  data = na.omit(sub_rt_qu)
) 
summary(gm2.1_response_time)
# Family:  c("GA", "Gamma") 
# 
# Call:  gamlss(formula = response_time ~ Change_category_EV *  
#     Perf_Strivings_c + Change_category_PR_SvsF * Perf_Strivings_c +  
#     Change_category_PR_LvsH * Perf_Strivings_c + CESD_c +  
#     GAD7_c + pb(subject_nr) + pb(block_loop), sigma.formula = ~guessed_block_rule,  
#     family = GA(mu.link = "log"), data = na.omit(sub_rt_qu)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  log
# Mu Coefficients:
#                                            Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                               7.387e+00  1.746e-02 423.145  < 2e-16 ***
# Change_category_EV                       -1.638e-02  8.416e-03  -1.946  0.05162 .  
# Perf_Strivings_c                         -1.114e-02  2.861e-03  -3.893 9.96e-05 ***
# Change_category_PR_SvsF                  -1.480e-02  1.073e-02  -1.380  0.16755    
# Change_category_PR_LvsH                   1.347e-02  7.342e-03   1.835  0.06647 .  
# CESD_c                                    1.914e-03  6.234e-04   3.071  0.00214 ** 
# GAD7_c                                    3.303e-03  1.112e-03   2.970  0.00298 ** 
# pb(subject_nr)                           -1.495e-06  1.614e-06  -0.926  0.35440    
# pb(block_loop)                            1.170e-02  3.906e-03   2.996  0.00274 ** 
# Change_category_EV:Perf_Strivings_c      -4.711e-03  4.210e-03  -1.119  0.26310    
# Perf_Strivings_c:Change_category_PR_SvsF  1.276e-02  5.703e-03   2.237  0.02530 *  
# Perf_Strivings_c:Change_category_PR_LvsH  7.176e-04  1.556e-03   0.461  0.64461    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  log
# Sigma Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        -0.64257    0.01320 -48.686   <2e-16 ***
# guessed_block_rule  0.03035    0.01466   2.071   0.0384 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms maybe are not accurate. 
# ------------------------------------------------------------------
# No. of observations in the fit:  14977 
# Degrees of Freedom for the fit:  21.57641
#       Residual Deg. of Freedom:  14955.42 
#                       at cycle:  3 
#  
# Global Deviance:     243564.9 
#             AIC:     243608 
#             SBC:     243772.3 

# Exponantiated coefficients
print(round(exp(gm2.1_response_time$mu.coefficients),4))
# (Intercept)                       Change_category_EV 
# 1614.1789                                   0.9838 
# Perf_Strivings_c                  Change_category_PR_SvsF 
# 0.9889                                   0.9853 
# Change_category_PR_LvsH                                   CESD_c 
# 1.0136                                   1.0019 
# GAD7_c                           pb(subject_nr) 
# 1.0033                                   1.0000 
# pb(block_loop)      Change_category_EV:Perf_Strivings_c 
# 1.0118                                   0.9953 
# Perf_Strivings_c:Change_category_PR_SvsF Perf_Strivings_c:Change_category_PR_LvsH 
# 1.0128                                   1.0007

# Model comparison
lrtest(gm2.1_response_time, gm1_response_time)
#      #Df  LogLik      Df  Chisq Pr(>Chisq)    
# 1 21.576 -121782                              
# 2 17.456 -121796 -4.1203 27.138  1.864e-05 ***

AIC(gm2.1_response_time, gm1_response_time)
#                           df      AIC
# gm2.1_response_time 21.57641 243608.0
# gm1_response_time   17.45607 243626.9

# Full Model: Perf_Concerns
gm2.2_response_time <- gamlss(
  response_time ~ 
    Change_category_EV*Perf_Concerns_c +
    Change_category_PR_SvsF*Perf_Concerns_c +
    Change_category_PR_LvsH*Perf_Concerns_c +
    CESD_c + GAD7_c +   
    pb(subject_nr) +    
    pb(block_loop) ,    
  family = GA(mu.link = "log"),   
  sigma.formula = ~guessed_block_rule,   
  data = na.omit(sub_rt_qu)
)
summary(gm2.2_response_time)
# Family:  c("GA", "Gamma") 
# 
# Call:  gamlss(formula = response_time ~ Change_category_EV * Perf_Concerns_c +  
#     Change_category_PR_SvsF * Perf_Concerns_c + Change_category_PR_LvsH *  
#     Perf_Concerns_c + CESD_c + GAD7_c + pb(subject_nr) + pb(block_loop),  
#     sigma.formula = ~guessed_block_rule, family = GA(mu.link = "log"),      data = na.omit(sub_rt_qu)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  log
# Mu Coefficients:
#                                           Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                              7.391e+00  1.648e-02 448.420  < 2e-16 ***
# Change_category_EV                      -1.624e-02  7.806e-03  -2.081  0.03748 *  
# Perf_Concerns_c                          6.433e-03  2.479e-03   2.595  0.00946 ** 
# Change_category_PR_SvsF                 -1.517e-02  9.853e-03  -1.540  0.12361    
# Change_category_PR_LvsH                  1.361e-02  6.764e-03   2.012  0.04427 *  
# CESD_c                                   1.084e-03  6.483e-04   1.673  0.09438 .  
# GAD7_c                                   2.013e-03  1.070e-03   1.881  0.06003 .  
# pb(subject_nr)                          -3.012e-06  1.259e-06  -2.393  0.01672 *  
# pb(block_loop)                           1.179e-02  3.630e-03   3.247  0.00117 ** 
# Change_category_EV:Perf_Concerns_c      -3.033e-03  2.966e-03  -1.022  0.30658    
# Perf_Concerns_c:Change_category_PR_SvsF  2.213e-03  4.189e-03   0.528  0.59722    
# Perf_Concerns_c:Change_category_PR_LvsH -2.604e-04  1.275e-03  -0.204  0.83812    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  log
# Sigma Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        -0.64238    0.01293 -49.668   <2e-16 ***
# guessed_block_rule  0.03079    0.01430   2.153   0.0313 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms may not be reliable. 
# ------------------------------------------------------------------
# No. of observations in the fit:  14977 
# Degrees of Freedom for the fit:  21.43204
#       Residual Deg. of Freedom:  14955.57 
#                       at cycle:  3 
#  
# Global Deviance:     243583 
#             AIC:     243625.8 
#             SBC:     243789 

# Exponantiated coefficients
print(round(exp(gm2.2_response_time$mu.coefficients),4))
# (Intercept)                      Change_category_EV 
# 1622.0331                                  0.9839 
# Perf_Concerns_c                 Change_category_PR_SvsF 
# 1.0065                                  0.9849 
# Change_category_PR_LvsH                                  CESD_c 
# 1.0137                                  1.0011 
# GAD7_c                          pb(subject_nr) 
# 1.0020                                  1.0000 
# pb(block_loop)      Change_category_EV:Perf_Concerns_c 
# 1.0119                                  0.9970 
# Perf_Concerns_c:Change_category_PR_SvsF Perf_Concerns_c:Change_category_PR_LvsH 
# 1.0022                                  0.9997 

# Model comparison
lrtest(gm2.2_response_time, gm1_response_time)
#      #Df  LogLik     Df  Chisq Pr(>Chisq)  
# 1 21.432 -121791                           
# 2 17.456 -121796 -3.976 9.0275    0.06041 .

AIC(gm2.2_response_time, gm1_response_time)
#                           df      AIC
# gm2.2_response_time 21.43204 243625.8
# gm1_response_time   17.45607 243626.9
