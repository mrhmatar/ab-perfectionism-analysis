# AB Perfectionism - Study 1 

#-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-#
# Supplementary Analysis: Semantic Category as Factor #
#-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-#

source("AB_Perfectionism_Study1_Preparation.R")

# Path a: Perfectionism → Attention Bias 
#---------------------------------------

# Create a subset of the data for model fitting
sub_et_qu <- et_qu[c("first_fix_prob", "first_fix_latency", "dwell_time",
                     "dwell_time_early", "dwell_time_intermediate", "dwell_time_late",
                     "subject_nr", "block_loop", "guessed_block_rule","Change_category01", 
                     "IA_LABEL","IA_LABEL_PR_SvsF", "IA_LABEL_PR_LvsH","IA_LABEL_EV", 
                     "Perf_Strivings_c" , "Perf_Concerns_c")]

# Set neutral as the reference
sub_et_qu$IA_LABEL <- relevel(as.factor(sub_et_qu$IA_LABEL), ref = "Neutral")

# Transform 0 and 1 observations
transform01 <- function(x) {
  (x * (length(x) - 1) + 0.5) / (length(x))
}

sub_et_qu$first_fix_prob_t01 <- transform01(sub_et_qu$first_fix_prob)
sub_et_qu$dwell_time_early_t01 <- transform01(sub_et_qu$dwell_time_early)
sub_et_qu$dwell_time_intermediate_t01 <- transform01(sub_et_qu$dwell_time_intermediate)
sub_et_qu$dwell_time_late_t01 <- transform01(sub_et_qu$dwell_time_late)

# Define a function to back-transform from cauchit
inv.cauchit <- function(eta) {
  (1 / pi) * atan(eta) + 0.5
}

# Fit a Beta glmm for first fixation probability 
#-----------------------------------------------

# Null model
bm0_first_fix_prob <- gamlss(
  first_fix_prob_t01 ~ 
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
# Call:  gamlss(formula = first_fix_prob_t01 ~ pb(subject_nr) +  
#     pb(block_loop) + pb(Change_category01, by = block_loop),  
#     sigma.formula = ~guessed_block_rule, family = BE(mu.link = "logit"),  
#     data = na.omit(sub_et_qu)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  logit
# Mu Coefficients:
#                                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                            -3.445e-01  1.938e-02 -17.772  < 2e-16 ***
# pb(subject_nr)                          2.868e-06  5.731e-06   0.500    0.617    
# pb(block_loop)                          4.018e-03  3.026e-03   1.328    0.184    
# pb(Change_category01, by = block_loop) -1.721e-01  2.185e-02  -7.879 3.64e-15 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                     Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        -0.238738   0.021532 -11.088   <2e-16 ***
# guessed_block_rule  0.001833   0.023516   0.078    0.938    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms maybe are not accurate. 
# ------------------------------------------------------------------
# No. of observations in the fit:  10145 
# Degrees of Freedom for the fit:  7.161835
#       Residual Deg. of Freedom:  10137.84 
#                       at cycle:  4 
#  
# Global Deviance:     -3369.865 
#             AIC:     -3355.541 
#             SBC:     -3303.799 

# Back-transformed coefficients
print(round(inv.logit(bm0_first_fix_prob$mu.coefficients),4))
# (Intercept)                         pb(subject_nr) 
# 0.4147                                 0.5000 
# pb(block_loop) pb(Change_category01, by = block_loop) 
# 0.5010                                 0.4571

 # Partial Model
bm1_first_fix_prob <- gamlss(
  first_fix_prob_t01 ~ 
    IA_LABEL+ 
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
# Call:  gamlss(formula = first_fix_prob_t01 ~ IA_LABEL + pb(subject_nr) +  
#     pb(block_loop) + pb(Change_category01, by = block_loop),  
#     sigma.formula = ~guessed_block_rule, family = BE(mu.link = "logit"),  
#     data = na.omit(sub_et_qu)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  logit
# Mu Coefficients:
#                                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                            -3.422e-01  2.595e-02 -13.188  < 2e-16 ***
# IA_LABELNegative                       -8.391e-02  2.749e-02  -3.052  0.00228 ** 
# IA_LABELFailure                        -2.381e-02  2.748e-02  -0.866  0.38629    
# IA_LABELPositive                        9.158e-02  2.742e-02   3.340  0.00084 ***
# IA_LABELSuccess                         2.372e-03  2.746e-02   0.086  0.93116    
# pb(subject_nr)                          2.863e-06  5.731e-06   0.500  0.61740    
# pb(block_loop)                          4.068e-03  3.023e-03   1.346  0.17845    
# pb(Change_category01, by = block_loop) -1.726e-01  2.182e-02  -7.909 2.87e-15 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                     Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        -0.246126   0.021558 -11.417   <2e-16 ***
# guessed_block_rule  0.007138   0.023539   0.303    0.762    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms maybe are not accurate. 
# ------------------------------------------------------------------
# No. of observations in the fit:  10145 
# Degrees of Freedom for the fit:  11.1536
#       Residual Deg. of Freedom:  10133.85 
#                       at cycle:  3 
#  
# Global Deviance:     -3412.103 
#             AIC:     -3389.795 
#             SBC:     -3309.214 

# Back-transformed coefficients
print(round(inv.logit(bm1_first_fix_prob$mu.coefficients),4))
# (Intercept)                       IA_LABELNegative 
# 0.4153                                 0.4790 
# IA_LABELFailure                       IA_LABELPositive 
# 0.4940                                 0.5229 
# IA_LABELSuccess                         pb(subject_nr) 
# 0.5006                                 0.5000 
# pb(block_loop) pb(Change_category01, by = block_loop) 
# 0.5010                                 0.4570 

# Model comparison
lrtest(bm0_first_fix_prob, bm1_first_fix_prob)
#       #Df LogLik     Df  Chisq Pr(>Chisq)    
# 1  7.1618 1684.9                             
# 2 11.1536 1706.0 3.9918 42.238  1.489e-08 ***

AIC(bm1_first_fix_prob, bm0_first_fix_prob)
#                           df       AIC
# bm1_first_fix_prob 11.153599 -3389.795
# bm0_first_fix_prob  7.161835 -3355.541

# Full Model: Perf_Strivings
bm2.1_first_fix_prob <- gamlss(
  first_fix_prob_t01 ~ 
    IA_LABEL * Perf_Strivings_c + 
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
# Call:  gamlss(formula = first_fix_prob_t01 ~ IA_LABEL * Perf_Strivings_c +  
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
# (Intercept)                            -3.439e-01  2.593e-02 -13.264  < 2e-16 ***
# IA_LABELNegative                       -8.364e-02  2.748e-02  -3.044 0.002340 ** 
# IA_LABELFailure                        -2.382e-02  2.746e-02  -0.867 0.385700    
# IA_LABELPositive                        9.161e-02  2.740e-02   3.343 0.000831 ***
# IA_LABELSuccess                         2.361e-03  2.744e-02   0.086 0.931434    
# Perf_Strivings_c                       -2.452e-02  1.079e-02  -2.273 0.023059 *  
# pb(subject_nr)                          3.340e-06  5.731e-06   0.583 0.560084    
# pb(block_loop)                          4.066e-03  3.022e-03   1.346 0.178493    
# pb(Change_category01, by = block_loop) -1.724e-01  2.181e-02  -7.908 2.89e-15 ***
# IA_LABELNegative:Perf_Strivings_c      -7.866e-03  1.525e-02  -0.516 0.606033    
# IA_LABELFailure:Perf_Strivings_c        4.423e-03  1.524e-02   0.290 0.771624    
# IA_LABELPositive:Perf_Strivings_c       2.685e-02  1.521e-02   1.765 0.077529 .  
# IA_LABELSuccess:Perf_Strivings_c        3.100e-02  1.522e-02   2.037 0.041691 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                     Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        -0.247744   0.021559  -11.49   <2e-16 ***
# guessed_block_rule  0.007534   0.023538    0.32    0.749    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms maybe are not accurate. 
# ------------------------------------------------------------------
# No. of observations in the fit:  10145 
# Degrees of Freedom for the fit:  16.15243
#       Residual Deg. of Freedom:  10128.85 
#                       at cycle:  4 
#  
# Global Deviance:     -3430.135 
#             AIC:     -3397.83 
#             SBC:     -3281.133 

# Back-transformed coefficients
print(round(inv.logit(bm2.1_first_fix_prob$mu.coefficients),4))
# (Intercept)                       IA_LABELNegative 
# 0.4149                                 0.4791 
# IA_LABELFailure                       IA_LABELPositive 
# 0.4940                                 0.5229 
# IA_LABELSuccess                       Perf_Strivings_c 
# 0.5006                                 0.4939 
# pb(subject_nr)                         pb(block_loop) 
# 0.5000                                 0.5010 
# pb(Change_category01, by = block_loop)      IA_LABELNegative:Perf_Strivings_c 
# 0.4570                                 0.4980 
# IA_LABELFailure:Perf_Strivings_c      IA_LABELPositive:Perf_Strivings_c 
# 0.5011                                 0.5067 
# IA_LABELSuccess:Perf_Strivings_c 
# 0.5077 

# Model comparison
lrtest(bm1_first_fix_prob, bm2.1_first_fix_prob)
#      #Df LogLik     Df  Chisq Pr(>Chisq)   
# 1 11.154 1706.0                            
# 2 16.152 1715.1 4.9988 18.032   0.002906 **

AIC(bm2.1_first_fix_prob, bm1_first_fix_prob)
#                            df       AIC
# bm2.1_first_fix_prob 16.15243 -3397.830
# bm1_first_fix_prob   11.15360 -3389.795

# Full Model: Perf_Concerns
bm2.2_first_fix_prob <- gamlss(
  first_fix_prob_t01 ~ 
    IA_LABEL * Perf_Concerns_c +
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
# Call:  gamlss(formula = first_fix_prob_t01 ~ IA_LABEL * Perf_Concerns_c +  
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
# (Intercept)                            -3.438e-01  2.594e-02 -13.256  < 2e-16 ***
# IA_LABELNegative                       -8.388e-02  2.748e-02  -3.052 0.002275 ** 
# IA_LABELFailure                        -2.379e-02  2.747e-02  -0.866 0.386400    
# IA_LABELPositive                        9.178e-02  2.741e-02   3.349 0.000814 ***
# IA_LABELSuccess                         2.456e-03  2.745e-02   0.089 0.928711    
# Perf_Concerns_c                        -1.268e-02  8.255e-03  -1.536 0.124469    
# pb(subject_nr)                          3.267e-06  5.732e-06   0.570 0.568726    
# pb(block_loop)                          4.095e-03  3.022e-03   1.355 0.175493    
# pb(Change_category01, by = block_loop) -1.725e-01  2.181e-02  -7.910 2.84e-15 ***
# IA_LABELNegative:Perf_Concerns_c        7.252e-03  1.167e-02   0.622 0.534146    
# IA_LABELFailure:Perf_Concerns_c        -4.289e-03  1.168e-02  -0.367 0.713452    
# IA_LABELPositive:Perf_Concerns_c        7.391e-04  1.164e-02   0.063 0.949383    
# IA_LABELSuccess:Perf_Concerns_c         2.623e-02  1.169e-02   2.243 0.024916 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        -0.24942    0.02158  -11.56   <2e-16 ***
# guessed_block_rule  0.01012    0.02356    0.43    0.667    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms maybe are not accurate. 
# ------------------------------------------------------------------
# No. of observations in the fit:  10145 
# Degrees of Freedom for the fit:  16.15199
#       Residual Deg. of Freedom:  10128.85 
#                       at cycle:  4 
#  
# Global Deviance:     -3423.849 
#             AIC:     -3391.545 
#             SBC:     -3274.852 

# Back-transformed coefficients
print(round(inv.logit(bm2.2_first_fix_prob$mu.coefficients),4))
# (Intercept)                       IA_LABELNegative 
# 0.4149                                 0.4790 
# IA_LABELFailure                       IA_LABELPositive 
# 0.4941                                 0.5229 
# IA_LABELSuccess                        Perf_Concerns_c 
# 0.5006                                 0.4968 
# pb(subject_nr)                         pb(block_loop) 
# 0.5000                                 0.5010 
# pb(Change_category01, by = block_loop)       IA_LABELNegative:Perf_Concerns_c 
# 0.4570                                 0.5018 
# IA_LABELFailure:Perf_Concerns_c       IA_LABELPositive:Perf_Concerns_c 
# 0.4989                                 0.5002 
# IA_LABELSuccess:Perf_Concerns_c 
# 0.5066

# Model comparison
lrtest(bm1_first_fix_prob, bm2.2_first_fix_prob)
#      #Df LogLik     Df  Chisq Pr(>Chisq)  
# 1 11.154 1706.0                           
# 2 16.152 1711.9 4.9984 11.747    0.03843 *

AIC(bm2.2_first_fix_prob, bm1_first_fix_prob)
#                            df       AIC
# bm2.2_first_fix_prob 16.15199 -3391.545
# bm1_first_fix_prob   11.15360 -3389.795

# Simulate predicted values
preds_first_fix_prob_PS <- ggpredict(bm2.1_first_fix_prob, terms = c("Perf_Strivings_c", "IA_LABEL"))
preds_first_fix_prob_PC <- ggpredict(bm2.2_first_fix_prob, terms = c("Perf_Concerns_c", "IA_LABEL"))

# Plot the retained interaction 
dti_PS <- ggplot(preds_first_fix_prob_PS, aes(x = x, y = predicted, color = group)) +
  geom_line(size = 1) +                                # Line only, no CI ribbon
  labs(
    x = "Perfectionistic Strivings",
    y = "",
    color = "Semantic Category"
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

dti_PC <- ggplot(preds_first_fix_prob_PC, aes(x = x, y = predicted, color = group)) +
  geom_line(size = 1) +                                # Line only, no CI ribbon
  labs(
    x = "Perfectionistic Concerns",
    y = "",
    color = "Semantic Category"
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
             top = textGrob("Predicted First Fixation Probability",
                            gp = gpar(fontsize = 16, fontface = "bold")
                            )
             )


# Fit a Beta glmm for first fixation latency
#-------------------------------------------

# Null model
bm0_first_fix_latency <- gamlss(
  first_fix_latency ~ 
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
# Call:  gamlss(formula = first_fix_latency ~ pb(subject_nr) + pb(block_loop) +  
#     pb(Change_category01, by = block_loop), sigma.formula = ~guessed_block_rule,  
#     family = BE(mu.link = "logit"), data = na.omit(sub_et_qu)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  logit
# Mu Coefficients:
#                                          Estimate Std. Error  t value Pr(>|t|)    
# (Intercept)                            -1.392e+00  3.589e-03 -387.745  < 2e-16 ***
# pb(subject_nr)                         -9.233e-08  4.035e-07   -0.229    0.819    
# pb(block_loop)                          2.135e-04  5.230e-04    0.408    0.683    
# pb(Change_category01, by = block_loop)  2.556e-02  3.739e-03    6.836 8.64e-12 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                    Estimate Std. Error  t value Pr(>|t|)    
# (Intercept)        -2.72029    0.01733 -156.994   <2e-16 ***
# guessed_block_rule -0.02384    0.01918   -1.243    0.214    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms may not be reliable. 
# ------------------------------------------------------------------
# No. of observations in the fit:  10145 
# Degrees of Freedom for the fit:  6.503363
#       Residual Deg. of Freedom:  10138.5 
#                       at cycle:  5 
#  
# Global Deviance:     -46713.92 
#             AIC:     -46700.91 
#             SBC:     -46653.93 

# Back-transformed coefficients
print(round(inv.logit(bm0_first_fix_latency$mu.coefficients),4))
# (Intercept)                         pb(subject_nr) 
# 0.1991                                 0.5000 
# pb(block_loop) pb(Change_category01, by = block_loop) 
# 0.5001                                 0.5064 

 # Partial Model
bm1_first_fix_latency <- gamlss(
  first_fix_latency ~ 
    IA_LABEL + 
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
# Call:  gamlss(formula = first_fix_latency ~ IA_LABEL + pb(subject_nr) +  
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
# (Intercept)                            -1.394e+00  1.189e-02 -117.208  < 2e-16 ***
# IA_LABELNegative                        1.187e-02  9.622e-03    1.234    0.217    
# IA_LABELFailure                         9.611e-03  1.018e-02    0.944    0.345    
# IA_LABELPositive                       -9.909e-03  1.022e-02   -0.969    0.332    
# IA_LABELSuccess                        -1.317e-03  1.051e-02   -0.125    0.900    
# pb(subject_nr)                         -9.342e-08  8.119e-07   -0.115    0.908    
# pb(block_loop)                          2.212e-04  6.858e-04    0.323    0.747    
# pb(Change_category01, by = block_loop)  2.554e-02  3.741e-03    6.827 9.18e-12 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                    Estimate Std. Error  t value Pr(>|t|)    
# (Intercept)        -2.72403    0.01767 -154.204   <2e-16 ***
# guessed_block_rule -0.02103    0.01963   -1.072    0.284    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms maybe are not accurate. 
# ------------------------------------------------------------------
# No. of observations in the fit:  10145 
# Degrees of Freedom for the fit:  10.50445
#       Residual Deg. of Freedom:  10134.5 
#                       at cycle:  5 
#  
# Global Deviance:     -46741.6 
#             AIC:     -46720.59 
#             SBC:     -46644.7 

# Back-transformed coefficients
print(round(inv.logit(bm1_first_fix_latency$mu.coefficients),4))
# (Intercept)                       IA_LABELNegative 
# 0.1988                                 0.5030 
# IA_LABELFailure                       IA_LABELPositive 
# 0.5024                                 0.4975 
# IA_LABELSuccess                         pb(subject_nr) 
# 0.4997                                 0.5000 
# pb(block_loop) pb(Change_category01, by = block_loop) 
# 0.5001                                 0.5064                        

# Model comparison
lrtest(bm0_first_fix_latency, bm1_first_fix_latency)
#       #Df LogLik     Df  Chisq Pr(>Chisq)    
# 1  6.5034  23357                             
# 2 10.5044  23371 4.0011 27.677   1.45e-05 ***

AIC(bm1_first_fix_latency, bm0_first_fix_latency)
#                              df       AIC
# bm1_first_fix_latency 10.504445 -46720.59
# bm0_first_fix_latency  6.503363 -46700.91

# Full Model: Perf_Strivings
bm2.1_first_fix_latency <- gamlss(
  first_fix_latency ~ 
    IA_LABEL * Perf_Strivings_c + 
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
# Call:  gamlss(formula = first_fix_latency ~ IA_LABEL * Perf_Strivings_c +  
#     pb(subject_nr) + pb(block_loop) + pb(Change_category01, by = block_loop),  
#     sigma.formula = ~guessed_block_rule, family = BE(mu.link = "logit"),  
#     data = na.omit(sub_et_qu)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  logit
# Mu Coefficients:
#                                          Estimate Std. Error  t value Pr(>|t|)    
# (Intercept)                            -1.394e+00  4.681e-03 -297.757  < 2e-16 ***
# IA_LABELNegative                        1.187e-02  4.739e-03    2.505   0.0122 *  
# IA_LABELFailure                         9.614e-03  4.741e-03    2.028   0.0426 *  
# IA_LABELPositive                       -9.907e-03  4.754e-03   -2.084   0.0372 *  
# IA_LABELSuccess                        -1.315e-03  4.748e-03   -0.277   0.7819    
# Perf_Strivings_c                       -1.258e-03  1.864e-03   -0.675   0.4999    
# pb(subject_nr)                         -9.913e-08  4.041e-07   -0.245   0.8062    
# pb(block_loop)                          2.152e-04  5.223e-04    0.412   0.6803    
# pb(Change_category01, by = block_loop)  2.554e-02  3.734e-03    6.840 8.36e-12 ***
# IA_LABELNegative:Perf_Strivings_c       3.088e-04  2.629e-03    0.117   0.9065    
# IA_LABELFailure:Perf_Strivings_c        2.329e-03  2.631e-03    0.885   0.3761    
# IA_LABELPositive:Perf_Strivings_c       2.811e-03  2.638e-03    1.066   0.2866    
# IA_LABELSuccess:Perf_Strivings_c        1.684e-03  2.635e-03    0.639   0.5227    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                    Estimate Std. Error  t value Pr(>|t|)    
# (Intercept)        -2.72404    0.01732 -157.239   <2e-16 ***
# guessed_block_rule -0.02113    0.01917   -1.102     0.27    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms may not be reliable. 
# ------------------------------------------------------------------
# No. of observations in the fit:  10145 
# Degrees of Freedom for the fit:  15.5045
#       Residual Deg. of Freedom:  10129.5 
#                       at cycle:  5 
#  
# Global Deviance:     -46743.39 
#             AIC:     -46712.38 
#             SBC:     -46600.36  

# Back-transformed coefficients
print(round(inv.logit(bm2.1_first_fix_latency$mu.coefficients),4))
# (Intercept)                       IA_LABELNegative 
# 0.1988                                 0.5030 
# IA_LABELFailure                       IA_LABELPositive 
# 0.5024                                 0.4975 
# IA_LABELSuccess                       Perf_Strivings_c 
# 0.4997                                 0.4997 
# pb(subject_nr)                         pb(block_loop) 
# 0.5000                                 0.5001 
# pb(Change_category01, by = block_loop)      IA_LABELNegative:Perf_Strivings_c 
# 0.5064                                 0.5001 
# IA_LABELFailure:Perf_Strivings_c      IA_LABELPositive:Perf_Strivings_c 
# 0.5006                                 0.5007 
# IA_LABELSuccess:Perf_Strivings_c 
# 0.5004 

# Model comparison
lrtest(bm1_first_fix_latency, bm2.1_first_fix_latency)
#      #Df LogLik     Df Chisq Pr(>Chisq)
# 1 10.504  23371                        
# 2 15.505  23372 5.0001 1.792     0.8771

AIC(bm2.1_first_fix_latency, bm1_first_fix_latency)
#                               df       AIC
# bm1_first_fix_latency   10.50445 -46720.59
# bm2.1_first_fix_latency 15.50450 -46712.38

# Full Model: Perf_Concerns
bm2.2_first_fix_latency <- gamlss(
  first_fix_latency ~ 
    IA_LABEL * Perf_Concerns_c +
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
# Call:  gamlss(formula = first_fix_latency ~ IA_LABEL * Perf_Concerns_c +  
#     pb(subject_nr) + pb(block_loop) + pb(Change_category01, by = block_loop),  
#     sigma.formula = ~guessed_block_rule, family = BE(mu.link = "logit"),  
#     data = na.omit(sub_et_qu)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  logit
# Mu Coefficients:
#                                          Estimate Std. Error  t value Pr(>|t|)    
# (Intercept)                            -1.394e+00  4.681e-03 -297.713  < 2e-16 ***
# IA_LABELNegative                        1.187e-02  4.738e-03    2.505   0.0123 *  
# IA_LABELFailure                         9.611e-03  4.741e-03    2.027   0.0427 *  
# IA_LABELPositive                       -9.909e-03  4.754e-03   -2.084   0.0372 *  
# IA_LABELSuccess                        -1.316e-03  4.748e-03   -0.277   0.7817    
# Perf_Concerns_c                         7.307e-05  1.428e-03    0.051   0.9592    
# pb(subject_nr)                         -9.736e-08  4.050e-07   -0.240   0.8100    
# pb(block_loop)                          2.160e-04  5.223e-04    0.414   0.6792    
# pb(Change_category01, by = block_loop)  2.554e-02  3.734e-03    6.840 8.38e-12 ***
# IA_LABELNegative:Perf_Concerns_c       -1.055e-03  2.013e-03   -0.524   0.6003    
# IA_LABELFailure:Perf_Concerns_c         1.673e-04  2.014e-03    0.083   0.9338    
# IA_LABELPositive:Perf_Concerns_c        1.556e-03  2.020e-03    0.770   0.4411    
# IA_LABELSuccess:Perf_Concerns_c        -7.246e-04  2.017e-03   -0.359   0.7195    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                    Estimate Std. Error  t value Pr(>|t|)    
# (Intercept)        -2.72403    0.01732 -157.239   <2e-16 ***
# guessed_block_rule -0.02115    0.01917   -1.103     0.27    
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
# Global Deviance:     -46743.61 
#             AIC:     -46712.6 
#             SBC:     -46600.58 

# Back-transformed coefficients
print(round(inv.logit(bm2.2_first_fix_latency$mu.coefficients),4))
# (Intercept)                       IA_LABELNegative 
# 0.1988                                 0.5030 
# IA_LABELFailure                       IA_LABELPositive 
# 0.5024                                 0.4975 
# IA_LABELSuccess                        Perf_Concerns_c 
# 0.4997                                 0.5000 
# pb(subject_nr)                         pb(block_loop) 
# 0.5000                                 0.5001 
# pb(Change_category01, by = block_loop)       IA_LABELNegative:Perf_Concerns_c 
# 0.5064                                 0.4997 
# IA_LABELFailure:Perf_Concerns_c       IA_LABELPositive:Perf_Concerns_c 
# 0.5000                                 0.5004 
# IA_LABELSuccess:Perf_Concerns_c 
# 0.4998 

# Model comparison
lrtest(bm1_first_fix_latency, bm2.2_first_fix_latency)
#      #Df LogLik     Df Chisq Pr(>Chisq)
# 1 10.504  23371                        
# 2 15.505  23372 5.0001 2.013     0.8473

AIC(bm2.2_first_fix_latency, bm1_first_fix_latency)
#                               df       AIC
# bm1_first_fix_latency   10.50445 -46720.59
# bm2.2_first_fix_latency 15.50451 -46712.60

# Fit a Beta glmm for Dwell Time
#-------------------------------

# Early Phase
#-------------

# Null model
bm0_dwell_time_early <- gamlss(
  dwell_time_early_t01 ~ 
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
# Call:  gamlss(formula = dwell_time_early_t01 ~ pb(subject_nr) +  
#     pb(block_loop) + pb(Change_category01, by = block_loop),  
#     sigma.formula = ~guessed_block_rule, family = BE(mu.link = "logit"),  
#     data = na.omit(sub_et_qu)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  logit
# Mu Coefficients:
#                                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                            -1.429e+00  1.651e-02 -86.561   <2e-16 ***
# pb(subject_nr)                          2.351e-06  1.817e-06   1.294    0.196    
# pb(block_loop)                          2.861e-03  2.390e-03   1.197    0.231    
# pb(Change_category01, by = block_loop)  1.601e-02  1.721e-02   0.930    0.353    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        -0.93062    0.01987 -46.841   <2e-16 ***
# guessed_block_rule  0.02875    0.02174   1.322    0.186    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms maybe are not accurate. 
# ------------------------------------------------------------------
# No. of observations in the fit:  10145 
# Degrees of Freedom for the fit:  6.97434
#       Residual Deg. of Freedom:  10138.03 
#                       at cycle:  6 
#  
# Global Deviance:     -17001.41 
#             AIC:     -16987.46 
#             SBC:     -16937.08 

# Back-transformed coefficients
print(round(inv.logit(bm0_dwell_time_early$mu.coefficients),4))
# (Intercept)                         pb(subject_nr) 
# 0.1932                                 0.5000 
# pb(block_loop) pb(Change_category01, by = block_loop) 
# 0.5007                                 0.5040

 # Partial Model
bm1_dwell_time_early <- gamlss(
  dwell_time_early_t01 ~ 
    IA_LABEL + 
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
# Call:  gamlss(formula = dwell_time_early_t01 ~ IA_LABEL + pb(subject_nr) +  
#     pb(block_loop) + pb(Change_category01, by = block_loop),  
#     sigma.formula = ~guessed_block_rule, family = BE(mu.link = "logit"),  
#     data = na.omit(sub_et_qu)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  logit
# Mu Coefficients:
#                                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                            -1.415e+00  2.110e-02 -67.059   <2e-16 ***
# IA_LABELNegative                       -5.437e-02  2.147e-02  -2.532   0.0114 *  
# IA_LABELFailure                         7.298e-03  2.134e-02   0.342   0.7323    
# IA_LABELPositive                       -5.040e-05  2.135e-02  -0.002   0.9981    
# IA_LABELSuccess                        -2.727e-02  2.141e-02  -1.274   0.2028    
# pb(subject_nr)                          2.336e-06  1.810e-06   1.291   0.1967    
# pb(block_loop)                          2.912e-03  2.358e-03   1.235   0.2168    
# pb(Change_category01, by = block_loop)  1.535e-02  1.690e-02   0.908   0.3637    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        -0.93354    0.01960 -47.629   <2e-16 ***
# guessed_block_rule  0.03139    0.02170   1.446    0.148    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms may not be reliable. 
# ------------------------------------------------------------------
# No. of observations in the fit:  10145 
# Degrees of Freedom for the fit:  10.97362
#       Residual Deg. of Freedom:  10134.03 
#                       at cycle:  6 
#  
# Global Deviance:     -17012.9 
#             AIC:     -16990.95 
#             SBC:     -16911.67 

# Back-transformed coefficients
print(round(inv.logit(bm1_dwell_time_early$mu.coefficients),4))
# (Intercept)                       IA_LABELNegative 
# 0.1955                                 0.4864 
# IA_LABELFailure                       IA_LABELPositive 
# 0.5018                                 0.5000 
# IA_LABELSuccess                         pb(subject_nr) 
# 0.4932                                 0.5000 
# pb(block_loop) pb(Change_category01, by = block_loop) 
# 0.5007                                 0.5038 

# Model comparison
lrtest(bm1_dwell_time_early, bm0_dwell_time_early)
#      #Df LogLik      Df  Chisq Pr(>Chisq)  
# 1 10.9736 8506.4                            
# 2  6.9743 8500.7 -3.9993 11.487     0.0216 *

AIC(bm1_dwell_time_early, bm0_dwell_time_early)
#                            df       AIC
# bm1_dwell_time_early 10.97362 -16990.95
# bm0_dwell_time_early  6.97434 -16987.46

# Full Model: Perf_Strivings
bm2.1_dwell_time_early <- gamlss(
  dwell_time_early_t01 ~ 
    IA_LABEL * Perf_Strivings_c + 
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
# Call:  gamlss(formula = dwell_time_early_t01 ~ IA_LABEL * Perf_Strivings_c +  
#     pb(subject_nr) + pb(block_loop) + pb(Change_category01, by = block_loop),  
#     sigma.formula = ~guessed_block_rule, family = BE(mu.link = "logit"),  
#     data = na.omit(sub_et_qu)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  logit
# Mu Coefficients:
#                                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                            -1.417e+00  2.109e-02 -67.175  < 2e-16 ***
# IA_LABELNegative                       -5.439e-02  2.146e-02  -2.534  0.01128 *  
# IA_LABELFailure                         7.282e-03  2.132e-02   0.342  0.73272    
# IA_LABELPositive                        1.562e-04  2.134e-02   0.007  0.99416    
# IA_LABELSuccess                        -2.706e-02  2.140e-02  -1.265  0.20606    
# Perf_Strivings_c                        1.212e-02  8.376e-03   1.446  0.14810    
# pb(subject_nr)                          2.619e-06  1.814e-06   1.444  0.14882    
# pb(block_loop)                          3.046e-03  2.356e-03   1.293  0.19605    
# pb(Change_category01, by = block_loop)  1.492e-02  1.689e-02   0.883  0.37711    
# IA_LABELNegative:Perf_Strivings_c      -9.980e-03  1.190e-02  -0.839  0.40175    
# IA_LABELFailure:Perf_Strivings_c       -1.335e-02  1.182e-02  -1.129  0.25879    
# IA_LABELPositive:Perf_Strivings_c      -3.396e-02  1.182e-02  -2.872  0.00409 ** 
# IA_LABELSuccess:Perf_Strivings_c       -3.837e-02  1.186e-02  -3.236  0.00122 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        -0.93507    0.01960 -47.713   <2e-16 ***
# guessed_block_rule  0.03174    0.02170   1.463    0.144    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms may not be reliable. 
# ------------------------------------------------------------------
# No. of observations in the fit:  10145 
# Degrees of Freedom for the fit:  15.98795
#       Residual Deg. of Freedom:  10129.01 
#                       at cycle:  6 
#  
# Global Deviance:     -17031.69 
#             AIC:     -16999.72 
#             SBC:     -16884.21 

# Back-transformed coefficients
print(round(inv.logit(bm2.1_dwell_time_early$mu.coefficients),4))
# (Intercept)                       IA_LABELNegative 
# 0.1952                                 0.4864 
# IA_LABELFailure                       IA_LABELPositive 
# 0.5018                                 0.5000 
# IA_LABELSuccess                       Perf_Strivings_c 
# 0.4932                                 0.5030 
# pb(subject_nr)                         pb(block_loop) 
# 0.5000                                 0.5008 
# pb(Change_category01, by = block_loop)      IA_LABELNegative:Perf_Strivings_c 
# 0.5037                                 0.4975 
# IA_LABELFailure:Perf_Strivings_c      IA_LABELPositive:Perf_Strivings_c 
# 0.4967                                 0.4915 
# IA_LABELSuccess:Perf_Strivings_c 
# 0.4904 

# Model comparison
lrtest(bm2.1_dwell_time_early, bm1_dwell_time_early)
#      #Df LogLik      Df  Chisq Pr(>Chisq)   
# 1 15.988 8515.8                             
# 2 10.974 8506.4 -5.0143 18.796   0.002098 **

AIC(bm2.1_dwell_time_early, bm1_dwell_time_early)
#                              df       AIC
# bm2.1_dwell_time_early 15.98795 -16999.72
# bm1_dwell_time_early   10.97362 -16990.95

# Full Model: Perf_Concerns
bm2.2_dwell_time_early <- gamlss(
  dwell_time_early_t01 ~ 
    IA_LABEL * Perf_Concerns_c + 
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
# Call:  gamlss(formula = dwell_time_early_t01 ~ IA_LABEL * Perf_Concerns_c +  
#     pb(subject_nr) + pb(block_loop) + pb(Change_category01, by = block_loop),  
#     sigma.formula = ~guessed_block_rule, family = BE(mu.link = "logit"),  
#     data = na.omit(sub_et_qu)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  logit
# Mu Coefficients:
#                                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                            -1.415e+00  2.110e-02 -67.057   <2e-16 ***
# IA_LABELNegative                       -5.436e-02  2.147e-02  -2.532   0.0114 *  
# IA_LABELFailure                         7.296e-03  2.133e-02   0.342   0.7324    
# IA_LABELPositive                       -6.515e-05  2.135e-02  -0.003   0.9976    
# IA_LABELSuccess                        -2.720e-02  2.141e-02  -1.271   0.2039    
# Perf_Concerns_c                         6.607e-03  6.428e-03   1.028   0.3041    
# pb(subject_nr)                          2.492e-06  1.819e-06   1.370   0.1706    
# pb(block_loop)                          2.862e-03  2.357e-03   1.214   0.2248    
# pb(Change_category01, by = block_loop)  1.529e-02  1.690e-02   0.905   0.3654    
# IA_LABELNegative:Perf_Concerns_c       -8.828e-03  9.135e-03  -0.966   0.3339    
# IA_LABELFailure:Perf_Concerns_c        -1.308e-02  9.079e-03  -1.441   0.1497    
# IA_LABELPositive:Perf_Concerns_c       -6.826e-03  9.081e-03  -0.752   0.4523    
# IA_LABELSuccess:Perf_Concerns_c        -1.790e-02  9.112e-03  -1.964   0.0495 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        -0.93333    0.01960 -47.619   <2e-16 ***
# guessed_block_rule  0.03072    0.02170   1.416    0.157    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms may not be reliable. 
# ------------------------------------------------------------------
# No. of observations in the fit:  10145 
# Degrees of Freedom for the fit:  15.96925
#       Residual Deg. of Freedom:  10129.03 
#                       at cycle:  6 
#  
# Global Deviance:     -17018.05 
#             AIC:     -16986.12 
#             SBC:     -16870.74 

# Back-transformed coefficients
print(round(inv.logit(bm2.2_dwell_time_early$mu.coefficients),4))
# (Intercept)                       IA_LABELNegative 
# 0.1954                                 0.4864 
# IA_LABELFailure                       IA_LABELPositive 
# 0.5018                                 0.5000 
# IA_LABELSuccess                        Perf_Concerns_c 
# 0.4932                                 0.5017 
# pb(subject_nr)                         pb(block_loop) 
# 0.5000                                 0.5007 
# pb(Change_category01, by = block_loop)       IA_LABELNegative:Perf_Concerns_c 
# 0.5038                                 0.4978 
# IA_LABELFailure:Perf_Concerns_c       IA_LABELPositive:Perf_Concerns_c 
# 0.4967                                 0.4983 
# IA_LABELSuccess:Perf_Concerns_c 
# 0.4955

# Model comparison
lrtest(bm2.2_dwell_time_early, bm1_dwell_time_early)
#      #Df LogLik      Df  Chisq Pr(>Chisq)
# 1 15.969 8509.0                          
# 2 10.974 8506.4 -4.9956 5.1556     0.3972

AIC(bm2.2_dwell_time_early, bm1_dwell_time_early)
#                              df       AIC
# bm1_dwell_time_early   10.97362 -16990.95
# bm2.2_dwell_time_early 15.96925 -16986.12

# Plot the retained interaction
preds_dwell_time_early_PS <- ggpredict(bm2.1_dwell_time_early, terms = c("Perf_Strivings_c", "IA_LABEL"))
preds_dwell_time_early_PC <- ggpredict(bm2.2_dwell_time_early, terms = c("Perf_Concerns_c", "IA_LABEL"))

dte_PS <- ggplot(preds_dwell_time_early_PS, aes(x = x, y = predicted, color = group)) +
  geom_line(size = 1) +                                # Line only, no CI ribbon
  labs(
    title = "Predicted Dwell Time (Early)",
    x = "Perfectionistic Strivings",
    y = "",
    color = "Semantic Category"
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

dte_PC <- ggplot(preds_dwell_time_early_PC, aes(x = x, y = predicted, color = group)) +
  geom_line(size = 1) +                                # Line only, no CI ribbon
  labs(
    title = "Predicted Dwell Time (Early)",
    x = "Perfectionistic Concerns",
    y = "",
    color = "Semantic Category"
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
grid.arrange(dte_PS, dte_PC,
             ncol = 2, nrow = 1,
             top = textGrob("Predicted Dwell Time (Early)",
                            gp = gpar(fontsize = 16, fontface = "bold")
                            )
             )


# Intermediate Phase
#-------------------

# Null model
bm0_dwell_time_intermediate <- gamlss(
  dwell_time_intermediate_t01 ~ 
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
# Call:  gamlss(formula = dwell_time_intermediate_t01 ~ pb(subject_nr) +  
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
# (Intercept)                            -1.442e+00  1.027e-02 -140.333   <2e-16 ***
# pb(subject_nr)                          3.451e-07  1.202e-06    0.287    0.774    
# pb(block_loop)                          3.795e-04  1.289e-03    0.294    0.768    
# pb(Change_category01, by = block_loop)  2.523e-01  1.105e-02   22.829   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        -1.47701    0.01858 -79.513   <2e-16 ***
# guessed_block_rule  0.02005    0.02039   0.983    0.326    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms maybe are not accurate. 
# ------------------------------------------------------------------
# No. of observations in the fit:  10145 
# Degrees of Freedom for the fit:  6.795059
#       Residual Deg. of Freedom:  10138.2 
#                       at cycle:  6 
#  
# Global Deviance:     -24333.63 
#             AIC:     -24320.04 
#             SBC:     -24270.95  

# Back-transformed coefficients
print(round(inv.logit(bm0_dwell_time_intermediate$mu.coefficients),4))
# (Intercept)                         pb(subject_nr) 
# 0.1913                                 0.5000 
# pb(block_loop) pb(Change_category01, by = block_loop) 
# 0.5001                                 0.5627

 # Partial Model
bm1_dwell_time_intermediate <- gamlss(
  dwell_time_intermediate_t01 ~ 
    IA_LABEL + 
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
# Call:  gamlss(formula = dwell_time_intermediate_t01 ~ IA_LABEL +  
#     pb(subject_nr) + pb(block_loop) + pb(Change_category01,  
#     by = block_loop), sigma.formula = ~guessed_block_rule,  
#     family = BE(mu.link = "logit"), data = na.omit(sub_et_qu)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  logit
# Mu Coefficients:
#                                          Estimate Std. Error  t value Pr(>|t|)    
# (Intercept)                            -1.472e+00  1.415e-02 -104.060  < 2e-16 ***
# IA_LABELNegative                        3.903e-02  1.457e-02    2.679 0.007402 ** 
# IA_LABELFailure                         1.151e-02  1.480e-02    0.778 0.436880    
# IA_LABELPositive                        5.123e-02  1.456e-02    3.520 0.000434 ***
# IA_LABELSuccess                         4.559e-02  1.460e-02    3.122 0.001802 ** 
# pb(subject_nr)                          3.830e-07  1.476e-06    0.259 0.795340    
# pb(block_loop)                          4.759e-04  1.413e-03    0.337 0.736344    
# pb(Change_category01, by = block_loop)  2.524e-01  1.105e-02   22.850  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        -1.47900    0.01964 -75.315   <2e-16 ***
# guessed_block_rule  0.02109    0.02183   0.966    0.334    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms maybe are not accurate. 
# ------------------------------------------------------------------
# No. of observations in the fit:  10145 
# Degrees of Freedom for the fit:  10.79403
#       Residual Deg. of Freedom:  10134.21 
#                       at cycle:  6 
#  
# Global Deviance:     -24352.85 
#             AIC:     -24331.26 
#             SBC:     -24253.27 

# Back-transformed coefficients
print(round(inv.logit(bm1_dwell_time_intermediate$mu.coefficients),4))
# (Intercept)                       IA_LABELNegative 
# 0.1866                                 0.5098 
# IA_LABELFailure                       IA_LABELPositive 
# 0.5029                                 0.5128 
# IA_LABELSuccess                         pb(subject_nr) 
# 0.5114                                 0.5000 
# pb(block_loop) pb(Change_category01, by = block_loop) 
# 0.5001                                 0.5628 

# Model comparison
lrtest(bm1_dwell_time_intermediate, bm0_dwell_time_intermediate)
#       #Df LogLik     Df  Chisq Pr(>Chisq)    
# 1 10.7940  12176                             
# 2  6.7951  12167 -3.999 19.213  0.0007136 ***

AIC(bm1_dwell_time_intermediate, bm0_dwell_time_intermediate)
#                                    df       AIC
# bm1_dwell_time_intermediate 10.794034 -24331.26
# bm0_dwell_time_intermediate  6.795059 -24320.04

# Full Model: Perf_Strivings
bm2.1_dwell_time_intermediate <- gamlss(
  dwell_time_intermediate_t01 ~ 
    IA_LABEL * Perf_Strivings_c + 
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
# Call:  gamlss(formula = dwell_time_intermediate_t01 ~ IA_LABEL *  
#     Perf_Strivings_c + pb(subject_nr) + pb(block_loop) +  
#     pb(Change_category01, by = block_loop), sigma.formula = ~guessed_block_rule,  
#     family = BE(mu.link = "logit"), data = na.omit(sub_et_qu)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  logit
# Mu Coefficients:
#                                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                            -1.472e+00  1.975e-02 -74.566  < 2e-16 ***
# IA_LABELNegative                        3.910e-02  1.459e-02   2.679 0.007395 ** 
# IA_LABELFailure                         1.151e-02  1.508e-02   0.763 0.445355    
# IA_LABELPositive                        5.123e-02  1.455e-02   3.521 0.000432 ***
# IA_LABELSuccess                         4.559e-02  1.459e-02   3.124 0.001791 ** 
# Perf_Strivings_c                       -5.544e-03  5.665e-03  -0.979 0.327783    
# pb(subject_nr)                          4.337e-07  1.573e-06   0.276 0.782707    
# pb(block_loop)                          4.682e-04  3.017e-03   0.155 0.876656    
# pb(Change_category01, by = block_loop)  2.524e-01  1.105e-02  22.842  < 2e-16 ***
# IA_LABELNegative:Perf_Strivings_c      -1.097e-02  8.140e-03  -1.348 0.177811    
# IA_LABELFailure:Perf_Strivings_c        7.902e-03  8.044e-03   0.982 0.325926    
# IA_LABELPositive:Perf_Strivings_c       1.187e-02  7.961e-03   1.491 0.135953    
# IA_LABELSuccess:Perf_Strivings_c        9.698e-03  8.065e-03   1.203 0.229197    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        -1.48012    0.01975 -74.933   <2e-16 ***
# guessed_block_rule  0.02163    0.02199   0.984    0.325    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms maybe are not accurate. 
# ------------------------------------------------------------------
# No. of observations in the fit:  10145 
# Degrees of Freedom for the fit:  15.80416
#       Residual Deg. of Freedom:  10129.2 
#                       at cycle:  6 
#  
# Global Deviance:     -24364.33 
#             AIC:     -24332.72 
#             SBC:     -24218.54 

# Back-transformed coefficients
print(round(inv.logit(bm2.1_dwell_time_intermediate$mu.coefficients),4))
# (Intercept)                       IA_LABELNegative 
# 0.1866                                 0.5098 
# IA_LABELFailure                       IA_LABELPositive 
# 0.5029                                 0.5128 
# IA_LABELSuccess                       Perf_Strivings_c 
# 0.5114                                 0.4986 
# pb(subject_nr)                         pb(block_loop) 
# 0.5000                                 0.5001 
# pb(Change_category01, by = block_loop)      IA_LABELNegative:Perf_Strivings_c 
# 0.5628                                 0.4973 
# IA_LABELFailure:Perf_Strivings_c      IA_LABELPositive:Perf_Strivings_c 
# 0.5020                                 0.5030 
# IA_LABELSuccess:Perf_Strivings_c 
# 0.5024

# Model comparison
lrtest(bm2.1_dwell_time_intermediate, bm1_dwell_time_intermediate)
#      #Df LogLik      Df  Chisq Pr(>Chisq)  
# 1 15.804  12182                            
# 2 10.794  12176 -5.0101 11.479    0.04267 *

AIC(bm2.1_dwell_time_intermediate, bm1_dwell_time_intermediate)
#                                     df       AIC
# bm2.1_dwell_time_intermediate 15.80416 -24332.72
# bm1_dwell_time_intermediate   10.79403 -24331.26

# Full Model: Perf_Concerns
bm2.2_dwell_time_intermediate <- gamlss(
  dwell_time_intermediate_t01 ~ 
    IA_LABEL * Perf_Concerns_c + 
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
# Call:  gamlss(formula = dwell_time_intermediate_t01 ~ IA_LABEL *  
#     Perf_Concerns_c + pb(subject_nr) + pb(block_loop) +  
#     pb(Change_category01, by = block_loop), sigma.formula = ~guessed_block_rule,  
#     family = BE(mu.link = "logit"), data = na.omit(sub_et_qu)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  logit
# Mu Coefficients:
#                                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                            -1.472e+00  1.782e-02 -82.614  < 2e-16 ***
# IA_LABELNegative                        3.914e-02  1.465e-02   2.672 0.007560 ** 
# IA_LABELFailure                         1.151e-02  1.547e-02   0.744 0.457192    
# IA_LABELPositive                        5.125e-02  1.475e-02   3.474 0.000515 ***
# IA_LABELSuccess                         4.557e-02  1.479e-02   3.081 0.002068 ** 
# Perf_Concerns_c                         2.026e-03  5.832e-03   0.347 0.728282    
# pb(subject_nr)                          4.608e-07  1.367e-06   0.337 0.736007    
# pb(block_loop)                          4.444e-04  1.611e-03   0.276 0.782616    
# pb(Change_category01, by = block_loop)  2.523e-01  1.104e-02  22.860  < 2e-16 ***
# IA_LABELNegative:Perf_Concerns_c       -1.721e-02  7.320e-03  -2.352 0.018703 *  
# IA_LABELFailure:Perf_Concerns_c         3.299e-03  8.129e-03   0.406 0.684919    
# IA_LABELPositive:Perf_Concerns_c       -6.169e-03  7.438e-03  -0.829 0.406904    
# IA_LABELSuccess:Perf_Concerns_c         3.286e-03  7.517e-03   0.437 0.662044    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        -1.48016    0.01873 -79.008   <2e-16 ***
# guessed_block_rule  0.02132    0.02061   1.034    0.301    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms maybe are not accurate. 
# ------------------------------------------------------------------
# No. of observations in the fit:  10145 
# Degrees of Freedom for the fit:  15.79271
#       Residual Deg. of Freedom:  10129.21 
#                       at cycle:  6 
#  
# Global Deviance:     -24369.26 
#             AIC:     -24337.67 
#             SBC:     -24223.57 

# Back-transformed coefficients
print(round(inv.logit(bm2.2_dwell_time_intermediate$mu.coefficients),4))
# (Intercept)                       IA_LABELNegative 
# 0.1866                                 0.5098 
# IA_LABELFailure                       IA_LABELPositive 
# 0.5029                                 0.5128 
# IA_LABELSuccess                        Perf_Concerns_c 
# 0.5114                                 0.5005 
# pb(subject_nr)                         pb(block_loop) 
# 0.5000                                 0.5001 
# pb(Change_category01, by = block_loop)       IA_LABELNegative:Perf_Concerns_c 
# 0.5627                                 0.4957 
# IA_LABELFailure:Perf_Concerns_c       IA_LABELPositive:Perf_Concerns_c 
# 0.5008                                 0.4985 
# IA_LABELSuccess:Perf_Concerns_c 
# 0.5008 

# Model comparison
lrtest(bm2.2_dwell_time_intermediate, bm1_dwell_time_intermediate)
#      #Df LogLik      Df Chisq Pr(>Chisq)   
# 1 15.793  12185                            
# 2 10.794  12176 -4.9987 16.41   0.005766 **

AIC(bm2.2_dwell_time_intermediate, bm1_dwell_time_intermediate)
#                                     df       AIC
# bm2.2_dwell_time_intermediate 15.79271 -24337.67
# bm1_dwell_time_intermediate   10.79403 -24331.26

# Simulate predicted values
preds_dwell_time_intermediate_PS <- ggpredict(bm2.1_dwell_time_intermediate, terms = c("Perf_Strivings_c", "IA_LABEL"))
preds_dwell_time_intermediate_PC <- ggpredict(bm2.2_dwell_time_intermediate, terms = c("Perf_Concerns_c", "IA_LABEL"))

# Plot the retained interaction 
dti_PS <- ggplot(preds_dwell_time_intermediate_PS, aes(x = x, y = predicted, color = group)) +
  geom_line(size = 1) +                                # Line only, no CI ribbon
  labs(
    x = "Perfectionistic Strivings",
    y = "",
    color = "Semantic Category"
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
  labs(
    x = "Perfectionistic Concerns",
    y = "",
    color = "Semantic Category"
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
             top = textGrob("Predicted Dwell Time (Intermediate)",
                            gp = gpar(fontsize = 16, fontface = "bold")
                            )
             )

#Late Phase
#----------

# Null model
bm0_dwell_time_late <- gamlss(
  dwell_time_late_t01 ~ 
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
# Call:  gamlss(formula = dwell_time_late_t01 ~ pb(subject_nr) +  
#     pb(block_loop) + pb(Change_category01, by = block_loop),  
#     sigma.formula = ~guessed_block_rule, family = BE(mu.link = "logit"),  
#     data = na.omit(sub_et_qu)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  logit
# Mu Coefficients:
#                                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                            -1.480e+00  2.000e-02 -73.986  < 2e-16 ***
# pb(subject_nr)                         -9.552e-06  2.149e-06  -4.445  8.9e-06 ***
# pb(block_loop)                          1.924e-03  2.817e-03   0.683    0.495    
# pb(Change_category01, by = block_loop)  6.101e-01  1.939e-02  31.458  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        -0.80343    0.02010  -39.98   <2e-16 ***
# guessed_block_rule  0.25655    0.02198   11.67   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms maybe are not accurate. 
# ------------------------------------------------------------------
# No. of observations in the fit:  10145 
# Degrees of Freedom for the fit:  9.562981
#       Residual Deg. of Freedom:  10135.44 
#                       at cycle:  8 
#  
# Global Deviance:     -14389.41 
#             AIC:     -14370.28 
#             SBC:     -14301.19  

# Back-transformed coefficients
print(round(inv.logit(bm0_dwell_time_late$mu.coefficients),4))
# (Intercept)                         pb(subject_nr) 
# 0.1854                                 0.5000 
# pb(block_loop) pb(Change_category01, by = block_loop) 
# 0.5005                                 0.6480 

 # Partial Model
bm1_dwell_time_late <- gamlss(
  dwell_time_late_t01 ~ 
    IA_LABEL + 
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
# Call:  gamlss(formula = dwell_time_late_t01 ~ IA_LABEL + pb(subject_nr) +  
#     pb(block_loop) + pb(Change_category01, by = block_loop),  
#     sigma.formula = ~guessed_block_rule, family = BE(mu.link = "logit"),  
#     data = na.omit(sub_et_qu)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  logit
# Mu Coefficients:
#                                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                            -1.525e+00  2.575e-02 -59.217  < 2e-16 ***
# IA_LABELNegative                        5.634e-02  2.537e-02   2.221  0.02637 *  
# IA_LABELFailure                         5.005e-02  2.536e-02   1.973  0.04848 *  
# IA_LABELPositive                        4.072e-02  2.542e-02   1.602  0.10922    
# IA_LABELSuccess                         6.844e-02  2.534e-02   2.701  0.00693 ** 
# pb(subject_nr)                         -9.490e-06  2.146e-06  -4.422 9.89e-06 ***
# pb(block_loop)                          2.129e-03  2.764e-03   0.770  0.44116    
# pb(Change_category01, by = block_loop)  6.107e-01  1.939e-02  31.502  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        -0.80403    0.02010  -40.01   <2e-16 ***
# guessed_block_rule  0.25652    0.02198   11.67   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms maybe are not accurate. 
# ------------------------------------------------------------------
# No. of observations in the fit:  10145 
# Degrees of Freedom for the fit:  13.51252
#       Residual Deg. of Freedom:  10131.49 
#                       at cycle:  8 
#  
# Global Deviance:     -14397.65 
#             AIC:     -14370.62 
#             SBC:     -14273 

# Back-transformed coefficients
print(round(inv.logit(bm1_dwell_time_late$mu.coefficients),4))
# (Intercept)                       IA_LABELNegative 
# 0.1788                                 0.5141 
# IA_LABELFailure                       IA_LABELPositive 
# 0.5125                                 0.5102 
# IA_LABELSuccess                         pb(subject_nr) 
# 0.5171                                 0.5000 
# pb(block_loop) pb(Change_category01, by = block_loop) 
# 0.5005                                 0.6481 

# Model comparison
lrtest(bm1_dwell_time_late, bm0_dwell_time_late)
#      #Df LogLik      Df  Chisq Pr(>Chisq)  
# 1 13.513 7198.8                            
# 2  9.563 7194.7 -3.9495 8.2407    0.08315 .

AIC(bm1_dwell_time_late, bm0_dwell_time_late)
#                            df       AIC
# bm1_dwell_time_late 13.512515 -14370.62
# bm0_dwell_time_late  9.562981 -14370.28

# Full Model: Perf_Strivings
bm2.1_dwell_time_late <- gamlss(
  dwell_time_late_t01 ~ 
    IA_LABEL * Perf_Strivings_c + 
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
# Call:  gamlss(formula = dwell_time_late_t01 ~ IA_LABEL * Perf_Strivings_c +  
#     pb(subject_nr) + pb(block_loop) + pb(Change_category01, by = block_loop),  
#     sigma.formula = ~guessed_block_rule, family = BE(mu.link = "logit"),  
#     data = na.omit(sub_et_qu)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  logit
# Mu Coefficients:
#                                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                            -1.524e+00  2.541e-02 -59.996  < 2e-16 ***
# IA_LABELNegative                        5.639e-02  2.537e-02   2.222  0.02628 *  
# IA_LABELFailure                         4.998e-02  2.540e-02   1.968  0.04910 *  
# IA_LABELPositive                        4.070e-02  2.541e-02   1.602  0.10924    
# IA_LABELSuccess                         6.839e-02  2.537e-02   2.696  0.00702 ** 
# Perf_Strivings_c                        1.731e-02  9.961e-03   1.738  0.08226 .  
# pb(subject_nr)                         -9.683e-06  2.135e-06  -4.536 5.81e-06 ***
# pb(block_loop)                          2.146e-03  2.790e-03   0.769  0.44181    
# pb(Change_category01, by = block_loop)  6.108e-01  1.897e-02  32.196  < 2e-16 ***
# IA_LABELNegative:Perf_Strivings_c      -2.793e-02  1.401e-02  -1.994  0.04619 *  
# IA_LABELFailure:Perf_Strivings_c       -3.078e-03  1.402e-02  -0.220  0.82623    
# IA_LABELPositive:Perf_Strivings_c      -3.128e-02  1.402e-02  -2.231  0.02572 *  
# IA_LABELSuccess:Perf_Strivings_c       -1.067e-05  1.400e-02  -0.001  0.99939    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        -0.80590    0.01974  -40.83   <2e-16 ***
# guessed_block_rule  0.25786    0.02190   11.77   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms may not be reliable. 
# ------------------------------------------------------------------
# No. of observations in the fit:  10145 
# Degrees of Freedom for the fit:  18.52322
#       Residual Deg. of Freedom:  10126.48 
#                       at cycle:  8 
#  
# Global Deviance:     -14409.03 
#             AIC:     -14371.98 
#             SBC:     -14238.16 

# Back-transformed coefficients
print(round(inv.logit(bm2.1_dwell_time_late$mu.coefficients),4))
# (Intercept)                       IA_LABELNegative 
# 0.1788                                 0.5141 
# IA_LABELFailure                       IA_LABELPositive 
# 0.5125                                 0.5102 
# IA_LABELSuccess                       Perf_Strivings_c 
# 0.5171                                 0.5043 
# pb(subject_nr)                         pb(block_loop) 
# 0.5000                                 0.5005 
# pb(Change_category01, by = block_loop)      IA_LABELNegative:Perf_Strivings_c 
# 0.6481                                 0.4930 
# IA_LABELFailure:Perf_Strivings_c      IA_LABELPositive:Perf_Strivings_c 
# 0.4992                                 0.4922 
# IA_LABELSuccess:Perf_Strivings_c 
# 0.5000 

# Model comparison
lrtest(bm1_dwell_time_late, bm2.1_dwell_time_late)
#      #Df LogLik     Df  Chisq Pr(>Chisq)  
# 1 13.513 7198.8                           
# 2 18.523 7204.5 5.0107 11.382    0.04431 *

AIC(bm1_dwell_time_late, bm2.1_dwell_time_late)
#                             df       AIC
# bm2.1_dwell_time_late 18.52322 -14371.98
# bm1_dwell_time_late   13.51252 -14370.62

# Full Model: Perf_Concerns
bm2.2_dwell_time_late <- gamlss(
  dwell_time_late_t01 ~ 
    IA_LABEL * Perf_Concerns_c +
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
# Call:  gamlss(formula = dwell_time_late_t01 ~ IA_LABEL * Perf_Concerns_c +  
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
# (Intercept)                            -1.526e+00  2.613e-02 -58.398  < 2e-16 ***
# IA_LABELNegative                        5.656e-02  2.551e-02   2.217  0.02666 *  
# IA_LABELFailure                         5.006e-02  2.544e-02   1.967  0.04918 *  
# IA_LABELPositive                        4.096e-02  2.545e-02   1.610  0.10753    
# IA_LABELSuccess                         6.855e-02  2.545e-02   2.693  0.00709 ** 
# Perf_Concerns_c                         3.213e-03  6.002e-03   0.535  0.59243    
# pb(subject_nr)                         -9.183e-06  2.153e-06  -4.265 2.01e-05 ***
# pb(block_loop)                          2.091e-03  2.775e-03   0.754  0.45116    
# pb(Change_category01, by = block_loop)  6.108e-01  1.938e-02  31.510  < 2e-16 ***
# IA_LABELNegative:Perf_Concerns_c       -1.438e-02  9.592e-03  -1.499  0.13386    
# IA_LABELFailure:Perf_Concerns_c        -3.131e-03  8.796e-03  -0.356  0.72188    
# IA_LABELPositive:Perf_Concerns_c       -1.684e-02  9.636e-03  -1.748  0.08052 .  
# IA_LABELSuccess:Perf_Concerns_c        -7.068e-03  9.203e-03  -0.768  0.44248    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        -0.80442    0.02010  -40.02   <2e-16 ***
# guessed_block_rule  0.25655    0.02198   11.67   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms maybe are not accurate. 
# ------------------------------------------------------------------
# No. of observations in the fit:  10145 
# Degrees of Freedom for the fit:  18.46329
#       Residual Deg. of Freedom:  10126.54 
#                       at cycle:  8 
#  
# Global Deviance:     -14402.91 
#             AIC:     -14365.99 
#             SBC:     -14232.6 

# Back-transformed coefficients
print(round(inv.logit(bm2.2_dwell_time_late$mu.coefficients),4))
# (Intercept)                       IA_LABELNegative 
# 0.1786                                 0.5141 
# IA_LABELFailure                       IA_LABELPositive 
# 0.5125                                 0.5102 
# IA_LABELSuccess                        Perf_Concerns_c 
# 0.5171                                 0.5008 
# pb(subject_nr)                         pb(block_loop) 
# 0.5000                                 0.5005 
# pb(Change_category01, by = block_loop)       IA_LABELNegative:Perf_Concerns_c 
# 0.6481                                 0.4964 
# IA_LABELFailure:Perf_Concerns_c       IA_LABELPositive:Perf_Concerns_c 
# 0.4992                                 0.4958 
# IA_LABELSuccess:Perf_Concerns_c 
# 0.4982 

# Model comparison
lrtest(bm1_dwell_time_late, bm2.2_dwell_time_late)
#      #Df LogLik     Df  Chisq Pr(>Chisq)
# 1 13.513 7198.8                         
# 2 18.463 7201.5 4.9508 5.2661     0.3843

AIC(bm1_dwell_time_late, bm2.2_dwell_time_late)
#                             df       AIC
# bm1_dwell_time_late   13.51252 -14370.62
# bm2.2_dwell_time_late 18.46329 -14365.99

# Simulate predicted values
preds_dwell_time_late_PS <- ggpredict(bm2.1_dwell_time_late, terms = c("Perf_Strivings_c", "IA_LABEL"))
preds_dwell_time_late_PC <- ggpredict(bm2.2_dwell_time_late, terms = c("Perf_Concerns_c", "IA_LABEL"))

# Plot the retained interaction 
dtl_PS <- ggplot(preds_dwell_time_late_PS, aes(x = x, y = predicted, color = group)) +
  geom_line(size = 1) +                                # Line only, no CI ribbon
  labs(
    x = "Perfectionistic Strivings",
    y = "",
    color = "Semantic Category"
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

dtl_PC <- ggplot(preds_dwell_time_late_PC, aes(x = x, y = predicted, color = group)) +
  geom_line(size = 1) +                                # Line only, no CI ribbon
  labs(
    x = "Perfectionistic Concerns",
    y = "",
    color = "Semantic Category"
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
grid.arrange(dtl_PS, dtl_PC,
             ncol = 2, nrow = 1,
             top = textGrob("Predicted Dwell Time (Late)",
                            gp = gpar(fontsize = 16, fontface = "bold")
                            )
             )
             

# Whole Trial
#------------

# Null model
bm0_dwell_time <- gamlss(
  dwell_time ~ 
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
# Call:  gamlss(formula = dwell_time ~ pb(subject_nr) + pb(block_loop) +  
#     pb(Change_category01, by = block_loop), sigma.formula = ~guessed_block_rule,  
#     family = BE(mu.link = "cauchit"), data = na.omit(sub_et_qu)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  cauchit
# Mu Coefficients:
#                                          Estimate Std. Error  t value Pr(>|t|)    
# (Intercept)                            -1.645e+00  1.589e-02 -103.535   <2e-16 ***
# pb(subject_nr)                          8.903e-07  1.664e-06    0.535    0.593    
# pb(block_loop)                         -2.697e-04  2.158e-03   -0.125    0.901    
# pb(Change_category01, by = block_loop)  9.940e-01  1.241e-02   80.094   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        -1.59181    0.01865 -85.360  < 2e-16 ***
# guessed_block_rule  0.16074    0.02069   7.769 8.67e-15 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms may not be reliable. 
# ------------------------------------------------------------------
# No. of observations in the fit:  10145 
# Degrees of Freedom for the fit:  7.119865
#       Residual Deg. of Freedom:  10137.88 
#                       at cycle:  6 
#  
# Global Deviance:     -24642.03 
#             AIC:     -24627.79 
#             SBC:     -24576.35 

# Back-transformed coefficients
print(round(inv.cauchit(bm0_dwell_time$mu.coefficients),4))
# (Intercept)                         pb(subject_nr) 
# 0.1738                                 0.5000 
# pb(block_loop) pb(Change_category01, by = block_loop) 
# 0.4999                                 0.7490 

 # Partial Model
bm1_dwell_time <- gamlss(
  dwell_time ~ 
    IA_LABEL + 
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
# Call:  gamlss(formula = dwell_time ~ IA_LABEL + pb(subject_nr) +  
#     pb(block_loop) + pb(Change_category01, by = block_loop),  
#     sigma.formula = ~guessed_block_rule, family = BE(mu.link = "cauchit"),  
#     data = na.omit(sub_et_qu)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  cauchit
# Mu Coefficients:
#                                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                            -1.717e+00  2.359e-02 -72.764  < 2e-16 ***
# IA_LABELNegative                        8.488e-02  1.988e-02   4.269 1.98e-05 ***
# IA_LABELFailure                         4.334e-02  2.080e-02   2.084  0.03721 *  
# IA_LABELPositive                        6.243e-02  2.024e-02   3.084  0.00205 ** 
# IA_LABELSuccess                         8.878e-02  2.102e-02   4.223 2.43e-05 ***
# pb(subject_nr)                          9.767e-07  1.643e-06   0.594  0.55221    
# pb(block_loop)                          2.269e-03  2.330e-03   0.974  0.33024    
# pb(Change_category01, by = block_loop)  9.948e-01  1.263e-02  78.762  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        -1.59615    0.01890 -84.435  < 2e-16 ***
# guessed_block_rule  0.16414    0.02103   7.803 6.63e-15 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms maybe are not accurate. 
# ------------------------------------------------------------------
# No. of observations in the fit:  10145 
# Degrees of Freedom for the fit:  11.12448
#       Residual Deg. of Freedom:  10133.88 
#                       at cycle:  6 
#  
# Global Deviance:     -24667.61 
#             AIC:     -24645.36 
#             SBC:     -24564.99 

# Back-transformed coefficients
print(round(inv.cauchit(bm1_dwell_time$mu.coefficients),4))
# (Intercept)                       IA_LABELNegative 
# 0.1679                                 0.5270 
# IA_LABELFailure                       IA_LABELPositive 
# 0.5138                                 0.5198 
# IA_LABELSuccess                         pb(subject_nr) 
# 0.5282                                 0.5000 
# pb(block_loop) pb(Change_category01, by = block_loop) 
# 0.5007                                 0.7492 

# Model comparison
lrtest(bm1_dwell_time, bm0_dwell_time)
#       #Df LogLik      Df Chisq Pr(>Chisq)    
# 1 11.1245  12334                             
# 2  7.1199  12321 -4.0046 25.58  3.845e-05 ***

AIC(bm1_dwell_time, bm0_dwell_time)
#                       df       AIC
# bm1_dwell_time 11.124480 -24645.36
# bm0_dwell_time  7.119865 -24627.79

# Full Model: Perf_Strivings
bm2.1_dwell_time <- gamlss(
  dwell_time ~ 
    IA_LABEL * Perf_Strivings_c + 
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
# Call:  gamlss(formula = dwell_time ~ IA_LABEL * Perf_Strivings_c +  
#     pb(subject_nr) + pb(block_loop) + pb(Change_category01,  
#     by = block_loop), sigma.formula = ~guessed_block_rule,  
#     family = BE(mu.link = "cauchit"), data = na.omit(sub_et_qu)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  cauchit
# Mu Coefficients:
#                                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                            -1.717e+00  2.604e-02 -65.940  < 2e-16 ***
# IA_LABELNegative                        8.490e-02  2.023e-02   4.198 2.72e-05 ***
# IA_LABELFailure                         4.342e-02  2.152e-02   2.018   0.0437 *  
# IA_LABELPositive                        6.245e-02  2.073e-02   3.012   0.0026 ** 
# IA_LABELSuccess                         8.886e-02  2.165e-02   4.104 4.09e-05 ***
# Perf_Strivings_c                        7.407e-03  7.183e-03   1.031   0.3025    
# pb(subject_nr)                          9.162e-07  1.689e-06   0.542   0.5876    
# pb(block_loop)                          2.306e-03  2.413e-03   0.956   0.3393    
# pb(Change_category01, by = block_loop)  9.952e-01  1.263e-02  78.794  < 2e-16 ***
# IA_LABELNegative:Perf_Strivings_c      -2.684e-03  8.847e-03  -0.303   0.7616    
# IA_LABELFailure:Perf_Strivings_c       -8.101e-03  1.079e-02  -0.750   0.4530    
# IA_LABELPositive:Perf_Strivings_c      -1.241e-02  1.082e-02  -1.147   0.2513    
# IA_LABELSuccess:Perf_Strivings_c       -3.955e-03  9.673e-03  -0.409   0.6827    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        -1.59626    0.01889  -84.51  < 2e-16 ***
# guessed_block_rule  0.16411    0.02101    7.81 6.28e-15 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms maybe are not accurate. 
# ------------------------------------------------------------------
# No. of observations in the fit:  10145 
# Degrees of Freedom for the fit:  16.12931
#       Residual Deg. of Freedom:  10128.87 
#                       at cycle:  6 
#  
# Global Deviance:     -24669.56 
#             AIC:     -24637.3 
#             SBC:     -24520.77 

# Back-transformed coefficients
print(round(inv.cauchit(bm2.1_dwell_time$mu.coefficients),4))
# (Intercept)                       IA_LABELNegative 
# 0.1679                                 0.5270 
# IA_LABELFailure                       IA_LABELPositive 
# 0.5138                                 0.5199 
# IA_LABELSuccess                       Perf_Strivings_c 
# 0.5282                                 0.5024 
# pb(subject_nr)                         pb(block_loop) 
# 0.5000                                 0.5007 
# pb(Change_category01, by = block_loop)      IA_LABELNegative:Perf_Strivings_c 
# 0.7492                                 0.4991 
# IA_LABELFailure:Perf_Strivings_c      IA_LABELPositive:Perf_Strivings_c 
# 0.4974                                 0.4961 
# IA_LABELSuccess:Perf_Strivings_c 
# 0.4987 

# Model comparison
lrtest(bm2.1_dwell_time, bm1_dwell_time)
#      #Df LogLik      Df  Chisq Pr(>Chisq)
# 1 16.129  12335                          
# 2 11.124  12334 -5.0048 1.9446     0.8568

AIC(bm2.1_dwell_time, bm1_dwell_time)
#                        df       AIC
# bm1_dwell_time   11.12448 -24645.36
# bm2.1_dwell_time 16.12931 -24637.30

# Full Model: Perf_Concerns
bm2.2_dwell_time <- gamlss(
  dwell_time ~ 
    IA_LABEL * Perf_Concerns_c + 
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
# Call:  gamlss(formula = dwell_time ~ IA_LABEL * Perf_Concerns_c +  
#     pb(subject_nr) + pb(block_loop) + pb(Change_category01,  
#     by = block_loop), sigma.formula = ~guessed_block_rule,  
#     family = BE(mu.link = "cauchit"), data = na.omit(sub_et_qu)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  cauchit
# Mu Coefficients:
#                                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                            -1.717e+00  2.463e-02 -69.710  < 2e-16 ***
# IA_LABELNegative                        8.476e-02  2.016e-02   4.205 2.64e-05 ***
# IA_LABELFailure                         4.330e-02  2.129e-02   2.034  0.04200 *  
# IA_LABELPositive                        6.239e-02  2.068e-02   3.017  0.00256 ** 
# IA_LABELSuccess                         8.877e-02  2.162e-02   4.106 4.06e-05 ***
# Perf_Concerns_c                         1.189e-03  3.217e-03   0.370  0.71164    
# pb(subject_nr)                          9.789e-07  1.678e-06   0.583  0.55965    
# pb(block_loop)                          2.269e-03  2.380e-03   0.953  0.34041    
# pb(Change_category01, by = block_loop)  9.954e-01  1.263e-02  78.817  < 2e-16 ***
# IA_LABELNegative:Perf_Concerns_c        7.219e-03  6.479e-03   1.114  0.26521    
# IA_LABELFailure:Perf_Concerns_c         2.723e-03  5.870e-03   0.464  0.64274    
# IA_LABELPositive:Perf_Concerns_c       -8.383e-03  6.509e-03  -1.288  0.19779    
# IA_LABELSuccess:Perf_Concerns_c        -5.753e-03  6.217e-03  -0.925  0.35477    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  logit
# Sigma Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        -1.59608    0.01889 -84.479  < 2e-16 ***
# guessed_block_rule  0.16366    0.02102   7.785 7.62e-15 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms maybe are not accurate. 
# ------------------------------------------------------------------
# No. of observations in the fit:  10145 
# Degrees of Freedom for the fit:  16.123
#       Residual Deg. of Freedom:  10128.88 
#                       at cycle:  6 
#  
# Global Deviance:     -24672.37 
#             AIC:     -24640.12 
#             SBC:     -24523.63  

# Back-transformed coefficients
print(round(inv.cauchit(bm2.2_dwell_time$mu.coefficients),4))
# (Intercept)                       IA_LABELNegative 
# 0.1679                                 0.5269 
# IA_LABELFailure                       IA_LABELPositive 
# 0.5138                                 0.5198 
# IA_LABELSuccess                        Perf_Concerns_c 
# 0.5282                                 0.5004 
# pb(subject_nr)                         pb(block_loop) 
# 0.5000                                 0.5007 
# pb(Change_category01, by = block_loop)       IA_LABELNegative:Perf_Concerns_c 
# 0.7493                                 0.5023 
# IA_LABELFailure:Perf_Concerns_c       IA_LABELPositive:Perf_Concerns_c 
# 0.5009                                 0.4973 
# IA_LABELSuccess:Perf_Concerns_c 
# 0.4982 

# Model comparison
lrtest(bm2.2_dwell_time, bm1_dwell_time)
#      #Df LogLik      Df  Chisq Pr(>Chisq)
# 1 16.123  12336                          
# 2 11.124  12334 -4.9985 4.7534     0.4467

AIC(bm2.2_dwell_time, bm1_dwell_time)
#                        df       AIC
# bm1_dwell_time   11.12448 -24645.36
# bm2.2_dwell_time 16.12300 -24640.12

# Fit a Gamma glmm for response time
#----------------------------------------------

# Create a subset of the data for model fitting
sub_rt_qu <- rt_qu[c("response_time",
                     "subject_nr", "block_loop", "guessed_block_rule", "Change_category",
                     "Change_category_PR_SvsF", "Change_category_PR_SvsF", "Change_category_PR_LvsH", "Change_category_EV", 
                     "Perf_Strivings_c" , "Perf_Concerns_c")]

# Set Neutral as the reference category
rt_qu$Change_category <- as.factor(rt_qu$Change_category)
rt_qu$Change_category <- relevel(rt_qu$Change_category, ref = "Neutral")

# Null model
gm0_response_time <- gamlss(
  response_time ~ 
    pb(subject_nr) +   
    pb(block_loop) ,   
  family = GA(mu.link = "log"),  
  sigma.formula = ~guessed_block_rule,  
  data = na.omit(sub_rt_qu)
)
summary(gm0_response_time)
# Family:  c("GA", "Gamma") 
# 
# Call:  gamlss(formula = response_time ~ pb(subject_nr) + pb(block_loop),  
#               sigma.formula = ~guessed_block_rule, family = GA(mu.link = "log"),      data = na.omit(sub_rt_qu)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  log
# Mu Coefficients:
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     7.428e+00  1.006e-02 738.687  < 2e-16 ***
# pb(subject_nr) -3.641e-06  1.185e-06  -3.071  0.00213 ** 
# pb(block_loop)  3.557e-03  1.489e-03   2.389  0.01692 *  
#   ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  log
# Sigma Coefficients:
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        -0.64101    0.01259 -50.922   <2e-16 ***
# guessed_block_rule  0.03054    0.01384   2.206   0.0274 *  
#   ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
# i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms maybe are not accurate. 
# ------------------------------------------------------------------
# No. of observations in the fit:  14977 
# Degrees of Freedom for the fit:  16.88565
# Residual Deg. of Freedom:  14960.11 
# at cycle:  4 
# 
# Global Deviance:     243621 
#           AIC:     243654.8 
#           SBC:     243783.4 

# Exponantiated coefficients
print(round(exp(gm0_response_time$mu.coefficients),4))
# (Intercept) pb(subject_nr) pb(block_loop) 
# 1682.9115         1.0000         1.0036 

 # Partial Model
gm1_response_time <- gamlss(
  response_time ~ 
    Change_category + 
    pb(subject_nr) +   
    pb(block_loop) ,   
  family = GA(mu.link = "log"),  
  sigma.formula = ~guessed_block_rule,  
  data = na.omit(sub_rt_qu)
)
summary(gm1_response_time)
# Family:  c("GA", "Gamma") 
# 
# Call:  gamlss(formula = response_time ~ Change_category +  
#     pb(subject_nr) + pb(block_loop), sigma.formula = ~guessed_block_rule,  
#     family = GA(mu.link = "log"), data = na.omit(sub_rt_qu)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  log
# Mu Coefficients:
#                           Estimate Std. Error t value Pr(>|t|)    
# (Intercept)              7.437e+00  2.011e-02 369.857  < 2e-16 ***
# Change_categoryNegative  4.062e-02  1.991e-02   2.040 0.041340 *  
# Change_categoryNeutral   1.036e-01  2.760e-02   3.753 0.000175 ***
# Change_categoryPositive  3.913e-02  1.523e-02   2.569 0.010209 *  
# Change_categorySuccess   2.882e-02  1.724e-02   1.672 0.094636 .  
# pb(subject_nr)          -3.669e-06  1.181e-06  -3.107 0.001893 ** 
# pb(block_loop)          -5.712e-03  4.273e-03  -1.337 0.181357    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  log
# Sigma Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        -0.64010    0.01233 -51.933   <2e-16 ***
# guessed_block_rule  0.02986    0.01349   2.214   0.0268 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms maybe are not accurate. 
# ------------------------------------------------------------------
# No. of observations in the fit:  14977 
# Degrees of Freedom for the fit:  16.37435
#       Residual Deg. of Freedom:  14960.63 
#                       at cycle:  4 
#  
# Global Deviance:     243632.8 
#             AIC:     243665.5 
#             SBC:     243790.2 

# Exponantiated coefficientss
print(round(exp(gm1_response_time$mu.coefficients),4))
# (Intercept) Change_categoryNegative  Change_categoryNeutral Change_categoryPositive 
# 1697.8248                  1.0415                  1.1092                  1.0399 
# Change_categorySuccess          pb(subject_nr)          pb(block_loop) 
# 1.0292                  1.0000                  0.9943 

# Model comparison
lrtest(gm1_response_time, gm0_response_time)
#  #Df  LogLik      Df  Chisq Pr(>Chisq)    
# 1 16.374 -121816                              
# 2 16.886 -121811 0.51131 11.725  0.0006168 ***

AIC(gm1_response_time, gm0_response_time)
#                         df      AIC
# gm0_response_time 16.88565 243654.8
# gm1_response_time 16.37435 243665.5

# Full Model: Perf_Strivings
gm2.1_response_time <- gamlss(
  response_time ~ 
    Change_category*Perf_Strivings_c + 
    pb(subject_nr) +   
    pb(block_loop) ,   
  family = GA(mu.link = "log"),  
  sigma.formula = ~guessed_block_rule,  
  data = na.omit(sub_rt_qu)
) 
summary(gm2.1_response_time)
# Family:  c("GA", "Gamma") 
# 
# Call:  gamlss(formula = response_time ~ Change_category *  
#     Perf_Strivings_c + pb(subject_nr) + pb(block_loop),  
#     sigma.formula = ~guessed_block_rule, family = GA(mu.link = "log"),      data = na.omit(sub_rt_qu)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  log
# Mu Coefficients:
#                                            Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                               7.437e+00  3.927e-02 189.407  < 2e-16 ***
# Change_categoryNegative                   4.163e-02  4.176e-02   0.997  0.31882    
# Change_categoryNeutral                    1.052e-01  6.192e-02   1.699  0.08931 .  
# Change_categoryPositive                   3.971e-02  2.394e-02   1.659  0.09722 .  
# Change_categorySuccess                    2.823e-02  2.681e-02   1.053  0.29234    
# Perf_Strivings_c                          4.509e-03  5.477e-03   0.823  0.41040    
# pb(subject_nr)                           -3.466e-06  1.199e-06  -2.890  0.00386 ** 
# pb(block_loop)                           -5.991e-03  1.030e-02  -0.581  0.56098    
# Change_categoryNegative:Perf_Strivings_c -1.430e-02  7.935e-03  -1.802  0.07153 .  
# Change_categoryNeutral:Perf_Strivings_c  -1.289e-02  8.416e-03  -1.532  0.12551    
# Change_categoryPositive:Perf_Strivings_c -4.458e-03  8.882e-03  -0.502  0.61574    
# Change_categorySuccess:Perf_Strivings_c  -1.655e-02  7.962e-03  -2.079  0.03766 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  log
# Sigma Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        -0.64180    0.01380 -46.520   <2e-16 ***
# guessed_block_rule  0.03152    0.01548   2.037   0.0417 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms maybe are not accurate. 
# ------------------------------------------------------------------
# No. of observations in the fit:  14977 
# Degrees of Freedom for the fit:  21.41312
#       Residual Deg. of Freedom:  14955.59 
#                       at cycle:  4 
#  
# Global Deviance:     243621.5 
#             AIC:     243664.3 
#             SBC:     243827.3 

# Exponantiated coefficients
print(round(exp(gm2.1_response_time$mu.coefficients),4))
# (Intercept)                  Change_categoryNegative 
# 1698.3029                                   1.0425 
# Change_categoryNeutral                  Change_categoryPositive 
# 1.1110                                   1.0405 
# Change_categorySuccess                         Perf_Strivings_c 
# 1.0286                                   1.0045 
# pb(subject_nr)                           pb(block_loop) 
# 1.0000                                   0.9940 
# Change_categoryNegative:Perf_Strivings_c  Change_categoryNeutral:Perf_Strivings_c 
# 0.9858                                   0.9872 
# Change_categoryPositive:Perf_Strivings_c  Change_categorySuccess:Perf_Strivings_c 
# 0.9956                                   0.9836

# Model comparison
lrtest(gm2.1_response_time, gm1_response_time)
#      #Df  LogLik      Df  Chisq Pr(>Chisq)  
# 1 21.413 -121811                            
# 2 16.374 -121816 -5.0388 11.306    0.04564 *

AIC(gm2.1_response_time, gm1_response_time)
#                          df      AIC
# gm2.1_response_time 21.41312 243664.3
# gm1_response_time  16.37435 243665.5

# Full Model: Perf_Concerns
gm2.2_response_time <- gamlss(
  response_time ~ 
    Change_category*Perf_Concerns_c +
    pb(subject_nr) +   
    pb(block_loop) ,   
  family = GA(mu.link = "log"),  
  sigma.formula = ~guessed_block_rule,  
  data = na.omit(sub_rt_qu)
) 
summary(gm2.2_response_time)
# Family:  c("GA", "Gamma") 
# 
# Call:  gamlss(formula = response_time ~ Change_category *  
#     Perf_Concerns_c + pb(subject_nr) + pb(block_loop),  
#     sigma.formula = ~guessed_block_rule, family = GA(mu.link = "log"),      data = na.omit(sub_rt_qu)) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
# Mu link function:  log
# Mu Coefficients:
#                                           Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                              7.439e+00  1.675e-02 444.193  < 2e-16 ***
# Change_categoryNegative                  4.086e-02  1.995e-02   2.048 0.040600 *  
# Change_categoryNeutral                   1.036e-01  2.757e-02   3.759 0.000171 ***
# Change_categoryPositive                  3.894e-02  1.388e-02   2.805 0.005040 ** 
# Change_categorySuccess                   2.867e-02  1.313e-02   2.183 0.029048 *  
# Perf_Concerns_c                          1.030e-02  1.852e-03   5.558 2.77e-08 ***
# pb(subject_nr)                          -4.320e-06  1.182e-06  -3.656 0.000257 ***
# pb(block_loop)                          -5.753e-03  4.137e-03  -1.391 0.164363    
# Change_categoryNegative:Perf_Concerns_c -2.647e-03  1.729e-03  -1.531 0.125885    
# Change_categoryNeutral:Perf_Concerns_c   4.331e-03  2.569e-03   1.686 0.091859 .  
# Change_categoryPositive:Perf_Concerns_c  3.479e-03  2.181e-03   1.595 0.110772    
# Change_categorySuccess:Perf_Concerns_c   1.589e-03  1.025e-03   1.550 0.121155    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# Sigma link function:  log
# Sigma Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        -0.64326    0.01203 -53.464   <2e-16 ***
# guessed_block_rule  0.03233    0.01307   2.474   0.0134 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
# NOTE: Additive smoothing terms exist in the formulas: 
#  i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms maybe are not accurate. 
# ------------------------------------------------------------------
# No. of observations in the fit:  14977 
# Degrees of Freedom for the fit:  21.34703
#       Residual Deg. of Freedom:  14955.65 
#                       at cycle:  4 
#  
# Global Deviance:     243595.1 
#             AIC:     243637.8 
#             SBC:     243800.4 

# Exponantiated coefficients
print(round(exp(gm2.2_response_time$mu.coefficients),4))
# (Intercept)                 Change_categoryNegative 
# 1701.4048                                  1.0417 
# Change_categoryNeutral                 Change_categoryPositive 
# 1.1092                                  1.0397 
# Change_categorySuccess                         Perf_Concerns_c 
# 1.0291                                  1.0103 
# pb(subject_nr)                          pb(block_loop) 
# 1.0000                                  0.9943 
# Change_categoryNegative:Perf_Concerns_c  Change_categoryNeutral:Perf_Concerns_c 
# 0.9974                                  1.0043 
# Change_categoryPositive:Perf_Concerns_c  Change_categorySuccess:Perf_Concerns_c 
# 1.0035                                  1.0016 

# Model comparison
lrtest(gm2.2_response_time, gm1_response_time)
#      #Df  LogLik      Df  Chisq Pr(>Chisq)    
# 1 21.347 -121798                              
# 2 16.374 -121816 -4.9727 37.655  4.425e-07 ***

AIC(gm2.2_response_time, gm1_response_time)
#                          df      AIC
# gm2.2_response_time 21.34703 243637.8
# gm1_response_time  16.37435 243665.5

# Simulate predicted values
preds_response_time_PS <- ggpredict(gm2.1_response_time, terms = c("Perf_Strivings_c", "Change_category"))
preds_response_time_PC <- ggpredict(gm2.2_response_time, terms = c("Perf_Concerns_c", "Change_category"))

# Plot the retained interaction
rt_PS <- ggplot(preds_response_time_PS, aes(x = x, y = predicted, color = group)) +
  geom_line(size = 1) +                                # Line only, no CI ribbon
  labs(
    x = "Perfectionistic Strivings",
    y = "Response Time",
    color = "Semantic Category"
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

rt_PC <- ggplot(preds_response_time_PC, aes(x = x, y = predicted, color = group)) +
  geom_line(size = 1) +                                # Line only, no CI ribbon
  labs(
    x = "Perfectionistic Concerns",
    y = "Response Time",
    color = "Semantic Category"
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
grid.arrange(rt_PS, rt_PC,
             ncol = 2, nrow = 1,
             top = textGrob("Predicted Response Time",
                            gp = gpar(fontsize = 16, fontface = "bold")
                            )
             )
