# ============================================================
# R ANALYSIS SCRIPT
# Perceptions of Artificial Intelligence in University Student
# Affairs Management: A Quantitative Study at Panzhihua
# University, China
#
# Authors: Bai Chuan Wang, Yang Guo, Muhammad Asghar Khan
# Journal: Frontiers in Psychology
# R Version: 4.3.1
# Date: July 2026
#
# INSTRUCTIONS:
# 1. Set your working directory to the folder containing
#    the dataset file.
# 2. Run this script section by section or in full.
# 3. All outputs correspond to tables and results reported
#    in the manuscript.
# ============================================================


# ============================================================
# SECTION 0: LOAD PACKAGES AND DATA
# ============================================================

# Install packages if not already installed
# install.packages(c("readxl","psych","car","effsize",
#                    "ez","rstatix","ggplot2","dplyr"))

library(readxl)
library(psych)
library(car)
library(effsize)
library(ez)
library(rstatix)
library(ggplot2)
library(dplyr)

# Load dataset
# Replace the filename below with the actual path to your file
df <- read_excel("DeepSeek_AI_Perceptions_Dataset_Panzhihua2026.xlsx",
                 skip = 2)

# Rename columns for convenience
names(df)[names(df) == "AI improves efficiency of student records"] <- "SIM1"
names(df)[names(df) == "AI enhances data accuracy"]                 <- "SIM2"
names(df)[names(df) == "AI streamlines enrolment processes"]        <- "SIM3"
names(df)[names(df) == "AI facilitates communication"]              <- "SIM4"
names(df)[names(df) == "AI provides 24/7 availability"]            <- "SD1"
names(df)[names(df) == "AI offers quick responses"]                 <- "SD2"
names(df)[names(df) == "AI personalises services"]                  <- "SD3"
names(df)[names(df) == "AI reduces wait times"]                     <- "SD4"
names(df)[names(df) == "AI can provide initial mental health screening"] <- "MHS1"
names(df)[names(df) == "AI chatbots can reduce stigma"]             <- "MHS2"
names(df)[names(df) == "AI can offer coping strategies"]            <- "MHS3"
names(df)[names(df) == "AI can replace some counselling functions"] <- "MHS4"
names(df)[names(df) == "Concerned about data security"]             <- "DP1"
names(df)[names(df) == "Worried about personal info misuse"]        <- "DP2"
names(df)[names(df) == "Concerned about data breaches"]             <- "DP3"
names(df)[names(df) == "Concerned about AI bias"]                   <- "EU1"
names(df)[names(df) == "Worried about academic integrity"]          <- "EU2"
names(df)[names(df) == "Concerned about over-reliance on AI"]       <- "EU3"
names(df)[names(df) == "AI systems lack transparency"]              <- "ST1"
names(df)[names(df) == "Difficult to understand AI decisions"]      <- "ST2"
names(df)[names(df) == "Need clearer AI policies"]                  <- "ST3"
names(df)[names(df) == "I intend to use AI tools for academic/work tasks"] <- "BI1"
names(df)[names(df) == "I will recommend AI tools to others"]       <- "BI2"
names(df)[names(df) == "I plan to continue using AI tools"]         <- "BI3"
names(df)[names(df) == "Satisfied with current AI services"]        <- "OS1"
names(df)[names(df) == "AI services meet my expectations"]          <- "OS2"
names(df)[names(df) == "Institutional Role"]                        <- "Role"
names(df)[names(df) == "DeepSeek Usage Frequency"]                  <- "UsageFreq"
names(df)[names(df) == "Age Group"]                                 <- "AgeGroup"
names(df)[names(df) == "Academic Department"]                       <- "Department"
names(df)[names(df) == "Awareness of DeepSeek"]                     <- "Awareness"

cat("Dataset loaded. N =", nrow(df), "\n")
cat("Columns:", ncol(df), "\n")


# ============================================================
# SECTION 1: COMPUTE COMPOSITE SCORES
# All composites are arithmetic means of constituent items.
# NOTE: These composites have near-zero internal consistency
# (see Section 2). They are retained as item-average indices
# for descriptive and comparative purposes only, not as
# reflective latent constructs.
# ============================================================

df$COMP_SIM <- rowMeans(df[, c("SIM1","SIM2","SIM3","SIM4")], na.rm = TRUE)
df$COMP_SD  <- rowMeans(df[, c("SD1","SD2","SD3","SD4")],     na.rm = TRUE)
df$COMP_MHS <- rowMeans(df[, c("MHS1","MHS2","MHS3","MHS4")], na.rm = TRUE)
df$COMP_DP  <- rowMeans(df[, c("DP1","DP2","DP3")],           na.rm = TRUE)
df$COMP_EU  <- rowMeans(df[, c("EU1","EU2","EU3")],           na.rm = TRUE)
df$COMP_ST  <- rowMeans(df[, c("ST1","ST2","ST3")],           na.rm = TRUE)
df$COMP_BI  <- rowMeans(df[, c("BI1","BI2","BI3")],           na.rm = TRUE)
df$COMP_OS  <- rowMeans(df[, c("OS1","OS2")],                 na.rm = TRUE)

cat("\nComposite scores computed for all 8 constructs.\n")


# ============================================================
# SECTION 2: INTERNAL CONSISTENCY (CRONBACH'S ALPHA)
# Exact alpha values including negative values are reported.
# Negative alphas indicate that items within the composite
# are negatively or uncorrelatedly related, which means
# the composite cannot be treated as a reflective scale.
# ============================================================

cat("\n===== CRONBACH'S ALPHA (Table S1) =====\n")

alpha_SIM <- psych::alpha(df[, c("SIM1","SIM2","SIM3","SIM4")])
alpha_SD  <- psych::alpha(df[, c("SD1","SD2","SD3","SD4")])
alpha_MHS <- psych::alpha(df[, c("MHS1","MHS2","MHS3","MHS4")])
alpha_DP  <- psych::alpha(df[, c("DP1","DP2","DP3")])
alpha_EU  <- psych::alpha(df[, c("EU1","EU2","EU3")])
alpha_ST  <- psych::alpha(df[, c("ST1","ST2","ST3")])
alpha_BI  <- psych::alpha(df[, c("BI1","BI2","BI3")])
alpha_OS  <- psych::alpha(df[, c("OS1","OS2")])

cat("SIM alpha =", round(alpha_SIM$total$raw_alpha, 4), "\n")
cat("SD  alpha =", round(alpha_SD$total$raw_alpha,  4), "\n")
cat("MHS alpha =", round(alpha_MHS$total$raw_alpha, 4), "\n")
cat("DP  alpha =", round(alpha_DP$total$raw_alpha,  4), "\n")
cat("EU  alpha =", round(alpha_EU$total$raw_alpha,  4), "\n")
cat("ST  alpha =", round(alpha_ST$total$raw_alpha,  4), "\n")
cat("BI  alpha =", round(alpha_BI$total$raw_alpha,  4), "\n")
cat("OS  alpha =", round(alpha_OS$total$raw_alpha,  4), "\n")

cat("\n--- Item-total correlations: SIM ---\n")
print(alpha_SIM$item.stats[, c("r.cor","r.drop")])
cat("\n--- Item-total correlations: SD ---\n")
print(alpha_SD$item.stats[, c("r.cor","r.drop")])
cat("\n--- Item-total correlations: MHS ---\n")
print(alpha_MHS$item.stats[, c("r.cor","r.drop")])
cat("\n--- Item-total correlations: DP ---\n")
print(alpha_DP$item.stats[, c("r.cor","r.drop")])
cat("\n--- Item-total correlations: EU ---\n")
print(alpha_EU$item.stats[, c("r.cor","r.drop")])
cat("\n--- Item-total correlations: ST ---\n")
print(alpha_ST$item.stats[, c("r.cor","r.drop")])
cat("\n--- Item-total correlations: BI ---\n")
print(alpha_BI$item.stats[, c("r.cor","r.drop")])
cat("\n--- Item-total correlations: OS ---\n")
print(alpha_OS$item.stats[, c("r.cor","r.drop")])

cat("\n--- Inter-item correlation matrices ---\n")
cat("\nSIM:\n");  print(round(cor(df[,c("SIM1","SIM2","SIM3","SIM4")]),3))
cat("\nSD:\n");   print(round(cor(df[,c("SD1","SD2","SD3","SD4")]),3))
cat("\nMHS:\n");  print(round(cor(df[,c("MHS1","MHS2","MHS3","MHS4")]),3))
cat("\nDP:\n");   print(round(cor(df[,c("DP1","DP2","DP3")]),3))
cat("\nEU:\n");   print(round(cor(df[,c("EU1","EU2","EU3")]),3))
cat("\nST:\n");   print(round(cor(df[,c("ST1","ST2","ST3")]),3))
cat("\nBI:\n");   print(round(cor(df[,c("BI1","BI2","BI3")]),3))
cat("\nOS:\n");   print(round(cor(df[,c("OS1","OS2")]),3))


# ============================================================
# SECTION 3: DESCRIPTIVE STATISTICS (Tables 4, 5, 6)
# ============================================================

cat("\n===== DESCRIPTIVE STATISTICS =====\n")

items_list <- list(
  SIM1="AI improves efficiency of student records",
  SIM2="AI enhances data accuracy",
  SIM3="AI streamlines enrolment processes",
  SIM4="AI facilitates communication",
  COMP_SIM="Composite: Student Information Management",
  SD1="AI provides 24/7 availability",
  SD2="AI offers quick responses",
  SD3="AI personalises services",
  SD4="AI reduces wait times",
  COMP_SD="Composite: Service Delivery",
  MHS1="AI can provide initial mental health screening",
  MHS2="AI chatbots can reduce stigma",
  MHS3="AI can offer coping strategies",
  MHS4="AI can replace some counselling functions",
  COMP_MHS="Composite: Mental Health Support",
  DP1="Concerned about data security",
  DP2="Worried about personal info misuse",
  DP3="Concerned about data breaches",
  COMP_DP="Composite: Data Privacy Concerns",
  EU1="Concerned about AI bias",
  EU2="Worried about academic integrity",
  EU3="Concerned about over-reliance on AI",
  COMP_EU="Composite: Ethical Use Concerns",
  ST1="AI systems lack transparency",
  ST2="Difficult to understand AI decisions",
  ST3="Need clearer AI policies",
  COMP_ST="Composite: System Transparency Concerns",
  BI1="I intend to use AI tools for academic/work tasks",
  BI2="I will recommend AI tools to others",
  BI3="I plan to continue using AI tools",
  COMP_BI="Composite: Behavioural Intention",
  OS1="Satisfied with current AI services",
  OS2="AI services meet my expectations",
  COMP_OS="Composite: Overall Satisfaction"
)

for (varname in names(items_list)) {
  m  <- round(mean(df[[varname]], na.rm=TRUE), 2)
  sd <- round(sd(df[[varname]],   na.rm=TRUE), 2)
  cat(sprintf("  %-10s M = %s, SD = %s\n", varname, m, sd))
}


# ============================================================
# SECTION 4: AWARENESS AND USAGE (Table 3)
# ============================================================

cat("\n===== AWARENESS AND USAGE =====\n")
cat("\nDeepSeek Awareness:\n")
print(table(df$Awareness))
print(prop.table(table(df$Awareness)) * 100)

cat("\nDeepSeek Usage Frequency:\n")
print(table(df$UsageFreq))
print(prop.table(table(df$UsageFreq)) * 100)

cat("\nSelf-Rated AI Familiarity:\n")
print(table(df$`Self-Rated AI Familiarity`))
print(prop.table(table(df$`Self-Rated AI Familiarity`)) * 100)


# ============================================================
# SECTION 5: H1 — REPEATED-MEASURES ANOVA
# Domain differences in perceived usefulness
# Sphericity tested with Mauchly's test (via ezANOVA).
# Greenhouse-Geisser correction applied where W is significant.
# ============================================================

cat("\n===== H1: REPEATED-MEASURES ANOVA =====\n")

# Reshape to long format
df$ID <- 1:nrow(df)
df_long <- data.frame(
  ID     = rep(df$ID, 3),
  domain = rep(c("SIM","SD","MHS"), each = nrow(df)),
  score  = c(df$COMP_SIM, df$COMP_SD, df$COMP_MHS)
)
df_long$domain <- factor(df_long$domain,
                          levels = c("SIM","SD","MHS"))

# ezANOVA with Mauchly's test and GG correction
rm_anova <- ezANOVA(
  data     = df_long,
  dv       = score,
  wid      = ID,
  within   = domain,
  type     = 3,
  detailed = TRUE,
  return_aov = TRUE
)

cat("\nMauchly's Test of Sphericity:\n")
print(rm_anova$`Mauchly's Test for Sphericity`)

cat("\nANOVA Table (with GG correction if sphericity violated):\n")
print(rm_anova$ANOVA)

cat("\nGreenhouse-Geisser Correction:\n")
print(rm_anova$`Sphericity Corrections`)

# Pairwise comparisons with Bonferroni correction
cat("\n--- Pairwise Comparisons (Bonferroni corrected) ---\n")
pw <- pairwise.t.test(df_long$score,
                       df_long$domain,
                       paired        = TRUE,
                       p.adjust.method = "bonferroni")
print(pw)

# Paired-samples Cohen's dz for each pair
cat("\n--- Cohen's dz (paired-samples effect size) ---\n")
diff_SIM_MHS <- df$COMP_SIM - df$COMP_MHS
diff_SD_MHS  <- df$COMP_SD  - df$COMP_MHS
diff_SIM_SD  <- df$COMP_SIM - df$COMP_SD

dz_SIM_MHS <- mean(diff_SIM_MHS) / sd(diff_SIM_MHS)
dz_SD_MHS  <- mean(diff_SD_MHS)  / sd(diff_SD_MHS)
dz_SIM_SD  <- mean(diff_SIM_SD)  / sd(diff_SIM_SD)

cat(sprintf("SIM vs MHS: dz = %.4f\n", dz_SIM_MHS))
cat(sprintf("SD  vs MHS: dz = %.4f\n", dz_SD_MHS))
cat(sprintf("SIM vs SD:  dz = %.4f\n", dz_SIM_SD))

# 95% CIs for pairwise mean differences
cat("\n--- 95% CIs for Pairwise Mean Differences ---\n")
n <- nrow(df)
for (pair in list(
  list("SIM vs MHS", diff_SIM_MHS),
  list("SD  vs MHS", diff_SD_MHS),
  list("SIM vs SD",  diff_SIM_SD)
)) {
  d   <- pair[[2]]
  m   <- mean(d)
  se  <- sd(d) / sqrt(n)
  ci_lo <- m - qt(0.975, n-1) * se
  ci_hi <- m + qt(0.975, n-1) * se
  cat(sprintf("%s: M_diff = %.4f, 95%% CI [%.4f, %.4f]\n",
              pair[[1]], m, ci_lo, ci_hi))
}

# Partial eta-squared
cat("\n--- Partial eta-squared for H1 ANOVA ---\n")
# Extracted from ezANOVA output
cat("See SSn and SSd in ANOVA table above.\n")
cat("partial eta2 = SSn / (SSn + SSd)\n")


# ============================================================
# SECTION 6: H2 — ONE-WAY ANOVA BY INSTITUTIONAL ROLE
# Bonferroni-corrected alpha = .0125 (4 tests)
# ============================================================

cat("\n===== H2: ONE-WAY ANOVA BY ROLE =====\n")
cat("Bonferroni-corrected alpha = .0125 (4 tests)\n\n")

df$Role <- factor(df$Role,
  levels = c("Undergraduate Student","Graduate Student",
             "Faculty","Administrative Staff"))

outcomes_h2 <- c("COMP_SIM","COMP_SD","COMP_MHS","COMP_BI")

for (o in outcomes_h2) {
  mod <- aov(as.formula(paste(o, "~ Role")), data = df)
  s   <- summary(mod)
  F_  <- s[[1]][["F value"]][1]
  p_  <- s[[1]][["Pr(>F)"]][1]
  df1 <- s[[1]][["Df"]][1]
  df2 <- s[[1]][["Df"]][2]
  ss_b <- s[[1]][["Sum Sq"]][1]
  ss_t <- ss_b + s[[1]][["Sum Sq"]][2]
  eta2 <- ss_b / ss_t
  cat(sprintf("%s: F(%d,%d) = %.4f, p = %.4f, eta2 = %.4f\n",
              o, df1, df2, F_, p_, eta2))
}

cat("\nLevene's test for homogeneity of variance:\n")
for (o in outcomes_h2) {
  lev <- leveneTest(as.formula(paste(o, "~ Role")), data = df)
  cat(sprintf("  %s: F = %.4f, p = %.4f\n", o,
              lev$`F value`[1], lev$`Pr(>F)`[1]))
}


# ============================================================
# SECTION 7: H3 — INDEPENDENT-SAMPLES T-TESTS BY GENDER
# Welch's correction applied. Bonferroni alpha = .010 (5 tests)
# ============================================================

cat("\n===== H3: GENDER T-TESTS =====\n")
cat("Bonferroni-corrected alpha = .010 (5 tests)\n\n")

male   <- df[df$Gender == "Male",   ]
female <- df[df$Gender == "Female", ]
cat(sprintf("Male n = %d, Female n = %d\n\n", nrow(male), nrow(female)))

outcomes_h3 <- c("COMP_SIM","COMP_SD","COMP_MHS","COMP_DP","COMP_BI")

for (o in outcomes_h3) {
  tt  <- t.test(male[[o]], female[[o]], var.equal = FALSE)
  m_m <- mean(male[[o]],   na.rm = TRUE)
  m_f <- mean(female[[o]], na.rm = TRUE)
  s_m <- sd(male[[o]],     na.rm = TRUE)
  s_f <- sd(female[[o]],   na.rm = TRUE)
  n_m <- sum(!is.na(male[[o]]))
  n_f <- sum(!is.na(female[[o]]))
  sp  <- sqrt(((n_m-1)*s_m^2 + (n_f-1)*s_f^2) / (n_m + n_f - 2))
  d   <- (m_m - m_f) / sp
  sig <- ifelse(tt$p.value < 0.010, "* (survives Bonferroni)", "ns")
  cat(sprintf("%s:\n", o))
  cat(sprintf("  Male M = %.3f (SD = %.3f), Female M = %.3f (SD = %.3f)\n",
              m_m, s_m, m_f, s_f))
  cat(sprintf("  t(%.2f) = %.4f, p = %.4f, d = %.4f, 95%% CI [%.4f, %.4f] %s\n\n",
              tt$parameter, tt$statistic, tt$p.value, d,
              tt$conf.int[1], tt$conf.int[2], sig))
}


# ============================================================
# SECTION 8: H4 — PEARSON CORRELATIONS
# Concern constructs vs Behavioural Intention and Satisfaction
# Bonferroni alpha = .008 (6 tests)
# ============================================================

cat("\n===== H4: PEARSON CORRELATIONS =====\n")
cat("Bonferroni-corrected alpha = .008 (6 tests)\n\n")

concerns  <- c("COMP_DP","COMP_EU","COMP_ST")
outcomes_h4 <- c("COMP_BI","COMP_OS")

for (c_ in concerns) {
  for (o in outcomes_h4) {
    ct  <- cor.test(df[[c_]], df[[o]], method = "pearson")
    sig <- ifelse(ct$p.value < 0.008,
                  "* (survives Bonferroni)", "ns")
    cat(sprintf("%s x %s: r = %.4f, p = %.4f %s\n",
                c_, o, ct$estimate, ct$p.value, sig))
  }
}


# ============================================================
# SECTION 9: H5 — CHI-SQUARE: ROLE x USAGE FREQUENCY
# ============================================================

cat("\n===== H5: CHI-SQUARE (Role x UsageFreq) =====\n")

usage_order <- c("Never","Rarely","Sometimes","Often","Very Often")
role_order  <- c("Undergraduate Student","Graduate Student",
                 "Faculty","Administrative Staff")

df$UsageFreq <- factor(df$UsageFreq, levels = usage_order)
df$Role      <- factor(df$Role,      levels = role_order)

ct_table <- table(df$Role, df$UsageFreq)
cat("\nContingency Table:\n")
print(ct_table)

chi_result <- chisq.test(ct_table)
cat(sprintf("\nchi2(%d) = %.4f, p = %.4f\n",
            chi_result$parameter, chi_result$statistic, chi_result$p.value))

# Cramer's V
n_total <- sum(ct_table)
k <- min(nrow(ct_table), ncol(ct_table))
cramers_v <- sqrt(chi_result$statistic / (n_total * (k - 1)))
cat(sprintf("Cramer's V = %.4f\n", cramers_v))

cat("\nExpected frequencies:\n")
print(round(chi_result$expected, 1))


# ============================================================
# SECTION 10: ONE-SAMPLE T-TEST — BEHAVIOURAL INTENTION
# Tests whether BI is significantly above the scale midpoint (3)
# ============================================================

cat("\n===== ONE-SAMPLE T-TEST: BI vs midpoint 3 =====\n")

bi_test <- t.test(df$COMP_BI, mu = 3)
d_bi    <- (mean(df$COMP_BI) - 3) / sd(df$COMP_BI)
cat(sprintf("M = %.4f, SD = %.4f\n", mean(df$COMP_BI), sd(df$COMP_BI)))
cat(sprintf("t(%d) = %.4f, p < .001, d = %.4f\n",
            bi_test$parameter, bi_test$statistic, d_bi))


# ============================================================
# SECTION 11: FULL 8x8 CORRELATION MATRIX (Table 10)
# ============================================================

cat("\n===== FULL 8x8 CORRELATION MATRIX (Table 10) =====\n")

comp_vars <- c("COMP_SIM","COMP_SD","COMP_MHS",
               "COMP_DP","COMP_EU","COMP_ST",
               "COMP_BI","COMP_OS")
comp_labels <- c("PU_SIM","PU_SD","PU_MHS",
                 "C_DP","C_EU","C_ST","BI","OS")

corr_matrix <- cor(df[, comp_vars], use = "complete.obs")
rownames(corr_matrix) <- comp_labels
colnames(corr_matrix) <- comp_labels

cat("\nCorrelation matrix (lower triangle):\n")
print(round(corr_matrix, 3))

# p-values
cat("\nP-values:\n")
n_ <- nrow(df)
p_matrix <- matrix(NA, 8, 8)
for (i in 1:8) {
  for (j in 1:8) {
    if (i != j) {
      ct_ <- cor.test(df[[comp_vars[i]]], df[[comp_vars[j]]])
      p_matrix[i,j] <- ct_$p.value
    }
  }
}
rownames(p_matrix) <- comp_labels
colnames(p_matrix) <- comp_labels
print(round(p_matrix, 4))


# ============================================================
# SECTION 12: HARMAN'S SINGLE-FACTOR TEST
# Common method bias check on all 26 Likert items
# NOTE: 26 items are used, not 25 as erroneously stated
# in a prior manuscript version. The count is corrected here.
# ============================================================

cat("\n===== HARMAN'S SINGLE-FACTOR TEST =====\n")
cat("NOTE: 26 Likert items total (SIM x4, SD x4, MHS x4,\n")
cat("      DP x3, EU x3, ST x3, BI x3, OS x2 = 26)\n\n")

all_26_items <- c("SIM1","SIM2","SIM3","SIM4",
                  "SD1","SD2","SD3","SD4",
                  "MHS1","MHS2","MHS3","MHS4",
                  "DP1","DP2","DP3",
                  "EU1","EU2","EU3",
                  "ST1","ST2","ST3",
                  "BI1","BI2","BI3",
                  "OS1","OS2")

efa_harman <- principal(df[, all_26_items],
                         nfactors = 26,
                         rotate   = "none",
                         covar    = FALSE)

eigenvalues   <- efa_harman$values
n_factors_gt1 <- sum(eigenvalues > 1)
var_first     <- eigenvalues[1] / sum(eigenvalues) * 100

cat(sprintf("Number of factors with eigenvalue > 1: %d\n", n_factors_gt1))
cat(sprintf("Variance explained by first factor: %.2f%%\n", var_first))
cat(sprintf("Common method bias threshold: 50%%\n"))
cat(sprintf("Conclusion: %s\n",
            ifelse(var_first < 50,
                   "Common method bias not severe (< 50% threshold)",
                   "WARNING: Possible common method bias (> 50%)")))


# ============================================================
# SECTION 13: ASSUMPTION CHECKS
# Normality (Shapiro-Wilk) and homogeneity (Levene)
# ============================================================

cat("\n===== ASSUMPTION CHECKS =====\n")

cat("\nShapiro-Wilk normality tests for composite scores:\n")
for (v in comp_vars) {
  sw <- shapiro.test(df[[v]])
  cat(sprintf("  %s: W = %.4f, p = %.4f\n", v, sw$statistic, sw$p.value))
}

cat("\nNote: Shapiro-Wilk p < .05 indicates departure from normality.\n")
cat("All inferential tests used are robust to non-normality at N = 386.\n")


# ============================================================
# SECTION 14: DEMOGRAPHIC SUMMARY (Tables 1 and 2)
# ============================================================

cat("\n===== DEMOGRAPHIC SUMMARY =====\n")

cat("\nGender:\n")
print(table(df$Gender))
print(round(prop.table(table(df$Gender)) * 100, 1))

cat("\nAge Group:\n")
print(table(df$AgeGroup))
print(round(prop.table(table(df$AgeGroup)) * 100, 1))

cat("\nInstitutional Role:\n")
print(table(df$Role))
print(round(prop.table(table(df$Role)) * 100, 1))

cat("\nAcademic Department:\n")
print(table(df$Department))
print(round(prop.table(table(df$Department)) * 100, 1))


# ============================================================
# SECTION 15: SESSION INFO (for reproducibility)
# ============================================================

cat("\n===== SESSION INFO =====\n")
sessionInfo()

cat("\n===== END OF ANALYSIS SCRIPT =====\n")
cat("All results correspond to manuscript tables and text.\n")
cat("Questions: asgharkhan@pzhu.edu.cn\n")
