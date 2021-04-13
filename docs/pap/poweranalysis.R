# Power Analysis
rm(list = ls())
source(here::here("src/lib/functions.R"))

#H1 & H2
N <- 100
assignment_prob <- 0.25 # only 2 in 8 conditions is have women/migration background + compromise condition. 
treatment_effect <- 0.1

population <- declare_population(N = N, u = rnorm(N))
potential_outcomes <- declare_potential_outcomes(Y_Z_0 = u, Y_Z_1 = Y_Z_0 + treatment_effect)
estimand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))
assignment <- declare_assignment(prob = assignment_prob)
reveal_Y <- declare_reveal()
estimator <- declare_estimator(Y ~ Z, estimand = estimand, term="Z", model=lm)
two_arm_design <- population + potential_outcomes + estimand + 
  assignment + reveal_Y + estimator
designs <- redesign(design=two_arm_design, N=seq(500,2500,250), treatment_effect=seq(0.1,0.6,0.1))

alpha <- .05
my_diagnosands <- declare_diagnosands(power.onetailed=mean(p.value<alpha), keep_defaults=TRUE)
diagnosis1 <- diagnose_designs(designs,diagnosands=my_diagnosands)
diagnosis <- diagnosis1$diagnosands_df
diagnosis1  <- diagnosis %>% 
  select(N, treatment_effect, power.onetailed) %>%
  mutate(id = "Power-Analysis for H1 and H2")

#H3
N <- 100
assignment_prob <- 0.125 # only 1 in 8 conditions is have women + migration background + compromise condition. 
treatment_effect <- 0.1

population <- declare_population(N = N, u = rnorm(N))
potential_outcomes <- declare_potential_outcomes(Y_Z_0 = u, Y_Z_1 = Y_Z_0 + treatment_effect)
estimand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))
assignment <- declare_assignment(prob = assignment_prob)
reveal_Y <- declare_reveal()
estimator <- declare_estimator(Y ~ Z, estimand = estimand, term="Z", model=lm)
two_arm_design <- population + potential_outcomes + estimand + 
  assignment + reveal_Y + estimator
designs <- redesign(design=two_arm_design, N=seq(500,2500,250), treatment_effect=seq(0.1,0.6,0.1))

alpha <- .05
my_diagnosands <- declare_diagnosands(power.onetailed=mean(p.value<alpha), keep_defaults=TRUE)
diagnosis2 <- diagnose_designs(designs,diagnosands=my_diagnosands)
diagnosis <- diagnosis2$diagnosands_df
diagnosis2  <- diagnosis %>% 
  select(N, treatment_effect, power.onetailed) %>%
  mutate(id = "Power-Analysis for H3")

diagnosis1 %>% add_case(diagnosis2) %>%
  ggplot(aes(x=N, y=power.onetailed, group=factor(treatment_effect), colour=factor(treatment_effect))) +
  geom_line() +
  geom_hline(yintercept=0.95, linetype="dashed") +
  geom_hline(yintercept=0.8, linetype="dashed", color = "grey") +
  geom_vline(xintercept = 2000,linetype="dotdash", color = "darkgrey") +
  ylim(c(0,1)) +
  labs(x = "Number of Respondents", y = "Power: One-Tailed",
       subtitle = "Various Levels of Treatment Effect") +
  scale_y_continuous(labels=percent) +
  theme_minimal() +
  facet_grid(.~id) +
  scale_color_viridis_d() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank()) +
  guides(color=guide_legend(nrow=1,byrow=TRUE))
ggsave(here("docs/pap/power_analysis.eps"), width=8, height=5, dpi=900)

#Pooling data for .1 effect two-tailed
#H1 & H2
N <- 100
assignment_prob <- 0.25 # only 2 in 8 conditions is have women/migration background + compromise condition. 
treatment_effect <- 0.1

population <- declare_population(N = N, u = rnorm(N))
potential_outcomes <- declare_potential_outcomes(Y_Z_0 = u, Y_Z_1 = Y_Z_0 + treatment_effect)
estimand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))
assignment <- declare_assignment(prob = assignment_prob)
reveal_Y <- declare_reveal()
estimator <- declare_estimator(Y ~ Z, estimand = estimand, term="Z", model=lm)
two_arm_design <- population + potential_outcomes + estimand + 
  assignment + reveal_Y + estimator
designs <- redesign(design=two_arm_design, N=seq(2000,8000,500), treatment_effect=.1)

alpha <- .05
my_diagnosands <- declare_diagnosands(power.twotailed=mean(p.value<alpha), keep_defaults=TRUE)
diagnosis3 <- diagnose_designs(designs,diagnosands=my_diagnosands)
diagnosis <- diagnosis3$diagnosands_df
diagnosis3  <- diagnosis %>% 
  select(N, treatment_effect, power.twotailed) %>%
  mutate(id = "Power-Analysis for H1 and H2")

#H3
N <- 100
assignment_prob <- 0.125 # only 2 in 8 conditions is have women/migration background + compromise condition. 
treatment_effect <- 0.1

population <- declare_population(N = N, u = rnorm(N))
potential_outcomes <- declare_potential_outcomes(Y_Z_0 = u, Y_Z_1 = Y_Z_0 + treatment_effect)
estimand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))
assignment <- declare_assignment(prob = assignment_prob)
reveal_Y <- declare_reveal()
estimator <- declare_estimator(Y ~ Z, estimand = estimand, term="Z", model=lm)
two_arm_design <- population + potential_outcomes + estimand + 
  assignment + reveal_Y + estimator
designs <- redesign(design=two_arm_design, N=seq(2000,8000,500), treatment_effect=.1)

alpha <- .05
my_diagnosands <- declare_diagnosands(power.twotailed=mean(p.value<alpha), keep_defaults=TRUE)
diagnosis4 <- diagnose_designs(designs,diagnosands=my_diagnosands)
diagnosis <- diagnosis4$diagnosands_df
diagnosis4  <- diagnosis %>% 
  select(N, treatment_effect, power.twotailed) %>%
  mutate(id = "Power-Analysis for H3")

diagnosis3 %>% add_case(diagnosis4) %>%
  ggplot(aes(x=N, y=power.twotailed, group=factor(treatment_effect), colour=factor(treatment_effect))) +
  geom_line() +
  geom_hline(yintercept=0.95, linetype="dashed") +
  geom_hline(yintercept=0.8, linetype="dashed", color = "grey") +
  facet_grid(.~id) +
  ylim(c(0,1)) +
  labs(x = "Number of Respondents", y = "Power: Two-Tailed",
       subtitle = "Treatment Effect = 0.1") +
  scale_y_continuous(labels=percent) +
  theme_minimal() +
  scale_color_viridis_d() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="none",
        legend.title = element_blank()) +
  guides(color=guide_legend(nrow=1,byrow=TRUE))
ggsave(here("docs/pap/power_analysis-pooled.eps"), width=8, height=5, dpi=900)
