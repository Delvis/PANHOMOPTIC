library(dplyr)
library(ggplot2)
library(ggridges)
library(glue)
library(stringr)
library(forcats)
library(brms)
library(tidybayes)
library(sjPlot)

# Data pre-processing

source('prepare_data.R')

ma <- HomoPanDivergences # read dataset

# The most precise studies will have more weight in our calculation of the
# average divergence estimate (= effect, in a meta-analysis sense).
# if the metric is SE, we extract v = SE^2
# else the metric is bayesian 95% credibility intervals, we need to apply some standardization technique first


ma$v <- ifelse(
  ma$ErrorMetric == "Bayes", # rule selects
  (ma$Max - ma$Min)/3.92, # assuming Gaussian shape of the posterior distribution, convert credible interval into SE
  # else: SE
  (ma$Max - ma$Min)/2 # Get Standard Error from mean of max and min (since they are symmetrical)
)

ma <- ma[!is.na(ma$v),] # remove models without (!) any error metrics published

ma$Author <- paste(ma$Reference, ma$Year)

#~~~~ Fitting a Bayesian Meta-Analysis Model ~~~~~#

# Define priors

# Let's use our average for the the Intercept prior (mu)
mean(ma$ESTIMATION) # ~ 8.9 Ma
# Let's use our average of the error metric the the Intercept prior (sd)
mean(ma$v) # ~ 1.1 Ma

priors <- c(prior(normal(8.9, 1.1), class = Intercept), # 8.9 mu; 1.1 sd
            prior(cauchy(0, 0.1), class = sd)) # Half-Cauchy Distribution

formula <- bf(ESTIMATION | se(v) ~ 1 + (1|Author))

# Run Bayesian meta-analysis

mod <- brm(
  formula = formula, 
  family = gaussian(),
  prior = priors,
  data = ma, 
  cores = 4,
  seed = 1
)

# Assessing Convergence

pp_check(mod)

summary(mod)

# For example, what is the probability that the average effect size is greater than

avg_es <- as.data.frame(mod, variable = "b_Intercept")[,1]
cat( (sum(avg_es > 5) / length(avg_es))*100, "%")

# calculate posteriors


post.samples <- posterior_samples(mod, c("^b", "^sd"))
names(post.samples) <- c("smd", "tau")

# FOREST PLOTS

study.draws <- spread_draws(mod, r_Author[Author,], b_Intercept) %>% 
  mutate(b_Intercept = r_Author + b_Intercept)

pooled.effect.draws <- spread_draws(mod, b_Intercept) %>% 
  mutate(Author = "Pooled Effect")

forest.data <- bind_rows(study.draws, pooled.effect.draws) %>% 
  ungroup() %>%
  mutate(Author = str_replace_all(Author, "[.]", " ")) %>% 
  mutate(Author = reorder(Author, b_Intercept))

forest.data.summary <- group_by(forest.data, Author) %>% 
  mean_qi(b_Intercept)

# gg plot

ggALL <- ggplot(aes(x = b_Intercept,
           y = relevel(as_factor(Author), "Pooled Effect", after = Inf),
           fill = stat(x)),
       data = forest.data) +
  annotate("rect", xmin = 15.1, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "white") +
  geom_vline(xintercept = fixef(mod)[1, 1], color = "grey23", size = 1, linetype = 4) +
  geom_vline(xintercept = fixef(mod)[1, 3:4], color = "grey", linetype = 2) +
  geom_density_ridges_gradient(rel_min_height = 0.01, col = NA, scale = 1) +
  scale_fill_viridis_c() +
  geom_pointinterval(data = forest.data.summary, aes(xmin = .lower, xmax = .upper), size = 1) +
  geom_text(data = mutate_if(forest.data.summary, is.numeric, round, 2),
            aes(label = glue("{b_Intercept} [{.lower}, {.upper}]"), x = Inf), hjust = "inward") +
  labs(y = element_blank()) +
  scale_x_continuous(breaks =  seq(0, 15, 1), name = "Standardized Mean Divergence (Ma)", limits = c(1,16)) +
  guides(fill = 'none') +
  theme_minimal()

ggsave("SI_BMA_FullDataset.tiff", ggALL, width = 8, height = 12,
       dpi = "retina", device = "tiff",  bg  = 'white', scale = 1.3)


######### THRESHOLD A #########


ma_A <- ma[which(ma$ESTIMATION > FAD_A),]

mod_A <- brm(
  ESTIMATION | se(v) ~ 1 + (1|Author), 
  family = gaussian(),
  prior = priors,
  data = ma_A, 
  cores = 4,
  seed = 1
)

# Assessing Convergence

pp_check(mod_A)

summary(mod_A)
tab_model(mod_A)

# For example, what is the probability that the average effect size is greater than

avg_es <- as.data.frame(mod_A, variable = "b_Intercept")[,1]
cat((sum(avg_es > 6.5) / length(avg_es))*100, "%")

# calculate posteriors


post.samples <- posterior_samples(mod_A, c("^b", "^sd"))
names(post.samples) <- c("smd", "tau")

# FOREST PLOTS


study.draws <- spread_draws(mod_A, r_Author[Author,], b_Intercept) %>% 
  mutate(b_Intercept = r_Author + b_Intercept)

pooled.effect.draws <- spread_draws(mod_A, b_Intercept) %>% 
  mutate(Author = "Pooled Effect")

forest.data <- bind_rows(study.draws, pooled.effect.draws) %>% 
  ungroup() %>%
  mutate(Author = str_replace_all(Author, "[.]", " ")) %>% 
  mutate(Author = reorder(Author, b_Intercept))

forest.data.summary <- group_by(forest.data, Author) %>% 
  mean_qi(b_Intercept)

# gg plot

ggA <- ggplot(aes(x = b_Intercept,
           y = relevel(as_factor(Author), "Pooled Effect", after = Inf),
           fill = stat(x)),
       data = forest.data) +
  annotate("rect", xmin = 15.1, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "white") +
  geom_vline(xintercept = fixef(mod_A)[1, 1], color = "grey23", size = 1, linetype = 4) +
  geom_vline(xintercept = fixef(mod_A)[1, 3:4], color = "grey", linetype = 2) +
  geom_density_ridges_gradient(rel_min_height = 0.01, col = NA, scale = 1) +
  scale_fill_viridis_c() +
  geom_pointinterval(data = forest.data.summary, aes(xmin = .lower, xmax = .upper), size = 1) +
  geom_text(data = mutate_if(forest.data.summary, is.numeric, round, 2),
            aes(label = glue("{b_Intercept} [{.lower}, {.upper}]"), x = Inf), hjust = "inward") +
  labs(y = element_blank()) +
  scale_x_continuous(breaks =  seq(0, 15, 1), name = "Standardized Mean Divergence (Ma)", limits = c(3,16)) +
  guides(fill = 'none') +
  theme_minimal()

ggsave("SI_BMA_Filter_A.tiff", ggA, width = 8, height = 11,
       dpi = "retina", device = "tiff", bg  = 'white', scale = 1.2)


######### THRESHOLD B #########

ma_B <- ma[which(ma$ESTIMATION > FAD_B),]

# Fitting a Bayesian Meta-Analysis Model

mod_B <- brm(
  ESTIMATION | se(v) ~ 1 + (1|Author), 
  family = gaussian(),
  prior = priors,
  data = ma_B, 
  cores = 4,
  seed = 1
)

# Assessing Convergence

pp_check(mod_B)

summary(mod_B)

tab_model(mod_B)


# For example, what is the probability that the average effect size is greater than

avg_es <- as.data.frame(mod_B, variable = "b_Intercept")[,1]
cat((sum(avg_es > 7) / length(avg_es))*100, "%")


post.samples <- posterior_samples(mod_B, c("^b", "^sd"))
names(post.samples) <- c("smd", "tau")


study.draws <- spread_draws(mod_B, r_Author[Author,], b_Intercept) %>% 
  mutate(b_Intercept = r_Author + b_Intercept)

pooled.effect.draws <- spread_draws(mod_B, b_Intercept) %>% 
  mutate(Author = "Pooled Effect")

forest.data <- bind_rows(study.draws, pooled.effect.draws) %>% 
  ungroup() %>%
  mutate(Author = str_replace_all(Author, "[.]", " ")) %>% 
  mutate(Author = reorder(Author, b_Intercept))

forest.data.summary <- group_by(forest.data, Author) %>% 
  mean_qi(b_Intercept)

# gg plot

ggB <- ggplot(aes(x = b_Intercept,
           y = relevel(as_factor(Author), "Pooled Effect", after = Inf),
           fill = stat(x)),
       data = forest.data) +
  annotate("rect", xmin = 15.1, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "white") +
  geom_vline(xintercept = fixef(mod_B)[1, 1], color = "grey23", size = 1, linetype = 4) +
  geom_vline(xintercept = fixef(mod_B)[1, 3:4], color = "grey", linetype = 2) +
  geom_density_ridges_gradient(rel_min_height = 0.01, col = NA, scale = 1) +
  scale_fill_viridis_c() +
  geom_pointinterval(data = forest.data.summary, aes(xmin = .lower, xmax = .upper), size = 1) +
  geom_text(data = mutate_if(forest.data.summary, is.numeric, round, 2),
            aes(label = glue("{b_Intercept} [{.lower}, {.upper}]"), x = Inf), hjust = "inward") +
  labs(y = element_blank()) +
  scale_x_continuous(breaks =  seq(0, 15, 1), name = "Standardized Mean Divergence (Ma)", limits = c(4.5,15.1)) +
  guides(fill = 'none') +
  theme_minimal()

ggsave("SI_BMA_Filter_B.tiff", ggB, width = 8, height = 10,
       dpi = "retina", device = "tiff", bg  = 'white', scale = 1.1)


######### THRESHOLD C #########

ma_C <- ma[which(ma$ESTIMATION > FAD_C),]


# Fitting a Bayesian Meta-Analysis Model

mod_C <- brm(
  ESTIMATION | se(v) ~ 1 + (1|Author), 
  family = gaussian(),
  prior = priors,
  data = ma_C, 
  cores = 4,
  seed = 1
)

# Assessing Convergence

pp_check(mod_C)

summary(mod_C)

tab_model(mod_C)


# For example, what is the probability that the average effect size is greater than

avg_es <- as.data.frame(mod_C, variable = "b_Intercept")[,1]
cat( (sum(avg_es > 8) / length(avg_es))*100, "%")


post.samples <- posterior_samples(mod_C, c("^b", "^sd"))
names(post.samples) <- c("smd", "tau")



study.draws <- spread_draws(mod_C, r_Author[Author,], b_Intercept) %>% 
  mutate(b_Intercept = r_Author + b_Intercept)

pooled.effect.draws <- spread_draws(mod_C, b_Intercept) %>% 
  mutate(Author = "Pooled Effect")

forest.data <- bind_rows(study.draws, pooled.effect.draws) %>% 
  ungroup() %>%
  mutate(Author = str_replace_all(Author, "[.]", " ")) %>% 
  mutate(Author = reorder(Author, b_Intercept))

forest.data.summary <- group_by(forest.data, Author) %>% 
  mean_qi(b_Intercept)

# gg plot

ggC <- ggplot(aes(x = b_Intercept,
           y = relevel(as_factor(Author), "Pooled Effect", after = Inf),
           fill = stat(x)),
       data = forest.data) +
  annotate("rect", xmin = 15.1, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "white") +
  geom_vline(xintercept = fixef(mod_C)[1, 1], color = "grey23", size = 1, linetype = 4) +
  geom_vline(xintercept = fixef(mod_C)[1, 3:4], color = "grey", linetype = 2) +
  geom_density_ridges_gradient(rel_min_height = 0.01, col = NA, scale = 1) +
  scale_fill_viridis_c() +
  geom_pointinterval(data = forest.data.summary, aes(xmin = .lower, xmax = .upper), size = 1) +
  geom_text(data = mutate_if(forest.data.summary, is.numeric, round, 2),
            aes(label = glue("{b_Intercept} [{.lower}, {.upper}]"), x = Inf), hjust = "inward") +
  labs(y = element_blank()) +
  scale_x_continuous(breaks =  seq(0, 15, 1), name = "Standardized Mean Divergence (Ma)", limits = c(5.5,15.1)) +
  guides(fill = 'none') +
  theme_minimal()

ggsave("SI_BMA_Filter_C.tiff", ggC, width = 8, height = 7,
       dpi = "retina", device = "tiff",  bg  = 'white')

