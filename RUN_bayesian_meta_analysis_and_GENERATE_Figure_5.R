library(brms)
source('prepare_data.R')

# Select the studies that are supported by the strictest fossil-threshold to avoid biases
ma <- HomoPanDivergences[which(HomoPanDivergences$ESTIMATION > FAD_C),]

# Aesthetics: New "Author" variable combines (Name, Year) to show studies in plots
ma$Author <- paste(ma$Reference, ma$Year)

# "v" - ERROR METRICS IN META-ANALYSIS: BAYESIAN VS FREQUENTISTS

# Read more about the topic here:
# 1) https://discourse.mc-stan.org/t/integrating-different-types-of-error-intervals-in-meta-analysis/26031
# 2) https://stats.stackexchange.com/questions/526981/converting-a-confidence-interval-into-a-credible-interval
# 3) https://www.jstor.org/stable/23059129
# 4) https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6630113/
# 5) https://discourse.datamethods.org/t/transforming-confidence-intervals-into-credible-intervals/4166/13

# since the metric is bayesian 95% credibility intervals, we need to apply some standardization technique first

ma$v <- ifelse(
  ma$ErrorMetric == "Bayes", # rule selects
  (ma$Max - ma$Min)/3.92, # assuming Gaussian shape of the posterior distribution, convert credible interval into SE
  # else: SE
  (ma$Max - ma$Min)/2 # Get Standard Error from mean of max and min (since they are symmetrical)
)

paste('The bayesian meta-analysis will be performed using a total of',
      table(is.na(ma$v))[1], 'studies. Unfortunately',
      table(is.na(ma$v))[2],
      'studies were removed because they did not include standard errors.')

ma <- ma[!is.na(ma$v),] # remove models without (!) any error metrics published


# "the range of a posterior probability (PP) 95% credible interval would be equal to the range of a 95% confidence/compatibility if one would have used a non-informative (flat) prior."

##### Fitting a Bayesian Meta-Analysis Model ##### pre-steps

### 0: Formula

formula <- bf(ESTIMATION | se(v) ~ 1 + (1|Author))

### 1: Considerations on Priors

# we can use the function get_prior() to tell us which priors need to be specified
# This function also provides suggestions for default priors:

get_prior(formula, data = ma, family = gaussian) # intercept-only model

# Let's use our average for the the Intercept prior (mu)
mean(ma$ESTIMATION) # ~ 8.9 Ma
# Let's use our average of the error metric the the Intercept prior (sd)
mean(ma$v) # ~ 1.1 Ma

# 2: Define priors

priors <- c(prior(normal(8.9, 1.1), class = Intercept), # 8.9 mu; 1.1 sd
            prior(cauchy(0, 0.1), class = sd)) # Half-Cauchy Distribution

# 3: Run Bayesian meta-analysis

mod <- brm(
  formula = formula, 
  family = gaussian(),
  prior = priors,
  data = ma, 
  cores = 4
)

# Assessing Convergence

as_draws_rvars(mod)

pp_check(mod, ndraws = 100) # predictions from our priors (blue) vs. data (black)

# The plot indicates that our model captures the overall distribution of our
# dependent variable (although there is a lot of uncertainty in model predictions).

summary(mod)

# The Estimate(s) obtained for the Intercept is centered on approx. ... and
# has an estimated error of ... The 95 % credible interval is approximately ... .
# In the ‘Family-Specific Parameters’ section, the residual variation (i.e., sigma) is estimated to be approximately...

# For example, what is the probability that the average effect size is greater than

avg_es <- as.data.frame(mod, variable = "b_Intercept")[,1]
cat( (sum(avg_es > 8) / length(avg_es))*100, "%")

# calculate posteriors

library(tidybayes)

post.samples <- posterior_samples(mod, c("^b", "^sd"))
names(post.samples) <- c("smd", "tau")

# FOREST PLOTS

library(dplyr)
library(ggplot2)
library(ggridges)
library(glue)
library(stringr)
library(forcats)

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

ggplot(aes(x = b_Intercept,
           y = relevel(as_factor(Author), "Pooled Effect", after = Inf),
           fill = stat(x)),
       data = forest.data) +
  annotate("rect", xmin = 15.1, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "white") +
  geom_vline(xintercept = fixef(mod)[1, 1], color = "grey23", size = 1, linetype = 4) +
  geom_vline(xintercept = fixef(mod)[1, 3:4], color = "grey", linetype = 2) +
  geom_density_ridges_gradient(rel_min_height = 0.01, col = NA, scale = 1.5) +
  scale_fill_viridis_c() +
  geom_pointinterval(data = forest.data.summary, aes(xmin = .lower, xmax = .upper), size = 1) +
  geom_text(data = mutate_if(forest.data.summary, is.numeric, round, 2),
            aes(label = glue("{b_Intercept} [{.lower}, {.upper}]"), x = Inf), hjust = "inward") +
  labs(y = element_blank()) +
  scale_x_continuous(breaks =  seq(0, 15, 1), name = "Standardized Mean Divergence (Ma)", limits = c(5.5,15.1)) +
  guides(fill = 'none') +
  theme_minimal()

ggsave("Fig5_MetaBayes_FAD_C.tiff", last_plot(), width = 8, height = 7,
       dpi = "retina", device = "tiff", type = "cairo", bg  = 'white')

