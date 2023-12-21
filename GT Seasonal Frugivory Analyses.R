rm(list=ls(all=TRUE))
gc()
library(readxl); library(brms); library(cmdstanr); library(ordbetareg); library(bayesplot); library(ggplot2); library(marginaleffects); library(tidyverse); library(tidybayes); library(loo); library(performance); library(ggtext); library(patchwork); library(posterior); library(ggthemes)

# Load data
data <- read_excel("~/Graduate Research/Dissertation/Data/GT Scat Dissections/gt.frugivory.xlsx", sheet = "data")

# Specify categorical variables as factors
data$ind <- as.factor(data$ind.5samp)
data$w.season <- as.factor(data$w.season)
data$season <- as.factor(data$season)
data$town <- as.factor(data$town)
data$w.season <- relevel(data$w.season, ref = "Dry")
data$season <- relevel(data$season, ref = "Winter")
data$town <- relevel(data$town, ref = "E")



#=================================FRUGIVORY GAMM==========================================#
{
data <- read_excel("~/Graduate Research/Dissertation/Data/GT Scat Dissections/gt.frugivory.xlsx", sheet = "data")
  
# Specify categorical variables as factors
data$ind <- as.factor(data$ind.5samp)
data$season <- as.factor(data$season)
data$town <- as.factor(data$town)
data$season <- relevel(data$season, ref = "Winter")
data$town <- relevel(data$town, ref = "E")

### FIT GAMM FOR FRUGIVORY THROUGHOUT STUDY PERIOD, ACROSS TOWNS AND CONTROLLING FOR INDS
fit.spline <- ordbetareg(formula = prop.fruit ~ s(cday) + (1|ind),
                      data=data,
                      chains=4, 
                      iter=40000, 
                      warmup=8000, 
                      cores=14, 
                      seed=1234, 
                      backend="cmdstanr",
                      save_pars = save_pars(all = TRUE))
### PPCHECK AND OTHER DATA EXTRACTION
summary(fit.spline)
plot(fit.spline)
conditional_effects(fit.spline)

# Visualize the model cutpoints by showing them relative to the empirical distribution
{all_draws1 <- prepare_predictions(fit.spline)
  
  cutzero1 <- plogis(all_draws1$dpars$cutzero)
  cutone1 <- plogis(all_draws1$dpars$cutzero + exp(all_draws1$dpars$cutone))
  
  data %>% 
    ggplot(aes(x=prop.fruit)) +
    geom_histogram(bins=100) +
    theme_stata() + 
    theme(panel.grid=element_blank()) +
    scale_x_continuous(breaks=c(0,0.25,0.50,0.75,1.00),
                       labels=c("0","0.25","0.50","0.75","1.00")) +
    geom_vline(xintercept = mean(cutzero1)*1.00,linetype=2) +
    geom_vline(xintercept = mean(cutone1)*1.00,linetype=2) +
    labs(x = "Proportion of Fecal Volume Comprised of Fruit",
         y = "Density") +
    ggtitle(paste0("Distribution of Fruit Consumption across all (n = ",sum(!is.na(data$prop.fruit)),") Fecal Samples")) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
          legend.title = element_text(face = "bold", size = 14),
          legend.text = element_text(size = 14),
          axis.title = element_text(face = "bold", size = 14, margin = margin(t = 5, b = 10)),
          axis.text = element_text(size = 14),
          plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt"))
}
# New theme option will add in new ggplot2 themes or themes from other packages
plots1 <- pp_check_ordbeta(fit.spline,
                           ndraws=100,
                           outcome_label="Strength of Frugivory",
                           new_theme=ggthemes::theme_stata())
plots1$discrete
plots1$continuous


precip.data <- read_excel("~/Graduate Research/Dissertation/Data/GT Scat Dissections/gt.frugivory.xlsx", sheet = "precip.data")
precip.gam <- brm(formula = precip ~ s(cday),
                  data=precip.data,
                  family = gaussian(),
                  chains=4, 
                  iter=40000, 
                  warmup=8000, 
                  cores=14, 
                  seed=1234, 
                  backend="cmdstanr",
                  save_pars = save_pars(all = TRUE))
conditional_effects(precip.gam)

precip.predict <- conditional_effects(precip.gam)[[1]]

mean(precip.predict$estimate__[precip.predict$cday >= 0 & precip.predict$cday <= 31])
mean(precip.predict$estimate__[precip.predict$cday >= 32 & precip.predict$cday <= 59])
mean(precip.predict$estimate__[precip.predict$cday >= 60 & precip.predict$cday <= 90])
mean(precip.predict$estimate__[precip.predict$cday >= 91 & precip.predict$cday <= 120])
mean(precip.predict$estimate__[precip.predict$cday >= 121 & precip.predict$cday <= 151])
mean(precip.predict$estimate__[precip.predict$cday >= 152 & precip.predict$cday <= 181])
mean(precip.predict$estimate__[precip.predict$cday >= 182 & precip.predict$cday <= 212])
mean(precip.predict$estimate__[precip.predict$cday >= 213 & precip.predict$cday <= 243])
mean(precip.predict$estimate__[precip.predict$cday >= 244 & precip.predict$cday <= 273])
mean(precip.predict$estimate__[precip.predict$cday >= 274 & precip.predict$cday <= 304])
mean(precip.predict$estimate__[precip.predict$cday >= 305 & precip.predict$cday <= 334])
mean(precip.predict$estimate__[precip.predict$cday >= 335 & precip.predict$cday <= 365])


# PLOT SPLINE FOR PRECIPITATION AND FRUGIVORY
{
  seasons <- data.frame(season = c("Winter", "Spring", "Summer", "Fall", "Winter"), start = c(0, 79, 171, 265, 355), end = c(79, 171, 265, 355, 365))
  
  pred <- predictions(fit.spline,
                      newdata = datagrid(cday = seq(0,365, by=1)), 
                      re_formula = NA, 
                      type = "response", 
                      ndraws = 4000)
  head(pred)
  
  pred <- posteriordraws(pred)
  
  head(pred)
  
  
  pred_precip <- predictions(precip.gam,
                             newdata = datagrid(cday = seq(0,365, by=1)), 
                             re_formula = NA, 
                             type = "response", 
                             ndraws = 4000)
  head(pred_precip)
  
  pred_precip <- posteriordraws(pred_precip)
  
  head(pred_precip)
  
  
  
  precip.spline <- ggplot(pred_precip, aes(x = cday, y = draw)) + 
    ylim(0, 1.00) + theme_bw() + theme_linedraw() +
    ggtitle("Precipitation over Study Period") + theme(plot.title = element_text(hjust = 0.5)) +
    annotate("rect", xmin = 0, xmax = 79, ymin = -Inf, ymax = Inf,
             alpha = .2,fill = "darkblue") +
    annotate("rect", xmin = 79, xmax = 171, ymin = -Inf, ymax = Inf,
             alpha = .2,fill = "darkgreen") +
    annotate("rect", xmin = 171, xmax = 265, ymin = -Inf, ymax = Inf,
             alpha = .2,fill = "darkred") +
    annotate("rect", xmin = 265, xmax = 355, ymin = -Inf, ymax = Inf,
             alpha = .2,fill = "orange") +
    annotate("rect", xmin = 355, xmax = 365, ymin = -Inf, ymax = Inf,
             alpha = .2,fill = "darkblue") +
    stat_summary(
      data = pred_precip,
      aes(x = cday, y = draw),
      fun = "mean",  # Calculate the mean of draw
      geom = "line",
      color = "darkblue",
      linetype = "dashed",
      size = 1.5
    )+
    labs(x = "Calendar Days",
         y = "Avg. Daily Precipitation (in)",
         fill = "Credible Interval") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 24, 
                                    margin = margin(t = 5, b = 20)),
          legend.title = element_text(face = "bold", size = 20),
          legend.text = element_text(size = 20),
          axis.title = element_text(face = "bold", size = 20),
          axis.title.x = element_text(margin = margin(t = 10)),
          axis.title.y = element_text(margin = margin(r = 10)),
          axis.text = element_text(size = 18),
          plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt"))
  
  
  
  
  precip.spline <- ggplot(pred_precip, aes(x = cday, y = draw)) + 
    ylim(0, 1.00) + theme_bw() + theme_linedraw() +
    ggtitle("Precipitation over Study Period") + theme(plot.title = element_text(hjust = 0.5)) +
    annotate("rect", xmin = 0, xmax = 121.6667, ymin = -Inf, ymax = Inf,
             alpha = .2, fill = "darkred") +
    annotate("rect", xmin = 121.6667, xmax = 273.75, ymin = -Inf, ymax = Inf,
             alpha = .2, fill = "darkblue") +
    annotate("rect", xmin = 273.75, xmax = 365, ymin = -Inf, ymax = Inf,
             alpha = .2, fill = "darkred") +
    geom_vline(xintercept = c(121.6667, 273.75), linetype = "dashed", color = "black", size = 1.5) +
    stat_summary(
      data = pred_precip,
      aes(x = cday, y = draw),
      fun = "mean",  # Calculate the mean of draw
      geom = "line",
      color = "darkblue",
      linetype = "solid",
      size = 1.5
    ) +
    labs(x = "Calendar Month",
         y = "Avg. Daily Precipitation (in)",
         fill = "Credible Interval") +
    scale_x_continuous(
      breaks = seq(1, 365, by = 30.416666666666666666666666666667),
      labels = seq(1, length(seq(1, 365, by = 30.416666666666666666666666666667)), by = 1)
    ) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 24, 
                                    margin = margin(t = 5, b = 20)),
          legend.title = element_text(face = "bold", size = 20),
          legend.text = element_text(size = 20),
          axis.title = element_text(face = "bold", size = 20),
          axis.title.x = element_text(margin = margin(t = 10)),
          axis.title.y = element_text(margin = margin(r = 10)),
          axis.text = element_text(size = 18),
          plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt"),
      panel.grid.major.x = element_line(color = "gray", size = 0.5),  # Customize gridlines
      panel.grid.minor.x = element_blank())
  precip.spline
  
  
  precip.frug.spline <- ggplot(pred, aes(x = cday, y = draw)) + ylim(0, 1.00) + theme_bw() + theme_linedraw() +
    ggtitle("Population-Wide Frugivory Through Time") + theme(plot.title = element_text(hjust = 0.5)) +
    annotate("rect", xmin = 0, xmax = 79, ymin = -Inf, ymax = Inf,
             alpha = .2,fill = "darkblue") +
    annotate("rect", xmin = 79, xmax = 171, ymin = -Inf, ymax = Inf,
             alpha = .2,fill = "darkgreen") +
    annotate("rect", xmin = 171, xmax = 265, ymin = -Inf, ymax = Inf,
             alpha = .2,fill = "darkred") +
    annotate("rect", xmin = 265, xmax = 355, ymin = -Inf, ymax = Inf,
             alpha = .2,fill = "orange") +
    annotate("rect", xmin = 355, xmax = 365, ymin = -Inf, ymax = Inf,
             alpha = .2,fill = "darkblue") +
    stat_lineribbon() +
    scale_fill_brewer(palette = "Greys")  +
    stat_summary(
      data = pred_precip,
      aes(x = cday, y = draw),
      fun = "mean",  # Calculate the mean of draw
      geom = "line",
      color = "darkblue",
      linetype = "dashed",
      size = 1.5
    )+
    labs(x = "Calendar Days",
         y = "Proportion of Fecal Volume made of Fruit",
         fill = "Credible Interval") +
    geom_point(data = data, aes(x = cday, y = prop.fruit), inherit.aes = FALSE) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 24, 
                                    margin = margin(t = 5, b = 20)),
          legend.title = element_text(face = "bold", size = 20),
          legend.text = element_text(size = 20),
          axis.title = element_text(face = "bold", size = 20),
          axis.title.x = element_text(margin = margin(t = 10)),
          axis.title.y = element_text(margin = margin(r = 10)),
          axis.title.y.right = element_text(margin = margin(l = 10)),
          axis.text = element_text(size = 18),
          plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt")) + 
    scale_y_continuous(
      sec.axis = sec_axis(~., name = "Precipitation (in)"), limits = c(0,1.00)
    )
}
precip.spline
precip.frug.spline



# Find the maximum lower bound and minimum upper bound
max_lower_bound <- max(pred$conf.low)
max_estimate <- max(pred$estimate)
max_upper_bound <- max(pred$conf.high)

# Calculate the range of the 95% credible interval
range_95_ci <- max_upper_bound - max_lower_bound

# Print the range
c(max_lower_bound, max_estimate, max_upper_bound)
print(range_95_ci)
}
#=========================================================================================#





#=================================FRUGIVORY GLMM==========================================#
{
data <- read_excel("~/Graduate Research/Dissertation/Data/GT Scat Dissections/gt.frugivory.xlsx", sheet = "data")
  
# Specify categorical variables as factors
data$ind <- as.factor(data$ind.5samp)
data$season <- as.factor(data$season)
data$w.season <- as.factor(data$w.season)
data$town <- as.factor(data$town)
data$season <- relevel(data$season, ref = "Winter")
data$w.season <- relevel(data$w.season, ref = "Dry")
data$town <- relevel(data$town, ref = "E")
  
### GLMM FOR WITHIN-SEASON TRENDS IN FRUGIVORY ###
# Explore non-zero values for purposes of prior-setting
summary(subset(data, data$prop.fruit>0))
mean(data$prop.fruit == 0)
# Before fitting model, change reference level to Winter
data$season <- relevel(data$season, ref = "Winter")

# FIT WITHIN-SEASON MODEL AS AN ORDERED BETA REGRESSION
fit <- ordbetareg(prop.fruit ~ sday*season + (1|ind),
                  # other specification could be: prop.fruit ~ sday*season*town + (1|ind)
                      data=data,
                      chains=4, 
                      iter=8000, 
                      warmup=4000, 
                      cores=14, 
                      seed=1234, 
                      backend="cmdstanr",
                      save_pars = save_pars(all = TRUE),
                      control = list(max_treedepth=15))
summary(fit)
coef(fit)


# Visualize the model cutpoints by showing them relative to the empirical distribution
{all_draws <- prepare_predictions(fit)

cutzero <- plogis(all_draws$dpars$cutzero)
cutone <- plogis(all_draws$dpars$cutzero + exp(all_draws$dpars$cutone))

data %>% 
  ggplot(aes(x=prop.fruit)) +
  geom_histogram(bins=100) +
  ggthemes::theme_stata() + 
  theme(panel.grid=element_blank()) +
  scale_x_continuous(breaks=c(0,0.25,0.50,0.75,1.00),
                     labels=c("0","0.25","0.50","0.75","1.00")) +
  geom_vline(xintercept = mean(cutzero)*1.00,linetype=2) +
  geom_vline(xintercept = mean(cutone)*1.00,linetype=2) +
  ylab("Density") +
  xlab("Proportion of Fecal Volume Comprised of Fruit") +
  labs(caption=paste0("Figure shows the distribution of fruit consumption in ",sum(!is.na(data$prop.fruit))," fecal samples.")) +
  ggtitle("Response Variable Distribution with Cutpoints") +
  theme(plot.title = element_text(hjust = 0.5))
}

# Plot Posterior Predictive Checks with new theme option. Will add in new ggplot2 themes or themes from other packages.
plots <- pp_check_ordbeta(fit,
                          ndraws=100,
                          outcome_label="Strength of Frugivory",
                          new_theme=ggthemes::theme_stata())
# pp_check in discrete form
plots$discrete
# pp_check in continuous form
plots$continuous

### GGPLOT CODE FOR SEASON ###
{pred_Winter <- predictions(fit,
                            newdata = datagrid(sday = seq(0,95, by=1), 
                                               season="Winter"), 
                            re_formula = NULL, 
                            type = "response",
                            allow_new_levels = TRUE,
                            ndraws = 4000)
  head(pred_Winter)
  
  pred_Winter <- posteriordraws(pred_Winter)
  
  head(pred_Winter)
  
  glmm.Winter <- ggplot(pred_Winter, aes(x = sday, y = draw)) + ylim(0, 1.00) + theme_bw() + theme_linedraw() + 
    stat_lineribbon() +
    scale_fill_brewer(palette = "Blues") +
    geom_point(data = subset(data, season == "Winter"), aes(x = sday, y = prop.fruit), inherit.aes = FALSE) +
    labs(x = "Days into Season",
         y = "",
         title = "Winter",
         fill = "Credible Interval") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
          legend.title = element_text(face = "bold", size = 14),
          legend.text = element_text(size = 14),
          axis.title = element_text(face = "bold", size = 14, margin = margin(t = 5, b = 10)),
          axis.text = element_text(size = 14),
          plot.margin = margin(t = 20, r = 10, b = 10, l = 10, unit = "pt"))}
{pred_Spring <- predictions(fit,
                            newdata = datagrid(sday = seq(0,95, by=1), 
                                               season="Spring"), 
                            re_formula = NULL, 
                            type = "response",
                            allow_new_levels = TRUE,
                            ndraws = 4000)
  head(pred_Spring)
  
  pred_Spring <- posteriordraws(pred_Spring)
  
  head(pred_Spring)
  
  glmm.Spring <- ggplot(pred_Spring, aes(x = sday, y = draw)) + ylim(0, 1.00) + theme_bw() + theme_linedraw() +
    stat_lineribbon() +
    scale_fill_brewer(palette = "Greens") +
    geom_point(data = subset(data, season == "Spring"), aes(x = sday, y = prop.fruit), inherit.aes = FALSE) +
    labs(x = "",
         y = "Prop. of Fecal Volume made of Fruit",
         title = "Spring",
         fill = "Credible Interval") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
          legend.title = element_text(face = "bold", size = 14),
          legend.text = element_text(size = 14),
          axis.title = element_text(face = "bold", size = 14, margin = margin(t = 5, b = 10)),
          axis.text = element_text(size = 14),
          plot.margin = margin(t = 10, r = 10, b = 20, l = 10, unit = "pt"))}
{pred_Summer <- predictions(fit,
                            newdata = datagrid(sday = seq(0,95, by=1), 
                                               season="Summer"), 
                            re_formula = NULL, 
                            type = "response",
                            allow_new_levels = TRUE,
                            ndraws = 4000)
  head(pred_Summer)
  
  pred_Summer <- posteriordraws(pred_Summer)
  
  head(pred_Summer)
  
  glmm.Summer <- ggplot(pred_Summer, aes(x = sday, y = draw)) + ylim(0, 1.00) + theme_bw() + theme_linedraw() +
    stat_lineribbon() +
    scale_fill_brewer(palette = "Reds") +
    geom_point(data = subset(data, season == "Summer"), aes(x = sday, y = prop.fruit), inherit.aes = FALSE) +
    labs(x = "",
         y = "",
         title = "Summer",
         fill = "Credible Interval") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
          legend.title = element_text(face = "bold", size = 14),
          legend.text = element_text(size = 14),
          axis.title = element_text(face = "bold", size = 14, margin = margin(t = 5, b = 10)),
          axis.text = element_text(size = 14),
          plot.margin = margin(t = 10, r = 10, b = 20, l = 10, unit = "pt"))}
{pred_Fall <- predictions(fit,
                          newdata = datagrid(sday = seq(0,95, by=1), 
                                             season="Fall"), 
                          re_formula = NULL, 
                          type = "response",
                          allow_new_levels = TRUE,
                          ndraws = 4000)
  head(pred_Fall)
  
  pred_Fall <- posteriordraws(pred_Fall)
  
  head(pred_Fall)
  
  glmm.Fall <- ggplot(pred_Fall, aes(x = sday, y = draw)) + ylim(0, 1.00) + theme_bw() + theme_linedraw() +
    stat_lineribbon() +
    scale_fill_brewer(palette = "Oranges") +
    geom_point(data = subset(data, season == "Fall"), aes(x = sday, y = prop.fruit), inherit.aes = FALSE) +
    labs(x = "Days into Season",
         y = "Prop. of Fecal Volume made of Fruit",
         title = "Fall",
         fill = "Credible Interval") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
          legend.title = element_text(face = "bold", size = 14),
          legend.text = element_text(size = 14),
          axis.title = element_text(face = "bold", size = 14, margin = margin(t = 5, b = 10)),
          axis.text = element_text(size = 14),
          plot.margin = margin(t = 20, r = 10, b = 10, l = 10, unit = "pt"))}
glmm.seasons <- glmm.Spring + glmm.Summer + glmm.Fall + glmm.Winter + plot_annotation(title = "Within-Season Patterns of Frugivory", theme = theme(plot.title = element_text(hjust = 0.5, vjust = 0.5, face = "bold", size = 22)))
glmm.seasons



fit.w <- ordbetareg(prop.fruit ~ w.season + (1|ind),
                  data=data,
                  chains=4, 
                  iter=8000, 
                  warmup=4000, 
                  cores=14, 
                  seed=1234, 
                  backend="cmdstanr",
                  save_pars = save_pars(all = TRUE),
                  control = list(max_treedepth=15))
{
  pred_wet_dry <- predictions(fit.w,
                            newdata = datagrid(w.season=c("Dry","Wet")), 
                            re_formula = NULL, 
                            type = "response",
                            allow_new_levels = TRUE,
                            ndraws = 4000)
  head(pred_wet_dry)
  
  pred_wet_dry <- posteriordraws(pred_wet_dry)
  
  head(pred_wet_dry)

# GGPLOT OBJECT IS PLOTTING POSTERIOR PREDICTIONS RATHER THAN RAW DATAPOINTS IN
#  geom_point(data = subset(pred_wet_dry, w.season == c("Dry","Wet")), 
#  aes(x = w.season, y = prop.fruit), inherit.aes = FALSE)
  glmm.wet.dry <- ggplot(pred_wet_dry, aes(x = w.season, y = draw)) + ylim(0, 0.50) + theme_bw() + theme_linedraw() + 
    geom_boxplot(fill = "lightblue", color = "black", alpha = 0.7) +
    geom_point(data = subset(pred_wet_dry, w.season == c("Dry","Wet")), aes(x = w.season, y = prop.fruit), inherit.aes = FALSE) +
    labs(x = "Season",
         y = "Proportion of Fecal Volume Made of Fruit",
         title = "Wet vs. Dry Season Frugivory",
         fill = "Credible Interval") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
          legend.title = element_text(face = "bold", size = 14),
          legend.text = element_text(size = 14),
          axis.title = element_text(face = "bold", size = 14, margin = margin(t = 5, b = 10)),
          axis.text = element_text(size = 14),
          plot.margin = margin(t = 20, r = 10, b = 10, l = 10, unit = "pt"))
}
glmm.wet.dry


library(bayestestR)
# Create a Posterior Description Summary Table
post_desc <- describe_posterior(fit,
                                centrality = "median",
                                dispersion = FALSE,
                                ci = 0.95,
                                ci_method = "eti",
                                test = c("p_direction"),
                                rope_range = "default",
                                rope_ci = 0.95,
                                keep_iterations = FALSE,
                                bf_prior = NULL,
                                diagnostic = c("ESS", "Rhat"),
                                effects = c("all"),
                                component = c("all"),
                                parameters = NULL,
                                BF = 1,
                                priors = FALSE)

# Convert ordbetareg estimates using plogis()
post_desc[,4] <- plogis(post_desc[,4])
post_desc[,6] <- plogis(post_desc[,6])
post_desc[,7] <- plogis(post_desc[,7])

# Print the transformed summary table
post_desc
}
#=========================================================================================#





#===============================PREPARE FIGURES===========================================#
{
### Extract Marginal Effects Among seasons
mfx <- avg_slopes(fit, variables="season")
mfx

mfx <- posterior_draws(mfx)
# Specify the order of the levels
mfx$contrast <- factor(mfx$contrast, levels = c("Spring - Winter", "Summer - Winter", "Fall - Winter"))
among_post <- ggplot(mfx, aes(x = draw, fill = factor(contrast))) +
                     stat_halfeye(slab_alpha = .5) +
                     labs(x = "Effect of Seasonality on Frugivory",
                     y = "Posterior Density",
                     fill = "Season Contrast") +
                     scale_fill_manual(values = c("darkgreen", "darkred", "darkorange")) +
                     geom_vline(xintercept = 0.00, linetype = "dashed", color = "black") +
                     ggtitle("Overall Difference in Frugivory Between Seasons") +
                     theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
                           legend.title = element_text(face = "bold", size = 14),
                           legend.text = element_text(size = 14),
                           axis.title = element_text(face = "bold", size = 14, margin = margin(t = 5, b = 10)),
                           axis.text = element_text(size = 14),
                           plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt"))
among_post

among_boxplot <- conditional_effects(fit, effects = "season")
among_boxplot



mfx_summary <- mfx %>% group_by(contrast) 
mfx_summary[1:3,]





### Plot Marginal Effects Within Seasons
nfx <- avg_slopes(fit, variables="sday", by="season")
nfx

nfx <- posterior_draws(nfx)

# Specify the order of the levels
nfx$season <- factor(nfx$season, levels = c("Fall", "Winter", "Spring", "Summer"))
within_post <- ggplot(nfx, aes(x = draw*30, fill = factor(season))) +
                      stat_halfeye(slab_alpha = .5) +
                      labs(x = "Change in Frugivory (per month)",
                      y = "Posterior Density",
                      fill = "Season") +
                      scale_fill_manual(values = c("darkorange", "darkblue", "forestgreen", "darkred")) +
                      geom_vline(xintercept = 0.00, linetype = "dashed", color = "black") +
                      ggtitle("Within-Season Changes in Frugivory") +
                      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
                            legend.title = element_text(face = "bold", size = 14),
                            legend.text = element_text(size = 14),
                            axis.title = element_text(face = "bold", size = 14, margin = margin(t = 5, b = 10)),
                            axis.text = element_text(size = 14),
                            plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt")) +
                      coord_cartesian(xlim = c(-0.30, NA))
within_post



nfx_summary <- nfx %>% group_by(season) 
nfx_summary[1:4,]




### Extract Marginal Effects Among Wet/Dry Season
ofx <- avg_slopes(fit.w, variables="w.season")
ofx

ofx <- posterior_draws(ofx)
# Specify the order of the levels
ofx$contrast <- factor(ofx$contrast, levels = c("Wet - Dry"))
wet_dry_post <- ggplot(ofx, aes(x = draw, fill = factor(contrast))) +
  stat_halfeye(slab_alpha = .5) +
  labs(x = "Effect of Seasonality on Frugivory",
       y = "Posterior Density",
       fill = "Season Contrast") +
  scale_fill_manual(values = c("darkblue")) +
  geom_vline(xintercept = 0.00, linetype = "dashed", color = "black") +
  ggtitle("Difference in Frugivory Between Wet and Dry Season") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        legend.title = element_text(face = "bold", size = 14),
        legend.text = element_text(size = 14),
        axis.title = element_text(face = "bold", size = 14, margin = margin(t = 5, b = 10)),
        axis.text = element_text(size = 14),
        plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt"))
wet_dry_post

wet_dry_boxplot <- conditional_effects(fit.w, effects = "w.season")
wet_dry_boxplot



ofx_summary <- ofx %>% group_by(contrast) 
ofx_summary[1:3,]




### Extract Variance Component of the Individual Random Effect
post_samples <- as_draws_df(fit, "sd_ind__Intercept")

# Create ggplot() object of posterior samples for individual sd values
#ind_post <- ggplot(post_samples, aes(x = sd_ind__Intercept)) +
#                   stat_halfeye(slab_alpha = .5, slab_fill = "darkred") +
#                   ggtitle("Posterior Distribution for Standard Deviation of Individual Random Effect") +
#                   labs(x = "Standard Deviation of Individual Random Effect", 
#                        y = "Posterior Density") +
#                    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
#                          legend.title = element_text(face = "bold", size = 14),
#                          legend.text = element_text(size = 14),
#                          axis.title = element_text(face = "bold", size = 14, margin = margin(t = 5, b = 10)),
#                          axis.text = element_text(size = 14),
#                          plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt"))
#ind_post





### Quantify Random Effect Estimates
qfx <- ranef(fit, variables="ind", pars = "Intercept")
qfx
qfx <- as.data.frame(qfx)
qfx$ind.Estimate.Intercept <- plogis(qfx$ind.Estimate.Intercept)
qfx$ind.Est.Error.Intercept <- plogis(qfx$ind.Est.Error.Intercept)
qfx$ind.Q2.5.Intercept <- plogis(qfx$ind.Q2.5.Intercept)
qfx$ind.Q97.5.Intercept <- plogis(qfx$ind.Q97.5.Intercept)
qfx
hist(qfx$ind.Estimate.Intercept)

row_names <- row.names(qfx)
qfx <- cbind(ind = row_names, qfx)
remove(row_names)

qfx
q1 <- quantile(qfx$ind.Estimate.Intercept, 0.25)
q3 <- quantile(qfx$ind.Estimate.Intercept, 0.75)





### Extract posterior draws for individual tortoise random effect and plot
ind_draws <- avg_predictions(fit, variables="ind")
ind_draws <- posterior_draws(ind_draws)

### Plot individual predictions of tortoise frugivory
ind_pred <- ggplot(ind_draws, aes(x = draw, fill = factor(ind))) +
                    stat_halfeye(slab_alpha = .5) +
                    labs(x = "Predicted Strength of Frugivory",
                         y = "Posterior Density",
                           fill = "Individual") +
                    ggtitle("(a)") + 
                    theme(plot.title = element_text(face = "bold", size = 18),
                          legend.title = element_text(face = "bold", size = 14),
                          legend.text = element_text(size = 14),
                          axis.title = element_text(face = "bold", size = 14, margin = margin(t = 5, b = 10)),
                          axis.text = element_text(size = 14),
                          plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt"))
ind_pred





# Plot individual predictions compared to each other
ind_plot <- ggplot(ind_draws, aes(x = draw, y = reorder(ind, draw))) +
                   geom_vline(xintercept = mean(ind_draws$draw), color = "#839496", size = 1) +
                   geom_vline(xintercept = mean(ind_draws$draw) - sd(ind_draws$draw), color = "#839496", linetype = 2) +
                   geom_vline(xintercept = mean(ind_draws$draw) + sd(ind_draws$draw), color = "#839496", linetype = 2) +
                   stat_halfeye(.width = .5, size = 2/3, fill = "purple") +
                   labs(x = "Predicted Strength of Frugivory",
                        y = "Individual") +
                   theme(panel.grid   = element_blank(),
                         axis.ticks.y = element_blank(),
                         axis.text.y  = element_text(hjust = 0)) +
                   ggtitle("(b)") +
                  theme(plot.title = element_text(face = "bold", size = 18),
                        legend.title = element_text(face = "bold", size = 14),
                        legend.text = element_text(size = 14),
                        axis.title = element_text(face = "bold", size = 14, margin = margin(t = 5, b = 10)),
                        axis.text = element_text(size = 14),
                        plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt")) +
                   xlim(0.00,0.50)
ind_plot





### Plot Individual r_ind values (deviations from the global intercept mean) to identify the Q1 and Q3 individuals
fit %>%
  spread_draws(r_ind[ind,]) %>%
  # add the grand mean to the group-specific deviations
  mutate(mu = r_ind) %>%
  ungroup() %>%
  mutate(ind = str_replace_all(ind, "[.]", " ")) %>% 
  
  # plot
  ggplot(aes(x = mu, y = reorder(ind, mu))) +
  geom_vline(xintercept = -0.00194926, color = "#839496", size = 1) +
  geom_vline(xintercept = c(-0.24685900, 0.23386625), color = "#839496", linetype = 2) +
  stat_halfeye(.width = .5, size = 2/3, fill = "#859900") +
  labs(x = "Individual Deviations from Global Mean",
       y = "Individuals ordered by mean Deviation from Global Mean") +
  ggtitle("Posterior Distribution for Individual Deviations from Global Mean") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        legend.title = element_text(face = "bold", size = 14),
        legend.text = element_text(size = 14),
        axis.title = element_text(face = "bold", size = 14, margin = margin(t = 5, b = 10)),
        axis.text = element_text(size = 14),
        plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt")) 
# Vertical lines in the above ggplot object show Q1 and Q3 at dotted lines while Q2 is at solid line...





# Calculate the quartiles for the r_ind values
ind_df <- fit %>%
                  spread_draws(b_Intercept, r_ind[ind,]) %>%
                  # add the grand mean to the group-specific deviations
                  mutate(mu = b_Intercept + r_ind) %>%
                  ungroup() %>%
                  mutate(ind = str_replace_all(ind, "[.]", " "))

quantile(ind_df$r_ind, prob = c(0, 0.25, 0.50, 0.75, 1.00))





### Then, plot the random intercept's distribution (b_Intercept + r_ind) for the Q1 individual (Mr. 300) and the Q3 individual (Ms. 349)
ind_df <- filter(ind_df, ind == c("300", "349", "21")) # a bug in the packages requires I add an individual and then remove them
ind_df <- filter(ind_df, ind == c("300", "349"))
ind_df$quartile <- ifelse(ind_df$ind == 300, "300 (Q1 Ind.)", "349 (Q3 Ind.)")

quart_int <- ggplot(ind_df, aes(x = r_ind, fill = factor(quartile))) +
                    stat_halfeye(slab_alpha = .5) +
                    labs(x = "Deviation from Global Mean",
                         y = "Posterior Density",
                         fill = "Individual") +
                    scale_fill_manual(values = c("darkorange", "darkblue"))+
            ggtitle("(c)") + 
            theme(plot.title = element_text(face = "bold", size = 18),
                  legend.title = element_text(face = "bold", size = 14),
                  legend.text = element_text(size = 14),
                  axis.title = element_text(face = "bold", size = 14, margin = margin(t = 5, b = 10)),
                  axis.text = element_text(size = 14),
                  plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt")) 
quart_int



ind_frugivory <- ind_pred + ind_plot + quart_int + plot_layout(ncol = 3, nrow = 1) + facet_grid() + plot_annotation(title = "Frugivory Across Individuals", theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 20, margin = margin(t = 20, b = 30))))

ind_frugivory





### Create Summary Table
ind_df %>%
  filter(ind %in% c(300, 349)) %>%
  group_by(ind) %>%
  summarize(mean = mean(mu),
            lower_ci = quantile(mu, 0.025),
            upper_ci = quantile(mu, 0.975))



data <- read_excel("~/Graduate Research/Dissertation/Data/GT Scat Dissections/gt.frugivory.xlsx", sheet = "data")

# Specify categorical variables as factors
data$ind <- as.factor(data$ind.5samp)
data$season <- as.factor(data$season)
data$town <- as.factor(data$town)
data$season <- relevel(data$season, ref = "Winter")
data$town <- relevel(data$town, ref = "E")



### Average values for each season
# group data by treatment
grouped_data <- group_by(data, season)

# compute mean and standard deviation of score for each group
summary_data <- summarize(grouped_data,
                          mean = mean(prop.fruit[prop.fruit != 0]),
                          sd = sd(prop.fruit[prop.fruit != 0]))
# print summary data
print(summary_data)
}
#=========================================================================================#





#======================EXTRACT VALUES FOR SUMMARY STATISTICS==============================#
{
# Extract posterior draws for certain variables in an array format
print(summary(as_draws_array(fit, variable = "b_season", regex = TRUE)))
print(summary(as_draws_array(fit, variable = "b_town", regex = TRUE)))
print(summary(as_draws_array(fit, variable = "b_sday:season", regex = TRUE)))
summary(as_draws_array(fit, variable = "^b_", regex = TRUE))
print(summary(as_draws_array(fit, variable = "ind", regex = TRUE)), n=30)
print(summary(as_draws_array(fit, regex = TRUE)), n=45)

#season_values <- print(summary(as_draws_array(fit, variable = "b_season", regex = TRUE)))
#converted_season_values <- cbind(season_values[1],apply(season_values[2:7], 2, plogis), season_values[8])
#converted_season_values

#season_slope_values <- print(summary(as_draws_array(fit, variable = "b_sday:season", regex = TRUE)))
#converted_season_slope_values <- cbind(season_slope_values[1],apply(season_slope_values[2:7], 2, plogis), season_slope_values[8])
#converted_season_slope_values

#town_values <- print(summary(as_draws_array(fit, variable = "b_town", regex = TRUE)))
#converted_town_values <- cbind(town_values[1],apply(town_values[2:7], 2, plogis), town_values[8])
#converted_town_values





### OBTAIN POSTERIOR ESTIMATES AND CALCULATE PROBABILITY OF DIRECTION
btwn_seas_summary <- mfx_summary
{
  # Filter the dataframe for contrast value of "Fall - Winter"
  filtered_mfx_Fall <- mfx[mfx$contrast == "Fall - Winter", ]
  # Further filter to include only "mean.PS.cons" observations greater than 0
  filtered_mfx_positive_Fall <- filtered_mfx_Fall[filtered_mfx_Fall$draw > 0, ]
  # Count the number of observations
  num_mfx_positive_Fall <- nrow(filtered_mfx_positive_Fall)
  # Count the total number of observations for "Fall - Winter"
  total_mfx_observations_Fall <- nrow(filtered_mfx_Fall)
  # Calculate the ratio
  mfx_ratio_Fall <- num_mfx_positive_Fall / total_mfx_observations_Fall
  # Print the result
  cat("Probability of Direction for contrast 'Fall - Winter':", mfx_ratio_Fall, "\n")
  
  
  # Filter the dataframe for contrast value of "Spring - Winter"
  filtered_mfx_Spring <- mfx[mfx$contrast == "Spring - Winter", ]
  # Further filter to include only "mean.PS.cons" observations greater than 0
  filtered_mfx_positive_Spring <- filtered_mfx_Spring[filtered_mfx_Spring$draw > 0, ]
  # Count the number of observations
  num_mfx_positive_Spring <- nrow(filtered_mfx_positive_Spring)
  # Count the total number of observations for "Spring - Winter"
  total_mfx_observations_Spring <- nrow(filtered_mfx_Spring)
  # Calculate the ratio
  mfx_ratio_Spring <- num_mfx_positive_Spring / total_mfx_observations_Spring
  # Print the result
  cat("Probability of Direction for contrast 'Spring - Winter':", mfx_ratio_Spring, "\n")
  
  
  
  # Filter the dataframe for contrast value of "Summer - Winter"
  filtered_mfx_Summer <- mfx[mfx$contrast == "Summer - Winter", ]
  # Further filter to include only "mean.PS.cons" observations greater than 0
  filtered_mfx_positive_Summer <- filtered_mfx_Summer[filtered_mfx_Summer$draw > 0, ]
  # Count the number of observations
  num_mfx_positive_Summer <- nrow(filtered_mfx_positive_Summer)
  # Count the total number of observations for "Summer - Winter"
  total_mfx_observations_Summer <- nrow(filtered_mfx_Summer)
  # Calculate the ratio
  mfx_ratio_Summer <- num_mfx_positive_Summer / total_mfx_observations_Summer
  # Print the result
  cat("Probability of Direction for contrast 'Summer - Winter':", mfx_ratio_Summer, "\n")
  }
btwn_seas_summary[1:3,]
write.csv(btwn_seas_summary[1:3,], "C:/Users/adria/Downloads/btwn_seas_summary.csv")



wthn_seas_summary <- nfx_summary
{
# Filter the dataframe for contrast value of "Spring"
  filtered_nfx_Spring <- nfx[nfx$season == "Spring", ]
  # Further filter to include only "mean.PS.cons" observations greater than 0
  filtered_nfx_positive_Spring <- filtered_nfx_Spring[filtered_nfx_Spring$draw > 0, ]
  # Count the number of observations
  num_nfx_positive_Spring <- nrow(filtered_nfx_positive_Spring)
  # Count the total number of observations for "Spring"
  total_nfx_observations_Spring <- nrow(filtered_nfx_Spring)
  # Calculate the ratio
  nfx_ratio_Spring <- num_nfx_positive_Spring / total_nfx_observations_Spring
  # Print the result
  cat("Probability of Direction for contrast 'Spring':", nfx_ratio_Spring, "\n")
  
  
  
  # Filter the dataframe for contrast value of "Summer"
  filtered_nfx_Summer <- nfx[nfx$season == "Summer", ]
  # Further filter to include only "mean.PS.cons" observations greater than 0
  filtered_nfx_positive_Summer <- filtered_nfx_Summer[filtered_nfx_Summer$draw > 0, ]
  # Count the number of observations
  num_nfx_positive_Summer <- nrow(filtered_nfx_positive_Summer)
  # Count the total number of observations for "Summer"
  total_nfx_observations_Summer <- nrow(filtered_nfx_Summer)
  # Calculate the ratio
  nfx_ratio_Summer <- num_nfx_positive_Summer / total_nfx_observations_Summer
  # Print the result
  cat("Probability of Direction for contrast 'Summer':", nfx_ratio_Summer, "\n")
  
  
  
  # Filter the dataframe for contrast value of "Fall"
  filtered_nfx_Fall <- nfx[nfx$season == "Fall", ]
  # Further filter to include only "mean.PS.cons" observations greater than 0
  filtered_nfx_positive_Fall <- filtered_nfx_Fall[filtered_nfx_Fall$draw > 0, ]
  # Count the number of observations
  num_nfx_positive_Fall <- nrow(filtered_nfx_positive_Fall)
  # Count the total number of observations for "Fall"
  total_nfx_observations_Fall <- nrow(filtered_nfx_Fall)
  # Calculate the ratio
  nfx_ratio_Fall <- num_nfx_positive_Fall / total_nfx_observations_Fall
  # If ratio is < 0.50 Subtract it from 1.00 and print the result
  cat("Probability of Direction for contrast 'Fall':", 1-nfx_ratio_Fall, "\n")
  
  
  
  # Filter the dataframe for contrast value of "Winter"
  filtered_nfx_Winter <- nfx[nfx$season == "Winter", ]
  # Further filter to include only "mean.PS.cons" observations greater than 0
  filtered_nfx_positive_Winter <- filtered_nfx_Winter[filtered_nfx_Winter$draw > 0, ]
  # Count the number of observations
  num_nfx_positive_Winter <- nrow(filtered_nfx_positive_Winter)
  # Count the total number of observations for "Winter"
  total_nfx_observations_Winter <- nrow(filtered_nfx_Winter)
  # Calculate the ratio
  nfx_ratio_Winter <- num_nfx_positive_Winter / total_nfx_observations_Winter
  # If ratio is < 0.50 Subtract it from 1.00 and print the result
  cat("Probability of Direction for contrast 'Winter':", 1-nfx_ratio_Winter, "\n")
  }
wthn_seas_summary[1:4,]
write.csv(wthn_seas_summary[1:4,], "C:/Users/adria/Downloads/wthn_seas_summary.csv")



btwn_town_summary <- ofx_summary
{
  # Filter the dataframe for contrast value of "S - E"
  filtered_ofx_South <- ofx[ofx$contrast == "S - E", ]
  # Further filter to include only "mean.PS.cons" observations greater than 0
  filtered_ofx_positive_South <- filtered_ofx_South[filtered_ofx_South$draw > 0, ]
  # Count the number of observations
  num_ofx_positive_South <- nrow(filtered_ofx_positive_South)
  # Count the total number of observations for "S - E"
  total_ofx_observations_South <- nrow(filtered_ofx_South)
  # Calculate the ratio
  ofx_ratio_South <- num_ofx_positive_South / total_ofx_observations_South
  # Print the result
  cat("Probability of Direction for contrast 'S - E':", ofx_ratio_South, "\n")
  
  
  # Filter the dataframe for contrast value of "W - E"
  filtered_ofx_West <- ofx[ofx$contrast == "W - E", ]
  # Further filter to include only "mean.PS.cons" observations greater than 0
  filtered_ofx_positive_West <- filtered_ofx_West[filtered_ofx_West$draw > 0, ]
  # Count the number of observations
  num_ofx_positive_West <- nrow(filtered_ofx_positive_West)
  # Count the total number of observations for "W - E"
  total_ofx_observations_West <- nrow(filtered_ofx_West)
  # Calculate the ratio
  ofx_ratio_West <- num_ofx_positive_West / total_ofx_observations_West
  # Print the result
  cat("Probability of Direction for contrast 'W - E':", ofx_ratio_West, "\n")
  }
btwn_town_summary[1:2,]
write.csv(btwn_town_summary[1:2,], "C:/Users/adria/Downloads/btwn_town_summary.csv")


### PD CALCULATIONS IN CURLY BRACES ARE FOR 3-WAY INTERACTION
{
btwn_seas_by_town_summary <- sfx_summary
{
  # Filter the dataframe for contrast value of "Fall - Winter"
  filtered_sfx_Fall_E <- sfx[sfx$contrast == "mean(Fall) - mean(Winter)" & grepl("E", sfx$town), ]
  # Further filter to include only "mean.PS.cons" observations greater than 0
  filtered_sfx_positive_Fall_E <- filtered_sfx_Fall_E[filtered_sfx_Fall_E$draw > 0, ]
  # Count the number of observations
  num_sfx_positive_Fall_E <- nrow(filtered_sfx_positive_Fall_E)
  # Count the total number of observations for "Fall - Winter"
  total_sfx_observations_Fall_E <- nrow(filtered_sfx_Fall_E)
  # Calculate the ratio
  sfx_ratio_Fall_E <- num_sfx_positive_Fall_E / total_sfx_observations_Fall_E
  # Print the result
  cat("Probability of Direction for contrast 'Fall - Winter' in East Tortoise Town", sfx_ratio_Fall_E, "\n")
  
  
  
  # Filter the dataframe for contrast value of "Fall - Winter"
  filtered_sfx_Fall_S <- sfx[sfx$contrast == "mean(Fall) - mean(Winter)" & grepl("S", sfx$town), ]
  # Further filter to include only "mean.PS.cons" observations greater than 0
  filtered_sfx_positive_Fall_S <- filtered_sfx_Fall_S[filtered_sfx_Fall_S$draw > 0, ]
  # Count the number of observations
  num_sfx_positive_Fall_S <- nrow(filtered_sfx_positive_Fall_S)
  # Count the total number of observations for "Fall - Winter"
  total_sfx_observations_Fall_S <- nrow(filtered_sfx_Fall_S)
  # Calculate the ratio
  sfx_ratio_Fall_S <- num_sfx_positive_Fall_S / total_sfx_observations_Fall_S
  # Print the result
  cat("Probability of Direction for contrast 'Fall - Winter' in South Tortoise Town", sfx_ratio_Fall_S, "\n")
  
  
  
  # Filter the dataframe for contrast value of "Fall - Winter"
  filtered_sfx_Fall_W <- sfx[sfx$contrast == "mean(Fall) - mean(Winter)" & grepl("W", sfx$town), ]
  # Further filter to include only "mean.PW.cons" observations greater than 0
  filtered_sfx_positive_Fall_W <- filtered_sfx_Fall_W[filtered_sfx_Fall_W$draw > 0, ]
  # Count the number of observations
  num_sfx_positive_Fall_W <- nrow(filtered_sfx_positive_Fall_W)
  # Count the total number of observations for "Fall - Winter"
  total_sfx_observations_Fall_W <- nrow(filtered_sfx_Fall_W)
  # Calculate the ratio
  sfx_ratio_Fall_W <- num_sfx_positive_Fall_W / total_sfx_observations_Fall_W
  # Print the result
  cat("Probability of Direction for contrast 'Fall - Winter' in West Tortoise Town", sfx_ratio_Fall_W, "\n")
  
  
  
  # Filter the dataframe for contrast value of "Spring - Winter"
  filtered_sfx_Spring_E <- sfx[sfx$contrast == "mean(Spring) - mean(Winter)" & grepl("E", sfx$town), ]
  # Further filter to include only "mean.PS.cons" observations greater than 0
  filtered_sfx_positive_Spring_E <- filtered_sfx_Spring_E[filtered_sfx_Spring_E$draw > 0, ]
  # Count the number of observations
  num_sfx_positive_Spring_E <- nrow(filtered_sfx_positive_Spring_E)
  # Count the total number of observations for "Spring - Winter"
  total_sfx_observations_Spring_E <- nrow(filtered_sfx_Spring_E)
  # Calculate the ratio
  sfx_ratio_Spring_E <- num_sfx_positive_Spring_E / total_sfx_observations_Spring_E
  # Print the result
  cat("Probability of Direction for contrast 'Spring - Winter' in East Tortoise Town", sfx_ratio_Spring_E, "\n")
  
  
  
  # Filter the dataframe for contrast value of "Spring - Winter"
  filtered_sfx_Spring_S <- sfx[sfx$contrast == "mean(Spring) - mean(Winter)" & grepl("S", sfx$town), ]
  # Further filter to include only "mean.PS.cons" observations greater than 0
  filtered_sfx_positive_Spring_S <- filtered_sfx_Spring_S[filtered_sfx_Spring_S$draw > 0, ]
  # Count the number of observations
  num_sfx_positive_Spring_S <- nrow(filtered_sfx_positive_Spring_S)
  # Count the total number of observations for "Spring - Winter"
  total_sfx_observations_Spring_S <- nrow(filtered_sfx_Spring_S)
  # Calculate the ratio
  sfx_ratio_Spring_S <- num_sfx_positive_Spring_S / total_sfx_observations_Spring_S
  # If ratio is < 0.50 Subtract it from 1.00 and print the result
  cat("Probability of Direction for contrast 'Spring - Winter' in South Tortoise Town", 1-sfx_ratio_Spring_S, "\n")
  
  
  
  # Filter the dataframe for contrast value of "Spring - Winter"
  filtered_sfx_Spring_W <- sfx[sfx$contrast == "mean(Spring) - mean(Winter)" & grepl("W", sfx$town), ]
  # Further filter to include only "mean.PW.cons" observations greater than 0
  filtered_sfx_positive_Spring_W <- filtered_sfx_Spring_W[filtered_sfx_Spring_W$draw > 0, ]
  # Count the number of observations
  num_sfx_positive_Spring_W <- nrow(filtered_sfx_positive_Spring_W)
  # Count the total number of observations for "Spring - Winter"
  total_sfx_observations_Spring_W <- nrow(filtered_sfx_Spring_W)
  # Calculate the ratio
  sfx_ratio_Spring_W <- num_sfx_positive_Spring_W / total_sfx_observations_Spring_W
  # Print the result
  cat("Probability of Direction for contrast 'Spring - Winter' in West Tortoise Town", sfx_ratio_Spring_W, "\n")
  
  
  
  
  
  # Filter the dataframe for contrast value of "Summer - Winter"
  filtered_sfx_Summer_E <- sfx[sfx$contrast == "mean(Summer) - mean(Winter)" & grepl("E", sfx$town), ]
  # Further filter to include only "mean.PS.cons" observations greater than 0
  filtered_sfx_positive_Summer_E <- filtered_sfx_Summer_E[filtered_sfx_Summer_E$draw > 0, ]
  # Count the number of observations
  num_sfx_positive_Summer_E <- nrow(filtered_sfx_positive_Summer_E)
  # Count the total number of observations for "Summer - Winter"
  total_sfx_observations_Summer_E <- nrow(filtered_sfx_Summer_E)
  # Calculate the ratio
  sfx_ratio_Summer_E <- num_sfx_positive_Summer_E / total_sfx_observations_Summer_E
  # Print the result
  cat("Probability of Direction for contrast 'Summer - Winter' in East Tortoise Town", sfx_ratio_Summer_E, "\n")
  
  
  
  # Filter the dataframe for contrast value of "Summer - Winter"
  filtered_sfx_Summer_S <- sfx[sfx$contrast == "mean(Summer) - mean(Winter)" & grepl("S", sfx$town), ]
  # Further filter to include only "mean.PS.cons" observations greater than 0
  filtered_sfx_positive_Summer_S <- filtered_sfx_Summer_S[filtered_sfx_Summer_S$draw > 0, ]
  # Count the number of observations
  num_sfx_positive_Summer_S <- nrow(filtered_sfx_positive_Summer_S)
  # Count the total number of observations for "Summer - Winter"
  total_sfx_observations_Summer_S <- nrow(filtered_sfx_Summer_S)
  # Calculate the ratio
  sfx_ratio_Summer_S <- num_sfx_positive_Summer_S / total_sfx_observations_Summer_S
  # Print the result
  cat("Probability of Direction for contrast 'Summer - Winter' in South Tortoise Town", sfx_ratio_Summer_S, "\n")
  
  
  
  # Filter the dataframe for contrast value of "Summer - Winter"
  filtered_sfx_Summer_W <- sfx[sfx$contrast == "mean(Summer) - mean(Winter)" & grepl("W", sfx$town), ]
  # Further filter to include only "mean.PW.cons" observations greater than 0
  filtered_sfx_positive_Summer_W <- filtered_sfx_Summer_W[filtered_sfx_Summer_W$draw > 0, ]
  # Count the number of observations
  num_sfx_positive_Summer_W <- nrow(filtered_sfx_positive_Summer_W)
  # Count the total number of observations for "Summer - Winter"
  total_sfx_observations_Summer_W <- nrow(filtered_sfx_Summer_W)
  # Calculate the ratio
  sfx_ratio_Summer_W <- num_sfx_positive_Summer_W / total_sfx_observations_Summer_W
  # Print the result
  cat("Probability of Direction for contrast 'Summer - Winter' in West Tortoise Town", sfx_ratio_Summer_W, "\n")
  }
btwn_seas_by_town_summary[1:9,4:9]
write.csv(btwn_seas_by_town_summary[1:9,4:9], "C:/Users/adria/Downloads/btwn_seas_by_town_summary.csv")
                          
                          
wthn_seas_by_town_summary <-tfx_summary
{
  # Filter the dataframe for contrast value of "Fall"
  filtered_tfx_Fall_E <- tfx[tfx$season == "Fall" & grepl("E", tfx$town), ]
  # Further filter to include only "mean.PS.cons" observations greater than 0
  filtered_tfx_positive_Fall_E <- filtered_tfx_Fall_E[filtered_tfx_Fall_E$draw > 0, ]
  # Count the number of observations
  num_tfx_positive_Fall_E <- nrow(filtered_tfx_positive_Fall_E)
  # Count the total number of observations for "Fall"
  total_tfx_observations_Fall_E <- nrow(filtered_tfx_Fall_E)
  # Calculate the ratio
  tfx_ratio_Fall_E <- num_tfx_positive_Fall_E / total_tfx_observations_Fall_E
  # Print the result
  cat("Probability of Direction for contrast 'Fall' in East Tortoise Town", tfx_ratio_Fall_E, "\n")
  
  
  
  # Filter the dataframe for contrast value of "Fall"
  filtered_tfx_Fall_S <- tfx[tfx$season == "Fall" & grepl("S", tfx$town), ]
  # Further filter to include only "mean.PS.cons" observations greater than 0
  filtered_tfx_positive_Fall_S <- filtered_tfx_Fall_S[filtered_tfx_Fall_S$draw > 0, ]
  # Count the number of observations
  num_tfx_positive_Fall_S <- nrow(filtered_tfx_positive_Fall_S)
  # Count the total number of observations for "Fall"
  total_tfx_observations_Fall_S <- nrow(filtered_tfx_Fall_S)
  # Calculate the ratio
  tfx_ratio_Fall_S <- num_tfx_positive_Fall_S / total_tfx_observations_Fall_S
  # Print the result
  cat("Probability of Direction for contrast 'Fall' in South Tortoise Town", tfx_ratio_Fall_S, "\n")
  
  
  
  # Filter the dataframe for contrast value of "Fall"
  filtered_tfx_Fall_W <- tfx[tfx$season == "Fall" & grepl("W", tfx$town), ]
  # Further filter to include only "mean.PW.cons" observations greater than 0
  filtered_tfx_positive_Fall_W <- filtered_tfx_Fall_W[filtered_tfx_Fall_W$draw > 0, ]
  # Count the number of observations
  num_tfx_positive_Fall_W <- nrow(filtered_tfx_positive_Fall_W)
  # Count the total number of observations for "Fall"
  total_tfx_observations_Fall_W <- nrow(filtered_tfx_Fall_W)
  # Calculate the ratio
  tfx_ratio_Fall_W <- num_tfx_positive_Fall_W / total_tfx_observations_Fall_W
  # If ratio is < 0.50 Subtract it from 1.00 and print the result
  cat("Probability of Direction for contrast 'Fall' in West Tortoise Town", 1-tfx_ratio_Fall_W, "\n")
  
  
  
  # Filter the dataframe for contrast value of "Spring"
  filtered_tfx_Spring_E <- tfx[tfx$season == "Spring" & grepl("E", tfx$town), ]
  # Further filter to include only "mean.PS.cons" observations greater than 0
  filtered_tfx_positive_Spring_E <- filtered_tfx_Spring_E[filtered_tfx_Spring_E$draw > 0, ]
  # Count the number of observations
  num_tfx_positive_Spring_E <- nrow(filtered_tfx_positive_Spring_E)
  # Count the total number of observations for "Spring"
  total_tfx_observations_Spring_E <- nrow(filtered_tfx_Spring_E)
  # Calculate the ratio
  tfx_ratio_Spring_E <- num_tfx_positive_Spring_E / total_tfx_observations_Spring_E
  # Print the result
  cat("Probability of Direction for contrast 'Spring' in East Tortoise Town", tfx_ratio_Spring_E, "\n")
  
  
  
  # Filter the dataframe for contrast value of "Spring"
  filtered_tfx_Spring_S <- tfx[tfx$season == "Spring" & grepl("S", tfx$town), ]
  # Further filter to include only "mean.PS.cons" observations greater than 0
  filtered_tfx_positive_Spring_S <- filtered_tfx_Spring_S[filtered_tfx_Spring_S$draw > 0, ]
  # Count the number of observations
  num_tfx_positive_Spring_S <- nrow(filtered_tfx_positive_Spring_S)
  # Count the total number of observations for "Spring"
  total_tfx_observations_Spring_S <- nrow(filtered_tfx_Spring_S)
  # Calculate the ratio
  tfx_ratio_Spring_S <- num_tfx_positive_Spring_S / total_tfx_observations_Spring_S
  # Print the result
  cat("Probability of Direction for contrast 'Spring' in South Tortoise Town", tfx_ratio_Spring_S, "\n")
  
  
  
  # Filter the dataframe for contrast value of "Spring"
  filtered_tfx_Spring_W <- tfx[tfx$season == "Spring" & grepl("W", tfx$town), ]
  # Further filter to include only "mean.PW.cons" observations greater than 0
  filtered_tfx_positive_Spring_W <- filtered_tfx_Spring_W[filtered_tfx_Spring_W$draw > 0, ]
  # Count the number of observations
  num_tfx_positive_Spring_W <- nrow(filtered_tfx_positive_Spring_W)
  # Count the total number of observations for "Spring"
  total_tfx_observations_Spring_W <- nrow(filtered_tfx_Spring_W)
  # Calculate the ratio
  tfx_ratio_Spring_W <- num_tfx_positive_Spring_W / total_tfx_observations_Spring_W
  # Print the result
  cat("Probability of Direction for contrast 'Spring' in West Tortoise Town", tfx_ratio_Spring_W, "\n")
 
  
  
  # Filter the dataframe for contrast value of "Summer"
  filtered_tfx_Summer_E <- tfx[tfx$season == "Summer" & grepl("E", tfx$town), ]
  # Further filter to include only "mean.PS.cons" observations greater than 0
  filtered_tfx_positive_Summer_E <- filtered_tfx_Summer_E[filtered_tfx_Summer_E$draw > 0, ]
  # Count the number of observations
  num_tfx_positive_Summer_E <- nrow(filtered_tfx_positive_Summer_E)
  # Count the total number of observations for "Summer"
  total_tfx_observations_Summer_E <- nrow(filtered_tfx_Summer_E)
  # Calculate the ratio
  tfx_ratio_Summer_E <- num_tfx_positive_Summer_E / total_tfx_observations_Summer_E
  # Print the result
  cat("Probability of Direction for contrast 'Summer' in East Tortoise Town", tfx_ratio_Summer_E, "\n")
  
  
  
  # Filter the dataframe for contrast value of "Summer"
  filtered_tfx_Summer_S <- tfx[tfx$season == "Summer" & grepl("S", tfx$town), ]
  # Further filter to include only "mean.PS.cons" observations greater than 0
  filtered_tfx_positive_Summer_S <- filtered_tfx_Summer_S[filtered_tfx_Summer_S$draw > 0, ]
  # Count the number of observations
  num_tfx_positive_Summer_S <- nrow(filtered_tfx_positive_Summer_S)
  # Count the total number of observations for "Summer"
  total_tfx_observations_Summer_S <- nrow(filtered_tfx_Summer_S)
  # Calculate the ratio
  tfx_ratio_Summer_S <- num_tfx_positive_Summer_S / total_tfx_observations_Summer_S
  # Print the result
  cat("Probability of Direction for contrast 'Summer' in South Tortoise Town", tfx_ratio_Summer_S, "\n")
  
  
  
  # Filter the dataframe for contrast value of "Summer"
  filtered_tfx_Summer_W <- tfx[tfx$season == "Summer" & grepl("W", tfx$town), ]
  # Further filter to include only "mean.PW.cons" observations greater than 0
  filtered_tfx_positive_Summer_W <- filtered_tfx_Summer_W[filtered_tfx_Summer_W$draw > 0, ]
  # Count the number of observations
  num_tfx_positive_Summer_W <- nrow(filtered_tfx_positive_Summer_W)
  # Count the total number of observations for "Summer"
  total_tfx_observations_Summer_W <- nrow(filtered_tfx_Summer_W)
  # Calculate the ratio
  tfx_ratio_Summer_W <- num_tfx_positive_Summer_W / total_tfx_observations_Summer_W
  # Print the result
  cat("Probability of Direction for contrast 'Summer' in West Tortoise Town", tfx_ratio_Summer_W, "\n")
  
  
  
  # Filter the dataframe for contrast value of "Winter"
  filtered_tfx_Winter_E <- tfx[tfx$season == "Winter" & grepl("E", tfx$town), ]
  # Further filter to include only "mean.PS.cons" observations greater than 0
  filtered_tfx_positive_Winter_E <- filtered_tfx_Winter_E[filtered_tfx_Winter_E$draw > 0, ]
  # Count the number of observations
  num_tfx_positive_Winter_E <- nrow(filtered_tfx_positive_Winter_E)
  # Count the total number of observations for "Winter"
  total_tfx_observations_Winter_E <- nrow(filtered_tfx_Winter_E)
  # Calculate the ratio
  tfx_ratio_Winter_E <- num_tfx_positive_Winter_E / total_tfx_observations_Winter_E
  # If ratio is < 0.50 Subtract it from 1.00 and print the result
  cat("Probability of Direction for contrast 'Winter' in East Tortoise Town", 1-tfx_ratio_Winter_E, "\n")
  
  
  
  # Filter the dataframe for contrast value of "Winter"
  filtered_tfx_Winter_S <- tfx[tfx$season == "Winter" & grepl("S", tfx$town), ]
  # Further filter to include only "mean.PS.cons" observations greater than 0
  filtered_tfx_positive_Winter_S <- filtered_tfx_Winter_S[filtered_tfx_Winter_S$draw > 0, ]
  # Count the number of observations
  num_tfx_positive_Winter_S <- nrow(filtered_tfx_positive_Winter_S)
  # Count the total number of observations for "Winter"
  total_tfx_observations_Winter_S <- nrow(filtered_tfx_Winter_S)
  # Calculate the ratio
  tfx_ratio_Winter_S <- num_tfx_positive_Winter_S / total_tfx_observations_Winter_S
  # If ratio is < 0.50 Subtract it from 1.00 and print the result
  cat("Probability of Direction for contrast 'Winter' in South Tortoise Town", 1-tfx_ratio_Winter_S, "\n")
  
  
  
  # Filter the dataframe for contrast value of "Winter"
  filtered_tfx_Winter_W <- tfx[tfx$season == "Winter" & grepl("W", tfx$town), ]
  # Further filter to include only "mean.PW.cons" observations greater than 0
  filtered_tfx_positive_Winter_W <- filtered_tfx_Winter_W[filtered_tfx_Winter_W$draw > 0, ]
  # Count the number of observations
  num_tfx_positive_Winter_W <- nrow(filtered_tfx_positive_Winter_W)
  # Count the total number of observations for "Winter"
  total_tfx_observations_Winter_W <- nrow(filtered_tfx_Winter_W)
  # Calculate the ratio
  tfx_ratio_Winter_W <- num_tfx_positive_Winter_W / total_tfx_observations_Winter_W
  # If ratio is < 0.50 Subtract it from 1.00 and print the result
  cat("Probability of Direction for contrast 'Winter' in West Tortoise Town", 1-tfx_ratio_Winter_W, "\n")
}
wthn_seas_by_town_summary[1:12,4:9]
write.csv(wthn_seas_by_town_summary[1:12,4:9], "C:/Users/adria/OneDrive/Downloads/wthn_seas_by_town_summary.csv")



btwn_town_by_season_summary <- pfx_summary
{
  # Filter the dataframe for contrast value of "South - East"
  filtered_pfx_S_Winter <- pfx[pfx$contrast == "mean(S) - mean(E)" & grepl("Winter", pfx$season), ]
  # Further filter to include only "mean.PS.cons" observations greater than 0
  filtered_pfx_positive_S_Winter <- filtered_pfx_S_Winter[filtered_pfx_S_Winter$draw > 0, ]
  # Count the number of observations
  num_pfx_positive_S_Winter <- nrow(filtered_pfx_positive_S_Winter)
  # Count the total number of observations for "South - East"
  total_pfx_observations_S_Winter <- nrow(filtered_pfx_S_Winter)
  # Calculate the ratio
  pfx_ratio_S_Winter <- num_pfx_positive_S_Winter / total_pfx_observations_S_Winter
  # Print the result
  cat("Probability of Direction for contrast 'South - East' in Winter", pfx_ratio_S_Winter, "\n")
  
  
  
  # Filter the dataframe for contrast value of "West - East"
  filtered_pfx_W_Winter <- pfx[pfx$contrast == "mean(W) - mean(E)" & grepl("Winter", pfx$season), ]
  # Further filter to include only "mean.PS.cons" observations greater than 0
  filtered_pfx_positive_W_Winter <- filtered_pfx_W_Winter[filtered_pfx_W_Winter$draw > 0, ]
  # Count the number of observations
  num_pfx_positive_W_Winter <- nrow(filtered_pfx_positive_W_Winter)
  # Count the total number of observations for "West - East"
  total_pfx_observations_W_Winter <- nrow(filtered_pfx_W_Winter)
  # Calculate the ratio
  pfx_ratio_W_Winter <- num_pfx_positive_W_Winter / total_pfx_observations_W_Winter
  # If ratio is < 0.50 Subtract it from 1.00 and print the result
  cat("Probability of Direction for contrast 'West - East' in Winter", 1-pfx_ratio_W_Winter, "\n")
  
  
  
  # Filter the dataframe for contrast value of "South - East"
  filtered_pfx_S_Fall <- pfx[pfx$contrast == "mean(S) - mean(E)" & grepl("Fall", pfx$season), ]
  # Further filter to include only "mean.PS.cons" observations greater than 0
  filtered_pfx_positive_S_Fall <- filtered_pfx_S_Fall[filtered_pfx_S_Fall$draw > 0, ]
  # Count the number of observations
  num_pfx_positive_S_Fall <- nrow(filtered_pfx_positive_S_Fall)
  # Count the total number of observations for "South - East"
  total_pfx_observations_S_Fall <- nrow(filtered_pfx_S_Fall)
  # Calculate the ratio
  pfx_ratio_S_Fall <- num_pfx_positive_S_Fall / total_pfx_observations_S_Fall
  # Print the result
  cat("Probability of Direction for contrast 'South - East' in Fall", pfx_ratio_S_Fall, "\n")
  
  
  
  # Filter the dataframe for contrast value of "West - East"
  filtered_pfx_W_Fall <- pfx[pfx$contrast == "mean(W) - mean(E)" & grepl("Fall", pfx$season), ]
  # Further filter to include only "mean.PS.cons" observations greater than 0
  filtered_pfx_positive_W_Fall <- filtered_pfx_W_Fall[filtered_pfx_W_Fall$draw > 0, ]
  # Count the number of observations
  num_pfx_positive_W_Fall <- nrow(filtered_pfx_positive_W_Fall)
  # Count the total number of observations for "West - East"
  total_pfx_observations_W_Fall <- nrow(filtered_pfx_W_Fall)
  # Calculate the ratio
  pfx_ratio_W_Fall <- num_pfx_positive_W_Fall / total_pfx_observations_W_Fall
  # Print the result
  cat("Probability of Direction for contrast 'West - East' in Fall", pfx_ratio_W_Fall, "\n")
  
  
  
  # Filter the dataframe for contrast value of "South - East"
  filtered_pfx_S_Spring <- pfx[pfx$contrast == "mean(S) - mean(E)" & grepl("Spring", pfx$season), ]
  # Further filter to include only "mean.PS.cons" observations greater than 0
  filtered_pfx_positive_S_Spring <- filtered_pfx_S_Spring[filtered_pfx_S_Spring$draw > 0, ]
  # Count the number of observations
  num_pfx_positive_S_Spring <- nrow(filtered_pfx_positive_S_Spring)
  # Count the total number of observations for "South - East"
  total_pfx_observations_S_Spring <- nrow(filtered_pfx_S_Spring)
  # Calculate the ratio
  pfx_ratio_S_Spring <- num_pfx_positive_S_Spring / total_pfx_observations_S_Spring
  # Print the result
  cat("Probability of Direction for contrast 'South - East' in Spring", pfx_ratio_S_Spring, "\n")
  
  
  
  # Filter the dataframe for contrast value of "West - East"
  filtered_pfx_W_Spring <- pfx[pfx$contrast == "mean(W) - mean(E)" & grepl("Spring", pfx$season), ]
  # Further filter to include only "mean.PS.cons" observations greater than 0
  filtered_pfx_positive_W_Spring <- filtered_pfx_W_Spring[filtered_pfx_W_Spring$draw > 0, ]
  # Count the number of observations
  num_pfx_positive_W_Spring <- nrow(filtered_pfx_positive_W_Spring)
  # Count the total number of observations for "West - East"
  total_pfx_observations_W_Spring <- nrow(filtered_pfx_W_Spring)
  # Calculate the ratio
  pfx_ratio_W_Spring <- num_pfx_positive_W_Spring / total_pfx_observations_W_Spring
  # If ratio is < 0.50 Subtract it from 1.00 and print the result
  cat("Probability of Direction for contrast 'West - East' in Spring", 1-pfx_ratio_W_Spring, "\n")
  
  
  
  # Filter the dataframe for contrast value of "South - East"
  filtered_pfx_S_Summer <- pfx[pfx$contrast == "mean(S) - mean(E)" & grepl("Summer", pfx$season), ]
  # Further filter to include only "mean.PS.cons" observations greater than 0
  filtered_pfx_positive_S_Summer <- filtered_pfx_S_Summer[filtered_pfx_S_Summer$draw > 0, ]
  # Count the number of observations
  num_pfx_positive_S_Summer <- nrow(filtered_pfx_positive_S_Summer)
  # Count the total number of observations for "South - East"
  total_pfx_observations_S_Summer <- nrow(filtered_pfx_S_Summer)
  # Calculate the ratio
  pfx_ratio_S_Summer <- num_pfx_positive_S_Summer / total_pfx_observations_S_Summer
  # Print the result
  cat("Probability of Direction for contrast 'South - East' in Summer", pfx_ratio_S_Summer, "\n")
  
  
  
  # Filter the dataframe for contrast value of "West - East"
  filtered_pfx_W_Summer <- pfx[pfx$contrast == "mean(W) - mean(E)" & grepl("Summer", pfx$season), ]
  # Further filter to include only "mean.PS.cons" observations greater than 0
  filtered_pfx_positive_W_Summer <- filtered_pfx_W_Summer[filtered_pfx_W_Summer$draw > 0, ]
  # Count the number of observations
  num_pfx_positive_W_Summer <- nrow(filtered_pfx_positive_W_Summer)
  # Count the total number of observations for "West - East"
  total_pfx_observations_W_Summer <- nrow(filtered_pfx_W_Summer)
  # Calculate the ratio
  pfx_ratio_W_Summer <- num_pfx_positive_W_Summer / total_pfx_observations_W_Summer
  # Print the result
  cat("Probability of Direction for contrast 'West - East' in Summer", pfx_ratio_W_Summer, "\n")
  }
btwn_town_by_season_summary[1:8,4:9]
write.csv(btwn_town_by_season_summary[1:8,4:9], "C:/Users/adria/OneDrive/Downloads/btwn_town_by_season_summary.csv")
}
}
#=========================================================================================#





#=============================ENDO/FIFR GLMM TESTS========================================#
  # Load data
  data <- read_excel("~/Graduate Research/Dissertation/Data/GT Scat Dissections/gt.frugivory.xlsx", sheet = "data")
  
  # Specify categorical variables as factors
  data$ind <- as.factor(data$ind.5samp)
  data$w.season <- as.factor(data$w.season)
  data$season <- as.factor(data$season)
  data$town <- as.factor(data$town)
  data$w.season <- relevel(data$w.season, ref = "Dry")
  data$season <- relevel(data$season, ref = "Winter")
  data$town <- relevel(data$town, ref = "E")
  

fit.endo <- brm(endo.species ~ prop.fruit + (1|ind),
                family = negbinomial(),
                data=data,
                chains=4, 
                iter=8000, 
                warmup=4000, 
                cores=14, 
                seed=1234, 
                backend="cmdstanr",
                save_pars = save_pars(all = TRUE),
                control = list(max_treedepth=15))
summary(fit.endo)

### PLOT POSTERIOR PREDICTIVE VALUES
{
  pred_endo <- predictions(fit.endo,
                           newdata = datagrid(prop.fruit = seq(0,1, by=0.01)), 
                           re_formula = NULL, 
                           type = "response",
                           allow_new_levels = TRUE,
                           ndraws = 4000)
  head(pred_endo)
  
  pred_endo <- posteriordraws(pred_endo)
  
  head(pred_endo)
  
  glmm.endo <- ggplot(pred_endo, aes(x = prop.fruit, y = draw)) + ylim(0, 10) +
    stat_lineribbon() +
    scale_fill_brewer(palette = "Purples") +
    geom_point(data = data, aes(x = prop.fruit, y = endo.species), inherit.aes = FALSE) +
    labs(x = "Proportion of Fecal Volume made of Fruit",
         y = "Number of Endozoochorous Species Dispersed",
         title = "",
         fill = "Credible Interval") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
          legend.title = element_text(face = "bold", size = 14),
          legend.text = element_text(size = 14),
          axis.title = element_text(face = "bold", size = 14, margin = margin(t = 5, b = 10)),
          axis.text = element_text(size = 14),
          plot.margin = margin(t = 20, r = 10, b = 10, l = 10, unit = "pt")) +
    scale_y_continuous(breaks = 0:10)
  glmm.endo
}

### EXTRACT MARGINAL EFFECTS AND PLOT
{
  endo.fx <- avg_slopes(fit.endo, variables="prop.fruit")
  endo.fx
  
  endo.fx <- posterior_draws(endo.fx)
  
  
  
  endo_slope_post <- ggplot(endo.fx, aes(x = draw, fill = factor(term))) +
    stat_halfeye(slab_alpha = .75) +
    labs(x = "Marginal Effect of Frugivory on Dispersal of Endozoochorous Species",
         y = "Posterior Density",
         fill = "Dispersal Syndrome") +
    scale_fill_manual(values = c("purple4"),
                      labels = "Endozoochory") +
    geom_vline(xintercept = 0.00, linetype = "dashed", color = "black") +
    ggtitle("") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
          legend.title = element_text(face = "bold", size = 14),
          legend.text = element_text(size = 14),
          axis.title = element_text(face = "bold", size = 14, margin = margin(t = 5, b = 10)),
          axis.text = element_text(size = 14),
          plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt"))
  endo_slope_post
}
glmm.endo.post <- glmm.endo + endo_slope_post + plot_annotation(title = "Effect of Frugivory on Dispersal of Endozoochorous Species", theme = theme(plot.title = element_text(hjust = 0.5, vjust = -1.5, face = "bold", size = 22)))
glmm.endo.post
#=========================================================================================#





#===============================LOGRATIO ANALYSIS=========================================#
{
  ### PERFORM LOGRATIO ANALYSIS
  # Load data
  lra.data <- read.csv("~/Graduate Research/Dissertation/Data/GT Scat Dissections/lra.data.csv", fileEncoding = "UTF-8-BOM")
  
  # Specify categorical variables as factors
  lra.data$ind <- as.factor(lra.data$ind)
  lra.data$town <- as.factor(lra.data$town)
  lra.data$season <- as.factor(lra.data$season)
  
  ### Fruit importance in diet analysis ###
  library(easyCODA)
  library(dplyr)
  comp <- data.frame(lra.data)
  comp.nz <- filter(comp, prop.fruit > 0)
  
  ### Perform LRA and PCA in curly braces below
  {
    ###  Input non-zero frugivory data and run Log Ratio Analysis to ID top fruit species that explain most variance
    PF <- comp.nz
    PF[1:4,1:7]
    #      Season      14:0 14:1(n-5)    i-15:0    a-15:0
    # 1 B5 winter 13.854102 0.2025212 1.1903286 0.4463732
    # 2 B6 winter 11.826581 0.1479822 1.2358517 0.4599448
    # 3 B7 winter  6.457139 0.0000000 0.7680038 0.2481243
    
    ### save first two columns and then remove them from FA data frame, then converted to a matrix
    ind  <- PF[,1]
    season <- PF[,2] 
    town <- PF[,3] 
    PF     <- PF[,-(1:4)]
    colnames(PF) <- c("Byrsonima lucida", "Cassytha filiformis", "Coccothrinax argentata", "Guettarda scabra", "Lantana camara", "Metopium toxiferum", "Miconia bicolor", "Momordica charantia", "Morinda royoc", "Mosiera longipes", "Opuntia austrina", "Physalis walteri", "Quercus pumila", "Sabal palmetto", "Serenoa repens", "Vaccinium myrsinites")
    PF     <- as.matrix(PF)
    dim(PF)
    # [1] 42 40
    
    ### average proportions
    round(colMeans(PF),2)
    
    ### replace zeros with NAs in new object FA_NA and find minimum positive FA values
    PF_NA <- PF
    PF_NA[PF==0] <- NA
    PFmin <- apply(PF_NA, 2, min, na.rm=T)
    
    ### FA0 has 0s replaced with half the minimum value for each FA
    PF0 <- PF
    for(j in 1:ncol(PF)) {
      for(i in 1:nrow(PF)) {
        if(PF[i,j]==0) PF0[i,j] <- 0.5 * PFmin[j]
      }
    }
    
    ### reclose (i.e. renormalize) the data
    PF0 <- PF0 / rowSums(PF0)
    
    print.ratios <- function(rationames, R2, procr=NA, N=10) {
      # function prints ratios and the corresponding R2, optionally Procrustes correlations
      # prints first 10 ratios by default  
      # split names of parts in ratio into PF1 and PF2
      # notice that the ratios can be reported as PF1/PF2 or PF2/PF1  
      foo    <- strsplit(rationames, "/")
      parts  <- matrix(unlist(foo), ncol=2, byrow=TRUE)
      df   <- as.data.frame(parts)[1:N,]
      if(is.na(procr)) {
        df <- cbind(df, round(100*R2[1:N], 2))
        colnames(df) <- c("PF1", "PF2","R2")
      }
      if(!is.na(procr)) {
        df <- cbind(df, round(100*R2[1:N], 2), round(procr[1:N], 3))
        colnames(df) <- c("PF1", "PF2", "R2","Procrustes")
      }
      print(df[1:N,])
    }
    
    
    
    PF.LRA <- LRA(PF0, weight=TRUE)
    PF.CLR <- CLR(PF0, weight=FALSE)
    TOT.LRVAR <- LR.VAR(PF.CLR, vars = TRUE)
    TOT.LRVAR$LRtotvar
    TOT.LRVAR$LRvars
    
    CLR.vars <- LR.VAR(PF.CLR, vars = TRUE)$LRvars
    
    
    PF.step1 <- STEP(PF0, nsteps = 1, top=10)
    print.ratios(PF.step1$names.top, PF.step1$R2.top)
    
    logratios1 <- PF.step1$logratios.top[,6]
    numratios1 <- PF.step1$ratios.top[6,]
    
    
    
    PF.step2 <- STEP(PF0, nsteps = 1, top=10, previous=logratios1)
    print.ratios(PF.step2$names.top, PF.step2$R2.top)
    
    logratios2 <- cbind(logratios1, PF.step2$logratios.top[,4])
    numratios2 <- rbind(numratios1, PF.step2$ratios.top[4,])
    
    
    
    PF.step3 <- STEP(PF0, nsteps = 1, top=10, previous=logratios2)
    print.ratios(PF.step3$names.top, PF.step3$R2.top,N=10)
    
    logratios3 <- cbind(logratios2, PF.step3$logratios.top[,3])
    numratios3 <- rbind(numratios2, PF.step3$ratios.top[3,])
    
    
    PF.step4 <- STEP(PF0, nsteps = 1, top=10, previous=logratios3)
    print.ratios(PF.step4$names.top, PF.step4$R2.top,N=10)
    
    logratios4 <- cbind(logratios3, PF.step4$logratios.top[,3])
    numratios4 <- rbind(numratios3, PF.step4$ratios.top[3,])
    
    
    
    rownames(numratios4) <- paste("Step",1:4,sep="")
    colnames(numratios4) <- c("PF1","PF2")
    finalratios <- as.data.frame(cbind(numratios4, 
                                       Ratio=paste(colnames(PF0)[numratios4[,1]],"/",colnames(PF0)[numratios4[,2]],sep="")))
    finalratios
    
    colnames(logratios4) <- finalratios[,3]
    
    
    partsinratios <- sort(unique(as.numeric(numratios4)))
    colnames(PF0)[partsinratios]
    
    
    plot(colMeans(PF0), CLR.vars, type="n")
    text(colMeans(PF0), CLR.vars, labels=colnames(PF0), col="red")
    
    PF0.lra <- LRA(PF0, weight = TRUE)
    
    PF0.ccc <- PF0.lra$colcoord * sqrt(PF0.lra$colmass)
    PF0.ctr <- (PF0.ccc[,1]^2 * PF0.lra$sv[1]^2 + PF0.ccc[,2]^2 * PF0.lra$sv[2]^2) / 
      (PF0.lra$sv[1]^2 + PF0.lra$sv[2]^2) > 1/ncol(PF0)
    
    PF0.lra$colcoord <- PF0.lra$colcoord[PF0.ctr,]
    PF0.lra$colmass  <- PF0.lra$colmass[PF0.ctr]
    PF0.lra$colnames <- PF0.lra$colnames[PF0.ctr]
    
    
    season.num <- as.numeric(season)
    season.col <- c("orange","darkgreen","darkred","blue")
    season.pch <- c(23,22,24, 25)
    season.cex <- c(2,2,2,2)
    
    town.num <- as.numeric(town)
    town.shape <- c(21, 21, 21) 
    
    PF0.rpc <- PF0.lra$rowpcoord
    
    finalratios

    par(mfrow= c(1,2))
    par(mar=c(4.2,4,4.5,2), mgp=c(2,0.7,0), font.lab=2, cex.axis=0.8)
    # Define unique shapes for each tortoise town
     # Replace with the desired shapes for each town level
    
    # Plot of Seasonal Effect
    PLOT.LRA(PF0.lra)
    text(PF0.ccc[!PF0.ctr,], labels=colnames(PF0)[!PF0.ctr], col="pink", cex=0.60)
    points(PF0.rpc, pch=town.shape[town.num], col=season.col[season.num], 
           bg=season.col[season.num], cex=1.2, font=4)
    title(main = "LRA of Spatiotemporal Effect")
    legend("topright", legend = c("Winter", "Spring", "Summer", "Fall"), pch = 21, bty = "n", 
           col = c("blue", "darkgreen", "darkred", "darkorange"), pt.bg = c("blue", "darkgreen", "darkred", "darkorange"), title = "Season")
    
    # Add a legend for the tortoise town
    #town.legend <- c("East", "South", "West")  # Replace with the appropriate town level labels
    #legend("bottomright", legend = town.legend, pch = town.shape, bty = "n", col = "black", pt.bg = "black", title = "Site")
    
    
    
    ### the PCA of the 4 ratios involving 5 fruit species
    logratios4.pca <- PCA(logratios4, weight=FALSE)
    logratios4.rpc <- logratios4.pca$rowpcoord

    par(mar=c(4.2,4,4.5,2), mgp=c(2,0.7,0), font.lab=2, cex.axis=0.8)
    
    # Define unique shapes for each tortoise town
    town.shape <- c(21, 21, 21)  # Replace with the desired shapes for each town level
    
    # Plot of PCA
    PLOT.PCA(logratios4.pca, map="contribution", axes.inv=c(1,-1), rescale = 2)
    # change second axis direction
    logratios4.rpc[,2] <- -logratios4.rpc[,2]
    ### Add samples as coloured symbols for each season
    points(logratios4.rpc, pch=town.shape[town.num], col=season.col[season.num], 
           bg=season.col[season.num], cex=1.2, font=2)
    title(main = "PCA of Spatiotemporal Effect")
    legend("topright", legend = c("Winter", "Spring", "Summer", "Fall"), pch = 21, bty = "n", 
           col = c("blue", "darkgreen", "darkred", "darkorange"), pt.bg = c("blue", "darkgreen", "darkred", "darkorange"), title = "Season")
    
    # Add a legend for the tortoise town
    #town.legend <- c("East", "South", "West")  # Replace with the appropriate town level labels
    #legend("bottomright", legend = town.legend, pch = town.shape, bty = "n", col = "black", pt.bg = "black", title = "Site")
    
    
    
    par(mfrow= c(1,1))
    proc.results <- protest(PF0.rpc, logratios4.rpc, permutations = 10000)
    proc.results
    
    #plot(proc.results)
    
    # Calculate the Procrustes correlation
    protest(PF0.rpc, logratios4.rpc, permutations = 10000)$t0
    }
}
#=========================================================================================#
