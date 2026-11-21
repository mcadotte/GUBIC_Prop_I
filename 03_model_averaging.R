# Run after 02_prop_I.R from the project R directory.
library(betareg)
library(MuMIn)
library(ggplot2)

stopifnot(!anyDuplicated(city.env$ID_HDC_G0_2))
environment_index <- match(city_summary$ID_HDC_G0_2, city.env$ID_HDC_G0_2)
environment_data <- city.env[environment_index, ]

model_data <- data.frame(
  Prop.I = city_summary$Prop.I,
  greenness = environment_data$Greenness_density_unitless,
  records = log(city_summary$tot.records),
  human_population = log(city_summary$pop),
  gdp = log(city_summary$gdp),
  precipitation_seasonality = environment_data$Precipitaiton_seasonality_mm.year,
  builtup = environment_data$Landuse_building_km2,
  forest = environment_data$Landuse_closedforest_km2 +
    environment_data$Landuse_openforest_other_km2,
  city_centre_population = environment_data$Pop_citycentre_num,
  temperature_seasonality = environment_data$Temperature_seasonality_.C,
  crop = environment_data$Landuse_crop_km2,
  observed_richness = city_summary$tot.spp,
  gdp_ppp = environment_data$GDP_total_PPP,
  temperature = environment_data$MAT_.C,
  human_development = environment_data$Human_development_unitless,
  elevation = environment_data$Elevation_mean_m,
  precipitation = environment_data$MAP_mm.year
)

# Use the same cities in every model; coefficients are per predictor SD.
finite_rows <- apply(model_data, 1, function(row) all(is.finite(row)))
model_data <- model_data[finite_rows, ]
predictors <- setdiff(names(model_data), "Prop.I")
model_data[predictors] <- lapply(model_data[predictors], function(x) as.numeric(scale(x)))
stopifnot(all(model_data$Prop.I > 0 & model_data$Prop.I < 1))

PI_global <- betareg(Prop.I ~ ., data = model_data, link = "logit", na.action = na.fail)
stopifnot(isTRUE(PI_global$converged))

PI_candidates <- dredge(PI_global,rank = "AICc",
extra = list(converged = function(model) as.integer(isTRUE(model$converged))))

usable_models <- PI_candidates$converged == 1 & is.finite(PI_candidates$AICc)
expected_models <- 2^length(predictors)
if (sum(usable_models) != expected_models) {
  warning(sprintf("Averaging %d of %d candidate models.", sum(usable_models), expected_models))
}

# Full averaging gives terms a coefficient of zero in models that omit them.
PI_average <- model.avg(PI_candidates, subset = usable_models, revised.var = TRUE)
PI_estimates <- coef(PI_average, full = TRUE)
PI_intervals <- confint(PI_average, full = TRUE, level = 0.95)

PI_labels <- c(
  greenness = "Greenness density",
  records = "Number of records (log)",
  human_population = "Human population (log)",
  gdp = "GDP (log)",
  precipitation_seasonality = "Precip. seasonality",
  builtup = "Built-up area",
  forest = "Forested area",
  city_centre_population = "City centre population",
  temperature_seasonality = "Temp. seasonality",
  crop = "Crop area",
  observed_richness = "Observed city richness",
  gdp_ppp = "GDP PPP",
  temperature = "Mean annual temp.",
  human_development = "Human development",
  elevation = "Avg. elevation",
  precipitation = "Mean annual precip."
)

# MuMIn prefixes beta-regression coefficients with their model component.
PI_terms <- sub("^mean[_:]", "", names(PI_estimates))
PI_keep <- PI_terms %in% predictors
PI_plot_data <- data.frame(
  term = PI_terms[PI_keep],
  estimate = unname(PI_estimates[PI_keep]),
  lower = PI_intervals[names(PI_estimates)[PI_keep], 1],
  upper = PI_intervals[names(PI_estimates)[PI_keep], 2]
)
stopifnot(setequal(PI_plot_data$term, predictors))

PI_plot_data$label <- unname(PI_labels[PI_plot_data$term])
PI_plot_data$direction <- ifelse(PI_plot_data$estimate < 0, "Negative", "Positive")

PI_forest <- ggplot(PI_plot_data, aes(x = estimate, y = reorder(label, estimate))) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey45") +
  geom_errorbar(aes(xmin = lower, xmax = upper, colour = direction),
                orientation = "y", width = 0.16) +
  geom_point(aes(colour = direction), size = 2.7) +
  scale_colour_manual(values = c(Negative = "#458B74", Positive = "#CC6600")) +
  labs(x = "Model-averaged coefficient (95% CI)", y = NULL, colour = NULL,
       subtitle = "Logit scale; coefficients per predictor SD") +
  theme_minimal(base_size = 12)



# ggsave("out/prop_I_model_averaged.pdf", PI_forest, width = 8, height = 6)
