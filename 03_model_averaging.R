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
  log.records = log(city_summary$tot.records),
  log.human_population = log(city_summary$pop),
  log.gdp = log(city_summary$gdp),
  precipitation_seasonality = environment_data$Precipitaiton_seasonality_mm.year,
  builtup = environment_data$Builtup_percapita_m2.person,
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


PI_global <- betareg(Prop.I ~ ., data = model_data, link = "logit", na.action = na.fail)


#the dredge takes a lot of time, >60k models, use readRDS below
PI_candidates <- dredge(PI_global,rank = "AICc",
extra = list(converged = function(model) as.integer(isTRUE(model$converged))))

# saveRDS(PI_candidates, "tables_out/All_models.rds")
# PI_candidates <- readRDS("tables_out/All_models.rds")

usable_models <- PI_candidates$converged == 1 & is.finite(PI_candidates$AICc)
expected_models <- 2^length(predictors)
if (sum(usable_models) != expected_models) {
  warning(sprintf("Averaging %d of %d candidate models.", sum(usable_models), expected_models))
}



# Full averaging gives terms a coefficient of zero in models that omit them.
PI_average <- model.avg(PI_candidates, subset = usable_models, revised.var = TRUE)
PI_estimates <- coef(PI_average, full = TRUE)
PI_intervals <- confint(PI_average, full = TRUE, level = 0.95)
PI_coef_table <- summary(PI_average)$coefmat.full
PI_sw <- sw(PI_average)

write.csv(PI_coef_table,"tables_out/modl_ave.csv")

PI_labels <- c(
  greenness = "Greenness density",
  log.records = "Number of records (log)",
  log.human_population = "Human population (log)",
  log.gdp = "GDP (log)",
  precipitation_seasonality = "Precip. seasonality",
  builtup = "Per capita built-up area",
  forest = "Forested area",
  city_centre_population = "City centre population",
  temperature_seasonality = "Temp. seasonality",
  crop = "Crop area",
  observed_richness = "Observed city richness",
  gdp_ppp = "GDP PPP",
  temperature = "Mean annual temp.",
  human_development = "Human development",
  elevation = "Avg. elevation",
  precipitation = "Mean annual precip.")

# MuMIn prefixes beta-regression coefficients with their model component.
PI_terms <- sub("^mean[_:]", "", names(PI_estimates))
PI_keep <- PI_terms %in% predictors
PI_coef_names <- names(PI_estimates)[PI_keep]
PI_plot_data <- data.frame(
  term = PI_terms[PI_keep],
  estimate = unname(PI_estimates[PI_keep]),
  lower = PI_intervals[PI_coef_names, 1],
  upper = PI_intervals[PI_coef_names, 2],
  p_value = PI_coef_table[PI_coef_names, "Pr(>|z|)"],
  SW = PI_sw[PI_coef_names]
)


PI_plot_data$label <- unname(PI_labels[PI_plot_data$term])
PI_plot_data$direction <- ifelse(PI_plot_data$estimate < 0, "Negative", "Positive")

PI_forest <- ggplot(PI_plot_data, aes(x = estimate, y = reorder(label, estimate))) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey45") +
  geom_errorbar(aes(xmin = lower, xmax = upper, colour = direction),
                orientation = "y", width = 0.16) +
  geom_point(aes(colour = direction), size = 2.7) +
  geom_text(data = subset(PI_plot_data, p_value < 0.05),
  aes(label = "*"),vjust = 1.3,size = 7,show.legend = FALSE)+
  scale_colour_manual(values = c(Negative = "#458B74", Positive = "#CC6600")) +
  labs(x = "Model-averaged coefficient (95% CI)", y = NULL, colour = NULL,) +
  theme_minimal(base_size = 12)

# ggsave("out/prop_I_model_averaged.pdf", PI_forest, width = 6, height = 8)


#variables with SW > 0.9
big_names <- PI_plot_data$term[which(PI_plot_data$SW > 0.9)]
sw_big <- model_data[, c("Prop.I", big_names), drop = FALSE]

sw_annotation <- function(term) {
  weight <- PI_plot_data$SW[match(term, PI_plot_data$term)]
  label <- if (is.finite(weight)) sprintf("SW = %.2f", weight) else "SW = NA"

  annotate("text", x = Inf, y = Inf, label = label,
           hjust = 1.1, vjust = 1.4, size = 3.2)
}

# These fixed panels use the full model data, regardless of the SW cutoff.
P1<-ggplot(model_data,aes(x=builtup,y=Prop.I))+
  geom_point(colour = "#668B8B", alpha = 0.45, size = 1.4) +
  geom_smooth(method = "lm",formula = y ~ x,se = TRUE,
    level = 0.95,colour = "#17365D") +
  labs(x = "Per capita built-up area", y = "Proportion non-native") +
  sw_annotation("builtup") +
  ylim(0,NA)+
  theme_classic(base_size = 12)
P2<-ggplot(model_data,aes(x=log.human_population,y=Prop.I))+
  geom_point(colour = "#668B8B", alpha = 0.45, size = 1.4) +
  geom_smooth(method = "lm",formula = y ~ x,se = TRUE,
    level = 0.95,colour = "#17365D") +
  labs(x = "Human population (log)", y = "Proportion non-native") +
  sw_annotation("log.human_population") +
  ylim(0,NA)+
  theme_classic(base_size = 12)
P3<-ggplot(model_data,aes(x=elevation,y=Prop.I))+
  geom_point(colour = "#668B8B", alpha = 0.45, size = 1.4) +
  geom_smooth(method = "lm",formula = y ~ x,se = TRUE,
    level = 0.95,colour = "#17365D") +
  labs(x = "Avg. elevation", y = "Proportion non-native") +
  sw_annotation("elevation") +
  ylim(0,NA)+
  theme_classic(base_size = 12)
P4<-ggplot(model_data,aes(x=observed_richness,y=Prop.I))+
  geom_point(colour = "#668B8B", alpha = 0.45, size = 1.4) +
  geom_smooth(method = "lm",formula = y ~ x,se = TRUE,
    level = 0.95,colour = "#17365D") +
  labs(x = "Observed city richness", y = "Proportion non-native") +
  sw_annotation("observed_richness") +
  ylim(0,NA)+
  theme_classic(base_size = 12)
P5<-ggplot(model_data,aes(x=precipitation,y=Prop.I))+
  geom_point(colour = "#668B8B", alpha = 0.45, size = 1.4) +
  geom_smooth(method = "lm",formula = y ~ x,se = TRUE,
    level = 0.95,colour = "#17365D") +
  labs(x = "Mean annual precipitation", y = "Proportion non-native") +
  sw_annotation("precipitation") +
  ylim(0,NA)+
  theme_classic(base_size = 12)
P6<-ggplot(model_data,aes(x=log.records,y=Prop.I))+
  geom_point(colour = "#668B8B", alpha = 0.45, size = 1.4) +
  geom_smooth(method = "lm",formula = y ~ x,se = TRUE,
    level = 0.95,colour = "#17365D") +
  labs(x = "Number of records (log)", y = "Proportion non-native") +
  sw_annotation("log.records") +
  ylim(0,NA)+
  theme_classic(base_size = 12)
P7<-ggplot(model_data,aes(x=temperature,y=Prop.I))+
  geom_point(colour = "#668B8B", alpha = 0.45, size = 1.4) +
  geom_smooth(method = "lm",formula = y ~ x,se = TRUE,
    level = 0.95,colour = "#17365D") +
  labs(x = "Mean annual temperature", y = "Proportion non-native") +
  sw_annotation("temperature") +
  ylim(0,NA)+
  theme_classic(base_size = 12)
P8<-ggplot(model_data,aes(x=temperature_seasonality,y=Prop.I))+
  geom_point(colour = "#668B8B", alpha = 0.45, size = 1.4) +
  geom_smooth(method = "lm",formula = y ~ x,se = TRUE,
    level = 0.95,colour = "#17365D") +
  labs(x = "Temperature seasonality", y = "Proportion non-native") +
  sw_annotation("temperature_seasonality") +
  ylim(0,NA)+
  theme_classic(base_size = 12)

Large_sw <- ((P1 | P2) / (P3 | P4) / (P5 | P6) / (P7 | P8))+
  plot_annotation(tag_levels = "A",
    theme = theme(
      plot.margin = margin(t = 25, r = 5, b = 5, l = 5))) &
  theme(plot.tag = element_text(face = "bold", size = 16),
    plot.tag.position = c(0.1, 1.1))

ggsave("out/prop_I_sw_large.pdf", Large_sw, width = 6, height = 10)


#variables with SW < 0.9 (weak predictors)
weak_names <- PI_plot_data$term[which(PI_plot_data$SW < 0.9)]
sw_weak <- model_data[, c("Prop.I", big_names), drop = FALSE]

# These fixed panels use the full model data, regardless of the SW cutoff.
Pw1<-ggplot(model_data,aes(x=crop,y=Prop.I))+
  geom_point(colour = "#668B8B", alpha = 0.45, size = 1.4) +
  geom_smooth(method = "lm",formula = y ~ x,se = TRUE,
    level = 0.95,colour = "#17365D") +
  labs(x = "Crop area", y = "Proportion non-native") +
  sw_annotation("crop") +
  ylim(0,NA)+
  theme_classic(base_size = 12)
Pw2<-ggplot(model_data,aes(x=forest,y=Prop.I))+
  geom_point(colour = "#668B8B", alpha = 0.45, size = 1.4) +
  geom_smooth(method = "lm",formula = y ~ x,se = TRUE,
    level = 0.95,colour = "#17365D") +
  labs(x = "Forested area", y = "Proportion non-native") +
  sw_annotation("forest") +
  ylim(0,NA)+
  theme_classic(base_size = 12)
Pw3<-ggplot(model_data,aes(x=log.gdp,y=Prop.I))+
  geom_point(colour = "#668B8B", alpha = 0.45, size = 1.4) +
  geom_smooth(method = "lm",formula = y ~ x,se = TRUE,
    level = 0.95,colour = "#17365D") +
  labs(x = "GDP (log)", y = "Proportion non-native") +
  sw_annotation("log.gdp") +
  ylim(0,NA)+
  theme_classic(base_size = 12)
Pw4<-ggplot(model_data,aes(x=greenness,y=Prop.I))+
  geom_point(colour = "#668B8B", alpha = 0.45, size = 1.4) +
  geom_smooth(method = "lm",formula = y ~ x,se = TRUE,
    level = 0.95,colour = "#17365D") +
  labs(x = "Greenness density", y = "Proportion non-native") +
  sw_annotation("greenness") +
  ylim(0,NA)+
  theme_classic(base_size = 12)
Pw5<-ggplot(model_data,aes(x=gdp_ppp,y=Prop.I))+
  geom_point(colour = "#668B8B", alpha = 0.45, size = 1.4) +
  geom_smooth(method = "lm",formula = y ~ x,se = TRUE,
    level = 0.95,colour = "#17365D") +
  labs(x = "GDP PPP", y = "Proportion non-native") +
  sw_annotation("gdp_ppp") +
  ylim(0,NA)+
  theme_classic(base_size = 12)
Pw6<-ggplot(model_data,aes(x=precipitation_seasonality,y=Prop.I))+
  geom_point(colour = "#668B8B", alpha = 0.45, size = 1.4) +
  geom_smooth(method = "lm",formula = y ~ x,se = TRUE,
    level = 0.95,colour = "#17365D") +
  labs(x = "Precip. seasonality", y = "Proportion non-native") +
  sw_annotation("precipitation_seasonality") +
  ylim(0,NA)+
  theme_classic(base_size = 12)
Pw7<-ggplot(model_data,aes(x=city_centre_population,y=Prop.I))+
  geom_point(colour = "#668B8B", alpha = 0.45, size = 1.4) +
  geom_smooth(method = "lm",formula = y ~ x,se = TRUE,
    level = 0.95,colour = "#17365D") +
  labs(x = "City centre population", y = "Proportion non-native") +
  sw_annotation("city_centre_population") +
  ylim(0,NA)+
  theme_classic(base_size = 12)
Pw8<-ggplot(model_data,aes(x=human_development,y=Prop.I))+
  geom_point(colour = "#668B8B", alpha = 0.45, size = 1.4) +
  geom_smooth(method = "lm",formula = y ~ x,se = TRUE,
    level = 0.95,colour = "#17365D") +
  labs(x = "Human development", y = "Proportion non-native") +
  sw_annotation("human_development") +
  ylim(0,NA)+
  theme_classic(base_size = 12)

weak_sw <- ((Pw1 | Pw2) / (Pw3 | Pw4) / (Pw5 | Pw6) / (Pw7 | Pw8))+
  plot_annotation(tag_levels = "A",
    theme = theme(
      plot.margin = margin(t = 25, r = 5, b = 5, l = 5))) &
  theme(plot.tag = element_text(face = "bold", size = 16),
    plot.tag.position = c(0.1, 1.1))

ggsave("out/prop_I_sw_waek.pdf", weak_sw, width = 6, height = 10)
