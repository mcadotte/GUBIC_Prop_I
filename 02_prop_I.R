# Run from the project R directory.
library(ggplot2)
library(patchwork)
library(reshape2)
#dir.create("out", showWarnings = FALSE)

city <- read.csv("data/urban_sf_sgubik.csv")
city.env <- read.csv("data/urban_env.csv")
records <- readRDS("data/gbif_gubic_list_per_city_final2_20241015.rds")

# Include singletons. Cities with no positive observed count have no effort estimate.
observed <- which(records$n_records_urban > 0)
urban_record_totals <- aggregate(
  list(tot.records = records$n_records_urban[observed]),
  by = list(ID_HDC_G0_2 = records$ID_HDC_G0_2[observed]),
  FUN = sum)

city_index <- match(city$ID_HDC_G0_2, urban_record_totals$ID_HDC_G0_2)

city_summary <- data.frame(
  ID_HDC_G0_2 = city$ID_HDC_G0_2,
  pop = city$POP_2015,
  gdp = city$gdp_md_est_2016,
  continent = city$continent,
  not_in_glonaf = city$nsp_not_in_GloNAF,
  intro = city$nsp_naturalized_GloNAF,
  tot.records = urban_record_totals$tot.records[city_index])

city_summary$tot.spp <- city_summary$not_in_glonaf + city_summary$intro
city_summary$Prop.I <- with(city_summary, ifelse(tot.spp > 0, intro / tot.spp, NA_real_))

PI_hist<-ggplot(city_summary, aes(x = Prop.I)) +
  geom_histogram(position = "identity", alpha = 0.7, bins = 30,
       colour = "#000000", fill = "#668B8B") +
  labs(x = "Proportion non-native", y = "Count") +
  theme_classic(base_size = 12)

# ggsave(file.path("out", "prop_I_histogram.pdf"), PI_hist, width = 7, height = 5)

mPI.1<-lm(Prop.I~tot.spp,data=city_summary)
summary(mPI.1)

mPI.2<-lm(Prop.I~log(tot.records),data=city_summary)
summary(mPI.2)

mPI.3<-lm(Prop.I~log(gdp),data=city_summary)
summary(mPI.3)

mPI.4<-lm(Prop.I~log(pop),data=city_summary)
summary(mPI.4)

mPI.5<-lm(Prop.I~as.factor(continent),data=city_summary)
summary(mPI.5)
anova(mPI.5)

# Plot relationships
PI_species <- ggplot(city_summary, aes(x = tot.spp, y = Prop.I)) +
  geom_point(colour = "#668B8B", alpha = 0.45, size = 1.4) +
  geom_smooth(method = "lm",formula = y ~ x,se = TRUE,
    level = 0.95,colour = "#17365D",linetype = "dashed") +
  labs(x = "Total number of species", y = "Proportion non-native") +
  theme_classic(base_size = 12)

PI_records <- ggplot(city_summary, aes(x = log(tot.records), y = Prop.I)) +
  geom_point(colour = "#668B8B", alpha = 0.45, size = 1.4, na.rm = TRUE) +
  geom_smooth(method = "lm",formula = y ~ x,se = TRUE,
    level = 0.95,colour = "#17365D",na.rm = TRUE) +
  labs(x = "Log urban records", y = "Proportion non-native") +
  theme_classic(base_size = 12)

PI_gdp <- ggplot(city_summary, aes(x = log(gdp), y = Prop.I)) +
  geom_point(colour = "#668B8B", alpha = 0.45, size = 1.4) +
  geom_smooth(method = "lm",formula = y ~ x,se = TRUE,
    level = 0.95,colour = "#17365D") +
  labs(x = "Log GDP", y = "Proportion non-native") +
  theme_classic(base_size = 12)

PI_population <- ggplot(city_summary, aes(x = log(pop), y = Prop.I)) +
  geom_point(colour = "#668B8B", alpha = 0.45, size = 1.4) +
  geom_smooth(method = "lm",formula = y ~ x,se = TRUE,
    level = 0.95,colour = "#17365D") +
  labs(x = "Log city population", y = "Proportion non-native") +
  theme_classic(base_size = 12)

PI_continent <- ggplot(city_summary, aes(x = continent, y = Prop.I)) +
  geom_boxplot(fill = "#668B8B") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(x = "Continent", y = "Proportion non-native") +
  theme_classic(base_size = 12)

# Combine after editing the individual panels.
PI_bivariate <- (PI_hist / PI_species / PI_records |
  PI_gdp / PI_population / PI_continent)

# ggsave("out/prop_I_bivariate.pdf", PI_bivariate, width = 10, height = 7)


#predictors
