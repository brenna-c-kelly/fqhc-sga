
library(sf)
library(tmap)
library(INLA)
library(dplyr)
library(spdep)
library(haven)
library(tidyr)
library(stringr)
library(data.table)
library(tidycensus)

#individual birth data (weight, year, race, fips, etc)
birth <- read.csv("data/IndividualBirthDatav2.csv")
#contains tract fips, year the site was added (siteaddedt)
dat_2015 <- st_read("data/2015")
#contains annual FQHC density, tract fips
fqhc <- read_dta("FQHC/FQHCdens2008-2014_births2009-2016compiled_12-2019.dta")
length(unique(fqhc$fips))*8 #2348
summary(fqhc)

length(unique(birth$birthyear, birth$censustractcode))

birth <- birth %>%
  filter(estgest < 44) %>%
  mutate(preterm = ifelse(estgest < 37, 1, 0))# %>%
 # mutate(gest_status <- ifelse(estgest > ))
#summary(births_density_wo16$density)

#summary(glm(birthwt ~ matage + anysmoke + hisp_m, data = fhqc, family = "poisson"))

##    dataframe structure
##  tract | year | prop(low birth weight) | density

## start with low_bw ~ space, time

head(dat_2015)
dat_2015$tract_fips <- paste(dat_2015$countyfips, dat_2015$censustrac, sep = "")

fqhc_years <- dat_2015[, c("tract_fips", "siteaddedt")]
fqhc_years <- st_drop_geometry(fqhc_years)
length(unique(fqhc_years$tract_fips))
fqhc_years <- fqhc_years[order(fqhc_years$tract_fips, fqhc_years$siteaddedt),]


#birth data
birth <- birth %>% 
  drop_na(birthwt)
birth$tract_fips <- paste(birth$countyfips, birth$censustractcode, sep = "")

birth$low_bw <- ifelse(birth$birthwt < 2500, 1, 0)
birth$low_bw <- as.numeric(birth$low_bw)
#birth$total[birth$birthwt < 2500] <- 1

summary(birth$low_bw)
names(birth)
birth$tract_fips <- str_pad(birth$tract, 11, pad = "0")
birth$birthyear <- str_pad(birth$birthyear, 2, pad = "0")
birth$birthyear <- paste("20", birth$birthyear, sep = "")

#summarizing birth variables
births_low <- aggregate(birth[, "low_bw"], 
                        by = list(tract = birth$tract_fips,
                                  year = birth$birthyear),
                        FUN = sum)
births_wks <- aggregate(birth[, "wksgest"], 
                       by = list(tract = birth$tract_fips,
                                 year = birth$birthyear),
                       FUN = mean)
pre_bmi <- aggregate(birth[, "prepregbmi"], 
                        by = list(tract = birth$tract_fips,
                                  year = birth$birthyear),
                        FUN = mean)
tnpctsx_agg <- aggregate(birth[, "tenpctsx"], 
                         by = list(tract = birth$tract_fips,
                                   year = birth$birthyear),
                         FUN = mean)
pre_wt <- aggregate(birth[, "prepregwt"], 
                         by = list(tract = birth$tract_fips,
                                   year = birth$birthyear),
                         FUN = mean)
gest_est <- aggregate(birth[, "estgest"], 
                    by = list(tract = birth$tract_fips,
                              year = birth$birthyear),
                    FUN = mean)
maternal_age <- aggregate(birth[, "matage"], 
                      by = list(tract = birth$tract_fips,
                                year = birth$birthyear),
                      FUN = mean)

births_spread <- birth %>%
  pivot_wider(names_from = c(momrace_m), values_from = c(momrace_m))
head(births_spread)

births_spread$`1`[!is.na(births_spread$`1`)] <- 1
births_spread$`1`[is.na(births_spread$`1`)] <- 0
births_spread$`2`[!is.na(births_spread$`2`)] <- 1
births_spread$`2`[is.na(births_spread$`2`)] <- 0
births_spread$`3`[!is.na(births_spread$`3`)] <- 1
births_spread$`3`[is.na(births_spread$`3`)] <- 0
births_spread$`4`[!is.na(births_spread$`4`)] <- 1
births_spread$`4`[is.na(births_spread$`4`)] <- 0
births_spread$`5`[!is.na(births_spread$`5`)] <- 1
births_spread$`5`[is.na(births_spread$`5`)] <- 0
births_spread$`6`[!is.na(births_spread$`6`)] <- 1
births_spread$`6`[is.na(births_spread$`6`)] <- 0
births_spread$`7`[!is.na(births_spread$`7`)] <- 1
births_spread$`7`[is.na(births_spread$`7`)] <- 0
births_spread$`NA`[!is.na(births_spread$`NA`)] <- 1
births_spread$`NA`[is.na(births_spread$`NA`)] <- 0

births_race <- aggregate(births_spread[, 77:84], 
                         by = list(tract = births_spread$tract_fips,
                                   year = births_spread$birthyear),
                         FUN = sum)
names(births_race)
births_race$total_race <- births_race$`1` + births_race$`2` +
  births_race$`3` + births_race$`4` + births_race$`5` +
  births_race$`6` + births_race$`7` + births_race$`NA`
head(births_race)

births_race$prop_1 <- births_race$`1` / births_race$total_race 
births_race$prop_2 <- births_race$`2` / births_race$total_race 
births_race$prop_3 <- births_race$`3` / births_race$total_race 
births_race$prop_4 <- births_race$`4` / births_race$total_race 
births_race$prop_5 <- births_race$`5` / births_race$total_race 
births_race$prop_6 <- births_race$`6` / births_race$total_race 
births_race$prop_7 <- births_race$`7` / births_race$total_race
births_race$prop_NA <- births_race$`NA` / births_race$total_race
str(births_race)
race_props <- aggregate(cbind(prop_1, prop_2, prop_3, prop_4,
                        prop_5, prop_6, prop_7, prop_NA) ~
                    tract + year, data = births_race,
                        FUN = sum, na.rm = TRUE)

missing_race_1 <- c("06037701100", 2013, 0, 0, 0, 0, 0, 0, 0, 0)
missing_race_2 <- c("06037214901", 2016, 0, 0, 0, 0, 0, 0, 0, 0)
race_props <- rbind(race_props, missing_race_1, missing_race_2)


race_count <- aggregate(cbind(`1`, `2`, `3`, `4`,
                              `5`, `6`, `7`, `NA`) ~
                          tract + year, data = births_race,
                        FUN = sum, na.rm = TRUE)

birth_char <- aggregate(cbind(mom_wic, matforborn, multip, hisp_m,
                              hisp_d, anysmoke, pxchtn, pih, pxdiab,
                              gdm, pxhlthis, art, hxpoorob, cerclage, 
                              sti_preg, hepatitis, mfmappt, transfer,
                              matmorb, chorio, normwtmom, momcomorb, 
                              wtflag, lbw, vlbw, multibw,
                              multibw2, ptb, eptb, vptb, mptb, lptb,
                              multiptb, sga10sx, sgalbwsx, agaptbsx,
                              ptbagalbwsx, privins_pnc, medicaid_pnc,
                              latepnc, nopnc) ~ tract_fips + birthyear,
                        data = birth, FUN = sum, na.rm = TRUE)
names(births_low) <- c("tract", "year", "low_wt_births")
names(births_wks) <- c("tract", "year", "wksgest_mean")
names(pre_bmi) <- c("tract", "year", "prepregbmi")
names(tnpctsx_agg) <- c("tract", "year", "tenpctsx")
names(pre_wt) <- c("tract", "year", "prepregwt")
names(gest_est) <- c("tract", "year", "estgest")
names(maternal_age) <- c("tract", "year", "matage")

library(tidyverse)
births_total <- birth %>% 
  group_by(tract_fips, birthyear) %>%
  tally()
head(births_total)
names(births_total) <- c("tract", "year", "n")

df_list <- list(race_count, births_low, births_wks, pre_bmi, #race_props, 
                tnpctsx_agg, pre_wt, gest_est, maternal_age, births_total)
birth_attr <- df_list %>% 
  reduce(full_join, by = c("tract", "year"))

#births_tract$tract <- str_pad(births_tract$tract, 11, pad = "0")
## pause > take birth attributes down to model

# fqhc facilities
fqhc_years$tract_fips <- str_pad(fqhc_years$tract_fips, 11, pad = "0")
births_fqhc <- merge(birth_attr, fqhc_years, by.x = "tract", by.y = "tract_fips", all = TRUE)
summary(births_fqhc)

#births_fhqc$birthyear <- str_pad(births_fhqc$birthyear, 2, pad = "0")
#births_fhqc$birthyear <- paste("20", births_fhqc$birthyear, sep = "")
births_fqhc$year <- as.numeric(births_fqhc$year)
names(fqhc)
fqhc_density <- fqhc[, c("fips", "FQHCdens_d5_2008",
                         "FQHCdens_d5_2009", "FQHCdens_d5_2010",
                         "FQHCdens_d5_2011", "FQHCdens_d5_2012",
                         "FQHCdens_d5_2013", "FQHCdens_d5_2014")]
names(fqhc_density) <- c("tract_fips", "2008", "2009",
                         "2010", "2011", "2012",
                         "2013", "2014")

fqhc_long <- fqhc_density %>%
  gather(year, density, `2008`:`2014`, factor_key = TRUE)

fqhc_long$year <- as.numeric(as.character(fqhc_long$year)) + 1 #lag by 1 year
fqhc_long$tract_fips <- str_pad(fqhc_long$tract_fips, 11, pad = "0")
births_density <- merge(births_fqhc, fqhc_long, by.x = c("tract", "year"), 
                        by.y = c("tract_fips", "year"), all = TRUE) #was .all = TRUE
births_density$n[is.na(births_density$total_births)] <- 0
births_density$low_wt_births[is.na(births_density$low_wt_births)] <- 0
births_density$density[is.na(births_density$density)] <- 0

births_density <- births_density %>%
  drop_na(tract)

table(births_density$year)
births_density_wo16 <- births_density %>%
  filter(year != 2016)
###
# confirm that 2016 has missing tracts, zero births
#  send 2016 missing census tract IDS > Michelle
#   check birth level file
#   make sure it's not a coding issue
ca_geog <- get_decennial(geography = "tract",
                         variables = c("H010001"),
                         year = 2010,
                         state = "CA",
                         geometry = TRUE)
table(births_density$year)

#summary(births_density)

#test <- births_density %>%
#  filter(density == 0)
#test_2 <- births_density %>%
#  filter(density != 0)
#summary(test$low_bw_prop)
#summary(test_2$low_bw_prop)

geom_reduced <- merge(births_density_wo16, ca_geog, by.x = "tract", by.y = "GEOID")

#geom_reduced_a <- merge(test_2, ca_geog, by.x = "tract_fips", by.y = "GEOID")
names(geom_reduced)
geom_reduced <- geom_reduced[, -(3:31)]
#remove empty geometries:
geom_reduced <- st_as_sf(geom_reduced)
geom_reduced = geom_reduced[!st_is_empty(geom_reduced), , drop=TRUE]
geom_reduced <- st_as_sf(geom_reduced)
names(geom_reduced)

#now the other way around
keep_tracts <- unique(geom_reduced$tract)
names(geom_reduced)
birth_attr_red <- births_density_wo16 %>%
  filter(tract %in% keep_tracts) %>%
  distinct(year, tract, .keep_all = TRUE)

geom_reduced <- geom_reduced[geom_reduced$year == 2015,] %>%
  distinct(year, tract, .keep_all = TRUE)

ca_cropped <- st_crop(ca_geog, xmin = -119, xmax = -117.5,
                          ymin = 32, ymax = 35)
plot(st_geometry(ca_cropped))
#plot(st_geometry(ca_geog))
plot(st_geometry(geom_reduced), border = 'blue', add = TRUE)
#plot(st_geometry(geom_reduced_a), border = 'blue', add = TRUE)

## getting expected
# total births by year
births_by_year <- aggregate(birth_attr_red$n, 
                            by = list(year = birth_attr_red$year),
                            FUN = sum, na.rm = TRUE)
birth_attr_red$annual_sum[birth_attr_red$year == 2009] <- births_by_year[births_by_year$year == 2009, "x"]
birth_attr_red$annual_sum[birth_attr_red$year == 2010] <- births_by_year[births_by_year$year == 2010, "x"]
birth_attr_red$annual_sum[birth_attr_red$year == 2011] <- births_by_year[births_by_year$year == 2011, "x"]
birth_attr_red$annual_sum[birth_attr_red$year == 2012] <- births_by_year[births_by_year$year == 2012, "x"]
birth_attr_red$annual_sum[birth_attr_red$year == 2013] <- births_by_year[births_by_year$year == 2013, "x"]
birth_attr_red$annual_sum[birth_attr_red$year == 2014] <- births_by_year[births_by_year$year == 2014, "x"]
birth_attr_red$annual_sum[birth_attr_red$year == 2015] <- births_by_year[births_by_year$year == 2015, "x"]
# total lw births by year 
low_births_by_year <- aggregate(birth_attr_red$low_wt_births, 
                            by = list(year = birth_attr_red$year),
                            FUN = sum, na.rm = TRUE)
birth_attr_red$annual_bw_sum[birth_attr_red$year == 2009] <- low_births_by_year[low_births_by_year$year == 2009, "x"]
birth_attr_red$annual_bw_sum[birth_attr_red$year == 2010] <- low_births_by_year[low_births_by_year$year == 2010, "x"]
birth_attr_red$annual_bw_sum[birth_attr_red$year == 2011] <- low_births_by_year[low_births_by_year$year == 2011, "x"]
birth_attr_red$annual_bw_sum[birth_attr_red$year == 2012] <- low_births_by_year[low_births_by_year$year == 2012, "x"]
birth_attr_red$annual_bw_sum[birth_attr_red$year == 2013] <- low_births_by_year[low_births_by_year$year == 2013, "x"]
birth_attr_red$annual_bw_sum[birth_attr_red$year == 2014] <- low_births_by_year[low_births_by_year$year == 2014, "x"]
birth_attr_red$annual_bw_sum[birth_attr_red$year == 2015] <- low_births_by_year[low_births_by_year$year == 2015, "x"]
# annual proportion
birth_attr_red$prop <- birth_attr_red$annual_bw_sum / birth_attr_red$annual_sum
birth_attr_red$E <- birth_attr_red$n * birth_attr_red$prop
birth_attr_red$SIR <- birth_attr_red$low_wt_births / birth_attr_red$E

table(birth_attr_red$year)
table(geom_reduced$year)



# Model

## Create index
geom_reduced$idarea <- 1:nrow(geom_reduced)

## Queens case
dat_nb <- poly2nb(geom_reduced)

## Write to file
nb2INLA("map_fqhc.adj", dat_nb)
g_fqhc <- inla.read.graph(filename = "map_fqhc.adj")

nyrs <- length(unique(birth_attr_red$year))
nobs <- nrow(geom_reduced)
nyrs*nobs
nrow(birth_attr_red)
## Spatial index
birth_attr_red$idspace <- rep(geom_reduced$idarea, nyrs)
birth_attr_red$idspace1 <- birth_attr_red$idspace

## Temporal index
birth_attr_red$idtime <- birth_attr_red$year - min(birth_attr_red$year, na.rm = TRUE)
table(birth_attr_red$idtime, birth_attr_red$year) #confirming

birth_attr_red <- birth_attr_red[order(
  birth_attr_red$tract,
  birth_attr_red$year
), ]

library(SpatialEpi)
n.strata <- 2
E <- expected(
  population = birth_attr_red$n,
  cases = birth_attr_red$low_wt_births,
  n.strata = 1
)

nyears <- length(unique(birth_attr_red$year))
tractsE <- rep(unique(birth_attr_red$tract),
                 each = nyears)
ntracts <- length(unique(birth_attr_red$tract))
length(unique(birth_attr_red$tract)) * 7
yearsE <- rep(unique(birth_attr_red$year),
              times = ntracts)
dE <- data.frame(geoid = tractsE, year = yearsE, E = E)

birth_attr_red_2 <- merge(birth_attr_red, dE, by.x = c("tract", "year"), by.y = c("geoid", "year"))
# rate model
rs <- sum(birth_attr_red_2$low_wt_births, na.rm = TRUE) / sum(birth_attr_red_2$n, na.rm = TRUE)
# would need to be for each year
rs

birth_attr_red_2$Ei <- birth_attr_red_2$n * rs

summary(birth_attr_red_2)
nrow(birth_attr_red_2)


# models
birth_attr_red_2$Ei[is.na(birth_attr_red_2$Ei)] <- 0
birth_attr_red_2$density_c <- (birth_attr_red_2$density - mean(birth_attr_red_2$density, na.rm = TRUE))

x <- inla(low_wt_births ~ f(idspace, model = "bym", graph = g_fqhc) + 
               f(idspace1, idtime, model = "iid") + idtime + density,
            family = "poisson", data = birth_attr_red_2, E = Ei,
            control.compute = list(dic = TRUE, waic = TRUE),
            control.predictor = list(compute = TRUE))
summary(x)

# time + density
# WAIC 59344.49
# + white slight negative association (-0.003), not credible
# Watanabe-Akaike information criterion (WAIC) ...: 59344.94
birth_attr_red_2$estgest_sc <- birth_attr_red_2$estgest / 10
names(birth_attr_red_2)
x <- inla(low_wt_births ~ f(idspace, model = "bym", graph = g_fqhc) + 
            f(idspace1, idtime, model = "iid") + estgest_sc,
          family = "poisson", data = birth_attr_red_2, E = Ei,
          control.compute = list(dic = TRUE, waic = TRUE),
          control.predictor = list(compute = TRUE))
summary(x)
# + est_gest_sc
# slight negative association (-0.001) between gestational age, not credible
x <- inla(low_wt_births ~ f(idspace, model = "bym", graph = g_fqhc) + 
            f(idspace1, idtime, model = "iid") + wksgest_mean,
          family = "poisson", data = birth_attr_red_2, E = Ei,
          control.compute = list(dic = TRUE, waic = TRUE),
          control.predictor = list(compute = TRUE))
summary(x)

head(dat_full)

hist(dat_full$low)
plot(dat_full$low, dat_full$density)
summary(dat_full$low)
cor.test(dat_full$low, dat_full$density)


formula1 <- low ~ f(idarea, model = "bym", graph = g) +
  f(idarea1, idtime, model = "iid") + idtime
res1 <- inla(formula1,
            family = "poisson", data = dat_full, E = E,
            control.compute = list(dic = TRUE, waic = TRUE),
            control.predictor = list(compute = TRUE))
summary(res1)
#Watanabe-Akaike information criterion (WAIC) ...: 61088.71

formula2 <- low ~ f(idarea, model = "bym", graph = g) + density
res2 <- inla(formula2,
            family = "poisson", data = dat_full, E = E,
            control.compute = list(dic = TRUE, waic = TRUE),
            control.predictor = list(compute = TRUE))
summary(res2)
#Watanabe-Akaike information criterion (WAIC) ...: 61093.51


#adding presence of a clinic (binary)
fhqc_years$flag <- fhqc_years$year_open
fhqc_years$flag[fhqc_years$year_open <= 2008] <- 2009
fhqc_years$flag1 <- 1
test <- merge(dat_full, fhqc_years, by.x = c("geoid", "year"), by.y = c("tract", "flag"), all = TRUE)

test <- test[
  with(test, order(geoid, year)),
  ]

test1 <- test %>%
  group_by(geoid) %>%
  fill(flag1, .direction = "down")
test1$flag1[is.na(test1$flag1)] <- 0
names(test1)


geom_reduced_a <- merge(test1, ca_geog, by.x = "geoid", by.y = "GEOID")
#remove empty geometries:
geom_reduced_a <- st_as_sf(geom_reduced_a)
geom_reduced_a = geom_reduced_a[!st_is_empty(geom_reduced_a), , drop=TRUE]
geom_reduced_a <- st_as_sf(geom_reduced_a)
names(geom_reduced_a)
geom_reduced_a$idarea <- 1:nrow(geom_reduced_a)

## Queens case
dat_nb_a <- poly2nb(geom_reduced_a)

## Write to file
nb2INLA("map_a.adj", dat_nb)
g_a <- inla.read.graph(filename = "map_a.adj")
n.strata <- 2
E <- expected(
  population = test1$births,
  cases = test1$low,
  n.strata = 1
)
test1$flag1 <- as.factor(test1$flag1)
formula2b <- low ~ f(idarea, model = "bym", graph = g_a) + flag1
res2b <- inla(formula2b,
              family = "poisson", data = test1, E = E,
              control.compute = list(dic = TRUE, waic = TRUE),
              control.predictor = list(compute = TRUE))
summary(res2b)
#Watanabe-Akaike information criterion (WAIC) ...: 61915.30
# presence of an operating fqhc doesn't explain low births

summary(fhqc_years)

head(dat_full)

formula2a <- low ~ f(idarea, model = "bym", graph = g) + density_binary
res2a <- inla(formula2a,
             family = "poisson", data = dat_full, E = E,
             control.compute = list(dic = TRUE, waic = TRUE),
             control.predictor = list(compute = TRUE))
summary(res2a)
#Watanabe-Akaike information criterion (WAIC) ...: 61090.63


formula3 <- low ~ f(idarea, model = "bym", graph = g) #+ density
res3 <- inla(formula3,
             family = "poisson", data = dat_full, E = E,
             control.compute = list(dic = TRUE, waic = TRUE),
             control.predictor = list(compute = TRUE))
summary(res3)
#Watanabe-Akaike information criterion (WAIC) ...: 61091.81

dat_full$density_binary[dat_full$density > 36.72] <- 1
dat_full$density_binary[dat_full$density <= 36.72] <- 0




formula4 <- low ~ f(idarea, model = "bym", graph = g) +
  f(idarea1, idtime, model = "iid") + idtime + x

res4 <- inla(formula4,
             family = "poisson", data = dat_full, E = E,
             control.compute = list(dic = TRUE, waic = TRUE),
             control.predictor = list(compute = TRUE))
summary(res1)
#Watanabe-Akaike information criterion (WAIC) ...: 61088.71

map_sf_1 <- merge(
  map_sf, test1,
  by.x = c("tract", "birthyear"),
  by.y = c("geoid", "year")
)
map_sf_1$flag1 <- as.factor(map_sf_1$flag1)
ggplot(map_sf_1) + geom_sf(aes(fill = flag1), size = 0.01) +
  facet_wrap(~birthyear, dir = "h", ncol = 7) +
  ggtitle("Presence of FQHC") + theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  )


names(fhqc_years) <- c("tract", "year_open")
fhqc_years$flag <- 1

test <- merge(dat_full, fhqc_years, by.x = c("geoid", "year"),
              by.y = c("tract", "year_open"),
              all.x = TRUE, all.y = TRUE)
test$flag[is.na(test$flag)] <- 0

test$flag[test$flag == 1] <- test$year

test <- test %>%
  group_by(geoid) %>%
  mutate(present = cumsum(flag>1 & year > flag))

head(test)
     test$year[test$flag == 0 &
            test$year] <- test$year



test <- full_join(dat_full, fhqc_years, 
                       by = c("geoid" = "tract", 
                              "year" = "year_open"))

nrow(dat_full) + nrow(fhqc_years)
head(test)
summary(glm(low ~ density + year + E, data = test))


dat_full$RR <- res$summary.fitted.values[, "mean"]
dat_full$LL <- res$summary.fitted.values[, "0.025quant"]
dat_full$UL <- res$summary.fitted.values[, "0.975quant"]

names(dat_full)
map_sf <- merge(
  geom_reduced, dat_full,
  by.x = c("tract", "birthyear"),
  by.y = c("geoid", "year")
)
library(ggplot2)
names(map_sf)
ggplot(map_sf) + geom_sf(aes(fill = RR), size = 0.1) +
  facet_wrap(~birthyear, dir = "h", ncol = 7) +
  ggtitle("RR") + theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  ) +
  scale_fill_gradient2(
    midpoint = 1, low = "blue", mid = "white", high = "red"
  )

library(gganimate)
library(transformr)
str(map_sf)
s <- ggplot(map_sf) + geom_sf(aes(fill = RR), size = 0.1) +
  theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  ) +
  scale_fill_gradient2(
    midpoint = 1, low = "blue", mid = "white", high = "red"
  ) +
  transition_time(birthyear) +
  labs(title = "Year: {round(frame_time, 0)}")
anim_save(s, animation = last_animation())

#summary(glm(low ~ births + year + density, family = "poisson", data = dat_full))

## Temporal index
dat_full$idtime <- rep(1:nyrs, each = nobs)

str(dat_full)
str(g)
formula <- x ~ f(idspace, model = "bym", graph = g) + idtime
inla.list.models()
hist(dat_full$x)
res <- inla(formula,
            family = "binomial", data = dat_full,# E = E,
            control.predictor = list(compute = TRUE)
)
summary(dat_full$x)
hist(dat_full$x)

summary(glm(low ~ density, data = dat_full, family = "poisson"))




