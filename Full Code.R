library(tidyverse)
library(SpecsVerification)
library(patchwork)

# load data from compressed csv files
fcst = read_csv('/Users/alex/Desktop/pangu_uk_sfc_fcst_1day_v2.csv.gz')
obs = read_csv('/Users/alex/Desktop/pangu_uk_sfc_obs_v2.csv.gz')

# prepare forecast data to be joined with obs data
obs = obs |>
  rename(mslp_pf = mslp,
         u10_pf = u10,
         v10_pf = v10,
         t2m_pf = t2m,
         init_date = date)
pangu_uk = inner_join(fcst, obs, by=c('init_date', 'lon', 'lat')) |>
  rename(mslp_fcst = mslp,
         u10_fcst = u10,
         v10_fcst = v10,
         t2m_fcst = t2m,
         date = target_date) |>
  select(-init_date)
obs = obs |>
  rename(mslp = mslp_pf,
         u10 = u10_pf,
         v10 = v10_pf,
         t2m = t2m_pf,
         date = init_date)
pangu_uk = inner_join(pangu_uk, obs, by=c('date', 'lon', 'lat'))

## Convert temperature to degrees Celsius
if (max(pangu_uk$t2m_fcst) > 100) {
  pangu_uk$t2m_fcst <- pangu_uk$t2m_fcst - 273.15
}
if (max(pangu_uk$t2m) > 100) {
  pangu_uk$t2m <- pangu_uk$t2m - 273.15
}
if (max(pangu_uk$t2m_pf) > 100) {
  pangu_uk$t2m_pf <- pangu_uk$t2m_pf - 273.15
}

## Introduce Wind Speed
pangu_uk <- pangu_uk |> 
  mutate(Ws_fcst = sqrt(pangu_uk$u10_fcst^2 + pangu_uk$v10_fcst^2)) |>
  select(1:which(names(pangu_uk) == 'v10_fcst'), Ws_fcst, everything())
pangu_uk <- pangu_uk |>
  mutate(Ws = sqrt(pangu_uk$u10^2 + pangu_uk$v10^2)) |>
  select(1:which(names(pangu_uk) == 'v10'), Ws, everything())
pangu_uk <- pangu_uk |>
  mutate(Ws_pf = sqrt(pangu_uk$u10_pf^2 + pangu_uk$v10_pf^2)) |>
  select(1:which(names(pangu_uk) == 'v10_pf'), Ws_pf, everything())


## Average of observed, forecast and difference temperature (2m) in July thermal map
Jul_t2m_diff = pangu_uk |>
  filter(month(date) == '7') |>
  group_by(lat, lon) |>
  summarise(t2m_obs = mean(t2m), 
            t2m_fcst = mean(t2m_fcst), 
            t2m_diff = mean(t2m_fcst - t2m))
ggplot(Jul_t2m_diff) + 
  geom_raster(aes(x=lon, y=lat, fill=t2m_obs)) +
  labs(x = 'lon', y = 'lat', fill = 'Observed \ntemperature', 
       title = 'Average of observed temperature (2m) in July for all years') +
  borders(regions=c('UK','Ireland'), colour='black') +
  scale_fill_viridis_c(option='B') +
  theme_minimal() + 
  ggplot(Jul_t2m_diff) + 
  geom_raster(aes(x=lon, y=lat, fill=t2m_fcst)) +
  labs(x = 'lon', y = 'lat', fill = 'Forecast \ntemperature', 
       title = 'Average of forecast temperature (2m) in July for all years') +
  borders(regions=c('UK','Ireland'), colour='black') +
  scale_fill_viridis_c(option='B') +
  theme_minimal() + 
  ggplot(Jul_t2m_diff) + 
  geom_raster(aes(x=lon, y=lat, fill=t2m_diff)) +
  labs(x = 'lon', y = 'lat', fill = 'difference', 
       title = 'Average of difference between observed and forecast temperature (2m) in July') +
  borders(regions=c('UK','Ireland'), colour='black') +
  scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red') +
  theme_minimal()

## Average of observed, forecast and difference temperature (2m) in February thermal map
Feb_t2m_diff = pangu_uk |>
  filter(month(date) == '2') |>
  group_by(lat, lon) |>
  summarise(t2m_obs = mean(t2m), 
            t2m_fcst = mean(t2m_fcst), 
            t2m_diff = mean(t2m_fcst - t2m))
ggplot(Feb_t2m_diff) + 
  geom_raster(aes(x=lon, y=lat, fill=t2m_obs)) +
  labs(x = 'lon', y = 'lat', fill = 'Observed \ntemperature',
       title = 'Average of observed temperature (2m) in February for all years') +
  borders(regions=c('UK','Ireland'), colour='black') +
  scale_fill_viridis_c(option='B') +
  theme_minimal() + 
  ggplot(Feb_t2m_diff) + 
  geom_raster(aes(x=lon, y=lat, fill=t2m_fcst)) +
  labs(x = 'lon', y = 'lat', fill = 'Forecast \ntemperature', 
       title = 'Average of forecast temperature (2m) in February for all years') +
  borders(regions=c('UK','Ireland'), colour='black') +
  scale_fill_viridis_c(option='B') +
  theme_minimal() + 
  ggplot(Feb_t2m_diff) + 
  geom_raster(aes(x=lon, y=lat, fill=t2m_diff)) +
  labs(x = 'lon', y = 'lat', fill = 'difference', 
       title = 'Average of difference between observed and forecast temperature (2m) in February') +
  borders(regions=c('UK','Ireland'), colour='black') +
  scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red') +
  theme_minimal()

## Average of observed and forecast temperature (2m) in specific month each year 
mean_t2m_Jul = pangu_uk |> 
  filter(month(date) == '7') |>
  mutate(year = format(as.Date(date), '%Y')) |>
  group_by(year) |> 
  summarise(obs = mean(t2m), 
            fcst = mean(t2m_fcst), 
            SE = sd(t2m_fcst)/sqrt(n()))
ggplot(mean_t2m_Jul, aes(x = year)) + 
  geom_point(aes(y = obs, color = 'Observed')) +
  geom_point(aes(y = fcst, color = 'Forecast')) +
  geom_errorbar(aes(ymin = fcst - SE, ymax = fcst + SE, color = 'Forecast'), width = 0.3) +
  labs(x = 'Year', y = 'Temperature', color = 'Legend', 
       title = 'Average of observed and forecast temperature (2m) in July for each year') +
  scale_color_manual(values = c('Observed' ='blue', 'Forecast' = 'red')) +
  theme_minimal()

mean_t2m_Feb = pangu_uk |> 
  filter(month(date) == '2') |>
  mutate(year = format(as.Date(date), '%Y')) |>
  group_by(year) |> 
  summarise(obs = mean(t2m), 
            fcst = mean(t2m_fcst), 
            SE = sd(t2m_fcst)/sqrt(n()))
ggplot(mean_t2m_Feb, aes(x = year)) + 
  geom_point(aes(y = obs, color = 'Observed')) +
  geom_point(aes(y = fcst, color = 'Forecast')) +
  geom_errorbar(aes(ymin = fcst - SE, ymax = fcst + SE, color = 'Forecast'), width = 0.3) +
  labs(x = 'Year', y = 'Temperature', color = 'Legend', 
       title = 'Average of observed and forecast temperature (2m) in February for each year') +
  scale_color_manual(values = c('Observed' ='blue', 'Forecast' = 'red')) +
  theme_minimal()

## Average of observed and forecast temperature (2m) for each month
mean_t2m <- pangu_uk |>
  mutate(month = format(as.Date(date), '%m')) |>
  group_by(month) |>
  summarise(obs = mean(t2m), 
            fcst = mean(t2m_fcst), 
            SE = sd(t2m_fcst)/sqrt(n()),
            diff = mean(t2m_fcst - t2m))
ggplot(mean_t2m, aes(x = month)) +
  geom_point(aes(y = obs, color = 'Observed')) +
  geom_point(aes(y = fcst, color = 'Forecast')) +
  geom_errorbar(aes(ymin = fcst - SE, ymax = fcst + SE, color = 'Forecast'), width = 0.2) +
  labs(x = 'Month', y = 'Temperature', color = 'Legend',
       title = 'Average of observed and forecast temperature (2m) for each month') +
  scale_color_manual(values = c('Observed' = 'blue', 'Forecast' = 'red')) +
  theme_minimal()
ggplot(mean_t2m) +
  geom_point(aes(x = month, y = fcst, color = 'Forecast'), size = 2) +
  geom_errorbar(aes(x = month, ymin = fcst - SE, ymax = fcst + SE, color = 'Forecast'), width = 0.2) +
  geom_point(aes(x = month, y = obs, color = 'Observed'), size = 2) +
  theme_minimal() +
  labs(x = 'Month', y = 'Temperature', 
       title = 'Average of observed and forecast temperature (2m) for each month', color = "Legend") +
  scale_color_manual(values = c('Observed' = 'blue', 'Forecast' = 'red')) +
  facet_wrap(~ month, scales = "free")
ggplot(mean_t2m, aes(x = month)) +
  geom_point(aes(y = diff, color = diff > 0)) + 
  geom_hline(yintercept = 0, color = "black") + 
  geom_segment(aes(x = month, xend = month, y = 0, yend = diff, color = diff > 0), linetype = "dashed") + 
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "blue"), guide = FALSE) + 
  labs(x = 'Month', y = 'Difference (temperature)', 
       title = 'Average of difference between forecast and observed temperature (2m) for each month') +
  theme_minimal()


## MSE and skill score (focus on month)
Mse_month_mslp = pangu_uk |> mutate(month = format(as.Date(date), '%m')) |>
  group_by(month) |> 
  summarise(Mse = mean((mslp_fcst - mslp)^2), 
            Mse_mf = mean((mean(mslp) - mslp)^2),
            Mse_pf = mean((mslp_pf - mslp)^2),
            SE = sd((mslp_fcst - mslp)^2)/sqrt(n())) |>
  mutate(SS_mf = 1 - (Mse/Mse_mf), SS_pf = 1 - (Mse/Mse_pf))
Mse_month_mslp
ggplot(Mse_month_mslp) + geom_point(aes(x = month, y = Mse)) +
  geom_errorbar(aes(x = month, ymin = Mse - SE, ymax = Mse + SE), width = 0.2) +
  labs(x = 'Month', y = 'MSE', 
       title = 'The MSE of mslp forecast for each month') +
  theme_minimal() 
ggplot(Mse_month_mslp) + 
  geom_point(aes(x = month, y = Mse_mf, color = 'Mean forecast')) +
  geom_line(aes(x = as.numeric(month), y = Mse_mf, color = 'Mean forecast')) + 
  geom_point(aes(x = month, y = Mse_pf, color = 'Persistence forecast')) +
  geom_line(aes(x = as.numeric(month), y = Mse_pf, color = 'Persistence forecast')) +
  labs(x = 'Month', y = 'MSE', color = 'Legend', 
       title = 'The MSE of mslp mean forecast (equal to the variance of observed mslp) \nand the MSE of mslp persistence forecast for each month') +
  scale_color_manual(values = c('Mean forecast' = 'blue', 'Persistence forecast' = 'red')) + 
  theme_minimal()
ggplot(Mse_month_mslp) + 
  geom_point(aes(x = month, y = SS_mf, color = 'Mean forecast')) + 
  geom_line(aes(x = as.numeric(month), y = SS_mf, color = 'Mean forecast')) + 
  geom_point(aes(x = month, y = SS_pf, color = 'Persistence forecast')) + 
  geom_line(aes(x = as.numeric(month), y = SS_pf, color = 'Persistence forecast')) +
  labs(x = 'Month', y= 'Skill Score', color = 'Legend', 
       title = 'The skill scores of mslp forecast based on mean forecast and persistence forecast for each month') +
  scale_color_manual(values = c('Mean forecast' = 'blue', 'Persistence forecast' = 'red')) +
  theme_minimal()

Mse_month_u10 = pangu_uk |> mutate(month = format(as.Date(date), '%m')) |>
  group_by(month) |> 
  summarise(Mse = mean((u10_fcst - u10)^2), 
            Mse_mf = mean((mean(u10) - u10)^2),
            Mse_pf = mean((u10_pf - u10)^2),
            SE = sd((u10_fcst - u10)^2)/sqrt(n())) |>
  mutate(SS_mf = 1 - (Mse/Mse_mf), SS_pf = 1 - (Mse/Mse_pf))
Mse_month_u10
ggplot(Mse_month_u10) + geom_point(aes(x = month, y = Mse)) +
  geom_errorbar(aes(x = month, ymin = Mse - SE, ymax = Mse + SE), width = 0.2) +
  labs(x = 'Month', y = 'MSE', 
       title = 'The MSE of u10 forecast for each month') +
  theme_minimal() 
ggplot(Mse_month_u10) + 
  geom_point(aes(x = month, y = Mse_mf, color = 'Mean forecast')) +
  geom_line(aes(x = as.numeric(month), y = Mse_mf, color = 'Mean forecast')) + 
  geom_point(aes(x = month, y = Mse_pf, color = 'Persistence forecast')) +
  geom_line(aes(x = as.numeric(month), y = Mse_pf, color = 'Persistence forecast')) +
  labs(x = 'Month', y = 'MSE', color = 'Legend', 
       title = 'The MSE of u10 mean forecast (equal to the variance of observed u10) \nand the MSE of u10 persistence forecast for each month') +
  scale_color_manual(values = c('Mean forecast' = 'blue', 'Persistence forecast' = 'red')) + 
  theme_minimal()
ggplot(Mse_month_u10) + 
  geom_point(aes(x = month, y = SS_mf, color = 'Mean forecast')) + 
  geom_line(aes(x = as.numeric(month), y = SS_mf, color = 'Mean forecast')) + 
  geom_point(aes(x = month, y = SS_pf, color = 'Persistence forecast')) + 
  geom_line(aes(x = as.numeric(month), y = SS_pf, color = 'Persistence forecast')) +
  labs(x = 'Month', y= 'Skill Score', color = 'Legend', 
       title = 'The skill scores of u10 forecast based on mean forecast and persistence forecast for each month') +
  scale_color_manual(values = c('Mean forecast' = 'blue', 'Persistence forecast' = 'red')) +
  theme_minimal()

Mse_month_v10 = pangu_uk |> mutate(month = format(as.Date(date), '%m')) |>
  group_by(month) |> 
  summarise(Mse = mean((v10_fcst - v10)^2), 
            Mse_mf = mean((mean(v10) - v10)^2),
            Mse_pf = mean((v10_pf - v10)^2),
            SE = sd((v10_fcst - v10)^2)/sqrt(n())) |>
  mutate(SS_mf = 1 - (Mse/Mse_mf), SS_pf = 1 - (Mse/Mse_pf))
Mse_month_v10
ggplot(Mse_month_v10) + geom_point(aes(x = month, y = Mse)) +
  geom_errorbar(aes(x = month, ymin = Mse - SE, ymax = Mse + SE), width = 0.2) +
  labs(x = 'Month', y = 'MSE', 
       title = 'The MSE of v10 forecast for each month') +
  theme_minimal() 
ggplot(Mse_month_v10) + 
  geom_point(aes(x = month, y = Mse_mf, color = 'Mean forecast')) +
  geom_line(aes(x = as.numeric(month), y = Mse_mf, color = 'Mean forecast')) + 
  geom_point(aes(x = month, y = Mse_pf, color = 'Persistence forecast')) +
  geom_line(aes(x = as.numeric(month), y = Mse_pf, color = 'Persistence forecast')) +
  labs(x = 'Month', y = 'MSE', color = 'Legend', 
       title = 'The MSE of v10 mean forecast (equal to the variance of observed v10) \nand the MSE of v10 persistence forecast for each month') +
  scale_color_manual(values = c('Mean forecast' = 'blue', 'Persistence forecast' = 'red')) + 
  theme_minimal()
ggplot(Mse_month_v10) + 
  geom_point(aes(x = month, y = SS_mf, color = 'Mean forecast')) + 
  geom_line(aes(x = as.numeric(month), y = SS_mf, color = 'Mean forecast')) + 
  geom_point(aes(x = month, y = SS_pf, color = 'Persistence forecast')) + 
  geom_line(aes(x = as.numeric(month), y = SS_pf, color = 'Persistence forecast')) +
  labs(x = 'Month', y= 'Skill Score', color = 'Legend', 
       title = 'The skill scores of v10 forecast based on mean forecast and persistence forecast for each month') +
  scale_color_manual(values = c('Mean forecast' = 'blue', 'Persistence forecast' = 'red')) +
  theme_minimal()

Mse_month_Ws = pangu_uk |> mutate(month = format(as.Date(date), '%m')) |>
  group_by(month) |> 
  summarise(Mse = mean((Ws_fcst - Ws)^2), 
            Mse_mf = mean((mean(Ws) - Ws)^2),
            Mse_pf = mean((Ws_pf - Ws)^2),
            SE = sd((Ws_fcst - Ws)^2)/sqrt(n())) |>
  mutate(SS_mf = 1 - (Mse/Mse_mf), SS_pf = 1 - (Mse/Mse_pf))
Mse_month_Ws
ggplot(Mse_month_Ws) + geom_point(aes(x = month, y = Mse)) +
  geom_errorbar(aes(x = month, ymin = Mse - SE, ymax = Mse + SE), width = 0.2) +
  labs(x = 'Month', y = 'MSE', 
       title = 'The MSE of Ws forecast for each month') +
  theme_minimal() 
ggplot(Mse_month_Ws) + 
  geom_point(aes(x = month, y = Mse_mf, color = 'Mean forecast')) +
  geom_line(aes(x = as.numeric(month), y = Mse_mf, color = 'Mean forecast')) + 
  geom_point(aes(x = month, y = Mse_pf, color = 'Persistence forecast')) +
  geom_line(aes(x = as.numeric(month), y = Mse_pf, color = 'Persistence forecast')) +
  labs(x = 'Month', y = 'MSE', color = 'Legend', 
       title = 'The MSE of Ws mean forecast (equal to the variance of observed Ws) \nand the MSE of Ws persistence forecast for each month') +
  scale_color_manual(values = c('Mean forecast' = 'blue', 'Persistence forecast' = 'red')) + 
  theme_minimal()
ggplot(Mse_month_Ws) + 
  geom_point(aes(x = month, y = SS_mf, color = 'Mean forecast')) + 
  geom_line(aes(x = as.numeric(month), y = SS_mf, color = 'Mean forecast')) + 
  geom_point(aes(x = month, y = SS_pf, color = 'Persistence forecast')) + 
  geom_line(aes(x = as.numeric(month), y = SS_pf, color = 'Persistence forecast')) +
  labs(x = 'Month', y= 'Skill Score', color = 'Legend', 
       title = 'The skill scores of Ws forecast based on mean forecast and persistence forecast for each month') +
  scale_color_manual(values = c('Mean forecast' = 'blue', 'Persistence forecast' = 'red')) +
  theme_minimal()

Mse_month_t2m = pangu_uk |> mutate(month = format(as.Date(date), '%m')) |>
  group_by(month) |> 
  summarise(Mse = mean((t2m_fcst - t2m)^2), 
            Mse_mf = mean((mean(t2m) - t2m)^2),
            Mse_pf = mean((t2m_pf - t2m)^2),
            SE = sd((t2m_fcst - t2m)^2)/sqrt(n())) |>
  mutate(SS_mf = 1 - (Mse/Mse_mf), SS_pf = 1 - (Mse/Mse_pf))
Mse_month_t2m
ggplot(Mse_month_t2m) + geom_point(aes(x = month, y = Mse)) +
  geom_errorbar(aes(x = month, ymin = Mse - SE, ymax = Mse + SE), width = 0.2) +
  labs(x = 'Month', y = 'MSE', 
       title = 'The MSE of temperature forecast for each month') +
  theme_minimal() 
ggplot(Mse_month_t2m) + 
  geom_point(aes(x = month, y = Mse_mf, color = 'Mean forecast')) +
  geom_line(aes(x = as.numeric(month), y = Mse_mf, color = 'Mean forecast')) + 
  geom_point(aes(x = month, y = Mse_pf, color = 'Persistence forecast')) +
  geom_line(aes(x = as.numeric(month), y = Mse_pf, color = 'Persistence forecast')) +
  labs(x = 'Month', y = 'MSE', color = 'Legend', 
       title = 'The MSE of t2m mean forecast (equal to the variance of observed t2m) and the MSE of t2m persistence forecast for each month') +
  scale_color_manual(values = c('Mean forecast' = 'blue', 'Persistence forecast' = 'red')) + 
  theme_minimal()
ggplot(Mse_month_t2m) + 
  geom_point(aes(x = month, y = SS_mf, color = 'Mean forecast')) + 
  geom_line(aes(x = as.numeric(month), y = SS_mf, color = 'Mean forecast')) + 
  geom_point(aes(x = month, y = SS_pf, color = 'Persistence forecast')) + 
  geom_line(aes(x = as.numeric(month), y = SS_pf, color = 'Persistence forecast')) +
  labs(x = 'Month', y= 'Skill Score', color = 'Legend', 
       title = 'The skill scores of t2m forecast based on mean forecast and persistence forecast for each month') +
  scale_color_manual(values = c('Mean forecast' = 'blue', 'Persistence forecast' = 'red')) +
  theme_minimal()


## MSE and skill score (focus on year)
Mse_year_mslp = pangu_uk |> mutate(year = format(as.Date(date), '%Y')) |>
  group_by(year) |> 
  summarise(Mse = mean((mslp_fcst - mslp)^2), 
            Mse_mf = mean((mean(mslp) - mslp)^2),
            Mse_pf = mean((mslp_pf - mslp)^2),
            SE = sd((mslp_fcst - mslp)^2)/sqrt(n())) |>
  mutate(SS_mf = 1 - (Mse/Mse_mf), SS_pf = 1 - (Mse/Mse_pf))
Mse_year_mslp
ggplot(Mse_year_mslp, aes(x = as.numeric(year), y = Mse)) + geom_line() + geom_point() +
  geom_errorbar(aes(x = as.numeric(year), ymin = Mse - SE, ymax = Mse + SE), width = 0.4) +
  geom_vline(xintercept = 2017.5, linetype = 'dashed', color = 'red') + 
  annotate("text", x = 2017, y = max(Mse_year_mslp$Mse), label = "Before 2017", hjust = 1, vjust = -0.5, color = "red") +
  annotate("text", x = 2018, y = max(Mse_year_mslp$Mse), label = "After 2017", hjust = 0, vjust = -0.5, color = "red") +
  labs(x = 'Year', y = 'MSE', title = 'The MSE of mslp forecast for each year') +
  theme_minimal()
ggplot(Mse_year_mslp, aes(x = as.numeric(year), y = Mse_mf)) + geom_line() + geom_point() +
  labs(x = 'Year', y = 'MSE', title = 'The MSE of mslp mean forecast for each year') +
  theme_minimal() +
  ggplot(Mse_year_mslp, aes(x = as.numeric(year), y = Mse_pf)) + geom_line() + geom_point() +
  labs(x = 'Year', y = 'MSE', title = 'The MSE of mslp persistence forecast for each year') +
  theme_minimal()
ggplot(Mse_year_mslp) +
  geom_point(aes(x = as.numeric(year), y = SS_mf, color = 'Mean forecast')) +
  geom_line(aes(x = as.numeric(year), y = SS_mf, color = 'Mean forecast')) +
  geom_point(aes(x = as.numeric(year), y = SS_pf, color = 'Persistence forecast')) +
  geom_line(aes(x = as.numeric(year), y = SS_pf, color = 'Persistence forecast')) +
  labs(x = 'Year', y = 'Skill Score', color = 'Legend', 
       title = 'The skill scores of mslp forecast based on mean forecast and persistence forecast for each year') +
  scale_color_manual(values = c('Mean forecast' = 'blue', 'Persistence forecast' = 'red')) + 
  geom_vline(xintercept = 2017.5, linetype = 'dashed', color = 'black') +
  theme_minimal()

Mse_year_u10 = pangu_uk |> mutate(year = format(as.Date(date), '%Y')) |>
  group_by(year) |> 
  summarise(Mse = mean((u10_fcst - u10)^2), 
            Mse_mf = mean((mean(u10) - u10)^2),
            Mse_pf = mean((u10_pf - u10)^2),
            SE = sd((u10_fcst - u10)^2)/sqrt(n())) |>
  mutate(SS_mf = 1 - (Mse/Mse_mf), SS_pf = 1 - (Mse/Mse_pf))
Mse_year_u10
ggplot(Mse_year_u10, aes(x = as.numeric(year), y = Mse)) + geom_line() + geom_point() +
  geom_errorbar(aes(x = as.numeric(year), ymin = Mse - SE, ymax = Mse + SE), width = 0.4) +
  geom_vline(xintercept = 2017.5, linetype = 'dashed', color = 'red') + 
  annotate("text", x = 2017, y = max(Mse_year_u10$Mse), label = "Before 2017", hjust = 1, vjust = -0.5, color = "red") +
  annotate("text", x = 2018, y = max(Mse_year_u10$Mse), label = "After 2017", hjust = 0, vjust = -0.5, color = "red") +
  labs(x = 'Year', y = 'MSE', title = 'The MSE of u10 forecast for each year') +
  theme_minimal()
ggplot(Mse_year_u10, aes(x = as.numeric(year), y = Mse_mf)) + geom_line() + geom_point() +
  labs(x = 'Year', y = 'MSE', title = 'The MSE of u10 mean forecast for each year') +
  theme_minimal() +
  ggplot(Mse_year_u10, aes(x = as.numeric(year), y = Mse_pf)) + geom_line() + geom_point() +
  labs(x = 'Year', y = 'MSE', title = 'The MSE of u10 persistence forecast for each year') +
  theme_minimal()
ggplot(Mse_year_u10) +
  geom_point(aes(x = as.numeric(year), y = SS_mf, color = 'Mean forecast')) +
  geom_line(aes(x = as.numeric(year), y = SS_mf, color = 'Mean forecast')) +
  geom_point(aes(x = as.numeric(year), y = SS_pf, color = 'Persistence forecast')) +
  geom_line(aes(x = as.numeric(year), y = SS_pf, color = 'Persistence forecast')) +
  labs(x = 'Year', y = 'Skill Score', color = 'Legend', 
       title = 'The skill scores of u10 forecast based on mean forecast and persistence forecast for each year') +
  scale_color_manual(values = c('Mean forecast' = 'blue', 'Persistence forecast' = 'red')) + 
  geom_vline(xintercept = 2017.5, linetype = 'dashed', color = 'black') +
  theme_minimal()

Mse_year_v10 = pangu_uk |> mutate(year = format(as.Date(date), '%Y')) |>
  group_by(year) |> 
  summarise(Mse = mean((v10_fcst - v10)^2), 
            Mse_mf = mean((mean(v10) - v10)^2),
            Mse_pf = mean((v10_pf - v10)^2),
            SE = sd((v10_fcst - v10)^2)/sqrt(n())) |>
  mutate(SS_mf = 1 - (Mse/Mse_mf), SS_pf = 1 - (Mse/Mse_pf))
Mse_year_v10
ggplot(Mse_year_v10, aes(x = as.numeric(year), y = Mse)) + geom_line() + geom_point() +
  geom_errorbar(aes(x = as.numeric(year), ymin = Mse - SE, ymax = Mse + SE), width = 0.4) +
  geom_vline(xintercept = 2017.5, linetype = 'dashed', color = 'red') + 
  annotate("text", x = 2017, y = max(Mse_year_v10$Mse), label = "Before 2017", hjust = 1, vjust = -0.5, color = "red") +
  annotate("text", x = 2018, y = max(Mse_year_v10$Mse), label = "After 2017", hjust = 0, vjust = -0.5, color = "red") +
  labs(x = 'Year', y = 'MSE', title = 'The MSE of v10 forecast for each year') +
  theme_minimal()
ggplot(Mse_year_v10, aes(x = as.numeric(year), y = Mse_mf)) + geom_line() + geom_point() +
  labs(x = 'Year', y = 'MSE', title = 'The MSE of v10 mean forecast for each year') +
  theme_minimal() +
  ggplot(Mse_year_v10, aes(x = as.numeric(year), y = Mse_pf)) + geom_line() + geom_point() +
  labs(x = 'Year', y = 'MSE', title = 'The MSE of v10 persistence forecast for each year') +
  theme_minimal()
ggplot(Mse_year_v10) +
  geom_point(aes(x = as.numeric(year), y = SS_mf, color = 'Mean forecast')) +
  geom_line(aes(x = as.numeric(year), y = SS_mf, color = 'Mean forecast')) +
  geom_point(aes(x = as.numeric(year), y = SS_pf, color = 'Persistence forecast')) +
  geom_line(aes(x = as.numeric(year), y = SS_pf, color = 'Persistence forecast')) +
  labs(x = 'Year', y = 'Skill Score', color = 'Legend', 
       title = 'The skill scores of v10 forecast based on mean forecast and persistence forecast for each year') +
  scale_color_manual(values = c('Mean forecast' = 'blue', 'Persistence forecast' = 'red')) + 
  geom_vline(xintercept = 2017.5, linetype = 'dashed', color = 'black') +
  theme_minimal()

Mse_year_Ws = pangu_uk |> mutate(year = format(as.Date(date), '%Y')) |>
  group_by(year) |> 
  summarise(Mse = mean((Ws_fcst - Ws)^2), 
            Mse_mf = mean((mean(Ws) - Ws)^2),
            Mse_pf = mean((Ws_pf - Ws)^2),
            SE = sd((Ws_fcst - Ws)^2)/sqrt(n())) |>
  mutate(SS_mf = 1 - (Mse/Mse_mf), SS_pf = 1 - (Mse/Mse_pf))
Mse_year_Ws
ggplot(Mse_year_Ws, aes(x = as.numeric(year), y = Mse)) + geom_line() + geom_point() +
  geom_errorbar(aes(x = as.numeric(year), ymin = Mse - SE, ymax = Mse + SE), width = 0.4) +
  geom_vline(xintercept = 2017.5, linetype = 'dashed', color = 'red') + 
  annotate("text", x = 2017, y = max(Mse_year_Ws$Mse), label = "Before 2017", hjust = 1, vjust = -0.5, color = "red") +
  annotate("text", x = 2018, y = max(Mse_year_Ws$Mse), label = "After 2017", hjust = 0, vjust = -0.5, color = "red") +
  labs(x = 'Year', y = 'MSE', title = 'The MSE of Ws forecast for each year') +
  theme_minimal()
ggplot(Mse_year_Ws, aes(x = as.numeric(year), y = Mse_mf)) + geom_line() + geom_point() +
  labs(x = 'Year', y = 'MSE', title = 'The MSE of Ws mean forecast for each year') +
  theme_minimal() +
  ggplot(Mse_year_Ws, aes(x = as.numeric(year), y = Mse_pf)) + geom_line() + geom_point() +
  labs(x = 'Year', y = 'MSE', title = 'The MSE of Ws persistence forecast for each year') +
  theme_minimal()
ggplot(Mse_year_Ws) +
  geom_point(aes(x = as.numeric(year), y = SS_mf, color = 'Mean forecast')) +
  geom_line(aes(x = as.numeric(year), y = SS_mf, color = 'Mean forecast')) +
  geom_point(aes(x = as.numeric(year), y = SS_pf, color = 'Persistence forecast')) +
  geom_line(aes(x = as.numeric(year), y = SS_pf, color = 'Persistence forecast')) +
  labs(x = 'Year', y = 'Skill Score', color = 'Legend', 
       title = 'The skill scores of Ws forecast based on mean forecast and persistence forecast for each year') +
  scale_color_manual(values = c('Mean forecast' = 'blue', 'Persistence forecast' = 'red')) + 
  geom_vline(xintercept = 2017.5, linetype = 'dashed', color = 'black') +
  theme_minimal()

Mse_year_t2m = pangu_uk |> mutate(year = format(as.Date(date), '%Y')) |>
  group_by(year) |> 
  summarise(Mse = mean((t2m_fcst - t2m)^2), 
            Mse_mf = mean((mean(t2m) - t2m)^2),
            Mse_pf = mean((t2m_pf - t2m)^2),
            SE = sd((t2m_fcst - t2m)^2)/sqrt(n())) |>
  mutate(SS_mf = 1 - (Mse/Mse_mf), SS_pf = 1 - (Mse/Mse_pf))
Mse_year_t2m
ggplot(Mse_year_t2m, aes(x = as.numeric(year), y = Mse)) + geom_line() + geom_point() +
  geom_errorbar(aes(x = as.numeric(year), ymin = Mse - SE, ymax = Mse + SE), width = 0.4) +
  geom_vline(xintercept = 2017.5, linetype = 'dashed', color = 'red') + 
  annotate("text", x = 2017, y = max(Mse_year_t2m$Mse), label = "Before 2017", hjust = 1, vjust = -0.5, color = "red") +
  annotate("text", x = 2018, y = max(Mse_year_t2m$Mse), label = "After 2017", hjust = 0, vjust = -0.5, color = "red") +
  labs(x = 'Year', y = 'MSE', title = 'The MSE of t2m forecast for each year') +
  theme_minimal()
ggplot(Mse_year_t2m, aes(x = as.numeric(year), y = Mse_mf)) + geom_line() + geom_point() +
  labs(x = 'Year', y = 'MSE', title = 'The MSE of t2m mean forecast for each year') +
  theme_minimal() +
  ggplot(Mse_year_t2m, aes(x = as.numeric(year), y = Mse_pf)) + geom_line() + geom_point() +
  labs(x = 'Year', y = 'MSE', title = 'The MSE of t2m persistence forecast for each year') +
  theme_minimal()
ggplot(Mse_year_t2m) +
  geom_point(aes(x = as.numeric(year), y = SS_mf, color = 'Mean forecast')) +
  geom_line(aes(x = as.numeric(year), y = SS_mf, color = 'Mean forecast')) +
  geom_point(aes(x = as.numeric(year), y = SS_pf, color = 'Persistence forecast')) +
  geom_line(aes(x = as.numeric(year), y = SS_pf, color = 'Persistence forecast')) +
  labs(x = 'Year', y = 'Skill Score', color = 'Legend', 
       title = 'The skill scores of t2m forecast based on mean forecast and persistence forecast for each year') +
  scale_color_manual(values = c('Mean forecast' = 'blue', 'Persistence forecast' = 'red')) + 
  geom_vline(xintercept = 2017.5, linetype = 'dashed', color = 'black') +
  theme_minimal()


## Changes in MSE before and after 2017
pangu_Mse <- pangu_uk |>
  mutate(t2m_mse = (t2m_fcst - t2m)^2,
         u10_mse = (u10_fcst - u10)^2,
         v10_mse = (v10_fcst - v10)^2,
         Ws_mse = (Ws_fcst - Ws)^2,
         mslp_mse = (mslp_fcst - mslp)^2) |>
  select(date,t2m_mse, u10_mse, v10_mse, Ws_mse, mslp_mse)
before_2017 <- pangu_Mse |>
  filter((date <= as.Date('2017-12-31')))
after_2017 <- pangu_Mse |>
  filter(date >= as.Date('2018-01-01'))
t_test_t2m <- t.test(before_2017$t2m_mse, after_2017$t2m_mse, var.equal = TRUE) 
print(t_test_t2m)
t_test_u10 <- t.test(before_2017$u10_mse, after_2017$u10_mse, var.equal = TRUE) 
print(t_test_u10)
t_test_v10 <- t.test(before_2017$v10_mse, after_2017$v10_mse, var.equal = TRUE) 
print(t_test_v10)
t_test_Ws <- t.test(before_2017$Ws_mse, after_2017$Ws_mse, var.equal = TRUE) 
print(t_test_Ws)
t_test_mslp <- t.test(before_2017$mslp_mse, after_2017$mslp_mse, var.equal = TRUE) 
print(t_test_mslp)

pangu_mse <- pangu_Mse |>
  mutate(lab = if_else(date <= as.Date('2017-12-31'), 'Mse before 2017', 'Mse after 2017')) |>
  group_by(lab) |>
  summarise(t2m = mean(t2m_mse), u10 = mean(u10_mse), v10 = mean(v10_mse), Ws = mean(Ws_mse), mslp = mean(mslp_mse)) |>
  pivot_longer(cols = c(t2m, u10, v10, Ws, mslp), names_to = "variable", values_to = "value") |>
  pivot_wider(names_from = lab, values_from = value) |>
  select('variable', 'Mse before 2017', 'Mse after 2017') |>
  mutate('difference' = `Mse after 2017` - `Mse before 2017`)
pangu_mse


## MSE for quantile
pangu_uk_t2m <- pangu_uk[, c('date', 'lat', 'lon', 't2m_fcst', 't2m')]
t2m_quan <- quantile(pangu_uk_t2m$t2m, probs = (seq(0, 1, by = 0.1)))
pangu_uk_t2m$quan <- cut(pangu_uk_t2m$t2m, breaks = t2m_quan, 
                         include.lowest = TRUE, labels = FALSE)
Mse_quan_t2m = pangu_uk_t2m |> group_by(quan) |> 
  summarise(Mse = mean((t2m_fcst - t2m)^2), 
            SE = sd((t2m_fcst - t2m)^2)/sqrt(n()))
Mse_all_t2m <- mean((pangu_uk_t2m$t2m_fcst - pangu_uk_t2m$t2m)^2)
ggplot(Mse_quan_t2m) + geom_point(aes(x = factor(quan), y = Mse, group = 1)) + 
  geom_line(aes(x = factor(quan), y = Mse, group = 2)) +
  geom_errorbar(aes(x = factor(quan), ymin = Mse - SE, ymax = Mse + SE), width = 0.2) + 
  geom_hline(yintercept = Mse_all_t2m, linetype = 'dashed', color = 'red') +
  annotate('text', x = max(as.numeric(factor(Mse_quan_t2m$quan))) - 2, y = Mse_all_t2m, 
           label = 'Overall MSE', color = 'red', vjust = -0.5, size = 4) +
  annotate('text', x = 1, y = Mse_all_t2m, label = paste0(round(Mse_all_t2m, 4)), 
           color = 'red', vjust = -0.5, size = 3.5) +
  labs(x = 'Quantile', y = 'MSE', title = 'The MSE of t2m forecast for each quantile') +
  theme_minimal()

pangu_uk_mslp <- pangu_uk[, c('date', 'lat', 'lon', 'mslp_fcst', 'mslp')]
mslp_quan <- quantile(pangu_uk_mslp$mslp, probs = (seq(0, 1, by = 0.1)))
pangu_uk_mslp$quan <- cut(pangu_uk_mslp$mslp, breaks = mslp_quan, 
                         include.lowest = TRUE, labels = FALSE)
Mse_quan_mslp = pangu_uk_mslp |> group_by(quan) |> 
  summarise(Mse = mean((mslp_fcst - mslp)^2), 
            SE = sd((mslp_fcst - mslp)^2)/sqrt(n()))
Mse_all_mslp <- mean((pangu_uk_mslp$mslp_fcst - pangu_uk_mslp$mslp)^2)
ggplot(Mse_quan_mslp) + geom_point(aes(x = factor(quan), y = Mse, group = 1)) + 
  geom_line(aes(x = factor(quan), y = Mse, group = 2)) +
  geom_errorbar(aes(x = factor(quan), ymin = Mse - SE, ymax = Mse + SE), width = 0.2) + 
  geom_hline(yintercept = Mse_all_mslp, linetype = 'dashed', color = 'red') +
  annotate('text', x = max(as.numeric(factor(Mse_quan_mslp$quan))) - 2, y = Mse_all_mslp, 
           label = 'Overall MSE', color = 'red', vjust = -0.5, size = 4) +
  annotate('text', x = 1, y = Mse_all_mslp, label = paste0(round(Mse_all_mslp, 4)), 
           color = 'red', vjust = -0.5, size = 3.5) +
  labs(x = 'Quantile', y = 'MSE', title = 'The MSE of mslp forecast for each quantile') +
  theme_minimal()

pangu_uk_u10 <- pangu_uk[, c('date', 'lat', 'lon', 'u10_fcst', 'u10')] |>
  mutate(sca_u10 = abs(u10))
u10_quan <- quantile(pangu_uk_u10$sca_u10, probs = (seq(0, 1, by = 0.1)))
pangu_uk_u10$quan <- cut(pangu_uk_u10$sca_u10, breaks = u10_quan, 
                         include.lowest = TRUE, labels = FALSE)
Mse_quan_u10 = pangu_uk_u10 |> group_by(quan) |> 
  summarise(Mse = mean((u10_fcst - u10)^2), 
            SE = sd((u10_fcst - u10)^2)/sqrt(n()))
Mse_all_u10 <- mean((pangu_uk_u10$u10_fcst - pangu_uk_u10$u10)^2)
ggplot(Mse_quan_u10) + geom_point(aes(x = factor(quan), y = Mse, group = 1)) + 
  geom_line(aes(x = factor(quan), y = Mse, group = 2)) +
  geom_errorbar(aes(x = factor(quan), ymin = Mse - SE, ymax = Mse + SE), width = 0.2) + 
  geom_hline(yintercept = Mse_all_u10, linetype = 'dashed', color = 'red') +
  annotate('text', x = max(as.numeric(factor(Mse_quan_u10$quan))) - 2, y = Mse_all_u10, 
           label = 'Overall MSE', color = 'red', vjust = -0.5, size = 4) +
  annotate('text', x = 1, y = Mse_all_u10, label = paste0(round(Mse_all_u10, 4)), 
           color = 'red', vjust = -0.5, size = 3.5) +
  labs(x = 'Quantile', y = 'MSE', title = 'The MSE of u10 forecast for each quantile') +
  theme_minimal()

pangu_uk_v10 <- pangu_uk[, c('date', 'lat', 'lon', 'v10_fcst', 'v10')] |>
  mutate(sca_v10 = abs(v10))
v10_quan <- quantile(pangu_uk_v10$sca_v10, probs = (seq(0, 1, by = 0.1)))
pangu_uk_v10$quan <- cut(pangu_uk_v10$sca_v10, breaks = v10_quan, 
                         include.lowest = TRUE, labels = FALSE)
Mse_quan_v10 = pangu_uk_v10 |> group_by(quan) |> 
  summarise(Mse = mean((v10_fcst - v10)^2), 
            SE = sd((v10_fcst - v10)^2)/sqrt(n()))
Mse_all_v10 <- mean((pangu_uk_v10$v10_fcst - pangu_uk_v10$v10)^2)
ggplot(Mse_quan_v10) + geom_point(aes(x = factor(quan), y = Mse, group = 1)) + 
  geom_line(aes(x = factor(quan), y = Mse, group = 2)) +
  geom_errorbar(aes(x = factor(quan), ymin = Mse - SE, ymax = Mse + SE), width = 0.2) + 
  geom_hline(yintercept = Mse_all_v10, linetype = 'dashed', color = 'red') +
  annotate('text', x = max(as.numeric(factor(Mse_quan_v10$quan))) - 2, y = Mse_all_v10, 
           label = 'Overall MSE', color = 'red', vjust = -0.5, size = 4) +
  annotate('text', x = 1, y = Mse_all_v10, label = paste0(round(Mse_all_v10, 4)), 
           color = 'red', vjust = -0.5, size = 3.5) +
  labs(x = 'Quantile', y = 'MSE', title = 'The MSE of v10 forecast for each quantile') +
  theme_minimal()

pangu_uk_Ws <- pangu_uk[, c('date', 'lat', 'lon', 'Ws_fcst', 'Ws')]
Ws_quan <- quantile(pangu_uk_Ws$Ws, probs = (seq(0, 1, by = 0.1)))
pangu_uk_Ws$quan <- cut(pangu_uk_Ws$Ws, breaks = Ws_quan, 
                          include.lowest = TRUE, labels = FALSE)
Mse_quan_Ws = pangu_uk_Ws |> group_by(quan) |> 
  summarise(Mse = mean((Ws_fcst - Ws)^2), 
            SE = sd((Ws_fcst - Ws)^2)/sqrt(n()))
Mse_all_Ws <- mean((pangu_uk_Ws$Ws_fcst - pangu_uk_Ws$Ws)^2)
ggplot(Mse_quan_Ws) + geom_point(aes(x = factor(quan), y = Mse, group = 1)) + 
  geom_line(aes(x = factor(quan), y = Mse, group = 2)) +
  geom_errorbar(aes(x = factor(quan), ymin = Mse - SE, ymax = Mse + SE), width = 0.2) + 
  geom_hline(yintercept = Mse_all_Ws, linetype = 'dashed', color = 'red') +
  annotate('text', x = max(as.numeric(factor(Mse_quan_Ws$quan))) - 2, y = Mse_all_Ws, 
           label = 'Overall MSE', color = 'red', vjust = -0.5, size = 4) +
  annotate('text', x = 1, y = Mse_all_Ws, label = paste0(round(Mse_all_Ws, 4)), 
           color = 'red', vjust = -0.5, size = 3.5) +
  labs(x = 'Quantile', y = 'MSE', title = 'The MSE of Ws forecast for each quantile') +
  theme_minimal()

ggplot(Mse_quan_t2m) + geom_point(aes(x = factor(quan), y = Mse, group = 1)) + 
  geom_line(aes(x = factor(quan), y = Mse, group = 2)) +
  geom_errorbar(aes(x = factor(quan), ymin = Mse - SE, ymax = Mse + SE), width = 0.2) + 
  geom_hline(yintercept = Mse_all_t2m, linetype = 'dashed', color = 'red') +
  annotate('text', x = max(as.numeric(factor(Mse_quan_t2m$quan))) - 2, y = Mse_all_t2m, 
           label = 'Overall MSE', color = 'red', vjust = -0.5, size = 4) +
  annotate('text', x = 1, y = Mse_all_t2m, label = paste0(round(Mse_all_t2m, 4)), 
           color = 'red', vjust = -0.5, size = 3.5) +
  labs(x = 'Quantile', y = 'MSE', title = 'The MSE of t2m forecast for each quantile') +
  theme_minimal() +
  ggplot(Mse_quan_mslp) + geom_point(aes(x = factor(quan), y = Mse, group = 1)) + 
  geom_line(aes(x = factor(quan), y = Mse, group = 2)) +
  geom_errorbar(aes(x = factor(quan), ymin = Mse - SE, ymax = Mse + SE), width = 0.2) + 
  geom_hline(yintercept = Mse_all_mslp, linetype = 'dashed', color = 'red') +
  annotate('text', x = max(as.numeric(factor(Mse_quan_mslp$quan))) - 2, y = Mse_all_mslp, 
           label = 'Overall MSE', color = 'red', vjust = -0.5, size = 4) +
  annotate('text', x = 1, y = Mse_all_mslp, label = paste0(round(Mse_all_mslp, 4)), 
           color = 'red', vjust = -0.5, size = 3.5) +
  labs(x = 'Quantile', y = 'MSE', title = 'The MSE of mslp forecast for each quantile') +
  theme_minimal()

ggplot(Mse_quan_u10) + geom_point(aes(x = factor(quan), y = Mse, group = 1)) + 
  geom_line(aes(x = factor(quan), y = Mse, group = 2)) +
  geom_errorbar(aes(x = factor(quan), ymin = Mse - SE, ymax = Mse + SE), width = 0.2) + 
  geom_hline(yintercept = Mse_all_u10, linetype = 'dashed', color = 'red') +
  annotate('text', x = max(as.numeric(factor(Mse_quan_u10$quan))) - 2, y = Mse_all_u10, 
           label = 'Overall MSE', color = 'red', vjust = -0.5, size = 4) +
  annotate('text', x = 1, y = Mse_all_u10, label = paste0(round(Mse_all_u10, 4)), 
           color = 'red', vjust = -0.5, size = 3.5) +
  labs(x = 'Quantile', y = 'MSE', title = 'The MSE of u10 forecast for each quantile') +
  theme_minimal() +
  ggplot(Mse_quan_v10) + geom_point(aes(x = factor(quan), y = Mse, group = 1)) + 
  geom_line(aes(x = factor(quan), y = Mse, group = 2)) +
  geom_errorbar(aes(x = factor(quan), ymin = Mse - SE, ymax = Mse + SE), width = 0.2) + 
  geom_hline(yintercept = Mse_all_v10, linetype = 'dashed', color = 'red') +
  annotate('text', x = max(as.numeric(factor(Mse_quan_v10$quan))) - 2, y = Mse_all_v10, 
           label = 'Overall MSE', color = 'red', vjust = -0.5, size = 4) +
  annotate('text', x = 1, y = Mse_all_v10, label = paste0(round(Mse_all_v10, 4)), 
           color = 'red', vjust = -0.5, size = 3.5) +
  labs(x = 'Quantile', y = 'MSE', title = 'The MSE of v10 forecast for each quantile') +
  theme_minimal() +
  ggplot(Mse_quan_Ws) + geom_point(aes(x = factor(quan), y = Mse, group = 1)) + 
  geom_line(aes(x = factor(quan), y = Mse, group = 2)) +
  geom_errorbar(aes(x = factor(quan), ymin = Mse - SE, ymax = Mse + SE), width = 0.2) + 
  geom_hline(yintercept = Mse_all_Ws, linetype = 'dashed', color = 'red') +
  annotate('text', x = max(as.numeric(factor(Mse_quan_Ws$quan))) - 2, y = Mse_all_Ws, 
           label = 'Overall MSE', color = 'red', vjust = -0.5, size = 4) +
  annotate('text', x = 1, y = Mse_all_Ws, label = paste0(round(Mse_all_Ws, 4)), 
           color = 'red', vjust = -0.5, size = 3.5) +
  labs(x = 'Quantile', y = 'MSE', title = 'The MSE of Ws forecast for each quantile') +
  theme_minimal()


## MSE and skill score (focus on location)
pangu_uk_t2m <- pangu_uk[, c('date', 'lat', 'lon', 't2m_fcst', 't2m', 't2m_pf')]
Mse_loc_t2m = pangu_uk_t2m |> 
  group_by(lat, lon) |>
  summarise(Mse = mean((t2m_fcst - t2m)^2), 
            Mse_mf = mean((mean(t2m) - t2m)^2),
            Mse_pf = mean((t2m_pf - t2m)^2)) |>
  mutate(SS_mf = 1 - (Mse/Mse_mf), SS_pf = 1 - (Mse/Mse_pf))
ggplot(Mse_loc_t2m) + 
  geom_raster(aes(x=lon, y=lat, fill=Mse)) +
  labs(x = 'lon', y = 'lat', title = 'The MSE of t2m forecast according to location') +
  borders(regions=c('UK','Ireland'), colour='black') +
  scale_fill_viridis_c(option='C', n = 'MSE')
ggplot(Mse_loc_t2m) + 
  geom_raster(aes(x=lon, y=lat, fill=SS_mf)) +
  labs(x = 'lon', y = 'lat', title = 'The skill scores of t2m forecast based on mean forecast according to location') +
  borders(regions=c('UK','Ireland'), colour='black') +
  scale_fill_viridis_c(option='D', n = 'Skill score') +
  ggplot(Mse_loc_t2m) + 
  geom_raster(aes(x=lon, y=lat, fill=SS_pf)) +
  labs(x = 'lon', y = 'lat', title = 'The skill scores of t2m forecast based on persistence forecast according to location') +
  borders(regions=c('UK','Ireland'), colour='black') +
  scale_fill_viridis_c(option='D', n = 'Skill score')

pangu_uk_mslp <- pangu_uk[, c('date', 'lat', 'lon', 'mslp_fcst', 'mslp', 'mslp_pf')]
Mse_loc_mslp = pangu_uk_mslp |> 
  group_by(lat, lon) |>
  summarise(Mse = mean((mslp_fcst - mslp)^2), 
            Mse_mf = mean((mean(mslp) - mslp)^2),
            Mse_pf = mean((mslp_pf - mslp)^2)) |>
  mutate(SS_mf = 1 - (Mse/Mse_mf), SS_pf = 1 - (Mse/Mse_pf))
ggplot(Mse_loc_mslp) + 
  geom_raster(aes(x=lon, y=lat, fill=Mse)) +
  labs(x = 'lon', y = 'lat', title = 'The MSE of mslp forecast according to location') +
  borders(regions=c('UK','Ireland'), colour='black') +
  scale_fill_viridis_c(option='C', n = 'MSE')
ggplot(Mse_loc_mslp) + 
  geom_raster(aes(x=lon, y=lat, fill=SS_mf)) +
  labs(x = 'lon', y = 'lat', title = 'The skill scores of mslp forecast based on mean forecast according to location') +
  borders(regions=c('UK','Ireland'), colour='black') +
  scale_fill_viridis_c(option='D', n = 'Skill score') +
  ggplot(Mse_loc_mslp) + 
  geom_raster(aes(x=lon, y=lat, fill=SS_pf)) +
  labs(x = 'lon', y = 'lat', title = 'The skill scores of mslp forecast based on persistence forecast according to location') +
  borders(regions=c('UK','Ireland'), colour='black') +
  scale_fill_viridis_c(option='D', n = 'Skill score')

pangu_uk_u10 <- pangu_uk[, c('date', 'lat', 'lon', 'u10_fcst', 'u10', 'u10_pf')]
Mse_loc_u10 = pangu_uk_u10 |> 
  group_by(lat, lon) |>
  summarise(Mse = mean((u10_fcst - u10)^2), 
            Mse_mf = mean((mean(u10) - u10)^2),
            Mse_pf = mean((u10_pf - u10)^2)) |>
  mutate(SS_mf = 1 - (Mse/Mse_mf), SS_pf = 1 - (Mse/Mse_pf))
ggplot(Mse_loc_u10) + 
  geom_raster(aes(x=lon, y=lat, fill=Mse)) +
  labs(x = 'lon', y = 'lat', title = 'The MSE of u10 forecast according to location') +
  borders(regions=c('UK','Ireland'), colour='black') +
  scale_fill_viridis_c(option='C', n = 'MSE')
ggplot(Mse_loc_u10) + 
  geom_raster(aes(x=lon, y=lat, fill=SS_mf)) +
  labs(x = 'lon', y = 'lat', title = 'The skill scores of u10 forecast based on mean forecast according to location') +
  borders(regions=c('UK','Ireland'), colour='black') +
  scale_fill_viridis_c(option='D', n = 'Skill score') +
  ggplot(Mse_loc_u10) + 
  geom_raster(aes(x=lon, y=lat, fill=SS_pf)) +
  labs(x = 'lon', y = 'lat', title = 'The skill scores of u10 forecast based on persistence forecast according to location') +
  borders(regions=c('UK','Ireland'), colour='black') +
  scale_fill_viridis_c(option='D', n = 'Skill score')

pangu_uk_v10 <- pangu_uk[, c('date', 'lat', 'lon', 'v10_fcst', 'v10', 'v10_pf')]
Mse_loc_v10 = pangu_uk_v10 |> 
  group_by(lat, lon) |>
  summarise(Mse = mean((v10_fcst - v10)^2), 
            Mse_mf = mean((mean(v10) - v10)^2),
            Mse_pf = mean((v10_pf - v10)^2)) |>
  mutate(SS_mf = 1 - (Mse/Mse_mf), SS_pf = 1 - (Mse/Mse_pf))
ggplot(Mse_loc_v10) + 
  geom_raster(aes(x=lon, y=lat, fill=Mse)) +
  labs(x = 'lon', y = 'lat', title = 'The MSE of v10 forecast according to location') +
  borders(regions=c('UK','Ireland'), colour='black') +
  scale_fill_viridis_c(option='C', n = 'MSE')
ggplot(Mse_loc_v10) + 
  geom_raster(aes(x=lon, y=lat, fill=SS_mf)) +
  labs(x = 'lon', y = 'lat', title = 'The skill scores of v10 forecast based on mean forecast according to location') +
  borders(regions=c('UK','Ireland'), colour='black') +
  scale_fill_viridis_c(option='D', n = 'Skill score') +
  ggplot(Mse_loc_v10) + 
  geom_raster(aes(x=lon, y=lat, fill=SS_pf)) +
  labs(x = 'lon', y = 'lat', title = 'The skill scores of v10 forecast based on persistence forecast according to location') +
  borders(regions=c('UK','Ireland'), colour='black') +
  scale_fill_viridis_c(option='D', n = 'Skill score')

pangu_uk_Ws <- pangu_uk[, c('date', 'lat', 'lon', 'Ws_fcst', 'Ws', 'Ws_pf')]
Mse_loc_Ws = pangu_uk_Ws |> 
  group_by(lat, lon) |>
  summarise(Mse = mean((Ws_fcst - Ws)^2), 
            Mse_mf = mean((mean(Ws) - Ws)^2),
            Mse_pf = mean((Ws_pf - Ws)^2)) |>
  mutate(SS_mf = 1 - (Mse/Mse_mf), SS_pf = 1 - (Mse/Mse_pf))
ggplot(Mse_loc_Ws) + 
  geom_raster(aes(x=lon, y=lat, fill=Mse)) +
  labs(x = 'lon', y = 'lat', title = 'The MSE of Ws forecast according to location') +
  borders(regions=c('UK','Ireland'), colour='black') +
  scale_fill_viridis_c(option='C', n = 'MSE')
ggplot(Mse_loc_Ws) + 
  geom_raster(aes(x=lon, y=lat, fill=SS_mf)) +
  labs(x = 'lon', y = 'lat', title = 'The skill scores of Ws forecast based on mean forecast according to location') +
  borders(regions=c('UK','Ireland'), colour='black') +
  scale_fill_viridis_c(option='D', n = 'Skill score') +
  ggplot(Mse_loc_Ws) + 
  geom_raster(aes(x=lon, y=lat, fill=SS_pf)) +
  labs(x = 'lon', y = 'lat', title = 'The skill scores of Ws forecast based on persistence forecast according to location') +
  borders(regions=c('UK','Ireland'), colour='black') +
  scale_fill_viridis_c(option='D', n = 'Skill score')

ggplot(Mse_loc_t2m) + 
  geom_raster(aes(x=lon, y=lat, fill=Mse)) +
  labs(x = 'lon', y = 'lat', title = 'The MSE of t2m forecast according to location') +
  borders(regions=c('UK','Ireland'), colour='black') +
  scale_fill_viridis_c(option='C', n = 'MSE')+
  theme(legend.position = 'bottom') +
  ggplot(Mse_loc_t2m) + 
  geom_raster(aes(x=lon, y=lat, fill=SS_mf)) +
  labs(x = 'lon', y = 'lat', title = 'The Skill score of t2m forecast based on MF') +
  borders(regions=c('UK','Ireland'), colour='black') +
  scale_fill_viridis_c(option='D', n = 'Skill score') +
  theme(legend.position = "bottom") +
  ggplot(Mse_loc_t2m) + 
  geom_raster(aes(x=lon, y=lat, fill=SS_pf)) +
  labs(x = 'lon', y = 'lat', title = 'The Skill score of t2m forecast based on PF') +
  borders(regions=c('UK','Ireland'), colour='black') +
  scale_fill_viridis_c(option='D', n = 'Skill score') +
  theme(legend.position = "bottom")

ggplot(Mse_loc_mslp) + 
  geom_raster(aes(x=lon, y=lat, fill=Mse)) +
  labs(x = 'lon', y = 'lat', title = 'The MSE of mslp forecast according to location') +
  borders(regions=c('UK','Ireland'), colour='black') +
  scale_fill_viridis_c(option='C', n = 'MSE')+
  theme(legend.position = "bottom", 
        legend.key.width = unit(1, 'cm'),
        legend.key.height = unit(0.5, 'cm')) +
  ggplot(Mse_loc_mslp) + 
  geom_raster(aes(x=lon, y=lat, fill=SS_mf)) +
  labs(x = 'lon', y = 'lat', title = 'The Skill score of mslp forecast based on MF') +
  borders(regions=c('UK','Ireland'), colour='black') +
  scale_fill_viridis_c(option='D', n = 'Skill score') +
  theme(legend.position = "bottom", 
        legend.key.width = unit(1, 'cm'),
        legend.key.height = unit(0.5, 'cm')) + 
  ggplot(Mse_loc_mslp) + 
  geom_raster(aes(x=lon, y=lat, fill=SS_pf)) +
  labs(x = 'lon', y = 'lat', title = 'The Skill score of mslp forecast based on PF') +
  borders(regions=c('UK','Ireland'), colour='black') +
  scale_fill_viridis_c(option='D', n = 'Skill score') +
  theme(legend.position = "bottom", 
        legend.key.width = unit(1, 'cm'),
        legend.key.height = unit(0.5, 'cm'))

ggplot(Mse_loc_u10) + 
  geom_raster(aes(x=lon, y=lat, fill=Mse)) +
  labs(x = 'lon', y = 'lat', title = 'The MSE of u10 forecast according to location') +
  borders(regions=c('UK','Ireland'), colour='black') +
  scale_fill_viridis_c(option='C', n = 'MSE')+
  theme(legend.position = 'bottom') +
  ggplot(Mse_loc_u10) + 
  geom_raster(aes(x=lon, y=lat, fill=SS_mf)) +
  labs(x = 'lon', y = 'lat', title = 'The Skill score of u10 forecast based on MF') +
  borders(regions=c('UK','Ireland'), colour='black') +
  scale_fill_viridis_c(option='D', n = 'Skill score') +
  theme(legend.position = "bottom") +
  ggplot(Mse_loc_u10) + 
  geom_raster(aes(x=lon, y=lat, fill=SS_pf)) +
  labs(x = 'lon', y = 'lat', title = 'The Skill score of u10 forecast based on PF') +
  borders(regions=c('UK','Ireland'), colour='black') +
  scale_fill_viridis_c(option='D', n = 'Skill score') +
  theme(legend.position = "bottom")

## MSE and skill score for season
pangu_uk_season <- pangu_uk |> 
  mutate(month = month(date)) |>
  mutate(season = case_when(
    month %in% c(3,4,5) ~ 'Spring',
    month %in% c(6,7,8) ~ 'Summer',
    month %in% c(9,10,11) ~ 'Autumn',
    month %in% c(12,1,2) ~ 'Winter',
  ))
season_order <- c('Spring', 'Summer', 'Autumn', 'Winter')
Mse_season_t2m = pangu_uk_season |>
  group_by(season) |>
  summarise(Mse = mean((t2m_fcst - t2m)^2), 
            Mse_mf = mean((mean(t2m) - t2m)^2),
            Mse_pf = mean((t2m_pf - t2m)^2)) |>
  mutate(SS_mf = 1 - (Mse/Mse_mf), SS_pf = 1 - (Mse/Mse_pf)) |>
  mutate(season = factor(season, levels = season_order)) |>
  arrange(season)
ggplot(Mse_season_t2m) + 
  geom_point(aes(x = factor(season, levels = season_order), y = Mse, group = 1)) +
  geom_line(aes(x = factor(season, levels = season_order), y = Mse, group = 2)) +
  labs(x = 'Season', y = 'MSE', title = 'The MSE of t2m forecast for each season') +
  theme_minimal() +
  ggplot(Mse_season_t2m) +
  geom_point(aes(x = factor(season, levels = season_order), y = Mse_mf, group = 1)) +
  geom_line(aes(x = factor(season, levels = season_order), y = Mse_mf, group = 2)) +
  labs(x ='Season', y = 'MSE', title = 'The MSE of t2m mean forecast for each season') +
  theme_minimal() +
  ggplot(Mse_season_t2m) +
  geom_point(aes(x = factor(season, levels = season_order), y = Mse_pf, group = 1)) +
  geom_line(aes(x = factor(season, levels = season_order), y = Mse_pf, group = 2)) +
  labs(x ='Season', y = 'MSE', title = 'The MSE of t2m persistence forecast for each season') +
  theme_minimal()
ggplot(Mse_season_t2m) + 
  geom_point(aes(x = factor(season, levels = season_order), y = SS_mf, group = 1, color = 'Mean forecast')) +
  geom_line(aes(x = factor(season, levels = season_order), y = SS_mf, group = 2, color = 'Mean forecast')) +
  geom_point(aes(x = factor(season, levels = season_order), y = SS_pf, group = 1, color = 'Persistence forecast')) +
  geom_line(aes(x = factor(season, levels = season_order), y = SS_pf, group = 2, color = 'Persistence forecast')) +
  labs(x = 'Season', y = 'Skill score', color = 'Legend',
       title = 'The skill scores of t2m forecast based on mean forecast and persistence forecast for each season') +
  scale_color_manual(values = c('Mean forecast' = 'blue', 'Persistence forecast' = 'red')) +
  theme_minimal()

Mse_season_mslp = pangu_uk_season |>
  group_by(season) |>
  summarise(Mse = mean((mslp_fcst - mslp)^2), 
            Mse_mf = mean((mean(mslp) - mslp)^2),
            Mse_pf = mean((mslp_pf - mslp)^2)) |>
  mutate(SS_mf = 1 - (Mse/Mse_mf), SS_pf = 1 - (Mse/Mse_pf)) |>
  mutate(season = factor(season, levels = season_order)) |>
  arrange(season)
ggplot(Mse_season_mslp) + 
  geom_point(aes(x = factor(season, levels = season_order), y = Mse, group = 1)) +
  geom_line(aes(x = factor(season, levels = season_order), y = Mse, group = 2)) +
  labs(x = 'Season', y = 'MSE', title = 'The MSE of mslp forecast for each season') +
  theme_minimal() +
  ggplot(Mse_season_mslp) +
  geom_point(aes(x = factor(season, levels = season_order), y = Mse_mf, group = 1)) +
  geom_line(aes(x = factor(season, levels = season_order), y = Mse_mf, group = 2)) +
  labs(x ='Season', y = 'MSE', title = 'The MSE of mslp mean forecast for each season') +
  theme_minimal() +
  ggplot(Mse_season_mslp) +
  geom_point(aes(x = factor(season, levels = season_order), y = Mse_pf, group = 1)) +
  geom_line(aes(x = factor(season, levels = season_order), y = Mse_pf, group = 2)) +
  labs(x ='Season', y = 'MSE', title = 'The MSE of mslp persistence forecast for each season') +
  theme_minimal()
ggplot(Mse_season_mslp) + 
  geom_point(aes(x = factor(season, levels = season_order), y = SS_mf, group = 1, color = 'Mean forecast')) +
  geom_line(aes(x = factor(season, levels = season_order), y = SS_mf, group = 2, color = 'Mean forecast')) +
  geom_point(aes(x = factor(season, levels = season_order), y = SS_pf, group = 1, color = 'Persistence forecast')) +
  geom_line(aes(x = factor(season, levels = season_order), y = SS_pf, group = 2, color = 'Persistence forecast')) +
  labs(x = 'Season', y = 'Skill score', color = 'Legend',
       title = 'The skill scores of mslp forecast based on mean forecast and persistence forecast for each season') +
  scale_color_manual(values = c('Mean forecast' = 'blue', 'Persistence forecast' = 'red')) +
  theme_minimal()

Mse_season_u10 = pangu_uk_season |>
  group_by(season) |>
  summarise(Mse = mean((u10_fcst - u10)^2), 
            Mse_mf = mean((mean(u10) - u10)^2),
            Mse_pf = mean((u10_pf - u10)^2)) |>
  mutate(SS_mf = 1 - (Mse/Mse_mf), SS_pf = 1 - (Mse/Mse_pf)) |>
  mutate(season = factor(season, levels = season_order)) |>
  arrange(season)
ggplot(Mse_season_u10) + 
  geom_point(aes(x = factor(season, levels = season_order), y = Mse, group = 1)) +
  geom_line(aes(x = factor(season, levels = season_order), y = Mse, group = 2)) +
  labs(x = 'Season', y = 'MSE', title = 'The MSE of u10 forecast for each season') +
  theme_minimal() +
  ggplot(Mse_season_u10) +
  geom_point(aes(x = factor(season, levels = season_order), y = Mse_mf, group = 1)) +
  geom_line(aes(x = factor(season, levels = season_order), y = Mse_mf, group = 2)) +
  labs(x ='Season', y = 'MSE', title = 'The MSE of u10 mean forecast for each season') +
  theme_minimal() +
  ggplot(Mse_season_u10) +
  geom_point(aes(x = factor(season, levels = season_order), y = Mse_pf, group = 1)) +
  geom_line(aes(x = factor(season, levels = season_order), y = Mse_pf, group = 2)) +
  labs(x ='Season', y = 'MSE', title = 'The MSE of u10 persistence forecast for each season') +
  theme_minimal()
ggplot(Mse_season_u10) + 
  geom_point(aes(x = factor(season, levels = season_order), y = SS_mf, group = 1, color = 'Mean forecast')) +
  geom_line(aes(x = factor(season, levels = season_order), y = SS_mf, group = 2, color = 'Mean forecast')) +
  geom_point(aes(x = factor(season, levels = season_order), y = SS_pf, group = 1, color = 'Persistence forecast')) +
  geom_line(aes(x = factor(season, levels = season_order), y = SS_pf, group = 2, color = 'Persistence forecast')) +
  labs(x = 'Season', y = 'Skill score', color = 'Legend',
       title = 'The skill scores of u10 forecast based on mean forecast and persistence forecast for each season') +
  scale_color_manual(values = c('Mean forecast' = 'blue', 'Persistence forecast' = 'red')) +
  theme_minimal()

Mse_season_v10 = pangu_uk_season |>
  group_by(season) |>
  summarise(Mse = mean((v10_fcst - v10)^2), 
            Mse_mf = mean((mean(v10) - v10)^2),
            Mse_pf = mean((v10_pf - v10)^2)) |>
  mutate(SS_mf = 1 - (Mse/Mse_mf), SS_pf = 1 - (Mse/Mse_pf)) |>
  mutate(season = factor(season, levels = season_order)) |>
  arrange(season)
ggplot(Mse_season_v10) + 
  geom_point(aes(x = factor(season, levels = season_order), y = Mse, group = 1)) +
  geom_line(aes(x = factor(season, levels = season_order), y = Mse, group = 2)) +
  labs(x = 'Season', y = 'MSE', title = 'The MSE of v10 forecast for each season') +
  theme_minimal() +
  ggplot(Mse_season_v10) +
  geom_point(aes(x = factor(season, levels = season_order), y = Mse_mf, group = 1)) +
  geom_line(aes(x = factor(season, levels = season_order), y = Mse_mf, group = 2)) +
  labs(x ='Season', y = 'MSE', title = 'The MSE of v10 mean forecast for each season') +
  theme_minimal() +
  ggplot(Mse_season_v10) +
  geom_point(aes(x = factor(season, levels = season_order), y = Mse_pf, group = 1)) +
  geom_line(aes(x = factor(season, levels = season_order), y = Mse_pf, group = 2)) +
  labs(x ='Season', y = 'MSE', title = 'The MSE of v10 persistence forecast for each season') +
  theme_minimal()
ggplot(Mse_season_v10) + 
  geom_point(aes(x = factor(season, levels = season_order), y = SS_mf, group = 1, color = 'Mean forecast')) +
  geom_line(aes(x = factor(season, levels = season_order), y = SS_mf, group = 2, color = 'Mean forecast')) +
  geom_point(aes(x = factor(season, levels = season_order), y = SS_pf, group = 1, color = 'Persistence forecast')) +
  geom_line(aes(x = factor(season, levels = season_order), y = SS_pf, group = 2, color = 'Persistence forecast')) +
  labs(x = 'Season', y = 'Skill score', color = 'Legend',
       title = 'The skill scores of v10 forecast based on mean forecast and persistence forecast for each season') +
  scale_color_manual(values = c('Mean forecast' = 'blue', 'Persistence forecast' = 'red')) +
  theme_minimal()

Mse_season_Ws = pangu_uk_season |>
  group_by(season) |>
  summarise(Mse = mean((Ws_fcst - Ws)^2), 
            Mse_mf = mean((mean(Ws) - Ws)^2),
            Mse_pf = mean((Ws_pf - Ws)^2)) |>
  mutate(SS_mf = 1 - (Mse/Mse_mf), SS_pf = 1 - (Mse/Mse_pf)) |>
  mutate(season = factor(season, levels = season_order)) |>
  arrange(season)
ggplot(Mse_season_Ws) + 
  geom_point(aes(x = factor(season, levels = season_order), y = Mse, group = 1)) +
  geom_line(aes(x = factor(season, levels = season_order), y = Mse, group = 2)) +
  labs(x = 'Season', y = 'MSE', title = 'The MSE of Ws forecast for each season') +
  theme_minimal() +
  ggplot(Mse_season_Ws) +
  geom_point(aes(x = factor(season, levels = season_order), y = Mse_mf, group = 1)) +
  geom_line(aes(x = factor(season, levels = season_order), y = Mse_mf, group = 2)) +
  labs(x ='Season', y = 'MSE', title = 'The MSE of Ws mean forecast for each season') +
  theme_minimal() +
  ggplot(Mse_season_Ws) +
  geom_point(aes(x = factor(season, levels = season_order), y = Mse_pf, group = 1)) +
  geom_line(aes(x = factor(season, levels = season_order), y = Mse_pf, group = 2)) +
  labs(x ='Season', y = 'MSE', title = 'The MSE of Ws persistence forecast for each season') +
  theme_minimal()
ggplot(Mse_season_Ws) + 
  geom_point(aes(x = factor(season, levels = season_order), y = SS_mf, group = 1, color = 'Mean forecast')) +
  geom_line(aes(x = factor(season, levels = season_order), y = SS_mf, group = 2, color = 'Mean forecast')) +
  geom_point(aes(x = factor(season, levels = season_order), y = SS_pf, group = 1, color = 'Persistence forecast')) +
  geom_line(aes(x = factor(season, levels = season_order), y = SS_pf, group = 2, color = 'Persistence forecast')) +
  labs(x = 'Season', y = 'Skill score', color = 'Legend',
       title = 'The skill scores of Ws forecast based on mean forecast and persistence forecast for each season') +
  scale_color_manual(values = c('Mean forecast' = 'blue', 'Persistence forecast' = 'red')) +
  theme_minimal()
############################## End of Chapter 1 ##############################

############################## Chapter 2 ##############################
library(tidyverse)
library(SpecsVerification)
library(patchwork)

# load data from compressed csv files
fcst = read_csv('/Users/alex/Desktop/pangu_uk_sfc_fcst_1day_v2.csv.gz')
obs = read_csv('/Users/alex/Desktop/pangu_uk_sfc_obs_v2.csv.gz')

# prepare forecast data to be joined with obs data
fcst = fcst |>
  rename(mslp_fcst = mslp,
         u10_fcst = u10,
         v10_fcst = v10,
         t2m_fcst = t2m,
         date = target_date) |>
  select(-init_date)

# join forecasts and observations 
pangu_uk = inner_join(fcst, obs, by=c('date','lat','lon'))

## Convert temperature to degrees Celsius
if (max(pangu_uk$t2m_fcst) > 100) {
  pangu_uk$t2m_fcst <- pangu_uk$t2m_fcst - 273.15
}
if (max(pangu_uk$t2m) > 100) {
  pangu_uk$t2m <- pangu_uk$t2m - 273.15
}

## Introduce Wind Speed
pangu_uk <- pangu_uk |> 
  mutate(Ws_fcst = sqrt(pangu_uk$u10_fcst^2 + pangu_uk$v10_fcst^2)) |>
  select(1:which(names(pangu_uk) == 'v10_fcst'), Ws_fcst, everything())
pangu_uk <- pangu_uk |> 
  mutate(Ws = sqrt(pangu_uk$u10^2 + pangu_uk$v10^2)) |>
  select(1:which(names(pangu_uk) == 'v10'), Ws, everything())

########## Simple Linear Regression
## Recalibration of forecasts with simple linear regression (for each month)
coef_mon_t2m = pangu_uk |> 
  select(date, t2m, t2m_fcst) |>
  mutate(month = format(as.Date(date), '%m')) |>
  group_by(month) |> 
  summarise(coefs = list(coef(lm(t2m ~ t2m_fcst)))) |>
  mutate(beta0 = map_dbl(coefs, ~ .[1]), 
         beta1 = map_dbl(coefs, ~ .[2]))
sk_mon_t2m = inner_join(
  coef_mon_t2m[, c('beta0', 'beta1', 'month')], 
  pangu_uk |>
    mutate(month = format(as.Date(date), '%m')) |>
    select(lat, lon, t2m_fcst, t2m, month),
  by = c('month')) |>
  mutate(t2m_rf = `beta0` + `beta1`*`t2m_fcst`) |>
  ungroup() |>
  summarise(Mse_rf = mean((t2m - t2m_rf)^2),
            Mse_Pw = mean((t2m - t2m_fcst)^2)) |>
  mutate(Sksc = 1 - (Mse_rf/Mse_Pw))

coef_mon_mslp = pangu_uk |> 
  select(date, mslp, mslp_fcst) |>
  mutate(month = format(as.Date(date), '%m')) |>
  group_by(month) |> 
  summarise(coefs = list(coef(lm(mslp ~ mslp_fcst)))) |>
  mutate(beta0 = map_dbl(coefs, ~ .[1]), 
         beta1 = map_dbl(coefs, ~ .[2]))
sk_mon_mslp = inner_join(
  coef_mon_mslp[, c('beta0', 'beta1', 'month')], 
  pangu_uk |>
    mutate(month = format(as.Date(date), '%m')) |>
    select(lat, lon, mslp_fcst, mslp, month),
  by = c('month')) |>
  mutate(mslp_rf = `beta0` + `beta1`*`mslp_fcst`) |>
  ungroup() |>
  summarise(Mse_rf = mean((mslp - mslp_rf)^2),
            Mse_Pw = mean((mslp - mslp_fcst)^2)) |>
  mutate(Sksc = 1 - (Mse_rf/Mse_Pw))

coef_mon_u10 = pangu_uk |> 
  select(date, u10, u10_fcst) |>
  mutate(month = format(as.Date(date), '%m')) |>
  group_by(month) |> 
  summarise(coefs = list(coef(lm(u10 ~ u10_fcst)))) |>
  mutate(beta0 = map_dbl(coefs, ~ .[1]), 
         beta1 = map_dbl(coefs, ~ .[2]))
sk_mon_u10 = inner_join(
  coef_mon_u10[, c('beta0', 'beta1', 'month')], 
  pangu_uk |>
    mutate(month = format(as.Date(date), '%m')) |>
    select(lat, lon, u10_fcst, u10, month),
  by = c('month')) |>
  mutate(u10_rf = `beta0` + `beta1`*`u10_fcst`) |>
  ungroup() |>
  summarise(Mse_rf = mean((u10 - u10_rf)^2),
            Mse_Pw = mean((u10 - u10_fcst)^2)) |>
  mutate(Sksc = 1 - (Mse_rf/Mse_Pw))

coef_mon_v10 = pangu_uk |> 
  select(date, v10, v10_fcst) |>
  mutate(month = format(as.Date(date), '%m')) |>
  group_by(month) |> 
  summarise(coefs = list(coef(lm(v10 ~ v10_fcst)))) |>
  mutate(beta0 = map_dbl(coefs, ~ .[1]), 
         beta1 = map_dbl(coefs, ~ .[2]))
sk_mon_v10 = inner_join(
  coef_mon_v10[, c('beta0', 'beta1', 'month')], 
  pangu_uk |>
    mutate(month = format(as.Date(date), '%m')) |>
    select(lat, lon, v10_fcst, v10, month),
  by = c('month')) |>
  mutate(v10_rf = `beta0` + `beta1`*`v10_fcst`) |>
  ungroup() |>
  summarise(Mse_rf = mean((v10 - v10_rf)^2),
            Mse_Pw = mean((v10 - v10_fcst)^2)) |>
  mutate(Sksc = 1 - (Mse_rf/Mse_Pw))

coef_mon_Ws = pangu_uk |> 
  select(date, Ws, Ws_fcst) |>
  mutate(month = format(as.Date(date), '%m')) |>
  group_by(month) |> 
  summarise(coefs = list(coef(lm(Ws ~ Ws_fcst)))) |>
  mutate(beta0 = map_dbl(coefs, ~ .[1]), 
         beta1 = map_dbl(coefs, ~ .[2]))
sk_mon_Ws = inner_join(
  coef_mon_Ws[, c('beta0', 'beta1', 'month')], 
  pangu_uk |>
    mutate(month = format(as.Date(date), '%m')) |>
    select(lat, lon, Ws_fcst, Ws, month),
  by = c('month')) |>
  mutate(Ws_rf = `beta0` + `beta1`*`Ws_fcst`) |>
  ungroup() |>
  summarise(Mse_rf = mean((Ws - Ws_rf)^2),
            Mse_Pw = mean((Ws - Ws_fcst)^2)) |>
  mutate(Sksc = 1 - (Mse_rf/Mse_Pw))

sks_mon_all <- rbind(sk_mon_t2m, sk_mon_mslp, sk_mon_u10, sk_mon_v10, sk_mon_Ws)
rownames(sks_mon_all) <- c('t2m', 'mslp', 'u10', 'v10', 'Ws')

coef_mon_t2m = pangu_uk |> 
  select(date, t2m, t2m_fcst) |>
  mutate(month = format(as.Date(date), '%m')) |>
  group_by(month) |> 
  summarise(coefs = list(coef(lm(t2m ~ t2m_fcst)))) |>
  mutate(beta0 = map_dbl(coefs, ~ .[1]), 
         beta1 = map_dbl(coefs, ~ .[2]))
sk_mon_t2m = inner_join(
  coef_mon_t2m[, c('beta0', 'beta1', 'month')], 
  pangu_uk |>
    mutate(month = format(as.Date(date), '%m')) |>
    select(lat, lon, t2m_fcst, t2m, month),
  by = c('month')) |>
  mutate(t2m_rf = `beta0` + `beta1`*`t2m_fcst`) |>
  group_by(month) |> 
  summarise(Mse_rf = mean((t2m - t2m_rf)^2),
            Mse_Pw = mean((t2m - t2m_fcst)^2)) |>
  mutate(Sksc = 1 - (Mse_rf/Mse_Pw))
ggplot(sk_mon_t2m, aes(x = month)) +
  geom_point(aes(y = Mse_Pw, color = 'Pangu', group = 1)) +
  geom_point(aes(y = Mse_rf, color = 'Recalibrated', group = 1)) +
  geom_line(aes(y = Mse_Pw, color = 'Pangu', group = 2)) +
  geom_line(aes(y = Mse_rf, color = 'Recalibrated', group = 2)) +
  labs(x = 'Month', y = 'MSE', color = 'Legend', title = 'MSE of recalibrated and original data for temperature') +
  scale_color_manual(values = c('Pangu' = 'blue', 'Recalibrated' = 'red')) +
  theme_minimal()


## Recalibration of forecasts with simple linear regression (for each location)
coef_loc_t2m = pangu_uk |> 
  select(lat, lon, t2m, t2m_fcst) |> 
  group_by(lat, lon) |> 
  summarise(coefs = list(coef(lm(t2m ~ t2m_fcst)))) |>
  mutate(beta0 = map_dbl(coefs, ~ .[1]), 
         beta1 = map_dbl(coefs, ~ .[2]))
ggplot(coef_loc_t2m) + 
  geom_raster(aes(x=lon, y=lat, fill=beta1)) +
  borders(regions=c('UK','Ireland'), colour='black') +
  theme_minimal() +
  ggplot(coef_loc_t2m) + 
  geom_raster(aes(x=lon, y=lat, fill=beta0)) +
  borders(regions=c('UK','Ireland'), colour='black') +
  theme_minimal()
sk_loc_t2m = inner_join(coef_loc_t2m[, c('lat', 'lon', 'beta0', 'beta1')], 
                        pangu_uk[, c('lat', 'lon', 't2m_fcst', 't2m')], 
                        by = c('lat', 'lon')) |>
  mutate(t2m_rf = `beta0` + `beta1`*`t2m_fcst`) |>
  ungroup() |> 
  summarise(Mse_rf = mean((t2m - t2m_rf)^2),
            Mse_Pw = mean((t2m - t2m_fcst)^2)) |>
  mutate(Sksc = 1 - (Mse_rf/Mse_Pw))

coef_loc_mslp = pangu_uk |> 
  select(lat, lon, mslp, mslp_fcst) |> 
  group_by(lat, lon) |> 
  summarise(coefs = list(coef(lm(mslp ~ mslp_fcst)))) |>
  mutate(beta0 = map_dbl(coefs, ~ .[1]), 
         beta1 = map_dbl(coefs, ~ .[2]))
ggplot(coef_loc_mslp) + 
  geom_raster(aes(x=lon, y=lat, fill=beta1)) +
  borders(regions=c('UK','Ireland'), colour='black') +
  theme_minimal() +
  ggplot(coef_loc_mslp) + 
  geom_raster(aes(x=lon, y=lat, fill=beta0)) +
  borders(regions=c('UK','Ireland'), colour='black') +
  theme_minimal()
sk_loc_mslp = inner_join(coef_loc_mslp[, c('lat', 'lon', 'beta0', 'beta1')], 
                         pangu_uk[, c('lat', 'lon', 'mslp_fcst', 'mslp')], 
                         by = c('lat', 'lon')) |>
  mutate(mslp_rf = `beta0` + `beta1`*`mslp_fcst`) |>
  ungroup() |> 
  summarise(Mse_rf = mean((mslp - mslp_rf)^2),
            Mse_Pw = mean((mslp - mslp_fcst)^2)) |>
  mutate(Sksc = 1 - (Mse_rf/Mse_Pw))

coef_loc_u10 = pangu_uk |> 
  select(lat, lon, u10, u10_fcst) |> 
  group_by(lat, lon) |> 
  summarise(coefs = list(coef(lm(u10 ~ u10_fcst)))) |>
  mutate(beta0 = map_dbl(coefs, ~ .[1]), 
         beta1 = map_dbl(coefs, ~ .[2]))
ggplot(coef_loc_u10) + 
  geom_raster(aes(x=lon, y=lat, fill=beta1)) +
  borders(regions=c('UK','Ireland'), colour='black') +
  theme_minimal() +
  ggplot(coef_loc_u10) + 
  geom_raster(aes(x=lon, y=lat, fill=beta0)) +
  borders(regions=c('UK','Ireland'), colour='black') +
  theme_minimal()
sk_loc_u10 = inner_join(coef_loc_u10[, c('lat', 'lon', 'beta0', 'beta1')], 
                        pangu_uk[, c('lat', 'lon', 'u10_fcst', 'u10')], 
                        by = c('lat', 'lon')) |>
  mutate(u10_rf = `beta0` + `beta1`*`u10_fcst`) |>
  ungroup() |> 
  summarise(Mse_rf = mean((u10 - u10_rf)^2),
            Mse_Pw = mean((u10 - u10_fcst)^2)) |>
  mutate(Sksc = 1 - (Mse_rf/Mse_Pw))

coef_loc_v10 = pangu_uk |> 
  select(lat, lon, v10, v10_fcst) |> 
  group_by(lat, lon) |> 
  summarise(coefs = list(coef(lm(v10 ~ v10_fcst)))) |>
  mutate(beta0 = map_dbl(coefs, ~ .[1]), 
         beta1 = map_dbl(coefs, ~ .[2]))
ggplot(coef_loc_v10) + 
  geom_raster(aes(x=lon, y=lat, fill=beta1)) +
  borders(regions=c('UK','Ireland'), colour='black') +
  theme_minimal() +
  ggplot(coef_loc_v10) + 
  geom_raster(aes(x=lon, y=lat, fill=beta0)) +
  borders(regions=c('UK','Ireland'), colour='black') +
  theme_minimal()
sk_loc_v10 = inner_join(coef_loc_v10[, c('lat', 'lon', 'beta0', 'beta1')], 
                        pangu_uk[, c('lat', 'lon', 'v10_fcst', 'v10')], 
                        by = c('lat', 'lon')) |>
  mutate(v10_rf = `beta0` + `beta1`*`v10_fcst`) |>
  ungroup() |> 
  summarise(Mse_rf = mean((v10 - v10_rf)^2),
            Mse_Pw = mean((v10 - v10_fcst)^2)) |>
  mutate(Sksc = 1 - (Mse_rf/Mse_Pw))

coef_loc_Ws = pangu_uk |> 
  select(lat, lon, Ws, Ws_fcst) |> 
  group_by(lat, lon) |> 
  summarise(coefs = list(coef(lm(Ws ~ Ws_fcst)))) |>
  mutate(beta0 = map_dbl(coefs, ~ .[1]), 
         beta1 = map_dbl(coefs, ~ .[2]))
ggplot(coef_loc_Ws) + 
  geom_raster(aes(x=lon, y=lat, fill=beta1)) +
  borders(regions=c('UK','Ireland'), colour='black') +
  theme_minimal() +
  ggplot(coef_loc_Ws) + 
  geom_raster(aes(x=lon, y=lat, fill=beta0)) +
  borders(regions=c('UK','Ireland'), colour='black') +
  theme_minimal()
sk_loc_Ws = inner_join(coef_loc_Ws[, c('lat', 'lon', 'beta0', 'beta1')], 
                       pangu_uk[, c('lat', 'lon', 'Ws_fcst', 'Ws')], 
                       by = c('lat', 'lon')) |>
  mutate(Ws_rf = `beta0` + `beta1`*`Ws_fcst`) |>
  ungroup() |> 
  summarise(Mse_rf = mean((Ws - Ws_rf)^2),
            Mse_Pw = mean((Ws - Ws_fcst)^2)) |>
  mutate(Sksc = 1 - (Mse_rf/Mse_Pw))

sks_loc_all <- rbind(sk_loc_t2m, sk_loc_mslp, sk_loc_u10, sk_loc_v10, sk_loc_Ws)
rownames(sks_loc_all) <- c('t2m', 'mslp', 'u10', 'v10', 'Ws')


## Recalibration of forecasts with simple linear regression (for each location and month)
coef_loc_mon_t2m = pangu_uk |> 
  select(date, lat, lon, t2m, t2m_fcst) |>
  mutate(month = format(as.Date(date), '%m')) |>
  group_by(lat, lon, month) |> 
  summarise(coefs = list(coef(lm(t2m ~ t2m_fcst)))) |>
  mutate(beta0 = map_dbl(coefs, ~ .[1]), 
         beta1 = map_dbl(coefs, ~ .[2]))
sk_loc_mon_t2m = inner_join(
  coef_loc_mon_t2m[, c('lat', 'lon', 'beta0', 'beta1', 'month')], 
  pangu_uk |>
    mutate(month = format(as.Date(date), '%m')) |>
    select(lat, lon, t2m_fcst, t2m, month),
  by = c('lat', 'lon', 'month')) |>
  mutate(t2m_rf = `beta0` + `beta1`*`t2m_fcst`) |>
  ungroup() |> 
  summarise(Mse_rf = mean((t2m - t2m_rf)^2),
            Mse_Pw = mean((t2m - t2m_fcst)^2)) |>
  mutate(Sksc = 1 - (Mse_rf/Mse_Pw))

coef_loc_mon_mslp = pangu_uk |> 
  select(date, lat, lon, mslp, mslp_fcst) |>
  mutate(month = format(as.Date(date), '%m')) |>
  group_by(lat, lon, month) |> 
  summarise(coefs = list(coef(lm(mslp ~ mslp_fcst)))) |>
  mutate(beta0 = map_dbl(coefs, ~ .[1]), 
         beta1 = map_dbl(coefs, ~ .[2]))
sk_loc_mon_mslp = inner_join(
  coef_loc_mon_mslp[, c('lat', 'lon', 'beta0', 'beta1', 'month')], 
  pangu_uk |>
    mutate(month = format(as.Date(date), '%m')) |>
    select(lat, lon, mslp_fcst, mslp, month),
  by = c('lat', 'lon', 'month')) |>
  mutate(mslp_rf = `beta0` + `beta1`*`mslp_fcst`) |>
  ungroup() |> 
  summarise(Mse_rf = mean((mslp - mslp_rf)^2),
            Mse_Pw = mean((mslp - mslp_fcst)^2)) |>
  mutate(Sksc = 1 - (Mse_rf/Mse_Pw))

coef_loc_mon_u10 = pangu_uk |> 
  select(date, lat, lon, u10, u10_fcst) |>
  mutate(month = format(as.Date(date), '%m')) |>
  group_by(lat, lon, month) |> 
  summarise(coefs = list(coef(lm(u10 ~ u10_fcst)))) |>
  mutate(beta0 = map_dbl(coefs, ~ .[1]), 
         beta1 = map_dbl(coefs, ~ .[2]))
sk_loc_mon_u10 = inner_join(
  coef_loc_mon_u10[, c('lat', 'lon', 'beta0', 'beta1', 'month')], 
  pangu_uk |>
    mutate(month = format(as.Date(date), '%m')) |>
    select(lat, lon, u10_fcst, u10, month),
  by = c('lat', 'lon', 'month')) |>
  mutate(u10_rf = `beta0` + `beta1`*`u10_fcst`) |>
  ungroup() |> 
  summarise(Mse_rf = mean((u10 - u10_rf)^2),
            Mse_Pw = mean((u10 - u10_fcst)^2)) |>
  mutate(Sksc = 1 - (Mse_rf/Mse_Pw))

coef_loc_mon_v10 = pangu_uk |> 
  select(date, lat, lon, v10, v10_fcst) |>
  mutate(month = format(as.Date(date), '%m')) |>
  group_by(lat, lon, month) |> 
  summarise(coefs = list(coef(lm(v10 ~ v10_fcst)))) |>
  mutate(beta0 = map_dbl(coefs, ~ .[1]), 
         beta1 = map_dbl(coefs, ~ .[2]))
sk_loc_mon_v10 = inner_join(
  coef_loc_mon_v10[, c('lat', 'lon', 'beta0', 'beta1', 'month')], 
  pangu_uk |>
    mutate(month = format(as.Date(date), '%m')) |>
    select(lat, lon, v10_fcst, v10, month),
  by = c('lat', 'lon', 'month')) |>
  mutate(v10_rf = `beta0` + `beta1`*`v10_fcst`) |>
  ungroup() |> 
  summarise(Mse_rf = mean((v10 - v10_rf)^2),
            Mse_Pw = mean((v10 - v10_fcst)^2)) |>
  mutate(Sksc = 1 - (Mse_rf/Mse_Pw))

coef_loc_mon_Ws = pangu_uk |> 
  select(date, lat, lon, Ws, Ws_fcst) |>
  mutate(month = format(as.Date(date), '%m')) |>
  group_by(lat, lon, month) |> 
  summarise(coefs = list(coef(lm(Ws ~ Ws_fcst)))) |>
  mutate(beta0 = map_dbl(coefs, ~ .[1]), 
         beta1 = map_dbl(coefs, ~ .[2]))
sk_loc_mon_Ws = inner_join(
  coef_loc_mon_Ws[, c('lat', 'lon', 'beta0', 'beta1', 'month')], 
  pangu_uk |>
    mutate(month = format(as.Date(date), '%m')) |>
    select(lat, lon, Ws_fcst, Ws, month),
  by = c('lat', 'lon', 'month')) |>
  mutate(Ws_rf = `beta0` + `beta1`*`Ws_fcst`) |>
  ungroup() |> 
  summarise(Mse_rf = mean((Ws - Ws_rf)^2),
            Mse_Pw = mean((Ws - Ws_fcst)^2)) |>
  mutate(Sksc = 1 - (Mse_rf/Mse_Pw))

sks_loc_mon_all <- rbind(sk_loc_mon_t2m, sk_loc_mon_mslp, sk_loc_mon_u10, sk_loc_mon_v10, sk_loc_mon_Ws)
rownames(sks_loc_mon_all) <- c('t2m', 'mslp', 'u10', 'v10', 'Ws')

sks_all <-rbind(sks_mon_all, sks_loc_all, sks_loc_mon_all)
rownames(sks_all) <- c('mon_t2m', 'mon_mslp', 'mon_u10', 'mon_v10', 'mon_Ws', 'loc_t2m', 'loc_mslp', 'loc_u10', 
                       'loc_v10', 'loc_Ws', 'loc_mon_t2m', 'loc_mon_mslp', 'loc_mon_u10', 'loc_mon_v10', 'loc_mon_Ws')
sks_all <- sks_all[c('mon_t2m', 'loc_t2m', 'loc_mon_t2m', 'mon_mslp', 'loc_mslp', 'loc_mon_mslp', 'mon_u10', 
                     'loc_u10', 'loc_mon_u10', 'mon_v10', 'loc_v10', 'loc_mon_v10', 'mon_Ws', 'loc_Ws', 'loc_mon_Ws'), ]
rownames(sks_all) <- c('mon_t2m', 'loc_t2m', 'loc_mon_t2m', 'mon_mslp', 'loc_mslp', 'loc_mon_mslp', 'mon_u10', 
                       'loc_u10', 'loc_mon_u10', 'mon_v10', 'loc_v10', 'loc_mon_v10', 'mon_Ws', 'loc_Ws', 'loc_mon_Ws')


## Recalibration of forecasts with simple linear regression (for t2m for each obs and fcst quantile)
pangu_uk_t2m <- pangu_uk[, c('date', 'lat', 'lon', 't2m_fcst', 't2m')]
t2m_quanobs <- quantile(pangu_uk_t2m$t2m, probs = (seq(0, 1, by = 0.1)))
t2m_quanfcst <- quantile(pangu_uk_t2m$t2m_fcst, probs = (seq(0, 1, by = 0.1)))
pangu_uk_t2m$quanobs <- cut(pangu_uk_t2m$t2m, breaks = t2m_quanobs, 
                            include.lowest = TRUE, labels = FALSE)
pangu_uk_t2m$quanfcst <- cut(pangu_uk_t2m$t2m_fcst, breaks = t2m_quanfcst, 
                             include.lowest = TRUE, labels = FALSE)
coef_quanobs_t2m = pangu_uk_t2m |> 
  select(quanobs, t2m, t2m_fcst) |> 
  group_by(quanobs) |> 
  summarise(coefs = list(coef(lm(t2m ~ t2m_fcst)))) |>
  mutate(beta0 = map_dbl(coefs, ~ .[1]), 
         beta1 = map_dbl(coefs, ~ .[2]))
coef_quanfst_t2m = pangu_uk_t2m |> 
  select(quanfcst, t2m, t2m_fcst) |> 
  group_by(quanfcst) |> 
  summarise(coefs = list(coef(lm(t2m ~ t2m_fcst)))) |>
  mutate(beta0 = map_dbl(coefs, ~ .[1]), 
         beta1 = map_dbl(coefs, ~ .[2]))
sk_quanobs_t2m = inner_join(coef_quanobs_t2m[, c('quanobs', 'beta0', 'beta1')], 
                            pangu_uk_t2m[, c('quanobs', 't2m_fcst', 't2m')], 
                            by = c('quanobs')) |>
  mutate(t2m_rf = `beta0` + `beta1`*`t2m_fcst`) |>
  ungroup() |> 
  summarise(Mse_rf = mean((t2m - t2m_rf)^2),
            Mse_Pw = mean((t2m - t2m_fcst)^2)) |>
  mutate(Sksc_Pw = 1 - (Mse_rf/Mse_Pw))
sk_quanfcst_t2m = inner_join(coef_quanfst_t2m[, c('quanfcst', 'beta0', 'beta1')], 
                             pangu_uk_t2m[, c('quanfcst', 't2m_fcst', 't2m')], 
                             by = c('quanfcst')) |>
  mutate(t2m_rf = `beta0` + `beta1`*`t2m_fcst`) |>
  ungroup() |> 
  summarise(Mse_rf = mean((t2m - t2m_rf)^2),
            Mse_Pw = mean((t2m - t2m_fcst)^2)) |>
  mutate(Sksc_Pw = 1 - (Mse_rf/Mse_Pw))


########## Generalized Additive Model (for temperature)
library(mgcv)
library(dplyr)
library(purrr)
library(tidyr)


## Recalibration of temperature with generalized additive model (t2m) (for each month)
rec_mon_t2m = pangu_uk |>
  select(date, lat, lon, t2m, t2m_fcst) |>
  mutate(month = format(as.Date(date), '%m')) |>
  group_by(month) |>
  nest() |>
  mutate(model = map(data, ~ gam(t2m ~ s(t2m_fcst), data = .)),
         recalibrated = map2(data, model, ~ {
           predicted = predict(.y, newdata = .x)
           .x |> mutate(t2m_rf = predicted)
         })) |>
  unnest(recalibrated) |>
  select(month, date, lat, lon, t2m, t2m_fcst, t2m_rf)
sk_mon_t2m = rec_mon_t2m |> 
  ungroup() |>
  summarise(Mse_rf = mean((t2m - t2m_rf)^2),
            Mse_Pw = mean((t2m - t2m_fcst)^2)) |>
  mutate(Sksc = 1 - (Mse_rf/Mse_Pw))
# result: MSE_rf = 0.3161565, MSE_Pw = 0.3174442, Skill score = 0.004056285

## Recalibration of temperature with generalized additive model(t2m) (for each location)
rec_loc_t2m = pangu_uk |>
  select(date, lat, lon, t2m, t2m_fcst) |> 
  group_by(lat, lon) |> 
  nest() |> 
  mutate(model = map(data, ~ gam(t2m ~ s(t2m_fcst), data = .)), 
         recalibrated = map2(data, model, ~ { 
           predicted = predict(.y, newdata = .x) 
           .x |> mutate(t2m_rf = predicted) 
         })) |> 
  unnest(recalibrated) |> 
  select(date, lat, lon, t2m, t2m_fcst, t2m_rf)
sk_loc_t2m = rec_loc_t2m |> 
  ungroup() |>
  summarise(Mse_rf = mean((t2m - t2m_rf)^2), 
            Mse_Pw = mean((t2m - t2m_fcst)^2)) |> 
  mutate(Sksc = 1 - (Mse_rf/Mse_Pw))  
# result: MSE_rf = 0.2997657, MSE_Pw = 0.3174442, Skill score = 0.05568991

## Recalibration of temperature with generalized additive model(t2m) (for each location and month)
rec_gam_t2m = pangu_uk |>
  select(date, lat, lon, t2m, t2m_fcst) |> 
  mutate(month = format(as.Date(date), '%m')) |> 
  group_by(month, lat, lon) |> 
  nest() |> 
  mutate(model = map(data, ~ gam(t2m ~ s(t2m_fcst), data = .)), 
         recalibrated = map2(data, model, ~ { 
           predicted = predict(.y, newdata = .x) 
           .x |> mutate(t2m_rf = predicted) 
         })) |> 
  unnest(recalibrated) |> 
  select(month, date, lat, lon, t2m, t2m_fcst, t2m_rf)
sk_gam_t2m = rec_gam_t2m |> 
  ungroup() |>
  summarise(Mse_rf = mean((t2m - t2m_rf)^2), 
            Mse_Pw = mean((t2m - t2m_fcst)^2)) |> 
  mutate(Sksc = 1 - (Mse_rf/Mse_Pw))                                         
# result: MSE_rf = 0.2876376, MSE_Pw = 0.3174442, Skill score = 0.09389538

rec_model1 <- pangu_uk |> 
  select(date, lat, lon, t2m, t2m_fcst, mslp_fcst) |>
  mutate(month = format(as.Date(date), '%m')) |>
  group_by(month, lat, lon) |> 
  do({
    train_data = . |> sample_n(100)
    model <- gam(t2m ~ s(t2m_fcst) + s(mslp_fcst), data = train_data)
    . |> mutate(t2m_rf = predict(model, newdata=.))}) |>
  ungroup() |> 
  summarise(Mse_rf = mean((t2m - t2m_rf)^2), 
            Mse_Pw = mean((t2m - t2m_fcst)^2)) |> 
  mutate(Sksc = 1 - (Mse_rf/Mse_Pw))

rec_model2 <- pangu_uk |> 
  select(date, lat, lon, t2m, t2m_fcst, mslp_fcst) |>
  mutate(month = format(as.Date(date), '%m')) |>
  group_by(month, lat, lon) |> 
  do({
    train_data = . |> sample_n(200)
    model <- gam(t2m ~ s(t2m_fcst) + s(mslp_fcst), data = train_data)
    . |> mutate(t2m_rf = predict(model, newdata=.))}) |>
  ungroup() |> 
  summarise(Mse_rf = mean((t2m - t2m_rf)^2), 
            Mse_Pw = mean((t2m - t2m_fcst)^2)) |> 
  mutate(Sksc = 1 - (Mse_rf/Mse_Pw))

rec_model3 <- pangu_uk |> 
  select(date, lat, lon, t2m, t2m_fcst, mslp_fcst) |>
  mutate(month = format(as.Date(date), '%m')) |>
  group_by(month, lat, lon) |> 
  do({
    train_data = . |> sample_n(300)
    model <- gam(t2m ~ s(t2m_fcst) + s(mslp_fcst), data = train_data)
    . |> mutate(t2m_rf = predict(model, newdata=.))}) |>
  ungroup() |> 
  summarise(Mse_rf = mean((t2m - t2m_rf)^2), 
            Mse_Pw = mean((t2m - t2m_fcst)^2)) |> 
  mutate(Sksc = 1 - (Mse_rf/Mse_Pw))

rec_model4 <- pangu_uk |> 
  select(date, lat, lon, t2m, t2m_fcst, mslp_fcst) |>
  mutate(month = format(as.Date(date), '%m')) |>
  group_by(month, lat, lon) |> 
  do({
    train_data = . |> sample_n(400)
    model <- gam(t2m ~ s(t2m_fcst) + s(mslp_fcst), data = train_data)
    . |> mutate(t2m_rf = predict(model, newdata=.))}) |>
  ungroup() |> 
  summarise(Mse_rf = mean((t2m - t2m_rf)^2), 
            Mse_Pw = mean((t2m - t2m_fcst)^2)) |> 
  mutate(Sksc = 1 - (Mse_rf/Mse_Pw))

rec_model5 <- pangu_uk |> 
  select(date, lat, lon, t2m, t2m_fcst, mslp_fcst) |>
  mutate(month = format(as.Date(date), '%m')) |>
  group_by(month, lat, lon) |> 
  do({
    train_data = . |> sample_n(500)
    model <- gam(t2m ~ s(t2m_fcst) + s(mslp_fcst), data = train_data)
    . |> mutate(t2m_rf = predict(model, newdata=.))}) |>
  ungroup() |> 
  summarise(Mse_rf = mean((t2m - t2m_rf)^2), 
            Mse_Pw = mean((t2m - t2m_fcst)^2)) |> 
  mutate(Sksc = 1 - (Mse_rf/Mse_Pw))

rec_model6 <- pangu_uk |> 
  select(date, lat, lon, t2m, t2m_fcst, mslp_fcst) |>
  mutate(month = format(as.Date(date), '%m')) |>
  group_by(month, lat, lon) |> 
  do({
    train_data = . |> sample_n(600)
    model <- gam(t2m ~ s(t2m_fcst) + s(mslp_fcst), data = train_data)
    . |> mutate(t2m_rf = predict(model, newdata=.))}) |>
  ungroup() |> 
  summarise(Mse_rf = mean((t2m - t2m_rf)^2), 
            Mse_Pw = mean((t2m - t2m_fcst)^2)) |> 
  mutate(Sksc = 1 - (Mse_rf/Mse_Pw))

rec_all <- rbind(rec_model1, rec_model2, rec_model3, rec_model4, rec_model5, rec_model6)
sample_sizes <-c(100, 200, 300, 400, 500, 600)
rec_all$sample_sizes <- sample_sizes
ggplot(rec_all, aes(x = sample_sizes, y = Mse_rf)) +
  geom_line() + 
  geom_point() + 
  geom_hline(yintercept = sk_loc_mon_t2m$Mse_rf, linetype = 'dashed', color = 'red') +
  annotate('text', x = 500, y = sk_loc_mon_t2m$Mse_rf, 
           label = 'MSE of simple linear regression', color = 'red', vjust = -0.5, size = 4) +
  geom_hline(yintercept = sk_gam_t2m$Mse_rf, linetype = 'dashed', color = 'blue') +
  annotate('text', x = 200, y = sk_gam_t2m$Mse_rf, 
           label = 'MSE of GAM(t2m ~ t2m_fcst)', color = 'blue', vjust = -0.5, size = 4) +
  labs(title = 'MSE of GAM (t2m ~ t2m_fcst + mslp_fcst) each sample sizes', x = 'Sample sizes', y = 'MSE') +
  theme_minimal()            
############################## End of Chapter 2 ##############################


############################## Chapter 3 ##############################
########## Smoothing parameters
coeflm_loc_t2m = pangu_uk |> 
  select(lat, lon, t2m, t2m_fcst) |> 
  group_by(lat, lon) |> 
  summarise(coefs = list(coef(lm(t2m ~ t2m_fcst)))) |>
  mutate(beta0 = map_dbl(coefs, ~ .[1]), 
         beta1 = map_dbl(coefs, ~ .[2]))

model_beta0 <- gam(beta0 ~ s(lat, lon), data = coeflm_loc_t2m)
model_beta1 <- gam(beta1 ~ s(lat, lon), data = coeflm_loc_t2m)
smooth_beta0 <- predict(model_beta0, newdata = coeflm_loc_t2m)
smooth_beta1 <- predict(model_beta1, newdata = coeflm_loc_t2m)
coeflm_loc_t2m$smooth_beta0 <- smooth_beta0
coeflm_loc_t2m$smooth_beta1 <- smooth_beta1

ggplot(coeflm_loc_t2m) + 
  geom_raster(aes(x = lon, y = lat, fill = smooth_beta0)) +
  borders(regions = c('UK', 'Ireland'), colour = 'black') +
  theme_minimal() +
  ggplot(coeflm_loc_t2m) + 
  geom_raster(aes(x = lon, y = lat, fill = smooth_beta1)) +
  borders(regions = c('UK', 'Ireland'), colour = 'black') +
  theme_minimal()

smo_loc_t2m = inner_join(coeflm_loc_t2m[, c('lat', 'lon', 'smooth_beta0', 'smooth_beta1')], 
                         pangu_uk[, c('lat', 'lon', 't2m_fcst', 't2m')], 
                         by = c('lat', 'lon')) |>
  mutate(t2m_rf = `smooth_beta0` + `smooth_beta1`*`t2m_fcst`) |>
  ungroup() |> 
  summarise(Mse_rf = mean((t2m - t2m_rf)^2),
            Mse_Pw = mean((t2m - t2m_fcst)^2)) |>
  mutate(Sksc = 1 - (Mse_rf/Mse_Pw))




