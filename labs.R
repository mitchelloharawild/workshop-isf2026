library(fpp3)

PBS |> 
  summarise(Scripts = sum(Scripts)) |> 
  features(Scripts, guerrero)

canadian_gas |> 
  autoplot(Volume)

aus_retail |> 
  summarise(Turnover = sum(Turnover)) |> 
  autoplot(Turnover)


aus_retail |> 
  summarise(Turnover = sum(Turnover)) |> 
  model(STL(Turnover)) |> 
  components() |> 
  autoplot()

aus_retail |>
  summarise(Turnover = sum(Turnover)) |> 
  model(STL(box_cox(Turnover, lambda = 0.2))) |> 
  components() |> 
  as_tsibble() |> 
  select(-.model) |> 
  model(SNAIVE(season_year)) |> 
  forecast(h="10 years") |> 
  autoplot()

aus_retail |> 
  summarise(Turnover = sum(Turnover)) |>
  model(ARIMA(box_cox(Turnover, 0.2) ~ PDQ(0,0,0) + fourier("year", K = 4))) |> 
  forecast(h="10 years") |> 
  autoplot(aus_retail |> summarise(Turnover = sum(Turnover)))

as_tsibble(USAccDeaths) |> 
  model(
    dummy = TSLM(value ~ trend() + season()),
    fourier = TSLM(value ~ trend() + fourier("year", K = 3))
  ) |> 
  forecast(h = "2 years") |> 
  autoplot(
    as_tsibble(USAccDeaths),
    level = NULL
  )

aus_retail |> 
  filter(Industry == "Cafes, restaurants and catering services") |>
  summarise(Turnover = sum(Turnover)) |> 
  model(ETS(Turnover)) |> 
  gg_tsresiduals()


aus_retail |> 
  filter(Industry == "Cafes, restaurants and catering services") |>
  summarise(Turnover = sum(Turnover)) |> 
  model(ETS(Turnover)) |> 
  forecast(bootstrap = TRUE)

PBS |> 
  distinct(ATC1_desc)
PBS |> 
  distinct(ATC2_desc)
PBS |> 
  distinct(Concession, Type)

PBS |> 
  summarise(Scripts = sum(Scripts)) |>
  autoplot()

fit <- PBS |> 
  aggregate_key((ATC1 / ATC2) * Concession, Scripts = sum(Scripts)) |> 
  model(ETS(Scripts))
fit
fc <- fit |> 
  forecast()

fc |> 
  slice(7000:7010)

fit |> 
  coherent_smat() |> 
  Matrix::image()
fit |> 
  coherent_cmat() |> 
  Matrix::image()


## DOING IT ALL!

mdl <- combination_ensemble(
  ETS(Trips),
  ARIMA(Trips),
  decomposition_model(
    STL(Trips ~ trend(window = 7) + season(window = "periodic")),
    ETS(season_adjust),
    SNAIVE(season_year)
  )
)

fit <- tourism |> 
  aggregate_key(Purpose, Trips = sum(Trips)) |> 
  model(mdl) |> 
  reconcile(mdl = min_trace(mdl))

fit |> 
  forecast()
