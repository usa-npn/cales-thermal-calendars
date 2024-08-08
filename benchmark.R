library(tidyverse)
library(units)
library(ggtext)

meta <- read_csv("tar_meta_hpc.csv")

meta |> 
  filter(str_starts(name, "gam_"), !str_detect(name, "df")) |> 
  select(name, bytes, seconds) |> 
  separate(name, into = c("trash", "resolution", "df")) |> 
  select(-trash) |> 
  mutate(
    resolution_km = as.numeric(resolution)/1000,
    minutes = seconds/60,
    size_mb = set_units(bytes, "bytes") |> set_units("megabytes") |> as.numeric(),
    df = fct_inseq(df)
  ) |> 
  #filter out any that are probably "null"
  filter(bytes > 500) |> 
  ggplot(aes(x = resolution_km, y = minutes, color = df)) +
  geom_line() +
  geom_point() +
  scale_x_reverse() +
  scale_color_viridis_d(option = "D", end = 0.8) +
  labs(
    title = "Time spent on HPC fitting models",
    color = "k (df)",
    x = "Resolution (km)",
    y = "Time (minutes)"
  ) 
           

## check k

k_check <- read_csv("k_check.csv")
k_check |> 
  filter(term == "ti(y,x)") |> 
  mutate(res_km = factor(res_m/1000), res_km = fct_inseq(res_km)) |> 
  ggplot(aes(x = k, y = edf, color = res_km)) + 
  geom_line() +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = 2, alpha = 0.5) +
  scale_color_viridis_d(direction = -1, end = 0.8) +
  labs(
    title = "Results of `k.check()` for the `ti(x,y)` spatial smoother",
    caption = "Lines dropping below the 1:1 line (dashed) indicate that enough degrees of freedom have been supplied.",
    color = "Resolution (km)"
  ) +
  theme(
    plot.caption = element_textbox_simple(margin = margin(t=10, b = 5))
  )
