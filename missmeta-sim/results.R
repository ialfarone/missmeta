library(tidyverse)

sim = readRDS("sim_res.rds")
sim

# fix common names

sim$res = lapply(sim$res, function(x) {
  cm = intersect(names(x), names(sim))
  select(x, -all_of(cm))
})

sim |> 
  unnest(res) |> 
  saveRDS(file = "sim_res.rds")
