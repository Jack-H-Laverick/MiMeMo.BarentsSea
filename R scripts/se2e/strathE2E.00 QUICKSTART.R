
library(StrathE2E2)
library(tictoc)

tic()
model <- read_model("North_Sea","1970-1999")
results <- StrathE2E(model,nyears=5)

tic()
plot_full_length_timeseries(model, results)
toc()
toc()