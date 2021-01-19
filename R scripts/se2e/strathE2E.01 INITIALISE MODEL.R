
## Initialise model

#### Setup ####

rm(list=ls())                                                               # Wipe the brain

library(StrathE2E2)

e2e_copy(model.name = "North_Sea", model.variant =  "1970-1999" , dest.path = "./StrathE2E") # Copy example model

file.rename("./StrathE2E/Models/North_Sea", "./StrathE2E/Models/Barents Sea")                # Rename model
file.rename("./StrathE2E/Models/Barents Sea/1970-1999", "./StrathE2E/Models/Barents Sea/2011-2019") # Rename variant

e2e_copy(model.name = "North_Sea", model.variant =  "1970-1999" , dest.path = "./StrathE2E") # Keep example model

Physics_template <- read.csv("./StrathE2E/Models/Barents Sea/2011-2019/Driving/physics_NORTH_SEA_1970-1999.csv") # Read in example Physical drivers
Boundary_template <- read.csv("./StrathE2E/Models/Barents Sea/2011-2019/Driving/chemistry_NORTH_SEA_1970-1999.csv")  # Read in example boundary drivers
