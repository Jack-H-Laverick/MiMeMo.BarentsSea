
## Create model variants for new time periods

rm(list=ls())                                                                  # Wipe the brain
library(MiMeMo.tools)

#### Mid-century ####

R.utils::copyDirectory("./StrathE2E/Models/Barents Sea/2011-2019",             # Copy the parameterised model
                       "./StrathE2E/Models/Barents Sea/2040-2049")             # To make a new variant

update_boundary_period(2050, 2059, "./StrathE2E/Models/Barents Sea/2040-2049") # Re-parameterise boundary conditions
update_physics_period(2050, 2059, "./StrathE2E/Models/Barents Sea/2040-2049")  # Re-parameterise physics
# The physical parameters file isn't changed with time.

#### End of century ####

R.utils::copyDirectory("./StrathE2E/Models/Barents Sea/2011-2019",          
                       "./StrathE2E/Models/Barents Sea/2090-2099")          

update_boundary_period(2090, 2099, "./StrathE2E/Models/Barents Sea/2090-2099")
update_physics_period(2090, 2099, "./StrathE2E/Models/Barents Sea/2090-2099")

#### AMBIO ####

R.utils::copyDirectory("./StrathE2E/Models/Barents Sea/2011-2019",             # Copy the parameterised model
                       "./StrathE2E/Models/Barents Sea/2030-2039")             # To make a new variant

R.utils::copyDirectory("./StrathE2E/Models/Barents Sea/2011-2019",          
                       "./StrathE2E/Models/Barents Sea/1990-1999")          

update_boundary_period(2030, 2039, "./StrathE2E/Models/Barents Sea/2030-2039") # Re-parameterise boundary conditions
update_physics_period(2030, 2039, "./StrathE2E/Models/Barents Sea/2030-2039")  # Re-parameterise physics

update_boundary_period(1990, 1999, "./StrathE2E/Models/Barents Sea/1990-1999")
update_physics_period(1990, 1999, "./StrathE2E/Models/Barents Sea/1990-1999")

