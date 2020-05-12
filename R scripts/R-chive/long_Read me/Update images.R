
# Update the images used in the readme file to the most recently saved figures

rm(list=ls())                                                 # Wipe the brain

folder <- "~/R scripts/Read me/images"                        # Path to folder

unlink(folder, recursive = TRUE)                              # Delete old folder and contents

dir.create(folder)                                            # Recreate folder

images <- c("~/Data/Bathymetry GEBCO/MiMeMo Schematic3Dice_cap.cont.png", # List images used in the readme file with path
            "~/Data/Bathymetry GEBCO/FIG_GEBCO Polar Bathymetry.png",
            "~/Data/Bathymetry GEBCO/FIG_GEBCO many contours.png",
            "~/Data/Bathymetry GEBCO/FIG_depth choices2.png",
            "~/Data/Bathymetry GEBCO/FIG_distance from shore.png",
            "~/Data/Bathymetry GEBCO/FIG_Polygon dropping.png",
            "~/Data/Bathymetry GEBCO/FIG_Domains.png",
            "~/Data/MiMeMo/FIG_SP_Temperature S 2030.png",
            "~/Data/MiMeMo/FIG_TS_Temperature_avg.png",
            "~/Data/MiMeMo/FIG_TS_Vertical currents.png",
            "~/Data/MiMeMo/FIG_Segments.png",
            "~/Data/MiMeMo/FIG_curent direction example.png")      

sapply(images, file.copy, to = folder)                        # Refill with most recent image files
  