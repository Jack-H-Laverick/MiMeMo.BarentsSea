<img src='./Figures/rayshade-hi.png' align="center" width="100%" />

MiMeMo.BarentsSea
=================

This page details at a broad level the code written to extract data and
parameterise StrathE2E*Polar* as part of MiMeMo. You can find
information on the depedencies, runtimes, and tasks performed by each of
the scripts. Scripts are loosely classified into families - *bathymetry*
- *fish* - *flows* - *nemo-medusa* - *saltless* - *StrathE2E*. These can
all be run from a master script, with a region file allowing the user to
define the model domain for data extraction.

Script Objectives
-----------------

In the future I plan to detail blow by blow what exactly each script
does. This will likely take the form of a dropdown list of vignettes on
this website. As there are about 40 scripts at the moment this isn’t
going to happen immediately. For now this section is here to briefly
mention the purpose of each script.

### bathymetry

| Script           | Purpose |
|------------------|---------|
| 1 DATA WRANGLING |         |
| 2 PLOTTING       |         |
| 3 RAYSHADER      |         |
| 4 DOMAIN CHOICES |         |
| 5 DEFINE DOMAIN  |         |

### fish

| Script                   | Purpose |
|--------------------------|---------|
| 1 FAO REGIONS            |         |
| 2 SUMMARIES AND PLOTTING |         |
| 3 DATA WRANGLING         |         |
| 4 PLOTTING               |         |

### flows

| Script             | Purpose |
|--------------------|---------|
| 1 VC-EXTRACTION    |         |
| 2 MAKE TRANSECTS   |         |
| 3 LABEL TRANSECTS  |         |
| 4 SAMPLE PERIMETER |         |
| 5 SAMPLE FLUXES    |         |
| 6 VOLUME CHECK     |         |
| 7 PLOT EXCHANGES   |         |

### nemo-medusa

| Script           | Purpose |
|------------------|---------|
| 1 GRID           |         |
| 2 EXTRACTION     |         |
| 3 SPATIAL        |         |
| 4 TIME SERIES    |         |
| 5 PLOTTING       |         |
| 6 LIGHT AND TEMP |         |

### saltless

| Script                        | Purpose |
|-------------------------------|---------|
| 2 COMPILE DRIVING DATA        |         |
| 3 COMPILE PHYSICAL PARAMETERS |         |

### StrathE2E

| Script                        | Purpose |
|-------------------------------|---------|
| 2 COMPILE DRIVING DATA        |         |
| 3 COMPILE PHYSICAL PARAMETERS |         |

Run Times
---------

<img src="README_files/figure-markdown_github/code runtimes-1.png" width="100%" />
