FORESFUTURE management
================
Miquel De Cáceres/Giuseppe Capizzi
2024-10-31

## Goals

## R scripts

### Preprocessing

- *1_Target_plots.R*: Read IFN data, subset plot types, determine
  dominant species, isolates tree data for FORMES, get IFN strata for
  spatial upscaling.
- *2_LowQuality.R*: Estimate low quality plots using surface stoniness
  (IFN3) and annual precipitation.
- *A1_masks_priorizations.r*: Create mask of protected areas, determine
  which plots will change ther land use to agriculture/pasture under
  scenario RSB.
- *A2_demanda_provincia.r*: Estimate wood demand per decade and species
  using data from 2001-2020 period.
- *A3_ErosionData.R*: Preparation of static variables for erosion
  estimation (requires data outside repository).
- *A8_LCCmap.r*: Creates a map of land use changes for RSB scenario.

#### FORDYN

- *3_RockContentEstimation.R*: Estimation of rock fragment content for
  FORDYN.
- *4_RockContentAnalysis.R*: Analysis of the results of rock fragment
  content estimation.

#### FORMES

- *A4_climate_interpolation.R*: Interpolacio del clima diari
  (projeccions)
- *A5_SWHC.r*: Estimate soil water holding capacity for FORMES.

### Simulation

#### FORMES

#### FORDYN

### Postprocessing

#### FORMES

#### FORDYN

## Data folders

### Raw-data

- *mpiesm_rca4_rcp\_\[x\]*daily_interpolator*\[y\].nc*: Interpoladors
  climatic
- *AprofitamentsForestals_EspècieProvíncia_2001-2009.xlsx*

### data

- *forestfuture_sf.rds*: Input sf with plot selection
- *forestfuture_lq_sf.rds*: Input sf with low quality data
- *nfiplots.rds*: Plot data, including protected areas mask and
  priorizations for land use changes.
- *aprofit_decade_prov_spp.txt*: Wood demand per species and province,
  averaged for 2001-2020 period.

#### FORDYN

#### FORMES

- *forest_future_tree_data.rds* : Input tree data for FORMES
- \*nfi\_\[rcp\]\_\[y\].rds\*: Interpolated climate projections
- *SWHC_OCCC.rds*: Soil water holding capacity for FORMES.

#### FORDYN
