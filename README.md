FORESFUTURE management
================
Miquel De Cáceres/Giuseppe Capizzi
2024-10-31

## Goals

## R scripts

### Preprocessing

#### Common

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

- *A6_demand_function.R*: Generates wood demand for FORMES as a function
  of growth in the previous step.
- *A3b_run_scenarios_function.R*: Runs FORMES from preceeding step and
  stores the demand for further steps.
- *A3_run_scenarios.r*: Runs FORMES scenarios.
- *A7b_WaterBalance_Miquel.R*: Runs water balance using medfate for each
  scenario and 10-year step.
- *A7c_PARground_Miquel.R*: Calculates PAR at the ground level, using
  medfate, for each scenario and 10-year step.

#### FORDYN

- *5_SimulationReal.R*: Runs historical period (2001-2010 and 2011-2020)
  and simulated periods under two climate scenarios and different
  management scenarios (BAU, AMF, RSB, ASEA, ACG and NOG).
- *A2_utils.R*: Ancillary functions for FORDYN simulations (volume
  function, summary functions, etc)

### Postprocessing

#### FORMES

- *B2c_demand_cal.R*: Allows extract actual demand and offset from
  FORMES
- *B2b_utils.R*: Utility functions for FORMES (post-processing?)
- *B2_BuildingResults_FORMES.R*: Binds results for FORMES, by province,
  climate scenario and management scenario.
- *A4_FORMES_WB_BindingResults.R*: Binds water balance results for
  FORMES.
- *B8b_my_plots.R*: Analysis of results for FORMES.

#### FORDYN

- *6_BindingResults_FORDYN.R*: Combines the simulation results of
  different years for a given province, management scenario, climate
  scenario.
- *7_AnnualIndicators_FORDYN.R*: Generates annual indicators for a given
  management scenario and climate scenario (merges provinces).
- *8_Plots_tables_FORDYN.R*: Analysis of annual results for FORDYN.

#### Common

- *9a_EcosystemServices_calculation.R*: Functions to calculate forest
  ecosystem services. Two kinds of ES exist, those defined by period and
  those defined for a given forest state.
- *C10_corrections_RSB_FORMES.R*: Plots in RSB scenario, that were not
  simulated in FORMES, but whose water balance existed in FORDYN.
  Results from FORDYN are taken as FORMES results.
- *9b_EcosystemServices_plots.R*: Maps and trend plots for forest
  ecosystem services.
- *C10_corrections_RSB_FORMES.R*: Plots of structural dynamics,
  extraction rates,…
- *B10_climatic_plots.R*: Plots of climate trends under each scenarios.
- *B11_tables_ES.R*: Tables of mean ecosystem service values across
  plots for each climate and management scenario.

## Data folders

### Raw-data

- *mpiesm_rca4_rcp\_\[x\]*daily_interpolator*\[y\].nc*: Interpoladors
  climatic
- *AprofitamentsForestals_EspècieProvíncia_2001-2009.xlsx*

#### FORDYN

- *CO2_escenarios.xlsx*: CO2 concentration by year and climatic
  scenario.
- *prescription_by_spp.xlsx*: Silviculture prescriptions by species
  (FORDYN)

#### FORMES

- *SilviculturalPrescriptions_OCCC.xlsx*: Silviculture prescriptions by
  species (FORMES)

### data

#### Common

- *forestfuture_sf.rds*: Input sf with plot selection
- *forestfuture_lq_sf.rds*: Input sf with low quality data
- *nfiplots.rds*: Plot data, including protected areas mask and
  priorizations for land use changes.
- *aprofit_decade_prov_spp.txt*: Wood demand per species and province,
  averaged for 2001-2020 period.
- \*ES/ES\_\[period/state\]\_\[FORMES/FORDYN\].rds\*: Forest ecosystem
  services for a given model, including all scenarios.

#### FORDYN

- \*\[scenario\]/\[scenario\]*\[province\]*\[climate_model\]*\[climate_scenario\]*\[year_ini\]\_\[year_fin\].rds\*:
  Simulation output
- \*binded/\[province\]*\[scenario\]*\[climate_model\]\_\[climate_scenario\].res\*:
  Binded simulation results for a given province, scenario and climate.
- *annual_indicators/\[management_scenario\]*\[climate_model\]*\[climate_scenario\].rds*:
  Annual indicators (province merged).

#### FORMES

- *forest_future_tree_data.rds* : Input tree data for FORMES
- \*nfi\_\[rcp\]\_\[y\].rds\*: Interpolated climate projections
- *SWHC_OCCC.rds*: Soil water holding capacity for FORMES.
- *treedata_FORMES.rds*: Input data for FORMES (2001).
- \*\[scenario\]/\[scenario\]/\[scenario\]*\[climate_scenario/\[scenario\]*\[climate_scenario\]\_\[step\]/\[province\].rds\*:
  Simulation output
- \*WB/\[scenario\]/wb\_\[climate_scenario\]*\[province\]*\[year_ini\]\_\[year_fin\].rds\*:
  Water balance simulation results.
- \*PARground/\[scenario\]/PARground\_\[climate_scenario\]*\[province\]*\[year_ini\]\_\[year_fin\].rds\*:
  Estimation of PAR at ground level.
