# Objective: Use Matilda to generate an ensemble of Hector results to be used
# in the Hector-Facts workflow and compress the Hector inputs that will be
# used in the Hector-FACTS pipeline.
# TODOs: Areas for potential improvement
#   1) Figure out a more robust criteria for filtering
#   2) Better way to handle the hector params compression step
# 0. Set Up --------------------------------------------------------------------
# Required packages
library(dplyr)
remotes::install_github("jgcri/hector@v3.2.0")
stopifnot(packageVersion("hector") == "3.2.0")
library(hector)
library(matilda)

# Set the seed to be reproducible
set.seed(42)

# Set up locations for output
BASEDIR <- here::here()

# Option to write intermediate files to a temp file, it can be helpful when
# debugging but also not necessary.
WRITE_TO_TMP <- TRUE
if(WRITE_TO_TMP){
    TMP_OUTDIR <- file.path(BASEDIR, "TEMP")
    dir.create(TMP_OUTDIR, showWarnings = FALSE)
}


# 1. Generate parameters, run Hector, & score results  -------------------------
ini <- system.file("input/hector_ssp245.ini", package = "hector")
hc <- newcore(ini)
param_values <- generate_params(hc, draws = 5000)

if(WRITE_TO_TMP){
    write.csv(param_values, file = file.path(TMP_OUTDIR, "param_values.csv"),
              row.names = FALSE)
}

# Run Hector repeatedly over all parameter values
rslts <- iterate_model(core = hc, params = param_values)


if(WRITE_TO_TMP){
    write.csv(rslts, file = file.path(TMP_OUTDIR, "all_hector_rslts.csv"),
              row.names = FALSE)
    rslts <- read.csv(file.path(TMP_OUTDIR, "all_hector_rslts.csv"))
}


# Score Hector runs with observed CO2 data
scores <- score_runs(rslts, criterion_co2_obs(), score_ramp, w1 = 2, w2 = 20)

if(WRITE_TO_TMP){
    write.csv(scores, file = file.path(TMP_OUTDIR, "score_rslts.csv"),
              row.names = FALSE)
}

# TODO address this!
# This cut off was honestly chosen some what arbitrarily we should revisit this!
cut_off <- 0.00020
to_keep <- which(scores$weights >= cut_off)

params <- param_values[to_keep, ]
params <- cbind("simulation" = 1:nrow(params), params)
row.names(params) <- NULL

if(WRITE_TO_TMP){
    write.csv(params, file = file.path(TMP_OUTDIR, "cehck_these_params.csv"),
              row.names = FALSE)
    params <- read.csv(file.path(TMP_OUTDIR, "cehck_these_params.csv"))
}

# 2. Double check that parameters actually run!  -------------------------
# Function that runs Hector with the various parameter combinations to
# try and prevent bad parameter combos from being used in the Hector-FACTS
# This function is designed to be applied to a data frame
# Args
#   hc: active hector object, should be created from the ini that goes out to 2500 that will be used in the run
#   pars: named numeric vector containing the parameter values to use in a single hector run
# Return: Boolean indicator that 0 (no errors) or 1 (failed to run)
runhector <- function(hc, pars){

    years <- 1750:2500

    tryCatch({

        # Not the best way to do this but will do for now
        setvar(hc, dates = NA, var = BETA(), values = pars[[BETA()]], unit = getunits(BETA()))
        setvar(hc, dates = NA, var = Q10_RH(), values = pars[[Q10_RH()]], unit = getunits(Q10_RH()))
        setvar(hc, dates = NA, var = NPP_FLUX0(), values = pars[[NPP_FLUX0()]], unit = getunits(NPP_FLUX0()))
        setvar(hc, dates = NA, var = AERO_SCALE(), values = pars[[AERO_SCALE()]], unit = getunits(AERO_SCALE()))
        setvar(hc, dates = NA, var = DIFFUSIVITY(), values = pars[[DIFFUSIVITY()]], unit = getunits(DIFFUSIVITY()))
        setvar(hc, dates = NA, var = ECS(), values = pars[[ECS()]], unit = getunits(ECS()))
        reset(hc)

        # TODO set up ini and emissions tables that extend all the way to 2500 so we can
        # have Hector results to 2500 without doing the padding with NAs.
        run(hc, runtodate = max(years))
        0
    }, error = function(e){
        return(1)
    }) ->
        out

    return(out)

}

# Path to the local ini file that will be used in the Hector-FACTS set up
hc_ssp585 <- newcore(file.path(BASEDIR, "data","hector-input", "hector_ssp585.ini"))

# Run hector for all the different parameter combinations
ssp585_rstlts <- apply(params, 1, runhector, hc = hc_ssp585)

if(WRITE_TO_TMP){
    write.csv(ssp585_rstlts, file = file.path(TMP_OUTDIR, "ssp585_rstlts.csv"),
              row.names = FALSE)
    ssp585_rstlts <- read.csv(file.path(TMP_OUTDIR, "ssp585_rstlts.csv"))
}

# Remove the parameters that throw errors!
to_drop <- which(ssp585_rstlts$x == 1)
params <- read.csv(file.path(TMP_OUTDIR, "cehck_these_params.csv"))
final_params <- params[-to_drop, ]

# Re index the simulations and rename the columns to be compatible with hector setvar.
colnames(final_params) <- c("simluation", BETA(), Q10_RH(), NPP_FLUX0(), AERO_SCALE(), DIFFUSIVITY(), ECS())
final_params$simulation <- 1:nrow(final_params)

# 2. Save Results  -------------------------------------------------------------
write.csv(final_params, file = file.path("hector_params.csv"),
          row.names = FALSE)

# Need to change the working directory in order to do the file compression (not ideal) that
# is needed for the FACTS framework but let's make sure to reset it back.
system(paste("tar -czf", "hector_params.tgz", "hector_params.csv"))


# 3. Compress the hector-inputs directory  -------------------------------------
system(paste("tar -czvf", "data/hector-input.tgz", "hector-input"))


