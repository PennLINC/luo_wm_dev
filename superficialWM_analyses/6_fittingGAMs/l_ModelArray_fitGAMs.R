
library(ModelArray)
 
################## 
# Set Variables 
################## 
args <- commandArgs(trailingOnly=TRUE)
config_file <- args[1]
scalar <- args[2]
depth <- args[3]

################## 
# Set Directories 
################## 
config <- read.csv(config_file)
outputs_root <- config$outputs_root
outdir <-  paste0(config$outputs_root, "/GAM/", scalar)

if (!dir.exists(outdir)) {
  # If directory doesn't exist, create it
  dir.create(outdir, recursive = TRUE)
  print(paste("Directory", outdir, "created."))
} else {
  print(paste("Directory", outdir, "already exists."))
}

################## 
# Define Functions
################## 
fitGAMs_ModelArray <- function(scalar, outdir) {
    
  # Create a ModelArray-class object
  # filename of dsistudio scalar data (.h5 file):
  h5_path <- sprintf("%1$s/all_subjects/h5_files/%2$s/%2$s_%3$s.h5", outputs_root, scalar, depth)
  #h5_path <- "/cbica/projects/luo_wm_dev/output/HCPD/superficialWM/all_subjects/h5_files/dti_fa/dti_fa_depth_1p25.h5"
  
  # create a ModelArray-class object:
  modelarray <- ModelArray(h5_path, scalar_types = c(scalar))
  # let's check what's in it:
  #modelarray
  #scalars(modelarray)[["dti_fa"]]
  
  phenotypes <- read.csv(sprintf("%1$s/all_subjects/cohortfiles/dsistudio_scalars/%2$s/%2$s_%3$s_cohortfile.csv", outputs_root, scalar, depth))
  
  # formula:
  smooth_var <- "age"
  covariates <- "sex + mean_fd"
  formula.gam <- as.formula(sprintf("%s ~ s(%s, k=3, fx=T) + %s", scalar, smooth_var, covariates))
 
  
  
  print(paste0("fitting GAMs for ", scalar))
  
  mygam.try <- ModelArray.gam(
    formula.gam,
    modelarray,
    phenotypes,
    scalar,
    changed.rsq.term.index = c(1)
  )
  
  saveRDS(mygam.try, sprintf("%1$s/GAMresults.%2$s.age.RData", outdir, scalar))
}


fitGAMs_ModelArray_GMfiltered <- function(scalar, outdir, depth) {
  
   
  # Create a ModelArray-class object
  # filename of dsistudio scalar data (.h5 file):
  h5_path <- sprintf("%1$s/all_subjects/h5_files/%2$s/%2$s_%3$s_GMfiltered_noMW.h5", outputs_root, scalar, depth)
   
  # create a ModelArray-class object:
  modelarray <- ModelArray(h5_path, scalar_types = c(scalar))
  phenotypes <- read.csv(sprintf("%1$s/all_subjects/cohortfiles/dsistudio_scalars/%2$s/%2$s_%3$s_cohortfile_GMfiltered_noMW.csv", outputs_root, scalar, depth))
  
  # formula:
  smooth_var <- "age"
  covariates <- "sex + mean_fd"
  formula.gam <- as.formula(sprintf("%s ~ s(%s, k=3, fx=T) + %s", scalar, smooth_var, covariates))
   
  print(paste0("fitting GAMs for ", scalar))
  
  mygam.try <- ModelArray.gam(
    formula.gam,
    modelarray,
    phenotypes,
    scalar,
    changed.rsq.term.index = c(1)
  )
  
  saveRDS(mygam.try, sprintf("%1$s/GAMresults.%2$s_%3$s.age_GMfiltered_noMW.RData", outdir, scalar, depth))
  write.csv(mygam.try, sprintf("%1$s/GAMresults.%2$s_%3$s.age_GMfiltered_noMW.csv", outdir, scalar, depth))
}



################## 
# Fit GAMs!
################## 

#fitGAMs_ModelArray("dti_fa", outdir) # preliminary GAMs - didn't filter for GM probability
#fitGAMs_ModelArray("md", outdir)

fitGAMs_ModelArray_GMfiltered(scalar, outdir, depth)


