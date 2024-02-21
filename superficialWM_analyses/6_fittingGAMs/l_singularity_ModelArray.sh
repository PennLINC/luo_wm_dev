#!/bin/bash

singularity run --cleanenv \
  /cbica/projects/luo_wm_dev/software/ModelArray/modelarray_confixel_latest.sif \
   Rscript --save /cbica/projects/luo_wm_dev/code/superficialWM_analyses/6_fittingGAMs/l_ModelArray_fitGAMs.R $1 $2 $3
