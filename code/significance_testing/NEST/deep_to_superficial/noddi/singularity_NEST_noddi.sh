#!/bin/bash

dataset=$1
tract_list=$2
scalar=$3

inputarray=()
while IFS= read -r line; do
    inputarray+=("$line")
done < "${tract_list}"

# Get the tract for this SLURM array task
tract_name="${inputarray[$SLURM_ARRAY_TASK_ID]}"


# Run the R script for the specific dataset and tract
singularity run --cleanenv /cbica/projects/luo_wm_dev/two_axes/software/r_packages/r-packages-for-cubic_0.0.7.sif Rscript --no-save /cbica/projects/luo_wm_dev/two_axes/code/significance_testing/NEST/deep_to_superficial/noddi/NEST_wrapper_clipEnds_noddi.R ${dataset} ${tract_name} ${scalar}
