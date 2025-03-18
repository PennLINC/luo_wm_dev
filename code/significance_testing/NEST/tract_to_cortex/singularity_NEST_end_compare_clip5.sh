#!/bin/bash

dataset=$1
tract_list=$2

inputarray=()
while IFS= read -r line; do
    inputarray+=("$line")
done < "${tract_list}"

# Get the tract for this SLURM array task
tract_name="${inputarray[$SLURM_ARRAY_TASK_ID]}"

# Run the R script for the specific dataset and tract
singularity run --cleanenv /cbica/projects/luo_wm_dev/two_axes_manuscript/software/r_packages/r-packages-for-cubic_0.0.7.sif Rscript --save /cbica/projects/luo_wm_dev/two_axes_manuscript/code/significance_testing/NEST/tract_to_cortex/NEST_wrapper_end_compare_clip5.R ${dataset} ${tract_name}
