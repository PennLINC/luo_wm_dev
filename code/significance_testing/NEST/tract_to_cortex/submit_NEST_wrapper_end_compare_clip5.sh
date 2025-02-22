#!/bin/bash

# set variables
datasets=("PNC" "HCPD" "HBN")

r_script="/cbica/projects/luo_wm_dev/two_axes_manuscript/code/significance_testing/NEST/tract_to_cortex/NEST_wrapper_end_compare_clip5.R"
#tract_list=("Inferior_Fronto-occipital" "Callosum_Motor")
 
#inputarray=("${tract_list[@]}")
#tract_count=${#inputarray[@]}

# submit job array for each dataset with elements in array being tracts
for dataset in "${datasets[@]}"; do
    logs_dir="/cbica/projects/luo_wm_dev/two_axes_manuscript/code/logs/NEST/${dataset}"
    mkdir -p ${logs_dir}

    sbatch --job-name=NEST_${dataset}_end_compare \
           --nodes=1 \
           --ntasks=1 \
           --cpus-per-task=8 \
           --mem-per-cpu=4G \
           --time=48:00:00  \
           --output=${logs_dir}/NEST_${dataset}_end_compare_%j_ifo.out \
           --error=${logs_dir}/NEST_${dataset}_end_compare_%j_ifo.err \
           singularity_NEST_end_compare_clip5.sh ${dataset} "Inferior_Fronto-occipital"
done


for dataset in "${datasets[@]}"; do
    logs_dir="/cbica/projects/luo_wm_dev/two_axes_manuscript/code/logs/NEST/${dataset}"
    mkdir -p ${logs_dir}

    sbatch --job-name=NEST_${dataset}_end_compare \
           --nodes=1 \
           --ntasks=1 \
           --cpus-per-task=8 \
           --mem-per-cpu=4G \
           --time=48:00:00  \
           --output=${logs_dir}/NEST_${dataset}_end_compare_%j_cmot.out \
           --error=${logs_dir}/NEST_${dataset}_end_compare_%j_cmot.err \
           singularity_NEST_end_compare_clip5.sh ${dataset} "Callosum_Motor"
done
