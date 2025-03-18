#!/bin/bash

# set variables
datasets=("PNC" "HCPD" "HBN")
tract_list="/cbica/projects/luo_wm_dev/input/tract_list/tract_list.txt"

inputarray=()
while IFS= read -r line; do
    inputarray+=("$line")
done < "${tract_list}"

tract_count=${#inputarray[@]}

# submit job array for each dataset
for dataset in "${datasets[@]}"; do
    logs_dir="/cbica/projects/luo_wm_dev/two_axes_manuscript/code/logs/NEST/${dataset}"
    mkdir -p ${logs_dir}

    sbatch --job-name=NEST_${dataset}_end_compare \
           --nodes=1 \
           --ntasks=1 \
           --cpus-per-task=8 \
           --mem-per-cpu=2G \
           --time=48:00:00  \
           --array=0-$((tract_count-1)) \
           --output=${logs_dir}/NEST_${dataset}_end_compare_%A_%a.out \
           --error=${logs_dir}/NEST_${dataset}_end_compare_%A_%a.err \
           singularity_NEST_end_compare_clip5.sh ${dataset} ${tract_list} 
done

 
