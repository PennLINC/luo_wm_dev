#!/bin/bash

# set variables
datasets=("HCPD" "HBN")
scalars=("RTOP")

r_script="/cbica/projects/luo_wm_dev/two_axes/code/significance_testing/NEST/deep_to_superficial/mapmri/NEST_wrapper_clipEnds_mapmri.R"
tract_list="/cbica/projects/luo_wm_dev/input/tract_list/tract_list.txt"

inputarray=()
while IFS= read -r line; do
    inputarray+=("$line")
done < "${tract_list}"

tract_count=${#inputarray[@]}

# submit job array for each dataset
for dataset in "${datasets[@]}"; do
    logs_dir="/cbica/projects/luo_wm_dev/two_axes/code/logs/NEST/${dataset}"
    mkdir -p ${logs_dir}

    for scalar in "${scalars[@]}"; do
        sbatch --job-name=NEST_${dataset}_mapmri \
            --nodes=1 \
            --ntasks=1 \
            --cpus-per-task=4 \
            --mem-per-cpu=2G \
            --time=48:00:00  \
            --array=0-$((tract_count-1)) \
            --output=${logs_dir}/NEST_${dataset}_mapmri_%A_%a.out \
            --error=${logs_dir}/NEST_${dataset}_mapmri_%A_%a.err \
            singularity_NEST_mapmri.sh ${dataset} ${tract_list} ${scalar}
    done
done


