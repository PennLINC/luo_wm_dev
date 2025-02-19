#!/bin/bash

datasets=("PNC" "HCPD" "HBN")

# set variables
r_script="/cbica/projects/luo_wm_dev/two_axes_manuscript/code/significance_testing/spin_tests/main_figures_spintests.R"
r_script_avg="/cbica/projects/luo_wm_dev/two_axes_manuscript/code/significance_testing/spin_tests/main_figures_spintests_avgdatasets.R"

for dataset in "${datasets[@]}"; do
    logs_dir="/cbica/projects/luo_wm_dev/two_axes_manuscript/code/logs/spin_tests/${dataset}"
    if [ ! -d "${logs_dir}" ]; then
        mkdir -p ${logs_dir}
    fi
    job_name="main_figures_spintests_${dataset}"
    sbatch --job-name=${job_name} \
           --nodes=1 \
           --ntasks=1 \
           --cpus-per-task=1 \
           --mem-per-cpu=2G \
           --time=24:00:00  \
           --propagate=NONE \
           --output=${logs_dir}/${job_name}_%j.out \
           --error=${logs_dir}/${job_name}_%j.err \
           --wrap="singularity run --cleanenv /cbica/projects/luo_wm_dev/software/docker/r-packages-for-cubic_0.0.7.sif Rscript --save ${r_script} ${dataset}"
done


logs_avgdatasets_dir="/cbica/projects/luo_wm_dev/two_axes_manuscript/code/logs/spin_tests/avgdatasets"
if [ ! -d "${logs_avgdatasets_dir}" ]; then
    mkdir -p ${logs_avgdatasets_dir}
fi
job_name="main_figures_spintests_avgdatasets"
sbatch --job-name=main_figures_spintests_avgdatasets \
           --nodes=1 \
           --ntasks=1 \
           --cpus-per-task=1 \
           --mem-per-cpu=2G \
           --time=24:00:00  \
           --propagate=NONE \
           --output=${logs_avgdatasets_dir}/${job_name}_%j.out \
           --error=${logs_avgdatasets_dir}/${job_name}_%j.err \
           --wrap="singularity run --cleanenv /cbica/projects/luo_wm_dev/software/docker/r-packages-for-cubic_0.0.7.sif Rscript --save ${r_script_avg}"