#!/bin/bash

########################################
# Set directories
########################################


PYTHON_INTERPRETER="/cbica/projects/luo_wm_dev/.conda/envs/luo_wm_dev/bin/python3.11"
source activate luo_wm_dev

# Check if conda environment is activated
conda env list

data_root=$(jq -r '.data_root' ../config_HCPD.json)
outputs_root=$(jq -r '.outputs_root' ../config_HCPD.json)
dataset=$(jq -r '.dataset' ../config_HCPD.json)

 
# Specify the path to the subjects file
subjects_file="${data_root}/subject_list/${dataset}_subject_list.txt"
 

# Loop through subjects in the file
while IFS= read -r sub; do
  echo "Processing ${sub}"
	########################################
	# Check if subject is running/completed
	########################################

	if [ -d ${outputs_root}/${sub}/surfaces/native_acpc ]; then
		echo "Skipping ${sub} - already running or completed"
		continue
	fi

	########################################
	# Check for required directories
	########################################

	# Check for a output/sub/surfaces/freesurfer directory
	if [ ! -d ${outputs_root}/${sub}/surfaces/freesurfer ]; then
		echo "Skipping ${sub} - no surfaces directory"
		continue
	fi
 

	########################################
	# Create log directory if not already made
	########################################

	if [ ! -d ${outputs_root}/${sub}/logs ]; then
		mkdir -p ${outputs_root}/${sub}/logs
	fi
	outputs_dir_logs=${outputs_root}/${sub}/logs

	########################################
	# Submit job
	########################################
	qsub -l h_vmem=16G,s_vmem=16G \
		-N b_qsub_align_surfaces_${sub} \
		-b y \
		-V \
		-j n \
		-o ${outputs_dir_logs} \
		-e ${outputs_dir_logs} \
		${PYTHON_INTERPRETER} b_align_surfaces_with_tract_volumes.py ../config_HCPD.json ${sub}  
		
		

done < "$subjects_file"
 