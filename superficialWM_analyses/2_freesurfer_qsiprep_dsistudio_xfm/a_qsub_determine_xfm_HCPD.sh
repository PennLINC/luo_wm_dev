#!/bin/bash

########################################
# Set directories
########################################
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

	if [ -d ${outputs_root}/${sub} ]; then
		echo "Skipping ${sub} - already running or completed"
		continue
	fi

	########################################
	# Check for required directories
	########################################

	# Check for a qsiprep directory
	if [ ! -d ${data_root}/${dataset}_qsiprep/${sub} ]; then
		echo "Skipping ${sub} - no QSIPrep directory"
		continue
	fi

	# Check for a freesurfer directory
	if [ ! -d ${data_root}/${dataset}_freesurfer/${sub} ]; then
		echo "Skipping ${sub} - no Freesurfer directory"
		continue
	fi

	########################################
	# Create log directory
	########################################

	if [ ! -d ${outputs_root}/${sub}/logs ]; then
		mkdir -p ${outputs_root}/${sub}/logs
	fi
	outputs_dir_logs=${outputs_root}/${sub}/logs

	########################################
	# Submit job
	########################################
	qsub -l h_vmem=16G,s_vmem=16G \
		-j n \
		-o ${outputs_dir_logs} \
		-e ${outputs_dir_logs} \
		./a_determine_freesurfer-to-native_acpc_volume_xfm.sh -c ../config_HCPD.json ${sub}  
		
		

done < "$subjects_file"

 