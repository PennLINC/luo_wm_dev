#!/bin/bash


data_root=$(jq -r '.data_root' ../config_HCPD.json)
outputs_root=$(jq -r '.outputs_root' ../config_HCPD.json)
dataset=$(jq -r '.dataset' ../config_HCPD.json)
scalars=("ad" "dti_fa" "gfa" "ha" "iso" "md" "qa" "rd" "rd1" "rd2" "rdi")
depths=("depth_1p25" "depth_3")

# Loop through scalars
for scalar in "${scalars[@]}"; do
 
  echo "Processing ${scalar}"
	########################################
	# Check if scalar is running/completed
	########################################

	#if [ -d ${outputs_root}/GAM/${scalar} ] ; then
	#	echo "Skipping ${scalar} - already running or completed"
	#	continue
	#fi

	########################################
	# Check for required directories
	########################################

	# Check for GAM directory
	if [ ! -d ${outputs_root}/GAM/ ]; then
		echo "Skipping ${scalar} - no GAM directory"
		continue
	fi

	########################################
	# Create log directory if not already made
	########################################

	if [ ! -d ${outputs_root}/GAM/logs ]; then
		mkdir -p ${outputs_root}/GAM/logs
	fi
	outputs_dir_logs=${outputs_root}/GAM/logs

	########################################
	# Submit job
	########################################
	for depth in "${depths[@]}"; do
		qsub -l h_vmem=64G,s_vmem=64G \
			-N l_qsub_ModelArray_${scalar} \
			-b y \
			-V \
			-j n \
			-o ${outputs_dir_logs} \
			-e ${outputs_dir_logs} \
			./l_singularity_ModelArray.sh ../config_HCPD.csv ${scalar} ${depth}
	done

done  
 