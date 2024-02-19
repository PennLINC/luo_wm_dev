
########################################
# Set directories
########################################

PYTHON_INTERPRETER="/cbica/projects/luo_wm_dev/.conda/envs/luo_wm_dev/bin/python3.8" # note that i changed python from 3.11 to 3.8 in environment!!
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
	# Check for required directories
	########################################

	# Check for fsaverage5 directory
	if [ ! -d ${outputs_root}/${sub}/vol_to_surf/probseg_depths/fsaverage5 ] || [ ! -d ${outputs_root}/${sub}/vol_to_surf/dsistudio_scalars/fsaverage5 ]; then
		echo "Skipping ${sub} - no fsaverage5 directory"
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
		-N i_filter_scalars_by_GMprobseg_${sub} \
		-b y \
		-V \
		-j n \
		-o ${outputs_dir_logs} \
		-e ${outputs_dir_logs} \
		${PYTHON_INTERPRETER} i_filter_scalars_by_GMprobseg.py ../config_HCPD.json ${sub}  
		

done < "$subjects_file"
 