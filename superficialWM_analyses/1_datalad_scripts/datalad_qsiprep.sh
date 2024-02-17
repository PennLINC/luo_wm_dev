#!/bin/bash

##############################################
# Set config file (based on dataset to datalad get)
##############################################

# Parse command-line arguments
while getopts ":c:" opt; do
  case $opt in
    c) config_file="$OPTARG"
    ;;
    \?) echo "Invalid option: -$OPTARG" >&2
    ;;
  esac
done

# Check if the config_file variable is set
if [ -z "$config_file" ]; then
  echo "Error: Please specify a config file using the -c option." >&2
  exit 1
fi
 
# example:  
# qsub -l h_vmem=60G,s_vmem=60G -o ~/jobs -e ~/jobs datalad_qsiprep.sh -c ../config_HCPD.json 
 
########################################
# Set directories
########################################
dataset=$(jq -r '.dataset' "$config_file")

data_root=$(jq -r '.data_root' "$config_file")

qsiprep_datalad_dir="${data_root}/datalad_qsiprep"
qsiprep_dir="${data_root}/${dataset}_qsiprep"
 

# Create directories for qsiprep and freesurfer files
if [ ! -d ${qsiprep_dir} ]; then
    mkdir -p ${qsiprep_dir} 
fi
 

########################################
# Set ssh key 
########################################
# to avoid putting in password for each datalad get
eval $(ssh-agent)
ssh-add ~/.ssh/id_rsa
  

########################################
# qsiprep
########################################

# note that datalad clone was already done in earlier analyses for HCPD...
cd ${qsiprep_datalad_dir}


for file in *.zip; do
	
	id=${file%_*}
	echo ${id}
	echo ${file}
	

    if ! [ -d ${qsiprep_dir}/$id ]; then
        datalad get $file
        mkdir ${qsiprep_dir}/$id  ###make dir to put extracted files
        
        # Only uncompress necessary files
        unzip -j "$file" "qsiprep/${id}/ses-V1/dwi/*dwiref.nii.gz" -d ${qsiprep_dir}/$id
        unzip -j "$file" "qsiprep/${id}/anat/${id}_desc-preproc_T1w.nii.gz" -d ${qsiprep_dir}/$id
        unzip -j "$file" "qsiprep/${id}/anat/${id}_label-GM_probseg.nii.gz" -d ${qsiprep_dir}/$id
        unzip -j "$file" "qsiprep/${id}/anat/${id}_label-WM_probseg.nii.gz" -d ${qsiprep_dir}/$id

    fi 

	
	# Drop everything else
	datalad drop --nocheck $file

done


 