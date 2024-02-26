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
# qsub -l h_vmem=60G,s_vmem=60G -o ~/jobs -e ~/jobs datalad_freesurfer.sh -c ../config_HCPD.json 
 
########################################
# Set directories
########################################
dataset=$(jq -r '.dataset' "$config_file")

data_root=$(jq -r '.data_root' "$config_file")
 

freesurfer_datalad_dir="${data_root}/datalad_freesurfer"
freesurfer_dir="${data_root}/${dataset}_freesurfer"

# Create directories for freesurfer files
if [ ! -d ${freesurfer_dir} ]; then
    mkdir -p ${freesurfer_dir} 
fi

########################################
# Set ssh key 
########################################
# to avoid putting in password for each datalad get
eval $(ssh-agent)
ssh-add ~/.ssh/id_rsa
  
 
########################################
# freesurfer
########################################
 
datalad clone ria+ssh://audluo@bblsub.pmacs.upenn.edu:/static/LINC_HCPD#~fstabulate datalad_freesurfer

cd ${freesurfer_datalad_dir}/inputs/data

datalad get -n .

for file in sub-*.zip; do
        
	id=${file%_*}
	echo ${id}
    echo ${file}

	

    if ! [ -d ${freesurfer_dir}/$id ]; then
        datalad get $file 
        mkdir -p ${freesurfer_dir}/$id/mri ###make dir to put extracted files
        mkdir -p ${freesurfer_dir}/$id/surf

        # Only uncompress necessary files
        unzip -j "$file" "freesurfer/${id}/mri/nu.mgz" -d ${freesurfer_dir}/$id/mri
        unzip -j "$file" "freesurfer/${id}/surf/lh.pial" -d ${freesurfer_dir}/$id/surf
        unzip -j "$file" "freesurfer/${id}/surf/rh.pial" -d ${freesurfer_dir}/$id/surf
        unzip -j "$file" "freesurfer/${id}/surf/lh.white" -d ${freesurfer_dir}/$id/surf
        unzip -j "$file" "freesurfer/${id}/surf/rh.white" -d ${freesurfer_dir}/$id/surf
        unzip -j "$file" "freesurfer/${id}/surf/lh.sphere.reg" -d ${freesurfer_dir}/$id/surf
        unzip -j "$file" "freesurfer/${id}/surf/rh.sphere.reg" -d ${freesurfer_dir}/$id/surf
        unzip -j "$file" "freesurfer/${id}/surf/lh.midthickness" -d ${freesurfer_dir}/$id/surf
        unzip -j "$file" "freesurfer/${id}/surf/rh.midthickness" -d ${freesurfer_dir}/$id/surf
        unzip -j "$file" "freesurfer/${id}/surf/lh.inflated" -d ${freesurfer_dir}/$id/surf
        unzip -j "$file" "freesurfer/${id}/surf/rh.inflated" -d ${freesurfer_dir}/$id/surf
    fi

	# Drop everything else
	datalad drop --nocheck $file 

done