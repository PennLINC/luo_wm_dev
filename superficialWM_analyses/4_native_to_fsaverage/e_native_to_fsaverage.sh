#!/bin/bash

# convert native to fsaverage using freesurfer


##############################################
# Set config file (based on dataset to analyze)
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

# qsub your_script.sh -c config_dataset.json

########################################
# Read in subject ID
########################################
sub=${@:OPTIND}

########################################
# Set directories
########################################
dataset=$(jq -r '.dataset' "$config_file")
data_root=$(jq -r '.data_root' "$config_file")
outputs_root=$(jq -r '.outputs_root' "$config_file")
SUBJECTS_DIR="${data_root}/${dataset}_freesurfer"


indir_GM="${outputs_root}/${sub}/vol_to_surf/probseg_depths/native" 
outdir_fsaverage_GM="${outputs_root}/${sub}/vol_to_surf/probseg_depths/fsaverage" 
outdir_fsaverage5_GM="${outputs_root}/${sub}/vol_to_surf/probseg_depths/fsaverage5" 

indir_scalars="${outputs_root}/${sub}/vol_to_surf/dsistudio_scalars/native" 
outdir_fsaverage_scalars="${outputs_root}/${sub}/vol_to_surf/dsistudio_scalars/fsaverage"  
outdir_fsaverage5_scalars="${outputs_root}/${sub}/vol_to_surf/dsistudio_scalars/fsaverage5"  

# first copy /datalad_freesurfer/inputs/data/freesurfer/fsaverage into SUBJECTS_DIR
if [[ ! -d ${SUBJECTS_DIR}/fsaverage ]]; then
    cp -r ${data_root}/datalad_freesurfer/inputs/data/freesurfer/fsaverage ${SUBJECTS_DIR}
fi

# copy fsaverage5 from freesurfer (local) to cubic
# scp -r /Applications/freesurfer/7.4.1/subjects/fsaverage5 luoau@cubic-login.uphs.upenn.edu:/cbica/projects/luo_wm_dev/dropbox/
if [[ ! -d ${SUBJECTS_DIR}/fsaverage5 ]]; then
    cp -r /cbica/projects/luo_wm_dev/dropbox/fsaverage5 ${SUBJECTS_DIR}
fi

########################################
# Create output directories
########################################   
if [[ ! -d ${outdir_fsaverage_GM} ]]; then
    mkdir ${outdir_fsaverage_GM}
fi

if [[ ! -d ${outdir_fsaverage5_GM} ]]; then
    mkdir ${outdir_fsaverage5_GM}
fi

 
if [[ ! -d ${outdir_fsaverage_scalars} ]]; then
    mkdir ${outdir_fsaverage_scalars}
fi

if [[ ! -d ${outdir_fsaverage5_scalars} ]]; then
    mkdir ${outdir_fsaverage5_scalars}
fi

########################################
# Set variables for singularity
######################################## 
freesurfer_license="/cbica/projects/luo_wm_dev/software/freesurfer/license.txt"
freesurfer_sif="/cbica/projects/luo_wm_dev/software/freesurfer/fmriprep-20.2.3.sif"
 

########################################
# Set depths
######################################## 
depths=('depth_0' 'depth_0p5' 'depth_0p75' 'depth_1' 'depth_1p25' 'depth_1p5' 'depth_1p75' 'depth_2' 'depth_2p25' 'depth_2p5' 'depth_2p75' 'depth_3')


###############################################################################
# Convert GM probseg vol_to_surf output from native to fsaverage and fsaverage5
############################################################################### 

for hemi in lh rh; do
    for depth in "${depths[@]}"; do
        # Check for a native ${hemi}.GMprobseg_${depth}.shape.gii
        if [ ! -f ${indir_GM}/${hemi}.GMprobseg_${depth}.shape.gii ]; then
            echo "No ${hemi}.GMprobseg_${depth}.shape.gii image for ${sub}"
            continue
        fi

        singularity exec --writable-tmpfs \
        -B ${SUBJECTS_DIR}:/opt/freesurfer/subjects \
        -B ${indir_GM}:/mnt \
        -B ${freesurfer_license}:/opt/freesurfer/license.txt \
        $freesurfer_sif \
        mri_surf2surf --srcsubject $sub --hemi $hemi --srcsurfval /mnt/${hemi}.GMprobseg_${depth}.shape.gii \
        --trgsubject fsaverage --trgsurfval ${outdir_fsaverage_GM}/${hemi}.GMprobseg_${depth}_fsaverage.shape.gii 
        #  Reading source surface reg /Applications/freesurfer/7.4.1/subjects/sub-2987185/surf/lh.sphere.reg
    done
done
 

for hemi in lh rh; do
    for depth in "${depths[@]}"; do
        # Check for a native ${hemi}.GMprobseg_${depth}.shape.gii
        if [ ! -f ${indir_GM}/${hemi}.GMprobseg_${depth}.shape.gii ]; then
            echo "No ${hemi}.GMprobseg_${depth}.shape.gii image for ${sub}"
            continue
        fi

        singularity exec --writable-tmpfs \
        -B ${SUBJECTS_DIR}:/opt/freesurfer/subjects \
        -B ${indir_GM}:/mnt \
        -B ${freesurfer_license}:/opt/freesurfer/license.txt \
        $freesurfer_sif \
        mri_surf2surf --srcsubject $sub --hemi $hemi --srcsurfval /mnt/${hemi}.GMprobseg_${depth}.shape.gii \
        --trgsubject fsaverage5 --trgsurfval ${outdir_fsaverage5_GM}/${hemi}.GMprobseg_${depth}_fsaverage5.shape.gii 
        
    done
done
 
  
######################################################################################
# Convert DSIStudio scalars vol_to_surf output from native to fsaverage and fsaverage5
######################################################################################
 
scalars=($(find "${indir_scalars}" -maxdepth 1 -mindepth 1 -type d -exec basename {} \; | sort))
echo "${scalars[@]}"

for scalar in "${scalars[@]}"; do
    for hemi in lh rh; do
        for depth in "${depths[@]}"; do
            # Check for a native ${hemi}.GMprobseg_${depth}.shape.gii
            if [ ! -f ${indir_scalars}/${scalar}/${sub}_${scalar}_${hemi}_${depth}.shape.gii ]; then
                echo "No ${sub}_${scalar}_${hemi}_${depth}.shape.gii for ${sub}"
                continue
            fi
            
            # make scalar output dir
            if [[ ! -d ${outdir_fsaverage_scalars}/${scalar} ]]; then
                mkdir ${outdir_fsaverage_scalars}/${scalar}
            fi

            singularity exec --writable-tmpfs \
            -B ${SUBJECTS_DIR}:/opt/freesurfer/subjects \
            -B ${indir_scalars}/${scalar}:/mnt \
            -B ${freesurfer_license}:/opt/freesurfer/license.txt \
            $freesurfer_sif \
            mri_surf2surf --srcsubject $sub --hemi $hemi --srcsurfval /mnt/${sub}_${scalar}_${hemi}_${depth}.shape.gii \
            --trgsubject fsaverage --trgsurfval ${outdir_fsaverage_scalars}/${scalar}/${sub}_${scalar}_${hemi}_${depth}_fsaverage.shape.gii
        done
    done
done
 
 

for scalar in "${scalars[@]}"; do
    for hemi in lh rh; do
        for depth in "${depths[@]}"; do
            # Check for a native ${hemi}.GMprobseg_${depth}.shape.gii
            if [ ! -f ${indir_scalars}/${scalar}/${sub}_${scalar}_${hemi}_${depth}.shape.gii ]; then
                echo "No ${sub}_${scalar}_${hemi}_${depth}.shape.gii for ${sub}"
                continue
            fi
             
            # make scalar output dir
            if [[ ! -d ${outdir_fsaverage5_scalars}/${scalar} ]]; then
                mkdir ${outdir_fsaverage5_scalars}/${scalar}
            fi

            singularity exec --writable-tmpfs \
            -B ${SUBJECTS_DIR}:/opt/freesurfer/subjects \
            -B ${indir_scalars}/${scalar}:/mnt \
            -B ${freesurfer_license}:/opt/freesurfer/license.txt \
            $freesurfer_sif \
            mri_surf2surf --srcsubject $sub --hemi $hemi --srcsurfval /mnt/${sub}_${scalar}_${hemi}_${depth}.shape.gii \
            --trgsubject fsaverage5 --trgsurfval ${outdir_fsaverage5_scalars}/${scalar}/${sub}_${scalar}_${hemi}_${depth}_fsaverage5.shape.gii
        done
    done
done
 
 