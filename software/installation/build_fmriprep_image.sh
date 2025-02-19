#build a singularity image for fmriprep
#docker site: https://hub.docker.com/r/nipreps/fmriprep/tags
#tag: https://hub.docker.com/layers/nipreps/fmriprep/20.2.3/images/sha256-102db5fe8b0a34298f2eb2fd5962ad99ff0a948d258cbf08736fcc1b845cad9f


cd /cbica/projects/luo_wm_dev/two_axes_manuscript/software/
mkdir -p freesurfer
singularity build freesurfer/fmriprep-20.2.3.sif docker://nipreps/fmriprep:20.2.3
 