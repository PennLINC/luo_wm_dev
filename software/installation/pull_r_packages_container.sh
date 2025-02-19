


#pull docker image with necessary R packages
#docker site: https://hub.docker.com/r/audreycluo/r-packages-for-cubic/tags 
#tag: https://hub.docker.com/layers/audreycluo/r-packages-for-cubic/0.0.7/images/sha256-7064f5c09ef83f442986d51061e963951f9499cdfe92717a883e6cc4010516f3 

# pull the docker image onto cubic cluster:
cd /cbica/projects/luo_wm_dev/two_axes_manuscript/software
mkdir -p r_packages
singularity pull r_packages/r-packages-for-cubic_0.0.7.sif docker://audreycluo/r-packages-for-cubic:0.0.7