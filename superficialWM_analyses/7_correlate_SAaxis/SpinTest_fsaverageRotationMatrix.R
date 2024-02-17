
library(stringr, lib.loc="/cbica/home/luoau/Rlibs")
library(dplyr, lib.loc="/cbica/home/luoau/Rlibs")
library(magrittr, lib.loc="/cbica/home/luoau/Rlibs")


#This Rmd file using rotate_parcellation to generate 10k rotated permutations of a parcellation, given the coordinates of left and right hemispheres on the sphere.  
#The output is a vector of 1:Number_of_parcels for each permutation which tries to conserve the relative position of each parcel

#- All Centroid_RAS.csv's for Schaefer atlases were downloaded from: https://github.com/ThomasYeoLab/CBIG/tree/master/stable_projects/brain_parcellation/Schaefer2018_LocalGlobal/Parcellations/MNI/Centroid_coordinates 
 
# rotate.parcellation.R and perm.sphere.p.R are downloaded from https://github.com/frantisekvasa/rotate_parcellation
source("/cbica/projects/luo_wm_dev/software/spin_test/rotate.parcellation.R")
source("/cbica/projects/luo_wm_dev/software/spin_test/perm.sphere.p.R")
 
 
schaefer100.coords <- read.csv("/cbica/projects/luo_wm_dev/software/spin_test/rotate_parcellation/Schaefer2018_100Parcels_17Networks_order_FSLMNI152_1mm.Centroid_RAS.csv") #coordinates of schaefer100.coords parcel centroids on the freesurfer sphere

perm.id.full <- rotate.parcellation(coord.l = as.matrix(schaefer100.coords[1:50,3:5]), coord.r = as.matrix(schaefer100.coords[51:100,3:5]), nrot = 10000) #rotate the schaefer100.coords parcellation 10,000 times on the freesurfer sphere to generate spatial nulls for spin-based permutation significance testing 
 
saveRDS(perm.id.full, "/cbica/projects/luo_wm_dev/software/spin_test/rotate_parcellation/schaefer100x7.coords_sphericalrotations_N10k.rds")
 