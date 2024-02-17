library(freesurfer)
library(freesurferformats)


# load fsaverage5 S-A axis
SAaxis <- read.csv("/cbica/projects/luo_wm_dev/SAaxis/SensorimotorAssociation_Axis.fsaverage5.csv", header=T)
lh.SAaxis <- SAaxis[1:10242,]
rh.SAaxis <- SAaxis[10243:20484,]

# load freesurfer fsaverage5 sphere
lh.freesurfer <- read.fs.surface("/cbica/projects/luo_wm_dev/input/HCPD/HCPD_freesurfer/fsaverage5/surf/lh.sphere")
rh.freesurfer <- read.fs.surface("/cbica/projects/luo_wm_dev/input/HCPD/HCPD_freesurfer/fsaverage5/surf/rh.sphere")


# included medial wall:
fsaverage5.coords <- rbind(lh.freesurfer$vertices, rh.freesurfer$vertices)
names(fsaverage5.coords) <- c("x", "y", "z")

write.csv(fsaverage5.coords, "/cbica/projects/luo_wm_dev/software/spin_test/rotate_parcellation/fsaverage5_fsLRmedialwall.Centroid.csv", row.names=F)


# we will exclude the medial wall based on the vertices with S-A axis value = 0
# note that fsaverage5 S-A axis was resampled from fsLR, so the medial wall isn't identical to what is labeled as such in freesurfer's fsaverage5 files
lh.SAaxis_medialwall <- which(lh.SAaxis==0)
rh.SAaxis_medialwall <- which(rh.SAaxis==0)

# exclude medial wall
lhcoords <- lh.freesurfer$vertices[-lh.SAaxis_medialwall,]
rhcoords <- rh.freesurfer$vertices[-rh.SAaxis_medialwall,]
 
fsaverage5.coords_noMW <- rbind(lhcoords,rhcoords)
names(fsaverage5.coords_noMW) <- c("x", "y", "z")

write.csv(fsaverage5.coords_noMW, "/cbica/projects/luo_wm_dev/software/spin_test/rotate_parcellation/fsaverage5_nofsLRmedialwall.Centroid.csv", row.names=F)
