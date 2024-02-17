# preliminary S-A axis correlation with age effect

gam.age.dti_fa <- readRDS("/cbica/projects/luo_wm_dev/output/HCPD/superficialWM/GAM/GAMresults.dti_fa.age.RData")



SAaxis <- read.csv("/cbica/projects/luo_wm_dev/SAaxis/SensorimotorAssociation_Axis.fsaverage5.csv", header=T)

medial_wall <- which(SAaxis==0)
SAaxis <- SAaxis$X0[-medial_wall]

gam.age.dti_fa <- gam.age.dti_fa[-medial_wall,]

cor.test(gam.age.dti_fa$s_age.partial.rsq, SAaxis, method=c("spearman")) # r = -0.1576228, pspin?


cor.test(gam.age.dti_fa$s_age.delta.adj.rsq, SAaxis, method=c("spearman")) # r = -0.1578304 , pspin?

 

gam.age.md <- readRDS("/cbica/projects/luo_wm_dev/output/HCPD/superficialWM/GAM/GAMresults.md.age.RData")



SAaxis <- read.csv("/cbica/projects/luo_wm_dev/SAaxis/SensorimotorAssociation_Axis.fsaverage5.csv", header=T)

medial_wall <- which(SAaxis==0)
SAaxis <- SAaxis$X0[-medial_wall]

gam.age.md <- gam.age.md[-medial_wall,]

cor.test(gam.age.md$s_age.partial.rsq, SAaxis, method=c("spearman")) # r = 0.1699813 , pspin?


cor.test(gam.age.md$s_age.delta.adj.rsq, SAaxis, method=c("spearman")) # r = 0.170142  , pspin?
