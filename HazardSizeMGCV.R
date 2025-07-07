library(mgcv)
library(ordPens)
library(Metrics)
#####################################################
#########read data and get target variable###########
#####################################################
data.path = "C:/Users/DELL/OneDrive/data_Luding/Codes/"

## load data from file: 
data.raw = read.table(paste0(data.path, "LuDingData.csv"),sep=",", header=TRUE) ## read data matrix
data.size = data.raw

data4fit = subset(data.size, Area_SUM > 0)
data4fit$AreaLog = log(data4fit$Area_SUM)
#########################################################################
colnames(data4fit)
plot(density(data4fit$Area_SUM))
plot(density(data.raw$newsize))
plot(density(log((data4fit$Area_SUM+1))))

# plot(density(data4fit$ExpandedSlide),  family = "sans",  = 20)
# plot(density(data4fit$Area_SUM))
# plot(density(log(data4fit$ExpandedSlide)))
# hist(data4fit$ExpandedSlide)
# hist(log(data4fit$ExpandedSlide))

# data4fit$Lithology[data4fit$Lithology == 27] = 19 
# data4pred = subset(data.size, ExpandedSlide == 1000)
# data4predhist(log(data4fit$ExpandedSlide))


num.baseFun = 8
Formula.size = log(Area_SUM) ~ 
  s(Epicent_m, k = num.baseFun) +
  s(Slope_m, k = num.baseFun) +
  s(Road_m, k = num.baseFun) +
  s(Fault_m, k = num.baseFun)+
  s(Shape_Area, k = num.baseFun) +
  s(PAG_3M_m, k = num.baseFun)+
  s(Aspect_m, k = num.baseFun) +
  # s(PRC_m, k = num.baseFun) +
  # s(travel_ang, k = num.baseFun)+
  s(Lon, Lat, bs = "gp", k=200, m=2)
  # as.factor(Lith_magor)

Fit.sizeSpa = mgcv::gam(Formula.size, family = "gaussian", data = data4fit)
# Fit.size = mgcv::gam(Formula.size, family = Gamma(link = "log"), data = data4fit)
cor(Fit.sizeSpa$fitted.values, data4fit$Area_SUM, method = "spearman")

predict.size = predict(Fit.sizeSpa, data.raw, type = 'response',se.fit = TRUE)
result.size = data.frame(ID = data.raw$OBJECTID, Truearea = log(data.raw$Area_SUM),
                         predictSize = predict.size$fit, uncerSize = 3.92*predict.size$se.fit)
# save and load the model
save(Fit.sizeSpa,file=paste0(data.path, "ModelFitSizeSpa.RData"))
# save(Fit.size,file=paste0(data.path, "ModelFitSize.RData"))
# load("ModelFitSize.RData")
summary(Fit.size)

cor(Fit.sizeSpa$fitted.values, data4fit$Area_SUM, method = "spearman")


# Fit.size = mgcv::gam(Formula.size, family = "gaussian", data = data4fit, select = TRUE)  
# Fit.size = mgcv::gam(Formula.size, family = "gaussian", data = data4fit, method = "REML")
# windows()
mod.check = gam.check(Fit.size, type = c('deviance'))
reds = residuals.gam(Fit.size)
plot(density(reds))
hist(reds)

# lines(c(0, 15),c(0, 15), lwd = 2, lty = 2, col = "red")
mae(Fit.sizeSpa$fitted.values, log(data4fit$Area_SUM))
rmse(Fit.sizeSpa$fitted.values, log(data4fit$Area_SUM))
cor(Fit.sizeSpa$fitted.values, log(data4fit$Area_SUM), method = "spearman")

smoothScatter(log(data4fit$Area_SUM), Fit.size$fitted.values,
              xlab = expression("Observed"~"["*"log"*"("*"m"^"2"*")"*"]"),
              ylab = expression("Fitted"~"["*"log"*"("*"m"^"2"*")"*"]"),
              xlim = c(0, 14.5), ylim = c(0, 14.5),
              family = "sans",cex.axis = 1.6, cex.lab=1.6,
              postPlotHook = fudgeit)


## polt the effects
windows()
plot(Fit.size, shade = TRUE, shade.col = "grey", ylim = c(-2.,2)) 
abline(h=0.3, col = "gray40", lwd = 2, lty=2)
## plot categorical covariates
plot.gam(Fit.size, select = 14, all.terms = TRUE)
a = plot.gam(Fit.size, select = 14, all.terms = TRUE)


#####################################################
######fitting performance scatter plo--smoothScattert###############
#####################################################
library(ggplot2)
load(paste0(data.path, "ModelFitSizeSpa.RData"))
load(paste0(data.path, "ModelFitSize.RData"))

data.fitplotSpa = data.frame(obs = log(data4fit$Area_SUM), fit = Fit.sizeSpa$fitted.values)
data.fitplot = data.frame(obs = log(data4fit$Area_SUM), fit = Fit.size$fitted.values)
data.lineplot = data.frame(linx = c(0,15), liny = c(0,15))
dev.off()
windows()


plot.sizeSpa = ggplot(data.fitplotSpa, aes(x = obs, y = fit)) + xlim(4, 15) + ylim(4, 15)+
  geom_point(size = 1) +  # 绘制散点
  labs(x = "Observed"~"["*"ln"*"("*"m"^"2"*")"*"]", y = "Predicted"~"["*"ln"*"("*"m"^"2"*")"*"]") +
  theme(panel.grid = element_blank(), # 让图中间没有灰色格网,
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        axis.title = element_text(size = 24,  color = "black"),
        axis.ticks = element_line(color = "black", size=0.5),
        axis.text = element_text(size = 20, family = "sans", color="black"),
        plot.title = element_text(size = 24, color = "black", family = "sans"))+ 
  geom_abline(intercept = 0, slope = 1, colour="red", linetype="dashed",size=1)+
  annotation_custom(
    grob = textGrob("MAE = 0.947\nRMSE = 1.2\nPCC = 0.72",
                    gp = gpar(fontsize = 20, col = "black"),
                    just = "left"),
    xmin = 8.5, xmax = 13, ymin = 3, ymax = 7
  )

plot.size = ggplot(data.fitplot, aes(x = obs, y = fit)) + xlim(4, 15) + ylim(4, 15)+
  geom_point(size = 1) +  # 绘制散点
  labs(x = "Observed"~"["*"ln"*"("*"m"^"2"*")"*"]", y = "Predicted"~"["*"ln"*"("*"m"^"2"*")"*"]") +
  theme(panel.grid = element_blank(), # 让图中间没有灰色格网,
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        axis.title = element_text(size = 24,  color = "black"),
        axis.ticks = element_line(color = "black", size=0.5),
        axis.text = element_text(size = 20, family = "sans", color="black"),
        plot.title = element_text(size = 24, color = "black", family = "sans"))+ 
  geom_abline(intercept = 0, slope = 1, colour="red", linetype="dashed",size=1)+
  annotation_custom(
    grob = textGrob("MAE = 1.007\nRMSE = 1.272\nPCC = 0.681",
                    gp = gpar(fontsize = 20, col = "black"),
                    just = "left"),
    xmin = 8.5, xmax = 13, ymin = 3, ymax = 7)

main.plot = gridExtra::grid.arrange(
  plot.sizeSpa, plot.size,   # 两个图
  ncol = 2, nrow = 1      
)
ggsave(
  filename = "FittingSize.pdf",  # PDF 文件名
  plot = main.plot,            # 要保存的图形对象
  path = "C:/Users/DELL/OneDrive/data_Luding/Results/",  # 文件保存路径（可根据需要修改）
  width = 12,                   # PDF 宽度（单位：英寸）
  height = 6,                  # PDF 高度（单位：英寸）
  units = "in"                 # 单位（英寸）
)
#######################################################################################
####################################################################################


#####################################################
#####fitting performance QQ plot and residuals#######
#####################################################
## qqplot
library(mgcViz)
fit.vis = getViz(Fit.size)

dev.off()
windows()
par(mar=c(4.5,4.5,2,2.3))

qq(fit.vis, method = "simul2", discrete = TRUE)

pdf(file = "SizeFittingQQplot.pdf", width = 7, height=7)
par(mar=c(4.5,4.5,2,2.3))
a = qq.gam(Fit.size, level = 10, pch = 16, cex=.4, family = "sans",s.rep = 5,
       cex.lab=2, xlim=c(-3.6, 3.6),xaxt = 'n', yaxt = 'n')
box(lwd = 1.5)
axis(side = 1, lwd.ticks = 1.5, cex.axis = 1.6)
axis(side = 2, lwd.ticks = 1.5, cex.axis = 1.6)
abline(0, 1, col = 'black', lwd = 1.5)
dev.off()

## rediduals
dev.off()
windows()

pdf(file = "SizeFittingResiduals.pdf", width = 7, height=7)
par(mar=c(4.5,4.5,2,2.3))
reds = residuals.gam(Fit.size)
par(lwd = 1.5)
a = hist(reds, breaks = 20, xlab = "Residuals",ylab = "Frequency",
     family = "sans", cex.lab=2,,xaxt = 'n', yaxt = 'n',
     main = "")
box(lwd = 1.5)
axis(side = 1, lwd.ticks = 1.5, cex.axis = 1.6)
axis(side = 2, at=c(0,5000,15000,25000),labels = c(0,5000,15000,25000),
     lwd.ticks = 1.5, cex.axis = 1.6)
dev.off()
#####################################################################


########################################
##
##########################################
library(ggplot2)
library(ensembleBMA)
# 提取残差
residuals_model <- residuals(Fit.size, type = "response")
# Extract fitted values and residuals
fitted_values <- predict(Fit.size, type = "response")

deviance_residuals <- residuals(Fit.size, type = "deviance")

# Create a plot of deviance residuals against linear predictor
plot(fitted_values, deviance_residuals, main = "Deviance Residuals vs. Linear Predictor", 
     xlab = "Linear Predictor", ylab = "Deviance Residuals")

# Add a horizontal line at y=0 for reference
abline(h = 0, col = "red", lty = 2)


############################################



a = data.frame(x = effect.cor$x, y=effect.cor$fit)
#####################################################
###plot Coefficients of  nonlinear covariates########
#####################################################
figure.info = plot.gam(Fit.size, pages = 1) # extract the effect information from the plot.gam
dev.off()
windows()
par(mar=c(8,6,2,2.3))


effect.cor = figure.info[[2]] # denote which covariate extract: plot the nonlinear effect
pdf(file = "size time.pdf", width = 7, height=7)
par(mar=c(4.7,4.7,2,2.3))
## plot the mean coefficients value, x is the x value, fit is the y value, se is the confidence interval
## in fact, the se is the 2 standard error value
plot(effect.cor$x, effect.cor$fit, type = "l", col = "#0000FF", lwd = 3, ylim = c(-4.,2.5),
     xlab = "Time period", ylab = "Regression coefficients",
     family = "sans", cex.lab=2, xaxt = 'n', yaxt = 'n')
box(lwd = 1.5)
axis(side = 1, lwd.ticks = 1.5, cex.axis = 1.6)
# axis(side = 1, at = c(-0.35, -0.15,0,0.15,0.35), labels= c(-0.35, -0.15,0,0.15,0.35),
#      lwd.ticks = 1.5, cex.axis = 1.6)
axis(side = 2, at = c(-4, -3,-2,-1,0,1,2), labels=c(-4, -3,-2,-1,0,1,2),
     lwd.ticks = 1.5, cex.axis = 1.6)

polygon(c(rev(effect.cor$x), effect.cor$x), 
        c(rev(effect.cor$fit - effect.cor$se), effect.cor$fit + effect.cor$se),
        col = 'gray80', border = NA)
## the polygon will cover the line, so we plot the line again
lines(effect.cor$x, effect.cor$fit, type = "l", col = "#0000FF", lwd = 3)
abline(h=0, col = "gray40", lwd = 2, lty=2)
dev.off()
#############################################################################



#####################################################
###plot Coefficients of  categorical covariates######
#####################################################
## plot the effect of lithology
dev.off()
windows()
par(mar=c(4.5,4.5,2,2.3))
summ.mod = summary(Fit.size)
coeff.mod = summ.mod$p.coeff  #get all the coefficient of linear/categorical covariates
error.mod = summ.mod$se  #get all the standard error of all covariates
lith.names = c("as.factor(Lithology)2","as.factor(Lithology)3","as.factor(Lithology)4","as.factor(Lithology)5",
               "as.factor(Lithology)6","as.factor(Lithology)7","as.factor(Lithology)8","as.factor(Lithology)9",
               "as.factor(Lithology)10","as.factor(Lithology)11","as.factor(Lithology)12","as.factor(Lithology)13",
               "as.factor(Lithology)14","as.factor(Lithology)15")
lith.coeff = coeff.mod[which(names(coeff.mod) %in% lith.names)]
lith.error = error.mod[which(names(error.mod) %in% lith.names)]
## plot 
library(plotrix)
par(mar=c(4.5,4.5,2,2.3))
plotCI(x=1:15,y=c(0, lith.coeff),
       li=c(0, lith.coeff - 2*lith.error),ui=c(0, lith.coeff + 2*lith.error),
       ylim=c(-2,2.5), xlab = "Lithology", ylab = "Regression coefficient",
       family = "sans",cex.axis = 1.7, cex.lab=2,xaxt="n", pch=19, col="red", scol="black",
       lwd=2)
abline(h=0, col = "gray40", lwd = 2, lty=2)
axis(side = 1, at=1:15,labels=1:15, cex.axis = 1.7, family = "sans")
abline(h=0, col = "gray40", lwd = 2, lty=2)
text(1,0.15,"A",cex=1.4,family = "sans")
text(2,0.38,"B",cex=1.4,family = "sans")
text(3,0.32,"C",cex=1.4,family = "sans")
text(4,0.35,"D",cex=1.4,family = "sans")
text(5,0.4,"E",cex=1.4,family = "sans")
text(6,0.88,"F",cex=1.4,family = "sans")
text(7,0.34,"G",cex=1.4,family = "sans")
text(8,0.35,"H",cex=1.4,family = "sans")
text(9,0.26,"I",cex=1.4,family = "sans")
text(10,0.52,"J",cex=1.4,family = "sans")
text(11,0.4,"K",cex=1.4,family = "sans")
text(12,0.43,"L",cex=1.4,family = "sans")
text(13,0.35,"M",cex=1.4,family = "sans")
text(14,0.65,"N",cex=1.4,family = "sans")
text(15,0.36,"O",cex=1.4,family = "sans")
##########################################################################################


#####################################################
###plot Coefficients of  coordX and coordY##########
#####################################################
vis = getViz(Fit.size)
library(mgcViz)

dev.off()
windows()
par(mar=c(4.5,4.5,2,2.3))

## Plot 2D effect with noised-up raster, contour and rug for design points 
## Opacity is proportional to the significance of the effect
# plot(sm(vis, 12)) + l_fitRaster(noiseup = TRUE) + l_fitContour() 
plot(sm(vis, 11)) + l_fitRaster(noiseup = F) + l_fitContour() + 
  labs(title= "",x="Longitude", y="Latitude", fill = "sdf")+
  theme(axis.text=element_text(size=16, family = "sans", color = "black"), 
        axis.title=element_text(size=20, family = "sans"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_rect( colour = "black",size = 0.8),
        legend.position = c(0.89,0.2),
        legend.text = element_text(size=12, family = "sans", color = "black"),
        # legend.title = element_blank()
        legend.title = element_text(size=16, family = "sans", color = "black"))
#############################################################################



#####################################################
########plot Coefficients of cateforical year########
#####################################################
year.names = c("as.factor(Year)2","as.factor(Year)3","as.factor(Year)4","as.factor(Year)5",
               "as.factor(Year)6","as.factor(Year)7","as.factor(Year)8","as.factor(Year)9",
               "as.factor(Year)10","as.factor(Year)11","as.factor(Year)12","as.factor(Year)13",
               "as.factor(Year)14")
year.coeff = coeff.mod[which(names(coeff.mod) %in% year.names)]
year.error = error.mod[which(names(error.mod) %in% year.names)]
## plot the effect of categorical covariates
library(plotrix)
par(mar=c(4.3,4.3,0.5,2))
plotCI(x=1:14,y=c(0, year.coeff),
       li=c(0, year.coeff - 2*year.error),ui=c(0, year.coeff + 2*year.error),
       ylim=c(-1,1), xlab = "Year", ylab = "Regression coefficient",
       family = "sans",cex.axis = 1.7, cex.lab=2,xaxt="n", pch=19, col="red", scol="black",
       lwd=2)
axis(side = 1, at=1:14,labels=1:14, cex.axis = 1.2, family = "sans")
abline(h=0, col = "gray40", lwd = 2, lty=2)
##########################################################################################




#####################################################
#####predict the size of all the slope units#########
#####################################################
# pred.su = predict(Fit.size, data.size[1:10,], type = "terms")
pred.su = predict(Fit.size, data.size[1:10,], type = "terms", se.fit = TRUE)

restult_size = cbind(ID = data.size$ID_SU[1:46074], rawsize = data.size$ExpandedSlide[1:46074], 
                     rawlog = log(data.size$ExpandedSlide[1:46074]), realsize = exp(pred.su), 
                     logsize = pred.su)
write.table (restult_size, file =paste0("size",2004,".csv"), sep =",", row.names =FALSE)

#############################################################################



#####################################################
########calculate the exceedance probability#########
#####################################################
## based on 500 posterior samples
n = 500
threshold.size = 8 # the threshold in log scale

## a matrix is returned which yields the values of the linear predictor(minus any offset) 
## when postmultiplied by the parameter vector
lpmat = predict(Fit.size, data.size[1:46074,], type = "lpmatrix") #46074*120

## Generates multivariate normal or t random deviates, 
## and evaluates the corresponding log densities.
sims = rmvn(n, mu = coef(Fit.size), V = Fit.size$Vp) #500*120
fits = lpmat %*% t(sims) # matrix multiplication (46074*500)
exceedance_probability = apply(fits, 1, function(x){sum(x>threshold.size)/n})
#############################################################################



#####################################################
#############spatial autocorrelation########## ######
#####################################################
slopeunits.tr = rgdal::readOGR("Slope_units.shp")
nb = spdep::poly2nb(slopeunits.tr, queen=F, row.names = slopeunits.tr$value)
names(nb) <- attr(nb, "region.id")
str(nb[1:6])
library(spdep)
# nb2INLA("adj.csv", nb)
# save adjacent matrix to csv
result.adj = matrix(0,nrow = 46074,ncol = 23)
for (i in 1:46074) {
  adj.data = nb[i]
  result.adj[i,1] = i
  j = 2
  for (value in adj.data) {
    for (m in value) {
      result.adj[i,j] = m
      j = j + 1
    }
  }
}
write.table (result.adj, file ="adjacentMatrix.csv", sep =",", row.names =FALSE)


for (i in 1:46074) {
  data = nb[i]
  temp = list()
  temp = append(temp, i)
  for (j in data) {
    temp = append(temp, j)
  }
  write.table(temp, "adjacentMatrix.csv",col.names = F,row.names = F, append = T, sep =",")
}
nb1 = matrix(nb)
write.table (nb1, file ="adj.csv", sep =",", row.names =FALSE)
#####################################################



