library(mgcv)
library(Hmisc)
library(Metrics)
library(pROC)
#####################################################
#########read data and get target variable###########
#####################################################
data.path = "C:/Users/DELL/OneDrive/data_Luding/Codes/"

## load data from file: 
data.raw = read.table(paste0(data.path, "LuDingData.csv"),sep=",", header=TRUE) ## read data matrix
data.raw$Label = ifelse(data.raw$Area_SUM > 0, 1, 0)
data.raw$newID = 1:27272

##################################
###create the formula for model###
##################################
num.baseFun = 5
Formula.sus = Label ~ 
  s(Epicent_m, k = num.baseFun)+
  s(Road_m, k = num.baseFun)+
  s(PRC_s, k = num.baseFun)+
  s(PAG_3M_m, k = num.baseFun)+
  s(Aspect_m, k = num.baseFun)+
  s(Aspect_s, k = num.baseFun)+
  s(Slope_m, k = num.baseFun)+
  # s(Lon, Lat, bs = "gp", k=40, m=2)+
  as.factor(Lith_magor)+
  as.factor(LULC_magor)
#########################################################################


##################################
######execute the 10-fold CV######
##################################
n = 27272
n.partition=c(rep(2727,9),2729)## here you should write 9 times the size of the
set.seed(1)
idxlist=partition.vector(sample(1:n,size=n),n.partition)

## loop for 10-fold CV modelling
predict.all = data.frame() # save all the predictions
for (i in 1:10) {
  idx2pred=idxlist[[i]] # find the index
  data.train = data.raw[-idx2pred,] # nine subsets
  data.val = data.raw[idx2pred,] # the left-out subset
  
  start.time = Sys.time()
  Fit.sus = mgcv::gam(Formula.sus, family = "binomial", data = data.train)
  end.time = Sys.time()
  print(end.time-start.time)
  
  predict.val = predict(Fit.sus, data.val, type = 'response', se.fit=TRUE)  # predict the validation data
  temp = cbind(ID = data.val$newID, presence = data.val$Label, 
               predictions = predict.val$fit, randomID = replicate(length(idx2pred), c(i)))
  predict.all = rbind(predict.all, temp)
  
  ## calculate the metrics
  dataLabel.train = data.train$Label
  roc.train = roc(dataLabel.train ~ Fit.sus$fitted.values)
  
  dataLabel.val = data.val$Label
  roc.val = roc(dataLabel.val ~ predict.val$fit)

  print(paste0(i," train ROC:", round(roc.train$auc,3), " val ROC:", round(roc.val$auc,3)))
  print('------------------------------------')
  
}
#########################################################################
write.table(predict.all, file = paste0(data.path,"Sus_Predictions_CV_10fold.csv"), sep = ",", row.names =FALSE)


predict.all = read.csv("CV10fold.csv")
########################################################
#################predictive performance#################
########################################################
library(ggplot2)
library(pROC)
library(dplyr)
library(tidyr)

# 读取数据
data.auc <- read.table(paste0(data.path, "Sus_Predictions_CV_10fold.csv"), sep = ",", header = TRUE)

# 存储 ROC 数据
roc_data <- data.frame()
auc_values <- c()

# 遍历 10 折交叉验证
for (id in 1:10) {
  index <- which(data.auc$randomID == id)
  dataLabel <- data.auc$presence[index]
  predictedSus <- data.auc$predictions[index]
  
  ROC.fit <- roc(dataLabel ~ predictedSus)
  
  auc_values <- c(auc_values, ROC.fit$auc) # 记录 AUC
  
  # 存储当前折的 ROC 数据
  roc_data <- rbind(roc_data, data.frame(
    Specificity = ROC.fit$specificities,
    Sensitivity = ROC.fit$sensitivities,
    Fold = as.factor(id) # 用作分组
  ))
}

# 计算 AUC 均值
mean_auc <- mean(auc_values)
windows()
# 画 ROC 曲线
p1 = ggplot(roc_data, aes(x = 1 - Specificity, y = Sensitivity,  group = Fold)) +
  geom_line(size = 0.6, color = "black") +  # 每个折的 ROC 曲线
  scale_color_viridis_d() +  # 颜色映射
  labs(x = "1 - Specificity", y = "Sensitivity") +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.border = element_rect(colour = "black", fill=NA, size=1),
    panel.grid = element_blank(),
    axis.ticks = element_line(color = "black", size=0.5),
    axis.title = element_text(size = 24),  # 坐标轴标题大小
    axis.text = element_text(size = 20, family = "sans", color="black"),   # 坐标轴刻度大小
    legend.title = element_blank(),        # 去掉图例标题
    # legend.position = c(0.8, 0.1),
    legend.text = element_text(size = 20)  # 图例文本大小
  ) +
  annotate("text", x = 0.32, y = 0.18, label = "十折交叉验证", size = 5, hjust = 0) +
  annotate("text", x = 0.32, y = 0.08, label = paste0("Mean AUC = ", round(mean_auc, 2)), size = 5, hjust = 0)+
  annotate("segment", x = 0, xend = 1, y = 0, yend = 1, 
           linetype = "dashed", size = 1, color = "black")  # 添加虚线

# 画 AUC 箱线图
auc_data = data.frame(AUC = auc_values)

p2 = ggplot(auc_data, aes(x = "", y = AUC)) +
  geom_boxplot(fill = "#4682B4", color = "black", size = 1.2) +
  labs(y = "", x = "") +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.border = element_rect(colour = "black", fill=NA, size=1),
    panel.grid = element_blank(),
    axis.ticks = element_line(color = "black", size=0.5),
    axis.text.x = element_blank(),
    axis.title.y = element_text(size = 20, family = "sans", color="black"),
    axis.text.y = element_text(size = 20, family = "sans", color="black")
  ) +
  scale_y_continuous(limits = c(0.8, 1), breaks = seq(0.8, 1, by = 0.05),position = "right")

# 组合图
# library(patchwork)
main.plot = p1 + p2 + plot_layout(widths = c(6, 1)) # ROC 图较宽，AUC 统计较窄

# main.plot = gridExtra::grid.arrange(
#   p1, p2,   # 两个图
#   ncol = 2, nrow = 1,
#   widths = c(3, 1) 
# )

ggsave(
  filename = "10foldSus.pdf",  # PDF 文件名
  plot = main.plot,            # 要保存的图形对象
  path = "C:/Users/DELL/OneDrive/data_Luding/Results/",  # 文件保存路径（可根据需要修改）
  width = 7,                   # PDF 宽度（单位：英寸）
  height = 5,                  # PDF 高度（单位：英寸）
  units = "in"                 # 单位（英寸）
)





