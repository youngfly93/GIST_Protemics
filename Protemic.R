# 加载必需的R包
library(data.table)
library(stringr)
library(ggpubr)
library(tidyverse)
library(ggplot2)
library(ggsci)
library(patchwork)
library(pROC)  # 用于ROC分析
library(survival)  # 生存分析
library(survminer)  # 生存分析可视化

######### 加载数据集  #########

Protemics_list <- readRDS("Protemics_list.rds")

#########    Function module 1:  临床性状模块   ##########     

dbGIST_Proteomics_boxplot_TvsN <- function(ID, DB = Protemics_list){
  
#  ID = "KIT"
#  DB <- Protemics_list
  # 只有数据集1和2有这个信息，其中2需要调用4
  p1 <- NULL
  p2 <- NULL 
  
  if(ID %in% rownames(DB[[1]]$Matrix) | 
     ((ID %in% rownames(DB[[2]]$Matrix)) & 
      (ID %in% rownames(DB[[4]]$Matrix)))){
    
    if(ID %in% rownames(DB[[1]]$Matrix)){
      
      p1_table <- data.frame(ID = c(as.numeric(DB[[1]]$Matrix[which(rownames(DB[[1]]$Matrix) == ID),]),
                                    rep(1,ncol(DB[[1]]$Matrix))),
                             Clinical = c(rep("Tumor",ncol(DB[[1]]$Matrix)),
                                          rep("Normal",ncol(DB[[1]]$Matrix))),
                             Sample = c(c(str_c("T",1:ncol(DB[[1]]$Matrix)),
                                          str_c("T",1:ncol(DB[[1]]$Matrix)))))
      
      p1 <- ggplot(p1_table,aes(Clinical,ID,fill=Clinical))+
        geom_boxplot(outlier.colour = NA,alpha = 0.4,notch = F,size = 0.5)+
        geom_jitter(shape = 21,size=2,width = 0.2) + 
        geom_violin(position = position_dodge(width = .75), 
                    size = NA,alpha = 0.4,trim = T) + 
        geom_line(aes(group = Sample), color = "gray50", alpha = 0.6, 
                  linetype = "dashed", linewidth = 0.8) +
        scale_fill_lancet() + 
        theme_bw() + 
        xlab("Risk") +
        ylab(ID)+ 
        ggtitle(DB[[1]]$ID) +
        theme(legend.position = 'none',
              panel.background = element_rect(fill = "#F3F6F6"),
              panel.border = element_rect(linewidth = 1.2),
              panel.grid.major = element_line(colour = "#DEE2E4",
                                              linewidth = 1.0,
                                              linetype = "dashed"),
              plot.title = element_text(hjust = 0.5,
                                        size = 14,
                                        colour = "darkred",
                                        face = "bold"),
              axis.title.y = element_text(size=12,
                                          colour = "darkred",
                                          face = "bold"),
              axis.text.x =element_text(size=12,
                                        angle = 45,
                                        hjust = 1,
                                        #  colour = "black",
                                        face = "bold"),
              axis.text.y = element_text(size=10,
                                         #  colour = "black",
                                         face = "bold"),
              axis.title.x = element_blank()) + 
        stat_compare_means(
          method = "t.test",  # 自动根据正态性选择t检验/Wilcoxon[3](@ref)
          paired = TRUE,      # 启用配对检验
          label = "p.format", # 显示p值或星号(p.signif)[8](@ref)
        )
      
    }
    
    # 只有癌与癌旁同时被检测到，才会进行比较
    if((ID %in% rownames(DB[[2]]$Matrix)) & 
       (ID %in% rownames(DB[[4]]$Matrix))){
      
      Protemics2_T <- DB[[2]]$Matrix
      Protemics2_N <- DB[[4]]$Matrix
      
      p1_table <- data.frame(ID = c(as.numeric(Protemics2_T[which(rownames(Protemics2_T) == ID),]),
                                    as.numeric(Protemics2_N[which(rownames(Protemics2_N) == ID),])),
                             Clinical = c(rep("Tumor",ncol(Protemics2_T)),
                                          rep("Normal",ncol(Protemics2_N))))
      
      p1_table <- na.omit(p1_table)
      
      p2 <- ggplot(p1_table,aes(Clinical,ID,fill=Clinical))+
        geom_boxplot(outlier.colour = NA,alpha = 0.4,notch = F,size = 0.5)+
        geom_jitter(shape = 21,size=2,width = 0.2) + 
        geom_violin(position = position_dodge(width = .75), 
                    size = NA,alpha = 0.4,trim = T) + 
        scale_fill_lancet() + 
        theme_bw() + 
        xlab("Risk") +
        ylab(ID)+ 
        ggtitle(DB[[2]]$ID)+
        theme(legend.position = 'none',
              panel.background = element_rect(fill = "#F3F6F6"),
              panel.border = element_rect(linewidth = 1.2),
              panel.grid.major = element_line(colour = "#DEE2E4",
                                              linewidth = 1.0,
                                              linetype = "dashed"),
              plot.title = element_text(hjust = 0.5,
                                        size = 14,
                                        colour = "darkred",
                                        face = "bold"),
              axis.title.y = element_text(size=12,
                                          colour = "darkred",
                                          face = "bold"),
              axis.text.x =element_text(size=12,
                                        angle = 45,
                                        hjust = 1,
                                        #  colour = "black",
                                        face = "bold"),
              axis.text.y = element_text(size=10,
                                         #  colour = "black",
                                         face = "bold"),
              axis.title.x = element_blank()) + stat_compare_means()
      
      
    }
    
  } else {
    
    return(NULL)
    
  }
  
 if(is.null(p1)){
   
   p <- p2
   
 } else {
   
   p <- p1
   
   if(is.null(p2)){
     
     p <- p1
     
   } else {
     
     p <- p1 + p2
     
   }
   
 }
 
  return(p)
  
}

dbGIST_Proteomics_boxplot_Risk <- function(ID, DB = Protemics_list){
  
  # ID = "P4HA1"
  # DB = Protemics_list
  # 只有数据集1和2有这个信息，其中2需要调用4
  
  p1 <- NULL
  p2 <- NULL 
  p3 <- NULL
  
  
  if(ID %in% rownames(DB[[1]]$Matrix) | 
     (ID %in% rownames(DB[[2]]$Matrix)) | 
     (ID %in% rownames(DB[[3]]$Matrix))){
    
    if(ID %in% rownames(DB[[1]]$Matrix)){
      
      p1_table <- data.frame(ID = as.numeric(DB[[1]]$Matrix[which(rownames(DB[[1]]$Matrix) == ID),]),
                             Clinical = DB[[1]]$Clinical$NIH[match(colnames(DB[[1]]$Matrix),
                                                                   DB[[1]]$Clinical$ID)])
      
      p1_table$Clinical <- factor(p1_table$Clinical,
                                  levels = c("Low","Intermediate","High"))
      
      p1 <- ggplot(p1_table,aes(Clinical,ID,fill=Clinical))+
        geom_boxplot(outlier.colour = NA,alpha = 0.4,notch = F,size = 0.5)+
        geom_jitter(shape = 21,size=2,width = 0.2) + 
        geom_violin(position = position_dodge(width = .75), 
                    size = NA,alpha = 0.4,trim = T) + 
        scale_fill_lancet() + 
        theme_bw() + 
        xlab("Risk") +
        ylab(ID)+ 
        ggtitle(DB[[1]]$ID) +
        theme(legend.position = 'none',
              panel.background = element_rect(fill = "#F3F6F6"),
              panel.border = element_rect(linewidth = 1.2),
              panel.grid.major = element_line(colour = "#DEE2E4",
                                              linewidth = 1.0,
                                              linetype = "dashed"),
              plot.title = element_text(hjust = 0.5,
                                        size = 14,
                                        colour = "darkred",
                                        face = "bold"),
              axis.title.y = element_text(size=12,
                                          colour = "darkred",
                                          face = "bold"),
              axis.text.x =element_text(size=12,
                                        angle = 45,
                                        hjust = 1,
                                        #  colour = "black",
                                        face = "bold"),
              axis.text.y = element_text(size=10,
                                         #  colour = "black",
                                         face = "bold"),
              axis.title.x = element_blank()) + 
        stat_compare_means()
      
    }
    
    # 只有癌与癌旁同时被检测到，才会进行比较
    if(ID %in% rownames(DB[[2]]$Matrix)){
      
      Protemics2_T <- DB[[2]]$Matrix
      
      p1_table <- data.frame(ID = as.numeric(Protemics2_T[which(rownames(Protemics2_T) == ID),]),
                             Clinical = DB[[2]]$Clinical$NIH[match(colnames(DB[[2]]$Matrix),
                                                                   DB[[2]]$Clinical$Sample.ID)])
      
      p1_table <- na.omit(p1_table)
      
      p1_table$Clinical <- factor(p1_table$Clinical,
                                  levels = c("Very low","Low","Intermediate","High"))
      
      p2 <- ggplot(p1_table,aes(Clinical,ID,fill=Clinical))+
        geom_boxplot(outlier.colour = NA,alpha = 0.4,notch = F,size = 0.5)+
        geom_jitter(shape = 21,size=2,width = 0.2) + 
        geom_violin(position = position_dodge(width = .75), 
                    size = NA,alpha = 0.4,trim = T) + 
        scale_fill_lancet() + 
        theme_bw() + 
        xlab("Risk") +
        ylab(ID)+ 
        ggtitle(DB[[2]]$ID)+
        theme(legend.position = 'none',
              panel.background = element_rect(fill = "#F3F6F6"),
              panel.border = element_rect(linewidth = 1.2),
              panel.grid.major = element_line(colour = "#DEE2E4",
                                              linewidth = 1.0,
                                              linetype = "dashed"),
              plot.title = element_text(hjust = 0.5,
                                        size = 14,
                                        colour = "darkred",
                                        face = "bold"),
              axis.title.y = element_text(size=12,
                                          colour = "darkred",
                                          face = "bold"),
              axis.text.x =element_text(size=12,
                                        angle = 45,
                                        hjust = 1,
                                        #  colour = "black",
                                        face = "bold"),
              axis.text.y = element_text(size=10,
                                         #  colour = "black",
                                         face = "bold"),
              axis.title.x = element_blank()) + stat_compare_means()
      
      
    }
    
    if(ID %in% rownames(DB[[3]]$Matrix)){
      
      p1_table <- data.frame(ID = as.numeric(DB[[3]]$Matrix[which(rownames(DB[[3]]$Matrix) == ID),]),
                             Clinical = DB[[3]]$Clinical$NIH[match(colnames(DB[[3]]$Matrix),DB[[3]]$Clinical$Sample)])
      
      p1_table <- na.omit(p1_table)
      
      p1_table$Clinical <- factor(p1_table$Clinical,
                                  levels = c("Very low","Low","Intermediate","High"))
      
      p3 <- ggplot(p1_table,aes(Clinical,ID,fill=Clinical))+
        geom_boxplot(outlier.colour = NA,alpha = 0.4,notch = F,size = 0.5)+
        geom_jitter(shape = 21,size=2,width = 0.2) + 
        geom_violin(position = position_dodge(width = .75), 
                    size = NA,alpha = 0.4,trim = T) + 
        scale_fill_lancet() + 
        theme_bw() + 
        xlab("Risk") +
        ylab(ID)+ 
        ggtitle(DB[[3]]$ID)+
        theme(legend.position = 'none',
              panel.background = element_rect(fill = "#F3F6F6"),
              panel.border = element_rect(linewidth = 1.2),
              panel.grid.major = element_line(colour = "#DEE2E4",
                                              linewidth = 1.0,
                                              linetype = "dashed"),
              plot.title = element_text(hjust = 0.5,
                                        size = 14,
                                        colour = "darkred",
                                        face = "bold"),
              axis.title.y = element_text(size=12,
                                          colour = "darkred",
                                          face = "bold"),
              axis.text.x =element_text(size=12,
                                        angle = 45,
                                        hjust = 1,
                                        #  colour = "black",
                                        face = "bold"),
              axis.text.y = element_text(size=10,
                                         #  colour = "black",
                                         face = "bold"),
              axis.title.x = element_blank()) + stat_compare_means(method = "t.test")
      
    }

    
  } else {
    
    return(NULL)
    
  }
  
  plots <- list()
  plots[[1]] <- p1
  plots[[2]] <- p2
  plots[[3]] <- p3
  
  return(patchwork:::wrap_plots(plots))
  
}

dbGIST_Proteomics_boxplot_Gender <- function(ID, DB = Protemics_list){
  
  # ID = "P4HA1"
  # DB = Protemics_list
  # 只有数据集1和2有这个信息，其中2需要调用4
  
  p1 <- NULL
  p2 <- NULL 
  
  if((ID %in% rownames(DB[[2]]$Matrix)) | 
     (ID %in% rownames(DB[[3]]$Matrix))){
    
    if(ID %in% rownames(DB[[2]]$Matrix)){
      
      p1_table <- data.frame(ID = as.numeric(DB[[2]]$Matrix[which(rownames(DB[[2]]$Matrix) == ID),]),
                             Clinical = DB[[2]]$Clinical$Gender[match(colnames(DB[[2]]$Matrix),
                                                                   DB[[2]]$Clinical$Sample.ID)])
      
      p1_table$Clinical <- factor(p1_table$Clinical,
                                  levels = c("Male","Female"))
      
      p1 <- ggplot(p1_table,aes(Clinical,ID,fill=Clinical))+
        geom_boxplot(outlier.colour = NA,alpha = 0.4,notch = F,size = 0.5)+
        geom_jitter(shape = 21,size=2,width = 0.2) + 
        geom_violin(position = position_dodge(width = .75), 
                    size = NA,alpha = 0.4,trim = T) + 
        scale_fill_lancet() + 
        theme_bw() + 
        xlab("Risk") +
        ylab(ID)+ 
        ggtitle(DB[[2]]$ID) +
        theme(legend.position = 'none',
              panel.background = element_rect(fill = "#F3F6F6"),
              panel.border = element_rect(linewidth = 1.2),
              panel.grid.major = element_line(colour = "#DEE2E4",
                                              linewidth = 1.0,
                                              linetype = "dashed"),
              plot.title = element_text(hjust = 0.5,
                                        size = 14,
                                        colour = "darkred",
                                        face = "bold"),
              axis.title.y = element_text(size=12,
                                          colour = "darkred",
                                          face = "bold"),
              axis.text.x =element_text(size=12,
                                        angle = 45,
                                        hjust = 1,
                                        #  colour = "black",
                                        face = "bold"),
              axis.text.y = element_text(size=10,
                                         #  colour = "black",
                                         face = "bold"),
              axis.title.x = element_blank()) + 
        stat_compare_means()
      
    }
    
    # 只有癌与癌旁同时被检测到，才会进行比较
    if(ID %in% rownames(DB[[3]]$Matrix)){
      
      Protemics2_T <- DB[[3]]$Matrix
      
      p1_table <- data.frame(ID = as.numeric(Protemics2_T[which(rownames(Protemics2_T) == ID),]),
                             Clinical = DB[[3]]$Clinical$Gender[match(colnames(DB[[3]]$Matrix),
                                                                   DB[[3]]$Clinical$Sample)])
      
      p1_table <- na.omit(p1_table)
      
      p1_table$Clinical <- factor(p1_table$Clinical,
                                  levels = c("Male","Female"))
      
      p2 <- ggplot(p1_table,aes(Clinical,ID,fill=Clinical))+
        geom_boxplot(outlier.colour = NA,alpha = 0.4,notch = F,size = 0.5)+
        geom_jitter(shape = 21,size=2,width = 0.2) + 
        geom_violin(position = position_dodge(width = .75), 
                    size = NA,alpha = 0.4,trim = T) + 
        scale_fill_lancet() + 
        theme_bw() + 
        xlab("Risk") +
        ylab(ID)+ 
        ggtitle(DB[[3]]$ID)+
        theme(legend.position = 'none',
              panel.background = element_rect(fill = "#F3F6F6"),
              panel.border = element_rect(linewidth = 1.2),
              panel.grid.major = element_line(colour = "#DEE2E4",
                                              linewidth = 1.0,
                                              linetype = "dashed"),
              plot.title = element_text(hjust = 0.5,
                                        size = 14,
                                        colour = "darkred",
                                        face = "bold"),
              axis.title.y = element_text(size=12,
                                          colour = "darkred",
                                          face = "bold"),
              axis.text.x =element_text(size=12,
                                        angle = 45,
                                        hjust = 1,
                                        #  colour = "black",
                                        face = "bold"),
              axis.text.y = element_text(size=10,
                                         #  colour = "black",
                                         face = "bold"),
              axis.title.x = element_blank()) + stat_compare_means()
      
      
    }
    
    
  } else {
    
    return(NULL)
    
  }
  
  plots <- list()
  plots[[1]] <- p1
  plots[[2]] <- p2
  
  return(patchwork:::wrap_plots(plots))
  
}

dbGIST_Proteomics_boxplot_Age <- function(ID, DB = Protemics_list){
  
  # ID = "P4HA1"
  # DB = Protemics_list
  # 只有数据集1和2有这个信息，其中2需要调用4
  
  p1 <- NULL
  
  if(ID %in% rownames(DB[[2]]$Matrix)){
    
    p1_table <- data.frame(ID = as.numeric(DB[[2]]$Matrix[which(rownames(DB[[2]]$Matrix) == ID),]),
                             Clinical = DB[[2]]$Clinical$Age[match(colnames(DB[[2]]$Matrix),
                                                                      DB[[2]]$Clinical$Sample.ID)])
      
    p1_table$Clinical <- factor(p1_table$Clinical,
                                  levels = c("<=60",">60"))
      
    p1 <- ggplot(p1_table,aes(Clinical,ID,fill=Clinical))+
        geom_boxplot(outlier.colour = NA,alpha = 0.4,notch = F,size = 0.5)+
        geom_jitter(shape = 21,size=2,width = 0.2) + 
        geom_violin(position = position_dodge(width = .75), 
                    size = NA,alpha = 0.4,trim = T) + 
        scale_fill_lancet() + 
        theme_bw() + 
        xlab("Risk") +
        ylab(ID)+ 
        ggtitle(DB[[2]]$ID) +
        theme(legend.position = 'none',
              panel.background = element_rect(fill = "#F3F6F6"),
              panel.border = element_rect(linewidth = 1.2),
              panel.grid.major = element_line(colour = "#DEE2E4",
                                              linewidth = 1.0,
                                              linetype = "dashed"),
              plot.title = element_text(hjust = 0.5,
                                        size = 14,
                                        colour = "darkred",
                                        face = "bold"),
              axis.title.y = element_text(size=12,
                                          colour = "darkred",
                                          face = "bold"),
              axis.text.x =element_text(size=12,
                                        angle = 45,
                                        hjust = 1,
                                        #  colour = "black",
                                        face = "bold"),
              axis.text.y = element_text(size=10,
                                         #  colour = "black",
                                         face = "bold"),
              axis.title.x = element_blank()) + 
        stat_compare_means()
      
    }  else {
    
    return(NULL)
    
  }
  
  plots <- list()
  plots[[1]] <- p1
  
  return(patchwork:::wrap_plots(plots))
  
}

dbGIST_Proteomics_boxplot_Tumor.size <- function(ID, DB = Protemics_list){
  
  # ID = "P4HA1"
  # DB = Protemics_list
  # 只有数据集1和2有这个信息，其中2需要调用4
  
  p1 <- NULL
  p2 <- NULL 
  
  if((ID %in% rownames(DB[[2]]$Matrix)) | 
     (ID %in% rownames(DB[[3]]$Matrix))){
    
    if(ID %in% rownames(DB[[2]]$Matrix)){
      
      p1_table <- data.frame(ID = as.numeric(DB[[2]]$Matrix[which(rownames(DB[[2]]$Matrix) == ID),]),
                             Clinical = DB[[2]]$Clinical$Tumor.size[match(colnames(DB[[2]]$Matrix),
                                                                      DB[[2]]$Clinical$Sample.ID)])
      
      p1_table$Clinical <- factor(p1_table$Clinical,
                                  levels = c("<2",">2 <5",">5<10",">10"))
      
      p1_table <- na.omit(p1_table)
      
      p1 <- ggplot(p1_table,aes(Clinical,ID,fill=Clinical))+
        geom_boxplot(outlier.colour = NA,alpha = 0.4,notch = F,size = 0.5)+
        geom_jitter(shape = 21,size=2,width = 0.2) + 
        geom_violin(position = position_dodge(width = .75), 
                    size = NA,alpha = 0.4,trim = T) + 
        scale_fill_lancet() + 
        theme_bw() + 
        xlab("Risk") +
        ylab(ID)+ 
        ggtitle(DB[[2]]$ID) +
        theme(legend.position = 'none',
              panel.background = element_rect(fill = "#F3F6F6"),
              panel.border = element_rect(linewidth = 1.2),
              panel.grid.major = element_line(colour = "#DEE2E4",
                                              linewidth = 1.0,
                                              linetype = "dashed"),
              plot.title = element_text(hjust = 0.5,
                                        size = 14,
                                        colour = "darkred",
                                        face = "bold"),
              axis.title.y = element_text(size=12,
                                          colour = "darkred",
                                          face = "bold"),
              axis.text.x =element_text(size=12,
                                        angle = 45,
                                        hjust = 1,
                                        #  colour = "black",
                                        face = "bold"),
              axis.text.y = element_text(size=10,
                                         #  colour = "black",
                                         face = "bold"),
              axis.title.x = element_blank()) + 
        stat_compare_means()
      
    }
    
    # 只有癌与癌旁同时被检测到，才会进行比较
    if(ID %in% rownames(DB[[3]]$Matrix)){
      
      Protemics2_T <- DB[[3]]$Matrix
      
      p1_table <- data.frame(ID = as.numeric(Protemics2_T[which(rownames(Protemics2_T) == ID),]),
                             Clinical = DB[[3]]$Clinical$Tumor.size[match(colnames(DB[[3]]$Matrix),
                                                                      DB[[3]]$Clinical$Sample)])
      
      p1_table$Clinical <- factor(p1_table$Clinical,
                                  levels = c("<2",">2<5",">5<10",">10"))
      
      p1_table <- na.omit(p1_table)
      
      p2 <- ggplot(p1_table,aes(Clinical,ID,fill=Clinical))+
        geom_boxplot(outlier.colour = NA,alpha = 0.4,notch = F,size = 0.5)+
        geom_jitter(shape = 21,size=2,width = 0.2) + 
        geom_violin(position = position_dodge(width = .75), 
                    size = NA,alpha = 0.4,trim = T) + 
        scale_fill_lancet() + 
        theme_bw() + 
        xlab("Risk") +
        ylab(ID)+ 
        ggtitle(DB[[3]]$ID)+
        theme(legend.position = 'none',
              panel.background = element_rect(fill = "#F3F6F6"),
              panel.border = element_rect(linewidth = 1.2),
              panel.grid.major = element_line(colour = "#DEE2E4",
                                              linewidth = 1.0,
                                              linetype = "dashed"),
              plot.title = element_text(hjust = 0.5,
                                        size = 14,
                                        colour = "darkred",
                                        face = "bold"),
              axis.title.y = element_text(size=12,
                                          colour = "darkred",
                                          face = "bold"),
              axis.text.x =element_text(size=12,
                                        angle = 45,
                                        hjust = 1,
                                        #  colour = "black",
                                        face = "bold"),
              axis.text.y = element_text(size=10,
                                         #  colour = "black",
                                         face = "bold"),
              axis.title.x = element_blank()) + stat_compare_means()
      
      
    }
    
    
  } else {
    
    return(NULL)
    
  }
  
  plots <- list()
  plots[[1]] <- p1
  plots[[2]] <- p2
  
  return(patchwork:::wrap_plots(plots))
  
}

dbGIST_Proteomics_boxplot_Mitotic.count <- function(ID, DB = Protemics_list){
  
  # ID = "P4HA1"
  # DB = Protemics_list
  # 只有数据集1和2有这个信息，其中2需要调用4
  
  p1 <- NULL
  p2 <- NULL 
  
  if((ID %in% rownames(DB[[2]]$Matrix)) | 
     (ID %in% rownames(DB[[3]]$Matrix))){
    
    if(ID %in% rownames(DB[[2]]$Matrix)){
      
      p1_table <- data.frame(ID = as.numeric(DB[[2]]$Matrix[which(rownames(DB[[2]]$Matrix) == ID),]),
                             Clinical = DB[[2]]$Clinical$Mitotic.count[match(colnames(DB[[2]]$Matrix),
                                                                          DB[[2]]$Clinical$Sample.ID)])
      
      p1_table$Clinical <- factor(p1_table$Clinical,
                                  levels = c("<=5",">5"))
      
      p1_table <- na.omit(p1_table)
      
      p1 <- ggplot(p1_table,aes(Clinical,ID,fill=Clinical))+
        geom_boxplot(outlier.colour = NA,alpha = 0.4,notch = F,size = 0.5)+
        geom_jitter(shape = 21,size=2,width = 0.2) + 
        geom_violin(position = position_dodge(width = .75), 
                    size = NA,alpha = 0.4,trim = T) + 
        scale_fill_lancet() + 
        theme_bw() + 
        xlab("Risk") +
        ylab(ID)+ 
        ggtitle(DB[[2]]$ID) +
        theme(legend.position = 'none',
              panel.background = element_rect(fill = "#F3F6F6"),
              panel.border = element_rect(linewidth = 1.2),
              panel.grid.major = element_line(colour = "#DEE2E4",
                                              linewidth = 1.0,
                                              linetype = "dashed"),
              plot.title = element_text(hjust = 0.5,
                                        size = 14,
                                        colour = "darkred",
                                        face = "bold"),
              axis.title.y = element_text(size=12,
                                          colour = "darkred",
                                          face = "bold"),
              axis.text.x =element_text(size=12,
                                        angle = 45,
                                        hjust = 1,
                                        #  colour = "black",
                                        face = "bold"),
              axis.text.y = element_text(size=10,
                                         #  colour = "black",
                                         face = "bold"),
              axis.title.x = element_blank()) + 
        stat_compare_means()
      
    }
    
    # 只有癌与癌旁同时被检测到，才会进行比较
    if(ID %in% rownames(DB[[3]]$Matrix)){
      
      Protemics2_T <- DB[[3]]$Matrix
      
      p1_table <- data.frame(ID = as.numeric(Protemics2_T[which(rownames(Protemics2_T) == ID),]),
                             Clinical = DB[[3]]$Clinical$Mitotic.count[match(colnames(DB[[3]]$Matrix),
                                                                          DB[[3]]$Clinical$Sample)])
      
      p1_table$Clinical <- factor(p1_table$Clinical,
                                  levels = c("<=5",">5"))
      
      p1_table <- na.omit(p1_table)
      
      p2 <- ggplot(p1_table,aes(Clinical,ID,fill=Clinical))+
        geom_boxplot(outlier.colour = NA,alpha = 0.4,notch = F,size = 0.5)+
        geom_jitter(shape = 21,size=2,width = 0.2) + 
        geom_violin(position = position_dodge(width = .75), 
                    size = NA,alpha = 0.4,trim = T) + 
        scale_fill_lancet() + 
        theme_bw() + 
        xlab("Risk") +
        ylab(ID)+ 
        ggtitle(DB[[3]]$ID)+
        theme(legend.position = 'none',
              panel.background = element_rect(fill = "#F3F6F6"),
              panel.border = element_rect(linewidth = 1.2),
              panel.grid.major = element_line(colour = "#DEE2E4",
                                              linewidth = 1.0,
                                              linetype = "dashed"),
              plot.title = element_text(hjust = 0.5,
                                        size = 14,
                                        colour = "darkred",
                                        face = "bold"),
              axis.title.y = element_text(size=12,
                                          colour = "darkred",
                                          face = "bold"),
              axis.text.x =element_text(size=12,
                                        angle = 45,
                                        hjust = 1,
                                        #  colour = "black",
                                        face = "bold"),
              axis.text.y = element_text(size=10,
                                         #  colour = "black",
                                         face = "bold"),
              axis.title.x = element_blank()) + stat_compare_means()
      
      
    }
    
    
  } else {
    
    return(NULL)
    
  }
  
  plots <- list()
  plots[[1]] <- p1
  plots[[2]] <- p2
  
  return(patchwork:::wrap_plots(plots))
  
}

dbGIST_Proteomics_boxplot_Location <- function(ID, DB = Protemics_list){
  
  # ID = "P4HA1"
  # DB = Protemics_list
  # 只有数据集1和2有这个信息，其中2需要调用4
  
  p1 <- NULL
  p2 <- NULL 
  
  if((ID %in% rownames(DB[[2]]$Matrix)) | 
     (ID %in% rownames(DB[[3]]$Matrix))){
    
    if(ID %in% rownames(DB[[2]]$Matrix)){
      
      p1_table <- data.frame(ID = as.numeric(DB[[2]]$Matrix[which(rownames(DB[[2]]$Matrix) == ID),]),
                             Clinical = DB[[2]]$Clinical$Tumor.location[match(colnames(DB[[2]]$Matrix),
                                                                             DB[[2]]$Clinical$Sample.ID)])
      
      p1_table <- na.omit(p1_table)
      
      p1 <- ggplot(p1_table,aes(Clinical,ID,fill=Clinical))+
        geom_boxplot(outlier.colour = NA,alpha = 0.4,notch = F,size = 0.5)+
        geom_jitter(shape = 21,size=2,width = 0.2) + 
        geom_violin(position = position_dodge(width = .75), 
                    size = NA,alpha = 0.4,trim = T) + 
        scale_fill_lancet() + 
        theme_bw() + 
        xlab("Risk") +
        ylab(ID)+ 
        ggtitle(DB[[2]]$ID) +
        theme(legend.position = 'none',
              panel.background = element_rect(fill = "#F3F6F6"),
              panel.border = element_rect(linewidth = 1.2),
              panel.grid.major = element_line(colour = "#DEE2E4",
                                              linewidth = 1.0,
                                              linetype = "dashed"),
              plot.title = element_text(hjust = 0.5,
                                        size = 14,
                                        colour = "darkred",
                                        face = "bold"),
              axis.title.y = element_text(size=12,
                                          colour = "darkred",
                                          face = "bold"),
              axis.text.x =element_text(size=12,
                                        angle = 45,
                                        hjust = 1,
                                        #  colour = "black",
                                        face = "bold"),
              axis.text.y = element_text(size=10,
                                         #  colour = "black",
                                         face = "bold"),
              axis.title.x = element_blank()) + 
        stat_compare_means()
      
    }
    
    # 只有癌与癌旁同时被检测到，才会进行比较
    if(ID %in% rownames(DB[[3]]$Matrix)){
      
      Protemics2_T <- DB[[3]]$Matrix
      
      p1_table <- data.frame(ID = as.numeric(Protemics2_T[which(rownames(Protemics2_T) == ID),]),
                             Clinical = DB[[3]]$Clinical$Location[match(colnames(DB[[3]]$Matrix),
                                                                             DB[[3]]$Clinical$Sample)])
      
      p1_table <- na.omit(p1_table)
      
      p2 <- ggplot(p1_table,aes(Clinical,ID,fill=Clinical))+
        geom_boxplot(outlier.colour = NA,alpha = 0.4,notch = F,size = 0.5)+
        geom_jitter(shape = 21,size=2,width = 0.2) + 
        geom_violin(position = position_dodge(width = .75), 
                    size = NA,alpha = 0.4,trim = T) + 
        scale_fill_lancet() + 
        theme_bw() + 
        xlab("Risk") +
        ylab(ID)+ 
        ggtitle(DB[[3]]$ID)+
        theme(legend.position = 'none',
              panel.background = element_rect(fill = "#F3F6F6"),
              panel.border = element_rect(linewidth = 1.2),
              panel.grid.major = element_line(colour = "#DEE2E4",
                                              linewidth = 1.0,
                                              linetype = "dashed"),
              plot.title = element_text(hjust = 0.5,
                                        size = 14,
                                        colour = "darkred",
                                        face = "bold"),
              axis.title.y = element_text(size=12,
                                          colour = "darkred",
                                          face = "bold"),
              axis.text.x =element_text(size=12,
                                        angle = 45,
                                        hjust = 1,
                                        #  colour = "black",
                                        face = "bold"),
              axis.text.y = element_text(size=10,
                                         #  colour = "black",
                                         face = "bold"),
              axis.title.x = element_blank()) + stat_compare_means()
      
      
    }
    
    
  } else {
    
    return(NULL)
    
  }
  
  plots <- list()
  plots[[1]] <- p1
  plots[[2]] <- p2
  
  return(patchwork:::wrap_plots(plots))
  
}

dbGIST_Proteomics_boxplot_WHO <- function(ID, DB = Protemics_list){
  
  # ID = "P4HA1"
  # DB = Protemics_list
  # 只有数据集1和2有这个信息，其中2需要调用4
  
  p1 <- NULL
  
  if(ID %in% rownames(DB[[2]]$Matrix)){
    
    p1_table <- data.frame(ID = as.numeric(DB[[2]]$Matrix[which(rownames(DB[[2]]$Matrix) == ID),]),
                           Clinical = DB[[2]]$Clinical$WHO[match(colnames(DB[[2]]$Matrix),
                                                                 DB[[2]]$Clinical$Sample.ID)])
    
    p1_table$Clinical <- factor(p1_table$Clinical,
                                levels = c("Low","High"))
    
    p1_table <- na.omit(p1_table)
    
    p1 <- ggplot(p1_table,aes(Clinical,ID,fill=Clinical))+
      geom_boxplot(outlier.colour = NA,alpha = 0.4,notch = F,size = 0.5)+
      geom_jitter(shape = 21,size=2,width = 0.2) + 
      geom_violin(position = position_dodge(width = .75), 
                  size = NA,alpha = 0.4,trim = T) + 
      scale_fill_lancet() + 
      theme_bw() + 
      xlab("Risk") +
      ylab(ID)+ 
      ggtitle(DB[[2]]$ID) +
      theme(legend.position = 'none',
            panel.background = element_rect(fill = "#F3F6F6"),
            panel.border = element_rect(linewidth = 1.2),
            panel.grid.major = element_line(colour = "#DEE2E4",
                                            linewidth = 1.0,
                                            linetype = "dashed"),
            plot.title = element_text(hjust = 0.5,
                                      size = 14,
                                      colour = "darkred",
                                      face = "bold"),
            axis.title.y = element_text(size=12,
                                        colour = "darkred",
                                        face = "bold"),
            axis.text.x =element_text(size=12,
                                      angle = 45,
                                      hjust = 1,
                                      #  colour = "black",
                                      face = "bold"),
            axis.text.y = element_text(size=10,
                                       #  colour = "black",
                                       face = "bold"),
            axis.title.x = element_blank()) + 
      stat_compare_means()
    
  }  else {
    
    return(NULL)
    
  }
  
  plots <- list()
  plots[[1]] <- p1
  
  return(patchwork:::wrap_plots(plots))
  
}

dbGIST_Proteomics_boxplot_Ki.67 <- function(ID, DB = Protemics_list){
  
  # ID = "P4HA1"
  # DB = Protemics_list
  # 只有数据集1和2有这个信息，其中2需要调用4
  
  p1 <- NULL
  
  if(ID %in% rownames(DB[[3]]$Matrix)){
    
    p1_table <- data.frame(ID = as.numeric(DB[[3]]$Matrix[which(rownames(DB[[3]]$Matrix) == ID),]),
                           Clinical = DB[[3]]$Clinical$Ki.67[match(colnames(DB[[3]]$Matrix),
                                                                 DB[[3]]$Clinical$Sample)])
    
    p1_table$Clinical <- factor(p1_table$Clinical,
                                levels = c("<=10",">10"))
    
    p1_table <- na.omit(p1_table)
    
    p1 <- ggplot(p1_table,aes(Clinical,ID,fill=Clinical))+
      geom_boxplot(outlier.colour = NA,alpha = 0.4,notch = F,size = 0.5)+
      geom_jitter(shape = 21,size=2,width = 0.2) + 
      geom_violin(position = position_dodge(width = .75), 
                  size = NA,alpha = 0.4,trim = T) + 
      scale_fill_lancet() + 
      theme_bw() + 
      xlab("Risk") +
      ylab(ID)+ 
      ggtitle(DB[[2]]$ID) +
      theme(legend.position = 'none',
            panel.background = element_rect(fill = "#F3F6F6"),
            panel.border = element_rect(linewidth = 1.2),
            panel.grid.major = element_line(colour = "#DEE2E4",
                                            linewidth = 1.0,
                                            linetype = "dashed"),
            plot.title = element_text(hjust = 0.5,
                                      size = 14,
                                      colour = "darkred",
                                      face = "bold"),
            axis.title.y = element_text(size=12,
                                        colour = "darkred",
                                        face = "bold"),
            axis.text.x =element_text(size=12,
                                      angle = 45,
                                      hjust = 1,
                                      #  colour = "black",
                                      face = "bold"),
            axis.text.y = element_text(size=10,
                                       #  colour = "black",
                                       face = "bold"),
            axis.title.x = element_blank()) + 
      stat_compare_means()
    
  }  else {
    
    return(NULL)
    
  }
  
  plots <- list()
  plots[[1]] <- p1
  
  return(patchwork:::wrap_plots(plots))
  
}

dbGIST_Proteomics_boxplot_CD34 <- function(ID, DB = Protemics_list){
  
  # ID = "P4HA1"
  # DB = Protemics_list
  # 只有数据集1和2有这个信息，其中2需要调用4
  
  p1 <- NULL
  
  if(ID %in% rownames(DB[[3]]$Matrix)){
    
    p1_table <- data.frame(ID = as.numeric(DB[[3]]$Matrix[which(rownames(DB[[3]]$Matrix) == ID),]),
                           Clinical = DB[[3]]$Clinical$CD34[match(colnames(DB[[3]]$Matrix),
                                                                   DB[[3]]$Clinical$Sample)])
    
    p1_table$Clinical <- factor(p1_table$Clinical,
                                levels = c("Positive","Negative"))
    
    p1_table <- na.omit(p1_table)
    
    p1 <- ggplot(p1_table,aes(Clinical,ID,fill=Clinical))+
      geom_boxplot(outlier.colour = NA,alpha = 0.4,notch = F,size = 0.5)+
      geom_jitter(shape = 21,size=2,width = 0.2) + 
      geom_violin(position = position_dodge(width = .75), 
                  size = NA,alpha = 0.4,trim = T) + 
      scale_fill_lancet() + 
      theme_bw() + 
      xlab("Risk") +
      ylab(ID)+ 
      ggtitle(DB[[2]]$ID) +
      theme(legend.position = 'none',
            panel.background = element_rect(fill = "#F3F6F6"),
            panel.border = element_rect(linewidth = 1.2),
            panel.grid.major = element_line(colour = "#DEE2E4",
                                            linewidth = 1.0,
                                            linetype = "dashed"),
            plot.title = element_text(hjust = 0.5,
                                      size = 14,
                                      colour = "darkred",
                                      face = "bold"),
            axis.title.y = element_text(size=12,
                                        colour = "darkred",
                                        face = "bold"),
            axis.text.x =element_text(size=12,
                                      angle = 45,
                                      hjust = 1,
                                      #  colour = "black",
                                      face = "bold"),
            axis.text.y = element_text(size=10,
                                       #  colour = "black",
                                       face = "bold"),
            axis.title.x = element_blank()) + 
      stat_compare_means()
    
  }  else {
    
    return(NULL)
    
  }
  
  plots <- list()
  plots[[1]] <- p1
  
  return(patchwork:::wrap_plots(plots))
  
}

dbGIST_Proteomics_boxplot_Mutation <- function(ID, DB = Protemics_list){
  
  # ID = "P4HA1"
  # DB = Protemics_list
  # 只有数据集1和2有这个信息，其中2需要调用4
  
  p1 <- NULL
  
  if(ID %in% rownames(DB[[2]]$Matrix)){
    
    p1_table <- data.frame(ID = as.numeric(DB[[2]]$Matrix[which(rownames(DB[[2]]$Matrix) == ID),]),
                           Clinical = DB[[2]]$Clinical$Mutation[match(colnames(DB[[2]]$Matrix),
                                                                 DB[[2]]$Clinical$Sample.ID)])
    
    p1_table <- na.omit(p1_table)
    
    p1 <- ggplot(p1_table,aes(Clinical,ID,fill=Clinical))+
      geom_boxplot(outlier.colour = NA,alpha = 0.4,notch = F,size = 0.5)+
      geom_jitter(shape = 21,size=2,width = 0.2) + 
      geom_violin(position = position_dodge(width = .75), 
                  size = NA,alpha = 0.4,trim = T) + 
      scale_fill_lancet() + 
      theme_bw() + 
      xlab("Risk") +
      ylab(ID)+ 
      ggtitle(DB[[2]]$ID) +
      theme(legend.position = 'none',
            panel.background = element_rect(fill = "#F3F6F6"),
            panel.border = element_rect(linewidth = 1.2),
            panel.grid.major = element_line(colour = "#DEE2E4",
                                            linewidth = 1.0,
                                            linetype = "dashed"),
            plot.title = element_text(hjust = 0.5,
                                      size = 14,
                                      colour = "darkred",
                                      face = "bold"),
            axis.title.y = element_text(size=12,
                                        colour = "darkred",
                                        face = "bold"),
            axis.text.x =element_text(size=12,
                                      angle = 45,
                                      hjust = 1,
                                      #  colour = "black",
                                      face = "bold"),
            axis.text.y = element_text(size=10,
                                       #  colour = "black",
                                       face = "bold"),
            axis.title.x = element_blank()) + 
      stat_compare_means()
    
  }  else {
    
    return(NULL)
    
  }
  
  plots <- list()
  plots[[1]] <- p1
  
  return(patchwork:::wrap_plots(plots))
  
}

#########    Function module 2:  分子相关性模块   ##########   

dbGIST_Proteomics_cor_ID <- function(ID, ID2,DB = Protemics_list){
  
  # ID = "P4HA1"
  # 
  # ID2 = "MCM7"

  
  p1 <- NULL
  p2 <- NULL 
  
  if(((ID %in% rownames(DB[[2]]$Matrix)) & 
     (ID2 %in% rownames(DB[[2]]$Matrix))) |
     ((ID %in% rownames(DB[[3]]$Matrix)) & 
      (ID2 %in% rownames(DB[[3]]$Matrix)))){
    
    if(ID %in% rownames(DB[[2]]$Matrix) & 
       ID2 %in% rownames(DB[[2]]$Matrix)){
      
      p1_table <- data.frame(ID = as.numeric(DB[[2]]$Matrix[match(ID,rownames(DB[[2]]$Matrix)),]),
                             ID2 = as.numeric(DB[[2]]$Matrix[match(ID2,rownames(DB[[2]]$Matrix)),]))

      p1_table <- na.omit(p1_table)

      # 检查数据是否足够进行分析
      if(nrow(p1_table) < 3) {
        cat("Warning: Insufficient data points for correlation analysis in dataset 2\n")
        p1 <- NULL
      } else {
      
      #  my_comparisons <- list(c("Hypo-ICD","Hyper-ICD"))
      
      P1_R <- cor.test(p1_table$ID,p1_table$ID2)
      
      ggsub <- str_c("r=",round(as.numeric(P1_R$estimate),2),",p=",round(as.numeric(P1_R$p.value),2))
      
      p1 <- ggplot(p1_table,
                   aes(ID,ID2)) + 
        geom_point(size = 3,
                   alpha = 0.6) + 
        #    scale_color_manual(values = c("#E13220","#3450A8")) + 
        theme_bw() + #ylim(c(-3,4.5)) + xlim(c(0,8)) +
        stat_smooth(method='lm',formula = y~x) +
        guides(fill = "none")+
        xlab(str_c(ID," (Protein level)")) +
        ylab(str_c(ID2," (Protein level)"))+ 
        ggtitle(DB[[2]]$ID)+
        annotate("text",
                 x = min(p1_table$ID) + 0.7*(max(p1_table$ID) - min(p1_table$ID)),
                 y = min(p1_table$ID2) + 0.85*(max(p1_table$ID2) - min(p1_table$ID2)),
                 label = ggsub,
                 #   size = 5,
                 color = "darkred") +
        #scale_colour_manual(values = c("#E13220","#3450A8"))+
        theme(legend.position = 'none',
              panel.background = element_rect(fill = "#F3F6F6"),
              panel.border = element_rect(linewidth = 1.2),
              panel.grid.major = element_line(colour = "#DEE2E4",
                                              linewidth = 1.0,
                                              linetype = "dashed"),
              plot.title = element_text(hjust = 0.5,
                                        size = 14,
                                        colour = "darkred",
                                        face = "bold"),
              axis.title.y = element_text(size=12,
                                          colour = "darkred",
                                          face = "bold"),
              axis.title.x = element_text(size=12,
                                          colour = "darkred",
                                          face = "bold"),
              axis.text.x =element_text(size=12,
                                        angle = 45,
                                        hjust = 1,
                                        #  colour = "black",
                                        face = "bold"),
              axis.text.y = element_text(size=10,
                                         #  colour = "black",
                                         face = "bold"),
              legend.text = element_text(size = 11),
              legend.title = element_text(size = 13),
              plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), 'cm'))
      }
    }
    
    if(ID %in% rownames(DB[[3]]$Matrix) & 
       ID2 %in% rownames(DB[[3]]$Matrix)){
      
      p1_table <- data.frame(ID = as.numeric(DB[[3]]$Matrix[match(ID,rownames(DB[[3]]$Matrix)),]),
                             ID2 = as.numeric(DB[[3]]$Matrix[match(ID2,rownames(DB[[3]]$Matrix)),]))

      p1_table <- na.omit(p1_table)

      # 检查数据是否足够进行分析
      if(nrow(p1_table) < 3) {
        cat("Warning: Insufficient data points for correlation analysis in dataset 3\n")
        p2 <- NULL
      } else {
      
      P1_R <- cor.test(p1_table$ID,p1_table$ID2)
      
      ggsub <- str_c("r=",round(as.numeric(P1_R$estimate),2),",p=",round(as.numeric(P1_R$p.value),2))
      
      p2 <- ggplot(p1_table,
                   aes(ID,ID2)) + 
        geom_point(size = 3,
                   alpha = 0.6) + 
        #    scale_color_manual(values = c("#E13220","#3450A8")) + 
        theme_bw() + #ylim(c(-3,4.5)) + xlim(c(0,8)) +
        stat_smooth(method='lm',formula = y~x) +
        guides(fill = "none")+
        xlab(str_c(ID," (Protein level)")) +
        ylab(str_c(ID2," (Protein level)"))+ 
        ggtitle(DB[[3]]$ID)+
        annotate("text",
                 x = min(p1_table$ID) + 0.7*(max(p1_table$ID) - min(p1_table$ID)),
                 y = min(p1_table$ID2) + 0.85*(max(p1_table$ID2) - min(p1_table$ID2)),
                 label = ggsub,
                 #   size = 5,
                 color = "darkred") +
        #scale_colour_manual(values = c("#E13220","#3450A8"))+
        theme(legend.position = 'none',
              panel.background = element_rect(fill = "#F3F6F6"),
              panel.border = element_rect(linewidth = 1.2),
              panel.grid.major = element_line(colour = "#DEE2E4",
                                              linewidth = 1.0,
                                              linetype = "dashed"),
              plot.title = element_text(hjust = 0.5,
                                        size = 14,
                                        colour = "darkred",
                                        face = "bold"),
              axis.title.y = element_text(size=12,
                                          colour = "darkred",
                                          face = "bold"),
              axis.title.x = element_text(size=12,
                                          colour = "darkred",
                                          face = "bold"),
              axis.text.x =element_text(size=12,
                                        angle = 45,
                                        hjust = 1,
                                        #  colour = "black",
                                        face = "bold"),
              axis.text.y = element_text(size=10,
                                         #  colour = "black",
                                         face = "bold"),
              legend.text = element_text(size = 11),
              legend.title = element_text(size = 13),
              plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), 'cm'))
      }
    }
    
  } else {
    
    return(NULL)
    
  }
  
  # 创建图表列表，只包含非空图表
  plots <- list()
  if(!is.null(p1)) plots[[length(plots) + 1]] <- p1
  if(!is.null(p2)) plots[[length(plots) + 1]] <- p2

  # 如果有图表，使用patchwork组合
  if(length(plots) > 0) {
    if(length(plots) == 1) {
      return(plots[[1]])
    } else {
      return(patchwork::wrap_plots(plots, ncol = 2))
    }
  } else {
    return(NULL)
  }
  
}

#########    Function module 3:  功能相关性模块   ##########

# 1. 使用相关性分析，找出共表达的基因，然后看基因集富集所在的通路
# 2. 计算通路得分，然后看通路与基因之间的相关性（pheatmap），提前进行计算

# 加载Hallmark通路数据
# Hallmarker_list <- readRDS("Proteomics_hallmark_list.rds")

dbGIST_Proteomics_Protein_Pathway_cor <- function(ID, ID2, DB = Protemics_list, DB2 = NULL){

  # ID = "P4HA1"
  # ID2 = "HALLMARK_HYPOXIA"

  # 如果没有提供DB2，尝试加载默认的Hallmark数据
  if(is.null(DB2)) {
    if(file.exists("Proteomics_hallmark_list.rds")) {
      DB2 <- readRDS("Proteomics_hallmark_list.rds")
    } else {
      stop("Hallmark数据文件不存在，请提供DB2参数或确保Proteomics_hallmark_list.rds文件存在")
    }
  }

  p1 <- NULL
  p2 <- NULL

  if(((ID %in% rownames(DB[[2]]$Matrix)) &
      (ID2 %in% rownames(DB2[[2]]))) |
     ((ID %in% rownames(DB[[3]]$Matrix)) &
      (ID2 %in% rownames(DB2[[3]])))){

    if(ID %in% rownames(DB[[2]]$Matrix) &
       ID2 %in% rownames(DB2[[2]])){

      p1_table <- data.frame(ID = as.numeric(DB[[2]]$Matrix[match(ID,rownames(DB[[2]]$Matrix)),]),
                             ID2 = as.numeric(DB2[[2]][match(ID2,rownames(DB2[[2]])),]))

      p1_table <- na.omit(p1_table)

      P1_R <- cor.test(p1_table$ID,p1_table$ID2)

      ggsub <- str_c("r=",round(as.numeric(P1_R$estimate),2),",p=",round(as.numeric(P1_R$p.value),2))

      p1 <- ggplot(p1_table,
                   aes(ID,ID2)) +
        geom_point(size = 3,
                   alpha = 0.6) +
        theme_bw() +
        stat_smooth(method='lm',formula = y~x) +
        guides(fill = "none")+
        xlab(str_c(ID," (Protein level)")) +
        ylab(ID2)+
        ggtitle(DB[[2]]$ID)+
        annotate("text",
                 x = min(p1_table$ID) + 0.7*(max(p1_table$ID) - min(p1_table$ID)),
                 y = min(p1_table$ID2) + 0.85*(max(p1_table$ID2) - min(p1_table$ID2)),
                 label = ggsub,
                 color = "darkred") +
        theme(legend.position = 'none',
              panel.background = element_rect(fill = "#F3F6F6"),
              panel.border = element_rect(linewidth = 1.2),
              panel.grid.major = element_line(colour = "#DEE2E4",
                                              linewidth = 1.0,
                                              linetype = "dashed"),
              plot.title = element_text(hjust = 0.5,
                                        size = 14,
                                        colour = "darkred",
                                        face = "bold"),
              axis.title.y = element_text(size=12,
                                          colour = "darkred",
                                          face = "bold"),
              axis.title.x = element_text(size=12,
                                          colour = "darkred",
                                          face = "bold"),
              axis.text.x =element_text(size=12,
                                        angle = 45,
                                        hjust = 1,
                                        face = "bold"),
              axis.text.y = element_text(size=10,
                                         face = "bold"),
              legend.text = element_text(size = 11),
              legend.title = element_text(size = 13),
              plot.margin = unit(c(0.4,0.4,0.4,0.4),'cm'))
    }

    if(ID %in% rownames(DB[[3]]$Matrix) &
       ID2 %in% rownames(DB2[[3]])){

      p1_table <- data.frame(ID = as.numeric(DB[[3]]$Matrix[match(ID,rownames(DB[[3]]$Matrix)),]),
                             ID2 = as.numeric(DB2[[3]][match(ID2,rownames(DB2[[3]])),]))

      p1_table <- na.omit(p1_table)

      P1_R <- cor.test(p1_table$ID,p1_table$ID2)

      ggsub <- str_c("r=",round(as.numeric(P1_R$estimate),2),",p=",round(as.numeric(P1_R$p.value),2))

      p2 <- ggplot(p1_table,
                   aes(ID,ID2)) +
        geom_point(size = 3,
                   alpha = 0.6) +
        theme_bw() +
        stat_smooth(method='lm',formula = y~x) +
        guides(fill = "none")+
        xlab(str_c(ID," (Protein level)")) +
        ylab(ID2)+
        ggtitle(DB[[3]]$ID)+
        annotate("text",
                 x = min(p1_table$ID) + 0.7*(max(p1_table$ID) - min(p1_table$ID)),
                 y = min(p1_table$ID2) + 0.85*(max(p1_table$ID2) - min(p1_table$ID2)),
                 label = ggsub,
                 color = "darkred") +
        theme(legend.position = 'none',
              panel.background = element_rect(fill = "#F3F6F6"),
              panel.border = element_rect(linewidth = 1.2),
              panel.grid.major = element_line(colour = "#DEE2E4",
                                              linewidth = 1.0,
                                              linetype = "dashed"),
              plot.title = element_text(hjust = 0.5,
                                        size = 14,
                                        colour = "darkred",
                                        face = "bold"),
              axis.title.y = element_text(size=12,
                                          colour = "darkred",
                                          face = "bold"),
              axis.title.x = element_text(size=12,
                                          colour = "darkred",
                                          face = "bold"),
              axis.text.x =element_text(size=12,
                                        angle = 45,
                                        hjust = 1,
                                        face = "bold"),
              axis.text.y = element_text(size=10,
                                         face = "bold"),
              legend.text = element_text(size = 11),
              legend.title = element_text(size = 13),
              plot.margin = unit(c(0.4,0.4,0.4,0.4),'cm'))
    }

  } else {

    return(NULL)

  }

  plots <- list()
  plots[[1]] <- p1
  plots[[2]] <- p2

  return(patchwork:::wrap_plots(plots))

}



#########   Function module 4:  伊马替尼耐药相关性模块   ##########  

dbGIST_Proteomics_boxplot_IM.Response <- function(ID, DB = Protemics_list){
  
  # ID = "P4HA1"
  # DB = Protemics_list
  # 只有数据集1和2有这个信息，其中2需要调用4
  
  p1 <- NULL
  
  if(ID %in% rownames(DB[[2]]$Matrix)){
    
    p1_table <- data.frame(ID = as.numeric(DB[[2]]$Matrix[which(rownames(DB[[2]]$Matrix) == ID),]),
                           Clinical = DB[[2]]$Clinical$IM.Response[match(colnames(DB[[2]]$Matrix),
                                                                         DB[[2]]$Clinical$Sample.ID)])
    
    p1_table <- na.omit(p1_table)
    
    #  my_comparisons <- list(c("Hypo-ICD","Hyper-ICD"))
    
    p1 <- ggplot(p1_table,aes(Clinical,ID,fill=Clinical))+
      geom_boxplot(outlier.colour = NA,notch = F,size = 0.4)+
      geom_jitter(shape = 21,size=2,width = 0.2) + 
      geom_violin(position = position_dodge(width = .75), 
                  size = 0.4,alpha = 0.4,trim = T) + 
      scale_fill_lancet() + 
      theme_bw() + 
      xlab("Risk") +
      ylab(str_c(ID," (Protein level)"))+ 
      ggtitle(DB[[2]]$ID)+
      theme(legend.position = 'none',
            panel.background = element_rect(fill = "#F3F6F6"),
            panel.border = element_rect(linewidth = 1.2),
            panel.grid.major = element_line(colour = "#DEE2E4",
                                            linewidth = 1.0,
                                            linetype = "dashed"),
            plot.title = element_text(hjust = 0.5,
                                      size = 14,
                                      colour = "darkred",
                                      face = "bold"),
            axis.title.y = element_text(size=12,
                                        colour = "darkred",
                                        face = "bold"),
            axis.text.x =element_text(size=12,
                                      angle = 45,
                                      hjust = 1,
                                      #  colour = "black",
                                      face = "bold"),
            axis.text.y = element_text(size=10,
                                       #  colour = "black",
                                       face = "bold"),
            axis.title.x = element_blank())+
      stat_compare_means()
    
    p1_roc <- roc(p1_table$Clinical,p1_table$ID)
    tt <- p1_roc
    tp <- tt$sensitivities%>%as.data.frame()
    fp <- (1-tt$specificities)%>%as.data.frame()
    dd <- data.frame(fp = fp,tp = tp);colnames(dd) <- c("fp","tp")
    dd <- dd %>% arrange(desc(fp), tp)
    
    ggsub <- str_c("AUC: ",round(p1_roc$auc,2))
    
    p2 <- ggplot(dd,aes(fp,tp))+
      geom_line(linewidth=1)+
      labs(x='1-Specificity',y='Sensitivity',color=NULL) +
      theme_bw(base_rect_size = 1.5)+
      geom_abline(slope = 1,color='grey70')+
      ggtitle(DB[[2]]$ID)+
      annotate("text",
               x = 0.7,
               y = 0.2,
               label = ggsub,
              # size = 6,
               color = "darkred")+
      theme(panel.background = element_rect(fill = "#F3F6F6"),
            panel.border = element_rect(linewidth = 1.2),
            panel.grid.major = element_line(colour = "#DEE2E4",
                                            linewidth = 1.0,
                                            linetype = "dashed"),
            plot.title = element_text(hjust = 0.5,
                                      size = 14,
                                      colour = "darkred",
                                      face = "bold"),
            axis.title.x = element_text(size=12,
                                        colour = "darkred",
                                        face = "bold"),
            axis.title.y = element_text(size=12,
                                        colour = "darkred",
                                        face = "bold"),
            axis.text.x =element_text(size=12,
                                      angle = 45,
                                      hjust = 1,
                                      #  colour = "black",
                                      face = "bold"),
            axis.text.y = element_text(size=12,
                                       #  colour = "black",
                                       face = "bold"),
            legend.text = element_text(size=12),
            legend.position.inside = c(0.995,0.012),
            legend.justification = c(1,0))+
      scale_color_nejm()+
      scale_x_continuous(expand = c(0.01,0.01))+
      scale_y_continuous(expand = c(0.01,0.01))
    
    p1 <- p1 + p2
    
  }  else {
    
    return(NULL)
    
  }
  
  plots <- list()
  plots[[1]] <- p1
  
  return(patchwork:::wrap_plots(plots))
  
}

########### 使用1：临床性状模块使用 ############

ID = "P4HA1"

dbGIST_Proteomics_boxplot_TvsN(ID = ID,DB = Protemics_list)

dbGIST_Proteomics_boxplot_Risk(ID = ID,DB = Protemics_list)

dbGIST_Proteomics_boxplot_Gender(ID = ID,DB = Protemics_list)

dbGIST_Proteomics_boxplot_Age(ID = ID,DB = Protemics_list)

dbGIST_Proteomics_boxplot_Tumor.size(ID = ID,DB = Protemics_list)

dbGIST_Proteomics_boxplot_Mitotic.count(ID = ID,DB = Protemics_list)

dbGIST_Proteomics_boxplot_Location(ID = ID,DB = Protemics_list)

dbGIST_Proteomics_boxplot_WHO(ID = ID,DB = Protemics_list)

dbGIST_Proteomics_boxplot_Ki.67(ID = ID,DB = Protemics_list)

dbGIST_Proteomics_boxplot_CD34(ID = ID,DB = Protemics_list)

dbGIST_Proteomics_boxplot_Mutation(ID = ID,DB = Protemics_list)

################ 使用2：分子间相关性模块 ########################

ID = "P4HB"
ID2 = "P4HA1"

dbGIST_Proteomics_cor_ID(ID,ID2,DB = Protemics_list)

################ 使用3：分子通路相关性模块 ########################



################ 使用3：分子通路相关性模块 ########################

# 该通路相关的
# ID <- "FTO"
# dbGIST_Proteomics_Protein_Pathway_cor(ID = ID,
#                                       ID2 = "HALLMARK_HYPOXIA",
#                                       DB = Protemics_list)

################ 使用4：伊马替尼药物预测模块 ###################

dbGIST_Proteomics_boxplot_IM.Response(ID = "FN1",DB = Protemics_list)

##########    Function module 5:  生存分析相关模块   ############

KM_function <- function(Protemics2_Clinical = Protemics_list[[2]]$Clinical,
                        CutOff_point = "Auto",
                        Survival_type = "OS",
                        ID = ID) {

  Protemics2_Clinical$Expr <- as.numeric(Protemics_list[[2]]$Matrix[which(rownames(Protemics_list[[2]]$Matrix)
                                                                          == ID),])

  # 参数校验
  if (is.null(Protemics2_Clinical)) stop("输入数据不能为NULL")
  required_cols <- if (Survival_type == "OS") c("OS.time", "OS", "Expr") else c("PFS.time", "PFS", "Expr")
  if (!all(required_cols %in% colnames(Protemics2_Clinical))) {
    stop(paste("输入数据缺少必需列：", paste(required_cols, collapse = ", ")))
  }

  # 动态设置时间和事件列
  time_col <- ifelse(Survival_type == "OS", "OS.time", "PFS.time")
  event_col <- ifelse(Survival_type == "OS", "OS", "PFS")

  # 计算cutpoint
  res.cut <- surv_cutpoint(Protemics2_Clinical,
                           time = time_col,
                           event = event_col,
                           variables = "Expr")

  if (CutOff_point == "Median") {
    res.cut$cutpoint$cutpoint <- median(Protemics2_Clinical$Expr, na.rm = TRUE)
  } else if (CutOff_point == "Mean") {
    res.cut$cutpoint$cutpoint <- mean(Protemics2_Clinical$Expr, na.rm = TRUE)
  }

  # 分类并拟合生存曲线
  res.cat <- surv_categorize(res.cut)
  formula <- as.formula(paste("Surv(", time_col, ", ", event_col, ") ~ Expr"))

  res.cat$Expr <- factor(res.cat$Expr,levels = c("low","high"))

  fit <- surv_fit(formula, data = res.cat)

  # 绘制图形
  plot <- ggsurvplot(fit,
                     pval = TRUE, conf.int = FALSE,
                     risk.table = TRUE,
                     risk.table.col = "strata",
                     palette ="aaas",
                     linetype = "strata",
                     surv.median.line = "hv",
                     ggtheme = theme_bw(),
                     xlab = "Time (Month)")

  # 返回结果
  return(plot)
}

################ 使用5：生存分析相关模块  ######################

# ID <- "FTO"
# result <- KM_function(Protemics2_Clinical = Protemics_list[[2]]$Clinical,
#                       CutOff_point = "Auto",
#                       Survival_type = "PFS",
#                       ID = ID)
# result

# ==== 新增辅助函数：提取生存分析数据 ====
extract_survival_data <- function(gene_name, survival_type, cutoff_point) {
  tryCatch({
    # 获取临床数据
    clinical_data <- Protemics_list[[2]]$Clinical

    # 检查基因是否存在
    if(!gene_name %in% rownames(Protemics_list[[2]]$Matrix)) {
      return(NULL)
    }

    # 获取基因表达数据
    gene_expr <- as.numeric(Protemics_list[[2]]$Matrix[gene_name, ])

    # 添加表达数据到临床数据
    clinical_data$Gene_Expression <- gene_expr
    clinical_data$Gene_Name <- gene_name

    # 根据生存类型选择相应的列
    if(survival_type == "OS") {
      if(all(c("OS.time", "OS") %in% colnames(clinical_data))) {
        clinical_data$Survival_Time <- clinical_data$OS.time
        clinical_data$Event <- clinical_data$OS
      } else {
        return(NULL)
      }
    } else if(survival_type == "PFS") {
      if(all(c("PFS.time", "PFS") %in% colnames(clinical_data))) {
        clinical_data$Survival_Time <- clinical_data$PFS.time
        clinical_data$Event <- clinical_data$PFS
      } else {
        return(NULL)
      }
    }

    # 根据cutoff方法分组
    if(cutoff_point == "Median") {
      cutoff_value <- median(gene_expr, na.rm = TRUE)
    } else if(cutoff_point == "Mean") {
      cutoff_value <- mean(gene_expr, na.rm = TRUE)
    } else {
      # Auto方法，使用最优cutoff
      cutoff_value <- median(gene_expr, na.rm = TRUE)
    }

    clinical_data$Group <- ifelse(gene_expr > cutoff_value, "High", "Low")

    # 移除缺失值
    clinical_data <- clinical_data[complete.cases(clinical_data[c("Survival_Time", "Event", "Gene_Expression")]), ]

    return(clinical_data)

  }, error = function(e) {
    cat("Extract survival data error:", e$message, "\n")
    return(NULL)
  })
}
