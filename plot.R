pe = c(d1,d2,d3)
#pe = c(d1.p,d2.p,d3.p)
M = rep("",30000) # "BCD","PEM","NAM","LRM","RVE"
S = rep("S",30000)  # S1,S2,...
par = c(rep("p1", 10000), rep("p2", 10000), rep("p3", 10000))
df = data.frame(pe,M,S,par)

write.csv(df, file="MiSi.csv", row.names=FALSE)

M1S4 = read.csv(file = "M1S4.csv", header = T, sep = ",")
M2S4 = read.csv(file = "M2S4.csv", header = T, sep = ",")
M3S4 = read.csv(file = "M3S4.csv", header = T, sep = ",")
M4S4 = read.csv(file = "M4S4.csv", header = T, sep = ",")
M1S3 = read.csv(file = "M1S3.csv", header = T, sep = ",")
M2S3 = read.csv(file = "M2S3.csv", header = T, sep = ",")
M3S3 = read.csv(file = "M3S3.csv", header = T, sep = ",")
M4S3 = read.csv(file = "M4S3.csv", header = T, sep = ",")
M1S2 = read.csv(file = "M1S2.csv", header = T, sep = ",")
M2S2 = read.csv(file = "M2S2.csv", header = T, sep = ",")
M3S2 = read.csv(file = "M3S2.csv", header = T, sep = ",")
M4S2 = read.csv(file = "M4S2.csv", header = T, sep = ",")
M1S1 = read.csv(file = "M1S1.csv", header = T, sep = ",")
M2S1 = read.csv(file = "M2S1.csv", header = T, sep = ",")
M3S1 = read.csv(file = "M3S1.csv", header = T, sep = ",")
M4S1 = read.csv(file = "M4S1.csv", header = T, sep = ",")

M5S1 = read.csv(file = "M5S1.csv", header = T, sep = ",")
M5S2 = read.csv(file = "M5S2.csv", header = T, sep = ",")
M5S3 = read.csv(file = "M5S3.csv", header = T, sep = ",")
M5S4 = read.csv(file = "M5S4.csv", header = T, sep = ",")

df = rbind(M1S1, M2S1, M3S1, M4S1, M5S1, 
           M1S2, M2S2, M3S2, M4S2, M5S2, 
           M1S3, M2S3, M3S3, M4S3, M5S3,
           M1S4, M2S4, M3S4, M4S4, M5S4)

df$M = factor(df$M, levels=c("BCD","PEM","NAM","LRM","RVE"))

df1 = df[which(df$par=="p1"),]
df2 = df[which(df$par=="p2"),]
df3 = df[which(df$par=="p3"),]


library(plotly)
library(ggplot2)

p1 <- ggplot(df1, aes(x=S, y=pe, fill=M)) + geom_boxplot() +
  labs(title=expression(paste('p = 0.2')), fill = "method",
       x ="Scenario", y = "Incidence Probability") + ylim(0,0.35) + # theme_classic() +
  geom_hline(yintercept=0.2, linetype="dashed", color = "black") 

p2 <- ggplot(df2, aes(x=S, y=pe, fill=M)) + geom_boxplot() +
  labs(title=expression(paste('p = 0.05')), fill = "method",
       x ="Scenario", y = "Incidence Probability") + ylim(0,0.15) +
  geom_hline(yintercept=0.05, linetype="dashed", color = "black")

p3 <- ggplot(df3, aes(x=S, y=pe, fill=M)) + geom_boxplot() +
  labs(title=expression(paste('p = 0.01')), fill = "method",
       x ="Scenario", y = "Incidence Probability") + ylim(0,0.08) +
  geom_hline(yintercept=0.01, linetype="dashed", color = "black")

#pdf("fig1PE.pdf", height = 5, width = 15)
setEPS()
postscript("fig1EPS.eps", height = 5, width = 15)
library(ggpubr)
ggarrange(p1, p2, p3,ncol=3, nrow = 1, common.legend = TRUE, legend = "right") 
dev.off()


df$d = ifelse(df$par=="p1", "d1", ifelse(df$par=="p2", "d2", "d3"))
df1 = df[which(df$S=="S1"),]
df2 = df[which(df$S=="S2"),]
df3 = df[which(df$S=="S3"),]
df4 = df[which(df$S=="S4"),]


library(plotly)
library(ggplot2)

p1 <- ggplot(df1, aes(x=d, y=pe, fill=M)) + geom_boxplot() +
  labs(title="Scenario 1: 0%", fill = "method",
       x ="Drug", y = "Incidence Rate") + ylim(0,0.35) + # theme_classic() +
  geom_hline(yintercept=c(0.05,0.01,0.2), linetype="dashed", color = "black") 

p2 <- ggplot(df2, aes(x=d, y=pe, fill=M)) + geom_boxplot() +
  labs(title="Scenario 2: 40%", fill = "method",
       x ="Drug", y = "Incidence Rate") + ylim(0,0.35) +
  geom_hline(yintercept=c(0.05,0.01,0.2), linetype="dashed", color = "black")

p3 <- ggplot(df3, aes(x=d, y=pe, fill=M)) + geom_boxplot() +
  labs(title="Scenario 3: 80%", fill = "method",
       x ="Drug", y = "Incidence Rate") + ylim(0,0.35) +
  geom_hline(yintercept=c(0.05,0.01,0.2), linetype="dashed", color = "black")

p4 <- ggplot(df4, aes(x=d, y=pe, fill=M)) + geom_boxplot() +
  labs(title="Scenario 4: 0%/40%/80%", fill = "method",
       x ="Drug", y = "Incidence Rate") + ylim(0,0.35) +
  geom_hline(yintercept=c(0.05,0.01,0.2), linetype="dashed", color = "black")

setEPS()
postscript("fig1pe.eps", height = 10, width = 10)
library(ggpubr)
ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2, common.legend = TRUE, legend = "right") 
dev.off()


# update to date: 12/28/2021
CP = c(0.947, 0.951, 0.929, 0.946, 0.875, # S1: d1
       0.947, 0.960, 0.787, 0.954, 0.678, # S1: d2
       0.948, 0.976, 0.404, 0.962, 0.170, # S1: d3
       0.950, 0.732, 0.630, 0.732, 0.282,
       0.951, 0.785, 0.471, 0.724, 0.154,
       0.951, 0.822, 0.218, 0.706, 0.022, # S2
       0.947, 0.627, 0.530, 0.627, 0.020,
       0.959, 0.638, 0.474, 0.638, 0.002,
       0.985, 0.735, 0.160, 0.471, 0.000, # S3
       0.945, 0.869, 0.819, 0.860, 0.855,
       0.951, 0.693, 0.387, 0.629, 0.146,
       0.984, 0.700, 0.150, 0.441, 0.001) # S4
M = rep(c("BCD","PEM","NAM","LRM","RVE"),12)
S = c(rep("S1", 15), rep("S2", 15),
      rep("S3", 15), rep("S4", 15))
p = rep(c(rep("p1", 5), rep("p2", 5),
          rep("p3", 5)), 4)
df = data.frame(CP, M, S, p)

#write.csv(df, file="fig2.csv", row.names=FALSE)

df1 = df[which(df$p=="p1"),]
df2 = df[which(df$p=="p2"),]
df3 = df[which(df$p=="p3"),]

# Figure 2: CPs
level.order = c("BMCD","PEM","NAM","LRM","RVE")

library(ggplot2)
p1 <- ggplot(df1, aes(x = S, y = CP)) +
  geom_bar(aes(fill = factor(M, level = level.order)), stat = "identity", 
           position = position_dodge(0.7),width = 0.5) + 
  labs(title=expression(paste('p = 0.2')), fill = "method",
       x ="Scenario", y = "Coverage Probability") +
  geom_hline(yintercept=0.95, linetype="dashed", color = "black") 
#p1
p2 <- ggplot(df2, aes(x = S, y = CP)) +
  geom_bar(aes(fill = factor(M,level = level.order)), stat = "identity", 
           position = position_dodge(0.7),width = 0.5) + 
  labs(title=expression(paste('p = 0.05')), fill = "method",
       x ="Scenario", y = "Coverage Probability") +
  geom_hline(yintercept=0.95, linetype="dashed", color = "black")
#p2
p3 <- ggplot(df3, aes(x = S, y = CP)) +
  geom_bar(aes(fill = factor(M,level = level.order)), stat = "identity", 
           position = position_dodge(0.7),width = 0.5) + 
  labs(title=expression(paste('p = 0.01')), fill = "method",
       x ="Scenario", y = "Coverage Probability") +
  geom_hline(yintercept=0.95, linetype="dashed", color = "black")

pdf("fig2CP.pdf", height = 5, width = 15)
library(ggpubr)
ggarrange(p1, p2, p3, ncol=3, nrow = 1, common.legend = TRUE, legend = "right")
dev.off()

setEPS()
postscript("fig2.eps", height = 5, width = 15)
