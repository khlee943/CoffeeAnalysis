rm(list= ls())

library(ggplot2)
install.packages("coin")
install.packages("conover.test")
install.packages("dunn.test")
install.packages("FSA")

library(tidyverse)
#library(Rcpp)
library(dplyr)
library(tidyr)
library(ggbiplot)
library(ggpubr)
library(RColorBrewer)
library(coin)
library(conover.test)
library(dunn.test)
library(FSA)  

mytheme <-   theme(panel.grid.minor = element_blank(), #gets rid of grey and lines in the middle
                   panel.grid.major = element_blank(), #gets rid of grey and lines in the middle
                   panel.background = element_rect(fill="white"),#gets rid of grey and lines in the middle
                   panel.border = element_blank(), #gets rid of square going around the entire graph
                   axis.line.x = element_line(color = 'black', linewidth = 0.7),#sets the axis line size
                   axis.line.y = element_line(color = 'black', linewidth = 0.7),#sets the axis line size
                   axis.ticks = element_line(color = 'black', linewidth = 0.7), #sets the tick lines
                   axis.title.x = element_text(size=12, color="black"), #size of x-axis title
                   axis.title.y = element_text(size=12, color="black"), #size of y-axis title
                   axis.text.x = element_text(size=12, color="black", hjust = 0.5), #size of x-axis text
                   axis.text.y = element_text(size=12, color="black")) #size of y-axis text


csv_file <- read.csv("C:/Users/aquak/OneDrive/SP2024/Data_of_Coffee_Samples.csv", header = TRUE)
coffee <- csv_file

coffee$Coding <- NULL
coffee[,1] <- NULL
str(coffee)

coffee$TA <- as.numeric(coffee$TA)
coffee$pH <- as.numeric(coffee$pH)
coffee$TPC <- as.numeric(coffee$TPC)
coffee$DPPH <- as.numeric(coffee$DPPH)

coffee$Brew <- as.factor(coffee$Brew)
coffee$Freshness <- as.factor(coffee$Freshness)

#####TA
coffee_TA <- filter(coffee, TA != "--")

hist(coffee_TA$TA)
ggqqplot(coffee_TA$TA)
ta_model <- lm(TA ~ Freshness, data = coffee_TA)
summary(ta_model)
hist(ta_model$residuals) #model is significant p-value < 2.2e-16
ggqqplot(ta_model$residuals) #non-normalized
sigma(ta_model)/mean(coffee_TA$TA) #coefficient of variance - 0.3687515

#summary stats of TAs
fresh_cold_brew <- subset(coffee, Freshness == "F" & Brew == "CB")
fresh_hot_brew <- subset(coffee, Freshness == "F" & Brew == "HB")

spent_cold_brew <- subset(coffee, Freshness == "SCB")
spent_hot_brew <- subset(coffee, Freshness == "SHB")

summary_FCB <- summarise(fresh_cold_brew, Mean_TA = mean(TA, na.rm = TRUE), SD_TA = sd(TA, na.rm = TRUE))
summary_FHB <- summarise(fresh_hot_brew, Mean_TA = mean(TA, na.rm = TRUE), SD_TA = sd(TA, na.rm = TRUE))
summary_SCB <- summarise(spent_cold_brew, Mean_TA = mean(TA, na.rm = TRUE), SD_TA = sd(TA, na.rm = TRUE))
summary_SHB <- summarise(spent_hot_brew, Mean_TA = mean(TA, na.rm = TRUE), SD_TA = sd(TA, na.rm = TRUE))

summary_table <- rbind(summary_FCB, summary_FHB, summary_SCB, summary_SHB)
summary_table$Group <- c("Fresh Cold Brew", "Fresh Hot Brew", "Spent Cold Brew", "Spent Hot Brew")

summary_table_transposed <- t(summary_table[, c("Mean_TA", "SD_TA")])

colnames(summary_table_transposed) <- summary_table$Group
rownames(summary_table_transposed) <- c("Mean_TA", "SD_TA")

summary_table_transposed

#TA comparing hot to cold data
ta_hc_plot <- ggplot(coffee_TA, aes(x = Brew, y = TA, fill = Brew)) +
  geom_boxplot(position = "dodge", color = "black") +
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
  labs(x = "Brew Type", y = "TA_NaOH") +
  theme_minimal()
print(ta_hc_plot)

dunnTest(TA ~ Brew, data = coffee, method = "bonferroni")
#input signif variables into a table for comparisons
table_ta_hc <- tibble::tribble(
  ~group1, ~group2, ~p.adj,
  #"CB", "HB", 0.3670379,
)
print(table_ta_hc)

#TA comparing F, SHB, SCB data
coffee_TA$Brew <- as.factor(coffee$Brew)
coffee_TA$Freshness <- as.factor(coffee$Freshness)
coffee_TA$TA <- as.numeric(coffee_TA$TA)

spent_cold_brew <- subset(coffee, (Freshness == "SCB"))
spent_hot_brew <- subset(coffee, (Freshness == "SHB"))

new_ta_fresh_plot <- ggplot(data = NULL) +
  geom_boxplot(data = fresh_cold_brew, aes(x = "Fresh CB", y = TA, fill = "Fresh CB"), color = "black", position = position_dodge(width = 0.75), width = 0.5) +
  geom_boxplot(data = fresh_hot_brew, aes(x = "Fresh HB", y = TA, fill = "Fresh HB"), color = "black", position = position_dodge(width = 1.25), width = 0.5) +
  geom_boxplot(data = subset(coffee_TA, Freshness == "SCB"), aes(x = "SCB", y = TA, fill = "SCB"), color = "black", position = position_dodge(width = 1.75), width = 0.5) +
  geom_boxplot(data = subset(coffee_TA, Freshness == "SHB"), aes(x = "SHB", y = TA, fill = "SHB"), color = "black", position = position_dodge(width = 2.25), width = 0.5) +
  scale_fill_viridis_d() +
  labs(x = "Freshness and Brew Type", y = "TA_NaOH") +
  theme_minimal() +
  stat_pvalue_manual(table_ta_fresh, label = "p.adj", y.position = 1.2, step.increase = 0.1)

print(new_ta_fresh_plot)

#ALL INfO WITH DOING NEW P-VALUES WITH FHB/FCB
csv_file <- read.csv("C:/Users/aquak/OneDrive/SP2024/Data_of_Coffee_Samples_Freshness.csv", header = TRUE)
coffee_freshness <- csv_file

coffee_freshness$Coding <- NULL
coffee_freshness[,1] <- NULL
str(coffee_freshness)

coffee_freshness$Freshness <- as.factor(coffee$Freshness)
coffee_freshness$TA <- as.numeric(coffee_freshness$TA)


dunnTest(TA ~ New_Freshness, data = coffee_freshness, method = "bonferroni")
table_ta_fresh <- tibble::tribble(
  ~group1, ~group2, ~p.adj,
  "Fresh CB", "SCB", "***",
  "Fresh HB", "SCB", "***",
  "Fresh CB", "SHB", "***",
  "Fresh HB", "SHB", "***",
)

#TA comparing all spent data
spent_C_to_C <- subset(coffee, (Freshness == "SCB") & Brew == "CB")
spent_C_to_H <- subset(coffee, (Freshness == "SHB") & Brew == "CB")
spent_H_to_H <- subset(coffee, (Freshness == "SHB") & Brew == "HB")
spent_H_to_C <- subset(coffee, (Freshness == "SCB") & Brew == "HB")



all_spent <- rbind(
  data.frame(Group = "Cold to Cold", TA = spent_C_to_C$TA),
  data.frame(Group = "Cold to Hot", TA = spent_C_to_H$TA),
  data.frame(Group = "Hot to Hot", TA = spent_H_to_H$TA),
  data.frame(Group = "Hot to Cold", TA = spent_H_to_C$TA)
)

spent_overview <- ggplot(all_spent, aes(x = Group, y = TA, col = Group)) +
  geom_boxplot() +
  labs(x = "Brew Type", y = "TA", title = "Spent Brew Overview for TA Data") +
  scale_fill_manual(values = c("#e78ac3", "#B0C4DE", "#FFD700", "#4682B4")) +
  theme_minimal()+
  stat_pvalue_manual(table_spent_overview, label = "p.adj", y.position = 1.0, step.increase = 0.1)

#testing out how to do spent overview with fill
spent <- ggplot(data = all_spent, aes(x = Group, y = TA, fill = Group))+
  geom_boxplot()+
  labs(x = "Brew Type", y = "TA", title = "Spent Brew Overview for TA Data") +
  scale_fill_manual(values = c("#e78ac3", "#B0C4DE", "#FFD700", "#4682B4")) +
  theme_minimal()

spent + stat_pvalue_manual(table_spent_overview, label = "p.adj", y.position = 1.0, step.increase = 0.1)


print(spent)  

print(spent_overview)

dunnTest(TA ~ Group, data = all_spent, method = "bonferroni")
table_spent_overview <- tibble::tribble(
  ~group1, ~group2, ~p.adj,
  "Cold to Cold", "Cold to Hot", "**",
  "Cold to Cold", "Hot to Cold", "***",
  #"Cold to Hot", "Hot to Cold", "1.833523e-01",
  "Cold to Cold", "Hot to Hot", "***",
  #"Cold to Hot", "Hot to Hot", "1.00000000",
  #"Hot to Cold", "Hot to Hot", "1.00000000"
)
print(table_spent_overview)

fresh_cold_brew <- subset(coffee, Freshness == "F" & Brew == "CB")
fresh_hot_brew <- subset(coffee, Freshness == "F" & Brew == "HB")

#These three graphs show the broad comparisons of fresh coffee, second-brew
#hot coffee, and second-brew cold coffee.

TA_cold <- fresh_cold_brew$TA
TA_hot <- fresh_hot_brew$TA

coffee_fresh = subset(coffee, Freshness == "F")
dunnTest(TA ~ Brew, data = coffee_fresh, method = "bonferroni")

table_ta_fresh_cold <- tibble::tribble(
  ~group1, ~group2, ~p.adj,
  "CB", "HB", "0.2461878",
)

TA_fresh_plot <- ggplot() +
  geom_boxplot(data = fresh_cold_brew, aes(x = Brew, y = TA, fill = Brew), position = "dodge", color = "black") +
  geom_boxplot(data = fresh_hot_brew, aes(x = Brew, y = TA, fill = Brew), position = "dodge", color = "black") +
  scale_fill_manual(values = c("#e78ac3", "#B0C4DE")) +
  mytheme+
  labs(x = "Brew Type", y = "TA", title = "TA Values of Fresh Cold Brew and Fresh Hot Brew")
print(TA_fresh_plot)

coffee_spent_cold = subset(coffee, (Freshness == "SCB" |Freshness == "SHB"))
dunnTest(TA ~ Brew, data = coffee_spent_cold, method = "bonferroni")

table_ta_spent_cold <- tibble::tribble(
  ~group1, ~group2, ~p.adj,
  #"CB", "HB", "3.179324e-05",
  "CB", "HB", "***",
)

TA_spent_C_plot <- ggplot() +
  geom_boxplot(data = spent_C_to_C, aes(x = Brew, y = TA, fill = Brew), position = "dodge", color = "black") +
  geom_boxplot(data = spent_H_to_C, aes(x = Brew, y = TA, fill = Brew), position = "dodge", color = "black") +
  scale_fill_manual(values = c("#e78ac3", "#008080")) +
  mytheme+
  labs(x = "Brew Type", y = "TA", title = "TA Values of Spent Cold Brew")+
  stat_pvalue_manual(table_ta_spent_cold, label = "p.adj", y.position = 1.0, step.increase = 0.1)
print(TA_spent_C_plot)

coffee_spent_hot = subset(coffee, (Freshness == "SHB"))
dunnTest(TA ~ Brew, data = coffee_spent_hot, method = "bonferroni")

table_ta_spent_hot <- tibble::tribble(
  ~group1, ~group2, ~p.adj,
  #"CB", "HB", "0.1428499",
)
TA_spent_H_plot <- ggplot() +
  geom_boxplot(data = spent_C_to_H, aes(x = Brew, y = TA, fill = Brew), position = "dodge", color = "black") +
  geom_boxplot(data = spent_H_to_H, aes(x = Brew, y = TA, fill = Brew), position = "dodge", color = "black") +
  scale_fill_manual(values = c("#556B2F", "#B0C4DE")) +
  mytheme+
  labs(x = "Brew Type", y = "TA", title = "TA Values of Spent Hot Brew")
print(TA_spent_H_plot)


kruskal.test(TA ~ Freshness, data = coffee_TA)
#p-value is <2.2e-16 which means XXXXXXXXX

conover.test(coffee_TA$TA , coffee_TA$Freshness) 
table_conover_ta <- tibble::tribble(
  ~group1, ~group2, ~p.adj, ~significance,
  #"F", "SCB", 0.0000,
  #   "F", "SHB", 0.0000,
  #   "SCB", "SHB", 0.1191,
  "F", "SCB", "0.0000", "***",
  "F", "SHB",  "0.0000", "***",
  "SCB", "SHB",  "0.1191", "-",
)
print(table_conover_ta)

####TPC
coffee_TPC <- filter(coffee, TPC != "--")

hist(coffee_TPC$TPC)
ggqqplot(coffee_TPC$TPC)
tpc_model <- lm(TPC ~ Freshness, data = coffee_TPC)
summary(tpc_model)
hist(tpc_model$residuals) #model is significant p-value < 2.2e-16
ggqqplot(tpc_model$residuals) #non-normalized

#summary stats of TPCs
fresh_cold_brew <- subset(coffee, Freshness == "F" & Brew == "CB")
fresh_hot_brew <- subset(coffee, Freshness == "F" & Brew == "HB")

spent_cold_brew <- subset(coffee, Freshness == "SCB")
spent_hot_brew <- subset(coffee, Freshness == "SHB")

summary_FCB <- summarise(fresh_cold_brew, Mean_TPC = mean(TPC, na.rm = TRUE), SD_TPC = sd(TPC, na.rm = TRUE))
summary_FHB <- summarise(fresh_hot_brew, Mean_TPC = mean(TPC, na.rm = TRUE), SD_TPC = sd(TPC, na.rm = TRUE))
summary_SCB <- summarise(spent_cold_brew, Mean_TPC = mean(TPC, na.rm = TRUE), SD_TPC = sd(TPC, na.rm = TRUE))
summary_SHB <- summarise(spent_hot_brew, Mean_TPC = mean(TPC, na.rm = TRUE), SD_TPC = sd(TPC, na.rm = TRUE))

summary_table <- rbind(summary_FCB, summary_FHB, summary_SCB, summary_SHB)
summary_table$Group <- c("Fresh Cold Brew", "Fresh Hot Brew", "Spent Cold Brew", "Spent Hot Brew")

summary_table_transposed <- t(summary_table[, c("Mean_TPC", "SD_TPC")])

colnames(summary_table_transposed) <- summary_table$Group
rownames(summary_table_transposed) <- c("Mean_TPC", "SD_TPC")

summary_table_transposed

#TPC comparing hot to cold data
tpc_hc_plot <- ggplot(coffee_TPC, aes(x = Brew, y = TPC, fill = Brew)) +
  geom_boxplot(position = "dodge", color = "black") +
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
  labs(x = "Brew Type", y = "TPC") +
  theme_minimal()
print(tpc_hc_plot)

dunnTest(TPC ~ Brew, data = coffee, method = "bonferroni")
#input signif variables into a table for comparisons
table_tpc_hc <- tibble::tribble(
  ~group1, ~group2, ~p.adj,
  #"CB", "HB", 0.7827846,
)
print(table_tpc_hc)

#TPC comparing F, SHB, SCB data
coffee_TPC$Brew <- as.factor(coffee$Brew)
coffee_TPC$Freshness <- as.factor(coffee$Freshness)
coffee_TPC$TPC <- as.numeric(coffee_TPC$TPC)

spent_cold_brew <- subset(coffee, (Freshness == "SCB"))
spent_hot_brew <- subset(coffee, (Freshness == "SHB"))

tpc_fresh_plot <- ggplot(data = NULL) +
  geom_boxplot(data = fresh_cold_brew, aes(x = "Fresh CB", y = TPC, fill = "Fresh CB"), color = "black", position = position_dodge(width = 0.75), width = 0.5) +
  geom_boxplot(data = fresh_hot_brew, aes(x = "Fresh HB", y = TPC, fill = "Fresh HB"), color = "black", position = position_dodge(width = 1.25), width = 0.5) +
  geom_boxplot(data = subset(coffee_TPC, Freshness == "SCB"), aes(x = "SCB", y = TPC, fill = "SCB"), color = "black", position = position_dodge(width = 1.75), width = 0.5) +
  geom_boxplot(data = subset(coffee_TPC, Freshness == "SHB"), aes(x = "SHB", y = TPC, fill = "SHB"), color = "black", position = position_dodge(width = 2.25), width = 0.5) +
  scale_fill_viridis_d() +
  labs(x = "Freshness and Brew Type", y = "TPC") +
  theme_minimal() +
  stat_pvalue_manual(table_tpc_fresh, label = "p.adj", y.position = 5000, step.increase = 0.1)

print(tpc_fresh_plot)

#ALL INfO WITH DOING NEW P-VALUES WITH FHB/FCB
csv_file <- read.csv("C:/Users/aquak/OneDrive/SP2024/Data_of_Coffee_Samples_Freshness.csv", header = TRUE)
coffee_freshness <- csv_file

coffee_freshness$Coding <- NULL
coffee_freshness[,1] <- NULL
str(coffee_freshness)

coffee_freshness$New_Freshness <- as.factor(coffee$New_Freshness)
coffee_freshness$TPC <- as.numeric(coffee_freshness$TPC)


dunnTest(TPC ~ New_Freshness, data = coffee_freshness, method = "bonferroni")
table_tpc_fresh <- tibble::tribble(
  ~group1, ~group2, ~p.adj,
  "Fresh CB", "SCB", "***",
  "Fresh HB", "SCB", "***",
  "Fresh CB", "SHB", "***",
  "Fresh HB", "SHB", "***",
)

#TPC comparing all spent data
spent_C_to_C <- subset(coffee, (Freshness == "SCB") & Brew == "CB")
spent_C_to_H <- subset(coffee, (Freshness == "SHB") & Brew == "CB")
spent_H_to_H <- subset(coffee, (Freshness == "SHB") & Brew == "HB")
spent_H_to_C <- subset(coffee, (Freshness == "SCB") & Brew == "HB")



all_spent <- rbind(
  data.frame(Group = "Cold to Cold", TPC = spent_C_to_C$TPC),
  data.frame(Group = "Cold to Hot", TPC = spent_C_to_H$TPC),
  data.frame(Group = "Hot to Hot", TPC = spent_H_to_H$TPC),
  data.frame(Group = "Hot to Cold", TPC = spent_H_to_C$TPC)
)


spent_overview <- ggplot(all_spent, aes(x = Group, y = TPC, col = Group)) +
  geom_boxplot() +
  labs(x = "Brew Type", y = "TPC", title = "Spent Brew Overview for TPC Data") +
  scale_fill_manual(values = c("#e78ac3", "#B0C4DE", "#FFD700", "#4682B4")) +
  theme_minimal()+
  stat_pvalue_manual(table_spent_overview, label = "p.adj", y.position = 2600, step.increase = 0.1)

print(spent_overview)

dunnTest(TPC ~ Group, data = all_spent, method = "bonferroni")
table_spent_overview <- tibble::tribble(
  ~group1, ~group2, ~p.adj,
  #"Cold to Cold", "Cold to Hot", "1.000000e+00",
  "Cold to Cold", "Hot to Cold", "***",
  #"Cold to Hot", "Hot to Cold", "7.231195e-02",
  "Cold to Cold", "Hot to Hot", "***",
  "Cold to Hot", "Hot to Hot", "***",
  #"Hot to Cold", "Hot to Hot", "1.000000e+00"
)
print(table_spent_overview)

fresh_cold_brew <- subset(coffee, Freshness == "F" & Brew == "CB")
fresh_hot_brew <- subset(coffee, Freshness == "F" & Brew == "HB")

#These three graphs show the broad comparisons of fresh coffee, second-brew
#hot coffee, and second-brew cold coffee.

TPC_cold <- fresh_cold_brew$TPC
TPC_hot <- fresh_hot_brew$TPC

coffee_fresh = subset(coffee, Freshness == "F")
dunnTest(TPC ~ Brew, data = coffee_fresh, method = "bonferroni")

table_tpc_fresh <- tibble::tribble(
  ~group1, ~group2, ~p.adj,
  "CB", "HB", "***",
)

tpc_fresh_plot <- ggplot() +
  geom_boxplot(data = fresh_cold_brew, aes(x = Brew, y = TPC, fill = Brew), position = "dodge", color = "black") +
  geom_boxplot(data = fresh_hot_brew, aes(x = Brew, y = TPC, fill = Brew), position = "dodge", color = "black") +
  scale_fill_manual(values = c("#e78ac3", "#B0C4DE")) +
  mytheme+
  labs(x = "Brew Type", y = "TPC", title = "TPC Values of Fresh Cold Brew and Fresh Hot Brew")+
  stat_pvalue_manual(table_tpc_fresh, label = "p.adj", y.position = 5000.0, step.increase = 0.1)
print(tpc_fresh_plot)

coffee_spent_cold = subset(coffee, (Freshness == "SCB" |Freshness == "SHB"))
dunnTest(TPC ~ Brew, data = coffee_spent_cold, method = "bonferroni")

table_tpc_spent_cold <- tibble::tribble(
  ~group1, ~group2, ~p.adj,
  #"CB", "HB", ".112301e-07",
  "CB", "HB", "***",
)

tpc_spent_C_plot <- ggplot() +
  geom_boxplot(data = spent_C_to_C, aes(x = Brew, y = TPC, fill = Brew), position = "dodge", color = "black") +
  geom_boxplot(data = spent_H_to_C, aes(x = Brew, y = TPC, fill = Brew), position = "dodge", color = "black") +
  scale_fill_manual(values = c("#e78ac3", "#008080")) +
  mytheme+
  labs(x = "Brew Type", y = "TPC", title = "TPC Values of Spent Cold Brew")+
  stat_pvalue_manual(table_tpc_spent_cold, label = "p.adj", y.position = 2700.0, step.increase = 0.1)
print(tpc_spent_C_plot)

coffee_spent_hot = subset(coffee, (Freshness == "SHB"))
dunnTest(TPC ~ Brew, data = coffee_spent_hot, method = "bonferroni")

table_tpc_spent_hot <- tibble::tribble(
  ~group1, ~group2, ~p.adj,
  #"CB", "HB", "0.0001942695",
  "CB", "HB", "***"
)
tpc_spent_H_plot <- ggplot() +
  geom_boxplot(data = spent_C_to_H, aes(x = Brew, y = TPC, fill = Brew), position = "dodge", color = "black") +
  geom_boxplot(data = spent_H_to_H, aes(x = Brew, y = TPC, fill = Brew), position = "dodge", color = "black") +
  scale_fill_manual(values = c("#556B2F", "#B0C4DE")) +
  mytheme+
  labs(x = "Brew Type", y = "TPC", title = "TPC Values of Spent Hot Brew")+
  stat_pvalue_manual(table_tpc_spent_hot, label = "p.adj", y.position = 2700.0, step.increase = 0.1)
print(tpc_spent_H_plot)


kruskal.test(TPC ~ Freshness, data = coffee_TA)
#p-value is <2.2e-16 which means XXXXXXXXX

conover.test(coffee_TPC$TPC , coffee_TPC$Freshness) 
table_conover_tpc <- tibble::tribble(
  ~group1, ~group2, ~p.adj, ~significance,
  "F", "SCB", "0.0000", "***",
  "F", "SHB",  "0.0000", "***",
  "SCB", "SHB",  "0.0420", "*",
)
print(table_conover_tpc)

####pH
coffee_pH <- filter(coffee, pH != "--")

hist(coffee_pH$pH)
ggqqplot(coffee_pH$pH)
ph_model <- lm(pH ~ Freshness, data = coffee_pH)
summary(ph_model)
hist(ph_model$residuals) #model is significant p-value < 2.2e-16
ggqqplot(ph_model$residuals) #non-normalized

#summary stats of pH
fresh_cold_brew <- subset(coffee, Freshness == "F" & Brew == "CB")
fresh_hot_brew <- subset(coffee, Freshness == "F" & Brew == "HB")

spent_cold_brew <- subset(coffee, Freshness == "SCB")
spent_hot_brew <- subset(coffee, Freshness == "SHB")

summary_FCB <- summarise(fresh_cold_brew, Mean_pH = mean(pH, na.rm = TRUE), SD_pH = sd(pH, na.rm = TRUE))
summary_FHB <- summarise(fresh_hot_brew, Mean_pH = mean(pH, na.rm = TRUE), SD_pH = sd(pH, na.rm = TRUE))
summary_SCB <- summarise(spent_cold_brew, Mean_pH = mean(pH, na.rm = TRUE), SD_pH = sd(pH, na.rm = TRUE))
summary_SHB <- summarise(spent_hot_brew, Mean_pH = mean(pH, na.rm = TRUE), SD_pH = sd(pH, na.rm = TRUE))

summary_table <- rbind(summary_FCB, summary_FHB, summary_SCB, summary_SHB)
summary_table$Group <- c("Fresh Cold Brew", "Fresh Hot Brew", "Spent Cold Brew", "Spent Hot Brew")

summary_table_transposed <- t(summary_table[, c("Mean_pH", "SD_pH")])

colnames(summary_table_transposed) <- summary_table$Group
rownames(summary_table_transposed) <- c("Mean_pH", "SD_pH")

summary_table_transposed

#pH comparing hot to cold data
ph_hc_plot <- ggplot(coffee_pH, aes(x = Brew, y = pH, fill = Brew)) +
  geom_boxplot(position = "dodge", color = "black") +
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
  labs(x = "Brew Type", y = "pH") +
  theme_minimal()
print(ph_hc_plot)

dunnTest(pH ~ Brew, data = coffee, method = "bonferroni")
#input signif variables into a table for comparisons
table_pH_hc <- tibble::tribble(
  ~group1, ~group2, ~p.adj,
  #"CB", "HB", 0.4124576,
)
print(table_pH_hc)

#pH comparing F, SHB, SCB data
ph_fresh_plot <- ggplot(data = NULL) +
  geom_boxplot(data = fresh_cold_brew, aes(x = "Fresh CB", y = pH, fill = "Fresh CB"), color = "black", position = position_dodge(width = 0.75), width = 0.5) +
  geom_boxplot(data = fresh_hot_brew, aes(x = "Fresh HB", y = pH, fill = "Fresh HB"), color = "black", position = position_dodge(width = 1.25), width = 0.5) +
  geom_boxplot(data = subset(coffee_pH, Freshness == "SCB"), aes(x = "SCB", y = pH, fill = "SCB"), color = "black", position = position_dodge(width = 1.75), width = 0.5) +
  geom_boxplot(data = subset(coffee_pH, Freshness == "SHB"), aes(x = "SHB", y = pH, fill = "SHB"), color = "black", position = position_dodge(width = 2.25), width = 0.5) +
  scale_fill_viridis_d() +
  labs(x = "Freshness and Brew Type", y = "pH") +
  theme_minimal() +
  stat_pvalue_manual(new_table_pH_fresh, label = "p.adj", y.position = 8.5, step.increase = 0.1)

print(ph_fresh_plot)


new_table_pH_fresh <- tibble::tribble(
  ~group1, ~group2, ~p.adj,
  "Fresh CB", "SCB", "***",
  "Fresh HB", "SCB", "***",
  "Fresh CB", "SHB", "***",
  "Fresh HB", "SHB", "***",
)


dunnTest(pH ~ Freshness, data = coffee, method = "bonferroni")
table_pH_fresh <- tibble::tribble(
  ~group1, ~group2, ~p.adj,
  "F", "SCB", "***",
  "F", "SHB", "***",
)

#pH comparing all spent data
spent_C_to_C <- subset(coffee, (Freshness == "SCB") & Brew == "CB")
spent_C_to_H <- subset(coffee, (Freshness == "SHB") & Brew == "CB")
spent_H_to_H <- subset(coffee, (Freshness == "SHB") & Brew == "HB")
spent_H_to_C <- subset(coffee, (Freshness == "SCB") & Brew == "HB")



all_spent <- rbind(
  data.frame(Group = "Cold to Cold", pH = spent_C_to_C$pH),
  data.frame(Group = "Cold to Hot", pH = spent_C_to_H$pH),
  data.frame(Group = "Hot to Hot", pH = spent_H_to_H$pH),
  data.frame(Group = "Hot to Cold", pH = spent_H_to_C$pH)
)

spent_overview <- ggplot(all_spent, aes(x = Group, y = pH, col = Group)) +
  geom_boxplot() +
  labs(x = "Brew Type", y = "pH", title = "Spent Brew Overview for pH Data") +
  scale_fill_manual(values = c("#e78ac3", "#B0C4DE", "#FFD700", "#4682B4")) +
  theme_minimal()+
  stat_pvalue_manual(table_spent_overview, label = "p.adj", y.position = 8.5, step.increase = 0.1)

print(spent_overview)

dunnTest(pH ~ Group, data = all_spent, method = "bonferroni")
table_spent_overview <- tibble::tribble(
  ~group1, ~group2, ~p.adj,
  "Cold to Cold", "Cold to Hot", "***",
  "Cold to Cold", "Hot to Cold", "***",
  #"Cold to Hot", "Hot to Cold", "1.000000e+00",
  "Cold to Cold", "Hot to Hot", "*",
  #"Cold to Hot", "Hot to Hot", "1.00000000",
  #"Hot to Cold", "Hot to Hot", "9.644729e-01"
)
print(table_spent_overview)

fresh_cold_brew <- subset(coffee, Freshness == "F" & Brew == "CB")
fresh_hot_brew <- subset(coffee, Freshness == "F" & Brew == "HB")

#These three graphs show the broad comparisons of fresh coffee, second-brew
#hot coffee, and second-brew cold coffee.

pH_cold <- fresh_cold_brew$pH
pH_hot <- fresh_hot_brew$pH

coffee_fresh = subset(coffee, Freshness == "F")
dunnTest(pH ~ Brew, data = coffee_fresh, method = "bonferroni")

table_pH_fresh <- tibble::tribble(
  ~group1, ~group2, ~p.adj,
  "CB", "HB", "***",
)

pH_fresh_plot <- ggplot() +
  geom_boxplot(data = fresh_cold_brew, aes(x = Brew, y = pH, fill = Brew), position = "dodge", color = "black") +
  geom_boxplot(data = fresh_hot_brew, aes(x = Brew, y = pH, fill = Brew), position = "dodge", color = "black") +
  scale_fill_manual(values = c("#e78ac3", "#B0C4DE")) +
  mytheme+
  labs(x = "Brew Type", y = "pH", title = "pH Values of Fresh Cold Brew and Fresh Hot Brew")+
  stat_pvalue_manual(table_pH_fresh, label = "p.adj", y.position = 8.3, step.increase = 0.1)
print(pH_fresh_plot)

coffee_spent_cold = subset(coffee, (Freshness == "SCB" |Freshness == "SHB"))
dunnTest(pH ~ Brew, data = coffee_spent_cold, method = "bonferroni")

table_pH_spent_cold <- tibble::tribble(
  ~group1, ~group2, ~p.adj,
  #"CB", "HB", "0.002742534",
  "CB", "HB", "***",
)

pH_spent_C_plot <- ggplot() +
  geom_boxplot(data = spent_C_to_C, aes(x = Brew, y = pH, fill = Brew), position = "dodge", color = "black") +
  geom_boxplot(data = spent_H_to_C, aes(x = Brew, y = pH, fill = Brew), position = "dodge", color = "black") +
  scale_fill_manual(values = c("#e78ac3", "#008080")) +
  mytheme+
  labs(x = "Brew Type", y = "pH", title = "pH Values of Spent Cold Brew")+
  stat_pvalue_manual(table_pH_spent_cold, label = "p.adj", y.position = 8.3, step.increase = 0.1)
print(pH_spent_C_plot)

coffee_spent_hot = subset(coffee, (Freshness == "SHB"))
dunnTest(pH ~ Brew, data = coffee_spent_hot, method = "bonferroni")

table_pH_spent_hot <- tibble::tribble(
  ~group1, ~group2, ~p.adj,
  #"CB", "HB", "0.0001942695",
  "CB", "HB", "***"
)
pH_spent_H_plot <- ggplot() +
  geom_boxplot(data = spent_C_to_H, aes(x = Brew, y = pH, fill = Brew), position = "dodge", color = "black") +
  geom_boxplot(data = spent_H_to_H, aes(x = Brew, y = pH, fill = Brew), position = "dodge", color = "black") +
  scale_fill_manual(values = c("#556B2F", "#B0C4DE")) +
  mytheme+
  labs(x = "Brew Type", y = "pH", title = "pH Values of Spent Hot Brew")
  #stat_pvalue_manual(table_pH_spent_hot, label = "p.adj", y.position = 2700.0, step.increase = 0.1)
print(pH_spent_H_plot)


kruskal.test(pH ~ Freshness, data = coffee_TA)
#p-value is <2.2e-16 which means XXXXXXXXX

conover.test(coffee_pH$pH , coffee_pH$Freshness) 
table_conover_pH <- tibble::tribble(
  ~group1, ~group2, ~p.adj, ~significance,
  "F", "SCB", "0.0000", "***",
  "F", "SHB",  "0.0000", "***",
  "SCB", "SHB",  "0.0216", "*",
)
print(table_conover_pH)

###DPPH
coffee_DPPH <- filter(coffee, DPPH != "--")

hist(coffee_DPPH$DPPH)
ggqqplot(coffee_DPPH$DPPH)
dpph_model <- lm(DPPH ~ Freshness, data = coffee_DPPH)
summary(dpph_model)
hist(dpph_model$residuals) #model is significant p-value < 2.2e-16
ggqqplot(dpph_model$residuals) #non-normalized

#summary stats of DPPH
fresh_cold_brew <- subset(coffee, Freshness == "F" & Brew == "CB")
fresh_hot_brew <- subset(coffee, Freshness == "F" & Brew == "HB")

spent_cold_brew <- subset(coffee, Freshness == "SCB")
spent_hot_brew <- subset(coffee, Freshness == "SHB")

summary_FCB <- summarise(fresh_cold_brew, Mean_DPPH = mean(DPPH, na.rm = TRUE), SD_DPPH = sd(DPPH, na.rm = TRUE))
summary_FHB <- summarise(fresh_hot_brew, Mean_DPPH = mean(DPPH, na.rm = TRUE), SD_DPPH = sd(DPPH, na.rm = TRUE))
summary_SCB <- summarise(spent_cold_brew, Mean_DPPH = mean(DPPH, na.rm = TRUE), SD_DPPH = sd(DPPH, na.rm = TRUE))
summary_SHB <- summarise(spent_hot_brew, Mean_DPPH = mean(DPPH, na.rm = TRUE), SD_DPPH = sd(DPPH, na.rm = TRUE))

summary_table <- rbind(summary_FCB, summary_FHB, summary_SCB, summary_SHB)
summary_table$Group <- c("Fresh Cold Brew", "Fresh Hot Brew", "Spent Cold Brew", "Spent Hot Brew")

summary_table_transposed <- t(summary_table[, c("Mean_DPPH", "SD_DPPH")])

colnames(summary_table_transposed) <- summary_table$Group
rownames(summary_table_transposed) <- c("Mean_DPPH", "SD_DPPH")

summary_table_transposed

#DPPH comparing hot to cold data
dpph_hc_plot <- ggplot(coffee_DPPH, aes(x = Brew, y = DPPH, fill = Brew)) +
  geom_boxplot(position = "dodge", color = "black") +
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
  labs(x = "Brew Type", y = "DPPH") +
  theme_minimal()
print(dpph_hc_plot)

dunnTest(DPPH ~ Brew, data = coffee_DPPH, method = "bonferroni")
#input signif variables into a table for comparisons
table_DPPH_hc <- tibble::tribble(
  ~group1, ~group2, ~p.adj,
  #"CB", "HB", 0.9972391,
)
print(table_DPPH_hc)

#DPPH comparing F, SHB, SCB data
dpph_fresh_plot <- ggplot(data = NULL) +
  geom_boxplot(data = fresh_cold_brew, aes(x = "Fresh CB", y = DPPH, fill = "Fresh CB"), color = "black", position = position_dodge(width = 0.75), width = 0.5) +
  geom_boxplot(data = fresh_hot_brew, aes(x = "Fresh HB", y = DPPH, fill = "Fresh HB"), color = "black", position = position_dodge(width = 1.25), width = 0.5) +
  geom_boxplot(data = subset(coffee_DPPH, Freshness == "SCB"), aes(x = "SCB", y = DPPH, fill = "SCB"), color = "black", position = position_dodge(width = 1.75), width = 0.5) +
  geom_boxplot(data = subset(coffee_DPPH, Freshness == "SHB"), aes(x = "SHB", y = DPPH, fill = "SHB"), color = "black", position = position_dodge(width = 2.25), width = 0.5) +
  scale_fill_viridis_d() +
  labs(x = "Freshness and Brew Type", y = "DPPH") +
  theme_minimal() +
  stat_pvalue_manual(new_table_dpph_fresh, label = "p.adj", y.position = 5, step.increase = 0.1)

print(dpph_fresh_plot)


new_table_dpph_fresh <- tibble::tribble(
  ~group1, ~group2, ~p.adj,
  "Fresh CB", "SCB", "***",
  "Fresh HB", "SCB", "***",
  "Fresh CB", "SHB", "***",
  "Fresh HB", "SHB", "***",
)

dunnTest(DPPH ~ Freshness, data = coffee_DPPH, method = "bonferroni")
table_pH_fresh <- tibble::tribble(
  ~group1, ~group2, ~p.adj,
  "F", "SCB", "***",
  "F", "SHB", "***",
)

#DPPH comparing all spent data
spent_C_to_C <- subset(coffee, (Freshness == "SCB") & Brew == "CB")
spent_C_to_H <- subset(coffee, (Freshness == "SHB") & Brew == "CB")
spent_H_to_H <- subset(coffee, (Freshness == "SHB") & Brew == "HB")
spent_H_to_C <- subset(coffee, (Freshness == "SCB") & Brew == "HB")


all_spent <- rbind(
  data.frame(Group = "Cold to Cold", DPPH = spent_C_to_C$DPPH),
  data.frame(Group = "Cold to Hot", DPPH = spent_C_to_H$DPPH),
  data.frame(Group = "Hot to Hot", DPPH = spent_H_to_H$DPPH),
  data.frame(Group = "Hot to Cold", DPPH = spent_H_to_C$DPPH)
)

spent_overview <- ggplot(all_spent, aes(x = Group, y = DPPH, col = Group)) +
  geom_boxplot() +
  labs(x = "Brew Type", y = "DPPH", title = "Spent Brew Overview for DPPH") +
  scale_fill_manual(values = c("#e78ac3", "#B0C4DE", "#FFD700", "#4682B4")) +
  theme_minimal()
  #stat_pvalue_manual(table_spent_overview, label = "p.adj", y.position = 1.0, step.increase = 0.1)

print(spent_overview)

dunnTest(DPPH ~ Group, data = all_spent, method = "bonferroni")
table_spent_overview <- tibble::tribble(
  ~group1, ~group2, ~p.adj,
  #"Cold to Cold", "Cold to Hot", "1",
  #"Cold to Cold", "Hot to Cold", "1",
  #"Cold to Hot", "Hot to Cold", "1",
  #"Cold to Cold", "Hot to Hot", "1",
  #"Cold to Hot", "Hot to Hot", "1",
  #"Hot to Cold", "Hot to Hot", "1"
)
print(table_spent_overview)

fresh_cold_brew <- subset(coffee, Freshness == "F" & Brew == "CB")
fresh_hot_brew <- subset(coffee, Freshness == "F" & Brew == "HB")

#These three graphs show the broad comparisons of fresh coffee, second-brew
#hot coffee, and second-brew cold coffee.

DPPH_cold <- fresh_cold_brew$DPPH
DPPH_hot <- fresh_hot_brew$DPPH

coffee_fresh = subset(coffee, Freshness == "F")
dunnTest(DPPH ~ Brew, data = coffee_fresh, method = "bonferroni")

table_dpph_fresh <- tibble::tribble(
  ~group1, ~group2, ~p.adj,
  "CB", "HB", "***",
)

dpph_fresh_plot <- ggplot() +
  geom_boxplot(data = fresh_cold_brew, aes(x = Brew, y = DPPH, fill = Brew), position = "dodge", color = "black") +
  geom_boxplot(data = fresh_hot_brew, aes(x = Brew, y = DPPH, fill = Brew), position = "dodge", color = "black") +
  scale_fill_manual(values = c("#e78ac3", "#B0C4DE")) +
  mytheme+
  labs(x = "Brew Type", y = "pH", title = "DPPH Values of Fresh Cold Brew and Fresh Hot Brew")
print(dpph_fresh_plot)

coffee_spent_cold = subset(coffee, (Freshness == "SCB" |Freshness == "SHB"))
dunnTest(DPPH ~ Brew, data = coffee_spent_cold, method = "bonferroni")

table_dpph_spent_cold <- tibble::tribble(
  ~group1, ~group2, ~p.adj,
  #"CB", "HB", "0.002742534",
  "CB", "HB", "***",
)

dpph_spent_C_plot <- ggplot() +
  geom_boxplot(data = spent_C_to_C, aes(x = Brew, y = DPPH, fill = Brew), position = "dodge", color = "black") +
  geom_boxplot(data = spent_H_to_C, aes(x = Brew, y = DPPH, fill = Brew), position = "dodge", color = "black") +
  scale_fill_manual(values = c("#e78ac3", "#008080")) +
  mytheme+
  labs(x = "Brew Type", y = "DPPH", title = "DPPH Values of Spent Cold Brew")
print(dpph_spent_C_plot)

coffee_spent_hot = subset(coffee, (Freshness == "SHB"))
dunnTest(DPPH ~ Brew, data = coffee_spent_hot, method = "bonferroni")

table_dpph_spent_hot <- tibble::tribble(
  ~group1, ~group2, ~p.adj,
  #"CB", "HB", "0.0001942695",
  "CB", "HB", "***"
)
dpph_spent_H_plot <- ggplot() +
  geom_boxplot(data = spent_C_to_H, aes(x = Brew, y = DPPH, fill = Brew), position = "dodge", color = "black") +
  geom_boxplot(data = spent_H_to_H, aes(x = Brew, y = DPPH, fill = Brew), position = "dodge", color = "black") +
  scale_fill_manual(values = c("#556B2F", "#B0C4DE")) +
  mytheme+
  labs(x = "Brew Type", y = "DPPH", title = "DPPH Values of Spent Hot Brew")
#stat_pvalue_manual(table_dpph_spent_hot, label = "p.adj", y.position = 2700.0, step.increase = 0.1)
print(dpph_spent_H_plot)


kruskal.test(DPPH ~ Freshness, data = coffee_TA)
#p-value is <2.2e-16 which means XXXXXXXXX

conover.test(coffee_DPPH$DPPH , coffee_DPPH$Freshness) 
table_conover_dpph <- tibble::tribble(
  ~group1, ~group2, ~p.adj, ~significance,
  "F", "SCB", "0.0000", "***",
  "F", "SHB",  "0.0000", "***",
  "SCB", "SHB",  "0.4170", "-",
)
print(table_conover_dpph)
