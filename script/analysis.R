# Load packages -----------------------------------------------------------

library(tidyverse)
library(grid)
library(gridExtra)
library(chron)
library(RColorBrewer)
library(ggrepel)
library(cowplot)
library(ggforce)
library(GGally)
library(ggbeeswarm)
library(viridis)

#create my own "theme"
kp_theme <- function(){
  theme_bw()+
  theme(strip.background =element_rect(fill="white"))
}

# Load data ---------------------------------------------------------------

df <- read_csv("./data/2022-01-08_sports-activity-data.csv") %>% 
  rename_all(tolower) %>% 
  select(datum, aktivitetstyp, namn, sträcka, kalorier, medelpuls, maxpuls, `total stigning`, `total tid`)  %>% 
  rename(date = datum,
         activity_type = aktivitetstyp,
         name = namn,
         distance = sträcka,
         calories = kalorier,
         mean_pulse = medelpuls,
         max_pulse = maxpuls,
         tot_climb = `total stigning`,
         tot_time = `total tid`)

# Activity names to English
df <- df %>% 
  mutate(activity = case_when(
    str_detect(name, "Löpning") ~ "Running",
    str_detect(name, "Rullskidor") ~ "Roller ski",
    str_detect(name, "Längdskidor") ~ "Cross country ski",
    str_detect(name, "Cykling") ~ "Cycling",
    str_detect(name, "Vandring") ~ "Hiking")
  ) %>% 
  relocate(activity)

# Comma-separated number to dot and numeric
df$distance <- as.numeric(gsub(",", ".", gsub("\\.", "", df$distance )))
df$calories <- as.numeric(gsub(",", ".", gsub("\\.", "", df$calories  )))
df$mean_pulse <- as.numeric(gsub(",", ".", gsub("\\.", "", df$mean_pulse)))
df$max_pulse <- as.numeric(gsub(",", ".", gsub("\\.", "", df$max_pulse)))
df$tot_climb<- as.numeric(gsub(",", ".", gsub("\\.", "", df$tot_climb)))

# Create some new columns and fix total time
# Add running race
df <- df %>% 
  mutate(
    stifa = tot_climb/distance,
    tot_time = 60 * 24 * as.numeric(times(tot_time)),
    activity_and_race = case_when(
      str_detect(name, regex("Tävling", ignore_case = T)) & activity == "Running" ~ "Running Race",
      TRUE ~ activity
    ))

# Remove rows with NA
df_clean <- df %>% 
  drop_na() %>% 
  relocate(activity, activity_and_race, activity_type,  name)

# Plot exploratory PCA ----------------------------------------------------

df_pca <- prcomp(df_clean[,6:12], center = TRUE,scale. = TRUE)

df_out <- as.data.frame(df_pca$x)
df_out <- cbind(df_out, df_clean)

s <- summary(df_pca)
set_xlab=paste("PC 1 (", round(s$importance[2,1]*100, 1), "%)", sep = "")
set_ylab=paste("PC 2 (", round(s$importance[2,2]*100, 1), "%)", sep = "")

# Fix misspelled names  
df_out <- df_out %>% 
  mutate(name_outliers = case_when(
    str_detect(name, regex("Öppet spår", ignore_case = T)) ~ "Vasaloppet",
    str_detect(name, regex("Motala Cykling", ignore_case = T)) ~ "Vätternrundan",
    str_detect(name, regex("Ironman Köpenhamn Cykling", ignore_case = T)) ~ "Ironman - Cycling"
  ))

#set  activity colors
colors_activity <- viridis(5, option = "turbo", begin = 0, end = 0.85)
names(colors_activity) <- c("Cross country ski", "Cycling", "Hiking", "Roller ski", "Running")
set_colors_activity <- function(){
  scale_color_manual(values = colors_activity)
  }


p1_pca_no_log <-ggplot(df_out,aes(x=PC1,y=PC2,color=activity,label = name_outliers)) + 
  geom_point(alpha = 0.75) +
  geom_label_repel(show.legend = FALSE)+
  set_colors_activity()+
  xlab(set_xlab) + 
  ylab(set_ylab) +
  labs(color = "",
       title = "PCA scores",
       subtitle = "Raw data")+
  kp_theme()

ggsave(plot = p1_pca_no_log, filename = "p1_pca_no_log.pdf", path = "output", device = "pdf", width = 7, height = 4)
ggsave(plot = p1_pca_no_log, filename = "p1_pca_no_log.png", path = "output", device = "png", width = 7, height = 4, dpi = 300)

# Check distributions of data --------------------------------------------

df_clean_long <- df_clean %>% 
  pivot_longer(!c(activity, activity_and_race, activity_type, name, date), names_to = "measurement", values_to = "value")%>% 
  mutate(
    measurement = case_when(
      measurement == "calories" ~ "Calories (kcal)",
      measurement == "distance" ~ "Distance (km)",
      measurement == "max_pulse" ~ "Max pulse (bpm)",
      measurement == "mean_pulse" ~ "Mean pulse (bpm)",
      measurement == "stifa" ~ "Stifa (total climb / distance)",
      measurement == "tot_climb" ~ "Total climb (m)",
      measurement == "tot_time" ~ "Total time (min)",
      TRUE ~ measurement
    ))

#set variable colors
colors_variables <- viridis(length(levels(factor(df_clean_long$measurement))),  option = "viridis", begin = 0, end = 0.85)
names(colors_variables) <-levels(factor(df_clean_long$measurement))
set_colors_variables <- function(){
  scale_fill_manual(values = colors_variables)
  }


# no log
p2_variable_distribution_no_log<- df_clean_long %>%  
  ggplot(aes(x = value, fill = measurement))+
  geom_histogram(position = 'identity', show.legend = FALSE, col = "black")+
  facet_wrap(~measurement, scales = "free")+
  kp_theme()+
  labs(title = "Data distribution of the raw variables",
       x = "",
       y = "Count")+
  set_colors_variables()

ggsave(plot = p2_variable_distribution_no_log, filename = "p2_variable_distribution_no_log.pdf", path = "output", device = "pdf", width = 7, height = 4)
ggsave(plot = p2_variable_distribution_no_log, filename = "p2_variable_distribution_no_log.png", path = "output", device = "png", width = 7, height = 4, dpi = 300)

# log10 of some variables
df_clean_long_log <- df_clean_long %>% 
  mutate(value = ifelse(measurement == "Calories (kcal)", log10(value), value),
         value = ifelse(measurement == "Distance (km)", log10(value), value),
         value = ifelse(measurement == "Stifa (total climb / distance)", log10(value), value),
         value = ifelse(measurement == "Total climb (m)", log10(value), value),
         value = ifelse(measurement == "Total time (min)", log10(value), value)) %>% 
  mutate(
    measurement = case_when(
      measurement == "Calories (kcal)" ~ "Calories log10(kcal)",
      measurement == "Distance (km)" ~ "Distance log10(km)",
      measurement == "Max pulse (bpm)" ~ "Max pulse (bpm)",
      measurement == "Mean pulse (bpm)" ~ "Mean pulse (bpm)",
      measurement == "Stifa (total climb / distance)" ~ "Stifa log10(total climb / distance)",
      measurement == "Total climb (m)" ~ "Total climb log10(m)",
      measurement == "Total time (min)" ~ "Total time log10(min)",
      TRUE ~ measurement
    )
  )

#set variable colors log10
colors_variables <- viridis(length(levels(factor(df_clean_long_log$measurement))),  option = "viridis", begin = 0, end = 0.85)
names(colors_variables) <-levels(factor(df_clean_long_log$measurement))
set_colors_variables <- function(){
  scale_fill_manual(values = colors_variables)
}

p3_variable_distribution_log10<- df_clean_long_log %>% 
  ggplot(aes(x = value, fill = measurement))+
  geom_histogram(position = 'identity', show.legend = FALSE, col = "black")+
  facet_wrap(~measurement, scales = "free")+
  kp_theme()+
  labs(title = "Data distribution of log transformed variables",
       x = "",
       y = "Count")+
  set_colors_variables()
ggsave(plot = p3_variable_distribution_log10, filename = "p3_variable_distribution_log10.pdf", path = "output", device = "pdf", width = 7, height = 4)
ggsave(plot = p3_variable_distribution_log10, filename = "p3_variable_distribution_log10.png", path = "output", device = "png", width = 7, height = 4, dpi = 300)


# create df clean log
df_clean_log <- df_clean_long_log %>% 
  pivot_wider(names_from = measurement, values_from = value)


# PCA with log10 data -----------------------------------------------------

df_pca <- prcomp(df_clean_log[,6:12], center = TRUE,scale. = TRUE)

df_out <- as.data.frame(df_pca$x)
df_out <- cbind(df_out, df_clean_log)

s <- summary(df_pca)
set_xlab=paste("PC 1 (", round(s$importance[2,1]*100, 1), "%)", sep = "")
set_ylab=paste("PC 2 (", round(s$importance[2,2]*100, 1), "%)", sep = "")
# Fix misspelled names  
df_out <- df_out %>% 
  mutate(name_outliers = case_when(
    str_detect(name, regex("Öppet spår", ignore_case = T)) ~ "Vasaloppet",
    str_detect(name, regex("Motala Cykling", ignore_case = T)) ~ "Vätternrundan",
    str_detect(name, regex("Ironman Köpenhamn Cykling", ignore_case = T)) ~ "Ironman - Cycling"
  ))


p4_pca_log10<-ggplot(df_out,aes(x=PC1,y=PC2,color=activity,label = name_outliers)) + 
  geom_point(alpha = 0.75) +
  geom_label_repel( max.overlaps = 70, show.legend = FALSE)+
  xlab(set_xlab) + 
  ylab(set_ylab) +
  kp_theme()+
  labs(color = "",
       title = "PCA scores",
       subtitle = "Log transformation of some variables")+
  kp_theme()+
  set_colors_activity()

ggsave(plot = p4_pca_log10, filename = "p4_pca_log10.pdf", path = "output", device = "pdf", width = 7, height = 5)
ggsave(plot = p4_pca_log10, filename = "p4_pca_log10.png", path = "output", device = "png", width = 7, height = 5, dpi = 300)

# Scores and loadings
df_out_r <- as.data.frame(df_pca$rotation)
df_out_r$variable <- row.names(df_out_r)
df_out_r$PC1 <- df_out_r$PC1*max(abs(df_out$PC1))
df_out_r$PC2 <- df_out_r$PC2*max(abs(df_out$PC2))

# Set colors activity and race
colors_activity_and_race <- c(viridis(5, option = "turbo", begin = 0, end = 0.85), viridis(1, option = "turbo", begin = 1, end = 1))
names(colors_activity_and_race) <- c("Cross country ski", "Cycling", "Hiking", "Roller ski", "Running", "Running Race")
set_color_activity_and_race <- function(){
  scale_color_manual(values = colors_activity_and_race)}
set_fill_activity_and_race <- function(){
  scale_fill_manual(values = colors_activity_and_race)}

# plot biplot activity and race
p5_biplot_activity_and_race_same <-ggplot(df_out,aes(x=PC1,y=PC2)) + 
  stat_ellipse(geom = "polygon",
               aes(col = activity_and_race, fill = activity_and_race),
               alpha = 0.1)+
  geom_point(aes(color=activity_and_race), alpha = 0.75) +
  geom_segment(data = df_out_r, aes(xend=PC1, yend = PC2), x = 0, y=0, show.legend = FALSE,
               arrow = arrow(length = unit(0.5, "cm")))+
  geom_label_repel(data = df_out_r,aes(label=variable),
                   label.size = NA, 
                   alpha = 0.6, 
                   label.padding=.1, 
                   na.rm=TRUE,
                   seed = 1234) +
  geom_label_repel(data = df_out_r,aes(label=variable),
                   label.size = NA, 
                   alpha = 1, 
                   label.padding=.1, 
                   na.rm=TRUE,
                   fill = NA,
                   seed = 1234)+
  xlab(set_xlab) + 
  ylab(set_ylab) +
  kp_theme()+
  labs(color = "",
       fill = "")+
  labs(title = "PCA scores and loadings")+
  set_color_activity_and_race()+
  set_fill_activity_and_race()

#create same plot without text to use later
p5_biplot_activity_and_race_same_no_text <-ggplot(df_out,aes(x=PC1,y=PC2)) + 
  stat_ellipse(geom = "polygon",
               aes(col = activity_and_race, fill = activity_and_race),
               alpha = 0.1)+
  geom_point(aes(color=activity_and_race), alpha = 0.75) +
  geom_segment(data = df_out_r, aes(xend=PC1, yend = PC2), x = 0, y=0, show.legend = FALSE,
               arrow = arrow(length = unit(0.5, "cm")))+
  geom_label_repel(data = df_out_r,aes(label=variable),
                   label.size = NA,
                   alpha = 0.6,
                   label.padding=.1,
                   na.rm=TRUE,
                   seed = 1234,
                   size = 3) +
  geom_label_repel(data = df_out_r,aes(label=variable),
                   label.size = NA,
                   alpha = 1,
                   label.padding=.1,
                   na.rm=TRUE,
                   fill = NA,
                   seed = 1234,
                   size = 3)+
  xlab(set_xlab) + 
  ylab(set_ylab) +
  kp_theme()+
  labs(color = "",
       fill = "")+
  labs(title = "PCA scores and loadings")+
  set_color_activity_and_race()+
  set_fill_activity_and_race()

ggsave(plot = p5_biplot_activity_and_race_same, filename = "p5_biplot_activity_and_race_same.pdf", path = "output", device = "pdf", width = 7, height = 5)
ggsave(plot = p5_biplot_activity_and_race_same, filename = "p5_biplot_activity_and_race_same.png", path = "output", device = "png", width = 7, height = 5, dpi = 300)

# PCA labelled with all variables ------------------------------------------

variables <- df_out_r$variable
pca_list = list()
p5_biplot_activity_and_race_same_no_text <- p5_biplot_activity_and_race_same_no_text+ theme(legend.position = "none")
  pca_list[["p5_biplot_activity_and_race_same_no_text"]] <- p5_biplot_activity_and_race_same_no_text

aes_string2 <- function(...){
  args <- lapply(list(...), function(x) sprintf("`%s`", x))
  do.call(aes_string, args)
}
for (variable in variables) {
  pca_list[[variable]] <- p_pca_mean_pulse<-ggplot(df_out,aes_string2(x="PC1",y="PC2",color=variable)) + 
    geom_point() +
    theme_bw()+
    xlab(set_xlab) + 
    ylab(set_ylab) +
    labs(title = variable,
         color = "")+
    scale_colour_gradient(low = "midnightblue", high = "firebrick3")
    
}

p8_pca_all_variables <- do.call(grid.arrange, pca_list)
ggsave(plot = p8_pca_all_variables, filename = "p8_pca_all_variables.pdf", path = "output", device = "pdf", width = 9, height = 7)
ggsave(plot = p8_pca_all_variables, filename = "p8_pca_all_variables.png", path = "output", device = "png", width = 9, height = 7, dpi = 300)

# Distribution of variables depending on sports activity -------------------

# inspiration from: https://stackoverflow.com/questions/52214071/how-to-order-data-by-value-within-ggplot-facets

reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
  new_x <- paste(x, within, sep = sep)
  stats::reorder(new_x, by, FUN = fun)
}

scale_x_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
}

p6_variable_signal_log10 <- df_clean_long_log %>% 
  ggplot(aes(x = reorder_within(activity_and_race, value, measurement), y = value, col = activity_and_race))+
  geom_boxplot(aes(fill = activity_and_race), outlier.shape = NA, alpha = 0.1)+
  facet_wrap(~measurement, scales = "free")+
  scale_x_reordered() + 
  geom_quasirandom(alpha = 0.5)+
  kp_theme()+
  theme(axis.text.x=element_text(angle=25,hjust=1,vjust = 1),
        legend.position="top")+
  labs(title = "Data distribution of the variables within every sports activity",
       subtitle = "Activities are ordered by median within every variable",
       y = "",
       x = "",
       col = "",
       fill = "")+
  guides(color = guide_legend(nrow = 1),
         fill = guide_legend(nrow = 1))+
  set_color_activity_and_race()+
  set_fill_activity_and_race()

ggsave(plot = p6_variable_signal_log10, filename = "p6_variable_signal_log10.pdf", path = "output", device = "pdf", width = 7.5, height = 6.5)
ggsave(plot = p6_variable_signal_log10, filename = "p6_variable_signal_log10.png", path = "output", device = "png", width = 7.5, height = 6.5, dpi = 300)


# Correlation of variables -------------------------------------------------

# inspiration from: https://www.r-bloggers.com/2016/02/multiple-regression-lines-in-ggpairs/
plot_regressions1 <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method=lm, ...)
  p
}
cor_fun <- function(data, mapping, method = "pearson", ndp = 2, sz = 5, stars = TRUE, ...){
  
  x <- eval_data_col(data, mapping$x)
  y <- eval_data_col(data, mapping$y)
  
  corr <- cor.test(x, y, method = method)
  est <- corr$estimate
  lb.size <- sz * abs(est)
  
  if(stars){
    stars <- c("***", "**", "*", "")[findInterval(corr$p.value, c(0, 0.001, 0.01, 1))]
    lbl <- paste0(round(est, ndp), stars)
  }else{
    lbl <- round(est, ndp)
  }
  
  ggplot(data = data, mapping = mapping) +
    annotate("text", x = mean(x, na.rm =TRUE), y = mean(y, na.rm = TRUE), label = lbl, size = lb.size, ...)+
    theme(panel.grid = element_blank())
}

p7_variable_correlation <- ggpairs(df_clean_log,aes(col = activity_and_race, alpha = 0.7, fill = activity_and_race), columns = 6:12,
        lower = list(continuous = plot_regressions1),
        diag = list(continuous = "blankDiag"))+ 
  kp_theme()+
  scale_fill_brewer(palette = "Set1")+
  scale_color_brewer(palette = "Set1")

# Add diagonal line
for (i in 2:p7_variable_correlation$nrow) {
  for (j in 1:(i-1)) {
    p7_variable_correlation[i,j] = p7_variable_correlation[i,j] + geom_abline(intercept=0,slope=1)
  }
}

  
ggsave(plot = p7_variable_correlation, filename = "p7_variable_correlation.pdf", path = "output", device = "pdf", width = 14, height = 14)
ggsave(plot = p7_variable_correlation, filename = "p7_variable_correlation.png", path = "output", device = "png", width = 14, height = 14, dpi = 300)

# Only running ------------------------------------------------------------

#Add run_type
df_clean_running <- df_clean_log %>% 
  filter(activity == "Running") %>% 
  mutate(run_type = case_when(
    str_detect(name, regex("Tävling", ignore_case = T)) ~ "Race",
    str_detect(name, regex("Intervall", ignore_case = T)) ~ "Interval",
    str_detect(name, regex("Tempo", ignore_case = T))  ~ "Tempo",
    str_detect(name, regex("Distans", ignore_case = T))  ~ "No category",
    str_detect(name, regex("Långpass", ignore_case = T)) ~ "Long distance",
    TRUE ~ "No annotation")
  ) %>% 
  relocate(run_type)


# PCA on running data -----------------------------------------------------

df_pca <- prcomp(df_clean_running[,7:13], center = TRUE,scale. = TRUE)

df_out <- as.data.frame(df_pca$x)
df_out <- cbind(df_out, df_clean_running)


s <- summary(df_pca)
set_xlab=paste("PC 1 (", round(s$importance[2,1]*100, 1), "%)", sep = "")
set_ylab=paste("PC 2 (", round(s$importance[2,2]*100, 1), "%)", sep = "")


# Scores and loadings
df_out_r <- as.data.frame(df_pca$rotation)
df_out_r$variable <- row.names(df_out_r)
df_out_r$PC1 <- df_out_r$PC1*max(abs(df_out$PC1))
df_out_r$PC2 <- df_out_r$PC2*max(abs(df_out$PC2))


colors_run_type <- viridis(length(levels(factor(df_out$run_type))), option = "magma", begin = 0, end = 0.9)
names(colors_run_type) <- c("Race", "Interval", "Tempo", "No category", "Long distance", "No annotation")
set_color_run_type<- function(){
  scale_color_manual(values = colors_run_type)}
set_fill_run_type <- function(){
  scale_fill_manual(values = colors_run_type)}

p9_biplot_running <-ggplot(df_out,aes(x=PC1,y=PC2)) + 
  stat_ellipse(geom = "polygon",
               aes(col = run_type, fill = run_type),
               alpha = 0.1)+
  geom_point(aes(color=run_type), alpha = 0.75) +
  geom_segment(data = df_out_r, aes(xend=PC1, yend = PC2), x = 0, y=0, show.legend = FALSE,
               arrow = arrow(length = unit(0.5, "cm")))+
  geom_label_repel(data = df_out_r,aes(label=variable),
                   label.size = NA, 
                   alpha = 0.6, 
                   label.padding=.1, 
                   na.rm=TRUE,
                   seed = 1234) +
  geom_label_repel(data = df_out_r,aes(label=variable),
                   label.size = NA, 
                   alpha = 1, 
                   label.padding=.1, 
                   na.rm=TRUE,
                   fill = NA,
                   seed = 1234)+
  set_color_run_type()+
  set_fill_run_type()+
  xlab(set_xlab) + 
  ylab(set_ylab) +
  
  kp_theme()+
  labs(color = "",
       fill = "")+
  labs(title = "Running data",
       subtitle = "PCA scores and loadings")

ggsave(plot = p9_biplot_running, filename = "p9_biplot_running.pdf", path = "output", device = "pdf", width = 7, height = 5)
ggsave(plot = p9_biplot_running, filename = "p9_biplot_running.png", path = "output", device = "png", width = 7, height = 5, dpi = 300)



# Running activites, distribution of variables depending on run type -------

p10_running_variable_signal_log10 <- df_clean_running %>% 
  pivot_longer(!c(activity, activity_and_race, activity_type, name, date, run_type), names_to = "measurement", values_to = "value") %>% 
  ggplot(aes(x = reorder_within(run_type, value, measurement), y = value, fill = run_type, col = run_type))+
  geom_boxplot(aes(fill = run_type), outlier.shape = NA, alpha = 0.1)+
  facet_wrap(~measurement, scales = "free")+
  scale_x_reordered() + 
  geom_quasirandom(alpha = 0.5)+
  kp_theme()+
  theme(axis.text.x=element_text(angle=25,hjust=1,vjust = 1),
        legend.position="top")+
  labs(title = "Data distribution of the variables within run type",
       subtitle = "Run types are ordered by median within every variable",
       y = "",
       x = "",
       col = "",
       fill = "")+
  guides(color = guide_legend(nrow = 1),
         fill = guide_legend(nrow = 1))+
  set_color_run_type()+
  set_fill_run_type()

ggsave(plot = p10_running_variable_signal_log10, filename = "p10_running_variable_signal_log10.pdf", path = "output", device = "pdf", width = 7.5, height = 6.5)
ggsave(plot = p10_running_variable_signal_log10, filename = "p10_running_variable_signal_log10.png", path = "output", device = "png", width = 7.5, height = 6.5, dpi = 300)

# Correlation of variables, running ----------------------------------------

plot_regressions2 <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method=lm, ...)
  p
}

p_extra_1_running_variable_correlation <- ggpairs(df_clean_running,aes(col = run_type, fill = run_type, alpha = 0.7), columns = 7:13,
                   lower = list(continuous = plot_regressions2),
                   diag = list(continuous = "blankDiag"))+ 
  kp_theme()+
  scale_fill_brewer(palette = "Set1")+
  scale_color_brewer(palette = "Set1")

for (i in 2:p_extra_1_running_variable_correlation$nrow) {
  for (j in 1:(i-1)) {
    p_extra_1_running_variable_correlation[i,j] = p_extra_1_running_variable_correlation[i,j] + geom_abline(intercept=0,slope=1)
  }
}

ggsave(plot = p_extra_1_running_variable_correlation, filename = "p_extra_1_running_variable_correlation.pdf", path = "output", device = "pdf", width = 14, height = 14)
ggsave(plot = p_extra_1_running_variable_correlation, filename = "p_extra_1_running_variable_correlation.png", path = "output", device = "png", width = 14, height = 14, dpi = 300)


# save Rdata  -------------------------------------------------------------------

save(
  df_clean,
  file = "./output/data.Rdata"
)
