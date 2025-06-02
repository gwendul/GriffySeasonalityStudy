# Griffy Lake Manuscript Figures

# Import Data
#setwd("W:/Projects/Griffy Woods Soil Fauna Project/Analyses")
setwd("W:/Lab/Franco Lab/Projects/Griffy Woods Soil Fauna Project/Analyses")
macrofauna <- read.csv("GW_SoilMacrofauna.csv")
nematode <- read.csv("GW_Nematode.csv")

# Packages Used
library(ggplot2)
library(ggrepel)
library(ggsci)
library(patchwork)
library(nlme)
library(vegan)
library(dplyr)
library(lme4)
library(ggeffects)

# Misc. classifying
macrofauna$sample.id <- as.factor(macrofauna$sample.id)
macrofauna$week <- as.factor(macrofauna$week)
macrofauna$sample.date <- as.Date(macrofauna$sample.date)
macrofauna$sample.type <- as.factor(macrofauna$sample.type)
macrofauna$mycorrhizal.fungi.type <- as.factor(macrofauna$mycorrhizal.fungi.type)
macrofauna$landscape.position <- as.factor(macrofauna$landscape.position)
macrofauna$tree.identity <- as.factor(macrofauna$tree.identity)
macrofauna$season <- as.factor(macrofauna$season)
macrofauna$season <- factor(macrofauna$season, levels = c("Winter.late", "Spring", "Summer", "Fall", "Winter.early"))

nematode$sample.id <- as.factor(nematode$sample.id)
nematode$week <- as.factor(nematode$week)
nematode$mycorrhizal.fungi.type <- as.factor(nematode$mycorrhizal.fungi.type)
nematode$landscape.position <- as.factor(nematode$landscape.position)
nematode$sample.date <- as.Date(nematode$sample.date, "%m/%d/%y")
nematode$tree.identity <- as.factor(nematode$tree.identity)
nematode$season <- as.factor(nematode$season)
nematode$season <- factor(nematode$season, levels = c("Winter.late", "Spring", "Summer", "Fall", "Winter.early"))
nematode$season.1 <- factor(nematode$season.1, levels = c("Spring", "Summer", "Fall", "Winter"))

## Nematode Groups
nematode$om.pr=nematode$omnivorous+nematode$predators
nematode$trophic.index=nematode$om.pr/(nematode$bacterial.feeders+nematode$fungal.feeders+nematode$plant.parasites.aph.)
nematode$lifestyle.index=nematode$plant.parasites.aph./(nematode$om.pr+nematode$bacterial.feeders+nematode$fungal.feeders)
nematode$channel.index=nematode$fungal.feeders/nematode$bacterial.feeders

# Merge Winter Season for Macrofauna
macrofauna <- macrofauna %>%
  mutate(season.1 = case_when(
    season == "Winter.early" ~ "Winter",
    season == "Winter.late" ~ "Winter",
    TRUE ~ as.factor(season)
  ))

macrofauna$season.1 <- factor(macrofauna$season.1, levels = c("Spring", "Summer", "Fall", "Winter"))

# Figure Theme
mytheme=theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),text=element_text(size=13),
        axis.text.x=element_text(angle=60,vjust=1,hjust=1, size = 13),
        axis.text.y=element_text(size = 13),
        panel.border = element_rect(colour = "black", fill = NA, size=1.25),
        axis.ticks = element_line(colour = "black", size = 1)
  )

### Figure 1 ###
# Panel 1: Nematode by date
model.total <-lme(total.nematodes~season+landscape.position+mycorrhizal.fungi.type+season:landscape.position+
                    mycorrhizal.fungi.type:landscape.position+season:mycorrhizal.fungi.type+
                    season:landscape.position:mycorrhizal.fungi.type, 
                  random=list(sample.id=~1, tree.identity=~1), data = nematode)
summary(model.total)
intervals(model.total, which = "fixed")

nematode$fit_total <- fitted(model.total)
nematode$fit_resid <- residuals(model.total)

figure1_panel1 <- ggplot(nematode, aes(x=sample.date, y=total.nematodes)) + 
  geom_jitter(size=3, aes(color = mycorrhizal.fungi.type, shape = landscape.position)) +
  scale_color_manual(values = c("#5773CCFF","#FFA319FF")) + 
  scale_x_date(date_labels = "%b %d", date_breaks = "2 weeks") +
  labs(x = "Date", y = expression(paste("Total Nematode Abundance (ind. ",Kg^-1, "dry soil)"))) +
  guides(color = guide_legend(title = "Tree Functional Type", order = 1), shape = guide_legend(title = "Topographic Position")) +
  mytheme +
  geom_smooth(aes(x= sample.date, y = fit_total), color = "black", linewidth = 2, method = 'lm', 
              se = TRUE, data = nematode, show.legend = FALSE) 
 
figure1_panel1

# Panel 2: box plot of topog and fungi interaction
figure1_panel2 <- ggplot(nematode, aes(x = landscape.position, y = total.nematodes)) +
  geom_boxplot(lwd=0.5, aes(fill=mycorrhizal.fungi.type), show.legend = FALSE) +
  scale_fill_manual(values = c("#5773CCFF","#FFA319FF")) +
  mytheme + 
  labs(y = expression(paste("Total Nematode Abundance (ind. ",Kg^-1, "dry soil)")), x= "Topographic Position") +
  guides(fill = guide_legend(title = "Tree Functional Type"))

figure1_panel2

# Figure 1 Full:
figure1_panel1 + figure1_panel2 + plot_annotation(tag_levels = 'A') + 
  plot_layout(widths = c(2, 1), axes = "collect", guides = "collect") 
ggsave("Figure1_NematodeTotalAbundance.tiff", plot= last_plot(), device = "tiff",
       units = "in", width = 12, height = 7, dpi = 300)



### Figure 2 ###
# Panel 1: Bacteria Feeders
figure2_panel1 <- ggplot(nematode, aes(x = season, y = bacterial.feeders)) +
  geom_boxplot(aes(fill = season.1), lwd=0.5, show.legend = FALSE) +
  scale_x_discrete(labels = c("Late Winter", "Spring", "Summer", "Fall", "Early Winter")) +
  scale_fill_futurama() + 
  mytheme +
  labs(y= "Nematode Abundance", x = "Season", subtitle = "Bacterial Feeders") 
figure2_panel1

# Panel 2: Fungal Feeders
figure2_panel2 <- ggplot(nematode, aes(x = season, y = fungal.feeders)) +
  geom_boxplot(aes(fill = season.1), lwd=0.5, show.legend = FALSE) +
  scale_x_discrete(labels = c("Late Winter", "Spring", "Summer", "Fall", "Early Winter")) +
  scale_fill_futurama() + 
  mytheme +
  labs(y= "Nematode Abundance", x = "Season", subtitle = "Fungal Feeders") 
figure2_panel2

# Panel 3: Root Feeders
figure2_panel3 <- ggplot(nematode, aes(x = season, y = plant.parasites.aph.)) +
  geom_boxplot(aes(fill = season.1), lwd=0.5, show.legend = FALSE) +
  scale_x_discrete(labels = c("Late Winter", "Spring", "Summer", "Fall", "Early Winter")) +
  scale_fill_futurama() + 
  mytheme +
  labs(y= "Nematode Abundance", x = "Season", subtitle = "Plant Parasites") 
figure2_panel3

# Panel 4: Omnivores + Predators
figure2_panel4 <- ggplot(nematode, aes(x = season, y = om.pr)) +
  geom_boxplot(aes(fill = season.1), lwd=0.5, show.legend = FALSE) +
  scale_x_discrete(labels = c("Late Winter", "Spring", "Summer", "Fall", "Early Winter")) +
  scale_fill_futurama() + 
  mytheme +
  labs(y= "Nematode Abundance", x = "Season", subtitle = "Omnivores and Predators") 
figure2_panel4

# Figure 2 Combined
figure2_panel1 + figure2_panel2 + figure2_panel3 + figure2_panel4 + 
  plot_annotation(caption = 'Nematodes are measured as ind. Kg\u207B\u00b9 dry soil', tag_levels = "A") +
  plot_layout(axes = "collect")
ggsave("Figure2_NematodeFeedingGuilds.tiff", plot= last_plot(), device = "tiff",
       units = "in", width = 12, height = 7, dpi = 300, pointsize = 5)


### Figure 3 ###
# Panel 1: Pred/Prey
figure3_panel1 <- ggplot(nematode, aes(x = season, y = trophic.index)) +
  geom_boxplot(aes(fill = season.1), lwd=0.5, show.legend = FALSE) +
  scale_x_discrete(labels = c("Late Winter", "Spring", "Summer", "Fall", "Early Winter")) +
  scale_fill_futurama() + 
  mytheme +
  labs(y= "Index Value", x = "Season", subtitle = "Predator-Prey Index") 

figure3_panel1

# Panel 2: Free Living vs Root Feeders
figure3_panel2 <- ggplot(nematode, aes(x = season, y = lifestyle.index)) +
  geom_boxplot(aes(fill = season.1), lwd=0.5, show.legend = FALSE) +
  scale_x_discrete(labels = c("Late Winter", "Spring", "Summer", "Fall", "Early Winter")) +
  scale_fill_futurama() + 
  mytheme +
  labs(y= "Index Value", x = "Season", subtitle = "Plant Parasite/Free Living Nematodes") 

figure3_panel2

# Panel 3: Channel
figure3_panel3 <- ggplot(nematode, aes(x = season, y = channel.index)) +
  geom_boxplot(aes(fill = season.1), lwd=0.5, show.legend = FALSE) +
  scale_x_discrete(labels = c("Late Winter", "Spring", "Summer", "Fall", "Early Winter")) +
  scale_fill_futurama() + 
  mytheme +
  labs(y= "Index Value", x = "Season", subtitle = "Channel Ratio") 

figure3_panel3

# Panel 4: Simpson
figure3_panel4 <- ggplot(nematode, aes(x = season, y = simpson)) +
  geom_boxplot(aes(fill = season.1), lwd=0.5, show.legend = FALSE) +
  scale_x_discrete(labels = c("Late Winter", "Spring", "Summer", "Fall", "Early Winter")) +
  scale_fill_futurama() + 
  mytheme +
  labs(subtitle = "Simpson Diversity Index", x= "Season", y = "Index Value")
figure3_panel4

# Figure 3 Combined
figure3_panel4 + figure3_panel3 + figure3_panel2 + figure3_panel1 + 
  plot_annotation(tag_levels = "A") +
  plot_layout(axes = "collect")
ggsave("Figure3_NematodeIndices.tiff", plot= last_plot(), device = "tiff",
       units = "in", width = 12, height = 7, dpi = 300)


### Figure 4 ###
relative_abundance <- function(matrix) {
  # Apply the function to each row
  new_matrix <- t(apply(matrix, 1, function(row) row / row[length(row)]*100))
  new_matrix <- new_matrix[, -ncol(new_matrix)]
  return(new_matrix)
}
nematode_matrix <- data.frame(nematode[c(10,11,13,15,16,18)], row.names = nematode$unique.id)
nematode_matrix.rel <- relative_abundance(nematode_matrix)
nematode_matrix.rel <- as.data.frame(nematode_matrix.rel)

Nematode_NMDS1=metaMDS(nematode_matrix.rel,k=4,trymax = 999)

nmds.sites <- as.data.frame(scores(Nematode_NMDS1)$sites) 
nmds.spp <- as.data.frame(scores(Nematode_NMDS1)$species) 

nmds.sites <- nmds.sites %>%
  mutate(Season = as.factor(nematode$season), 
         Topography = as.factor(nematode$landscape.position),
         Mycorrhizae = as.factor(nematode$mycorrhizal.fungi.type))

nmds.spp$TrophicGroup <- c("Bacterial Feeders", "Fungal Feeders", "Plant Parasites", "Omnivores", "Predators")


# Panel 1: Season
veganCovEllipse<-function (cov, center = c(0, 0), scale = 1, npoints = 100)
{
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}
season_ellipse_df <- data.frame()
for(g in levels(nmds.sites$Season)){
  season_ellipse_df <- rbind(season_ellipse_df, cbind(as.data.frame(with(nmds.sites[nmds.sites$Season==g,],
                                                                         veganCovEllipse(cov.wt(cbind(NMDS1,NMDS2),
                                                                                                wt=rep(1/length(NMDS1),length(NMDS1)))$cov,
                                                                                         center=c(mean(NMDS1),mean(NMDS2)))))
                                                      ,Season=g))
}

figure4_panel1 <- ggplot(data = nmds.sites, aes(NMDS1, NMDS2)) +
  geom_point(aes(color = Season, shape = Season), size= 3) +
  geom_path(data=season_ellipse_df, aes(x=NMDS1, y=NMDS2, color = Season),linewidth=2, show.legend = FALSE) +
  scale_color_manual(values = c("#FF95A8FF", "#FF6F00FF","#C71000FF", "#008EA0FF", "#8A4198FF"),
                     labels = c("Late Winter", "Spring", "Summer", "Fall", "Early Winter")) + 
  scale_shape_manual(values = c(15,16,17,18,15), labels = c("Late Winter", "Spring", "Summer", "Fall", "Early Winter"))+
  geom_text_repel(data = nmds.spp, aes(NMDS1, NMDS2), label = nmds.spp$TrophicGroup, cex =7, fontface= "bold") +
  guides(color = guide_legend(title = "Season")) + 
  theme_bw() + 
  mytheme + 
  theme(panel.grid = element_blank())  

figure4_panel1

# Panel 2: Landscape Position
veganCovEllipse<-function (cov, center = c(0, 0), scale = 1, npoints = 100)
{
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}
topog_ellipse_df <- data.frame()
for(g in levels(nmds.sites$Topography)){
  topog_ellipse_df <- rbind(topog_ellipse_df, cbind(as.data.frame(with(nmds.sites[nmds.sites$Topography==g,],
                                                                       veganCovEllipse(cov.wt(cbind(NMDS1,NMDS2),
                                                                                              wt=rep(1/length(NMDS1),length(NMDS1)))$cov,
                                                                                       center=c(mean(NMDS1),mean(NMDS2)))))
                                                    ,Topography=g))
}

figure4_panel2 <- ggplot(data = nmds.sites, aes(NMDS1, NMDS2)) +
  geom_point(aes(color = Topography, shape = Topography), size =3) +
  geom_path(data=topog_ellipse_df, aes(x=NMDS1, y=NMDS2, color = Topography), 
            linewidth=2, show.legend = FALSE) +
  scale_color_manual(name = "Topographic Position", labels = c("Downhill", "Uphill"), values = c("#5C88DAFF","#CC0C00FF")) + 
  scale_shape_manual(name = "Topographic Position", labels = c("Downhill", "Uphill"), values = c(19, 17)) + 
  geom_text_repel(data = nmds.spp, aes(NMDS1, NMDS2), label = nmds.spp$TrophicGroup, cex =7, fontface= "bold") +
 # guides(color = guide_legend(title = "Topographic Position"), shape = "none") +
  theme_bw() +
  mytheme + 
  theme(panel.grid = element_blank()) 

figure4_panel2

# Panel 3: Fungi Type
veganCovEllipse<-function (cov, center = c(0, 0), scale = 1, npoints = 100)
{
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}
mycor_ellipse_df <- data.frame()
# adding data for ellipse, in this case using distance as a grouping factor
for(g in levels(nmds.sites$Mycorrhizae)){
  mycor_ellipse_df <- rbind(mycor_ellipse_df, cbind(as.data.frame(with(nmds.sites[nmds.sites$Mycorrhizae==g,],
                                                                       veganCovEllipse(cov.wt(cbind(NMDS1,NMDS2),
                                                                                              wt=rep(1/length(NMDS1),length(NMDS1)))$cov,
                                                                                       center=c(mean(NMDS1),mean(NMDS2)))))
                                                    ,Mycorrhizae=g))
}

figure4_panel3 <- ggplot(data = nmds.sites, aes(NMDS1, NMDS2)) +
  geom_point(aes(color = Mycorrhizae, shape = Mycorrhizae), size= 3) +
  geom_path(data=mycor_ellipse_df, aes(x=NMDS1, y=NMDS2, color = Mycorrhizae),linewidth=2, show.legend = FALSE) +
  scale_color_manual(name = "Tree Functional Type", labels = c("AM", "ECM"), values = c("#5773CCFF","#FFA319FF")) + 
  scale_shape_manual(name = "Tree Functional Type", labels = c("AM", "ECM"), values = c(19, 17)) + 
  geom_text_repel(data = nmds.spp, aes(NMDS1, NMDS2), label = nmds.spp$TrophicGroup, cex =7, fontface= "bold") +
  #guides(shape = guide_legend(title = "Tree Functional Type"), color = "none") + 
  theme_bw() + 
  mytheme + 
  theme(panel.grid = element_blank())  

figure4_panel3

# Figure 4 full:
figure4_panel1 + (figure4_panel2 / figure4_panel3) + 
  plot_layout(axes = "collect", widths = c(3, 2)) +
  plot_annotation(tag_levels = 'A') 
ggsave("Figure4_NematodeCommunity.tiff", plot= last_plot(), device = "tiff",
       units = "in", width = 15, height = 8, dpi = 300)
  

### Figure 5 ###
# Macrofauna by Season
# Macrofauna by Landscape x Fungi
figure5_panel1 <- ggplot(macrofauna, aes(x = season, y = totalAbundance)) +
  geom_boxplot(aes(fill = season.1), lwd=0.5, show.legend = FALSE) +
  scale_x_discrete(labels = c("Late Winter", "Spring", "Summer", "Fall", "Early Winter")) +
  scale_fill_futurama() + 
  mytheme +
  labs(y= "Total Macrofauna Abundance", x = "Season") 

figure5_panel1

# Panel 2: box plot of topog and fungi interaction
figure5_panel2 <- ggplot(macrofauna, aes(x = mycorrhizal.fungi.type, y = totalAbundance)) +
  geom_boxplot(lwd=0.5, aes(fill=landscape.position)) +
  scale_fill_manual(values = c("#5C88DAFF","#CC0C00FF")) + 
  guides(fill = guide_legend(title = "Topographic Position", order = 1), shape = guide_legend(title = "Fungi Type")) +
  mytheme + 
  labs(y = "Total Macrofauna Abundance", x= "Tree Functional Type")
figure5_panel2
  
# Figure 5 Full:
figure5_panel1 + figure5_panel2 + plot_annotation(tag_levels = 'A') + 
   plot_layout(widths = c(2, 1), guides = "collect", axes = "collect")
ggsave("Figure5_MacrofaunaTotalAbundance.tiff", plot= last_plot(), device = "tiff",
       units = "in", width = 12, height = 7, dpi = 300)

### Figure 6 ###
# 2 panels w sample type
  # panel 1: total abundance
  # panel 2: shannon

figure6_panel1 <- ggplot(macrofauna, aes(x = sample.type, y = totalAbundance)) +
  geom_boxplot(aes(fill = sample.type), lwd=0.5, show.legend = FALSE) +
  scale_fill_manual(values = c("#008280FF", "#631879FF")) +
  scale_x_discrete(labels = c("Epigeic", "Endogeic")) +
  mytheme +
  labs(subtitle = "Total Abundance", x= "Ecological Group", y = "Total Macrofauna Abundance")
figure6_panel1

figure6_panel2 <- ggplot(macrofauna, aes(x = sample.type, y = shannon)) +
  geom_boxplot(aes(fill = sample.type), lwd=0.5, show.legend = FALSE) +
  scale_fill_manual(values = c("#008280FF", "#631879FF")) + 
  scale_x_discrete(labels = c("Epigeic", "Endogeic")) +
  mytheme +
  labs(subtitle = "Shannon Diversity", x= "Ecological Group", y = "Shannon Diversity Index Value")
figure6_panel2

figure6_panel1 + figure6_panel2 + plot_annotation(tag_levels = 'A') +
  plot_layout(axes = "collect")
ggsave("Figure6_MacrofaunaSampleType.tiff", plot= last_plot(), device = "tiff",
       units = "in", width = 12, height = 7, dpi = 300)

### Figure 7 ###
relative_abundance <- function(matrix) {
  # Apply the function to each row
  new_matrix <- t(apply(matrix, 1, function(row) row / row[length(row)]*100))
  new_matrix <- new_matrix[, -ncol(new_matrix)]
  return(new_matrix)
}
macrofauna_matrix <- data.frame(macrofauna[c(10:30,32)], row.names = macrofauna$unique.id)
macrofauna_matrix.rel <- relative_abundance(macrofauna_matrix)
macrofauna_matrix.rel <- as.data.frame(macrofauna_matrix.rel)
Macrofauna_NMDS1=metaMDS(macrofauna_matrix.rel,k=4,trymax = 999)

nmds.sites1 <- as.data.frame(scores(Macrofauna_NMDS1)$sites) 
nmds.spp1 <- as.data.frame(scores(Macrofauna_NMDS1)$species) 

nmds.sites1 <- nmds.sites1 %>%
  mutate(Season = as.factor(macrofauna$season), 
         Sample = as.factor(macrofauna$sample.type))

nmds.spp1$Taxa <- c("Acarina", "Araneida", "Coleoptera", "Collembola", "Diptera", "Enchytraeida", "Gastropoda",
                    "Hemiptera", "Hymenoptera", "Formicidae", "Isopoda", "Myriapoda", "Opisthopora", "Diplura",
                    "Dermaptera", "Lepidoptera", "Opiliones", "Blattodea", "Pseudoscorpiones", "Psocoptera", 
                    "Thysanoptera")

# Panel 1 - Macrofauna Community By Season
veganCovEllipse<-function (cov, center = c(0, 0), scale = 1, npoints = 100)
{
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}
season_ellipse_df1 <- data.frame()
for(g in levels(nmds.sites1$Season)){
  season_ellipse_df1 <- rbind(season_ellipse_df1, cbind(as.data.frame(with(nmds.sites1[nmds.sites1$Season==g,],
                                                                           veganCovEllipse(cov.wt(cbind(NMDS1,NMDS2),
                                                                                                  wt=rep(1/length(NMDS1),length(NMDS1)))$cov,
                                                                                           center=c(mean(NMDS1),mean(NMDS2)))))
                                                        ,Season=g))
}

figure7_panel1 <- ggplot(data = nmds.sites1, aes(NMDS1, NMDS2)) +
  geom_point(aes(color = Season, shape = Season), size= 4) +
  scale_color_manual(values = c("#FF95A8FF", "#FF6F00FF","#C71000FF", "#008EA0FF", "#8A4198FF"),
                     labels = c("Late Winter", "Spring", "Summer", "Fall", "Early Winter")) + 
  scale_shape_manual(values = c(15,16,17,18,15), labels = c("Late Winter", "Spring", "Summer", "Fall", "Early Winter"))+
  geom_path(data=season_ellipse_df1, aes(x=NMDS1, y=NMDS2, color = Season),linewidth=2, show.legend = FALSE) +
  geom_text_repel(data = nmds.spp1, aes(NMDS1, NMDS2), label = nmds.spp1$Taxa, cex =6, fontface= "bold") +
  theme_bw() +  # adding theme
  mytheme +
  theme(panel.grid = element_blank())  # remove background grid

figure7_panel1

# Panel 2 - Macrofauna Community By Sample Type
# define hidden vegan function that finds coordinates for drawing a covariance ellipse
veganCovEllipse<-function (cov, center = c(0, 0), scale = 1, npoints = 100)
{
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}

# mycorrhizae and topog ellipse
sample_ellipse_df1 <- data.frame()

# adding data for ellipse, in this case using distance as a grouping factor
for(g in levels(nmds.sites1$Sample)){
  sample_ellipse_df1 <- rbind(sample_ellipse_df1, cbind(as.data.frame(with(nmds.sites1[nmds.sites1$Sample==g,],
                                                                           veganCovEllipse(cov.wt(cbind(NMDS1,NMDS2),
                                                                                                  wt=rep(1/length(NMDS1),length(NMDS1)))$cov,
                                                                                           center=c(mean(NMDS1),mean(NMDS2)))))
                                                        ,Sample=g))
}

figure7_panel2 <- ggplot(data = nmds.sites1, aes(NMDS1, NMDS2)) +
  geom_point(aes(color = Sample, shape = Sample), size= 4) +
  geom_path(data=sample_ellipse_df1, aes(x=NMDS1, y=NMDS2, color = Sample),
            linewidth=2, show.legend = FALSE) +
  scale_color_manual(values = c("#008280FF", "#631879FF"), label= c("Epigeic", "Endogeic"), name= "Ecological Group") +
  scale_shape_discrete(label= c("Epigeic", "Endogeic"), name= "Ecological Group") +
  geom_text_repel(data = nmds.spp1, aes(NMDS1, NMDS2), label = nmds.spp1$Taxa, cex =6, fontface= "bold") +
  theme_bw() +  # adding theme
  mytheme +
  theme(panel.grid = element_blank())  # remove background grid

figure7_panel2

# Figure 7 Combined:
figure7_panel1 + figure7_panel2 + 
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = 'A')
ggsave("Figure7_MacrofaunaCommunity.tiff", plot= last_plot(), device = "tiff",
       units = "in", width = 15, height = 7, dpi = 300)

# Figure 8
## Nematode community with precip and air temp
relative_abundance <- function(matrix) {
  # Apply the function to each row
  new_matrix <- t(apply(matrix, 1, function(row) row / row[length(row)]*100))
  new_matrix <- new_matrix[, -ncol(new_matrix)]
  return(new_matrix)
}

extract.xyz <- function(obj) {
  xy <- expand.grid(x = obj$grid$x, y = obj$grid$y)
  xyz <- cbind(xy, c(obj$grid$z))
  names(xyz) <- c("x", "y", "z")
  return(xyz)
}

nematode_matrix <- data.frame(nematode[c(10,11,13,15,16,18)], row.names = nematode$unique.id)
nematode_matrix.rel <- relative_abundance(nematode_matrix)
nematode_matrix.rel <- as.data.frame(nematode_matrix.rel)

Nematode_NMDS1=metaMDS(nematode_matrix.rel,k=4,trymax = 999)

nmds.sites <- as.data.frame(scores(Nematode_NMDS1)$sites) 
nmds.spp <- as.data.frame(scores(Nematode_NMDS1)$species) 

nmds.sites <- nmds.sites %>%
  mutate(Season = as.factor(nematode$season), 
         Topography = as.factor(nematode$landscape.position),
         Mycorrhizae = as.factor(nematode$mycorrhizal.fungi.type),
         Precipitation = nematode$PRECIP,
         Temperature = nematode$AIRTEMP)

nmds.spp$TrophicGroup <- c("Bacterial Feeders", "Fungal Feeders", "Plant Parasites", "Omnivores", "Predators")

# Panel 1: Precipitation
plot(Nematode_NMDS1, type = "n")
points(Nematode_NMDS1, display = "sites", cex = 0.5, pch = 16, col = "grey")
ordisurf(Nematode_NMDS1, nematode$PRECIP, add = TRUE, lwd.cl= 3, labcex = 1)
text(Nematode_NMDS1, display = "species", cex = 1, col = "blue")

nematode.sf <- ordisurf(Nematode_NMDS1, nematode$PRECIP, plot = FALSE)
contour.precip <- extract.xyz(obj = nematode.sf)
contour.precip <- data.frame(na.omit(contour.precip))

ordisurf(Nematode_NMDS1, nematode$PRECIP, add = TRUE, lwd.cl= 3, labcex = 1)
#orditorp(Nematode_NMDS1,display="species",col="grey30",air=0.1,cex=1)

ordi <- ordisurf(Nematode_NMDS1 ~ nematode$PRECIP) #created the ordisurf object
ordi.grid <- ordi$grid #extracts the ordisurf object
str(ordi.grid) #it's a list though - cannot be plotted as is

ordi.nema <- expand.grid(x = ordi.grid$x, y = ordi.grid$y) #get x and ys
ordi.nema$z <- as.vector(ordi.grid$z) #unravel the matrix for the z scores
ordi.nema.na <- data.frame(na.omit(ordi.nema)) #gets rid of the nas
#ordi.nema.na #looks ready for plotting!

figure8_panel1 <- ggplot(data = contour.precip, aes(x,y, z = z)) + 
  geom_contour_filled(aes(fill = ..level..)) + 
  #scale_fill_gradient2(contour.air$z, low = "blue", high = "red")
  geom_point(data= nmds.sites, aes(NMDS1, NMDS2, z = NA), size = 2, color = "grey30") +
  geom_text_repel(data = nmds.spp, aes(NMDS1, NMDS2, z = NA), 
                  label = nmds.spp$TrophicGroup, cex =7, fontface= "bold", show.legend = FALSE) +
  #scale_fill_hue() +
  guides(fill = guide_legend(title = "Precipitation (mm)"), color = "none") +
  theme_bw() +
  mytheme +
  theme(panel.grid = element_blank()) 

figure8_panel1

# Panel 2: Air Temp
plot(Nematode_NMDS1, type = "n")
points(Nematode_NMDS1, display = "sites", cex = 0.5, pch = 16, col = "grey")
ordisurf(Nematode_NMDS1, nematode$AIRTEMP, add = TRUE, lwd.cl= 3, labcex = 1)
#text(nmds.spp, display = "TrophicGroup", cex = 1, col = "blue")
text(Nematode_NMDS1, display = "species", cex = 1, col = "blue")

nematode.sf1 <- ordisurf(Nematode_NMDS1, nematode$AIRTEMP, plot = FALSE)
contour.air <- extract.xyz(obj = nematode.sf1)
summary(nematode.sf1)

figure8_panel2 <- ggplot(data = contour.air, aes(x,y, z = z)) + 
  geom_contour_filled(aes(fill = ..level..)) + 
  #scale_fill_gradient2(contour.air$z, low = "blue", high = "red")
  geom_point(data= nmds.sites, aes(NMDS1, NMDS2, z = NA), size = 2, color = "grey30") +
  geom_text_repel(data = nmds.spp, aes(NMDS1, NMDS2, z = NA), 
                  label = nmds.spp$TrophicGroup, cex =7, fontface= "bold", show.legend = FALSE) +
  #scale_fill_hue() +
  guides(fill = guide_legend(title = "Air Temperature (\u00b0C)"), color = "none") +
  theme_bw() +
  mytheme +
  theme(panel.grid = element_blank()) 
figure8_panel2

figure8_panel1 + figure8_panel2  +
  plot_annotation(tag_levels = 'A') 
ggsave("SupplementalFigure3_NematodeCommunitybyClimate.tiff", plot= last_plot(), device = "tiff",
       units = "in", width = 15, height = 7, dpi = 300)

# Figure 9
## Macrofauna community with air temp and soil moisture
extract.xyz <- function(obj) {
  xy <- expand.grid(x = obj$grid$x, y = obj$grid$y)
  xyz <- cbind(xy, c(obj$grid$z))
  names(xyz) <- c("x", "y", "z")
  return(xyz)
}
relative_abundance <- function(matrix) {
  # Apply the function to each row
  new_matrix <- t(apply(matrix, 1, function(row) row / row[length(row)]*100))
  new_matrix <- new_matrix[, -ncol(new_matrix)]
  return(new_matrix)
}

macrofauna_matrix <- data.frame(macrofauna[c(10:30,32)], row.names = macrofauna$unique.id)
macrofauna_matrix.rel <- relative_abundance(macrofauna_matrix)
macrofauna_matrix.rel <- as.data.frame(macrofauna_matrix.rel)
Macrofauna_NMDS1=metaMDS(macrofauna_matrix.rel,k=4,trymax = 999)

nmds.sites1 <- as.data.frame(scores(Macrofauna_NMDS1)$sites) 
nmds.spp1 <- as.data.frame(scores(Macrofauna_NMDS1)$species) 

nmds.sites1 <- nmds.sites1 %>%
  mutate(Season = as.factor(macrofauna$season), 
         Sample = as.factor(macrofauna$sample.type))

nmds.spp1$Taxa <- c("Acarina", "Araneida", "Coleoptera", "Collembola", "Diptera", "Enchytraeida", "Gastropoda",
                    "Hemiptera", "Hymenoptera", "Formicidae", "Isopoda", "Myriapoda", "Opisthopora", "Diplura",
                    "Dermaptera", "Lepidoptera", "Opiliones", "Blattodea", "Pseudoscorpiones", "Psocoptera", 
                    "Thysanoptera")

# Panel 1: Precipitation
plot(Macrofauna_NMDS1, type = "n")
points(Macrofauna_NMDS1, display = "sites", cex = 0.5, pch = 16, col = "grey")
ordisurf(Macrofauna_NMDS1, macrofauna$AIRTEMP, add = TRUE, lwd.cl= 3, labcex = 1)
text(Macrofauna_NMDS1, display = "species", cex = 1, col = "blue")

macro.sf <- ordisurf(Macrofauna_NMDS1, macrofauna$PRECIP, plot = FALSE)
macro.precip <- extract.xyz(obj = macro.sf)
macro.precip <- data.frame(na.omit(macro.precip))

ordisurf(Macrofauna_NMDS1, macrofauna$PRECIP, add = TRUE, lwd.cl= 3, labcex = 1)
#orditorp(Nematode_NMDS1,display="species",col="grey30",air=0.1,cex=1)

ordisurf.macro <- ordisurf(Macrofauna_NMDS1 ~ macrofauna$PRECIP) #created the ordisurf object
ordi.grid.macro <- ordisurf.macro$grid #extracts the ordisurf object
str(ordi.grid.macro) #it's a list though - cannot be plotted as is

ordi.macro <- expand.grid(x = ordi.grid.macro$x, y = ordi.grid.macro$y) #get x and ys
ordi.macro$z <- as.vector(ordi.grid.macro$z) #unravel the matrix for the z scores
ordi.macro.na <- data.frame(na.omit(ordi.macro)) #gets rid of the nas
head(ordi.macro.na) #looks ready for plotting!

figure9_panel1 <- ggplot(data = macro.precip, aes(x,y, z = z)) + 
  geom_contour_filled(aes(fill = ..level..)) + 
  #scale_fill_gradient2(contour.air$z, low = "blue", high = "red")
  geom_point(data= nmds.sites1, aes(NMDS1, NMDS2, z = NA), size = 2, color = "grey30") +
  geom_text_repel(data = nmds.spp1, aes(NMDS1, NMDS2, z = NA), 
                  label = nmds.spp1$Taxa, cex =7, fontface= "bold", show.legend = FALSE) +
  #scale_fill_hue() +
  guides(fill = guide_legend(title = "Precipitation (mm)"), color = "none") +
  theme_bw() +
  mytheme +
  theme(panel.grid = element_blank()) 

figure9_panel1

# Panel 2: Soil Moisture
plot(Macrofauna_NMDS1, type = "n")
points(Macrofauna_NMDS1, display = "sites", cex = 0.5, pch = 16, col = "grey")
ordisurf(Macrofauna_NMDS1, macrofauna$soil.moisture, add = TRUE, lwd.cl= 3, labcex = 1)
text(Macrofauna_NMDS1, display = "species", cex = 1, col = "blue")

macro.sf1 <- ordisurf(Macrofauna_NMDS1, macrofauna$soil.moisture, plot = FALSE)
contour.moist <- extract.xyz(obj = macro.sf1)
summary(macro.sf1)

figure9_panel2 <- ggplot(data = contour.moist, aes(x,y, z = z)) + 
  geom_contour_filled(aes(fill = ..level..)) + 
  #scale_fill_gradient2(contour.air$z, low = "blue", high = "red")
  geom_point(data= nmds.sites1, aes(NMDS1, NMDS2, z = NA), size = 2, color = "grey30") +
  geom_text_repel(data = nmds.spp1, aes(NMDS1, NMDS2, z = NA), 
                  label = nmds.spp1$Taxa, cex =7, fontface= "bold", show.legend = FALSE) +
  #scale_fill_hue() +
  guides(fill = guide_legend(title = "Soil Moisture (%/g soil)"), color = "none") +
  theme_bw() +
  mytheme +
  theme(panel.grid = element_blank()) 
figure9_panel2

figure9_panel1 + figure9_panel2  +
  plot_annotation(tag_levels = 'A') 
ggsave("SupplementalFigure4_MacrofaunaCommunitybyClimate.tiff", plot= last_plot(), device = "tiff",
       units = "in", width = 15, height = 7, dpi = 300)

#### Supplementary ####
# Figure 1: Root Feeding Nematode Interaction with landscape and fungi
suppfig1 <- ggplot(nematode, aes(x = mycorrhizal.fungi.type, y = plant.parasites.aph.)) +
  geom_boxplot(lwd=0.5, aes(fill=landscape.position)) +
  scale_fill_manual(values = c("#5C88DAFF","#CC0C00FF")) + 
  guides(fill = guide_legend(title = "Topographic Position", order = 1), shape = guide_legend(title = "Fungi Type")) +
  mytheme + 
  labs(y = "Nematode Abundance", x= "Tree Functional Type", subtitle = "Plant Parasitic Nematodes")
suppfig1

# Figure 2: Omnivore and Pred nematodes landscape
suppfig2 <- ggplot(nematode, aes(x = mycorrhizal.fungi.type, y = om.pr)) +
  geom_boxplot(lwd=0.5, aes(fill=landscape.position)) +
  scale_fill_manual(values = c("#5C88DAFF","#CC0C00FF")) + 
  guides(fill = guide_legend(title = "Topographic Position", order = 1), shape = guide_legend(title = "Fungi Type")) +
  mytheme + 
  labs(y = "Nematode Abundance", x= "Tree Functional Type", subtitle = "Omnivorous and Predatory Nematodes")
suppfig2

# Figure 3: Lifestyle Index: landscape and fungi
suppfig3 <- ggplot(nematode, aes(x = mycorrhizal.fungi.type, y = lifestyle.index)) +
  geom_boxplot(lwd=0.5, aes(fill=landscape.position)) +
  scale_fill_manual(values = c("#5C88DAFF","#CC0C00FF")) + 
  guides(fill = guide_legend(title = "Topographic Position", order = 1), shape = guide_legend(title = "Fungi Type")) +
  mytheme + 
  labs(y = "Index Value", x= "Tree Functional Type", subtitle = "Plant Parasite/Free Living Nematodes")
suppfig3

# Figure 4: Simpsons fungi nematode
suppfig4 <- ggplot(nematode, aes(x = mycorrhizal.fungi.type, y = simpson)) +
  geom_boxplot(lwd=0.5, aes(fill=mycorrhizal.fungi.type)) +
  scale_fill_manual(values = c("#5773CCFF","#FFA319FF")) + 
  guides(fill = guide_legend(title = "Tree Func. Type", order = 1), shape = guide_legend(title = "Tree Func. Type")) +
  mytheme + 
  labs(y = "Index Value", x= "Tree Functional Type", subtitle = "Nematode Diversity (Simpson)")
suppfig4

suppfig1 + suppfig2 + suppfig3 + suppfig4 + 
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = 'A') 
ggsave("SupplementaryFigure2_NematodeTrends.tiff", plot= last_plot(), device = "tiff",
       units = "in", width = 12, height = 7, dpi = 300)

# Figure 5: Climate variables over time w season breaks
clim1 <- ggplot(nematode, aes(x=sample.date, y=soil.moisture)) +
  geom_area(fill = "cornflowerblue") +
  mytheme + 
  labs(y = "Moisture (%/g soil)", x= "Sample Date", subtitle = "Soil Moisture")
clim1

clim2 <- ggplot(nematode, aes(x=sample.date, y=PRECIP)) +
  geom_area(fill = "cornflowerblue") +
  mytheme + 
  labs(y = "Precipitation (mm)", x= "Sample Date", subtitle = "Precipitation")
clim2

clim3 <- ggplot(nematode, aes(x=sample.date, y=SOILTEMP)) +
  geom_area(fill = "cornflowerblue") +
  mytheme + 
  labs(y = "Temperature (\u00b0C)", x= "Sample Date", subtitle = "Soil Temperature")
clim3

clim4 <- ggplot(nematode, aes(x=sample.date, y=AIRTEMP)) +
  geom_area(fill = "cornflowerblue") +
  mytheme + 
  labs(y = "Temperature (\u00b0C)", x= "Sample Date", subtitle = "Air Temperature")
clim4

clim1 + clim2 + clim3 + clim4 + 
  plot_layout(axes = "collect_x") +
  plot_annotation(tag_levels = 'A') 
ggsave("SupplementaryFigure1_ClimateVariables.tiff", plot= last_plot(), device = "tiff",
       units = "in", width = 12, height = 7, dpi = 300)
