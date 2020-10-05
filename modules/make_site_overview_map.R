make_site_overview_map <- function(sDF) {
  
  ### read in prec data as background plot
  precDF <- read.table("input/prec_annual.txt", header=F)
  colnames(precDF) <- c("Lon", "Lat", "prec")
  
  precDF$prec_cat <- ifelse(precDF$prec <= 100, 1, 
                            ifelse(precDF$prec > 100 & precDF$prec <= 500, 2,
                                   ifelse(precDF$prec > 500 & precDF$prec <= 2000, 3, 
                                          ifelse(precDF$prec > 2000 & precDF$prec <= 4000, 4, 5))))
  
  ### plotting
  p1 <- ggplot() + 
    geom_tile(data=precDF, aes(y=Lat, x=Lon, fill=as.character(prec_cat))) +
    coord_quickmap(xlim=range(precDF$Lon), ylim=range(precDF$Lat))+
    borders("world", col="lightyellow") +
    geom_point(data=sDF, aes(y=lat2, x=lon2), col="black", fill="grey", size=1, pch=21)+
    geom_point(data=sDF, aes(y=lat3, x=lon3), col="black", fill="grey", size=1, pch=21)+
    geom_point(data=sDF, aes(y=lat4, x=lon4), col="black", fill="grey", size=1, pch=21)+
    geom_point(data=sDF, aes(y=lat5, x=lon5), col="black", fill="grey",  size=1, pch=21)+
    geom_point(data=sDF, aes(y=lat6, x=lon6), col="black", fill="grey",  size=1, pch=21)+
    geom_point(data=sDF, aes(y=lat7, x=lon7), col="black", fill="grey",  size=1, pch=21)+
    geom_point(data=sDF, aes(y=lat8, x=lon8), col="black", fill="grey",  size=1, pch=21)+
    geom_point(data=sDF, aes(y=lat9, x=lon9), col="black", fill="grey",  size=1, pch=21)+
    geom_point(data=sDF, aes(y=lat1, x=lon1), fill="black", size=1, pch=21)+
    geom_point(data=sDF, aes(y=slat, x=slon), fill="green", size=1, pch=21)+
    scale_fill_manual(name="Rainfall (mm/yr)", 
                      values=alpha(c("indianred4", "indianred1","thistle1", "skyblue", "blue"),0.2),
                      label=c("0-100", "100-500", "500-2000", "2000-4000", ">4000"))+
    scale_color_manual(name="Wet factor", 
                       values=c("indianred4", "indianred3", "indianred1","thistle1", 
                                "slateblue1","purple1",  "purple4"),
                       label=c("< -2", "-2 to -1", "-1 to -0.1", "-0.1 to 0.1", 
                               "0.1 to 1", "1 to 2", "> 2"))+
    theme(panel.grid.minor=element_blank(),
          axis.text.x=element_text(size=14),
          axis.title.x=element_text(size=16),
          axis.text.y=element_text(size=14),
          axis.title.y=element_text(size=16),
          legend.text=element_text(size=14),
          legend.title=element_text(size=16),
          panel.grid.major=element_blank(),
          legend.box = 'horizontal',
          legend.box.just = 'left',
          legend.background = element_rect(fill="white",
                                           size=0.5, linetype="solid", 
                                           colour ="black"),
          plot.title = element_text(size=14, face="bold.italic", 
                                    hjust = 0.5))+
    guides(fill=guide_legend(nrow=5), color=guide_legend(nrow=5))
  
  
  pdf("data/less_site_overview_map.pdf", width=8, height=4)
  plot(p1)
  dev.off()
  
}