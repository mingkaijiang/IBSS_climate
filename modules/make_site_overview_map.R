make_site_overview_map <- function(sDF) {
  
  ### read in prec data as background plot
  precDF <- read.table("input/prec_annual.txt", header=F)
  colnames(precDF) <- c("Lon", "Lat", "prec")
  
  precDF$prec_cat <- ifelse(precDF$prec <= 100, 1, 
                            ifelse(precDF$prec > 100 & precDF$prec <= 500, 2,
                                   ifelse(precDF$prec > 500 & precDF$prec <= 2000, 3, 
                                          ifelse(precDF$prec > 2000 & precDF$prec <= 4000, 4, 5))))
  
  ### read in high god presence absence information
  godDF <- read.csv("input/HighGodVariablesForMap.csv")
  
  ### merge sDF and godDF
  plotDF <- merge(sDF, godDF, by.x="sccs_id", by.y="SCCS_ID")
  
  subDF <- subset(plotDF, G1_AbsentOrNA_Associated %in%c(1,2))
  
  test1 <- data.frame(subDF$lat1, subDF$lon1)
  test2 <- data.frame(subDF$lat2, subDF$lon2)
  test3 <- data.frame(subDF$lat3, subDF$lon3)
  test4 <- data.frame(subDF$lat4, subDF$lon4)
  test5 <- data.frame(subDF$lat5, subDF$lon5)

  
  colnames(test1) <- colnames(test2) <- colnames(test3) <- colnames(test4) <- colnames(test5) <- c("lat", "lon")
  
  test1$lab <- "2_primary"
  test10 <- rbind(test2, test3, test4, test5)
  test10$lab <- "1_secondary"
  
  ghcnDF <- rbind(test10,test1)
  
  
  
  ### plotting
  p1 <- ggplot() + 
    geom_tile(data=precDF, aes(y=Lat, x=Lon, fill=as.character(prec_cat))) +
    coord_quickmap(xlim=range(precDF$Lon), ylim=range(precDF$Lat))+
    borders("world", col="black", lwd=0.2) +
    geom_point(data=ghcnDF, aes(y=lat, x=lon, col=lab), size=1, pch=19)+
    geom_point(data=subDF, aes(y=slat, x=slon), 
               col="black", size=1, pch=19)+
    scale_fill_manual(name="Rainfall (mm/yr)", 
                      values=alpha(c("indianred4", "indianred1","thistle1", "skyblue", "blue"),0.2),
                      label=c("0-100", "100-500", "500-2000", "2000-4000", ">4000"))+
    scale_color_manual(name="GHCN station", 
                       values=c("orange", "purple"),
                       label=c("secondary", "primary"))+
    scale_shape_manual(name="SCCS site",
                       values=c(4,3,19),
                       labels=c("High God absence/Not codable",
                                "High God presence, not associated with weather",
                                "High God presence, associated with weather"))+
    theme(panel.grid.minor=element_blank(),
          axis.text.x=element_text(size=14),
          axis.title.x=element_text(size=16),
          axis.text.y=element_text(size=14),
          axis.title.y=element_text(size=16),
          legend.text=element_text(size=10),
          legend.title=element_text(size=12),
          panel.grid.major=element_blank(),
          legend.box = 'vertical',
          legend.box.just = 'left',
          legend.position = "bottom",
          legend.background = element_rect(fill="white",
                                           size=0.5, linetype="solid", 
                                           colour ="white"),
          plot.title = element_text(size=14, face="bold.italic", 
                                    hjust = 0.5))+
    guides(fill=guide_legend(nrow=1), color=guide_legend(nrow=1), shape=guide_legend(nrow=2))
  
  
  pdf("data/Figure_S3.pdf", width=8, height=5)
  plot(p1)
  dev.off()
  
  
}