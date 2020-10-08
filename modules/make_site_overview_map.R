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
  
  test1 <- data.frame(plotDF$lat1, plotDF$lon1)
  test2 <- data.frame(plotDF$lat2, plotDF$lon2)
  test3 <- data.frame(plotDF$lat3, plotDF$lon3)
  test4 <- data.frame(plotDF$lat4, plotDF$lon4)
  test5 <- data.frame(plotDF$lat5, plotDF$lon5)
  test6 <- data.frame(plotDF$lat6, plotDF$lon6)
  test7 <- data.frame(plotDF$lat7, plotDF$lon7)
  test8 <- data.frame(plotDF$lat8, plotDF$lon8)
  test9 <- data.frame(plotDF$lat9, plotDF$lon9)
  
  colnames(test1) <- colnames(test2) <- colnames(test3) <- colnames(test4) <- colnames(test5) <- colnames(test6) <- colnames(test7) <- colnames(test8) <- colnames(test9) <- c("lat", "lon")
  
  test1$lab <- "primary"
  test10 <- rbind(test2, test3, test4, test5, test6, test7, test8, test9)
  test10$lab <- "secondary"
  
  ghcnDF <- rbind(test1, test10)
  
  ### plotting
  p1 <- ggplot() + 
    geom_tile(data=precDF, aes(y=Lat, x=Lon, fill=as.character(prec_cat))) +
    coord_quickmap(xlim=range(precDF$Lon), ylim=range(precDF$Lat))+
    borders("world", col="black", lwd=0.2) +
    geom_point(data=ghcnDF, aes(y=lat, x=lon, col=lab), size=1, pch=19)+
    #geom_point(data=plotDF, aes(y=lat3, x=lon3), col="orange", size=1, pch=19)+
    #geom_point(data=plotDF, aes(y=lat4, x=lon4), col="orange", size=1, pch=19)+
    #geom_point(data=plotDF, aes(y=lat5, x=lon5), col="orange",  size=1, pch=19)+
    #geom_point(data=plotDF, aes(y=lat6, x=lon6), col="orange",  size=1, pch=19)+
    #geom_point(data=plotDF, aes(y=lat7, x=lon7), col="orange",  size=1, pch=19)+
    #geom_point(data=plotDF, aes(y=lat8, x=lon8), col="orange",  size=1, pch=19)+
    #geom_point(data=plotDF, aes(y=lat9, x=lon9), col="orange",  size=1, pch=19)+
    #geom_point(data=plotDF, aes(y=lat1, x=lon1), col="purple", size=1, pch=19)+
    geom_point(data=plotDF, aes(y=slat, x=slon, pch=as.character(G1_AbsentOrNA_Associated)), col="black", size=1)+
    scale_fill_manual(name="Rainfall (mm/yr)", 
                      values=alpha(c("indianred4", "indianred1","thistle1", "skyblue", "blue"),0.2),
                      label=c("0-100", "100-500", "500-2000", "2000-4000", ">4000"))+
    scale_color_manual(name="GHCN station", 
                       values=c("purple", "orange"),
                       label=c("primary", "secondary"))+
    scale_shape_manual(name="SCCS site",
                         values=c(4,17,19),
                         labels=c("Not codable",
                                  "Not associated with weather",
                                  "Associated with weather"))+
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
    guides(fill=guide_legend(nrow=1), color=guide_legend(nrow=1), shape=guide_legend(nrow=1))
  
  
  pdf("data/complete_site_overview_map.pdf", width=8, height=5)
  plot(p1)
  dev.off()
  
}