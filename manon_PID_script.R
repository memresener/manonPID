
library(ggplot2)
library(plotly)

smooth_a<-0.001
filename="PID_120422_lab435_dev4A"

manon1<-read.csv(paste0("C:/Users/memre/Documents/Easter break raw data/",filename,".txt"))

manon1[,2]<-as.POSIXct(manon1[,2])
pid<-data.frame(date=manon1[,2],volt=manon1[,3])

lims=c(as.POSIXct(format(min(pid$date), format="%Y-%m-%d")),
       +        as.POSIXct(format(max(pid$date), format="%Y-%m-%d")))
minor_break=seq(lims[1],lims[2],6*60*60)

theme_set(theme_bw())

gp<-ggplot(pid,aes(x=date,y=volt,group = NA))+
  geom_smooth(method="loess",span=smooth_a)+
  scale_x_datetime(minor_breaks = minor_break, date_breaks = "1 day",date_labels = '%b %d')+
  theme(panel.grid.minor = element_line(size = 0.5), panel.grid.major = element_line(colour = "grey", size = 0.7))+
  labs(subtitle=filename,
       y="Output Voltage",
       x="Time",
       title="PID Output Voltage over Time")

A4<-ggplotly(gp, tooltip="text")
text_2 <-  paste0("Time:" , format(as.POSIXct(w$x$data[[1]]$x,origin="1970-01-01"),format="%H:%M"), "<br />", "Voltage: ", format(round(w$x$data[[1]]$y,4),nsmall=4))
A4 %>% 
  style(text = text_2, traces = 1)%>%
  style(hoverinfo = 'none',traces = c(2))%>%
  layout(title = list(text = paste0('PID Output Voltage over Time',
                                    '<br>',
                                    '<sup>',
                                    filename,
                                    '</sup>')))