paste("Il est content",paste(c(1,2,3),collapse = ", ")," ")

g=fnetwork("Earnings before operating expenses and income tax",banks_names)
g$important
grepl(pattern = "expenses(operating expenses)",tolower("Amortization(Operating expenses)") )
################################3333
fdf_ratios()[[1]][,"BNC"]
#################################
Gauge <-  googleVis::gvisGauge(df1[1,"BNC"], 
                    options=list(min=0, max=20000, greenFrom=0,
                                 greenTo=5000, yellowFrom=5000, yellowTo=15000,
                                 redFrom=15000, redTo=20000, width=800, height=800))
#
plot(Gauge)
gauge=googleVis::gvisGauge(data.frame(data.frame(fdf_ratios()[[1]])[1,c("BNC")]) )
p<-plot(gauge);p

########################
gauge=data.frame(motnh=c("jan","feb","march","april","may","june"
                         ,"july","augost","sept"),percent_s=c(-0.1,0,0.03,-0.06,-0.15,-0.39
                                                              ,0.36,-0.53,-0.68))

gauge_plot=googleVis::gvisGauge(gauge[1,], 
                     options=list(min=-1, max=1, redFrom=-1,
                                  redTo=-0.3, yellowFrom=-0.3, yellowTo=0.3,
                                  greenFrom=0.3, greenTo=1, width=800, 
                                  height=600))
plot(gauge_plot)
##########
dadou=data.frame(fdf_ratios()[[1]])%>%
  t()%>%as.data.frame()%>%
  mutate(Banks=banks_names);dadou
  
  
gauge<-googleVis::gvisGauge(dadou[2,c("Banks","2001-12-01")],labelvar = "Banks",
                 options=list(min=0, max=1, redFrom=0,
                 redTo=0.3, yellowFrom=0.3, yellowTo=0.5,
                 greenFrom=0.5, greenTo=1, width=800, 
                 height=600))                    ;plot(gauge)

plot(gauge_plot ) 
