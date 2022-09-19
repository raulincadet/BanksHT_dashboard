#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(dygraphs)
library(forcats)
library(igraph)
library(googleVis)
library(htmltools)
# js <- '.nav-tabs-custom .nav-tabs li.active {
#     border-top-color: blue;
# }"' # to change color
##########################333
gtheme<-function(...){ # a theme gor ggplot2 graphics
  theme_classic()+
    theme(axis.ticks = element_blank(),
          axis.line=element_blank(),
          axis.text = element_text(colour="black"),
          plot.margin = margin(1,1,1,3)
    )+
    theme(legend.position = "none", legend.direction = "horizontal")
  
}
#############33
fgraph_xts<-function(ind,bk){
  require(dplyr)
  da=unlist(df1%>%filter(Indicators_en==ind)%>%select(Date))
  names(da)=unlist(df1%>%filter(Indicators_en==ind)%>%select(Date1))
  xts::xts(df1%>%filter(Indicators_en==ind)%>%select(bk),
           order.by = as.Date(da))
}
#View(cbind(fgraph_xts("Allowance for bad debts",banks_names)))
##############
fdecompose_xts<-function(ind,bk){
  require(dplyr)
  da=unlist(df1%>%filter(Indicators_en==ind)%>%select(Date))
  names(da)=c(df1%>%filter(Indicators_en==ind)%>%select(Date1))
  ####################################
  start=c(integer(unlist(stringr::str_split(min(da),"-"))[1]),integer(unlist(stringr::str_split(min(da),"-"))[2]))
  
  trying=try((ts(df1%>%filter(Indicators_en==ind)%>%select(bk),
                 start = start,frequency = 4 )%>%decompose())$seasonal[1:4])
  if (class(trying) == "try-error"){
    df=data.frame(c(0.01,0.01,0.01,0.01))
    colnames(df)=bk
    je=xts::xts(df%>%select(bk),order.by = as.Date(c("0000-03-01","0000-06-01","0000-09-01","0000-12-01")  ))
  }else{
    y=(ts(df1%>%filter(Indicators_en==ind)%>%select(bk),
          start = start,frequency = 4 )%>%decompose())$seasonal[1:4]
    seas=data.frame(y);colnames(seas)=bk
    
    je=xts::xts(seas%>%select(bk),order.by =as.Date(c("0000-03-01","0000-06-01","0000-09-01","0000-12-01") ))
    
  }
  
  return(je)
  
  
}


fdecomp<-function(Indic,Bk){
  y=NULL
  for (i in 1:length(Bk)) {
    y[[i]]=fdecompose_xts(Indic,Bk[i])
    
  }
  df=data.frame(y)
  return(df)
}
###################3
fstats<-function(Indic,Bk){# growth rate for 
  growth=function(x){
    y=NULL
    for (i in 1:length(x)) {
      y[i]=(x[i]-x[i-1])/x[i-1]
    }
    y
  }
  ############
  z=NULL
  for(i in 1:length(Bk)){
    z[[i]]=unlist(df1%>%filter(Indicators_en==Indic)%>%select(Bk[i]))
  }
  yy=rowSums(data.frame(z))
  g=growth(yy)*100
  s=sd(yy,na.rm = T) # standard deviation of the sum of the indicator for all selected banks
  mg=mean(g,na.rm=T)  # average growth of rate of the sum of th selected indicator for all selected banks
  m=(mean(unlist(z),na.rm=T))/1000 # mean of values of selected indicator for all selected banks
  dd=data.frame(Sd=s,MeanG=mg,Mean=m)
  return(dd)
}
##########################3
findic_xts<-function(ind,bk){# function to create a list of time series by bank selected for an indicator
  require(dplyr)
  da=unlist(df1%>%filter(Indicators_en==ind)%>%select(Date))
  names(da)=c(df1%>%filter(Indicators_en==ind)%>%select(Date1))
  xts::xts(df1%>%filter(Indicators_en==ind)%>%select(bk),
           order.by = as.Date(da))
}
##################################
# ratios_names=c("Non performing loan ratio","ROE","ROA","Efficiency ratio","Equity to asset ratio",
#                "Liability to equity ratio")
# listratios=1:length(ratios_names)
# names(listratios)=ratios_names
correlat_pairs<-function(ind,bk,Ratios=NULL){ # function to calculate correlation between pairs of banks
  if(is.null(Ratios)==T){
    df=cbind.data.frame(findic_xts(ind,bk=bk))
  }else{
    df=cbind.data.frame(fdf_ratios()[[listratios[ind]]])[,bk]
    
      
  }
  
  zo=NULL
  for (i in colnames(df)) { # to look for columns with only NA
    zo[i]=!is.na(df[,i])
  }
  df=df[,zo] # to remove banks with only NA as values
  
  bk_pairs=data.frame(t(data.frame(combn(colnames(df),2))))
  corr=NULL;pval=NULL
  for(i in 1:nrow(bk_pairs)){
    corr[i]=(cor.test(df[,bk_pairs[i,1]],df[,bk_pairs[i,2]]))$estimate
    pval[i]=(cor.test(df[,bk_pairs[i,1]],df[,bk_pairs[i,2]]))$p.value
  }
  bk_pairs$corr=corr
  bk_pairs$pval=pval
  
  bk_pairs
  
}
# correlat_pairs("ROE",c("BNC","BUH","SOGEBK"),Ratios = "y")
#################graph_from_data_frame(d, directed = TRUE, vertices = NULL)
fnetwork<-function(ind,bk,Ratios=NULL){
  df_net=correlat_pairs(ind,bk,Ratios=Ratios)
  df_net=df_net%>%filter(pval<0.05)%>%
    filter(corr>0.5)%>%
    mutate(color=ifelse(corr>=0,"forestgreen","red"))%>%
    mutate(weight=corr*(1-pval))%>%
     filter(weight>=0) # selected only positive weight, then positive correlation
  gogo=igraph::graph_from_data_frame(df_net, directed = FALSE, vertices = NULL)
  
  do=data.frame(as.vector(igraph::V(gogo)),igraph::degree(gogo))
  # import=do[do[,2]==max(do[,2],na.rm = T),][,1] # most important bank
  deg=data.frame(degree(gogo))
  colnames(deg)="Degree"
  deg$label=rownames(deg)
  import=deg[deg[,1]==max(deg[,1]),"label"] # most important bank
  least=deg[deg[,1]==min(deg[,1]),"label"] # less important bank
  robust=ifelse(is_connected(gogo,mode="strong")==TRUE,"strong connections",
         ifelse(is_connected(gogo,mode="weak")==TRUE,"weak connections","several components"))   
  components=igraph::count_components(gogo)
  ##############
  fimport=function(){ # function to add a text instead of list of banks if all have the same number of connections
    y=NULL
    for (i in 1:length(import)) {
      if(length(import)==nrow(deg)){
        y[i]="indeed, all the banks of the network have the same number of connections "
      }else{
        y[i]=import[i]
        }
    }
    return(unique(y))
  }
   
  import1=fimport()
  ##############
  fleast=function(){ # function to add a text instead of list of banks if all have the same number of connections
    y=NULL
    for (i in 1:length(least)) {
      if(length(least)==nrow(deg)){
        y[i]="indeed, none of the banks of the network, since they have the same number of connections. "
      }else{
        y[i]=least[i]
      }
    }
    return(unique(y))
  }
  
  least1=fleast()
  #############################
  z=list(graph=gogo,important=import1,components=components,robust=robust,leastImport=least1)
    
  return(z)
  }
###################
 fgraph_network<-function(gogo,annotate.plot=T,...){ # x is an igraph object. this function should plot it
   igraph::plot.igraph(gogo,
                            layout=igraph::layout.kamada.kawai(gogo),
                            # layout=igraph::layout.circle(gogo),
                            # layout=igraph::layout.davidson.harel(gogo),
                            #edge.curved=TRUE,
                            edge.dist=5,
                            label.cex=4,
                            label.color="red",
                            # edge.arrow.size=.4, # taille des liens
                            vertex.size=24, #t= taille des noeuds
                            # vertex.color=nodes$nodes_color,
                            vertex.color="lightblue",
                            vertex.label.color="black",
                            vertex.frame.color="gray",
                            #edge.label=edges$sign,
                            edge.label.color="red",
                            # edge.arrow.size=0.25,
                            # edge.color=igraph::edge.attributes(gogo)$color
                            # edge.color=edges$sign,
   )
   # legend(x=-1.2, y=-0.98, c("Positive correlation",
   #                           "Negative correlation"), pch=22,
   #        col="#777777", pt.bg=c("forestgreen","orangered"), pt.cex=0.95, cex=.8, bty="n", ncol=1)
   
 } 
################################
# unique(df1$Indicators_en)
# fratios<-function(ind1,ind2){
#   
# }
foperatEx=function(){ # function to retrieve name of elements of operating expenses
  z=unique(df1$Indicators_en)
  y=NULL
  for (i in 1:length(z)) {
    x=tolower(z[i])
    #x=stringi::stri_split(x,regex=" ")
    y[[i]]=grepl(pattern = "(Operating expenses)",z[i])
  }
  #names(y)=z
  indicOp=z[unlist(y)] # vector of names of operating expenses elements
  opex=NULL
  for (j in indicOp) {
    opex[[j]]=fgraph_xts(j,banks_names)
  }
  opex # list of cbind data frame of xts variables
  op_ex=opex[[1]]
  for(h in 2:length(indicOp)){
    op_ex=op_ex+opex[[h]]
  }
  op_ex  # total of operating expenses
}

###############
fdf_ratios<-function(){ # these results have been verified to ensure that the code is correct to calcule the ratios
  npl_on_loans=fgraph_xts("Non performing loan",banks_names)/fgraph_xts("Loan portfolio",banks_names)*100
  profit_on_equity=fgraph_xts("Net profit (loss)",banks_names)/fgraph_xts("Equity",banks_names)*100
  profit_on_assets=fgraph_xts("Net profit (loss)",banks_names)/fgraph_xts("Asset",banks_names)  *100
  operatingExp_on_netBankingInc=foperatEx()/fgraph_xts("Net banking income",banks_names)*100 # Cost to Income Ratio
  equity_on_assets=fgraph_xts("Equity",banks_names)/fgraph_xts("Asset",banks_names)  *100
  # exchangeGains_on_NetBankingIncome=fgraph_xts("Foreign exchange gains(Other income)" ,banks_names)/fgraph_xts("Net banking income",banks_names)  
  liability_on_equity=fgraph_xts("Liability",banks_names)/fgraph_xts("Equity",banks_names) *100
  
  return(list(`NPL ratio`=npl_on_loans,
              ROE=profit_on_equity,
              ROA=profit_on_assets,
              `Efficiency ratio`=operatingExp_on_netBankingInc,
              `Equity to asset ratio`=equity_on_assets,
              `Liability to equity ratio`=liability_on_equity))
  }
###
# dadou<-data.frame(fdf_ratios()[[1]])%>%
#   t()%>%as.data.frame()%>%
#   mutate(Banks=banks_names)
################
# function to define limits of colors in gauge; x is the data frame such as df_ROA
seq_gauge<-function(x,inverse=FALSE){
  zz=data.frame(t(x[-ncol(x)]))[,"System"]
  lower_bound=quantile(zz,0.25)
  upper_bound=quantile(zz,0.975)
  zou=NULL
  for (i in 1:length(zz)) {
    if(zz[i]>lower_bound){
      if(zz[i]<upper_bound){
        zou[i]=zz[i]
      }
    }
  }
  zou=na.omit(zou)
  # y=(max(zou,na.rm = T)-min(zou,na.rm = T))/3
  # s=seq(min(zou,na.rm = T),max(zou,na.rm = T),by=y)
  percentPerc=c(0,0.33,0.67,1)
  s=NULL
  for (i in seq_along(percentPerc)) {
    s[i]=quantile(zou,na.rm = T,percentPerc[i])
  }
  if(inverse==TRUE){
    return(
      list(
           colors="['#109618', '#FF9900','#DC3912']",min=s[1], max=s[4], redFrom=s[1],
           redTo=s[2], yellowFrom=s[2], yellowTo=s[3],
           greenFrom=s[3], greenTo=s[4], width=185,
           height=120,greenColor='#DC3912',yellowColor='#FF9900',
           redColor='#109618'))#"['#c69c6e','#603913','#cbb69d']"))
  }else{
    return(list(min=s[1], max=s[4], redFrom=s[1],
                redTo=s[2], yellowFrom=s[2], yellowTo=s[3],
                greenFrom=s[3], greenTo=s[4], width=185,
                height=120))
  }
  
}



################################
#cor(data.frame(fdecomp("Asset",c("BNC","BUH","UNIBNK","SOGEBK","BPH","CAPITALBK"))))

########################33
# fdecompose_xts("Equity","BUH")
# fdecompose_xts("Equity","BIDC")           
# ##########################################
load("banksHT.rda")
indicators_names=unique(df1$Indicators_en)
banks_names=colnames(df1)[!colnames(df1)%in% c("Indicators","Indicators_en","Date","Date1","Parts1_fr","Parts2_fr","Parts1_en","Parts2_en")]
dates_list=as.Date(unique(df1$Date))
names(dates_list)<-as.yearqtr(dates_list)
###################################################33
df_NPL_ratio=data.frame(fdf_ratios()[[1]])%>%
  t()%>%as.data.frame()%>%
  mutate(Banks=banks_names)
#
df_ROE=data.frame(fdf_ratios()[[2]])%>%
  t()%>%as.data.frame()%>%
  mutate(Banks=banks_names)
#
#
df_ROA=data.frame(fdf_ratios()[[3]])%>%
  t()%>%as.data.frame()%>%
  mutate(Banks=banks_names)
#
df_efficiency=data.frame(fdf_ratios()[[4]])%>%
  t()%>%as.data.frame()%>%
  mutate(Banks=banks_names)
#
df_leverage=data.frame(fdf_ratios()[[5]])%>%
  t()%>%as.data.frame()%>%
  mutate(Banks=banks_names)
#
df_libilityEquity=data.frame(fdf_ratios()[[6]])%>%
  t()%>%as.data.frame()%>%
  mutate(Banks=banks_names)
###########
ratios_names=c("Non performing loan ratio","ROE","ROA","Efficiency ratio","Equity to asset ratio",
               "Liability to equity ratio")
listratios=1:length(ratios_names)
names(listratios)=ratios_names
# listratios["ROE"]
flistratios<-function(ratio,bk){
  listratios=1:length(ratios_names)
  names(listratios)=ratios_names
  df=data.frame(fdf_ratios()[[listratios[ratio]]])%>%select(bk)
  return(df)
}
# flistratios("ROE","BNC")
#############################################################33333
#############  DASHBOARD          ##########################

#########################3
ui <- dashboardPage(
  
  dashboardHeader(title = "Haitian Banking Trend and Performance",
                  
                  titleWidth = 400, uiOutput("git")
                #(tags$li(class="dropdown",href=tags$a("https://twitter.com/","Twitter",icon=icon("twitter"),target="_blank")))
                
                 ), # end of dashboardHeader
  
  ###############
  
  ##################3
  dashboardSidebar(
    sidebarMenu(
      menuItem("About the dashboard",tabName = "about", icon = icon("dashboard")),
      menuItem("Raw indicators", tabName = "raw_indicators"),
      menuItem("Financial ratios", tabName = "performance", badgeLabel = "Ratios", badgeColor = "green", icon = icon("bar-chart-o"))
    ),
    
    selectInput("indicators", label = "Indicators",
                choices = sort(indicators_names), selected = indicators_names[indicators_names=="Equity"]),
    
    selectInput("banks", label = "Banks",
                choices = sort(banks_names), selected = banks_names[banks_names=="System"],multiple = TRUE)
    
    
    # selectInput("dates", label="Quarter",
                # choices=dates_list,selected=dates_list[length(dates_list)])


    
  ), # end of dashboardSidebar
  
  #################################
  dashboardBody(
    # tags$style(js),
    tabItems(
      # First tab content
      tabItem(tabName = "about",
              
            
                fluidRow(
                  box(title = tags$b("About the Dashboard"),#background = "olive",
                      # red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
                  align = "left",
                  HTML(paste("To realize this dashboard, data provided on the website of the Haitian central bank (Banque de la République d'Haïti), 
                  have been used. The dashboard is not intended to be used as a tool for decision making, although 
    the information extracted from raw financial data may relevent to understand some aspects of 
    the Haitian banking sector.
    Any suggestion to improve the dashboard is welcomed. Do not hesitate to send me
                  an ",tags$a(href="mailto:raulin.cadet@uniq.edu.ht","email","."))," To navigate through the dashboard,
                  use the menu on the left side.",sep=""),
                  tags$br(),tags$br(),
                  tags$p(HTML(paste(tags$b("Author: "),tags$a(href="https://raulincadet.github.io/","Raulin Cadet"),sep=""))),
                  
                  tags$br(),tags$br(), # to add new blank lines
                  imageOutput("coverImage"),
                  # tags$br(),# to add new blank lines
                  # HTML("<a href="https://www.freepik.com/free-vector/illustration-financial-concept_2825786.htm#page=4&query=finance&position=29&from_view=keyword">Image by rawpixel.com</a> on Freepik")
                  tags$a(href="https://www.freepik.com/free-vector/illustration-financial-concept_2825786.htm#page=4&query=finance&position=29&from_view=keyword","Image by rawpixel.com on Freepik"),
                 # tags$br(), tags$br()# to add new blank lines
                  # textOutput("aboutText")
                  ), # end of box
                
                  
                # ), # end of column
            
                # fluidRow(width = 12,
                  box(title = tags$b("My approach"),#background = "olive",
                    align="left",
                    tags$p("Financial data related to the banks, provided on the website of the central banks
                    are not structured as time series data. I had to use R to download 97 files
                    (format: Excel). Each 
                    excel file contains many sheets, with different kinds of financial data. My targets 
                    were the balance sheet and the income statement. I should have chronological data
                    for each variable of the reports, to be able to plot their trends. In this regards, 
                    several Python functions have been written to import data from the excel files, cleaning
                    and gathering them as panel data (by quarters and by banks)."),
          
                    
                    
                    tags$p(HTML(paste("The dashboard has been realized, using the packages Shiny and Shinydashboard of R programming language. ","The page ", tags$b('Raw indicators')," of the dashboard can be used to observe changes in financial
                           variables such as total assets, net banking income etc. This page presents also
                           a graph of network similarity between the banks (including the banking system),
                           for each variable selected by the user. The network is builded considering the
                           significant Pearson correlation between each pair of banks 
                           (including the banking system). The threshold of 5% is used for the test of correlation.
                            The user can select the banks he wants to consider. 
                            Data related to the whole banking system is considered, allowing to compare
                            its behavior with the one of any bank.","",sep=""))
                            ), # tags$p
                           tags$p(HTML(paste("The page ",shiny::tags$b("Financial ratios"),
                            " presents some gauges related to some key financial ratios, by bank and by quarter.
                            A user of the dashboard can choose the quarter and the bank he wants to consider. In addition,
                            this page allowd the user to choose a financial ratio and one or several banks,
                            to observe the trend of the selected indicator and its network of similarity.",
                                             "",sep="")),
                            tags$p("To define the intervals of the gauges, the percentile approach is used. Since, 
                            three intervals should be defined, the following probabilities are considered, to calculate
                            the percentile: 0, 0.33, 0.67, 1. The outliers are removed before calculating the bounds 
                            of a gauge. To ensure that each gauge accounts for the context of the Haitian
                            banking sector, ratios related to agregate data of the banking system are used to define their bounds. 
                            Ratios related to a bank are indicated in the gauges, whithout remove any outliers. Thus, the user of the dashboard can observe when
                            an entity outperforms or underperforms the banking industry.")
                           
                    
                    ) # ends of tags$p
                  ) #end of box
              
                ) # end of fluidRow
      ), # end of tabItem
      tabItem(tabName = "raw_indicators",
   
    # Boxes need to be put in a row (or column)
    
    
    fluidRow(
      
      box(title = "Evolution of selected indicator, by selected bank(s)",
         status="info", solidHeader = TRUE, dygraphOutput("plot1", height = 260)),
      
      box(title = "Ratio of selected indicator on asset, by selected bank(s)",
          status = "info", solidHeader = TRUE, dygraphOutput("plot2", height = 260)),
      
      
    ),  # end of fluidRow
    
    fluidRow(
      box(title="Network of similarity between banks, for the selected indicator",
          status="info", solidHeader = TRUE, plotOutput("plot4", height = 400)),
      ###############################
      box(title="",status="info", solidHeader = TRUE, 
          fluidRow(
          column(width = 5,offset = 0,
          selectInput("banksnet", label="Banks included in the network",
                      choices=sort(banks_names[!banks_names %in% c("SOCABK","SCOTIA","BICH",
                  "PROMOBK","SOCABL","BIDC","BHD")]),selected=banks_names[!banks_names %in% c("SOCABK","SCOTIA","BICH","PROMOBK","SOCABL","BIDC","BHD")],
                      multiple = T),
          ), #end of column
          column(width = 7,offset = 0,
           textOutput("NetText")
          ) # end of column
          ) # end of fluidros
          ), #end of box

      
    ) # end of fluidRow
  ##########
  # fluidRow(
  # 
  #   valueBoxOutput("valuebox1"),
  #   valueBoxOutput("valuebox2"),
  #   valueBoxOutput("valuebox3")
  # 
  # ) # end of fluidRow
  # 
  
      ), # end first tabitem
  tabItem(tabName = "performance",
          # h2("Widgets tab content"),
          fluidRow(
            column(width = 2,offset = 0,
                
                   selectInput("datesgauge", label="Select a quarter",
                               choices=sort(dates_list,decreasing=T),selected=dates_list[length(dates_list)])
                   
             ), #end of column
             column(width = 2,offset = 0,
           
                   selectInput("banksgauge", label = "Select a bank",
                               choices = sort(banks_names), selected = banks_names[banks_names=="System"])
                 
                   
            ) #end of column
          ) ,# end of fluidRow
          fluidRow(
            box(title = "NPL ratio",width = 2, status = "primary", solidHeader = TRUE,
               align="center", uiOutput("gauge1")
            ),
 
            box(title = "ROE",width = 2, status = "primary", solidHeader = TRUE,
                align="center",uiOutput("gauge2")
            ),
            
            box(title = "ROA",width = 2, status = "primary", solidHeader = TRUE,
                align="center",uiOutput("gauge3")
            ),
            box(title = "Efficiency ratio",width =2, status = "primary", solidHeader = TRUE,
                align="center",uiOutput("gauge4")
            ),
            box(title = "Equity to asset",width = 2, status = "primary", solidHeader = TRUE,
                align="center",uiOutput("gauge5")
            ) # end of box
            
          ), # end of fluidRow 
    ###################
    fluidRow(
      column(width = 3,offset = 0,
             selectInput("ratios", label="Select a financial ratio",
                         choices=sort(ratios_names),selected="ROA")
             
      ), #end of column
      column(width = 5,offset = 0,
             # selectInput("banksratios", label = "Select one or several bank(s)",
             #             choices = sort(banks_names), selected = banks_names,multiple = T)#banks_names[banks_names=="System"]
             selectInput("banksratios", label="Select or remove one or several banks",
                         choices=banks_names[!banks_names %in% c("SOCABK","SCOTIA","BICH","PROMOBK","SOCABL","BIDC","BHD")],selected=banks_names[!banks_names %in% c("SOCABK","SCOTIA","BICH","PROMOBK","SOCABL","BIDC","BHD")],
                         multiple = T),
             
      ) #end of column
    ), # end of fluidRow
    ##
    fluidRow(
      tabBox(#title = "Financial ratios", 
             id = "tab_earning", #height = "250px",
             tabPanel(tags$b("Graph of selected ratio"), 
  
                      dygraphOutput("plotratios")
               ), # end of tabPanel
              tabPanel(tags$b("Definitions of the ratios"), uiOutput("defearning"))
      ),# end of tabbox
     tabBox(#title=tags$b("Graph of banks network"),
      tabPanel(tags$b("Network"), plotOutput("plotNetRatios")
        
      ), # end of tabPanel
        tabPanel(tags$b("Network characteristics"), textOutput("NetTextRatios")
        ) # end of tabPanel
      ) # end of tabBox
   
  ) # end of fluidRow
    ##########################
  ) # end of second tabitem
    ) # end of all tabitems

  ) # end of dashboardBody
)# end of dashboardPage


########################################################
############### SERVER  ################################
server <- function(input, output) {
  
  
  output$plot1 <- renderDygraph({
    dygraphs::dygraph(cbind(
      (fgraph_xts(input$indicators,input$banks))/1000
    ) # end cbind
    ,ylab="Thousands HTG",xlab = "Quarters",main = input$indicators
    )%>% # end dygraphs
      dyOptions()%>%
      dyRangeSelector()%>%
      dyHighlight(highlightCircleSize = 5, 
                  highlightSeriesBackgroundAlpha = 0.2,
                  hideOnMouseOut = FALSE)%>%
      dyOptions(fillGraph = T)
    
    
  }) # end of renderDygraph
  ######
  output$plot2<-renderDygraph({
    dygraphs::dygraph(cbind(
      (fgraph_xts(input$indicators,input$banks)/fgraph_xts("Asset",input$banks))*100 # put in percentage
    ) # end cbind
    ,ylab = "Percentages",xlab = "Quarters",main = paste(input$indicators," (in % of total assets)",sep = ""))%>% # end dygraphs
      dyOptions()%>%
      dyRangeSelector()%>%
      dyHighlight(highlightCircleSize = 5, 
                  highlightSeriesBackgroundAlpha = 0.2,
                  hideOnMouseOut = FALSE)
    ###############
    
  }) # end of renderDygraph
  #############################33
  
  # output$plot3 <- renderDygraph({
  #   dygraphs::dygraph(
  #     fdecomp(input$indicators,input$banks)
  #   )%>% # end dygraphs
  #     # 
  #     # dyAxis("y",axisLineColor = "white",axisLabelColor = "white")%>%
  #     dyOptions(axisLineWidth =0,drawGrid = FALSE)%>%
  #     dyEvent(c("0000-03-03","0000-06-01","0000-09-01","0000-12-01"), c("Q1","Q2","Q3","Q4"), labelLoc = "bottom") #%>%
  # 
  # }) # end of renderDygraph
  #########################333
  output$valuebox1 <- renderValueBox({
    valueBox(paste(round(((fstats(input$indicators,input$banks))$MeanG),digits=2),"%"),
             "Average growth rate",icon = icon("credit-card"))
  })
  
  output$valuebox2 <- renderValueBox({
    valueBox((round(((fstats(input$indicators,input$banks))$Mean),digits=2)),
             "Mean (Thousands HTG)",icon = icon("credit-card"))
  })
   
  output$valuebox3 <- renderValueBox({
    valueBox((round(((fstats(input$indicators,input$banks))$Sd),digits=2)),
             "Standard deviation",icon = icon("credit-card"))
  })
  
  ########################333
  output$plot4<-renderPlot({
    fgraph_network((fnetwork(input$indicators,input$banksnet))$graph)
    title(paste("Network of similarity of the banks,considering the variable ",input$indicators,sep = ""))
    
  })
  ##################3
  output$NetText<-renderText(paste("Considering the variable ",(input$indicators), 
                                   ", the network of the banking sector, regarding their similarity,"," has ",(fnetwork(input$indicators,input$banksnet))$robust,". ",
                                   "It has ",(fnetwork(input$indicators,input$banksnet))$components,
                                   " components, as showed in the network graphic. ",
                                   "The bank(s) with the maximum of connections, thus the bank(s)
                                   which is/are more similar to the other is/are: ",
                                    paste((fnetwork(input$indicators,input$banksnet))$important,collapse = ", "),".",
                                    " The least connected bank(s) is/are: ",paste((fnetwork(input$indicators,input$banksnet))$leastImp,collapse = ", "),
                                   sep="")
                             ) # end of renderText
  ####################################
  
  
  
  output$gauge1 <-renderGvis({
    gvisGauge(df_NPL_ratio[input$banksgauge,c("Banks",input$datesgauge)],labelvar = "Banks",
                         options=seq_gauge(df_NPL_ratio,inverse = TRUE))
  }) # end of renderGvis
#####
  output$gauge2 <-renderGvis({
    gvisGauge(df_ROE[input$banksgauge,c("Banks",input$datesgauge)],labelvar = "Banks",
              options=seq_gauge(df_ROE))
  }) # end of renderGvis
  
  #####
  output$gauge3 <-renderGvis({
    gvisGauge(df_ROA[input$banksgauge,c("Banks",input$datesgauge)],labelvar = "Banks",
              options = seq_gauge(df_ROA)
              )
  }) # end of renderGvis
  #####
  output$gauge4 <-renderGvis({
    gvisGauge(df_efficiency[input$banksgauge,c("Banks",input$datesgauge)],labelvar = "Banks",
              options = seq_gauge(df_efficiency,inverse = TRUE)
              )
  }) # end of renderGvis
  #####
  output$gauge5 <-renderGvis({
    gvisGauge(df_leverage[input$banksgauge,c("Banks",input$datesgauge)],labelvar = "Banks",
              options=seq_gauge(df_leverage))
  }) # end of renderGvis
  
  output$defearning<-renderUI({
    
    tags$ul(
      tags$li(HTML(paste(tags$b("Efficiency ratio: "), "Operating expenses, as a percentage of net banking income."),sep="")),
      tags$li(HTML(paste(tags$b("Equity to asset ratio: ")," Equity, as a percentage of total assets."),sep="")),
      tags$li(HTML(paste(tags$b("ROA: "), " Net profit or loss, as a percentage of total assets."),sep="")), 
      tags$li(HTML(paste(tags$b("ROE: "),"Net profit or loss, as a percentage of equity."),sep="")),
      tags$li(HTML(paste(tags$b("Non performing loan ratio: "),"Non performing loans, as a percentage of loans portfolio."),sep="")),
      tags$li(HTML(paste(tags$b("Liability to equity ratio: "),"Liability, as a percentage of equity"),sep=""))
      
      )
    }) # end of renderText
  
  output$plotratios<-renderDygraph({
    dygraphs::dygraph(
      (flistratios(input$ratios,input$banksratios))
   
    ,ylab = "Percentages","Quarters",main = input$ratios)%>% # end dygraphs
      dyOptions()%>%
      dyRangeSelector()%>%
      dyHighlight(highlightCircleSize = 5, 
                  highlightSeriesBackgroundAlpha = 0.2,
                  hideOnMouseOut = FALSE)
  }) # end of renderdygraph
  
  ########################333
  output$plotNetRatios<-renderPlot({
    fgraph_network((fnetwork(input$ratios,input$banksratios,Ratios = "yes"))$graph)
    title(paste("Network of similarity of the banks,considering the variable ",input$ratios,sep = ""))
                     # paste("Network of similarity of the banks,considering the variable ",input$ratios,sep = ""))
  })
  ##########
  ##################3
  output$NetTextRatios<-renderText({paste("Considering the variable ",input$ratios,
                                          " the network of the banking system has ",(fnetwork(input$ratios,input$banksratios,Ratios = "yes"))$robust,". ",
                                   "It has ",(fnetwork(input$ratios,input$banksratios,Ratios = "yes"))$components,
                                   " component(s), as showed in the network graphic. ",
                                   "The bank(s) with the maximum of connections, thus the bank(s)
                                   which is/are more similar to the other(s) is/are: ",
                                   paste((fnetwork(input$ratios,input$banksratios,Ratios = "yes"))$important,collapse = ", "),".",
                                   " The least connected bank(s) is/are: ",paste((fnetwork(input$ratios,input$banksratios,Ratios = "yes"))$leastImp,collapse = ", "),
                                   sep="")
  }) # end of renderText
  ####
  output$aboutText=renderText({
    
    "To realize this dashboard, data provided by the Haitian central bank (Banque de la République d'Haïti), on its webpage 
    have been used. The dashboard is not intended to be used as a tool for decision making, although 
    the information extracted from raw financial data may be relevent to understand some aspects of 
    the Haitian banking sector.\n
    
    Any suggestion to improve the dashboard is welcomed."
   
  }) # end of renderText
  ###
  output$coverImage=renderImage({
    list(src = "www/20546.jpg",
         width = "100%"
         # ,
         # height = 330
         ) # end of renderImage
    
     })
  output$git=renderUI(
    shinydashboardPlus::socialButton(
      url = "https://github.com",
      type = "github"
    )
  )
  ####3
} # end of server

shinyApp(ui, server)

