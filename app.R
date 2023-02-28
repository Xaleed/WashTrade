
library(shinydashboard)
library(ggplot2)
library(tidyr)
library(formattable)
library(igraph)
library(dplyr)
library("plyr")                                                 
library("readr")  
########################3
data1 <- read.csv("D:\\Data\\charkhesi\\ForCharkheshi.csv")

data1 = data1 %>% slice(1:70000)
data = data1
length(data$CValueB)

############################
ui <- dashboardPage(
  dashboardHeader(title = "Market supervision"),
  dashboardSidebar(
 
    fluidPage(
      selectInput("NumberOfDay", "Minimum Number Of Trade",
                  unique(c(4,8,1,2,3,5,6,7,9,10))
      ),
      selectInput("LengthOfPath", "Length Of Wash Trade",
                  unique(c(3,1,2,3,4, 5, 6))
      ),
      uiOutput("n1")
      # tableOutput("data")
    )
    
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    formattableOutput("table"),
    tabsetPanel(id = "tabSelected",
                tags$style(
                  "li a {
        font-size: 20px;
        font-weight: bold;
      }
    "
                ),
              
                tabPanel("Details of Trades",
                         
                         fluidRow( 
                           DT::dataTableOutput("TotalRankingV"
                           )),
                         downloadButton("TotalRankingVD", "Download")
                       ),
                tabPanel("Frequency of Wash Trades",
                         fluidRow( 
                           DT::dataTableOutput(
                           "mytable1V")),
                        
                         fluidRow( 
                           DT::dataTableOutput("mytable2V"
                           ))
                ),
                tabPanel("Graph",

                         fluidRow(
                            title = "Graph",
                                 plotlyOutput("plot2", height = 250)
                         ),
                         plotOutput("plot1", click = "plot_click"),
                         plotOutput("plot3", click = "plot_click")
                         
                )
                )
    )
)

server <- function(input, output) {
  ddd <- reactive({
    dataK = data
    from1 = sapply(dataK$CodOfSeller
                   ,
                   toString)
    
    to1 = sapply(dataK$CodOfBuyer
                 ,
                 toString)
    from1 = from1
    to1 = to1
    dataa2 = cbind(from1, to1)
    
    dataK$concat = paste0(dataK$CodOfSeller,
                          dataK$CodOfBuyer)
    ##############################
    ###############################
    Frosh2 = c()
    

    Kharid2 = c()
    
    count = c()
    
    DD = data.frame(FF = from1, Too = to1)
    from11 = DD$FF
    to11 = DD$Too
    #dataa2 = cbind(from1, to1)
    #graph1 <- graph_from_edgelist(dataa1)
    #graph2 <- graph_from_edgelist(dataa2)
    #dataa2 = dataa2[from1 == from1[1],]
    mm = unique(DD$FF)

    for (k in 1:length(mm)){
      dataa2 = cbind(from1, to1)
      #graph1 <- graph_from_edgelist(dataa1)
      graph2 <- graph_from_edgelist(dataa2)
      Ne = neighbors(graph2, mm[k])
      for (j in 1:length(Ne)){
        # print(unique(from)[k])
        a = all_simple_paths(graph2, Ne[j],mm[k], cutoff = input$LengthOfPath)
        if (length(a) > 0){
          for (i in 1:length(a)){
            tt = as_ids(a[[i]])
            
            if(length(tt) == input$LengthOfPath){
              # CountVector = append(CountVector, rep(paste(c(k,j,i), collapse = '_'), times = length(tt)))
              Kharid2 = append(Kharid2, c(tt))
              Frosh2 = append(Frosh2, c(mm[k], tt[1:length(tt)-1]))
              #print(c(unique(from)[k], as_ids(a[[1]])))
              #print(Kharid2)
             # print(Frosh2)
            }
            #print(length((list(all_simple_paths(graph, Ne[j],unique(from)[k], cutoff = LengthOfPath)))))
          }
        }
      }
      DD = DD[DD$FF != mm[k],]
      from1 = DD$FF
      to1 = DD$Too
    }
    
    from1 = Frosh2
    #CountVector
    from1 = from1
    to1 = Kharid2
    to1 = to1
    cONCAT = paste0(from1, to1)
    dataK[dataK$concat %in% unique(cONCAT), ]
  })
  ##########################
  output$n1 <- renderUI({
    selectInput("no1", "code of Trader", choices=unique(ddd()$CodOfSeller), multiple=T)
  })
  ##########################Rank
  
  output$TotalRankingV <-DT::renderDataTable({
    if(!is.null(input$no1)){
      ddd()[(ddd()$CodOfSeller %in% input$no1) | (ddd()$CodOfBuyer %in% input$no1), ]
    }else{
      ddd()
    }
    
  })

  output$mytable1V <-DT::renderDataTable({
    if(!is.null(input$no1)){
    ddd = ddd()[(ddd()$CodOfSeller %in% input$no1) | (ddd()$CodOfBuyer %in% input$no1), ]
    
    from1 = sapply(ddd$CodOfSeller
                   ,
                   toString)
    
    to1 = sapply(ddd$CodOfBuyer
                 ,
                 toString)
    from1 = from1
    to1 = to1
    dataa2 = cbind(from1, to1)

    ##############################
    ###############################
    Frosh2 = c()
    
    
    Kharid2 = c()
    
    count = c()
    
    DD = data.frame(FF = from1, Too = to1)
    from11 = DD$FF
    to11 = DD$Too
    #dataa2 = cbind(from1, to1)
    #graph1 <- graph_from_edgelist(dataa1)
    #graph2 <- graph_from_edgelist(dataa2)
    #dataa2 = dataa2[from1 == from1[1],]
    mm = unique(DD$FF)
    
    for (k in 1:length(mm)){
      dataa2 = cbind(from1, to1)
      #graph1 <- graph_from_edgelist(dataa1)
      graph2 <- graph_from_edgelist(dataa2)
      Ne = neighbors(graph2, mm[k])
      for (j in 1:length(Ne)){
        # print(unique(from)[k])
        a = all_simple_paths(graph2, Ne[j],mm[k], cutoff = input$LengthOfPath)
        if (length(a) > 0){
          for (i in 1:length(a)){
            tt = as_ids(a[[i]])
            
            if(length(tt) == input$LengthOfPath){
              # CountVector = append(CountVector, rep(paste(c(k,j,i), collapse = '_'), times = length(tt)))
              Kharid2 = append(Kharid2, c(tt))
              Frosh2 = append(Frosh2, c(mm[k], tt[1:length(tt)-1]))
              #print(c(unique(from)[k], as_ids(a[[1]])))
              #print(Kharid2)
              # print(Frosh2)
            }
            #print(length((list(all_simple_paths(graph, Ne[j],unique(from)[k], cutoff = LengthOfPath)))))
          }
        }
      }
      DD = DD[DD$FF != mm[k],]
      from1 = DD$FF
      to1 = DD$Too
    }
    
    
    
    Dir <- data.frame(
      x1 = numeric(),
      x2 = numeric(),
      x3 = numeric(),
      x4 = numeric(),
      x5 = numeric(),
      x6 = numeric(),
      x7 = numeric(),
      x8 = numeric(),
      x9 = numeric(),
      x10 = numeric(),
      x11 = numeric(),
      x12 = numeric(),
      x13 = numeric(),
      x14 = numeric(),
      x15 = numeric(),
      x16 = numeric(),
      x17 = numeric(),
      x18 = numeric(),
      x19 = numeric())
    
    #Cir = Dir %>% select(1:(input$LengthOfPath))
    Cir = Dir[,c(1:(input$LengthOfPath))]
    
   # Frosh2 = ddd()$کد.جديد.مشتري.فروشنده
   # Kharid2 = ddd()$کد.جديد.مشتري.خريدار
    
    
    Cir$Sum = numeric()
    Frosh22 = as.double(Frosh2)
    co= as.numeric(input$LengthOfPath)
    for (i in 1:(length(Frosh22)/co)){
      Cir[i,] = c(Frosh22[((i-1)*co+1):((i-1)*co+co)],
                       sum(Frosh22[((i-1)*co+1):((i-1)*co+co)]))
    } 
    
    USum = unique(Cir$Sum)
    NCir = Cir[Cir$Sum == USum[1],][1,]
    Fre = c(length(Cir[Cir$Sum == USum[1],]$Sum))
    for (i in 2:length(USum)){
      Fre = append(Fre,length(Cir[Cir$Sum == USum[i],]$Sum))
      NCir = rbind.fill(NCir, Cir[Cir$Sum == USum[i],][1,])
    }
    NCir$Fre = Fre
    
    NCir$Rank = rank(Fre,ties.method = "first")
    NCir
    }else{
      ddd = ddd()
      from1 = sapply(ddd$CodOfSeller
                     ,
                     toString)
      
      to1 = sapply(ddd$CodOfBuyer
                   ,
                   toString)
      from1 = from1
      to1 = to1
      dataa2 = cbind(from1, to1)
      
      ##############################
      ###############################
      Frosh2 = c()
      
      
      Kharid2 = c()
      
      count = c()
      
      DD = data.frame(FF = from1, Too = to1)
      from11 = DD$FF
      to11 = DD$Too
      #dataa2 = cbind(from1, to1)
      #graph1 <- graph_from_edgelist(dataa1)
      #graph2 <- graph_from_edgelist(dataa2)
      #dataa2 = dataa2[from1 == from1[1],]
      mm = unique(DD$FF)
      
      for (k in 1:length(mm)){
        dataa2 = cbind(from1, to1)
        #graph1 <- graph_from_edgelist(dataa1)
        graph2 <- graph_from_edgelist(dataa2)
        Ne = neighbors(graph2, mm[k])
        for (j in 1:length(Ne)){
          # print(unique(from)[k])
          a = all_simple_paths(graph2, Ne[j],mm[k], cutoff = input$LengthOfPath)
          if (length(a) > 0){
            for (i in 1:length(a)){
              tt = as_ids(a[[i]])
              
              if(length(tt) == input$LengthOfPath){
                # CountVector = append(CountVector, rep(paste(c(k,j,i), collapse = '_'), times = length(tt)))
                Kharid2 = append(Kharid2, c(tt))
                Frosh2 = append(Frosh2, c(mm[k], tt[1:length(tt)-1]))
                #print(c(unique(from)[k], as_ids(a[[1]])))
                #print(Kharid2)
                # print(Frosh2)
              }
              #print(length((list(all_simple_paths(graph, Ne[j],unique(from)[k], cutoff = LengthOfPath)))))
            }
          }
        }
        DD = DD[DD$FF != mm[k],]
        from1 = DD$FF
        to1 = DD$Too
      }
      
      
      
      Dir <- data.frame(
        x1 = numeric(),
        x2 = numeric(),
        x3 = numeric(),
        x4 = numeric(),
        x5 = numeric(),
        x6 = numeric(),
        x7 = numeric(),
        x8 = numeric(),
        x9 = numeric(),
        x10 = numeric(),
        x11 = numeric(),
        x12 = numeric(),
        x13 = numeric(),
        x14 = numeric(),
        x15 = numeric(),
        x16 = numeric(),
        x17 = numeric(),
        x18 = numeric(),
        x19 = numeric())
      
      #Cir = Dir %>% select(1:(input$LengthOfPath))
      Cir = Dir[,c(1:(input$LengthOfPath))]
      
      # Frosh2 = ddd()$کد.جديد.مشتري.فروشنده
      # Kharid2 = ddd()$کد.جديد.مشتري.خريدار
      
      
      Cir$Sum = numeric()
      Frosh22 = as.double(Frosh2)
      co= as.numeric(input$LengthOfPath)
      for (i in 1:(length(Frosh22)/co)){
        Cir[i,] = c(Frosh22[((i-1)*co+1):((i-1)*co+co)],
                    sum(Frosh22[((i-1)*co+1):((i-1)*co+co)]))
      } 
      
      USum = unique(Cir$Sum)
      NCir = Cir[Cir$Sum == USum[1],][1,]
      Fre = c(length(Cir[Cir$Sum == USum[1],]$Sum))
      for (i in 2:length(USum)){
        Fre = append(Fre,length(Cir[Cir$Sum == USum[i],]$Sum))
        NCir = rbind.fill(NCir, Cir[Cir$Sum == USum[i],][1,])
      }
      NCir$Fre = Fre
      
      NCir$Rank = rank(Fre,ties.method = "first")
      NCir
    }
      })
  
  
  
  

  
  
  
  
  output$plot1 <- renderPlot({
    if(!is.null(input$no1)){
      ddd = ddd()[(ddd()$CodOfSeller %in% input$no1) | (ddd()$CodOfBuyer %in% input$no1), ]
      
      from1 = sapply(ddd$CodOfSeller
                     ,
                     toString)
      
      to1 = sapply(ddd$CodOfBuyer
                   ,
                   toString)
      from1 = from1
      to1 = to1
      dataa2 = cbind(from1, to1)
      
      graph <- graph_from_edgelist(dataa2)
      fig <- plot(graph, layout = layout.circle, vertex.size = 8)
      fig
    }else{
    from1 = sapply(ddd()$CodOfSeller
                   ,
                   toString)
    
    to1 = sapply(ddd()$CodOfBuyer
                 ,
                 toString)
    from1 = from1
    to1 = to1
    dataa2 = cbind(from1, to1)
    
    graph <- graph_from_edgelist(dataa2)
    fig <- plot(graph, layout = layout.circle, vertex.size = 8)
    fig}
  })
}


shinyApp(ui, server)
