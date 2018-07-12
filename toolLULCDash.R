rm(list=ls(all=TRUE))
# Shiny app for exploring health facility distributions

library(shiny)
library(shinydashboard)
library(ggplot2)

source('tool functions.R')
tmp=read.csv('data/glm table.csv',as.is=T)
coef1=tmp[,'Estimate']
names(coef1)=tmp$X

# Define UI 
ui <- dashboardPage(
  
  dashboardHeader(title = "Optimal layout of roads", titleWidth = 300),
  
  dashboardSidebar(
    radioButtons(inputId = "landscape",
                 label = "Landscape",
                 choices = c(1, 2),
                 selected = 1),
    textInput("road.cost", label="Road cost (per km)", value=1),
    textInput("pa.cost", label="Cost of deforesting PA (per 1 km2 pixel)", value='1'),
    textInput("forest.cost", label="Cost of deforesting forested UA (per 1 km2 pixel)", value='1'),
    textInput("protect", label="Protective effect of PA in reducing deforestation (%)", value='0.3'),
    conditionalPanel(
        condition = "input.tipo == 'user-defined'",
        textInput("x", label="x coordinates", value='20,20',placeholder='0-100'),
        textInput("y", label="y coordinates", value='10,90',placeholder='0-100')
    ),
    radioButtons(inputId = "tipo", 
                 label = "Road layout:", 
                 choices = c('straight line', 'user-defined','optimized'),
                 selected='straight line')
    
  ),
    
    dashboardBody(
      # h1("Determining the optimal location of roads"),
      p("Some explanations here..."),
      p("PA = Protected forested Area; UA = Unprotected forested Area"),
      fluidRow(
        column(width = 4,
               infoBoxOutput("rdLength", width = NULL),
               infoBoxOutput("paDef", width = NULL),
               infoBoxOutput("forDef", width = NULL),
               infoBoxOutput("costs", width = NULL)
        ),
        column(
          width = 8,
          box(
            title = "Proposed route",
            width = NULL,
            status = "primary",
            solidHeader = T,
            plotOutput("LULC", height = "600px")
          )
        )
      )
    )
)

# Input list for debug
# input=list();
# input$road.cost='1'
# input$pa.cost='2'
# input$forest.cost='1'
# input$tipo='straight line'
# input$protect='1'
# input$landscape=1

# Define server logic 
server <- function(input, output) {
  # Load data based on landscape
  landscapeList <- reactive({
    l <- as.numeric(input$landscape)
    optimFile <- paste('data/optimized', l, '.csv', sep="")
    gridFile <- paste('data/grid', l, '.csv', sep="")
    optim1 <- read.csv(optimFile,as.is=T)
    grid1 <- read.csv(gridFile,as.is=T)
    
    startEnd <- read.csv('data/startEnd.csv')
    start <- startEnd[startEnd$layout == l, c("startX", "startY")]
    start <- as.numeric(start)
    end <- startEnd[startEnd$layout == l, c("endX", "endY")]
    end <- as.numeric(end)
    
    #get distance to urban centers
    uc=data.frame(x=c(20,20),y=c(20,90),type=rep("Urban Center", 2))
    dist=numeric()
    for (i in 1:nrow(uc)){
      x2=(grid1$x-uc$x[i])^2
      y2=(grid1$y-uc$y[i])^2
      dist=cbind(dist,sqrt(x2+y2))
    }
    grid1$dist_uc=apply(dist,1,min)
    
    #points
    # pt <- data.frame(x=c(start[1], end[1]), y=c(start[2], end[2]),
    #                  type=c("End Point"))
    # pt <- rbind(pt, uc)
    
    L <- list(optim1, grid1, start, end, uc)
  })
  
  outList <- reactive({
    optim1 <- landscapeList()[[1]]
    grid1 <- landscapeList()[[2]]
    start <- landscapeList()[[3]]
    end <- landscapeList()[[4]]
    uc <- landscapeList()[[5]]
    
    road.cost=as.numeric(input$road.cost) #per length of road
    pa.cost=as.numeric(input$pa.cost) #per area of deforested pa
    forest.cost=as.numeric(input$forest.cost) #per area of deforested land
    protect=as.numeric(input$protect)/100 #% of deforestation probability
    
    str.coords <- user.coords <- data.frame(x=c(start[1],end[1]),y=c(start[2],end[2]))
    user.grid <- grid1
    
    if (input$tipo=='user-defined'){
      #create user coordinates
      x=as.numeric(unlist(strsplit(input$x,split=',')))
      y=as.numeric(unlist(strsplit(input$y,split=',')))
      if (length(x)==length(y)){
        user.coords=data.frame(x=c(start[1],x,end[1]),y=c(start[2],y,end[2]))
      }
    }
    if (input$tipo=='optimized'){
      cost=pa.cost*(1-protect)*optim1$d.pa +
        forest.cost*optim1$d.ua +
        road.cost*optim1$l.road
      ind=which(cost==min(cost))[1]
      x=unlist(optim1[ind,c('x1','x2','x3')])
      y=unlist(optim1[ind,c('y1','y2','y3')])
      user.coords=data.frame(x=c(start[1],x,end[1]),y=c(start[2],y,end[2]))
    }
    
    #get nearest distance
    user.grid$dist_road=get.dist(user.coords, user.grid)
    
    #predict deforestation
    tmp=with(user.grid, exp(coef1['(Intercept)']+
                              coef1['dist_road']*dist_road+
                              coef1['dist_uc']*dist_uc+
                              coef1['dist_road:dist_uc']*dist_road*dist_uc))
    user.grid$prob=def.prob(user.grid, coef1)
    
    # tuning up the deforestation on pixels on the road
    # cond <- user.grid$dist <= 1
    # user.grid$prob[cond] <- max(user.grid$prob)*1.5
    
    #get length of road
    user.length <- get.length(user.coords)
    
    #calculate expected cost
    ecost=get.cost(user.grid, road.cost, pa.cost, forest.cost, protect, user.length)
    
    #change name from "Forest" to "UA"
    cond=user.grid$tipo=='Forest'
    user.grid$tipo[cond]='UA'
    
    #get prob.cor (this helps displaying deforestation probability)
    cond=user.grid$tipo%in%c('UA','PA')
    user.grid2=user.grid[cond,]
    cond=user.grid2$tipo=='PA'
    user.grid2$prob[cond]=user.grid2$prob[cond]*(1-protect)
    prob.thresh=0.1
    cond=user.grid2$prob>prob.thresh
    user.grid2=user.grid2[cond,]
    
    #relabel LC type for plotting
    user.grid$tipo <- factor(user.grid$tipo, levels = c("PA", "UA", "Pasture"),
                             labels = c("Protected Forested Area (PA)", 
                                        "Unprotected Forested Area (UA)",
                                        "Pasture"))
    
    #plot results
    res=ggplot() +
      geom_tile(data = user.grid, alpha = 0.8,aes(x = x, y = y,fill = tipo)) +
      geom_path(data = user.coords, aes(x = x, y = y), show.legend = F, lwd = 1.5) +
      scale_fill_manual(values=c('darkgreen','green','darkseagreen1'),name='Land use type') +
      geom_point(data = user.coords, aes(x = x,y = y), size = 3, show.legend=F) +
      geom_point(data = user.grid2, aes(x = x, y = y, colour = prob), size = 1, alpha = 0.5) +
      scale_colour_continuous(low = "#ffcccc", high = "#ff0000",
                              name = 'Probability\nof deforestation') +
      geom_point(data = uc, aes(x = x, y = y, pch = type), size = 5) +
      scale_shape_manual(values=10, name='') +
      coord_fixed() + theme_bw(base_size = 14)
    
    #calculate straight line cost
    str.grid <- grid1
    str.grid$dist_road <- get.dist(str.coords, str.grid)
    str.grid$prob <- def.prob(str.grid, coef1)
    str.length <- get.length(str.coords)
    
    #sideplots
    rd.len.inc <- (user.length - str.length) / str.length * 100
    d.pa.perc <- ecost$d.pa.prop * 100
    d.ua.perc <- ecost$d.ua.prop * 100
    
    L <- list(main.plot=res, rd.len.inc=rd.len.inc, ecost=ecost)
    L
  }
    
  )
  
  output$LULC <- renderPlot(outList()$main.plot)
  output$rdLength <- renderInfoBox({
    infoBox(
      "Road Length Increased",
      value = paste(round(outList()$rd.len.inc, 1), "%"),
      subtitle = "Proposed vs. straight-line route",
      icon = icon("road"),
      color = "black"
    )
  })
  
  output$paDef <- renderInfoBox({
    infoBox(
      "Deforestation - PA",
      value = paste(round(outList()$ecost$d.pa.prop*100, 1), "%"),
      subtitle = "of original area",
      icon = icon("tree"),
      color = "olive"
    )
  })
  
  output$forDef <- renderInfoBox({
    infoBox(
      "Deforestation - UA",
      value = paste(round(outList()$ecost$d.ua.prop*100, 1), "%"),
      subtitle = "of original area",
      icon = icon("tree"),
      color = "lime"
    )
  })
  
  output$costs <- renderInfoBox({
    ecost = outList()$ecost
    road.cost = round(ecost$road.cost)
    pa.cost = round(ecost$pa.cost)
    forest.cost = round(ecost$forest.cost)
    total.cost = round(road.cost+pa.cost+forest.cost)
    disp.value = paste(total.cost, "=", road.cost, "+", pa.cost, "+", forest.cost)
    infoBox(
      "Costs",
      value = disp.value,
      subtitle = "Total = Road + PA + UA",
      icon = icon("usd"),
      color = "orange"
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)