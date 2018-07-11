rm(list=ls(all=TRUE))
# Shiny app for exploring health facility distributions

library(shiny)
library(shinydashboard)
library(ggplot2)

source('toolInit/tool init functions.R')
dat=read.csv('toolInit/fake data.csv',as.is=T)
res=glm(defor~dist.road,data=dat,family=binomial(link='logit'))
coef1=res$coefficients

# Define UI 
ui <- dashboardPage(
  
  dashboardHeader(),
  
  dashboardSidebar(
    radioButtons(inputId = "layout",
                 label = "Landscape",
                 choices = c(1, 2),
                 selected = 1),
    sliderInput("road.cost", label="Road cost (per unit length)", value=1,step=1,min=0,max=5),
    sliderInput("pa.cost", label="Cost of deforesting PA (per 1x1 pixel)", value=1,step=1,min=0,max=5),
    sliderInput("forest.cost", label="Cost of deforesting UA (per 1x1 pixel)", value=1,step=1,min=0,max=5),
    conditionalPanel(
        condition = "input.tipo == 'user-defined'",
        textInput("x", label="x coordinates", value='20,20',placeholder='0-100'),
        textInput("y", label="y coordinates", value='10,90',placeholder='0-100')
    ),
    radioButtons(inputId = "tipo", 
                 label = "Type:", 
                 choices = c('straight line', 'user-defined','optimized'),
                 selected='straight line')
    
  ),
    
    dashboardBody(
      h1("Determining the optimal location of roads"),
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
# input <- list()
# input$layout = 1
# input$road.cost = 1
# input$pa.cost = 1
# input$forest.cost = 1
# input$tipo = "optimized"

# Define server logic 
server <- function(input, output) {
  layoutList <- reactive({
    l <- as.numeric(input$layout)
    optimFile <- paste('toolInit/optimized', l, '.csv', sep="")
    gridFile <- paste('toolInit/fakeLayout', l, '.csv', sep="")
    optim1 <- read.csv(optimFile,as.is=T)
    grid1 <- read.csv(gridFile,as.is=T)
    
    startEnd <- read.csv('toolInit/startEnd.csv')
    start <- startEnd[startEnd$layout == l, c("startX", "startY")]
    start <- as.numeric(start)
    end <- startEnd[startEnd$layout == l, c("endX", "endY")]
    end <- as.numeric(end)
    
    L <- list(optim1, grid1, start, end)
  })
  
  outList <- reactive({
    optim1 <- layoutList()[[1]]
    grid1 <- layoutList()[[2]]
    start <- layoutList()[[3]]
    end <- layoutList()[[4]]
    
    road.cost=as.numeric(input$road.cost) #per length of road
    pa.cost=as.numeric(input$pa.cost) #per area of deforested pa
    forest.cost=as.numeric(input$forest.cost) #per area of deforested land
    
    str.coords <- user.coords <- data.frame(x=c(start[1],end[1]),y=c(start[2],end[2]))
    user.grid=grid1
    
    if (input$tipo=='user-defined'){
      #create user coordinates
      x=as.numeric(unlist(strsplit(input$x,split=',')))
      y=as.numeric(unlist(strsplit(input$y,split=',')))
      if (length(x)==length(y)){
        user.coords=data.frame(x=c(start[1],x,end[1]),y=c(start[2],y,end[2]))
      }
    }
    if (input$tipo=='optimized'){
      cond=input$pa.cost==optim1$pa.cost & 
        input$road.cost==optim1$road.cost &
        input$forest.cost==optim1$forest.cost
      tmp=optim1[cond,]
      x=unlist(tmp[,c('x1','x2','x3')])
      y=unlist(tmp[,c('y1','y2','y3')])
      user.coords=data.frame(x=c(start[1],x,end[1]),y=c(start[2],y,end[2]))
    }
    
    #get nearest distance
    user.grid$dist=get.dist(user.coords,user.grid)
    
    #predict deforestation
    tmp=exp(coef1[1]+coef1[2]*user.grid$dist)
    user.grid$prob=tmp/(1+tmp)
    cond <- user.grid$dist <= 1
    user.grid$prob[cond] <- max(user.grid$prob)*1.5
    
    prob.thresh=0.25
    cond=user.grid$tipo%in%c('Forest','PA') & user.grid$prob>prob.thresh; mean(cond)
    grid.tmp=user.grid[cond,]
    user.grid2 <- user.grid[(user.grid$tipo != "Pasture") & (user.grid$prob > 0.15),]
    
    #get length of road
    length2=get.length(user.coords)
    
    #calculate expected cost
    ecost=get.cost(road.cost,pa.cost,forest.cost,user.grid,length2)
    
    #change name from "Forest" to "UA"
    cond=user.grid$tipo=='Forest'
    user.grid$tipo[cond]='UA'
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
                              name = 'Probability of deforestation') +
      coord_fixed() + theme_bw(base_size = 14)
    # if (nrow(grid.tmp)==0) {
    #   res1=res
    # } else{
    #   res1=res+geom_point(data = grid.tmp, aes(x = x,y=y,size=1,color='red'),show.legend=F)     
    # }
    
    #calculate straight line cost
    grid0=grid1
    grid0$dist=get.dist(str.coords,grid0)
    tmp=exp(coef1[1]+coef1[2]*grid0$dist)
    grid0$prob=tmp/(1+tmp)
    length0=get.length(str.coords)
    
    #sideplots
    usrVStrDf <- data.frame(Route=c("Straight Line", "Proposed"),
                            Length=c(length0, length2))
    rdLenInc <- (length2 - length0) / length0 * 100
    paDef <- ecost$pa.def * 100
    forDef <- ecost$forest.def * 100
    # rdPlot = ggplot(usrVStrDf, aes(x = Route, y = Length)) +
    #   geom_col()
    
    L <- list(res, rdLenInc, paDef, forDef, ecost)
    L
  }
    
  )
  
  output$LULC <- renderPlot(outList()[[1]])
  # output$rdLength <- renderPlot(calc()[[2]])
  output$rdLength <- renderInfoBox({
    infoBox(
      "Road Length Increased",
      value = paste(round(outList()[[2]], 1), "%"),
      subtitle = "Proposed vs. straight-line route",
      icon = icon("road"),
      color = "black"
    )
  })
  
  output$paDef <- renderInfoBox({
    infoBox(
      "Deforestation - PA",
      value = paste(round(outList()[[3]], 1), "%"),
      subtitle = "of original area",
      icon = icon("tree"),
      color = "olive"
    )
  })
  
  output$forDef <- renderInfoBox({
    infoBox(
      "Deforestation - UA",
      value = paste(round(outList()[[4]], 1), "%"),
      subtitle = "of original area",
      icon = icon("tree"),
      color = "lime"
    )
  })
  
  output$costs <- renderInfoBox({
    ecost = outList()[[5]]
    road.cost = round(ecost$road.cost)
    pa.cost = round(ecost$pa.cost)
    forest.cost = round(ecost$forest.cost)
    total.cost = round(ecost$total.cost)
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