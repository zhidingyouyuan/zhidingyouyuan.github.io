#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
# 用于岗位模型等模型验证 095example

library(shiny)
library(shinythemes)
library(tidyverse)
library(openxlsx)
library(knitr)
library(ggthemes)
source('func_compution.r')
load('data/MAPR_tran.Rda')
load('data/SZ_norm.Rda')
load('data/SZ_Zscore_normgroup.Rda')

# source('split_col.r')

# Define UI
ui <- fluidPage(
  # tagList(
  # shinythemes::themeSelector(),
  tags$head(
    tags$style(HTML("
        /* Smaller font for preformatted text */
        pre, table.table {
          font-size: smaller;
        }

        body {
          min-height: 2000px;
        }

        .option-group {
          border: 1px solid #ccc;
          border-radius: 6px;
          padding: 0px 5px;
          margin: 5px -10px;
          background-color: #f5f5f5;
        }

      "))
  ),
  navbarPage(
    theme = shinytheme("flatly"),
    # <--- To use a theme, uncomment this
    "模型验证小工具",
    tabPanel("上传原始数据",
      # fluidPage(
      titlePanel("上传含绩效指标的数据"),
      sidebarLayout(
        sidebarPanel(
          # wellPanel(
          selectInput(
            inputId = "datatype",
            label = "Your data type:",
            choices = c("A-B", "Z分数")
          ),
          fileInput(
            "file1",
            "选择文件上传",
            multiple = FALSE,
            accept = c(".xlsx")
          ),
          
          helpText("数据格式需为.xlsx,数据需包含一列叫**绩效分组**的变量（绩优、绩普、绩差，可没有绩普）。"),
          # Input: Select separator ----
          radioButtons("sep",
            "如果为AB数据，则需选择连接符号",
            choices = c(逗号  = ",",
                          分号  = ";",
                          短横线  = "-"),
            selected = ","
          ), 
          textInput("name_jy", "绩优标签:", "绩优"),
          textInput("name_jx", "绩差标签:", "绩差")
          
          # )
        ),
        
        mainPanel(tabsetPanel(
          tabPanel("数据样例", img(src = "data-sample.png")),
          tabPanel("你上传的数据", DT::dataTableOutput('datasample')),
          tabPanel("绩效信息统计表", tableOutput('jixiao_stat'))
        ))
      )
    ),
    
    tabPanel("定义权重大小",
             
      column(2,div(class = "option-group",
        numericInput("创新", "创新:",0),
        numericInput("前瞻性", "前瞻性:", 0),
        numericInput("理论思考", "理论思考:", 0),
        numericInput("质疑", "质疑:", 0),
        numericInput("数据导向", "数据导向:", 0),
        numericInput("实践性", "实践性:" ,0),
        numericInput("关注细节", "关注细节:",  0)
      )),
      column(2,div(class = "option-group",
        numericInput("情绪稳定", "情绪稳定:", 0),
        numericInput("乐观性", "乐观性:",  0),
        numericInput("坚韧性", "坚韧性:", 0),
        numericInput("尽责", "尽责:",  0),
        numericInput("守规性", "守规性:",  0),
        numericInput("主动性", "主动性:",  0),
        numericInput("成就动机", "成就动机:",  0),
        numericInput("好胜心", "好胜心:",  0),
        numericInput("内省性", "内省性:",  0),
        numericInput("果断性", "果断性:",  0)
        
      )),
      column(2,div(class = "option-group",
        numericInput("社交自信", "社交自信:",  0),
        numericInput("乐群性", "乐群性:", 0),
        numericInput("人际敏锐", "人际敏锐:", 0),
        numericInput("助人倾向", "助人倾向:",  0),
        numericInput("支配性", "支配性:", 0),
        numericInput("说服他人", "说服他人:", 0),
        numericInput("自主性", "自主性:",  0)
      )),
      column(6,div(class = "option-group",
                   h5('常模咯：'),
                   tableOutput('normvalue'),
                   h5('交叉表-人数分布：'),
                   tableOutput('crosstable1'),
                   h5('交叉表-人数(%)分布：'),
                   tableOutput('crosstable2'),
                   h5('簇状分布图：'),
                   plotOutput('groupbarchart2'),
                   plotOutput('groupbarchart') 
                   ))
      # ,
      # 
      # h5("权重调整结果"),div(class = "option-group",
      #                 tableOutput('qzresult'))
      ),
    #   div(h3("如果准备好了……就"), style = "color:#CD4F39"),
    #   actionButton("update", "提交吧")
    # ),
    # tabPanel("模型结果图",div(class = "option-group",
    #   tableOutput('qzresult'))
    #   # plotOutput('barchart'),
    #   # tableOutput('correlation'),
    # ),
    
    tabPanel("下载",
             wellPanel(
               selectInput(
                 inputId = "downloadtype",
                 label = "选一下下什么数据",
                 choices = c("调整后的模型参数", "个人匹配分数")
               ),
               textInput("text", label = "给你的文件起个名字吧", value = "Enter text..."),
               downloadButton("downloadData", "戳我please")
             )),
    
    tabPanel("More",h2(
               "🌚",
               span("hey!", style = "color:red")
             ))
  ))


# Define server
server <- function(input, output) {
  df <- reactive({
    read.xlsx(input$file1$datapath) #读取上传的Z分数
  })
  
  ## 读取模型调试数据
  num_dim <- reactive({
    data.frame(
      性格特质 = na.omit(data_tran$wdrank),
      权重初值 = c(
        input$创新,
        input$前瞻性,
        input$理论思考,
        input$质疑,
        input$数据导向,
        input$实践性,
        input$关注细节,
        input$情绪稳定,
        input$乐观性,
        input$坚韧性,
        input$尽责,
        input$守规性,
        input$主动性,
        input$成就动机,
        input$好胜心,
        input$内省性,
        input$果断性,
        input$社交自信,
        input$乐群性,
        input$人际敏锐,
        input$助人倾向,
        input$支配性,
        input$说服他人,
        input$自主性
      )
    )
  })
  
  ## 计算常模值存入newnorm
  newnorm <- reactive({

    num_dim <- num_dim()
    weights <- data.frame(nul=c(rep(1,24)),
                          模型=c(num_dim$权重初值)/rep(sum(abs(num_dim$权重初值)),24))
    dim1_zscore <- as.matrix(dim1_zscore)
    weight2 <- as.matrix(weights) ### input
    dim2_score <- dim1_zscore %*% weight2
    dim2_zscore <- scale(dim2_score)
    new_mean <- round(attr(dim2_zscore,"scaled:center"),12)[2]
    new_sd <-  round(attr(dim2_zscore,"scaled:scale"),12)[2]
    n <- data.frame(mean=format(new_mean, digits = 12),sd=format(new_sd, digits = 12),stringsAsFactors = F)
    n
          })
  
  
  ## 计算岗位P分
  pvalue <- reactive({
    num_dim <- num_dim()
    data <- df() 
    # n <- newnorm()
    zscore <- as.matrix(data[,na.omit(data_tran$wdrank)]) # 将上传的数据按特定维度顺序整理
    
    # 再次计算常模
    weights <- as.matrix(data.frame(nul=c(rep(1,24)),
                          模型=c(num_dim$权重初值)/rep(sum(abs(num_dim$权重初值)),24)))
    dim1_zscore <- as.matrix(dim1_zscore)
    dim2_score <- dim1_zscore %*% weights
    dim2_zscore <- scale(dim2_score)
    new_mean <- round(attr(dim2_zscore,"scaled:center"),12)[2]
    new_sd <-  round(attr(dim2_zscore,"scaled:scale"),12)[2]
    ### 
    
    dimscore <- zscore %*% weights
    # new_mean <- n[1]
    # new_sd <- n[2]
    
    #### 计算岗位匹配度 ####
    posi_zscore <- as.data.frame(scale(dimscore[,2], center = new_mean, scale = new_sd))
    # pscore
    posi_pscore <- round(pnorm(as.matrix(posi_zscore)), 4) * 100
    posi_pscore[which(posi_pscore<=5,arr.ind = T)] <- 5
    posi_pscore[which(posi_pscore>=95,arr.ind = T)] <- 95
    posi_pscore
    
  })
  
  
  ## 计算岗位rank
  rankvalue <- reactive({
    posi_pscore2 <- pvalue()
    posi_pscore2 <- cut(posi_pscore2[,1],c(0,9,29,69,90,100),labels = c('E','D','C','B','A'))
    posi_pscore2
  })
  
  tabledata <- reactive({
    data <- df()
    x <- rankvalue()
    dat <- data.frame(绩效水平=data$绩效分组,匹配等级=x)
    all <- as.data.frame(table(dat)) %>% spread(匹配等级,Freq) %>% mutate(pA=round(A/rowSums(.[,-1])*100,2),
                                                     pB=round(B/rowSums(.[,-1])*100,2),
                                                     pC=round(C/rowSums(.[,-1])*100,2),
                                                     pD=round(D/rowSums(.[,-1])*100,2),
                                                     pE=round(E/rowSums(.[,-1])*100,2))
    all
  })
  
  
  ############################################################################################
  
  ## 输出norm
  output$normvalue <- renderTable({
     newnorm()   
  })
    
  ## 可视化上传的数据
  output$datasample <- DT::renderDataTable({
    data <- df()
    DT::datatable(as.matrix(data), options = list(lengthMenu = c(25, 30, 100), pageLength = 25))
  })
  
  ## 输出绩效分组统计
  output$jixiao_stat <- renderTable({
    data <- df()
    table(data$绩效分组)
    
  })
  
  ## 输出权重调适初值
  # output$qzresult <- renderTable({
  #   t(num_dim())
  # })
  
  ## 输出绩优绩差 vs 匹配rank
  output$groupbarchart2 <- renderPlot({
    
    plotdata2 <- tabledata() %>% select(c('绩效水平','pA','pB','pC','pD','pE')) %>% gather(匹配等级,percent,-绩效水平)
    ggplot(data = plotdata2, mapping = aes(x = factor(匹配等级), y = percent, fill = 绩效水平))+ 
      geom_bar(stat = 'identity', position = 'dodge')+
      geom_text(aes(label=percent))+
      theme_minimal()+ggtitle("不同绩效的人在各匹配等级上的占比")
  })
  
  output$groupbarchart <- renderPlot({

    plotdata <- tabledata() %>% select(c('绩效水平','A','B','C','D','E')) %>% gather(匹配等级,Freq,-绩效水平)
    ggplot(data = plotdata, mapping = aes(x = factor(匹配等级), y = Freq, fill = 绩效水平))+ 
      geom_bar(stat = 'identity', position = 'dodge')+
      geom_text(aes(label=Freq))+theme_minimal()+ggtitle("各匹配等级上不同绩效的人数")
    # +
    #   scale_y_continuous(limits = c(0,100),expand = c(0,0),breaks = seq(0,100,10))
    # +
    #   geom_text(stat='count',aes(label=scales::percent(..count../sum(..count..)))
    #             , color="white", size=3.5,position=position_fill(0.5))# ggplot(data = poltdata, mapping = aes(x = factor(绩效水平), y = z, fill = y)) 
    # # + geom_bar(stat = 'identity', position = 'dodge')
    # # xtabs('绩效水平'~'匹配等级',dat)
    
  })
  output$crosstable1 <- renderTable({
    # tabledata()
    table1 <- tabledata() %>% select(.,c('绩效水平','A','B','C','D','E'))
    table1
    
  })
  
  output$crosstable2 <- renderTable({
    # tabledata()
    table2 <- tabledata() %>% select(.,c('绩效水平','pA','pB','pC','pD','pE'))
    table2
    
  })
  # output$crosstable <- renderTable({
  #  pvalue()
  # })

  
  # You can access the value of the widget with input$text, e.g.
  # output$value <- renderPrint({ input$text })
  
  # Reactive value for selected dataset ----
  # datasetInput <- reactive({
  #   switch(input$dataset,
  #          "rock" = rock,
  #          "pressure" = pressure,
  #          "cars" = cars)
  # })
  # # Table of selected dataset ----
  # output$table <- renderTable({
  #   datasetInput()
  # })
  #
 

  # 
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$text, ".xlsx")
    },
    content = function(file) {
      
      if (input$downloadtype=='个人匹配分数') {
      rawdata <- df()
      write.xlsx(data.frame(绩效分组=rawdata$绩效分组,匹配度=pvalue(),匹配等级=rankvalue()), file)
      }else{
        write.xlsx(num_dim(), file)

      }

    }
  
  )
  
}

# Run the application
shinyApp(ui = ui, server = server)
