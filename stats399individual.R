library(shiny)
library(fmsb)
library(bslib)
library(tidyverse)
library(tidytext)
library(wordcloud)
library(syuzhet)




ui <- navbarPage(title = "Sentiment analysis",
                 theme = bs_theme(bootswatch = "united"),
                 tags$head(tags$style(HTML("
                     #pink-text {
                     background-color: #FFD1DC;  # 浅粉色的十六进制颜色值
                     display: inline-block;
                     padding: 5px;
                     border-radius: 10px; 
                     }
                     
                     #yellow-text {
                     background-color: #FFFACD;  # 浅米黄色的十六进制颜色值
                     display: inline-block;
                     padding: 5px;
                     border-radius: 10px; 
                     }
                     .custom-plot {
                     margin-left: -30px;  # 负的左边距将图形向左移动
                     }
                     "))),
                 
                 tabPanel("Introduction",
                          fluidRow(
                            column(1),
                            column(2,  # 分配2个列宽给图片
                                   tags$img(src = "p1.png", height = "128px", width = "128px", alt = "something wrong", deleteFile = FALSE)
                            ),
                            column(8,  # 分配8个列宽给文本
                                   tags$div("This application is aiming to display an analysis of the sentiment description provided by you, your classmates and instructors. Hopefully, you may have a breif idea about how people's feelings differ through out the interactive diagrams. Let's get started!", id = "pink-text"),
                                   br(),
                                   tags$div("The data used in this application has been collected by the survey before class, and has been cleaned up manually for the accuracy.", id = "yellow-text")
                            )
                          )
                 ),
                 
                 tabPanel("Radar chart",
                          selectInput("select", h3("Select group"), 
                                      choices = list("All" = 1, "Students' sentiments" = 2,
                                                     "Classmates' sentiments" = 3, "Instructors' sentiments" = 4), selected = 1),
                          div(class = "custom-plot", plotOutput("sentiment_radarchart", width = "100%", height = "600px"))
                          
                 ),
                 
                 tabPanel("Word cloud",
                         fluidRow(
                           column(12,  # 分配12个列宽给wordcloud
                                  plotOutput("sentiment_wordcloud", height = "800px", width = "800px")
                          
                 )))
                 

)





# Define server logic ----
server <- function(input, output) {
  
  output$sentiment_radarchart <- renderPlot({
    if(input$select == 1) {
    radar <- radarchart(df,
                        pcol=rainbow(3),
                        plwd=3,
                        plty=1,
                        cglcol='black',
                        cglty=3,
                        axistype=1,
                        axislabcol='red',
                        caxislabels=seq(0,100,20),
                        cglwd=1,
                        vlcex=0.8)
    
    # Add title
    title(main="Radar chart of your feelings about Stats399")
    
    # 添加图例
    legend(x=1.2, y=1.2, legend = rownames(df[-c(1,2),]), 
           bty = "n", pch=20 , col=rainbow(3) , 
           text.col = "black", cex= 0.9, pt.cex=3)
    
    
    # Add detailed description below the radar chart
    description <- " The vertices represent different sentiment classifications."
    mtext(description, side=1, line=, cex=0.8, adj=0.5)
    } else if(input$select == 2) {
      
      student_data <- df[3, ]  # 选择第三行，因为你在rbind之后添加了两行
      student_data <- rbind(rep(100,5), rep(0,5), student_data)
      
      # 绘制雷达图
      radarchart(student_data,
                 pcol="red",
                 plwd=3,
                 plty=1,
                 cglcol='black',
                 cglty=3,
                 axistype=1,
                 axislabcol='red',
                 caxislabels=seq(0,100,20),
                 cglwd=1,
                 vlcex=0.8)
      
      # Add title
      title(main="Radar chart of students feelings of themselves about Stats399")
      
      # 添加图例
      legend(x=1.2, y=1.2, legend = "students' sentiments", 
             bty = "n", pch=20 , col= "red" , 
             text.col = "black", cex= 0.9, pt.cex=3)
      
      # Add detailed description below the radar chart
      description <- " The vertices represent different sentiment classifications."
      mtext(description, side=1, line=, cex=0.8, adj=0.5)
    } else if(input$select == 3) {
      
      classmate_data <- df[4, ]  # 选择第三行，因为你在rbind之后添加了两行
      classmate_data <- rbind(rep(100,5), rep(0,5), classmate_data)
      
      # 绘制雷达图
      radarchart(classmate_data,
                 pcol="green",
                 plwd=3,
                 plty=1,
                 cglcol='black',
                 cglty=3,
                 axistype=1,
                 axislabcol='red',
                 caxislabels=seq(0,100,20),
                 cglwd=1,
                 vlcex=0.8)
      
      # Add title
      title(main="Radar chart of students' feeling of classmates about Stats399")
      
      # 添加图例
      legend(x=1.2, y=1.2, legend = "classmates' sentiments", 
             bty = "n", pch=20 , col= "green" , 
             text.col = "black", cex= 0.9, pt.cex=3)
      
      # Add detailed description below the radar chart
      description <- " The vertices represent different sentiment classifications."
      mtext(description, side=1, line=, cex=0.8, adj=0.5)
    } else if(input$select == 4) {
      
      instructor_data <- df[5, ]  # 选择第三行，因为你在rbind之后添加了两行
      instructor_data <- rbind(rep(100,5), rep(0,5), instructor_data)
      
      # 绘制雷达图
      radarchart(instructor_data,
                 pcol="blue",
                 plwd=3,
                 plty=1,
                 cglcol='black',
                 cglty=3,
                 axistype=1,
                 axislabcol='red',
                 caxislabels=seq(0,100,20),
                 cglwd=1,
                 vlcex=0.8)
      
      # Add title
      title(main="Radar chart of students' feelings of instructors about Stats399")
      
      # 添加图例
      legend(x=1.2, y=1.2, legend = "instructors' sentiments", 
             bty = "n", pch=20 , col= "blue" , 
             text.col = "black", cex= 0.9, pt.cex=3)
      
      # Add detailed description below the radar chart
      description <- " The vertices represent different sentiment classifications."
      mtext(description, side=1, line=, cex=0.8, adj=0.5)
    }})
      output$sentiment_wordcloud <- renderPlot({
        colors <- colorRampPalette(brewer.pal(12, "Paired"))(100)  # 创建一个包含100种颜色的调色板
        wordcloud(all_data$word, all_data$n, max.words=500, random.order=FALSE, 
                  colors=colors, 
                  scale=c(7, 3))
        title(main="Word cloud of 'your' feelings about Stats399")}
      
        )
  
}

shinyApp(ui, server)




