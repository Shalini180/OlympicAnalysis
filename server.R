library(shiny)
library(plotly)
library(DT)
library(naivebayes)
library(dplyr)
library(ggplot2)
library(psych)
library(class)
library(e1071)
library(caTools)
library(ROCR)
library(shinydashboard)
shinyServer(function(input,output){
  output$plot<-renderPlotly({
    olympics<- read.csv('C:/olypics_preprocessed.csv')
    df<-olympics[!(olympics$Medal=="DNW"),]
    qplot(x=Age, data=df,
          xlab="Age",
          ylab="Number of People",
          main = ("Distribution of Ages"),
          geom="histogram",
          binwidth=1,
          fill=I("pink"),
          col=I("blue"))+
      scale_x_continuous(limits = c(10,71), breaks = seq(10,71,10))
  })
  output$plot1<-renderPlotly({
    df<- read.csv('C:/olypics_preprocessed.csv')
    p <- ggplot(df, aes(x = `Sport`))+
      geom_bar(color="darkblue", fill="lightblue")+
      ggtitle("Distribution of Sport Branchs") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    p
    
  })
  output$plot2<-renderPlotly({
    df<- read.csv('C:/olypics_preprocessed.csv')
    qplot(x=Height, data=df,
          xlab="Height",
          ylab="Number of People",
          main = ("Distribution of Height"),
          geom="histogram",
          binwidth=1,
          fill=I("black"),
          col=I("blue"))+
      scale_x_continuous(limits = c(127,226), breaks = seq(127,226,10))
    
  })
  output$plot3<-renderPlotly({
    df<- read.csv('C:/olypics_preprocessed.csv')
    qplot(x=Weight, data=df,
          xlab="Weight",
          ylab="Number of People",
          main = ("Distribution of Weight"),
          geom="histogram",
          binwidth=1,
          fill=I("grey"),
          col=I("red"))+
      scale_x_continuous(limits = c(25,214), breaks = seq(25,214,10))
  })
  output$plot4<-renderPlotly({
    df<- read.csv('C:/olypics_preprocessed.csv')
    qplot(x=Medal, y=Age, data=df, main=('Distribution of Medal by Age'), geom='boxplot', color='purple')
    #historical medal counts of football in Olympics
    
    
  })
  output$plot5<-renderPlotly({
    df<- read.csv('C:/olypics_preprocessed.csv')
    df %>%
      group_by(Year, Season) %>%
      summarise(NoOfCountries = length(unique(NOC))) %>%
      ggplot(aes(x = Year, y = NoOfCountries, group = Season)) +
      geom_line(aes(color = Season)) +
      geom_point(aes(color = Season)) +
      labs(x = "Year", y = "No of countries that participated", title = "no of countries that participated in the Olympics") +
      theme_minimal()
    
    
  })
  output$plot6<-renderPlotly({
    df<- read.csv('C:/olypics_preprocessed.csv')
    counts <- df %>% group_by(Year, Sex) %>%summarize(Athletes = length(unique(ID)))
    counts$Year <- as.integer(counts$Year)
    
    ggplot(counts, aes(x=Year, y=Athletes, color=Sex)) +
      geom_point(size=2) +
      geom_line()  +
      scale_color_manual(values=c("darkblue","red")) +
      labs(title = "Number of male and female Olympians over time") +
      theme(plot.title = element_text(hjust = 0.5))
    
  })
  output$plot7<-renderPlotly({
    df<- read.csv('C:/olypics_preprocessed.csv')
    ggplot(df, aes(x=Sex, fill=Medal )) +
      geom_bar(position = position_dodge()) +ggtitle("MEDAL WON BY MALE AND FEMALE")+
      theme_classic()
    
    
    
  })
  output$plot8<-renderPlotly({
    olympics<- read.csv('C:/olypics_preprocessed.csv')
    df<-olympics[!(olympics$Medal=="DNW"),]
    df1<-df %>% group_by(NOC,Medal) %>% summarise(n = n()) %>% arrange(desc(n))
    df2<-head(df1,30)
    
    plot_ly(df2, x = ~NOC, y = ~n, type = 'bar', 
            name = ~Medal, color = ~Medal) %>%
      layout(yaxis = list(title = 'Count'), barmode = 'stack')
    
  })
  output$plot9<-renderPlotly({
    olympics<- read.csv('C:/olypics_preprocessed.csv')
    df<-olympics[!(olympics$Medal=="DNW"),]
    df3<-df %>% group_by(Sport,Medal) %>% summarise(n = n()) %>% arrange(desc(n))
    df4<-head(df3,30)
    plot_ly(df4, x = ~Sport, y = ~n, type = 'bar', 
            name = ~Medal, color = ~Medal) %>%
      layout(yaxis = list(title = 'Count'), barmode = 'stack')
    
  })
  output$plot10<-renderPlotly({
    olympics<- read.csv('C:/olypics_preprocessed.csv')
    df<-olympics[!(olympics$Medal=="DNW"),]
    df5<-df %>% group_by(Age,Medal) %>% summarise(n = n()) %>% arrange(desc(n))
    df5<-head(df5,40)
    
    
    df5 %>%
      plot_ly(
        x = ~n
        ,y = ~Age
        ,color = ~Medal
        ,name = ~Medal
        ,type = "bar"
        ,orientation = "h"
      ) %>%
      layout(
        barmode = "stack"
      )
    
  })
  output$plot11<-renderPlotly({
    olympics<- read.csv('C:/olypics_preprocessed.csv')
    df<-olympics[!(olympics$Medal=="DNW"),]
    fig <- plot_ly(df, x = ~Height, y = ~Weight, text = ~Name, type = 'scatter', mode = 'markers', size = ~Age, color = ~Age, colors = 'Paired',
                   #Choosing the range of the bubbles' sizes:
                   sizes = c(10, 50),
                   marker = list(opacity = 0.5, sizemode = 'diameter'))
    fig <- fig %>% layout(title = 'Weight vs Height',
                          xaxis = list(showgrid = FALSE),
                          yaxis = list(showgrid = FALSE),
                          showlegend = FALSE)
    
    fig
    
  })
  output$plot12<-renderPlotly({
    olympics<- read.csv('C:/olypics_preprocessed.csv')
    df<-olympics[!(olympics$Medal=="DNW"),]
    df1<-df[order(df$GDP,decreasing = TRUE),]
    df1<-head(df1,4000)
    
    
    fig <- plot_ly(df1, labels = ~df1$NOC, values = ~df1$GDP, type = 'pie')
    fig <- fig %>% layout(title = 'GDP of the country',
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    fig
    
    
  })
  output$plot13<-renderPlotly({
    olympics<- read.csv('C:/olypics_preprocessed.csv')
    df<-olympics[!(olympics$Medal=="DNW"),]
    fig <- plot_ly(df, x = ~df$NOC, y = ~df$Population*1000, type = 'bar', name = 'Population')
    fig <- fig %>% add_trace(y = ~df$GDP, name = 'GDP')
    fig <- fig %>% layout(yaxis = list(title = 'Count'), barmode = 'group')
    
    fig
  })
  
  output$plot15<-renderPlotly({
    olympics<- read.csv('C:/olypics_preprocessed.csv')
    df<-olympics[!(olympics$Medal=="DNW"),]
    ftbl <- df %>% filter(Sport == input$Sport) %>%select(Name, Sex, Age, Team, NOC, Year, City, Event, Medal)
    
    medal_counts_ftbl <- ftbl %>% group_by(Team, Medal) %>%summarize(Count=length(Medal)) 
    
    ggplot(medal_counts_ftbl, aes(x=Team, y=Count, fill=Medal)) +
      geom_col() +
      coord_flip() +
      scale_fill_manual(values=c("#CD7F32","#FFD700","#C0C0C0")) +
      ggtitle("Historical medal counts") +
      theme(plot.title = element_text(hjust = 0.5))
    
    
  })
  output$plot16<-renderPlotly({
    olympics<- read.csv('C:/olypics_preprocessed.csv')
    df<-olympics[!(olympics$Medal=="DNW"),]
    df1<-df %>% group_by(Team,Medal,GDP,Year) %>% summarise(n = n()) %>% arrange(desc(Year))
    df1<-head(df1,1000)
    fig <- plot_ly(df1, x = ~Year, y = ~n*10000000000, type = 'bar', name = 'count of medal')
    fig <- fig %>% add_trace(y = ~GDP, name = 'GDP')
    fig <- fig %>% layout(yaxis = list(title = 'Count'), barmode = 'group')
    
    
    fig
    
    
  })
  output$plot17<-renderPlotly({
    olympics<- read.csv('C:/olypics_preprocessed.csv')
    df<-olympics[!(olympics$Medal=="DNW"),]
    df1<-df %>% group_by(Team,Medal,GDP,Year) %>% summarise(n = n()) %>% arrange(desc(Year))
    df1<-head(df1,1000)
    fig <- plot_ly(df1, x = ~Team, y = ~n*10000000000, type = 'bar', name = 'count of medal')
    fig <- fig %>% add_trace(y = ~GDP, name = 'GDP')
    fig <- fig %>% layout(yaxis = list(title = 'Count'), barmode = 'group')
    
    fig
    
    
  })
  output$mytable = DT::renderDataTable({
    olympics<- read.csv('C:/olypics_preprocessed.csv')
    df<-olympics[!(olympics$Medal=="DNW"),]
    df$Year <- as.numeric(df$Year)
    medals <- df[!is.na(df$Year), ]
    summer<-filter(medals,Season=="Summer")
    medalsByYear <- aggregate(summer$Year, list(Year = summer$Year), length)
    
    
    #Graph9:Which countries have won the most medals?
    sort(table(summer$NOC), dec = TRUE)
    NOC50Plus <- names(table(summer$NOC)[table(summer$NOC) > 50])
    medalsSubset <- medals[summer$NOC %in% NOC50Plus, ]
    medalsByMedalByNOC <- prop.table(table(medalsSubset$NOC, medalsSubset$Medal), margin = 1)
    medalsByMedalByNOC <- medalsByMedalByNOC[order(medalsByMedalByNOC[, "Gold"], 
                                                   decreasing = TRUE), c("Gold", "Silver", "Bronze")]
    round(medalsByMedalByNOC, 2)
  })
  output$mytable1 = DT::renderDataTable({
    olympics<- read.csv('C:/olypics_preprocessed.csv')
    df<-olympics[!(olympics$Medal=="DNW"),]
    df$Year <- as.numeric(df$Year)
    medals <- df[!is.na(df$Year), ]
    summer<-filter(medals,Season=="Summer")
    medalsByYear <- aggregate(summer$Year, list(Year = summer$Year), length)
    
    
    #Graph9:Which countries have won the most medals?
    sort(table(summer$NOC), dec = TRUE)
    NOC50Plus <- names(table(summer$NOC)[table(summer$NOC) > 50])
    medalsSubset <- medals[summer$NOC %in% NOC50Plus, ]
    medalsByMedalByNOC <- prop.table(table(medalsSubset$NOC, medalsSubset$Medal), margin = 1)
    medalsByMedalByNOC <- medalsByMedalByNOC[order(medalsByMedalByNOC[, "Gold"], 
                                                   decreasing = TRUE), c("Gold", "Silver", "Bronze")]
    propYearsOnePlusMedals <- apply(table(summer$NOC, summer$Year) > 0, 1, mean)
    names(propYearsOnePlusMedals[propYearsOnePlusMedals == 1.0]) 
    cbind(sort(propYearsOnePlusMedals, decreasing = TRUE))
    
  })
  
  output$plot18<-renderPlotly({
    olympics<- read.csv('C:/olypics_preprocessed.csv')
    df<-olympics[!(olympics$Medal=="DNW"),]
    df$Year <- as.numeric(df$Year)
    medals <- df[!is.na(df$Year), ]
    summer<-filter(medals,Season=="Summer")
    medalsByYear <- aggregate(summer$Year, list(Year = summer$Year), length)
    medalsByYear %>% 
      ggplot(aes(x=medalsByYear$Year,y=medalsByYear$x)) + 
      geom_point() + 
      labs(x="Year",y="Total Medal Count") +
      ggtitle("Total Medals Awarded in summer Olympics by Year") +
      theme(plot.title = element_text(face="bold", size=14, hjust=0.5)) +
      theme(axis.title = element_text(face="bold")) 
    
  })
  
  output$mytable2 = DT::renderDataTable({
    olympics<- read.csv('C:/olypics_preprocessed.csv')
    df<-olympics[!(olympics$Medal=="DNW"),]
    df$Year <- as.numeric(df$Year)
    medals <- df[!is.na(df$Year), ]
    winter<-filter(medals,medals$Season=="Winter")
    sapply(winter, function(x) cbind(sort(table(x), decreasing = TRUE)))
    medalsByYear<- aggregate(winter$Year, list(Year = winter$Year), length)
    
    
    #Graph9:Which countries have won the most medals?
    sort(table(winter$NOC), dec = TRUE)
    NOC50Plus <- names(table(winter$NOC)[table(winter$NOC) > 50])
    medalsSubset <- medals[winter$NOC %in% NOC50Plus, ]
    medalsByMedalByNOC <- prop.table(table(medalsSubset$NOC, medalsSubset$Medal), margin = 1)
    medalsByMedalByNOC <- medalsByMedalByNOC[order(medalsByMedalByNOC[, "Gold"], 
                                                   decreasing = TRUE), c("Gold", "Silver", "Bronze")]
    round(medalsByMedalByNOC, 2)
    
  })
  output$mytable3 = DT::renderDataTable({
    olympics<- read.csv('C:/olypics_preprocessed.csv')
    df<-olympics[!(olympics$Medal=="DNW"),]
    df$Year <- as.numeric(df$Year)
    medals <- df[!is.na(df$Year), ]
    winter<-filter(medals,medals$Season=="Winter")
    medalsByYear<- aggregate(winter$Year, list(Year = winter$Year), length)
    
    
    #Graph9:Which countries have won the most medals?
    sort(table(winter$NOC), dec = TRUE)
    NOC50Plus <- names(table(winter$NOC)[table(winter$NOC) > 50])
    medalsSubset <- medals[winter$NOC %in% NOC50Plus, ]
    medalsByMedalByNOC <- prop.table(table(medalsSubset$NOC, medalsSubset$Medal), margin = 1)
    medalsByMedalByNOC <- medalsByMedalByNOC[order(medalsByMedalByNOC[, "Gold"], 
                                                   decreasing = TRUE), c("Gold", "Silver", "Bronze")]
    propYearsOnePlusMedals <- apply(table(winter$NOC, winter$Year) > 0, 1, mean)
    names(propYearsOnePlusMedals[propYearsOnePlusMedals == 1.0]) 
    cbind(sort(propYearsOnePlusMedals, decreasing = TRUE))
    
  })
  output$plot19<-renderPlotly({
    olympics<- read.csv('C:/olypics_preprocessed.csv')
    df<-olympics[!(olympics$Medal=="DNW"),]
    df$Year <- as.numeric(df$Year)
    medals <- df[!is.na(df$Year), ]
    winter<-filter(medals,Season=="Winter")
    
    medalsByYear <- aggregate(winter$Year, list(Year = winter$Year), length)
    medalsByYear %>% 
      ggplot(aes(x=medalsByYear$Year,y=medalsByYear$x)) + 
      geom_point() + 
      labs(x="Year",y="Total Medal Count") +
      ggtitle("Total Medals Awarded in Winter Olympics by Year") +
      theme(plot.title = element_text(face="bold", size=14, hjust=0.5)) +
      theme(axis.title = element_text(face="bold")) 
  })
  
  
  output$Alignment<-renderImage({
    olympics<- read.csv('C:/olypics_preprocessed.csv')
    df = subset(olympics, select = -c(X,ID,Name,NOC,Games,City,Event,notes,Country.Code,Country) )
    p =df$Medal
    df = subset(df, select = -c(Medal) )
    df$Medal <- p
    b<-df
    df$Season <- as.numeric(as.factor(df$Season))
    df$Sport <- as.numeric(as.factor(df$Sport))
    df$Team <- as.numeric(as.factor(df$Team))
    df$Medal <- as.numeric(as.factor(df$Medal))
    df$Sex <- as.numeric(as.factor(df$Sex))
    
    df$Height[is.na(df$Height)]<-mean(df$Height,na.rm=TRUE)
    df$Age[is.na(df$Age)]<-mean(df$Age,na.rm=TRUE)
    df$Weight[is.na(df$Weight)]<-mean(df$Weight,na.rm=TRUE)
    df$GDP[is.na(df$GDP)]<-mean(df$GDP,na.rm=TRUE)
    df$Population[is.na(df$Population)]<-mean(df$Population,na.rm=TRUE)
    
    split <- sample.split(df, SplitRatio = 0.7)
    train_cl <- subset(df, split == "TRUE")
    test_cl <- subset(df, split == "FALSE")
    
    train_scale <- scale(train_cl[, 1:8])
    test_scale <- scale(test_cl[, 1:8])
    
    summary(df)
    #Prediction
    s1<-levels(factor(b$Sex))
    s2<-as.numeric(levels(factor(df$Sex)))
    ss1<-levels(factor(b$Season))
    
    ss2<-as.numeric(levels(factor(df$Season)))
    
    
    sp1<-levels(factor(b$Sport))
    
    sp2<-as.numeric(levels(factor(df$Sport)))
    
    
    t1<-levels(factor(b$Team))
    
    t2<-as.numeric(levels(factor(df$Team)))
    
    medal1<-levels(factor(b$Medal))
    #medal1
    medal2<-as.numeric(levels(factor(df$Medal)))
    #medal2
    #input1 categorical:
    Sex <- input$Sex
    Age <- input$Age
    Height <-input$Height
    Weight <- input$Weight
    Year <- input$Year
    Season <- input$Season
    Sport <-input$Sport
    Team <- input$Team
    
    S1<-Sex
    p<-as.numeric(match(S1,s1))
    Sex=s2[p]
    
    Season1=Season
    p<-as.numeric(match(Season1,ss1))
    Season<-ss2[p]
    Sport1<-Sport
    p<-as.numeric(match(Sport1,sp1))
    Sport<-sp2[p]
    #Team
    Team1 <- Team
    p<-as.numeric(match(Team1,t1))
    Team=t2[p]
    x=data.frame(Sex=Sex,Age=Age,Height=Height,Weight=Weight,Year=Year,Season=Season,Sport=Sport,Team=Team)
    #x=c(Sex,Age,Height,Weight,Year,Season,Sport,Team)
    z<-knn(train=train_cl[,1:8],test=x,cl=train_cl$Medal,k=15)
    z1<-as.numeric(match(z,medal2))
    #cat("Medal :",medal1[z1])
    if(medal1[z1]=="DNW"){
      return(list(src = "c:/Capture.PNG",contentType = "image/png",width = 350,
                  height = 160,alt = "Alignment"))
    }
    else if(medal1[z1]=="Gold"){
      return(list(src = "c:/Capture1.PNG",contentType = "image/png",width = 350,
                  height = 160,alt = "Alignment"))
    }
    else if(medal1[z1]=="Silver"){
      return(list(src = "c:/silver.PNG",contentType = "image/png",width = 350,
                  height = 160,width = 400,
                  height = 300,alt = "Alignment"))
    }
    else{
      return(list(src = "c:/Capture3.PNG",contentType = "image/png",width = 350,
                  height = 160,alt = "Alignment"))
    }
  })
})