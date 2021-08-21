library(shiny)
library(shinydashboard)
library(tidyverse)
library(reshape2)
library(dplyr)
library(TTR)
library(hms)
library(ggwordcloud)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(tm)


data <-
    read_csv("Benchmark_mod.csv")

#add
data <- data %>%
    mutate(severity = ifelse(ANGRY == 5, "red",
                             ifelse(
                                 ANGRY >= 3 & ANGRY < 5,
                                 "orange",
                                 ifelse(ANGRY < 3 &
                                            ANGRY > 1, "yellow",
                                        ifelse(ANGRY <= 1, "green", NA))
                             )))


data1 <- data %>% group_by(chatid, user) %>% count()
data3 <- inner_join(data, data1)


#Dashboard header carrying the title of the dashboard
ui <- dashboardPage(
    dashboardHeader(title = "MAD Dashboard", dropdownMenuOutput("msgOutput")),
    
    dashboardSidebar(sidebarMenu(
        menuItem("User Dashboard",
                 tabName = "main_tab",
                 icon = icon("user")),
        menuItem(
            "Severity",
            tabName = "user_tab",
            icon = shiny::icon("skull-crossbones")
        ),
        menuItem(
            "Admin Dashboard",
            tabName = "admin_tab",
            icon = icon("dashboard")
        )
        
    )),
    
    dashboardBody(tabItems(
        tabItem(
            tabName = "main_tab",
            fluidRow(
                valueBoxOutput("value1", width = 4),
                valueBoxOutput("value2", width = 4),
                valueBoxOutput("value3", width = 4)
            ),
            
            fluidRow(box(
                selectInput(
                    "select_user",
                    label = "Select user",
                    choices = c("All Users", unique(data$user))
                )
            )),
            
            fluidRow(
                box(
                    title = "Happy Score"
                    ,
                    status = "primary"
                    ,
                    solidHeader = TRUE
                    ,
                    collapsible = TRUE
                    ,
                    plotOutput("Happy_level_plot", height = "300px", click = "happy_plot_click")
                ) ,
                box(
                    title = "Angry Score"
                    ,
                    status = "primary"
                    ,
                    solidHeader = TRUE
                    ,
                    collapsible = TRUE
                    ,
                    plotOutput("angry_level_plot", height = "300px", click = "happy_plot_click")
                )
            ),
            fluidRow(box(htmlOutput("happy_text"),
                         width = 16))
        ),
        tabItem(
            tabName = "admin_tab",
            fluidRow(
                box(
                    title = "Bot Monitoring"
                    ,
                    status = "primary"
                    ,
                    solidHeader = TRUE
                    ,
                    collapsible = TRUE
                    ,
                    plotOutput("bot_monitoring", width = "100%", height = "400px")
                    ,
                    width = "600px"
                )),
            fluidRow(
                box(
                    title = "Users Monitoring"
                    ,
                    status = "primary"
                    ,
                    solidHeader = TRUE
                    ,
                    collapsible = TRUE
                    ,
                    plotOutput("user_monitoring", width = "100%", height = "400px")
                    ,
                    width = "600px"
                )
            ),
            fluidRow(
                box(
                    title = "Number of conversations last 15 chats"
                    ,
                    status = "primary"
                    ,
                    solidHeader = TRUE
                    ,
                    collapsible = TRUE
                    ,
                    plotOutput("num_chats", width = "100%", height = "400px")
                    ,
                    width = "600px"
                )
            ),
            fluidRow(
                box(
                    title = "Conversation durations last 15 chats"
                    ,
                    status = "primary"
                    ,
                    solidHeader = TRUE
                    ,
                    collapsible = TRUE
                    ,
                    plotOutput("chats_durations", width = "100%", height = "400px")
                    ,
                    width = "600px"
                )
            ),
            fluidRow(
                box(
                    title = "User chat Word frequency last 100 chats"
                    ,
                    status = "primary"
                    ,
                    solidHeader = TRUE
                    ,
                    collapsible = TRUE
                    ,
                    plotOutput("user_word_freq", width = "100%", height = "400px")
                    ,
                    width = "600px"
                )
            ),
            fluidRow(
                box(
                    title = "User chat Wordcloud last 100 chats"
                    ,
                    status = "primary"
                    ,
                    solidHeader = TRUE
                    ,
                    collapsible = TRUE
                    ,
                    plotOutput("user_wordcloud", width = "100%", height = "400px")
                    ,
                    width = "600px"
                )
            )    
        ),
        tabItem(tabName = "user_tab",
                fluidRow(h1(
                    box(
                        radioButtons(
                            "sev_rad_1",
                            label = HTML('<FONT color="black"><FONT size="5pt">Select Severity Level'),
                            choices = c("red", "orange", "yellow", "green"),
                            selected = "red",
                            inline = TRUE,
                            width = '100%'
                        ),
                        width = 16
                    )
                )),
                fluidRow(h1(
                    box(
                        plotOutput("angry_plot1", click = "severity_plot_click"),
                        width = 16
                    )
                )),
                fluidRow(h1(
                    box(htmlOutput("severity_text"),
                        width = 16)
                )))
    ))
)



# create the server functions for the dashboard
server <- function(input, output, session) {
    ##write.csv(data,file = "C:/Users/vijet/OneDrive/OMIS 605/New folder/Benchmark.csv")
    
    output$msgOutput <- renderMenu({
        msgs <- apply(read.csv("red code.csv"), 1, function(row) {
            messageItem(
                from = row[["user"]],
                message = row[["text"]],
                time = row[["Time"]],
                icon = shiny::icon("user-circle")
            )
        })
        dropdownMenu(type = "message", .list = msgs)
    })
    
    #creating the valueBox Output content
    output$value1 <- renderValueBox({
        value <- data %>%
            filter(str_detect(
                user,
                if_else(input$select_user == "All Users", "", input$select_user)
            )) %>%
            summarise(value = mean(ANGRY, na.rm = TRUE)) %>%
            pluck("value", 1)
        
        valueBox(
            format(
                round(value, digits = 3),
                format = "d",
                big.mark = ','
            ),
            subtitle = "Average Angry Score",
            icon = icon("angry", lib = 'font-awesome'),
            color = "red"
        )
    })
    
    output$value2 <- renderValueBox({
        value <- data %>%
            filter(str_detect(
                user,
                if_else(input$select_user == "All Users", "", input$select_user)
            )) %>%
            summarise(value = mean(HAPPY, na.rm = TRUE)) %>%
            pluck("value", 1)
        
        valueBox(
            format(
                round(value, digits = 3),
                format = "d",
                big.mark = ','
            ),
            subtitle = "Average Happy Score",
            icon = icon("smile", lib = 'font-awesome'),
            color = "green"
        )
    })
    
    output$value3 <- renderValueBox({
        final <-  data %>%
            group_by(user) %>%
            mutate(latest_chat = max(chatorder)) %>%
            filter(chatorder == latest_chat)
        
        
        value <- if (input$select_user == "All Users")
        {
            value <- sum(final$latest_chat, na.rm = TRUE)
        }
        else
        {
            value <- final %>%
                filter(str_detect(user, input$select_user)) %>%
                summarise(value = latest_chat, na.rm = TRUE) %>%
                pluck("value", 1)
        }
        valueBox(
            format(value,
                   format = "d",
                   big.mark = ','),
            subtitle = "Total Utterances",
            icon = icon("chart-bar", lib = 'font-awesome'),
            color = "orange"
        )
    })
    
    #creating the plotOutput content
    output$Happy_level_plot <- renderPlot({
        if (input$select_user == "All Users") {
            filtered <-
                data %>%
                group_by(user) %>%
                mutate(latest_chat = max(chatorder)) %>%
                filter(chatorder == latest_chat)
            
            #Plot
            g <-
                ggplot(data = filtered, aes(y = HAPPY,
                                            x = user))
            g + geom_bar(stat = "identity", fill = "green4") +
                geom_text(
                    aes(label = latest_chat),
                    vjust = 1.5,
                    color = "black",
                    size = 3.5
                ) +
                ggtitle("Most recent Happy score by user")
        }
        else{
            filtered <-
                data %>%
                filter(user == input$select_user)
            
            #Plot
            g <-
                ggplot(data = filtered, aes(
                    x = chatorder,
                    y = HAPPY,
                    group = 1
                ))
            g +    geom_line(color = "green4") +
                geom_point()
        }
        
    })
    
    
    output$angry_level_plot <- renderPlot({
        if (input$select_user == "All Users") {
            filtered <-
                data %>%
                group_by(user) %>%
                mutate(latest_chat = max(chatorder)) %>%
                filter(chatorder == latest_chat)
            
            #Plot
            g <-
                ggplot(data = filtered, aes(
                    y = ANGRY,
                    x = user,
                    fill = ANGRY
                ))
            g + geom_bar(stat = "identity", fill = "coral1") +
                geom_text(
                    aes(label = latest_chat),
                    vjust = 1.5,
                    color = "black",
                    size = 3.5
                ) +
                ggtitle("Most recent angry score by user")
        }
        else{
            filtered <-
                data %>%
                filter(user == input$select_user)
            #Plot
            g <-
                ggplot(data = filtered, aes(
                    x = chatorder,
                    y = ANGRY,
                    group = 1
                ))
            g +    geom_line(color = "red") +
                geom_point()
            
        }
        
    })
    
    output$happy_text <- renderUI({
        if (input$select_user != "All Users") {
            x_val = input$happy_plot_click$x
            
            if (is.numeric(x_val)) {
                selected_chat_order = round(x_val, digits = 0)
                filtered <-
                    data %>%
                    filter(user == input$select_user) %>%
                    filter(chatorder == selected_chat_order)
                
                out <- "<!DOCTYPE html><html><body><h4>"
                out <- str_c(out, filtered$text)
                out <- str_c(out, "</h4></body></html>")
                HTML(out)
            }
        }
    })
    
    
    severity_df <- data %>%  filter(user != "Bot") %>%
        group_by(user) %>%
        mutate(latest_chat = max(chatorder)) %>%
        mutate(running_avg = round(runMean(ANGRY, 5), digits = 2)) %>%
        filter(chatorder == latest_chat)
    
    # Severity page
    output$angry_plot1 <- renderPlot({
        filtered_sev_df <- severity_df %>%
            filter(severity == input$sev_rad_1) %>%
            select(user, ANGRY, running_avg) %>%
            melt() %>%
            mutate(col_color = case_when(variable == "ANGRY" ~ input$sev_rad_1,
                                         TRUE ~ "gray49"))
        ggplot(filtered_sev_df, aes(user, value, fill = col_color)) +
            geom_bar(stat = "identity", position = "dodge") +
            scale_fill_manual(
                values = c(
                    "green" = "green",
                    "red" = "red",
                    "orange" = "orange",
                    "yellow" = "yellow",
                    "gray49" = "gray49"
                )
            ) +
            scale_fill_identity(name = 'legend', guide = 'legend',labels = c('Running Avg', 'Last Chat')) +
            geom_text(
                aes(
                    x = user,
                    y = value,
                    label = (value)
                ),
                vjust = -0.2,
                color = "black",
                size = 4.5,
                position = position_dodge2(width = 1)
            )
    })
    
    output$severity_text <- renderUI({
        x_val <- input$severity_plot_click$x
        if (is.numeric(x_val)) {
            selected_user_no = round(x_val, digits = 0)
            
            users_df <- severity_df %>%
                filter(severity == input$sev_rad_1) %>%
                arrange(user)
            
            selected_chat_id <- users_df[selected_user_no,]$chatid
            
            all_chats_selected_user <-
                data %>%
                filter(chatid == selected_chat_id) %>%
                top_n(6, chatorder) %>%
                select(user, text)
            
            out <-
                "<!DOCTYPE html><html><body><h2>Recent Chats:</h2><dl>"
            
            for (i in 1:nrow(all_chats_selected_user)) {
                out <-
                    str_c(
                        out,
                        "<dt><h4><b>",
                        all_chats_selected_user[i, ]$user,
                        "</b></h4></dt>",
                        "<dd><h5>",
                        all_chats_selected_user[i, ]$text,
                        "</5></dd>"
                    )
            }
            out <- str_c(out, "</dl></body></html>")
            HTML(out)
        }
    })
    
    #Bot monitoring
    output$bot_monitoring <- renderPlot({
        
        bot_df <-  data %>%
            filter(user == "Bot") %>%
            select(date_time, ANGRY, HAPPY) %>%
            group_by(date_time) %>%
            summarise(across(everything(), list(mean = mean, max = max))) %>%
            tail(10)
        
        ggplot(data=bot_df, aes(x=date_time, group = 1)) +
            geom_line(aes(y=ANGRY_mean),color = "coral", linetype = "longdash", size = 1) +
            geom_line(aes(y=ANGRY_max),color = "firebrick1", size = 1) +
            geom_line(aes(y=HAPPY_mean),color = "chartreuse3", linetype = "longdash", size = 1) +
            geom_line(aes(y=HAPPY_max),color = "forestgreen", size = 1)+
            ylab('Emotion Level') + xlab('Time')
        # theme(
        #     legend.position = c(0.95, 0.95),
        #     legend.justification = c("right", "top")
        # ) +
        # scale_colour_manual(name = 'legend',
        #                     values =c('coral'='coral','firebrick1'='firebrick1','chartreuse3'='chartreuse3','forestgreen'='forestgreen'),
        #                     labels = c('Angry Mean','Angry Max', 'Happy Mean', 'Happy Max'))
        
        
    })
    
    #User monitoring
    output$user_monitoring <- renderPlot({
        
        user_df <-  data %>%
            filter(user != "Bot") %>%
            select(date_time, ANGRY, HAPPY) %>%
            group_by(date_time) %>%
            summarise(across(everything(), list(mean = mean, max = max))) %>%
            tail(10)
        
        ggplot(data=user_df, aes(x=date_time, group = 1)) +
            geom_line(aes(y=ANGRY_mean),color = "coral", linetype = "longdash", size = 1) +
            geom_line(aes(y=ANGRY_max),color = "firebrick1", size = 1) +
            geom_line(aes(y=HAPPY_mean),color = "chartreuse3", linetype = "longdash", size = 1) +
            geom_line(aes(y=HAPPY_max),color = "forestgreen", size = 1)+
            ylab('Emotion Level') + xlab('Time')  
        
    })
    
    #Number of Chats monitoring
    output$num_chats <- renderPlot({
        
        num_chats_df <-  data %>%
            select(chatid) %>%
            group_by(chatid) %>%
            summarise(chat_count = n()) %>%
            tail(15)
        
        ggplot(num_chats_df, aes(chatid, chat_count, fill = "blue")) +
            geom_bar(stat = "identity", position = "dodge") +
            scale_fill_identity(name = 'legend', guide = 'legend',labels = c('Num of Conversations')) +
            geom_text(
                aes(
                    x = chatid,
                    y = chat_count,
                    label = (chat_count)
                ),
                vjust = -0.2,
                color = "black",
                size = 4.5,
                position = position_dodge2(width = 1)
            )
    })
    
    #Number of Chats monitoring
    # output$chats_durations <- renderPlot({
    #     
    #     chat_dur_df <-  data %>%
    #         select(chatid, date_time) %>%
    #         group_by(chatid) %>%
    #         summarise(chat_dur = difftime(max(date_time), min(date_time), units = "mins")) %>%
    #         tail(15)
    #     
    #     print(chat_dur_df)
    #     ggplot(num_chats_df, aes(chatid, chat_count, fill = "blue")) +
    #         geom_bar(stat = "identity", position = "dodge") +
    #         scale_fill_identity(name = 'legend', guide = 'legend',labels = c('Num of Conversations')) +
    #         geom_text(
    #             aes(
    #                 x = chatid,
    #                 y = chat_count,
    #                 label = (chat_count)
    #             ),
    #             vjust = -0.2,
    #             color = "black",
    #             size = 4.5,
    #             position = position_dodge2(width = 1)
    #         )
    # })
    
    #User Word frequency
    output$user_word_freq <- renderPlot({
        
        user_text <-  data %>%
            filter(user != "Bot") %>%
            select(text) %>%
            tail(100)
        
        gsub("https\\S*", "", user_text) 
        gsub("@\\S*", "", user_text) 
        gsub("amp", "", user_text) 
        gsub("[\r\n]", "", user_text)
        gsub("[[:punct:]]", "", user_text)
        
        # Create a corpus  
        docs <- Corpus(VectorSource(user_text))
        
        dtm <- TermDocumentMatrix(docs) 
        matrix <- as.matrix(dtm) 
        words <- sort(rowSums(matrix),decreasing=TRUE) 
        top20 <- data.frame(word = names(words),freq=words) %>%
            head(20)
        top20$word <- reorder(top20$word, top20$freq)
        
        ggplot(top20, aes(x = word, y = freq, fill = word, label = freq)) +
            geom_bar(stat="identity", show.legend = FALSE) +
            coord_flip() +
            labs(title = "Top 20 Most Used Words in Chats", x = "Word", y = "Word Count") +
            geom_label(aes(fill = word),colour = "white", fontface = "bold", show.legend = FALSE)
    })
    
    #User Wordcloud
    output$user_wordcloud <- renderPlot({
        
        user_text <-  data %>%
            filter(user != "Bot") %>%
            select(text) %>%
            tail(100)
        
        gsub("https\\S*", "", user_text) 
        gsub("@\\S*", "", user_text) 
        gsub("amp", "", user_text) 
        gsub("[\r\n]", "", user_text)
        gsub("[[:punct:]]", "", user_text)
        
        # Create a corpus  
        docs <- Corpus(VectorSource(user_text))
        
        dtm <- TermDocumentMatrix(docs) 
        matrix <- as.matrix(dtm) 
        words <- sort(rowSums(matrix),decreasing=TRUE) 
        df <- data.frame(word = names(words),freq=words)
        
        wordcloud(words = df$word,
                  freq = df$freq,
                  min.freq = 1,
                  max.words=200,
                  random.order=FALSE,
                  colors=brewer.pal(8, "Dark2"),
                  rot.per=0,
                  fixed.asp=FALSE)
        
    })
}


#run/call the shiny app
shinyApp(ui, server)
