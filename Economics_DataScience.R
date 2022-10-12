# Import libraries for Shiny App
library(shiny)
library(shinythemes)
library(shinyalert)
library(plotly)

# Import libraries for data and modeling
library(fpp3)
library(dplyr)
library(gtrendsR)
library(vars)
library(reshape2)

# Grab Google Trends data for Data Science and Economics, if API limit is reached get most recent csv
read_data <- function() {
  tryCatch(
    {
      df <- gtrends(c("Data Science", "Economics"), time = "all", )$interest_over_time
      df <- subset(df, select = c("date", "hits", "keyword"))
      write.csv(df, "data.csv", row.names = FALSE)
    },
    error = function(e) {
      df <- read.csv("data.csv")
    }
  )
  return(df)
}

# Save out data
df <- read_data()

# Clean data frame
Data_Science <- df[which(df$keyword == "Data Science"), c("date", "hits")]
Economics <- df[which(df$keyword == "Economics"), c("date", "hits")]
df <- merge(Data_Science, Economics, by = "date", )
df <- df %>%
  rename(
    "Data_Science" = "hits.x",
    "Economics" = "hits.y"
  )

# Group by year and month to get proper interval
df$Date <- as.Date(df$date, format = "%Y-%m")

df <- df %>%
  group_by(Date) %>%
  mutate(Date = yearmonth(date)) %>%
  summarize(
    Economics = sum(as.numeric(Economics)),
    Data_Science = sum(as.numeric(Data_Science))
  )

# Convert to tsibble
df <- df %>%
  as_tsibble(index = Date)

# Remove last row since it is an incomplete observation
df <- slice(df, 1:(n() - 1))

# Replace null value with 1
df[is.na(df)] <- 1

# Save out Data Science where it starts to see current trend
tempds_df <- df[df$Date >= yearmonth("2015-01"), ]

# Define variables
ec <- ts(df$Economics, start = c(2004, 1), frequency = 12)
ds <- ts(df$Data_Science, start = c(2004, 1), frequency = 12)

# Reconstruct format to make it compatible for VAR
ecds_df <- cbind(ds, ec)
colnames(ecds_df) <- cbind("Data_Science", "Economics")

# Create VAR model
varmodel <- VAR(ecds_df, p = 19, type = "both")

# Develop Shiny App
ui <- fluidPage(
  theme = shinytheme("united"), # Main theme
  navbarPage(
    "Economics vs. Data Science:", # Main Title
    useShinyalert(force = TRUE),
    tabPanel(
      "Exploring Data", # First tab
      fluidRow( # Separate into two columns
        column(
          6,
          h2("Economics"),
          plotlyOutput("econts"), # Display general plot
          h3("Initial Thoughts"),
          div(align = "left", p(uiOutput(outputId = "econtxt"),
            style = "text-align:left; color:black"
          )), # Display initial analysis
          h3(""),
          div(
            align = "left",
            selectInput(
              inputId = "selected",
              label = "Select a graph", # User chooses graph
              choices = c(
                "Autocorrelation",
                "Seasonality",
                "Decomposition"
              )
            )
          ),
          plotlyOutput(outputId = "choseplt"), # Display chosen graph
          h3("Analysis"),
          div(align = "left", p(uiOutput(outputId = "chosetxt"),
            style = "text-align:left; color:black"
          )), # Show corresponding analysis
          h3("")
        ),
        column(
          6, # Establish other column and repeat similar steps
          h2("Data Science"),
          plotlyOutput("dsts"),
          h3("Initial Thoughts"),
          div(align = "left", p(uiOutput(outputId = "dstxt"),
            style = "text-align:left; color:black"
          )),
          h3(""),
          div(
            align = "left",
            selectInput(
              inputId = "selected2",
              label = "Select a graph",
              choices = c(
                "Autocorrelation",
                "Seasonality",
                "Decomposition"
              )
            )
          ),
          plotlyOutput(outputId = "choseplt2"),
          h3("Analysis"),
          div(align = "left", p(uiOutput(outputId = "chosetxt2"),
            style = "text-align:left; color:black"
          )),
          h3("")
        )
      )
    ),

    # Creates new panel for Data Science forecast section
    tabPanel(
      "Forecast Data Science",
      fluidRow(
        column(2),
        column(8, # Makes width of app more reactive and less wide
          h3("Let's Forecast Data Science Hits!"),
          align = "center",
          h3(""),
          "From the previous analysis, we know that Data Sicence
                           is strongly correlated with more recent lags, trend, and
                           maybe seaosnality. Since there is a strong linear positive trend in
                           the data, there is a couple of models I would like to try.",
          h3(""),
          div(
            selectInput(
              inputId = "selected_m",
              label = "Select a model", # User chooses stage of model
              choices = c(
                "Drift Model",
                "Drift Model Adjusted",
                "Time Series Linear Regression Model"
              )
            )
          ),
          h3(""),
          plotOutput(outputId = "choseplt_m"), # Display model
          h4(""),
          h3("Description"),
          h4(""),
          textOutput(outputId = "chosetxt_m"), # Show analysis
          h3("")
        )
      ),
      column(2)
    ),

    # New menu for choosing which analysis section you are on
    navbarMenu(
      "Understanding Economics",
      tabPanel(
        "Relationship/Predicting", # Tab 1 is relationships
        fluidRow(
          column(2),
          column(8,
            h3("Relationship Between Economics and Data Science"),
            align = "center",
            h3(""),
            div(
              selectInput(
                inputId = "selected_f",
                label = "Select a figure", # Look at variable relationships
                choices = c(
                  "Time Series Comparison",
                  "Economics vs Data Science",
                  "Prediction Model"
                )
              )
            ),
            h3(""),
            plotlyOutput(outputId = "chosefig"), # Plot analysis
            h3(""),
            div(align = "left", p(uiOutput(outputId = "chosetxt_fig"),
              style = "text-align:left; color:black"
            )), # Display analysis based on user
            h3("")
          ),
          column(2)
        )
      ),
      tabPanel(
        "Forecasting", # Tab 2 is forecasting
        fluidRow(
          column(2),
          column(8,
            h3("Let's Forecast Economics Hits!"), # Repeats similar formats
            align = "center",
            h3(""),
            "Similar to the model used for predicting Economics hits,
                                    the model used for forecasting Economics hits
                                    will depend solely on lags of the two variables. To do this,
                                    we are going to use the vector autoregressive technique. The
                                    issue with the previous models used in this app, is that
                                    they impose a one directional assumption of causality, where
                                    x only effects y and y does not effect x. However, this
                                    may not be true, especially since Data Science is merging
                                    and competing with the Economics industry. So, it is possible
                                    that the change of interest for these two phrases are dependent
                                    on each other, i.e. they are endogenous. Since the purpose
                                    of this model is to forecast Economics hits, and to not
                                    interpret the results, the vector autoregressive technique
                                    is a great option for us! However, before we forecast using the
                                    model, we need to ensure that our system is stable.",
            h3(""),
            div(
              selectInput(
                inputId = "selected_ec",
                label = "Select a type of analysis",
                choices = c(
                  "Stability",
                  "Granger Causality/ IRF plot",
                  "Variance Decomposition",
                  "VAR Forecast"
                )
              )
            ),
            h3(""),
            plotOutput(outputId = "chose_ec"),
            h3(""),
            h3("Analysis"),
            h3(""),
            textOutput(outputId = "chose_ectxt"),
            h3(""),
            h3("Summary"),
            "From this project, we were able to explore
                                    Google Trends data for Economics and Data Science
                                    hits. After evaluating and understanding our
                                    data and features at a deeper level, we then
                                    were able to develop a handful of models to
                                    forecast out Data Sicence hits, which we ultimately
                                    met our industry intuition with a sound
                                    statistical model. We were also able to explore
                                    the relationship between Data Science and Economcis,
                                    we were even able to develop a linear model
                                    that could accurately predict Economics
                                    hits. However, we thought we could do better
                                    and develop an actual forecasting model. So,
                                    by using vector autoregression (VAR) and diving into
                                    the relationship of Data Science and Economics, we
                                    were able to learn much more about our system.
                                    After understanding the univariate relationship,
                                    and what the influence of lags in our system,
                                    we used the VAR model to forecast out 18 months
                                    on the horizon. Furthemore, given what we
                                    expect to happen, and what the statistics show,
                                    we felt pleased about our final forecasting model.
                                    So, thank you for going on this journey with me.
                                    I hope you enjoyed this project and playing around
                                    in this app! Feel free to come back to see how
                                    the forecast and graphs change overtime as this
                                    will automatically updated with new Google Trends
                                    data. Also if you enjoyed this analysis, check out some of my
                                    other projects on my GitHub! Thank you
                                    for your time and I hope you have a nice day :)",
            h1("")
          ),
          column(2)
        )
      )
    )
  )
)


server <- function(input, output, session) {

  # Due to limited flexibility, had to write code like this to ensure it looked nice on the app
  shinyalert("Welcome", "Welcome to my Economics vs. Data Science Project! This Shinyapp takes you through each stage of my process, besides the data cleaning of course :) The first tab, Exploring the Data, let's you visualize the Google Trends data for Data Science hits and Econmic hits. The second tab, Forecast Data Science, let's you follow the rational and progression for each forecasting model. Finally, the third tab, Understanding Economics, allows you to delve into the relationship between Data Science and Economics, along with predicting and forecasting Economics hits using lagged values.

  Before you proceed, it is important to note that the hits number provided by Google represents the general interest of a phrase. However, it is not the raw number of total searchers, rather a sample of the total searches for that phrase. Also this analysis was written on 10/11/2022, so newer plots may have outdated analysis.")

  # Render Economics general time series
  output$econts <- renderPlotly({
    p1 <- ggplot(data = df, aes(y = Economics, x = Date)) +
      geom_line(color = "orange") +
      ggtitle("Economics Google Trend Hits") +
      ylab("Hits") +
      theme_classic()
    ggplotly(p1) %>%
      config(displayModeBar = FALSE) %>%
      layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
  })

  # Render corresponding text
  output$econtxt <- renderUI({
    tags$ul(
      tags$li("This plot displays the Google Trends data for the phrase Economics."),
      tags$li("From this initial plot of the data, it appears that people are searching for Economics less overtime."),
      tags$li("There are also some cylical patterns, which might indicate there is seasonality."),
      tags$li("Now let's look at some other plots to see if this hunch is correct!")
    )
  })

  # Look into configuration

  # Render plot for Economics based on user choice
  output$choseplt <- renderPlotly({
    if (input$selected == "Autocorrelation") {
      p1 <- autoplot(ACF(df, Economics)) +
        ggtitle("Economics ACF Plot") +
        ylab("ACF") +
        xlab("Lag (1 Month)") +
        theme_classic()

      ggplotly(p1) %>%
        config(displayModeBar = FALSE) %>%
        layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
    } else if (input$selected == "Seasonality") {
      p2 <- gg_season(df, Economics) +
        ggtitle("Economics Seasonality Plot") +
        ylab("Hits") +
        theme_classic()

      ggplotly(p2) %>%
        config(displayModeBar = FALSE) %>%
        layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
    } else if (input$selected == "Decomposition") {
      dcmp_ec <- df %>%
        model(classical_decomposition(Economics, type = "additive")) %>%
        components()

      p3 <- autoplot(dcmp_ec) +
        theme_bw()

      ggplotly(p3) %>%
        config(displayModeBar = FALSE) %>%
        layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
    }
  })

  # Render corresponding analysis based on user choice
  output$chosetxt <- renderUI({
    if (input$selected == "Autocorrelation") {
      tags$ul(
        tags$li("From the ACF plot we can see what lags (previous values) are highly correlated with the most recent value."),
        tags$li("In this case, the two lags that have the highest correlation with this month's total Economics hits are the previous
                      month's number of hits and the number of hits from a year ago."),
        tags$li("We can also see that there is a decreasing trend as the ACF bars are consistenlty getting smaller."),
        tags$li("Furthermore, we can see some cylcial patterns in the data as the bars form wave like patterns.")
      )
    } else if (input$selected == "Seasonality") {
      tags$ul(
        tags$li("This seasonality graph indicates that there has been a general decrease in Economics hits over time."),
        tags$li("The earliest years tend to be at the top of the graph, meaning that they had higher number of hits, and the more recent years
      tend to have lower number of hits."),
        tags$li("We can also see seasonal patterns in the Economics data. From January to May we see a
      relatively constant hit count, and in June we see a drop off. Lastly we see a spike in the number of
      hits from August to October, and after October this another decline in Economics hits."),
        tags$li("This seasonal component could be tied to macro-economics seasonality trends or when school is occuring."),
        tags$li("Economics is a broad term so pinpointing what exactly these cycles indicate is difficult without
      other external data or analysis.")
      )
    } else if (input$selected == "Decomposition") {
      tags$ul(
        tags$li("For the Classical Decomposition model, I chose an additive model instead of multiplicative."),
        tags$li("Since the cycles appeared to be somewhat consistent (no monotonic change in the variance of the cyles),
                      I did not see a need for a Box Cox transformation or multiplicative model."),
        tags$li("From the decomposition itself, we see a general decreasing trend, which is what we
      expected from looking at the original time series and other models."),
        tags$li("As this is a classical decomposition model, there is no change in seasonality over time."),
        tags$li("Lastly, the random residuals look similar to white noise and cover a fairly small range."),
        tags$li("This indicates that Economics hits can largely be accounted for by its trend and seasonality components.")
      )
    }
  })

  # Render plot for general time series for Data Science
  output$dsts <- renderPlotly({
    p1 <- ggplot(data = df, aes(y = Data_Science, x = Date)) +
      geom_line(color = "darkgreen") +
      ggtitle("Data Science Google Trend Hits") +
      ylab("Hits") +
      theme_classic()
    ggplotly(p1) %>%
      config(displayModeBar = FALSE) %>%
      layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
  })

  # Render corresponding text
  output$dstxt <- renderUI({
    tags$ul(
      tags$li("This plot displays the Google Trends data for the phrase Data Science."),
      tags$li("From this initial plot of the data, we can see Data Science searches are rapidly increasing overtime."),
      tags$li("Furthermore, since it is a younger field, it makes sense that it has less monthly hits than Economics."),
      tags$li("Let's dive into some other graphs to get a better understanding of the data!")
    )
  })

  # Render plot based on user choice for Data Science
  output$choseplt2 <- renderPlotly({
    if (input$selected2 == "Autocorrelation") {
      p1 <- autoplot(ACF(df, Data_Science)) +
        ggtitle("Data Science ACF Plot") +
        ylab("ACF") +
        xlab("Lag (1 Month)") +
        theme_classic()

      ggplotly(p1) %>%
        config(displayModeBar = FALSE) %>%
        layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
    } else if (input$selected2 == "Seasonality") {
      p2 <- gg_season(df, Data_Science) +
        ggtitle("Data Science Seasonality Plot") +
        ylab("Hits") +
        theme_classic()

      ggplotly(p2) %>%
        config(displayModeBar = FALSE) %>%
        layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
    } else if (input$selected2 == "Decomposition") {
      dcmp_ds <- df %>%
        model(classical_decomposition(Data_Science, type = "multiplicative")) %>%
        components()

      p3 <- autoplot(dcmp_ds) +
        theme_bw()

      ggplotly(p3) %>%
        config(displayModeBar = FALSE) %>%
        layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
    }
  })

  # Render corresponding analysis based on user's choice
  output$chosetxt2 <- renderUI({
    if (input$selected2 == "Autocorrelation") {
      tags$ul(
        tags$li("From the ACF plot we can see what lags (previous values) are highly correlated with the most recent value."),
        tags$li("Unlike the Economics ACF plot, there is no indication of seasonality occuring as the bars are not
      going through cyclical patterns"),
        tags$li("However, there is evidently a strong trend as the bars are contiously decreasing."),
        tags$li("We can then infer that months that are close to the current month, are strong indicators of what the current number of
      Data Science hits will be.")
      )
    } else if (input$selected2 == "Seasonality") {
      tags$ul(
        tags$li("This seasonality graph indicates that there has been an increase in interest of Data Science over time."),
        tags$li("The earliest years tend to be on the bottom with a lower number of Data Science hits, whereas the
                more recent years are gradually higher, meaning they have more hits."),
        tags$li("Also notice how the lines are almost completely flat, so there are little to no seasonal componenets for Data Science."),
        tags$li("Unlike the term Economics, Data Science almost exclusively refers to the field of Data Science.
                So I would have expected to see some change tied to schools being in session."),
        tags$li("It is also possible
                that there exists some small seaosnal component, but since the scale of Data Science is now being
                compared to the magintude of Economics, it is hard to parse out those trends.")
      )
    } else if (input$selected2 == "Decomposition") {
      tags$ul(
        tags$li("For the Classical Decomposition model, I chose an multiplicative model instead of additive."),
        tags$li("Since the cycles appeared to be somewhat increasing monotonically, and there is a quick increase in the Data Science trend, a multiplicative
      model makes more sense here"),
        tags$li(" As expected, there is a strong trend occuring for Data Science hits,as it has signifcantly increased over the last five years."),
        tags$li("There does appear to be some seasonality, but based on the scale and the small bar, it is negligible."),
        tags$li("Lastly, our residuals look like white noise as
      they do not have any noticable trends or cycles, however, the scale of the residuals is still fairly large."),
        tags$li("So, trend, more so than seasonality, makes up a decent chunk of what is occuring
      in our data, but there is still more going on in our data that we are not
      capturing from this decomposition.")
      )
    }
  })

  # Render model plots based on user choice
  output$choseplt_m <- renderPlot({
    if (input$selected_m == "Drift Model") {
      # Create dotted line values
      first_val <- head(df$Data_Science, 1)
      last_val <- tail(df$Data_Science, 1)
      line_vals <- seq(first_val, last_val, length.out = nrow(df))
      my_ts <- df
      my_ts$drift_vals <- line_vals

      # Plot forecast and dotted line
      my_ts %>%
        model(RW(Data_Science ~ drift())) %>%
        forecast() %>%
        autoplot(my_ts) +
        geom_line(data = my_ts, aes(Date, drift_vals), color = "blue", linetype = "dashed") +
        labs(title = "Forecast of Data Science using Drift Model", y = "Hits") +
        theme_classic()
    } else if (input$selected_m == "Drift Model Adjusted") {
      # Created dotted line values
      first_val2 <- head(tempds_df$Data_Science, 1)
      last_val2 <- tail(tempds_df$Data_Science, 1)
      line_vals2 <- seq(first_val2, last_val2, length.out = nrow(tempds_df))
      tempds_df$drift_vals <- line_vals2

      # Plot forecast and dotted line
      fitadj <- tempds_df %>%
        model(NAIVE(Data_Science ~ drift()))
      fitadj %>%
        forecast() %>%
        autoplot(df) +
        labs(
          title = "Forecast of Data Science using Drift Model (Adjusted)",
          y = "Hits"
        ) +
        geom_line(data = tempds_df, aes(Date, drift_vals), color = "blue", linetype = "dashed") +
        theme_classic()
    } else if (input$selected_m == "Time Series Linear Regression Model") {
      fit_ds <- tempds_df %>%
        model(TSLM(Data_Science ~ trend() + season()))
      fc_ds <- forecast(fit_ds)
      fc_ds %>%
        autoplot(df) +
        labs(
          title = "Forecast of Data Science using Regression Model",
          y = "Hits"
        ) +
        theme_classic()
    }
  })

  # Render corresponding analysis for the model that user chose
  output$chosetxt_m <- renderText({
    if (input$selected_m == "Drift Model") {
      "This was the first model I developed to forecast Data Science hits. I initially chose a Naive Drift model, as I wanted to use
      a model that would heavily emphasize a general linear trend. However, I underistamted
      the impact the first 10 years of data had in this model. As you can see 
      with the dotted line, it connects the intial value to the last value and that line
      become your forcast. The issue with this, is that this model is likely going to undervalue
      the future hits. Data Science is a new field. In the data we saw this
      as it had constant hits flexuating around 1 and 2 up until January 2015. 
      After January 2015, we can see a quick explosion in Data Science popularity. 
      So, I found way to adjust this initial model to better represent the current increasing trend."
      
    } else if (input$selected_m == "Drift Model Adjusted") {
      "This model is similar to the first Naive Drift model, except I account
      for the large flatline in the beginning of the data. Instead of creating a
      forecast using all of the data, I decided to only select January 2015 and
      onwards. This way, the initial value is when the new increase in hits started to occur.
      Since the Data Science field has a positive outlook and concepts like machine learning
      and aritical intelligence are becoming industry standards, I think it is fair
      to assume there will be a steep increasing slope in public interest for data science.
      Thus, with this hypothesis in mind, I feel like this is a much better model.
      However, given the rate that interest for Data Science is growing in the data,
      I believe this model might be overestimating how fast it will grow. Thus,
      I still believe I could develop a better model."
    } else if (input$selected_m == "Time Series Linear Regression Model") {
      "Since I wanted to emphasize trend, but still thought the previous model
      was over predicting what future growth would look like, I read into time
      series linear regression models. Instead of using lags to forecast, I decided
      that using its seasonality and trend to forecast future values should be
      somewhat accurate. Similar to the last model, I trained the regression using only
      Google Trend data after January 2015. So, looking at this graph, I would argue this prediction is
      more accurate than the previous two models. Instead of the forecast being
      linear we are accounting for some cylical patterns using seasonality, and we
      are still emphasizing the general trend in the data. Also, this model
      does not overestimate the amount of growth Data Science searches will have.
      Thus, I feel like this is a more accurate model."
    }
  })

  # Render plot to understand relationship based on user choice
  output$chosefig <- renderPlotly({
    if (input$selected_f == "Time Series Comparison") {
      # Create long dataframe
      tempdf <- melt(df, id.vars = "Date", variable.name = "series")

      # Plot Data Science and Economics together
      group.colors <- c(Data_Science = "darkgreen", Economics = "orange")

      p1 <- ggplot(tempdf, aes(Date, value)) +
        geom_line(aes(colour = series), size = .8) +
        theme_classic() +
        scale_color_manual(values = group.colors) +
        labs(y = "Hits", x = "Date", title = "Economics and Data Science Hits Over Time")
      ggplotly(p1) %>%
        config(displayModeBar = FALSE) %>%
        layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
    } else if (input$selected_f == "Economics vs Data Science") {
      p2 <- ggplot(df, aes(x = Data_Science, y = Economics)) +
        geom_point(color = "darkred") +
        labs(
          y = "Economics", x = "Data Science",
          title = "Data Science vs Economics Hits"
        ) +
        theme_classic()
      ggplotly(p2) %>%
        config(displayModeBar = FALSE) %>%
        layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
    } else if (input$selected_f == "Prediction Model") {
      # Create a time regression Economics ~ Data Science + lags
      fit_econ <- df %>%
        model(TSLM(Economics ~ lag(Data_Science) + lag(Economics) + lag(Economics, 12)))

      # Plot model compare predictions to actual
      p3 <- augment(fit_econ) %>%
        ggplot(aes(x = Economics, y = .fitted)) +
        geom_point(color = "orange") +
        labs(
          y = "Predicted values", x = "Actual values",
          title = "Economics Hits Predictions vs. Actuals"
        ) +
        geom_abline(intercept = 0, slope = 1) +
        theme_classic()

      ggplotly(p3) %>%
        config(displayModeBar = FALSE) %>%
        layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
    }
  })

  # Render corresponidng text based on user choice
  output$chosetxt_fig <- renderUI({
    if (input$selected_f == "Time Series Comparison") {
      tags$ul(
        tags$li("This plot compares the Google Trend hits of Economics and Data Science
      across the same time period. Although each line was plotted separately on the first
      tab, I thought it would be helpful to see them plotted together. "),
        tags$li("As you can see, Economics always has more searches and generally more interests from the public.
      This could be true for a number of reasons: the breadth of the term economics,
      people's interest in the economy during a recession/ down turn, and/or the
      amount of people who go into economics in higher education."),
        tags$li("However, despite a relatively high number of hits, the phrase Economics has decreased over time.
      Data Science on the other hand has started to increase over the past 5
      years."),
        tags$li("Data Science is a new field and the term is typically only used to describe
      the Data Science industry (not as broad as the term Economics). Despite this
      growth it has yet to catch up to Economcis level of Google Trend interest."),
        tags$li("Furthermore, Data Science and Economics have been competitive fields to a degree,
      as many PhD Economic programs are learning to applied econometrics, machine learning,
      or Data Science related topics."),
        tags$li("Since these two industries are competing and merging,
      I thought it would be interesting to directly compare the two time series plots.")
      )
    } else if (input$selected_f == "Economics vs Data Science") {
      tags$ul(
        tags$li("Plotting two discrete variables with a wide spread over time was a challenge.
      To best capture the data, I decided to use a scatter plot so you could see where
      each indiviudal point lied on the graph."),
        tags$li("As expected, there are a lot of high
      Economics hits and low Data Science hits. When Data Science starts to grow
      (has a hit value greater than 3), we can see
      that Economics stays somewhat constant."),
        tags$li("Interestingly, when Data Science has its highest values, we see a cluster of higher Economics searches.
      The sudden increase in Economics hits likely reflects people's negative
      outlook on the economy rather than an increase of interest in the
      Economics industry."),
        tags$li("So, from the time series graphs and this plot, there is perhaps a
      relationship between Data Science and Economics, but it is difficult to tell."),
        tags$li("Let's look at the model we developed to see if we can predict Economics hits based
      on Data Science hits and its own lags.")
      )
    } else if (input$selected_f == "Prediction Model") {
      tags$ul(
        tags$li("The purpose of this time series linear regression model was to predict
      Economics hits using only its lags and Data Science hits."),
        tags$li("Thus, the dependent
      variable in the equation was Economics hits, and the explanatory varaibles were:
      a lag 1 of Data Science hits, lag 1 of Economics hits, and lag 12 of Economics hits.
      I chose these lags based on the most significant bars on each ACF plot, and my own intuition."),
        tags$li("From the summary statistics, all three explanatory variables were statistically
      significant, and interestingly, Data Science had a positive coefficient."),
        tags$li("More specefically, for each hit in the previous month's total Data Science hits,
      we would expect to see a .228 increase in the current month's Economics hits.
      The total adjusted R squared for the model was .81, meaning that 81%
      of the variation in Economics hits can be explained by this model."),
        tags$li("From the plot, which compares the actual Economics hits versus the predicted Economics hits,
      it appears that our model did a good job overall. The points are clustered
      somewhat tightly around the linear line of 1 (a perfect prediction), and
      there are no extreme outliers."),
        tags$li("There is maybe some slight heteroskedasticity in the residuals, but
      in general I think this model holds up well.")
      )
    }
  })

  # Render plot based on stage of analysis and user choice
  output$chose_ec <- renderPlot({
    if (input$selected_ec == "Stability") {

      # Check for Stability
      stability_1 <- stability(varmodel, type = "OLS-CUSUM")
      plot(stability_1, lwd = 2.0)
    } else if (input$selected_ec == "Granger Causality/ IRF plot") {
      Economics_irf <- irf(varmodel,
        impulse = "Data_Science", response = "Economics",
        n.ahead = 40, boot = TRUE
      )
      plot(Economics_irf, ylab = "Economics", main = "Shock From Data Science", lwd = 2.0)
    } else if (input$selected_ec == "Variance Decomposition") {
      FEVD1 <- fevd(varmodel, n.ahead = 15)
      plot(FEVD1)
    } else if (input$selected_ec == "VAR Forecast") {
      # VAR Forecast
      fc_econ <- predict(varmodel, n.ahead = 18, ci = 0.95)
      plot(fc_econ,
        names = "Economics", xlab = "Months from Start (January 2004)", ylab = "Hits",
        main = "Forecast of Economics Hits using Vector Autoregression Model", lwd = 2.0, cex = 3
      )
    }
  })

  # Render text based on user choice for stage of analysis
  output$chose_ectxt <- renderText({
    if (input$selected_ec == "Stability") {
      "The vector autoregression model that was developed has an
      optimal lag number of 19. However, before we start using this model for more analysis
      of our data, we have to check one of its core assumptions: is this system stable?
      In the plot of stability above, the red lines above and below
      the x-axis represents our confidence interval. Furthemore, if our black variable line
      crosses the red lines, we claim that our system is not stable. Lucky for us,
      both of our varibles used in this model, Data Science and Economics, are always
      in between the two red lines. Therefore, our system is stable, and we may proceed."
    } else if (input$selected_ec == "Granger Causality/ IRF plot") {
      "Before discussing the Impulse Response Function graph, I wanted to touch on the
      Granger Causality test that was conducted on this model. The Granger Causality
      test provides more insight into the causality of our variables. Instead of assuming
      there is a univariate directional relationship between our variables, like linear
      regression, it assumes that there are variables can be multivariate, where they effect
      each other. From the Granger Causality test, we saw similar results to what we
      concluded in the previous tabs of this project. Data Science does Granger cause
      Economics hits as the p-value for Data Science was 4.946e-05, which is less than
      the standard alpha of .05, and we can reject the null hypothesis that the lagged
      values of Data Science do not explain the variation in Economics. However, Economics
      does not Granger cuase Data Science hits as the p-vaue was .7051, which is more
      than .05, and we fail to reject the null hypothesis that the lagged values of
      Economics do not explain the variation in Data Science. So from the Granger
      Causality test, we can see that there is a univariate relationship where Data
      Science hits can effect Economics hits, which we also saw in other sections.

      Now let's move onto the Impulse Response Function (IMF). IMF looks at shocks of one variable,
      in this case Data Science hits, and evaluates what would happen to another variable,
      Economics hits. Since we established in the Granger Causality section that
      Data Science hits does Granger casues Economics hits, it makes sense to test
      Data Science hits shocks on Economics hits. From the graph, we generally
      see a positive relationsihp, between Data Science shocks and Economics,
      but it is still possible to have an indifferent or negative relationship as
      0 and negative values exist within the confidence interval (red dotted lines),
      Furthemore, note that this particular IRF plot is evaluated at the 95%
      Confidence Interval, forecasted over 40 months, and over 100 trials."
    } else if (input$selected_ec == "Variance Decomposition") {
      "Variance Decomposition evaluates how much the varaibles in our system,
      Data Science and Economics, are influenced by shocks. So when forecasting
      out 15 months on the horizon, we see that Economics is mainly influenced
      by its own shocks and Data Science shocks has virtually no effect on it.
      Economics is also mainly effected by its own shocks, however, it is also heavily
      impacted by Data Science shocks as the time horizon progresses.
      Again, these results makes sense as they coincide well with the Granger
      Causality test and IRF simulaiton conclusions."
    } else if (input$selected_ec == "VAR Forecast") {
      "This plot depicts a vector autoregression (VAR) model based on Data Sicence and
      Economics lags that forecasts a year and a half ahead (18 months) from where
      the data is currently at. The light grey line indicates when the forecast
      starts. The dotted blue lines represents the actual prediction of where
      we would expect Economics hits to be on that given month. The dotted
      red lines represents the confidence interval, which in this case is at the 95%
      level. From this forecast, we would expect Economics hits to face a downturn in
      the near future, and would continue its clycical pattern of increasing and
      decreasing without really change on average (no signifcant trend). Given
      historical data, and knowing that Economics will likely not grow in searches
      (assuming no unexpected shocks in the Economics hits occur), this VAR mode follows
      my intuition! If unexpected shocks do occur, then this model will be updated
      automatically as it is collecting data directly from the Google Trends API."
    }
  })
}

# Produce the app
shinyApp(ui = ui, server = server)
