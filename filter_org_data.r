library(shiny)
library(shinysurveys)
library(dplyr)
library(stringr)
library(shinyBS)
library(shinyjs)
library(httr)




  


library(shiny)
library(shinysurveys)
library(dplyr)
library(stringr)
library(shinyBS)
library(shinyjs)

response <- GET("https://raw.githubusercontent.com/anccaz/EnchanTobor/main/stu_org.csv")

text <- content(response, "text")

stu_org <- read.csv(text = text)

stu_org <- stu_org %>% 
  select(-Contact.Information.Email, -Picture) %>%
  rename(President = Purpose, Email = and.Organization.Description, Image = President.s.Full.Name)
stu_org$CategoryClean <- str_extract(stu_org$Category, "\\w+")
stu_org[334, 7] <- "Fraternity"

stu_org <- stu_org %>% 
  mutate(CategoryClean = ifelse(CategoryClean == "Club", "Sports", CategoryClean)) %>%
  mutate(CategoryClean = ifelse(CategoryClean == "Service", "Services", CategoryClean)) %>%
  mutate(CategoryClean = ifelse(CategoryClean == "Arts", "Arts and Music", CategoryClean)) %>%
  mutate(CategoryClean = ifelse(CategoryClean == "Art", "Arts and Music", CategoryClean)) %>%
  mutate(CategoryClean = ifelse(CategoryClean == "Departmental", "Departmental/Educational", CategoryClean)) %>%
  mutate(CategoryClean = ifelse(CategoryClean == "Educational", "Departmental/Educational", CategoryClean)) %>%
  mutate(CategoryClean = ifelse(CategoryClean == "University", "University Department", CategoryClean)) %>%
  mutate(CategoryClean = ifelse(CategoryClean == "Fraternity", "Fraternity & Sorority", CategoryClean)) %>%
  mutate(CategoryClean = ifelse(CategoryClean == "Honor", "Honor Society", CategoryClean)) %>%
  mutate(CategoryClean = ifelse(CategoryClean == "Special", "Special Interest", CategoryClean))

stu_org[302, 7] <- "Cultural"
stu_org[280, 7] <- "Arts and Music"
stu_org[318, 7] <- "University Department"
stu_org[382, 7] <- "University Department"
stu_org[348, 7] <- "University Department"

stu_org <- stu_org %>%
  distinct(Title, .keep_all = TRUE)


user_input <- data.frame(question = "Select organization category:",
                         option = c("Academic", "Arts and Music", "Departmental/Educational", "Services", "Sports", "Cultural", "Social", "Religious", "Honor Society", "Special Interest", "University Department", "Fraternity & Sorority", "Political", "Recreation"), 
                         input_type = "select",
                         input_id = "org_category",
                         dependence = NA,
                         dependence_value = NA,
                         required = F)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
                    body {
                      background-color: f7ed40cc;
                    }
                    "))
  ),
  surveyOutput(df = user_input,
               survey_title = "EnchanTobor's Student Organization Survey",
               survey_description = "Take this simple survey to be paired with a couple of UTD's 400+ registered student organizations on campus!"),
)

server <- function(input, output, session) {
  filtered_org <- reactive({
    stu_org %>% 
      filter(CategoryClean == input$org_category) %>%
      select(Title, Mission) %>%
      slice_sample(n = 5)
  })
  
  observeEvent(input$submit, {
    showModal(modalDialog(
      title = "EnchanTobor recommends these clubs!",
      renderUI({
        navbarPage(
          "Organizations",
          tabPanel("Page 1", tableOutput("org_cat_1")),
          tabPanel("Page 2", tableOutput("org_cat_2")),
          tabPanel("Page 3", tableOutput("org_cat_3")),
          tabPanel("Page 4", tableOutput("org_cat_4")),
          tabPanel("Page 5", tableOutput("org_cat_5"))
        )
      })
    ))
  })
  
  observe({
    if (input$submit > 0) {
      output$org_cat_1 <- renderTable({
        filtered_org() %>% 
          slice(1) %>%
          mutate(Mission = str_wrap(Mission, width = 50)) %>%
          summarise(TitleMission = paste(Title, ": \n", Mission)) %>%
          pull(TitleMission) %>%
          `colnames<-`(NULL)
      }, header = FALSE)
      output$org_cat_2 <- renderTable({
        filtered_org() %>% 
          slice(2) %>%
          mutate(Mission = str_wrap(Mission, width = 50)) %>%
          summarise(TitleMission = paste(Title, ": ", Mission)) %>%
          pull(TitleMission)
      })
      output$org_cat_3 <- renderTable({
        filtered_org() %>% 
          slice(3) %>%
          mutate(Mission = str_wrap(Mission, width = 50)) %>%
          summarise(TitleMission = paste(Title, ": ", Mission)) %>%
          pull(TitleMission)
      })
      output$org_cat_4 <- renderTable({
        filtered_org() %>% 
          slice(4) %>%
          mutate(Mission = str_wrap(Mission, width = 50)) %>%
          summarise(TitleMission = paste(Title, ": ", Mission)) %>%
          pull(TitleMission)
      })
      output$org_cat_5 <- renderTable({
        filtered_org() %>% 
          slice(5) %>%
          mutate(Mission = str_wrap(Mission, width = 50)) %>%
          summarise(TitleMission = paste(Title, ": ", Mission)) %>%
          pull(TitleMission)
      })
    }
  })
}

shinyApp(ui, server)