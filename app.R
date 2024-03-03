



library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
library(scales)
library(ggrepel)
library(googlesheets4)
library(googledrive)
library(shinydashboard)



options(rsconnect.timeout = 300)
options(gargle_oauth_cache = ".secrets/client_secret.json")

drive_auth(path = ".secrets/client_secret.json")
gs4_auth(path = ".secrets/client_secret.json")
mainsub <- read_sheet(sheet = 2, "https://docs.google.com/spreadsheets/d/1mj_xHOWpUyKLIT_fJf2-iXRSYcWP7BfUqn77YooPlic/edit#gid=1536770875")





bsize <- 16
sz <- 4.4
plotmain <- mainsub %>% 
  group_by(aoscategory) %>% 
  count() %>% 
  ungroup() %>% 
  mutate_at('aoscategory', ~replace_na(.,"No Answer")) %>%
  mutate(perc = n / sum(n)) %>% 
  arrange(perc) %>%
  mutate(ypos = cumsum(perc)- 0.5*perc,
         aoscategory = factor(aoscategory, levels=aoscategory[order(-(perc))], ordered=TRUE))


plotmain$perc <- as.numeric(plotmain$perc)

plotmain <- plotmain %>%
  mutate(labels = percent(perc),
         labels = factor(labels, levels=labels[order(-(perc))], ordered=TRUE))

plotmain$labels <- as.character(plotmain$labels)

plotmain2 <- mainsub %>% 
  mutate_at('aoscategory', ~replace_na(.,"No Answer"))

mainsub <- mainsub %>%
  mutate(
    Philosopherover20 = case_when(
      Philosopher== "Anscombe" ~ "Anscombe",
      Philosopher== "Aquinas" ~ "Aquinas",
      Philosopher== "Aristotle" ~ "Aristotle",
      Philosopher== "Carnap" ~ "Carnap",
      Philosopher== "Davidson" ~ "Davidson",
      Philosopher== "Descartes" ~ "Descartes",
      Philosopher== "Dewey" ~ "Dewey",
      Philosopher== "Frege" ~ "Frege",
      Philosopher== "Hegel" ~ "Hegel",
      Philosopher== "Hume" ~ "Hume",
      Philosopher== "Kant" ~ "Kant",
      Philosopher== "No Answer" ~ "No Answer",
      Philosopher== "Parfit" ~ "Parfit",
      Philosopher== "Rawls" ~ "Rawls",
      TRUE ~ "Other"
    )
  )

log_where <- function(req){
  cli::cat_rule(
    sprintf(
      "%s - %s", 
      Sys.time(), 
      req$PATH_INFO
    )
  )
  req
}






tabsdemo<- tabsetPanel(
  tabPanel("Age and Gender",  plotOutput("age_gender")),
  tabPanel("Nationality", plotOutput("thingnat")),
  tabPanel("Category of Specialization",plotOutput("aoscpie")),
  tabPanel("Philosopher Alignment", plotOutput("phil"))
)
tabsabout<- tabsetPanel(
  tabPanel("Philosophers on Philosophy Paper","Info About the Paper"),
  tabPanel("Making an App","Info About us and making the paper")
)



tabsage<- tabsetPanel(
  tabPanel("Table",   tableOutput(outputId = "distPlotage")),
  tabPanel("Bar Graph", plotOutput("barage")),
  tabPanel("Pie Chart",plotOutput("pieage"))
)


tabstradition<- tabsetPanel(
  tabPanel("Table",   tableOutput(outputId = "distPlottrad")),
  tabPanel("Bar Graph", plotOutput("bartrad")),
  tabPanel("Pie Chart",plotOutput("pietrad"))
)


tabsaos<- tabsetPanel(
  tabPanel("Table",   tableOutput(outputId = "distPlotaos")),
  tabPanel("Bar Graph", plotOutput("baraos")),
  tabPanel("Pie Chart",plotOutput("pieaos"))
)


tabsphilosopher<- tabsetPanel(
  tabPanel("Table",   tableOutput(outputId = "distPlotphil")),
  tabPanel("Bar Graph", plotOutput("barphil")),
  tabPanel("Pie Chart",plotOutput("piephil"))
)


tabsnationality<- tabsetPanel(
  tabPanel("Table",   tableOutput(outputId = "distPlotnat")),
  tabPanel("Bar Graph", plotOutput("barnat")),
  tabPanel("Pie Chart",plotOutput("pienat"))
)




ui <- dashboardPage(
  dashboardHeader(title = "Philosopher Beliefs"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Demographics", tabName = "demo"),
      menuItem("About Page", tabName = "about"),
      menuItem("Age", tabName = "age"),
      menuItem("Tradition", tabName = "trad"),
      menuItem("Area of Specialization", tabName = "aos"),
      menuItem("Philosopher Alignment", tabName = "phil"),
      menuItem("Nationality", tabName = "nat")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "demo",
        fluidRow(
          h1("Demographics Information"), 
          tabsdemo
        ),
        
        
        
      ),
      tabItem(
        tabName = "about",
        fluidRow(
          
          h1("About"),
          mainPanel(
            "    A PhilPapers survey conducted by David Bourget and David Chalmers asked over 1700 philosophers around the world to provide their thoughts on a variety of philosophical problems. All participants published papers in English. The participants were asked to answer at least 50 questions, but many of them answered all 100 questions. This survey follows up on a survey conducted in 2009 that asked over 900 philosophers about their opinions on 30 questions, resulting in the published paper 'What Do Philosophers Believe' (2014). In early 2023, Dr. Aaron Nielsen and Josh Brekel reached out to Bourget and Chalmers to obtain the raw survey information in an effort to create an interactive web application. In May 2023, the task of developing the app was assigned to undergraduate Jacob Shankles, who programmed the app with advisory help from Josh Brekel and Dr. Aaron Nielsen, which was then continued through the following semester."
            
          )
        )
      ),
      tabItem(
        tabName = "age",
        fluidRow(
          
          tagList(
            h1("  Sort by Age of Philosopher"),
            sidebarLayout(
              # Inputs: Select variables to plot
              sidebarPanel(
                selectInput(
                  inputId = "yage",
                  label = "Question",
                  choices = gsub('[_]',' ', c("Abortion", "A_Priori", "Abstract_Objects", "Normativity_or_Metaphysically", "Aesthetic_Value", "Aim_of_Philosophy", "Analytic_synthetic_Distinction", "Eating_Animals_Animal_Products", "Epistemic_Justification", "Experience_Machine", "External_World", "Footbridge", "Free_Will", "Gender_Q", "God", "Knowledge_Claims", "Knowledge", "Laws_of_Nature", "Logic", "Meaning_of_Life", "Mental_Content", "Meta_Ethics", "Metaphilosophy", "Mind", "Moral_Judgement", "Moral_Motivation", "Newcombs_Problem", "Normative_Ethics", "Perceptual_Experience", "Personal_Identity", "Philosophical_Progress", "Political_Philosophy", "Proper_Names", "Race", "Science", "Teletransporter", "Time", "Trolley_Problem", "Truth", "Vagueness", "Zombies", "Aesthetic_Experience", "Analysis_of_Knowledge", "Arguments_for_Theism", "Belief_or_Credence", "Capital_Punishment", "Chinese_Room", "Concepts", "Consciousness", "Continuum_Hypothesis", "Cosmological_fine_tuning", "Environmental_Ethics", "Extended_Mind", "Foundations_of_Mathematics", "Gender_Categories", "Grounds_of_Intentionality", "Hard_Problem_of_Consciousness", "Human_Genetic_Engineering", "Hume", "Immortality", "Interlevel_Metaphysics", "Justification", "Kant", "Law", "Material_Composition", "Metaontology", "Method_in_History_of_Philosophy", "Method_in_Political_Philosophy", "Mind_Uploading", "Moral_Principles", "Morality", "Normative_Concepts", "Ought_Implies_Can", "Philosophical_Knowledge", "Plato", "Politics", "Possible_Worlds", "Practical_Reason", "Principle_of_Sufficient_Reason", "Properties", "Propositional_Attitudes", "Propositions", "Quantum_Mechanics", "Race_Categories", "Rational_Disagreement", "Response_to_External_World_Skepticism", "Semantic_Content", "Sleeping_Beauty", "Spacetime", "Statue_and_Lump", "Temporal_Ontology", "Theory_of_Reference", "Time_Travel", "True_Contradictions", "Units_of_Selection", "Values_in_Science", "Well_Being", "Wittgenstein")),
                  selected = "Abortion"
                ),
                selectInput(
                  inputId = "xage",
                  label = "Age",
                  choices = c("Under 40", "40 to 60", "Older Than 60", "All Ages"),
                  selected = "All Ages"
                )
              ),
              # Outputs: Display the table
              mainPanel(
                tabsage,
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "trad",
        fluidRow(
          tagList(
            h1("Sort by Philosopher Tradition"),
            sidebarLayout(
              # Inputs: Select variables to plot
              sidebarPanel(
                selectInput(
                  inputId = "ytrad",
                  label = "Question",
                  choices = gsub('[_]',' ', c("Abortion", "A_Priori", "Abstract_Objects", "Normativity_or_Metaphysically", "Aesthetic_Value", "Aim_of_Philosophy", "Analytic_synthetic_Distinction", "Eating_Animals_Animal_Products", "Epistemic_Justification", "Experience_Machine", "External_World", "Footbridge", "Free_Will", "Gender_Q", "God", "Knowledge_Claims", "Knowledge", "Laws_of_Nature", "Logic", "Meaning_of_Life", "Mental_Content", "Meta_Ethics", "Metaphilosophy", "Mind", "Moral_Judgement", "Moral_Motivation", "Newcombs_Problem", "Normative_Ethics", "Perceptual_Experience", "Personal_Identity", "Philosophical_Progress", "Political_Philosophy", "Proper_Names", "Race", "Science", "Teletransporter", "Time", "Trolley_Problem", "Truth", "Vagueness", "Zombies", "Aesthetic_Experience", "Analysis_of_Knowledge", "Arguments_for_Theism", "Belief_or_Credence", "Capital_Punishment", "Chinese_Room", "Concepts", "Consciousness", "Continuum_Hypothesis", "Cosmological_fine_tuning", "Environmental_Ethics", "Extended_Mind", "Foundations_of_Mathematics", "Gender_Categories", "Grounds_of_Intentionality", "Hard_Problem_of_Consciousness", "Human_Genetic_Engineering", "Hume", "Immortality", "Interlevel_Metaphysics", "Justification", "Kant", "Law", "Material_Composition", "Metaontology", "Method_in_History_of_Philosophy", "Method_in_Political_Philosophy", "Mind_Uploading", "Moral_Principles", "Morality", "Normative_Concepts", "Ought_Implies_Can", "Philosophical_Knowledge", "Plato", "Politics", "Possible_Worlds", "Practical_Reason", "Principle_of_Sufficient_Reason", "Properties", "Propositional_Attitudes", "Propositions", "Quantum_Mechanics", "Race_Categories", "Rational_Disagreement", "Response_to_External_World_Skepticism", "Semantic_Content", "Sleeping_Beauty", "Spacetime", "Statue_and_Lump", "Temporal_Ontology", "Theory_of_Reference", "Time_Travel", "True_Contradictions", "Units_of_Selection", "Values_in_Science", "Well_Being", "Wittgenstein")),
                  selected = "Abortion"
                ),
                selectInput(
                  inputId = "xtrad",
                  label = "Tradition",
                  choices = c("All Traditions","Continental", "Analytic", "Other Traditions" ),
                  selected = "All Traditions"
                )
              ),
              # Outputs: Display the table
              mainPanel(
                tabstradition
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "aos",
        fluidRow(
          tagList(
            h1("Sort by Philosopher Area of Specialization"),
            sidebarLayout(
              # Inputs: Select variables to plot
              sidebarPanel(
                selectInput(
                  inputId = "yaos",
                  label = "Question",
                  choices = gsub('[_]',' ', c("Abortion", "A_Priori", "Abstract_Objects", "Normativity_or_Metaphysically", "Aesthetic_Value", "Aim_of_Philosophy", "Analytic_synthetic_Distinction", "Eating_Animals_Animal_Products", "Epistemic_Justification", "Experience_Machine", "External_World", "Footbridge", "Free_Will", "Gender_Q", "God", "Knowledge_Claims", "Knowledge", "Laws_of_Nature", "Logic", "Meaning_of_Life", "Mental_Content", "Meta_Ethics", "Metaphilosophy", "Mind", "Moral_Judgement", "Moral_Motivation", "Newcombs_Problem", "Normative_Ethics", "Perceptual_Experience", "Personal_Identity", "Philosophical_Progress", "Political_Philosophy", "Proper_Names", "Race", "Science", "Teletransporter", "Time", "Trolley_Problem", "Truth", "Vagueness", "Zombies", "Aesthetic_Experience", "Analysis_of_Knowledge", "Arguments_for_Theism", "Belief_or_Credence", "Capital_Punishment", "Chinese_Room", "Concepts", "Consciousness", "Continuum_Hypothesis", "Cosmological_fine_tuning", "Environmental_Ethics", "Extended_Mind", "Foundations_of_Mathematics", "Gender_Categories", "Grounds_of_Intentionality", "Hard_Problem_of_Consciousness", "Human_Genetic_Engineering", "Hume", "Immortality", "Interlevel_Metaphysics", "Justification", "Kant", "Law", "Material_Composition", "Metaontology", "Method_in_History_of_Philosophy", "Method_in_Political_Philosophy", "Mind_Uploading", "Moral_Principles", "Morality", "Normative_Concepts", "Ought_Implies_Can", "Philosophical_Knowledge", "Plato", "Politics", "Possible_Worlds", "Practical_Reason", "Principle_of_Sufficient_Reason", "Properties", "Propositional_Attitudes", "Propositions", "Quantum_Mechanics", "Race_Categories", "Rational_Disagreement", "Response_to_External_World_Skepticism", "Semantic_Content", "Sleeping_Beauty", "Spacetime", "Statue_and_Lump", "Temporal_Ontology", "Theory_of_Reference", "Time_Travel", "True_Contradictions", "Units_of_Selection", "Values_in_Science", "Well_Being", "Wittgenstein")),
                  selected = "Abortion"
                ),
                selectInput(
                  inputId = "xaos",
                  label = "Category of Specialization",
                  choices = c("Metaphysics and Epistemology", "Science, Logic, and Mathematics", "Value Theory", "History of Western Philosophy", "Philosophical Traditions", "All Areas" ),
                  selected = "All Areas"
                )
              ),
              # Outputs: Display the table
              mainPanel(
                tabsaos
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "phil",
        fluidRow(
          tagList(
            h1("Sort by Philosopher Alignment"),
            sidebarLayout(
              # Inputs: Select variables to plot
              sidebarPanel(
                selectInput(
                  inputId = "yphil",
                  label = "Question",
                  choices = gsub('[_]',' ', c("Abortion", "A_Priori", "Abstract_Objects", "Normativity_or_Metaphysically", "Aesthetic_Value", "Aim_of_Philosophy", "Analytic_synthetic_Distinction", "Eating_Animals_Animal_Products", "Epistemic_Justification", "Experience_Machine", "External_World", "Footbridge", "Free_Will", "Gender_Q", "God", "Knowledge_Claims", "Knowledge", "Laws_of_Nature", "Logic", "Meaning_of_Life", "Mental_Content", "Meta_Ethics", "Metaphilosophy", "Mind", "Moral_Judgement", "Moral_Motivation", "Newcombs_Problem", "Normative_Ethics", "Perceptual_Experience", "Personal_Identity", "Philosophical_Progress", "Political_Philosophy", "Proper_Names", "Race", "Science", "Teletransporter", "Time", "Trolley_Problem", "Truth", "Vagueness", "Zombies", "Aesthetic_Experience", "Analysis_of_Knowledge", "Arguments_for_Theism", "Belief_or_Credence", "Capital_Punishment", "Chinese_Room", "Concepts", "Consciousness", "Continuum_Hypothesis", "Cosmological_fine_tuning", "Environmental_Ethics", "Extended_Mind", "Foundations_of_Mathematics", "Gender_Categories", "Grounds_of_Intentionality", "Hard_Problem_of_Consciousness", "Human_Genetic_Engineering", "Hume", "Immortality", "Interlevel_Metaphysics", "Justification", "Kant", "Law", "Material_Composition", "Metaontology", "Method_in_History_of_Philosophy", "Method_in_Political_Philosophy", "Mind_Uploading", "Moral_Principles", "Morality", "Normative_Concepts", "Ought_Implies_Can", "Philosophical_Knowledge", "Plato", "Politics", "Possible_Worlds", "Practical_Reason", "Principle_of_Sufficient_Reason", "Properties", "Propositional_Attitudes", "Propositions", "Quantum_Mechanics", "Race_Categories", "Rational_Disagreement", "Response_to_External_World_Skepticism", "Semantic_Content", "Sleeping_Beauty", "Spacetime", "Statue_and_Lump", "Temporal_Ontology", "Theory_of_Reference", "Time_Travel", "True_Contradictions", "Units_of_Selection", "Values_in_Science", "Well_Being", "Wittgenstein")),
                  selected = "Abortion"
                ),
                selectInput(
                  inputId = "xphil",
                  label = "Philosopher",
                  choices = c("Anscombe", "Aquinas", "Arendt", "Aristotle", "Augustine", "Austin", "Berkeley", "Carnap", "Davidson", "De Beauvoir", "Descartes", "Dewey", "Foucault", "Frege", "Hegel", "Heidegger", "Hobbes", "Hume", "Husserl", "James", "Kant", "Leibniz", "Lewis", "Locke", "No Answer", "Other", "Parfit", "Plato", "Quine", "Rawls", "Russell", "Socrates", "Wittgenstein", "All Philosophers"),
                  selected = "All Philosophers"
                )
              ),
              # Outputs: Display the table
              mainPanel(
                tabsphilosopher
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "nat",
        fluidRow(
          
          tagList(
            h1("  Sort by Philosopher Nationality"),
            sidebarLayout(
              # Inputs: Select variables to plot
              sidebarPanel(
                selectInput(
                  inputId = "ynat",
                  label = "Question",
                  choices = gsub('[_]',' ', c("Abortion", "A_Priori", "Abstract_Objects", "Normativity_or_Metaphysically", "Aesthetic_Value", "Aim_of_Philosophy", "Analytic_synthetic_Distinction", "Eating_Animals_Animal_Products", "Epistemic_Justification", "Experience_Machine", "External_World", "Footbridge", "Free_Will", "Gender_Q", "God", "Knowledge_Claims", "Knowledge", "Laws_of_Nature", "Logic", "Meaning_of_Life", "Mental_Content", "Meta_Ethics", "Metaphilosophy", "Mind", "Moral_Judgement", "Moral_Motivation", "Newcombs_Problem", "Normative_Ethics", "Perceptual_Experience", "Personal_Identity", "Philosophical_Progress", "Political_Philosophy", "Proper_Names", "Race", "Science", "Teletransporter", "Time", "Trolley_Problem", "Truth", "Vagueness", "Zombies", "Aesthetic_Experience", "Analysis_of_Knowledge", "Arguments_for_Theism", "Belief_or_Credence", "Capital_Punishment", "Chinese_Room", "Concepts", "Consciousness", "Continuum_Hypothesis", "Cosmological_fine_tuning", "Environmental_Ethics", "Extended_Mind", "Foundations_of_Mathematics", "Gender_Categories", "Grounds_of_Intentionality", "Hard_Problem_of_Consciousness", "Human_Genetic_Engineering", "Hume", "Immortality", "Interlevel_Metaphysics", "Justification", "Kant", "Law", "Material_Composition", "Metaontology", "Method_in_History_of_Philosophy", "Method_in_Political_Philosophy", "Mind_Uploading", "Moral_Principles", "Morality", "Normative_Concepts", "Ought_Implies_Can", "Philosophical_Knowledge", "Plato", "Politics", "Possible_Worlds", "Practical_Reason", "Principle_of_Sufficient_Reason", "Properties", "Propositional_Attitudes", "Propositions", "Quantum_Mechanics", "Race_Categories", "Rational_Disagreement", "Response_to_External_World_Skepticism", "Semantic_Content", "Sleeping_Beauty", "Spacetime", "Statue_and_Lump", "Temporal_Ontology", "Theory_of_Reference", "Time_Travel", "True_Contradictions", "Units_of_Selection", "Values_in_Science", "Well_Being", "Wittgenstein")),
                  selected = "Abortion"
                ),
                selectInput(
                  inputId = "xnat",
                  label = "Natinoality",
                  choices = c("EU", "Canada", "USA", "Africa", "Oceania", "Great Britian", "South America", "Asia", "No Answer", "All Nationalities"),
                  selected = "All Nationalities"
                )
              ),
              # Outputs: Display the table
              mainPanel(
                tabsnationality
              )
            )
          )
        )
      )
      
      
      
      
    )
  )
)


server <- function(input, output, session) {
  
  
  
  ##demo
  
  output$age_gender <- renderPlot({
    ggplot(data = mainsub, aes(x = YOB, fill = Gender)) +
      geom_histogram(binwidth = 5.5) +
      theme_bw(base_size = bsize) +
      ylab("Count") +
      xlab("Year of Birth") +
      ggtitle("Breakdown of Year born and Gender")
  })
  
  output$thingnat <- renderPlot({
    
    ggplot(data = mainsub, aes( y= fct_rev(fct_infreq(Nationality)))) +
      geom_bar( fill = "darkgreen") +
      theme_bw(base_size = bsize) +
      ylab("Nationality") +
      xlab("Respondents") +
      ggtitle("Respondent Nationalities") +
      geom_text(stat='count', aes(label = ..count..), hjust = -.2, size = 4) +
      xlim(0,1000)
    
  })
  
  output$aoscpie <- renderPlot({
    
    
    
    ggplot(plotmain, aes(x="", y=perc, fill=aoscategory))+
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start=0) +
      theme_void(base_size = bsize) +
      labs(title = "Sample Categories of Specialization", fill = "Category of Specialization") +
      theme(legend.title = element_text(size = bsize),
            legend.key.size = unit(1, "cm"),
            legend.text = element_text(size=14)) +
      geom_label_repel(aes(y = ypos, label = labels),
                       size = sz, nudge_x = 1, show.legend = FALSE) +
      scale_fill_brewer(palette="Set3")
  })    
  
  
  
  output$phil <- renderPlot({
    
    ggplot(data = mainsub, aes( y= fct_rev(fct_infreq(Philosopherover20)))) +
      geom_bar(fill = "purple") +
      theme_bw(base_size = bsize) +
      ylab("Philosopher") +
      xlab("Count") +
      ggtitle("Philosopher Alignment") +
      geom_text(stat='count', aes(label = ..count..), hjust = -.2, size = sz) +
      xlim(0,600)
  })
  ##age
  
  selectedy1age <- reactive({
    req(input$yage)
  })
  
  selectedy2age <- reactive({
    strsplit(req(selectedy1age()), ";")[[1]]
  })
  
  selectedyage <- reactive({
    gsub('[ ]', '_', selectedy2age())
  })
  
  selectedx1age <- reactive({
    req(input$xage)
  })
  
  selectedxage <- reactive({
    strsplit(req(selectedx1age()), ";")[[1]]
  })
  
  
  
  
  
  
  output$distPlotage <- renderTable({
    
    
    
    if (selectedxage() == "All Ages") {
      kablething <- mainsub %>%
        pivot_longer(selectedyage(), names_to = "Question") %>%
        count(Question, value) %>%
        group_by(Question) %>%
        filter(value != "NA") %>%
        mutate(pct = percent(n / sum(n)),
               Question = str_replace_all(Question, "_", " ")) %>%
        arrange(desc(n))
      
      return(kablething)
      
    } else if (selectedxage() != "All Ages") {
      kablething <- mainsub %>%
        filter(Age == selectedxage()) %>%
        pivot_longer(selectedyage(), names_to = "Question") %>%
        count(Question, value) %>%
        group_by(Question) %>%
        filter(value != "NA") %>%
        mutate(pct = percent(n / sum(n)),
               Question = str_replace_all(Question, "_", " ")) %>%
        arrange(desc(n))
      
      return(kablething)
    }
  })
  output$barage <- renderPlot({
    
    
    
    if (selectedxage() == "All Ages") {
      bardata <- mainsub %>%
        pivot_longer(selectedyage(), names_to = "Question") %>%
        count(Question, value) %>%
        group_by(Question) %>%
        filter(value != "NA") %>%
        mutate(n = n)
      
      plotthing <- ggplot(bardata, aes(y= reorder(value, n), x = n)) +
        geom_bar(fill = "black", stat = 'identity') +
        theme_bw(base_size = bsize) +
        geom_text(stat='identity', aes(label = n), hjust = -.2, size = sz) +
        xlab("Number of Responses") +
        ylab("Answer") +
        xlim(0,1400)
      
      return(plotthing)
      
    } else if (selectedxage() != "All Ages") {
      bardata <- mainsub %>%
        filter(Age == selectedxage()) %>%
        pivot_longer(selectedyage(), names_to = "Question") %>%
        count(Question, value) %>%
        group_by(Question) %>%
        filter(value != "NA") %>%
        mutate(n = n)
      
      
      
      
      plotthing <- ggplot(bardata, aes(y= reorder(value, n), x = n)) +
        geom_bar(fill = "black", stat = 'identity') +
        theme_bw(base_size = bsize) +
        geom_text(stat='identity', aes(label = n), hjust = -.2, size = sz) +
        xlab("Number of Responses") +
        ylab("Answer") +
        xlim(0,1400)
      
      
      return(plotthing)
    }
    
    
  })
  
  
  output$pieage <- renderPlot({
    
    
    
    if (selectedxage() == "All Ages") {
      bardata <- mainsub %>%
        pivot_longer(selectedyage(), names_to = "Question") %>%
        count(Question, value) %>%
        group_by(Question) %>%
        filter(value != "NA") %>%
        mutate(n = n,
               pct = (n / sum(n))) %>%
        arrange(pct, value) %>%
        mutate(pos = cumsum(pct)- 0.5*pct,
               value = factor(value, levels=value[order(-(pct))], ordered=TRUE)) %>%
        ungroup() %>%
        mutate(label = scales::percent(pct))
      
      plotthing <-  ggplot(bardata, aes(x="", y=pct, fill=value))+
        geom_bar(width = 1, stat = "identity") +
        coord_polar(theta = "y", start=0) +
        theme_void() +
        labs(fill = "Answer") +
        theme(legend.title = element_text(size = bsize),
              legend.key.size = unit(1, "cm"),
              legend.text = element_text(size=14)) +
        geom_label_repel(aes(y = pos, label = label),
                         size = sz, nudge_x = 1, show.legend = FALSE) +
        scale_fill_brewer(palette="Set3")
      
      return(plotthing)
      
    } else if (selectedxage() != "All Ages") {
      bardata <- mainsub %>%
        filter(Age == selectedxage()) %>%
        pivot_longer(selectedyage(), names_to = "Question") %>%
        count(Question, value) %>%
        group_by(Question) %>%
        filter(value != "NA") %>%
        mutate(n = n,
               pct = (n / sum(n))) %>%
        arrange(pct, value) %>%
        mutate(pos = cumsum(pct)- 0.5*pct,
               value = factor(value, levels=value[order(-(pct))], ordered=TRUE)) %>%
        ungroup() %>%
        mutate(label = scales::percent(pct))
      
      plotthing <-  ggplot(bardata, aes(x="", y=pct, fill=value))+
        geom_bar(width = 1, stat = "identity") +
        coord_polar(theta = "y", start=0) +
        theme_void() +
        labs(fill = "Answer") +
        theme(legend.title = element_text(size = bsize),
              legend.key.size = unit(1, "cm"),
              legend.text = element_text(size=14)) +
        geom_label_repel(aes(y = pos, label = label),
                         size = sz, nudge_x = 1, show.legend = FALSE) +
        scale_fill_brewer(palette="Set3")
      
      return(plotthing)
    }
    
  }) 
  
  
  ##Tradition
  
  selectedy1trad <- reactive({
    req(input$ytrad)
  })
  
  selectedy2trad <- reactive({
    strsplit(req(selectedy1trad()), ";")[[1]]
  })
  
  selectedytrad <- reactive({
    gsub('[ ]', '_', selectedy2trad())
  })
  
  selectedx1trad <- reactive({
    req(input$xtrad)
  })
  
  selectedxtrad <- reactive({
    strsplit(req(selectedx1trad()), ";")[[1]]
  })
  
  
  output$distPlottrad <- renderTable({
    
    if (selectedxtrad() == "All Traditions") {
      kablething <- mainsub %>%
        pivot_longer(selectedytrad(), names_to = "Question") %>%
        count(Question, value) %>%
        group_by(Question) %>%
        filter(value != "NA") %>%
        mutate(pct = percent(n / sum(n)),
               Question = str_replace_all(Question, "_", " ")) %>%
        arrange(desc(n))
      
      return(kablething)
    }
    else if(selectedxtrad() != "All Traditions") {
      kablething <- mainsub %>%
        filter(Tradition == selectedxtrad()) %>%
        pivot_longer(selectedytrad(), names_to = "Question") %>%
        count(Question, value) %>%
        group_by(Question) %>%
        filter(value != "NA") %>%
        mutate(pct = percent(n / sum(n)),
               Question = str_replace_all(Question, "_", " ")) %>%
        arrange(desc(n))
      
      return(kablething)
    }
  })
  
  
  output$bartrad <- renderPlot({
    
    if (selectedxtrad() == "All Traditions") {
      bardata <- mainsub %>%
        pivot_longer(selectedytrad(), names_to = "Question") %>%
        count(Question, value) %>%
        group_by(Question) %>%
        filter(value != "NA") %>%
        mutate(n = n)
      
      plotthing <- ggplot(bardata, aes(y= reorder(value, n), x = n)) +
        geom_bar(fill = "black", stat = 'identity') +
        theme_bw(base_size = bsize) +
        geom_text(stat='identity', aes(label = n), hjust = -.2, size = sz) +
        xlab("Number of Responses") +
        ylab("Answer") +
        xlim(0,1400)
      
      return(plotthing)
      
      
      
    } else if (selectedxtrad() != "All Traditions") {
      bardata <- mainsub %>%
        filter(Tradition == selectedxtrad()) %>%
        pivot_longer(selectedytrad(), names_to = "Question") %>%
        count(Question, value) %>%
        group_by(Question) %>%
        filter(value != "NA") %>%
        mutate(n = n)
      
      
      plotthing <- ggplot(bardata, aes(y= reorder(value, n), x = n)) +
        geom_bar(fill = "black", stat = 'identity') +
        theme_bw(base_size = bsize) +
        geom_text(stat='identity', aes(label = n), hjust = -.2, size = sz) +
        xlab("Number of Responses") +
        ylab("Answer") +
        xlim(0,1400)
      
      return(plotthing)
    }
    
  })
  output$pietrad <- renderPlot({
    
    
    
    if (selectedxtrad() == "All Traditions") {
      bardata <- mainsub %>%
        pivot_longer(selectedytrad(), names_to = "Question") %>%
        count(Question, value) %>%
        group_by(Question) %>%
        filter(value != "NA") %>%
        mutate(n = n,
               pct = (n / sum(n))) %>%
        arrange(pct, value) %>%
        mutate(pos = cumsum(pct)- 0.5*pct,
               value = factor(value, levels=value[order(-(pct))], ordered=TRUE)) %>%
        ungroup() %>%
        mutate(label = scales::percent(pct))
      
      plotthing <-  ggplot(bardata, aes(x="", y=pct, fill=value))+
        geom_bar(width = 1, stat = "identity") +
        coord_polar(theta = "y", start=0) +
        theme_void() +
        labs(fill = "Answer") +
        theme(legend.title = element_text(size = bsize),
              legend.key.size = unit(1, "cm"),
              legend.text = element_text(size=14)) +
        geom_label_repel(aes(y = pos, label = label),
                         size = sz, nudge_x = 1, show.legend = FALSE) +
        scale_fill_brewer(palette="Set3")
      
      return(plotthing)
      
    } else if (selectedxtrad() != "All Traditions") {
      bardata <- mainsub %>%
        filter(Tradition == selectedxtrad()) %>%
        pivot_longer(selectedytrad(), names_to = "Question") %>%
        count(Question, value) %>%
        group_by(Question) %>%
        filter(value != "NA") %>%
        mutate(n = n,
               pct = (n / sum(n))) %>%
        arrange(pct, value) %>%
        mutate(pos = cumsum(pct)- 0.5*pct,
               value = factor(value, levels=value[order(-(pct))], ordered=TRUE)) %>%
        ungroup() %>%
        mutate(label = scales::percent(pct))
      
      plotthing <-  ggplot(bardata, aes(x="", y=pct, fill=value))+
        geom_bar(width = 1, stat = "identity") +
        coord_polar(theta = "y", start=0) +
        theme_void() +
        labs(fill = "Answer") +
        theme(legend.title = element_text(size = bsize),
              legend.key.size = unit(1, "cm"),
              legend.text = element_text(size=14)) +
        geom_label_repel(aes(y = pos, label = label),
                         size = sz, nudge_x = 1, show.legend = FALSE) +
        scale_fill_brewer(palette="Set3")
      
      return(plotthing)
    }
    
  })
  
  ##aos
  
  selectedy1aos <- reactive({
    req(input$yaos)
  })
  
  selectedy2aos <- reactive({
    strsplit(req(selectedy1aos()), ";")[[1]]
  })
  
  selectedyaos <- reactive({
    gsub('[ ]', '_', selectedy2aos())
  })
  
  selectedx1aos <- reactive({
    req(input$xaos)
  })
  
  selectedxaos <- reactive({
    strsplit(req(selectedx1aos()), ";")[[1]]
  })
  output$distPlotaos <- renderTable({
    
    if (selectedxaos() == "All Areas") {
      kablething <- mainsub %>%
        pivot_longer(selectedyaos(), names_to = "Question") %>%
        count(Question, value) %>%
        group_by(Question) %>%
        filter(value != "NA") %>%
        mutate(pct = percent(n / sum(n)),
               Question = str_replace_all(Question, "_", " ")) %>%
        arrange(desc(n))
      
      return(kablething)
    }
    else if(selectedxaos() != "All Areas") {
      kablething <- mainsub %>%
        filter(aoscategory == selectedxaos()) %>%
        pivot_longer(selectedyaos(), names_to = "Question") %>%
        count(Question, value) %>%
        group_by(Question) %>%
        filter(value != "NA") %>%
        mutate(pct = percent(n / sum(n)),
               Question = str_replace_all(Question, "_", " ")) %>%
        arrange(desc(n))
      
      return(kablething)
    }
  })
  
  
  output$baraos <- renderPlot({
    
    if (selectedxaos() == "All Areas") {
      bardata <- mainsub %>%
        pivot_longer(selectedyaos(), names_to = "Question") %>%
        count(Question, value) %>%
        group_by(Question) %>%
        filter(value != "NA") %>%
        mutate(n = n)
      
      plotthing <- ggplot(bardata, aes(y= reorder(value, n), x = n)) +
        geom_bar(fill = "black", stat = 'identity') +
        theme_bw(base_size = bsize) +
        geom_text(stat='identity', aes(label = n), hjust = -.2, size = sz) +
        xlab("Number of Responses") +
        ylab("Answer") +
        xlim(0,1400)
      
      return(plotthing)
      
      
      
    } else if (selectedxaos() != "All Areas") {
      bardata <- mainsub %>%
        filter(aoscategory == selectedxaos()) %>%
        pivot_longer(selectedyaos(), names_to = "Question") %>%
        count(Question, value) %>%
        group_by(Question) %>%
        filter(value != "NA") %>%
        mutate(n = n)
      
      
      plotthing <- ggplot(bardata, aes(y= reorder(value, n), x = n)) +
        geom_bar(fill = "black", stat = 'identity') +
        theme_bw(base_size = bsize) +
        geom_text(stat='identity', aes(label = n), hjust = -.2, size = sz) +
        xlab("Number of Responses") +
        ylab("Answer") +
        xlim(0,1400)
      
      return(plotthing)
    }
    
    
  })
  output$pieaos <- renderPlot({
    
    
    
    if (selectedxaos() == "All Areas") {
      bardata <- mainsub %>%
        pivot_longer(selectedyaos(), names_to = "Question") %>%
        count(Question, value) %>%
        group_by(Question) %>%
        filter(value != "NA") %>%
        mutate(n = n,
               pct = (n / sum(n))) %>%
        arrange(pct, value) %>%
        mutate(pos = cumsum(pct)- 0.5*pct,
               value = factor(value, levels=value[order(-(pct))], ordered=TRUE)) %>%
        ungroup() %>%
        mutate(label = scales::percent(pct))
      
      plotthing <-  ggplot(bardata, aes(x="", y=pct, fill=value))+
        geom_bar(width = 1, stat = "identity") +
        coord_polar(theta = "y", start=0) +
        theme_void(base_size = bsize) +
        labs(fill = "Answer") +
        theme(legend.title = element_text(size = bsize),
              legend.key.size = unit(1, "cm"),
              legend.text = element_text(size=14)) +
        geom_label_repel(aes(y = pos, label = label),
                         size = sz, nudge_x = 1, show.legend = FALSE) +
        scale_fill_brewer(palette="Set3")
      
      return(plotthing)
      
    } else if (selectedxaos() != "All Areas") {
      bardata <- mainsub %>%
        filter(aoscategory == selectedxaos()) %>%
        pivot_longer(selectedyaos(), names_to = "Question") %>%
        count(Question, value) %>%
        group_by(Question) %>%
        filter(value != "NA") %>%
        mutate(n = n,
               pct = (n / sum(n))) %>%
        arrange(pct, value) %>%
        mutate(pos = cumsum(pct)- 0.5*pct,
               value = factor(value, levels=value[order(-(pct))], ordered=TRUE)) %>%
        ungroup() %>%
        mutate(label = scales::percent(pct))
      
      plotthing <-  ggplot(bardata, aes(x="", y=pct, fill=value))+
        geom_bar(width = 1, stat = "identity") +
        coord_polar(theta = "y", start=0) +
        theme_void(base_size = bsize) +
        labs(fill = "Answer") +
        theme(legend.title = element_text(size = bsize),
              legend.key.size = unit(1, "cm"),
              legend.text = element_text(size=14)) +
        geom_label_repel(aes(y = pos, label = label),
                         size = sz, nudge_x = 1, show.legend = FALSE) +
        scale_fill_brewer(palette="Set3")
      
      return(plotthing)
    }
  })
  
  ##phil
  
  selectedy1phil <- reactive({
    req(input$yphil)
  })
  
  selectedy2phil <- reactive({
    strsplit(req(selectedy1phil()), ";")[[1]]
  })
  
  selectedyphil <- reactive({
    gsub('[ ]', '_', selectedy2phil())
  })
  
  selectedx1phil <- reactive({
    req(input$xphil)
  })
  
  selectedxphil <- reactive({
    strsplit(req(selectedx1phil()), ";")[[1]]
  })
  output$distPlotphil <- renderTable({
    
    if (selectedxphil() == "All Philosophers") {
      kablething <- mainsub %>%
        pivot_longer(selectedyphil(), names_to = "Question") %>%
        count(Question, value) %>%
        group_by(Question) %>%
        filter(value != "NA") %>%
        mutate(pct = percent(n / sum(n)),
               Question = str_replace_all(Question, "_", " ")) %>%
        arrange(desc(n))
      
      return(kablething)
    } else if (selectedxphil() != "All Philosophers") {
      kablething <- mainsub %>%
        filter(Philosopher == selectedxphil()) %>%
        pivot_longer(selectedyphil(), names_to = "Question") %>%
        count(Question, value) %>%
        group_by(Question) %>%
        filter(value != "NA") %>%
        mutate(pct = percent(n / sum(n)),
               Question = str_replace_all(Question, "_", " ")) %>%
        arrange(desc(n))
      
      return(kablething)
    }
  })
  
  output$barphil <- renderPlot({
    
    if (selectedxphil() == "All Philosophers") {
      bardata <- mainsub %>%
        pivot_longer(selectedyphil(), names_to = "Question") %>%
        count(Question, value) %>%
        group_by(Question) %>%
        filter(value != "NA") %>%
        mutate(n = n)
      
      plotthing <- ggplot(bardata, aes(y= reorder(value, n), x = n)) +
        geom_bar(fill = "black", stat = 'identity') +
        theme_bw(base_size = bsize) +
        geom_text(stat='identity', aes(label = n), hjust = -.2, size = sz) +
        xlab("Number of Responses") +
        ylab("Answer") +
        xlim(0,1400)
      
      return(plotthing)
      
      
      
    } else if (selectedxphil() != "All Philosophers") {
      bardata <- mainsub %>%
        filter(Philosopher == selectedxphil()) %>%
        pivot_longer(selectedyphil(), names_to = "Question") %>%
        count(Question, value) %>%
        group_by(Question) %>%
        filter(value != "NA") %>%
        mutate(n = n)
      
      
      plotthing <- ggplot(bardata, aes(y= reorder(value, n), x = n)) +
        geom_bar(fill = "black", stat = 'identity') +
        theme_bw(base_size = bsize) +
        geom_text(stat='identity', aes(label = n), hjust = -.2, size = sz) +
        xlab("Number of Responses") +
        ylab("Answer") +
        xlim(0,1400)
      
      return(plotthing)
    }
    
    
  })
  output$piephil <- renderPlot({
    
    
    
    if (selectedxphil() == "All Philosophers") {
      bardata <- mainsub %>%
        pivot_longer(selectedyphil(), names_to = "Question") %>%
        count(Question, value) %>%
        group_by(Question) %>%
        filter(value != "NA") %>%
        mutate(n = n,
               pct = (n / sum(n))) %>%
        arrange(pct, value) %>%
        mutate(pos = cumsum(pct)- 0.5*pct,
               value = factor(value, levels=value[order(-(pct))], ordered=TRUE)) %>%
        ungroup() %>%
        mutate(label = scales::percent(pct))
      
      plotthing <-  ggplot(bardata, aes(x="", y=pct, fill=value))+
        geom_bar(width = 1, stat = "identity") +
        coord_polar(theta = "y", start=0) +
        theme_void() +
        labs(fill = "Answer") +
        theme(legend.title = element_text(size = bsize),
              legend.key.size = unit(1, "cm"),
              legend.text = element_text(size=14)) +
        geom_label_repel(aes(y = pos, label = label),
                         size = sz, nudge_x = 1, show.legend = FALSE) +
        scale_fill_brewer(palette="Set3")
      
      return(plotthing)
      
    } else if (selectedxphil() != "All Philosophers") {
      bardata <- mainsub %>%
        filter(Philosopher == selectedxphil()) %>%
        pivot_longer(selectedyphil(), names_to = "Question") %>%
        count(Question, value) %>%
        group_by(Question) %>%
        filter(value != "NA") %>%
        mutate(n = n,
               pct = (n / sum(n))) %>%
        arrange(pct, value) %>%
        mutate(pos = cumsum(pct)- 0.5*pct,
               value = factor(value, levels=value[order(-(pct))], ordered=TRUE)) %>%
        ungroup() %>%
        mutate(label = scales::percent(pct))
      
      plotthing <-  ggplot(bardata, aes(x="", y=pct, fill=value))+
        geom_bar(width = 1, stat = "identity") +
        coord_polar(theta = "y", start=0) +
        theme_void() +
        labs(fill = "Answer") +
        theme(legend.title = element_text(size = bsize),
              legend.key.size = unit(1, "cm"),
              legend.text = element_text(size=14)) +
        geom_label_repel(aes(y = pos, label = label),
                         size = sz, nudge_x = 1, show.legend = FALSE) +
        scale_fill_brewer(palette="Set3")
      
      return(plotthing)
    }
    
  })
  
  ##nat
  selectedy1nat <- reactive({    
    req(input$ynat)
  })
  
  selectedy2nat <- reactive({
    strsplit(req(selectedy1nat()), ";")[[1]]
  })
  
  selectedynat <- reactive({
    gsub('[ ]', '_', selectedy2nat())
  })
  
  selectedx1nat <- reactive({
    req(input$xnat)
  })
  
  selectedxnat <- reactive({
    strsplit(req(selectedx1nat()), ";")[[1]]
  })
  
  output$distPlotnat <- renderTable({
    
    if (selectedxnat() == "All Nationalities") {
      kablething <- mainsub %>%
        pivot_longer(selectedynat(), names_to = "Question") %>%
        count(Question, value) %>%
        group_by(Question) %>%
        filter(value != "NA") %>%
        mutate(pct = percent(n / sum(n)),
               Question = str_replace_all(Question, "_", " ")) %>%
        arrange(desc(n))
      
      return(kablething)
    } else if (selectedxnat() != "All Nationalities") {
      kablething <- mainsub %>%
        filter(Nationality == selectedxnat()) %>%
        pivot_longer(selectedynat(), names_to = "Question") %>%
        count(Question, value) %>%
        group_by(Question) %>%
        filter(value != "NA") %>%
        mutate(pct = percent(n / sum(n)),
               Question = str_replace_all(Question, "_", " ")) %>%
        arrange(desc(n))
      
      return(kablething)
    }
  })
  
  output$barnat <- renderPlot({
    
    if (selectedxnat() == "All Nationalities") {
      bardata <- mainsub %>%
        pivot_longer(selectedynat(), names_to = "Question") %>%
        count(Question, value) %>%
        group_by(Question) %>%
        filter(value != "NA") %>%
        mutate(n = n)
      
      plotthing <- ggplot(bardata, aes(y= reorder(value, n), x = n)) +
        geom_bar(fill = "black", stat = 'identity') +
        theme_bw(base_size = bsize) +
        geom_text(stat='identity', aes(label = n), hjust = -.2, size = sz) +
        xlab("Number of Responses") +
        ylab("Answer") +
        xlim(0,1400)
      
      return(plotthing)
      
      
      
    } else if (selectedxnat() != "All Nationalities") {
      bardata <- mainsub %>%
        filter(Nationality == selectedxnat()) %>%
        pivot_longer(selectedynat(), names_to = "Question") %>%
        count(Question, value) %>%
        group_by(Question) %>%
        filter(value != "NA") %>%
        mutate(n = n)
      
      
      plotthing <- ggplot(bardata, aes(y= reorder(value, n), x = n)) +
        geom_bar(fill = "black", stat = 'identity') +
        theme_bw(base_size = bsize) +
        geom_text(stat='identity', aes(label = n), hjust = -.2, size = sz) +
        xlab("Number of Responses") +
        ylab("Answer") +
        xlim(0,1400)
      
      return(plotthing)
    }
    
    
  })
  
  output$pienat <- renderPlot({
    
    
    
    if (selectedxnat() == "All Nationalities") {
      bardata <- mainsub %>%
        pivot_longer(selectedynat(), names_to = "Question") %>%
        count(Question, value) %>%
        group_by(Question) %>%
        filter(value != "NA") %>%
        mutate(n = n,
               pct = (n / sum(n))) %>%
        arrange(pct, value) %>%
        mutate(pos = cumsum(pct)- 0.5*pct,
               value = factor(value, levels=value[order(-(pct))], ordered=TRUE)) %>%
        ungroup() %>%
        mutate(label = scales::percent(pct))
      
      plotthing <-  ggplot(bardata, aes(x="", y=pct, fill=value))+
        geom_bar(width = 1, stat = "identity") +
        coord_polar(theta = "y", start=0) +
        theme_void() +
        labs(fill = "Answer") +
        theme(legend.title = element_text(size = bsize),
              legend.key.size = unit(1, "cm"),
              legend.text = element_text(size=14)) +
        geom_label_repel(aes(y = pos, label = label),
                         size = sz, nudge_x = 1, show.legend = FALSE) +
        scale_fill_brewer(palette="Set3")
      
      return(plotthing)
      
    } else if (selectedxnat() != "All Nationalities") {
      bardata <- mainsub %>%
        filter(Nationality == selectedxnat()) %>%
        pivot_longer(selectedynat(), names_to = "Question") %>%
        count(Question, value) %>%
        group_by(Question) %>%
        filter(value != "NA") %>%
        mutate(n = n,
               pct = (n / sum(n))) %>%
        arrange(pct, value) %>%
        mutate(pos = cumsum(pct)- 0.5*pct,
               value = factor(value, levels=value[order(-(pct))], ordered=TRUE)) %>%
        ungroup() %>%
        mutate(label = scales::percent(pct))
      
      plotthing <-  ggplot(bardata, aes(x="", y=pct, fill=value))+
        geom_bar(width = 1, stat = "identity") +
        coord_polar(theta = "y", start=0) +
        theme_void() +
        labs(fill = "Answer") +
        theme(legend.title = element_text(size = bsize),
              legend.key.size = unit(1, "cm"),
              legend.text = element_text(size=14)) +
        geom_label_repel(aes(y = pos, label = label),
                         size = sz, nudge_x = 1, show.legend = FALSE) +
        scale_fill_brewer(palette="Set3")
      
      return(plotthing)
    }
    
  }) 
  
  
}




shinyApp(ui, server)

