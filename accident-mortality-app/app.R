library(tidyverse)
library(shiny)

global_mortality = read_csv("data/global_mortality.csv")
continent = read_csv("data/countryContinent.csv")


global_mortality = global_mortality |>
  select(country, country_code, year, `Road accidents (%)`, `Suicide (%)`, `Homicide (%)`,
         `Drowning (%)`, `Fire (%)`, `Heat-related (hot and cold exposure) (%)`,
         `Natural disasters (%)`, `Conflict (%)`, `Terrorism (%)`) |>
  left_join(continent, by = c("country_code" = "code_3")) |>
  select(country.x, continent, sub_region, year, `Road accidents (%)`, `Suicide (%)`, `Homicide (%)`,
         `Drowning (%)`, `Fire (%)`, `Heat-related (hot and cold exposure) (%)`,
         `Natural disasters (%)`, `Conflict (%)`, `Terrorism (%)`) |>
  rename(country = country.x, `Road accidents` = `Road accidents (%)`, `Suicide` = `Suicide (%)`,
         `Homicide` = `Homicide (%)`, `Drowning` = `Drowning (%)`, `Fire` = `Fire (%)`,
         `Heat-related` = `Heat-related (hot and cold exposure) (%)`,
         `Natural disasters` = `Natural disasters (%)`, `Conflict` = `Conflict (%)`,
         `Terrorism` = `Terrorism (%)`)


ui = navbarPage(
  title = "Accident Mortality",
  tabPanel(
    title = "Input / Visualization",
    titlePanel(title = "Percent of Mortality Caused by Accidents: 1990 - 2016"),
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "continent",
                    label = "Continent:",
                    choices = sort(unique(global_mortality$continent)),
                    selected = "Americas"),
        selectInput(inputId = "sub_reg",
                    label = "Sub Region:",
                    choices = sort(unique(global_mortality$sub_region))),
        selectInput(inputId = "country",
                    label = "Country (Region):",
                    choices = sort(unique(global_mortality$country))),
        checkboxInput(inputId = "all_countries",
                      label = "Filter Table to Selected Countries",
                      value = FALSE)
      ),
      mainPanel(plotOutput("plot"))
    )
    ),
  tabPanel(title = "Table", dataTableOutput("table")),
  tabPanel(title = "About", includeMarkdown("about.Rmd"))
  )


server = function(input, output) {

  mortality_continent = reactive({
    global_mortality |>
      filter(continent == input$continent)
  })

  observeEvent(
    eventExpr = input$continent,
    handlerExpr = {
      updateSelectInput(inputId = "sub_reg",
                        choices = sort(unique(mortality_continent()$sub_region)),
                        selected = sort(unique(mortality_continent()$sub_region))[1])
      updateSelectInput(inputId = "country",
                        choices = sort(unique(mortality_continent_subregion()$country)),
                        selected = sort(unique(mortality_continent_subregion()$country))[1])

    }
    )

  mortality_continent_subregion = reactive({
    mortality_continent() |>
      filter(sub_region == input$sub_reg)
  })

  observeEvent(
    eventExpr = input$sub_reg,
    handlerExpr = {
      updateSelectInput(inputId = "country",
                        choices = sort(unique(mortality_continent_subregion()$country)),
                        selected = sort(unique(mortality_continent_subregion()$country))[1])
    }
  )

    output$plot = renderPlot({

      global_mortality |>
        filter(continent == input$continent) |>
        filter(sub_region == input$sub_reg) |>
        filter(country == input$country) |>
        pivot_longer(`Road accidents`:`Terrorism`, names_to = "Cause", values_to = "Percentage") |>
        summarise(Percentage = (sum(Percentage, na.rm = TRUE) / 27), .by = Cause) |>
        ggplot() +
        aes(x = Cause, y = Percentage, fill = Cause) +
        geom_bar(stat = "identity") +
        ylim(0, 10) +
        theme_bw()

    })

    output$table = renderDataTable({

      tab = mortality_continent() |>
        calc_perc()

      if(input$all_countries) {
        tab = tab |>
          filter(country == input$country)
      }

      tab

    })
}


shinyApp(ui = ui, server = server)
