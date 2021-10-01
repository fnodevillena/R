# UI Script | Learning Evidence
# Author: Francis Nathanael De Villena
# Github Repo: https://github.com/Frobbly

# Required libraries
library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)
library(mgcv)
library(mgcViz)
library(markdown)
library(shinythemes)

navbarPage(theme = shinytheme("cosmo"), "Road Infrastucture and National Development",
          tabPanel("Home",
                   fluidRow(
                     column(6,
                            p(style="text-align: justify;","Road infrastructure and land transport are essential for well-functioning economies, especially in developing countries such as the Philippines, where they play a multifaceted role in the pursuit of development objectives. Moreover, road networks are the driver for a nation's productivity and progress, as they can hasten agricultural and rural growth, stimulate industry and commerce, boost the viability of urban areas, create career opportunities, widen the options of better education, and make social and health services readily reachable. Not only did road networks make land transportation more cost-effective and time-saving but also allow access to various remote rural areas throughout the country, provide mobility of the workforce, encourage the influx of tourism. and promote a wide variety of commercial and social activities."),
                            p(style="text-align: justify;","Using a Gaussian generalized additive model approach with penalized cubic regression splines provided a flexible and useful tool for revealing insight into the relationship of road infrastructure and regional economic output. The findings of this study contribute to existing literature and can be used to aid a further understanding of public road infrastructure and its effects on the national macroeconomy. In this application, different analyses were performed, and it was observed that values in different regions may vary and that the relationship between GDP per capita and road density of every region shows a strong positive correlation and linear relationship. Thus, the expansion of public road infrastructure yields regional economic growth."),
                     ),
                     column(6,
                            img(src='aurora.jpg')

                     )
                   )
          ),
          tabPanel("Analysis",
                   navlistPanel(
                     "Visual Exploration",
                       tabPanel("Population",
                                tabsetPanel(type = "tabs",
                                            tabPanel("Bar Graph", plotOutput("bar1")),
                                            tabPanel("Box Plot", plotOutput("plot.box1")),
                                            tabPanel("Histogram", plotOutput("plot.norm1")),
                                            tabPanel("Density Plot", plotOutput("plot.dens1"))
                                )
                       ),
                       tabPanel("Gross Domestic Product",
                                tabsetPanel(type = "tabs",
                                            tabPanel("Bar Graph", plotOutput("bar2")),
                                            tabPanel("Box Plot", plotOutput("plot.box2")),
                                            tabPanel("Histogram", plotOutput("plot.norm2")),
                                            tabPanel("Density Plot", plotOutput("plot.dens2"))
                                )
                       ),
                       tabPanel("Gross Domestic Product per Capita",
                                tabsetPanel(type = "tabs",
                                            tabPanel("Bar Graph", plotOutput("bar4")),
                                            tabPanel("Box Plot", plotOutput("plot.box3")),
                                            tabPanel("Histogram", plotOutput("plot.norm3")),
                                            tabPanel("Density Plot", plotOutput("plot.dens3"))
                                )
                       ),
                       tabPanel("Road Density",
                                tabsetPanel(type = "tabs",
                                            tabPanel("Bar Graph", plotOutput("bar3")),
                                            tabPanel("Box Plot", plotOutput("plot.box4")),
                                            tabPanel("Histogram", plotOutput("plot.norm4")),
                                            tabPanel("Density Plot", plotOutput("plot.dens4"))
                                )
                       ),
                     "Correlation Analysis",
                      tabPanel("Road Density and \nGross Domestic Product per Capita",
                              tabsetPanel(type = "tabs",
                                          tabPanel("Scatter Plot", plotOutput("plot.scat1")),
                                          tabPanel("Pearson", verbatimTextOutput("summary.pears")),
                                          tabPanel("Shapiro-Wilk", 
                                                   fluidRow (verbatimTextOutput("summary.shap1")), 
                                                   fluidRow (verbatimTextOutput("summary.shap2"))
                                          )
                              )
                      )
                     )
          ),
          tabPanel("Modelling",
                   fluidRow(
                     column(12,
                          fluidRow(
                           column(4,
                                  h3("Generalized Additive Model"),
                                  br(),
                                  p(style="text-align: justify;","To obtain a fitting model that has reliable and stable results, a generalized additive model with univariate penalized cubic regression spline smooths and the restricted maximum likelihood was used to represent the causal relationship of road density and gross domestic product per capita.")
                           ),
                           column(8,
                                  tabsetPanel(type = "tabs",
                                              tabPanel("Model", plotOutput("plot.spline")),
                                              tabPanel("Summary", verbatimTextOutput("summary")),
                                              tabPanel("Check",
                                                       fluidRow (verbatimTextOutput("summary.check")), 
                                                       fluidRow (verbatimTextOutput("summary.conc"))
                                                       ),
                                              tabPanel("Diagnostic Plots", plotOutput("plot.gamdiag"))
                                              )
                                  )
                           )
                          )
                     )
          ),
          navbarMenu("More",
                     tabPanel("Table",
                              DT::dataTableOutput("table")
                     ),
                     tabPanel("About",
                              h2("Authors"),
                              fluidRow(
                                column(6,
                                       h3("Francis Nathanael De Villena"),
                                       p("University of Southeastern Philippines"),
                                       p("Inigo St., Bo. Obrero, Davao City, Davao del Sur"),
                                       p("fnodevillena@usep.edu.ph")
                                ),
                                column(6,
                                       h3("Kent Cyril Bordios"),
                                       p("University of Southeastern Philippines"),
                                       p("Inigo St., Bo. Obrero, Davao City, Davao del Sur"),
                                       p("kcbordios@usep.edu.ph")
                                )
                              )
                     )
          )
)
