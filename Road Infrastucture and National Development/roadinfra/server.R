# Server Script | Learning Evidence
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

# Note: Kindly make sure to install dependencies first before running the app.

function(input, output, session) {
  
  # PSA/DPWH Dataset
  road = read.csv("road.csv",
                  header = TRUE,
                  sep = ";",
                  colClasses = c("year"="character"));
  
  # 2020
  road = road[road$year %in% "2020",]
  
  # GAM
  road.regression = gam(capita ~ s(rdensity, bs="cr"), data = road, method = "REML", select=TRUE)
  
  road.regression = getViz(road.regression)
  
  # Scientific Notation
  options(scipen=999)
  
  # ==============
  # Bar Graph
  
  # Population
  output$bar1 <- renderPlot({
    ggplot(road, aes(region, population)) +
      geom_bar(stat="identity", position=position_dodge(), fill = "#4579fd") +
      labs(x = "Region",
           y = "Population") +
      theme_minimal()
  })
  
  # GDP
  output$bar2 <- renderPlot({
    ggplot(road, aes(region, gdp)) +
      geom_bar(stat = "identity", position = position_dodge(), fill = "#4579fd") +
      labs(x = "Region",
           y = expression(paste("GDP ", italic("(1000 Php)")))) +
      theme_minimal()
  })
  
  # Road Density
  output$bar3 <- renderPlot({
    ggplot(road, aes(region, rdensity)) +
      geom_bar(stat="identity", position=position_dodge(), fill = "#4579fd") +
      labs(x = "Region",
           y = expression(paste("Road Density ", italic("(km/100 sqkm)")))) +

      theme_minimal()
  })
  
  # Capita
  output$bar4 <- renderPlot({
    ggplot(road, aes(region, capita)) +
      geom_bar(stat="identity", position=position_dodge(), fill = "#4579fd") +
      labs(x = "Region",
           y = expression(paste("GDP per capita ", italic("(Php)")))) +

      theme_minimal()
  })
  
  # =================
  # Histogram
  
  # Population
  output$plot.norm1 <- renderPlot({
    ggplot(road, aes(population)) +
      geom_histogram(aes(y = ..count..), colour="#0044ff", fill = "#4579fd") +
      labs(x = "Population",
           y = "Frequency") +
      geom_density() +
      theme_minimal()
  })
  
  # GDP
  output$plot.norm2 <- renderPlot({
    ggplot(road, aes(gdp)) +
      geom_histogram(aes(y = ..count..), colour="#0044ff", fill = "#4579fd") +
      labs(x = expression(paste("GDP", italic(" (1000 Php)"))),
           y = "Frequency") +
      geom_density() +
      theme_minimal()
  })
  
  # Capita
  output$plot.norm3 <- renderPlot({
    ggplot(road, aes(capita)) +
      geom_histogram(aes(y = ..count..), colour="#0044ff", fill = "#4579fd") +
      labs(x = expression(paste("GDP per capita", italic(" (Php)"))),
           y = "Frequency") +
      geom_density() +
      theme_minimal()
  })

  # Road Density
  output$plot.norm4 <- renderPlot({
    ggplot(road, aes(rdensity)) +
      geom_histogram(aes(y = ..count..), colour="#0044ff", fill = "#4579fd") +
      labs(x = expression(paste("Road Density", italic(" (km/sqkm)"))),
           y = "Frequency") +
      geom_density() +
      theme_minimal()
  })
  
  # ==============
  # Scatterplot
  
  # Road Capita
  output$plot.scat1 <- renderPlot({
    ggplot(road,aes(rdensity, capita)) +
      geom_point(size = 1.75, color = "#4579fd") +
      geom_smooth(method = "loess", col = "#000000") +
      labs(x = expression(paste("Road Density", italic(" (km/sqkm)"))),
           y = expression(paste("GDP per capita", italic(" (Php)")))) +
      theme_minimal()
  }) 
  
  # Pop GDP
  output$plot.scat2 <- renderPlot({
    ggplot(road,aes(population, gdp, color = year)) +
      geom_point(size = 1.75) +
      labs(x = "Population",
           y = expression(paste("GDP", italic(" (Php)")))) +
      scale_color_manual("Year", values = c("#0044ff", "#4579fd", "#82a0ff")) +
      theme_minimal()
  }) 
  
  # =============
  # Boxplot
  
  # Population
  output$plot.box1 <- renderPlot({
    ggplot(road, aes(year, population)) + 
      geom_boxplot(outlier.size=2,
                   size=.75,
                   fill = "#4579fd") +
      labs(x = "Year",
           y = "Population") +

      theme_minimal() +
      theme(legend.position="none")
  }) 
  
  # GDP
  output$plot.box2 <- renderPlot({
    ggplot(road, aes(year, gdp)) + 
      geom_boxplot(outlier.size=2,
                   size=.75,
                   fill = "#4579fd") +
      labs(x = "Year",
           y = expression(paste("GDP", italic(" (1000 Php)")))) +

      theme_minimal() +
      theme(legend.position="none")
  }) 
  
  # Capita
  output$plot.box3 <- renderPlot({
    ggplot(road, aes(year, capita)) + 
      geom_boxplot(outlier.size=2,
                   size=.75,
                   fill = "#4579fd") +
      labs(x = "Year",
           y = expression(paste("GDP per capita", italic(" (Php)")))) +

      theme_minimal() +
      theme(legend.position="none")
  }) 
  
  # Road Density
  output$plot.box4 <- renderPlot({
    ggplot(road, aes(year, rdensity)) + 
      geom_boxplot(outlier.size=2,
                   size=.75,
                   fill = "#4579fd") +
      labs(x = "Year",
           y = expression(paste("Road Density", italic(" (km/sqkm)")))) +

      theme_minimal() +
      theme(legend.position="none")
  }) 
  
  # ==============
  # Density Plot
  
  # Population
  output$plot.dens1 <- renderPlot({
    ggplot(road, aes(population, color=year)) +
      geom_density(aes(x = population), alpha = 0.2) +
      geom_rug(aes(x = population, y = 0), position = position_jitter(height = 0)) +
      geom_vline(aes(xintercept=mean(population)),
                 color="#000000",
                 linetype="dashed") +
      labs(x = "Population",
           y = "Density")+
      scale_color_manual("Year", values = c("#0044ff", "#4579fd", "#82a0ff")) +

      theme_minimal()
  }) 
  
  # GDP
  output$plot.dens2 <- renderPlot({
    ggplot(road, aes(gdp, color=year)) +
      geom_density(aes(x = gdp), alpha = 0.2) +
      geom_rug(aes(x = gdp, y = 0), position = position_jitter(height = 0)) +
      geom_vline(aes(xintercept=mean(gdp)),
                 color="#000000",
                 linetype="dashed") +
      labs(x = expression(paste("GDP ", italic("(1000 Php)"))),
           y = "Density")+
      scale_color_manual("Year", values = c("#0044ff", "#4579fd", "#82a0ff")) +

      theme_minimal()
  }) 
  
  # Capita
  output$plot.dens3 <- renderPlot({
    ggplot(road, aes(capita, color=year)) +
      geom_density(aes(x = capita), alpha = 0.2) +
      geom_rug(aes(x = capita, y = 0), position = position_jitter(height = 0)) +
      geom_vline(aes(xintercept=mean(capita)),
                 color="#000000",
                 linetype="dashed") +
      labs(x = expression(paste("GDP per capita ", italic("(Php)"))),
           y = "Density")+
      scale_color_manual("Year", values = c("#0044ff", "#4579fd", "#82a0ff")) +

      theme_minimal()
  }) 
  
  # Road Density
  output$plot.dens4 <- renderPlot({
    ggplot(road, aes(rdensity, color=year)) +
      geom_density(aes(x = rdensity), alpha = 0.2) +
      geom_rug(aes(x = rdensity, y = 0), position = position_jitter(height = 0)) +
      geom_vline(aes(xintercept=mean(rdensity)),
                 color="#000000",
                 linetype="dashed") +
      labs(x = expression(paste("Road Density", italic(" (km/sqkm)"))),
           y = "Density")+
      scale_color_manual("Year", values = c("#0044ff", "#4579fd", "#82a0ff")) +

      theme_minimal()
  }) 
  
  # ============
  # Spline Regression
  
  # Road Infrastructure and Economy Model 
  output$plot.spline <- renderPlot({
    ggplot(road,aes(rdensity, capita)) +
      geom_point(size = 1.75, color = "#4579fd") +
      geom_rug(aes(x = rdensity, y = 0), position = position_jitter(height = 0), colour="#4579fd") +
      stat_smooth(method = gam, formula = y ~ s(x, bs="cr"), color = "#000000") +
      labs(x = expression(paste("Road Density", italic(" (km/sqkm)"))),
           y = expression(paste("GDP per capita", italic(" (Php)")))) +
      scale_color_manual("Year", values = c("#0044ff", "#4579fd", "#82a0ff")) +
      theme_minimal()
  }) 
  
  # =============
  # Residual Analysis
  
  # Residuals vs Fitted Values
  output$plot.resid <- renderPlot({
    ggplot(hehe, aes(fitted.values, residuals)) +
      geom_point(size = 1.75, color = "#4579fd") +
      geom_smooth(method="loess", color= "#000000") + 
      geom_hline(yintercept = 0, linetype=2, color= "#000000") +
      labs(x = "Fitted Values",
           y = "Residuals") +
      theme_minimal()
  })
  
  # Normal Quantile-Quantile
  output$plot.qq <- renderPlot({
    ggplot(road.regression, aes(sample = rstandard(road.regression))) + 
      geom_qq(size = 1.75, color = "#4579fd") +
      stat_qq_line(color= "#000000") +
      labs(x = "Theoretical Quantiles",
           y = "Standardized Residuals") +
      theme_minimal()
  })
  
  # Scale-Location
  output$plot.scaloc <- renderPlot({
    ggplot(road.regression, aes(.fitted, sqrt(abs(.stdresid)))) +
      geom_point(na.rm=TRUE, color = "#4579fd") +
      stat_smooth(method="loess", na.rm = TRUE, color= "#000000") +
      labs(x = "Fitted Values",
           y = expression(sqrt("|Standardized Residuals|"))) +
      theme_minimal()
  }) 
  
  # Cook's Distance
  output$plot.cooks <- renderPlot({
    ggplot(road.regression, aes(seq_along(.cooksd), .cooksd)) + 
      geom_bar(stat="identity", position="identity", fill = "#4579fd") +
      labs(x = "Observed Number",
           y = "Cook's Distance") +
      theme_minimal()
  }) 
  
  # Residual vs Leverage
  output$plot.leverage <- renderPlot({
    ggplot(road.regression, aes(.hat, .stdresid)) + 
      geom_point(aes(size=.cooksd), na.rm=TRUE, color = "#4579fd") +
      stat_smooth(method="loess", na.rm=TRUE, color= "#000000") +
      scale_size_continuous("Cook's Distance", range=c(1,5)) +
      labs(x = "Leverage",
           y = "Standardized Residuals") +
      theme_minimal()
  }) 
  
  # Cook's Distance vs Leverage
  output$plot.cooklev <- renderPlot({
    ggplot(road.regression, aes(.hat, .cooksd)) +
      geom_point(na.rm=TRUE, color = "#4579fd") +
      stat_smooth(method="loess", na.rm=TRUE, color= "#000000") +
      geom_abline(slope=seq(0,3,0.5), color= "#000000", linetype="dashed") +
      labs(x = "Leverage",
           y = "Cook's Distance") +
      theme_minimal()
  }) 
  
  # GAM Diagnostics
  
  output$plot.gamdiag <- renderPlot({
  check(road.regression,
        a.qq = list(method = "auto",
                    a.cipoly = list(fill = "#4579fd"),
                    a.qqpoi = list(color = "#4579fd",
                                   size = 10)),
        a.respoi = list(size = 1, color = "#4579fd"),
        a.hist = list(bins = 8, fill = "#4579fd"));
  }) 
  
  # Summary Text
  output$summary <- renderPrint({
    summary(road.regression)
  })
  
  # Check
  output$summary.check <- renderPrint({
    check(road.regression)
  })
  
  # Pearson
  output$summary.pears <- renderPrint({
    cor.test(road$rdensity, road$capita, method = "pearson")
  })
  
  # Shapiro-Wilk
  output$summary.shap1 <- renderPrint({
    shapiro.test(road$capita)
  })
  
  output$summary.shap2 <- renderPrint({
    shapiro.test(road$rdensity)
  })
  
  # Concurvity
  output$summary.conc <- renderPrint({
    concurvity(road.regression, full = TRUE)
  })
  
  # Dataset Table
  output$table <- DT::renderDataTable({
    DT::datatable(road)
  })
}