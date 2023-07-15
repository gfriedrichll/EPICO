#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(tidyverse)
library(broom)
library(kableExtra)
library(readxl)
library(DT)
library(plotly)

#Historial de solicitudes
#datos <- read_excel("C:/Users/gaga1/OneDrive/Documents/MEPI/EPICO/Control de calidad/Solicitudes_CC_4_14_2021/Historial_Solicitudes.xlsx")
datos <- read_excel("C:/Users/gaga1/OneDrive/Documents/MEPI/EPICO/Control de calidad/Respuesta_Solicitudes_CC_20_08_2021/Historial_Solicitudes_29_08_2021.xlsx")
datos$Tipo <- tolower(datos$Tipo)

#Número de pacientes
data=read_excel('C:/Users/gaga1/OneDrive/Documents/MEPI/EPICO/Control de calidad/Datos_Todos_29_08_2021/EPICOCOLOMBIA_DATA_2021-08-30_0304.xlsx')
#data=data[,! names(data) %in% ("redcap_data_access_group") ]


ui <- dashboardPage(
    dashboardHeader(title = "Control Calidad"),
    dashboardSidebar(
        menuItem("General", tabName = "general", icon = icon("dashboard")),
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        
        selectInput("v_hospital","hospital",choices = datos %>% 
                        select(hospital) %>% 
                        distinct() %>% 
                        arrange(hospital)),
        menuItem("Historial Solicitudes", tabName = "historial", icon = icon("dashboard"))
    ),
    dashboardBody(
        tabItems(
        tabItem("general",
                fluidRow(
                    
                    # Número de missings
                    valueBoxOutput("g_NAs"),
                    
                    # Número de queries por tipo
                    valueBoxOutput("g_queries"),
                    
                    # Estado == Finalizado con éxito
                    valueBoxOutput("g_fin_exito"),
                    
                    # Estado == Finalizado sin éxtio
                    valueBoxOutput("g_sin_exito"),
                    
                    # Estado == solicitud enviada o en proceso
                    valueBoxOutput("g_en_proceso"),
                    
                    #Records en revisión
                    valueBoxOutput("g_revision")
                ),
                
                fluidRow(
                    box(width=12,plotOutput("solicitudes_todos"))
                ),
                
                fluidRow(
                    box(width=6,tableOutput("table_tipo_todos")),box(width=6,tableOutput("table_missing_todos"))
                ),
        ),
        tabItem("dashboard",
                    fluidRow(
            
                    # Número de missings
                    valueBoxOutput("v_NAs"),
                    
                    # Número de queries por tipo
                    valueBoxOutput("v_queries"),
                    
                    # Estado == Finalizado con éxito
                    valueBoxOutput("v_fin_exito"),
                    
                    # Estado == Finalizado sin éxtio
                    valueBoxOutput("v_sin_exito"),
                    
                    # Estado == solicitud enviada o en proceso
                    valueBoxOutput("v_en_proceso"),
                    
                    #Records en revisión
                    valueBoxOutput("v_revision")
                    ),

                    fluidRow(
                        box(width=12,plotOutput("missing_variable_plot"))
                    ),
                    fluidRow(
                        box(width=6, tableOutput("table_missing_variable")),box(width=6, tableOutput("table_tipo"))
                    ),
            ),
            tabItem("historial",
                        fluidPage(
                            h1("Historial de solicitudes"),
                            dataTableOutput("data_table_hist"))
            ) 
            
        )    
    )
)





server <- function(input, output) { 
    
    ##### General
    
    output$g_NAs <- renderValueBox({
        valueBox(datos %>% filter(Estado != "finalizado con éxito") %>%filter(Tipo == "missing")  %>% count(Tipo) %>% select(n), "# NAs", icon = icon("calculator"),
                 color = "purple"
        )
    })
    
    output$g_queries <- renderValueBox({
        valueBox(dim(datos)[1], "# Queries", icon = icon("exclamation-triangle"),
                 color = "maroon"
        )
    })
    
    output$g_fin_exito <- renderValueBox({
        valueBox(datos %>% filter(Estado == "finalizado con éxito") %>% count(Estado) %>% select(n), "# Solicitudes finalizadas con éxito", icon = icon("check-circle"),
                 color = "green"
        )
    })
    
    output$g_sin_exito <- renderValueBox({
        valueBox(datos %>% filter(Estado == "finalizado sin solución") %>% count(Estado) %>% select(n), "# Solicitudes finalizadas sin solución", icon = icon("window-close"),
                 color = "red"
        )
    })
    
    
    
    output$g_en_proceso <- renderValueBox({
        valueBox(dim(datos %>%filter(Estado == "seguimiento" | Estado == "solicitud enviada"))[1], "# Solicitudes en proceso", icon = icon("angle-double-right"),
                 color = "light-blue"
        )
    })
    
    output$g_revision <- renderValueBox({
        valueBox(datos %>% filter(Estado == "solicitud enviada") %>% summarise(incompletos = n_distinct(record)) %>% select(incompletos),"# Records en revisión de un total de 333" , icon = icon("user-cog"),
                 color = "yellow"
        )
    })
    
    output$solicitudes_todos <- renderPlot({
        datos %>%
            select(hospital, Estado, Variable, Tipo) %>%
            group_by(hospital, Estado)%>%
            summarise(count = n()) %>%
            arrange(-count)%>%
            ggplot(aes(x=hospital, y=count, fill=Estado))+
            geom_bar(stat="identity", alpha=.6, width=.6) +
            scale_fill_manual(values = c("chartreuse3","red3","lightskyblue2","dodgerblue2"))+
            coord_flip() +
            theme(legend.position="bottom")+
            ylab("# de queries por hospital")+
            xlab("Hospitales")
    })
    
    output$table_tipo_todos <- function(){
        datos %>% 
            count(Tipo,Estado,sort=F) %>%
            kable() %>%
            kable_styling() %>%
            scroll_box(height = "400px",width="700px")
    }
    
    output$pacientes_todos <- renderPlot({
        data %>%
            select(hospital, record) %>%
            group_by(hospital)%>%
            summarise(count=n_distinct(record)) %>%
            arrange(-count)%>%
            mutate(hospital = fct_reorder(hospital, count)) %>%
            ggplot(aes(x=hospital, y=count, fill=hospital))+
            geom_bar(stat="identity", alpha=.6, width=.7) +
            scale_fill_viridis(discrete = T, option = "E") +
            coord_flip() +
            theme(legend.position="none")+
            ylab("# de pacientes por hospital")+
            xlab("Hospitales")
    })
    
    output$table_missing_todos <- function(){
        datos %>%
            select(hospital, Estado, Variable, Tipo) %>%
            filter(Variable != "size_percentile" & Variable != "weight_percentile") %>%
            filter(Estado != "finalizado con éxito") %>%
            filter(Tipo == "missing")%>%
            group_by(Variable)%>%
            summarise(count = n()) %>%
            arrange(-count) %>%
            kable() %>%
            kable_styling() %>%
            scroll_box(height = "400px",width="700px")
    }
    
    
    ##### Por hospital
    
    output$missing_variable_plot <- renderPlot({
        datos %>%
            select(hospital, Estado, Variable, Tipo) %>%
            filter(Variable != "size_percentile" & Variable != "weight_percentile") %>%
            filter(Estado != "finalizado con éxito") %>%
            filter(Tipo == "missing")%>%
            filter(hospital == input$v_hospital)%>%
            group_by(Variable)%>%
            summarise(count = n()) %>%
            arrange(-count) %>%
            slice(1:10) %>%
            mutate(Variable = fct_reorder(Variable, count)) %>%
            ggplot(aes(x=Variable, y=count, fill=count))+
            geom_bar(stat="identity", alpha=.6, width=.4) +
            coord_flip() +
            theme(legend.position="none")+
            ylab("# de missing por variable")+
            xlab("Variables") 
            
    })
    
    output$table_tipo <- function(){
        datos %>% 
            filter(hospital == input$v_hospital) %>%
            count(Tipo,Estado,sort=F) %>%
            kable() %>%
            kable_styling() %>%
            scroll_box(height = "400px",width="700px")
    
    }
    
    output$table_missing_variable <- function(){
        datos %>%
            select(hospital, Estado, Variable, Tipo) %>%
            filter(Variable != "size_percentile" & Variable != "weight_percentile") %>%
            filter(Estado != "finalizado con éxito") %>%
            filter(Tipo == "missing")%>%
            filter(hospital == input$v_hospital)%>%
            group_by(Variable)%>%
            summarise(count = n()) %>%
            arrange(-count) %>%
            kable() %>%
            kable_styling() %>%
            scroll_box(height = "400px",width="700px")
        
    }

    
    output$v_NAs <- renderValueBox({
        valueBox(datos %>%  filter(Estado != "finalizado con éxito") %>% filter(Tipo == "missing") %>% filter(hospital == input$v_hospital) %>% count(Tipo) %>% select(n), "# NAs", icon = icon("calculator"),
            color = "purple"
        )
    })
    
    output$v_queries <- renderValueBox({
        valueBox(dim(datos %>% filter(hospital == input$v_hospital))[1], "# Queries", icon = icon("exclamation-triangle"),
            color = "maroon"
        )
    })
    
    output$v_fin_exito <- renderValueBox({
        valueBox(datos %>% filter(hospital == input$v_hospital) %>% filter(Estado == "finalizado con éxito") %>% count(Estado) %>% select(n), "# Solicitudes finalizadas con éxito", icon = icon("check-circle"),
                 color = "green"
        )
    })
    
    output$v_sin_exito <- renderValueBox({
        valueBox(datos %>% filter(hospital == input$v_hospital) %>% filter(Estado == "finalizado sin solución") %>% count(Estado) %>% select(n), "# Solicitudes finalizadas sin solución", icon = icon("window-close"),
                 color = "red"
        )
    })
    

    
    output$v_en_proceso <- renderValueBox({
        valueBox(dim(datos %>% filter(hospital==input$v_hospital)%>%filter(Estado == "seguimiento" | Estado == "solicitud enviada"))[1], "# Solicitudes en proceso", icon = icon("angle-double-right"),
                 color = "light-blue"
        )
    })
    
    output$v_revision <- renderValueBox({
        valueBox(datos %>%  filter(hospital==input$v_hospital) %>% filter(Estado == "solicitud enviada" | Estado =="seguimiento") %>% summarise(incompletos = n_distinct(record)) %>% select(incompletos),paste0("# Records en revisión de un total de:",dim(data %>% filter(hospital == input$v_hospital))[1]), icon = icon("user-cog"),
            color = "yellow"
        )
    })
    
    #### Historial
    
    output$data_table_hist <- renderDataTable(datos)
}

shinyApp(ui, server)
