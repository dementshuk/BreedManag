SearchMale <- function() {

  require(shiny)
  require(readxl)
  require(readr)
  require(DT)

  options(shiny.maxRequestSize=2000*1024^2)
  app<-shinyApp(
    ###############################################UI#######################
    ui = fluidPage(
      titlePanel("UDG: Search Mating Recommendation"),

      sidebarLayout(
        sidebarPanel("",width=3,

          ####################Recommendation

          fileInput("file0", "Mating Recommendation",
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv")
          ),
          tags$hr(),
          checkboxInput("header0", "Header", TRUE),



          checkboxInput("leng", "Traduzir: Inglês -> Português", TRUE),

        textInput("tag", h3("Tag"),
                  value = ""),
        checkboxInput("customer", "Customer Recommendation", TRUE),

      actionButton("run", "RUN"),


      ####################rANKING

      fileInput("file1", "Ranking file",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      ),
      tags$hr(),
      checkboxInput("header1", "Header", TRUE),


      numericInput("pure", "Pures:", min = 0, max = 50, value = 10),

      textAreaInput(inputId = "caption", label="Put femeale tag here (sep: , )", width="100%", height="100px", value="", placeholder = "Placeholder"),
      actionButton("run2", "RUN")

        ),

        mainPanel(
          tabsetPanel(

            tabPanel("Log",

                              verbatimTextOutput("info"),
                     verbatimTextOutput("console_text")
                              ),


            tabPanel("Recommendation",
                     fixedRow(
                       column(3,tableOutput("viewpure")

                                ),
                                column(8,
                                       dataTableOutput("result") )
                              )


            ),

            tabPanel("Ranking",         dataTableOutput("viewall")),
            tabPanel("Input files",         tableOutput("viewR"),
                     tableOutput("viewrank"))

        )






      )
    ))

  ,

  server = function(input, output) {

    #####Recommendation

    output$info<- renderPrint(

      cat(" DATA INFORMATION
    *For Search, the Tag or ID should be write identical to the recommendation (
careful with upper and lower case letters )

    *Column name in data files:
    Recommendation:
    Tag_F/ID_F/Tag_M/ID_M/Recommendation/Order/Inb/Local/Males per Female/obs

    Ranking *if neeeded:
    Tag/ID/Order/Index

    To rank females write female's TAG separated by ; .
    * WARING!!!
    If recommendation is for customer farms, check customer option.
  ")
    )



     datasetInputR <- reactive({

      inFile0 <- input$file0
      if (is.null(inFile0))
        return(NULL)

      read_delim(inFile0$datapath,
                 ";", escape_double = FALSE, col_types = cols(ID_F = col_character(),
                                                              ID_M = col_character(),
                                                              Local = col_character(),
                                                              Recommendation = col_character(),
                                                              Tag_F = col_character(), Tag_M = col_character()),
                 locale = locale(decimal_mark = ".",
                                 grouping_mark = ","),trim_ws = TRUE,col_names = input$header0)




    })

     output$viewR <- renderTable({
       head(datasetInputR())
     })

     ######################RANK

     datasetInputRank <- reactive({

       inFile1 <- input$file1
       if (is.null(inFile1))
         return(NULL)
      read_delim(inFile1$datapath,
                          ";", escape_double = FALSE, col_types = cols(col_character(),
                                                                       col_character(), col_character(),
                                                                       col_number()), col_names = input$header1,locale = locale(decimal_mark = "."),
                          trim_ws = TRUE)

       })

     output$viewrank <- renderTable({
       head( datasetInputRank())
     })
#####################RR


     forout_reactive2 <- reactiveValues()
     observeEvent(input$run2, {
       pure<-input$pure
       if (is.null(pure))
         return(NULL)

       female<- as.character(unlist(strsplit(input$caption,",")))
       rankpure<- ranking(datasetInputRank(),female)
       output$console_text <- renderPrint( rankingcmd(datasetInputRank(),female)
       )


       output$viewall <- renderDT(
         rankpure, # data
         class = "display nowrap compact", # style
         filter = "top" # location of column filters
       )


         output$viewpure <- renderTable({
           rankpure[1:pure,c(4,1)]
         })


     })
#############FUN

     forout_reactive <- reactiveValues()
     observeEvent(input$run, {

       tag<-input$tag

       if (input$customer == TRUE & input$leng == FALSE){

         dataReco<- search10(datasetInputR(),tag)

         output$result <- renderDT(
           dataReco, # data
           class = "display nowrap compact", # style
           filter = "top" # location of column filters
         )



       }
       else if (input$customer == TRUE & input$leng ==TRUE){
         dataReco<- search11(datasetInputR(),tag)

         output$result <- renderDT(
           dataReco, # data
           class = "display nowrap compact", # style
           filter = "top" # location of column filters
         )



       }
     else if (input$customer == FALSE & input$leng ==TRUE){
       dataReco<- search01(datasetInputR(),tag)

       output$result <- renderDT(
         dataReco, # data
         class = "display nowrap compact", # style
         filter = "top" # location of column filters
       )
     }
   else if (input$customer == FALSE & input$leng == FALSE){
     dataReco<- search00(datasetInputR(),tag)
     output$result <- renderDT(
       dataReco, # data
       class = "display nowrap compact", # style
       filter = "top" # location of column filters
     )
  }


     })


     }
)



  #############Functioni
  search10<- function(rec,tag){
  rec<-rec[c("Tag_F","ID_F","Tag_M","ID_M","Recommendation","Inb","Local")]
  if (tag %in% rec$Tag_F){
    df <- rec[rec$Tag_F==tag,]}
  else{
    df<-rec
  }
  }

  search11<- function(rec,tag){
    rec<-rec[c("Tag_F","ID_F","Tag_M","ID_M","Recommendation","Inb","Local")]
    if (tag %in%rec$Tag_F){
      df <- rec[rec$Tag_F==tag,]}
    else{
      df<-rec
    }

    df$Recommendation[df$Recommendation=="Preferential"] <- "Preferência"
    df$Recommendation[df$Recommendation=="Accept"] <- "Aceito"
    df$Recommendation[df$Recommendation=="Not recommended"] <- "Não aceito"
    df<-df[,c(1,2,3,4,6,7,5)]
    colnames(df)<-c("Brinco_F","Tatu_F","Brico_M","Tatu_M","Endog","Local","Recomendação")
    return(df)
    rm(list = ls())
  }

  search00<- function(rec,tag){
    rec<-rec[c("Tag_F","ID_F","Tag_M","ID_M","Recommendation","Inb","Local","obs")]
    if (tag %in%rec$Tag_F){
      df <- rec[rec$Tag_F==tag,]}
    else{
      df<-rec
    }
  }

  search01<- function(rec,tag){
    rec<-rec[c("Tag_F","ID_F","Tag_M","ID_M","Recommendation","Inb","Local","obs")]
    if (tag %in% rec$Tag_F){
      df <- rec[rec$Tag_F==tag,]}
    else{
      df<-rec
    }

    df$Recommendation[df$Recommendation=="Preferential"] <- "Preferência"
    df$Recommendation[df$Recommendation=="Accept"] <- "Aceito"
    df$Recommendation[df$Recommendation=="Not recommended"] <- "Não Recomendado"

    df<-df[,c(1,2,3,4,6,7,8,5)]
    colnames(df)<-c("Brinco_F","Tatu_F","Brico_M","Tatu_M","Endog","Local","Recomendação","obs")
    return(df)
    rm(list = ls())
  }


   ranking<- function(rank,fem){

   rank<-rank[,1:4]
   colnames(rank)<-c("TAG_F","ID_F","Order","Index")
   rank$TAG_F<- as.character(rank$TAG_F)
   fem<-as.data.frame(fem)
   colnames(fem)[1]<-"TAG_F"
   fem$TAG_F<- as.character(fem$TAG_F)
   rank<-rank[order(rank$Index,decreasing=c(TRUE)), ]
   rank$Position<-1:nrow(rank)
   df<-rank[rank$TAG_F %in% fem$TAG_F,]
  if(nrow(df)==nrow(fem)){
  return(df)
    #rm(list = ls())
  }
  else{
    rm(list = ls())
  }


   }

   rankingcmd<- function(rank,fem){
     rank<-rank[,1:4]
     colnames(rank)<-c("TAG_F","ID_F","Order","Index")
     rank$TAG_F<- as.character(rank$TAG_F)
     fem<-as.data.frame(fem)
     colnames(fem)[1]<-"TAG_F"
     fem$TAG_F<- as.character(fem$TAG_F)
     rank<-rank[order(rank$Index,decreasing=c(TRUE)), ]
     rank$Position<-1:nrow(rank)
     df<-rank[rank$TAG_F %in% fem$TAG_F,]
     if(nrow(df)==nrow(fem)){
       cat("Ranking complete!
        ")
       rm(list = ls())
     }else{
       cat("Unidentified females
        ")
       print(fem$TAG_F[!(fem$TAG_F %in% rank$TAG_F)])
       rm(list = ls())
     }


   }

return(app)
}



