Inb <- function() {

  require(shiny)
  require(readxl)
  require(pedigreemm)
  require(readr)
  require(DT)

  options(shiny.maxRequestSize=2000*1024^2)
  app<-shinyApp(
    ###############################################UI#######################
    ui =
      navbarPage("Inbreeding Calculation",
                 tabPanel("Input",


                          fluidPage(
                            titlePanel("Input files"),
      sidebarLayout(
        sidebarPanel("", width=12,
                     fixedRow( #Title
                       column(10,"Pedigree file:Reordered pedigree: sire and dam information (ID/Sire/Dam/Original - .prn) - Default:CFC output (skipping 8 lines).  ||
                                  Animal File: Tag/ID"),
                       column(2,actionButton("run", "RUN")
                       )
                     ),
                     fixedRow(column(4,
                                     fileInput("file0", "Pedigree File",
                                               accept = c(
                                                 "text/csv",
                                                 "text/comma-separated-values,text/plain",
                                                 ".csv")
                                     ),
                                     tags$hr(),
                                     checkboxInput("header0", "Header", FALSE),
                                     numericInput("ng", "Skip lines (PED):",min = 0, 8 )
                     )),
                     fixedRow( #animal file
                       column(3,
                              fileInput("file1", "Animals File .xls (Tag/ID)",
                                        accept = c(
                                          "text/csv",
                                          "text/comma-separated-values,text/plain",
                                          ".csv")),
                              tags$hr(),
                              checkboxInput("header1","Header", TRUE)
                       ),

                       column(3,
                              fileInput("file2", "Animals File .txt (Tag/ID)",
                                        accept = c(
                                          "text/csv",
                                          "text/comma-separated-values,text/plain",
                                          ".csv")),
                              tags$hr(),
                              checkboxInput("header2","Header", TRUE)
                       ),
                       column(3,
                              checkboxGroupInput("checkGroup",
                                                 h6("File type (one option)"),
                                                 choices = list("XLSX/XLS" = "EX",
                                                                "TEXT" = "TX"),
                                                 selected = "EX"))

                     ),fixedRow( #animal file

                       column(3, textInput("save_name","File name","Inbreeding"),),
                       column(3, downloadButton("downloadData", "Download Inbreeding (.csv)") )#Download


                     )

        ),

        mainPanel(

        )
      )
    )),

    tabPanel("Output",
             fluidPage(
               titlePanel("Output file"),

               sidebarLayout(
                 sidebarPanel("", width=0

                 ),

                 mainPanel(


                   tabsetPanel(
                     tabPanel("Table",  dataTableOutput("viewR")),
                     tabPanel("Summary",  verbatimTextOutput("mean"))

                   )


                 )
               )
             )


    )

    ),

    ################################server##################

    server = function(input, output) {

      ped<- reactive({

        inFile0 <- input$file0
        if (is.null(inFile0))
          return(NULL)
        read_table2(inFile0$datapath,
                    col_names = input$header0, skip = input$ng)     })
      animal1 <- reactive({

        inFile1 <- input$file1
        if (is.null(inFile1))
          return(NULL)
        read_excel(inFile1$datapath, col_names = input$header1)
      })
      animal2 <- reactive({

        inFile2 <- input$file2
        if (is.null(inFile2))
          return(NULL)
        read_table2(inFile2$datapath, col_names = input$header2)
      })
      forout_reactive <- reactiveValues()
      observeEvent(input$run, {
        cat("Runing
            ")
        file<-input$checkGroup

        if (is.null(file))
          return(NULL)
        if (is.null(animal1()) & is.null(animal2()))
          return(NULL)
        if (is.null(ped()))
          return(NULL)
        if(file=="EX"){
          animal<-animal1()[,1:2]
          animais<-animal

          cat("
              Excel file!")
        }else{
          animal<-animal2()[,1:2]
          animais<-animal
          cat("
              Text file!")
        }

        Inb<-endog(animais,ped())


        output$viewR<- renderDT(
          Inb, # data
          class = "display nowrap compact", # style
          filter = "top" # location of column filters
        )
        forout_reactive$R<- Inb

      })
      datarank <- reactive({
        forout_reactive$R
      })
      output$mean<-renderPrint({
        Rank<-datarank()
        summary(Rank$Inb,na.rm=TRUE)
      })
      output$downloadData <- downloadHandler(

        filename = function() {
          paste(input$save_name, ".csv", sep = "")
        },
        content = function(file) {

          write.table(datarank(), file, row.names = FALSE, dec = ".", sep = ";", quote = FALSE,na = "")
        }
      )


    }
  )

  endog<- function(animals,pedigree){
    if(is.null(pedigree)|is.null(animals)){

      return(NULL)
      cat("
          No inbreeding calculatiobn. No all files.")
    }else{
      cat("
          Calculating...")
      animals<-animals[,1:2]

      colnames(animals)<-c("Tag","ID")

      pedigree<-pedigree[,1:4]

      colnames(pedigree)<-c("ID", "Sire", "Dam","Original")
      animals$ID<-as.character(animals$ID)
      pedigree$Original<-as.character(pedigree$Original)


      ped<-pedigree[,1:3]
      ped2<-pedigree(sire = ped$Sire,ped$Dam,label=ped$ID)
      pedigree$inb<- as.numeric(inbreeding(ped2))

      df<-pedigree[,c("Original","inb")]
      colnames(df)[2]<-"Inb"
      df<-merge(animals,df,by.x="ID",by.y="Original",all=T)
      df<-df[!(is.na(df$Tag)),]
      rm(ped2)

      df$Inb<-as.numeric(df$Inb)
      df$Inb<-df$Inb*100
      df$Inb<-round(df$Inb, digits=2)
      colorder<-colnames(df)
      colorder<-colorder[!(colorder %in% c("Tag","ID"))]
      Rank<-df[,c("Tag","ID",colorder)]
      return(Rank)
      cat("
          Inbreeding...ok!")
    }

  }
  return(app)
}
