MatingRecommendation <- function() {
  require(shiny)
  require(readxl)
  require(pedigreemm)
  require(readr)
  require(DT)
  require(plyr)
  options(shiny.maxRequestSize=3000*1024^2)
  app<-shinyApp(
    ###############################################UI#######################
    ui = fluidPage(
      titlePanel("Mating Recommendation"),

      sidebarLayout(
        sidebarPanel("Option Panel", width=3,

          ########LIMITES DE F



          numericInput("idf", "Ideal F limit (%):", ""),
          numericInput("rf", "Regular F limit (%):","" ),
          checkboxInput("port","Output Português",FALSE),
          ###################PEDIGREE FILE

          fileInput("file0", "Pedigree File",
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv")
          ),
          tags$hr(),
          numericInput("ng", "Skip lines (PED):",min = 0, 8 ),

          ####################MALE FILE
          fileInput("file1", "Male File",
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv")
          ),
          tags$hr(),
          checkboxInput("header1", "Header", TRUE),

          ####################FEMALE FILE
          fileInput("file2", "Female File",
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv")
          ),
          tags$hr(),
          checkboxInput("header2", "Header", TRUE),
          ######################matings/male
          fileInput("file4", "Matings/male file",
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv")
          ),
          tags$hr(),
          checkboxInput("header4", "Header", TRUE),

          numericInput("limit", "Matings/Male Limit", ""),
          checkboxInput("udg","Use mating limit",FALSE),

          #numericInput("ng", "Generation limit:",min = 0, 5 ),
          #checkboxInput("lg","Use Generation limit",FALSE),

          fileInput("file5", "Index file",
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv")
          ),

          checkboxInput("uvg","Progeny Index",FALSE),

          actionButton("run", "RUN"),




          ########### DOWNLOAD

          textInput("save_name","File name","Recommendation"),



          downloadButton("downloadData", "Download File")




        ),

        mainPanel(

          tabsetPanel(
            tabPanel("Log",  verbatimTextOutput("info"),
                     verbatimTextOutput("console_text")),
            tabPanel("Input files",         tableOutput("viewP"),
                     tableOutput("viewM"),
                     tableOutput("viewF"),
                     tableOutput("viewLi"),
                     tableOutput("viewvg")),
            tabPanel("Output",  dataTableOutput("viewR")),
            tabPanel("Summary",p("Males/Female"),  verbatimTextOutput("ML"),plotOutput(outputId = "distPlot"),p("Inbreeding Coefficient"),
                     verbatimTextOutput("MF"))
          )





        )
      )
    ),

################################server##################

    server = function(input, output) {


      ##########################################Input data#################################
      ##ped
      datasetInputPED <- reactive({

        inFile0 <- input$file0
        if (is.null(inFile0))
          return(NULL)
       read_table2(inFile0$datapath,
                                 col_names = FALSE, skip = input$ng, col_types = cols(`X4` = col_character()))
      })


      ##male
      datasetInputMA <- reactive({

        inFile1 <- input$file1
        if (is.null(inFile1))
          return(NULL)
        read_excel(inFile1$datapath, col_names = input$header1)


      })

      ##female

      datasetInputFEM <- reactive({

        inFile2 <- input$file2
        if (is.null(inFile2))
          return(NULL)
        read_excel(inFile2$datapath, col_names = input$header2)


      })

      ##limit

      datasetInputLIMIT <- reactive({

        inFile4 <- input$file4
        if (is.null(inFile4))
          return(NULL)
        read_excel(inFile4$datapath, col_names = input$header4)


      })
      ##### VG

      vg <- reactive({

        inFile5 <- input$file5
        if (is.null(inFile5))
          return(NULL)
          read_delim(inFile5$datapath,
                   ";", escape_double = FALSE, col_types = cols(Tag = col_character()),
                   locale = locale(), trim_ws = TRUE,col_names =TRUE)

      })

      ##########################################View data#################################

      output$info<- renderPrint(

        cat(" DATA INFORMATION
    *Ideal F should be less than regular F (Adjust before input data):
    1%-100%

    *Column order in data files:

    Reordered pedigree: sire and dam information (ID/Sire/Dam/Original - .prn) - Default:CFC output - No header!
    Male File: Tag/ID/Local (.xls or .xlsx)
    Female file: Tag/ID/Parity Order (.xls or .xlsx)
    Matings/Male Limit: Tag/ID/Numb. of matings *to use this file check: Use mating limit

    To calculate progeny index from each mating you need add a index file with all ranked parents in Index file (from IndexSM)
    and check: Progeny index.
    Tag/ID/Obs/Index (.csv)
  ")
      )

      output$viewP <- renderTable({
        head(datasetInputPED())
      })
      output$viewM <- renderTable({
        head(datasetInputMA())
      })

      output$viewF <- renderTable({

        head(datasetInputFEM())
      })
      output$viewLi <- renderTable({

        head(datasetInputLIMIT())
      })
      output$viewvg <- renderTable({

        head(vg())
      })




      #### func reactive
      forout_reactive <- reactiveValues()
      observeEvent(input$run, {

        rf<-input$rf
        ri <-input$idf

        PED<-datasetInputPED()







        if (input$udg == TRUE){
          ml <-input$limit
          output$console_text <- renderPrint( mating_cmd(PED,datasetInputMA(),datasetInputFEM(),rf,ri)
          )
          dataReco<- mating2(PED,datasetInputMA(),datasetInputFEM(),rf,ri,ml,datasetInputLIMIT())

           if(input$port==TRUE){

            dataReco$Recommendation[dataReco$Recommendation == "Accept"]<-"Aceito"
            dataReco$Recommendation[dataReco$Recommendation == "Not recommended"]<-"Não Recomendado"
            dataReco$Recommendation[dataReco$Recommendation == "Preferential"]<-"Preferencial"
            dataReco$obs[dataReco$obs == "Limited"]<-"Limite de Cob."
            dataReco$obs[dataReco$obs == "In use"]<-"Em uso"
          }

          output$viewR<- renderDT(
            dataReco, # data
            class = "display nowrap compact", # style
            filter = "top" # location of column filters
          )

          forout_reactive$R<-dataReco


        }else{
        output$console_text <- renderPrint( mating_cmd(PED,datasetInputMA(),datasetInputFEM(),rf,ri)
        )
        dataReco<- mating(PED,datasetInputMA(),datasetInputFEM(),rf,ri)
        output$viewR<- renderDT(
          dataReco, # data
          class = "display nowrap compact", # style
          filter = "top" # location of column filters
        )

        forout_reactive$R<-dataReco

if(input$port==TRUE){

  dataReco$Recommendation[dataReco$Recommendation == "Accept"]<-"Aceito"
  dataReco$Recommendation[dataReco$Recommendation == "Not recommended"]<-"Não Recomendado"
  dataReco$Recommendation[dataReco$Recommendation == "Preferential"]<-"Preferencial"


}else{
  dataReco<-dataReco
}
if(input$uvg == TRUE){
  if (is.null(vg()))
    return(NULL)
  vg<-vg()
  vg<-vg[,c(1,4)]
  colnames(vg)[2]<-"im"
  df<-merge(dataReco,vg,by.x="Tag_F",by.y="Tag",all=T)
  colnames(vg)[2]<-"ip"
  df2<-merge(df,vg,by.x="Tag_M",by.y="Tag",all=T)
  df2<-df2[!(is.na(df2$Inb)),]
  df2$Index<-(df2$im+df2$ip)/2
  df2$Index<-round(df2$Index, digits = 2)
  df2$im<-NULL
  df2$ip<-NULL
  df2<-df2[,c(2,3,1,4,5:ncol(df2))]
  dataReco<-df2
}else{
  dataReco<-dataReco
}

        output$viewR<- renderDT(
          dataReco, # data
          class = "display nowrap compact", # style
          filter = "top" # location of column filters
        )
        forout_reactive$R<-dataReco
        }

      })

      dataReco <- reactive({
        forout_reactive$R
      })


      output$ML<-renderPrint({
        Ran<-dataReco()
        summary(Ran$`Males/Female`)
      })
      output$distPlot <- renderPlot({
      Ran<-dataReco()
      R<-Ran[!duplicated(Ran$Tag_F),c("Tag_F","Males/Female")]
      barplot(R$`Males/Female`,xlab = "Females",ylab = "Number of acceptable males")
      })
      output$MF<-renderPrint({
        Ran<-dataReco()
        summary(Ran$Inb)
      })
      ################ DOWNLOAD 1
      output$downloadData <- downloadHandler(

        filename = function() {
          paste(input$save_name, ".csv", sep = "")
        },
        content = function(file) {
          write.table(dataReco(), file, row.names = FALSE, dec = ".", sep = ";", quote = FALSE)
        }
      )



    }
  )

  ##################### FUNCTION

  mating <- function(pedigree,machos,femeas,Flimit_regular,Flimit_good)
  {
    time_1 = Sys.time()
    colnames(femeas)[1:3]<- c("BRF","TATF","Ciclo")
    colnames(machos)[1:3]<- c("BRM","TATM","Local")
    colnames(pedigree)[1:4]<-c("ID", "Sire", "Dam", "Original" )
    femeas<-femeas[,1:3]
    machos<-machos[,1:3]
    pedigree<-pedigree[,1:4]
    males_NI<-machos[!(machos$TATM %in% pedigree$Original),]
    females_NI<-femeas[!(femeas$TATF %in% pedigree$Original),]

  rm(males_NI,females_NI)
  cod<-pedigree[,c(1,4)]
  machos<-merge(machos, cod, by.x="TATM",by.y="Original")
  femeas<-merge(femeas, cod, by.x="TATF",by.y="Original")

      ID<-1:(nrow(femeas)*nrow(machos))
      ID<- paste(ID,"00000001")
      ID<- gsub(' ', '', ID)
      sim<-as.data.frame(ID)
      rm(ID)
      sim$Dam<-rep(femeas$ID, each = nrow(machos))
      sim$Sire<-machos$ID

      pedigree <- pedigree[,1:3]
      pedigree<-pedigree[order(pedigree$ID,decreasing=c(FALSE)), ]
      pedigree$ID<-as.character(pedigree$ID)
      pedigree$Dam<-as.character(pedigree$Dam)
      pedigree$Sire<-as.character(pedigree$Sire)
      ped<-rbind(pedigree,sim)


      ped<-ped[,c("ID","Sire","Dam")]


      ped2<-pedigree(sire = ped$Sire,ped$Dam,label=ped$ID)


      ped$inb<-  as.numeric(inbreeding(ped2))

      list<- ped[ped$ID %in% sim$ID,]
      rm(ped2)
      list$ID<- NULL

      list$inb<-list$inb*100
      list$inb<-round(list$inb, digits=2)

      list$Recomendacao[list$inb<=Flimit_good] <- "Preferential"
      list$Recomendacao[list$inb> Flimit_good & list$inb <= Flimit_regular] <- "Accept"
      list$Recomendacao[list$inb>Flimit_regular] <- "Not recommended"


      idm<-machos[,1:4]
      idf<-femeas[,1:4]
      colnames(idm)[4]<-"Sire"
      colnames(idf)[4]<-"Dam"
      df1<-merge(list,idf,by="Dam")
      df2<-merge(df1,idm,by="Sire")
      df3<-df2[,c("BRF","TATF","BRM","TATM","Recomendacao","Ciclo","inb","Local")]
      colnames(df3)<-c("BRF","Dam","BRM","Sire","Recomendacao","Ciclo","inb","Local")
      df3$ID<-paste(df3$Dam,df3$Sire)
      df3<-df3[!duplicated(df3$ID),]
      df3$ID<-NULL

      df4<-table(df3$Dam[df3$inb <= Flimit_regular])
      df4 <- as.data.frame(df4)
      colnames(df4)[1]<-c("Var1")
      df5<-merge(df3,df4,by.x="Dam",by.y ="Var1",all=T)

      df6<-table(df3$Sire[df3$inb <= Flimit_regular])
      df6 <- as.data.frame(df6)
      colnames(df6)[1]<-c("Var1")
      df7<-merge(df5,df6,by.x="Sire",by.y ="Var1",all=T)
      df5<-df7

      if(ncol(df4)==1){
        colnames(df5)<- c("ID_F","Tag_F","Tag_M","ID_M","Recommendation","Order","Inb","Local")
        df5<-df5[,c("Tag_F","ID_F","Tag_M","ID_M","Recommendation","Order","Inb","Local")]
        df5<-df5[order(df5$Tag_F, df5$Inb,decreasing=c(FALSE,FALSE)), ]

      }else{
        colnames(df5)<- c("ID_M","ID_F","Tag_F","Tag_M","Recommendation","Order","Inb","Local", "Males/Female","Females/Male")
        df5$'Males/Female'[is.na(df5$'Males/Female')]<-0
        df5$'Females/Male'[is.na(df5$'Females/Male')]<-0
        df5<-df5[,c("Tag_F","ID_F","Tag_M","ID_M","Recommendation","Order","Inb","Local", "Males/Female","Females/Male")]
        df5<-df5[order(df5$Tag_F, df5$Inb,decreasing=c(FALSE,FALSE)), ]
      }

      time_2 = Sys.time()

      return(df5)
      rm(list = ls())

  }

  mating_cmd <- function(pedigree,machos,femeas,Flimit_regular,Flimit_good)
  {
    time_1 = Sys.time()
    colnames(femeas)[1:3]<- c("BRF","TATF","Ciclo")
    colnames(machos)[1:3]<- c("BRM","TATM","Local")
    colnames(pedigree)[1:4]<-c("ID", "Sire", "Dam" ,"Original")
    femeas<-femeas[,1:3]
    machos<-machos[,1:3]
    pedigree<-pedigree[,1:4]
    males_NI<-machos[!(machos$TATM %in% pedigree$Original),]
    females_NI<-femeas[!(femeas$TATF %in% pedigree$Original),]

    fm<-dim(males_NI)
    ff<-dim(females_NI)
    if(ff[1] != 0 | fm[1] != 0){
      cat("
Correct IDs!")
      cat("
      ")
      cat("Males:","
  ")
      print(males_NI[,1:2])
      cat("females:","
  ")
      print(females_NI[,1:2])
    } else {
      cat("IDs ok!")
      cat("

      ","Continuing...")
      rm(males_NI,females_NI,fm,ff)}

      rm(list = ls())

  }




mating2 <- function(pedigree,machos,femeas,Flimit_regular,Flimit_good,malelimit,datalimit)
{
  time_1 = Sys.time()
  colnames(femeas)[1:3]<- c("BRF","TATF","Ciclo")
  colnames(machos)[1:3]<- c("BRM","TATM","Local")
  colnames(pedigree)[1:4]<-c("ID", "Sire", "Dam","Original" )
  femeas<-femeas[,1:3]
  machos<-machos[,1:3]
  pedigree<-pedigree[,1:4]
  males_NI<-machos[!(machos$TATM %in% pedigree$Original),]
  females_NI<-femeas[!(femeas$TATF %in% pedigree$Original),]
  colnames(datalimit)[1:3]<- c("Tag_M","ID_M","cob")
  datalimit<-datalimit[,1:3]
  datalimit$obs<-"In use"
  datalimit$obs[datalimit$cob>=malelimit]<-"Limited"
  datalimit<-datalimit[,c(2,4)]
  cod<-pedigree[,c(1,4)]
  fm<-dim(males_NI)
  ff<-dim(females_NI)
  rm(males_NI,females_NI,fm,ff)

  machos<-merge(machos, cod, by.x="TATM",by.y="Original")
  femeas<-merge(femeas, cod, by.x="TATF",by.y="Original")

  ID<-1:(nrow(femeas)*nrow(machos))
  ID<- paste(ID,"SIM")
  ID<- gsub(' ', '', ID)
  sim<-as.data.frame(ID)
  rm(ID)
  sim$Dam<-rep(femeas$ID, each = nrow(machos))
  sim$Sire<-machos$ID

  pedigree <- pedigree[,1:3]
  pedigree<-pedigree[order(pedigree$ID,decreasing=c(FALSE)), ]
  pedigree$ID<-as.character(pedigree$ID)
  pedigree$Dam<-as.character(pedigree$Dam)
  pedigree$Sire<-as.character(pedigree$Sire)
  ped<-rbind(pedigree,sim)


  ped<-ped[,c("ID","Sire","Dam")]


  ped2<-pedigree(sire = ped$Sire,ped$Dam,label=ped$ID)


  ped$inb<-  as.numeric(inbreeding(ped2))

  list<- ped[ped$ID %in% sim$ID,]
  rm(ped2)
  list$ID<- NULL

  list$inb<-list$inb*100
  list$inb<-round(list$inb, digits=2)

  list$Recomendacao<-NULL
  list$Recomendacao[list$inb<=Flimit_good] <- "Preferential"
  list$Recomendacao[list$inb> Flimit_good & list$inb <= Flimit_regular] <- "Accept"
  list$Recomendacao[list$inb>Flimit_regular] <- "Not recommended"


  idm<-machos[,1:4]
  idf<-femeas[,1:4]
  colnames(idm)[4]<-"Sire"
  colnames(idf)[4]<-"Dam"
  df1<-merge(list,idf,by="Dam")
  df2<-merge(df1,idm,by="Sire")
  df2<-df2[order(df2$BRF, df2$inb,decreasing=c(FALSE,FALSE)), ]
  df3<-df2[,c("BRF","TATF","BRM","TATM","Recomendacao","Ciclo","inb","Local")]
  colnames(df3)<-c("BRF","Dam","BRM","Sire","Recomendacao","Ciclo","inb","Local")

    df3$ID<-paste(df3$Dam,df3$Sire)
    df3<-df3[!duplicated(df3$ID),]
    df3$ID<-NULL

    df4<-table(df3$Dam[df3$inb <= Flimit_regular])
    df4 <- as.data.frame(df4)
    colnames(df4)[1]<-c("Var1")
    df5<-merge(df3,df4,by.x="Dam",by.y ="Var1",all=T)

    df6<-table(df3$Sire[df3$inb <= Flimit_regular])
    df6 <- as.data.frame(df6)
    colnames(df6)[1]<-c("Var1")
    df7<-merge(df5,df6,by.x="Sire",by.y ="Var1",all=T)
    df5<-df7

    if(ncol(df4)==1){
      colnames(df5)<- c("ID_F","Tag_F","Tag_M","ID_M","Recommendation","Order","Inb","Local")
      df5<-df5[,c("Tag_F","ID_F","Tag_M","ID_M","Recommendation","Order","Inb","Local")]
      df6<-merge(df5,datalimit,by="ID_M",all=T)
      df6<-df6[!(is.na(df6$Tag_F)),]
      df5<-df6[,c("Tag_F","ID_F","Tag_M","ID_M","Recommendation","Order","Inb","Local","obs")]
      df5<-df5[order(df5$Tag_F, df5$Inb,decreasing=c(FALSE,FALSE)), ]
    }else{
      colnames(df5)<- c("ID_M","ID_F","Tag_F","Tag_M","Recommendation","Order","Inb","Local", "Males/Female","Females/Male")
      df5$'Males/Female'[is.na(df5$'Males/Female')]<-0
      df5$'Females/Male'[is.na(df5$'Females/Male')]<-0
      df5<-df5[,c("Tag_F","ID_F","Tag_M","ID_M","Recommendation","Order","Inb","Local", "Males/Female","Females/Male")]
      df6<-merge(df5,datalimit,by="ID_M",all=T)
      df6<-df6[!(is.na(df6$Tag_F)),]
      df5<-df6[,c("Tag_F","ID_F","Tag_M","ID_M","Recommendation","Order","Inb","Local", "Males/Female","Females/Male","obs")]
      df5<-df5[order(df5$Tag_F, df5$Inb,decreasing=c(FALSE,FALSE)), ]
    }

    time_2 = Sys.time()
 cat("Ok")
    return(df5)
    rm(list = ls())

}
return(app)
}

