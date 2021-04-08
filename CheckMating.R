CheckMating <- function() {

  require(shiny)
  require(readxl)
  require(readr)
  require(DT)

  options(shiny.maxRequestSize=2000*1024^2)
  app<-shinyApp(
    ###############################################UI#######################
    ui = fluidPage(
      titlePanel("Check Mating"),

      sidebarLayout(
        sidebarPanel(

          ####################Recommendation

          fileInput("file0", "Mating Recommendation",
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv")
          ),
          tags$hr(),
          checkboxInput("header0", "Header", TRUE),

          numericInput("F", "F limit:", min = 0, max = 50, value = 14.5),



          ####################MATing

          fileInput("file1", "Matings file",
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv")
          ),
          tags$hr(),
          checkboxInput("header1", "Header", TRUE),



          fileInput("file2", "Males File",
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv")),
          tags$hr(),
          checkboxInput("header2","Header", TRUE),

          checkboxGroupInput("checkGroup",
                             h3("Male Identify (one option)"),
                             choices = list("Tag" = "BRM",
                                            "ID" = "TATM"),
                             selected = "BRM"),
          textInput("breed_ggmother","GGmother breed","DB30"),
          checkboxInput("obs","obs column", FALSE),
          actionButton("run", "RUN"),
          textInput("save_name","File name","Check_Matings"),




          downloadButton("downloadData", "Download")

        ),

        mainPanel(
          tabsetPanel(
            tabPanel("Help",  verbatimTextOutput("info")),
            tabPanel("Table",  dataTableOutput("viewR")),
            tabPanel("input",  tableOutput("mat"),tableOutput("male"),tableOutput("rec"))

          )

                    )
      ))

    ,

    server = function(input, output) {

      mat <- reactive({

        inFile1 <- input$file1
        if (is.null(inFile1))
          return(NULL)
        read_excel(inFile1$datapath, col_names = input$header1)
      })

      male<- reactive({

        inFile2 <- input$file2
        if (is.null(inFile2))
          return(NULL)
        read_excel(inFile2$datapath, col_names = input$header2)
      })

      rec <- reactive({

        inFile0 <- input$file0
        if (is.null(inFile0))
          return(NULL)

        read_delim(inFile0$datapath,
                   ";", escape_double = FALSE, col_types = cols(ID_F = col_character(),
                                                                ID_M = col_character(),
                                                               Recommendation = col_character(),
                                                                Tag_F = col_character(), Tag_M = col_character()),
                   locale = locale(decimal_mark = ".",
                                   grouping_mark = ","),trim_ws = TRUE,col_names = input$header0)

      })

      output$info<- renderPrint(

        cat(" DATA INFORMATION
    *Ideal F should be the regular F :    1%-100%

    *Column order in data files:

    Reccomendation: Tag_F/ID_F/Tag_M/ID_M/Recommendation/Order/Inb (same column names and order - exit of Mating Recommendation)
    Male File: Tag/ID/Breed (.xls or .xlsx) (same order) *if possible include all farm males (different breed)
    Mating file: Tag_F/ID_F/Order/Mating date/Male1/Male2/Male3/Prev (same order)

    Male Identify in the mating file (Tag or ID)


  ")
      )


      forout_reactive <- reactiveValues()
      observeEvent(input$run, {
        order<-input$checkGroup
        breed_ggmother <- input$breed_ggmother
        Flimit<-input$F
        obs<-input$obs
        if (length(order)==2)
          return(NULL)

        if (is.null(order))
          return(NULL)

        if (is.null(breed_ggmother))
          return(NULL)

        if (is.null(Flimit))
          return(NULL)

        if (is.null(male()))
          return(NULL)

        if (is.null(rec()))
          return(NULL)

        if (is.null(mat()))
          return(NULL)

         date <- checkmating2(rec(),mat(),male(),order,breed_ggmother,Flimit,obs)


         output$viewR<- renderDT(
           date, # data
           class = "display nowrap compact", # style
           filter = "top" # location of column filters
         )


      forout_reactive$R<-date

              })

      df2 <- reactive({
        forout_reactive$R
      })

      output$mat <- renderTable({
        head(mat())
      })
      output$male <- renderTable({
        head(male())
      })
      output$rec <- renderTable({
        head(rec())
      })

      output$downloadData <- downloadHandler(

        filename = function() {
          paste(input$save_name, ".csv", sep = "")
        },
        content = function(file) {
          write.table(df2(), file, row.names = FALSE, dec = ".", sep = ";", quote = FALSE)
        })


    }


  )

######################

  checkmating2 <- function(recom,acasal,machos,order,breed_ggmother,Flimit,obs) {
    cat("Runing...")

    if(obs==FALSE){
      recom<-recom[,c(1:5,7)]
      colnames(recom)[1:6]<-c("BRF","TATF","BRM","TATM","REC","F")
      colnames(acasal)[1:8]<-c("BRF","TATF","Cic","Cob","macho1","macho2","macho3", "Prev")
      acasal<-acasal[,1:8]}else{
        df<-recom[,c(1:5,7)]
        df$obs<-recom$obs
        recom<-df
        colnames(recom)[1:7]<-c("BRF","TATF","BRM","TATM","REC","F","obs")
        dfobs<-recom[!(duplicated(recom$TATM)),c(order,"obs")]
        colnames(acasal)[1:8]<-c("BRF","TATF","Cic","Cob","macho1","macho2","macho3", "Prev")
        acasal<-acasal[,1:8]
      }
    colnames(machos)[1:3]<-c("BRM","TATM","Raça")
    machos<-machos[,1:3]
    machos <- machos[,c(order,"Raça")]
    colnames(machos)[1]<-"order"
    machos$macho1<- machos$order
    machos$macho2<- machos$order
    machos$macho3<- machos$order
    machos$Raça1<- machos$Raça
    machos$Raça2<- machos$Raça
    machos$Raça3<- machos$Raça

    b1<-merge(machos[,c("macho1","Raça1")],acasal,by="macho1",all=T)
    b2<-merge(machos[,c("macho2","Raça2")],b1,by="macho2",all=T)
    b3<-merge(machos[,c("macho3","Raça3")],b2,by="macho3",all=T)
    b3 <-b3[!(is.na(b3$BRF)),]
    b3$homoz <- "ok"
    b3$macho2[is.na(b3$macho2)]<-"0"
    b3$macho3[is.na(b3$macho3)]<-"0"
    if (nrow(b3)>0){
      for (i in 1:nrow(b3)){
        if (b3$macho1[i] != b3$macho2[i] & b3$macho2[i] != "0" )
        {b3$homoz [i] = "without homozygosis"}
        if (b3$macho1[i] != b3$macho3[i] & b3$macho3[i] != "0" )
        {b3$homoz [i] = "without homozygosis"}
        if (b3$macho2[i] != b3$macho3[i] & b3$macho2[i] != "0" & b3$macho3[i] != "0" )
        {b3$homoz [i] = "without homozygosis"}
      }

      b3$lin <- "ok"
      b3$Raça1[is.na(b3$Raça1)]<-"0"
      b3$Raça2[is.na(b3$Raça2)]<-"0"
      b3$Raça3[is.na(b3$Raça3)]<-"0"
      b3$Raça1[b3$macho1 != 0 & b3$Raça1 == 0]<- "NI"
      b3$Raça2[b3$macho2 != 0 & b3$Raça2 == 0]<- "NI"
      b3$Raça3[b3$macho3 != 0 & b3$Raça3 == 0]<- "NI"

      b3a<-b3[b3$macho2 =="0",]
      b3b <- b3[b3$macho2 !="0" & b3$macho3 == "0",]
      b3c <- b3[b3$macho3 !="0",]
      if(nrow(b3b)>0){
        for (i in 1:nrow(b3b)){
          if (b3b$Raça1[i] != b3b$Raça2[i] )
          {b3b$lin [i] = "different breed"}

        }}
      if(nrow(b3c)>0){
        for (i in 1:nrow(b3c)){
          if (b3c$Raça1[i] != b3c$Raça2[i] | b3c$Raça1[i] != b3c$Raça3[i]|b3c$Raça2[i] != b3c$Raça3[i])
          {b3c$lin [i] = "different breed"}
        }
      }
      b4 <- rbind(b3a,b3b,b3c)
      b4$bisa <- "no"
      b4$bisa[b4$TATF %in% recom$TATF] <- "yes"

      b4$id<-paste(b4$macho1, b4$TATF)


      c <- recom[,c("TATF", order)]
      colnames(c)[2]<-"k"
      recom$id<-paste(c$k,recom$TATF)
      rm(c)
      c1<- merge(recom[,c("id","F","REC")],b4,by="id",all=T)
      c1$TATF[is.na(c1$TATF)]<-0

      c1<-c1[!(c1$TATF==0),]

      c1$endog[is.na(c1$REC)]<-"NI"
      c1$endog[c1$F<=Flimit]<-"ok"
      c1$endog[c1$F>=Flimit]<-"high inbreeding"

      d1<-c1
      d1$definição <- NA

      bisa <- d1[d1$bisa=="yes",]
      if(nrow(bisa)> 1 ){
        for (i in 1:nrow(bisa)){
          if (bisa$endog[i] == "ok" & bisa$homoz[i]  == "ok" & bisa$Raça1[i] ==breed_ggmother)
          {bisa$definição [i] = "mating ok"}
          if (bisa$endog[i] == "high inbreeding" & bisa$homoz[i]  == "ok" & bisa$Raça1[i] ==breed_ggmother)
          {bisa$definição [i] = "high inbreeding"}}

        bisa$definição[is.na(bisa$definição)]<-"loss of opportunity"}
      outras <- d1[d1$bisa=="no",]

      outras$definição <- outras$lin
      outras$definição[outras$lin=="ok"]<- NA
      if(nrow(bisa)< 1 ){
        df<-outras
      }else{
        df <- rbind(bisa,outras)}
      df$definição[df$Raça1=="NI"|df$Raça2=="NI"|df$Raça3 =="NI"]<-"unidentified male"
      bisadef <-as.data.frame(table(df$definição[df$bisa=="yes"]))
      Other_Def <- as.data.frame(table(df$definição[df$bisa=="no"]))
      colnames(bisadef)[1]<-"definição"
      colnames(Other_Def)[1]<-"definição"
      df$dup <- paste(df$BRF,df$Cob)
      df <- df[!(duplicated(df$dup)),]
      if(obs=="TRUE"){

        df2<-merge(df,dfobs,by.x="macho1",by.y=order,all=TRUE)
        df<-df2[!(is.na(df2$BRF)),]
        df <- df[,c("BRF","TATF","Cic","Cob","Prev","macho1","Raça1","macho2","Raça2","macho3","Raça3","bisa","definição","F","obs")]

        colnames(df)<-c("Tag","ID","Order","Insemination","Birth.","Male1","Breed1","Male2","Breed2","Male3","Breed3","G.G.mother","Definition","F","obs")

      }else{


        df <- df[,c("BRF","TATF","Cic","Cob","Prev","macho1","Raça1","macho2","Raça2","macho3","Raça3","bisa","definição","F")]

        colnames(df)<-c("Tag","ID","Order","Insemination","Birth.","Male1","Breed1","Male2","Breed2","Male3","Breed3","G.G.mother","Definition","F")
      }
      df[(is.na(df))]<-" "
      cat("ok!")
      return(df)
    }

  }




  return(app)
}


