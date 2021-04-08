IndexSM <- function() {

  require(shiny)
  require(readxl)
  require(pedigreemm)
  require(readr)
  require(DT)
  require(plyr)
  require(reshape2)

  options(shiny.maxRequestSize=3000*1024^2)
  app<-shinyApp(
    ###############################################UI#######################
    ui =

      navbarPage("Index Calculation",
                 tabPanel("Single Index",


      fluidPage(
      titlePanel("Single Index"),

      sidebarLayout(
             sidebarPanel("Enter with informations:", width=12,

                              ######################################3

                          fixedRow( #Title
                            column(10,""),
                          column(2,actionButton("run", "RUN")
                            )
                          ),
                               fixedRow( #Title
                                column(6,"FILES"),

                                 #Downloa
                              ),


                              fixedRow( #animal file
                                column(3,
                                       fileInput("file1", "Animals File",
                                                 accept = c(
                                                   "text/csv",
                                                   "text/comma-separated-values,text/plain",
                                                   ".csv")),
                                       tags$hr(),
                                       checkboxInput("header1","Header", TRUE)
                                ),
                                column(3,checkboxInput("tudo", "All columns", FALSE)),

                                  column(3, textInput("save_name","File name","Ranking_single"),),
                                column(3, downloadButton("downloadData", "Download Rank (.csv)") )#Download


                              ),


                              fixedRow(
                                column(5,
                                       fileInput("file2", "Solution file 1",
                                                 accept = c(
                                                   "text/csv",
                                                   "text/comma-separated-values,text/plain",
                                                   ".csv")
                                       )), #sol1
                                column(5,
                                       fileInput("file3", "Renadd 1",
                                                 accept = c(
                                                   "text/csv",
                                                   "text/comma-separated-values,text/plain",
                                                   ".csv")
                                       )), #ren1
                              ),
                              fixedRow(
                                column(5,
                                       fileInput("file4", "Solution file 2",
                                                 accept = c(
                                                   "text/csv",
                                                   "text/comma-separated-values,text/plain",
                                                   ".csv")
                                       )), #sol2
                                column(5,
                                       fileInput("file5", "Renadd 2",
                                                 accept = c(
                                                   "text/csv",
                                                   "text/comma-separated-values,text/plain",
                                                   ".csv")
                                       )), #ren2
                              ),
                              fixedRow(
                                column(5,
                                       fileInput("file6", "Solution file 3",
                                                 accept = c(
                                                   "text/csv",
                                                   "text/comma-separated-values,text/plain",
                                                   ".csv")
                                       )), #sol3
                                column(5,
                                       fileInput("file7", "Renadd 3",
                                                 accept = c(
                                                   "text/csv",
                                                   "text/comma-separated-values,text/plain",
                                                   ".csv")
                                       )), #ren3
                              ),
                              fixedRow( #Title
                                column(10,"INDEX")
                              ),
                              fixedRow(
                                column(3,
                                       numericInput("an", "Number of solutions file:", min = 1, max = 3, value = 2)
                                ),
                                column(3,
                                       numericInput("nt", "Traits:", min = 1, max = 6, value = 5)
                                ), #N trait

                                column(3,
                                       numericInput("base", "Base:", min = 0, max = 999, value = 120)
                                ) # base
                              ),
                          fixedRow(
                            column(2, "TRAIT 1"), #T1
                            column(2, "TRAIT 2"), #T2
                            column(2,"TRAIT 3"  ), #T3
                            column(2,"TRAIT 4"), #T4
                            column(2, "TRAIT 5"), #T5
                             column(2,"TRAIT 6")
                            ),

                              fixedRow(
                                column(2,
                                       numericInput("f1", "File 1:", min = 1, max =3, value = 1)
                                ), #T1
                                column(2,
                                       numericInput("f2", "File 2:", min = 1, max =3, value = 1)
                                ), #T2
                                column(2,
                                       numericInput("f3", "File 3:", min = 1, max =3, value = 1)
                                ), #T3


                                column(2,
                                       numericInput("f4", "File 4:", min = 1, max =3, value = 1)
                                ), #T4
                                column(2,
                                       numericInput("f5", "File 5:", min = 1, max =3, value = 2)
                                ), #T5

                                column(2,
                                       numericInput("f6", "File 6:", min = 1, max =3, value = "")
                                )), #T6

                          fixedRow(
                                column(2,
                                       numericInput("e1", "Effect 1:", min = 1, max = 20, value = 4)
                                ), #T1
                                column(2,
                                       numericInput("e2", "Effect 2:", min = 1, max = 20, value = 4)
                                ), #T2
                                column(2,
                                       numericInput("e3", "Effect 3:", min = 1, max = 20, value = 4)
                                ), #T3
                                column(2,
                                       numericInput("e4", "Effect 4:", min = 1, max = 20, value = 4)
                                ), #T4
                                column(2,
                                       numericInput("e5", "Effect 5:", min = 1, max = 20, value = 3)
                                ), #T5
                                column(2,
                                       numericInput("e6", "Effect 6:", min = 1, max = 20, value = "")
                                ) #T6
                          ),

                          fixedRow(

                                column(2,
                                       numericInput("t1", "Trait 1:", min = 1, max = 20, value = 1)
                                ), #T1
                                column(2,
                                       numericInput("t2", "Trait 2:", min = 1, max = 20, value = 2)
                                ), #T2
                                column(2,
                                       numericInput("t3", "Trait 3:", min = 1, max = 20, value = 3)
                                ), #T3
                                column(2,
                                       numericInput("t4", "Trait 4:", min = 1, max = 20, value = 4)
                                ), #T4
                                column(2,
                                       numericInput("t5", "Trait 5:", min = 1, max = 20, value = 1)
                                ), #T5
                                column(2,
                                       numericInput("t6", "Trait 6:", min = 1, max = 20, value = "")
                                )#T6
                          ),

                          fixedRow(
                                column(2,
                                       numericInput("w1", "Weight 1:", min = 1, max = 100, value = 1.5)
                                ),#T1

                            column(2,
                                       numericInput("w2", "Weight 2:", min = 1, max = 100, value = -4.2)
                                ), #T2


                                column(2,
                                       numericInput("w3", "Weight 3:", min = 1, max = 100, value = -1)
                                ),#T3
                                column(2,
                                       numericInput("w4", "Weight 4:", min = 1, max = 100, value = 1)
                                ), #T4

                                column(2,
                                       numericInput("w5", "Weight 5:", min = 1, max = 100, value = 2.3)
                                ), #T5
                                column(2,
                                       numericInput("w6", "Weight 6:", min = 1, max = 100, value = "")
                                ) #T6


                              ),




                              ),

             mainPanel(

             )
             )

      )),


      ###################MULTI
      tabPanel("Multiple Index",


               fluidPage(
                 titlePanel("Multiple Index"),

                 sidebarLayout(
                   sidebarPanel("Enter with informations:", width=12,

                                ######################################3


                                fixedRow( #Title
                                  column(10,""),
                                  column(2,actionButton("run2", "RUN")
                                  )
                                ),
                                fixedRow( #Title
                                  column(6,"FILES"),

                                  #Downloa
                                ),


                                fixedRow( #animal file
                                  column(3,
                                         fileInput("file12", "Animals File",
                                                   accept = c(
                                                     "text/csv",
                                                     "text/comma-separated-values,text/plain",
                                                     ".csv")),
                                         tags$hr(),
                                         checkboxInput("header12","Header", TRUE)
                                  ),
                                  column(3,checkboxInput("tudo2", "All columns", FALSE)),

                                  column(3, textInput("save_name2","File name","Ranking_multiple"),),
                                  column(3, downloadButton("downloadData2", "Download Rank (.csv)") )#Download


                                ),
                                fixedRow(
                                  column(5,
                                         fileInput("file13", "Solution file 1",
                                                   accept = c(
                                                     "text/csv",
                                                     "text/comma-separated-values,text/plain",
                                                     ".csv")
                                         )), #sol1
                                  column(5,
                                         fileInput("file14", "Renadd 1",
                                                   accept = c(
                                                     "text/csv",
                                                     "text/comma-separated-values,text/plain",
                                                     ".csv")
                                         )),#ren1
                                  column(2,
                                         numericInput("bn", "Number of traits:", min = 0, max = 6, value = 5)
                                  ),
                                  column(2,
                                         numericInput("effect", "Number of effect:", min = 0, max = 6, value = 5)
                                  ),
                                ),

                                fixedRow( #Title
                                  column(10,"INDEX")
                                ),
                                fixedRow(
                                  column(2,
                                         numericInput("base2", "Base value:", min = 0, value = 120)
                                  ),

                                   column(2,
                                         numericInput("ano1", "Year 1:", min = 0, value = 201500)
                                  ),
                                  column(2,
                                         numericInput("ano2", "Year 2:", min = 0, value = 201600),
                                         checkboxInput("fixed","Fixed EBV mean:", TRUE)
                                  ),
                                  column(5,
                                         fileInput("file15", "Breeds File",
                                                   accept = c(
                                                     "text/csv",
                                                     "text/comma-separated-values,text/plain",
                                                     ".csv")),
                                         tags$hr(),
                                         checkboxInput("useB","Use file", TRUE)
                                  )#N trait

                                ),
                                fixedRow( #tITLE
                                  column(4,"Index 1")
                                ),
                                fixedRow(
                                  column(2,
                                         textInput("d1", h5("Breed:"),   value = "DB50")
                                  ), #T1

                                  column(2,
                                         numericInput("b11", "b1:", min = 1, max = 20, value = 2)
                                  ), #T1
                                  column(2,
                                         numericInput("b21", "b2:", min = 1, max = 20, value = -2)
                                  ), #T1
                                  column(2,
                                         numericInput("b31", "b3:", min = 1, max = 100, value = -2)
                                  ),column(2,
                                           numericInput("b41", "b5:", min = 1, max =100, value = 2)
                                  ), #T1
                                  column(2,
                                         numericInput("b51", "b5:", min = 1, max = 20, value = -2)
                                  ), #T1
                                  column(2,
                                         numericInput("b61", "b6:", min = 1, max = 20, value = "")
                                  )
                                ),#I1

                                fixedRow( #tITLE
                                  column(4,"Index 2")
                                ),
                                fixedRow(
                                  column(2,
                                         textInput("d2", h5("Breed:"),   value = "DB42")
                                  ), #T1

                                  column(2,
                                         numericInput("b12", "b1:", min = 1, max = 20, value = 3)
                                  ), #T1
                                  column(2,
                                         numericInput("b22", "b2:", min = 1, max = 20, value = -3)
                                  ), #T1
                                  column(2,
                                         numericInput("b32", "b3:", min = 1, max = 100, value = -1.5)
                                  ),column(2,
                                           numericInput("b42", "b5:", min = 1, max =100, value = 1.5)
                                  ), #T1
                                  column(2,
                                         numericInput("b52", "b5:", min = 1, max = 20, value = -1)
                                  ), #T1
                                  column(2,
                                         numericInput("b62", "b6:", min = 1, max = 20, value = "")
                                  )
                                ),
                                fixedRow( #tITLE
                                  column(4,"Index 3")
                                ),
                                fixedRow(
                                  column(2,
                                         textInput("d3", h5("Breed:"),   value = "DB46")
                                  ), #T1

                                  column(2,
                                         numericInput("b13", "b1:", min = 1, max = 20, value = 2)
                                  ), #T1
                                  column(2,
                                         numericInput("b23", "b2:", min = 1, max = 20, value = -2)
                                  ), #T1
                                  column(2,
                                         numericInput("b33", "b3:", min = 1, max = 100, value = -2)
                                  ),column(2,
                                           numericInput("b43", "b5:", min = 1, max =100, value = 2)
                                  ), #T1
                                  column(2,
                                         numericInput("b53", "b5:", min = 1, max = 20, value = -2)
                                  ), #T1
                                  column(2,
                                         numericInput("b63", "b6:", min = 1, max = 20, value = "")
                                  )
                                ),
                                fixedRow( #tITLE
                                  column(4,"Index 4")
                                ),
                                fixedRow(
                                  column(2,
                                         textInput("d4", h5("Breed:"),   value = "DB70")
                                  ), #T1

                                  column(2,
                                         numericInput("b14", "b1:", min = 1, max = 20, value = 1)
                                  ), #T1
                                  column(2,
                                         numericInput("b24", "b2:", min = 1, max = 20, value = -3)
                                  ), #T1
                                  column(2,
                                         numericInput("b34", "b3:", min = 1, max = 100, value = -2)
                                  ),column(2,
                                           numericInput("b44", "b5:", min = 1, max =100, value = 2)
                                  ), #T1
                                  column(2,
                                         numericInput("b54", "b5:", min = 1, max = 20, value = -2)
                                  ), #T1
                                  column(2,
                                         numericInput("b64", "b6:", min = 1, max = 20, value = "")
                                  ) #T1

                                ),
                                fixedRow( #tITLE
                                  column(4,"Index 5")
                                ),
                                fixedRow(
                                  column(2,
                                         textInput("d5", h5("Breed:"),   value = "LI76")
                                  ), #T1

                                  column(2,
                                         numericInput("b15", "b1:", min = 1, max = 20, value = 2)
                                  ), #T1
                                  column(2,
                                         numericInput("b25", "b2:", min = 1, max = 20, value = -2)
                                  ), #T1
                                  column(2,
                                         numericInput("b35", "b3:", min = 1, max = 100, value = -2)
                                  ),column(2,
                                           numericInput("b45", "b5:", min = 1, max =100, value = 2)
                                  ), #T1
                                  column(2,
                                         numericInput("b55", "b5:", min = 1, max = 20, value = -2)
                                  ), #T1
                                  column(2,
                                         numericInput("b65", "b6:", min = 1, max = 20, value = "")
                                  )
                                ),
                                fixedRow( #tITLE
                                  column(4,"Index 6")
                                ),
                                fixedRow(
                                  column(2,
                                         textInput("d6", h5("Breed:"),   value = "LM62")
                                  ),


                                  column(2,
                                         numericInput("b16", "b1:", min = 1, max = 20, value = 2)
                                  ), #T1
                                  column(2,
                                         numericInput("b26", "b2:", min = 1, max = 20, value = -2)
                                  ), #T1
                                  column(2,
                                         numericInput("b36", "b3:", min = 1, max = 100, value = -2)
                                  ),column(2,
                                           numericInput("b46", "b5:", min = 1, max =100, value = 2)
                                  ), #T1
                                  column(2,
                                         numericInput("b56", "b5:", min = 1, max = 20, value = -2)
                                  ), #T1
                                  column(2,
                                         numericInput("b66", "b6:", min = 1, max = 20, value = "")
                                  )
                                ),
                                fixedRow( #tITLE
                                  column(4,"Index 7")
                                ),
                                fixedRow(
                                  column(2,
                                         textInput("d7", h5("Breed:"),   value = "LQ")
                                  ),
                                  #T1

                                  column(2,
                                         numericInput("b17", "b1:", min = 1, max = 20, value = 2)
                                  ), #T1
                                  column(2,
                                         numericInput("b27", "b2:", min = 1, max = 20, value = -2)
                                  ), #T1
                                  column(2,
                                         numericInput("b37", "b3:", min = 1, max = 100, value = -2)
                                  ),column(2,
                                           numericInput("b47", "b5:", min = 1, max =100, value = 2)
                                  ), #T1
                                  column(2,
                                         numericInput("b57", "b5:", min = 1, max = 20, value = -2)
                                  ), #T1
                                  column(2,
                                         numericInput("b67", "b6:", min = 1, max = 20, value = "")
                                  )
                                )






                                ########################################3

                   ),

                   mainPanel(

                   )
                 )

               )),
############################options

tabPanel("Options",
         fluidPage(
           titlePanel("Options"),

           sidebarLayout(
             sidebarPanel("", width=12,
                          fixedRow(
                            column(4,checkboxInput("end", "Calc. Inb.¹", FALSE)),
                            column(4,checkboxInput("count", "Count progeny²", FALSE)),#female_replacement and count progeny
                          ),
                          fixedRow(

                            column(4,
                                   fileInput("file9", "Pedigree File¹²³",
                                             accept = c(
                                               "text/csv",
                                               "text/comma-separated-values,text/plain",
                                               ".csv")
                                   ),
                                   tags$hr()
                            ),

                            column(4,
                                   numericInput("ng9", "Skip lines (PED):",min = 0, 8 )
                            )

                          ),

                          fixedRow(
                            column(5,
                                   fileInput("file10", "Phenotype",
                                             accept = c(
                                               "text/csv",
                                               "text/comma-separated-values,text/plain",
                                               ".csv")),
                                   tags$hr(),
                                   checkboxInput("header10", "Header", TRUE),
                                   checkboxInput("useF", "Use phenotype file", FALSE)

                            ), #Phenotype
                            column(5,
                                   fileInput("file11", "Recommendation",
                                             accept = c(
                                               "text/csv",
                                               "text/comma-separated-values,text/plain",
                                               ".csv")),
                                   tags$hr(),
                                   checkboxInput("header11", "Header", TRUE),
                                   checkboxInput("recom", "Use recommendation file", FALSE),
                                   checkboxInput("male", "For males", FALSE)

                            )
                          ),
                          fixedRow(
                            column(5,
                                   fileInput("file16", "Aditional Animals 1",
                                             accept = c(
                                               "text/csv",
                                               "text/comma-separated-values,text/plain",
                                               ".csv")),
                                   tags$hr(),
                                   checkboxInput("header16", "Header", TRUE),
                                   checkboxInput("useA", "Use aditional animals file 1", FALSE)

                            ),
                            column(5,
                                   fileInput("file18", "Aditional Animals 2",
                                             accept = c(
                                               "text/csv",
                                               "text/comma-separated-values,text/plain",
                                               ".csv")),
                                   tags$hr(),
                                   checkboxInput("header18", "Header", TRUE),
                                   checkboxInput("useA2", "Use aditional animals file 2", FALSE)

                            ) #Adicional
                          ),
                          fixedRow(
                            column(5,
                                   fileInput("file17", "Sire Information³",
                                             accept = c(
                                               "text/csv",
                                               "text/comma-separated-values,text/plain",
                                               ".csv")),
                                   tags$hr(),
                                   checkboxInput("header17", "Header", TRUE),
                                   checkboxInput("useS", "Use sire information", FALSE)

                            ) #Adicional
                          )




             ),

             mainPanel(


             )
           )
         )

),
      #########################help
      tabPanel("Help",

               fluidPage(
                 titlePanel(""),

                 sidebarLayout(
                   sidebarPanel("", width=0,


                   ),

                   mainPanel(
                     h2("Help Information"),
                     p("Use this information to build the settings."),
                     h3("Caution - Essential elements",style = "color:red"),
                     p("- Requires animal file."),
                     p("- Requires at least one solution/renadd file."),
                     p("- At least one trait should be included."),
                     p("- After adding the necessary files, click RUN to analyze."),
                     p("- To download final file click DOWNLOAD RANK."),
                     h3("Options",style = "color:blue"),
                     #p("Replacement: include corresponding files (marked with 1)."),
                     #p("   Including the active and pedigree file you check the familiarity of the new females with the farm's active ones.",style="color:grey"),
                     p("Count progeny: include corresponding file (marked with 2)."),
                     #p("Never select Count progeny with Replacement",style="color:red"),
                     p("   Check paternal familiarity of your females or males.",style="color:grey"),
                     p("Phenotype: just include the file."),
                     p("   Include some important information to your rank file (Tag/ID/Other informations...)*no ID.",style="color:grey"),
                     p("Mating Recommendation: just include the file (format of app) and check: Use Recommendation"),
                     p("   Verify the number of males available for each female.For evaluate numeber os females per male check: For male.",style="color:grey"),
                     p("Aditional Animals:  just include the file and check: Use adicional animals file"),
                     p("   Include more animals to be ranked.",style="color:grey"),
                     h3("Files type",style = "color:grey"),
                    p("Animal file: animals to be ranking (Tg/ID/INFO - .xls/.xlsx)**In case of MultpleIndex and no breed file third colun is the breed column to identfy the correct index to be used."),
                     p("Solution/Renadd: Blup family programs exit."),
                     p("Traits: Number of traits that will be used."),
                     p("Base: base index value."),
                     h4("Single Index",style = "color:grey"),
                     p("For each trait: File(which solution file)|Effect (in the solution file)|Trait(in the solution file)|Weight(in the index)."),
                     h4("Multiple Index",style = "color:grey"),
                    p("Effect's number of EBV in solution file"),
                     p("For each Index: Breed (same of the breed's column ou breed file)|Weight(in the index)."),
                    p("To fixed EBV mean between years is necessary a breed file with year and year1|year2(Check:Fixed)"),
                    p("Breed file: ID|ID Sire |ID Dam| Breed (or category)|Birth Year (numeric)"),
                    h4("Options",style = "color:grey"),
                     #p("Active females file: animals to be ranking (Tg/ID/Local - .xls/.xlsx)."),
                     p("Reordered pedigree: sire and dam information (ID/Sire/Dam/Original - .prn) - Default:CFC output (skipping 8 lines)."),
                     p("Reccomendation: exit of MatingRecommendation fuction."),
                    p("Aditional Animal: Same style of animals file (Tg/ID/INFO/...-.xls/.xlsx)"),
                     p("Aditional information will be ignored"),
                    p("Sire information: Same style of animals file *use with count progeny² (ID/INFO-.xls/.xlsx)"),
                    p("Aditional information will be ignored")


                   )
                 )
               )


    ),

    tabPanel("Input",
             fluidPage(
               titlePanel("Input files"),

               sidebarLayout(
                 sidebarPanel("", width=0

                 ),

                 mainPanel(
                   tableOutput("animal"),
                   tableOutput("animal2"),
                   tableOutput("breed"),
                   tableOutput("at"),
                   tableOutput("ped"),
                   tableOutput("phen"),
                   tableOutput("rec"),
                   tableOutput("add"),
                   tableOutput("add2"),
                   tableOutput("sire")

                 )
               )
             )

    ),
    tabPanel("Output",
             fluidPage(
               titlePanel("Output file"),

               sidebarLayout(
                 sidebarPanel("", width=0

                 ),

                 mainPanel(


                   tabsetPanel(
                     tabPanel("Table",  dataTableOutput("viewR")),
                     tabPanel("Summary/Hist",  verbatimTextOutput("mean"),
                              plotOutput(outputId = "distPlot"),
                              plotOutput(outputId = "plotPlot")),
                     tabPanel("Cat",  verbatimTextOutput("num"),verbatimTextOutput("sum"))

                   )


                 )
               )
             )


    )



    ),

#################################server##################
#
#
#
#
#
#
    server = function(input, output) {
#############################################INPUT AND VIEW
      animal <- reactive({

        inFile1 <- input$file1
        if (is.null(inFile1))
          return(NULL)
        read_excel(inFile1$datapath, col_names = input$header1)
      })

      sol1 <- reactive({

        inFile2 <- input$file2
        if (is.null(inFile2))
          return(NULL)
        read_table2(inFile2$datapath,
                    col_names = FALSE, skip = 1)
      })


      ren1 <- reactive({

        inFile3 <- input$file3
        if (is.null(inFile3))
          return(NULL)
        read_table2(inFile3$datapath, col_names = FALSE)
      })


      sol2<- reactive({

        inFile4 <- input$file4
        if (is.null(inFile4))
          return(NULL)
        read_table2(inFile4$datapath,
                    col_names = FALSE, skip = 1)
      })


      ren2<-reactive({

        inFile5 <- input$file5
        if (is.null(inFile5))
          return(NULL)
        read_table2(inFile5$datapath, col_names = FALSE)
      })

      sol3 <- reactive({

        inFile6 <- input$file6
        if (is.null(inFile6))
          return(NULL)
        read_table2(inFile6$datapath,
                    col_names = FALSE, skip = 1)
      })


      ren3<-reactive({

        inFile7 <- input$file7
        if (is.null(inFile7))
          return(NULL)
        read_table2(inFile7$datapath, col_names = FALSE)
      })


      at <- reactive({

        inFile8 <- input$file8
        if (is.null(inFile8))
          return(NULL)
        read_excel(inFile8$datapath, col_names = input$header8)
      })
      ped<- reactive({

        inFile9 <- input$file9
        if (is.null(inFile9))
          return(NULL)
        read_table2(inFile9$datapath,
                    col_names = FALSE, skip = input$ng9, col_types = cols(`X4` = col_character()))
        })

      phen<- reactive({

        inFile10 <- input$file10
        if (is.null(inFile10))
          return(NULL)
        read_excel(inFile10$datapath, col_names = input$header10)
      })

      rec <- reactive({

        inFile11 <- input$file11
        if (is.null(inFile11))
          return(NULL)

        read_delim(inFile11$datapath,
                   ";", escape_double = FALSE, col_types = cols(ID_F = col_character(),
                                                                ID_M = col_character(),
                                                                Local = col_character(),
                                                                Recommendation = col_character(),
                                                                Tag_F = col_character(), Tag_M = col_character()),
                   locale = locale(decimal_mark = ".",
                                   grouping_mark = ","),trim_ws = TRUE,col_names = input$header11)

      })
      animal2 <- reactive({

        inFile12 <- input$file12
        if (is.null(inFile12))
          return(NULL)
        read_excel(inFile12$datapath, col_names = input$header12)
      })

      sol12 <- reactive({

        inFile13 <- input$file13
        if (is.null(inFile13))
          return(NULL)
        read_table2(inFile13$datapath,
                    col_names = FALSE, skip = 1)
      })


      ren12 <- reactive({

        inFile14 <- input$file14
        if (is.null(inFile14))
          return(NULL)
        read_table2(inFile14$datapath, col_names = FALSE)
      })

      breed2  <- reactive({

        inFile15 <- input$file15
        if (is.null(inFile15))
          return(NULL)
        read_delim(inFile15$datapath,
                   ";", escape_double = FALSE, col_names = FALSE,col_types = cols(X5 = col_number()),
                   trim_ws = TRUE)

      })
      add <- reactive({

        inFile16 <- input$file16
        if (is.null(inFile16))
          return(NULL)
        read_excel(inFile16$datapath, col_names = input$header16)
      })
      add2 <- reactive({

        inFile18 <- input$file18
        if (is.null(inFile18))
          return(NULL)
        read_excel(inFile18$datapath, col_names = input$header16)
      })
      sire <- reactive({

        inFile17 <- input$file17
        if (is.null(inFile17))
          return(NULL)
        read_excel(inFile17$datapath, col_names = input$header17)
      })

      output$animal <- renderTable({
        head(animal())
      })
      output$animal2 <- renderTable({
        head(animal2())
      })
      output$breed <- renderTable({
        head(breed2())
      })
      output$at <- renderTable({
        head(at())
      })
      output$ped <- renderTable({
        head(ped())
      })
      output$phen <- renderTable({
        head(phen())
      })
      output$rec <- renderTable({
        head(rec())
      })
      output$add <- renderTable({
        head(add())
      })
      output$add2 <- renderTable({
        head(add2())
      })
      output$sire <- renderTable({
        head(sire())
      })


#############################################func
      forout_reactive <- reactiveValues()
      observeEvent(input$run, {
        cat("Runing
            ")


        useA<-input$useA
        useA2<-input$useA2
        analysis<-input$an
        base<-input$base
        traits<-input$nt
        useS<-input$useS
        b1<-c(input$f1,input$e1,input$t1,input$w1)
        b2<-c(input$f2,input$e2,input$t2,input$w2)
        b3<-c(input$f3,input$e3,input$t3,input$w3)
        b4<-c(input$f4,input$e4,input$t4,input$w4)
        b5<-c(input$f5,input$e5,input$t5,input$w5)
        b6<-c(input$f6,input$e6,input$t6,input$w6)
        tudo<-input$tudo
        if (is.null(analysis)){
          return(NULL)}
        if (is.null(base)){
          return(NULL)}
        if (is.null(traits)){
          return(NULL)}
        if (is.null(b1)){
          return(NULL)}
        if (is.null(b2)){
          return(NULL)}
        if (is.null(b3)){
          return(NULL)}
        if (is.null(b4)){
          return(NULL)}
        if (is.null(b5)){
          return(NULL)}
        if (is.null(b6)){
          return(NULL)}

        animal<-animal()
        if(useA==TRUE & !(is.null(add()))){
        colnames(animal)[1:3]<-c("Tag","ID","Order")
        add<-add()
        colnames(add)[1:3]<-c("Tag","ID","Order")
        animal$Tag<-as.character(animal$Tag)
        animal$ID<-as.character(animal$ID)
        animal$Order<-as.character(animal$Order)
        add$Tag<-as.character(add$Tag)
        add$ID<-as.character(add$ID)
        add$Order<-as.character(add$Order)
        df0<-rbind.fill(animal,add)
        df0<-df0[!(duplicated(df0$ID)),]
        animal<-df0
        rm(df0)
        cat(" Animals add!")
        }else{
          animal<-animal
          cat("No animals 1 to add!")}

        if(useA2==TRUE & !(is.null(add2()))){
          colnames(animal)[1:3]<-c("Tag","ID","Order")
          add<-add2()
          colnames(add)[1:3]<-c("Tag","ID","Order")
          animal$Tag<-as.character(animal$Tag)
          animal$ID<-as.character(animal$ID)
          animal$Order<-as.character(animal$Order)
          add$Tag<-as.character(add$Tag)
          add$ID<-as.character(add$ID)
          add$Order<-as.character(add$Order)
          df0<-rbind.fill(animal,add)
          df0<-df0[!(duplicated(df0$ID)),]
          animal<-df0
          rm(df0)
          cat(" Animals 2 add!")
        }else{
          animal<-animal
          cat("No animals 2 to add!")}

        count2<-input$count
        #rep<-input$rep
        end<-input$end
        recom<-input$recom
        male<-input$male
        useF<-input$useF
        datarank<- ranking(animal,analysis,sol1(),ren1(),sol2(),ren2(),sol3(),ren3(),
                           base,traits,b1,b2,b3,b4,b5,b6)


        endog<-endog(datarank,ped(),end)

        reco<-reco(endog,rec(),recom,male)
        phen<-pheno(reco,phen(),useF)
        count<-count(phen,ped(),count2)


        if(useS == FALSE|is.null(sire())|is.null(ped()) |count2==FALSE){
          count<-count
        }else{
          sire<-sire()
          colnames(sire)<-c("ID_Sire","SI")
          rep2<-merge(count,sire,by="ID_Sire",all=T)
          rep<-rep2[!(is.na(rep2$Tag)),]
          col_order <- colnames(rep)
          col_order <- col_order[!(col_order %in% (c("ID_Sire","ID_Dam","Tag","ID","Index"))) ]
          count<-rep[,c("Tag","ID","ID_Sire","Dam","Index", col_order)]
          cat("Sire information add!")
        }
        output$viewR<- renderDT(
          count, # data
          class = "display nowrap compact", # style
          filter = "top" # location of column filters
        )

        if(ncol(animal)<= 3 | tudo == FALSE ){
          Rank<-count
        }else{
          colnames(animal)[2]<-"ID"
          animal$ID<-as.character(animal$ID)
          an<-animal[,4:ncol(animal)]
          an<-as.data.frame(an)
          colnames(animal)[2]<-"ID"
          an$ID<-animal$ID
          Rank2<-merge(count,an,by="ID",all=T)
          Rank2<-Rank2[!(duplicated(Rank2$ID)),]
          colord<-colnames(Rank2[,3:ncol(Rank2)])
          Rank2<-Rank2[,c("Tag","ID",colord)]

          Rank<-Rank2
        }




        forout_reactive$R<- Rank






      })
      #############################################func2
      forout_reactive <- reactiveValues()
      observeEvent(input$run2, {
        cat("Runing
            ")

        useA<-input$useA
        useA2<-input$useA2
        useB<-input$useB
        effect<-input$effect
        base2<-input$base2
        bn<-input$bn
        ano1<-input$ano1
        ano2<-input$ano2
        fixed<-input$fixed
        tudo2<-input$tudo2
        useS<-input$useS

        count2<-input$count
        #rep<-input$rep
        end<-input$end
        recom<-input$recom
        male<-input$male
        useF<-input$useF

        if (is.null(base2))
          return(NULL)
        if (is.null(effect))
          return(NULL)
        if (is.null(bn))
          return(NULL)

        i1<-c(input$d1,input$b11,input$b21,input$b31,input$b41,input$b51,input$b61)
        i2<-c(input$d2,input$b12,input$b22,input$b32,input$b42,input$b52,input$b62)
        i3<-c(input$d3,input$b13,input$b23,input$b33,input$b43,input$b53,input$b63)
        i4<-c(input$d4,input$b14,input$b24,input$b34,input$b44,input$b54,input$b64)
        i5<-c(input$d5,input$b15,input$b25,input$b35,input$b45,input$b55,input$b65)
        i6<-c(input$d6,input$b16,input$b26,input$b36,input$b46,input$b56,input$b66)
        i7<-c(input$d7,input$b17,input$b27,input$b37,input$b47,input$b57,input$b67)

        i1<-i1 [!(is.na(i1))]
        i2<-i2 [!(is.na(i2))]
        i3<-i3 [!(is.na(i3))]
        i4<-i4 [!(is.na(i4))]
        i5<-i5 [!(is.na(i5))]
        i6<-i6 [!(is.na(i6))]
        i7<-i7 [!(is.na(i7))]
        animal<-animal2()
        if(useA==TRUE & !(is.null(add()))){
          colnames(animal)[1:3]<-c("Tag","ID","Order")
          add<-add()
          colnames(add)[1:3]<-c("Tag","ID","Order")
          animal$Tag<-as.character(animal$Tag)
          animal$ID<-as.character(animal$ID)
          animal$Order<-as.character(animal$Order)
          add$Tag<-as.character(add$Tag)
          add$ID<-as.character(add$ID)
          add$Order<-as.character(add$Order)
          df0<-rbind.fill(animal,add)
          df0<-df0[!(duplicated(df0$ID)),]
          animal<-df0
          rm(df0)
          cat("Animals add!")
        }else{
          animal<-animal
          cat("No animals to add!")}

        if(useA2==TRUE & !(is.null(add2()))){
          colnames(animal)[1:3]<-c("Tag","ID","Order")
          add<-add2()
          colnames(add)[1:3]<-c("Tag","ID","Order")
          animal$Tag<-as.character(animal$Tag)
          animal$ID<-as.character(animal$ID)
          animal$Order<-as.character(animal$Order)
          add$Tag<-as.character(add$Tag)
          add$ID<-as.character(add$ID)
          add$Order<-as.character(add$Order)
          df0<-rbind.fill(animal,add)
          df0<-df0[!(duplicated(df0$ID)),]
          animal<-df0
          rm(df0)
          cat(" Animals 2 add!")
        }else{
          animal<-animal
          cat("No animals 2 to add!")}

        datarank<- rankmulti(animal,sol12(),ren12(),breed2(),base2,useB,i1,i2,i3,i4,i5,i6,i7,effect,bn,fixed,ano1,ano2)

        endog<-endog(datarank,ped(),end)

        reco<-reco(endog,rec(),recom,male)

        phen<-pheno(reco,phen(),useF)

        count<-count(phen,ped(),count2)


        if(useS == FALSE|is.null(sire())|is.null(ped()) |count2==FALSE){
          count<-count
        }else{
          sire<-sire()
          colnames(sire)<-c("ID_Sire","SI")
          rep2<-merge(count,sire,by="ID_Sire",all=T)
          rep<-rep2[!(is.na(rep2$Tag)),]
          rep<-rep[!(duplicated(rep$ID)),]
          col_order <- colnames(rep)
          col_order <- col_order[!(col_order %in% (c("ID_Sire","ID_Dam","Tag","ID","Index"))) ]
          count<-rep[,c("Tag","ID","ID_Sire","ID_Dam","Index", col_order)]
          cat("Sire information add!")
        }





        output$viewR<- renderDT(
          count, # data
          class = "display nowrap compact", # style
          filter = "top" # location of column filters
        )


        if(ncol(animal)<= 3 | tudo2 == FALSE ){
          Rank<- count
        }else{

          colnames(animal)[2]<-"ID"
          animal$ID<-as.character(animal$ID)
          an<-animal[,4:ncol(animal)]
          an<-as.data.frame(an)
          colnames(animal)[2]<-"ID"
          an$ID<-animal$ID
          Rank2<-merge(count,an,by="ID",all=T)
          Rank2<-Rank2[!(duplicated(Rank2$ID)),]
          colord<-colnames(Rank2[,3:ncol(Rank2)])
          Rank2<-Rank2[,c("Tag","ID",colord)]
          Rank<-Rank2


        }


        forout_reactive$R<- Rank









      })
#############################################dow
      datarank <- reactive({
      forout_reactive$R
    })

      output$sum<-renderPrint({
        Ran<-datarank()
        tapply(Ran$Index, Ran$Cat, summary)
})
      output$mean<-renderPrint({
        Rank<-datarank()
        mean(Rank$Index,na.rm=TRUE)
      })
      output$num<-renderPrint({
        Rank<-datarank()
        table(Rank$Cat)
      })

      output$distPlot <- renderPlot({
        Rank<-datarank()
        hist(Rank$Index, col = "#75AADB", border = "white",
             xlab = "Index",
             main = "Histogram of Herd Index")
      })
      output$plotPlot <- renderPlot({
        Rank<-datarank()
        Rank$Cat<-as.character(Rank$Cat)
        plot(Rank$Cat,Rank$Index)
        ml<-lm(Rank$Index ~Rank$Cat)
        abline(ml, col = "blue")
      })

      output$downloadData <- downloadHandler(

        filename = function() {
          paste(input$save_name, ".csv", sep = "")
        },
        content = function(file) {

          write.table(datarank(), file, row.names = FALSE, dec = ".", sep = ";", quote = FALSE,na = "")
        }
      )
      output$downloadData2 <- downloadHandler(

        filename = function() {
          paste(input$save_name2, ".csv", sep = "")
        },
        content = function(file) {

          write.table(datarank(), file, row.names = FALSE, dec = ".", sep = ";", quote = FALSE,na = "")
        }
      )
#



#
#
#################################end server
    }
  )

##################################################FUBÇÃO RANK
  #
  #
  #

  ranking <- function(animals,analysis,sol_1,renadd_1,sol_2,renadd_2,sol_3,renadd_3,
                      base,traits,b1,b2,b3,b4,b5,b6){
    time_1 = Sys.time()
    if(is.null(animals))
      return(NULL)
    colnames(animals)[1]<-"Tag"
    animals$Tag<-as.character(animals$Tag)
    if(is.null(sol_1))
      return(NULL)
    if(is.null(renadd_1))
      return(NULL)
    if(is.null(base))
      return(NULL)
    if(is.null(b1))
      return(NULL)
    if(is.null(traits))
      return(NULL)

    colnames(animals)[1:2]<-c("Tag","ID")

    if (analysis==3 & traits ==3){
      cat("
Calculating Index")
      cat("
      ")
      cat("Animals:","
  ")
      print(nrow(animals))



      #Avalia??o 1

      r1<- renadd_1[,c(1,10,2,3)]

      colnames(r1)<-c("coda","ID","codp","codm")
      r1<-r1[ order(r1$coda,decreasing=c(FALSE)), ]
      r1 <- as.data.frame(r1[2])
      ren1<-r1


      colnames(sol_1)<- c("trait","effect","ID","solution","se")


      #Avalia??o 2

      r2<- renadd_2[,c(1,10,2,3)]
      rm(renadd_2)
      colnames(r2)<-c("coda","ID","codp","codm")
      r2<-r2[ order(r2$coda,decreasing=c(FALSE)), ]
      r2 <- as.data.frame(r2[2])
      ren1<-r1


      colnames(sol_2)<- c("trait","effect","ID","solution","se")



      #Avalia??o 3

      r3<- renadd_1[,c(1,10,2,3)]
      rm(renadd_3)
      colnames(r3)<-c("coda","ID","codp","codm")
      r3<-r3[ order(r3$coda,decreasing=c(FALSE)), ]
      r3 <- as.data.frame(r3[2])
      ren3<-r3


      colnames(sol_3)<- c("trait","effect","ID","solution","se")





      list<-list(sol_1,sol_2,sol_3)
      list_r<-list(r1,r2,r3)


      #VGs

      s1<- list[[b1[1]]]
      s2<- list[[b2[1]]]
      s3<- list[[b3[1]]]





      vg1<- s1[s1$effect == b1[2] & s1$trait == b1[3],]
      vg2<- s2[s2$effect == b2[2] & s2$trait == b2[3],]
      vg3<- s3[s3$effect == b3[2] & s3$trait == b3[3],]




      vg1 <- as.data.frame(vg1[,4])
      vg2 <- as.data.frame(vg2[,4])
      vg3 <- as.data.frame(vg3[,4])

      a1 <- list_r[[b1[1]]]
      a2 <- list_r[[b2[1]]]
      a3 <- list_r[[b3[1]]]


      vg1$animal <- a1[,1]
      vg2$animal <- a2[,1]
      vg3$animal <- a3[,1]


      colnames(vg1)<-c("vg1","ID")
      colnames(vg2)<-c("vg2","ID")
      colnames(vg3)<-c("vg3","ID")


      vg01 <- merge(vg1,vg2, by="ID")
      vg <- merge(vg01,vg3, by="ID")

      rm(vg01)

      vg$ind <- base + b1[4]*((vg$vg1-mean(vg$vg1))/sd(vg$vg1)) +
        b2[4]*((vg$vg2-mean(vg$vg2))/sd(vg$vg2))+
        b3[4]*((vg$vg3-mean(vg$vg3))/sd(vg$vg3))




    }###############################################################################
    if (analysis==3 & traits ==4){
      cat("
Calculating Index")
      cat("
      ")
      cat("Animals:","
  ")
      print(nrow(animals))



      #Avalia??o 1

      r1<- renadd_1[,c(1,10,2,3)]

      colnames(r1)<-c("coda","ID","codp","codm")
      r1<-r1[ order(r1$coda,decreasing=c(FALSE)), ]
      r1 <- as.data.frame(r1[2])
      ren1<-r1


      colnames(sol_1)<- c("trait","effect","ID","solution","se")


      #Avalia??o 2

      r2<- renadd_2[,c(1,10,2,3)]
      rm(renadd_2)
      colnames(r2)<-c("coda","ID","codp","codm")
      r2<-r2[ order(r2$coda,decreasing=c(FALSE)), ]
      r2 <- as.data.frame(r2[2])
      ren1<-r1


      colnames(sol_2)<- c("trait","effect","ID","solution","se")



      #Avalia??o 3

      r3<- renadd_1[,c(1,10,2,3)]
      rm(renadd_3)
      colnames(r3)<-c("coda","ID","codp","codm")
      r3<-r3[ order(r3$coda,decreasing=c(FALSE)), ]
      r3 <- as.data.frame(r3[2])
      ren3<-r3


      colnames(sol_3)<- c("trait","effect","ID","solution","se")





      list<-list(sol_1,sol_2,sol_3)
      list_r<-list(r1,r2,r3)


      #VGs

      s1<- list[[b1[1]]]
      s2<- list[[b2[1]]]
      s3<- list[[b3[1]]]
      s4<- list[[b4[1]]]




      vg1<- s1[s1$effect == b1[2] & s1$trait == b1[3],]
      vg2<- s2[s2$effect == b2[2] & s2$trait == b2[3],]
      vg3<- s3[s3$effect == b3[2] & s3$trait == b3[3],]
      vg4<- s4[s4$effect == b4[2] & s4$trait == b4[3],]



      vg1 <- as.data.frame(vg1[,4])
      vg2 <- as.data.frame(vg2[,4])
      vg3 <- as.data.frame(vg3[,4])
      vg4 <- as.data.frame(vg4[,4])


      a1 <- list_r[[b1[1]]]
      a2 <- list_r[[b2[1]]]
      a3 <- list_r[[b3[1]]]
      a4 <- list_r[[b4[1]]]


      vg1$animal <- a1[,1]
      vg2$animal <- a2[,1]
      vg3$animal <- a3[,1]
      vg4$animal <- a4[,1]


      colnames(vg1)<-c("vg1","ID")
      colnames(vg2)<-c("vg2","ID")
      colnames(vg3)<-c("vg3","ID")
      colnames(vg4)<-c("vg4","ID")


      vg01 <- merge(vg1,vg2, by="ID")
      vg02 <- merge(vg01,vg3, by="ID")
      vg <- merge(vg02,vg4, by="ID")

      rm(vg02,vg01)

      vg$ind <- base + b1[4]*((vg$vg1-mean(vg$vg1))/sd(vg$vg1)) +
        b2[4]*((vg$vg2-mean(vg$vg2))/sd(vg$vg2))+
        b3[4]*((vg$vg3-mean(vg$vg3))/sd(vg$vg3))+
        b4[4]*((vg$vg4-mean(vg$vg4))/sd(vg$vg4))



    } #############################################################################################
    if (analysis==3 & traits ==5){
      cat("
Calculating Index")
      cat("
      ")
      cat("Animals:","
  ")
      print(nrow(animals))


      #Avalia??o 1

      r1<- renadd_1[,c(1,10,2,3)]

      colnames(r1)<-c("coda","ID","codp","codm")
      r1<-r1[ order(r1$coda,decreasing=c(FALSE)), ]
      r1 <- as.data.frame(r1[2])
      ren1<-r1


      colnames(sol_1)<- c("trait","effect","ID","solution","se")


      #Avalia??o 2

      r2<- renadd_2[,c(1,10,2,3)]
      rm(renadd_2)
      colnames(r2)<-c("coda","ID","codp","codm")
      r2<-r2[ order(r2$coda,decreasing=c(FALSE)), ]
      r2 <- as.data.frame(r2[2])
      ren1<-r1


      colnames(sol_2)<- c("trait","effect","ID","solution","se")



      #Avalia??o 3

      r3<- renadd_1[,c(1,10,2,3)]
      rm(renadd_3)
      colnames(r3)<-c("coda","ID","codp","codm")
      r3<-r3[ order(r3$coda,decreasing=c(FALSE)), ]
      r3 <- as.data.frame(r3[2])
      ren3<-r3


      colnames(sol_3)<- c("trait","effect","ID","solution","se")





      list<-list(sol_1,sol_2,sol_3)
      list_r<-list(r1,r2,r3)


      #VGs

      s1<- list[[b1[1]]]
      s2<- list[[b2[1]]]
      s3<- list[[b3[1]]]
      s4<- list[[b4[1]]]
      s5<- list[[b5[1]]]



      vg1<- s1[s1$effect == b1[2] & s1$trait == b1[3],]
      vg2<- s2[s2$effect == b2[2] & s2$trait == b2[3],]
      vg3<- s3[s3$effect == b3[2] & s3$trait == b3[3],]
      vg4<- s4[s4$effect == b4[2] & s4$trait == b4[3],]
      vg5<- s5[s5$effect == b5[2] & s5$trait == b5[3],]


      vg1 <- as.data.frame(vg1[,4])
      vg2 <- as.data.frame(vg2[,4])
      vg3 <- as.data.frame(vg3[,4])
      vg4 <- as.data.frame(vg4[,4])
      vg5 <- as.data.frame(vg5[,4])

      a1 <- list_r[[b1[1]]]
      a2 <- list_r[[b2[1]]]
      a3 <- list_r[[b3[1]]]
      a4 <- list_r[[b4[1]]]
      a5 <-list_r[[b5[1]]]

      vg1$animal <- a1[,1]
      vg2$animal <- a2[,1]
      vg3$animal <- a3[,1]
      vg4$animal <- a4[,1]
      vg5$animal <- a5[,1]

      colnames(vg1)<-c("vg1","ID")
      colnames(vg2)<-c("vg2","ID")
      colnames(vg3)<-c("vg3","ID")
      colnames(vg4)<-c("vg4","ID")
      colnames(vg5)<-c("vg5","ID")

      vg01 <- merge(vg1,vg2, by="ID")
      vg02 <- merge(vg01,vg3, by="ID")
      vg01 <- merge(vg02,vg4, by="ID")
      vg <- merge(vg01,vg5, by="ID")
      rm(vg02,vg01)

      vg$ind <- base + b1[4]*((vg$vg1-mean(vg$vg1))/sd(vg$vg1)) +
        b2[4]*((vg$vg2-mean(vg$vg2))/sd(vg$vg2))+
        b3[4]*((vg$vg3-mean(vg$vg3))/sd(vg$vg3))+
        b4[4]*((vg$vg4-mean(vg$vg4))/sd(vg$vg4))+
        b5[4]*((vg$vg5-mean(vg$vg5))/sd(vg$vg5))






    }############################################################################
    if (analysis==3 & traits ==6){
      cat("
Calculating Index")
      cat("
      ")
      cat("Animals:","
  ")
      print(nrow(animals))



      #Avalia??o 1

      r1<- renadd_1[,c(1,10,2,3)]

      colnames(r1)<-c("coda","ID","codp","codm")
      r1<-r1[ order(r1$coda,decreasing=c(FALSE)), ]
      r1 <- as.data.frame(r1[2])
      ren1<-r1


      colnames(sol_1)<- c("trait","effect","ID","solution","se")


      #Avalia??o 2

      r2<- renadd_2[,c(1,10,2,3)]
      rm(renadd_2)
      colnames(r2)<-c("coda","ID","codp","codm")
      r2<-r2[ order(r2$coda,decreasing=c(FALSE)), ]
      r2 <- as.data.frame(r2[2])
      ren1<-r1


      colnames(sol_2)<- c("trait","effect","ID","solution","se")



      #Avalia??o 3

      r3<- renadd_1[,c(1,10,2,3)]
      rm(renadd_3)
      colnames(r3)<-c("coda","ID","codp","codm")
      r3<-r3[ order(r3$coda,decreasing=c(FALSE)), ]
      r3 <- as.data.frame(r3[2])
      ren3<-r3


      colnames(sol_3)<- c("trait","effect","ID","solution","se")





      list<-list(sol_1,sol_2,sol_3)
      list_r<-list(r1,r2,r3)


      #VGs

      s1<- list[[b1[1]]]
      s2<- list[[b2[1]]]
      s3<- list[[b3[1]]]
      s4<- list[[b4[1]]]
      s5<- list[[b5[1]]]
      s6<- list[[b6[1]]]


      vg1<- s1[s1$effect == b1[2] & s1$trait == b1[3],]
      vg2<- s2[s2$effect == b2[2] & s2$trait == b2[3],]
      vg3<- s3[s3$effect == b3[2] & s3$trait == b3[3],]
      vg4<- s4[s4$effect == b4[2] & s4$trait == b4[3],]
      vg5<- s5[s5$effect == b5[2] & s5$trait == b5[3],]
      vg6<- s6[s6$effect == b6[2] & s6$trait == b6[3],]

      vg1 <- as.data.frame(vg1[,4])
      vg2 <- as.data.frame(vg2[,4])
      vg3 <- as.data.frame(vg3[,4])
      vg4 <- as.data.frame(vg4[,4])
      vg5 <- as.data.frame(vg5[,4])
      vg6 <- as.data.frame(vg6[,4])

      a1 <- list_r[[b1[1]]]
      a2 <- list_r[[b2[1]]]
      a3 <- list_r[[b3[1]]]
      a4 <- list_r[[b4[1]]]
      a5 <-list_r[[b5[1]]]
      a6 <-list_r[[b6[1]]]

      vg1$animal <- a1[,1]
      vg2$animal <- a2[,1]
      vg3$animal <- a3[,1]
      vg4$animal <- a4[,1]
      vg5$animal <- a5[,1]
      vg5$animal <- a6[,1]

      colnames(vg1)<-c("vg1","ID")
      colnames(vg2)<-c("vg2","ID")
      colnames(vg3)<-c("vg3","ID")
      colnames(vg4)<-c("vg4","ID")
      colnames(vg5)<-c("vg5","ID")
      colnames(vg6)<-c("vg6","ID")

      vg01 <- merge(vg1,vg2, by="ID")
      vg02 <- merge(vg01,vg3, by="ID")
      vg01 <- merge(vg02,vg4, by="ID")
      vg02 <- merge(vg01,vg5, by="ID")
      vg<- merge(vg02,vg6, by="ID")
      rm(vg02,vg01)

      vg$ind <- base + b1[4]*((vg$vg1-mean(vg$vg1))/sd(vg$vg1)) +
        b2[4]*((vg$vg2-mean(vg$vg2))/sd(vg$vg2))+
        b3[4]*((vg$vg3-mean(vg$vg3))/sd(vg$vg3))+
        b4[4]*((vg$vg4-mean(vg$vg4))/sd(vg$vg4))+
        b5[4]*((vg$vg5-mean(vg$vg5))/sd(vg$vg5))+
        b6[4]*((vg$vg6-mean(vg$vg6))/sd(vg$vg6))






    }
    if(analysis==2 & traits ==2){
      cat("
Calculating Index")
      cat("
      ")
      cat("Animals:","
  ")
      print(nrow(animals))



      #Avalia??o 1

      r1<- renadd_1[,c(1,10,2,3)]

      colnames(r1)<-c("coda","ID","codp","codm")
      r1<-r1[ order(r1$coda,decreasing=c(FALSE)), ]
      r1 <- as.data.frame(r1[2])
      ren1<-r1


      colnames(sol_1)<- c("trait","effect","ID","solution","se")


      #Avalia??o 2

      r2<- renadd_2[,c(1,10,2,3)]
      rm(renadd_2)
      colnames(r2)<-c("coda","ID","codp","codm")
      r2<-r2[ order(r2$coda,decreasing=c(FALSE)), ]
      r2<- as.data.frame(r2[2])
      ren1<-r1


      colnames(sol_2)<- c("trait","effect","ID","solution","se")



      list<-list(sol_1,sol_2)
      list_r<-list(r1,r2)


      #VGs

      s1<- list[[b1[1]]]
      s2<- list[[b2[1]]]






      vg1<- s1[s1$effect == b1[2] & s1$trait == b1[3],]
      vg2<- s2[s2$effect == b2[2] & s2$trait == b2[3],]




      vg1 <- as.data.frame(vg1[,4])
      vg2 <- as.data.frame(vg2[,4])



      a1 <- list_r[[b1[1]]]
      a2 <- list_r[[b2[1]]]


      vg1$animal <- a1[,1]
      vg2$animal <- a2[,1]


      colnames(vg1)<-c("vg1","ID")
      colnames(vg2)<-c("vg2","ID")


      vg <- merge(vg1,vg2, by="ID")



      vg$ind <- base + b1[4]*((vg$vg1-mean(vg$vg1))/sd(vg$vg1)) +
        b2[4]*((vg$vg2-mean(vg$vg2))/sd(vg$vg2))




    } ########################################################


    if (analysis==2 & traits ==3){
      cat("
Calculating Index")
      cat("
      ")
      cat("Animals:","
  ")
      print(nrow(animals))



      #Avalia??o 1

      r1<- renadd_1[,c(1,10,2,3)]

      colnames(r1)<-c("coda","ID","codp","codm")
      r1<-r1[ order(r1$coda,decreasing=c(FALSE)), ]
      r1 <- as.data.frame(r1[2])
      ren1<-r1


      colnames(sol_1)<- c("trait","effect","ID","solution","se")


      #Avalia??o 2

      r2<- renadd_2[,c(1,10,2,3)]
      rm(renadd_2)
      colnames(r2)<-c("coda","ID","codp","codm")
      r2<-r2[ order(r2$coda,decreasing=c(FALSE)), ]
      r2<- as.data.frame(r2[2])
      ren1<-r1


      colnames(sol_2)<- c("trait","effect","ID","solution","se")



      list<-list(sol_1,sol_2)
      list_r<-list(r1,r2)


      #VGs

      s1<- list[[b1[1]]]
      s2<- list[[b2[1]]]
      s3<- list[[b3[1]]]





      vg1<- s1[s1$effect == b1[2] & s1$trait == b1[3],]
      vg2<- s2[s2$effect == b2[2] & s2$trait == b2[3],]
      vg3<- s3[s3$effect == b3[2] & s3$trait == b3[3],]



      vg1 <- as.data.frame(vg1[,4])
      vg2 <- as.data.frame(vg2[,4])
      vg3 <- as.data.frame(vg3[,4])



      a1 <- list_r[[b1[1]]]
      a2 <- list_r[[b2[1]]]
      a3 <- list_r[[b3[1]]]


      vg1$animal <- a1[,1]
      vg2$animal <- a2[,1]
      vg3$animal <- a3[,1]


      colnames(vg1)<-c("vg1","ID")
      colnames(vg2)<-c("vg2","ID")
      colnames(vg3)<-c("vg3","ID")

      vg01 <- merge(vg1,vg2, by="ID")
      vg <- merge(vg01,vg3, by="ID")

      rm(vg01)

      vg$ind <- base + b1[4]*((vg$vg1-mean(vg$vg1))/sd(vg$vg1)) +
        b2[4]*((vg$vg2-mean(vg$vg2))/sd(vg$vg2))+
        b3[4]*((vg$vg3-mean(vg$vg3))/sd(vg$vg3))

    } #############################################################################
    if (analysis==2 & traits ==4){
      cat("
Calculating Index")
      cat("
      ")
      cat("Animals:","
  ")
      print(nrow(animals))

      #Avalia??o 1

      r1<- renadd_1[,c(1,10,2,3)]

      colnames(r1)<-c("coda","ID","codp","codm")
      r1<-r1[ order(r1$coda,decreasing=c(FALSE)), ]
      r1 <- as.data.frame(r1[2])
      ren1<-r1


      colnames(sol_1)<- c("trait","effect","ID","solution","se")


      #Avalia??o 2

      r2<- renadd_2[,c(1,10,2,3)]
      rm(renadd_2)
      colnames(r2)<-c("coda","ID","codp","codm")
      r2<-r2[ order(r2$coda,decreasing=c(FALSE)), ]
      r2<- as.data.frame(r2[2])
      ren1<-r1


      colnames(sol_2)<- c("trait","effect","ID","solution","se")



      list<-list(sol_1,sol_2)
      list_r<-list(r1,r2)


      #VGs

      s1<- list[[b1[1]]]
      s2<- list[[b2[1]]]
      s3<- list[[b3[1]]]
      s4<- list[[b4[1]]]




      vg1<- s1[s1$effect == b1[2] & s1$trait == b1[3],]
      vg2<- s2[s2$effect == b2[2] & s2$trait == b2[3],]
      vg3<- s3[s3$effect == b3[2] & s3$trait == b3[3],]
      vg4<- s4[s4$effect == b4[2] & s4$trait == b4[3],]


      vg1 <- as.data.frame(vg1[,4])
      vg2 <- as.data.frame(vg2[,4])
      vg3 <- as.data.frame(vg3[,4])
      vg4 <- as.data.frame(vg4[,4])



      a1 <- list_r[[b1[1]]]
      a2 <- list_r[[b2[1]]]
      a3 <- list_r[[b3[1]]]
      a4 <- list_r[[b4[1]]]


      vg1$animal <- a1[,1]
      vg2$animal <- a2[,1]
      vg3$animal <- a3[,1]
      vg4$animal <- a4[,1]


      colnames(vg1)<-c("vg1","ID")
      colnames(vg2)<-c("vg2","ID")
      colnames(vg3)<-c("vg3","ID")
      colnames(vg4)<-c("vg4","ID")


      vg01 <- merge(vg1,vg2, by="ID")
      vg02 <- merge(vg01,vg3, by="ID")
      vg <- merge(vg02,vg4, by="ID")

      rm(vg02,vg01)

      vg$ind <- base + b1[4]*((vg$vg1-mean(vg$vg1))/sd(vg$vg1)) +
        b2[4]*((vg$vg2-mean(vg$vg2))/sd(vg$vg2))+
        b3[4]*((vg$vg3-mean(vg$vg3))/sd(vg$vg3))+
        b4[4]*((vg$vg4-mean(vg$vg4))/sd(vg$vg4))


    } #################################################################################
    if (analysis==2 & traits ==5){
      cat("
Calculating Index")
      cat("
      ")
      cat("Animals:","
  ")
      print(nrow(animals))


      #Avalia??o 1

      r1<- renadd_1[,c(1,10,2,3)]

      colnames(r1)<-c("coda","ID","codp","codm")
      r1<-r1[ order(r1$coda,decreasing=c(FALSE)), ]
      r1 <- as.data.frame(r1[2])
      ren1<-r1


      colnames(sol_1)<- c("trait","effect","ID","solution","se")


      #Avalia??o 2

      r2<- renadd_2[,c(1,10,2,3)]
      rm(renadd_2)
      colnames(r2)<-c("coda","ID","codp","codm")
      r2<-r2[ order(r2$coda,decreasing=c(FALSE)), ]
      r2<- as.data.frame(r2[2])
      ren1<-r1


      colnames(sol_2)<- c("trait","effect","ID","solution","se")



      list<-list(sol_1,sol_2)
      list_r<-list(r1,r2)


      #VGs

      s1<- list[[b1[1]]]
      s2<- list[[b2[1]]]
      s3<- list[[b3[1]]]
      s4<- list[[b4[1]]]
      s5<- list[[b5[1]]]



      vg1<- s1[s1$effect == b1[2] & s1$trait == b1[3],]
      vg2<- s2[s2$effect == b2[2] & s2$trait == b2[3],]
      vg3<- s3[s3$effect == b3[2] & s3$trait == b3[3],]
      vg4<- s4[s4$effect == b4[2] & s4$trait == b4[3],]
      vg5<- s5[s5$effect == b5[2] & s5$trait == b5[3],]


      vg1 <- as.data.frame(vg1[,4])
      vg2 <- as.data.frame(vg2[,4])
      vg3 <- as.data.frame(vg3[,4])
      vg4 <- as.data.frame(vg4[,4])
      vg5 <- as.data.frame(vg5[,4])


      a1 <- list_r[[b1[1]]]
      a2 <- list_r[[b2[1]]]
      a3 <- list_r[[b3[1]]]
      a4 <- list_r[[b4[1]]]
      a5 <-list_r[[b5[1]]]

      vg1$animal <- a1[,1]
      vg2$animal <- a2[,1]
      vg3$animal <- a3[,1]
      vg4$animal <- a4[,1]
      vg5$animal <- a5[,1]

      colnames(vg1)<-c("vg1","ID")
      colnames(vg2)<-c("vg2","ID")
      colnames(vg3)<-c("vg3","ID")
      colnames(vg4)<-c("vg4","ID")
      colnames(vg5)<-c("vg5","ID")

      vg01 <- merge(vg1,vg2, by="ID")
      vg02 <- merge(vg01,vg3, by="ID")
      vg01 <- merge(vg02,vg4, by="ID")
      vg <- merge(vg01,vg5, by="ID")
      rm(vg02,vg01)

    vg$ind <- base + b1[4]*((vg$vg1-mean(vg$vg1))/sd(vg$vg1)) +
        b2[4]*((vg$vg2-mean(vg$vg2))/sd(vg$vg2))+
      b3[4]*((vg$vg3-mean(vg$vg3))/sd(vg$vg3))+
      b4[4]*((vg$vg4-mean(vg$vg4))/sd(vg$vg4))+
      b5[4]*((vg$vg5-mean(vg$vg5))/sd(vg$vg5))



    }  ################################################################
    if (analysis==2 & traits ==6){
      cat("
Calculating Index")
      cat("
      ")
      cat("Animals:","
  ")
      print(nrow(animals))



      #Avalia??o 1

      r1<- renadd_1[,c(1,10,2,3)]

      colnames(r1)<-c("coda","ID","codp","codm")
      r1<-r1[ order(r1$coda,decreasing=c(FALSE)), ]
      r1 <- as.data.frame(r1[2])
      ren1<-r1


      colnames(sol_1)<- c("trait","effect","ID","solution","se")


      #Avalia??o 2

      r2<- renadd_2[,c(1,10,2,3)]
      rm(renadd_2)
      colnames(r2)<-c("coda","ID","codp","codm")
      r2<-r2[ order(r2$coda,decreasing=c(FALSE)), ]
      r2<- as.data.frame(r2[2])
      ren1<-r1


      colnames(sol_2)<- c("trait","effect","ID","solution","se")



      list<-list(sol_1,sol_2)
      list_r<-list(r1,r2)


      #VGs

      s1<- list[[b1[1]]]
      s2<- list[[b2[1]]]
      s3<- list[[b3[1]]]
      s4<- list[[b4[1]]]
      s5<- list[[b5[1]]]
      s6<- list[[b6[1]]]


      vg1<- s1[s1$effect == b1[2] & s1$trait == b1[3],]
      vg2<- s2[s2$effect == b2[2] & s2$trait == b2[3],]
      vg3<- s3[s3$effect == b3[2] & s3$trait == b3[3],]
      vg4<- s4[s4$effect == b4[2] & s4$trait == b4[3],]
      vg5<- s5[s5$effect == b5[2] & s5$trait == b5[3],]
      vg6<- s6[s6$effect == b6[2] & s6$trait == b6[3],]

      vg1 <- as.data.frame(vg1[,4])
      vg2 <- as.data.frame(vg2[,4])
      vg3 <- as.data.frame(vg3[,4])
      vg4 <- as.data.frame(vg4[,4])
      vg5 <- as.data.frame(vg5[,4])
      vg6 <- as.data.frame(vg6[,4])

      a1 <- list_r[[b1[1]]]
      a2 <- list_r[[b2[1]]]
      a3 <- list_r[[b3[1]]]
      a4 <- list_r[[b4[1]]]
      a5 <-list_r[[b5[1]]]
      a6 <-list_r[[b6[1]]]

      vg1$animal <- a1[,1]
      vg2$animal <- a2[,1]
      vg3$animal <- a3[,1]
      vg4$animal <- a4[,1]
      vg5$animal <- a5[,1]
      vg5$animal <- a6[,1]

      colnames(vg1)<-c("vg1","ID")
      colnames(vg2)<-c("vg2","ID")
      colnames(vg3)<-c("vg3","ID")
      colnames(vg4)<-c("vg4","ID")
      colnames(vg5)<-c("vg5","ID")
      colnames(vg6)<-c("vg6","ID")

      vg01 <- merge(vg1,vg2, by="ID")
      vg02 <- merge(vg01,vg3, by="ID")
      vg01 <- merge(vg02,vg4, by="ID")
      vg02 <- merge(vg01,vg5, by="ID")
      vg<- merge(vg02,vg6, by="ID")
      rm(vg02,vg01)

      vg$ind <- base + b1[4]*((vg$vg1-mean(vg$vg1))/sd(vg$vg1)) +
        b2[4]*((vg$vg2-mean(vg$vg2))/sd(vg$vg2))+
        b3[4]*((vg$vg3-mean(vg$vg3))/sd(vg$vg3))+
        b4[4]*((vg$vg4-mean(vg$vg4))/sd(vg$vg4))+
        b5[4]*((vg$vg5-mean(vg$vg5))/sd(vg$vg5))+
        b6[4]*((vg$vg6-mean(vg$vg6))/sd(vg$vg6))



    }
    if(analysis==1 & traits ==1){
      cat("
Calculating Index")
      cat("
      ")
      cat("Animals:","
  ")
      print(nrow(animals))



      #Avalia??o 1

      r1<- renadd_1[,c(1,10,2,3)]

      colnames(r1)<-c("coda","ID","codp","codm")
      r1<-r1[ order(r1$coda,decreasing=c(FALSE)), ]
      r1 <- as.data.frame(r1[2])
      ren1<-r1


      colnames(sol_1)<- c("trait","effect","ID","solution","se")

      list<-list(sol_1)
      list_r<-list(r1)


      #VGs

      s1<- list[[b1[1]]]


      vg1<- s1[s1$effect == b1[2] & s1$trait == b1[3],]


      vg1 <- as.data.frame(vg1[,4])

      a1 <- list_r[[b1[1]]]


      vg1$animal <- a1[,1]

      vg<-vg1[,c("ID","vg1")]
      colnames(vg)[2]<-"ind"




    }#######################################################

    if (analysis==1 & traits ==2){
      cat("
Calculating Index")
      cat("
      ")
      cat("Animals:","
  ")
      print(nrow(animals))


      #Avalia??o 1

      r1<- renadd_1[,c(1,10,2,3)]

      colnames(r1)<-c("coda","ID","codp","codm")
      r1<-r1[ order(r1$coda,decreasing=c(FALSE)), ]
      r1 <- as.data.frame(r1[2])
      ren1<-r1


      colnames(sol_1)<- c("trait","effect","ID","solution","se")

      list<-list(sol_1)
      list_r<-list(r1)


      #VGs

      s1<- list[[b1[1]]]
      s2<- list[[b2[1]]]




      vg1<- s1[s1$effect == b1[2] & s1$trait == b1[3],]
      vg2<- s2[s2$effect == b2[2] & s2$trait == b2[3],]




      vg1 <- as.data.frame(vg1[,4])
      vg2 <- as.data.frame(vg2[,4])




      a1 <- list_r[[b1[1]]]
      a2 <- list_r[[b2[1]]]


      vg1$animal <- a1[,1]
      vg2$animal <- a2[,1]


      colnames(vg1)<-c("vg1","ID")
      colnames(vg2)<-c("vg2","ID")


      vg <- merge(vg1,vg2, by="ID")

      vg$ind <- base + b1[4]*((vg$vg1-mean(vg$vg1))/sd(vg$vg1)) +
        b2[4]*((vg$vg2-mean(vg$vg2))/sd(vg$vg2))
    } ###########################################################################
    if (analysis==1 & traits ==3){
      cat("
Calculating Index")
      cat("
      ")
      cat("Animals:","
  ")
      print(nrow(animals))

      #Avalia??o 1

      r1<- renadd_1[,c(1,10,2,3)]

      colnames(r1)<-c("coda","ID","codp","codm")
      r1<-r1[ order(r1$coda,decreasing=c(FALSE)), ]
      r1 <- as.data.frame(r1[2])
      ren1<-r1


      colnames(sol_1)<- c("trait","effect","ID","solution","se")

      list<-list(sol_1)
      list_r<-list(r1)


      #VGs

      s1<- list[[b1[1]]]
      s2<- list[[b2[1]]]
      s3<- list[[b3[1]]]




      vg1<- s1[s1$effect == b1[2] & s1$trait == b1[3],]
      vg2<- s2[s2$effect == b2[2] & s2$trait == b2[3],]
      vg3<- s3[s3$effect == b3[2] & s3$trait == b3[3],]



      vg1 <- as.data.frame(vg1[,4])
      vg2 <- as.data.frame(vg2[,4])
      vg3 <- as.data.frame(vg3[,4])




      a1 <- list_r[[b1[1]]]
      a2 <- list_r[[b2[1]]]
      a3 <- list_r[[b3[1]]]


      vg1$animal <- a1[,1]
      vg2$animal <- a2[,1]
      vg3$animal <- a3[,1]


      colnames(vg1)<-c("vg1","ID")
      colnames(vg2)<-c("vg2","ID")
      colnames(vg3)<-c("vg3","ID")


      vg01 <- merge(vg1,vg2, by="ID")
      vg <- merge(vg01,vg3, by="ID")

      rm(vg01)

      vg$ind <- base + b1[4]*((vg$vg1-mean(vg$vg1))/sd(vg$vg1)) +
        b2[4]*((vg$vg2-mean(vg$vg2))/sd(vg$vg2))+
        b3[4]*((vg$vg3-mean(vg$vg3))/sd(vg$vg3))
    } ##########################################################
    if (analysis==1 & traits ==4){
      cat("
Calculating Index")
      cat("
      ")
      cat("Animals:","
  ")
      print(nrow(animals))


      #Avalia??o 1

      r1<- renadd_1[,c(1,10,2,3)]

      colnames(r1)<-c("coda","ID","codp","codm")
      r1<-r1[ order(r1$coda,decreasing=c(FALSE)), ]
      r1 <- as.data.frame(r1[2])
      ren1<-r1


      colnames(sol_1)<- c("trait","effect","ID","solution","se")

      list<-list(sol_1)
      list_r<-list(r1)


      #VGs

      s1<- list[[b1[1]]]
      s2<- list[[b2[1]]]
      s3<- list[[b3[1]]]
      s4<- list[[b4[1]]]



      vg1<- s1[s1$effect == b1[2] & s1$trait == b1[3],]
      vg2<- s2[s2$effect == b2[2] & s2$trait == b2[3],]
      vg3<- s3[s3$effect == b3[2] & s3$trait == b3[3],]
      vg4<- s4[s4$effect == b4[2] & s4$trait == b4[3],]



      vg1 <- as.data.frame(vg1[,4])
      vg2 <- as.data.frame(vg2[,4])
      vg3 <- as.data.frame(vg3[,4])
      vg4 <- as.data.frame(vg4[,4])



      a1 <- list_r[[b1[1]]]
      a2 <- list_r[[b2[1]]]
      a3 <- list_r[[b3[1]]]
      a4 <- list_r[[b4[1]]]


      vg1$animal <- a1[,1]
      vg2$animal <- a2[,1]
      vg3$animal <- a3[,1]
      vg4$animal <- a4[,1]

      colnames(vg1)<-c("vg1","ID")
      colnames(vg2)<-c("vg2","ID")
      colnames(vg3)<-c("vg3","ID")
      colnames(vg4)<-c("vg4","ID")


      vg01 <- merge(vg1,vg2, by="ID")
      vg02 <- merge(vg01,vg3, by="ID")
      vg <- merge(vg02,vg4, by="ID")

      rm(vg02,vg01)

      vg$ind <- base + b1[4]*((vg$vg1-mean(vg$vg1))/sd(vg$vg1)) +
        b2[4]*((vg$vg2-mean(vg$vg2))/sd(vg$vg2))+
        b3[4]*((vg$vg3-mean(vg$vg3))/sd(vg$vg3))+
        b4[4]*((vg$vg4-mean(vg$vg4))/sd(vg$vg4))






    } #################################################
    if (analysis==1 & traits ==5){
      cat("
Calculating Index")
      cat("
      ")
      cat("Animals:","
  ")
      print(nrow(animals))



      #Avalia??o 1

      r1<- renadd_1[,c(1,10,2,3)]

      colnames(r1)<-c("coda","ID","codp","codm")
      r1<-r1[ order(r1$coda,decreasing=c(FALSE)), ]
      r1 <- as.data.frame(r1[2])
      ren1<-r1


      colnames(sol_1)<- c("trait","effect","ID","solution","se")

      list<-list(sol_1)
      list_r<-list(r1)


      #VGs

      s1<- list[[b1[1]]]
      s2<- list[[b2[1]]]
      s3<- list[[b3[1]]]
      s4<- list[[b4[1]]]
      s5<- list[[b5[1]]]


      vg1<- s1[s1$effect == b1[2] & s1$trait == b1[3],]
      vg2<- s2[s2$effect == b2[2] & s2$trait == b2[3],]
      vg3<- s3[s3$effect == b3[2] & s3$trait == b3[3],]
      vg4<- s4[s4$effect == b4[2] & s4$trait == b4[3],]
      vg5<- s5[s5$effect == b5[2] & s5$trait == b5[3],]


      vg1 <- as.data.frame(vg1[,4])
      vg2 <- as.data.frame(vg2[,4])
      vg3 <- as.data.frame(vg3[,4])
      vg4 <- as.data.frame(vg4[,4])
      vg5 <- as.data.frame(vg5[,4])


      a1 <- list_r[[b1[1]]]
      a2 <- list_r[[b2[1]]]
      a3 <- list_r[[b3[1]]]
      a4 <- list_r[[b4[1]]]
      a5 <-list_r[[b5[1]]]
      a6 <-list_r[[b6[1]]]

      vg1$animal <- a1[,1]
      vg2$animal <- a2[,1]
      vg3$animal <- a3[,1]
      vg4$animal <- a4[,1]
      vg5$animal <- a5[,1]

      colnames(vg1)<-c("vg1","ID")
      colnames(vg2)<-c("vg2","ID")
      colnames(vg3)<-c("vg3","ID")
      colnames(vg4)<-c("vg4","ID")
      colnames(vg5)<-c("vg5","ID")


      vg01 <- merge(vg1,vg2, by="ID")
      vg02 <- merge(vg01,vg3, by="ID")
      vg01 <- merge(vg02,vg4, by="ID")
      vg <- merge(vg01,vg5, by="ID")

      rm(vg02,vg01)

      vg$ind <- base + b1[4]*((vg$vg1-mean(vg$vg1))/sd(vg$vg1)) +
        b2[4]*((vg$vg2-mean(vg$vg2))/sd(vg$vg2))+
        b3[4]*((vg$vg3-mean(vg$vg3))/sd(vg$vg3))+
        b4[4]*((vg$vg4-mean(vg$vg4))/sd(vg$vg4))+
        b5[4]*((vg$vg5-mean(vg$vg5))/sd(vg$vg5))



    } ########################################################################
    if (analysis==1 & traits ==6){
      cat("
Calculating Index")
      cat("
      ")
      cat("Animals:","
  ")
      print(nrow(animals))


      #Avalia??o 1

      r1<- renadd_1[,c(1,10,2,3)]

      colnames(r1)<-c("coda","ID","codp","codm")
      r1<-r1[ order(r1$coda,decreasing=c(FALSE)), ]
      r1 <- as.data.frame(r1[2])
      ren1<-r1


      colnames(sol_1)<- c("trait","effect","ID","solution","se")

      list<-list(sol_1)
      list_r<-list(r1)


      #VGs

      s1<- list[[b1[1]]]
      s2<- list[[b2[1]]]
      s3<- list[[b3[1]]]
      s4<- list[[b4[1]]]
      s5<- list[[b5[1]]]
      s6<- list[[b6[1]]]


      vg1<- s1[s1$effect == b1[2] & s1$trait == b1[3],]
      vg2<- s2[s2$effect == b2[2] & s2$trait == b2[3],]
      vg3<- s3[s3$effect == b3[2] & s3$trait == b3[3],]
      vg4<- s4[s4$effect == b4[2] & s4$trait == b4[3],]
      vg5<- s5[s5$effect == b5[2] & s5$trait == b5[3],]
      vg6<- s6[s6$effect == b6[2] & s6$trait == b6[3],]

      vg1 <- as.data.frame(vg1[,4])
      vg2 <- as.data.frame(vg2[,4])
      vg3 <- as.data.frame(vg3[,4])
      vg4 <- as.data.frame(vg4[,4])
      vg5 <- as.data.frame(vg5[,4])
      vg6 <- as.data.frame(vg6[,4])

      a1 <- list_r[[b1[1]]]
      a2 <- list_r[[b2[1]]]
      a3 <- list_r[[b3[1]]]
      a4 <- list_r[[b4[1]]]
      a5 <-list_r[[b5[1]]]
      a6 <-list_r[[b6[1]]]

      vg1$animal <- a1[,1]
      vg2$animal <- a2[,1]
      vg3$animal <- a3[,1]
      vg4$animal <- a4[,1]
      vg5$animal <- a5[,1]
      vg5$animal <- a6[,1]

      colnames(vg1)<-c("vg1","ID")
      colnames(vg2)<-c("vg2","ID")
      colnames(vg3)<-c("vg3","ID")
      colnames(vg4)<-c("vg4","ID")
      colnames(vg5)<-c("vg5","ID")
      colnames(vg6)<-c("vg6","ID")

      vg01 <- merge(vg1,vg2, by="ID")
      vg02 <- merge(vg01,vg3, by="ID")
      vg01 <- merge(vg02,vg4, by="ID")
      vg02 <- merge(vg01,vg5, by="ID")
      vg<- merge(vg02,vg6, by="ID")
      rm(vg02,vg01)

      vg$ind <- base + b1[4]*((vg$vg1-mean(vg$vg1))/sd(vg$vg1)) +
        b2[4]*((vg$vg2-mean(vg$vg2))/sd(vg$vg2))+
        b3[4]*((vg$vg3-mean(vg$vg3))/sd(vg$vg3))+
        b4[4]*((vg$vg4-mean(vg$vg4))/sd(vg$vg4))+
        b5[4]*((vg$vg5-mean(vg$vg5))/sd(vg$vg5))+
        b6[4]*((vg$vg6-mean(vg$vg6))/sd(vg$vg6))






    }


    vg<-vg[c("ID","ind")]
    colnames(vg) <- c("ID","Index")
    animais<-animals
    animals<-animals[,1:3]
    colnames(animals)[1]<-"Tag"
    vg_animal <- merge(animals,vg,by="ID",all="T")
    vg_animal<-vg_animal[!(is.na(vg_animal$Tag)),]
    vg_animal <-vg_animal [ order(vg_animal$Ind,decreasing=c(TRUE)), ]
   row.names(vg_animal)<-1:nrow(vg_animal)
   colnames(vg_animal)<-(c("ID","Tag","Cat","Index"))
   vg_animal<-vg_animal[,c("Tag","ID","Cat","Index")]
   vg_animal$Cat<-as.factor(vg_animal$Cat)
   Rank<-vg_animal
   Rank<-Rank[,c("Tag","ID","Cat","Index")]
   Rank$Index<-round( Rank$Index, digits = 2)

   return(Rank)
   cat("Ranking...ok!")
    time_2 = Sys.time()
    cat("

    ","Completed!


        ", "TIME:",(time_2 - time_1) )
    rm(list = ls())

  }
  ###################################################ENDOG
  endog<- function(animais,pedigree,end){
    if(is.null(pedigree)|end==FALSE){

      return(animais)
      cat("")
    }else{



      colnames(pedigree)[1:4]<-c("ID", "Sire", "Dam","Original" )
      cod<-pedigree[,c(1,4)]
      pedigree<-pedigree[,1:3]
      pedigree$ID<-as.character(pedigree$ID)
      pedigree$Dam<-as.character(pedigree$Dam)
      pedigree$Sire<-as.character(pedigree$Sire)
      ped<-pedigree


      ped <- data.frame(ped)
      ped<-ped[,c("ID","Dam","Sire")]



      ped2<-pedigree(sire = ped$Sire,ped$Dam,label=ped$ID)


      ped$inb<-  as.numeric(inbreeding(ped2))


      df<-ped[,c("ID","inb")]
      colnames(df)[2]<-"Inb"
      df1<-merge(df,cod,by="ID")
      df<-merge(animais,df1,by.x="ID",by.y ="Original" )
      df$ID.y<-NULL
      rm(ped2,df1)

      df$Inb<-as.numeric(df$Inb)
      df$Inb<-df$Inb*100
      df$Inb<-round(df$Inb, digits=2)
      Rank<-df[,c("Tag","ID","Cat","Index","Inb")]
      return(Rank)
      cat("Inbreeding...ok!")
    }

  }
  ############################################ REC
  reco<- function(animais,rec,recom,male){
    if(is.null(rec) | recom==FALSE){

      return(animais)
      cat("Rcommendation ...ok!")
    }
    if(!(is.null(rec)) & recom==TRUE & male==FALSE){
      rec<-rec[,c(1,9)]
      colnames(rec)[1]<-"Tag"
      rec<-rec[!(duplicated(rec$Tag)),]
      df<- merge(animais,rec, by="Tag",all=T)
      df<-df[!(is.na(df$ID)),]
      return(df)
      cat("Rcommendation ...ok!")
    }
    if(!(is.null(rec)) & recom==TRUE & male==TRUE){
      rec<-rec[,c(3,10)]
      colnames(rec)[1]<-"Tag"
      rec<-rec[!(duplicated(rec$Tag)),]
      df<- merge(animais,rec, by="Tag",all=T)
      df<-df[!(is.na(df$ID)),]
      return(df)
      cat("Rcommendation ...ok!")
    }
  }

  pheno<- function(animais,phen,useF){
      if(is.null(phen)| useF ==FALSE){
        return(animais)
        cat("Phenotype...ok!")
      }else{
        colnames(phen)[2]<-"ID"
        colnames(animais)[2]<-"ID"
        df<- merge(animais,phen, by="ID",all=T)
        return(df)
        cat("Phenotype...ok!")
      }}

  count<- function(animais,pedigree,countprog){
        if(is.null(pedigree) | countprog==FALSE){ return(animais)
          cat("Count...ok!")}

        else{
          colnames(pedigree)[1:4]<-c("ID", "Sire", "Dam","Original" )
          cod<-pedigree[,c(1,4)]
          ped <- pedigree
          ped$ID<-as.character(ped$ID)
          ped$Dam<-as.character(ped$Dam)
          ped$Sire<-as.character(ped$Sire)
          df<-merge(animais,ped,by.x="ID",by.y = "Original",all=T)
          df$ID.y<-NULL
          df<-df[!(is.na(df$Tag)),]
          colnames(cod)[2]<-"ID_Sire"
          df1<-merge(df,cod,by.x="Sire",by.y = "ID",all=T)
          df1<-df1[!(is.na(df1$Tag)),]
          colnames(cod)[2]<-"ID_Dam"
          df<-merge(df1,cod,by.x="Dam",by.y = "ID",all=T)
          df<-df[!(is.na(df$Tag)),]

          df$Sire<-NULL
          df$Dam <-NULL

          count<-table(df$ID_Sire)
          count<-as.data.frame(count)
          colnames(count)[1]<-"ID_Sire"
          colnames(count)[2]<-"N_progeny"
          df2<-merge(df,count,by="ID_Sire")
          df2$Class <- 1
          df2<-df2[ order(df2$ID_Sire,df2$Index,decreasing=c(TRUE)), ]
          for(i in 2:nrow(df2)){
            if(df2$ID_Sire[i] == df2$ID_Sire[i-1]){
              df2$Class[i] = df2$Class[i-1] + 1}
          }

          col_order <- colnames(df2)
          col_order <- col_order[!(col_order %in% (c("ID_Sire","ID_Dam","Tag","ID","Index"))) ]
          df2<-df2[,c("Tag","ID","ID_Sire","ID_Dam","Index", col_order)]
          cat("Count...ok!")
          return(df2)
        }}



  ###############################FINAL
#
    rankmulti <- function(animal,sol,ren,breed,base,useB,i1,i2,i3,i4,i5,i6,i7,effect,bn,fixed,ano1,ano2){

      if(is.null(animal))
        return(NULL)
      colnames(animal)[1]<-"Tag"
      animal$Tag<-as.character(animal$Tag)
      if(is.null(sol))
        return(NULL)
      if(is.null(ren))
        return(NULL)
      if(is.null(effect))
        return(NULL)
      if(is.null(i1))
        return(NULL)
      if(fixed==TRUE & useB ==TRUE){
        if(is.null(breed)){
          return(NULL)}
        if(is.null(ano1)){
          return(NULL)}
        if(is.null(ano2)){
          return(NULL)}
        }
      if(useB == TRUE){

        if(is.null(breed)){

          return(NULL)}
        else{
            colnames(animal)[1:2]<-c("Tag","ID")
            animal<-animal[,1:2]
            colnames(breed)[1:5]<-c("ID","Sire","Dam","Cat","Year")
            breed<-breed[,1:5]
            animais<-merge(animal,breed,by="ID")
            animais<-animais[,c("Tag","ID","Cat")]
            breed<-breed[,c(1,5)]
         }

        }
      if(useB == FALSE){
        colnames(animal)[1:3]<-c("Tag","ID","Cat")
        animais<-animal[1:3]
        }



      r1<- ren[,c(1,10,2,3)]

      colnames(r1)<-c("coda","ID","codp","codm")
      r1<-r1[ order(r1$coda,decreasing=c(FALSE)), ]
      r1 <- as.data.frame(r1[2])
      ren<-r1
      colnames(sol)<- c("trait","effect","ID","solution","se")


      if(bn == 1){
        vg1<- sol[sol$effect == effect & sol$trait == 1,]
        df<-as.data.frame(vg1)
        df$ID<-ren$ID
        if(fixed==TRUE & is.null(ano1)==FALSE & is.null(ano2)==FALSE & useB ==TRUE){
          if(is.null(breed)){
            return(NULL)}
          df00<-merge(df,breed,by="ID")
          df00<-df00[!(is.na(df00$Year)),]
          df00 <- df00[df00$Year >= ano1 & df00$Year <= ano2,]
          meanvg1<-mean(df00$vg1,na.rm=TRUE)

          sdvg1<-sd(df00$vg1,na.rm=TRUE)

        }else{
          meanvg1<-mean(df$vg1,na.rm=TRUE)


          sdvg1<-sd(df$vg1,na.rm=TRUE)

        }
        df$t1<-(df$vg1-meanvg1)/sdvg1
        df<-df[,c("ID","t1")]
      }

      if(bn == 2){
        vg1<- sol[sol$effect == effect & sol$trait == 1,]
        vg2<- sol[sol$effect == effect & sol$trait == 2,]
        vg1<-vg1[,4]
        vg2<-vg2[,4]

        colnames(vg1)<-"vg1"
        colnames(vg2)<-"vg2"

        df<-cbind(vg1,vg2)
        df$ID<-ren$ID
        if(fixed==TRUE & is.null(ano1)==FALSE & is.null(ano2)==FALSE & useB ==TRUE){
          if(is.null(breed)){

            return(NULL)}
          df00<-merge(df,breed,by="ID")
          df00<-df00[!(is.na(df00$Year)),]
          df00 <- df00[df00$Year >= ano1 & df00$Year <= ano2,]
          meanvg1<-mean(df00$vg1,na.rm=TRUE)
          meanvg2<-mean(df00$vg2,na.rm=TRUE)


          sdvg1<-sd(df00$vg1,na.rm=TRUE)
          sdvg2<-sd(df00$vg2,na.rm=TRUE)

        }else{
          meanvg1<-mean(df$vg1,na.rm=TRUE)
          meanvg2<-mean(df$vg2,na.rm=TRUE)


          sdvg1<-sd(df$vg1,na.rm=TRUE)
          sdvg2<-sd(df$vg2,na.rm=TRUE)

        }



        df$t1<-(df$vg1-meanvg1)/sdvg1
        df$t2<-(df$vg2-meanvg2)/sdvg2
        df<-df[,c("ID","t1","t2")]

      }

      if(bn == 3){
        vg1<- sol[sol$effect == effect & sol$trait == 1,]
        vg2<- sol[sol$effect == effect & sol$trait == 2,]
        vg3<- sol[sol$effect == effect & sol$trait == 3,]
        vg1<-vg1[,4]
        vg2<-vg2[,4]
        vg3<-vg3[,4]

        colnames(vg1)<-"vg1"
        colnames(vg2)<-"vg2"
        colnames(vg3)<-"vg3"

        df<-cbind(vg1,vg2,vg3)
        df$ID<-ren$ID
        if(fixed==TRUE & is.null(ano1)==FALSE & is.null(ano2)==FALSE & useB ==TRUE){
          if(is.null(breed)){

            return(NULL)}
          df00<-merge(df,breed,by="ID")
          df00<-df00[!(is.na(df00$Year)),]
          df00 <- df00[df00$Year >= ano1 & df00$Year <= ano2,]
          meanvg1<-mean(df00$vg1,na.rm=TRUE)
          meanvg2<-mean(df00$vg2,na.rm=TRUE)
          meanvg3<-mean(df00$vg3,na.rm=TRUE)


          sdvg1<-sd(df00$vg1,na.rm=TRUE)
          sdvg2<-sd(df00$vg2,na.rm=TRUE)
          sdvg3<-sd(df00$vg3,na.rm=TRUE)

        }else{
          meanvg1<-mean(df$vg1,na.rm=TRUE)
          meanvg2<-mean(df$vg2,na.rm=TRUE)
          meanvg3<-mean(df$vg3,na.rm=TRUE)


          sdvg1<-sd(df$vg1,na.rm=TRUE)
          sdvg2<-sd(df$vg2,na.rm=TRUE)
          sdvg3<-sd(df$vg3,na.rm=TRUE)

        }
        df$t1<-(df$vg1-meanvg1)/sdvg1
        df$t2<-(df$vg2-meanvg2)/sdvg2
        df$t3<-(df$vg3-meanvg3)/sdvg3
        df<-df[,c("ID","t1","t2","t3")]
      }

      if(bn == 4){
        vg1<- sol[sol$effect == effect & sol$trait == 1,]
        vg2<- sol[sol$effect == effect & sol$trait == 2,]
        vg3<- sol[sol$effect == effect & sol$trait == 3,]
        vg4<- sol[sol$effect == effect & sol$trait == 4,]

        vg1<-vg1[,4]
        vg2<-vg2[,4]
        vg3<-vg3[,4]
        vg4<-vg4[,4]

        colnames(vg1)<-"vg1"
        colnames(vg2)<-"vg2"
        colnames(vg3)<-"vg3"
        colnames(vg4)<-"vg4"


        df<-cbind(vg1,vg2,vg3,vg4)
        df$ID<-ren$ID
        if(fixed==TRUE & is.null(ano1)==FALSE & is.null(ano2)==FALSE & useB ==TRUE){
          if(is.null(breed)){

            return(NULL)}
          df00<-merge(df,breed,by="ID")
          df00<-df00[!(is.na(df00$Year)),]
          df00 <- df00[df00$Year >= ano1 & df00$Year <= ano2,]
          meanvg1<-mean(df00$vg1,na.rm=TRUE)
          meanvg2<-mean(df00$vg2,na.rm=TRUE)
          meanvg3<-mean(df00$vg3,na.rm=TRUE)
          meanvg4<-mean(df00$vg4,na.rm=TRUE)


          sdvg1<-sd(df00$vg1,na.rm=TRUE)
          sdvg2<-sd(df00$vg2,na.rm=TRUE)
          sdvg3<-sd(df00$vg3,na.rm=TRUE)
          sdvg4<-sd(df00$vg4,na.rm=TRUE)

        }else{
          meanvg1<-mean(df$vg1,na.rm=TRUE)
          meanvg2<-mean(df$vg2,na.rm=TRUE)
          meanvg3<-mean(df$vg3,na.rm=TRUE)
          meanvg4<-mean(df$vg4,na.rm=TRUE)


          sdvg1<-sd(df$vg1,na.rm=TRUE)
          sdvg2<-sd(df$vg2,na.rm=TRUE)
          sdvg3<-sd(df$vg3,na.rm=TRUE)
          sdvg4<-sd(df$vg4,na.rm=TRUE)

        }
        df$t1<-(df$vg1-meanvg1)/sdvg1
        df$t2<-(df$vg2-meanvg2)/sdvg2
        df$t3<-(df$vg3-meanvg3)/sdvg3
        df$t4<-(df$vg4-meanvg4)/sdvg4
        df<-df[,c("ID","t1","t2","t3","t4")]
      }

      if(bn == 5){
        vg1<- sol[sol$effect == effect & sol$trait == 1,]
        vg2<- sol[sol$effect == effect & sol$trait == 2,]
        vg3<- sol[sol$effect == effect & sol$trait == 3,]
        vg4<- sol[sol$effect == effect & sol$trait == 4,]
        vg5<- sol[sol$effect == effect & sol$trait == 5,]

        vg1<-vg1[,4]
        vg2<-vg2[,4]
        vg3<-vg3[,4]
        vg4<-vg4[,4]
        vg5<-vg5[,4]
        colnames(vg1)<-"vg1"
        colnames(vg2)<-"vg2"
        colnames(vg3)<-"vg3"
        colnames(vg4)<-"vg4"
        colnames(vg5)<-"vg5"

        df<-cbind(vg1,vg2,vg3,vg4,vg5)
        df$ID<-ren$ID
        if(fixed==TRUE & is.null(ano1)==FALSE & is.null(ano2)==FALSE & useB ==TRUE){
          if(is.null(breed)){

            return(NULL)}
          df00<-merge(df,breed,by="ID")
          df00<-df00[!(is.na(df00$Year)),]
          df00 <- df00[df00$Year >= ano1 & df00$Year <= ano2,]

          meanvg1<-mean(df00$vg1,na.rm=TRUE)
          meanvg2<-mean(df00$vg2,na.rm=TRUE)
          meanvg3<-mean(df00$vg3,na.rm=TRUE)
          meanvg4<-mean(df00$vg4,na.rm=TRUE)
          meanvg5<-mean(df00$vg5,na.rm=TRUE)


          sdvg1<-sd(df00$vg1,na.rm=TRUE)
          sdvg2<-sd(df00$vg2,na.rm=TRUE)
          sdvg3<-sd(df00$vg3,na.rm=TRUE)
          sdvg4<-sd(df00$vg4,na.rm=TRUE)
          sdvg5<-sd(df00$vg5,na.rm=TRUE)

        }else{
          meanvg1<-mean(df$vg1,na.rm=TRUE)
          meanvg2<-mean(df$vg2,na.rm=TRUE)
          meanvg3<-mean(df$vg3,na.rm=TRUE)
          meanvg4<-mean(df$vg4,na.rm=TRUE)
          meanvg5<-mean(df$vg5,na.rm=TRUE)


          sdvg1<-sd(df$vg1,na.rm=TRUE)
          sdvg2<-sd(df$vg2,na.rm=TRUE)
          sdvg3<-sd(df$vg3,na.rm=TRUE)
          sdvg4<-sd(df$vg4,na.rm=TRUE)
          sdvg5<-sd(df$vg5,na.rm=TRUE)

        }

        df$t1<-(df$vg1-meanvg1)/sdvg1
        df$t2<-(df$vg2-meanvg2)/sdvg2
        df$t3<-(df$vg3-meanvg3)/sdvg3
        df$t4<-(df$vg4-meanvg4)/sdvg4
        df$t5<-(df$vg5-meanvg5)/sdvg5
        df<-df[,c("ID","t1","t2","t3","t4","t5")]

      }

      if(bn == 6){
        vg1<- sol[sol$effect == effect & sol$trait == 1,]
        vg2<- sol[sol$effect == effect & sol$trait == 2,]
        vg3<- sol[sol$effect == effect & sol$trait == 3,]
        vg4<- sol[sol$effect == effect & sol$trait == 4,]
        vg5<- sol[sol$effect == effect & sol$trait == 5,]
        vg6<- sol[sol$effect == effect & sol$trait == 6,]

        vg1<-vg1[,4]
        vg2<-vg2[,4]
        vg3<-vg3[,4]
        vg4<-vg4[,4]
        vg5<-vg5[,4]
        vg6<-vg6[,4]
        colnames(vg1)<-"vg1"
        colnames(vg2)<-"vg2"
        colnames(vg3)<-"vg3"
        colnames(vg4)<-"vg4"
        colnames(vg5)<-"vg5"
        colnames(vg6)<-"vg6"
        df<-cbind(vg1,vg2,vg3,vg4,vg5,vg6)
        df$ID<-ren$ID

        if(fixed==TRUE & is.null(ano1)==FALSE & is.null(ano2)==FALSE & useB ==TRUE){
          if(is.null(breed)){

            return(NULL)}
          df00<-merge(df,breed,by="ID")
          df00<-df00[!(is.na(df00$Year)),]
          df00 <- df00[df00$Year >= ano1 & df00$Year <= ano2,]
          meanvg1<-mean(df00$vg1,na.rm=TRUE)
          meanvg2<-mean(df00$vg2,na.rm=TRUE)
          meanvg3<-mean(df00$vg3,na.rm=TRUE)
          meanvg4<-mean(df00$vg4,na.rm=TRUE)
          meanvg5<-mean(df00$vg5,na.rm=TRUE)
          meanvg6<-mean(df00$vg6,na.rm=TRUE)

          sdvg1<-sd(df00$vg1,na.rm=TRUE)
          sdvg2<-sd(df00$vg2,na.rm=TRUE)
          sdvg3<-sd(df00$vg3,na.rm=TRUE)
          sdvg4<-sd(df00$vg4,na.rm=TRUE)
          sdvg5<-sd(df00$vg5,na.rm=TRUE)
          sdvg6<-sd(df00$vg6,na.rm=TRUE)
        }else{
          meanvg1<-mean(df$vg1,na.rm=TRUE)
          meanvg2<-mean(df$vg2,na.rm=TRUE)
          meanvg3<-mean(df$vg3,na.rm=TRUE)
          meanvg4<-mean(df$vg4,na.rm=TRUE)
          meanvg5<-mean(df$vg5,na.rm=TRUE)
          meanvg6<-mean(df$vg6,na.rm=TRUE)

          sdvg1<-sd(df$vg1,na.rm=TRUE)
          sdvg2<-sd(df$vg2,na.rm=TRUE)
          sdvg3<-sd(df$vg3,na.rm=TRUE)
          sdvg4<-sd(df$vg4,na.rm=TRUE)
          sdvg5<-sd(df$vg5,na.rm=TRUE)
          sdvg6<-sd(df$vg6,na.rm=TRUE)
        }

        df$t1<-(df$vg1-meanvg1)/sdvg1
        df$t2<-(df$vg2-meanvg2)/sdvg2
        df$t3<-(df$vg3-meanvg3)/sdvg3
        df$t4<-(df$vg4-meanvg4)/sdvg4
        df$t5<-(df$vg5-meanvg5)/sdvg5
        df$t6<-(df$vg6-meanvg6)/sdvg6
        df<-df[,c("ID","t1","t2","t3","t4","t5","t6")]
      }


      df2<- merge(animais,df,by="ID",all="T")
      df2<-df2[!(is.na(df2$Tag)),]
      df3<- df2[,4:ncol(df2)]
      df3[,]<-NA

      i1[is.null(i1)]<-'a'
      i2[is.null(i2)]<-'a'
      i3[is.null(i3)]<-'a'
      i4[is.null(i4)]<-'a'
      i5[is.null(i5)]<-'a'
      i6[is.null(i6)]<-'a'
      i7[is.null(i7)]<-'a'


      for(i in 1:nrow(df2)){

        if(df2$Cat[i] == i1[1]){
          df3[i,]<-(as.numeric(df2[i,4:ncol(df2)])*as.numeric(i1[2:(bn+1)]))
        }
        if(df2$Cat[i] == i2[1]){
          df3[i,]<-(as.numeric(df2[i,4:ncol(df2)])*as.numeric(i2[2:(bn+1)]))
        }
        if(df2$Cat[i] == i3[1] ){
          df3[i,]<-(as.numeric(df2[i,4:ncol(df2)])*as.numeric(i3[2:(bn+1)]))
        }
        if(df2$Cat[i] == i4[1] ){
          df3[i,]<-(as.numeric(df2[i,4:ncol(df2)])*as.numeric(i4[2:(bn+1)]))
        }
        if(df2$Cat[i] == i5[1] ){
          df3[i,]<-(as.numeric(df2[i,4:ncol(df2)])*as.numeric(i5[2:(bn+1)]))
        }
        if(df2$Cat[i] == i6[1] ){
          df3[i,]<-(as.numeric(df2[i,4:ncol(df2)])*as.numeric(i6[2:(bn+1)]))
        }
        if(df2$Cat[i] == i7[1]){
          df3[i,]<-(as.numeric(df2[i,4:ncol(df2)])*as.numeric(i7[2:(bn+1)]))
        }
      }

      Index = rowSums(df3)
      Index<-Index+base
      df4<-as.data.frame(Index)

      df4$ID<-df2[,1]
      df4$Tag<-df2[,2]
      df4$Cat<-df2[,3]

      df4<-df4[,c("Tag","ID","Cat","Index")]
      df4 <-df4 [ order(df4$Index,decreasing=c(TRUE)), ]

      df4$Index<-round( df4$Index, digits = 2)

      return(df4)
      cat("Ranking...ok!")
      rm(list = ls())


    }

  #
  #
  #
  #
  return(app)
}



