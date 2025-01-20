library(shiny)
library(DT)
library(shinyalert)
library(shinyjs)

source('datatable_contest_source.R')
###-----------------UI----------------- > 
library(shiny)
library(shinyjs)
library(shinydashboard)
library(DT)

ui <- fluidPage(
  useShinyjs(),
  br(),
  h1(HTML("<span clor:Purple'>Chain Reaction</span>")),
  textInputRow("rows_dim", 
               label = "Rows", 
               value = "8"),
  textInputRow("column_dim", 
               label = "Columns", 
               value = "8"),
  actionButton('play', label = "Go"),
  
  br(),
  
  tabsetPanel(
    tabPanel("Game Board",
             fluidRow(
               column(width = 12, DT::dataTableOutput("game_board")),
               column(width = 12, br(), br(), br(), br(),
                      verbatimTextOutput('chain_reaction_rules')
               )
             ),
             br(), br(),
             textOutput('clicks')
    ),
    tabPanel("Values and Player Slots",
             fluidRow(
               column(width = 6, shinydashboard::box(width = 12, DT::dataTableOutput('reactive_tmp'), title = "Values", solidHeader = TRUE)), 
               column(width = 6, shinydashboard::box(width = 12, DT::dataTableOutput('slots_table'), title = "Player Slots", solidHeader = TRUE))
             )
    )
  )
)



## ----------------Server------------------>
shinyServer <- function(input, output) {
  
  tbl <- eventReactive(input$play, {
    rows <- as.numeric(input$rows_dim)
    columns <- as.numeric(input$column_dim)
    if(is.na(rows))
      shinyalert::shinyalert(title = "Check your input", text = "Please enter numeric value rows")
    if(is.na(columns))
      shinyalert::shinyalert(title = "Check your input", text = "Please enter numeric value columns")
    initialize_matrix(rows, columns, NA)
  })
  
  # Reactive values
  rvs <- reactiveValues(data = NA) #dynamic data object 
  rvs_tmp <- reactiveVal()
  rvs_clicks_count <- reactiveVal(NA)
  rvs_player_slots <- reactiveVal()
  
  observe({ rvs$data <- tbl() })
  observe({ rvs_tmp(tbl()) })
  observe({ rvs_clicks_count(0) })
  observe({ rvs_player_slots(tbl()) })
  
  output$game_board <- DT::renderDataTable({
    DT::datatable(isolate(rvs$data), 
                  selection = list(target = "cell"), 
                  editable = list(target = "cell", disable = list(columns = c(0))), # Enable single-click cell edit
                  class = 'cell-border', 
                  rownames = FALSE, 
                  options = list(
                    searching = FALSE, 
                    pageLength = nrow(tbl()),
                    info = FALSE,
                    dom = 't',
                    ordering = FALSE,
                    columnDefs = list(
                      list(className = "dt-center", targets = "_all"), 
                      list(width = "100px", targets = "_all")
                    ),
                    rowCallback = JS("function(r,d) {
                    $(r).attr('height', '100px');
                    $(r).find('td').css('width', '100px');
                  }"),
                    drawCallback = JS("function(settings) {
                    $(this.api().table().node()).css({
                      'border-collapse': 'separate',
                      'border-spacing': '0'
                    });
                    $(this.api().table().node()).find('td').css({
                      'box-shadow': 'inset 0 0 5px rgba(0, 0, 0, 0.5)',
                      'background-color': '#333333',
                      'color': 'white',
                      'font-family': 'Press Start 2P, cursive',
                      'border': '1px solid white'  // Added white border
                    });
                    $(this.api().table().node()).find('th').css({
                      'display': 'none'
                    });
                  }")
                  ), 
                  escape = FALSE)
  })
  output$reactive_tmp <- DT::renderDataTable({
    req(nrow(rvs_tmp())>0)
    DT::datatable(isolate(rvs_tmp()))
  })
  
  output$slots_table <- DT::renderDataTable({
    req(nrow(rvs_player_slots()) > 0)
    DT::datatable(isolate(rvs_player_slots()))
  })
  # t <- paste("<p style='font-size: 50px; font-family: Arial, sans-serif; color:#0000FF>", txt,  "</p>")
  output$clicks <- renderText(paste("Total clicks:", rvs_clicks_count()))
  
  output$chain_reaction_rules <- renderText({
    
    paste("                How to Play   ", " ","Objective:", " ", "Take control of the board by eliminating opponents' orbs.", "  ",
          "Board:", " ", "The game takes place on an m × n grid. Each cell has a critical mass (number of adjacent cells).","  ",
          "Gameplay:", " ", '- Red and Green players take turns placing orbs of their color.',
          "  ", "Orbs stack up in a cell.", '  ', "- When a cell reaches its critical mass, it explodes:","    ", "- Adjacent cells gain an orb.", '  ', "- The original cell loses orbs equal to its critical mass.", "  ", "- Explosions can convert adjacent cells to the player's color.", "Winning: Eliminate opponents' orbs to win.", sep= "\n")
    
  })
  
  observeEvent(input$game_board_cells_selected, {
    req(input$game_board_cells_selected)
    board <- rvs$data
    n_clicks <- rvs_clicks_count() # To track total_clicks
    temp_reactive_df <- rvs_tmp() # The tracker for number of clicks to save in the back end
    slots_table <- rvs_player_slots()
    
    # Cell value from the board
    x <- as.data.frame(input$game_board_cells_selected)
    if(nrow(x) > 1)
      x <- x[nrow(x),]
    selected_row_col <- c(x$V1, (x$V2+1))
    current_value <- unlist(temp_reactive_df[x$V1, (x$V2+1)])[1] 
    
    # Update number of clicks
    n_clicks <- n_clicks + 1
    rvs_clicks_count(n_clicks)
    print(rvs_clicks_count())
    is_odd <- ifelse(n_clicks %% 2 == 0, F, T)
    
    rw <- selected_row_col[1]
    clm <- selected_row_col[2]
    
    # Current slot (player slot allocation)
    player_slot <- unlist(slots_table[rw, clm])[1]
    slot_free <- ifelse(is.na(player_slot), TRUE, FALSE)
    
    if(slot_free) {
      if(is_odd) {
        slots_table[rw, clm] <- 'P1'
        rvs_player_slots(slots_table)
      } else {
        slots_table[rw, clm] <- 'P2'
        rvs_player_slots(slots_table)
      }
    } 
    
    if(is.na(current_value)) {
      temp_reactive_df[rw, clm] <- 1
    }
    
    type <- getNeighborType(temp_reactive_df, rw, clm)
    
    if(type == 'corner') {
      if (is.na(current_value)) {
        temp_reactive_df <- rvs_tmp()
        temp_reactive_df[x$V1, (x$V2+1)] <- 1
        rvs_tmp(temp_reactive_df)
        shp <- giveMeShapesForThisValue(is_odd = is_odd, current_value = 1)
        rvs$data[rw, clm] <- shp
        # proxy = dataTableProxy('game_board')
        # observe({
        #   DT::replaceData(proxy, rvs$data, rownames = FALSE, resetPaging = FALSE)
        # })
        
      } else if (current_value >= 1) {
        if(is_odd) {
          if(unlist(slots_table[rw, clm]) == 'P1') {
            print('Do the reaction')
            ## -------- Reaction (Odd corner Begin)--------------->
            slots_table <- rvs_player_slots()
            temp_reactive_df <- rvs_tmp()
            board <- rvs$data
            n_clicks <- rvs_clicks_count()
            
            cell <- list(c(x$V1, (x$V2+1)))
            res <- reactoR(data_frame = temp_reactive_df, cells = cell)
            lst <- extract_elements(res)
            
            affected_cells_shp <- lst$affected_cells
            affected_cells_player <- lst$affected_cells
            
            temp_reactive_df <- lst$data_frame
            rvs_tmp(temp_reactive_df)
            
            #Update shapes 
            for(i in 1:length(affected_cells_shp)) {
              this_cell <- affected_cells_shp[[i]]
              value <- unlist(temp_reactive_df[this_cell[1], this_cell[2]])
              shp <- giveMeShapesForThisValue(is_odd = is_odd, 
                                              current_value = value)
              rvs$data[this_cell[1], this_cell[2]] <- shp
              runjs("Shiny.setInputValue('update_table', true);")  # Trigger table update
              
              # proxy = dataTableProxy('game_board')
              # observe({
              #   DT::replaceData(proxy, rvs$data, rownames = FALSE, resetPaging = FALSE)
              # })
              # invalidateLater(millis = 500)  # Update every 500 milliseconds
              
              
            }            
            cur_val <- unlist(temp_reactive_df[rw, clm])
            if(is.na(cur_val)) {
              rvs$data[rw, clm] <- NA
              proxy = dataTableProxy('game_board')
              observe({
                DT::replaceData(proxy, rvs$data, rownames = FALSE, resetPaging = FALSE)
              })
              
            } else {
              shp <- giveMeShapesForThisValue(is_odd = is_odd, 
                                              current_value = cur_val)
              rvs$data[rw, clm] <- shp
              # proxy = dataTableProxy('game_board')
              # observe({
              #   DT::replaceData(proxy, rvs$data, rownames = FALSE, resetPaging = FALSE)
              # })
              
            }
            
            #Update slots 
            slots_table <- rvs_player_slots()
            # beepr::beep()
            # HTML(tags$audio(src = "wavs/beep.wav", type = "audio/wav", autoplay = NA, controls = NA))
            
            for(i in 1:length(affected_cells_player)) {
              #Update slots table
              slots_table <- rvs_player_slots()
              this_cell <- affected_cells_player[[i]]
              current_player <- unlist(slots_table[rw, clm])
              temp_reactive_df <- rvs_tmp()
              this_val <- unlist(temp_reactive_df[this_cell[1], this_cell[2]])
              if(is.na(this_val)){
                slots_table[this_cell[1], this_cell[2]] <- NA
                rvs_player_slots(slots_table) 
              } else {
                slots_table[this_cell[1], this_cell[2]] <- current_player
                rvs_player_slots(slots_table) 
              }
              
            }
            if(is.na(cur_val)) {
              slots_table <- rvs_player_slots()
              slots_table[rw, clm] <- NA
              rvs_player_slots(slots_table)
            }
            
            # check player won?
            x <- rvs_player_slots()
            check_winning_moment(x)
            
            ## update the reactive df
            rvs_tmp(temp_reactive_df)
            ## -------- Reaction (Odd corner end)--------------->
          } else {
            n_clicks <- rvs_clicks_count()
            n_clicks <- n_clicks - 1
            rvs_clicks_count(n_clicks)
            shinyalert::shinyalert(title = "Reserved for Player 1", 
                                   text = "Reserved for player 1")
          }
        } else {
          if(unlist(slots_table[rw, clm]) == 'P1') {
            n_clicks <- rvs_clicks_count()
            n_clicks <- n_clicks - 1
            rvs_clicks_count(n_clicks)
            shinyalert::shinyalert(title = "Reserved for Player 1", 
                                   text = "Reserved for player 1")
          } else {
            print('Do the reaction')
            ## -------- Reaction (Even corner corner Begin)--------------->
            
            slots_table <- rvs_player_slots()
            temp_reactive_df <- rvs_tmp()
            board <- rvs$data
            n_clicks <- rvs_clicks_count()
            
            cell <- list(c(x$V1, (x$V2+1)))
            res <- reactoR(data_frame = temp_reactive_df, cells = cell)
            lst <- extract_elements(res)
            
            affected_cells_shp <- lst$affected_cells
            affected_cells_player <- lst$affected_cells
            
            temp_reactive_df <- lst$data_frame
            rvs_tmp(temp_reactive_df)
            
            #Update shapes 
            for(i in 1:length(affected_cells_shp)) {
              this_cell <- affected_cells_shp[[i]]
              value <- unlist(temp_reactive_df[this_cell[1], this_cell[2]])
              shp <- giveMeShapesForThisValue(is_odd = is_odd, 
                                              current_value = value)
              rvs$data[this_cell[1], this_cell[2]] <- shp
              runjs("Shiny.setInputValue('update_table', true);")  # Trigger table update
              
              # proxy = dataTableProxy('game_board')
              # observe({
              #   DT::replaceData(proxy, rvs$data, rownames = FALSE, resetPaging = FALSE)
              # })
              # invalidateLater(millis = 500)  # Update every 500 milliseconds
              
              
            }            
            cur_val <- unlist(temp_reactive_df[rw, clm])
            if(is.na(cur_val)) {
              rvs$data[rw, clm] <- NA
            } else {
              shp <- giveMeShapesForThisValue(is_odd = is_odd, 
                                              current_value = cur_val)
              rvs$data[rw, clm] <- shp
            }
            
            #Update slots 
            slots_table <- rvs_player_slots()
            
            tags$audio(src = "wavs/beep.wav", type = "audio/wav", autoplay = NA, controls = NA)
            
            for(i in 1:length(affected_cells_player)) {
              #Update slots table
              slots_table <- rvs_player_slots()
              this_cell <- affected_cells_player[[i]]
              current_player <- unlist(slots_table[rw, clm])
              temp_reactive_df <- rvs_tmp()
              this_val <- unlist(temp_reactive_df[this_cell[1], this_cell[2]])
              if(is.na(this_val)){
                slots_table[this_cell[1], this_cell[2]] <- NA
                rvs_player_slots(slots_table) 
              } else {
                slots_table[this_cell[1], this_cell[2]] <- current_player
                rvs_player_slots(slots_table) 
              }
              
            }
            if(is.na(cur_val)) {
              slots_table[rw, clm] <- NA
              rvs_player_slots(slots_table)
            }
            
            # check player won?
            x <- rvs_player_slots()
            check_winning_moment(x)
            
            ## update the reactive df
            rvs_tmp(temp_reactive_df)
            
            ## -------- Reaction (Even player corner end)--------------->
            
          }
        }
      }
    } else if(type == 'edge') {
      if (is.na(current_value)) {
        temp_reactive_df <- rvs_tmp()
        temp_reactive_df[x$V1, (x$V2+1)] <- 1
        rvs_tmp(temp_reactive_df)
        shp <- giveMeShapesForThisValue(is_odd = is_odd, current_value = 1)
        rvs$data[rw, clm] <- shp
      } else if(current_value == 1) {
        if(is_odd) {
          if(unlist(slots_table[rw, clm]) == 'P1') {
            temp_reactive_df <- rvs_tmp()
            temp_reactive_df[x$V1, (x$V2+1)] <- 2
            rvs_tmp(temp_reactive_df)
            shp <- giveMeShapesForThisValue(is_odd = is_odd, current_value = 2)
            rvs$data[x$V1, (x$V2+1)] <- shp
          } else {
            n_clicks <- rvs_clicks_count()
            n_clicks <- n_clicks - 1
            rvs_clicks_count(n_clicks)
            shinyalert::shinyalert(title = "Reserved for Player 1", 
                                   text = "Reserved for player 1")
          }
        } else {
          if(unlist(slots_table[rw, clm]) == 'P1') {
            n_clicks <- rvs_clicks_count()
            n_clicks <- n_clicks - 1
            rvs_clicks_count(n_clicks)
            shinyalert::shinyalert(title = "Reserved for Player 2", 
                                   text = "Reserved for player 2")
          } else {
            temp_reactive_df <- rvs_tmp()
            temp_reactive_df[x$V1, (x$V2+1)] <- 2
            rvs_tmp(temp_reactive_df)
            shp <- giveMeShapesForThisValue(is_odd = is_odd, current_value = 2)
            rvs$data[x$V1, (x$V2+1)] <- shp
          }
        }
      } else if (current_value >= 2) {
        if(is_odd) {
          if(unlist(slots_table[rw, clm]) == 'P1') {
            print('Do the reaction')
            
            ## -------- Reaction (Odd edge Begin)--------------->
            
            slots_table <- rvs_player_slots()
            temp_reactive_df <- rvs_tmp()
            board <- rvs$data
            n_clicks <- rvs_clicks_count()
            
            cell <- list(c(x$V1, (x$V2+1)))
            res <- reactoR(data_frame = temp_reactive_df, cells = cell)
            lst <- extract_elements(res)
            
            affected_cells_shp <- lst$affected_cells
            affected_cells_player <- lst$affected_cells
            
            temp_reactive_df <- lst$data_frame
            rvs_tmp(temp_reactive_df)
            
            #Update shapes 
            for(i in 1:length(affected_cells_shp)) {
              this_cell <- affected_cells_shp[[i]]
              value <- unlist(temp_reactive_df[this_cell[1], this_cell[2]])
              shp <- giveMeShapesForThisValue(is_odd = is_odd, 
                                              current_value = value)
              rvs$data[this_cell[1], this_cell[2]] <- shp
            }            
            cur_val <- unlist(temp_reactive_df[rw, clm])
            if(is.na(cur_val)) {
              rvs$data[rw, clm] <- NA
            } else {
              shp <- giveMeShapesForThisValue(is_odd = is_odd, 
                                              current_value = cur_val)
              rvs$data[rw, clm] <- shp
            }
            
            #Update slots 
            slots_table <- rvs_player_slots()
            
            tags$audio(src = "wavs/beep.wav", type = "audio/wav", autoplay = NA, controls = NA)
            
            for(i in 1:length(affected_cells_player)) {
              #Update slots table
              slots_table <- rvs_player_slots()
              this_cell <- affected_cells_player[[i]]
              current_player <- unlist(slots_table[rw, clm])
              temp_reactive_df <- rvs_tmp()
              this_val <- unlist(temp_reactive_df[this_cell[1], this_cell[2]])
              if(is.na(this_val)){
                slots_table[this_cell[1], this_cell[2]] <- NA
                rvs_player_slots(slots_table) 
              } else {
                slots_table[this_cell[1], this_cell[2]] <- current_player
                rvs_player_slots(slots_table) 
              }
              
            }
            if(is.na(cur_val)) {
              slots_table <- rvs_player_slots()
              slots_table[rw, clm] <- NA
              rvs_player_slots(slots_table)
            }
            
            # check player won?
            x <- rvs_player_slots()
            check_winning_moment(x)
            
            ## update the reactive df
            rvs_tmp(temp_reactive_df)
            ## -------- Reaction (Odd edge end)--------------->
            
          } else {
            n_clicks <- rvs_clicks_count()
            n_clicks <- n_clicks - 1
            rvs_clicks_count(n_clicks)
            shinyalert::shinyalert(title = "Reserved for Player 1", 
                                   text = "Reserved for player 1")
          }
        } else {
          if(unlist(slots_table[rw, clm]) == 'P1') {
            n_clicks <- rvs_clicks_count()
            n_clicks <- n_clicks - 1
            rvs_clicks_count(n_clicks)
            shinyalert::shinyalert(title = "Reserved for Player 2", 
                                   text = "Reserved for player 2")
          } else {
            print("Do the reaction")
            
            
            ## -------- Reaction (even edge Begin)--------------->
            slots_table <- rvs_player_slots()
            temp_reactive_df <- rvs_tmp()
            board <- rvs$data
            n_clicks <- rvs_clicks_count()
            
            cell <- list(c(x$V1, (x$V2+1)))
            res <- reactoR(data_frame = temp_reactive_df, cells = cell)
            lst <- extract_elements(res)
            
            affected_cells_shp <- lst$affected_cells
            affected_cells_player <- lst$affected_cells
            
            temp_reactive_df <- lst$data_frame
            rvs_tmp(temp_reactive_df)
            
            #Update shapes 
            for(i in 1:length(affected_cells_shp)) {
              this_cell <- affected_cells_shp[[i]]
              value <- unlist(temp_reactive_df[this_cell[1], this_cell[2]])
              shp <- giveMeShapesForThisValue(is_odd = is_odd, 
                                              current_value = value)
              rvs$data[this_cell[1], this_cell[2]] <- shp
            }            
            cur_val <- unlist(temp_reactive_df[rw, clm])
            if(is.na(cur_val)) {
              rvs$data[rw, clm] <- NA
            } else {
              shp <- giveMeShapesForThisValue(is_odd = is_odd, 
                                              current_value = cur_val)
              rvs$data[rw, clm] <- shp
            }
            
            #Update slots 
            slots_table <- rvs_player_slots()
            
            tags$audio(src = "wavs/beep.wav", type = "audio/wav", autoplay = NA, controls = NA)
            
            for(i in 1:length(affected_cells_player)) {
              #Update slots table
              slots_table <- rvs_player_slots()
              this_cell <- affected_cells_player[[i]]
              current_player <- unlist(slots_table[rw, clm])
              temp_reactive_df <- rvs_tmp()
              this_val <- unlist(temp_reactive_df[this_cell[1], this_cell[2]])
              if(is.na(this_val)){
                slots_table[this_cell[1], this_cell[2]] <- NA
                rvs_player_slots(slots_table) 
              } else {
                slots_table[this_cell[1], this_cell[2]] <- current_player
                rvs_player_slots(slots_table) 
              }
              
            }
            if(is.na(cur_val)) {
              slots_table <- rvs_player_slots()
              slots_table[rw, clm] <- NA
              rvs_player_slots(slots_table)
            }
            
            # check player won?
            x <- rvs_player_slots()
            check_winning_moment(x)
            
            ## update the reactive df
            rvs_tmp(temp_reactive_df)
            
            ## -------- Reaction (Even edge end)--------------->
            
          }
        }
        
      }
    } else if(type == 'other') {
      if (is.na(current_value)) {
        temp_reactive_df <- rvs_tmp()
        temp_reactive_df[x$V1, (x$V2+1)] <- 1
        rvs_tmp(temp_reactive_df)
        shp <- giveMeShapesForThisValue(is_odd = is_odd, current_value = 1)
        rvs$data[rw, clm] <- shp
      } else if(current_value == 1) {
        if(is_odd) {
          if(unlist(slots_table[rw, clm]) == 'P1') {
            temp_reactive_df <- rvs_tmp()
            temp_reactive_df[x$V1, (x$V2+1)] <- 2
            rvs_tmp(temp_reactive_df)
            shp <- giveMeShapesForThisValue(is_odd = is_odd, current_value = 2)
            rvs$data[x$V1, (x$V2+1)] <- shp
          } else {
            n_clicks <- rvs_clicks_count()
            n_clicks <- n_clicks - 1
            rvs_clicks_count(n_clicks)
            shinyalert::shinyalert(title = "Reserved for Player 1", 
                                   text = "Reserved for player 1")
          }
        } else {
          if(unlist(slots_table[rw, clm]) == 'P1') {
            n_clicks <- rvs_clicks_count()
            n_clicks <- n_clicks - 1
            rvs_clicks_count(n_clicks)
            shinyalert::shinyalert(title = "Reserved for Player 2", 
                                   text = "Reserved for player 2")
          } else {
            temp_reactive_df <- rvs_tmp()
            temp_reactive_df[x$V1, (x$V2+1)] <- 2
            rvs_tmp(temp_reactive_df)
            shp <- giveMeShapesForThisValue(is_odd = is_odd, current_value = 2)
            rvs$data[x$V1, (x$V2+1)] <- shp
          }
        }
      } else if (current_value == 2) {
        if(is_odd) {
          if(unlist(slots_table[rw, clm]) == 'P1') {
            temp_reactive_df <- rvs_tmp()
            temp_reactive_df[x$V1, (x$V2+1)] <- 3
            rvs_tmp(temp_reactive_df)
            shp <- giveMeShapesForThisValue(is_odd = is_odd, current_value = 3)
            rvs$data[x$V1, (x$V2+1)] <- shp
          } else {
            n_clicks <- rvs_clicks_count()
            n_clicks <- n_clicks - 1
            rvs_clicks_count(n_clicks)
            shinyalert::shinyalert(title = "Reserved for Player 1", 
                                   text = "Reserved for player 1")
          }
        } else {
          if(unlist(slots_table[rw, clm]) == 'P1') {
            n_clicks <- rvs_clicks_count()
            n_clicks <- n_clicks - 1
            rvs_clicks_count(n_clicks)
            shinyalert::shinyalert(title = "Reserved for Player 2", 
                                   text = "Reserved for player 2")
          } else {
            temp_reactive_df <- rvs_tmp()
            temp_reactive_df[x$V1, (x$V2+1)] <- 3
            rvs_tmp(temp_reactive_df)
            shp <- giveMeShapesForThisValue(is_odd = is_odd, current_value = 3)
            rvs$data[x$V1, (x$V2+1)] <- shp
          }
        }
      } else if(current_value >= 3) {
        if(is_odd) {
          if(unlist(slots_table[rw, clm]) == 'P1') {
            print('Do the reaction')
            
            ## -------- Reaction (Odd other Begin)--------------->
            
            slots_table <- rvs_player_slots()
            temp_reactive_df <- rvs_tmp()
            board <- rvs$data
            n_clicks <- rvs_clicks_count()
            
            cell <- list(c(x$V1, (x$V2+1)))
            res <- reactoR(data_frame = temp_reactive_df, cells = cell)
            lst <- extract_elements(res)
            
            affected_cells_shp <- lst$affected_cells
            affected_cells_player <- lst$affected_cells
            
            temp_reactive_df <- lst$data_frame
            rvs_tmp(temp_reactive_df)
            
            #Update shapes 
            for(i in 1:length(affected_cells_shp)) {
              this_cell <- affected_cells_shp[[i]]
              value <- unlist(temp_reactive_df[this_cell[1], this_cell[2]])
              shp <- giveMeShapesForThisValue(is_odd = is_odd, 
                                              current_value = value)
              rvs$data[this_cell[1], this_cell[2]] <- shp
            }            
            cur_val <- unlist(temp_reactive_df[rw, clm])
            if(is.na(cur_val)) {
              rvs$data[rw, clm] <- NA
            } else {
              shp <- giveMeShapesForThisValue(is_odd = is_odd, 
                                              current_value = cur_val)
              rvs$data[rw, clm] <- shp
            }
            
            #Update slots 
            slots_table <- rvs_player_slots()
            
            tags$audio(src = "wavs/beep.wav", type = "audio/wav", autoplay = NA, controls = NA)
            
            for(i in 1:length(affected_cells_player)) {
              #Update slots table
              slots_table <- rvs_player_slots()
              this_cell <- affected_cells_player[[i]]
              current_player <- unlist(slots_table[rw, clm])
              temp_reactive_df <- rvs_tmp()
              this_val <- unlist(temp_reactive_df[this_cell[1], this_cell[2]])
              if(is.na(this_val)){
                slots_table[this_cell[1], this_cell[2]] <- NA
                rvs_player_slots(slots_table) 
              } else {
                slots_table[this_cell[1], this_cell[2]] <- current_player
                rvs_player_slots(slots_table) 
              }
              
            }
            if(is.na(cur_val)) {
              slots_table <- rvs_player_slots()
              slots_table[rw, clm] <- NA
              rvs_player_slots(slots_table)
            }
            
            # check player won?
            x <- rvs_player_slots()
            check_winning_moment(x)
            
            ## update the reactive df
            rvs_tmp(temp_reactive_df)
            ## -------- Reaction (Odd other end)--------------->
            
          } else {
            n_clicks <- rvs_clicks_count()
            n_clicks <- n_clicks - 1
            rvs_clicks_count(n_clicks)
            shinyalert::shinyalert(title = "Reserved for Player 1", 
                                   text = "Reserved for player 1")
          }
        } else {
          if(unlist(slots_table[rw, clm]) == 'P1') {
            n_clicks <- rvs_clicks_count()
            n_clicks <- n_clicks - 1
            rvs_clicks_count(n_clicks)
            shinyalert::shinyalert(title = "Reserved for Player 2", 
                                   text = "Reserved for player 2")
          } else {
            print("Do the reaction")
            
            ## -------- Reaction (even other Begin)--------------->
            
            slots_table <- rvs_player_slots()
            temp_reactive_df <- rvs_tmp()
            board <- rvs$data
            n_clicks <- rvs_clicks_count()
            
            cell <- list(c(x$V1, (x$V2+1)))
            res <- reactoR(data_frame = temp_reactive_df, cells = cell)
            lst <- extract_elements(res)
            
            affected_cells_shp <- lst$affected_cells
            affected_cells_player <- lst$affected_cells
            
            temp_reactive_df <- lst$data_frame
            rvs_tmp(temp_reactive_df)
            
            #Update shapes 
            for(i in 1:length(affected_cells_shp)) {
              this_cell <- affected_cells_shp[[i]]
              value <- unlist(temp_reactive_df[this_cell[1], this_cell[2]])
              shp <- giveMeShapesForThisValue(is_odd = is_odd, 
                                              current_value = value)
              rvs$data[this_cell[1], this_cell[2]] <- shp
            }            
            cur_val <- unlist(temp_reactive_df[rw, clm])
            if(is.na(cur_val)) {
              rvs$data[rw, clm] <- NA
            } else {
              shp <- giveMeShapesForThisValue(is_odd = is_odd, 
                                              current_value = cur_val)
              rvs$data[rw, clm] <- shp
            }
            
            #Update slots 
            slots_table <- rvs_player_slots()
            
            # HTML(tags$audio(src = "wavs/beep.wav", type = "audio/wav", autoplay = NA, controls = NA))
            
            for(i in 1:length(affected_cells_player)) {
              #Update slots table
              slots_table <- rvs_player_slots()
              this_cell <- affected_cells_player[[i]]
              current_player <- unlist(slots_table[rw, clm])
              temp_reactive_df <- rvs_tmp()
              this_val <- unlist(temp_reactive_df[this_cell[1], this_cell[2]])
              if(is.na(this_val)){
                slots_table[this_cell[1], this_cell[2]] <- NA
                rvs_player_slots(slots_table) 
              } else {
                slots_table[this_cell[1], this_cell[2]] <- current_player
                rvs_player_slots(slots_table) 
              }
              
            }
            if(is.na(cur_val)) {
              slots_table <-  rvs_player_slots()
              slots_table[rw, clm] <- NA
              rvs_player_slots(slots_table)
            }
            
            # check player won?
            x <- rvs_player_slots()
            check_winning_moment(x)
            
            ## update the reactive df
            rvs_tmp(temp_reactive_df)
            ## -------- Reaction (even other end)--------------->
            
          }
        }
      }
    }
    
  })
  # 
  proxy = dataTableProxy('game_board')
  observe({
    DT::replaceData(proxy, rvs$data, rownames = FALSE, resetPaging = FALSE)
  })
  
}

shinyApp(ui = ui, server = shinyServer)



## Edited value # ref https://stackoverflow.com/questions/58595096/editing-multiple-cells-in-a-datatable-in-shiny


# library(shiny)
# library(shinydashboard)
# library(tidyverse)
# library(DT)
# 
# header <- dashboardHeader(title = "demo")
# sidebar <- dashboardSidebar(
#   sidebarMenu(id = 'sidebarmenu',
#               menuItem("admin", tabName = "admin", icon = icon("adjust")),
#               downloadButton("downloadResults","Download Results")
#   )
# )
# 
# body <- dashboardBody(
#   tabItems(
#     tabItem(
#       tabName = 'admin', class = 'active', 
#       fluidRow(
#         box(
#           dataTableOutput('userTable'), width = 6
#         )
#       )
#     )
#   )
# )
# 
# 
# ui <- dashboardPage(title = 'admin function test', header, sidebar, body, skin='blue')
# 
# server <- function(input, output, session){
#   
#   dat <- data.frame(userName = c("John","Mary","Mike"), start = c("06/08/2019","01/01/2019","23/10/2019"), stringsAsFactors = FALSE)
#   
#   output$userTable <- renderDataTable({
#     DT::datatable(isolate(dat),
#                   editable = TRUE,
#                   rownames = FALSE)
#   })
#   
#   ###Tracking Changes###
#   rvs <- reactiveValues(
#     data = NA #dynamic data object
#   )
#   
#   observe({
#     rvs$data <- dat
#   })
#   
#   proxy = dataTableProxy('userTable')
#   observe({
#     DT::replaceData(proxy, rvs$data, rownames = FALSE, resetPaging = FALSE)
#   })
#   
#   observeEvent(input$userTable_cell_edit, {
#     rvs$data <<- editData(rvs$data, input$userTable_cell_edit, rownames = FALSE)
#   })
#   
#   # observeEvent(
#   #   input$do,{
#   #     write.csv(rvs$data,'userTest.csv', row.names = FALSE)
#   #   })
#   
#   output$downloadResults <- downloadHandler(
#     filename = function(){paste("userTest.csv.csv", sep = "")},
#     content = function(file){write.csv(rvs$data, file, row.names = FALSE)}
#   )
#   
# }
# 
# shinyApp(ui = ui, server = server)

# Example : Determine clicked row
# 
# library(shiny)
# library(DT)
# data("mtcars")
# 
# ui <- shinyUI(
#   fluidRow(
#     DT::dataTableOutput("myDatatable"),
#     verbatimTextOutput("selectedCells")
#   )
# )
# 
# server <- shinyServer(function(input, output, session) {
#   output$myDatatable <- DT::renderDataTable({mtcars, 
#                                             selection=list(target="cell"),
#                                             server = FALSE,
#                                             rownames=FALSE})
#   
#   output$selectedCells <- renderPrint(input$myDatatable_cells_selected)
# })
# 
# shinyApp(ui, server)
# 
# 
# library(shiny)
# library(DT)
# 
# shinyApp(
#   ui <- fluidPage(
#     DT::dataTableOutput("data"),
#     textOutput('myText')
#   ),
#   
#   server <- function(input, output) {
#     
#     myValue <- reactiveValues(employee = '')
#     
#     shinyInput <- function(FUN, len, id, ...) {
#       inputs <- character(len)
#       for (i in seq_len(len)) {
#         inputs[i] <- as.character(FUN(paste0(id, i), ...))
#       }
#       inputs
#     }
#     
#     df <- reactiveValues(data = data.frame(
#       
#       Name = c('Dilbert', 'Alice', 'Wally', 'Ashok', 'Dogbert'),
#       Motivation = c(62, 73, 3, 99, 52),
#       Actions = shinyInput(actionButton, 5, 'button_', label = "Fire", onclick = 'Shiny.onInputChange(\"select_button\",  this.id)' ),
#       stringsAsFactors = FALSE,
#       row.names = 1:5
#     ))
#     
#     
#     output$data <- DT::renderDataTable(
#       df$data, server = FALSE, escape = FALSE, selection = 'none'
#     )
#     
#     observeEvent(input$select_button, {
#       browser()
#       selectedRow <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
#       myValue$employee <<- paste('click on ',df$data[selectedRow,1])
#     })
#     
#     output$myText <- renderText({
#       
#       myValue$employee
#       
#     })
#     
#   }
# )
# 
# 

# Objective: Take control of the board by eliminating opponents’ orbs.
# Board: The game takes place on an m × n grid (commonly 9 × 6). Each cell has a critical mass (number of adjacent cells).
# Gameplay:
#   Red and Green players take turns placing orbs of their color.
# Orbs stack up in a cell.
# When a cell reaches its critical mass, it explodes:
#   Adjacent cells gain an orb.
# The original cell loses orbs equal to its critical mass.
# Explosions can convert adjacent cells to the player’s color.
# Winning: Eliminate opponents’ orbs to win.
# Remember, the game’s unpredictability makes it fascinating!