#########################
######   TableOCR  ######
######  08/04/2023 ######
#########################

###### Written by Thomas Tichelbaecker, Princeton University

#' @export

table_ocr <- function(auth_file_path){

####### 1. Libraries ########

####### 2. Helper functions ########

##### Line segmentation via line overlap
# Basic idea is to capture all bounding boxes that overlap 
# with midpoint of randomly chosen bounding box, repeat until all 
# OCR and associated results are assigned to a line

# 3 Arguments
#      - centered_y: midpoint of all bounding boxes
#      - y_upper: upper y limit of all bounding boxes
#      - y_lower: lower y limit of all bounding boxes
line_segment_overlap <- function(centered_y, 
                                 y_upper, 
                                 y_lower){
  
  # output vector
  lines_y <- rep(NA, times = length(centered_y))
  
  # while loop that continues as long as there are missing values
  while (sum(is.na(lines_y) != 0)){
    
    # set seed index, choose random box to see what 
    # boxes are on same line
    seed.index <- sample(1:length(centered_y), 1)
    
    # get midpoint of seed box
    seed <- centered_y[seed.index]
    
    # get indeces of all boxes that are cut by midpoint line
    indeces <- which(seed < y_lower & 
                       seed > y_upper)
    
    # return all lines
    lines_y[c(seed.index, indeces)] <- seed
    
  }
  
  return(lines_y)
}


####### 3. UI ########

ui <- fluidPage(
  waiter::use_waiter(),
  
  # Application title
  titlePanel("TableOCR"),
  
  # Sidebar Layout
  sidebarLayout(
    
    # Sidebar buttons and inputs
    div(class = "sidebar-layout", 
    sidebarPanel(width = 2,
                 
                 h6("Image Options", style = "margin-top = 0px; font-weight: 700;"), 
                 
                 # Choose image
                 fileInput("image", "", 
                           buttonLabel = "Select Document", 
                           placeholder = ""),
                 numericInput("page", "Page", 1, min = 1),
                 numericInput("rotate", "Rotate", 0, min = 0),
                 
                 # Zoom in
                 sliderInput("zoom", "Zoom:",
                             min = 0, max = 3000,
                             value = 750),
                 
                 hr(style = "border-top: 1px solid #000000;"),
                 h6("OCR", style = "font-weight: 700;"), 
                 
                 # Use tesseract
                 actionButton("tesseract", "OCR Tesseract"),
                 actionButton("google", "OCR Google"),
                

                 actionButton("append_previous", "Add results"),
                 
                 
                 hr(style = "border-top: 1px solid #000000;"),
                 
                 
                 ################### CLEAR
                 
                 h6("Clear Lines", style = "font-weight: 700;"), 
                 
                 fluidRow(
                   column(4, # Clear all set columns and rows
                          actionButton("clear", "All")),
                   column(8, # Clear selected line
                          actionButton("clear_selected", "Selected"))),

                 
                 hr(style = "border-top: 1px solid #000000;"),
                 
                 ################### SAVE
                 
                 h6("Save", style = "font-weight: 700;"),
                 
                 textInput("filename", "Input a name for the file", value = paste0("data-", Sys.Date(),".csv")),
                 
                 fluidRow(
                   column(6, # Save current page
                          downloadButton("download_current", label = "Current")),
                          #actionButton("save_current", "Save current")),
                   column(6, # Save all
                          downloadButton("download_all", label = "All"))),
                 
                 
                 ################### MOUSE CONTROLS
                 
                 hr(style = "border-top: 1px solid #000000;"),
                 
                 h6( "Mouse Controls", style = "font-weight: 700;"),
                 
                 
                 actionButton("btn", "Show"),
                 
                 ## Options for lack of keyboard
                 # Switch edit / remove lines
                 
                 conditionalPanel(
                   condition = 'output.bool', 
                   radioButtons("edit", "Edit/Remove lines:",
                                c("On" = "edit",
                                  "Off" = "remove"
                                )),
                   # Switch vertical / horizontal lines
                   radioButtons("lines_dir", "Direction of line:",
                                c("Vertical" = "ver",
                                  "Horizontal" = "hor"
                                )),
                 ),
                 
                 ################### Advanced
                 
                 hr(style = "border-top: 1px solid #000000;"),
                 
                 h6( "Advanced options", style = "font-weight: 700;"),
                 
                 # Tesseract language
                 selectInput("tesseract_language", "Language",
                             choices = c( "deu","eng", "dan"), width = "70%"),
                 
                 numericInput("tesseract_confidence", "Confidence", 80, min = 1, max = 100),
                 
                 actionButton("btn2", "Show"),
                 conditionalPanel(
                   condition = 'output.bool2', 
                   radioButtons("visual", "Visualization",
                                c("Off" = "off",
                                  "On" = "on"
                                )), 
                   radioButtons("remove_nonalpha_numeric", "Remove non-alpha-numeric",
                                c("On" = "on",
                                  "Off" = "off"
                                ))
                 )

    )),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        
        # Chosen image input
        tabPanel("Table Image", plotOutput("raster", click = "plot_click")),
        
        # OCR results of current page
        tabPanel("OCR Result", tableOutput("table")),
        
        # OCR results of all combined pages
        tabPanel("All Results", tableOutput("table_final"))
      )
    )
  ), 
  ## keydown
  tags$script('
      downKeyCount = 0;
      $(document).on("keydown", function (e) {
         Shiny.onInputChange("downKey", downKeyCount++);
         Shiny.onInputChange("downKeyId", e.code);
      });'
  ),
  ## keyup
  tags$script('
      upKeyCount = 0;
      $(document).on("keyup", function (e) {
         Shiny.onInputChange("upKey", upKeyCount++);
         Shiny.onInputChange("upKeyId", e.code);
      });'
  ),
  
  tags$script('
    $(document).keyup(function(event) {
      if (event.keyCode == 8) {
        $("#clear_selected").click();
      }
    });'
  ),
  tags$head(
    tags$style(HTML("
        
        .input-group .form-control {
          width = 0px;
          height = 0px;
        }
        
        .progress {
          margin-bottom: 5px;
          height: 20px;
        }
        
        .btn {
          font-size: 12px;
        
        }
        
        .control-label{
          font-weight: 400;
        } 
        
        .form-control {
          font-size: 12px;
        
        }
        
        .sidebar-layout { 
          font-size:12px;
          width: 90%;
          height: 80%;
        }
        
        .form-group {
        margin-bottom: 5px;
        margin-top: 5px;
        padding: 0px;
        }
  ")))
)


####### 4. Server ########
  
server <- function(input, output) {
    
  options(shiny.maxRequestSize=30*1024^2^2) 
  
  
  ####### 4.1 Toggle ########
  
  # Toggle Mouse Control
  value=TRUE
  value2 = TRUE
  output$bool <- eventReactive(input$btn,{
    value    
  })
  outputOptions(output,"bool",suspendWhenHidden=FALSE)
  
  observeEvent(input$btn,
               value <<- !value        
  )
  
  output$bool2 <- eventReactive(input$btn2,{
    value2    
  })
  outputOptions(output,"bool2",suspendWhenHidden=FALSE)
  
  observeEvent(input$btn2,
               value2 <<- !value2        
  )
  
  
  ####### 4.1 Initialize Data ########
    
    #### Dataframes
    
    # Current OCRed Page data
    final <- reactiveValues()
    final$DT <- data.frame()
    
    # All OCRed pages data
    all_pages <- reactiveValues()
    all_pages$data_lines <- list()
    all_pages$final_df <- data.frame()
    
    # Dataframe holding row & column coordinates
    values <- reactiveValues()
    values$DT <- data.frame(x = numeric(),
                            y = numeric(),
                            horizontal = numeric())
    values$rows <- data.frame()
    values$bbox <- data.frame()
    
    # Dataframe holding row & column coordinates that are to be removed
    rows_remove <- reactiveValues()
    rows_remove$DT <- data.frame(x = numeric(),
                                 y = numeric(),
                                 horizontal = numeric())
    
    add_row <- data.frame()
    
    # Hardcoded values
    path_temp <<- "temp.png"
    #path_temp2 <<- "temp2.png"
    # height is also hardcoded once image is loaded
    
  ####### 4.2 Define Key Press events ########
    
    keyRecords = reactiveValues()
    observeEvent(input$downKey, { keyRecords[[input$downKeyId]] = TRUE });
    observeEvent(input$upKey, { keyRecords[[input$upKeyId]] = FALSE });
    
    
  ####### 4.3 Load Image ########
    img <- reactive({
      
      # Save image/pdf path
      f <<- input$image
      
      # Return empty frame if no image provided
      if (is.null(f))
        return(NULL)
      
      # For PDF input
      if(str_detect(f$datapath, "\\.pdf")){
        
        # Convert PDF page to bitmap
        bitmap <- pdf_render_page(f$datapath, 
                                  numeric = F,
                                  page = input$page, # Page number
                                  dpi = 200) # resolution

        
        # load image
        img <- image_read(bitmap) %>% 
          image_convert(., colorspace = "Gray") %>% 
          image_threshold(type = "black", threshold = "65%") %>%
          image_threshold(type = "white", threshold = "65%") %>%
          #image_deskew() %>%
          image_rotate(., input$rotate) 
        
        # calculate height
        image_meta <- image_data(img)
        height <<- dim(image_meta)[3]
        
        # display image
        img
        
        # For image input  
      }else{
        
        # load image
        img <- image_read(f$datapath) %>% 
          image_convert(., colorspace = "Gray") %>% 
          image_threshold(type = "black", threshold = "65%") %>%
          image_threshold(type = "white", threshold = "65%") %>%
          #image_deskew() %>%
          image_rotate(., input$rotate) 
        
        # calculate height
        image_meta <- image_data(img)
        height <<- dim(image_meta)[3]
        
        # display image
        img
        
      }
      
    })
    
    
  ####### 4.4 Record Clicks ########
    
    observeEvent(input$plot_click, {
    
      keys = reactiveValuesToList(keyRecords)
      fun = ifelse(length(names(keys[unlist(keys)])) == 0, 
                   "Leer", 
                   names(keys[unlist(keys)]))
      
      ###### Add Rows
      if(input$edit == "edit" & fun != "KeyD"){
        
        if(input$lines_dir == "hor" | fun == "KeyA"){
          add_row <- data.frame(x = input$plot_click$x,
                                y = input$plot_click$y, 
                                horizontal = 1)
          # add row to the data.frame
          values$DT <- rbind(values$DT, add_row)
          print(values$DT)}else{
            add_row <- data.frame(x = input$plot_click$x,
                                  y = input$plot_click$y, 
                                  horizontal = 0)
            # add row to the data.frame
            values$DT <- rbind(values$DT, add_row)
            #print(values$DT) 
          }
      }
      
      ###### Remove Rows
      if(input$edit == "remove" | fun == "KeyD"){
        
        # New data frame with click position
        remove_row <- data.frame(x = input$plot_click$x,
                                 y = input$plot_click$y, 
                                 horizontal = 1)
        
        # Find whether click identified horizontal or vertical line
        min_v = min(abs(remove_row$x - values$DT$x[values$DT$horizontal == 0]))
        min_h = min(abs(remove_row$y - values$DT$y[values$DT$horizontal == 1]))
        
        # Write conditional statement
        if(min_v >= min_h){
          
          # Horizontal line
          row_h <- values$DT[which(min_h == abs(remove_row$y - values$DT$y)), ]
          
          # Check whether row is already selected for deletion
          if(row_h$x %in% rows_remove$DT$x & 
             row_h$y %in% rows_remove$DT$y &
             row_h$horizontal %in% rows_remove$DT$horizontal){
            
            # index in remove data
            remove_remove <- which(rows_remove$DT$x %in% row_h$x & 
                                     rows_remove$DT$y %in% row_h$y & 
                                     rows_remove$DT$horizontal %in% row_h$horizontal)
            
            # de-select row
            rows_remove$DT <- rows_remove$DT[-remove_remove, ]
          }else{
            # select row
            rows_remove$DT <- rbind(rows_remove$DT, row_h)
          }
          
        }else{
          # Vertical line
          row_v <- values$DT[which(min_v == abs(remove_row$x - values$DT$x)), ]
          
          if(row_v$x %in% rows_remove$DT$x & 
             row_v$y %in% rows_remove$DT$y &
             row_v$horizontal %in% rows_remove$DT$horizontal){
             
            # index in remove data
            remove_remove <- which(rows_remove$DT$x %in% row_v$x & 
                                     rows_remove$DT$y %in% row_v$y & 
                                     rows_remove$DT$horizontal %in% row_v$horizontal)
            
            # de-select row
            rows_remove$DT <- rows_remove$DT[-remove_remove, ]
          }else{
            # select row
            rows_remove$DT <- rbind(rows_remove$DT, row_v)
          }

        }
      }
    })
    
    
    
  ####### 4.5 Clear lines ########
    
    ###### Clear selected lines
    observeEvent(input$clear_selected, {
      
      # Identifies identical rows in selected lines and lines data frame
      line_selected <- which(values$DT$x %in% rows_remove$DT$x &
                               values$DT$y %in% rows_remove$DT$y &
                               values$DT$horizontal %in% rows_remove$DT$horizontal)
      
      # Removes selected lines
      values$DT <- values$DT[-line_selected, ]
      rows_remove$DT$x <- c()
      rows_remove$DT$y <- c()
      rows_remove$DT$horizontal <- c()
      
      
    })
    
    ###### Clear all lines 
    
    observeEvent(input$clear, {
      
      values$DT$x <- c()
      values$DT$y <- c()
      values$DT$horizontal <- c()
      
      rows_remove$DT$x <- c()
      rows_remove$DT$y <- c()
      rows_remove$DT$horizontal <- c()
      
    })
    
    observeEvent(input$lines_detect, {
      
      image_write(img(), path =  path_temp, format = "png", 
                  density = 120)
      
      lines <- return_image_lines(path_temp)
      fun <- image_attributes(img())
      height <<- fun$value[str_detect(fun$property, "height")] %>%
        strsplit(., ", ") %>%
        unlist() %>% .[2] %>%
        as.numeric()
      lines <- height - lines - input$lines_wiggle
      
      temp_df <- data.frame(y = lines, 
                 x = rep(1, times = length(lines)), 
                 horizontal = 1) 
      
      values$DT <- rbind.data.frame(values$DT, 
                                    temp_df)
    })  
    
  ####### 4.6 OCR ########
    
  observeEvent(input$google, {
      
    #### 4.6.1 Check user inputs & prepare data ####
      
    # write image to pass it to OCR functions
    # not possible to pass it directly as magick object
    image_write(img(), path = path_temp, format = "png",
                density = 120) 
      
    #### Initialize user provided input
    cols <<- values$DT$x[values$DT$horizontal == 0] %>% sort() # Columns
    y_lim <<- values$DT$y[values$DT$horizontal == 1] %>% sort() # Top/Buttom rule
    
    ##### Check user input #####
    tryCatch({
      
      # Check length of columns, minimum of two columns must be specified
      if(length(cols) < 2){stop()}
      
      # Check appropriate length of y limits
      if(length(y_lim) == 1 | length(y_lim) > 2){stop()}
      
      # Only throw warning does not stop OCR.
      if (length(y_lim) == 0){
        
      # Return warning 
      showNotification("No bottom or top rule indicated. OCR proceeds using
                        entire height of image.", type = "warning")
        
      # set to 0 and image height
      y_lim = c(0, height)
        
      # get image meta data
      height_temp <<- height
      }
      
      
    },
    # Error handling user input
    error = function(cond) {
      
      # Return error message if fewer than two columns given
      if(length(cols) < 2){
        showNotification("Please provide the location of table columns.", 
                         type = "error")}
      
      # Return error message if only one top or bottom rule given
      else if(length(y_lim) == 1 | length(y_lim) > 2){
      showNotification("Please indicate either top & bottom rule or none. 
                        You have either indicated only one or more than 
                       two values.", 
                       type = "error")
      }
      
      else {
        
        showNotification("An unknown error occurred when checking input.", 
                         type = "error")
      }
    }) # ends tryCatch for user input
    
    
    ##### Check Google OCR Auth #####
    tryCatch({
      
      #### Google OCR authentification
      #json_file <- list.files()[str_detect(list.files(), "json")]
      json_file <- auth_file_path
      if(!file.exists(json_file) | length(json_file) > 1){stop()}
      
      # Authenticate
      Sys.setenv("GCV_AUTH_FILE" = json_file)
      
    }, error = function(cond) {
      
      # Error for non-existing json
      if(!file.exists(json_file)){
        
        # Return error 
        showNotification("There is no Google Auth json file in your project folder. 
                          Look in documentation for resources to set up Google OCR.", 
                         type = "error")
      }
      
      # Error for more than one json
      else if(length(json_file) > 1){
        
        # Return error 
        showNotification("There is more than one Google Auth json file in 
                          your project folder. Please make sure there is only one 
                         json file.", 
                         type = "error")
      }
      
      else {
        
        showNotification("An unknown error occurred when checking Google Auth file.", 
                         type = "error")
      }
        
        
    })
    
    
    ##### Run Google OCR using temp file #####
          
    # Run OCR core function (w/ Google)
    tryCatch({
      
      # Google OCR run
      text_df <<- gcv_get_image_annotations(
                  imagePaths = path_temp,
                  feature = "TEXT_DETECTION",
                  maxNumResults = 15
                )
            
      # throw error if no results returned
      if(is.null(text_df$x)){stop()}
            
      # save x coordinates of bounding boxes
      x <<- text_df$x %>% strsplit(., ", ") %>% 
              unlist() %>% 
              as.numeric() %>% 
              matrix(., ncol = 4, byrow = T) %>%
              .[, 1:2]
            
      # save y coordinates of bounding boxes
      y <<- text_df$y %>% strsplit(., ", ") %>% 
              unlist() %>% 
              as.numeric() %>% 
              matrix(., ncol = 4, byrow = T) %>%
              .[, c(1,3)]
            
      # Reverse y for proper displaying and extraction
      y <- height - y
            
      # generate clean text_df data
      text_df$y1 <- y[, 1]
      text_df$y2 <- y[, 2]
      text_df$x1 <- x[, 1]
      text_df$x2 <- x[, 2]
      text_df$centered_y <- y[, 1] + (y[, 2] - y[, 1])/2
      text_df$centered_x <- x[, 1] + (x[, 2] - x[, 1])/2
            
      # rename text_df description
      text_df <- text_df %>% 
        rename("text" = "description")

      # generate clean bbox data
      values$bbox <- data.frame(x1 = x[, 1],
                                y1 = y[, 1],
                                x2 = x[, 2],
                                y2 = y[, 2])
      
      text_df$in_table <- ifelse(text_df$centered_x > cols[1] & 
                                   text_df$centered_x < cols[length(cols)] & 
                                   text_df$y1 > y_lim[1] &
                                   text_df$y2 < y_lim[2], T, F)
      text_df <- text_df %>% filter(in_table)
      
      
      ###### Line segmentation
      
      lines_y <- line_segment_overlap(text_df$centered_y, 
                                      text_df$y2, 
                                      text_df$y1)
      text_df$lines_y <- lines_y
      
      
      # add top and buttom rule to y lines, sort decreasing
      lines_y <- sort(unique(lines_y), decreasing = T)
      
      # set number of columns and rows for target matrix
      no_cols <- length(cols) - 1
      no_rows <- length(lines_y)
      
      # set up target matrix
      temp <- as.data.frame(matrix(ncol = no_cols, nrow = no_rows))
      
      for(i in 2:length(cols)){
        for(j in 1:length(lines_y)){
          
          x_left <- cols[i-1]
          x_right <- cols[i]
          
          # find all cells that
          ## are inbetween col[i-1] and col[i]
          ## are in line j
          cell_attempt <- text_df$text[which(x_left < text_df$centered_x &
                                               x_right > text_df$centered_x &
                                               text_df$lines_y == lines_y[j])]
          
          if(length(cell_attempt) > 1){
            cell <- paste0(cell_attempt, collapse = " ")
          }
          
          if(length(cell_attempt) == 1){
            cell <- cell_attempt
          }
          
          if(length(cell_attempt) == 0){
            cell <- NA
          }
          
          temp[j, i-1] <- cell
          
        }
      }
      
      final$DT <- temp
      file.remove(c(path_temp))},
      
      
      # Error handling
      error = function(cond) {
        
        # Error for more than one json
        if(is.null(text_df$x)){
          
          # Return error 
          showNotification("Looks like the Google OCR yielded no results. Maybe change
                           the area of the table or try another format.", 
                           type = "error")
        }else{

          showNotification("An unknown error occurred when extracting table.", 
                           type = "error")
        }
    }

        
  ) # close last tryCatch
      
  })
    
##### Tesseract ###### 
  observeEvent(input$tesseract, {

    #### 4.6.1 Check user inputs & prepare data ####

    # write image to pass it to OCR functions
    # not possible to pass it directly as magick object
    image_write(img(), path = path_temp, format = "png",
                density = 120)

    #### Initialize user provided input
    cols <<- values$DT$x[values$DT$horizontal == 0] %>% sort() # Columns
    y_lim <<- values$DT$y[values$DT$horizontal == 1] %>% sort() # Top/Buttom rule

    ##### Check user input #####
    tryCatch({

      # Check length of columns, minimum of two columns must be specified
      if(length(cols) < 2){stop()}

      # Check appropriate length of y limits
      if(length(y_lim) == 1 | length(y_lim) > 2){stop()}

      # Only throw warning does not stop OCR.
      if (length(y_lim) == 0){

        # Return warning
        showNotification("No bottom or top rule indicated. OCR proceeds using
                        entire height of image.", type = "warning")

        # set to 0 and image height
        y_lim = c(0, height)

        # get image meta data
        height_temp <<- height
      }


    },
    # Error handling user input
    error = function(cond) {

      # Return error message if fewer than two columns given
      if(length(cols) < 2){
        showNotification("Please provide the location of table columns.",
                         type = "error")}

      # Return error message if only one top or bottom rule given
      else if(length(y_lim) == 1 | length(y_lim) > 2){
        showNotification("Please indicate either top & bottom rule or none.
                        You have either indicated only one or more than
                       two values.",
                         type = "error")
      }

      else {

        showNotification("An unknown error occurred when checking input.",
                         type = "error")
      }
    }) # ends tryCatch for user input

    ###### Baustelle

    # Run OCR core function (w/ Tesseract)

    tryCatch({
      
      # Tesseract run
      lang <- tesseract(input$tesseract_language)
      text_df <- tesseract::ocr_data(path_temp, engine = lang) %>% 
        filter(confidence > input$tesseract_confidence)
      
      # throw error if no results returned
      if(is.null(text_df$bbox)){stop()}
      
      # save x coordinates of bounding boxes
      x <<- text_df$bbox %>% strsplit(., ",") %>%
        unlist() %>%
        as.numeric() %>%
        matrix(., ncol = 4, byrow = T) %>%
        .[, c(1,3)]
      
      # save y coordinates of bounding boxes
      y <<- text_df$bbox %>% strsplit(., ",") %>%
        unlist() %>%
        as.numeric() %>%
        matrix(., ncol = 4, byrow = T) %>%
        .[, c(2,4)]
    
      
      # Reverse y for proper displaying and extraction
      y <- height - y
      
      # generate clean text_df data
      text_df$y1 <- y[, 1]
      text_df$y2 <- y[, 2]
      text_df$x1 <- x[, 1]
      text_df$x2 <- x[, 2]
      text_df$centered_y <- y[, 1] + (y[, 2] - y[, 1])/2
      text_df$centered_x <- x[, 1] + (x[, 2] - x[, 1])/2
      
      
      # rename text_df description
      text_df <- text_df %>%
        rename("text" = "word")
      
      #generate clean bbox data
      values$bbox <- data.frame(x1 = x[, 1],
                                y1 = y[, 1],
                                x2 = x[, 2],
                                y2 = y[, 2])
      
      text_df$in_table <- ifelse(text_df$centered_x > cols[1] &
                                   text_df$centered_x < cols[length(cols)] &
                                   text_df$y1 > y_lim[1] &
                                   text_df$y2 < y_lim[2], T, F)
      text_df <- text_df %>% filter(in_table)
      
      
      ###### Line segmentation
      
      lines_y <- line_segment_overlap(text_df$centered_y,
                                      text_df$y2,
                                      text_df$y1)
      text_df$lines_y <- lines_y
      
      
      # add top and buttom rule to y lines, sort decreasing
      lines_y <- sort(unique(lines_y), decreasing = T)
      
      # set number of columns and rows for target matrix
      no_cols <- length(cols) - 1
      no_rows <- length(lines_y)
      
      # set up target matrix
      temp <- as.data.frame(matrix(ncol = no_cols, nrow = no_rows))
      
      for(i in 2:length(cols)){
        for(j in 1:length(lines_y)){
          
          x_left <- cols[i-1]
          x_right <- cols[i]
          
          # find all cells that
          ## are inbetween col[i-1] and col[i]
          ## are in line j
          cell_attempt <- text_df$text[which(x_left < text_df$centered_x &
                                               x_right > text_df$centered_x &
                                               text_df$lines_y == lines_y[j])]
          
          if(length(cell_attempt) > 1){
            cell <- paste0(cell_attempt, collapse = " ")
          }
          
          if(length(cell_attempt) == 1){
            cell <- cell_attempt
          }
          
          if(length(cell_attempt) == 0){
            cell <- NA
          }
          
          print(cell)
          temp[j, i-1] <- cell
          
        }
      }
      
      
      
      final$DT <- temp
      file.remove(c(path_temp))},
      
      
      # Error handling
      error = function(cond) {
        
        # Error for more than one json
        if(is.null(text_df$x)){
          
          # Return error 
          showNotification("Looks like the Google OCR yielded no results. Maybe change
                           the area of the table or try another format.", 
                           type = "error")
        }else{
          
          showNotification("An unknown error occurred when extracting table.", 
                           type = "error")
        }  
      
      
    })
    
    
    
  })

  ####### 4.8 Save results ########
    
    ## Append previous results to all results ## 
    
    observeEvent(input$append_previous, {
      
      tryCatch({
        
        all_pages$final_df <- rbind.data.frame(all_pages$final_df, final$DT)
        
      }, error = function(cond) {
        
        if(ncol(all_pages$final_df) != ncol(final$DT) & 
           nrow(all_pages$final_df) != 0){
          
          showNotification("Check whether 'OCR Results' and 'All results'
                         have same number of columns", type = "error")
        }else{
          
          showNotification("An unknown error occurred when adding OCR results to
                           All Results.", 
                           type = "error")
          
        }
        
        
        
      }
    ) # end tryCatch
      
          
    }) # end add previous event
    
    
    ## Save current table only ##
    
    output$download_current <- downloadHandler(
      filename = function(){
        input$filename
      },
      content = function(file) {
        write.csv(final$DT, file)
      }
    )
    
    ## Save current table only ##
    
    output$download_all <- downloadHandler(
      filename = function(){
        input$filename
      },
      content = function(file) {
        write.csv(all_pages$final_df, file)
      }
    )
    
    
    ###### 4.9 Showing Output #######
    
    ##### Table in Tabset #####
    output$table_final <- renderTable({all_pages$final_df})
    
    ##### Table in Tabset #####
    output$table <- renderTable({final$DT})
    
    
    ##### Image with lines printed on #####
    observe({
      output$raster <- renderPlot({
        req(img())
        
        plot(img())
        abline(h = values$DT$y[values$DT$horizontal == 1])
        abline(v = values$DT$x[values$DT$horizontal == 0])
        abline(h = rows_remove$DT$y[rows_remove$DT$horizontal == 1], col = "red")
        abline(v = rows_remove$DT$x[rows_remove$DT$horizontal == 0], col = "blue")
        
        if(input$visual == "on"){
          abline(h = values$rows$y, col = "red")
          rect(values$bbox$x1, values$bbox$y1, 
               values$bbox$x2, values$bbox$y2, border = "darkgreen")
        }
      }, height = input$zoom, width = input$zoom)
    })
  }

shinyApp(ui = ui, server = server)

}


