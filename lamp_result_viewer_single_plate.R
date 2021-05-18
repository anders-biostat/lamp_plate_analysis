library(hwriter)
library( readxl )
library( later )
library( httr )
library( tidyverse )
library( rlc )

controls <- c("ACTB", "Actin", "Zika")
results_order <- rev(1:5)
names(results_order) <- c("positive", "inconclusive", "negative", "repeat", "failed")

tecan_workbook <- commandArgs(TRUE)[1]
if(!file.exists(tecan_workbook))
  stop(str_c("File not found: ", tecan_workbook))
rack_to_plate_path <- file.path(dirname(tecan_workbook), 
                                "Files from Hamilton",
                                "1500InputOutputMapping.csv")

rack_to_plate = tibble(rack = "__empty__", plate = "__empty__")

if(file.exists(rack_to_plate_path)) {
  read_csv2(rack_to_plate_path) %>%
    mutate_all(~str_remove(., "\\w$")) -> rack_to_plate
  colnames(rack_to_plate) <- c("rack", "plate")
}
  

read_tecan_sheet <- function( filename, sheet ) {
  inner_join( by="well",
    read_excel( filename, sheet, skip=24, n_max=384 ) %>%
      assertr::verify( identical( colnames(.), c( "<>", "Value" ) ) ) %>%
      rename( "well" = "<>", "od434" = "Value" ),
    read_excel( filename, sheet, skip=425, n_max=384 ) %>%
      assertr::verify( identical( colnames(.), c( "<>", "Value" ) ) ) %>%
      rename( "well" = "<>", "od560" = "Value" ) )
}

read_excel( tecan_workbook, "PrimerSetsUsed" ) %>%
  rename( "plate"="Plate-ID", "corner" = "A1 position of 96-well in 384-well" ) %T>%
  {corners_all <<- .} %>%
  select(plate) %>%
  distinct() %>%
  full_join(rack_to_plate) %>%
  filter(plate != "__empty__") %>%
  mutate(rack = ifelse(is.na(rack), plate, rack)) %>%
  mutate(filename = str_c(rack, "_barcodes.xlsx"), 
         dirname = file.path(dirname(dirname(tecan_workbook)), rack)) %>%
  mutate(fullpath = file.path(dirname, filename)) %>%
  group_by(plate, rack, fullpath) %>%
  summarise(barcodes = list(read_excel(fullpath, 
                                       range = cell_cols("A:E"))),
            .groups = "drop") %>%
  unnest(barcodes) %>%
  rename( "well96" = "Tube Position", 
          "content" = "Type",
          "tubeId" = "Tube ID",
          "comment" = "Comment") %>%
  select(-fullpath, -`Rack ID`) %>%
  mutate(comment = ifelse(is.na(comment), "", comment)) %>%
  mutate_at( "well96", str_replace, "0(\\d+)", "\\1" ) -> contents_all

if(length(setdiff(corners_all$plate, contents_all$plate)) > 0)
  stop(str_interp("Content information is missing for the following plates: ${setdiff(corners_all$plate, contents_all$plate)}"))

excel_sheets( tecan_workbook ) %>%
  { tibble( sheet = . ) } %>%
  filter(!(sheet %in% c("PrimerSetsUsed", "Barcodes_SampleTypes"))) %>%
  group_by_all() %>%
  summarise(header = read_excel(tecan_workbook, sheet, "E14:E15", col_names = FALSE)[["...1"]]) %>%
  mutate(type = c("plate", "minutes")) %>%
  pivot_wider(names_from = type, values_from = header) %>%
  mutate( minutes = str_match( minutes, "(\\d+)\\w*((min)?)" ) %>% `[`(,2) ) %>%
  group_by_all() %>%
  summarise( a = list( read_tecan_sheet( tecan_workbook, sheet ) ), .groups = "drop" ) %>%
  unnest( a ) %>%
  mutate( row = ( str_sub( well, 1, 1 ) %>% map_int( utf8ToInt ) )- utf8ToInt("A") + 1  ) %>%
  mutate( col = as.integer( str_sub( well, 2, -1 ) ) ) %>%
  mutate( row96 = ceiling( row / 2 ) ) %>%
  mutate( col96 = ceiling( col / 2 ) ) %>%
  mutate( well96 = str_c( LETTERS[row96], col96 ) ) %>%
  mutate( corner = str_c( LETTERS[ 1 + (row+1) %% 2 ], 1 + (col+1) %% 2 ) ) %>%
  mutate(dOd = od434 - od560) %T>%
  {contents_all <<- select(., well, well96, corner, row96, col96, plate) %>%
    distinct() %>%
    mutate(row96Letter = LETTERS[row96]) %>%
    pivot_wider(names_from = corner, values_from = well) %>%
    left_join(contents_all)} %>%
  select(-od434, -od560, -row, -col, -row96, -col96, -well) %>%
  pivot_wider(names_from = well96, values_from = dOd) -> tblWide_all

tblWide_all %>%
  pivot_longer(names_to = "well96", values_to = "diff", -(sheet:corner)) %>%
  left_join(corners_all) %>%
  mutate(isControl = PrimerSet %in% controls) %>%
  filter((minutes <= 25 & !isControl) | (minutes <= 20 & isControl))  %>%
  group_by(plate, corner, well96, isControl) %>%
  summarise(baseline = mean(diff[minutes <= 10]),
            maxDiff = max(diff), .groups = "drop") %>%
  mutate(result = ifelse(maxDiff >= 0.1, "positive", "negative")) %>%
  group_by(well96, plate) %>% 
  summarise(positiveTest = sum(result == "positive" & !isControl),
            positiveControl = sum(result == "positive" & isControl),
            totalTest = sum(!isControl),
            totalControl = sum(isControl),
            lowBaseline = sum(baseline <= -0.1),
            .groups = "drop") %>%
  mutate(result = case_when(
    lowBaseline < totalTest + totalControl ~ "repeat",
    positiveTest == totalTest ~ "positive",
    positiveControl < totalControl ~ "repeat",
    positiveTest == 0 ~ "negative",
    TRUE ~ "inconclusive")) %>%
  select(-(positiveTest:lowBaseline)) %>%
  mutate(assigned = result) %>%
  right_join(contents_all) -> contents_all

colourBy <- "content"
highlighted <- -1

palette <- list(content = data.frame(colour = c("#79dd79", "#1cb01c", "#0e580e",
                                                "#c67c3b", "#4979e3", "#aeafb0"),
                                    type = c("positive control CT32", "positive control CT29", "positive control CT26", 
                                             "sample", "negative control", "empty"),
                                    stringsAsFactors = FALSE),
                assigned = data.frame(colour = c("#48b225", "#f58e09", "#d22d2d", "#194689", "#270404"),
                                    type = c("negative", "inconclusive", "positive", "repeat", "failed"),
                                    stringsAsFactors = FALSE))

getOpacity <- function(highlighted) {
  if(highlighted == -1) {
    op <- rep(1, nrow(layout))
    op[layout$content == "empty"] <- 0.05
  } else {
    op <- rep(0.2, nrow(layout))
    op[layout$content == "empty"] <- 0.05
    op[highlighted] <- 1
  }
  op
}
getContent <- function(highlighted) {
  if(highlighted == -1) {
    ses$sendCommand("d3.select('#highlighted').classed('failed', false);")
    return("No highlighted samples")
  }
  if(layout$assigned[highlighted] == "failed") {
    ses$sendCommand("d3.select('#highlighted').classed('failed', true);")
  } else {
    ses$sendCommand("d3.select('#highlighted').classed('failed', false);")
  }
  
  filter(contents, well96  == layout$well96[highlighted]) %>%
    mutate(rack = str_c("<b>", rack, "</b>")) -> highlighted_data
  select(highlighted_data, rack, tubeId, assigned, comment) %>%
    unlist() %>%
    matrix(nrow = 4, byrow = TRUE) %>%
    hwrite(border = 0, width = "100%", 
           style = "margin-top: 0; text-align: center; padding-top: 0;") -> table
  
  str_c(c(highlighted_data$well96[1], " -> ", highlighted_data[1, c("A1", "A2", "B1", "B2")] %>% 
                           unlist %>% 
                           str_c(collapse = ", "), ";<br>",
                         "<center>", highlighted_data$content[1], "</center>",
                         table), collapse = "")
}

clearHighlighted <- function() {
  if(highlighted == -1){
    updateCharts(chartId = c("A1", "A2", "B1", "B2", "content", "assigned"), 
                 updateOnly = "ElementStyle")
    updateCharts("highlighted")
    ses$sendCommand("d3.select('#highlighted').classed('failed', false);")
  }
}

assign <- function(new_type) {
  wells <- layout$well96[unique(c(getMarked("content"), getMarked("assigned")))]
  if(length(wells) > 0){
    contents <<- mutate(contents, assigned = ifelse(well96 %in% wells, 
                                                    new_type, assigned))
    layout <<- getLayout(contents)
    updateCharts("assigned", updateOnly = "ElementStyle")
    if(colourBy == "result")
      updateCharts(c("A1", "A2", "B1", "B2"), updateOnly = "ElementStyle")
    updateMessage("New statuses assigned")
  }
}

comment <- function(com = NULL) {
  wells <- layout$well96[unique(c(getMarked("content"), getMarked("assigned")))]
  if(length(wells) > 0 & !is.null(com)){
    mark(c(), "content")
    mark(c(), "assigned")
    contents <<- mutate(contents, comment = ifelse(well96 %in% wells, 
                                                    com, comment))
    layout <<- getLayout(contents)
    updateMessage("Comments added")
  }
}

reset <- function() {
  contents$assigned <- contents$result
  contents <<- contents
  layout <<- getLayout(contents)
  updateCharts(allCharts, updateOnly="ElementStyle")
}

export <- function() {
  saveAssignment()
  file_name <- str_c(format(Sys.time(), "%y%m%d_%H%M%S_"),
                     str_replace(basename(tecan_workbook), "\\.\\w+$", ".csv"))
  contents_all %>%
    filter(content == "sample") %>%
    select(-result) %>%
    rename(result = assigned) %>%
    mutate(LAMPStatus = case_when(result == "positive" ~ "LAMPPOS",
                                  result == "negative" ~ "LAMPNEG",
                                  result == "repeat" ~ "LAMPREPEAT",
                                  result == "failed" ~ "LAMPFAILED",
                                  result == "inconclusive" ~ "LAMPINC")) %>%
    select(plate, well96, tubeId, result, LAMPStatus, comment) %>%
    write_csv(file.path(dirname(tecan_workbook), file_name))
  
  messages[plates] <- str_c("Last exported at ", format(Sys.time(), "%H:%M:%S"))
  messages <<- messages
  updateMessage(messages[1])
}

updateMessage <- function(message) {
  ses$sendCommand(str_c("d3.select('#plateMessage').text('", message, "');"))
}

saveAssignment <- function() {
  contents_all <<- left_join(contents_all, select(contents, assigned, comment, plate, rack, well96), 
                             by = c("plate", "rack", "well96"), suffix = c("", "_new")) %>%
    mutate(comment = if_else(is.na(comment_new), comment, comment_new),
           assigned = if_else(is.na(assigned_new), assigned, assigned_new)) %>%
    select(-assigned_new, -comment_new)
  
}

switchPlate <- function(pl) {
  if(pl %in% plates) {
    saveAssignment()
    contents <<- filter(contents_all, plate == pl)
    layout <<- getLayout(contents)
    tblWide <<- filter(tblWide_all, plate == pl)
    corners <<- filter(corners_all, plate == pl) %>%
      column_to_rownames("corner")
    updateCharts(allCharts)
    updateMessage(messages[pl])
  }
}

post <- function(username, password) {
  auth <- authenticate(username, password)
  
  posted <- tibble()
  
  contents %>%
    ungroup() %>%
    filter(content == "sample") %>%
    mutate(LAMPStatus = case_when(assigned == "positive" ~ "LAMPPOS",
                                  assigned == "negative" ~ "LAMPNEG",
                                  assigned == "repeat" ~ "LAMPREPEAT",
                                  assigned == "failed" ~ "LAMPFAILED",
                                  assigned == "inconclusive" ~ "LAMPINC")) %>%
    select(tubeId, LAMPStatus, rack, comment) %>%
    rename(barcode = tubeId, status = LAMPStatus) %T>%
    {posted <<- .} %>%
    rowwise() %>%
    do(response = POST("https://covidtest-hd.de/lab/samples/update_status", 
                        auth, encode = "json", body = as.list(.)),
       barcode = .$barcode) -> resps
  
    res <- t(sapply(pull(resps, response), function(r) {
            c(str_extract(http_status(r)$message, "\\d+"), 
              str_c(capture.output(message(r), type = "message"), collapse = ""))
    }))
    res <- cbind(sapply(resps$barcode, `[[`, 1), res)
    colnames(res) <- c( "barcode", "status", "message")
    
    if(all(res[, "status"] == "403")){
      ses$callFunction("wrongPassword")
    } else {
      res %>%
        as_tibble() %>%
        filter(status != "201") -> errorLog
      
      ses$callFunction("reportSuccess", 
                       list(sum(res[, "status"] == "201"), 
                            sum(contents$content == "sample"),
                            errorLog))
      messages[contents$plate[1]] <- str_c("Last posted at ", format(Sys.time(), "%H:%M:%S"))
      messages <<- messages
      updateMessage(messages[contents$plate[1]])
      
      file_name <- str_c(format(Sys.time(), "%y%m%d_%H%M%S_postLog_"), contents$plate[1], "_",
                         str_replace(basename(tecan_workbook), "\\.\\w+$", ".csv"))
      
      res %>%
        as_tibble() %>%
        rename(response_status = status) %>%
        left_join(rename(posted, new_status = status)) %>%
        write_csv(file.path(dirname(dirname(tecan_workbook)), contents$plate[1], file_name))
    }
}

getX <- function(cnr) {
  parse(text = str_c("as.numeric(filter(tblWide, corner == '", cnr, "') %>% pull(minutes))"))
}
getY <- function(cnr) {
  parse(text = str_c("(filter(tblWide, corner == '", cnr, "') %>% select(-(sheet:corner)) %>% as.matrix())[, layout$well96]"))
}

getTitle <- function(cnr) {
  parse(text = str_c('sprintf( "corner %s: plate %s, primer set %s", "', cnr, 
                      '", corners["', cnr, '", "plate"], corners["', cnr, '", "PrimerSet"] )'))
}

getLayout <- function(contents) {
  contents %>%
    group_by(well96, col96, row96Letter) %>%
    summarise(same_result = length(unique(assigned)) == 1,
              assigned = assigned[which.max(results_order[assigned])],
              content = content[1])
}

plates <- unique(corners_all$plate)
contents <- filter(contents_all, plate == plate[1])
layout <- getLayout(contents)
tblWide <- filter(tblWide_all, plate == plates[1])
corners <- filter(corners_all, plate == plates[1]) %>%
  column_to_rownames("corner")

messages <- setNames(rep("", length(plates)), plates)

last <- function() {}
loop <- create_loop()

app <- openPage( FALSE, startPage = "plateBrowser_sp.html", 
                 allowedFunctions = c("assign", "comment", "export", "reset", "switchPlate",
                                      "post"))
ses <- app$getSession()

opts <- plates
if(rack_to_plate$rack[1] != "__empty__")
  rack_to_plate %>%
    group_by(plate) %>%
    summarise(racks = paste(rack, collapse = ", ")) %>%
    unite("option", plate, racks, sep = ", racks: ") %>%
    pull(option) -> opts
    

ses$callFunction("addPlates", list(opts, plates), keepAsVector = TRUE)

allCharts <- c("A1", "A2", "B1", "B2", "assigned", "content")

# let's check that each well-plate combination is unique
contents_all %>%
  group_by(well96, rack) %>%
  tally() %>%
  filter(n > 1) -> spurious
if(nrow(spurious) > 0) {
  ses$sendData("spurious", spurious)
  ses$callFunction("alertContentProblem", list("duplicates"))
  while(length(app$getSessionIds()) > 0)
    httpuv::service()
  stop("Duplicated wells detected.")
}

# let's also check that content of the pooled wells is the same
# TO DO: Ask if one can theoretically pool empty well and sample
contents_all %>%
  group_by(plate, well96) %>%
  summarise(n = length(unique(content))) %>%
  filter(n > 1) -> spurious
if(nrow(spurious) > 0) {
  ses$sendData("spurious", spurious)
  ses$callFunction("alertContentProblem", list("mixed_content"))
  while(length(app$getSessionIds()) > 0)
    httpuv::service()
  stop("Pooled wells with mixed content detected.")
}


for( cnr in c( "A1", "A2", "B1", "B2" ) ) {
  lc_vLine(v = ifelse(corners[cnr, "PrimerSet"] %in% controls, 20, 25), 
           dasharray = 5, transitionDuration = 0, colour = "#777", place = cnr)
  lc_hLine(h = 0.1, dasharray = 5, transitionDuration = 0, colour = "#777", chartId = cnr, addLayer = TRUE)
  
  lc_line(
    dat(opacity = getOpacity(highlighted),
        lineWidth = ifelse(1:nrow(layout) == highlighted, 3, 1),
        palette = palette[[colourBy]][["colour"]],
        colourDomain = palette[[colourBy]][["type"]],
        colourValue = layout[[colourBy]]),
    x = getX(cnr),
    y = getY(cnr),
    title = getTitle(cnr),
    mode = "canvas",
    transitionDuration = 0,
    showLegend = FALSE,
    height = 300,
    titleSize = 18,
    on_mouseover = function(d) {
      highlighted <<- d
      last()
      updateCharts("highlighted")
      updateCharts(allCharts, updateOnly = "ElementStyle")
    },
    on_mouseout = function(d) {
      highlighted <<- -1
      last <<- later(clearHighlighted, 0.4, loop)
    },
    chartId = cnr, addLayer = TRUE)
}

lc_scatter(dat(opacity = getOpacity(highlighted), 
               x = layout$col96, y = layout$row96Letter,
               colourValue = layout$content,
               title = str_c("Plate ", corners$plate[1])),
  domainY = LETTERS[8:1],
  titleSize = 20,
  palette = palette$content$colour,
  colourDomain = palette$content$type,
  height = 290,
  width = 550,
  paddings = list(top = 35, right = 10, left = 20, bottom = 20),
  strokeWidth = 2,
  stroke = "black",
  on_mouseover = function(d) {
      highlighted <<- d
      updateCharts("highlighted")
      updateCharts(allCharts, updateOnly = "ElementStyle")
  },
  on_mouseout = function() {
    highlighted <<- -1
    clearHighlighted()
  },
  on_marked = function() {
    mark(getMarked(.chartId), "assigned")
  },    
  on_click = function(d) {
    if(colourBy != "content") {
      colourBy <<- "content"
      updateCharts(c("A1", "A2", "B1", "B2"), updateOnly = "ElementStyle")
    }
  },
  showLegend = FALSE,
  transitionDuration = 0,
  size = 10,
  place = "plates", chartId = "content")

lc_scatter(dat(opacity = getOpacity(highlighted), 
               x = layout$col96, y = layout$row96Letter, 
               colourValue = layout$assigned),
           domainY = LETTERS[8:1],
           palette = palette$assigned$colour,
           colourDomain = palette$assigned$type,
           height = 265,
           width = 550,
           paddings = list(top = 10, right = 10, left = 20, bottom = 20),
           showPanel = FALSE,
           strokeWidth = 2,
           stroke = "black",
           on_mouseover = function(d) {
             highlighted <<- d
             updateCharts("highlighted")
             updateCharts(allCharts, updateOnly = "ElementStyle")
           },
           on_mouseout = function() {
             highlighted <<- -1
             clearHighlighted()
           },
           on_marked = function() {
             mark(getMarked(.chartId), "content")
           },
           on_click = function(d) {
             if(colourBy != "assigned") {
               colourBy <<- "assigned"
               updateCharts(c("A1", "A2", "B1", "B2"), updateOnly = "ElementStyle") 
             }
           },
           showLegend = FALSE,
           transitionDuration = 0,
           size = 10,
           place = "plates", chartId = "assigned")

lc_html(dat(content = getContent(highlighted)), place = "highlighted")

ses$sendCommand(str_c("charts.A1.legend.container(d3.select('#info').select('#legend_sample')).legend.sampleHeight(25).legend.width(250);",
                      "charts.A1.showLegend(true).update();"))
ses$sendCommand(str_c("charts.assigned.legend.container(d3.select('#info').select('#legend_res')).legend.sampleHeight(30);",
                      "charts.assigned.showLegend(true).update();"))
ses$sendCommand('d3.selectAll("#legend_res").selectAll("text").attr("font-size", 17).attr("dy", 7)')
ses$sendCommand('d3.selectAll("#legend_sample").selectAll("text").attr("font-size", 17).attr("dy", 7)')
fileName <- basename(tecan_workbook)
ses$sendCommand(str_interp('d3.select("h3").html("File: <i>${fileName}</i>")'))

while(length(app$getSessionIds()) > 0)
  httpuv::service()

Sys.sleep(3)
