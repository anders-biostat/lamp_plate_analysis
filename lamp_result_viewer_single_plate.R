library( later )
library(httr)
library( tidyverse )

controls <- c("ACTB", "Actin", "Zika")

tecan_workbook <- commandArgs(TRUE)[1]
if(!file.exists(tecan_workbook))
  stop(str_c("File not found: ", tecan_workbook))

read_tecan_sheet <- function( filename, sheet ) {
  inner_join( by="well",
    readxl::read_excel( filename, sheet, skip=24, n_max=384 ) %>%
      assertr::verify( identical( colnames(.), c( "<>", "Value" ) ) ) %>%
      rename( "well" = "<>", "od434" = "Value" ),
    readxl::read_excel( filename, sheet, skip=425, n_max=384 ) %>%
      assertr::verify( identical( colnames(.), c( "<>", "Value" ) ) ) %>%
      rename( "well" = "<>", "od560" = "Value" ) )
}

readxl::read_excel( tecan_workbook, "PrimerSetsUsed" ) %>%
  rename( "plate"="Plate-ID", "corner" = "A1 position of 96-well in 384-well" ) -> corners_all

readxl::read_excel( tecan_workbook, "Barcodes_SampleTypes" )  %>%
  rename( "well96" = "Tube Position", 
          "plate" = "Rack ID", 
          "content" = "Type",
          "tubeId" = "Tube ID") %>%
  mutate_at( "well96", str_replace, "0(\\d+)", "\\1" ) -> contents_all

if(length(setdiff(corners_all$plate, contents_all$plate)) > 0)
  stop(str_interp("Content information is missing for the following plates: ${setdiff(corners$plate, contents$plate)}"))

readxl::excel_sheets( tecan_workbook ) %>%
  { tibble( sheet = . ) } %>%
  filter(!(sheet %in% c("PrimerSetsUsed", "Barcodes_SampleTypes"))) %>%
  group_by_all() %>%
  summarise(header = readxl::read_excel(tecan_workbook, sheet, skip = 12, col_names = FALSE)[["...5"]][1:2]) %>%
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
  group_by(well96, corner, plate) %>%
  mutate(baseline = mean(diff[minutes <= 10])) %>%
  mutate(increase = diff - baseline) %>%
  filter(minutes == 25) %>%
  left_join(corners_all) %>%
  mutate(result = ifelse(increase > 0.4, "positive", "negative"),
         isControl = PrimerSet %in% controls) %>% 
  group_by(well96, plate) %>% 
  summarise(positiveTest = sum(result == "positive" & !isControl),
            positiveControl = sum(result == "positive" & isControl),
            totalTest = sum(!isControl),
            totalControl = sum(isControl)) %>%
  mutate(result = case_when(positiveTest == totalTest ~ "positive",
                   positiveControl < totalControl ~ "repeat",
                   positiveTest == 0 ~ "negative",
                   TRUE ~ "inconclusive")) %>%
  select(-(positiveTest:totalControl)) %>%
  mutate(comment = "", assigned = result) %>%
  right_join(contents_all) -> contents_all

library( rlc )

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
    op <- rep(1, nrow(contents))
    op[contents$content == "empty"] <- 0.05
  } else {
    op <- rep(0.2, nrow(contents))
    op[contents$content == "empty"] <- 0.05
    op[highlighted] <- 1
  }
  op
}
getContent <- function(highlighted) {
  if(highlighted == -1) {
    ses$sendCommand("d3.select('#highlighted').classed('failed', false);")
    return("No highlighted lines")
  }
  if(contents$result[highlighted] == "failed") {
    ses$sendCommand("d3.select('#highlighted').classed('failed', true);")
  } else {
    ses$sendCommand("d3.select('#highlighted').classed('failed', false);")
  }
  str_c(str_replace_na(c("Highlighted: ", contents$tubeId[highlighted], "<br>", 
                         contents$content[highlighted], "<br>",
                         "96 well position: ", contents$well96[highlighted], "<br>",
                         "384 well position: ", contents[highlighted, c("A1", "A2", "B1", "B2")] %>% 
                           unlist %>% 
                           str_c(collapse = ", "), "<br><i>",
                         contents$comment[highlighted], "</i>")), 
        collapse = "")
  
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
  wells <- unique(c(getMarked("content"), getMarked("assigned")))
  if(length(wells) > 0){
    mark(c(), "content")
    mark(c(), "assigned")
    contents[wells, "assigned"] <- new_type
    contents <<- contents
    updateCharts("assigned", updateOnly = "ElementStyle")
    if(colourBy == "result")
      updateCharts(c("A1", "A2", "B1", "B2"), updateOnly = "ElementStyle")
    updateMessage("New statuses assigned")
  }
}

comment <- function(com = NULL) {
  wells <- unique(c(getMarked("content"), getMarked("assigned")))
  if(length(wells) > 0 & !is.null(com)){
    mark(c(), "content")
    mark(c(), "assigned")
    contents[wells, "comment"] <- com
    contents <<- contents
    updateMessage("Comments added")
  }
}

reset <- function() {
  contents$assigned <- contents$result
  contents <<- contents
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
                                  result == "repeat" ~ "WAIT",
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
  contents_all <<- left_join(contents_all, select(contents, assigned, comment, plate, well96), 
                             by = c("plate", "well96"), suffix = c("", "_new")) %>%
    mutate(comment = if_else(is.na(comment_new), comment, comment_new),
           assigned = if_else(is.na(assigned_new), assigned, assigned_new)) %>%
    select(-assigned_new, -comment_new)
  
}

switchPlate <- function(pl) {
  if(pl %in% plates) {
    saveAssignment()
    contents <<- filter(contents_all, plate == pl)
    tblWide <<- filter(tblWide_all, plate == pl)
    corners <<- filter(corners_all, plate == pl) %>%
      column_to_rownames("corner")
    updateCharts(allCharts)
    updateMessage(messages[pl])
  }
}

post <- function(username, password) {
  auth <- authenticate(username, password)
  
  contents %>%
    ungroup() %>%
    filter(content == "sample") %>%
    mutate(LAMPStatus = case_when(assigned == "positive" ~ "LAMPPOS",
                                  assigned == "negative" ~ "LAMPNEG",
                                  assigned == "repeat" ~ "WAIT",
                                  assigned == "failed" ~ "LAMPFAILED",
                                  assigned == "inconclusive" ~ "LAMPINC")) %>%
    select(tubeId, LAMPStatus, plate, comment) %>%
    rename(barcode = tubeId, status = LAMPStatus, rack = plate) %>%
    rowwise() %>%
    do(response = POST("http://127.0.0.1:8000/lab/samples/update_status", 
                        auth, encode = "json", body = as.list(.)),
       barcode = .$barcode) -> resps
  
    res <- t(sapply(pull(resps, response), function(r) {
            c(str_extract(http_status(r)$message, "\\d+"), 
              str_c(capture.output(message(r), type = "message"), collapse = ""))
    }))
    res <- cbind(sapply(resps$barcode, `[[`, 1), res)
    colnames(res) <- c("status", "message", "barcode")
    
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
      
      if(nrow(errorLog) > 0) {
        file_name <- str_c(format(Sys.time(), "%y%m%d_%H%M%S_errorLog_"), contents$plate[1], "_"
                           str_replace(basename(tecan_workbook), "\\.\\w+$", ".csv"))
        write_csv(errorLog, file.path(dirname(tecan_workbook), file_name))
      }
    }
}

getX <- function(cnr) {
  parse(text = str_c("as.numeric(filter(tblWide, corner == '", cnr, "') %>% pull(minutes))"))
}
getY <- function(cnr) {
  parse(text = str_c("(filter(tblWide, corner == '", cnr, "') %>% select(-(sheet:corner)) %>% as.matrix())[, contents$well96]"))
}

getTitle <- function(cnr) {
  parse(text = str_c('sprintf( "corner %s: plate %s, primer set %s", "', cnr, 
                      '", corners["', cnr, '", "plate"], corners["', cnr, '", "PrimerSet"] )'))
}

plates <- unique(corners_all$plate)
contents <- filter(contents_all, plate == plates[1])
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

ses$callFunction("addPlates", list(plates, 1), keepAsVector = TRUE)

allCharts <- c("A1", "A2", "B1", "B2", "assigned", "content")

for( cnr in c( "A1", "A2", "B1", "B2" ) ) {
  lc_line(
    dat(opacity = getOpacity(highlighted),
        lineWidth = ifelse(1:nrow(contents) == highlighted, 3, 1),
        palette = palette[[colourBy]][["colour"]],
        colourDomain = palette[[colourBy]][["type"]],
        colourValue = contents[[colourBy]]),
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
    place = cnr)
}

lc_scatter(dat(opacity = getOpacity(highlighted), 
               x = contents$col96, y = contents$row96Letter,
               colourValue = contents$content,
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
               x = contents$col96, y = contents$row96Letter, 
               colourValue = contents$assigned),
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
