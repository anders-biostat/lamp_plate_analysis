library( later )
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
  rename( "plate"="Plate-ID", "corner" = "A1 position of 96-well in 384-well" ) %>%
  mutate_at( "plate", str_replace_all, " ", "_" ) %>%
  mutate_at( "plate", str_replace_all, "-", "." ) -> corners_all

readxl::read_excel( tecan_workbook, "Barcodes_SampleTypes" )  %>%
  rename( "well96" = "Tube Position", 
          "plate" = "Rack ID", 
          "content" = "Type",
          "tubeId" = "Tube ID") %>%
  mutate_at( "plate", str_replace_all, " ", "_" ) %>%
  mutate_at( "plate", str_replace_all, "-", "." ) %>%
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
  mutate_at( "plate", str_replace_all, " ", "_" ) %>%
  mutate_at( "plate", str_replace_all, "-", "." ) %>%
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
  group_by(well96, corner) %>%
  mutate(baseline = mean(diff[minutes <= 10])) %>%
  mutate(increase = diff - baseline) %>%
  filter(minutes  == 20) %>%
  left_join(corners_all) %>%
  mutate(result = ifelse(increase > 0.5, "positive", "negative"),
         isControl = PrimerSet %in% controls) %>% 
  group_by(well96, plate) %>%
  summarise(positiveTest = sum(result == "positive" & !isControl),
            positiveControl = sum(result == "positive" & isControl),
            totalTest = sum(!isControl),
            totalControl = sum(isControl)) %>%
  mutate(result = case_when(positiveTest == totalTest ~ "positive",
                   positiveControl < totalControl ~ "failed",
                   positiveTest == 0 ~ "negative",
                   TRUE ~ "repeat")) %>%
  select(-(positiveTest:totalControl)) %>%
  mutate(comment = "", assigned = result) %>%
  right_join(contents_all) %>%
  mutate(content = ifelse(str_detect(content, "^positive"), "positive control", content)) -> contents_all

library( rlc )

colourBy <- "content"

palette <- list(content = data.frame(colour = c("#1cb01c", "#c67c3b", "#4979e3", "#aeafb0"),
                                    type = c("positive control", "sample", "negative control", "empty"),
                                    stringsAsFactors = FALSE),
                assigned = data.frame(colour = c("#48b225", "#f58e09", "#d22d2d", "#270404"),
                                    type = c("negative", "repeat", "positive", "failed"),
                                    stringsAsFactors = FALSE))

getOpacity <- function(highlighted) {
  if(highlighted == -1) {
    op <- rep(1, nrow(contents))
  } else {
    op <- rep(0.2, nrow(contents))
    op[highlighted] <- 1
  }
  op
}
getContent <- function(highlighted) {
  if(highlighted == -1) {
    ses$sendCommand("d3.select('#highlighted').classed('failed', false);")
    return("No highlighted lines")
  }
  if(contents$result[highlighted] == "control failed") {
    ses$sendCommand("d3.select('#highlighted').classed('failed', true);")
  } else {
    ses$sendCommand("d3.select('#highlighted').classed('failed', false);")
  }
  str_c(str_replace_na(c("Highlighted: ", contents$tubeId[highlighted], "<br>",
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
  }
}

comment <- function(com = NULL) {
  wells <- unique(c(getMarked("content"), getMarked("assigned")))
  if(length(wells) > 0 & !is.null(com)){
    mark(c(), "content")
    mark(c(), "assigned")
    contents[wells, "comment"] <- com
    contents <<- contents    
  }
}

reset <- function() {
  contents$assigned <- contents$result
  contents <<- contents
  updateCharts(allCharts, updateOnly="ElementStyle")
}

export <- function() {
  contents %>%
    filter(content == "sample") %>%
    select(well96, tubeId, assigned, comment) %>%
    rename(result = assigned) %>%
    mutate(LAMPStatus = case_when(result == "positive" ~ "LAMPPOS",
                                  result == "negative" ~ "LAMPNEG",
                                  result == "repeat" ~ "WAIT",
                                  result == "failed" ~ "LAMPWAIT",
                                  TRUE ~ NA)) %>%
    write_csv(str_replace(tecan_workbook, "\\.\\w+$", ".csv"))
}

plates <- unique(corners_all$plate)
pl <- plates[1]
contents <- filter(contents_all, plate == pl)
tblWide <- filter(tblWide_all, plate == pl)
corners <- filter(corners_all, plate == pl) %>%
  column_to_rownames("corner")

last <- function() {}
loop <- create_loop()

app <- openPage( FALSE, startPage = "plateBrowser_sp.html", 
                 allowedFunctions = c("assign", "comment", "export", "reset"))
ses <- app$getSession()

allCharts <- c("A1", "A2", "B1", "B2", "assigned", "content")

for( cnr in c( "A1", "A2", "B1", "B2" ) ) {
  data <- filter(tblWide, corner == cnr)
  highlighted <- -1
  plate <- corners$plate[1]
  
  lc_line(
    dat(opacity = getOpacity(highlighted),
        lineWidth = ifelse(1:nrow(contents) == highlighted, 3, 1),
        palette = palette[[colourBy]][["colour"]],
        colourDomain = palette[[colourBy]][["type"]],
        colourValue = contents[[colourBy]]),
    x = as.numeric(data$minutes),
    y = (select(data, -(sheet:corner)) %>% as.matrix())[, contents$well96],
    mode = "canvas",
    title = sprintf( "corner %s: plate %s, primer set %s",
      cnr, corners[cnr, "plate"], corners[cnr, "PrimerSet"] ),
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
               x = col96, y = row96Letter,
               colourValue = content),
  domainY = LETTERS[8:1],
  title = str_c("Plate ", corners$plate[1]),    
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
  place = "plates", chartId = "content", with = contents)

lc_scatter(dat(opacity = getOpacity(highlighted), 
               x = col96, y = row96Letter, colourValue = assigned),
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
           place = "plates", chartId = "assigned", with = contents)

lc_html(dat(content = getContent(highlighted)), place = "highlighted")

ses$sendCommand(str_c("charts.A1.legend.container(d3.select('#info').select('#legend_sample')).legend.sampleHeight(30);",
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