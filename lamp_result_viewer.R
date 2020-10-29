library( later )
library( tidyverse )

tecan_workbook <- commandArgs(TRUE)[1]
if(!file.exists(tecan_workbook))
  stop(str_c("File not found: ", tecan_workbook))

read_tecan_sheet <- function( filename, sheet ) {
  inner_join( by="well",
    readxl::read_excel( filename, sheet, skip=22, n_max=384 ) %>%
      assertr::verify( identical( colnames(.), c( "<>", "Value" ) ) ) %>%
      rename( "well" = "<>", "od434" = "Value" ),
    readxl::read_excel( filename, sheet, skip=423, n_max=384 ) %>%
      assertr::verify( identical( colnames(.), c( "<>", "Value" ) ) ) %>%
      rename( "well" = "<>", "od560" = "Value" ) )
}

readxl::read_excel( tecan_workbook, "PrimerSetsUsed" ) %>%
  rename( "plate"="Plate-ID", "corner" = "A1 position of 96-well in 384-well" ) %>%
  mutate_at( "plate", str_replace_all, " ", "_" ) %>%
  mutate_at( "plate", str_replace_all, "-", "." ) %>%
  column_to_rownames("corner") %>%
  as.data.frame() -> corners
  
readxl::read_excel( tecan_workbook, "Barcodes_SampleTypes" )  %>%
  rename( "well96" = "Tube Position", 
          "plate" = "Rack ID", 
          "content" = "Type",
          "tubeId" = "Tube ID") %>%
  mutate_at( "plate", str_replace_all, " ", "_" ) %>%
  mutate_at( "plate", str_replace_all, "-", "." ) %>%
  mutate_at( "well96", str_replace, "0(\\d+)", "\\1" ) -> contents

if(length(setdiff(corners$plate, contents$plate)) > 0)
  stop(str_interp("Content information is missing for the following plates: ${setdiff(corners$plate, contents$plate)}"))

readxl::excel_sheets( tecan_workbook ) %>%
  { tibble( sheet = . ) } %>%
  mutate( minutes = str_match( sheet, "(\\d+)\\w*((min)?)" ) %>% `[`(,2) ) %>%
  filter( !is.na( minutes ) ) %>%
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
  {contents <<- select(., well, well96, corner, row96, col96) %>%
    distinct() %>%
    mutate(row96Letter = LETTERS[row96]) %>%
    pivot_wider(names_from = corner, values_from = well) %>%
    left_join(contents) %>%
    pivot_wider(names_from = plate, values_from = c("tubeId", "content"))} %>%
  select(-od434, -od560, -row, -col, -row96, -col96, -well) %>%
  pivot_wider(names_from = well96, values_from = dOd) -> tblWide

tblWide %>%
  filter(minutes  == 20) %>%
  pivot_longer(names_to = "well96", values_to = "diff", -(sheet:corner)) %>%
  left_join(corners %>% rownames_to_column("corner")) %>%
  mutate(result = ifelse(diff > 0, "positive", "negative")) %>% 
  group_by(well96, plate) %>%
  summarise(result = case_when(result[PrimerSet == "ACTB"] == "negative" ~ "control failed",
                               all(result[PrimerSet != "ACTB"] == "positive") ~ "all positive",
                               all(result[PrimerSet != "ACTB"] == "negative") ~ "all negative",
                               TRUE ~ "mixed")) %>%
  mutate(plate = str_c("result_", plate)) %>%
  pivot_wider(names_from = plate, values_from = result) %>%
  right_join(contents) -> contents

library( rlc )

colourBy <- "sample"

palette <- list(sample = data.frame(colour = c("#1cb01c", "#c67c3b", "#4979e3", "#aeafb0"),
                                    type = c("positive control", "sample", "water", "empty"),
                                    stringsAsFactors = FALSE),
                result = data.frame(colour = c("#48b225", "#f58e09", "#d22d2d", "#270404"),
                                    type = c("all negative", "mixed", "all positive", "control failed"),
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
getContent <- function(highlighted, plate) {
  if(highlighted == -1) {
    ses$sendCommand("d3.select('#highlighted').classed('failed', false);")
    return("No highlighted lines")
  }
  if(contents[highlighted, str_c("result_", plate)] == "control failed") {
    ses$sendCommand("d3.select('#highlighted').classed('failed', true);")
  } else {
    ses$sendCommand("d3.select('#highlighted').classed('failed', false);")
  }
  str_c(str_replace_na(c("Highlighted: ", contents[[str_c("tubeId_", plate)]][highlighted], "<br>",
                         "96 well position: ", contents$well96[highlighted], "<br>",
                         "384 well position: ", contents[highlighted, c("A1", "A2", "B1", "B2")] %>% 
                           unlist %>% 
                           str_c(collapse = ", "))), 
        collapse = "")
  
}
clearHighlighted <- function() {
  if(highlighted == -1){
    updateCharts(chartId = c("A1", "A2", "B1", "B2", 
                             unique(corners$plate), str_c("res_", unique(corners$plate))), 
                 updateOnly = "ElementStyle")
    updateCharts("highlighted")
    ses$sendCommand("d3.select('#highlighted').classed('failed', false);")
  }
}
last <- function() {}
loop <- create_loop()

app <- openPage( FALSE, startPage = "plateBrowser.html" )
ses <- app$getSession()

for( cnr in c( "A1", "A2", "B1", "B2" ) ) {
  data <- filter(tblWide, corner == cnr)
  highlighted <- -1
  plate <- corners$plate[1]
  
  lc_line(
    dat(opacity = getOpacity(highlighted),
        lineWidth = ifelse(1:nrow(contents) == highlighted, 3, 1),
        palette = palette[[colourBy]][["colour"]],
        colourDomain = palette[[colourBy]][["type"]]),
    x = as.numeric(data$minutes),
    y = (select(data, -(sheet:corner)) %>% as.matrix())[, contents$well96],
    colourValue = contents[[str_c("content_", corners[cnr, "plate"])]],
    mode = "canvas",
    title = sprintf( "corner %s: plate %s, primer set %s",
      cnr, corners[cnr, "plate"], corners[cnr, "PrimerSet"] ),
    transitionDuration = 0,
    showLegend = FALSE,
    height = 300,
    titleSize = 18,
    on_mouseover = function(d) {
        highlighted <<- d
        plate <<- corners[.chartId, "plate"]
        last()
        updateCharts("highlighted")
        for(c in c( "A1", "A2", "B1", "B2" ))
          if(corners[.chartId, "plate"] == corners[c, "plate"]){
            updateCharts(c, updateOnly = "ElementStyle")
          }
        updateCharts(c(corners[.chartId, "plate"], str_c("res_", corners[.chartId, "plate"])), updateOnly = "ElementStyle")
      },
    on_mouseout = function(d) {
      highlighted <<- -1
      last <<- later(clearHighlighted, 0.4, loop)
    },
    place = cnr)
}

for(pl in unique(corners$plate)){
  lc_scatter(dat(opacity = getOpacity(highlighted), 
                 x = col96, y = row96Letter),
    domainY = LETTERS[8:1],
    colourValue = contents[[str_c("content_", pl)]],
    title = str_interp("Plate ${pl}"),
    titleSize = 20,
    palette = palette$sample$colour,
    colourDomain = palette$sample$type,
    height = 350,
    width = 550,
    strokeWidth = 2,
    stroke = "black",
    on_mouseover = function(d) {
        highlighted <<- d
        plate <<- .chartId
        updateCharts("highlighted")
        for(c in c( "A1", "A2", "B1", "B2" ))
          if(.chartId == corners[c, "plate"]){
            updateCharts(c, updateOnly = "ElementStyle")
          }
        updateCharts(c(.chartId, str_c("res_", .chartId)), updateOnly = "ElementStyle")
      },
    on_mouseout = function() {
      highlighted <<- -1
      clearHighlighted()
    },
    on_click = function(d) {
      if(colourBy != "sample") {
        colourBy <<- "sample"
        for(cnr in c("A1", "A2", "B1", "B2")) {
          setProperties(dat(colourValue = contents[[str_c("content_", corners[cnr, "plate"])]]), cnr)
          updateCharts(cnr, updateOnly = "ElementStyle")
        }
      }
    },
    showLegend = FALSE,
    transitionDuration = 0,
    size = 10,
    place = "plates", chartId = pl, with = contents)
  
  lc_scatter(dat(opacity = getOpacity(highlighted), 
                 x = col96, y = row96Letter),
             domainY = LETTERS[8:1],
             colourValue = contents[[str_c("result_", pl)]],
             title = str_interp("Plate ${pl}"),
             titleSize = 20,
             palette = palette$result$colour,
             colourDomain = palette$result$type,
             height = 350,
             width = 550,
             strokeWidth = 2,
             stroke = "black",
             on_mouseover = function(d) {
               highlighted <<- d
               .chartId <- str_remove(.chartId, "res_")
               plate <<- .chartId
               updateCharts("highlighted")
               for(c in c( "A1", "A2", "B1", "B2" ))
                 if(.chartId == corners[c, "plate"]){
                   updateCharts(c, updateOnly = "ElementStyle")
                 }
               updateCharts(c(.chartId, str_c("res_", .chartId)), updateOnly = "ElementStyle")
             },
             on_mouseout = function() {
               highlighted <<- -1
               clearHighlighted()
             },
             on_click = function(d) {
               if(colourBy != "result") {
                 colourBy <<- "result"
                 for(cnr in c("A1", "A2", "B1", "B2")){
                   setProperties(dat(colourValue = contents[[str_c("result_", corners[cnr, "plate"])]]), cnr)
                   updateCharts(cnr, updateOnly = "ElementStyle")
                 }
               }
             },
             showLegend = FALSE,
             transitionDuration = 0,
             size = 10,
             place = "plates", chartId = str_c("res_", pl), with = contents)
  
}
  

lc_html(dat(content = getContent(highlighted, plate)), place = "highlighted")

ses$sendCommand(str_c("charts.A1.legend.container(d3.select('#info').select('#legend_sample')).legend.sampleHeight(30);",
                      "charts.A1.showLegend(true).update();"))
ses$sendCommand(str_c("charts.res_", plate, ".legend.container(d3.select('#info').select('#legend_res')).legend.sampleHeight(30);",
                      "charts.res_", plate, ".showLegend(true).update();"))
ses$sendCommand('d3.selectAll("#legend_res").selectAll("text").attr("font-size", 17).attr("dy", 7)')
ses$sendCommand('d3.selectAll("#legend_sample").selectAll("text").attr("font-size", 17).attr("dy", 7)')
fileName <- basename(tecan_workbook)
ses$sendCommand(str_interp('d3.select("h3").html("File: <i>${fileName}</i>")'))

while(length(app$getSessionIds()) > 0)
  httpuv::service()

Sys.sleep(3)