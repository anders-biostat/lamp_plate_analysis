library( later )
library( tidyverse )
library( here )

tecan_workbook <- here( "ZST00015.xlsx" )

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
  column_to_rownames("corner") %>%
  as.data.frame() -> corners
  
readxl::read_excel( tecan_workbook, "Barcodes_SampleTypes" )  %>%
  rename( "well96" = "Tube Position", 
          "plate" = "Rack ID", 
          "content" = "Type",
          "tubeId" = "Tube ID") %>%
  mutate_at( "well96", str_replace, "0(\\d+)", "\\1" ) -> contents

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
    left_join(contents)} %>%
  select(-od434, -od560, -row, -col, -row96, -col96, -well) %>%
  pivot_wider(names_from = well96, values_from = dOd) -> tblWide

library( rlc )

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
  if(highlighted == -1) return("No highlighted lines")
  str_c(str_replace_na(c("Highlighted: ", contents$tubeId[highlighted], "<br>",
        "96 well position: ", contents$well96[highlighted], "<br>",
        "384 well position: ", contents[highlighted, c("A1", "A2", "B1", "B2")] %>% 
          unlist %>% 
          str_c(collapse = ", "))), collapse = "")
}
clearHighlighted <- function() {
  if(highlighted == -1){
    updateCharts(chartId = c("A1", "A2", "B1", "B2"), updateOnly = "ElementStyle")
    updateCharts("highlighted")
    for(pl in unique(corners$plate))
      mark(NULL, chartId = pl)  
  }
}
last <- function() {}
loop <- create_loop()

app <- openPage( FALSE, startPage = "plateBrowser.html" )

for( cnr in c( "A1", "A2", "B1", "B2" ) ) {
  data <- filter(tblWide, corner == cnr)
  highlighted <- -1
  
  lc_line(
    dat(opacity = getOpacity(highlighted),
        lineWidth = ifelse(1:nrow(contents) == highlighted, 3, 1)),
    x = as.numeric(data$minutes),
    y = (select(data, -(sheet:corner)) %>% as.matrix())[, contents$well96],
    mode = "canvas",
    colourValue = contents$content,
    title = sprintf( "corner %s: plate %s, primer set %s",
      cnr, corners[cnr, "plate"], corners[cnr, "PrimerSet"] ),
    transitionDuration = 0,
    showLegend = FALSE,
    palette = c("#17a417", "#247ed7", "#737679", "#b45422"),
    colourDomain = c("positive control", "sample", "water", "empty"),
    height = 300,
    on_mouseover = (function(cnr) {
      return(function(d) {
        highlighted <<- d
        last()
        updateCharts("highlighted")
        for(c in c( "A1", "A2", "B1", "B2" ))
          if(corners[cnr, "plate"] == corners[c, "plate"]){
            updateCharts(c, updateOnly = "ElementStyle")
          }
        mark(c(contents$row96[d], contents$col96[d]), chartId = corners[cnr, "plate"],
             clear = TRUE)
      })
    })(cnr),
    on_mouseout = function(d) {
      highlighted <<- -1
      last <<- later(clearHighlighted, 0.75, loop)
    },
    place = cnr)
}

for(pl in unique(corners$plate)){
  contents %>% 
    filter(plate == pl) %>%
    select(row96Letter, col96, content) %>%
    pivot_wider(names_from = col96, values_from = content) %>%
    column_to_rownames("row96Letter") %>%
    as.matrix() -> data
  lc_heatmap(value = data, 
             showLegend = TRUE, 
             height = 300,
             palette = c("#17a417", "#247ed7", "#737679", "#b45422"),
             colourDomain = c("positive control", "sample", "water", "empty"),
             place = "plates", chartId = pl)
}
  

lc_html(dat(content = getContent(highlighted)), place = "highlighted")
ses <- app$getSession()
ses$sendCommand(str_c("charts.A1.legend.container(d3.select('#info').select('#legend'));",
                      "charts.A1.showLegend(true).update();"))

