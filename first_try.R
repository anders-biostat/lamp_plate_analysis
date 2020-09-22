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
  rename( "well96" = "Tube Position", "plate" = "Rack ID", "content" = "Type" ) %>%
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
  {contents <<- select(., well, well96, corner) %>%
    distinct() %>%
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
app <- openPage( FALSE, startPage = "plateBrowser.html" )

for( cnr in c( "A1", "A2", "B1", "B2" ) ) {
  data <- filter(tblWide, corner == cnr)
  highlighted <- -1
  
  lc_line(
    dat(opacity = getOpacity(highlighted)),
    x = as.numeric(data$minutes),
    y = (select(data, -(sheet:corner)) %>% as.matrix())[, contents$well96],
    colourValue = contents$content,
    title = sprintf( "corner %s: plate %s, primer set %s",
      cnr, corners[cnr, "plate"], corners[cnr, "PrimerSet"] ),
    transitionDuration = 0,
    showLegend = FALSE,
    on_mouseover = function(d) {
      highlighted <<- d
      updateCharts(updateOnly = "ElementStyle")
      for(c in c( "A1", "A2", "B1", "B2" ))
        mark(d, c)
    },
    on_mouseout = function(d) {
      highlighted <<- -1
      updateCharts(updateOnly = "ElementStyle")
      for(c in c( "A1", "A2", "B1", "B2" ))
        mark(NULL, chartId = c)
      
    },
    place = cnr )
}
ses <- app$getSession()
ses$sendCommand(str_c("charts.A1.legend.container(d3.select('#info').select('#legend'));",
                      "charts.A1.showLegend(true).update();"))
