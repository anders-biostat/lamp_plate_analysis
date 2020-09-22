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
  rename( "plate"="Plate-ID", "corner" = "A1 position of 96-well in 384-well" ) -> corners
  
readxl::read_excel( tecan_workbook, "Barcodes_SampleTypes" )  %>%
  rename( "well96" = "Tube Position", "plate" = "Rack ID", "content" = "Type" ) %>%
  mutate_at( "well96", str_replace, "0(\\d+)", "\\1" ) -> contents

contents %>%
mutate( row96 = fct_rev( str_sub( well96, 1, 1 ) ) ) %>%
mutate( col96 = as.integer( str_sub( well96, 2, -1 ) ) ) %>%
ggplot +
  geom_point( aes( x=col96, y=row96, col=content ), size=4 ) +
  scale_x_continuous( breaks=1:12 ) +
  facet_grid( ~ plate ) +
  coord_fixed()

readxl::excel_sheets( tecan_workbook ) %>%
{ tibble( sheet = . ) } %>%
mutate( minutes = str_match( sheet, "(\\d+)\\w*((min)?)" ) %>% `[`(,2) ) %>%
filter( !is.na( minutes ) ) %>%
group_by_all() %>%
summarise( a = list( read_tecan_sheet( tecan_workbook, sheet ) ) ) %>%
unnest( a ) %>%
mutate( row = ( str_sub( well, 1, 1 ) %>% map_int( utf8ToInt ) )- utf8ToInt("A") + 1  ) %>%
mutate( col = as.integer( str_sub( well, 2, -1 ) ) ) %>%
mutate( row96 = ceiling( row / 2 ) ) %>%
mutate( col96 = ceiling( col / 2 ) ) %>%
mutate( well96 = str_c( LETTERS[row96], col96 ) ) %>%
mutate( corner = str_c( LETTERS[ 1 + (row+1) %% 2 ], 1 + (col+1) %% 2 ) ) %>%
left_join( corners, by="corner" ) %>%
left_join( contents, by = c( "well96", "plate" ) ) %>%  
group_by( well ) %>%
mutate( maxOD = max( od434 + od560 ) ) %>%
ungroup() -> tbl

tbl %>%
ggplot(aes(x=od434, y=od560))+geom_point(aes(col=minutes,shape=content)) + facet_wrap(~corner)


tbl %>%
ggplot(aes(x=minutes, y=od434-od560))+geom_line(aes(group=well,col=content,lty=maxOD<2.2))+facet_wrap(~corner)

tbl %>%
mutate( row96 = fct_rev( factor( row96 ) ) ) %>%
ggplot +
  geom_point(aes(x=col96,y=row96,col=content),size=4) +
  coord_equal() +
  scale_x_continuous(breaks=1:12)

library( rlc )

tbl %>%
  mutate( dOD = od434 - od560 ) %>%
  select( -starts_with("od"), -sheet ) %>%
  pivot_wider( names_from = minutes, values_from = dOD, names_prefix = "min" ) ->
    tblWide

tblWide %>% 
select( starts_with("min") ) %>% 
as.matrix %>% t -> yMatrix

yMatrix %>% rownames() %>% str_remove("min") %>% 
as.numeric() %>% matrix( nrow=nrow(yMatrix), ncol=ncol(yMatrix) ) -> xMatrix

openPage( FALSE, layout = "table3x3" )

for( corner in c( "A1", "A2", "B1", "B2" ) ) {
  lc_line( 
    dat(),
    x = xMatrix[ , tblWide$corner == corner ],
    y = yMatrix[ , tblWide$corner == corner ],
    colourValue = tblWide$content[ tblWide$corner == corner ],
    label = sprintf( "Well: %s [96] / %s [384]", 
      tblWide$well96, tblWide$well ),
    title = sprintf( "corner %s: plate %s, primer set %s",
      corner,
      str_c( tblWide[tblWide$corner==corner,]$plate.x %>% unique, collapse = "/" ),   #should be "plate"
      str_c( tblWide[tblWide$corner==corner,]$PrimerSet %>% unique, collapse = "/" ) ),
    place = corner )
}
