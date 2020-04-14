create_choropleth <- function ( dat,
                                level = 0,
                                title = NULL, 
                                unit_text = NULL,
                                map_year = 2013,
                                map_time = as.Date("2013-01-01"),
                                cuts = 5, 
                                base_color = '#00348A', 
                                iceland = "if_present") {
  
  color_palette <- grDevices::colorRampPalette(c(
    'grey90', base_color ),
    bias=.5,space="rgb")(((cuts+1)*2)-1)[c(TRUE, FALSE)]
  
  load ( 'raw_data/geodata_sf.rda') 
  
  if ( level == 0 ) { 
    geodata <- geodata_nuts0
    nuts_ids <- geodata$id
    dat$geo <- ifelse ( dat$geo == "GB", "UK", 
                        ifelse ( dat$geo == "GR", "EL", dat$geo))
  } else if ( level == 1 )  {
    geodata <- geodata_nuts1 
    nuts_ids <- geodata$id
  } else if ( level == 2 )  {
    geodata <- geodata_nuts2
    nuts_ids <- geodata$id
  } else if ( level == 3 ) {
    geodata <- geodata_nuts3
    nuts_ids <- geodata$id
  }  else {
    stop ( "Level must be [NUTS] 0, 1, 2 or 3.")
  }
  rm ( geodata_nuts0, geodata_nuts1, geodata_nuts2, geodata_nuts3) 
  

  
  dat <- dat %>% 
    dplyr::mutate_if ( is.factor, as.character ) %>%
    dplyr::filter( geo %in% nuts_ids ) 
  
  geodata <- dplyr::left_join(geodata,  dat, by = 'geo') 
  geodata$cat = eurostat::cut_to_classes(geodata$values, n = cuts)
  
  title_text <- if ( is.null(title) ) { title_text <- ""} else {
    title_text <- as.character(title)
  }
  
  if ( class ( iceland ) == "character" ) {
    if (  ! any( c("true", "false") %in% tolower(iceland)) ) {
      iceland <- ifelse ( "IS" %in% dat$geo, TRUE, FALSE )
    } else if (tolower(iceland) == 'true') {
      iceland <- TRUE  
    }  else  if ( tolower (iceland) == 'false') {
      iceland <- FALSE
    }
  }
  
  if ( iceland ) {
    p <- ggplot(data=geodata) +
      geom_sf(aes(fill=cat),
              color="white", size=.05) + 
      scale_fill_manual(values = color_palette, na.value="green") +
      guides(fill = guide_legend(reverse=T, title = unit_text)) +
      labs(title=title_text,
           caption="") +
      theme_light() + theme(legend.position='right', 
                            axis.text = element_blank(), 
                            axis.ticks = element_blank()) +
      coord_sf(xlim = c(-23,34), ylim = c(34.5,71.5))
    
  } else {
    p <- ggplot(data=geodata) +
      geom_sf(aes(fill=cat),
              color="white", size=.05) + 
      scale_fill_manual(values = color_palette, na.value="green") +
      guides(fill = guide_legend(reverse=T, title = unit_text)) +
      labs(title=title_text,
           caption="") +
      theme_light() + theme(legend.position='right', 
                            axis.text = element_blank(), 
                            axis.ticks = element_blank()) +
      coord_sf(xlim = c(-11.7, 32.3), ylim = c(34.5,71.5))
  }
  
  
  if ( iceland ) {
    ggplot(data=geodata) +
      geom_sf(aes(fill=cat),
              color="white", size=.05) + 
      scale_fill_manual(values = color_palette, na.value="green") +
      guides(fill = guide_legend(reverse=T, title = unit_text)) +
      labs(title=title_text,
           caption="") +
      theme_light() + theme(legend.position='right', 
                            axis.text  = element_blank(), 
                            axis.ticks = element_blank()) +
      coord_sf(xlim = c(-23,34), ylim = c(34.5,71.5))
    
  } else {
    ggplot(data=geodata) +
      geom_sf(aes(fill=cat),
              color="white", size=.05) + 
      scale_fill_manual(values = color_palette, na.value="green") +
      guides(fill = guide_legend(reverse=T, title = unit_text)) +
      labs(title=title_text,
           caption="") +
      theme_light() + theme(legend.position='right', 
                            axis.text  = element_blank(), 
                            axis.ticks = element_blank()) +
      coord_sf(xlim = c(-11.7, 32.3), ylim = c(34.5,71.5))
  }
}