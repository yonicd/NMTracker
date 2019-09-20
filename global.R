ext_status_read <- function(path,est_ind = 1){
  

  x <- tidynm::read_extensions(path,exts = 'ext',est_ind = identity)%>%
    purrr::pluck('ext')%>%
    purrr::set_names(1:length(.))%>%
    purrr::map_df(function(x){
      x%>%tidyr::gather(STAT,VALUE,-1)  
    },.id='ESTIMATION')%>%
    dplyr::filter(ESTIMATION==est_ind)%>%
    dplyr::filter(ITERATION>=-1e9)%>%
    dplyr::mutate(ITERATION_TYPE = dplyr::case_when(
      ITERATION == -1e9 ~ -2,
      ITERATION < 0  ~ -1,
      TRUE ~ 1)
    )
  
  lbls <- 'Sample'
  
  if(min(x$ITERATION)<0){
    x <- dplyr::bind_rows(x,x%>%dplyr::filter(ITERATION==0)%>%dplyr::mutate(ITERATION_TYPE = -1))%>%
      dplyr::mutate(
        ITERATION = as.numeric(ITERATION),
        ITERATION=dplyr::if_else(ITERATION==-1e09,max(ITERATION) + 1,ITERATION)
      )%>%
      dplyr::arrange(ESTIMATION,STAT,ITERATION)
    
    lbls <- c('Final','Burn','Sample')[c(-2,-1,1)%in%unique(x$ITERATION_TYPE)]
    
  }
  
  ctl <- try(tidynm::ctl_parse(sprintf('%s.ctl',path)))
  
  if('THETA'%in%names(ctl)){
    THETA <- ctl$THETA%>%
      dplyr::select(Var1,LABEL)%>%
      dplyr::mutate(
        STAT = sprintf('THETA%s',Var1),
        LABEL = gsub('^\\s+|\\s+$','',LABEL),
        LABEL = sprintf('%s (%s)',sprintf('THETA%02d',Var1),LABEL)
      )%>%
      dplyr::select(-Var1)  
  }else{
    THETA <- tibble::tibble(STAT = unique(grep('^THETA',x$STAT,value = TRUE)), LABEL = NA)
  }
  
  x1 <- x%>%
    dplyr::left_join(THETA,by='STAT')%>%
    dplyr::mutate(
      ITERATION_TYPE = factor(ITERATION_TYPE,label = lbls),
      STAT_TYPE = factor(gsub('[.0-9]','',STAT)),
      LABEL = ifelse(is.na(LABEL),STAT,LABEL)
    )
  
  x2 <- split(x1,x1$STAT_TYPE)
  
  if('SAEMOBJ'%in%names(x2)){
    x2 <- x2[c('SAEMOBJ','THETA','OMEGA','SIGMA')]  
  }else{
    x2 <- x2[c('OBJ','THETA','OMEGA','SIGMA')]  
  }
  
  x2
}

ext_status_plot <- function(dat,stat){
  
  dat <- dat[names(dat)%in%stat]
  
  g <- purrr::map(dat,function(x){
    ret <- x%>%
      ggplot2::ggplot(ggplot2::aes(x=ITERATION,y=VALUE)) + 
      ggplot2::geom_path(ggplot2::aes(colour=ITERATION_TYPE,group = STAT)) + 
      ggplot2::geom_point(
        ggplot2::aes(colour=ITERATION_TYPE,group = STAT),
        data=x%>%dplyr::filter(ITERATION_TYPE=='Final')
      ) + 
      ggplot2::facet_wrap( ~ LABEL,scales='free') + 
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = 'bottom')
    
    if(!all(grepl('OBJ',x$LABEL))){
      
      ret <- ret + 
        ggplot2::geom_hline(ggplot2::aes(yintercept=0),linetype=2)
      
    }else{
      
      ret <- ret + 
        ggplot2::scale_y_log10()
      
    }
    
    ret
    
  })
  
  g[[1]] <- g[[1]] + ggplot2::labs(title = sprintf('Iteration: %s',max(dat[[1]]$ITERATION)))
  
  if(length(g)==1){
    return(g)
  }
  
  g%>%purrr::reduce(`+`) + patchwork::plot_layout(ncol=2)
}

library(magrittr)
library(patchwork)

path <- '/data/co/svn-proj-mer0501/model/SLD-OS'
run <- '013'