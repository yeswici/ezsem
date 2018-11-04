
hot.cb <- function(vec.obj, vec.con = NULL){
  if(is.null(vec.con)){vec <- vec.obj}else{vec <- c(vec.obj, vec.con)}
  n <- length(vec)
  
  df <- as.data.frame(diag(rep(NA, n)))
  colnames(df) <- vec
  
  if(is.null(vec.con)){
    rownames(df) <- paste0(vec, " ~")
    }else{
    rownames(df) <- c(paste0(vec.obj, " ~"), paste0(vec.con, " =~"))
  }

  hot.cell.ro <- function(j){
    paste0("hot_cell(",j, ", ", j,", readOnly = TRUE)")
  }
  
  hcr <- c()
  for(i in 1: n){hcr <- paste(hcr, hot.cell.ro(i), sep = " %>% ")}
  eval.text <- paste0(
    "rhandsontable(df, rowHeaderWidth = 200)",
    " %>% hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)",
    hcr)
  
  eval(parse(text = eval.text))
}

df.model <- function(df){
  n <- length(df[, 1])
  vec <- colnames(df)
  
  model <- NULL
  
  for(i in 1:n){
    tf.vec <- !is.na(df[i, ]) & df[i, ]
    
    if(length(which(tf.vec)) > 0){
      model0 <- paste(vec[tf.vec], collapse = " + ")
      model <- paste0(model, rownames(df)[i], model0, "\n")
    }
  }
  return(model)
}

df.model.input <- function(input.df){
  if(is.null(input.df)){return(NULL)}
  df.model(hot_to_r(input.df))
  }

ezsem <- function(model, data, estimator = "ML"){
  res <- try(sem(model = model, data = data, estimator = estimator), silent = FALSE)
  if(class(res) == "try-error"){return(NULL)}else{
    return(res)
  }
}

ezsem.plot <- function(model, font = "Japan1GothicBBB"){
  
  gc();gc()
  #https://cran.rstudio.com/web/packages/lavaanPlot/vignettes/Intro_to_lavaanPlot.html
  lavaanPlot(model = model, 
             node_options = list(shape = "box", fontname = font), 
             edge_options = list(color = "grey"),
             coefs = TRUE, covs = TRUE, stars = TRUE)
}

as.worth <- function(df){
  
  is.worth <- function(vec){
    if(!is.vector(vec)){stop("Value is not vector")}
    
    if(all(is.na(vec))){return(FALSE)}
    
    if(is.factor(vec) || is.character(vec)){
      if(length(unique(vec)) <= 1){return(FALSE)}else{return(TRUE)}
    }else{
      if(sd(vec) == 0){return(FALSE)}else{TRUE}
    }
  }
  
  chk <- apply(df, MARGIN = 2, is.worth)
  ret <- df[, which(chk)]
  
  return(ret)
  
  
}

data.rows <- function(data, rows){
  if(is.null(rows)){return(NULL)}
  if(length(data[, 1]) < max(rows)){return(NULL)}
  return(data[rows, ])
}

lv.sel <- function(input.lv){
  vec <- na.omit(as.vector(hot_to_r(input.lv)[, 1]))
  if(length(vec) == 0){return(NULL)}
  return(vec)
}

df.fac.num <- function(df, vec){
  if(is.null(df) || is.null(vec)){return(NULL)}
  form <- paste0("~", paste(vec, collapse = " + "))
  form <- as.formula(form)
  ret <- model.matrix(form, df)[, -1] #-1は切片除去
  return(ret)
}

imp.fit <- function(result, 
                    par = c("pvalue", "gfi", "agfi", "rmr", "rmsea", "aic", "bic"), 
                    names = c("p-value", "GFI", "AGFI", "RMR", "RMSEA", "AIC", "BIC")){
  
  if(is.null(result)){return(NULL)}

  
  fm <- fitMeasures(result)
  par.result <- round(as.vector(fm[par]), 3)
  
  ret <- list()
  ret$df <- data.frame(Name = names, Value = par.result)
  ret$text <- paste(names, par.result, sep = " = ", collapse = ", ")
  
  return(ret)

}







