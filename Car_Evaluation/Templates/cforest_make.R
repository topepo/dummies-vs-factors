
clear_previous <- TRUE

###################################################################

tempate_dummy <- 
  read.delim("cforest_dummy.R",
             sep = "\n", stringsAsFactors = FALSE, 
             colClasses = "character", quote = "",
             blank.lines.skip = FALSE, header = FALSE)[, 1]

tempate_factor <- 
  read.delim("cforest_factor.R",
             sep = "\n", stringsAsFactors = FALSE, 
             colClasses = "character", quote = "",
             blank.lines.skip = FALSE, header = FALSE)[, 1]

tempate_ordered <- 
  read.delim("cforest_ordered.R",
             sep = "\n", stringsAsFactors = FALSE, 
             colClasses = "character", quote = "",
             blank.lines.skip = FALSE, header = FALSE)[, 1]

tempate_ordereddummy <- 
  read.delim("cforest_ordereddummy.R",
             sep = "\n", stringsAsFactors = FALSE, 
             colClasses = "character", quote = "",
             blank.lines.skip = FALSE, header = FALSE)[, 1]

###################################################################

if(clear_previous) {
  unlink(list.files(file.path("..", "Code"), 
                    pattern = "^cforest",
                    full.names = TRUE), 
         force = TRUE)
  unlink(list.files(file.path("..", "Results"),
                    pattern = "^cforest",
                    full.names = TRUE), 
         force = TRUE)
}

###################################################################

num_sim <- 20

set.seed(747)
seeds <- sample.int(10000, num_sim)

for(i in seeds) {
  tmp_dummy <- tempate_dummy
  tmp_dummy <- gsub("SEED", i, tmp_dummy)
  cat(tmp_dummy, sep = "\n", 
      file =  file.path("..", "Code", 
                        paste0("cforest_dummy_", i, ".R")))
  
  tmp_factor <- tempate_factor
  tmp_factor <- gsub("SEED", i, tmp_factor)
  cat(tmp_factor, sep = "\n", 
      file =  file.path("..", "Code", 
                        paste0("cforest_factor_", i, ".R")))  
  
  tmp_ordered <- tempate_ordered
  tmp_ordered <- gsub("SEED", i, tmp_ordered)
  cat(tmp_ordered, sep = "\n", 
      file =  file.path("..", "Code", 
                        paste0("cforest_ordered_", i, ".R")))
  
  tmp_ordereddummy <- tempate_ordereddummy
  tmp_ordereddummy <- gsub("SEED", i, tmp_ordereddummy)
  cat(tmp_ordereddummy, sep = "\n", 
      file =  file.path("..", "Code", 
                        paste0("cforest_ordereddummy_", i, ".R")))  
}

###################################################################

r_names <-  c(paste0("cforest_dummy_", seeds, ".R"),
              paste0("cforest_factor_", seeds, ".R"),
              paste0("cforest_ordered_", seeds, ".R"),
              paste0("cforest_ordereddummy_", seeds, ".R"))
r_names <- sample(r_names)

rdata_names <- paste0(r_names, "Data")

###################################################################

over <- length(rdata_names) %% 3
out_names <- if(over > 0) c(rdata_names, rep("", 3 - over)) else rdata_names
deps <- matrix(out_names, nrow = 3)
deps <- apply(deps, 2, function(x) paste("\t", paste(x, collapse = " "), "\\\n"))

make_depend <- paste(deps, collapse = "")
make_depend <- substring(make_depend, 3)
make_depend <- substring(make_depend, 1, nchar(make_depend) - 2)

make_operations <- 
  paste0(rdata_names, 
        ": ", r_names, " ",
        "\n\t @date '+ %Y-%m-%d %H:%M:%S: starting  ", r_names, "'",
        "\n\t @$(RCMD) BATCH --vanilla ", r_names,
        "\n\t @date '+ %Y-%m-%d %H:%M:%S: finishing ", r_names, "'\n\n")


cat(paste("SHELL = /bin/bash\n",
          "R    ?= R \n",
          "RCMD =@$(R) CMD\n",
          "all: ",
          make_depend,
          "\n\n",
          paste0(make_operations, collapse = ""),
          sep = ""),
    file = file.path("..", "Code", "cforest_make"))

