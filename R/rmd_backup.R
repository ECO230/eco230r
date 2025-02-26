#' Backup all .Rmd files matching a pattern, if there is an exact copy already existing, with same hash, replace the most recent backup with this one timestamp is added as a suffix to the file name.
#'
#' @param search File name to search for default is Group*, for a specific file only pass in file name with no file type e.g. Group_Analysis_Plan
#'
#' @export
#'
rmd_backup <- function(search = 'Group*') {
  search_pth <- paste(search,'.Rmd',sep = '')

  #Create dir if it doesn't exist
  dir.create(file.path('Prompt', 'rmd_backups'),showWarnings = FALSE)

  ff <- data.frame(id = numeric()
                   ,path = character()
                   ,stem = character()
                   ,stem_date = character()
                   ,stem_nd = character()
                   ,hash = character())
  i <- 1

  #get list of current files in directory with hashes
  for (x in list.files('Prompt/rmd_backups', full.names = TRUE, pattern = '*.Rmd')) {
    print(x)
    stem <- sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(x))
    st_sp <- str_split(stem, "_")
    dt_cd <- st_sp[[1]][length(st_sp[[1]])]
    stem_nd <- paste(st_sp[[1]][0:(length(st_sp[[1]])-1)],collapse='_')
    hash <- rlang::hash_file(x)

    vec <- c(i,x,stem,dt_cd,stem_nd,hash)
    ff[i,] <- vec
    i = i+1
  }


  #find all .Rmd matching search pattern
  for (x in Sys.glob(search_pth)) {
    n_stem = print(sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(x)))
    bk <- paste('Prompt/rmd_backups/',n_stem,'_',format(Sys.time(), "%Y%m%d%H%M%S"),'.Rmd' , sep = '')
    n_hash = rlang::hash_file(x)

    cnt <- ff %>%
      filter(hash == n_hash) %>%
      summarise(n())

    if (cnt[[1]] > 0) {
      #There is a file with the same hash
      pth_del <- ff %>%
        filter(stem_nd == n_stem & hash == n_hash) %>%
        arrange(desc(stem_date)) %>%
        slice(1:1) %>%
        select(path)
    }



    #write new file current version of these files
    file.copy(from=x, to=bk,
              overwrite = TRUE, recursive = FALSE,
              copy.mode = TRUE)

    if(length(pth_del[[1]]) >0) {
      #if an existing file with the same hash replace the most recent (delete)
      #There is a file to delete
      if (file.exists(pth_del[[1]])){
        file.remove(pth_del[[1]])
      }

    }

  }
}
