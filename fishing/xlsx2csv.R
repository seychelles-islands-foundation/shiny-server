args <- commandArgs(trailingOnly = TRUE)

filename_with_path <- args[1]
outputdir <- args[2]
system(
  paste("xlsx2csv",
        filename[1],
        file.path(outputdir,
                  sub("\\..+", ".csv", basename(filename_with_path)))
  )
)