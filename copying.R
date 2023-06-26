copy.files <- function(source.dir = "", dest.dir = "", pat = ".raw", delete = F, overwrite = F) {
    list.of.files <- list.files(source.dir, pattern = pat, full.names = T)
    if (dir.exists(source.dir) & dir.exists(dest.dir)) {
            file.copy(list.of.files, dest.dir, copy.date = T, copy.mode = T, overwrite = overwrite) }
    else {message(paste0("One or more directory does not exist\n   ",source.dir,"\n   ",dest.dir))}
    message("Done copying files!")
    if (delete) {for (file in list.files(dest.dir, pattern = pat, full.names = T)) {
            if (file.exists(file)) { file.remove(paste0(source.dir,"/",basename(file)))}}
        message("Files deleted from source directory!")    }}
# copy.files("V:/Cook-Projects/E-prime/ddmface/ddmface_RawData",
# "X:/Data/ddmface/ddmface_RawData/test", ".raw", delete = F)

