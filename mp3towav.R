mp3towav <- function(dir = NULL) {
    if (is.null(dir))  dir <- dirname(file.choose()) # select file in directory
    for (song in list.files(dir)) {
        song <- paste0(dir,"/",song)
        song.dir = dirname(song)
        song.ext <- tolower(tools::file_ext(song))
        if (song.ext == "mp3") {
            song.file = tuneR::readMP3(song)
            new.name = paste0(as.character(gsub(".mp3.*", "",song)),".wav", sep = "")
            tuneR::writeWave(song.file,new.name, extensible=FALSE)
       #        tuneR::savewav(song.file, f = 22050, file=new.name,rescale=c(-1500,1500))
            if (file.exists(new.name)) {
                message(paste0("File Written:\n  ",new.name))
            } else {
                message(paste0("Error writing:\n  ",new.name))
            }
        } else {
        message("Skipped: Not an mp3 file")
        }
    }
}
