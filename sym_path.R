sym_path <- function(filename, set = 1, symbolic = c("J:","~/junk"), newpath = NULL) {
    if (.Platform$OS.type == "windows" & set == 1 ) {
        if (!file.exists(symbolic[1])) {
            try(system(paste0("C:/Windows/SysWOW64/subst.exe ",symbolic[1]," ",
                              dirname(filename)), intern = TRUE))
        }
        newpath = paste0(symbolic[1],"/",basename(filename))
        return(newpath)
    }
    if (.Platform$OS.type == "unix" & set == 1) {
        # make junk dir then make symbolic link to file
        if (!file.exists(symbolic[2]))    {
            dir.create(symbolic[2])
            try(system(paste0("ln -s ",filename," ",symbolic[2],"/",basename(filename))))
        }
        newpath = paste0(symbolic[2],"/",basename(filename))
        return(newpath) # may throw error
    }
    if (.Platform$OS.type == "windows" & set == 0) {
        # kill the subst drive letter
        try(system(paste0("C:/Windows/SysWOW64/subst.exe ", symbolic[1], " /D"), intern = TRUE))
    }
    if (.Platform$OS.type == "unix" & set == 0) {
        # kill the symbolic link to file
        try(system(paste0("rm ", symbolic[2],"/",basename(filename))))
        # or try(system(paste0("unlink ",macpath))
    } }