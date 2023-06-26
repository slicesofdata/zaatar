mailit <- function(to = "gcook@CMC.edu", subject = "Subject",
                   body = ":)", from = "gcook@CMC.edu", pw = NULL, attach = NULL,
                   host = c("smtp.office365.com",587), ...) {
    for (pkg in c("mailR","rJava")) {if (pkg %ni% installed.packages()) { install.packages(pkg, dep = T) }}

    `%ni%` = Negate(`%in%`); to = tolower(trimws(to, "both")); from = tolower(trimws(from, "both"))
    if (as.logical(grep('cmc.edu|cgu.edu', from))) {host[1] = "smtp.office365.com"; host[2] = 587}
    if (as.logical(grepl('gmail.com',  from))) {host[1] = "smtp.gmail.com"; host[2] = 465}
    if (!is.null(pw)) {
        if (is.null(attach)) {
            mailR::send.mail(to = to, from = from,
                             subject = subject, body = body,
                             smtp = list(host.name = host[1], # "smtp.office365.com"  or "smtp-mail.outlook.com"
                                         port = host[2],
                                         user.name = from,
                                         passwd = pw, tls = TRUE),
                             authenticate = TRUE, send = TRUE)
            message(paste0("Message sent to ", to, "\n"))
        } else {
            attachments = get.pathlist(attach)
            mailR::send.mail(to = to, from = from,
                             subject = subject, body = body,
                             smtp = list(host.name = host[1], # "smtp.office365.com", # or smtp-mail.outlook.com
                                         port = host[2], user.name = from,
                                         passwd = pw, tls = TRUE),
                             attach.files = attachments,
                             authenticate = TRUE, send = TRUE)
            message(paste0("Message sent to ", to, "\n"))
        }
    } else { message("No password passed into function.")   }
}


textit <- function(to = "9092607523@vtext.com", subject = "Message from R",
    body = "", from = "gcook@CMC.edu", password = "",
    host = "email.claremontmckenna.edu", portnum = 26) {
    if (password != "") {
    mailR::send.mail(to = to, subject = subject, body = body, from = from,
        authenticate = TRUE,
        smtp = list(host.name = host, # "smtp.office365.com", # or smtp-mail.outlook.com
            port = portnum, user.name = from, passwd = password, tls = TRUE))
    message(paste0("Message text to ", to))
    } else {
        message("No password set.")
    }}
#textit(to = "9092607523@vtext.com", subject = "Subject", body = "Here is a model.", from = "gcook@CMC.edu", password = "")


outlook.quick <- function(to = "gcook@CMC.edu", subject = "", body = "",
    attachment = "", from = "gcook@CMC.edu", table = "n") {
    if ("RDCOMClient" %ni% installed.packages()) {
        #install.packages("RDCOMClient", repos = "http://www.omegahat.net/R")
        #library(devtools); 
        library(RDCOMClient)
        devtools::install_github("omegahat/RDCOMClient") }
    OutApp <- RDCOMClient::COMCreate("Outlook.Application");
    for (email in unique(to)) {
        outMail = OutApp$CreateItem(0)
        outMail[["To"]] = email; outMail[["subject"]] = subject; outMail[["body"]] = body
        if (file.exists(attachment)) {  outMail[["Attachments"]]$Add(attachment)  }
        try(outMail$Send())
        message(paste0("Sending message to: ",to," - ", subject,"\n"))
        outMail = ""    }
#    if (table = "y") {
#        pander::panderOptions('table.split.table', Inf)
#        outMail[["body"]] = paste("Hello!", "", "The below summarises xxx:",
#            pander::pandoc.table.return(data.frame(V1 = 1:5, V2 = LETTERS[1:5])), sep = "\n")
#    } else {outMail[["body"]] = body }
}
#outlook.quick(c("gcook@CMC.edu; cook.gabriel@protonmail.com"), "Hey", "what's up", email.attachment)

outlook.grades <- function(x, to = "gcook@CMC.edu", from = "gcook@CMC.edu",
    subject = "Hey", body = "", attachment = "", name = "", grade = "") {
    if ("RDCOMClient" %ni% installed.packages()) {
        #install.packages("RDCOMClient", repos = "http://www.omegahat.net/R")
        library(devtools); devtools::install_github("omegahat/RDCOMClient")}
    df = as.data.frame(x); OutApp <- COMCreate("Outlook.Application");
    for (row in 1:nrow(df)) {
        outMail = OutApp$CreateItem(0)
        recipient.name <- df[row, name]; recipient.grade <- df[row, grade]
        recipient.email <- df[row, to] ; outMail[["To"]] = recipient.email
        outMail[["subject"]] = subject ; outMail[["body"]] = body
        if (file.exists(attachment)) {  outMail[["Attachments"]]$Add(attachment)  }
        try(outMail$Send())
        message(paste0("Sending message to: ", recipient.email," - ", subject,"\n"))
        outMail = ""    }}
#email.attachment = "G:/Sync/Courses/CMCStats/0.Curr/0.exams/2018/2018-PSYC-109-Grades.docx"
#outlook.grades(DF, to = "dummy.email", #"Student.Email"
#               from = "gcook@CMC.edu",
#               subject = "subject, subject",
#               body = "body body body",
#               name = "Name.First", grade = "Exam1",
#               attachment = "G:/Sync/Courses/CMCStats/0.Curr/0.exams/2018/2018-PSYC-109-Grades.docx"
#               )

#To embed table within the body of the email:
outlook.table <- function(to = "gcook@CMC.edu", from = "gcook@CMC.edu",
    subject = "Subject", body = "message body", attachment = "", table = "n") {
    #OutApp = "";
    OutApp <- COMCreate("Outlook.Application"); # outMail = OutApp$CreateItem(0)
    outMail = OutApp$CreateItem(0)
    outMail[["To"]] = to
    outMail[["subject"]] = subject ; #outMail[["body"]] = body
    if (file.exists(attachment)) {  outMail[["Attachments"]]$Add(attachment)  }
    pander::panderOptions('table.split.table', Inf)
    outMail[["body"]] = paste("Hello!", "", "The below summarises xxx:",
    pander::pandoc.table.return(data.frame(V1 = 1:5, V2 = LETTERS[1:5])), sep = "\n")
    #try(outMail$Send())
    message(paste0("Sending message to: ", recipient.email," - ", subject,"\n"))
    outMail = ""
    }

cleanup <- function(file.name = NULL) {
    if (file.exists(paste0(getwd(), "/",gsub(" ","_",file.name)," ",model,".R"))) file.remove(paste0(getwd(), "/",gsub(" ","_",file.name)," ",model,".R"))
    #if (file.exists(paste0(getwd(), "/",gsub(" ","_",file.name),"_",model,".R"))) file.remove(paste0(getwd(), "/",gsub(" ","_",file.name),"_",model,".R"))
    #if (file.exists(paste0(getwd(), "/",gsub(" ","_",file.name),"_",model,".spin.R"))) file.remove(paste0(getwd(), "/",gsub(" ","_",file.name),"_",model,".spin.R"))

    if (file.exists(paste0(getwd(), "/",gsub(" ","_",file.name),".spin.R"))) file.remove(paste0(getwd(), "/",gsub(" ","_",file.name),".spin.R"))
    if (file.exists(paste0(getwd(), "/",gsub(" ","_",file.name),".spin.Rmd"))) file.remove(paste0(getwd(), "/",gsub(" ","_",file.name),".spin.Rmd"))

    if (file.exists(paste0(getwd(), "/",gsub(" ","_",file.name)," ",model,".spin.Rmd"))) file.remove(paste0(getwd(), "/",gsub(" ","_",file.name)," ",model,".spin.Rmd"))
    if (file.exists(paste0(getwd(), "/",gsub(" ","_",file.name),"_",model,".spin.Rmd"))) file.remove(paste0(getwd(), "/",gsub(" ","_",file.name),"_",model,".spin.Rmd"))
    if (is.null(file.name)) message("File name not specified.")
}
