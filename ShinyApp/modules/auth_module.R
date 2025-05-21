# ── auth_module.R ──────────────────────────────────────────────────────────────
library(shiny)
library(bcrypt)      
library(shinyjs)

# ----------  UI ----------
loginUI <- function(id) {
  tagList()
}

# ----------  server ----------
loginServer <- function(id, credentials_path){
  moduleServer(id, function(input, output, session){
    
    # always read the current credentials file
    readCreds  <- function() readRDS(credentials_path)
    writeCreds <- function(df) saveRDS(df, credentials_path)
    
    logged <- reactiveVal(FALSE)
    
    #─────────────────────────
    #   1 · modal builder
    #─────────────────────────
    showLogin <- function(show_create = FALSE){
      
      # inner UI:  two columns side-by-side
      login_col <- tagList(
        h4("Log in"),
        textInput(session$ns("l_user"), "User name"),
        passwordInput(session$ns("l_pwd"), "Password"),
        actionButton(session$ns("l_go"), "Log in",
                     class = "btn btn-auth-login")   
      )
      
      create_col <- tagList(
        h4("Create account"),
        textInput(session$ns("c_user"), "User name"),
        passwordInput(session$ns("c_pwd1"), "Password"),
        passwordInput(session$ns("c_pwd2"), "Confirm"),
        actionButton(session$ns("c_go"), "Create",
                     class = "btn btn-auth-create")
      )
      
      showModal(
        modalDialog(
          fluidRow(
            column(6, login_col),
            column(6, create_col  %>% conditionalPanel(
              condition = if (show_create) "true" else "false",
              ns = session$ns
            ))
          ),
          footer = htmltools::tagAppendAttributes(
            modalButton("Close"),
            class = "btn-auth-close"
          ),
          easyClose = TRUE
        )
      )
    }
    
    # expose the helper so main.R can call it
    session$userData$showLogin <- showLogin
    
    #─────────────────────────
    #   2 ·  handle LOG-IN
    #─────────────────────────
    observeEvent(input$l_go,{
      req(input$l_user, input$l_pwd)
      creds <- readCreds()
      row   <- creds[creds$user == input$l_user, , drop=FALSE]
      
      if (nrow(row) == 1 &&
          bcrypt::checkpw(input$l_pwd, as.character(row$pwd_hash))){
        logged(TRUE)
        removeModal()
      } else {
        showNotification("Invalid credentials", type = "error")
      }
    })
    
    #─────────────────────────
    #   3 ·  handle ACCOUNT CREATION
    #─────────────────────────
    # ── inside loginServer() — replace the existing "handle ACCOUNT CREATION" block ──
    observeEvent(input$c_go, {
      
      # 1 · basic field checks -------------------------------------------------
      req(input$c_user, input$c_pwd1, input$c_pwd2)
      
      # a) passwords must match
      if (input$c_pwd1 != input$c_pwd2) {
        showNotification("Passwords do not match", type = "error")
        return()
      }
      
      # b) username cannot contain spaces or be empty
      if (grepl("\\s", input$c_user)) {
        showNotification("User name cannot contain spaces", type = "error")
        return()
      }
      
      # 2 · read the *latest* credentials file --------------------------------
      creds <- readCreds()
      print(creds)
      # c) user must be unique
      if (input$c_user %in% creds$user) {
        showNotification("User already exists", type = "error")
        return()
      }
      
      # 3 · add the new row and write it back ----------------------------------
      new_row <- data.frame(
        user     = input$c_user,
        pwd_hash = bcrypt::hashpw(input$c_pwd1),
        stringsAsFactors = FALSE
      )
      
      writeCreds(rbind(creds, new_row))
      
      # 4 · feedback & convenience --------------------------------------------
      showNotification("Account created – you can log in now!", type = "message")
      
      updateTextInput(session, "l_user", value = input$c_user)  # pre-fill login
      updateTextInput(session, "c_user",  value = "")
      updateTextInput(session, "c_pwd1",  value = "")
      updateTextInput(session, "c_pwd2",  value = "")
    })
    
    
    # logged() becomes TRUE after success
    logged
  })
}