# Switch to flip when updating (for debouncing the save)
trigger_state_save = reactiveVal(F)

# Read the last project name
read_last_project = function(){
  path = here(dir_user(), ".project.rds")
  if (check_path(path)){
    project = readRDS(path)
    if(is.character(project)) return(project)
    else return("")
  } else {
    return("")
  }
}

# Save the current project name
save_current_project = function(){
  if(check_path(dir_user())){
    saveRDS(.project, here(dir_user(), ".project.rds"))
    cat(sprintf("Saved current project name '%s'\n", .project))
  }
}

# Read input states
read_input_state = function(){
  path = here(dir_project(), "input_state.rds")
  if (check_path(path)) {
    .saved_input_state <<- readRDS(path)
  } else {
    .saved_input_state <<- list()
  }  
}

# Save input states
save_input_state = function(){
  if (check_path(dir_project())) {
    saveRDS(.saved_input_state, here(dir_project(), "input_state.rds"))
    cat("\t\tSaved input states\n")
    trigger_state_save(F)
  }
}

# Observe
observe_input = function(id, react_val){
  observe({
    if(length(react_val()) == 1 && is.na(react_val())){
      cat(sprintf("\tInvalid value for input '%s'\n", id))
    }else{
      update_state(id, react_val())
    }
  }) |> bindEvent(react_val(), ignoreInit = T)
}

# Update
update_state = function(id, value){
  .saved_input_state[[id]] <<- value
  # .rs[[id]] = value
  cat(sprintf("\tUpdated state '%s' to '%s'\n", id, paste(value, collapse = " ")))
  trigger_state_save(F)
  trigger_state_save(T)
}

# Restore (after project load)
restore_all_inputs = function(input){
  .saved_input_state %>% names %>% lapply(restore_input, input)
}

input_type = function(id){
  local_id = str_match(id, "(?:.*-)?(.+)")[, 2]
  str_match(local_id, "(.+?)_")[, 2]
}

update_input = function(id, value){
  type = input_type(id)
  if(type == "num") updateNumericInput(inputId = id, value = value)
  else if(type == "chk") updateCheckboxInput(inputId = id, value = value)
  else if(type == "sel") updateSelectInput(inputId = id, selected = value)
  else if(type == "cbg") updateCheckboxGroupInput(inputId = id, selected = value)
  else if(type == "rbn") updateRadioButtons(inputId = id, selected = value)
  else if(type == "tbs") updateTabsetPanel(inputId = id, selected = value)
}

restore_input = function(id, input){
  if(!id %in% names(input)) return()
  v0 = paste(input[[id]], collapse = " ")
  v = .saved_input_state[[id]]
  cat(sprintf("\tRestoring input '%s':\n", id))
  cat(sprintf("\t\tcurrent state: %s\n", v0))
  cat(sprintf("\t\tsaved state:   %s\n", paste(v, collapse = " ")))
  update_input(id, v)
}

# Saved state shortcut
ss = function(id){
  tryCatch(.saved_input_state[[id]], error = function(e) NULL)
}

# Input UIs that restore the saved state
observedNumericInput = function(id, label, value, ...){
  numericInput(id, label, ss(id) %||% value, ...)
}

observedCheckboxInput = function(id, label, value, ...){
  checkboxInput(id, label, ss(id) %||% value, ...)
}

observedSelectInput = function(id, label, choices, selected, ...){
  selectInput(id, label, choices, ss(id) %||% selected, ...)
}

observedCheckboxGroupInput = function(id, label, choices, selected, ...){
  checkboxGroupInput(id, label, choices, ss(id) %||% selected, ...)
}

observedRadioButtons = function(id, label, choices, selected, ...){
  radioButtons(id, label, choices, ss(id) %||% selected, ...)
}
