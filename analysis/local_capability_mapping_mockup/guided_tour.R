# Guided tour of UI using cicerone https://cicerone.john-coene.com/index.html 
guide <- Cicerone$
  new()$
  step(
    el = "chosen_detail_level",
    title = "Choose the capabilities level of detail",
    description = "Depending on your need for detail choose to either see the high level categorisations or the subcategories within these."
  )$
  step(
    el = ".capability_select",
    title = "Choose the capabilities",
    description = "Select the capabilities of interest",
    is_id = FALSE # since used <div> 
  )$
  step(
    el = "chosen_lad",
    title = "Choose the Local Auhtorites",
    description = "Choose the Local Authorities the organisation operates in."
  )$
  step(
    el = "chosen_inital_response_time",
    title = "Choose the inital response time.",
    description = "How fast does the organisation say they can respond to an emergency."
  )$
  step(
    el = "provides_vols",
    title = "Provides DBS volunteers",
    description = "Check this box if you require the organisation to provide DBS checked volunteers."
  )$
  step(
    el = "primary_only",
    title = "Primary response",
    description = "Check this box if you require the capability selected to be the organisations primary response (i.e. not secondary)."
  )
  
  






