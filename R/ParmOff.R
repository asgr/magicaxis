.mag_pick_args = function(dots, use_args = NULL, prefix = NULL){
  if(length(dots) == 0){
    return(list(current_args = list(), ignore_args = list()))
  }
  if(is.null(names(dots))){
    names(dots) = rep('', length(dots))
  }
  strip = NULL
  if(!is.null(prefix)){
    strip = paste0('^', prefix, '\\.')
  }
  ParmOff::ParmOff(
    function(...) NULL,
    .args = dots,
    .use_args = use_args,
    .strip = strip,
    .return = 'args',
    .check = FALSE
  )
}

.mag_split_args = function(dots, use_args = NULL, prefix = NULL){
  if(length(dots) == 0){
    return(list(args = list(), rest = list()))
  }
  if(is.null(names(dots))){
    names(dots) = rep('', length(dots))
  }
  if(is.null(prefix)){
    keep = names(dots) %in% use_args
    args = .mag_pick_args(dots[keep], use_args = use_args)$current_args
  }else{
    keep = grepl(paste0('^', prefix, '\\.'), names(dots))
    args = .mag_pick_args(dots[keep], use_args = use_args, prefix = prefix)$current_args
  }
  list(args = args, rest = dots[!keep])
}

.mag_merge_args = function(args, fixed){
  if(length(args) == 0){
    return(fixed)
  }
  if(length(fixed) == 0){
    return(args)
  }
  if(is.null(names(args))){
    names(args) = rep('', length(args))
  }
  if(is.null(names(fixed))){
    names(fixed) = rep('', length(fixed))
  }
  keep = names(args) == '' | !names(args) %in% names(fixed)[names(fixed) != '']
  c(args[keep], fixed)
}

.mag_defaults = function(defaults, user){
  if(length(defaults) == 0){
    return(user)
  }
  if(length(user) == 0){
    return(defaults)
  }
  if(is.null(names(defaults))){
    names(defaults) = rep('', length(defaults))
  }
  if(is.null(names(user))){
    names(user) = rep('', length(user))
  }
  keep = names(defaults) == '' | !names(defaults) %in% names(user)[names(user) != '']
  c(defaults[keep], user)
}

.mag_call = function(.func, .dots = list(), ...){
  ParmOff::ParmOff(.func, .args = .mag_merge_args(.dots, list(...)), .check = FALSE)
}
