balancing_zones <- function(){
  url <- "https://transparency.entsog.eu/api/v1/balancingZones"
  d <- api_req(url)
  d$balancingZones
}


operators <- function(){
  url <- "https://transparency.entsog.eu/api/v1/operators"
  d <- api_req(url)
  d$operators
}


interconnections <- function(from_operator_key=NULL, to_operator_key=NULL){
  url <- "https://transparency.entsog.eu/api/v1/interconnections"
  params <- list()

  if(!is.null(from_operator_key)){
    params['fromOperatorKey'] <- paste(from_operator_key, sep=",", collapse=",")
  }

  if(!is.null(to_operator_key)){
    params['toOperatorKey'] <- paste(to_operator_key, sep=",", collapse=",")
  }

  d <- api_req(url, params = params)
  d$interconnections
}


physical_flows <- function(operator_key, point_key, direction, date_from="2019-01-01", date_to=lubridate::today(), limit=-1){

  url <- "https://transparency.entsog.eu/api/v1/operationalData"

  if(!is.null(point_key) && is.null(operator_key)){
    stop("Needs to specify operator_key when point_key is given.")
  }

  # Can only do one operator at a time
  if(!is.null(operator_key) && length(unique(operator_key))>1){
    message("Splitting by operator")
    operator_points <- split(point_key, operator_key)
    return(lapply(names(operator_points), function(operator){
      physical_flows(operator_key=operator,
                            point_key = operator_points[[operator]],
                            direction=direction,
                            date_from=date_from,
                            date_to=date_to,
                            limit=limit)
    }) %>%
      do.call(bind_rows, .))
  }

  # Can only do limited days per call. Doing a call per semester
  dates <- seq(as.Date(date_from), as.Date(date_to), by="day")
  dates_group <- lubridate::year(dates)
  if(length(unique(dates_group))>1){
    message("Splitting by dates")
    return(lapply(split(dates, dates_group), function(dates){
      physical_flows(operator_key=operator_key,
                            point_key = point_key,
                            direction=direction,
                            date_from=min(dates),
                            date_to=max(dates),
                            limit=limit)
    }) %>%
      do.call(bind_rows, .))
  }

  params <- list(
    indicator="Physical Flow",
    periodType="day",
    timezone="CET"
  )

  if(!is.null(operator_key)){
    params['operatorKey'] <- paste(unique(operator_key), sep=",", collapse=",")
  }

  if(!is.null(point_key)){
    params['pointKey'] <- paste(unique(point_key), sep=",", collapse=",")
  }

  if(!is.null(date_from)){
    params['from'] <- strftime(as.Date(date_from),"%Y-%m-%d")
  }

  if(!is.null(date_to)){
    params['to'] <- strftime(as.Date(date_to),"%Y-%m-%d")
  }

  if(!is.null(direction)){
    params['directionKey'] <- direction
  }

  d <- api_req(url, params=params, limit=limit)

  if(is.null(d)){return(NULL)}

  d$operationalData %>%
    mutate_at("value", as.numeric) %>%
    mutate_at(c("isCmpRelevant","isCamRelevant"), as.logical) %>%
    mutate_at(c("periodFrom","periodTo"), as.POSIXct, tz="CET") %>%
    mutate(date=as.Date(periodFrom))
}

