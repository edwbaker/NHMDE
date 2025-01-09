.natureoverheard_data_cols <- function() {
  return(list(
    "identification_experience" = "factor",
    "road_type" = "factor",
    "distance_from_road" = "factor",
    "habitat_features" = "character",
    "temperature" = "numeric",
    "cloud_cover" = "factor",
    "sun_or_shade" = "factor",
    "device_used" = "factor",
    "device_brand" = "factor",
    "other_device_info" = "character",
    "additional_device_info" = "character",
    "distance_from_device" = "factor",
    "audio_submission" = "logical",
    "sample_location_latitude" = "numeric",
    "sample_location_longitude" = "numeric",
    "sample_location_county" = "factor",
    "verification_completion_status" = "factor",
    "apocrita" = "numeric",
    "coleoptera" = "numeric",
    "diptera" = "numeric",
    "formicidae" = "numeric",
    "hemiptera" = "numeric",
    "lepidoptera" = "numeric",
    "orthoptera" = "numeric",
    "syrphidae" = "numeric",
    "other" = "numeric"
  ))
}

.natureoverheard_verification_cols <- function() {
  return(list(
  ))
}

#' @importFrom forcats fct_recode fct_relevel
.natureoverheard_factor_recode <- function(data) {
  data$identification_experience <- fct_recode(as.factor(data$identification_experience),
    "No Response" = "",
    "ID Some Wildlife" = "I am familiar with identifying some wildlife (eg, birds or butterflies), but not most insects",
    "ID Insect Groups" = "I am familiar with recognising the main groups of insects",
    "New to ID" = "I am new to identifying wildlife",
    "ID Common Insects" = "I am confident in identifying many common insects to species level"
  )
  data$identification_experience <- fct_relevel(data$identification_experience,
    "New to ID",
    "ID Some Wildlife",
    "ID Insect Groups",
    "ID Common Insects",
    "No Response"
  )
  data$road_type <- fct_recode(data$road_type,
    "No Response" = "",
    "Residential" = "Residential street: a road that provides access to residential properties",
    "Backroad" = "Backroad: a secondary or little-used road, sometimes used as an alternative to main roads",
    "High Street" = "High street: main street that predominately features shops and businesses",
    "Main Road" = "Main road: frequently used roads that link cities, towns, or villages"
  )
  data$road_type <- fct_relevel(data$road_type,
    "Residential",
    "Backroad",
    "High Street",
    "Main Road",
    "No Response"
  )
  data$distance_from_road <- fct_recode(data$distance_from_road,
    "No Response" = "",
    "6-8" = "Between 6 and 8 metres",
    "2-4m" = "Between 2 and 4 metres",
    ">2m" = "Less than 2 metres",
    "4-6m" = "Between 4 and 6 metres",
    "8-10m" = "Between 8 and 10 metres"
  )
  data$distance_from_road <- fct_relevel(data$distance_from_road,
    ">2m",
    "2-4m",
    "4-6m",
    "6-8",
    "8-10m",
    "No Response"
  )
  data$cloud_cover <- fct_recode(data$cloud_cover,
    "No Response" = "",
    "Half cloud" = "Half clear and half cloud",
    "Clear" =  "All or mostly clear",
    "Cloudy" = "All or mostly cloud",
    "Not recorded" = "I didn\u2019t record this"
  )
  data$cloud_cover <- fct_relevel(data$cloud_cover,
    "Clear",
    "Half cloud",
    "Cloudy",
    "Not recorded",
    "No Response"
  )
  data$sun_or_shade <- fct_recode(data$sun_or_shade,
    "No Response" = "",
    "Shade" = "Entirely shaded",
    "Part shade" ="Partly in sun and partly shaded",
    "Sun" = "Entirely in sunshine",
    "Not recorded" = "I didn\u2019t record this"
  )
  data$sun_or_shade <- fct_relevel(data$sun_or_shade,
    "Sun",
    "Part shade",
    "Shade",
    "Not recorded",
    "No Response"
  )
  data$device_used <- fct_recode(data$device_used,
    "No Response" = "",
    "Phone" = "Mobile phone",
    "Other" =  "Other recording device (please specify below)",
    "Tablet" = "Tablet"
  )
  data$device_used <- fct_relevel(data$device_used,
    "Phone",
    "Tablet",
    "Other",
    "No Response"
  )
  data$device_brand <- fct_recode(data$device_brand,
    "No Response" = ""
  )
  data$device_brand <- fct_relevel(data$device_brand,
    "iPhone",
    "iPad",
    "Huawei",
    "Google Pixel",
    "Motorola",
    "Samsung",
    "Sony",
    "Tascam",
    "Other",
    "No Response"
  )
  data$distance_from_device <- fct_recode(data$distance_from_device,
    "No Response" = "",
    "In hand" = "It was in my hand",
    "<2m" = "Less than two metres",
    "2-5m" = "Two-five metres",
    ">5m" = "Over five metres"
  )
  data$distance_from_device <- fct_relevel(data$distance_from_device,
    "In hand",
    "<2m",
    "2-5m",
    ">5m",
    "No Response"
  )
  data$sample_location_county <- fct_recode(data$sample_location_county,
    "No Response" = ""
  )
  data$sample_location_county <- fct_relevel(data$sample_location_county,
    "No Response",
    after = Inf
  )
  return(data)
}

.natureoverheard_manipulate_cols <- function(data) {
  habitat_cols <- t(as.data.frame(lapply(data$habitat_features, .nature_overheard_hf),col.names=NULL, row.names=paste0("habitat_feature_", .natureoverheard_hf_features())))
  rownames(habitat_cols) <- NULL
  habitat_col = which(colnames(data) == "habitat_features")
  data <- cbind(data[,1:habitat_col], habitat_cols, data[,(habitat_col+1):ncol(data)])
  return(data)
}

#' Habitat features in the Nature Overheard dataset
#'
#' @return A character vector of habitat features
#' @export
.natureoverheard_hf_features <- function() {
  features <- c("Trees", "Shrubs", "Hedgerow", "Mown lawn", "Wildflowers", "Garden plants")
  return(features)
}

.nature_overheard_hf <- function(row) {
  hf <- .natureoverheard_hf_features()
  return(hf %in% unlist(strsplit(row, split=",")))
}

.natureoverheard_data_private <- function() {
  return(
    list(
      "name" = c("first_name", "last_name"),
      "group" = c("group_name")
    )
  )
}

#' Get list of taxon column names for Nature Overheard
#'
#' @return A character vector of taxon column names
nature_overheard_taxa <- function() {
  taxa <- c("apocrita", "coleoptera", "diptera", "formicidae", "hemiptera", "lepidoptera", "orthoptera", "syrphidae", "other")
  return(taxa)
}

#' Summarise verifications into samples table
#'
#' @param samples A data frame with samples data
#' @param verifications A data frame with verifications data
#' @return A data frame: samples data with the verifications data summarised
#' @export
nature_overheard_get_verified_samples <- function(samples, verifications) {
  verifications$identified_name <- tolower(verifications$identified_name)

  prototype_v <- vector(mode="numeric", length=nrow(samples))
  prototype_t <- cbind(prototype_v, prototype_v, prototype_v, prototype_v)
  colnames(prototype_t) <- c("Correct", "Considered_Correct", "Incorrect", "Unable_to_Verify")

  # Generate all the new columns
  for (i in 1:length(nature_overheard_taxa())) {
    taxon <- nature_overheard_taxa()[i]

    new_cols <- prototype_t
    colnames(new_cols) <- paste(taxon, colnames(new_cols), sep="_")

    samples <- cbind(samples, new_cols)

    if (i < length(nature_overheard_taxa())) {
      pos <- which(colnames(samples) == taxon)
      new_order <- c(colnames(samples)[1:pos], colnames(new_cols), colnames(samples)[(pos+1):(length(colnames(samples))-length(colnames(new_cols)))])
      samples <- samples[, new_order]
    }

  }

  # Fill in the new columns
  for (i in 1:length(samples$sample_id)) {
    sample_id <- samples$sample_id[i]
    for (j in 1:length(nature_overheard_taxa())) {
      taxon <- nature_overheard_taxa()[j]

      temp <- verifications[verifications$sample_id == sample_id & verifications$identified_name == taxon & verifications$level_2_status == "Correct", c("observation_count")]
      if (length(temp) == 0) {
        value <- 0
      } else {
        value <- sum(as.numeric(temp))
      }
      samples[samples$sample_id == sample_id, paste(taxon, "Correct", sep="_")] <- value

      temp <- verifications[verifications$sample_id == sample_id & verifications$identified_name == taxon & verifications$level_2_status == "Considered Correct", c("observation_count")]
      if (length(temp) == 0) {
        value <- 0
      } else {
        value <- sum(as.numeric(temp))
      }
      samples[samples$sample_id == sample_id, paste(taxon, "Considered_Correct", sep="_")] <- value

      temp <- verifications[verifications$sample_id == sample_id & verifications$identified_name == taxon & verifications$level_2_status == "Incorrect", c("observation_count")]
      if (length(temp) == 0) {
        value <- 0
      } else {
        value <- sum(as.numeric(temp))
      }
      samples[samples$sample_id == sample_id, paste(taxon, "Incorrect", sep="_")] <- value

      temp <- verifications[verifications$sample_id == sample_id & verifications$identified_name == taxon & verifications$level_2_status == "Unable to Verify", c("observation_count")]
      if (length(temp) == 0) {
        value <- 0
      } else {
        value <- sum(as.numeric(temp))
      }
      samples[samples$sample_id == sample_id, paste(taxon, "Unable_to_Verify", sep="_")] <- value
    }
  }
  return(samples)
}
