# spectral_outlier_filter.R

#–– Dependencies ––
library(dplyr)
library(tidyr)

#–– 1. Pearson‐similarity helper ––
calculate_similarity_pct <- function(query, reference) {
  cor_val <- cor(query, reference, use = "complete.obs")
  pmax(pmin(cor_val * 100, 100), 0)
}

#–– Safe PCA helper ––
perform_pca <- function(spectra_matrix) {
  # compute variances across columns (wavenumbers)
  vars <- apply(spectra_matrix, 2, var, na.rm = TRUE)
  # keep only non‐constant columns
  keep_idx <- which(vars > 0)
  if (length(keep_idx) < 2) {
    # if fewer than 2 informative columns, skip scaling
    return(stats::prcomp(spectra_matrix, center = TRUE, scale. = FALSE))
  }
  # subset matrix for PCA
  M2 <- spectra_matrix[, keep_idx, drop = FALSE]
  # perform PCA with scaling
  stats::prcomp(M2, center = TRUE, scale. = TRUE)
}

#–– 2. PCA central spectrum finder ––
find_central_spectrum_pca <- function(group_df) {
  spec_cols <- grep("^wn_", names(group_df), value = TRUE)
  M_full    <- as.matrix(group_df[, spec_cols])
  
  # if fewer than 2 scans, mark first as central
  if (nrow(M_full) < 2) {
    group_df$central <- FALSE
    group_df$central[1] <- TRUE
    return(group_df)
  }
  
  # run safe PCA
  pca_out <- perform_pca(M_full)
  
  # PCA scores (rows = scans)
  scores   <- pca_out$x
  centroid <- colMeans(scores, na.rm = TRUE)
  dists    <- apply(scores, 1, function(r) sqrt(sum((r - centroid)^2)))
  central_idx <- which.min(dists)
  
  group_df$central <- FALSE
  group_df$central[central_idx] <- TRUE
  group_df
}

#–– 3. Compute similarity to central spectrum ––
compute_similarity_to_central <- function(data) {
  if (nrow(data) < 2) {
    data$similarity_pct <- NA
    return(data)
  }
  spec_cols     <- grep("^wn_", names(data), value = TRUE)
  M             <- as.matrix(data[, spec_cols])
  central_spec  <- as.numeric(data[data$central, spec_cols])
  similarities  <- apply(M, 1, function(x) calculate_similarity_pct(x, central_spec))
  data$similarity_pct <- similarities
  data
}

#–– 4. Main outlier processing ––
process_spectral_outliers <- function(wide_spectra_df, similarity_threshold = 96) {
  library(dplyr)
  
  # A) Extract sample and replicate IDs using same logic as avg_data
  wide_spectra_df <- wide_spectra_df %>%                         # or wide_spectra_df <- …
    dplyr::mutate(
      ## 1) does the name end in ".<digit>" ?
      has_dot_ext = grepl("\\.[0-9]+$", file_id),
      ext_digit   = ifelse(has_dot_ext,
                           sub("^.*\\.([0-9]+)$", "\\1", file_id),
                           NA_character_),
      
      ## 2) are there *multiple* different ext digits?  (0/1/2/3 …)
      mixed_ext   = length(unique(na.omit(ext_digit))) > 1,
      
      ## 3) replicate logic  ---------------------------------------------
      rep = dplyr::case_when(
        mixed_ext ~ ext_digit,                              # normal .0/.1/.2/.3 case
        has_dot_ext ~ sub("^.*?([0-9]{2})\\.[0-9]+$", "\\1", file_id),  # only .0 files
        TRUE        ~ sub("^.*?([0-9]+)(\\.[^.]+)?$", "\\1", file_id)   # CSV / fallback
      ),
      
      ## 4) sample logic  -------------------------------------------------
      sample = dplyr::case_when(
        mixed_ext ~ sub("\\.[0-9]+$", "", file_id),         # strip .digit
        has_dot_ext ~ sub("([0-9]{2})\\.[0-9]+$", "", file_id), # strip last 2 digits + .0
        TRUE        ~ sub("([0-9]+)(\\.[^.]+)?$", "", file_id)
      )
    ) %>%
    dplyr::select(-has_dot_ext, -ext_digit, -mixed_ext)      # tidy up
  

  
  # 1) Identify central spectrum per sample
  df1 <- wide_spectra_df %>%
    group_by(sample) %>%
    group_modify(~ find_central_spectrum_pca(.x)) %>%
    ungroup()
  
  # 2) Compute similarity percentages
  sim_results <- df1 %>%
    group_by(sample) %>%
    group_modify(~ compute_similarity_to_central(.x)) %>%
    ungroup()
  
  debug_sim <- sim_results %>%
    select(sample, file_id, rep, similarity_pct)
  
  message("=== similarity table ===")
  print(debug_sim)
  
  # 3) Find samples with any replicate below threshold
  bad_samples <- sim_results %>%
    filter(similarity_pct < similarity_threshold) %>%
    pull(sample) %>% unique()
  
  # 4) Remove all scans for those bad samples
  cleaned_spectra <- wide_spectra_df %>% filter(! sample %in% bad_samples)
  
  # 5) Average the remaining spectra per sample
  average_spectra <- cleaned_spectra %>%
    group_by(sample) %>%
    summarise(
      across(starts_with("wn_"), ~ mean(.x, na.rm = TRUE)),
      .groups = "drop"
    )
  
  # Return results
  list(
    average_spectra    = average_spectra,
    cleaned_spectra    = cleaned_spectra,
    similarity_results = sim_results
  )
}


#–– Main filter function ––
filter_spectral_outliers <- function(df,
                                     start_seq,
                                     stop_seq,
                                     threshold = 96,
                                     frac_cut  = 0.50) {
  # Determine dynamic ID column (first column)
  id_col <- names(df)[1]
  
  # Identify numeric spectral columns
  spec_cols <- names(df)[sapply(df, is.numeric)]
  n         <- length(spec_cols)
  if (n < 2) {
    warning("Not enough numeric columns for outlier detection — skipping")
    return(list(
      cleaned_df        = df,
      removed_samples   = character(0),
      removed_replicates = character(0)
    ))
  }
  
  print(start_seq)
  print(stop_seq)
  
  # Build equal-length wavenumber sequence
  wns <- seq(from = start_seq, to = stop_seq, length.out = n)
  names(df)[match(spec_cols, names(df))] <- paste0("wn_", wns)
  
  # Add helper columns
  wide <- df
  wide$file_id  <- wide[[id_col]]
  wide <- wide %>%                         # or wide_spectra_df <- …
    dplyr::mutate(
      ## 1) does the name end in ".<digit>" ?
      has_dot_ext = grepl("\\.[0-9]+$", file_id),
      ext_digit   = ifelse(has_dot_ext,
                           sub("^.*\\.([0-9]+)$", "\\1", file_id),
                           NA_character_),
      
      ## 2) are there *multiple* different ext digits?  (0/1/2/3 …)
      mixed_ext   = length(unique(na.omit(ext_digit))) > 1,
      
      ## 3) replicate logic  ---------------------------------------------
      rep = dplyr::case_when(
        mixed_ext ~ ext_digit,                              # normal .0/.1/.2/.3 case
        has_dot_ext ~ sub("^.*?([0-9]{2})\\.[0-9]+$", "\\1", file_id),  # only .0 files
        TRUE        ~ sub("^.*?([0-9]+)(\\.[^.]+)?$", "\\1", file_id)   # CSV / fallback
      ),
      
      ## 4) sample logic  -------------------------------------------------
      sample = dplyr::case_when(
        mixed_ext ~ sub("\\.[0-9]+$", "", file_id),         # strip .digit
        has_dot_ext ~ sub("([0-9]{2})\\.[0-9]+$", "", file_id), # strip last 2 digits + .0
        TRUE        ~ sub("([0-9]+)(\\.[^.]+)?$", "", file_id)
      )
    ) %>%
    dplyr::select(-has_dot_ext, -ext_digit, -mixed_ext)      # tidy up
  
  
  # PCA-based similarity
  out <- process_spectral_outliers(wide, similarity_threshold = threshold)
  sim <- out$similarity_results %>%
    mutate(is_outlier = similarity_pct < threshold)
  
  # Compute outlier fractions
  sim <- sim %>%
    group_by(sample) %>%
    mutate(outlier_frac = mean(is_outlier, na.rm = TRUE)) %>%
    ungroup()
  
  # Identify samples/reps to drop
  drop_samples <- sim %>%
    filter(outlier_frac >= frac_cut) %>%
    distinct(sample) %>% pull(sample)
  drop_reps    <- sim %>%
    filter(outlier_frac < frac_cut & is_outlier) %>%
    pull(file_id)
  
  # Filter original data by dynamic ID
  cleaned <- wide %>%
    filter(
      !sub("([0-9]+\\.?[0-9]*)$", "", file_id) %in% drop_samples,
      !file_id %in% drop_reps
    ) %>%
    select(-file_id, -sample, -rep)
  
  list(
    cleaned_df         = cleaned,
    removed_samples    = drop_samples,
    removed_replicates = drop_reps
  )
}