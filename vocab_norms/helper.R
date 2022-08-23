
# DEMOGRAPHIC CLUMPING FUNCTIONS
clump_step <- function(groups, map, fun_demo, min_obs = 100) {
  
  # print("clump_step")
  if (fun_demo == "birth_order") {
    fine_groups <- groups[0:(nrow(groups) - 2),]
    small_groups <- slice(groups, (nrow(groups) - 1):nrow(groups))
    clump_demo <- small_groups$demo[1]
    clump <- data.frame(demo = clump_demo, n = sum(small_groups$n))
    clumped <- bind_rows(fine_groups, clump)
  } else if (fun_demo == "ethnicity") {
    fine_groups <- filter(groups, n >= min_obs & demo != "Other")
    small_groups <- filter(groups, n < min_obs | demo == "Other")
    clump_demo <- paste(small_groups$demo, collapse = ", ")
    clump <- data.frame(demo = clump_demo, n = sum(small_groups$n))
    clumped <- bind_rows(fine_groups, clump)
  } else if (fun_demo %in% c("sex", "caregiver_education")) {
    smallest <- row.names(groups)[groups$n == min(groups$n)] %>% as.numeric()
    neighbor <- row.names(groups)[groups$n == min(groups[smallest - 1,]$n,
                                                  groups[smallest + 1,]$n,
                                                  na.rm = TRUE)] %>%
      as.numeric()
    small_groups <- slice(groups, c(smallest, neighbor))
    pre_fine_groups <- groups[0:(min(smallest, neighbor) - 1),]
    if (max(smallest, neighbor) == nrow(groups)) {
      post_fine_groups <- NULL
    } else {
      post_fine_groups <- groups[(max(smallest, neighbor) + 1):nrow(groups),]
    }
    clump_demo <- paste(small_groups$demo, collapse = ", ")
    clump <- data.frame(demo = clump_demo, n = sum(small_groups$n))
    clumped <- bind_rows(pre_fine_groups, clump, post_fine_groups)
  }
  for (small_demo in small_groups$demo) {
    map[[small_demo]] <- as.character(clump_demo)
    map[which(map == small_demo)] <- as.character(clump_demo)
  }
  return(list("clumped" = clumped, "map" = map))
}

# DO THE CLUMPING
clump_demo_groups <- function(groups, map, fun_demo, min_obs = 100) {
  # print("clump_demo_groups")
  
  if (all(groups$n >= min_obs) | nrow(groups) == 1) {
    if (fun_demo == "birth_order" & nrow(groups) > 1) {
      demos <- unique(groups$demo)
      groups <- groups %>%
        mutate(demo = c(as.character(demos[1:(length(demos) - 1)]),
                        paste0(demos[length(demos)], "+")))
      plus <- max(which(names(map) == map))
      map[plus:length(map)] <- paste0(map[plus], "+")
    }
    groups <- groups %>%
      filter(fun_demo == "identity" | n >= min_obs) %>%
      rename(clump = demo) %>%
      mutate(demo_label = sprintf("%s (n = %s)", clump, n))
    return(list("groups" = groups, "map" = map))
  } else {
    step <- clump_step(groups, map, fun_demo)
    clumped_groups <- step$clumped
    clump_map <- step$map
    return(clump_demo_groups(clumped_groups, clump_map, fun_demo))
  }
}
