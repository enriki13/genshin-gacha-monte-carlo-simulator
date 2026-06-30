# ============================================================
# GENSHIN GACHA MONTE CARLO SIMULATOR
# ============================================================

setwd("C:/Users/stoja/Desktop/random excel exercises")

#----Setting up all the libraries needed---------------------
library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)

# --- SECTION 1: THE PROBABILITY FUNCTION --------------------
# The pity system works like this:
#   - Pulls 1-73:  flat 0.6% chance each pull
#   - Pulls 74-89: probability increases by 6% each pull
#   - Pull 90:     guaranteed 5-star (100%)

get_pull_probability <- function(pity) {
  
  # "function(pity)" means: this block of code accepts one 
  # input called "pity" and returns one output (the probability).
  
  if (pity <= 73) {
    return(0.006)                          # flat 0.6%
    
  } else if (pity >= 74 && pity <= 89) {
    return(0.006 + 0.06 * (pity - 73))    # ramp-up formula
    
  } else {
    return(1.0)                            # pity 90 = guaranteed
  }
}

# --- SECTION 2: THE SINGLE SESSION SIMULATOR ----------------
# This function simulates ONE player wishing until they hit
# their target number of copies (constellation goal).
#
# Inputs:
#   copies_needed  - how many copies of the character you want
#                    C0 = 1 copy, C1 = 2, C2 = 3, C6 = 7
#   start_pity     - pity counter you're starting with (default 0)
#   start_guaranteed - whether your next 5-star is guaranteed (default FALSE)
#
# Output:
#   total_pulls    - how many pulls it took to reach your goal

simulate_session <- function(copies_needed, 
                             start_pity = 0, 
                             start_guaranteed = FALSE) {
  
  # --- Initialize state variables ---
  pity        <- start_pity        # tracks pulls since last 5-star
  copies      <- 0                 # tracks how many featured copies collected
  total_pulls <- 0                 # counts total pulls made this session
  guaranteed  <- start_guaranteed  # TRUE = next 5-star is 100% featured
  
  
  # --- Main wishing loop ---
  # "while" means: keep doing this block UNTIL the condition is false.
  # We keep pulling until we have enough copies.
  
  while (copies < copies_needed) {
    
    # Increment pity BEFORE calculating probability.
    # Pity starts at 0 (just got a 5-star), so first pull = pity 1.
    pity <- pity + 1
    
    # Get the probability for this specific pity count.
    prob <- get_pull_probability(pity)
    
    # runif(1) generates ONE random number between 0.0 and 1.0.
    # If that random number is LESS than our probability, we hit a 5-star.
    # Example: prob = 0.006, so only ~0.6% of random numbers will be < 0.006.
    roll <- runif(1)
    
    if (roll < prob) {
      
      # We landed a 5-star! Now check the 50/50.
      # Two ways to get the featured character:
      #   A) We were already guaranteed (lost last 50/50), OR
      #   B) We win the 50/50 coin flip right now (50% chance)
      
      won_5050 <- runif(1) < 0.5   # another coin flip, TRUE = win
      
      if (guaranteed || won_5050) {
        # Got the featured character!
        copies     <- copies + 1
        guaranteed <- FALSE          # reset guarantee
        
      } else {
        # Lost the 50/50 — got a standard 5-star instead (Diluc, Jean, etc.)
        # Next 5-star is now guaranteed to be featured.
        guaranteed <- TRUE
      }
      
      # Whether we won or lost the 50/50, pity always resets after a 5-star.
      pity <- 0
    }
    
    # Count this pull regardless of outcome.
    total_pulls <- total_pulls + 1
  }
  
  return(total_pulls)
  # The function ends here and hands back the pull count.
}


# --- SECTION 3: RUN 100,000 SIMULATIONS ---------------------
# Now we run simulate_session() over and over and collect results.

# Set a "seed" for the random number generator.
# This makes your results reproducible — if you run the script
# again, you get the exact same numbers. Important for research.
set.seed(42)

# Configuration — change these to model different scenarios.
N_SIMS        <- 100000   # number of simulation runs
COPIES_NEEDED <- 3        # C2 = 3 copies of the featured character
START_PITY    <- 0        # change if you already have pity built up
GUARANTEED    <- FALSE    # change to TRUE if you already lost a 50/50
PRIMOS_SAVED  <- 28800    # your current primogem savings (for the table)
PRIMOS_PER_PULL <- 160    # cost of one pull in primogems

# replicate() is the efficient way to run a function N times and 
# collect all results into a single vector.
# It's equivalent to a for-loop that saves each result.

cat("Running", N_SIMS, "simulations... please wait.\n")

pulls_vector <- replicate(
  N_SIMS, 
  simulate_session(
    copies_needed    = COPIES_NEEDED,
    start_pity       = START_PITY,
    start_guaranteed = GUARANTEED
  )
)

# pulls_vector is now a list of 100,000 numbers.
# Each number = how many pulls one simulated player needed.
# Example: c(94, 187, 73, 241, 112, ...)

cat("Done! Simulations complete.\n")


# --- SECTION 4: QUICK SANITY CHECK --------------------------
# Before building tables, verify the numbers make sense.
# Community data tells us C0 median is roughly 80 pulls.
# C2 (3 copies) should be roughly 240-260 pulls median.

cat("\n--- Simulation Summary ---\n")
cat("Target: C", COPIES_NEEDED - 1, "(", COPIES_NEEDED, "copies )\n")
cat("Minimum pulls recorded :", min(pulls_vector), "\n")
cat("Median pulls (50th pct) :", median(pulls_vector), "\n")
cat("90th percentile         :", quantile(pulls_vector, 0.90), "\n")
cat("Maximum pulls recorded  :", max(pulls_vector), "\n")


# --- SECTION 5: BUILD TABLE 1 — RAW SIMULATION LOG ----------
# This is the big table: one row per simulation run.

sim_raw_results <- data.frame(
  simulation_id     = 1:N_SIMS,
  starting_primogems = PRIMOS_SAVED,
  target_goal       = paste0("C", COPIES_NEEDED - 1),  # formats as "C2"
  pulls_spent       = pulls_vector,
  primogems_spent   = pulls_vector * PRIMOS_PER_PULL,
  
  # goal_met: did this simulated player have enough primogems?
  # We check if primogems_spent <= their savings.
  # 1 = yes they succeeded, 0 = no they ran out
  goal_met = ifelse(pulls_vector * PRIMOS_PER_PULL <= PRIMOS_SAVED, 1, 0)
)

# Preview the first 6 rows to make sure it looks right
cat("\n--- Table 1 Preview (first 6 rows) ---\n")
print(head(sim_raw_results))


# --- SECTION 6: BUILD TABLE 2 — PROBABILITY DISTRIBUTION ----
# This is the analytical table Power BI will use for charts.
# We group by how many pulls were spent and calculate:
#   - empirical_probability: % of simulations that ended at exactly N pulls
#   - cumulative_probability: % of simulations that finished IN N pulls or fewer
#   - risk_of_failure: % who still haven't finished by pull N

sim_probability_dist <- sim_raw_results %>%
  
  # count() groups rows by pulls_spent and counts how many rows are in each group
  count(pulls_spent) %>%
  
  # mutate() adds new columns based on existing ones
  mutate(
    empirical_probability  = n / N_SIMS,               # fraction of simulations
    cumulative_probability = cumsum(empirical_probability), # running total
    risk_of_failure        = 1 - cumulative_probability     # inverse
  ) %>%
  
  # select() keeps only the columns we want (drops the raw "n" count column)
  select(pulls_spent, empirical_probability, cumulative_probability, risk_of_failure)

cat("\n--- Table 2 Preview (rows around the median) ---\n")
# Show rows where cumulative probability is near 50%
print(sim_probability_dist %>% filter(cumulative_probability > 0.45, 
                                      cumulative_probability < 0.55))


# --- SECTION 7: EXPORT TO CSV -------------------------------
# These files are what you'll import into Power BI.
# They save to whatever folder your R script is located in.

write.csv(sim_raw_results,      "sim_raw_results.csv",      row.names = FALSE)
write.csv(sim_probability_dist, "sim_probability_dist.csv", row.names = FALSE)

cat("\n--- Export Complete ---\n")
cat("Files saved:\n")
cat("  sim_raw_results.csv      (", N_SIMS, "rows )\n")
cat("  sim_probability_dist.csv ( summary table )\n")
cat("\nBring both files into Power BI to build your dashboard.\n")

#==========VISUALISATION OF THINGS============================
#Distributions of pulls for C2
#100000 simulated wishing sessions
#=============================================================
ggplot(sim_raw_results, aes(x = pulls_spent)) +
  geom_histogram(binwidth = 5, fill = "#5B8CFF", color = "white", alpha = 0.85) +
  geom_vline(xintercept = median(pulls_vector), color = "#FF6B6B", 
             linewidth = 1.2, linetype = "dashed") +
  annotate("text", 
           x = median(pulls_vector) + 10, 
           y = Inf, 
           label = paste("Median:", median(pulls_vector), "pulls"),
           vjust = 2, color = "#FF6B6B", fontface = "bold") +
  labs(
    title    = "Distribution of Pulls Needed to Reach C2",
    subtitle = "100,000 simulated wishing sessions",
    x        = "Total Pulls Spent",
    y        = "Number of Simulations",
    caption  = "Red dashed line = median pull count"
  ) +
  theme_minimal()
#=============Cumulative probability of reaching C2============================
#Probability of successs given N total pulls available
#"If I have X pulls worth of Primogems, what are my actual odds?"
#==============================================================================
ggplot(sim_probability_dist, aes(x = pulls_spent, y = cumulative_probability)) +
  geom_area(fill = "#5B8CFF", alpha = 0.3) +
  geom_line(color = "#5B8CFF", linewidth = 1.3) +
  
  # 50% confidence line
  geom_hline(yintercept = 0.50, linetype = "dashed", color = "#FF6B6B") +
  geom_hline(yintercept = 0.90, linetype = "dashed", color = "#FFA500") +
  
  # Labels on the confidence lines
  annotate("text", x = max(sim_probability_dist$pulls_spent) * 0.1,
           y = 0.52, label = "50% confidence", color = "#FF6B6B", fontface = "bold") +
  annotate("text", x = max(sim_probability_dist$pulls_spent) * 0.1,
           y = 0.92, label = "90% confidence", color = "#FFA500", fontface = "bold") +
  
  scale_y_continuous(labels = percent_format()) +
  labs(
    title    = "Cumulative Probability of Reaching C2",
    subtitle = "Probability of success given N total pulls available",
    x        = "Total Pulls Available",
    y        = "Probability of Success",
    caption  = "Based on 100,000 Monte Carlo simulations"
  ) +
  theme_minimal()

#==============Risk of Failure Curve====================================
#What's the chance I still fail at each pull count?
#probability of not reaching C2
#=======================================================================
ggplot(sim_probability_dist, aes(x = pulls_spent, y = risk_of_failure)) +
  geom_area(fill = "#FF6B6B", alpha = 0.3) +
  geom_line(color = "#FF6B6B", linewidth = 1.3) +
  geom_hline(yintercept = 0.10, linetype = "dashed", color = "#333333") +
  annotate("text", x = max(sim_probability_dist$pulls_spent) * 0.6,
           y = 0.12, label = "10% failure threshold", 
           color = "#333333", fontface = "bold") +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title    = "Risk of Failure at Each Pull Count",
    subtitle = "Probability of NOT reaching C2 given N pulls available",
    x        = "Total Pulls Available",
    y        = "Risk of Failure",
    caption  = "Based on 100,000 Monte Carlo simulations"
  ) +
  theme_minimal()
#======================Budget Reality Check===============================
#Given my actual Primogem savings, what can I realistically afford?
#=========================================================================

# Convert pulls to primogem cost for the x-axis
sim_probability_dist_primos <- sim_probability_dist %>%
  mutate(primogems_needed = pulls_spent * 160)

# Where does the player's budget land on the curve?
player_budget     <- PRIMOS_SAVED   # uses the variable you set earlier
player_max_pulls  <- floor(player_budget / 160)
player_odds       <- sim_probability_dist %>%
  filter(pulls_spent <= player_max_pulls) %>%
  summarise(odds = max(cumulative_probability)) %>%
  pull(odds)

ggplot(sim_probability_dist_primos, 
       aes(x = primogems_needed, y = cumulative_probability)) +
  geom_area(fill = "#5B8CFF", alpha = 0.25) +
  geom_line(color = "#5B8CFF", linewidth = 1.3) +
  
  # Player's budget vertical line
  geom_vline(xintercept = player_budget, color = "#00C896", 
             linewidth = 1.3, linetype = "solid") +
  annotate("text",
           x     = player_budget + 1000,
           y     = 0.15,
           label = paste0("Your budget:\n", 
                          format(player_budget, big.mark = ","), 
                          " primos\n",
                          round(player_odds * 100, 1), "% success odds"),
           hjust = 0, color = "#00C896", fontface = "bold", size = 3.5) +
  
  scale_x_continuous(labels = comma_format()) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title    = "Success Probability by Primogem Budget",
    subtitle = paste0("Target: C2 | Green line = your current savings"),
    x        = "Primogems Available",
    y        = "Probability of Success",
    caption  = "Based on 100,000 Monte Carlo simulations"
  ) +
  theme_minimal()
  theme_minimal()